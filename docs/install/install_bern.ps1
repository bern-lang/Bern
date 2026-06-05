<#
.SYNOPSIS
    Installs the Bern programming language from GitHub to C:\Bern and adds it to the user PATH.
.DESCRIPTION
    Downloads the latest Bern.exe release from GitHub, places it in C:\Bern, and updates the user PATH.
#>

$ErrorActionPreference = 'Stop'

# Version of THIS installer script. Compared against the manifest below so we
# can tell the user when a newer installer is available.
$ScriptVersion = "1.0.0"

# Single source of truth for what to install. Lives in the docs and is served
# by GitHub Pages, so a new Bern release only needs the manifest bumped - this
# script does not have to be edited every time.
$manifestUrl  = "https://bern-lang.github.io/Bern/install/manifest.json"
$installerUrl = "https://bern-lang.github.io/Bern/install/install_bern.ps1"

$installDir = "C:\Bern"

# Fallback values, used only if the manifest cannot be fetched/parsed.
$repo    = "bern-lang/Bern"
$zipName = "Bern_win_2.0.0.zip"
$exeName = "Bern.exe"
$version = "2.0.0"




# Fetch the manifest. Everything below degrades gracefully to the fallback
# values above if this fails (offline, Pages down, malformed JSON, ...).
$installerLatest = $null
try {
    $manifest = Invoke-RestMethod -Uri $manifestUrl -TimeoutSec 10
    if ($manifest.bern) {
        if ($manifest.bern.version)       { $version = $manifest.bern.version }
        if ($manifest.bern.repo)          { $repo    = $manifest.bern.repo }
        if ($manifest.bern.windows.zip)   { $zipName = $manifest.bern.windows.zip }
        if ($manifest.bern.windows.exe)   { $exeName = $manifest.bern.windows.exe }
    }
    if ($manifest.installer.version) { $installerLatest = $manifest.installer.version }
    if ($manifest.installer.ps1)     { $installerUrl    = $manifest.installer.ps1 }
} catch {
    Write-Host "[bern] Could not fetch manifest ($manifestUrl); using built-in defaults." -ForegroundColor DarkGray
}

$zipPath = Join-Path $installDir $zipName
$exePath = Join-Path $installDir $exeName

# Bern REPL-style banner
$banner = @(
    "______                  ",
    "| ___ \                       The Bern Language Installer ",
    "| |_/ / ___ _ __ _ __       ",
    "| ___ \/ _ \ '__| '_ \      This script will install Bern on",
    "| |_/ /  __/ |  | | | |                 your machine",
    "\____/ \___|_|  |_| |_|         [ v.$version ]"
)
foreach ($line in $banner) {
    Write-Host $line -ForegroundColor Magenta
}
Write-Host ""
Write-Host "--------------------------------------------------------------" -ForegroundColor DarkGray
Write-Host ""

# Tell the user if the installer itself is out of date.
if ($installerLatest) {
    try {
        if ([version]$installerLatest -gt [version]$ScriptVersion) {
            Write-Host "[bern] A newer installer is available (v$installerLatest, you have v$ScriptVersion)." -ForegroundColor Yellow
            Write-Host "[bern] Re-run to get the latest: iex (irm $installerUrl)" -ForegroundColor Yellow
            Write-Host ""
        }
    } catch { }
}

Write-Host "[bern] Installing to $installDir..." -ForegroundColor Cyan

# Detect an existing install and report whether this is an update.
$versionFile = Join-Path $installDir "version.txt"
if (Test-Path $versionFile) {
    $installed = (Get-Content $versionFile -Raw).Trim()
    if ($installed -eq $version) {
        Write-Host "[bern] Bern v$version is already installed - reinstalling/repairing." -ForegroundColor DarkGray
    } else {
        Write-Host "[bern] Updating Bern v$installed -> v$version." -ForegroundColor Cyan
    }
}

# Create install directory if it doesn't exist
if (!(Test-Path $installDir)) {
    New-Item -ItemType Directory -Path $installDir | Out-Null
}

# Get latest release info from GitHub
$releaseUrl = "https://api.github.com/repos/$repo/releases/latest"
Write-Host "[bern] Fetching latest release info from GitHub..." -ForegroundColor Cyan
$release = Invoke-RestMethod -Uri $releaseUrl

Write-Host ""

# Find Bern.zip asset download URL
$asset = $release.assets | Where-Object { $_.name -eq $zipName }
if (-not $asset) {
    Write-Error "Bern.zip not found in the latest release assets."
    exit 1
}
$downloadUrl = $asset.browser_download_url

Write-Host "[bern] Downloading $zipName from $downloadUrl..." -ForegroundColor Cyan
Invoke-WebRequest -Uri $downloadUrl -OutFile $zipPath

Write-Host ""

# Extract to a temporary folder to avoid overwrite errors
$tempDir = Join-Path $env:TEMP "bern_install_tmp"
if (Test-Path $tempDir) { Remove-Item $tempDir -Recurse -Force }
New-Item -ItemType Directory -Path $tempDir | Out-Null

Write-Host "[bern] Extracting Bern.exe from $zipName..." -ForegroundColor Cyan
Add-Type -AssemblyName System.IO.Compression.FileSystem
[System.IO.Compression.ZipFile]::ExtractToDirectory($zipPath, $tempDir)


# Copy Bern.exe to installDir
$tempExePath = Join-Path $tempDir $exeName
if (!(Test-Path $tempExePath)) {
    Write-Error "Bern.exe not found in the extracted zip."
    Remove-Item $tempDir -Recurse -Force
    exit 1
}
Copy-Item $tempExePath -Destination $exePath -Force

# Copy lib folder to installDir
$tempLibPath = Join-Path $tempDir 'lib'
$destLibPath = Join-Path $installDir 'lib'
if (Test-Path $tempLibPath) {
    if (Test-Path $destLibPath) {
        Remove-Item $destLibPath -Recurse -Force
    }
    Copy-Item $tempLibPath -Destination $installDir -Recurse
    Write-Host "[bern] lib folder copied to $installDir." -ForegroundColor Green
} else {
    Write-Host "[bern] lib folder not found in the extracted zip." -ForegroundColor Yellow
}

# Clean up temp and zip
Remove-Item $zipPath
Remove-Item $tempDir -Recurse -Force

Write-Host "[bern] Bern.exe extracted to $installDir." -ForegroundColor Green

# Record the installed version so future runs can detect updates.
Set-Content -Path $versionFile -Value $version -NoNewline

# Add installDir to user PATH if not already present
$envPath = [Environment]::GetEnvironmentVariable("Path", "User")
if ($envPath -notlike "*$installDir*") {
        Write-Host "[bern] Adding $installDir to user PATH..." -ForegroundColor Cyan
        [Environment]::SetEnvironmentVariable("Path", "$envPath;$installDir", "User")
        Write-Host "[bern] PATH updated. You may need to restart your terminal." -ForegroundColor Yellow
} else {
    Write-Host "[bern] $installDir is already in your PATH." -ForegroundColor DarkGray
}

Write-Host "[bern] Installation complete! Bern v$version is installed." -ForegroundColor Green
