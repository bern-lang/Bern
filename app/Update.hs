{-# LANGUAGE ScopedTypeVariables #-}

{-
Module: Update

English:
  A best-effort "is there a newer Bern?" check. When you start the REPL or run a
  .brn file with the interpreter, this fetches the version manifest published in
  the docs (the same JSON the installers read) and, if a newer Bern has been
  released, prints a one-line notice to stderr.

  Three principles keep it unobtrusive:
    * Fail-silent. Offline, no curl, a malformed manifest -- any failure just
      means no notice. It never errors, never blocks the language, never writes
      to stdout (so pipes and program output stay clean).
    * Throttled. The result is cached under the user's cache directory and only
      refreshed once a day, so the common path is a tiny file read with no
      network at all.
    * Opt-out. Setting BERN_NO_UPDATE_CHECK disables it entirely.

  We deliberately avoid pulling in an HTTP/JSON library: the check shells out to
  curl (and falls back to PowerShell on Windows), the very tools the installers
  already rely on, and scans the small, controlled manifest for the version
  string by hand.

Português:
  Uma verificação de melhor-esforço de "existe um Bern mais novo?". Ao iniciar o
  REPL ou rodar um arquivo .brn com o interpretador, isto busca o manifesto de
  versão publicado nos docs (o mesmo JSON que os instaladores leem) e, se um Bern
  mais novo foi lançado, imprime um aviso de uma linha no stderr.

  Três princípios o mantêm discreto:
    * Falha em silêncio. Offline, sem curl, um manifesto malformado -- qualquer
      falha apenas significa nenhum aviso. Nunca dá erro, nunca bloqueia a
      linguagem, nunca escreve no stdout (então pipes e a saída do programa ficam
      limpos).
    * Limitado por tempo. O resultado é cacheado no diretório de cache do usuário
      e só é atualizado uma vez por dia, então o caminho comum é uma leitura
      minúscula de arquivo sem nenhuma rede.
    * Opção de desligar. Definir BERN_NO_UPDATE_CHECK o desativa por completo.

  Evitamos de propósito puxar uma biblioteca de HTTP/JSON: a verificação invoca o
  curl (e cai para o PowerShell no Windows), as mesmas ferramentas das quais os
  instaladores já dependem, e varre o manifesto pequeno e controlado em busca da
  string de versão na mão.
-}
module Update
    ( currentVersion
    , notifyIfOutdated
    ) where

import Control.Exception (try, SomeException)
import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf, dropWhileEnd)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory
    ( XdgDirectory(XdgCache), getXdgDirectory, doesFileExist
    , getModificationTime, createDirectoryIfMissing )
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

-- The version this interpreter reports as its own. Keep in step with the
-- `version` field in Bern.cabal and the REPL banner.
-- A versão que este interpretador reporta como a sua. Mantenha em sincronia com
-- o campo `version` no Bern.cabal e com o banner do REPL.
currentVersion :: String
currentVersion = "2.2.0"

-- Where the published manifest lives (served by GitHub Pages).
-- Onde o manifesto publicado vive (servido pelo GitHub Pages).
manifestUrl :: String
manifestUrl = "https://bern-lang.github.io/Bern/install/manifest.json"

-- How long a cached answer is trusted before we look again (24h, in seconds).
-- Por quanto tempo uma resposta cacheada é confiável antes de olharmos de novo.
checkInterval :: Double
checkInterval = 86400

{-
English:
  The one public action. Swallows every exception so a broken check can never
  affect the program that called it. Honours BERN_NO_UPDATE_CHECK, then compares
  the latest known version against ours and prints a notice only when ours is
  behind.

Português:
  A única ação pública. Engole toda exceção para que uma verificação quebrada
  jamais afete o programa que a chamou. Respeita BERN_NO_UPDATE_CHECK, então
  compara a última versão conhecida com a nossa e imprime um aviso só quando a
  nossa está atrás.
-}
notifyIfOutdated :: IO ()
notifyIfOutdated = do
  result <- try go :: IO (Either SomeException ())
  case result of
    Right () -> return ()
    Left _   -> return ()   -- never let the update check disturb the user
  where
    go = do
      optedOut <- lookupEnv "BERN_NO_UPDATE_CHECK"
      case optedOut of
        Just _  -> return ()
        Nothing -> do
          mLatest <- getLatestVersion
          case mLatest of
            Just latest | isNewer latest currentVersion -> printNotice latest
            _                                            -> return ()

-- The notice. To stderr, so it never contaminates a program's real output.
-- O aviso. No stderr, para nunca contaminar a saída real de um programa.
printNotice :: String -> IO ()
printNotice latest = do
  let yellow = "\x1b[33m"
      reset  = "\x1b[0m"
  hPutStrLn stderr ""
  hPutStrLn stderr (yellow ++ "[bern] A new version is available: v" ++ latest
                    ++ " (you have v" ++ currentVersion ++ ")" ++ reset)
  hPutStrLn stderr (yellow ++ "[bern] Update at https://bern-lang.github.io/Bern" ++ reset)

{-
English:
  Return the latest version we know about. If the cache file was written within
  the last day, trust it (no network). Otherwise fetch the manifest, cache the
  result, and use it -- falling back to a stale cache if the fetch fails.

Português:
  Retorna a última versão que conhecemos. Se o arquivo de cache foi escrito no
  último dia, confia nele (sem rede). Caso contrário, busca o manifesto, cacheia o
  resultado e o usa -- caindo para um cache velho se a busca falhar.
-}
getLatestVersion :: IO (Maybe String)
getLatestVersion = do
  cacheFile <- cachePath
  fresh <- isFresh cacheFile
  if fresh
    then readCache cacheFile
    else do
      mJson <- fetchManifest manifestUrl
      case mJson >>= extractBernVersion of
        Just v  -> writeCache cacheFile v >> return (Just v)
        Nothing -> readCache cacheFile   -- network failed: use stale value if any

-- The cache file path, under the user's per-user cache directory.
-- O caminho do arquivo de cache, sob o diretório de cache do usuário.
cachePath :: IO FilePath
cachePath = do
  dir <- getXdgDirectory XdgCache "bern"
  return (dir </> "latest-version")

-- True if the cache exists and was refreshed within checkInterval.
-- Verdadeiro se o cache existe e foi atualizado dentro de checkInterval.
isFresh :: FilePath -> IO Bool
isFresh f = do
  ex <- doesFileExist f
  if not ex
    then return False
    else do
      modified <- getModificationTime f
      now <- getCurrentTime
      return (realToFrac (diffUTCTime now modified) < checkInterval)

-- Read the cached version string (first non-empty trimmed line), if present.
-- Lê a string de versão cacheada (primeira linha não-vazia aparada), se houver.
readCache :: FilePath -> IO (Maybe String)
readCache f = do
  ex <- doesFileExist f
  if not ex
    then return Nothing
    else do
      contents <- readFile f
      let v = trim (takeWhile (/= '\n') contents)
      return (if null v then Nothing else Just v)

-- Persist the latest version (this also bumps the file's mtime, our throttle).
-- Persiste a última versão (isto também atualiza o mtime do arquivo, nosso limite).
writeCache :: FilePath -> String -> IO ()
writeCache f v = do
  createDirectoryIfMissing True (takeDirectory f)
  writeFile f v

{-
English:
  Fetch the manifest text without a Haskell HTTP stack: try curl first (present
  on Linux, macOS, and Windows 10+), then PowerShell as a Windows fallback. Each
  attempt is wrapped so a missing tool or a non-zero exit just moves on. A short
  --max-time keeps the once-a-day refresh from ever hanging.

Português:
  Busca o texto do manifesto sem uma pilha HTTP em Haskell: tenta o curl primeiro
  (presente em Linux, macOS e Windows 10+), depois o PowerShell como reserva no
  Windows. Cada tentativa é protegida para que uma ferramenta ausente ou uma saída
  diferente de zero apenas siga adiante. Um --max-time curto impede que a
  atualização de uma vez por dia trave.
-}
fetchManifest :: String -> IO (Maybe String)
fetchManifest url = attempt candidates
  where
    candidates =
      [ ("curl", ["-fsSL", "--max-time", "3", url])
      , ("powershell",
          [ "-NoProfile", "-Command"
          , "(Invoke-WebRequest -UseBasicParsing -TimeoutSec 3 -Uri '"
            ++ url ++ "').Content" ])
      ]
    attempt [] = return Nothing
    attempt ((cmd, args):rest) = do
      r <- try (readProcessWithExitCode cmd args "")
             :: IO (Either SomeException (ExitCode, String, String))
      case r of
        Right (ExitSuccess, out, _) | not (null (trim out)) -> return (Just out)
        _ -> attempt rest

{-
English:
  Pull "bern" -> "version" out of the manifest with a hand scan. The manifest is
  small and we control its shape, so we find the "bern" object, then the first
  "version" key after it, then the quoted value. Anything unexpected yields
  Nothing, which the caller treats as "no notice".

Português:
  Extrai "bern" -> "version" do manifesto com uma varredura manual. O manifesto é
  pequeno e controlamos seu formato, então achamos o objeto "bern", depois a
  primeira chave "version" após ele, depois o valor entre aspas. Qualquer coisa
  inesperada resulta em Nothing, que o chamador trata como "nenhum aviso".
-}
extractBernVersion :: String -> Maybe String
extractBernVersion json = do
  afterBern    <- afterSub "\"bern\"" json
  afterVersion <- afterSub "\"version\"" afterBern
  takeQuoted afterVersion

-- Return what follows the first occurrence of `needle`, or Nothing.
-- Retorna o que segue a primeira ocorrência de `needle`, ou Nothing.
afterSub :: String -> String -> Maybe String
afterSub needle = go
  where
    go [] = Nothing
    go s@(_:cs)
      | needle `isPrefixOf` s = Just (drop (length needle) s)
      | otherwise             = go cs

-- Skip to the first '"' and return the text up to the next '"'.
-- Avança até a primeira '"' e retorna o texto até a próxima '"'.
takeQuoted :: String -> Maybe String
takeQuoted s =
  case dropWhile (/= '"') s of
    ('"':rest) -> Just (takeWhile (/= '"') rest)
    _          -> Nothing

{-
English:
  Is `latest` a strictly higher version than `cur`? Both are parsed into lists of
  integers ("2.0.0" -> [2,0,0]), zero-padded to equal length, and compared
  component by component. Non-numeric junk is ignored, and a parse miss counts as
  0, so a weird value can never spuriously claim to be newer.

Português:
  `latest` é uma versão estritamente maior que `cur`? Ambas são parseadas em
  listas de inteiros ("2.0.0" -> [2,0,0]), preenchidas com zeros até o mesmo
  comprimento, e comparadas componente a componente. Lixo não-numérico é ignorado,
  e uma falha de parse conta como 0, então um valor estranho nunca pode alegar
  falsamente ser mais novo.
-}
isNewer :: String -> String -> Bool
isNewer latest cur =
  let a = parseVersion latest
      b = parseVersion cur
      n = max (length a) (length b)
      pad xs = take n (xs ++ repeat 0)
  in pad a > pad b

parseVersion :: String -> [Int]
parseVersion = map toInt . splitDots . takeWhile (\c -> isDigit c || c == '.')
  where
    toInt x = case reads x of [(n, _)] -> n; _ -> 0

splitDots :: String -> [String]
splitDots s = case break (== '.') s of
  (a, [])       -> [a]
  (a, _:rest)   -> a : splitDots rest

-- Trim leading/trailing whitespace.
-- Apara espaços em branco do início e do fim.
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
