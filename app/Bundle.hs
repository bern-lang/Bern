{-
Module: Bundle

English:
  The ".exe bundler": it turns a Bern script into a single, self-contained
  executable that runs anywhere, even where Bern is not installed and there is no
  lib/ folder to find.

  The trick is simple and is called an "append blob". We take a fresh copy of the
  interpreter executable and STICK the program's source (and every library it
  imports) onto the end of it, followed by a small footer:

      [ interpreter bytes ][ payload ][ 20-byte length ][ 8-byte magic ]

  Appending bytes to the end of an executable does not stop it from running --
  the OS only looks at the real program at the front. So the resulting file is
  still a working interpreter, but now carrying its own script as cargo. On
  startup the interpreter reads the footer: if it finds the magic marker, it
  knows it is a bundle, extracts the payload to a temp directory, and runs the
  embedded main.brn (this is the `detectBundle`/`runBundle` half). If the marker
  is absent, it behaves as the normal interpreter.

  This module owns BOTH halves -- building a bundle (`buildCommand`) and running
  one (`detectBundle` + `runBundle`). To run the extracted script it needs the
  "parse a file and run it" routine, but that lives in Main; rather than import
  Main (which would be a cycle), `runBundle` simply takes that routine as a
  parameter.

Português:
  O "empacotador de .exe": ele transforma um script Bern em um único executável
  independente que roda em qualquer lugar, mesmo onde o Bern não está instalado e
  não há pasta lib/ para encontrar.

  O truque é simples e se chama "append blob" (blob anexado). Pegamos uma cópia
  nova do executável do interpretador e COLAMOS o código do programa (e toda
  biblioteca que ele importa) no fim dele, seguido de um pequeno rodapé:

      [ bytes do interpretador ][ payload ][ tamanho de 20 bytes ][ magic de 8 bytes ]

  Anexar bytes ao fim de um executável não o impede de rodar -- o SO só olha o
  programa de verdade na frente. Então o arquivo resultante ainda é um
  interpretador funcional, mas agora carregando o próprio script como carga. Na
  inicialização, o interpretador lê o rodapé: se encontra o marcador mágico, sabe
  que é um bundle, extrai o payload para um diretório temporário e roda o main.brn
  embutido (esta é a metade `detectBundle`/`runBundle`). Se o marcador não está
  presente, ele se comporta como o interpretador normal.

  Este módulo é dono das DUAS metades -- construir um bundle (`buildCommand`) e
  rodar um (`detectBundle` + `runBundle`). Para rodar o script extraído ele
  precisa da rotina de "parsear um arquivo e rodar", mas ela mora no Main; em vez
  de importar o Main (o que seria um ciclo), o `runBundle` simplesmente recebe
  essa rotina como parâmetro.
-}
module Bundle
    ( detectBundle
    , runBundle
    , buildCommand
    ) where

import System.IO (withFile, IOMode(ReadMode), hFileSize, hSeek, SeekMode(SeekFromEnd, AbsoluteSeek))
import System.Environment (getExecutablePath, setEnv)
import System.Exit (exitFailure)
import System.Directory (doesFileExist, getTemporaryDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), takeBaseName, takeDirectory, pathSeparator)
import Control.Monad (forM_)
import Text.Read (readMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

{-
English:
  The footer constants. `bundleMagic` is an arbitrary 8-byte tag we look for to
  decide "is this a bundle?"; it just has to be unlikely to occur by accident.
  `lenFieldLen` is how many bytes the payload-length number is padded to (always
  20 digits) so the footer has a fixed, known size we can seek to. `footerLen` is
  the two together -- the exact number of bytes to read from the end.

Português:
  As constantes do rodapé. `bundleMagic` é uma etiqueta arbitrária de 8 bytes que
  procuramos para decidir "isto é um bundle?"; ela só precisa ser improvável de
  ocorrer por acaso. `lenFieldLen` é em quantos bytes o número do tamanho do
  payload é preenchido (sempre 20 dígitos) para que o rodapé tenha um tamanho
  fixo e conhecido até onde possamos saltar. `footerLen` é os dois juntos -- o
  número exato de bytes a ler do fim.
-}
bundleMagic :: BS.ByteString
bundleMagic = BC.pack "BERNBNDL"

lenFieldLen :: Int
lenFieldLen = 20

footerLen :: Int
footerLen = lenFieldLen + BS.length bundleMagic

{-
English:
  Decide whether the file at `path` (in practice, our own executable) is a
  bundle, and if so return its payload bytes.

  We do NOT read the whole executable -- it could be tens of megabytes. Instead
  we open it, seek to `footerLen` bytes before the end, and read just the footer.
  If the last 8 bytes aren't our magic, it's a plain interpreter: return Nothing.
  Otherwise we parse the 20-byte length, seek back that far, and read exactly the
  payload. The `<= size - footerLen` guard is a sanity check so a corrupt or
  spoofed length can't make us read past the start of the file.

  `withFile ... ReadMode` guarantees the handle is closed afterwards even if
  something throws.

Português:
  Decide se o arquivo em `path` (na prática, o nosso próprio executável) é um
  bundle e, se for, retorna os bytes do payload.

  NÃO lemos o executável inteiro -- ele pode ter dezenas de megabytes. Em vez
  disso, o abrimos, saltamos para `footerLen` bytes antes do fim e lemos só o
  rodapé. Se os últimos 8 bytes não forem o nosso magic, é um interpretador puro:
  retorna Nothing. Caso contrário, parseamos o tamanho de 20 bytes, voltamos essa
  distância e lemos exatamente o payload. A guarda `<= size - footerLen` é uma
  checagem de sanidade para que um tamanho corrompido ou forjado não nos faça ler
  antes do início do arquivo.

  `withFile ... ReadMode` garante que o handle seja fechado depois, mesmo que
  algo lance uma exceção.
-}
detectBundle :: FilePath -> IO (Maybe BS.ByteString)
detectBundle path = withFile path ReadMode $ \h -> do
  size <- hFileSize h
  if size < fromIntegral footerLen
    then return Nothing                    -- too small to even contain a footer
    else do
      hSeek h SeekFromEnd (negate (fromIntegral footerLen))
      footer <- BS.hGet h footerLen
      let lenPart = BS.take lenFieldLen footer
          magicPart = BS.drop lenFieldLen footer
      if magicPart /= bundleMagic
        then return Nothing                -- no magic -> this is the plain interpreter
        else case readMaybe (BC.unpack lenPart) :: Maybe Int of
          Just plen | plen >= 0 && fromIntegral plen <= size - fromIntegral footerLen -> do
            -- Jump to where the payload starts and read exactly its bytes.
            hSeek h AbsoluteSeek (fromIntegral size - fromIntegral footerLen - fromIntegral plen)
            payload <- BS.hGet h plen
            return (Just payload)
          _ -> return Nothing

{-
English:
  Run an extracted bundle. We write every embedded file out to a temp directory,
  then point the module resolver at it by setting the BERN_LIB_PATH environment
  variable -- that is how imports inside the bundled program find their libraries
  without depending on the user's working directory. Finally we run the bundle's
  main.brn with the `runFile` we were handed.

  The working directory is left untouched on purpose: if the bundled program does
  its own file I/O (read_file/write_file), it should see the user's files, not the
  temp dir.

Português:
  Roda um bundle extraído. Escrevemos cada arquivo embutido em um diretório
  temporário e então apontamos o resolvedor de módulos para ele definindo a
  variável de ambiente BERN_LIB_PATH -- é assim que os imports dentro do programa
  empacotado encontram suas bibliotecas sem depender do diretório de trabalho do
  usuário. Por fim, rodamos o main.brn do bundle com o `runFile` que recebemos.

  O diretório de trabalho é deixado intacto de propósito: se o programa empacotado
  faz a própria I/O de arquivos (read_file/write_file), ele deve ver os arquivos
  do usuário, não o diretório temporário.
-}
runBundle :: (FilePath -> IO ()) -> BS.ByteString -> IO ()
runBundle runFile payload = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "bern_bundle_run"
  createDirectoryIfMissing True dir
  -- Lay every embedded file back onto disk under `dir`, recreating sub-folders
  -- (lib/...) and translating '/' to the host path separator.
  forM_ (parseEntries payload) $ \(rel, content) -> do
    let full = dir </> map (\c -> if c == '/' then pathSeparator else c) rel
    createDirectoryIfMissing True (takeDirectory full)
    BS.writeFile full content
  setEnv "BERN_LIB_PATH" dir
  runFile (dir </> "main.brn")

{-
English:
  Split a payload back into its (relative-path, contents) entries. The payload is
  a flat sequence of records, each laid out as:

      <relative path>\n<byte length>\n<that many raw bytes>

  We read the path line, the length line, take exactly that many content bytes,
  and recurse on whatever is left. The length prefix is what lets file contents
  contain newlines safely -- we never have to guess where a file ends.

Português:
  Separa um payload de volta nas suas entradas (caminho-relativo, conteúdo). O
  payload é uma sequência plana de registros, cada um disposto como:

      <caminho relativo>\n<tamanho em bytes>\n<essa quantidade de bytes crus>

  Lemos a linha do caminho, a linha do tamanho, pegamos exatamente essa
  quantidade de bytes de conteúdo e recursionamos no que sobra. O prefixo de
  tamanho é o que permite que o conteúdo dos arquivos contenha quebras de linha
  com segurança -- nunca precisamos adivinhar onde um arquivo termina.
-}
parseEntries :: BS.ByteString -> [(String, BS.ByteString)]
parseEntries bs
  | BS.null bs = []
  | otherwise =
      let (pathLine, r1) = BC.break (== '\n') bs
          (lenLine, r2)  = BC.break (== '\n') (BS.drop 1 r1)
          n              = maybe 0 id (readMaybe (BC.unpack lenLine) :: Maybe Int)
          (content, r3)  = BS.splitAt n (BS.drop 1 r2)
      in (BC.unpack pathLine, content) : parseEntries r3

{-
English:
  A deliberately crude scan for `import <module>` lines in a source file. It just
  splits each line into words and, when the first word is "import", takes the
  second as a module name. We don't fully parse the file here because at build
  time we only need the rough list of imports to know which libraries to embed; a
  missing one is reported as a warning, not a hard error.

Português:
  Uma varredura propositalmente rústica por linhas `import <módulo>` em um arquivo
  fonte. Ela só quebra cada linha em palavras e, quando a primeira palavra é
  "import", pega a segunda como nome de módulo. Não parseamos o arquivo
  completamente aqui porque, no momento do build, só precisamos da lista
  aproximada de imports para saber quais bibliotecas embutir; um faltante é
  reportado como aviso, não como erro fatal.
-}
scanImports :: BS.ByteString -> [String]
scanImports src =
  [ ws !! 1 | l <- lines (BC.unpack src), let ws = words l, length ws >= 2, head ws == "import" ]

{-
English:
  Implements `bern build`. The steps:
    1. Read our own executable bytes and strip any bundle already on the end
       (`stripBundle`), so building from an already-bundled interpreter still
       gives a clean base.
    2. Read the user's script and transitively collect every library it imports
       (`collectLibs`).
    3. Lay the script out as the first entry "main.brn", followed by the libs,
       and concatenate them into the payload.
    4. Append the 20-digit length + magic footer, and write
       [base][payload][footer] to the output file.

  The output name defaults to <script>.exe but can be overridden with -o.

Português:
  Implementa `bern build`. Os passos:
    1. Ler os bytes do nosso próprio executável e remover qualquer bundle já
       existente no fim (`stripBundle`), para que compilar a partir de um
       interpretador já empacotado ainda dê uma base limpa.
    2. Ler o script do usuário e coletar transitivamente toda biblioteca que ele
       importa (`collectLibs`).
    3. Dispor o script como a primeira entrada "main.brn", seguido das libs, e
       concatená-los no payload.
    4. Anexar o tamanho de 20 dígitos + o rodapé magic, e escrever
       [base][payload][rodapé] no arquivo de saída.

  O nome de saída por padrão é <script>.exe, mas pode ser sobrescrito com -o.
-}
buildCommand :: FilePath -> Maybe FilePath -> IO ()
buildCommand scriptPath mOut = do
  exists <- doesFileExist scriptPath
  if not exists
    then putStrLn ("File not found: " ++ scriptPath) >> exitFailure
    else do
      selfPath  <- getExecutablePath
      selfBytes <- BS.readFile selfPath
      let baseExe = stripBundle selfBytes
      mainSrc <- BS.readFile scriptPath
      let installDir = takeDirectory selfPath
      libs <- collectLibs installDir (scanImports mainSrc) [] []
      let entries = ("main.brn", mainSrc) : libs
      let payload = BS.concat (map mkEntry entries)
      let footer  = BS.append (padLen (BS.length payload)) bundleMagic
      let out = maybe (takeBaseName scriptPath ++ ".exe") id mOut
      BS.writeFile out (BS.concat [baseExe, payload, footer])
      putStrLn ("Created " ++ out)
      putStrLn ("  bundled " ++ show (length entries) ++ " file(s), "
                ++ show (BS.length payload) ++ " bytes of source")
  where
    -- Encode one file as "<path>\n<length>\n<bytes>" (the format parseEntries reads).
    -- Codifica um arquivo como "<caminho>\n<tamanho>\n<bytes>" (o formato que parseEntries lê).
    mkEntry (rel, content) =
      BS.concat [BC.pack (rel ++ "\n" ++ show (BS.length content) ++ "\n"), content]
    -- Left-pad the length with zeros to exactly lenFieldLen digits, fixing the footer size.
    -- Preenche o tamanho com zeros à esquerda até exatamente lenFieldLen dígitos, fixando o tamanho do rodapé.
    padLen n = let s = show n in BC.pack (replicate (lenFieldLen - length s) '0' ++ s)

{-
English:
  If the interpreter we are copying is ITSELF a bundle (someone ran `bern build`
  using a bundled bern), drop the appended payload+footer so the new bundle gets a
  clean interpreter base, not a bundle-inside-a-bundle. The logic mirrors
  detectBundle: check the trailing magic, read the length, and cut the payload off
  the end. If there's no valid bundle, return the bytes unchanged.

Português:
  Se o interpretador que estamos copiando é ELE MESMO um bundle (alguém rodou
  `bern build` usando um bern empacotado), remove o payload+rodapé anexado para
  que o novo bundle receba uma base de interpretador limpa, e não um
  bundle-dentro-de-bundle. A lógica espelha o detectBundle: checa o magic do fim,
  lê o tamanho e corta o payload do fim. Se não há um bundle válido, retorna os
  bytes inalterados.
-}
stripBundle :: BS.ByteString -> BS.ByteString
stripBundle bytes =
  let n = BS.length bytes
  in if n < footerLen
       then bytes
       else let footer = BS.drop (n - footerLen) bytes
                lenPart = BS.take lenFieldLen footer
                magicPart = BS.drop lenFieldLen footer
            in if magicPart == bundleMagic
                 then case readMaybe (BC.unpack lenPart) :: Maybe Int of
                        Just plen | plen >= 0 && plen <= n - footerLen ->
                          BS.take (n - footerLen - plen) bytes
                        _ -> bytes
                 else bytes

{-
English:
  Walk the import graph and gather every reachable library file. It is a
  breadth-first crawl with a `seen` list so each module is embedded once and
  import cycles can't loop forever:
    * if a module was already seen, skip it;
    * otherwise resolve it to a path, read it, and ALSO scan ITS imports, adding
      them to the work list (`ms ++ more`) -- that is what makes it transitive;
    * a module that can't be found is warned about and skipped, not fatal.
  `acc` builds the result in reverse for efficiency, hence the `reverse acc` at
  the end.

Português:
  Percorre o grafo de imports e junta todo arquivo de biblioteca alcançável. É
  uma travessia em largura com uma lista `seen` para que cada módulo seja embutido
  uma só vez e ciclos de import não fiquem em loop para sempre:
    * se um módulo já foi visto, pula;
    * caso contrário, resolve para um caminho, lê e TAMBÉM varre OS imports DELE,
      adicionando-os à lista de trabalho (`ms ++ more`) -- é isso que torna a
      coleta transitiva;
    * um módulo que não pode ser encontrado gera aviso e é pulado, não é fatal.
  `acc` constrói o resultado ao contrário por eficiência, daí o `reverse acc` no
  fim.
-}
collectLibs :: FilePath -> [String] -> [String] -> [(String, BS.ByteString)] -> IO [(String, BS.ByteString)]
collectLibs _ [] _ acc = return (reverse acc)
collectLibs installDir (m:ms) seen acc
  | m `elem` seen = collectLibs installDir ms seen acc
  | otherwise = do
      mpath <- resolveModule installDir m
      case mpath of
        Nothing -> do
          putStrLn ("  warning: module '" ++ m ++ "' not found; skipping")
          collectLibs installDir ms (m : seen) acc
        Just p -> do
          content <- BS.readFile p
          let rel  = "lib/" ++ m ++ ".brn"
              more = scanImports content
          collectLibs installDir (ms ++ more) (m : seen) ((rel, content) : acc)

{-
English:
  Find a module's source file by trying the same locations the running
  interpreter would: ./lib/<m>.brn, then ./<m>.brn, then <installDir>/lib/<m>.brn.
  Returns the first one that exists, or Nothing. Keeping this search order in
  sync with the evaluator's import resolution is what makes a bundle behave like
  the original program.

Português:
  Encontra o arquivo-fonte de um módulo tentando os mesmos locais que o
  interpretador em execução tentaria: ./lib/<m>.brn, depois ./<m>.brn, depois
  <installDir>/lib/<m>.brn. Retorna o primeiro que existir, ou Nothing. Manter
  esta ordem de busca em sincronia com a resolução de imports do avaliador é o que
  faz um bundle se comportar como o programa original.
-}
resolveModule :: FilePath -> String -> IO (Maybe FilePath)
resolveModule installDir m =
  firstExisting [ "lib" </> m ++ ".brn", m ++ ".brn", installDir </> "lib" </> m ++ ".brn" ]
  where
    firstExisting [] = return Nothing
    firstExisting (c:cs) = do
      e <- doesFileExist c
      if e then return (Just c) else firstExisting cs
