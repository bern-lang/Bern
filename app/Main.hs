{-# LANGUAGE ScopedTypeVariables #-}

{-
Module: Main

English:
  The program's entry point and command dispatcher -- the thin layer that turns
  "the user ran the `bern` executable" into one of a few concrete actions:
    * run a .brn file,
    * run ./main.brn (the `bern .` shortcut),
    * start the interactive REPL,
    * build a standalone .exe from a script, or
    * print help.

  It deliberately stays small. The heavy lifting lives elsewhere: the REPL is in
  Repl, the .exe bundler is in Bundle, and the actual language (parsing and
  evaluation) is the `Bern` library. Main just wires those pieces together. It
  also owns `runFile`, the one-shot "parse a file and run it" routine, because
  several entry points (a filename argument, `bern .`, and a bundled executable)
  all need it.

Português:
  O ponto de entrada do programa e o despachante de comandos -- a camada fina que
  transforma "o usuário rodou o executável `bern`" em uma de poucas ações
  concretas:
    * rodar um arquivo .brn,
    * rodar ./main.brn (o atalho `bern .`),
    * iniciar o REPL interativo,
    * compilar um .exe independente a partir de um script, ou
    * imprimir a ajuda.

  Ele é mantido pequeno de propósito. O trabalho pesado mora em outros lugares: o
  REPL está em Repl, o empacotador de .exe está em Bundle, e a linguagem de fato
  (parsing e avaliação) é a biblioteca `Bern`. O Main só conecta essas peças. Ele
  também é dono do `runFile`, a rotina de "parsear um arquivo e rodar" de uma vez,
  porque vários pontos de entrada (um argumento de nome de arquivo, `bern .` e um
  executável empacotado) precisam dela.
-}
module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering, LineBuffering), stdout, stdin)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (exitFailure)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>))
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Language.Ast
import Language.Eval
import Data.Hashtable.Hashtable
import Parsing.Parser
import Repl (startRepl)
import Bundle (detectBundle, runBundle, buildCommand)
import Update (notifyIfOutdated, currentVersion)

-- The starting scope for any run: an empty environment with no bindings yet.
-- O escopo inicial de qualquer execução: um ambiente vazio, ainda sem vínculos.
initialTable :: Hashtable String Value
initialTable = emptyHashtable

{-
English:
  The first thing that runs. The steps, in order:

    1. Force UTF-8 and set buffering. UTF-8 makes Bern source with accents/emoji
       behave the same on every OS; NoBuffering on stdout means output appears
       immediately (important so a REPL prompt shows before you type).

    2. Check whether THIS executable is itself a bundle. `bern build` appends a
       script + its libraries to a copy of the interpreter, so the very same
       program can either be the plain interpreter or a self-contained app. If a
       bundle is detected we run it and nothing else -- note we hand `runFile` to
       `runBundle` as an argument, which is how Bundle runs the extracted script
       without importing this module.

    3. Otherwise parse the command-line arguments. `--silent-pragmas` may appear
       anywhere, so we pull it out first (it only flips a flag), then match on
       what remains to pick the action.

Português:
  A primeira coisa que roda. Os passos, em ordem:

    1. Forçar UTF-8 e configurar o buffering. UTF-8 faz com que código Bern com
       acentos/emoji se comporte igual em todo SO; NoBuffering no stdout faz a
       saída aparecer na hora (importante para o prompt do REPL surgir antes de
       você digitar).

    2. Verificar se ESTE executável é, ele mesmo, um bundle. O `bern build` anexa
       um script + suas bibliotecas a uma cópia do interpretador, então o mesmo
       programa pode ser tanto o interpretador puro quanto um app independente. Se
       um bundle é detectado, rodamos ele e mais nada -- repare que passamos o
       `runFile` para o `runBundle` como argumento, que é como o Bundle roda o
       script extraído sem importar este módulo.

    3. Caso contrário, parsear os argumentos de linha de comando.
       `--silent-pragmas` pode aparecer em qualquer lugar, então o retiramos
       primeiro (ele só liga uma flag), e então casamos com o que sobra para
       escolher a ação.
-}
main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin LineBuffering
  -- If this executable has a bundled script appended to it, run that and exit.
  selfPath <- getExecutablePath
  mBundle <- detectBundle selfPath
  case mBundle of
    Just payload -> runBundle runFile payload
    Nothing -> do
      rawArgs <- getArgs
      -- `--silent-pragmas` may appear anywhere; pull it out before dispatching.
      setSilentPragmas ("--silent-pragmas" `elem` rawArgs)
      let args = filter (/= "--silent-pragmas") rawArgs
      -- Each pattern below is one way to invoke `bern`; the last is the catch-all
      -- usage message for anything we don't recognise.
      -- After running a script (but not for `build`/`help` or a bundled app), let
      -- the user know if a newer Bern has been released. The check is fail-silent
      -- and runs last, so it never delays or disturbs the program's own output.
      case args of
        ["."]                       -> runMainBrn >> notifyIfOutdated
        ["version"]                 -> printVersion
        ["--version"]               -> printVersion
        ["-v"]                      -> printVersion
        ["help"]                    -> printHelp
        ["build", script]           -> buildCommand script Nothing
        ["build", script, "-o", o]  -> buildCommand script (Just o)
        ["build"]                   -> putStrLn "Usage: bern build <file.brn> [-o output]"
        [filename]                  -> runFile filename >> notifyIfOutdated
        []                          -> startRepl
        _ -> putStrLn "Usage: bern [filename.brn | build <file.brn> | help]"

{-
English:
  Implements `bern .`: look for a file literally named main.brn in the current
  working directory and run it, or report a clear error if it is missing. A
  convenience so a project with a main.brn can be run by just typing `bern .`.

Português:
  Implementa `bern .`: procura um arquivo chamado literalmente main.brn no
  diretório de trabalho atual e o roda, ou informa um erro claro se ele não
  existir. Uma conveniência para que um projeto com um main.brn possa ser rodado
  só digitando `bern .`.
-}
runMainBrn :: IO ()
runMainBrn = do
  cwd <- getCurrentDirectory
  let mainFile = cwd </> "main.brn"
  exists <- doesFileExist mainFile
  if exists
    then runFile mainFile
    else do
      putStrLn "Error: No main.brn found in current directory."
      exitFailure

-- Print the interpreter's own version. Shares the single source of truth in
-- Update so `bern version` and the update check can never disagree.
-- Imprime a versão do próprio interpretador. Compartilha a fonte única de
-- verdade em Update para que `bern version` e a verificação de updates nunca
-- discordem.
printVersion :: IO ()
printVersion = putStrLn ("Bern " ++ currentVersion)

-- The text printed by `bern help`. Pure output; nothing clever here.
-- O texto impresso por `bern help`. Saída pura; nada de especial aqui.
printHelp :: IO ()
printHelp = do
  putStrLn "Usage: bern [filename.brn | build <file.brn> | help]"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "  bern                    Start the REPL"
  putStrLn "  bern .                  Run ./main.brn from the current directory"
  putStrLn "  bern <file.brn>         Run a Bern source file"
  putStrLn "  bern build <file.brn>   Bundle the script (+ its imported libs) into a"
  putStrLn "                          standalone executable. Use -o to set the output."
  putStrLn "  bern version            Show the installed Bern version"
  putStrLn "  bern help               Show this message"
  putStrLn ""
  putStrLn "Flags:"
  putStrLn "  --silent-pragmas        Suppress the note printed when an imported"
  putStrLn "                          module enables pragmas."

{-
English:
  Parse and run a whole .brn file, once, top to bottom. Unlike the REPL, this is
  non-interactive: any error aborts the process with a non-zero exit code, which
  is the right behaviour for scripts and CI.

  The pipeline:
    1. Read the source text.
    2. applyPragmas: scan it for {--! ... !--} directives and turn them on BEFORE
       parsing, because some pragmas (like no-written-operators) change how the
       text is parsed.
    3. Parse. On a parse error, print it nicely and exit.
    4. checkExhaustive: a load-time safety check that every multi-clause function
       can match all its inputs. If one can't, refuse to run (this is the default;
       the `partial` pragma opts out).
    5. Interpret the whole program against the empty starting scope.
    6. The `main` pragma: if set, call main() after the file finishes loading, so
       a file can define everything and then kick off execution explicitly (like
       a `main` entry point in other languages).

  Haskell note: `case ... of (e:_) -> ...` pulls the FIRST error out of the list
  checkExhaustive returns; `[]` means the list was empty, i.e. no problems.

Português:
  Parseia e roda um arquivo .brn inteiro, de uma vez, de cima para baixo. Ao
  contrário do REPL, isto é não-interativo: qualquer erro aborta o processo com
  um código de saída diferente de zero, que é o comportamento certo para scripts
  e CI.

  O pipeline:
    1. Ler o texto-fonte.
    2. applyPragmas: varrê-lo em busca de diretivas {--! ... !--} e ligá-las ANTES
       de parsear, porque alguns pragmas (como no-written-operators) mudam como o
       texto é parseado.
    3. Parsear. Em erro de parsing, imprime bonito e sai.
    4. checkExhaustive: uma checagem de segurança em tempo de carga de que toda
       função de múltiplas cláusulas consegue casar todas as suas entradas. Se
       alguma não consegue, recusa rodar (este é o padrão; o pragma `partial`
       desativa).
    5. Interpretar o programa inteiro contra o escopo inicial vazio.
    6. O pragma `main`: se ligado, chama main() depois que o arquivo termina de
       carregar, para que um arquivo possa definir tudo e então iniciar a execução
       explicitamente (como um ponto de entrada `main` em outras linguagens).

  Nota de Haskell: `case ... of (e:_) -> ...` tira o PRIMEIRO erro da lista que o
  checkExhaustive retorna; `[]` significa que a lista estava vazia, ou seja, sem
  problemas.
-}
runFile :: FilePath -> IO ()
runFile path = do
  contents <- readFile path
  applyPragmas contents
  case parseBernFile path contents of
    Left err -> putStrLn (formatParseError err) >> exitFailure
    Right cmd -> case checkExhaustive cmd of
      (e:_) -> putStrLn ("Non-exhaustive: " ++ e) >> exitFailure
      [] -> do
        finalTable <- interpretCommand Nothing cmd initialTable
        -- The `main` pragma runs main() after the file is loaded.
        if pragmaOn "main" ()
          then do _ <- interpretCommand Nothing (Print (FunctionCall "main" [])) finalTable
                  return ()
          else return ()
