{-# LANGUAGE ScopedTypeVariables #-}

{-
Module: Repl

English:
  The interactive Read-Eval-Print Loop -- what you get when you run `bern` with
  no arguments. Its job is to read a line (or several, for a multi-line block),
  run it, print the result, and loop, all WITHOUT ever crashing: a parse error or
  a runtime error must show a message and drop you back at the prompt, not kill
  the session.

  Two behaviours make the REPL different from running a file:
    * It keeps the scope ALIVE between inputs. Each line is evaluated against the
      table the previous line produced, so `x = 5` then `x + 1` works. The loop
      threads that table through itself.
    * A bare expression is evaluated, printed, AND bound to the special name `_`,
      so you can build on the last result.

  Only `startRepl` is exported; everything else is the loop's private plumbing.
  Main calls `startRepl` and never touches haskeline directly.

Português:
  O laço interativo Ler-Avaliar-Imprimir (Read-Eval-Print Loop) -- o que você
  obtém ao rodar `bern` sem argumentos. Sua função é ler uma linha (ou várias,
  para um bloco de múltiplas linhas), rodá-la, imprimir o resultado e repetir,
  tudo SEM nunca quebrar: um erro de parsing ou de execução deve mostrar uma
  mensagem e te devolver ao prompt, não encerrar a sessão.

  Dois comportamentos tornam o REPL diferente de rodar um arquivo:
    * Ele mantém o escopo VIVO entre entradas. Cada linha é avaliada contra a
      tabela que a linha anterior produziu, então `x = 5` e depois `x + 1`
      funciona. O laço passa essa tabela por si mesmo.
    * Uma expressão solta é avaliada, impressa E vinculada ao nome especial `_`,
      para que você possa construir em cima do último resultado.

  Só `startRepl` é exportado; todo o resto é o encanamento privado do laço. O Main
  chama `startRepl` e nunca toca no haskeline diretamente.
-}
module Repl
    ( startRepl
    ) where

import System.Exit (ExitCode)
import System.Directory (doesFileExist)
import Control.Exception (try, SomeException, fromException)
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline
import System.Console.Haskeline.History (addHistoryUnlessConsecutiveDupe)
import Data.Char (isAlphaNum)
import qualified Data.ByteString as BS
import Language.Ast
import Language.Eval
import Data.Hashtable.Hashtable
import Language.Helpers (getValueType)
import Parsing.Parser
import Update (notifyIfOutdated)

{-
English:
  The only entry point. It prints the banner, tidies the history file, and starts
  the loop on a fresh (empty) scope. `runInputT` is haskeline's wrapper that gives
  us line editing and history; `repl` runs inside it.

Português:
  O único ponto de entrada. Imprime o banner, arruma o arquivo de histórico e
  inicia o laço com um escopo novo (vazio). `runInputT` é o invólucro do haskeline
  que nos dá edição de linha e histórico; `repl` roda dentro dele.
-}
startRepl :: IO ()
startRepl = do
  printWelcome
  notifyIfOutdated   -- fail-silent; tells you if a newer Bern has been released
  putStrLn ""
  sanitizeHistory ".bern_history"
  runInputT replSettings (repl emptyHashtable)

{-
English:
  haskeline configuration. We point it at a history file so past sessions come
  back on the up-arrow, and turn OFF autoAddHistory because we add cleaned entries
  ourselves (see getMultiline) -- otherwise raw, possibly CRLF-tainted lines would
  be stored.

Português:
  Configuração do haskeline. Apontamos para um arquivo de histórico para que
  sessões antigas voltem na seta-para-cima, e DESLIGAMOS o autoAddHistory porque
  nós mesmos adicionamos entradas já limpas (ver getMultiline) -- senão, linhas
  cruas, possivelmente contaminadas com CRLF, seriam guardadas.
-}
replSettings :: Settings IO
replSettings = defaultSettings
  { historyFile = Just ".bern_history"
  , autoAddHistory = False   -- we add cleaned entries ourselves (see getMultiline)
  }

{-
English:
  Strip stray carriage returns ('\r', byte 13) from a pre-existing history file
  before haskeline loads it. Why this exists: on Windows, or when the REPL was
  fed through a pipe, lines arrive CRLF-terminated. A '\r' saved inside a history
  entry is poison -- on recall it carriage-returns the cursor back over the prompt
  and garbles the line. We rewrite the file as raw bytes (no text decoding) so we
  don't accidentally re-introduce newline translation.

Português:
  Remove retornos de carro perdidos ('\r', byte 13) de um arquivo de histórico
  pré-existente antes do haskeline carregá-lo. Por que isto existe: no Windows, ou
  quando o REPL foi alimentado por um pipe, as linhas chegam terminadas em CRLF.
  Um '\r' salvo dentro de uma entrada de histórico é veneno -- ao recuperar, ele
  leva o cursor de volta por cima do prompt e embaralha a linha. Reescrevemos o
  arquivo como bytes crus (sem decodificação de texto) para não reintroduzir
  tradução de quebra de linha por acidente.
-}
sanitizeHistory :: FilePath -> IO ()
sanitizeHistory path = do
  exists <- doesFileExist path
  if not exists
    then return ()
    else do
      raw <- BS.readFile path
      let cleaned = BS.filter (/= 13) raw   -- 13 = '\r'
      if cleaned == raw then return () else BS.writeFile path cleaned

-- The startup banner. Pure output -- ASCII art plus a couple of hints.
-- O banner de inicialização. Saída pura -- arte ASCII mais algumas dicas.
printWelcome :: IO ()
printWelcome = do
  let purple = "\x1b[35m"
  let reset  = "\x1b[0m"

  putStrLn (purple ++ "______                  " ++ reset)
  putStrLn (purple ++ "| ___ \\                      The Bern Language Interpreter " ++ reset)
  putStrLn (purple ++ "| |_/ / ___ _ __ _ __       " ++ reset)
  putStrLn (purple ++ "| ___ \\/ _ \\ '__| '_ \\     Type \":help\" for REPL commands." ++ reset)
  putStrLn (purple ++ "| |_/ /  __/ |  | | | |      Use \"bern <file>\" to run a program." ++ reset)
  putStrLn (purple ++ "\\____/ \\___|_|  |_| |_|            [ v.2.0.0  05.06.2026 ]" ++ reset)

{-
English:
  The loop itself. One turn: read input, then decide what to do with it.
    * Nothing (end-of-input / Ctrl-D) -> say goodbye and stop.
    * empty line -> loop again with the same scope.
    * a line starting with ':' -> a meta-command (handled by handleMeta), which
      may quit or hand back a (possibly new) scope.
    * anything else -> run it as Bern source via runLine, then loop with the
      resulting scope.

  Notice how `table` is threaded: each branch loops with the scope it should
  continue from, which is what gives the REPL its memory.

Português:
  O laço em si. Uma rodada: ler a entrada, então decidir o que fazer com ela.
    * Nothing (fim da entrada / Ctrl-D) -> dar tchau e parar.
    * linha vazia -> repetir com o mesmo escopo.
    * uma linha começando com ':' -> um metacomando (tratado por handleMeta), que
      pode sair ou devolver um escopo (possivelmente novo).
    * qualquer outra coisa -> rodar como código Bern via runLine, e então repetir
      com o escopo resultante.

  Repare como `table` é passada adiante: cada ramo repete com o escopo de onde
  deve continuar, que é o que dá ao REPL a sua memória.
-}
repl :: Hashtable String Value -> InputT IO ()
repl table = do
  minput <- getMultiline ""
  case minput of
    Nothing -> outputStrLn "Goodbye!"
    Just input -> do
      let t = trim input
      if null t
        then repl table
        else if head t == ':'
          then do
            mTable <- handleMeta t table
            case mTable of
              Nothing      -> outputStrLn "Goodbye!"   -- :quit
              Just table'  -> repl table'
          else do
            table' <- liftIO (runLine input table)
            repl table'

{-
English:
  Collect a complete input, reading extra continuation lines while the buffer is
  still an unfinished block (e.g. a `def ... do` waiting for its `end`). The
  `acc` argument is the text gathered so far.

  Subtleties handled here:
    * The prompt is intentionally plain text (no ANSI colour). haskeline counts
      every prompt byte toward the line width, so colour codes corrupt cursor
      maths, backspace and history editing.
    * We strip '\r' from each incoming line immediately, and add the CLEANED line
      to history ourselves (autoAddHistory is off), keeping the history file free
      of control characters no matter how the REPL is driven.
    * A blank continuation line force-submits, so a genuinely broken block shows
      its error instead of looping forever waiting for an `end`.

Português:
  Junta uma entrada completa, lendo linhas de continuação extras enquanto o buffer
  ainda é um bloco inacabado (ex.: um `def ... do` esperando seu `end`). O
  argumento `acc` é o texto reunido até agora.

  Sutilezas tratadas aqui:
    * O prompt é texto puro de propósito (sem cor ANSI). O haskeline conta cada
      byte do prompt na largura da linha, então códigos de cor corrompem a conta do
      cursor, o backspace e a edição de histórico.
    * Retiramos '\r' de cada linha que chega na hora, e adicionamos a linha JÁ
      LIMPA ao histórico nós mesmos (autoAddHistory está desligado), mantendo o
      arquivo de histórico livre de caracteres de controle não importa como o REPL
      seja conduzido.
    * Uma linha de continuação em branco força o envio, para que um bloco
      genuinamente quebrado mostre seu erro em vez de ficar em loop para sempre
      esperando um `end`.
-}
getMultiline :: String -> InputT IO (Maybe String)
getMultiline acc = do
  ml <- getInputLine (if null acc then "[bern]: " else "    ...: ")
  case ml of
    Nothing -> return (if null acc then Nothing else Just acc)
    Just raw -> do
      let l  = filter (/= '\r') raw
          lt = trim l
      if not (null lt) then modifyHistory (addHistoryUnlessConsecutiveDupe l) else return ()
      if null acc && not (null lt) && head lt == ':'
        then return (Just l)                              -- meta command (single line)
        else do
          let buf = if null acc then l else acc ++ "\n" ++ l
          if null (trim buf)
            then return (Just "")
            else if not (null acc) && null lt
              then return (Just buf)                      -- blank line: force submit
              else if blockDepth buf > 0
                then getMultiline buf                     -- inside an unclosed do/if block
                else return (Just buf)

{-
English:
  A cheap heuristic for "are we still inside an open block?". It counts the words
  `do` and `if` (each opens a block that an `end` closes) and subtracts the `end`s.
  A positive result means the block is unbalanced, so the REPL should keep reading.
  It is only a heuristic -- it tokenises by turning every non-identifier character
  into a space, and ignores `case`'s optional trailing `end` (rarely typed across
  lines) -- but it is good enough to know when a `def ... do ... end` is finished.

Português:
  Uma heurística barata para "ainda estamos dentro de um bloco aberto?". Ela conta
  as palavras `do` e `if` (cada uma abre um bloco que um `end` fecha) e subtrai os
  `end`s. Um resultado positivo significa que o bloco está desbalanceado, então o
  REPL deve continuar lendo. É só uma heurística -- tokeniza transformando todo
  caractere não-identificador em espaço, e ignora o `end` final opcional do `case`
  (raramente digitado em várias linhas) -- mas é boa o bastante para saber quando
  um `def ... do ... end` terminou.
-}
blockDepth :: String -> Int
blockDepth s =
  let ws = words (map (\c -> if isAlphaNum c || c == '_' then c else ' ') s)
      cnt w = length (filter (== w) ws)
  in cnt "do" + cnt "if" - cnt "end"

{-
English:
  Parse and run one (possibly multi-line) input against the current scope, and
  return the updated scope. First applyPragmas turns on any {--! ... !--} the user
  typed and stripPragmas removes them from the text; if nothing but a pragma was
  typed, the cleaned text is empty and we just return the scope unchanged.

Português:
  Parseia e roda uma entrada (possivelmente de várias linhas) contra o escopo
  atual, e retorna o escopo atualizado. Primeiro applyPragmas liga qualquer
  {--! ... !--} que o usuário digitou e stripPragmas os remove do texto; se nada
  além de um pragma foi digitado, o texto limpo fica vazio e só retornamos o escopo
  inalterado.
-}
runLine :: String -> Hashtable String Value -> IO (Hashtable String Value)
runLine input table = do
  applyPragmas input
  let cleaned = stripPragmas input
  if null (trim cleaned)
    then return table
    else runParsed cleaned table

{-
English:
  Parse the cleaned input and run the resulting command, catching ALL exceptions
  so the loop survives.

  Haskell behaviour worth noting: `interpretCommand` can call `die` (which throws
  an ExitCode exception) for fatal errors. In a file run that would end the
  process, which is correct; in the REPL we instead `try` it and inspect the
  caught exception with `fromException`. If it is an ExitCode, the error message
  was already printed by `die`, so we just return the unchanged scope. Any other
  exception is shown via formatErrorVal. Either way the prompt comes back.

Português:
  Parseia a entrada limpa e roda o comando resultante, capturando TODAS as
  exceções para que o laço sobreviva.

  Comportamento de Haskell que vale notar: `interpretCommand` pode chamar `die`
  (que lança uma exceção ExitCode) para erros fatais. Numa execução de arquivo
  isso encerraria o processo, o que é correto; no REPL, em vez disso, fazemos
  `try` e inspecionamos a exceção capturada com `fromException`. Se for um
  ExitCode, a mensagem de erro já foi impressa pelo `die`, então só retornamos o
  escopo inalterado. Qualquer outra exceção é mostrada via formatErrorVal. De
  qualquer forma, o prompt volta.
-}
runParsed :: String -> Hashtable String Value -> IO (Hashtable String Value)
runParsed input table =
  case parseBern input of
    Left err  -> putStrLn (formatParseError err) >> return table
    Right cmd -> do
      r <- try (runCmd cmd table) :: IO (Either SomeException (Hashtable String Value))
      case r of
        Right table' -> return table'
        Left e       -> case fromException e of
          Just (_ :: ExitCode) -> return table       -- die already printed the error
          Nothing              -> putStrLn (formatErrorVal (show e)) >> return table

{-
English:
  Run one parsed command. The REPL handles bare expressions specially: a `Print`
  or `AutoPrint` is evaluated, shown, and its result bound to `_` (printAndBind),
  rather than going through the normal interpreter path -- that is what lets the
  REPL echo results and remember the last one. Everything else (assignments,
  definitions, loops) goes straight to interpretCommand.

Português:
  Roda um comando já parseado. O REPL trata expressões soltas de forma especial:
  um `Print` ou `AutoPrint` é avaliado, mostrado, e seu resultado vinculado a `_`
  (printAndBind), em vez de passar pelo caminho normal do interpretador -- é isso
  que permite ao REPL ecoar resultados e lembrar o último. Todo o resto
  (atribuições, definições, laços) vai direto para interpretCommand.
-}
runCmd :: Command -> Hashtable String Value -> IO (Hashtable String Value)
runCmd (Print expr) table     = printAndBind False expr table
runCmd (AutoPrint expr) table = printAndBind True expr table
runCmd cmd table = interpretCommand Nothing cmd table

{-
English:
  Evaluate an expression, print it, and bind its result to `_` in the scope.
  `isAuto` distinguishes a bare expression (which honours the no-eval and
  show-types pragmas) from an explicit `print`. The cases:
    * Undefined -> print nothing (e.g. an assignment-like expression with no value);
    * ErrorVal  -> show the error nicely, but still bind it to `_` so it can be
      inspected;
    * a real value -> print it (with its type appended under show-types) and bind it;
    * a hard Left error -> show it and leave the scope unchanged.

Português:
  Avalia uma expressão, a imprime, e vincula seu resultado a `_` no escopo.
  `isAuto` distingue uma expressão solta (que respeita os pragmas no-eval e
  show-types) de um `print` explícito. Os casos:
    * Undefined -> não imprime nada (ex.: uma expressão tipo atribuição sem valor);
    * ErrorVal  -> mostra o erro bonito, mas ainda o vincula a `_` para que possa
      ser inspecionado;
    * um valor de verdade -> imprime (com o tipo anexado sob show-types) e vincula;
    * um erro Left fatal -> mostra e deixa o escopo inalterado.
-}
printAndBind :: Bool -> Expression -> Hashtable String Value -> IO (Hashtable String Value)
printAndBind isAuto expr table =
  case evaluate expr table of
    Right Undefined  -> return table
    Right (ErrorVal m) -> do
      putStrLn (formatErrorVal m)
      return (insertHashtable table "_" (ErrorVal m))
    Right val       -> do
      if isAuto && pragmaOn "no-eval" ()
        then return ()
        else putStrLn (if isAuto && pragmaOn "show-types" ()
                         then prettyValue val ++ " : " ++ getValueType val
                         else prettyValue val)
      return (insertHashtable table "_" val)
    Left err        -> putStrLn (formatError err) >> return table

{-
English:
  The `:`-prefixed meta-commands. Returns `Nothing` to quit the REPL, or
  `Just newTable` to continue with a (possibly changed) scope. Supported:
    :quit/:q          stop
    :help/:h          show command help
    :reset            forget all local definitions (start from an empty scope)
    :load/:l <file>   load and run a .brn file INTO the current session
    :pragma <name>    enable a pragma for the session
    :pragmas          list the active pragmas (and the known ones if none)
  Anything else is reported as unknown.

Português:
  Os metacomandos prefixados por `:`. Retorna `Nothing` para sair do REPL, ou
  `Just novaTabela` para continuar com um escopo (possivelmente alterado).
  Suportados:
    :quit/:q          parar
    :help/:h          mostrar a ajuda dos comandos
    :reset            esquecer todas as definições locais (recomeçar com escopo vazio)
    :load/:l <arq>    carregar e rodar um arquivo .brn DENTRO da sessão atual
    :pragma <nome>    ativar um pragma para a sessão
    :pragmas          listar os pragmas ativos (e os conhecidos, se nenhum)
  Qualquer outra coisa é reportada como desconhecida.
-}
handleMeta :: String -> Hashtable String Value -> InputT IO (Maybe (Hashtable String Value))
handleMeta input table =
  case words input of
    (":quit":_)  -> return Nothing
    (":q":_)     -> return Nothing
    (":help":_)  -> printReplHelp >> return (Just table)
    (":h":_)     -> printReplHelp >> return (Just table)
    (":reset":_) -> outputStrLn "Local definitions cleared." >> return (Just emptyHashtable)
    (":load":path:_) -> do t <- liftIO (loadFile path table); return (Just t)
    (":l":path:_)    -> do t <- liftIO (loadFile path table); return (Just t)
    (":load":_)  -> outputStrLn "Usage: :load <file.brn>" >> return (Just table)
    (":l":_)     -> outputStrLn "Usage: :load <file.brn>" >> return (Just table)
    (":pragma":name:_)
      | name `elem` knownPragmas -> do
          liftIO (setPragma name)
          outputStrLn ("Pragma '" ++ name ++ "' enabled.")
          return (Just table)
      | otherwise -> outputStrLn ("Unknown pragma: " ++ name ++ "  (try :pragmas)") >> return (Just table)
    (":pragma":_)  -> outputStrLn "Usage: :pragma <name>" >> return (Just table)
    (":pragmas":_) -> do
      let active = currentPragmas ()
      outputStrLn (if null active
                     then "No pragmas active. Known: " ++ unwords knownPragmas
                     else "Active: " ++ unwords active)
      return (Just table)
    _            -> outputStrLn ("Unknown command: " ++ input ++ "  (try :help)") >> return (Just table)

-- The text printed by :help. Pure output.
-- O texto impresso por :help. Saída pura.
printReplHelp :: InputT IO ()
printReplHelp = mapM_ outputStrLn
  [ "REPL commands:"
  , "  :help, :h         Show this help"
  , "  :load <file>, :l  Load and run a .brn file into the current session"
  , "  :reset            Clear local definitions"
  , "  :pragma <name>    Enable a pragma for the session"
  , "  :pragmas          List the active pragmas"
  , "  :quit, :q         Exit the REPL (or press Ctrl-D)"
  , ""
  , "Notes:"
  , "  - The result of the last expression is bound to _"
  , "  - Multi-line blocks (def ... do ... end) are read until complete"
  , "  - Pragmas also work inline, e.g.  {--! no-written-operators !--}"
  , "  - Use the up/down arrows for history"
  ]

{-
English:
  Implements `:load`. Read a file, parse it, and run it against the CURRENT scope
  so its top-level definitions are added to the session (unlike runFile, which
  starts from empty and exits on error). The same try/fromException dance as
  runParsed keeps a `die` inside the loaded file from killing the REPL.

Português:
  Implementa o `:load`. Lê um arquivo, o parseia e o roda contra o escopo ATUAL
  para que suas definições de topo sejam adicionadas à sessão (diferente do
  runFile, que começa do vazio e sai em erro). A mesma dança de try/fromException
  do runParsed impede que um `die` dentro do arquivo carregado mate o REPL.
-}
loadFile :: FilePath -> Hashtable String Value -> IO (Hashtable String Value)
loadFile path table = do
  exists <- doesFileExist path
  if not exists
    then putStrLn ("File not found: " ++ path) >> return table
    else do
      contents <- readFile path
      case parseBernFile path contents of
        Left err  -> putStrLn (formatParseError err) >> return table
        Right cmd -> do
          r <- try (interpretCommand Nothing cmd table) :: IO (Either SomeException (Hashtable String Value))
          case r of
            Right table' -> putStrLn ("Loaded " ++ path) >> return table'
            Left e       -> case fromException e of
              Just (_ :: ExitCode) -> return table
              Nothing              -> putStrLn ("Error: " ++ show e) >> return table

{-
English:
  Trim leading and trailing whitespace. `f = reverse . dropWhile isSpace` drops the
  leading run; doing `f . f` applies it once, reverses, applies it again (now the
  other end), and reverses back -- a tidy way to trim both ends with one helper.

Português:
  Remove espaços em branco do início e do fim. `f = reverse . dropWhile isSpace`
  remove o trecho do início; fazer `f . f` aplica uma vez, inverte, aplica de novo
  (agora a outra ponta) e inverte de volta -- um jeito enxuto de aparar as duas
  pontas com um único auxiliar.
-}
trim :: String -> String
trim = f . f where f = reverse . dropWhile (`elem` " \t\r\n")
