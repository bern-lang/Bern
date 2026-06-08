{-
Module: Language.Eval

English:
  The heart of the interpreter, and by far the biggest module. The parser produced
  an AST; THIS file gives that AST meaning -- it actually runs the program. If you
  want to know what any Bern construct really does, the answer is here.

  Two functions are the spine of everything:
    * interpretCommand -- runs a statement (Command) and returns the updated scope.
      It is what handles assignments, function/ADT definitions, loops, conditionals,
      imports, input, and so on. Each statement takes a scope (a Hashtable) and
      yields a new one, threaded through `Concat`, so a program is just one Command
      folded over the starting scope.
    * evaluate / evaluateRaw -- compute the VALUE of an Expression. evaluateRaw has
      one equation per Expression constructor; evaluate wraps it to implement the
      "errors as values" model.

  Cross-cutting ideas implemented here, each with its own little section below:
    * Scope. A local Hashtable is the normal scope; a separate MVar (globalScopeMVar)
      holds the `:=` global scope that survives across function calls.
    * Errors as values (default). A runtime failure does NOT crash; evaluate turns a
      Left error into a first-class `ErrorVal` that propagates through expressions
      and can be tested with is_error. The `abort-on-error` pragma restores crashing.
    * Pragmas are FILE-SCOPED. A function value is tagged with the pragma set of the
      file that defined it, and `withPragmas` installs that set for the duration of a
      call -- so a library behaves under its own pragmas, not the caller's.
    * Pattern matching (matchOne) decides which function clause runs and binds names.
    * Imports load another .brn file as a module, with qualified-name and conflict
      handling so two libraries can export the same name.
    * checkExhaustive is the load-time check that a function's clauses cover all
      inputs.

  Haskell note: most pure logic returns `Either EvalError Value` -- `Right` is
  success, `Left` is a failure carrying a message and position. The `do` notation
  over Either short-circuits on the first `Left`, which is how an error skips the
  rest of a computation. IO appears only where the real world is touched (printing,
  reading files, calling C, the global MVar).

Português:
  O coração do interpretador, e de longe o maior módulo. O parser produziu uma AST;
  ESTE arquivo dá significado a essa AST -- ele de fato roda o programa. Se você quer
  saber o que qualquer construção de Bern realmente faz, a resposta está aqui.

  Duas funções são a espinha de tudo:
    * interpretCommand -- roda um comando (Command) e retorna o escopo atualizado. É
      o que trata atribuições, definições de função/ADT, laços, condicionais,
      imports, input, etc. Cada comando recebe um escopo (uma Hashtable) e entrega um
      novo, passado adiante por `Concat`, então um programa é só um Command dobrado
      sobre o escopo inicial.
    * evaluate / evaluateRaw -- calculam o VALOR de uma Expression. evaluateRaw tem
      uma equação por construtor de Expression; evaluate o envolve para implementar o
      modelo de "erros como valores".

  Ideias transversais implementadas aqui, cada uma com sua pequena seção abaixo:
    * Escopo. Uma Hashtable local é o escopo normal; um MVar separado
      (globalScopeMVar) guarda o escopo global do `:=` que sobrevive entre chamadas de
      função.
    * Erros como valores (padrão). Uma falha de execução NÃO quebra; evaluate
      transforma um erro Left em um `ErrorVal` de primeira classe que se propaga pelas
      expressões e pode ser testado com is_error. O pragma `abort-on-error` restaura o
      comportamento de quebrar.
    * Pragmas têm ESCOPO DE ARQUIVO. Um valor de função é etiquetado com o conjunto de
      pragmas do arquivo que o definiu, e `withPragmas` instala esse conjunto pela
      duração de uma chamada -- então uma biblioteca se comporta sob os seus próprios
      pragmas, não os do chamador.
    * O casamento de padrões (matchOne) decide qual cláusula de função roda e vincula
      nomes.
    * Imports carregam outro arquivo .brn como módulo, com tratamento de nome
      qualificado e de conflito para que duas bibliotecas possam exportar o mesmo nome.
    * checkExhaustive é a checagem em tempo de carga de que as cláusulas de uma função
      cobrem todas as entradas.

  Nota de Haskell: a maior parte da lógica pura retorna `Either EvalError Value` --
  `Right` é sucesso, `Left` é uma falha carregando uma mensagem e posição. A notação
  `do` sobre Either curto-circuita no primeiro `Left`, que é como um erro pula o resto
  de uma computação. IO aparece só onde o mundo real é tocado (imprimir, ler arquivos,
  chamar C, o MVar global).
-}
module Language.Eval where
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (doesFileExist, getCurrentDirectory)
import Data.Maybe (isJust)
import Debug.Trace (trace)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath (takeDirectory, (</>))
import Language.Ast
import Data.Hashtable.Hashtable
import Language.Helpers
import System.Info (os)
import Parsing.Parser (parseBernFile, scanOperators, scanImports)
import Language.PragmaState (pragmaSetRef)
import Language.OperatorState (addOperatorSpecs)
import Text.Megaparsec (errorBundlePretty, sourcePosPretty, SourcePos, ParseErrorBundle, bundleErrors, bundlePosState, errorOffset, parseErrorTextPretty, PosState(pstateSourcePos))
import Text.Megaparsec.Stream (reachOffset)
import Text.Megaparsec.Pos (sourceName, sourceLine, sourceColumn, unPos)
import Data.Void (Void)
import qualified Data.List.NonEmpty as NE
import Data.List (isInfixOf, nub, isPrefixOf, intercalate, sort)
import qualified Language.FFI as FFI
import qualified Control.Exception as E
import Control.Concurrent.MVar
import Data.IORef
import System.Mem.StableName

{-
English:
  Bern has two scopes. The ordinary one is the local Hashtable threaded through
  every function as an argument -- bindings made with `=` live there and disappear
  when the function returns. The SECOND scope is this one: a single process-wide
  cell holding the GLOBAL scope, which `:=` writes to. A global binding survives
  across function-call boundaries (a local table does not), which is why a few
  things need it.

  It is an MVar (a thread-safe mutable box) created once via the unsafePerformIO +
  NOINLINE idiom, exactly like the pragma cell -- one shared box for the whole run.
  Code reads it with takeMVar/readMVar and writes with putMVar.

Português:
  Bern tem dois escopos. O comum é a Hashtable local passada por argumento a toda
  função -- vínculos feitos com `=` moram lá e somem quando a função retorna. O
  SEGUNDO escopo é este: uma única célula de processo guardando o escopo GLOBAL, no
  qual o `:=` escreve. Um vínculo global sobrevive entre fronteiras de chamada de
  função (uma tabela local não), e é por isso que algumas coisas precisam dele.

  É um MVar (uma caixa mutável segura entre threads) criada uma vez via o idioma
  unsafePerformIO + NOINLINE, exatamente como a célula de pragma -- uma caixa
  compartilhada para a execução inteira. O código a lê com takeMVar/readMVar e
  escreve com putMVar.
-}
-- Global mutable reference to the global scope using MVar
{-# NOINLINE globalScopeMVar #-}
globalScopeMVar :: MVar (Hashtable String Value)
globalScopeMVar = unsafePerformIO $ do
    newMVar emptyHashtable

-- ---------------------------------------------------------------------------
-- Pragmas
--
-- File-level directives written as `{--! name !--}` (a magic comment; the
-- parser already skips it as a `{- -}` block comment). They are picked up by a
-- pre-scan of the source (see `applyPragmas`) which records the enabled flags
-- in a process-global set, and the evaluator consults `pragmaOn` where the
-- relevant behaviour is gated.
--
-- Pragmas are FILE-SCOPED, not global: the `pragmaSetRef` always holds the
-- pragma set of the file whose code is *currently executing*. Every function
-- value is tagged at definition time with the pragma set of the file that
-- defined it (see `FunctionDef` / `LambdaExpr`), and `applyFunction` installs
-- that set for the duration of the call via `withPragmas`. So a library that
-- declares `{--! impure-lists !--}` gets impure lists inside its own code
-- without imposing it on the program that imports it, and vice-versa.
--
--   impure-lists     : lists may hold mixed element types
--   impure-sets      : set operations keep duplicates
--   strict-types     : disable implicit type coercion (e.g. string + number)
--   strict-arithmetic: division by zero is an error (instead of NaN)
--   immutable        : reassigning an existing variable is an error
--   no-eval          : do not auto-print top-level expressions
--   show-types       : auto-printed values include their type
--   safe-index       : out-of-bounds indexing returns undefined
--   no-undefined     : reading an unbound variable is an error
--   start-on-one     : indexing starts at 1 instead of 0
--   main             : after loading, call main()
-- ---------------------------------------------------------------------------

-- `pragmaSetRef` now lives in Language.PragmaState (a leaf module) so the
-- parser can consult it too; see the import at the top of this file.

-- The dummy argument plus NOINLINE keep this from being memoised as a constant,
-- so it reflects the current flag set at every call.
{-# NOINLINE pragmaOn #-}
pragmaOn :: String -> () -> Bool
pragmaOn name _ = unsafePerformIO (fmap (name `elem`) (readIORef pragmaSetRef))

-- The pragma set currently in effect (the executing file's). Used at value
-- construction time to tag a freshly-built closure with its origin pragmas.
{-# NOINLINE currentPragmas #-}
currentPragmas :: () -> [String]
currentPragmas _ = unsafePerformIO (readIORef pragmaSetRef)

-- The pragmas whose effect is decided lazily at value *construction* time
-- (rather than at point of use). When a call crosses into a file that enables
-- one of these, `withPragmas` must realise the result so the construction
-- decision is taken under the callee's pragmas, not the caller's.
constructionPragmas :: [String] -> [String]
constructionPragmas = filter (`elem` ["impure-lists", "impure-sets"])

-- Run a function body with `prags` (the callee file's pragma set) active, then
-- restore the caller's set. The common case -- caller and callee share the same
-- pragma context -- takes a zero-cost lazy fast path that preserves the original
-- semantics exactly. Only when crossing into a file that enables a construction
-- pragma do we force the result (so lazily-built heterogeneous lists/sets commit
-- to the callee's rule); otherwise we force only to WHNF so the body runs under
-- the right pragmas while lazy results (e.g. ranges) stay lazy.
{-# NOINLINE withPragmas #-}
withPragmas :: [String] -> Either EvalError Value -> Either EvalError Value
withPragmas prags act = unsafePerformIO $ do
    saved <- readIORef pragmaSetRef
    if samePragmas saved prags
        then return act                                  -- same file context: stay fully lazy
        else do
            writeIORef pragmaSetRef prags
            r <- if null (constructionPragmas prags)
                    then E.evaluate (forceWhnf act)      -- run body under callee pragmas
                    else E.evaluate (forceDeep act)      -- + commit lazy construction decisions
            writeIORef pragmaSetRef saved
            return r
  where
    samePragmas a b = sort a == sort b
    forceWhnf x = case x of { Left _ -> x; Right v -> v `seq` x }
    forceDeep x = case x of { Left _ -> x; Right v -> deepSeqValue v `seq` x }

-- Force a value to normal form, skipping function bodies. Used by `withPragmas`
-- to settle pragma-gated construction before the callee's pragmas are unwound.
-- NB: this realises a lazy range if one is returned across such a boundary --
-- the one place that can happen is a file that itself declares impure-lists/-sets.
deepSeqValue :: Value -> ()
deepSeqValue v = case v of
    Integer n              -> n `seq` ()
    Double d               -> d `seq` ()
    Boolean b              -> b `seq` ()
    Character c            -> c `seq` ()
    TextLiteral s n        -> s `seq` n `seq` ()
    ErrorVal m             -> m `seq` ()
    List xs n              -> n `seq` seqList xs
    Set xs n               -> n `seq` seqList xs
    Object kvs             -> seqList (map snd kvs)
    AlgebraicDataType _ xs -> seqList xs
    _                      -> ()   -- Function, Lambda, CBinding, NaN, Undefined
  where
    seqList = foldr (\x acc -> deepSeqValue x `seq` acc) ()

-- Suppresses the import-time pragma note when `--silent-pragmas` is passed.
{-# NOINLINE silentPragmasRef #-}
silentPragmasRef :: IORef Bool
silentPragmasRef = unsafePerformIO (newIORef False)

setSilentPragmas :: Bool -> IO ()
setSilentPragmas = writeIORef silentPragmasRef

-- Print a note when an imported module declares pragmas. They are file-scoped,
-- so the module's pragmas only affect the module's own code -- but a reader of
-- the importing program can't see the module's source, so we surface it.
warnImportPragmas :: String -> [String] -> IO ()
warnImportPragmas _ [] = return ()
warnImportPragmas moduleName prags = do
    silent <- readIORef silentPragmasRef
    if silent
        then return ()
        else putStrLn ("note: module '" ++ moduleName ++ "' enables pragma(s): "
                       ++ intercalate ", " prags ++ " (scoped to that module)")

knownPragmas :: [String]
knownPragmas =
    [ "impure-lists", "impure-sets", "strict-types", "strict-arithmetic"
    , "immutable", "no-eval", "show-types", "safe-index", "no-undefined"
    , "start-on-one", "main", "strict-imports", "no-curry", "abort-on-error"
    , "partial", "no-written-operators", "typed" ]

-- Structural keywords: always part of Bern's syntax, so they can never be used
-- as a variable name.
keywordNames :: [String]
keywordNames =
    [ "def", "do", "end", "if", "then", "else", "for", "loop", "in", "case"
    , "is", "when", "return", "returns", "lambda", "import", "as", "adt"
    , "iterative", "true", "false", "input", "foreign", "print" ]

-- The written-word operators (plus, length, and, ...). The parser treats these
-- as operators, so a variable named after one can't be read back. They are
-- therefore reserved -- UNLESS the `no-written-operators` pragma is on, which
-- switches the word operators off and frees these names for use as identifiers.
writtenOperatorNames :: [String]
writtenOperatorNames =
    [ "plus", "minus", "times", "modulo", "negate", "not", "and", "or"
    , "concat", "union", "intersect", "difference", "equals", "typeof"
    , "length", "be" ]

isReservedName :: String -> Bool
isReservedName name =
    name `elem` keywordNames
    || (name `elem` writtenOperatorNames && not (pragmaOn "no-written-operators" ()))

-- The error shown when an assignment targets a reserved name. The hint depends
-- on which kind of name it is.
reservedAssignError :: Maybe SourcePos -> String -> EvalError
reservedAssignError mpos name
    | name `elem` writtenOperatorNames = mkEvalErrorHint mpos
        ("cannot assign to '" ++ name ++ "': it is a written-word operator")
        "rename the variable, or add the {--! no-written-operators !--} pragma to free the written-operator names"
    | otherwise = mkEvalErrorHint mpos
        ("cannot assign to '" ++ name ++ "': it is a reserved keyword")
        "rename the variable -- this name is part of Bern's syntax"

-- Typed bindings (the `typed` pragma) ----------------------------------------
-- A declared type name is matched against a value's runtime type. We accept the
-- canonical names reported by `getValueType` (Integer, Double, Character,
-- Boolean, ...) as well as a few familiar aliases (Int, Float, Char, Bool), and
-- treat String/Text as "a list whose elements are all characters" -- which is
-- how Bern represents text. Any other name is taken literally, so an ADT type
-- (e.g. `Shape`) is matched against the value's own constructor type.

-- Fold an alias onto the canonical type name used by getValueType.
canonicalTypeName :: String -> String
canonicalTypeName ty = case ty of
    "Int"     -> "Integer"
    "Float"   -> "Double"
    "Char"    -> "Character"
    "Bool"    -> "Boolean"
    "Str"     -> "String"
    other     -> other

-- Does `val` satisfy the declared type `ty`? This covers scalars, collections,
-- and String/Text. ADT types need the table (a value only knows its constructor,
-- not its type), so they are handled separately by `adtValueMatchesType`.
valueMatchesType :: String -> Value -> Bool
valueMatchesType ty val =
    case canonicalTypeName ty of
        "Auto"   -> True              -- dynamic: accept whatever type was passed
        "Any"    -> True              -- synonym for Auto
        "String" -> isTextValue val
        "Text"   -> isTextValue val
        expected -> getValueType val == expected
  where
    isTextValue (List vs _) = all isChar vs
    isTextValue _           = False
    isChar (Character _)    = True
    isChar _                = False

-- Table key recording which ADT type a constructor belongs to (registered when
-- the ADT is defined, see AlgebraicTypeDef). Used to match a declared ADT type
-- name (e.g. `Shape`) against a value built from one of its constructors.
adtCtorTypeKey :: String -> String
adtCtorTypeKey ctorName = "__bern_adt_ctor_type_" ++ ctorName

-- An ADT value carries its constructor name (e.g. "Circle"); resolve that to its
-- declaring type and check it against the declared type name (e.g. "Shape").
adtValueMatchesType :: Hashtable String Value -> String -> Value -> Bool
adtValueMatchesType tbl ty (AlgebraicDataType ctorName _) =
    case lookupHashtable tbl (adtCtorTypeKey ctorName) of
        Just tv -> valueToString tv == Just ty
        Nothing -> False
adtValueMatchesType _ _ _ = False

-- | Scan a source string for `{--! name !--}` pragmas and apply each one.
applyPragmas :: String -> IO ()
applyPragmas src = mapM_ setPragma (scanPragmas src)

setPragma :: String -> IO ()
setPragma name
    | name `elem` knownPragmas =
        modifyIORef' pragmaSetRef (\xs -> if name `elem` xs then xs else name : xs)
    | otherwise = putStrLn ("Warning: unknown pragma '" ++ name ++ "'")

-- | Extract the names from every `{--! name !--}` occurrence in the source.
scanPragmas :: String -> [String]
scanPragmas [] = []
scanPragmas s
    | "{--!" `isPrefixOf` s =
        let (body, rest) = breakOnSub "!--}" (drop 4 s)
        in unwords (words body) : scanPragmas rest
    | otherwise = scanPragmas (drop 1 s)

-- | Remove every `{--! ... !--}` pragma from a source string (used by the REPL
-- so a pragma-only line is not re-parsed once its flag has been applied).
stripPragmas :: String -> String
stripPragmas [] = []
stripPragmas s
    | "{--!" `isPrefixOf` s = stripPragmas (snd (breakOnSub "!--}" (drop 4 s)))
    | otherwise = head s : stripPragmas (tail s)

-- Split a string at the first occurrence of `pat`, dropping `pat`.
breakOnSub :: String -> String -> (String, String)
breakOnSub pat = go []
  where
    go acc [] = (reverse acc, [])
    go acc str@(c:cs)
        | pat `isPrefixOf` str = (reverse acc, drop (length pat) str)
        | otherwise            = go (c : acc) cs

-- | Is `name` already bound in the local table or the global scope?
isBound :: String -> Hashtable String Value -> Bool
isBound name table =
    case lookupHashtable table name of
        Just _  -> True
        Nothing -> unsafePerformIO $ do
            g <- readMVar globalScopeMVar
            return (isJust (lookupHashtable g name))

-- ---------------------------------------------------------------------------
-- exhaustive-match (default)
--
-- Report single-argument functions whose clauses cannot cover every input.
-- The check is sound (it only flags a function when it is certain the clauses
-- are partial), so multi-argument functions are left alone and guarded clauses
-- are not counted towards coverage. Disabled by the `partial` pragma.
-- ---------------------------------------------------------------------------
checkExhaustive :: Command -> [String]
checkExhaustive cmd
    | pragmaOn "partial" () = []
    | otherwise =
        [ "function '" ++ name ++ "' is not exhaustive: its clauses do not cover all inputs"
          ++ " (add a catch-all clause, or use the `partial` pragma)"
        | (name, clauses) <- collectFunctionClauses cmd
        , not (null clauses)
        , all isUnaryClause clauses
        , not (exhaustiveUnary adts clauses) ]
  where
    isUnaryClause (Clause ps _ _) = length ps == 1
    adts = collectAdtConstructors cmd

collectFunctionClauses :: Command -> [(String, [Clause])]
collectFunctionClauses cmd =
    let pairs = go cmd
        names = nub (map fst pairs)
    in [ (n, [ c | (n', c) <- pairs, n' == n ]) | n <- names ]
  where
    go (Concat a b)          = go a ++ go b
    go (FunctionDef name cl) = [(name, cl)]
    go _                     = []

-- Collect every ADT declared in the command tree as (typeName, [constructor
-- names]). Used to decide whether a function that matches on an ADT covers all
-- of that type's constructors.
collectAdtConstructors :: Command -> [(String, [String])]
collectAdtConstructors = go
  where
    go (Concat a b) = go a ++ go b
    go (AlgebraicTypeDef (ADTDef _ tyName ctors)) =
        [(tyName, [ cn | ADTConstructor cn _ <- ctors ])]
    go _ = []

exhaustiveUnary :: [(String, [String])] -> [Clause] -> Bool
exhaustiveUnary adts clauses =
    let pats = [ p | Clause [p] Nothing _ <- clauses ]  -- only unguarded clauses cover
    in any irrefutable pats
       || (PList [] `elem` pats && any isCons pats)
       || (PBool True `elem` pats && PBool False `elem` pats)
       || (PSet [] `elem` pats && any isSetCons pats)
       || adtComplete pats
  where
    irrefutable PWildcard    = True
    irrefutable (PVar _)     = True
    irrefutable _            = False
    isCons (PCons _ _)       = True
    isCons _                 = False
    isSetCons (PSetCons _ _) = True
    isSetCons _              = False
    -- Like Haskell: a match on an ADT is exhaustive when every clause is an
    -- ADT-constructor pattern and, together, they cover every constructor of
    -- that type. (Covering all the shapes of one kind counts as total, just as
    -- []+[h|t] does for lists.)
    adtComplete ps = case mapM ctorName ps of
        Just (c : cs) -> case typeOfCtor c of
            Just allCtors -> all (`elem` (c : cs)) allCtors
            Nothing       -> False
        _ -> False
    ctorName (PADT c _) = Just c
    ctorName _          = Nothing
    typeOfCtor c = case [ ctors | (_, ctors) <- adts, c `elem` ctors ] of
        (ctors : _) -> Just ctors
        []          -> Nothing

-- | Error type for better error messages
data EvalError = EvalError
    { errorPos :: Maybe SourcePos
    , errorMsg :: String
    , errorHint :: Maybe String
    }

-- | Format error message
formatError :: EvalError -> String
formatError (EvalError mpos msg mhint) =
    let redBold = "\x1b[1;31m"
        red = "\x1b[31m"
        yellow = "\x1b[33m"
        cyan = "\x1b[36m"
        magenta = "\x1b[35m"
        dim = "\x1b[2m"
        reset = "\x1b[0m"
        hintStr = case mhint of
                    Just h -> "\n\n" ++ yellow ++ "Hint: " ++ reset ++ h
                    Nothing -> ""
    in case mpos of
        Just pos ->
            let fname = sourceName pos
                ln = show (unPos (sourceLine pos))
                col = show (unPos (sourceColumn pos))
                header = redBold ++ "[bern]" ++ redBold ++ " An error was found while executing the script: " ++ reset ++ cyan ++ fname ++ reset
                body = "Error" ++ reset ++ " at " ++ magenta ++ ln ++ ":" ++ col ++ reset ++ ": " ++ red ++ msg ++ reset
            in header ++ "\n" ++ body ++ hintStr
        Nothing ->
            redBold ++ "Error: " ++ reset ++ red ++ msg ++ reset ++ hintStr

-- | Pretty-print a parse error in the same coloured house style as
-- 'formatError', rather than dumping megaparsec's raw caret report. Renders
-- a Bern header, the offending source line with a caret, and turns the
-- "expecting ..." list into a yellow hint.
formatParseError :: ParseErrorBundle String Void -> String
formatParseError bundle =
    let redBold = "\x1b[1;31m"
        red = "\x1b[31m"
        yellow = "\x1b[33m"
        cyan = "\x1b[36m"
        magenta = "\x1b[35m"
        dim = "\x1b[2m"
        reset = "\x1b[0m"
        err = NE.head (bundleErrors bundle)
        (mLine, pst') = reachOffset (errorOffset err) (bundlePosState bundle)
        pos = pstateSourcePos pst'
        fname = sourceName pos
        ln = unPos (sourceLine pos)
        col = unPos (sourceColumn pos)
        gutterNum = show ln
        gutterPad = map (const ' ') gutterNum
        srcLine = maybe "" id mLine
        caretLine = replicate (max 0 (col - 1)) ' ' ++ "^"
        header = redBold ++ "[bern]" ++ reset ++ red ++ " A syntax error was found: " ++ reset ++ cyan ++ fname ++ reset
        posLine = red ++ "Error" ++ reset ++ " at " ++ magenta ++ gutterNum ++ ":" ++ show col ++ reset
        srcRender = dim ++ " " ++ gutterPad ++ " |" ++ reset ++ "\n"
                 ++ dim ++ " " ++ gutterNum ++ " | " ++ reset ++ srcLine ++ "\n"
                 ++ dim ++ " " ++ gutterPad ++ " | " ++ reset ++ red ++ caretLine ++ reset
        fmtMsg l
          | "expecting" `isPrefixOf` l = yellow ++ "Hint: " ++ reset ++ l
          | otherwise                  = red ++ l ++ reset
        msgRender = intercalate "\n" (map fmtMsg (filter (not . null) (lines (parseErrorTextPretty err))))
    in header ++ "\n" ++ posLine ++ "\n" ++ srcRender ++ "\n\n" ++ msgRender

-- | Render a first-class error value (errors-as-values) as a coloured
-- traceback for the REPL: a bold red "Error:" label followed by the message,
-- instead of the bare @Error(...)@ value form used when nesting.
formatErrorVal :: String -> String
formatErrorVal m =
    let redBold = "\x1b[1;31m"
        red = "\x1b[31m"
        reset = "\x1b[0m"
    in redBold ++ "Error:" ++ reset ++ " " ++ red ++ m ++ reset

-- | Create simple EvalError
mkEvalError :: Maybe SourcePos -> String -> EvalError
mkEvalError mpos msg = EvalError mpos msg Nothing

-- | Create EvalError with hint
mkEvalErrorHint :: Maybe SourcePos -> String -> String -> EvalError
mkEvalErrorHint mpos msg hint = EvalError mpos msg (Just hint)

stringToValue :: String -> Value
stringToValue s = List (map Character s) (length s)

importOwnerKey :: String -> String
importOwnerKey name = "__bern_import_owner_" ++ name

importConflictKey :: String -> String
importConflictKey name = "__bern_import_conflict_" ++ name

importConflictHintKey :: String -> String
importConflictHintKey name = "__bern_import_conflict_hint_" ++ name

importModuleSymbolsKey :: String -> String
importModuleSymbolsKey moduleName = "__bern_import_symbols_" ++ moduleName

isMetaSymbolName :: String -> Bool
isMetaSymbolName name = "__" `isPrefixOf` name || ':' `elem` name

collectImportSymbols :: Command -> [String]
collectImportSymbols cmd = nub (filter (not . isMetaSymbolName) (go cmd))
  where
    go Skip = []
    go (Concat c1 c2) = go c1 ++ go c2
    go (FunctionDef name _) = [name]
    go (Assign name _) = [name]
    go (TypedAssign name _ _) = [name]
    go (GlobalAssign name _) = [name]
    go (AlgebraicTypeDef (ADTDef _ typeName constructors)) =
        typeName : [ctorName | ADTConstructor ctorName _ <- constructors]
    go (CForeignDecl name _ _ _) = [name]
    go _ = []

registerImportedSymbol :: String -> String -> Hashtable String Value -> Hashtable String Value
registerImportedSymbol moduleName symbol table =
    case lookupHashtable table (importOwnerKey symbol) of
        Nothing ->
            insertHashtable table (importOwnerKey symbol) (stringToValue moduleName)
        Just ownerVal ->
            case valueToString ownerVal of
                Just ownerModule
                    | ownerModule /= moduleName ->
                        let tableWithConflict = insertHashtable table (importConflictKey symbol) (Boolean True)
                            hint = "symbol '" ++ symbol ++ "' exists in both '" ++ ownerModule ++ "' and '" ++ moduleName ++ "'"
                        in insertHashtable tableWithConflict (importConflictHintKey symbol) (stringToValue hint)
                _ -> table

isAmbiguousImportedSymbol :: Hashtable String Value -> String -> Bool
isAmbiguousImportedSymbol table name =
    case lookupHashtable table (importConflictKey name) of
        Just (Boolean True) -> True
        _                   -> False

ambiguousImportedSymbolError :: Hashtable String Value -> String -> EvalError
ambiguousImportedSymbolError table name =
    let detail =
            case lookupHashtable table (importConflictHintKey name) of
                Just v ->
                    case valueToString v of
                        Just s  -> s ++ ". Use a qualified call like '<module>:" ++ name ++ "(...)'."
                        Nothing -> "Use a qualified call like '<module>:" ++ name ++ "(...)'."
                Nothing -> "Use a qualified call like '<module>:" ++ name ++ "(...)'."
    in mkEvalErrorHint Nothing ("ambiguous imported symbol '" ++ name ++ "'") detail

splitQualifiedName :: String -> Maybe (String, String)
splitQualifiedName name =
    case break (== ':') name of
        (moduleName, ':' : symbol)
            | not (null moduleName) && not (null symbol) -> Just (moduleName, symbol)
        _ -> Nothing

withQualifiedModuleScope :: Hashtable String Value -> String -> Hashtable String Value
withQualifiedModuleScope table moduleName =
    case lookupHashtable table (importModuleSymbolsKey moduleName) of
        Just (Object symbols) ->
            foldl
                (\tbl (symbol, _) ->
                    let maybeQualified = lookupHashtable table (moduleName ++ ":" ++ symbol)
                        withSymbol = case maybeQualified of
                            Just value -> insertHashtable tbl symbol value
                            Nothing    -> tbl
                    in insertHashtable withSymbol (importConflictKey symbol) (Boolean False))
                table
                symbols
        _ -> table

{-
English:
  THE STATEMENT INTERPRETER -- the longest function in the project. It runs one
  Command against the current scope and returns the updated scope. There is one
  equation per Command constructor, so reading it top to bottom is a tour of every
  statement Bern has.

  The shape to keep in mind:
    * Type is `... -> IO (Hashtable ...)`: it is in IO because statements can touch
      the world (print, read input, load a file, call C). Each returns the NEW scope,
      and `Concat a b` runs a, then runs b against a's result -- that threading is
      how later statements see earlier bindings.
    * The first argument `mpos` is the source position of the current statement, kept
      so runtime errors can point at the right line.
    * `Return` works by stashing the value under a special key "__return" in the
      scope; `Concat` checks `hasReturn` and stops early when it sees it, which is how
      `return` exits a `do` block.

  A few highlights you'll meet below: Assign/GlobalAssign (with the reserved-name and
  immutable checks), the loop family (Repeat/While/ForIn/ForInCount), FunctionDef
  (which tags the function value with the file's pragmas), AlgebraicTypeDef (which
  installs constructor functions), Import (load a module, with conflict handling), and
  CForeignDecl (declare a C function via the FFI).

Português:
  O INTERPRETADOR DE COMANDOS -- a função mais longa do projeto. Ela roda um Command
  contra o escopo atual e retorna o escopo atualizado. Há uma equação por construtor
  de Command, então ler de cima para baixo é um passeio por todo comando que o Bern
  tem.

  O formato a ter em mente:
    * O tipo é `... -> IO (Hashtable ...)`: está em IO porque comandos podem tocar o
      mundo (imprimir, ler input, carregar um arquivo, chamar C). Cada um retorna o
      NOVO escopo, e `Concat a b` roda a, depois roda b contra o resultado de a -- esse
      encadeamento é como comandos posteriores enxergam vínculos anteriores.
    * O primeiro argumento `mpos` é a posição no fonte do comando atual, guardada para
      que erros de execução apontem a linha certa.
    * `Return` funciona guardando o valor sob uma chave especial "__return" no escopo;
      `Concat` confere `hasReturn` e para mais cedo quando o vê, que é como o `return`
      sai de um bloco `do`.

  Alguns destaques que você encontrará abaixo: Assign/GlobalAssign (com as checagens de
  nome reservado e de imutabilidade), a família de laços (Repeat/While/ForIn/
  ForInCount), FunctionDef (que etiqueta o valor da função com os pragmas do arquivo),
  AlgebraicTypeDef (que instala as funções construtoras), Import (carregar um módulo,
  com tratamento de conflito), e CForeignDecl (declarar uma função C via FFI).
-}
interpretCommand :: Maybe SourcePos -> Command -> Hashtable String Value -> IO (Hashtable String Value)
interpretCommand mpos Skip table = return table

interpretCommand mpos (Input varName promptExpr) table = do
    promptVal <- case evaluate promptExpr table of
                    Right v -> case valueToString v of
                                  Just s  -> return s
                                  Nothing -> die (mkEvalErrorHint mpos 
                                      "expected String for prompt"
                                      "the prompt expression must evaluate to a string")
                    Left err -> die err
    putStr promptVal
    input <- getLine
    let newTable = insertHashtable table varName (List (map Character input) (length input))
    return newTable

interpretCommand mpos (WriteFile filePath contentExpr) table = do
    pathVal <- case evaluate filePath table of
                    Right v -> case valueToString v of
                                  Just s  -> return s
                                  Nothing -> die (mkEvalErrorHint mpos
                                      "expected String for file path"
                                      "file paths must be strings")
                    Left err -> die err
    contentVal <- case evaluate contentExpr table of
                    Right v -> case valueToString v of
                                  Just s  -> return s
                                  Nothing -> die (mkEvalErrorHint mpos
                                      "expected String for file content"
                                      "content to write must be a string")
                    Left err -> die err
    writeFile pathVal contentVal
    return table

-- Explicit print(...) always produces output.
interpretCommand mpos (Print expr) table =
    case evalValue expr table of
        Right Undefined -> return table
        Right val -> putStrLn (prettyValue val) >> return table
        Left err  -> die err

-- A bare top-level expression auto-prints, unless no-eval is set.
interpretCommand mpos (AutoPrint expr) table
    | pragmaOn "no-eval" () = return table
    | otherwise =
        case evalValue expr table of
            Right Undefined -> return table
            Right val
                | pragmaOn "show-types" () ->
                    putStrLn (prettyValue val ++ " : " ++ getValueType val) >> return table
                | otherwise -> putStrLn (prettyValue val) >> return table
            Left err  -> die err

interpretCommand mpos (Assign name expr) table
    | isReservedName name =
        die (reservedAssignError mpos name)
    | pragmaOn "immutable" () && isBound name table =
        die (mkEvalErrorHint mpos
            ("cannot reassign '" ++ name ++ "'")
            "the immutable pragma forbids reassigning an existing variable")
    | otherwise =
        case evalValue expr table of
            Right v -> return (insertHashtable table name v)
            Left err -> die err

-- A type-annotated binding (`name :: Type = expr`), enabled by the `typed`
-- pragma. It behaves exactly like Assign once the value is in hand, but first
-- checks that the value's runtime type matches the declared one; a mismatch is a
-- hard error (like a reserved-name assignment), so typed files fail loudly.
interpretCommand mpos (TypedAssign name tyName expr) table
    | isReservedName name =
        die (reservedAssignError mpos name)
    | pragmaOn "immutable" () && isBound name table =
        die (mkEvalErrorHint mpos
            ("cannot reassign '" ++ name ++ "'")
            "the immutable pragma forbids reassigning an existing variable")
    | otherwise =
        case evalValue expr table of
            Left err -> die err
            Right v
                | valueMatchesType tyName v
                    || adtValueMatchesType table tyName v -> return (insertHashtable table name v)
                | otherwise -> die (mkEvalErrorHint mpos
                    ("type mismatch for '" ++ name ++ "': declared " ++ tyName
                        ++ " but value is " ++ getValueType v)
                    ("make the value a " ++ canonicalTypeName tyName
                        ++ ", or change the declared type"))

interpretCommand mpos (GlobalAssign name expr) table
    | isReservedName name =
        die (reservedAssignError mpos name)
interpretCommand mpos (GlobalAssign name expr) table =
    case evalValue expr table of
        Right v@(Integer _)   -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(Double _)    -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(Boolean _)   -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(Character _) -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(List _ _)    -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(Set _ _)     -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(Object _)    -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(Function _ _)  -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(Lambda _ _)    -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(AlgebraicDataType _ _) -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(CBinding _ _) -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(NaN)          -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v               -> die (mkEvalErrorHint mpos
            ("cannot assign unexpected value to '" ++ name ++ "'")
            ("value type: " ++ show v))
        Left err              -> die err

interpretCommand mpos (AssignIndex name idxExprs expr) table = do
    baseVal <- case lookupHashtable table name of
                  Just v  -> return v
                  Nothing -> die (mkEvalErrorHint mpos
                      ("undefined variable '" ++ name ++ "'")
                      "variables must be defined before indexing")
    idxVals <- mapM (\ie -> case evaluate ie table of
                              Right (Integer n)      -> return (IdxInt n)
                              Right (Character c)    -> return (IdxKey [c])
                              Right l@(List _ _)     ->
                                  case valueToString l of
                                    Just s  -> return (IdxKey s)
                                    Nothing -> die (mkEvalError mpos
                                        "index must be Int or String")
                              Right _                -> die (mkEvalError mpos
                                        "index must be Int or String")
                              Left err               -> die err) idxExprs
    newVal <- case evaluate expr table of
                Right v  -> return v
                Left err -> die err
    case setAt idxVals newVal baseVal of
        Right updated -> return (insertHashtable table name updated)
        Left err      -> die err

interpretCommand mpos (Conditional cond thenCmd elseCmd) table =
    case evaluate cond table of
        Right (Boolean True)  -> interpretCommand mpos thenCmd table
        Right (Boolean False) -> interpretCommand mpos elseCmd table
        Right v               -> die (mkEvalErrorHint mpos
            "condition must be Bool"
            ("found: " ++ getValueType v))
        Left err              -> die err

interpretCommand mpos (Repeat count cmd) table =
    case evaluate count table of
        Right (Integer n) -> loop n table
        Right v           -> die (mkEvalErrorHint mpos
            "repeat count must be Int"
            ("found: " ++ getValueType v))
        Left err          -> die err
  where
    loop 0 t = return t
    loop k t = do
        t' <- interpretCommand mpos cmd t
        if hasReturn t'
            then return t'
            else loop (k-1) t'

interpretCommand mpos (While cond cmd) table =
    let loop t =
            case evaluate cond t of
                Right (Boolean True)  -> do
                    t' <- interpretCommand mpos cmd t
                    if hasReturn t'
                        then return t'
                        else loop t'
                Right (Boolean False) -> return t
                Right v               -> die (mkEvalErrorHint mpos
                    "while condition must be Bool"
                    ("found: " ++ getValueType v))
                Left err              -> die err
    in loop table

interpretCommand mpos (ForIn varName collection cmd) table =
    case evaluate collection table of
        Right coll ->
            case toIterable table coll of
                Right vals -> loop vals table
                Left err   -> die err
        Left err -> die err
  where
    loop [] t = return t
    loop (v:vs) t = do
        t' <- interpretCommand mpos cmd (insertHashtable t varName v)
        if hasReturn t'
            then return t'
            else loop vs t'

interpretCommand mpos (ForInCount varName idxName collection cmd) table =
    case evaluate collection table of
        Right coll ->
            case toIterable table coll of
                Right vals -> loop (zip vals [0..]) table
                Left err   -> die err
        Left err -> die err
  where
    loop [] t = return t
    loop ((v,idx):rest) t = do
        let tWith = insertHashtable (insertHashtable t varName v) idxName (Integer idx)
        t' <- interpretCommand mpos cmd tWith
        if hasReturn t'
            then return t'
            else loop rest t'

interpretCommand mpos (FunctionDef name clause) table = do
    prags <- readIORef pragmaSetRef   -- tag the closure with the defining file's pragmas
    let newFunc = case lookupHashtable table name of
                    Just (Function cs _) -> Function (cs ++ [clause]) prags
                    Just _               -> Function [clause] prags
                    Nothing              -> Function [clause] prags
    return (insertHashtable table name newFunc)

interpretCommand mpos (Return expr) table =
    case evalValue expr table of
        Right v -> return (insertHashtable table "__return" v)
        Left err -> die err

interpretCommand mpos (AlgebraicTypeDef (ADTDef isIterative typeName constructors)) table = do
    let adtVal = AlgebraicDataType typeName []
    let newTable = insertHashtable table typeName adtVal
    let tableWithCtors = foldl (\tbl (ADTConstructor ctorName _) ->
                                  insertHashtable tbl ctorName (Function [] [])) newTable constructors
    let tableWithIterableFlags = foldl (\tbl (ADTConstructor ctorName _) ->
                                          insertHashtable tbl (iterableCtorKey ctorName) (Boolean isIterative))
                                       tableWithCtors
                                       constructors
    -- Record which type each constructor belongs to, so the `typed` pragma can
    -- match a declared ADT type name (e.g. `Shape`) against a value built from
    -- one of its constructors (e.g. `Circle(1.0)`).
    let tableWithCtorTypes = foldl (\tbl (ADTConstructor ctorName _) ->
                                      insertHashtable tbl (adtCtorTypeKey ctorName) (stringToValue typeName))
                                   tableWithIterableFlags
                                   constructors

    case constructors of
        [ADTConstructor _ fieldTypes] -> do
            let fieldTypeNames = map typeToString fieldTypes
            let layout = createStructLayout typeName fieldTypeNames
            FFI.registerStruct layout
        _ -> return () 
    
    return tableWithCtorTypes
  where
    iterableCtorKey :: String -> String
    iterableCtorKey ctorName = "__bern_iterable_adt_ctor_" ++ ctorName

    typeToString :: Type -> String
    typeToString TInt = "Int"
    typeToString TDouble = "Double"
    typeToString TBool = "Bool"
    typeToString TChar = "Char"
    typeToString TString = "String"
    typeToString TList = "List"
    typeToString TSet = "Set"
    typeToString (TCustom "Byte") = "Byte"
    typeToString (TCustom "UChar") = "UChar"
    typeToString (TCustom "Float") = "Float"
    typeToString (TCustom "Short") = "Short"
    typeToString (TCustom "UShort") = "UShort"
    typeToString (TCustom name) = name
    typeToString TAuto = "Auto"
    
    createStructLayout :: String -> [String] -> FFI.StructLayout
    createStructLayout name types =
        let fields = createFields types 0
            totalSize = sum (map FFI.fieldSize fields)
        in FFI.StructLayout name totalSize fields
    
    createFields :: [String] -> Int -> [FFI.FieldDef]
    createFields [] _ = []
    createFields (t:ts) offset =
        let btype = typeStringToBernCType t
            size = FFI.getFieldSize btype
            field = FFI.FieldDef ("field" ++ show offset) btype offset size
        in field : createFields ts (offset + size)
    
    typeStringToBernCType :: String -> FFI.BernCType
    typeStringToBernCType "Int" = FFI.BernInt
    typeStringToBernCType "Double" = FFI.BernDouble
    typeStringToBernCType "Bool" = FFI.BernBool
    typeStringToBernCType "Char" = FFI.BernChar
    typeStringToBernCType "Byte" = FFI.BernByte
    typeStringToBernCType "UChar" = FFI.BernByte
    typeStringToBernCType "Float" = FFI.BernFloat
    typeStringToBernCType "Short" = FFI.BernShort
    typeStringToBernCType "UShort" = FFI.BernUShort
    typeStringToBernCType "Ptr" = FFI.BernPtr
    typeStringToBernCType "Pointer" = FFI.BernPtr
    typeStringToBernCType _ = FFI.BernVoid

interpretCommand mpos (Import moduleName mAlias) table = do
    execPath <- getExecutablePath
    let installDir = takeDirectory execPath
    let qualifier = case mAlias of
            Just alias -> alias
            Nothing    -> moduleName

    mPath <- resolveModulePath moduleName

    case mPath of
        Nothing -> die (mkEvalErrorHint mpos
            ("module '" ++ moduleName ++ "' not found")
            ("searched in: ./lib/, ./, and " ++ installDir </> "lib/"))
        Just path -> do
            contents <- readFile path
            addOperatorSpecs (scanOperators contents)  -- register the module's own operators before parsing it
            case parseBernFile path contents of
                Left err -> do
                    putStrLn $ errorBundlePretty err
                    die (mkEvalErrorHint mpos
                        ("failed to parse module '" ++ moduleName ++ "'")
                        "see parse errors above")
                Right cmd -> do
                    -- Pragmas are file-scoped: activate the module's own pragmas
                    -- while its top-level code runs (so its functions are tagged
                    -- with them and its body honours them), then restore the
                    -- importer's set. The module's pragmas never leak outward.
                    let libPragmas = nub (filter (`elem` knownPragmas) (scanPragmas contents))
                    warnImportPragmas moduleName libPragmas
                    savedPragmas <- readIORef pragmaSetRef
                    writeIORef pragmaSetRef libPragmas
                    case checkExhaustive cmd of
                        []   -> return ()
                        errs -> do
                            writeIORef pragmaSetRef savedPragmas
                            die (mkEvalErrorHint mpos
                                    ("module '" ++ moduleName ++ "' has non-exhaustive functions")
                                    (intercalate "\n  " errs))
                    let symbols = collectImportSymbols cmd
                    let importBaseTable =
                            foldl (\tbl symbol -> insertHashtable tbl symbol Undefined) table symbols
                    importedTable <- interpretCommand mpos cmd importBaseTable
                    writeIORef pragmaSetRef savedPragmas   -- restore the importer's pragmas
                    let tableWithQualified = foldl
                            (\tbl symbol ->
                                case lookupHashtable importedTable symbol of
                                    Just value -> insertHashtable tbl (qualifier ++ ":" ++ symbol) value
                                    Nothing    -> tbl)
                            importedTable
                            symbols
                    let tableWithImportMeta = foldl
                            (\tbl symbol -> registerImportedSymbol qualifier symbol tbl)
                            tableWithQualified
                            symbols
                    let tableWithModuleSymbols =
                            insertHashtable tableWithImportMeta
                                (importModuleSymbolsKey qualifier)
                                (Object [(symbol, Boolean True) | symbol <- symbols])
                    return tableWithModuleSymbols

interpretCommand mpos (Concat cmd1 cmd2) table = do
    table' <- interpretCommand mpos cmd1 table
    if hasReturn table'
        then return table'
        else interpretCommand mpos cmd2 table'

interpretCommand mpos (CForeignDecl name libPath argTypes retType) table = do    
    libPathVal <- case evaluate libPath table of
        Right v -> case valueToString v of
            Just s  -> return s
            Nothing -> die (mkEvalError mpos "library path must be String")
        Left err -> die err

    result <- tryLoadFromLibs [libPathVal] name argTypes retType

    case result of
        Left err -> die (mkEvalErrorHint mpos
            ("failed to bind C function '" ++ name ++ "'")
            err)
        Right wrapper -> do
            return $ insertHashtable table name (CBinding name wrapper)
    
tryLoadFromLibs :: [String] -> String -> [String] -> String -> IO (Either String ([Value] -> IO Value))
tryLoadFromLibs [] funcName _ _ = 
    return $ Left $ "function not found in any library"
tryLoadFromLibs [libPath] funcName argTypes retType = do
    FFI.loadCFunction libPath funcName argTypes retType
tryLoadFromLibs (libPath:rest) funcName argTypes retType = do
    result <- FFI.loadCFunction libPath funcName argTypes retType
    case result of
        Right wrapper -> return $ Right wrapper
        Left _ -> tryLoadFromLibs rest funcName argTypes retType

-- Resolve a module name to a file path using the same search order as `import`:
-- a bundle dir (BERN_LIB_PATH), ./lib/, the install lib dir, then ./.
resolveModulePath :: String -> IO (Maybe FilePath)
resolveModulePath moduleName = do
    execPath <- getExecutablePath
    let installDir = takeDirectory execPath
        installLibPath = installDir </> "lib" </> moduleName ++ ".brn"
        localLibPath   = "lib" </> moduleName ++ ".brn"
        localPath      = moduleName ++ ".brn"
    mBundleDir <- lookupEnv "BERN_LIB_PATH"
    let bundleLibPath = case mBundleDir of
                          Just d  -> d </> "lib" </> moduleName ++ ".brn"
                          Nothing -> ""
    let firstExisting [] = return Nothing
        firstExisting (p:ps)
            | null p    = firstExisting ps
            | otherwise = do e <- doesFileExist p
                             if e then return (Just p) else firstExisting ps
    firstExisting [bundleLibPath, localLibPath, installLibPath, localPath]

-- Register every user-defined operator reachable from a source file BEFORE it is
-- parsed: the file's own operators, plus those of every module it imports,
-- transitively. This is what lets an operator declared in a library (e.g. core)
-- be used, infix, in any file that imports it -- operators propagate like the
-- functions they desugar to. Missing modules are ignored here (the importer will
-- report the error properly when it actually runs the `import`).
preloadOperators :: String -> IO ()
preloadOperators src = do
    addOperatorSpecs (scanOperators src)
    visit [] (scanImports src)
  where
    visit _ [] = return ()
    visit seen (m:ms)
        | m `elem` seen = visit seen ms
        | otherwise = do
            mp <- resolveModulePath m
            case mp of
                Nothing -> visit (m:seen) ms
                Just p  -> do
                    contents <- readFile p
                    addOperatorSpecs (scanOperators contents)
                    visit (m:seen) (ms ++ scanImports contents)

-- Apply a function or lambda value to arguments
{-
English:
  How a function is CALLED. applyFunction takes a function value, the argument
  values, and the scope, and produces a result (or an error). The pieces:
    * curryOrApply decides between applying and currying: if too few arguments were
      given, it returns a partial application (a new function remembering the
      arguments so far) instead of failing -- this is Bern's auto-currying, switched
      off by the `no-curry` pragma, in which case it just errors.
    * applyClauses walks the function's clauses in order, trying each with matchAll +
      the optional `when` guard, and runs the body of the first that matches; if none
      match, that is a "no matching clause" error.
    * runBody evaluates a clause body -- either a single expression or a `do` block.
    * Throughout, withPragmas reinstalls the DEFINING file's pragmas around the call,
      so the function behaves under its own rules (see the pragma section above).

Português:
  Como uma função é CHAMADA. applyFunction recebe um valor de função, os valores de
  argumento e o escopo, e produz um resultado (ou um erro). As peças:
    * curryOrApply decide entre aplicar e fazer currying: se argumentos de menos foram
      dados, ela retorna uma aplicação parcial (uma nova função que lembra os
      argumentos até agora) em vez de falhar -- este é o currying automático do Bern,
      desligado pelo pragma `no-curry`, caso em que ela apenas dá erro.
    * applyClauses percorre as cláusulas da função em ordem, tentando cada uma com
      matchAll + a guarda `when` opcional, e roda o corpo da primeira que casa; se
      nenhuma casa, isso é um erro de "nenhuma cláusula casa".
    * runBody avalia o corpo de uma cláusula -- ou uma única expressão ou um bloco `do`.
    * Por toda parte, withPragmas reinstala os pragmas do arquivo que DEFINIU em volta
      da chamada, para que a função se comporte sob as suas próprias regras (ver a seção
      de pragmas acima).
-}
applyFunction :: Value -> [Value] -> Hashtable String Value -> Either EvalError Value
applyFunction fn@(Function clauses prags) args table = withPragmas prags (curryOrApply fn clauses args table)
applyFunction fn@(Lambda clauses prags) args table = withPragmas prags (curryOrApply fn clauses args table)
applyFunction (CBinding _ wrapper) args _ = Right (unsafePerformIO (wrapper args))
applyFunction v _ _ = Left $ mkEvalError Nothing ("expected function, found " ++ getValueType v)

-- Auto-curry (default): calling a function with fewer arguments than every
-- clause expects yields a partial application instead of an error. The partial
-- is a closure that re-applies the original once the rest arrive. Disabled by
-- the `no-curry` pragma.
curryOrApply :: Value -> [Clause] -> [Value] -> Hashtable String Value -> Either EvalError Value
curryOrApply fn clauses args table
    | shouldCurry = Right (makePartial fn args table)
    | otherwise   = applyClauses clauses args table
  where
    arities = [ length ps | Clause ps _ _ <- clauses ]
    shouldCurry =
        not (pragmaOn "no-curry" ())
        && not (null arities)
        && length args > 0
        && minimum arities > length args

makePartial :: Value -> [Value] -> Hashtable String Value -> Value
makePartial fn captured table =
    CBinding "<partial>" $ \more ->
        case applyFunction fn (captured ++ more) table of
            Right v  -> return v
            Left err -> die err

applyClauses :: [Clause] -> [Value] -> Hashtable String Value -> Either EvalError Value
applyClauses [] args _ = Left $ mkEvalError Nothing ("no matching pattern for " ++ show (length args) ++ " argument(s)")
applyClauses (Clause patterns mGuard body : rest) args table =
    case matchAll patterns args of
        Nothing -> applyClauses rest args table
        Just bindings ->
            let newTable = foldl (\tbl (k,v) -> insertHashtable tbl k v) table bindings
            in case evalGuard mGuard newTable of
                Left err    -> Left err
                Right False -> applyClauses rest args table  -- guard failed: try the next clause
                Right True  -> runBody body newTable

-- Run a clause/branch body in its bound scope.
runBody :: FunctionBody -> Hashtable String Value -> Either EvalError Value
runBody (BodyExpr expr) tbl = evaluate expr tbl
runBody (BodyBlock cmd) tbl =
    let resultTable = unsafePerformIO (interpretCommand Nothing cmd tbl)
    in case lookupHashtable resultTable "__return" of
        Just v  -> Right v
        Nothing -> Right Undefined

-- Evaluate an optional `when` guard. A missing guard always passes. A guard
-- must evaluate to a boolean; anything else is a type error.
evalGuard :: Maybe Expression -> Hashtable String Value -> Either EvalError Bool
evalGuard Nothing  _   = Right True
evalGuard (Just g) tbl = case evaluate g tbl of
    Right (Boolean b) -> Right b
    Right v           -> Left $ mkEvalError Nothing ("guard must evaluate to a boolean, found " ++ getValueType v)
    Left err          -> Left err

len :: [a] -> Int
len = length

-- Match argument list against pattern list, returning bindings if successful.
-- combineBindings enforces linearity: if the same variable is bound more than
-- once (e.g. `def eq(x, x)`), the bound values must be equal or the match fails.
matchAll :: [Pattern] -> [Value] -> Maybe [(String, Value)]
matchAll ps vs
    | length ps /= len vs = Nothing
    | otherwise = sequence (zipWith matchOne ps vs) >>= combineBindings . concat

-- Merge bindings, rejecting conflicting bindings for the same variable.
combineBindings :: [(String, Value)] -> Maybe [(String, Value)]
combineBindings = foldr step (Just [])
  where
    step _     Nothing    = Nothing
    step (k,v) (Just acc) = case lookup k acc of
        Just v' -> if v == v' then Just acc else Nothing
        Nothing -> Just ((k, v) : acc)

{-
English:
  PATTERN MATCHING -- the function that decides whether a pattern fits a value, and
  if so, what names it binds. Returns `Just bindings` on a match (bindings is a list
  of name/value pairs to add to the scope) or `Nothing` on no match.

  Each equation handles one Pattern shape against a compatible value: a variable
  binds anything; a literal matches only itself; `[h|t]` (PCons) splits a non-empty
  list into its head and a tail list and recurses into both; PADT checks the
  constructor name and matches the fields; and so on. `matchAll` zips a list of
  patterns with a list of values, and `combineBindings` merges the per-pattern
  bindings, rejecting the match if the SAME name is bound to two different values
  (this is how `def same(x, x)` only matches equal arguments).

  Note this is pure `Maybe`, not `Either`: a non-match is an ordinary "no", not an
  error -- the caller just tries the next clause.

Português:
  CASAMENTO DE PADRÕES -- a função que decide se um padrão serve a um valor e, se
  serve, quais nomes ele vincula. Retorna `Just vínculos` num casamento (vínculos é
  uma lista de pares nome/valor a adicionar ao escopo) ou `Nothing` em não-casamento.

  Cada equação trata uma forma de Pattern contra um valor compatível: uma variável
  vincula qualquer coisa; um literal casa só consigo mesmo; `[h|t]` (PCons) divide uma
  lista não-vazia na cabeça e numa lista de cauda e recursiona nos dois; PADT confere
  o nome do construtor e casa os campos; e assim por diante. `matchAll` faz o zip de
  uma lista de padrões com uma lista de valores, e `combineBindings` funde os vínculos
  de cada padrão, rejeitando o casamento se o MESMO nome for vinculado a dois valores
  diferentes (é assim que `def same(x, x)` só casa argumentos iguais).

  Repare que isto é `Maybe` puro, não `Either`: um não-casamento é um "não" comum, não
  um erro -- o chamador apenas tenta a próxima cláusula.
-}
matchOne :: Pattern -> Value -> Maybe [(String, Value)]
matchOne PWildcard _ = Just []
matchOne (PVar name) v = Just [(name, v)]
matchOne (PInt n) (Integer m) | n == m = Just []
matchOne (PDouble x) (Double y) | x == y = Just []
-- Cross-type numeric matching: an int pattern matches an equal double and
-- vice-versa (e.g. PInt 5 matches Double 5.0).
matchOne (PInt n) (Double y) | fromIntegral n == y = Just []
matchOne (PDouble x) (Integer m) | x == fromIntegral m = Just []
matchOne (PBool b) (Boolean c) | b == c = Just []
matchOne (PChar c) (Character d) | c == d = Just []
matchOne (PADT typeName pats) (AlgebraicDataType valType vals)
    | typeName == valType && length pats == length vals = matchAll pats vals
    | otherwise = Nothing
matchOne (PString s) (List vs _) | allCharacter vs && map (\(Character c) -> c) vs == s = Just []
matchOne (PList []) (List [] _) = Just []
matchOne (PList pats) (List vals len)
    -- Compare against the stored length, not `length vals`, so matching a fixed
    -- pattern against a lazy range does not force the whole spine.
    | length pats == len = matchAll pats vals
    | otherwise = Nothing
matchOne (PCons headPat tailPat) (List (v:vs) len) = do
    headBindings <- matchOne headPat v
    tailBindings <- matchOne tailPat (List vs (len - 1))
    combineBindings (headBindings ++ tailBindings)
matchOne (PCons _ _) (List [] _) = Nothing
matchOne (PSet []) (Set [] _) = Just []
matchOne (PSet pats) (Set vals _)
    | length pats == length vals = matchAll pats vals
    | otherwise = Nothing
matchOne (PSetCons headPat tailPat) (Set (v:vs) _) = do
    headBindings <- matchOne headPat v
    tailBindings <- matchOne tailPat (Set vs (length vs))
    combineBindings (headBindings ++ tailBindings)
matchOne (PSetCons _ _) (Set [] _) = Nothing
matchOne _ _ = Nothing

hasReturn :: Hashtable String Value -> Bool
hasReturn tbl = isJust (lookupHashtable tbl "__return")

-- | Fatal error helper now accepts structured `EvalError`.
die :: EvalError -> IO a
die evalErr = putStrLn (formatError evalErr) >> exitFailure

-- errors-as-values (default): evaluate an expression, turning a runtime error
-- into a first-class ErrorVal instead of aborting. The abort-on-error pragma
-- restores the crash-on-error behaviour.
evalValue :: Expression -> Hashtable String Value -> Either EvalError Value
evalValue expr table = case evaluate expr table of
    Right v -> Right v
    Left err
        | pragmaOn "abort-on-error" () -> Left err
        | otherwise                    -> Right (ErrorVal (errorMsg err))

-- Poison propagation: the first ErrorVal among operands, if any.
firstError :: [Value] -> Maybe Value
firstError vs = case [ e | e@(ErrorVal _) <- vs ] of
    (e:_) -> Just e
    []    -> Nothing

-- Convert iterable values (list, set, and explicitly iterative ADTs) to a list of values for for-in loops
toIterable :: Hashtable String Value -> Value -> Either EvalError [Value]
toIterable table v@(AlgebraicDataType ctorName vals)
    | isIterativeCtor table ctorName = Right vals
    | otherwise = Left $ mkEvalError Nothing ("cannot iterate over " ++ getValueType v ++ ". Have you defined it with 'adt iterative'?")
toIterable _ (List vals _) = Right vals
toIterable _ (Set vals _)  = Right vals
toIterable _ v = Left $ mkEvalError Nothing ("cannot iterate over " ++ getValueType v)

isIterativeCtor :: Hashtable String Value -> String -> Bool
isIterativeCtor tbl ctorName =
    case lookupHashtable tbl ("__bern_iterable_adt_ctor_" ++ ctorName) of
        Just (Boolean True) -> True
        _                   -> False

-- Expand the qualifiers of a list comprehension into the binding environments
-- that satisfy them. A generator iterates its source (skipping elements whose
-- pattern does not match, like Haskell); a guard keeps a row only when it is
-- true. The result is one environment per surviving combination of bindings.
expandComp :: Hashtable String Value -> [CompQual] -> Either EvalError [Hashtable String Value]
expandComp t [] = Right [t]
expandComp t (CompGuard g : qs) = do
    gv <- evaluate g t
    case gv of
        Boolean True  -> expandComp t qs
        Boolean False -> Right []
        ErrorVal m    -> Left $ mkEvalError Nothing ("comprehension guard error: " ++ m)
        v             -> Left $ mkEvalError Nothing
                            ("comprehension guard must be Bool, found " ++ getValueType v)
expandComp t (CompGen pat srcExpr : qs) = do
    src <- evaluate srcExpr t
    case src of
        ErrorVal m -> Left $ mkEvalError Nothing ("comprehension source error: " ++ m)
        _          -> do
            elems <- toIterable t src
            rows  <- mapM step elems
            Right (concat rows)
  where
    step el = case matchOne pat el of
        Just bs -> expandComp (foldl (\tbl (k, v) -> insertHashtable tbl k v) t bs) qs
        Nothing -> Right []


-- Resolve a raw index into a 0-based offset. Negative indices count from the
-- end (default); the start-on-one pragma makes positive indices 1-based.
resolveIndex :: Int -> Int -> Int
resolveIndex len i =
    let i1 = if pragmaOn "start-on-one" () && i > 0 then i - 1 else i
    in if i1 < 0 then len + i1 else i1

indexLookup :: [Value] -> Int -> Int -> Either EvalError Value
indexLookup vals len i =
    let idx = resolveIndex len i
    in if idx >= 0 && idx < len
        then Right (vals !! idx)
        else if pragmaOn "safe-index" ()
            then Right Undefined
            else Left $ mkEvalError Nothing
                ("index " ++ show i ++ " out of bounds (length " ++ show len ++ ")")

{-
English:
  THE EXPRESSION EVALUATOR -- the other spine of the interpreter, alongside
  interpretCommand. `evaluateRaw` has one equation per Expression constructor and
  computes its Value (a literal is itself; a BinaryOperator evaluates both sides and
  combines them; a FunctionCall looks the function up and applies it; and so on).

  `evaluate` is the thin wrapper that implements ERRORS AS VALUES, the default model:
  if evaluateRaw returns a `Left` error, evaluate converts it into a `Right ErrorVal`
  -- a normal value carrying the message. Because an ErrorVal is just a value, it flows
  through the rest of the expression (operators "poison-propagate" it, see firstError)
  and can be tested with is_error, instead of aborting the program. The `abort-on-error`
  pragma turns this off, letting the Left through so the failure crashes as usual.

  So the rule of thumb when reading below: evaluateRaw says HOW each expression is
  computed; evaluate decides what a failure MEANS.

Português:
  O AVALIADOR DE EXPRESSÕES -- a outra espinha do interpretador, ao lado do
  interpretCommand. `evaluateRaw` tem uma equação por construtor de Expression e
  calcula seu Value (um literal é ele mesmo; um BinaryOperator avalia os dois lados e
  os combina; um FunctionCall busca a função e a aplica; e assim por diante).

  `evaluate` é o invólucro fino que implementa os ERROS COMO VALORES, o modelo padrão:
  se evaluateRaw retorna um erro `Left`, evaluate o converte em um `Right ErrorVal` --
  um valor normal carregando a mensagem. Como um ErrorVal é só um valor, ele flui pelo
  resto da expressão (operadores o "propagam como veneno", ver firstError) e pode ser
  testado com is_error, em vez de abortar o programa. O pragma `abort-on-error`
  desliga isto, deixando o Left passar para que a falha quebre como de costume.

  Então a regra ao ler abaixo: evaluateRaw diz COMO cada expressão é computada;
  evaluate decide o que uma falha SIGNIFICA.
-}
-- Public entry to expression evaluation. errors-as-values (default) turns any
-- runtime error into a first-class ErrorVal at every level, so an erroring
-- sub-expression poisons just its own value (and can be inspected with
-- is_error) rather than aborting. The abort-on-error pragma restores crashing.
evaluate :: Expression -> Hashtable String Value -> Either EvalError Value
evaluate expr table = case evaluateRaw expr table of
    Left err | not (pragmaOn "abort-on-error" ()) -> Right (ErrorVal (errorMsg err))
    other -> other

evaluateRaw :: Expression -> Hashtable String Value -> Either EvalError Value
evaluateRaw (Number n) _ = Right (Integer n)
evaluateRaw (DoubleNum d) _ = Right (Double d)
evaluateRaw (BoolLiteral b) _ = Right (Boolean b)
evaluateRaw (IfExpr cond thenExpr elseExpr) table = do
    condVal <- evaluate cond table
    case condVal of
        Boolean True  -> evaluate thenExpr table
        Boolean False -> evaluate elseExpr table
        v -> Left $ mkEvalErrorHint Nothing
            "condition must be Bool"
            ("found: " ++ getValueType v)
evaluateRaw (CaseExpr target branches) table = do
    targetVal <- evaluate target table
    evalBranches targetVal branches
  where
    evalBranches :: Value -> [CaseBranch] -> Either EvalError Value
    evalBranches _ [] = Left $ mkEvalError Nothing "no matching case branch"
    evalBranches v (CaseBranch pat mGuard body : rest) =
        case matchOne pat v of
            Just bindings ->
                let caseTable = foldl (\tbl (k,val) -> insertHashtable tbl k val) table bindings
                in case evalGuard mGuard caseTable of
                    Left err    -> Left err
                    Right False -> evalBranches v rest  -- guard failed: try the next branch
                    Right True  -> evaluate body caseTable
            Nothing -> evalBranches v rest
evaluateRaw (StringLiteral s) _ = Right (List (map Character s) (length s))
evaluateRaw (CharLiteral c) _ = Right (Character c)

evaluateRaw (ListLiteral exprs) table = do
    vals <- mapM (`evaluate` table) exprs
    if null vals
        then Right (List [] 0)
        else if pragmaOn "impure-lists" () || allSameType vals
            then Right (List vals (length vals))
            else Left $ mkEvalError Nothing "list elements must have the same type"

evaluateRaw (SetLiteral exprs) table = do
    vals <- mapM (`evaluate` table) exprs
    Right (Set vals (length vals))

evaluateRaw (Range start end) table = do
    s <- evaluate start table
    e <- evaluate end table
    case (s, e) of
        (Integer s', Integer e') ->
            -- lazy-ranges: the length is computed arithmetically so the element
            -- spine stays an unforced thunk. Indexing, head, take and iteration
            -- then only realise the elements they actually touch, so even
            -- [1..1000000000] is cheap to index or take from.
            let step = if s' <= e' then 1 else -1
                vals = [Integer i | i <- [s', s'+step .. e']]
            in Right (List vals (abs (e' - s') + 1))
        (Double s', Double e') ->
            let step = if s' <= e' then 1 else -1
                vals = [Double i | i <- [s', s'+step .. e']]
            in Right (List vals (length vals))
        (Integer s', Double e') ->
            let s'' = fromIntegral s'
                step = if s'' <= e' then 1 else -1
                vals = [Double i | i <- [s'', s''+step .. e']]
            in Right (List vals (length vals))
        (Double s', Integer e') ->
            let e'' = fromIntegral e'
                step = if s' <= e'' then 1 else -1
                vals = [Double i | i <- [s', s'+step .. e'']]
            in Right (List vals (length vals))
        _ -> Left $ mkEvalError Nothing "range bounds must be numeric"

evaluateRaw (ListCons headExprs tailExpr) table = do
    headVals <- mapM (`evaluate` table) headExprs
    tailVal  <- evaluate tailExpr table
    case firstError (headVals ++ [tailVal]) of   -- errors-as-values: poison propagates
        Just e  -> Right e
        Nothing -> case tailVal of
            List tvals tlen ->
                -- Prepend without forcing the tail spine: only one tail element
                -- is peeked for the homogeneity check, so [0 | [1..1000000000]]
                -- stays lazy just like a range.
                if pragmaOn "impure-lists" () || allSameType (headVals ++ take 1 tvals)
                    then Right (List (headVals ++ tvals) (length headVals + tlen))
                    else Left $ mkEvalError Nothing "list elements must have the same type"
            _ -> Left $ mkEvalError Nothing
                    ("the tail of [.. | tail] must be a list, found " ++ getValueType tailVal)

evaluateRaw (SetCons headExprs tailExpr) table = do
    headVals <- mapM (`evaluate` table) headExprs
    tailVal  <- evaluate tailExpr table
    case firstError (headVals ++ [tailVal]) of
        Just e  -> Right e
        Nothing -> case tailVal of
            Set tvals tlen -> Right (Set (headVals ++ tvals) (length headVals + tlen))
            _ -> Left $ mkEvalError Nothing
                    ("the tail of {.. | tail} must be a set, found " ++ getValueType tailVal)

evaluateRaw (ListComp headExpr quals) table = do
    rows <- expandComp table quals
    vals <- mapM (evaluate headExpr) rows
    case firstError vals of
        Just e  -> Right e
        Nothing ->
            if null vals
                then Right (List [] 0)
                else if pragmaOn "impure-lists" () || allSameType vals
                    then Right (List vals (length vals))
                    else Left $ mkEvalError Nothing "list elements must have the same type"

evaluateRaw (Keys objExpr) table = do
    v <- evaluate objExpr table
    case v of
        Object kvs -> Right (List (map (\(k, _) -> stringToValue k) kvs) (length kvs))
        _          -> Left $ mkEvalError Nothing ("keys expects an Object, found " ++ getValueType v)

evaluateRaw (MakeError msgExpr) table = do
    v <- evaluate msgExpr table
    case valueToString v of
        Just s  -> Right (ErrorVal s)
        Nothing -> Right (ErrorVal (prettyValue v))

evaluateRaw (Fmap coll func) table = do
    collection <- evaluate coll table
    functionVal <- evaluate func table
    case collection of
        AlgebraicDataType typeName args -> do
            newArgs <- mapM (\arg -> applyFunction functionVal [arg] table) args
            Right (AlgebraicDataType typeName newArgs)
        List vals len -> do
            newVals <- mapM (\v -> applyFunction functionVal [v] table) vals
            Right (List newVals len)
        Set vals len -> do
            newVals <- mapM (\v -> applyFunction functionVal [v] table) vals
            Right (Set newVals len)
        _ -> Left $ mkEvalError Nothing ("fmap requires List, Set, or ADT, found " ++ getValueType collection)
 
evaluateRaw (Index expr idxExpr) table = do
    collection <- evaluate expr table
    idx <- evaluate idxExpr table
    case (collection, idx) of
        (List vals len, Integer i) -> indexLookup vals len i
        (Set vals len, Integer i)  -> indexLookup vals len i
        (Object kvs, lst@(List _ _)) ->
            case valueToString lst of
                Just key -> case lookup key kvs of
                                Just v  -> Right v
                                Nothing -> Left $ mkEvalError Nothing ("key '" ++ key ++ "' not found")
                Nothing -> Left $ mkEvalError Nothing "object index must be String"
        (Object kvs, Integer i) ->
            let key = show i in
            case lookup key kvs of
                Just v  -> Right v
                Nothing -> Left $ mkEvalError Nothing ("key '" ++ key ++ "' not found")
        (Object kvs, Character c) ->
            case lookup [c] kvs of
                Just v  -> Right v
                Nothing -> Left $ mkEvalError Nothing ("key '" ++ [c] ++ "' not found")
        _ -> Left $ mkEvalError Nothing ("cannot index " ++ getValueType collection ++ " with " ++ getValueType idx)

evaluateRaw (Variable name) table
    | isAmbiguousImportedSymbol table name = Left (ambiguousImportedSymbolError table name)
    | otherwise = unsafePerformIO $ do
    globalScopeNow <- readMVar globalScopeMVar
    case lookupHashtable globalScopeNow name of
        Just val -> do
            return (Right val)
        Nothing  ->
            case lookupHashtable table name of
                Just val -> do
                    return (Right val)
                Nothing
                    | pragmaOn "no-undefined" () ->
                        return (Left (mkEvalErrorHint Nothing
                            ("undefined variable '" ++ name ++ "'")
                            "the no-undefined pragma forbids reading unbound names"))
                    | otherwise -> return (Right Undefined)

evaluateRaw (ReadFile filenameExpr) table = do
    pathVal <- evaluate filenameExpr table
    case valueToString pathVal of
        Just path -> do
            let contents = unsafePerformIO (readFile path)
            return (List (map Character contents) (length contents))
        Nothing -> Left $ mkEvalError Nothing "file path must be String"

evaluateRaw GetHostMachine _ = 
    let hostMachine = os
    in Right (List (map Character hostMachine) (length hostMachine))

evaluateRaw GetCurrentDir _ =
    let currentDir = unsafePerformIO (getCurrentDirectory)
    in Right (List (map Character currentDir) (length currentDir))

evaluateRaw (WithPos pos expr) table =
    case evaluate expr table of
        Left err ->
            case errorPos err of
                Nothing -> Left (err { errorPos = Just pos })
                Just _  -> Left err
        Right v  -> Right v

evaluateRaw (AlgebraicDataTypeConstruct typeName args) table = do
    argVals <- mapM (`evaluate` table) args
    if typeName == "List" then
        Right (List argVals (length argVals))
    else if typeName == "Set" then
        Right (Set argVals (length argVals))
    else
        Right (AlgebraicDataType typeName argVals)

-- Logical operators short-circuit: the right operand is only evaluated when the
-- left does not already determine the result.
evaluateRaw (BinaryOperator And left right) table = do
    leftVal <- evaluate left table
    case leftVal of
        Boolean False -> Right (Boolean False)
        Boolean True  -> evaluate right table >>= evalComparison And leftVal
        _             -> evaluate right table >>= evalComparison And leftVal
evaluateRaw (BinaryOperator Or left right) table = do
    leftVal <- evaluate left table
    case leftVal of
        Boolean True  -> Right (Boolean True)
        Boolean False -> evaluate right table >>= evalComparison Or leftVal
        _             -> evaluate right table >>= evalComparison Or leftVal

evaluateRaw (BinaryOperator op left right) table = do
    leftVal <- evaluate left table
    rightVal <- evaluate right table
    case firstError [leftVal, rightVal] of   -- errors-as-values: poison propagates
        Just e  -> Right e
        Nothing ->
            if isArithmetic op
                then evalArith op leftVal rightVal
                else if isUnion op
                    then evalUnion op leftVal rightVal
                    else evalComparison op leftVal rightVal

evaluateRaw (UnaryOperator op expr) table = do
    val <- evaluate expr table
    case (op, val) of
        (TypeOf, _)     -> evalUnaryOp op val   -- typeof must see through an ErrorVal
        (_, ErrorVal _) -> Right val            -- other ops poison-propagate
        _               -> evalUnaryOp op val

evaluateRaw (ObjectLiteral kvExprs) table = do
    kvVals <- mapM (\(k, vExpr) -> do
                        vVal <- evaluate vExpr table
                        return (k, vVal)) kvExprs
    Right (Object kvVals)

evaluateRaw (LambdaExpr params mGuard bodyExpr) _table =
    Right (Lambda [Clause params mGuard (BodyExpr bodyExpr)] (currentPragmas ()))

-- Apply a function-valued expression (lambda, or any expression evaluating to a
-- function) to arguments. Named functions go through FunctionCall instead; this
-- path exists mainly so the )| pipe can target literal lambdas.
evaluateRaw (Apply fExpr argExprs) table = do
    fnVal <- evaluate fExpr table
    vals  <- mapM (`evaluate` table) argExprs
    case fnVal of
        CBinding _ wrapper -> Right (unsafePerformIO (wrapper vals))
        _                  -> applyFunction fnVal vals table

evaluateRaw (FunctionCall name args) table
    | isAmbiguousImportedSymbol table name = Left (ambiguousImportedSymbolError table name)
    | otherwise =
        case lookupHashtable table name of
            Nothing -> Left $ mkEvalError Nothing ("undefined function with name '" ++ name ++ "'")
            Just fnVal ->
                case sequence (map (`evaluate` table) args) of
                    Left err -> Left err
                    Right vals ->
                        let callTable =
                                case splitQualifiedName name of
                                    Just (moduleName, baseSymbol) ->
                                        let moduleScoped = withQualifiedModuleScope table moduleName
                                            withBase = insertHashtable moduleScoped baseSymbol fnVal
                                        in insertHashtable withBase (importConflictKey baseSymbol) (Boolean False)
                                    Nothing -> table
                        in dispatchCall name fnVal vals callTable
  where
    dispatchCall callName (Function [] _) vals _ =
        let ctorName = case splitQualifiedName callName of
                        Just (_, base) -> base
                        Nothing        -> callName
        in Right (AlgebraicDataType ctorName vals)
    dispatchCall _ (CBinding _ wrapper) vals _ =
        Right (unsafePerformIO (wrapper vals))
    dispatchCall _ fnVal vals callTable =
        applyFunction fnVal vals callTable

evaluateRaw _ _ = Left $ mkEvalError Nothing "unsupported expression"

isArithmetic :: BinaryOperation -> Bool
isArithmetic Add = True
isArithmetic Subtract = True
isArithmetic Multiply = True
isArithmetic Divide = True
isArithmetic Modulo = True
isArithmetic _ = False

isUnion :: BinaryOperation -> Bool
isUnion Concatenation = True
isUnion Union = True
isUnion Intersection = True
isUnion Difference = True
isUnion _ = False

-- Division/modulo by zero: an error under strict-arithmetic, otherwise NaN.
divZero :: Either EvalError Value
divZero
    | pragmaOn "strict-arithmetic" () = Left (mkEvalError Nothing "division by zero")
    | otherwise = Right NaN

-- Raised when strict-types forbids an implicit String/number coercion.
strictCoerceErr :: Either EvalError Value
strictCoerceErr = Left (mkEvalErrorHint Nothing
    "implicit coercion between String and number"
    "the strict-types pragma forbids implicit coercion")

{-
English:
  The OPERATOR ENGINE: given an operator tag and two already-evaluated values, work
  out the result. evaluateRaw delegates here. It is split by category:
    * evalArith     -- +, -, *, /, % and the collection-aware versions. This is where
      Bern's overloading lives: + adds numbers, concatenates/maps over lists, merges
      objects, etc., and where pragmas like strict-types (no implicit coercion),
      strict-arithmetic (divide-by-zero is an error, not NaN) and impure-lists take
      effect. numericAdd/Sub/Mul/Div are the plain number kernels it calls.
    * evalComparison -- ==, !=, <, >, <=, >= , producing a Boolean.
    * evalUnion      -- the set-theory operators <> <| |> </> on lists and sets, with
      impure-sets deciding whether duplicates are kept.
    * evalUnaryOp    -- the prefix operators: negate, not, typeof (::), length (:>).
  Each returns `Either EvalError Value`, so a type mismatch becomes a Left that
  evaluate will turn into an ErrorVal.

Português:
  O MOTOR DE OPERADORES: dado um tag de operador e dois valores já avaliados, descobre
  o resultado. evaluateRaw delega para cá. Ele é dividido por categoria:
    * evalArith     -- +, -, *, /, % e as versões cientes de coleção. É aqui que mora a
      sobrecarga do Bern: + soma números, concatena/mapeia sobre listas, funde objetos,
      etc., e onde pragmas como strict-types (sem coerção implícita), strict-arithmetic
      (divisão por zero é erro, não NaN) e impure-lists fazem efeito. numericAdd/Sub/
      Mul/Div são os núcleos numéricos puros que ele chama.
    * evalComparison -- ==, !=, <, >, <=, >= , produzindo um Boolean.
    * evalUnion      -- os operadores de teoria de conjuntos <> <| |> </> em listas e
      conjuntos, com impure-sets decidindo se duplicatas são mantidas.
    * evalUnaryOp    -- os operadores prefixos: negate, not, typeof (::), length (:>).
  Cada um retorna `Either EvalError Value`, então uma incompatibilidade de tipo vira um
  Left que o evaluate transformará em um ErrorVal.
-}
evalArith :: BinaryOperation -> Value -> Value -> Either EvalError Value
evalArith Add (Integer l) (Integer r) = Right (Integer (l + r))
evalArith Add (Double l) (Double r) = Right (Double (l + r))
evalArith Add (Integer l) (Double r) = Right (Double (fromIntegral l + r))
evalArith Add (Double l) (Integer r) = Right (Double (l + fromIntegral r))
evalArith Add l1@(List _ _) l2@(List _ _)
    | Just s1 <- valueToString l1
    , Just s2 <- valueToString l2
    = Right (List (map Character (s1 ++ s2)) (length s1 + length s2))
evalArith Add l@(List _ _) (Character c)
    | Just s <- valueToString l
    = Right (List (map Character (s ++ [c])) (length s + 1))
evalArith Add (Character c) l@(List _ _)
    | Just s <- valueToString l
    = Right (List (map Character ([c] ++ s)) (1 + length s))
evalArith Add l@(List _ _) (Integer r)
    | pragmaOn "strict-types" () = strictCoerceErr
    | Just s <- valueToString l
    = let sr = show r in Right (List (map Character (s ++ sr)) (length s + length sr))
evalArith Add (Integer l) r@(List _ _)
    | pragmaOn "strict-types" () = strictCoerceErr
    | Just s <- valueToString r
    = let sl = show l in Right (List (map Character (sl ++ s)) (length sl + length s))
evalArith Add l@(List _ _) (Double r)
    | pragmaOn "strict-types" () = strictCoerceErr
    | Just s <- valueToString l
    = let sr = show r in Right (List (map Character (s ++ sr)) (length s + length sr))
evalArith Add (Double l) r@(List _ _)
    | pragmaOn "strict-types" () = strictCoerceErr
    | Just s <- valueToString r
    = let sl = show l in Right (List (map Character (sl ++ s)) (length sl + length s))
evalArith Add (List vals len) scalar@(Integer _) = do
    vals' <- mapM (numericAdd scalar) vals
    Right (List vals' len)
evalArith Add (List vals len) scalar@(Double _) = do
    vals' <- mapM (numericAdd scalar) vals
    Right (List vals' len)
evalArith Add scalar@(Integer _) (List vals len) = do
    vals' <- mapM (numericAdd scalar) vals
    Right (List vals' len)
evalArith Add scalar@(Double _) (List vals len) = do
    vals' <- mapM (numericAdd scalar) vals
    Right (List vals' len)
evalArith Add (List l1 len1) (List l2 len2) =
    if len1 /= len2
        then Left $ mkEvalError Nothing ("cannot add lists of different lengths (" ++ show len1 ++ " and " ++ show len2 ++ ")")
        else let combined = zipWith numericAdd l1 l2
             in if all isRight combined
                   then Right (List (map fromRight combined) len1)
                   else Left $ mkEvalError Nothing "list addition requires numeric elements"
  where
    isRight (Right _) = True
    isRight _        = False
    fromRight (Right v) = v
    fromRight _         = error "Unexpected Left value"
evalArith Add l@(List _ _) r@(AlgebraicDataType name vals)
    | Just s <- valueToString l
    = let sADT = prettyValue r
      in Right (List (map Character (s ++ sADT)) (length s + length sADT))
evalArith Add l@(AlgebraicDataType name vals) r@(List _ _)
    | Just s <- valueToString r
    = let sADT = prettyValue l
      in Right (List (map Character (sADT ++ s)) (length sADT + length s))
evalArith Add (Set s1 len1) v =
    if v `elem` s1
        then Right (Set s1 len1)
        else Right (Set (s1 ++ [v]) (len1 + 1))

evalArith Subtract (Integer l) (Integer r) = Right (Integer (l - r))
evalArith Subtract (Double l) (Double r) = Right (Double (l - r))
evalArith Subtract (Integer l) (Double r) = Right (Double (fromIntegral l - r))
evalArith Subtract (Double l) (Integer r) = Right (Double (l - fromIntegral r))
evalArith Subtract (List vals len) scalar@(Integer _) = do
    vals' <- mapM (\v -> numericSub v scalar) vals
    Right (List vals' len)
evalArith Subtract (List vals len) scalar@(Double _) = do
    vals' <- mapM (\v -> numericSub v scalar) vals
    Right (List vals' len)
evalArith Subtract scalar@(Integer _) (List vals len) = do
    vals' <- mapM (numericSub scalar) vals
    Right (List vals' len)
evalArith Subtract scalar@(Double _) (List vals len) = do
    vals' <- mapM (numericSub scalar) vals
    Right (List vals' len)
evalArith Subtract (List l1 len1) (List l2 len2) =
    if len1 /= len2
        then Left $ mkEvalError Nothing ("cannot subtract lists of different lengths (" ++ show len1 ++ " and " ++ show len2 ++ ")")
        else let combined = zipWith numericSub l1 l2
             in if all isRight combined
                   then Right (List (map fromRight combined) len1)
                   else Left $ mkEvalError Nothing "list subtraction requires numeric elements"
  where
    isRight (Right _) = True
    isRight _        = False
    fromRight (Right v) = v
    fromRight _         = error "Unexpected Left value"
evalArith Subtract (Set s1 len1) v =
    if v `elem` s1
        then Right (Set (filter (/= v) s1) (len1 - 1))
        else Right (Set s1 len1)

evalArith Multiply (Integer l) (Integer r) = Right (Integer (l * r))
evalArith Multiply (Double l) (Double r) = Right (Double (l * r))
evalArith Multiply (Integer l) (Double r) = Right (Double (fromIntegral l * r))
evalArith Multiply (Double l) (Integer r) = Right (Double (l * fromIntegral r))
evalArith Multiply (List vals len) scalar@(Integer _) = do
    vals' <- mapM (numericMul scalar) vals
    Right (List vals' len)
evalArith Multiply (List vals len) scalar@(Double _) = do
    vals' <- mapM (numericMul scalar) vals
    Right (List vals' len)
evalArith Multiply scalar@(Integer _) (List vals len) = do
    vals' <- mapM (numericMul scalar) vals
    Right (List vals' len)
evalArith Multiply scalar@(Double _) (List vals len) = do
    vals' <- mapM (numericMul scalar) vals
    Right (List vals' len)
evalArith Multiply (List l1 len1) (List l2 len2) =
    if len1 /= len2
        then Left $ mkEvalError Nothing ("cannot multiply lists of different lengths (" ++ show len1 ++ " and " ++ show len2 ++ ")")
        else let combined = zipWith numericMul l1 l2
             in if all isRight combined
                   then Right (List (map fromRight combined) len1)
                   else Left $ mkEvalError Nothing "list multiplication requires numeric elements"
  where
    isRight (Right _) = True
    isRight _        = False
    fromRight (Right v) = v
    fromRight _         = error "Unexpected Left value"

evalArith Divide (Integer l) (Integer r)
    | r == 0    = divZero
    | otherwise = Right (Integer (l `div` r))
evalArith Divide (Double l) (Double r)
    | r == 0.0  = divZero
    | otherwise = Right (Double (l / r))
evalArith Divide (Integer l) (Double r)
    | r == 0.0  = divZero
    | otherwise = Right (Double (fromIntegral l / r))
evalArith Divide (Double l) (Integer r)
    | r == 0    = divZero
    | otherwise = Right (Double (l / fromIntegral r))
evalArith Divide (List vals len) scalar@(Integer r)
    | r == 0 = Right (List (replicate len NaN) len)
    | otherwise = do
        vals' <- mapM (numericDiv scalar) vals
        Right (List vals' len)
evalArith Divide (List vals len) scalar@(Double r)
    | r == 0 = Right (List (replicate len NaN) len)
    | otherwise = do
        vals' <- mapM (numericDiv scalar) vals
        Right (List vals' len)
evalArith Divide scalar@(Integer _) (List vals len) = do
    vals' <- mapM (numericDivLeft scalar) vals
    Right (List vals' len)
evalArith Divide scalar@(Double _) (List vals len) = do
    vals' <- mapM (numericDivLeft scalar) vals
    Right (List vals' len)
evalArith Divide (List l1 len1) (List l2 len2) =
    if len1 /= len2
        then Left $ mkEvalError Nothing ("cannot divide lists of different lengths (" ++ show len1 ++ " and " ++ show len2 ++ ")")
        else let combined = zipWith numericDiv l1 l2
             in if all isRight combined
                   then Right (List (map fromRight combined) len1)
                   else Left $ mkEvalError Nothing "list division requires numeric elements"
  where
    isRight (Right _) = True
    isRight _        = False
    fromRight (Right v) = v
    fromRight _         = error "Unexpected Left value"

evalArith Modulo (Integer l) (Integer r)
    | r == 0    = divZero
    | otherwise = Right (Integer (l `mod` r))
evalArith Modulo (Double l) (Double r)
    | r == 0.0  = divZero
    | otherwise = Right (Double (mod' l r))
  where
    mod' x y = x - y * fromIntegral (floor (x / y))
evalArith Modulo (Integer l) (Double r)
    | r == 0.0  = divZero
    | otherwise = Right (Double (mod' (fromIntegral l) r))
  where
    mod' x y = x - y * fromIntegral (floor (x / y))
evalArith Modulo (Double l) (Integer r)
    | r == 0    = divZero
    | otherwise = Right (Double (mod' l (fromIntegral r)))
  where
    mod' x y = x - y * fromIntegral (floor (x / y))

evalArith op l r = Left $ mkEvalError Nothing ("cannot apply " ++ show op ++ " to " ++ getValueType l ++ " and " ++ getValueType r)

numericAdd :: Value -> Value -> Either EvalError Value
numericAdd (Integer l) (Integer r) = Right (Integer (l + r))
numericAdd (Double l) (Double r) = Right (Double (l + r))
numericAdd (Integer l) (Double r) = Right (Double (fromIntegral l + r))
numericAdd (Double l) (Integer r) = Right (Double (l + fromIntegral r))
numericAdd _ _ = Left $ mkEvalError Nothing "numeric operation requires Int or Double"

numericSub :: Value -> Value -> Either EvalError Value
numericSub (Integer l) (Integer r) = Right (Integer (l - r))
numericSub (Double l) (Double r) = Right (Double (l - r))
numericSub (Integer l) (Double r) = Right (Double (fromIntegral l - r))
numericSub (Double l) (Integer r) = Right (Double (l - fromIntegral r))
numericSub _ _ = Left $ mkEvalError Nothing "numeric operation requires Int or Double"

numericMul :: Value -> Value -> Either EvalError Value
numericMul (Integer l) (Integer r) = Right (Integer (l * r))
numericMul (Double l) (Double r) = Right (Double (l * r))
numericMul (Integer l) (Double r) = Right (Double (fromIntegral l * r))
numericMul (Double l) (Integer r) = Right (Double (l * fromIntegral r))
numericMul _ _ = Left $ mkEvalError Nothing "numeric operation requires Int or Double"

numericDiv :: Value -> Value -> Either EvalError Value
numericDiv (Integer l) (Integer r)
    | r == 0    = divZero
    | otherwise = Right (Integer (l `div` r))
numericDiv (Double l) (Double r)
    | r == 0.0  = divZero
    | otherwise = Right (Double (l / r))
numericDiv (Integer l) (Double r)
    | r == 0.0  = divZero
    | otherwise = Right (Double (fromIntegral l / r))
numericDiv (Double l) (Integer r)
    | r == 0    = divZero
    | otherwise = Right (Double (l / fromIntegral r))
numericDiv _ _ = Left $ mkEvalError Nothing "numeric operation requires Int or Double"

numericDivLeft :: Value -> Value -> Either EvalError Value
numericDivLeft = flip numericDiv

data IndexKey = IdxInt Int | IdxKey String deriving (Eq, Show)

setAt :: [IndexKey] -> Value -> Value -> Either EvalError Value
setAt [] _ _ = Left $ mkEvalError Nothing "invalid assignment target"
setAt (IdxInt i0:is) newVal (List vals len)
    | idx < 0 || idx >= len = Left $ mkEvalError Nothing ("index " ++ show i0 ++ " out of bounds (length " ++ show len ++ ")")
    | null is =
        let old = vals !! idx
        in if pragmaOn "impure-lists" () || getValueType old == getValueType newVal
              then Right (List (replaceAt idx newVal vals) len)
              else Left $ mkEvalError Nothing ("type mismatch: cannot assign " ++ getValueType newVal ++ " to " ++ getValueType old)
    | otherwise = do
        nested <- case vals !! idx of
                    l@(List _ _) -> Right l
                    o@(Object _) -> Right o
                    v            -> Left $ mkEvalError Nothing ("cannot index into " ++ getValueType v)
        updatedNested <- setAt is newVal nested
        Right (List (replaceAt idx updatedNested vals) len)
  where idx = resolveIndex len i0
setAt (IdxKey k:is) newVal (Object kvs)
    | null is = Right (Object (upsert k newVal kvs))
    | otherwise = do
        nested <- case lookup k kvs of
                    Just v@(Object _) -> Right v
                    Just v@(List _ _) -> Right v
                    Just v            -> Left $ mkEvalError Nothing ("cannot index into " ++ getValueType v)
                    Nothing           -> Left $ mkEvalError Nothing ("key '" ++ k ++ "' not found")
        updatedNested <- setAt is newVal nested
        Right (Object (upsert k updatedNested kvs))
setAt (IdxInt _ : _) _ (Object _) = Left $ mkEvalError Nothing "expected String key for Object, found Int"
setAt (IdxKey _ : _) _ (List _ _) = Left $ mkEvalError Nothing "expected Int index for List, found String"
setAt _ _ v = Left $ mkEvalError Nothing ("cannot assign into " ++ getValueType v)

upsert :: String -> Value -> [(String, Value)] -> [(String, Value)]
upsert key val [] = [(key, val)]
upsert key val ((k,v):rest)
    | key == k  = (key, val) : rest
    | otherwise = (k,v) : upsert key val rest

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 newVal (_:xs) = newVal : xs
replaceAt n newVal (x:xs) = x : replaceAt (n-1) newVal xs

evalComparison :: BinaryOperation -> Value -> Value -> Either EvalError Value
evalComparison Equal (Integer l) (Integer r) = Right (Boolean (l == r))
evalComparison Equal (Double l) (Double r) = Right (Boolean (l == r))
evalComparison Equal (Integer l) (Double r) = Right (Boolean (fromIntegral l == r))
evalComparison Equal (Double l) (Integer r) = Right (Boolean (l == fromIntegral r))
evalComparison Equal (Boolean l) (Boolean r) = Right (Boolean (l == r))
evalComparison Equal v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 = Right (Boolean (s1 == s2))
evalComparison Equal (Character l) (Character r) = Right (Boolean (l == r))
evalComparison Equal (List l _) (List r _) = Right (Boolean (l == r))
evalComparison Equal (Set l _) (Set r _) = Right (Boolean (l == r))

evalComparison Different (Integer l) (Integer r) = Right (Boolean (l /= r))
evalComparison Different (Double l) (Double r) = Right (Boolean (l /= r))
evalComparison Different (Boolean l) (Boolean r) = Right (Boolean (l /= r))
evalComparison Different v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 = Right (Boolean (s1 /= s2))
evalComparison Different (Character l) (Character r) = Right (Boolean (l /= r))
evalComparison Different (List l _) (List r _) = Right (Boolean (l /= r))
evalComparison Different (Set l _) (Set r _) = Right (Boolean (l /= r))

evalComparison And (Boolean l) (Boolean r) = Right (Boolean (l && r))
evalComparison And l r = Left $ mkEvalError Nothing ("logical AND requires Bool, found " ++ getValueType l ++ " and " ++ getValueType r)

evalComparison Or (Boolean l) (Boolean r) = Right (Boolean (l || r))
evalComparison Or l r = Left $ mkEvalError Nothing ("logical OR requires Bool, found " ++ getValueType l ++ " and " ++ getValueType r)

evalComparison GreaterThan (Integer l) (Integer r) = Right (Boolean (l > r))
evalComparison GreaterThan (Double l) (Double r) = Right (Boolean (l > r))
evalComparison GreaterThan v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 = Right (Boolean (s1 > s2))
evalComparison GreaterThan (List l ls) (List r rs) = Right (Boolean (ls > rs))
evalComparison GreaterThan (Set l ls) (Set r rs) = Right (Boolean (ls > rs))

evalComparison LessThan (Integer l) (Integer r) = Right (Boolean (l < r))
evalComparison LessThan (Double l) (Double r) = Right (Boolean (l < r))
evalComparison LessThan v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 = Right (Boolean (s1 < s2))
evalComparison LessThan (List l ls) (List r rs) = Right (Boolean (ls < rs))
evalComparison LessThan (Set l ls) (Set r rs) = Right (Boolean (ls < rs))

evalComparison GreaterThanEq (Integer l) (Integer r) = Right (Boolean (l >= r))
evalComparison GreaterThanEq (Double l) (Double r) = Right (Boolean (l >= r))
evalComparison GreaterThanEq v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 = Right (Boolean (s1 >= s2))
evalComparison GreaterThanEq (List l ls) (List r rs) = Right (Boolean (ls >= rs))
evalComparison GreaterThanEq (Set l ls) (Set r rs) = Right (Boolean (ls >= rs))

evalComparison LessThanEq (Integer l) (Integer r) = Right (Boolean (l <= r))
evalComparison LessThanEq (Double l) (Double r) = Right (Boolean (l <= r))
evalComparison LessThanEq v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 = Right (Boolean (s1 <= s2))
evalComparison LessThanEq (List l ls) (List r rs) = Right (Boolean (ls <= rs))
evalComparison LessThanEq (Set l ls) (Set r rs) = Right (Boolean (ls <= rs))

evalComparison op l r = Left $ mkEvalError Nothing ("cannot apply " ++ show op ++ " to " ++ getValueType l ++ " and " ++ getValueType r)

evalUnaryOp :: UnaryOperation -> Value -> Either EvalError Value
evalUnaryOp Negate (Integer n) = Right (Integer (-n))
evalUnaryOp Negate (Double d) = Right (Double (-d))
evalUnaryOp Negate v = Left $ mkEvalError Nothing ("cannot negate " ++ getValueType v)
evalUnaryOp Not (Boolean b) = Right (Boolean (not b))
evalUnaryOp Not v = Left $ mkEvalError Nothing ("logical NOT requires Bool, found " ++ getValueType v)
evalUnaryOp TypeOf v =
    let t = getValueType v
    in Right (List (map Character t) (length t))
evalUnaryOp SizeOf v
    | Just s <- valueToString v = Right (Integer (length s))
evalUnaryOp SizeOf (List _ len) = Right (Integer len)
evalUnaryOp SizeOf (Set _ len) = Right (Integer len)
evalUnaryOp SizeOf (Object kvs) = Right (Integer (length kvs))
evalUnaryOp SizeOf v = Left $ mkEvalError Nothing ("cannot get size of " ++ getValueType v)

evalUnion :: BinaryOperation -> Value -> Value -> Either EvalError Value
evalUnion Concatenation (List l1 len1) (List l2 len2)
    -- Lists are homogeneous by construction (see ListLiteral), so comparing
    -- only the head element types is enough to guarantee the result stays
    -- homogeneous. This keeps concatenation O(len l1) instead of rescanning
    -- the whole result with allSameType (O(n)); that O(n) rescan on every
    -- step is what made the stdlib list functions O(n^2).
    | null l1 = Right (List l2 len2)
    | null l2 = Right (List l1 len1)
    | pragmaOn "impure-lists" () = Right (List (l1 ++ l2) (len1 + len2))
    | getValueType (head l1) == getValueType (head l2) =
        Right (List (l1 ++ l2) (len1 + len2))
    | otherwise = Left $ mkEvalError Nothing "list concatenation requires same element types"
evalUnion Concatenation (Set s1 _) (Set s2 _) =
    -- `impure` is forced into WHNF of the result so the pragma is read at the
    -- operation site (under the active file's pragmas), not lazily when the set
    -- is later forced -- which, with file-scoped pragmas, could be elsewhere.
    let impure = pragmaOn "impure-sets" ()
        combined = if impure then s1 ++ s2 else s1 ++ filter (`notElem` s1) s2
    in impure `seq` Right (Set combined (length combined))
evalUnion Concatenation v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2
    = let combined = s1 ++ s2
      in Right (List (map Character combined) (length combined))
evalUnion Concatenation (Character c1) (Character c2) =
    Right (List [Character c1, Character c2] 2)

evalUnion Union (Set s1 _) (Set s2 _) =
    let impure = pragmaOn "impure-sets" ()       -- forced into WHNF; see Concatenation above
        combined = if impure then s1 ++ s2 else s1 ++ filter (`notElem` s1) s2
    in impure `seq` Right (Set combined (length combined))
evalUnion Union (List l1 len1) (List l2 len2) =
    let combined = l1 ++ filter (`notElem` l1) l2
    in Right (List combined (length combined))
evalUnion Union v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 =
        let combinedStr = s1 ++ filter (`notElem` s1) s2
        in Right (List (map Character combinedStr) (length combinedStr))

evalUnion Intersection (Set s1 len1) (Set s2 len2) =
    let common = filter (`elem` s2) s1
    in Right (Set common (length common))
evalUnion Intersection (List l1 len1) (List l2 len2) =
    let common = filter (`elem` l2) l1
    in Right (List common (length common))
evalUnion Intersection v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 =
        let commonChars = filter (`elem` s2) s1
        in Right (List (map Character commonChars) (length commonChars))

evalUnion Difference (Set s1 len1) (Set s2 len2) =
    let diff = filter (`notElem` s2) s1
    in Right (Set diff (length diff))
evalUnion Difference (List l1 len1) (List l2 len2) =
    let diff = filter (`notElem` l2) l1
    in Right (List diff (length diff))
evalUnion Difference v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 =
        let diffChars = filter (`notElem` s2) s1
        in Right (List (map Character diffChars) (length diffChars))

evalUnion op l r = Left $ mkEvalError Nothing ("cannot apply " ++ show op ++ " to " ++ getValueType l ++ " and " ++ getValueType r)

prettyValue :: Value -> String
prettyValue (Integer n) = show n
prettyValue (Double d) = show d
prettyValue NaN = "NaN"
prettyValue Undefined = "undefined"
prettyValue (Boolean True) = "true"
prettyValue (Boolean False) = "false"
prettyValue v | Just s <- valueToString v = s
prettyValue (Character c) = [c]
prettyValue (List vals _) = "[" ++ intercalateWith ", " (map prettyValue vals) ++ "]"
prettyValue (Set vals _) = "{" ++ intercalateWith ", " (map prettyValue vals) ++ "}"
prettyValue (Object pairs) = "#{" ++ intercalateWith ", " (map (\(k,v) -> k ++ ": " ++ prettyValue v) pairs) ++ "}#"
prettyValue (AlgebraicDataType name vals) = name ++ "(" ++ intercalateWith ", " (map prettyValue vals) ++ ")"
prettyValue (Function _ _) = "<function>"
prettyValue (Lambda _ _) = "<lambda>"
prettyValue (ErrorVal m) = "Error(" ++ m ++ ")"
prettyValue (_) = "<value>"

intercalateWith :: String -> [String] -> String
intercalateWith _ [] = ""
intercalateWith _ [x] = x
intercalateWith sep (x:xs) = x ++ sep ++ intercalateWith sep xs
