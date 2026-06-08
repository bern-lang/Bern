{-
Module: Language.OperatorState

English:
  Where user-defined operators live between the pre-scan and the parser. This is
  the operator analogue of Language.PragmaState, and it solves the same problem in
  the same way.

  Bern lets a program declare its own infix operators -- symbolic ones like `^` and
  word ones like `dot` -- with `def (x ^ y) -> ...` and an optional fixity line
  `infix ^ groups right, binds tighter than *`. But `makeExprParser` (the engine
  behind expression parsing) needs to know every operator AND its precedence/
  associativity BEFORE it builds the expression parser. So, exactly like pragmas,
  the driver does a quick pre-scan of the source, records the operators it finds in
  the mutable cell below, and the parser reads that cell while it runs.

  An `OpSpec` is one operator's fixity: its name, whether it is a word operator, its
  associativity, and a numeric precedence `rank` (higher binds tighter). The rank is
  resolved from the friendly `binds tighter than *` wording at scan time, so by the
  time the parser sees it, it is just a number to sort levels by.

Português:
  Onde os operadores definidos pelo usuário moram entre a pré-varredura e o parser.
  Este é o análogo de operadores do Language.PragmaState, e resolve o mesmo problema
  da mesma forma.

  O Bern permite que um programa declare seus próprios operadores infixos -- os
  simbólicos como `^` e os de palavra como `dot` -- com `def (x ^ y) -> ...` e uma
  linha de fixidez opcional `infix ^ groups right, binds tighter than *`. Mas o
  `makeExprParser` (o motor por trás do parsing de expressões) precisa conhecer todo
  operador E sua precedência/associatividade ANTES de construir o parser de
  expressão. Então, igual aos pragmas, o driver faz uma pré-varredura rápida do
  fonte, registra os operadores encontrados na célula mutável abaixo, e o parser lê
  essa célula enquanto roda.

  Um `OpSpec` é a fixidez de um operador: seu nome, se é um operador de palavra, sua
  associatividade, e um `rank` numérico de precedência (maior prende mais forte). O
  rank é resolvido a partir da redação amigável `binds tighter than *` no momento da
  varredura, então quando o parser o vê, é só um número para ordenar os níveis.
-}
module Language.OperatorState
    ( OpAssoc(..)
    , OpSpec(..)
    , operatorSpecsRef
    , currentOperatorSpecs
    , setOperatorSpecs
    , addOperatorSpecs
    , clearOperatorSpecs
    ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import System.IO.Unsafe (unsafePerformIO)

-- How a repeated operator groups when written without parentheses.
--   OpLeft:  a ~ b ~ c  ==  (a ~ b) ~ c
--   OpRight: a ~ b ~ c  ==  a ~ (b ~ c)
--   OpNone:  a ~ b ~ c  is a parse error (must be parenthesised)
data OpAssoc = OpLeft | OpRight | OpNone
    deriving (Show, Eq)

-- One user-defined operator's fixity. `opRank` is an absolute precedence rank
-- (higher binds tighter); the parser sorts operator levels by it.
data OpSpec = OpSpec
    { opName  :: String     -- "^" or "dot"
    , opWord  :: Bool       -- True for word operators (lexed as identifiers)
    , opAssoc :: OpAssoc
    , opRank  :: Int
    } deriving (Show, Eq)

{-# NOINLINE operatorSpecsRef #-}
operatorSpecsRef :: IORef [OpSpec]
operatorSpecsRef = unsafePerformIO (newIORef [])

-- Read the currently-known operators. The Int argument is ignored, but callers
-- pass the parser's current source offset so the expression `currentOperatorSpecs
-- off` cannot be floated into a top-level CAF and evaluated once: it is re-run on
-- every use, reading the IORef fresh. (A plain `()` argument is not enough here,
-- because the call site is itself a CAF -- unlike PragmaState, whose caller is a
-- repeatedly-invoked monadic guard.)
{-# NOINLINE currentOperatorSpecs #-}
currentOperatorSpecs :: Int -> [OpSpec]
currentOperatorSpecs _ = unsafePerformIO (readIORef operatorSpecsRef)

-- Replace the whole operator set.
setOperatorSpecs :: [OpSpec] -> IO ()
setOperatorSpecs = writeIORef operatorSpecsRef

-- Merge more operators in, keeping any already present (an existing spec for a
-- name wins, so an explicit `infix` declaration is never clobbered by a later
-- default-fixity sighting of the same operator).
addOperatorSpecs :: [OpSpec] -> IO ()
addOperatorSpecs new = modifyIORef' operatorSpecsRef $ \old ->
    let names = map opName old
    in old ++ filter (\s -> opName s `notElem` names) new

clearOperatorSpecs :: IO ()
clearOperatorSpecs = writeIORef operatorSpecsRef []
