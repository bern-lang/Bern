{-
Module: Parsing.Parser

English:
  The parser: it reads Bern SOURCE TEXT and produces the AST (the Command and
  Expression trees from Language.Ast). Everything downstream -- the evaluator, the
  REPL, the bundler -- works on that AST, never on the raw text, so this module is
  the bridge from "characters a human typed" to "a structured program".

  It is built with `megaparsec`, a parser-combinator library. The idea of
  combinators is that a parser is just a value of type `Parser a` (an action that
  consumes some input and yields an `a`), and we build big parsers by gluing small
  ones together with ordinary functions and operators:
    * `p <|> q`        try p, and if it fails WITHOUT consuming input, try q
    * `many p` / `some p`   zero-or-more / one-or-more repetitions
    * `p `sepBy` sep`  a list of p separated by sep
    * `try p`          turn p into "all-or-nothing": if it fails midway, pretend it
                       consumed nothing, so a following <|> can still fire. This is
                       essential because Bern has overlapping syntax (a list, a
                       range, a cons, and a comprehension all start with `[`).

  Whitespace handling is explicit and matters: `sc`/`scn` are "space consumers",
  and the convention (from megaparsec) is that each token parser consumes the
  whitespace AFTER itself (`lexeme`/`symbol`). Two flavours exist because some
  places may cross newlines (inside blocks) and some must not (operators in the
  middle of an expression), which is how Bern can be newline-sensitive at the top
  level yet free-form inside a `do ... end`.

  Operator precedence (who binds tighter, `*` vs `+`) is handled by
  `makeExprParser` over an `operatorTable`, a list of levels from highest to lowest
  precedence.

Português:
  O parser: ele lê o TEXTO-FONTE de Bern e produz a AST (as árvores de Command e
  Expression de Language.Ast). Tudo a jusante -- o avaliador, o REPL, o empacotador
  -- trabalha sobre essa AST, nunca sobre o texto cru, então este módulo é a ponte
  de "caracteres que um humano digitou" para "um programa estruturado".

  Ele é construído com `megaparsec`, uma biblioteca de combinadores de parser. A
  ideia dos combinadores é que um parser é só um valor de tipo `Parser a` (uma ação
  que consome alguma entrada e entrega um `a`), e construímos parsers grandes
  colando pequenos com funções e operadores comuns:
    * `p <|> q`        tenta p e, se falhar SEM consumir entrada, tenta q
    * `many p` / `some p`   zero-ou-mais / uma-ou-mais repetições
    * `p `sepBy` sep`  uma lista de p separada por sep
    * `try p`          torna p "tudo ou nada": se falhar no meio, finge que não
                       consumiu nada, para que um <|> seguinte ainda possa disparar.
                       Isto é essencial porque Bern tem sintaxe sobreposta (uma
                       lista, um intervalo, um cons e uma comprehension começam todos
                       com `[`).

  O tratamento de espaços em branco é explícito e importa: `sc`/`scn` são
  "consumidores de espaço", e a convenção (do megaparsec) é que cada parser de token
  consome o espaço DEPOIS de si mesmo (`lexeme`/`symbol`). Existem dois sabores
  porque alguns lugares podem cruzar quebras de linha (dentro de blocos) e alguns
  não podem (operadores no meio de uma expressão), que é como o Bern pode ser
  sensível a quebras de linha no topo e ainda livre dentro de um `do ... end`.

  A precedência de operadores (quem prende mais forte, `*` vs `+`) é tratada por
  `makeExprParser` sobre um `operatorTable`, uma lista de níveis do mais alto para o
  mais baixo de precedência.
-}
module Parsing.Parser where

import Language.Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Void
import Control.Monad (void, mzero)
import Language.PragmaState (pragmaActive)

-- The concrete parser type: consumes a String, with no custom error component
-- (Void). Every parser in this file is a `Parser something`.
-- O tipo concreto de parser: consome uma String, sem componente de erro
-- customizado (Void). Todo parser neste arquivo é um `Parser algo`.
type Parser = Parsec Void String

{-
English:
  The whitespace and token toolkit that the rest of the parser is built on.

    * scn  consumes spaces, tabs and comments but NOT newlines. Use it where a
      newline should end a statement (the top level).
    * sc   consumes ALL whitespace including newlines. Use it inside blocks, where
      a program can span many lines freely.
    * lexeme / symbol / symbolNoNl wrap a parser so it also eats the trailing
      whitespace (lexeme runs an arbitrary parser then sc; symbol matches a fixed
      string then sc; symbolNoNl is the newline-stopping version).
    * keyword matches a reserved word and then checks it is NOT followed by more
      identifier characters, so `iffy` is not read as the keyword `if`.

  Both comment styles (`--`/`//` line comments and `/* */`, `{- -}` block comments)
  are recognised here so they can appear anywhere whitespace can.

Português:
  O kit de espaços em branco e tokens sobre o qual o resto do parser é construído.

    * scn  consome espaços, tabs e comentários mas NÃO quebras de linha. Use onde
      uma quebra de linha deve terminar um comando (o nível de topo).
    * sc   consome TODO espaço em branco, incluindo quebras de linha. Use dentro de
      blocos, onde um programa pode se estender por muitas linhas livremente.
    * lexeme / symbol / symbolNoNl envolvem um parser para que ele também coma o
      espaço em branco seguinte (lexeme roda um parser arbitrário e então sc; symbol
      casa uma string fixa e então sc; symbolNoNl é a versão que para na quebra de
      linha).
    * keyword casa uma palavra reservada e então confere que ela NÃO é seguida por
      mais caracteres de identificador, para que `iffy` não seja lido como a
      palavra-chave `if`.

  Os dois estilos de comentário (de linha `--`/`//` e de bloco `/* */`, `{- -}`) são
  reconhecidos aqui para que possam aparecer onde quer que espaço em branco possa.
-}
-- Space consumer that consumes spaces, tabs, and comments but NOT newlines
scn :: Parser ()
scn = L.space (void $ some (char ' ' <|> char '\t')) lineCmnt blockCmnt
    where
        lineCmnt  = L.skipLineComment "//" <|> L.skipLineComment "--"
        blockCmnt = L.skipBlockComment "/*" "*/" <|> L.skipBlockComment "{-" "-}"

-- Space consumer that also consumes newlines (for inside blocks)
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
    where
        lineCmnt  = L.skipLineComment "//" <|> L.skipLineComment "--"
        blockCmnt = L.skipBlockComment "/*" "*/" <|> L.skipBlockComment "{-" "-}"

-- Lexeme parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

-- Symbol parser (consumes all whitespace including newlines)
symbol :: String -> Parser String
symbol = L.symbol sc

-- Symbol parser that does NOT consume newlines (for operators in expressions)
symbolNoNl :: String -> Parser String
symbolNoNl = L.symbol scn

-- Keyword parser (ensures keyword is not followed by alphanumeric/underscore)
keyword :: String -> Parser String
keyword kw = lexeme $ try $ do
    _ <- string kw
    notFollowedBy (alphaNumChar <|> char '_')
    return kw

-- The loop keyword. `loop` is the preferred spelling; `for` is kept as an
-- alias for backwards compatibility. Both introduce the repeat/while/for-in
-- forms. The trailing boundary check stops identifiers like `loopback` or
-- `forms` from matching the keyword prefix, then trailing space (incl.
-- newlines) is consumed as `symbol` did.
loopKeyword :: Parser String
loopKeyword = (try (kw "loop") <|> try (kw "for")) <* sc
  where
    kw name = string name <* notFollowedBy (alphaNumChar <|> char '_')

{-
English:
  From here down to the operator table are the LEAF parsers -- the ones that read a
  single literal or simple construct: numbers, doubles, variables, booleans,
  strings, characters, objects (#{...}#), lists, sets, and parentheses. Each
  produces one Expression node. Doubles are tried before integers (so `1.5` is not
  read as the integer `1`), and `L.signed` lets a literal carry a leading minus.

  parseList and parseSet are the interesting ones: a single `[` opens four possible
  shapes -- a literal `[a,b]`, a range `[a..b]`, a cons `[h|t]`, and a comprehension
  `[x | x <- xs]` -- so they parse the first element and then branch on what follows,
  using `try` so a wrong guess backtracks cleanly.

Português:
  Daqui até a tabela de operadores estão os parsers FOLHA -- os que leem um único
  literal ou construção simples: números, doubles, variáveis, booleanos, strings,
  caracteres, objetos (#{...}#), listas, conjuntos e parênteses. Cada um produz um
  nó Expression. Doubles são tentados antes de inteiros (para que `1.5` não seja
  lido como o inteiro `1`), e `L.signed` permite que um literal carregue um menos à
  frente.

  parseList e parseSet são os interessantes: um único `[` abre quatro formas
  possíveis -- um literal `[a,b]`, um intervalo `[a..b]`, um cons `[h|t]` e uma
  comprehension `[x | x <- xs]` -- então eles parseiam o primeiro elemento e então
  ramificam pelo que vem depois, usando `try` para que um palpite errado retroceda
  de forma limpa.
-}
-- Parser for numbers
parseNumber :: Parser Expression
parseNumber = lexeme $ do
    n <- L.signed sc L.decimal
    return $ Number n

parseDouble :: Parser Expression
parseDouble = lexeme $ do
    d <- L.signed sc L.float
    return $ DoubleNum d

-- Parser for variables
parseVariable :: Parser Expression
parseVariable = lexeme $ do
    firstChar <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_')
    return $ Variable (firstChar : rest)

-- Parser for Boolean literals
parseBoolean :: Parser Expression
parseBoolean = lexeme $ (string "true" >> return (BoolLiteral True))
                     <|> (string "false" >> return (BoolLiteral False))

-- Parser for string literals
parseString :: Parser Expression
parseString = lexeme $ do
    _ <- char '"'
    content <- manyTill L.charLiteral (char '"')
    return $ StringLiteral content

-- Parser for character literals
parseChar :: Parser Expression
parseChar = lexeme $ do
    _ <- char '\''
    c <- L.charLiteral
    _ <- char '\''
    return $ CharLiteral c

-- Key/value pairs for object literals. Uses the newline-tolerant space consumer
-- so objects can be written across multiple lines.
parseKeyValuePair :: Parser (String, Expression)
parseKeyValuePair = do
    key <- L.lexeme sc parseKey
    _ <- symbol ":"
    value <- parseExpression
    return (key, value)
  where
    parseKey = parseStringKey <|> parseIdentKey
    -- Allow quoted string keys: "key"
    parseStringKey = do
        _ <- char '"'
        content <- manyTill L.charLiteral (char '"')
        return content
    -- Or bare identifiers/digits: foo, foo1
    parseIdentKey = some (letterChar <|> char '_' <|> digitChar)

-- Parser for object literals #{ key: value, ... }#
parseObject :: Parser Expression
parseObject = try parseHashBraces <|> parseBareHash
    where
        parseHashBraces = do
            _ <- symbol "#{"
            pairs <- parseKeyValuePair `sepBy` symbol ","
            -- Allow closing after any intervening whitespace/newlines
            _ <- sc *> symbol "}#"
            return $ ObjectLiteral pairs
        parseBareHash = do
            _ <- lexeme (char '#')
            pairs <- parseKeyValuePair `sepBy` symbol ","
            -- Allow closing after any intervening whitespace/newlines
            _ <- sc *> symbol "#"
            return $ ObjectLiteral pairs

-- Parser for the bracket forms:
--   [a, b, c]            list literal
--   [start .. end]       range
--   [h, ... | tail]      cons expression (prepend head(s) onto a list)
--   [expr | x <- xs, p]  list comprehension (needs at least one generator)
parseList :: Parser Expression
parseList = do
    _ <- symbolNoNl "["
    try (symbolNoNl "]" >> return (ListLiteral []))   -- empty list
        <|> nonEmpty
  where
    nonEmpty = do
        first <- parseExpression
        choice
            [ try $ do  -- range
                _ <- symbolNoNl ".."
                end <- parseExpression
                _ <- symbolNoNl "]"
                return (Range first end)
            , do        -- single head followed by '|': comprehension or cons
                _ <- symbolNoNl "|"
                parseComprehensionTail first <|> parseConsTail [first]
            , do        -- more comma-separated elements, then '|' cons or ']' literal
                rest <- many (symbolNoNl "," *> parseExpression)
                let heads = first : rest
                (symbolNoNl "|" *> parseConsTail heads)
                    <|> (symbolNoNl "]" *> return (ListLiteral heads))
            ]
    parseConsTail heads = do
        tailExpr <- parseExpression
        _ <- symbolNoNl "]"
        return (ListCons heads tailExpr)
    -- A comprehension only if the part after '|' contains a generator; otherwise
    -- this fails (under try) so the cons parser runs on the same input.
    parseComprehensionTail headExpr = try $ do
        quals <- parseQual `sepBy1` symbolNoNl ","
        if any isGenerator quals
            then symbolNoNl "]" >> return (ListComp headExpr quals)
            else fail "list comprehension requires a generator (x <- xs)"

-- A single comprehension qualifier: a generator `pattern <- source` or a guard.
parseQual :: Parser CompQual
parseQual = try parseGenerator <|> (CompGuard <$> parseExpression)
  where
    parseGenerator = do
        pat <- parsePattern
        _ <- symbolNoNl "<-"
        src <- parseExpression
        return (CompGen pat src)

isGenerator :: CompQual -> Bool
isGenerator (CompGen _ _) = True
isGenerator _             = False

-- Parser for set literals {a, b, c} and the set cons form {h, ... | tail}.
parseSet :: Parser Expression
parseSet = do
    _ <- symbolNoNl "{"
    try (symbolNoNl "}" >> return (SetLiteral []))   -- empty set
        <|> nonEmpty
  where
    nonEmpty = do
        first <- parseExpression
        rest <- many (symbolNoNl "," *> parseExpression)
        let heads = first : rest
        (symbolNoNl "|" *> parseSetConsTail heads)
            <|> (symbolNoNl "}" *> return (SetLiteral heads))
    parseSetConsTail heads = do
        tailExpr <- parseExpression
        _ <- symbolNoNl "}"
        return (SetCons heads tailExpr)

-- Parser for parentheses
parseParens :: Parser Expression
parseParens = between (symbolNoNl "(") (symbolNoNl ")") parseExpression

-- Parse binary operations
binary :: String -> BinaryOperation -> Operator Parser Expression
binary name f = InfixL (BinaryOperator f <$ symbolNoNl name)

prefix :: String -> UnaryOperation -> Operator Parser Expression
prefix name f = Prefix (UnaryOperator f <$ symbolNoNl name)

-- Written-word alias for an operator (e.g. `is-greater-or-equal` for >=).
-- The boundary check excludes '-' as well as identifier characters so that a
-- shorter word (`and`) never matches the prefix of a longer one (`and-do`).
-- The written-word operators are switched off by the `no-written-operators`
-- pragma. `guardWritten` runs at parse time (via getParserState, so the flag is
-- re-read on every attempt, not captured once when the operator table is built)
-- and makes the whole word-operator fail, leaving the word free as an ordinary
-- identifier.
guardWritten :: Parser ()
guardWritten = getParserState >>= \_ ->
    if pragmaActive "no-written-operators" () then mzero else pure ()

wordOp :: String -> Parser ()
wordOp name = guardWritten >> (void $ lexeme $ try $ do
    _ <- string name
    notFollowedBy (alphaNumChar <|> char '_' <|> char '-'))

wordBinary :: String -> BinaryOperation -> Operator Parser Expression
wordBinary name f = InfixL (BinaryOperator f <$ wordOp name)

wordPrefix :: String -> UnaryOperation -> Operator Parser Expression
wordPrefix name f = Prefix (UnaryOperator f <$ wordOp name)

-- The result separator after a function/lambda parameter list: the `->` arrow
-- or its word synonyms `returns` / `such-that` (and their Brazilian Portuguese
-- forms `retorna` / `tal-que`).
arrowSep :: Parser ()
arrowSep = void (symbolNoNl "->")
    <|> void (keyword "returns") <|> wordOp "such-that"
    <|> void (keyword "retorna") <|> wordOp "tal-que"

-- The result separator in a case branch: `=` or the word synonyms.
caseSep :: Parser ()
caseSep = void (symbol "=")
    <|> void (keyword "returns") <|> wordOp "such-that"
    <|> void (keyword "retorna") <|> wordOp "tal-que"

-- Subtraction, but it must not swallow the '-' of '->' (the arrow used by
-- lambdas, function bodies, and `when` guards). Without this, a guard like
-- `when n > 0 ->` would parse `0 -` as the start of a subtraction.
subtractOp :: Operator Parser Expression
subtractOp = InfixL (BinaryOperator Subtract <$ try (symbolNoNl "-" <* notFollowedBy (char '>')))

-- The )| pipe operator: `x )| f` desugars to f(x), and `x )| f(a, b)` to
-- f(x, a, b) (the piped value becomes the first argument, Elixir-style).
-- `try` is required because ")|" begins with ")", a common terminator, so a
-- partial match must not consume input.
pipeOp :: Operator Parser Expression
pipeOp = InfixL (pipeInto <$ (void (try (symbolNoNl ")|")) <|> wordOp "and-do" <|> wordOp "e-faca"))

pipeInto :: Expression -> Expression -> Expression
pipeInto lhs rhs =
    let (mpos, core) = unwrap rhs
        result = case core of
            FunctionCall name args -> FunctionCall name (lhs : args)
            Variable name          -> FunctionCall name [lhs]
            other                  -> Apply other [lhs]
    in case mpos of
        Just p  -> WithPos p result
        Nothing -> result
  where
    -- Capture the outermost source position (if any) and strip WithPos wrappers
    -- to inspect the underlying expression.
    unwrap (WithPos p e) = (Just p, strip e)
    unwrap e             = (Nothing, e)
    strip (WithPos _ e)  = strip e
    strip e              = e

{-
English:
  Operator precedence, expressed as a TABLE for makeExprParser. The table is a list
  of levels; earlier levels bind TIGHTER. So multiplication sits above addition,
  which sits above comparison, which sits above `&&`/`||`, which sits above the
  pipe. Within a level, `InfixL` means left-associative (`a - b - c` = `(a-b)-c`).

  Each operator is listed TWICE -- once as a symbol (`binary "*" Multiply`) and once
  as its written word (`wordBinary "times" Multiply`, plus Portuguese synonyms) --
  because Bern lets you write either; they parse to the same AST node. The
  `no-written-operators` pragma is honoured inside `wordOp` (see guardWritten), so
  the word forms simply stop matching when that pragma is on.

  There are two tables: `operatorTable` (the normal one) and `operatorTableNoIs`,
  which drops `is` as an equality operator. The latter is used when parsing the
  TARGET of a `case ... is`, so the `is` there is read as the case keyword, not as
  `==`.

Português:
  A precedência de operadores, expressa como uma TABELA para o makeExprParser. A
  tabela é uma lista de níveis; níveis anteriores prendem MAIS FORTE. Então a
  multiplicação fica acima da adição, que fica acima da comparação, que fica acima
  de `&&`/`||`, que fica acima do pipe. Dentro de um nível, `InfixL` significa
  associativo à esquerda (`a - b - c` = `(a-b)-c`).

  Cada operador é listado DUAS vezes -- uma como símbolo (`binary "*" Multiply`) e
  uma como sua palavra escrita (`wordBinary "times" Multiply`, mais sinônimos em
  português) -- porque o Bern deixa você escrever qualquer um dos dois; eles parseiam
  para o mesmo nó da AST. O pragma `no-written-operators` é respeitado dentro do
  `wordOp` (ver guardWritten), então as formas em palavra simplesmente param de casar
  quando esse pragma está ligado.

  Há duas tabelas: `operatorTable` (a normal) e `operatorTableNoIs`, que descarta o
  `is` como operador de igualdade. Esta última é usada ao parsear o ALVO de um
  `case ... is`, para que o `is` ali seja lido como a palavra-chave do case, não como
  `==`.
-}
-- Prefix operators, symbolic and written-word forms. Shared by both tables.
prefixOps :: [Operator Parser Expression]
prefixOps =
    [ prefix "-" Negate
    , prefix "!" Not
    , prefix "::" TypeOf
    , prefix ":>" SizeOf
    , wordPrefix "negate" Negate
    , wordPrefix "not" Not
    , wordPrefix "typeof" TypeOf
    , wordPrefix "length" SizeOf
    -- Brazilian Portuguese forms
    , wordPrefix "negativo" Negate
    , wordPrefix "nao" Not
    , wordPrefix "tipo-de" TypeOf
    , wordPrefix "tamanho" SizeOf
    ]

-- Operator precedence levels above equality (shared; no `is` ambiguity here).
highPrecedenceOps :: [[Operator Parser Expression]]
highPrecedenceOps =
    [ prefixOps
    , [ binary "*" Multiply, binary "/" Divide, binary "%" Modulo
      , wordBinary "times" Multiply, wordBinary "divided-by" Divide, wordBinary "modulo" Modulo
      , wordBinary "vezes" Multiply, wordBinary "dividido-por" Divide, wordBinary "resto" Modulo
      ]
    , [ binary "+" Add, subtractOp
      , wordBinary "plus" Add, wordBinary "minus" Subtract
      , wordBinary "mais" Add, wordBinary "menos" Subtract
      ]
    , [ binary "<>" Concatenation, binary "<|" Union, binary "|>" Intersection, binary "</>" Difference
      , wordBinary "concat" Concatenation, wordBinary "union" Union
      , wordBinary "intersect" Intersection, wordBinary "difference" Difference
      , wordBinary "concatenar" Concatenation, wordBinary "uniao" Union
      , wordBinary "intersecao" Intersection, wordBinary "diferenca" Difference
      ]
    -- Longer words come first so the boundary check never lets a shorter word
    -- (maior-que) match the prefix of a longer one (maior-ou-igual).
    , [ wordBinary "is-greater-or-equal" GreaterThanEq, wordBinary "is-less-or-equal" LessThanEq
      , wordBinary "maior-ou-igual" GreaterThanEq, wordBinary "menor-ou-igual" LessThanEq
      , binary ">=" GreaterThanEq, binary "<=" LessThanEq
      , binary ">" GreaterThan, binary "<" LessThan
      , wordBinary "is-greater" GreaterThan, wordBinary "is-less" LessThan
      , wordBinary "maior-que" GreaterThan, wordBinary "menor-que" LessThan
      ]
    ]

-- Levels at and below equality. `is` (the word form of ==) is only included
-- when parsing outside a `case ... is` target.
lowPrecedenceOps :: [Operator Parser Expression] -> [[Operator Parser Expression]]
lowPrecedenceOps equalityExtra =
    [ [ binary "==" Equal, binary "!=" Different
      , wordBinary "equals" Equal, wordBinary "not-equals" Different
      , wordBinary "igual" Equal, wordBinary "diferente" Different
      ] ++ equalityExtra
    , [ binary "&&" And, wordBinary "and" And, wordBinary "e" And ]
    , [ binary "||" Or, wordBinary "or" Or, wordBinary "ou" Or ]
    , [ pipeOp ]
    ]

operatorTable :: [[Operator Parser Expression]]
operatorTable = highPrecedenceOps ++ lowPrecedenceOps [binary "is" Equal]

operatorTableNoIs :: [[Operator Parser Expression]]
operatorTableNoIs = highPrecedenceOps ++ lowPrecedenceOps []

{-
English:
  A "term" is a base value PLUS any postfix suffixes attached to it: index access
  `xs[0]` and function application `f(a)`. parseTerm parses one base term and then
  `many parseSuffix`, folding each suffix on so that `add(2)(3)` and `m["k"][0]`
  chain correctly (this chaining is what makes curried calls like `add(2)(3)` work
  at the syntax level). It also records the source position with WithPos for error
  messages.

  parseBaseTerm is the ordered menu of what a base term can be. Order matters: more
  specific / longer forms are tried first (each wrapped in `try` so a failed attempt
  backtracks), and parseParens is the last resort.

Português:
  Um "termo" é um valor base MAIS quaisquer sufixos pós-fixados a ele: acesso por
  índice `xs[0]` e aplicação de função `f(a)`. parseTerm parseia um termo base e
  então `many parseSuffix`, dobrando cada sufixo para que `add(2)(3)` e `m["k"][0]`
  encadeiem corretamente (esse encadeamento é o que faz chamadas curried como
  `add(2)(3)` funcionarem no nível da sintaxe). Ele também registra a posição no
  fonte com WithPos para mensagens de erro.

  parseBaseTerm é o cardápio ordenado do que um termo base pode ser. A ordem importa:
  formas mais específicas / mais longas são tentadas primeiro (cada uma envolta em
  `try` para que uma tentativa falha retroceda), e parseParens é o último recurso.
-}
-- Parse terms (numbers, variables, parentheses)
parseTerm :: Parser Expression
parseTerm = do
    pos <- getSourcePos
    base <- parseBaseTerm
    suffixes <- many parseSuffix
    return $ WithPos pos (foldl applySuffix base suffixes)
  where
    applySuffix e (Left idx)    = Index e idx
    applySuffix e (Right args)  = Apply e args

-- A postfix on a term: `[i]` index access, or `(args)` application (which makes
-- curried calls like `add(2)(3)` work).
parseSuffix :: Parser (Either Expression [Expression])
parseSuffix = (Left <$> parseIndexAccess) <|> (Right <$> parseCallSuffix)

parseCallSuffix :: Parser [Expression]
parseCallSuffix = try $ do
    _ <- symbolNoNl "("
    args <- parseExpression `sepBy` symbolNoNl ","
    _ <- symbolNoNl ")"
    return args

-- Parse base terms without index access
parseBaseTerm :: Parser Expression
parseBaseTerm = try parseObject
    <|> try parseLambda
    <|> try parseCaseExpression
    <|> try parseIfExpression
    <|> try parseDouble
    <|> try parseNumber
    <|> try parseString
    <|> try parseChar
    <|> try parseBoolean
    <|> try parseList
    <|> try parseSet
    <|> try parseGetHostMachine
    <|> try parseGetCurrentDir
    <|> try parseReadFile
    <|> try parseKeys
    <|> try parseMakeError
    <|> try parseFmap
    <|> try parseFunctionCallOrVar
    <|> parseParens

-- Parse case expressions:
-- case expr is pat = expr | pat = expr end
-- The trailing "end" is optional in one-line usage.
parseCaseExpression :: Parser Expression
parseCaseExpression = do
    _ <- symbol "case"
    target <- parseExpressionNoIs
    _ <- symbol "is"
    branches <- parseCaseBranch `sepBy1` symbol "|"
    _ <- optional (symbol "end")
    return $ CaseExpr target branches
  where
    parseCaseBranch :: Parser CaseBranch
    parseCaseBranch = do
        pat <- parsePattern
        mGuard <- optional (keyword "when" *> parseExpression)
        _ <- caseSep
        body <- parseExpression
        return (CaseBranch pat mGuard body)

-- Parse conditional expressions:
-- if cond then expr else expr end
-- if cond then expr else if cond2 then expr2 else expr3 end
parseIfExpression :: Parser Expression
parseIfExpression = do
    _ <- symbol "if"
    cond <- parseExpression
    _ <- symbol "then"
    thenExpr <- parseExpression
    elseExpr <- parseElseExpr
    _ <- symbol "end"
    return $ IfExpr cond thenExpr elseExpr
  where
    parseElseExpr :: Parser Expression
    parseElseExpr = try parseElseIf <|> parseElseOnly

    parseElseIf :: Parser Expression
    parseElseIf = do
        _ <- L.symbol scn "else"
        _ <- symbolNoNl "if"
        cond <- parseExpression
        _ <- symbol "then"
        thenExpr <- parseExpression
        elseExpr <- parseElseExpr
        return $ IfExpr cond thenExpr elseExpr

    parseElseOnly :: Parser Expression
    parseElseOnly = do
        _ <- symbol "else"
        parseExpression

parseCBinding ::  Parser Command
parseCBinding = do
    _ <- symbol "foreign"
    
    -- Parse the function name
    funcName <- lexeme $ do
        first <- letterChar <|> char '_'
        rest <- many (alphaNumChar <|> char '_')
        return (first :  rest)
    
    -- Parse the library path (the .so/.dll file)
    _ <- symbolNoNl "("
    libPathExpr <- parseExpression
    mArgs <- optional $ do
        _ <- symbolNoNl ","
        parseTypeString `sepBy1` symbolNoNl ","
    _ <- symbolNoNl ")"

    -- Arrow indicating return type
    _ <- symbolNoNl "->"
    retType <- parseTypeString

    let argTypes = maybe [] id mArgs
    -- Return a command that loads the C function
    return $ CForeignDecl funcName libPathExpr argTypes retType
  where
    parseStringLiteral ::  Parser String
    parseStringLiteral = lexeme $ do
        _ <- char '"'
        content <- manyTill L.charLiteral (char '"')
        return content
    
    parseTypeString :: Parser String
    parseTypeString = lexeme $ do
        _ <- char '"'
        typeStr <- some (letterChar <|> char '_')
        _ <- char '"'
        return typeStr

-- Parse a variable or a function call (name(args...))
parseFunctionCallOrVar :: Parser Expression
parseFunctionCallOrVar = lexeme $ do
    namespace <- optional $ try $ do
        nsFirst <- letterChar <|> char '_'
        nsRest <- many (alphaNumChar <|> char '_' <|> char '/')
        _ <- char ':'
        return (nsFirst : nsRest)
    firstChar <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_')
    let baseName = firstChar : rest
    let name = case namespace of
                Just ns -> ns ++ ":" ++ baseName
                Nothing -> baseName
    margs <- optional $ between (symbolNoNl "(") (symbolNoNl ")") (parseExpression `sepBy` symbolNoNl ",")
    case margs of
        Just args -> return $ FunctionCall name args
        Nothing   -> return $ Variable name

-- Parse a lambda: either \x,y -> expr or (x,y) -> expr
parseLambda :: Parser Expression
parseLambda = try backslashLambda <|> try keywordLambda <|> try parenLambda
  where
    -- \x -> body
    backslashLambda = char '\\' >> lambdaTail
    -- lambda x such-that body
    keywordLambda = keyword "lambda" >> lambdaTail
    -- (x) -> body
    parenLambda = do
        _ <- symbol "("
        params <- parsePatterns
        _ <- symbol ")"
        mGuard <- optional (keyword "when" *> parseExpression)
        _ <- arrowSep
        body <- parseExpression
        return $ LambdaExpr params mGuard body
    -- shared params/guard/arrow/body tail for the \ and lambda forms
    lambdaTail = do
        params <- parsePatterns
        mGuard <- optional (keyword "when" *> parseExpression)
        _ <- arrowSep
        body <- parseExpression
        return $ LambdaExpr params mGuard body

parsePatterns :: Parser [Pattern]
parsePatterns = parsePattern `sepBy` symbol ","

{-
English:
  Parse a PATTERN (the left side of a match, see Ast.Pattern). The order of the
  alternatives encodes priority: `_` is the wildcard; an ADT constructor pattern
  (capitalised name, maybe with sub-patterns); list/set patterns (which themselves
  handle the empty, cons `[h|t]`, and fixed-shape cases); signed number literals
  (doubles before ints so `-1.5` isn't read as `-1`); booleans; char and string
  literals; and finally a bare identifier, which becomes a variable binding `PVar`.

Português:
  Parseia um PADRÃO (o lado esquerdo de um casamento, ver Ast.Pattern). A ordem das
  alternativas codifica a prioridade: `_` é o coringa; um padrão de construtor de ADT
  (nome com maiúscula, talvez com sub-padrões); padrões de lista/conjunto (que eles
  mesmos tratam os casos vazio, cons `[h|t]` e de forma fixa); literais numéricos com
  sinal (doubles antes de ints para que `-1.5` não seja lido como `-1`); booleanos;
  literais de char e string; e por fim um identificador solto, que vira um vínculo de
  variável `PVar`.
-}
parsePattern :: Parser Pattern
parsePattern = lexeme $ choice
    [ string "_" >> return PWildcard
    , try parseADTConstructorPattern
    , try parseListPattern  -- [head|tail] or [] or [a, b, c]
    , try parseSetPattern   -- {head|tail} or {} or {a, b, c}
    , try $ PDouble <$> L.signed scn L.float    -- doubles first so -1.5 isn't read as PInt -1
    , try $ PInt <$> L.signed scn L.decimal     -- signed so negative literal patterns parse
    , try $ (string "true" >> return (PBool True))
    , try $ (string "false" >> return (PBool False))
    , try $ do _ <- char '\''; c <- L.charLiteral; _ <- char '\''; return (PChar c)
    , try $ do _ <- char '"'; s <- manyTill L.charLiteral (char '"'); return (PString s)
    , do firstChar <- letterChar <|> char '_'
         rest <- many (alphaNumChar <|> char '_')
         return (PVar (firstChar:rest))
    ]

parseADTConstructorPattern :: Parser Pattern
parseADTConstructorPattern = do
    ctorName <- lexeme $ do
        first <- upperChar
        rest <- many (alphaNumChar <|> char '_')
        return (first:rest)
    args <- optional $ between (symbolNoNl "(") (symbolNoNl ")") (parsePatterns)
    case args of
        Just ps -> return $ PADT ctorName ps
        Nothing -> return $ PADT ctorName []

-- Parse list patterns: [], [head|tail], [a, b, c]
parseListPattern :: Parser Pattern
parseListPattern = do
    _ <- char '['
    sc
    result <- choice
        [ try $ do  -- Empty list
            _ <- char ']'
            return (PList [])
        , try $ do  -- Cons pattern [head|tail]
            headPat <- parsePattern
            sc
            _ <- char '|'
            sc
            tailPat <- parsePattern
            sc
            _ <- char ']'
            return (PCons headPat tailPat)
        , do  -- List of patterns [a, b, c]
            pats <- parsePattern `sepBy` symbol ","
            _ <- char ']'
            return (PList pats)
        ]
    return result

-- Parse set patterns: {}, {head|tail}, {a, b, c}
parseSetPattern :: Parser Pattern
parseSetPattern = do
    _ <- char '{'
    sc
    result <- choice
        [ try $ do  -- Empty set
            _ <- char '}'
            return (PSet [])
        , try $ do  -- Set cons pattern {head|tail}
            headPat <- parsePattern
            sc
            _ <- char '|'
            sc
            tailPat <- parsePattern
            sc
            _ <- char '}'
            return (PSetCons headPat tailPat)
        , do  -- Set of patterns {a, b, c}
            pats <- parsePattern `sepBy` symbol ","
            _ <- char '}'
            return (PSet pats)
        ]
    return result

-- Parse index access [expr]
parseIndexAccess :: Parser Expression
parseIndexAccess = do
    _ <- char '['
    idx <- parseExpression
    _ <- char ']'
    return idx

parseExpression :: Parser Expression
parseExpression = do
    pos <- getSourcePos
    expr <- makeExprParser (lexeme parseTerm) operatorTable
    return $ WithPos pos expr

parseExpressionNoIs :: Parser Expression
parseExpressionNoIs = do
    pos <- getSourcePos
    expr <- makeExprParser (lexeme parseTerm) operatorTableNoIs
    return $ WithPos pos expr

parseVarWithIndices :: Parser (String, [Expression])
parseVarWithIndices = do
    firstChar <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_')
    let varName = firstChar : rest
    idxs <- many parseIndexAccess
    return (varName, idxs)

parseAssignment :: Parser Command
parseAssignment = try parseIndexedAssign <|> try parseGlobalAssign <|> parseSimpleAssign
  where
    parseIndexedAssign = try $ do
        (varName, idxs) <- lexeme parseVarWithIndices
        if null idxs then fail "no indices" else pure ()
        _ <- optional scn
        _ <- void (symbol "=") <|> wordOp "be" <|> wordOp "recebe"
        _ <- optional sc
        expr <- parseExpression
        return $ AssignIndex varName idxs expr
    parseGlobalAssign = do
        (varName, idxs) <- lexeme parseVarWithIndices
        if not (null idxs) then fail "indexed" else pure ()
        _ <- optional scn
        _ <- symbol ":="
        _ <- optional sc
        expr <- parseExpression
        return $ GlobalAssign varName expr
    parseSimpleAssign = do
        (varName, idxs) <- lexeme parseVarWithIndices
        if not (null idxs) then fail "indexed" else pure ()
        _ <- optional scn
        _ <- void (symbol "=") <|> wordOp "be" <|> wordOp "recebe"
        _ <- optional sc
        inputCall <- optional $ try $ do
            _ <- keyword "input"
            _ <- symbolNoNl "("
            promptExpr <- optional parseExpression
            _ <- symbolNoNl ")"
            return promptExpr
        case inputCall of
            Just promptExpr -> 
                let prompt = case promptExpr of
                               Just p  -> p
                               Nothing -> StringLiteral ""
                in return $ Input varName prompt
            Nothing -> do
                expr <- parseExpression
                return $ Assign varName expr

parsePrint :: Parser Command
parsePrint = do
    _ <- symbol "print"
    expr <- parseExpression
    return $ Print expr

parseFunctionDef :: Parser Command
parseFunctionDef = do
    _ <- symbol "def"
    name <- lexeme $ do
        first <- letterChar <|> char '_'
        rest <- many (alphaNumChar <|> char '_')
        return (first:rest)
    _ <- symbol "("
    params <- parsePatterns
    _ <- symbol ")"
    mGuard <- optional (keyword "when" *> parseExpression)
    body <- (arrowSep >> (BodyExpr <$> parseExpression))
        <|> (symbol "do" >> (BodyBlock <$> parseBlockCommands) <* symbol "end")
    return $ FunctionDef name (Clause params mGuard body)

parseConditional :: Parser Command
parseConditional = do
    _ <- symbol "if"
    cond <- parseExpression
    _ <- symbol "then"
    thenCmd <- parseBlockCommands
    elseCmd <- parseElseChain
    return $ Conditional cond thenCmd elseCmd

-- Handles "else if" chains ending with an "else" (or no else -> Skip)
parseElseChain :: Parser Command
parseElseChain = try parseElseIf <|> parseElseOnly <|> parseNoElse
  where
    parseElseIf = do
        _ <- L.symbol scn "else"
        _ <- symbolNoNl "if"
        cond <- parseExpression
        _ <- symbol "then"
        thenCmd <- parseBlockCommands
        elseCmd <- parseElseChain
        return $ Conditional cond thenCmd elseCmd

    parseElseOnly = do
        _ <- symbol "else"
        elseCmd <- parseBlockCommands
        _ <- symbol "end"
        return elseCmd

    parseNoElse = do
        _ <- symbol "end"
        return Skip

parseWriteFile :: Parser Command
parseWriteFile = do
    _ <- try (symbol "write_file")
    _ <- symbolNoNl "("
    filenameExpr <- parseExpression
    _ <- symbolNoNl ","
    contentExpr <- parseExpression
    _ <- symbolNoNl ")"
    return $ WriteFile filenameExpr contentExpr

parseGetHostMachine :: Parser Expression
parseGetHostMachine = do
    _ <- try (symbol "get_host_machine")
    _ <- symbolNoNl "("
    _ <- symbolNoNl ")"
    return GetHostMachine

parseGetCurrentDir :: Parser Expression
parseGetCurrentDir = do
    _ <- try (symbol "get_current_dir")
    _ <- symbolNoNl "("
    _ <- symbolNoNl ")"
    return GetCurrentDir

parseReadFile :: Parser Expression
parseReadFile = do
    _ <- try (symbol "read_file")
    _ <- symbolNoNl "("
    filenameExpr <- parseExpression
    _ <- symbolNoNl ")"
    return $ ReadFile filenameExpr

parseKeys :: Parser Expression
parseKeys = do
    _ <- try (symbol "keys")
    _ <- symbolNoNl "("
    objExpr <- parseExpression
    _ <- symbolNoNl ")"
    return $ Keys objExpr

parseMakeError :: Parser Expression
parseMakeError = do
    _ <- try (symbol "error")
    _ <- symbolNoNl "("
    msgExpr <- parseExpression
    _ <- symbolNoNl ")"
    return $ MakeError msgExpr

parseFmap :: Parser Expression
parseFmap = do
    _ <- try (symbol "fmap")
    _ <- symbolNoNl "("
    collectionExpr <- parseExpression
    _ <- symbolNoNl ","
    functionExpr <- parseExpression
    _ <- symbolNoNl ")"
    return $ Fmap collectionExpr functionExpr



parseAlgebraicDataType :: Parser Command
parseAlgebraicDataType = do
    _ <- symbol "adt"
    iterativeKw <- optional (symbol "iterative")
    let isIterative = case iterativeKw of
            Just _  -> True
            Nothing -> False
    typeName <- lexeme $ do
        firstChar <- letterChar <|> char '_'
        rest <- many (alphaNumChar <|> char '_')
        return (firstChar : rest)
    _ <- symbol "="
    constructors <- parseConstructor `sepBy1` symbol "|"
    return $ AlgebraicTypeDef (ADTDef isIterative typeName constructors)
  where
    parseConstructor = do
        consName <- lexeme $ do
            firstChar <- letterChar <|> char '_'
            rest <- many (alphaNumChar <|> char '_')
            return (firstChar : rest)
        argTypes <- many parseType
        return $ ADTConstructor consName argTypes

    parseType = lexeme $ choice
        [ string "Int"    >> return TInt
        , string "Double" >> return TDouble
        , string "Bool"   >> return TBool
        , string "Char"   >> return TChar
        , string "String" >> return TString
        , string "List"   >> return (TCustom "List")
        , string "Set"    >> return (TCustom "Set")
        , do
            first <- letterChar <|> char '_'
            rest <- many (alphaNumChar <|> char '_')
            return (TCustom (first:rest))
        ]

parseRepeat :: Parser Command
parseRepeat = do
    _ <- loopKeyword
    countExpr <- parseExpression
    _ <- symbol "do"
    cmd <- parseBlockCommands
    _ <- symbol "end"
    -- If it's a simple number (possibly wrapped with WithPos), use Repeat; otherwise use While (for conditions)
    case stripPos countExpr of
        Number _ -> return $ Repeat countExpr cmd
        _        -> return $ While countExpr cmd

-- Remove WithPos wrapper to inspect underlying expression
stripPos :: Expression -> Expression
stripPos (WithPos _ e) = stripPos e
stripPos e = e

parseForIn :: Parser Command
parseForIn = do
    _ <- loopKeyword
    varName <- lexeme $ some letterChar
    idxVar <- optional (symbol "," >> lexeme (some letterChar))
    _ <- symbol ":" <|> symbol "in"
    collection <- parseExpression
    _ <- symbol "do"
    body <- parseBlockCommands
    _ <- symbol "end"
    case idxVar of
        Just i  -> return $ ForInCount varName i collection body
        Nothing -> return $ ForIn varName collection body

parseForCond :: Parser Command
parseForCond = do
    _ <- loopKeyword
    cond <- parseExpression
    _ <- symbol "do"
    cmd <- parseBlockCommands
    _ <- symbol "end"
    return $ While cond cmd

{-
English:
  Parse ONE statement. This is the menu of every Command form (assignment, import,
  function definition, conditional, the loop variants, an ADT declaration, a
  foreign declaration, ...), tried in order with `try` so a partial match of one
  form backtracks and the next is attempted. The final fallback is `AutoPrint <$>
  parseExpression`: a line that is just a bare expression becomes an auto-print,
  which is why typing `1 + 1` at the top level shows its value.

  parseBlockCommands / parseCommands build a whole program by parsing many single
  commands and chaining them with `Concat`. Block parsing stops at `end`/`else`;
  top-level parsing stops at end-of-input.

Português:
  Parseia UM comando. Este é o cardápio de toda forma de Command (atribuição, import,
  definição de função, condicional, as variantes de laço, uma declaração de ADT, uma
  declaração foreign, ...), tentadas em ordem com `try` para que um casamento parcial
  de uma forma retroceda e a próxima seja tentada. O fallback final é
  `AutoPrint <$> parseExpression`: uma linha que é só uma expressão solta vira um
  auto-print, que é por que digitar `1 + 1` no topo mostra o valor.

  parseBlockCommands / parseCommands constroem um programa inteiro parseando muitos
  comandos únicos e os encadeando com `Concat`. O parsing de bloco para em
  `end`/`else`; o parsing de topo para no fim da entrada.
-}
parseSingleCommand :: Parser Command
parseSingleCommand = try parseCBinding
               <|> try parseWriteFile
               <|> try parseAssignment
               <|> try parseImport
               <|> try parseFunctionDef
               <|> try parseReturn
               <|> try parsePrint
               <|> try parseConditional
               <|> try parseForIn
               <|> try parseRepeat
               <|> try parseForCond
               <|> try parseAlgebraicDataType
               <|> (AutoPrint <$> parseExpression)

parseImport :: Parser Command
parseImport = do
    _ <- symbol "import"
    name <- lexeme $ some (letterChar <|> char '_' <|> char '/')
    alias <- optional $ try $ do
        _ <- keyword "as"
        first <- letterChar <|> char '_'
        rest <- many (alphaNumChar <|> char '_')
        return (first : rest)
    return (Import name alias)

parseReturn :: Parser Command
parseReturn = do
    _ <- symbol "return"
    expr <- parseExpression
    return (Return expr)

-- Parse commands inside a block (stops at end/else keywords)
parseBlockCommands :: Parser Command
parseBlockCommands = do
    sc  -- consume all whitespace including newlines
    cmd <- parseSingleCommand
    rest <- many $ try $ do
        sc
        notFollowedBy (string "end" <|> string "else")
        parseSingleCommand
    sc
    return $ foldl Concat cmd rest

-- Parse multiple top-level commands  
parseCommands :: Parser Command
parseCommands = do
    sc
    cmd <- parseSingleCommand
    rest <- many $ try $ do
        sc
        notFollowedBy eof
        parseSingleCommand
    sc
    return $ foldl Concat cmd rest

parseCommand :: Parser Command
parseCommand = parseCommands

{-
English:
  The PUBLIC ENTRY POINTS -- the only functions other modules call. They run the
  whole parser over an input string and return either the AST `Command` or a parse
  error bundle.

  `parse (sc *> parseCommand <* sc <* eof) name input` reads: skip leading
  whitespace, parse the program, skip trailing whitespace, and require `eof` (the
  entire input must be consumed -- trailing garbage is an error, not ignored). The
  `name` is the source label shown in error messages.

    * parseBernWithName -- the general form, given a source name.
    * parseBern        -- for the REPL, labelled "<interactive>".
    * parseBernFile    -- for files; the file path is the label, so errors point at
                          the real file and line.

Português:
  Os PONTOS DE ENTRADA PÚBLICOS -- as únicas funções que outros módulos chamam. Elas
  rodam o parser inteiro sobre uma string de entrada e retornam ou a `Command` da
  AST ou um pacote de erro de parsing.

  `parse (sc *> parseCommand <* sc <* eof) name input` lê-se: pular espaço em branco
  inicial, parsear o programa, pular espaço em branco final, e exigir `eof` (toda a
  entrada precisa ser consumida -- lixo no fim é erro, não é ignorado). O `name` é o
  rótulo de origem mostrado nas mensagens de erro.

    * parseBernWithName -- a forma geral, dado um nome de origem.
    * parseBern        -- para o REPL, rotulado "<interactive>".
    * parseBernFile    -- para arquivos; o caminho do arquivo é o rótulo, então os
                          erros apontam para o arquivo e a linha reais.
-}
-- Top-level parser. Accepts a source name so parse errors report filenames/line numbers.
parseBernWithName :: String -> String -> Either (ParseErrorBundle String Void) Command
parseBernWithName name input = parse (sc *> parseCommand <* sc <* eof) name input

-- Interactive parser defaults to <interactive>
parseBern :: String -> Either (ParseErrorBundle String Void) Command
parseBern = parseBernWithName "<interactive>"

-- File parser uses the provided filename
parseBernFile :: String -> String -> Either (ParseErrorBundle String Void) Command
parseBernFile = parseBernWithName
