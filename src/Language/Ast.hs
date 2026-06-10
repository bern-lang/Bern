module Language.Ast where

import Text.Megaparsec (SourcePos)

{-
Module: Language.Ast

English:
  The vocabulary of the whole interpreter. This module defines the data types
  that represent a Bern program after it has been read, and the values it
  produces while running. Nothing here "does" anything -- there is no logic, only
  shapes -- but almost every other module is written in terms of these shapes, so
  it is the best file to read first.

  There are three families of types to know:

    * The Abstract Syntax Tree (AST): `Expression`, `Command`, `Pattern`,
      `Clause`, and friends. The PARSER turns source text into these; the
      EVALUATOR walks them. An `Expression` is anything that produces a value
      (1 + 2, a list, an `if`); a `Command` is a statement that does something
      (an assignment, a loop, a function definition).

    * Runtime `Value`: what an expression evaluates TO -- an Integer, a List, a
      Function closure, an error, and so on. The AST is the recipe; a Value is
      the cooked result.

    * Small description types: `BinaryOperation`/`UnaryOperation` name the
      operators, `Type` names a declared ADT field type, and `CompQual`/
      `CaseBranch`/`ADTConstructor` are helper shapes used inside the bigger ones.

Português:
  O vocabulário de todo o interpretador. Este módulo define os tipos de dados que
  representam um programa Bern depois de lido, e os valores que ele produz
  enquanto roda. Nada aqui "faz" algo -- não há lógica, só formatos -- mas quase
  todo outro módulo é escrito em termos desses formatos, então é o melhor arquivo
  para ler primeiro.

  Há três famílias de tipos para conhecer:

    * A Árvore de Sintaxe Abstrata (AST): `Expression`, `Command`, `Pattern`,
      `Clause` e companhia. O PARSER transforma texto-fonte nisto; o AVALIADOR
      percorre. Uma `Expression` é qualquer coisa que produz um valor (1 + 2, uma
      lista, um `if`); um `Command` é um comando que faz algo (uma atribuição, um
      laço, uma definição de função).

    * O `Value` de runtime: aquilo que uma expressão avalia PARA -- um Integer,
      uma List, uma closure de Function, um erro, e assim por diante. A AST é a
      receita; um Value é o resultado cozido.

    * Pequenos tipos de descrição: `BinaryOperation`/`UnaryOperation` nomeiam os
      operadores, `Type` nomeia o tipo de um campo de ADT declarado, e
      `CompQual`/`CaseBranch`/`ADTConstructor` são formatos auxiliares usados
      dentro dos maiores.
-}

{-
English:
  The binary (two-operand) operators. The parser maps each symbol AND its written
  word (e.g. `+` or `plus`) to one of these tags, and the evaluator switches on
  the tag. Note Union/Intersection/Difference/Concatenation are overloaded to work
  on both lists and sets.

Português:
  Os operadores binários (de dois operandos). O parser mapeia cada símbolo E sua
  palavra escrita (ex.: `+` ou `plus`) para uma destas etiquetas, e o avaliador
  decide pela etiqueta. Repare que Union/Intersection/Difference/Concatenation são
  sobrecarregados para funcionar tanto em listas quanto em conjuntos.
-}
data BinaryOperation = Add           -- (+)
                     | Subtract      -- (-)
                     | Multiply      -- (*)
                     | Divide        -- (/)
                     | Modulo        -- (%)
                     | Equal         -- (==)
                     | Different     -- (!=) or (~=)
                     | And           -- (&&)
                     | Or            -- (||)
                     | GreaterThan   -- (>)
                     | LessThan      -- (<)
                     | GreaterThanEq -- (>=)
                     | LessThanEq    -- (<=)
                     | Concatenation        -- (<>)
                     | Union               -- (<|) for Sets and Lists
                     | Intersection        -- (|>) for Sets and Lists
                     | Difference          -- (</>) for Sets and Lists
                     deriving (Show, Eq)

{-
English:
  The unary (one-operand) prefix operators. Negate and Not are the usual
  arithmetic/boolean ones; TypeOf (`::`) reports a value's type as a string and
  SizeOf (`:>`) the length of a list/set/string.

Português:
  Os operadores unários (de um operando) prefixos. Negate e Not são os usuais de
  aritmética/booleano; TypeOf (`::`) informa o tipo de um valor como string e
  SizeOf (`:>`) o comprimento de uma lista/conjunto/string.
-}
data UnaryOperation = Negate   -- Negates Numbers (-)
                    | Not      -- (!) or (~)
                    | TypeOf   -- :: expr
                    | SizeOf   -- :> expr
                    deriving (Show, Eq)

{-
English:
  An algebraic-data-type DECLARATION, i.e. the result of parsing
  `adt Name = CtorA ... | CtorB ...`.
    * the Bool is the `iterative` flag (an `adt iterative ...` can be looped over);
    * the String is the type's name;
    * the list holds its constructors.
  Each `ADTConstructor` is a constructor name plus the declared types of its
  fields (used for arity, not enforced strongly at runtime).

Português:
  Uma DECLARAÇÃO de tipo de dado algébrico, ou seja, o resultado de parsear
  `adt Nome = CtorA ... | CtorB ...`.
    * o Bool é a flag `iterative` (um `adt iterative ...` pode ser percorrido em laço);
    * a String é o nome do tipo;
    * a lista guarda seus construtores.
  Cada `ADTConstructor` é um nome de construtor mais os tipos declarados dos seus
  campos (usados para a aridade, não fortemente impostos em runtime).
-}
data AlgebraicDataTypeDef = ADTDef Bool String [ADTConstructor]
    deriving (Show, Eq)

data ADTConstructor = ADTConstructor String [Type]
    deriving (Show, Eq)

-- The field types that may appear in an ADT declaration. `TCustom` is any
-- user-named type referenced by another ADT. `TAuto` is the dynamic "detect the
-- type automatically" placeholder (spelled `Auto`, or `Any`): it accepts a value
-- of whatever type is passed -- a natural fit for a dynamically typed language.
-- Os tipos de campo que podem aparecer numa declaração de ADT. `TCustom` é
-- qualquer tipo de nome do usuário referenciado por outro ADT. `TAuto` é o
-- marcador dinâmico "detecte o tipo automaticamente" (escrito `Auto`, ou `Any`):
-- aceita um valor de qualquer tipo que for passado -- ideal para uma linguagem
-- de tipagem dinâmica.
data Type = TInt | TDouble | TBool | TChar | TString | TList | TSet | TCustom String | TAuto
    deriving (Show, Eq)

{-
English:
  An EXPRESSION: any piece of program that evaluates to a value. This is the
  central, recursive AST type -- most constructors hold other Expressions inside
  them, which is how `1 + (2 * 3)` becomes a tree. The trailing comments show the
  surface syntax each constructor comes from.

  A few worth calling out:
    * WithPos wraps another Expression with its source position. The parser adds
      these so that, when evaluation fails deep inside an expression, the error can
      point at the exact line/column. It is "invisible" to evaluation, which just
      unwraps and continues.
    * FunctionCall is a named call (`f(x)`); Apply applies a value that is already
      a function (e.g. a lambda, or the right side of the `)|` pipe).
    * ListCons / SetCons / ListComp are the bracket constructor forms -- prepend
      (`[h | t]`) and comprehensions (`[x | x <- xs]`).
    * GetHostMachine / GetCurrentDir / ReadFile / Keys / MakeError / Fmap are
      built-in operations that read like function calls but get their own AST node
      so the evaluator can handle them directly.

Português:
  Uma EXPRESSÃO: qualquer pedaço de programa que avalia para um valor. Este é o
  tipo central e recursivo da AST -- a maioria dos construtores guarda outras
  Expressions dentro de si, e é assim que `1 + (2 * 3)` vira uma árvore. Os
  comentários ao lado mostram a sintaxe de superfície de onde cada construtor vem.

  Alguns que vale destacar:
    * WithPos envolve outra Expression com a posição no fonte. O parser adiciona
      isto para que, quando a avaliação falha bem dentro de uma expressão, o erro
      possa apontar a linha/coluna exata. É "invisível" para a avaliação, que só
      desembrulha e continua.
    * FunctionCall é uma chamada por nome (`f(x)`); Apply aplica um valor que já é
      uma função (ex.: uma lambda, ou o lado direito do pipe `)|`).
    * ListCons / SetCons / ListComp são as formas construtoras com colchetes --
      adicionar no início (`[h | t]`) e comprehensions (`[x | x <- xs]`).
    * GetHostMachine / GetCurrentDir / ReadFile / Keys / MakeError / Fmap são
      operações embutidas que se leem como chamadas de função mas ganham o próprio
      nó da AST para que o avaliador as trate diretamente.
-}
data Expression = Number Int
                | AlgebraicDataTypeConstruct String [Expression] -- ADT Name and its values
                | DoubleNum Double
                | BoolLiteral Bool -- true/false
                | IfExpr Expression Expression Expression -- if cond then expr else expr end
                | CaseExpr Expression [CaseBranch] -- case expr is pat = expr | pat = expr [end]
                | StringLiteral String -- "string"
                | CharLiteral Char -- 'c'
                | SetLiteral [Expression]      -- {elem1, elem2, ...}
                | ListLiteral [Expression]     -- [elem1, elem2, ...]
                | ObjectLiteral [(String, Expression)] -- #{ key: value, ... }#
                | Range Expression Expression          -- [start .. end]
                | RangeFrom Expression                 -- [start ..]            infinite, step +1
                | RangeFromThen Expression Expression  -- [start, next ..]      infinite, step (next-start)
                | RangeFromThenTo Expression Expression Expression -- [start, next .. end]  finite, custom step
                | ListCons [Expression] Expression     -- [h, ... | tail]  prepend element(s) onto a list
                | SetCons [Expression] Expression      -- {h, ... | tail}  prepend element(s) onto a set
                | ListComp Expression [CompQual]       -- [expr | gen/guard, ...]  list comprehension
                | Index Expression Expression  -- list[index], set[index] or string[index]
                | BinaryOperator BinaryOperation Expression Expression
                | UnaryOperator UnaryOperation Expression
                | Variable String
                | FunctionCall String [Expression]
                | Apply Expression [Expression]       -- apply a function-valued expression (e.g. a lambda) to args; used by the )| pipe
                | LambdaExpr [Pattern] (Maybe Expression) Expression  -- \params [when guard] -> body
                | WithPos SourcePos Expression        -- carries source position for better errors
                | ReadFile Expression                 -- readfile filename
                | InputExpr Expression                -- input(prompt) used as an expression
                | GetHostMachine                      -- get_host_machine()
                | GetCurrentDir                       -- get_current_dir()
                | Keys Expression                     -- keys(object) -> list of key strings
                | MakeError Expression                -- error("msg") -> a recoverable error value
                | Fmap Expression Expression          -- fmap collection function
                deriving (Show, Eq)

-- Qualifiers of a list comprehension [expr | q, q, ...]: either a generator
-- (a pattern drawn from a source collection) or a boolean guard that filters
-- the rows produced so far. A comprehension must contain at least one generator
-- (otherwise [a | b] is parsed as the cons expression instead).
data CompQual = CompGen Pattern Expression   -- pattern <- source
              | CompGuard Expression          -- boolean filter
    deriving (Show, Eq)

{-
English:
  A PATTERN: the left-hand side of a match. Patterns appear in function clauses
  (`def f(0) -> ...`), in `case` branches, and in comprehension generators. The
  evaluator tries to "match" a pattern against a value; a match either fails, or
  succeeds and binds names. The kinds:
    * PVar binds any value to a name; PWildcard (`_`) matches anything and binds
      nothing; the literal patterns (PInt, PString, ...) match only that exact
      value.
    * PList / PSet match a fixed-size collection; PCons (`[head|tail]`) and
      PSetCons (`{head|tail}`) split a non-empty collection into its first element
      and the rest -- the basis of recursion over lists.
    * PADT matches a specific ADT constructor and recurses into its fields.

Português:
  Um PADRÃO: o lado esquerdo de um casamento. Padrões aparecem em cláusulas de
  função (`def f(0) -> ...`), em ramos de `case` e em geradores de comprehension.
  O avaliador tenta "casar" um padrão com um valor; o casamento ou falha, ou dá
  certo e vincula nomes. Os tipos:
    * PVar vincula qualquer valor a um nome; PWildcard (`_`) casa com qualquer
      coisa e não vincula nada; os padrões literais (PInt, PString, ...) casam só
      com aquele valor exato.
    * PList / PSet casam uma coleção de tamanho fixo; PCons (`[head|tail]`) e
      PSetCons (`{head|tail}`) dividem uma coleção não-vazia no primeiro elemento e
      no resto -- a base da recursão sobre listas.
    * PADT casa um construtor de ADT específico e recursiona em seus campos.
-}
data Pattern = PVar String
            | PInt Int
            | PADT String [Pattern]      -- ADT pattern matching
            | PDouble Double
            | PBool Bool
            | PChar Char
            | PString String
            | PWildcard
            | PList [Pattern]           -- Empty list [] or list of patterns [a, b, c]
            | PCons Pattern Pattern     -- Cons pattern [head|tail]
            | PSet [Pattern]            -- Empty set {} or set of patterns {a, b, c}
            | PSetCons Pattern Pattern  -- Set cons pattern {head|tail}
            deriving (Show, Eq)

-- One branch of a `case`: a pattern, an optional `when` guard, and the body to
-- evaluate if the pattern matches and the guard (if any) is true.
-- Um ramo de um `case`: um padrão, uma guarda `when` opcional, e o corpo a
-- avaliar se o padrão casa e a guarda (se houver) é verdadeira.
data CaseBranch = CaseBranch Pattern (Maybe Expression) Expression
    deriving (Show, Eq)

-- The body of a function clause: either a single expression (the `-> expr` form)
-- or a multi-statement `do ... end` block.
-- O corpo de uma cláusula de função: ou uma única expressão (a forma `-> expr`)
-- ou um bloco `do ... end` de vários comandos.
data FunctionBody = BodyExpr Expression
                    | BodyBlock Command
                    deriving (Show, Eq)

{-
English:
  A runtime VALUE -- the result of evaluating an expression. Where Expression is
  the program text's shape, Value is what actually exists while the program runs.

  Things to understand:
    * Set/List carry a cached Length (the `type Length = Int` alias) alongside
      their elements. Storing the length lets `:>` and indexing be answered without
      walking the spine, which is what keeps lazy ranges like [1..1000000000] cheap.
    * Bern has NO separate string type at runtime: a "string" is a List of
      Character values (see Helpers.allCharacter). TextLiteral exists for the
      interpolating string form.
    * Function and Lambda each store their clauses AND the pragma set of the file
      that defined them, so a library function keeps behaving under its own pragmas
      even when called from a file with different ones.
    * CBinding wraps a real C function (via the FFI) as a callable value.
    * ErrorVal is a runtime error turned into a first-class value -- this is the
      "errors as values" model: a failure becomes data that propagates and can be
      tested with is_error, instead of crashing.

Português:
  Um VALUE de runtime -- o resultado de avaliar uma expressão. Onde Expression é o
  formato do texto do programa, Value é o que de fato existe enquanto o programa
  roda.

  Coisas para entender:
    * Set/List carregam um Length em cache (o alias `type Length = Int`) ao lado de
      seus elementos. Guardar o comprimento permite responder `:>` e indexação sem
      percorrer a estrutura, que é o que mantém intervalos preguiçosos como
      [1..1000000000] baratos.
    * Bern NÃO tem um tipo de string separado em runtime: uma "string" é uma List
      de valores Character (ver Helpers.allCharacter). TextLiteral existe para a
      forma de string com interpolação.
    * Function e Lambda guardam suas cláusulas E o conjunto de pragmas do arquivo
      que as definiu, para que uma função de biblioteca continue se comportando sob
      os seus próprios pragmas mesmo quando chamada de um arquivo com outros.
    * CBinding embrulha uma função C de verdade (via FFI) como um valor chamável.
    * ErrorVal é um erro de execução transformado em valor de primeira classe --
      este é o modelo de "erros como valores": uma falha vira dado que se propaga e
      pode ser testado com is_error, em vez de quebrar.
-}
-- A cached element count. A negative value is the sentinel for an UNBOUNDED
-- length: an infinite lazy range like [1..] stores it, and its spine is a
-- genuine infinite Haskell list. See `infiniteLength`/`lenPred`/`lenPlus` in
-- Language.Eval for the saturating arithmetic that keeps it infinite.
type Length = Int
data Value = Integer Int
            | Double Double
            | NaN
            | Undefined
            | Boolean Bool
            | Set [Value] Length          -- A Set ({}) can hold multiple values of any type
            | List [Value] Length         -- A List ([]) can hold multiple values of a single type
            | Object [(String, Value)]    -- An Object (#{ }#) holds key-value pairs
            | TextLiteral String Length   -- A String ([| |]) that preserves spaces and can add variables directly in it (like ```${name}```)
            | Character Char
            | Function [Clause] [String]  -- clauses + the pragma set of the file that defined it
            | Lambda [Clause] [String]    -- clauses + the pragma set of the file that defined it
            | CBinding String ([Value] -> IO Value)  -- A C function binding (runtime value)
            | AlgebraicDataType String [Value] -- An Algebraic Data Type (ADT) with its name and values
            | ErrorVal String                  -- A recoverable runtime error as a value (errors-as-values)

{-
English:
  Why Show and Eq are written by hand instead of `deriving`-ed.

  Haskell behaviour: `deriving (Show, Eq)` only works when EVERY field of EVERY
  constructor is itself Show/Eq. The CBinding constructor holds a function
  (`[Value] -> IO Value`), and functions have no Show or Eq instance (you cannot
  print a function, nor decide if two functions are "equal"). So we must define
  both instances manually, and for CBinding we sidestep the problem: Show prints a
  placeholder, and Eq compares only the binding's NAME. Likewise Function/Lambda
  ignore their pragma tag when comparing -- two functions are equal if their
  clauses are equal, the originating pragmas are not part of identity.

Português:
  Por que Show e Eq são escritos à mão em vez de derivados com `deriving`.

  Comportamento de Haskell: `deriving (Show, Eq)` só funciona quando TODO campo de
  TODO construtor é, ele mesmo, Show/Eq. O construtor CBinding guarda uma função
  (`[Value] -> IO Value`), e funções não têm instância de Show nem de Eq (não dá
  para imprimir uma função, nem decidir se duas funções são "iguais"). Então
  precisamos definir as duas instâncias manualmente, e para CBinding contornamos o
  problema: Show imprime um marcador, e Eq compara só o NOME do binding. Da mesma
  forma, Function/Lambda ignoram sua etiqueta de pragma ao comparar -- duas funções
  são iguais se suas cláusulas são iguais; os pragmas de origem não fazem parte da
  identidade.
-}
instance Show Value where
    show (Integer n) = "Integer " ++ show n
    show (Double d) = "Double " ++ show d
    show NaN = "NaN"
    show Undefined = "Undefined"
    show (Boolean b) = "Boolean " ++ show b
    show (Set vs l) = "Set " ++ show vs ++ " " ++ show l
    show (List vs l) = "List " ++ show vs ++ " " ++ show l
    show (Object kvs) = "Object " ++ show kvs
    show (TextLiteral s l) = "TextLiteral " ++ show s ++ " " ++ show l
    show (Character c) = "Character " ++ show c
    show (Function cs _) = "Function " ++ show cs
    show (Lambda cs _) = "Lambda " ++ show cs
    show (CBinding name _) = "CBinding " ++ show name ++ " <function>"  -- a function can't be shown
    show (AlgebraicDataType name vs) = "AlgebraicDataType " ++ show name ++ " " ++ show vs
    show (ErrorVal m) = "ErrorVal " ++ show m

instance Eq Value where
    (Integer a) == (Integer b) = a == b
    (Double a) == (Double b) = a == b
    NaN == NaN = True
    Undefined == Undefined = True
    (Boolean a) == (Boolean b) = a == b
    (Set a la) == (Set b lb) = a == b && la == lb
    (List a la) == (List b lb) = a == b && la == lb
    (Object a) == (Object b) = a == b
    (TextLiteral a la) == (TextLiteral b lb) = a == b && la == lb
    (Character a) == (Character b) = a == b
    (Function a _) == (Function b _) = a == b   -- pragma tag is not part of identity
    (Lambda a _) == (Lambda b _) = a == b
    (CBinding na _) == (CBinding nb _) = na == nb  -- compared by name only
    (AlgebraicDataType na va) == (AlgebraicDataType nb vb) = na == nb && va == vb
    (ErrorVal a) == (ErrorVal b) = a == b
    _ == _ = False

{-
English:
  One CLAUSE of a function. A Bern function is a list of these (pattern matching
  picks which one runs):
    * [Pattern] are the parameter patterns to match the call's arguments against;
    * the Maybe Expression is an optional `when` guard -- the clause only applies
      if the patterns match AND the guard is true;
    * FunctionBody is what to evaluate when it does apply.

Português:
  Uma CLÁUSULA de função. Uma função Bern é uma lista destas (o casamento de
  padrões escolhe qual roda):
    * [Pattern] são os padrões de parâmetro para casar com os argumentos da
      chamada;
    * o Maybe Expression é uma guarda `when` opcional -- a cláusula só se aplica se
      os padrões casam E a guarda é verdadeira;
    * FunctionBody é o que avaliar quando ela de fato se aplica.
-}
data Clause = Clause [Pattern] (Maybe Expression) FunctionBody
    deriving (Show, Eq)

{-
English:
  A COMMAND: a top-level (or block-level) statement. Where an Expression yields a
  value, a Command performs an action and produces an updated scope. A program is
  ultimately a tree of Commands chained with `Concat`.

  Highlights:
    * AutoPrint is a bare expression typed on its own; Bern prints it
      automatically (unless the no-eval pragma is on). This is why typing `1 + 1`
      shows `2`.
    * Assign (`=`) binds in the local scope; GlobalAssign (`:=`) writes to the
      shared global scope.
    * The loop forms are split by behaviour: Repeat (a count), While (a condition),
      ForIn (iterate a collection), ForInCount (iterate with an index). They all
      come from the one `for`/`loop` keyword in source.
    * CForeignDecl declares a C function to import via the FFI.

Português:
  Um COMMAND: um comando de topo (ou de bloco). Onde uma Expression entrega um
  valor, um Command realiza uma ação e produz um escopo atualizado. Um programa é,
  no fim, uma árvore de Commands encadeados com `Concat`.

  Destaques:
    * AutoPrint é uma expressão solta digitada sozinha; Bern a imprime
      automaticamente (a menos que o pragma no-eval esteja ligado). É por isso que
      digitar `1 + 1` mostra `2`.
    * Assign (`=`) vincula no escopo local; GlobalAssign (`:=`) escreve no escopo
      global compartilhado.
    * As formas de laço são separadas por comportamento: Repeat (uma contagem),
      While (uma condição), ForIn (itera uma coleção), ForInCount (itera com um
      índice). Todas vêm da única palavra-chave `for`/`loop` no fonte.
    * CForeignDecl declara uma função C a importar via FFI.
-}
data Command = Skip
                | Print Expression
                | AutoPrint Expression                         -- a bare top-level expression (suppressed by no-eval)
                | Assign String Expression                    -- (=)
                | TypedAssign String String Expression        -- name :: Type = expr  (under the `typed` pragma)
                | GlobalAssign String Expression              -- (:=) - global assignment
                | AssignIndex String [Expression] Expression  -- var[idx..] = expr
                | Conditional Expression Command Command      -- if-then-else
                | Repeat Expression Command                   -- for 3 do {}
                | While Expression Command                    -- for (x != 10) do
                | ForIn String Expression Command             -- for x : collection do ... end
                | ForInCount String String Expression Command -- for x,i : collection do ... end
                | Concat Command Command
                | FunctionDef String Clause                   -- def name(params) do ... end / -> expr / pattern clauses
                | Return Expression                           -- return expr
                | Import String (Maybe String)                -- import module [as alias]
                | Input String Expression                     -- input var prompt
                | WriteFile Expression Expression             -- writefile filename expr
                | AlgebraicTypeDef AlgebraicDataTypeDef
                | CForeignDecl String Expression [String] String -- foreign funcName (argTypes) -> retType
                deriving (Show, Eq)
