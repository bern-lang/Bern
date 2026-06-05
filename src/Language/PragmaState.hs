{-
Module: Language.PragmaState

English:
  This is the smallest module in the project, and it exists to solve one very
  specific problem: where to keep the set of pragmas that are currently active.

  A "pragma" in Bern is a per-file directive written as a magic comment, like
  {--! no-written-operators !--}. It flips the interpreter into a stricter or
  looser mode for that file. Two completely different parts of the code need to
  know which pragmas are on:
    * the PARSER (Parsing.Parser), because some pragmas change how source text
      is read -- for example `no-written-operators` turns the word operators
      (plus, length, and, ...) back into ordinary names while parsing; and
    * the EVALUATOR (Language.Eval), because most pragmas change how the program
      behaves while it runs.

  We cannot put this shared state inside Eval, because Eval already imports the
  Parser (to load `import`ed modules), and Haskell does not allow two modules to
  import each other (a cyclic import). So the state lives here instead, in a
  tiny "leaf" module that depends on nothing of ours and that both Parser and
  Eval can import freely.

Português:
  Este é o menor módulo do projeto, e existe para resolver um problema bem
  específico: onde guardar o conjunto de pragmas que estão ativos no momento.

  Um "pragma" em Bern é uma diretiva por arquivo, escrita como um comentário
  mágico, por exemplo {--! no-written-operators !--}. Ele coloca o interpretador
  em um modo mais rígido ou mais flexível para aquele arquivo. Duas partes bem
  diferentes do código precisam saber quais pragmas estão ligados:
    * o PARSER (Parsing.Parser), porque alguns pragmas mudam como o texto-fonte
      é lido -- por exemplo, `no-written-operators` transforma os operadores em
      palavras (plus, length, and, ...) de volta em nomes comuns durante o
      parsing; e
    * o AVALIADOR (Language.Eval), porque a maioria dos pragmas muda como o
      programa se comporta enquanto roda.

  Não podemos colocar esse estado compartilhado dentro do Eval, porque o Eval já
  importa o Parser (para carregar módulos via `import`), e o Haskell não permite
  que dois módulos importem um ao outro (import cíclico). Então o estado mora
  aqui, em um módulo "folha" minúsculo que não depende de nada nosso e que tanto
  o Parser quanto o Eval podem importar à vontade.
-}
module Language.PragmaState
    ( pragmaSetRef
    , pragmaActive
    ) where

import Data.IORef (IORef, newIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)

{-
English:
  `pragmaSetRef` is a single mutable cell holding the list of pragma names that
  are active right now (for instance ["no-written-operators", "immutable"]).

  Haskell behaviour: a normal Haskell value is immutable, so to have a piece of
  state we can read and overwrite we use an IORef (a mutable reference). Creating
  an IORef is an IO action, but we want this cell to be a top-level value that
  every function can reach without threading it through arguments. The usual
  trick for that is `unsafePerformIO`, which runs an IO action in a "pure"
  position. The {-# NOINLINE #-} pragma is essential here: it tells the compiler
  NOT to inline this definition. If it were inlined, each use site could end up
  creating its OWN fresh IORef, and the writes from one part of the program would
  never be seen by another. NOINLINE guarantees there is exactly one cell shared
  by everyone.

Português:
  `pragmaSetRef` é uma única célula mutável que guarda a lista de nomes de
  pragmas ativos neste momento (por exemplo ["no-written-operators",
  "immutable"]).

  Comportamento de Haskell: um valor normal em Haskell é imutável, então, para
  ter um estado que possamos ler e sobrescrever, usamos um IORef (uma referência
  mutável). Criar um IORef é uma ação de IO, mas queremos que esta célula seja um
  valor de topo que toda função possa acessar sem precisar passá-la por
  argumento. O truque usual para isso é o `unsafePerformIO`, que roda uma ação de
  IO numa posição "pura". O pragma {-# NOINLINE #-} é essencial aqui: ele diz ao
  compilador para NÃO fazer inline desta definição. Se fizesse inline, cada lugar
  de uso poderia acabar criando o SEU PRÓPRIO IORef novo, e as escritas de uma
  parte do programa nunca seriam vistas por outra. O NOINLINE garante que existe
  exatamente uma célula compartilhada por todos.
-}
{-# NOINLINE pragmaSetRef #-}
pragmaSetRef :: IORef [String]
pragmaSetRef = unsafePerformIO (newIORef [])

{-
English:
  `pragmaActive name ()` answers "is this pragma currently on?".

  Notice the strange-looking second argument: a dummy `()`. It is there on
  purpose. With NOINLINE alone, the compiler could still treat `pragmaActive
  "immutable"` as a constant and compute it once, caching the Bool forever -- but
  the flag set changes at runtime (a file enables a pragma, an imported module
  restores the previous set, the REPL toggles one), so a cached answer would be
  wrong. Forcing every caller to pass an extra `()` makes each call a genuinely
  new application that re-runs the read. This is the same idiom Eval uses for its
  own `pragmaOn`.

Português:
  `pragmaActive name ()` responde "este pragma está ligado agora?".

  Repare no segundo argumento de aparência estranha: um `()` de enfeite. Ele está
  ali de propósito. Só com o NOINLINE, o compilador ainda poderia tratar
  `pragmaActive "immutable"` como uma constante e calculá-la uma única vez,
  guardando o Bool para sempre -- mas o conjunto de flags muda em tempo de
  execução (um arquivo liga um pragma, um módulo importado restaura o conjunto
  anterior, o REPL alterna algum), então uma resposta em cache estaria errada.
  Obrigar todo chamador a passar um `()` extra faz de cada chamada uma aplicação
  genuinamente nova, que re-executa a leitura. É o mesmo idioma que o Eval usa no
  seu próprio `pragmaOn`.
-}
{-# NOINLINE pragmaActive #-}
pragmaActive :: String -> () -> Bool
pragmaActive name _ = unsafePerformIO (fmap (name `elem`) (readIORef pragmaSetRef))
