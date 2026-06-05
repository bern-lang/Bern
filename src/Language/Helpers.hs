{-
Module: Language.Helpers

English:
  Small, pure helper functions that inspect a runtime `Value` (the data type
  defined in Language.Ast). They answer simple questions the evaluator and the
  standard library ask all the time:
    * what TYPE is this value? (getValueType)
    * give me its plain textual content (getValueOnly)
    * are all these values the same type? (allSameType -- used to keep lists
      homogeneous)
    * is this list really a string, i.e. a list of characters? (allCharacter)
    * can this value be read as a Haskell String? (valueToString)

  Keeping them here, away from the big Eval module, makes them easy to find and
  reuse without dragging in the evaluator's machinery.

Português:
  Pequenas funções puras de apoio que inspecionam um `Value` de runtime (o tipo
  de dado definido em Language.Ast). Elas respondem perguntas simples que o
  avaliador e a biblioteca padrão fazem o tempo todo:
    * qual é o TIPO deste valor? (getValueType)
    * me dê o conteúdo textual simples dele (getValueOnly)
    * todos estes valores são do mesmo tipo? (allSameType -- usada para manter
      listas homogêneas)
    * esta lista é, na verdade, uma string, ou seja, uma lista de caracteres?
      (allCharacter)
    * este valor pode ser lido como uma String de Haskell? (valueToString)

  Mantê-las aqui, longe do grande módulo Eval, facilita encontrá-las e reutilizá-
  las sem arrastar junto toda a maquinaria do avaliador.
-}
module Language.Helpers where
import Language.Ast

{-
English:
  Return the type of a value as a human-readable string -- this is what Bern's
  `::` (typeof) operator reports.

  Haskell behaviour: the clauses are tried top to bottom, and the FIRST one that
  matches wins. That matters here: the unguarded `List _ _` line is reached
  before the guarded `List vals _ | allCharacter vals` line below it, so as
  written every list currently reports "List" and the "Text" branch never fires.
  It is kept to show the intent (a list of characters is conceptually Text); if
  you wanted it live, the guarded clause would have to come first.

Português:
  Retorna o tipo de um valor como uma string legível -- é isto que o operador
  `::` (typeof) do Bern informa.

  Comportamento de Haskell: as cláusulas são testadas de cima para baixo, e a
  PRIMEIRA que casa vence. Isso importa aqui: a linha sem guarda `List _ _` é
  alcançada antes da linha com guarda `List vals _ | allCharacter vals` logo
  abaixo, então, como está escrito, toda lista informa "List" e o ramo "Text"
  nunca dispara. Ele é mantido para mostrar a intenção (uma lista de caracteres é
  conceitualmente Text); se você quisesse ativá-lo, a cláusula com guarda teria
  que vir primeiro.
-}
getValueType :: Value -> String
getValueType (Integer _) = "Integer"
getValueType (Double _) = "Double"
getValueType NaN = "NaN"
getValueType Undefined = "Undefined"
getValueType (Boolean _) = "Boolean"
getValueType (Set _ _) = "Set"
getValueType (List _ _) = "List"
getValueType (Object _) = "Object"
getValueType (Character _) = "Character"
getValueType (List vals _) | allCharacter vals = "Text"  -- shadowed by `List _ _` above
-- An ADT value reports its own type name (e.g. "Shape"), not a generic tag.
-- Um valor de ADT informa o próprio nome do tipo (ex.: "Shape"), não um rótulo genérico.
getValueType (Function _ _) = "Function"
getValueType (Lambda _ _) = "Lambda"
getValueType (AlgebraicDataType name _) = name
getValueType (CBinding _ _) = "Function"   -- a C binding looks like a Function to Bern code
getValueType (ErrorVal _) = "Error"

{-
English:
  Produce the raw textual content of a value -- used when a value has to be shown
  as plain text. For scalars it is just their printed form; for a character it is
  the character itself; for a character-list it is the concatenated string.

  Same top-to-bottom shadowing caveat as above: the unguarded `List l _ = show l`
  line is reached before the guarded character-list line, so a list is rendered
  with `show` rather than unwrapped to its characters here.

Português:
  Produz o conteúdo textual cru de um valor -- usado quando um valor precisa ser
  mostrado como texto simples. Para escalares é só a forma impressa deles; para
  um caractere é o próprio caractere; para uma lista de caracteres é a string
  concatenada.

  Mesma ressalva de sombreamento de cima para baixo que acima: a linha sem guarda
  `List l _ = show l` é alcançada antes da linha com guarda de lista de
  caracteres, então uma lista é renderizada com `show` em vez de ser
  desembrulhada nos seus caracteres aqui.
-}
getValueOnly :: Value -> String
getValueOnly (Integer n) = show n
getValueOnly (Double d) = show d
getValueOnly NaN = "NaN"
getValueOnly Undefined = "Undefined"
getValueOnly (Boolean b) = show b
getValueOnly (Set s _) = show s
getValueOnly (List l _) = show l
getValueOnly (Object o) = show o
getValueOnly (Character c) = [c]
getValueOnly (List vals _) | allCharacter vals = concatMap getValueOnly vals  -- shadowed above
getValueOnly (Function _ _) = "<function>"
getValueOnly (Lambda _ _) = "<lambda>"

{-
English:
  True when every value in the list has the same type. Lists in Bern are
  homogeneous by default, so the evaluator calls this before building a list to
  reject mixes like [1, "two"]. An empty list is trivially "all the same type".

  How it works: take the first element's type as the reference and check that
  every other element matches it (`all` over the tail). Only the first element's
  type is computed repeatedly, which is fine for the small lists this guards.

Português:
  Verdadeiro quando todo valor da lista tem o mesmo tipo. Listas em Bern são
  homogêneas por padrão, então o avaliador chama isto antes de construir uma
  lista para rejeitar misturas como [1, "two"]. Uma lista vazia é trivialmente
  "todos do mesmo tipo".

  Como funciona: pega o tipo do primeiro elemento como referência e verifica que
  todos os outros casam com ele (`all` sobre a cauda). Só o tipo do primeiro
  elemento é recalculado, o que está ok para as listas pequenas que isto protege.
-}
allSameType :: [Value] -> Bool
allSameType [] = True
allSameType (x:xs) = all (\v -> getValueType v == getValueType x) xs

{-
English:
  True when the list is empty OR its first element is a Character and so are all
  the rest -- i.e. the list is really a Bern string. Bern has no separate string
  type: a string is just a List of Character values, so this predicate is how we
  recognise one.

Português:
  Verdadeiro quando a lista é vazia OU seu primeiro elemento é um Character e
  todos os demais também -- ou seja, a lista é, na verdade, uma string de Bern.
  Bern não tem um tipo de string separado: uma string é apenas uma List de
  valores Character, então este predicado é como reconhecemos uma.
-}
allCharacter :: [Value] -> Bool
allCharacter [] = True
allCharacter (x:xs) = case x of
    Character _ -> all isCharacter xs
    _           -> False
  where
    isCharacter (Character _) = True
    isCharacter _             = False

{-
English:
  Try to read a value as a plain Haskell String. It succeeds (Just ...) only for
  a character-list (a Bern string), unwrapping each Character; for anything else
  it returns Nothing. Callers use the Nothing case to mean "this value is not a
  string", rather than crashing.

Português:
  Tenta ler um valor como uma String comum de Haskell. Só dá certo (Just ...)
  para uma lista de caracteres (uma string de Bern), desembrulhando cada
  Character; para qualquer outra coisa retorna Nothing. Os chamadores usam o caso
  Nothing como "este valor não é uma string", em vez de quebrar.
-}
valueToString :: Value -> Maybe String
valueToString (List vals _) | allCharacter vals = Just (map (\(Character c) -> c) vals)
valueToString _ = Nothing
