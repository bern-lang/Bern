{-
Module: Data.Hashtable.Hashtable

English:
  A tiny, immutable key/value store. The whole interpreter uses one of these as
  its "environment" (or "scope"): the table that maps every variable and function
  name to the value it currently holds. When you write `x = 5` in Bern, the
  evaluator produces a new table that is the old one plus the binding x -> 5.

  Two design notes worth understanding:
    * It is a thin wrapper around Data.Map.Strict from the standard library. We
      keep our own name (`Hashtable`) and a 4-function interface so the rest of
      the codebase never has to care which container we chose -- if we ever
      swapped Data.Map for a real hash map, only this file would change.
    * It is IMMUTABLE. Inserting does not modify the table in place; it returns a
      brand-new table that shares most of its structure with the old one (this
      sharing is cheap because Data.Map is a balanced tree). That is why almost
      every evaluator function takes a table and RETURNS a (possibly new) table,
      instead of mutating one. Old snapshots stay valid, which is exactly what we
      want for things like function calls having their own local scope.

Português:
  Um pequeno armazém imutável de chave/valor. O interpretador inteiro usa um
  destes como seu "ambiente" (ou "escopo"): a tabela que mapeia cada nome de
  variável e função para o valor que ele guarda no momento. Quando você escreve
  `x = 5` em Bern, o avaliador produz uma nova tabela que é a antiga mais o
  vínculo x -> 5.

  Duas notas de design que vale entender:
    * É um invólucro fino em volta do Data.Map.Strict da biblioteca padrão.
      Mantemos nosso próprio nome (`Hashtable`) e uma interface de 4 funções para
      que o resto do código nunca precise se importar com qual container
      escolhemos -- se um dia trocássemos o Data.Map por um hash map de verdade,
      só este arquivo mudaria.
    * É IMUTÁVEL. Inserir não modifica a tabela no lugar; retorna uma tabela
      novinha que compartilha a maior parte da estrutura com a antiga (esse
      compartilhamento é barato porque o Data.Map é uma árvore balanceada). É por
      isso que quase toda função do avaliador recebe uma tabela e RETORNA uma
      tabela (possivelmente nova), em vez de mutar uma. Snapshots antigos
      continuam válidos, que é exatamente o que queremos para coisas como uma
      chamada de função ter o seu próprio escopo local.
-}
module Data.Hashtable.Hashtable
    ( Hashtable
    , emptyHashtable
    , insertHashtable
    , lookupHashtable
    ) where

import qualified Data.Map.Strict as M

{-
English:
  `newtype` (rather than `data`) means this wrapper has zero runtime cost: at
  runtime a `Hashtable` IS just the underlying Map, the wrapper only exists at
  compile time to give it a distinct name and hide the implementation. The record
  field `getMap` is the way back out to the raw Map, though nothing outside this
  module is allowed to use it (it is not exported).

Português:
  `newtype` (em vez de `data`) significa que este invólucro tem custo zero em
  tempo de execução: em runtime um `Hashtable` É apenas o Map por baixo, o
  invólucro só existe em tempo de compilação para lhe dar um nome distinto e
  esconder a implementação. O campo de registro `getMap` é o caminho de volta
  para o Map cru, embora nada fora deste módulo possa usá-lo (não é exportado).
-}
newtype Hashtable k v = Hashtable { getMap :: M.Map k v }
    deriving (Show, Eq)

-- Create an empty table -- the starting scope of a fresh program or REPL session.
-- Cria uma tabela vazia -- o escopo inicial de um programa novo ou sessão do REPL.
emptyHashtable :: Hashtable k v
emptyHashtable = Hashtable M.empty

{-
English:
  Insert (or overwrite) one binding and return the updated table. The original
  table is untouched. The `Ord k` constraint is required because Data.Map keeps
  keys in a sorted tree, so it needs to be able to compare them; our keys are
  always Strings, which are Ord.

Português:
  Insere (ou sobrescreve) um vínculo e retorna a tabela atualizada. A tabela
  original fica intacta. A restrição `Ord k` é necessária porque o Data.Map
  mantém as chaves numa árvore ordenada, então precisa conseguir compará-las;
  nossas chaves são sempre Strings, que são Ord.
-}
insertHashtable :: Ord k => Hashtable k v -> k -> v -> Hashtable k v
insertHashtable (Hashtable m) k v = Hashtable (M.insert k v m)

{-
English:
  Look a key up. The result is a `Maybe v`: `Just value` when the key exists, or
  `Nothing` when it does not. Returning Maybe (instead of crashing on a missing
  key) lets the caller decide what an unbound name should mean -- e.g. the
  evaluator turns it into the Bern value `Undefined`, or an error under the
  `no-undefined` pragma.

Português:
  Procura uma chave. O resultado é um `Maybe v`: `Just valor` quando a chave
  existe, ou `Nothing` quando não existe. Retornar Maybe (em vez de quebrar numa
  chave ausente) deixa o chamador decidir o que um nome não vinculado deve
  significar -- por exemplo, o avaliador o transforma no valor Bern `Undefined`,
  ou em erro sob o pragma `no-undefined`.
-}
lookupHashtable :: Ord k => Hashtable k v -> k -> Maybe v
lookupHashtable (Hashtable m) k = M.lookup k m
