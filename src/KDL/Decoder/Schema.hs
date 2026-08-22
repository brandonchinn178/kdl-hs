{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module KDL.Decoder.Schema (
  SchemaOf,
  Schema (..),
  SchemaItem (..),
  TypedNodeSchema (..),
  TypedValueSchema (..),
  schemaJoin,
  schemaAlt,

  -- * Fold over schema items
  foldSchema,
  getValueSchemaNames,
) where

import Data.Text (Text)
import Data.Typeable (TypeRep)
import KDL.Types (
  Node,
  NodeList,
  Value,
 )

type SchemaOf o = Schema (SchemaItem o)

data Schema a
  = SchemaOne a
  | SchemaSome (Schema a)
  | SchemaAnd [Schema a]
  | SchemaOr [Schema a]
  | SchemaUnknown
  deriving (Show, Eq)

data family SchemaItem a

data instance SchemaItem NodeList
  = NodeNamed Text TypedNodeSchema
  | RemainingNodes TypedNodeSchema
  deriving (Show, Eq)

data TypedNodeSchema = TypedNodeSchema
  { typeHint :: TypeRep
  , validTypeAnns :: [Text]
  , nodeSchema :: SchemaOf Node
  }
  deriving (Show, Eq)

data instance SchemaItem Node
  = NodeArg TypedValueSchema
  | NodeProp Text TypedValueSchema
  | NodeRemainingProps TypedValueSchema
  | NodeChildren (SchemaOf NodeList)
  deriving (Show, Eq)

data TypedValueSchema = TypedValueSchema
  { typeHint :: TypeRep
  , validTypeAnns :: [Text]
  , dataSchema :: SchemaOf Value
  }
  deriving (Show, Eq)

data instance SchemaItem Value
  = StringSchema
  | NumberSchema
  | BoolSchema
  | NullSchema
  deriving (Show, Eq, Ord, Enum, Bounded)

schemaJoin :: Schema a -> Schema a -> Schema a
schemaJoin = curry $ \case
  (SchemaAnd l, SchemaAnd r) -> SchemaAnd (l <> r)
  (l, SchemaAnd []) -> l
  (l, SchemaAnd r) -> SchemaAnd (l : r)
  (SchemaAnd [], r) -> r
  (SchemaAnd l, r) -> SchemaAnd (l <> [r])
  (l, r) -> SchemaAnd [l, r]

schemaAlt :: Schema a -> Schema a -> Schema a
schemaAlt = curry $ \case
  (SchemaOr l, SchemaOr r) -> SchemaOr (l <> r)
  (l, SchemaOr []) -> l
  (l, SchemaOr r) -> SchemaOr (l : r)
  (SchemaOr [], r) -> r
  (SchemaOr l, r) -> SchemaOr (l <> [r])
  (l, r) -> SchemaOr [l, r]

{------ Fold over schema items -----}

foldSchema :: (SchemaItem a -> b) -> SchemaOf a -> [b]
foldSchema f = go
 where
  go = \case
    SchemaOne valSchema -> [f valSchema]
    SchemaSome s -> go s
    SchemaAnd ss -> concatMap go ss
    SchemaOr ss -> concatMap go ss
    SchemaUnknown -> []

getValueSchemaNames :: SchemaOf Value -> [Text]
getValueSchemaNames = foldSchema $ \case
  StringSchema -> "string"
  NumberSchema -> "number"
  BoolSchema -> "bool"
  NullSchema -> "null"
