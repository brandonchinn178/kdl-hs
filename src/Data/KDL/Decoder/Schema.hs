{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Data.KDL.Decoder.Schema (
  SchemaOf,
  Schema (..),
  SchemaItem (..),
  schemaJoin,
  schemaAlt,
) where

import Data.KDL.Types (
  BaseNode,
  BaseValue,
  Node,
  NodeList,
  Value,
 )
import Data.Text (Text)
import Data.Typeable (TypeRep)

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
  = NodeNamed Text (SchemaOf Node)
  | RemainingNodes (SchemaOf Node)

data instance SchemaItem Node
  = NodeSchema
  { typeHint :: TypeRep
  , validTypeAnns :: [Text]
  , baseNodeSchema :: SchemaOf BaseNode
  }

data instance SchemaItem BaseNode
  = NodeArg (SchemaOf Value)
  | NodeProp Text (SchemaOf Value)
  | NodeRemainingProps (SchemaOf Value)
  | NodeChildren (SchemaOf NodeList)

data instance SchemaItem Value
  = ValueSchema
  { typeHint :: TypeRep
  , validTypeAnns :: [Text]
  , baseValueSchema :: SchemaOf BaseValue
  }

data instance SchemaItem BaseValue
  = TextSchema
  | NumberSchema
  | BoolSchema
  | NullSchema
  deriving (Show, Eq, Ord, Enum, Bounded)

schemaJoin :: Schema a -> Schema a -> Schema a
schemaJoin = curry $ \case
  (SchemaAnd l, SchemaAnd r) -> SchemaAnd (l <> r)
  (l, SchemaAnd r) -> SchemaAnd (l : r)
  (SchemaAnd l, r) -> SchemaAnd (r : l)
  (l, r) -> SchemaAnd [l, r]

schemaAlt :: Schema a -> Schema a -> Schema a
schemaAlt = curry $ \case
  (SchemaOr l, SchemaOr r) -> SchemaOr (l <> r)
  (l, SchemaOr r) -> SchemaOr (l : r)
  (SchemaOr l, r) -> SchemaOr (r : l)
  (l, r) -> SchemaOr [l, r]
