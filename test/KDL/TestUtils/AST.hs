{-# LANGUAGE RecordWildCards #-}

module KDL.TestUtils.AST (
  scrubFormat,
) where

import KDL.Types

class ScrubFormat a where
  scrubFormat :: a -> a
instance ScrubFormat NodeList where
  scrubFormat NodeList{..} =
    NodeList
      { nodes = map scrubFormat nodes
      , ext = def
      }
instance ScrubFormat Node where
  scrubFormat Node{..} =
    Node
      { ann = scrubFormat <$> ann
      , name = scrubFormat name
      , entries = map scrubFormat entries
      , children = scrubFormat <$> children
      , ext = def
      }
instance ScrubFormat Entry where
  scrubFormat Entry{..} =
    Entry
      { name = scrubFormat <$> name
      , value = scrubFormat value
      , ext = def
      }
instance ScrubFormat Value where
  scrubFormat Value{..} =
    Value
      { ann = scrubFormat <$> ann
      , data_ = data_
      , ext = def
      }
instance ScrubFormat Ann where
  scrubFormat Ann{..} =
    Ann
      { identifier = scrubFormat identifier
      , ext = def
      }
instance ScrubFormat Identifier where
  scrubFormat Identifier{..} =
    Identifier
      { value = value
      , ext = def
      }
