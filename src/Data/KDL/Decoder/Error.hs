{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.KDL.Decoder.Error (
  DecodeError (..),
  BaseDecodeError (..),
  Context,
  ContextItem (..),
  renderDecodeError,
) where

import Data.KDL.Render (
  renderIdentifier,
  renderValue,
 )
import Data.KDL.Types (
  Identifier,
  Value,
 )
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

data DecodeError = DecodeError [(Context, BaseDecodeError)]
  deriving (Show, Eq)
instance Semigroup DecodeError where
  DecodeError e1 <> DecodeError e2 = DecodeError (e1 <> e2)
instance Monoid DecodeError where
  mempty = DecodeError []

type Context = [ContextItem]

data ContextItem
  = ContextNode
      { name :: Identifier
      , index :: Int
      }
  | ContextArg
      { index :: Int
      }
  | ContextProp
      { name :: Identifier
      }
  deriving (Show, Eq, Ord)

data BaseDecodeError
  = DecodeError_Custom Text
  | DecodeError_ParseError Text
  | DecodeError_ExpectedNode {name :: Text, index :: Int}
  | DecodeError_ExpectedArg {index :: Int}
  | DecodeError_ExpectedProp {name :: Text}
  | DecodeError_MismatchedAnn {givenAnn :: Identifier, validAnns :: [Text]}
  | DecodeError_ValueDecodeFail {expectedType :: Text, value :: Value}
  | DecodeError_UnexpectedNode {identifier :: Identifier, index :: Int}
  | DecodeError_UnexpectedArg {index :: Int, value :: Value}
  | DecodeError_UnexpectedProp {identifier :: Identifier, value :: Value}
  deriving (Show, Eq)

renderDecodeError :: DecodeError -> Text
renderDecodeError = Text.intercalate "\n" . map renderCtxErrors . groupCtxErrors
 where
  -- Group errors with the same contexts together
  groupCtxErrors (DecodeError es) = Map.toAscList $ Map.fromListWith (<>) [(ctx, [e]) | (ctx, e) <- es]

  renderCtxErrors (ctx, errs) =
    Text.intercalate "\n" $ ("At: " <> renderCtxItems ctx) : renderErrors errs

  renderCtxItems items
    | null items = "<root>"
    | otherwise = Text.intercalate " > " . map renderCtxItem $ items
  renderCtxItem = \case
    ContextNode{..} -> renderIdentifier name <> " #" <> showT index
    ContextArg{..} -> "arg #" <> showT index
    ContextProp{..} -> "prop " <> renderIdentifier name

  renderErrors = map ("  " <>) . concatMap (Text.lines . renderError)
  renderError = \case
    DecodeError_Custom msg -> msg
    DecodeError_ParseError msg -> msg
    DecodeError_ExpectedNode{..}
      | index == 0 -> "Expected node: " <> name
      | otherwise -> "Expected another node: " <> name
    DecodeError_ExpectedArg{..} -> "Expected arg #" <> showT index
    DecodeError_ExpectedProp{..} -> "Expected prop: " <> name
    DecodeError_MismatchedAnn{..} -> "Expected annotation to be one of " <> showT validAnns <> ", got: " <> renderIdentifier givenAnn
    DecodeError_ValueDecodeFail{..} -> "Expected " <> expectedType <> ", got: " <> renderValue value
    DecodeError_UnexpectedNode{..} -> "Unexpected node: " <> renderIdentifier identifier <> " #" <> showT index
    DecodeError_UnexpectedArg{..} -> "Unexpected arg #" <> showT index <> ": " <> renderValue value
    DecodeError_UnexpectedProp{..} -> "Unexpected prop: " <> renderIdentifier identifier <> "=" <> renderValue value

  -- Replace with Text.show after requiring at least text-2.1.2
  showT :: (Show a) => a -> Text
  showT = Text.pack . show
