{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module KDL.Decoder.Internal.Error (
  DecodeError (..),
  BaseDecodeError,
  DecodeErrorKind (..),
  Context (..),
  ContextItem (..),
  renderDecodeError,
) where

import Control.Exception (Exception (..))
import Data.Default (Default (..))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import KDL.Render (
  renderIdentifier,
  renderValue,
 )
import KDL.Types (
  Identifier,
  Span (..),
  Value,
 )
import System.FilePath (takeFileName)
import Prelude hiding (span)

data DecodeError = DecodeError
  { filepath :: Maybe FilePath
  , errors :: NonEmpty BaseDecodeError
  }
  deriving (Show, Eq)

instance Exception DecodeError where
  displayException = Text.unpack . renderDecodeError

type BaseDecodeError = (Context, DecodeErrorKind)

data Context = Context
  { path :: [ContextItem]
  , span :: Maybe Span
  , srcLine :: Maybe Text
  }
  deriving (Show, Eq)

instance Default Context where
  def =
    Context
      { path = []
      , span = Nothing
      , srcLine = Nothing
      }

data ContextItem
  = ContextNode
      { name :: Identifier
      , index :: Int
      }
  | ContextArg
      { index :: Int
      , label :: Maybe Text
      }
  | ContextProp
      { name :: Identifier
      }
  deriving (Show, Eq, Ord)

data DecodeErrorKind
  = DecodeError_Custom Text
  | DecodeError_ParseError Text
  | DecodeError_ExpectedNode {name :: Text, index :: Int}
  | DecodeError_ExpectedArg {index :: Int, label :: Maybe Text, expectedTypes :: [Text]}
  | DecodeError_ExpectedProp {name :: Text, expectedTypes :: [Text]}
  | DecodeError_MismatchedAnn {givenAnn :: Identifier, validAnns :: [Text]}
  | DecodeError_ValueDecodeFail {expectedType :: Text, value :: Value}
  | DecodeError_UnexpectedNode {identifier :: Identifier, index :: Int}
  | DecodeError_UnexpectedArg {index :: Int, value :: Value}
  | DecodeError_UnexpectedProp {identifier :: Identifier, value :: Value}
  deriving (Show, Eq)

renderDecodeError :: DecodeError -> Text
renderDecodeError decodeError =
  Text.intercalate "\n"
    . concatMap renderCtxErrors
    . NonEmpty.groupAllWith1 groupKey
    $ decodeError.errors
 where
  -- Group errors with the same contexts together
  groupKey (ctx, _) = maybe (Left ctx.path) Right ctx.span

  renderCtxErrors = \case
    -- Special case parse errors, which shouldn't have a context
    (_, DecodeError_ParseError msg) NonEmpty.:| _ -> [msg]
    errs ->
      let (ctx, _) = NonEmpty.head errs
       in renderCtx ctx $ (map (renderError . snd) $ NonEmpty.toList errs)

  renderCtx (ctx :: Context) =
    case ctx.span of
      Nothing -> renderCtxPath ctx.path
      Just span -> renderCtxFull span ctx

  -- If we don't have the error span, the best we can do is render the context path:
  --
  -- At: foo.kdl > user #0 > arg #0
  -- ├─ error message
  -- └─ another error message
  renderCtxPath path errors =
    let pathDisplay =
          Text.intercalate " > " . concat $
            [ case decodeError.filepath of
                Nothing -> []
                Just fp -> [Text.pack $ takeFileName fp]
            , if null path then ["(root)"] else map renderCtxPathItem path
            ]
        errors' =
          [ (if isLast then "└─ " else "├─ ") <> err
          | (err, isLast) <- withIsLast errors
          ]
     in ("At: " <> pathDisplay) : errors'
  renderCtxPathItem = \case
    ContextNode{..} -> renderIdentifier name <> " #" <> showT index
    ContextArg{..} -> renderArg index label
    ContextProp{..} -> "prop " <> renderIdentifier name

  -- If we have the error span, show a descriptive error message:
  --
  -- foo.kdl:3:16:
  --     • Expected number, got string
  --   |
  -- 3 |     some_child bad-value
  --   |                ^^^^^^^^^
  renderCtxFull (span :: Span) ctx errors =
    let spanDisplay =
          Text.concat . map (<> ":") $
            [ maybe "<input>" Text.pack decodeError.filepath
            , showT span.startLine
            , showT span.startCol
            ]
        errors' = map ("    • " <>) errors
        preview =
          case ctx.srcLine of
            Nothing -> []
            Just line ->
              let lineNum = showT span.startLine
                  spaces n = Text.replicate n " "
                  renderPrefix isSpace = (if isSpace then spaces (Text.length lineNum) else lineNum) <> " │"
                  spanLength =
                    if span.startLine == span.endLine
                      then span.endCol - span.startCol + 1
                      else Text.length line - span.startCol + 1
               in [ renderPrefix True
                  , renderPrefix False <> " " <> line
                  , renderPrefix True <> spaces span.startCol <> Text.replicate spanLength "^"
                  ]
     in spanDisplay : errors' ++ preview

  renderError = \case
    DecodeError_Custom msg -> msg
    DecodeError_ParseError msg -> msg
    DecodeError_ExpectedNode{..}
      | index == 0 -> "Expected node: " <> name
      | otherwise -> "Expected another node: " <> name
    DecodeError_ExpectedArg{..} ->
      Text.concat
        [ "Expected "
        , renderArg index label
        , if null expectedTypes
            then ""
            else " with type: " <> oxfordList "or" expectedTypes
        ]
    DecodeError_ExpectedProp{..} ->
      Text.concat
        [ "Expected prop '" <> name <> "'"
        , if null expectedTypes
            then ""
            else " with type: " <> oxfordList "or" expectedTypes
        ]
    DecodeError_MismatchedAnn{..} -> "Expected annotation to be one of " <> showT validAnns <> ", got: " <> renderIdentifier givenAnn
    DecodeError_ValueDecodeFail{..} -> "Expected " <> expectedType <> ", got: " <> renderValue value
    DecodeError_UnexpectedNode{..} -> "Unexpected node: " <> renderIdentifier identifier <> " #" <> showT index
    DecodeError_UnexpectedArg{..} -> "Unexpected arg #" <> showT index <> ": " <> renderValue value
    DecodeError_UnexpectedProp{..} -> "Unexpected prop: " <> renderIdentifier identifier <> "=" <> renderValue value

  renderArg index label = "arg " <> maybe ("#" <> showT index) (\s -> "'" <> s <> "'") label

  oxfordList conj = \case
    [x] -> x
    [x, y] -> Text.unwords [x, conj, y]
    xs -> Text.intercalate ", " $ mapLast ((conj <> " ") <>) xs

  mapLast f = \case
    [] -> []
    [x] -> [f x]
    x : xs -> x : mapLast f xs

  withIsLast = mapLast (True <$) . map (\x -> (x, False))

  -- Replace with Text.show after requiring at least text-2.1.2
  showT :: (Show a) => a -> Text
  showT = Text.pack . show
