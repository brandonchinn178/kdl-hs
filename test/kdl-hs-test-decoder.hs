{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as ByteStringL
import Data.Either (partitionEithers)
import Data.Map qualified as Map
import Data.Scientific qualified as Scientific
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import KDL qualified
import System.Exit (exitFailure)
import System.IO (stderr)

main :: IO ()
main = do
  input <- Text.getContents
  doc <-
    case KDL.parse input of
      Right doc -> pure doc
      Left e -> do
        Text.hPutStrLn stderr e
        exitFailure
  ByteStringL.putStrLn . Aeson.encode $ encodeNodeList doc

encodeNodeList :: KDL.NodeList -> Aeson.Value
encodeNodeList nodes = Aeson.toJSON . map encodeNode $ nodes.nodes

encodeNode :: KDL.Node -> Aeson.Value
encodeNode node =
  Aeson.object
    [ "type" .= (encodeAnn <$> node.ann)
    , "name" .= encodeIdentifier node.name
    , "args" .= args
    , "props" .= Map.fromList props
    , "children" .= maybe (Aeson.Array mempty) encodeNodeList node.children
    ]
 where
  (args, props) =
    partitionEithers
      [ case entry.name of
          Nothing -> Left val
          Just name -> Right (name.value, val)
      | entry <- node.entries
      , let val = encodeEntry entry
      ]

encodeEntry :: KDL.Entry -> Aeson.Value
encodeEntry entry =
  Aeson.object
    [ "type" .= (encodeAnn <$> entry.value.ann)
    , "value" .= encodeValueData entry.value.data_
    ]

encodeValueData :: KDL.ValueData -> Aeson.Value
encodeValueData = \case
  KDL.Text s -> val "string" (Text.unpack s)
  KDL.Number x -> val "number" (Scientific.formatScientific Scientific.Fixed Nothing x)
  KDL.Bool x -> val "boolean" (if x then "true" else "false")
  KDL.Inf -> val "number" "inf"
  KDL.NegInf -> val "number" "-inf"
  KDL.NaN -> val "number" "nan"
  KDL.Null -> Aeson.object ["type" .= Text.pack "null"]
 where
  val :: String -> String -> Aeson.Value
  val ty v = Aeson.object ["type" .= ty, "value" .= v]

encodeAnn :: KDL.Ann -> Aeson.Value
encodeAnn = encodeIdentifier . (.identifier)

encodeIdentifier :: KDL.Identifier -> Aeson.Value
encodeIdentifier = Aeson.toJSON . (.value)
