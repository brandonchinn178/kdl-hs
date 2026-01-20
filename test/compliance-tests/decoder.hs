{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as ByteStringL
import Data.Char (intToDigit)
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
    , "entries" .= map encodeEntry node.entries
    , "children" .= maybe (Aeson.Array mempty) encodeNodeList node.children
    ]

encodeEntry :: KDL.Entry -> Aeson.Value
encodeEntry entry =
  Aeson.object
    [ "name" .= (encodeIdentifier <$> entry.name)
    , "type" .= (encodeAnn <$> entry.value.ann)
    , "value" .= encodeValueData entry.value.data_
    ]

encodeValueData :: KDL.ValueData -> Aeson.Value
encodeValueData = \case
  KDL.Text s -> val "string" (Text.unpack s)
  KDL.Number x -> val "number" (showNum x)
  KDL.Bool x -> val "boolean" (show x)
  KDL.Inf -> val "number" "inf"
  KDL.NegInf -> val "number" "-inf"
  KDL.NaN -> val "number" "nan"
  KDL.Null -> Aeson.object ["type" .= Text.pack "null"]
 where
  val :: String -> String -> Aeson.Value
  val ty v = Aeson.object ["type" .= ty, "value" .= v]

  showNum x =
    let (digits, e) = Scientific.toDecimalDigits x
        (i, f) = splitAt e $ map intToDigit digits
        orZero ds = if null ds then "0" else ds
     in orZero i <> "." <> orZero f

encodeAnn :: KDL.Ann -> Aeson.Value
encodeAnn = encodeIdentifier . (.identifier)

encodeIdentifier :: KDL.Identifier -> Aeson.Value
encodeIdentifier = Aeson.toJSON . (.value)
