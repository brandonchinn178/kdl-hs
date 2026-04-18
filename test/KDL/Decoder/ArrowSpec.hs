{-# LANGUAGE Arrows #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module KDL.Decoder.ArrowSpec (spec) where

import Control.Arrow (returnA)
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Typeable (typeRep)
import KDL.Arrow qualified as KDL
import KDL.Decoder.SharedSpec.Arrow
import Skeletest

spec :: Spec
spec = do
  apiSpec
  schemaSpec
  decodeNodeSpec
  decodeValueSpec

schemaSpec :: Spec
schemaSpec = do
  describe "documentSchema" $ do
    it "gets the schema of a decoder" $ do
      let decoder = KDL.document $ proc () -> do
            x <- KDL.nodeWith "foo" $ show <$> KDL.argWith decodeFoo -< ()
            ys <- KDL.many $ KDL.nodeWith "bar" $ KDL.arg @String -< ()

            zType <- KDL.argAt @Text "baz_type" -< ()
            z <- KDL.argAtWith "baz" decodeBaz -< zType

            returnA -< (x, ys, z)
          decodeFoo =
            KDL.oneOf
              [ Left <$> KDL.valueDecoder @Bool
              , Right <$> KDL.valueDecoder @Text
              ]
          decodeBaz = proc zType -> do
            case zType of
              "int" -> KDL.valueDecoder @Int64 -< ()
              "bool" -> (\b -> if b then 1 else 0) <$> KDL.valueDecoder @Bool -< ()
              _ -> KDL.fail -< "Invalid type: " <> zType
          expected =
            KDL.SchemaAnd
              [ KDL.SchemaOne . KDL.NodeNamed "foo" $
                  KDL.TypedNodeSchema
                    { typeHint = typeRep $ Proxy @String
                    , validTypeAnns = []
                    , nodeSchema =
                        KDL.SchemaOne . KDL.NodeArg $
                          KDL.TypedValueSchema
                            { typeHint = typeRep $ Proxy @(Either Bool Text)
                            , validTypeAnns = []
                            , dataSchema =
                                KDL.SchemaOr
                                  [ KDL.SchemaOne KDL.BoolSchema
                                  , KDL.SchemaOne KDL.TextSchema
                                  ]
                            }
                    }
              , KDL.SchemaOr
                  [ KDL.SchemaSome . KDL.SchemaOne . KDL.NodeNamed "bar" $
                      KDL.TypedNodeSchema
                        { typeHint = typeRep $ Proxy @String
                        , validTypeAnns = []
                        , nodeSchema =
                            KDL.SchemaOne . KDL.NodeArg $
                              KDL.TypedValueSchema
                                { typeHint = typeRep $ Proxy @String
                                , validTypeAnns = ["string"]
                                , dataSchema = KDL.SchemaOne KDL.TextSchema
                                }
                        }
                  , KDL.SchemaAnd []
                  ]
              , KDL.SchemaOne . KDL.NodeNamed "baz_type" $
                  KDL.TypedNodeSchema
                    { typeHint = typeRep $ Proxy @Text
                    , validTypeAnns = []
                    , nodeSchema =
                        KDL.SchemaOne . KDL.NodeArg $
                          KDL.TypedValueSchema
                            { typeHint = typeRep $ Proxy @Text
                            , validTypeAnns = ["string"]
                            , dataSchema = KDL.SchemaOne KDL.TextSchema
                            }
                    }
              , KDL.SchemaOne . KDL.NodeNamed "baz" $
                  KDL.TypedNodeSchema
                    { typeHint = typeRep $ Proxy @Int64
                    , validTypeAnns = []
                    , nodeSchema =
                        KDL.SchemaOne . KDL.NodeArg $
                          KDL.TypedValueSchema
                            { typeHint = typeRep $ Proxy @Int64
                            , validTypeAnns = []
                            , dataSchema =
                                KDL.SchemaOr
                                  [ KDL.SchemaOne KDL.NumberSchema
                                  , KDL.SchemaOne KDL.BoolSchema
                                  ]
                            }
                    }
              ]
      KDL.documentSchema decoder `shouldBe` expected
