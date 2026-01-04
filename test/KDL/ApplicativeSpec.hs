{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}

module KDL.ApplicativeSpec (spec) where

import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Typeable (typeRep)
import KDL.Applicative qualified as KDL
import KDL.Arrow ((>>>), (|||))
import KDL.Arrow qualified
import Skeletest

spec :: Spec
spec = do
  describe "documentSchema" $ do
    it "gets the schema of a decoder" $ do
      let decoder = KDL.document $ KDL.do
            x <-
              KDL.nodeWith "foo" $
                fmap show . KDL.argWith $
                  KDL.oneOf
                    [ Left <$> KDL.valueDecoder @Bool
                    , Right <$> KDL.valueDecoder @Text
                    ]

            ys <- KDL.many $ KDL.nodeWith "bar" $ KDL.arg @String

            z <- (parseBazType <$> KDL.argAt @Text "baz_type") >>> KDL.argAtWith "baz" decodeBaz

            pure (x, ys, z)
          parseBazType = \case
            "int" -> Left $ ()
            "bool" -> Right . Left $ ()
            ty -> Right . Right $ "Invalid type: " <> ty
          decodeBaz =
            -- "int"
            KDL.valueDecoder @Int64
              -- "bool"
              ||| ((\b -> if b then 1 else 0) <$> KDL.valueDecoder @Bool)
              -- else
              ||| KDL.Arrow.fail
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
                            , validTypeAnns = ["text"]
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
