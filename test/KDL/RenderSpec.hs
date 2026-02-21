{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module KDL.RenderSpec (spec) where

import KDL qualified
import Skeletest
import Skeletest.Predicate qualified as P

spec :: Spec
spec = do
  describe "render" $ do
    describe "default formatting" $ do
      let ident s = KDL.Identifier{value = s, ext = KDL.def}
          fooAnn =
            KDL.Ann
              { identifier = ident "Foo"
              , ext = KDL.def
              }
          doc =
            KDL.NodeList
              { nodes =
                  [ KDL.Node
                      { ann = Just fooAnn
                      , name = ident "foo"
                      , entries =
                          [ KDL.Entry
                              { name = Nothing
                              , value =
                                  KDL.Value
                                    { ann = Just fooAnn
                                    , data_ = KDL.Number 123
                                    , ext = KDL.def
                                    }
                              , ext = KDL.def
                              }
                          , KDL.Entry
                              { name = Just $ ident "a"
                              , value =
                                  KDL.Value
                                    { ann = Just fooAnn
                                    , data_ = KDL.Number 123
                                    , ext = KDL.def
                                    }
                              , ext = KDL.def
                              }
                          , KDL.Entry
                              { name = Nothing
                              , value =
                                  KDL.Value
                                    { ann = Nothing
                                    , data_ = KDL.String "test"
                                    , ext = KDL.def
                                    }
                              , ext = KDL.def
                              }
                          , KDL.Entry
                              { name = Just $ ident "b"
                              , value =
                                  KDL.Value
                                    { ann = Nothing
                                    , data_ = KDL.String "test"
                                    , ext = KDL.def
                                    }
                              , ext = KDL.def
                              }
                          ]
                      , children =
                          Just
                            KDL.NodeList
                              { nodes =
                                  [ KDL.Node
                                      { ann = Nothing
                                      , name = ident "bar"
                                      , entries = []
                                      , children =
                                          Just
                                            KDL.NodeList
                                              { nodes =
                                                  [ KDL.Node
                                                      { ann = Nothing
                                                      , name = ident "baz"
                                                      , entries = []
                                                      , children = Nothing
                                                      , ext = KDL.def
                                                      }
                                                  ]
                                              , ext = KDL.def
                                              }
                                      , ext = KDL.def
                                      }
                                  ]
                              , ext = KDL.def
                              }
                      , ext = KDL.def
                      }
                  , KDL.Node
                      { ann = Just fooAnn
                      , name = ident "foo"
                      , entries = []
                      , children =
                          Just
                            KDL.NodeList
                              { nodes = []
                              , ext = KDL.def
                              }
                      , ext = KDL.def
                      }
                  ]
              , ext = KDL.def
              }

      it "renders correctly" $ do
        KDL.render doc `shouldSatisfy` P.matchesSnapshot

      it "can be parsed" $ do
        (KDL.parse . KDL.render) doc `shouldSatisfy` P.right P.anything
