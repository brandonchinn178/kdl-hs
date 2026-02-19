# test/KDL/ParserSpec.hs

## parse / error messages / Unquoted numeric prop name

```
1:8:
  |
1 | foo 123=123
  |        ^
unexpected '='
expecting children block, decimal point, end of node, exponent, or node prop or arg
```

## parse / parses a KDL document

```haskell
NodeList
  { nodes =
      [ Node
          { ann = Nothing
          , name =
              Identifier
                { value = "foo"
                , ext =
                    IdentifierExtension
                      { format = Just IdentifierFormat { repr = Just "foo" } }
                }
          , entries =
              [ Entry
                  { name =
                      Just
                        Identifier
                          { value = "hello"
                          , ext =
                              IdentifierExtension
                                { format = Just IdentifierFormat { repr = Just "hello" } }
                          }
                  , value =
                      Value
                        { ann = Nothing
                        , data_ = String "world"
                        , ext =
                            ValueExtension
                              { format = Just ValueFormat { repr = Just "world" } }
                        }
                  , ext =
                      EntryExtension
                        { format =
                            Just
                              EntryFormat
                                { leading = " " , afterKey = "" , afterEq = "" , trailing = "" }
                        }
                  }
              , Entry
                  { name = Nothing
                  , value =
                      Value
                        { ann = Nothing
                        , data_ = Number 1.0
                        , ext =
                            ValueExtension { format = Just ValueFormat { repr = Just "1.0" } }
                        }
                  , ext =
                      EntryExtension
                        { format =
                            Just
                              EntryFormat
                                { leading = " " , afterKey = "" , afterEq = "" , trailing = "" }
                        }
                  }
              ]
          , children =
              Just
                NodeList
                  { nodes =
                      [ Node
                          { ann = Nothing
                          , name =
                              Identifier
                                { value = "bar"
                                , ext =
                                    IdentifierExtension
                                      { format = Just IdentifierFormat { repr = Just "bar" } }
                                }
                          , entries = []
                          , children = Nothing
                          , ext =
                              NodeExtension
                                { format =
                                    Just
                                      NodeFormat
                                        { leading = " "
                                        , beforeChildren = ""
                                        , beforeTerminator = ""
                                        , terminator = ";"
                                        , trailing = ""
                                        }
                                }
                          }
                      ]
                  , ext =
                      NodeListExtension
                        { format = Just NodeListFormat { leading = "" , trailing = " " } }
                  }
          , ext =
              NodeExtension
                { format =
                    Just
                      NodeFormat
                        { leading = ""
                        , beforeChildren = " "
                        , beforeTerminator = ""
                        , terminator = ""
                        , trailing = ""
                        }
                }
          }
      ]
  , ext =
      NodeListExtension
        { format = Just NodeListFormat { leading = "" , trailing = "" } }
  }
```
