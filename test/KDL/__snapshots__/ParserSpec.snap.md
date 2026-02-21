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
                      { format = Just IdentifierFormat { repr = Just "foo" }
                      , span =
                          Span { startLine = 0 , startCol = 0 , endLine = 0 , endCol = 0 }
                      }
                }
          , entries =
              [ Entry
                  { name =
                      Just
                        Identifier
                          { value = "hello"
                          , ext =
                              IdentifierExtension
                                { format = Just IdentifierFormat { repr = Just "hello" }
                                , span =
                                    Span { startLine = 0 , startCol = 0 , endLine = 0 , endCol = 0 }
                                }
                          }
                  , value =
                      Value
                        { ann = Nothing
                        , data_ = String "world"
                        , ext =
                            ValueExtension
                              { format = Just ValueFormat { repr = Just "world" }
                              , span =
                                  Span { startLine = 0 , startCol = 0 , endLine = 0 , endCol = 0 }
                              }
                        }
                  , ext =
                      EntryExtension
                        { format =
                            Just
                              EntryFormat
                                { leading = " " , afterKey = "" , afterEq = "" , trailing = "" }
                        , span =
                            Span { startLine = 0 , startCol = 0 , endLine = 0 , endCol = 0 }
                        }
                  }
              , Entry
                  { name = Nothing
                  , value =
                      Value
                        { ann = Nothing
                        , data_ = Number 1.0
                        , ext =
                            ValueExtension
                              { format = Just ValueFormat { repr = Just "1.0" }
                              , span =
                                  Span { startLine = 0 , startCol = 0 , endLine = 0 , endCol = 0 }
                              }
                        }
                  , ext =
                      EntryExtension
                        { format =
                            Just
                              EntryFormat
                                { leading = " " , afterKey = "" , afterEq = "" , trailing = "" }
                        , span =
                            Span { startLine = 0 , startCol = 0 , endLine = 0 , endCol = 0 }
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
                                      { format = Just IdentifierFormat { repr = Just "bar" }
                                      , span =
                                          Span
                                            { startLine = 0
                                            , startCol = 0
                                            , endLine = 0
                                            , endCol = 0
                                            }
                                      }
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
                                , span =
                                    Span { startLine = 0 , startCol = 0 , endLine = 0 , endCol = 0 }
                                }
                          }
                      ]
                  , ext =
                      NodeListExtension
                        { format = Just NodeListFormat { leading = "" , trailing = " " }
                        , span =
                            Span { startLine = 0 , startCol = 0 , endLine = 0 , endCol = 0 }
                        }
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
                , span =
                    Span { startLine = 0 , startCol = 0 , endLine = 0 , endCol = 0 }
                }
          }
      ]
  , ext =
      NodeListExtension
        { format = Just NodeListFormat { leading = "" , trailing = "" }
        , span =
            Span { startLine = 0 , startCol = 0 , endLine = 0 , endCol = 0 }
        }
  }
```

## parseWith / parses a KDL document with spans

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
                      { format = Just IdentifierFormat { repr = Just "foo" }
                      , span =
                          Span { startLine = 1 , startCol = 1 , endLine = 1 , endCol = 3 }
                      }
                }
          , entries =
              [ Entry
                  { name = Nothing
                  , value =
                      Value
                        { ann = Nothing
                        , data_ = Number 1.0
                        , ext =
                            ValueExtension
                              { format = Just ValueFormat { repr = Just "1" }
                              , span =
                                  Span { startLine = 1 , startCol = 5 , endLine = 1 , endCol = 5 }
                              }
                        }
                  , ext =
                      EntryExtension
                        { format =
                            Just
                              EntryFormat
                                { leading = " " , afterKey = "" , afterEq = "" , trailing = "" }
                        , span =
                            Span { startLine = 1 , startCol = 5 , endLine = 1 , endCol = 5 }
                        }
                  }
              , Entry
                  { name = Nothing
                  , value =
                      Value
                        { ann = Nothing
                        , data_ = Number 2.0
                        , ext =
                            ValueExtension
                              { format = Just ValueFormat { repr = Just "2" }
                              , span =
                                  Span { startLine = 1 , startCol = 7 , endLine = 1 , endCol = 7 }
                              }
                        }
                  , ext =
                      EntryExtension
                        { format =
                            Just
                              EntryFormat
                                { leading = " " , afterKey = "" , afterEq = "" , trailing = "" }
                        , span =
                            Span { startLine = 1 , startCol = 7 , endLine = 1 , endCol = 7 }
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
                                      { format = Just IdentifierFormat { repr = Just "bar" }
                                      , span =
                                          Span
                                            { startLine = 2
                                            , startCol = 3
                                            , endLine = 2
                                            , endCol = 5
                                            }
                                      }
                                }
                          , entries =
                              [ Entry
                                  { name = Nothing
                                  , value =
                                      Value
                                        { ann = Nothing
                                        , data_ = Number 3.0
                                        , ext =
                                            ValueExtension
                                              { format = Just ValueFormat { repr = Just "3" }
                                              , span =
                                                  Span
                                                    { startLine = 2
                                                    , startCol = 7
                                                    , endLine = 2
                                                    , endCol = 7
                                                    }
                                              }
                                        }
                                  , ext =
                                      EntryExtension
                                        { format =
                                            Just
                                              EntryFormat
                                                { leading = " "
                                                , afterKey = ""
                                                , afterEq = ""
                                                , trailing = ""
                                                }
                                        , span =
                                            Span
                                              { startLine = 2
                                              , startCol = 7
                                              , endLine = 2
                                              , endCol = 7
                                              }
                                        }
                                  }
                              ]
                          , children = Nothing
                          , ext =
                              NodeExtension
                                { format =
                                    Just
                                      NodeFormat
                                        { leading = "\n  "
                                        , beforeChildren = ""
                                        , beforeTerminator = ""
                                        , terminator = "\n"
                                        , trailing = ""
                                        }
                                , span =
                                    Span { startLine = 2 , startCol = 3 , endLine = 2 , endCol = 7 }
                                }
                          }
                      ]
                  , ext =
                      NodeListExtension
                        { format = Just NodeListFormat { leading = "" , trailing = "" }
                        , span =
                            Span { startLine = 2 , startCol = 3 , endLine = 2 , endCol = 7 }
                        }
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
                , span =
                    Span { startLine = 1 , startCol = 1 , endLine = 3 , endCol = 1 }
                }
          }
      ]
  , ext =
      NodeListExtension
        { format = Just NodeListFormat { leading = "" , trailing = "" }
        , span =
            Span { startLine = 1 , startCol = 1 , endLine = 3 , endCol = 1 }
        }
  }
```
