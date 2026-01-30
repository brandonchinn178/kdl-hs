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
                { value = "foo" , format = Just IdentifierFormat { repr = "foo" } }
          , entries =
              [ Entry
                  { name =
                      Just
                        Identifier
                          { value = "hello"
                          , format = Just IdentifierFormat { repr = "hello" }
                          }
                  , value =
                      Value
                        { ann = Nothing
                        , data_ = Text "world"
                        , format = Just ValueFormat { repr = "world" }
                        }
                  , format =
                      Just
                        EntryFormat
                          { leading = " " , afterKey = "" , afterEq = "" , trailing = "" }
                  }
              , Entry
                  { name = Nothing
                  , value =
                      Value
                        { ann = Nothing
                        , data_ = Number 1.0
                        , format = Just ValueFormat { repr = "1.0" }
                        }
                  , format =
                      Just
                        EntryFormat
                          { leading = " " , afterKey = "" , afterEq = "" , trailing = "" }
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
                                { value = "bar" , format = Just IdentifierFormat { repr = "bar" } }
                          , entries = []
                          , children = Nothing
                          , format =
                              Just
                                NodeFormat
                                  { leading = " "
                                  , beforeChildren = ""
                                  , beforeTerminator = ""
                                  , terminator = ";"
                                  , trailing = ""
                                  }
                          }
                      ]
                  , format = Just NodeListFormat { leading = "" , trailing = " " }
                  }
          , format =
              Just
                NodeFormat
                  { leading = ""
                  , beforeChildren = " "
                  , beforeTerminator = ""
                  , terminator = ""
                  , trailing = ""
                  }
          }
      ]
  , format = Just NodeListFormat { leading = "" , trailing = "" }
  }
```
