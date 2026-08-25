# test/KDL/DecoderSpec.hs

## decodeWith ≫ fails with helpful error if parsing fails

```
1:8:
  |
1 | foo 123=123
  |        ^
unexpected '='
expecting children block, decimal point, end of node, exponent, or node prop or arg
```

## decodeWith ≫ fails with user-defined error

```
<input>:1:5:
    • Got negative number: -1.0
  │
1 │ foo -1
  │     ^^
```

## decodeWith ≫ shows context in deeply nested error

```
<input>:1:39:
    • Expected string, got: 1
  │
1 │ foo; foo { bar { baz; baz; baz; baz a=1; }; }
  │                                       ^
```

## decodeFileWith ≫ fails with helpful error if parsing fails

```
test_config.kdl:1:8:
  |
1 | foo 123=123
  |        ^
unexpected '='
expecting children block, decimal point, end of node, exponent, or node prop or arg
```

## decodeFileWith ≫ fails with user-defined error

```
test_config.kdl:1:5:
    • Got negative number: -1.0
  │
1 │ foo -1
  │     ^^
```

## decodeFileWith ≫ shows context in deeply nested error

```
test_config.kdl:1:39:
    • Expected string, got: 1
  │
1 │ foo; foo { bar { baz; baz; baz; baz a=1; }; }
  │                                       ^
```
