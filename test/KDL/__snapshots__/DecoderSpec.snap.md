# test/KDL/DecoderSpec.hs

## decodeFileWith / fails with helpful error if parsing fails

```
test_config.kdl:1:8:
  |
1 | foo 123=123
  |        ^
unexpected '='
expecting children block, decimal point, end of node, exponent, or node prop or arg
```

## decodeFileWith / fails with user-defined error

```
Failed to decode test_config.kdl:
At: foo #0 > arg #0
  Got negative number: -1.0
```

## decodeFileWith / shows context in deeply nested error

```
Failed to decode test_config.kdl:
At: foo #1 > bar #0 > baz #3 > prop a
  Expected text, got: 1
```

## decodeWith / fails with helpful error if parsing fails

```
1:8:
  |
1 | foo 123=123
  |        ^
unexpected '='
expecting children block, decimal point, end of node, exponent, or node prop or arg
```

## decodeWith / fails with user-defined error

```
At: foo #0 > arg #0
  Got negative number: -1.0
```

## decodeWith / shows context in deeply nested error

```
At: foo #1 > bar #0 > baz #3 > prop a
  Expected text, got: 1
```
