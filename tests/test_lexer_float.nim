# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  faststreams, unittest2

# we want to test lexer internals
include
  ../toml_serialization/lexer

proc scanDecimalPart[T](input: string): T =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  lex.scanDecimalPart(result, Sign.None)

template testDecimalPart(input: string, expectedOutput: untyped) =
  let value = scanDecimalPart[type expectedOutput](input)
  check:
    when (type value) is string:
      value == expectedOutput
    else:
      abs(value - expectedOutput) < 1E-7

proc scanFrac[T](input: string): T =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  lex.scanFrac(result, Sign.None)

template testScanFrac(input: string, expectedOutput: untyped) =
  let value = scanFrac[type expectedOutput](input)
  check:
    when (type value) is string:
      value == expectedOutput
    else:
      abs(value - expectedOutput) < 1E-7

proc scanFloat[T](input: string): T =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  discard lex.scanFloat(result)

template testScanFloat(input: string, expectedOutput: untyped, special: static[bool] = false) =
  let value = scanFloat[type expectedOutput](input)
  when not special:
    check:
      when (type value) is string:
        value == expectedOutput
      else:
        abs(value - expectedOutput) < 1E-7
  else:
    check $value == $expectedOutput

suite "float test suite 1":
  test "scanDecimalPart":
    testDecimalPart("123", 0.123'f64)
    testDecimalPart("123", 0.123'f32)
    testDecimalPart("123", "123")

    testDecimalPart("1_2_3", 0.123'f64)
    testDecimalPart("1_2_3", 0.123'f32)
    testDecimalPart("1_2_3", "123")

    expect TomlError:
      testDecimalPart("1__2_3", 0.123'f64)

    expect TomlError:
      testDecimalPart("1__2_3", 0.123'f32)

    expect TomlError:
      testDecimalPart("1__2_3", "123")

    expect TomlError:
      testDecimalPart("_1_2_3", 0.123'f64)

    expect TomlError:
      testDecimalPart("_1_2_3", 0.123'f32)

    expect TomlError:
      testDecimalPart("_1_2_3", "123")

    expect TomlError:
      testDecimalPart("1_2_3_", 0.123'f64)

    expect TomlError:
      testDecimalPart("1_2_3_", 0.123'f32)

    expect TomlError:
      testDecimalPart("1_2_3_", "123")

  test "scanFrac string":
    testScanFrac("123", "123")
    testScanFrac("123e1", "123e1")
    testScanFrac("123e+1", "123e+1")
    testScanFrac("123e-1", "123e-1")
    testScanFrac("123e+0", "123e+0")
    testScanFrac("123e-0", "123e-0")
    testScanFrac("123e06", "123e06")

    testScanFrac("123E1",  "123E1")
    testScanFrac("123E+1", "123E+1")
    testScanFrac("123E06", "123E06")
    testScanFrac("123E+0", "123E+0")
    testScanFrac("123E-0", "123E-0")
    testScanFrac("123E-1", "123E-1")

    expect TomlError:
      testScanFrac("123e", "123e")

    expect TomlError:
      testScanFrac("123e+", "123e+")

    expect TomlError:
      testScanFrac("123e-", "123e-")

  test "scanFrac float":
    testScanFrac("123",    0.123'f64)
    testScanFrac("123e1",  0.123e1'f64)
    testScanFrac("123e+1", 0.123e1'f64)
    testScanFrac("123e-1", 0.123e-1'f64)
    testScanFrac("123e+0", 0.123e+0'f64)
    testScanFrac("123e-0", 0.123e-0'f64)
    testScanFrac("123e06", 0.123e06'f64)

    testScanFrac("123E1",  0.123E1'f64)
    testScanFrac("123E+1", 0.123E+1'f64)
    testScanFrac("123E-1", 0.123E-1'f64)
    testScanFrac("123E06", 0.123E06'f64)
    testScanFrac("123E+0", 0.123E+0'f64)
    testScanFrac("123E-0", 0.123E-0'f64)

    testScanFrac("123",    0.123'f32)
    testScanFrac("123e1",  0.123e1'f32)
    testScanFrac("123e+1", 0.123e1'f32)
    testScanFrac("123e-1", 0.123e-1'f32)
    testScanFrac("123e+0", 0.123e+0'f32)
    testScanFrac("123e-0", 0.123e-0'f32)
    testScanFrac("123e06", 0.123e06'f32)

    testScanFrac("123E1",  0.123E1'f32)
    testScanFrac("123E+1", 0.123E+1'f32)
    testScanFrac("123E-1", 0.123E-1'f32)
    testScanFrac("123E06", 0.123E06'f32)
    testScanFrac("123E+0", 0.123E+0'f32)
    testScanFrac("123E-0", 0.123E-0'f32)

    expect TomlError:
      testScanFrac("123e", 0.123e0)

    expect TomlError:
      testScanFrac("123e+", 0.123e+0)

    expect TomlError:
      testScanFrac("123e-", 0.123e-0)

suite "float test suite 2":
  test "scanFloat string":
    testScanFloat("123", "123")
    testScanFloat("+123", "+123")
    testScanFloat("-123", "-123")

    testScanFloat("+inf", "+inf")
    testScanFloat("-inf", "-inf")
    testScanFloat("+nan", "+nan")
    testScanFloat("-nan", "-nan")

    testScanFloat("123E1", "123E1")
    testScanFloat("-123E1", "-123E1")
    testScanFloat("+123E1", "+123E1")

    testScanFloat("123E+1", "123E+1")
    testScanFloat("-123E+1", "-123E+1")
    testScanFloat("+123E+1", "+123E+1")

    testScanFloat("123E-1", "123E-1")
    testScanFloat("-123E-1", "-123E-1")
    testScanFloat("+123E-1", "+123E-1")

    expect TomlError:
      testScanFloat("+", "+0")

    expect TomlError:
      testScanFloat("-", "-0")

    testScanFloat("-123.123", "-123.123")
    testScanFloat("123.123", "123.123")

    testScanFloat("-123.123E+1", "-123.123E+1")
    testScanFloat("-123.123E-1", "-123.123E-1")
    testScanFloat("123.123E-1", "123.123E-1")
    testScanFloat("123.123E+1", "123.123E+1")

    expect TomlError:
      testScanFloat("123.a", "123.")

    testScanFloat("0e0", "0e0")
    testScanFloat("-0e0", "-0e0")
    testScanFloat("+0e0", "+0e0")

suite "float test suite 3":
  test "scanFloat float":
    testScanFloat("123", 123'f64)
    testScanFloat("+123", 123'f64)
    testScanFloat("-123", -123'f64)

    testScanFloat("+inf", Inf, true)
    testScanFloat("-inf", -Inf, true)
    testScanFloat("+nan", NaN, true)
    testScanFloat("-nan", -NaN, true)

    testScanFloat("123E1", 123E1)
    testScanFloat("-123E1", -123E1)
    testScanFloat("+123E1", 123E1)

    testScanFloat("123E+1", 123E+1)
    testScanFloat("-123E+1", -123E+1)
    testScanFloat("+123E+1", 123E+1)

    testScanFloat("123E-1", 123E-1)
    testScanFloat("-123E-1", -123E-1)
    testScanFloat("+123E-1", 123E-1)

    testScanFloat("-123.123", -123.123'f64)
    testScanFloat("123.123", 123.123'f64)

    expect TomlError:
      testScanFloat("+", 0'f64)

    expect TomlError:
      testScanFloat("-", -0'f64)

suite "float test suite 4":
  test "scanFloat float":
    testScanFloat("123", 123'f32)
    testScanFloat("+123", 123'f32)
    testScanFloat("-123", -123'f32)

    testScanFloat("-123.123", -123.123'f32)
    testScanFloat("123.123", 123.123'f32)

    testScanFloat("-123.123E+1", -123.123E+1'f64)
    testScanFloat("-123.123E-1", -123.123E-1)
    testScanFloat("123.123E-1", 123.123E-1)
    testScanFloat("123.123E+1", 123.123E+1)

    testScanFloat("0e0", 0e0)
    testScanFloat("-0e0", -0e0)
    testScanFloat("+0e0", 0e0)

    expect TomlError:
      testScanFloat("123.a", 123.0'f32)

    expect TomlError:
      testScanFloat("+", 0'f32)

    expect TomlError:
      testScanFloat("-", -0'f32)

    expect TomlError:
      testScanFloat("123.a", 123.0'f64)

suite "test toml v1.0.0-rc.2":
  test "zero leading exponent":
    testScanFloat("123E01", 123E1)
    testScanFloat("-123.123E+01", -123.123E+1'f64)
    testScanFloat("-123.123E-01", -123.123E-1)
