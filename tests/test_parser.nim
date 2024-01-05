# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  unittest2, os, tables,
  ../toml_serialization,
  ../toml_serialization/lexer

template testNumOrDate(input: string, expectedOutput: untyped) =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  var value: type expectedOutput
  lex.parseNumOrDate(value)
  check:
    value == expectedOutput

template testParseValue(input: string, expectedOutput: untyped) =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  var value: type expectedOutput
  lex.parseValue(value)
  check:
    value == expectedOutput

template testParseValue(input: string, expectedOutput: untyped, flags: untyped) =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream, flags)
  var value: type expectedOutput
  lex.parseValue(value)
  check:
    value == expectedOutput

suite "test num or date parse to string":
  test "parseNumOrDate string":
    testNumOrDate("0xABCD", "ABCD")
    testNumOrDate("07:10:11", "07:10:11")
    testNumOrDate("0970-08-08 07:10:11", "0970-08-08 07:10:11")
    testNumOrDate("0", "0")
    testNumOrDate("+0", "+0")

    testNumOrDate("0.123", "0.123")
    testNumOrDate("0 ", "0")
    testNumOrDate("0[", "0")
    testNumOrDate("-0.123", "-0.123")
    testNumOrDate("-0 ", "-0")
    testNumOrDate("-0[", "-0")
    testNumOrDate("+0.123", "+0.123")
    testNumOrDate("+0 ", "+0")
    testNumOrDate("+0[", "+0")
    testNumOrDate("-nan", "-nan")
    testNumOrDate("-inf", "-inf")
    testNumOrDate("+nan", "+nan")
    testNumOrDate("+inf", "+inf")
    testNumOrDate("nan", "nan")
    testNumOrDate("inf", "inf")

    testNumOrDate("12:10:11", "12:10:11")
    testNumOrDate("1970-08-08 07:10:11", "1970-08-08 07:10:11")
    testNumOrDate("123", "123")
    testNumOrDate("123.123", "123.123")
    testNumOrDate("12_3.123", "123.123")
    testNumOrDate("123 ", "123")
    testNumOrDate("123[", "123")

    testNumOrDate("123E1", "123E1")
    testNumOrDate("123E+1", "123E+1")
    testNumOrDate("123E-1", "123E-1")
    testNumOrDate("123.123E-1", "123.123E-1")
    testNumOrDate("123.123E+1", "123.123E+1")

    expect TomlError:
      testNumOrDate("1__23 ", "123")

    expect TomlError:
      testNumOrDate("_123 ", "123")

    expect TomlError:
      testNumOrDate("123_ ", "123")

    testNumOrDate("-0", "-0")

suite "test num or date parse to toml":
  test "parseNumOrDate toml 1":
    var x = TomlValueRef(kind: TomlKind.Int, intVal: 0xABCD)
    testNumOrDate("0xABCD", x)

    var t = some(TomlTime(hour:7, minute:10, second:11))
    x = TomlValueRef(kind: TomlKind.DateTime, dateTime: TomlDateTime(time: t))
    testNumOrDate("07:10:11", x)

    var d = TomlDate(year:970, month:8, day:8)
    x.dateTime.date = some(d)
    testNumOrDate("0970-08-08 07:10:11", x)

  test "parseNumOrDate toml 2":
    var x = TomlValueRef(kind: TomlKind.Int, intVal: 0)
    testNumOrDate("0", x)
    testNumOrDate("-0", x)
    testNumOrDate("+0", x)
    testNumOrDate("0 ", x)
    testNumOrDate("0[", x)
    testNumOrDate("-0 ", x)
    testNumOrDate("-0[", x)
    testNumOrDate("+0 ", x)
    testNumOrDate("+0[", x)

    x = TomlValueRef(kind: TomlKind.Float, floatVal: 0.123'f64)
    testNumOrDate("+0.123", x)
    testNumOrDate("0.123", x)
    x = TomlValueRef(kind: TomlKind.Float, floatVal: -0.123'f64)
    testNumOrDate("-0.123", x)

    x = TomlValueRef(kind: TomlKind.Float, floatVal: NaN)
    testNumOrDate("-nan", x)
    x = TomlValueRef(kind: TomlKind.Float, floatVal: Inf)
    testNumOrDate("-inf", x)
    x = TomlValueRef(kind: TomlKind.Float, floatVal: NaN)
    testNumOrDate("+nan", x)
    x = TomlValueRef(kind: TomlKind.Float, floatVal: Inf)
    testNumOrDate("+inf", x)
    x = TomlValueRef(kind: TomlKind.Float, floatVal: NaN)
    testNumOrDate("nan", x)
    x = TomlValueRef(kind: TomlKind.Float, floatVal: Inf)
    testNumOrDate("inf", x)

    var t = some(TomlTime(hour:12, minute:10, second:11))
    x = TomlValueRef(kind: TomlKind.DateTime, dateTime: TomlDateTime(time: t))
    testNumOrDate("12:10:11", x)

    var d = TomlDate(year:1970, month:8, day:8)
    x.dateTime.date = some(d)
    testNumOrDate("1970-08-08 12:10:11", x)

    x = TomlValueRef(kind: TomlKind.Float, floatVal: 123.123'f64)
    testNumOrDate("123.123", x)
    testNumOrDate("12_3.123", x)

    x = TomlValueRef(kind: TomlKind.Int, intVal: 123)
    testNumOrDate("123", x)
    testNumOrDate("123 ", x)
    testNumOrDate("123[", x)

  test "parseNumOrDate toml 3":
    var x = TomlValueRef(kind: TomlKind.Float, floatVal: 123E1'f64)
    testNumOrDate("123E1", x)
    x = TomlValueRef(kind: TomlKind.Float, floatVal: 123E+1'f64)
    testNumOrDate("123E+1", x)
    x = TomlValueRef(kind: TomlKind.Float, floatVal: 123E-1'f64)
    testNumOrDate("123E-1", x)
    x = TomlValueRef(kind: TomlKind.Float, floatVal: 123.123E-1'f64)
    testNumOrDate("123.123E-1", x)
    x = TomlValueRef(kind: TomlKind.Float, floatVal: 123.123E+1'f64)
    testNumOrDate("123.123E+1", x)

    x = TomlValueRef(kind: TomlKind.Int, intVal: 123)
    expect TomlError:
      testNumOrDate("1__23 ", x)

    expect TomlError:
      testNumOrDate("_123 ", x)

    expect TomlError:
      testNumOrDate("123_ ", x)

  test "parseNumOrDate integer overflow":
    var x = TomlValueRef(kind: TomlKind.Int, intVal: high(int64))
    testNumOrDate($(high(int64)), x)
    expect TomlError:
      testNumOrDate($(high(int64).uint64 + 1'u64), x)
    x = TomlValueRef(kind: TomlKind.Int, intVal: low(int64))
    testNumOrDate($(low(int64)), x)
    expect TomlError:
      testNumOrDate("-" & $(high(int64).uint64 + 2'u64), x)

suite "test value parser":
  test "parseValue string":
    testParseValue("0", "0")
    testParseValue("123", "123")
    testParseValue("true", "true")
    testParseValue("nan", "nan")
    testParseValue("\"basic string\"", "basic string")
    testParseValue("\'literal string\'", "literal string")
    testParseValue("\"\"\"ml basic string\"\"\"", "ml basic string")
    testParseValue("\'\'\'ml literal string\'\'\'", "ml literal string")

  test "parseValue toml":
    var x = TomlValueRef(kind: TomlKind.Int, intVal: 0)
    testParseValue("0", x)
    x = TomlValueRef(kind: TomlKind.Int, intVal: 123)
    testParseValue("123", x)
    x = TomlValueRef(kind: TomlKind.Bool, boolVal: true)
    testParseValue("true", x)
    x = TomlValueRef(kind: TomlKind.Float, floatVal: NaN)
    testParseValue("nan", x)

    x = TomlValueRef(kind: TomlKind.String, stringVal: "basic string")
    testParseValue("\"basic string\"", x)
    x = TomlValueRef(kind: TomlKind.String, stringVal: "literal string")
    testParseValue("\'literal string\'", x)
    x = TomlValueRef(kind: TomlKind.String, stringVal: "ml basic string")
    testParseValue("\"\"\"ml basic string\"\"\"", x)
    x = TomlValueRef(kind: TomlKind.String, stringVal: "ml literal string")
    testParseValue("\'\'\'ml literal string\'\'\'", x)

suite "test array and table parser":
  test "parseArray":
    testParseValue("[123, \"hello\"]", "[123,hello]")
    testParseValue("[123, \"hello\",]", "[123,hello]")
    var x = TomlValueRef(kind: TomlKind.Array,
      arrayVal: @[
        TomlValueRef(kind: TomlKind.Int, intVal: 123),
        TomlValueRef(kind: TomlKind.String, stringVal: "hello")
      ]
    )
    testParseValue("[123, \"hello\"]", x)
    testParseValue("[123, \"hello\",]", x)

    expect TomlError:
      testParseValue("[123, \"hello\"", "[123,hello]")

    expect TomlError:
      testParseValue("[123, \"hello\"", x)

  test "parseInlineTable":
    testParseValue("{number = 123, name = \"hello\"}", "{number=123,name=hello}")

    var x = TomlValueRef(kind: TomlKind.InlineTable, tableVal: new(TomlTableRef))
    x.tableVal["number"] = TomlValueRef(kind: TomlKind.Int, intVal: 123)
    x.tableVal["name"] = TomlValueRef(kind: TomlKind.String, stringVal: "hello")

    testParseValue("{number = 123, name = \"hello\"}", x)

    expect TomlError:
      testParseValue("{number = 123, name = \"hello\",}", "{number=123,name=hello}")

suite "test misc parser":
  test "bugfix 1":
    testNumOrDate("0e0", "0e0")

  test "bugfix 2":
    testNumOrDate("-0e0", "-0e0")

  test "bugfix 3":
    testNumOrDate("+0e0", "+0e0")

  test "bugfix 4":
    var x = TomlValueRef(kind: TomlKind.Float, floatVal: 0.0)
    testNumOrDate("0e0", x)

  test "bugfix 5":
    var x = TomlValueRef(kind: TomlKind.Float, floatVal: -0.0)
    testNumOrDate("-0e0", x)

  test "bugfix 6":
    var x = TomlValueRef(kind: TomlKind.Float, floatVal: 0.0)
    testNumOrDate("+0e0", x)

  test "parseArray no comma":
    expect TomlError:
      testParseValue("[true false]", "[truefalse]", {TomlStrictComma})

  test "parseArray double comma":
    expect TomlError:
      testParseValue("[true , , false]", "[truefalse]", {TomlStrictComma})

  test "parseInlineTable no comma":
    expect TomlError:
      testParseValue("{a = 1 b = 2}", "{a=1", {TomlStrictComma})

  test "parseInlineTable double comma":
    expect TomlError:
      testParseValue("{a = 1, , b = 2}", "{a=1,", {TomlStrictComma})

  test "TomlStrictComma turned off":
    testParseValue("[true false]", "[truefalse]")
    testParseValue("[true , , false]", "[true,,false]")
    testParseValue("{a = 1 b = 2}", "{a=1b=2}")
    testParseValue("{a = 1, , b = 2}", "{a=1,,b=2}")
