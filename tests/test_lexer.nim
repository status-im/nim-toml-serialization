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

template testScanUint(input: string, output, base, leading: untyped) =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  type T = type output
  var value: T
  lex.scanUint(value, base, leading)
  check value == output

template testScanEncoding(input: string, expectedOutput, expectedBase: untyped) =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  type T = type expectedOutput
  var value: T
  let resultBase = lex.scanEncoding(value)
  check:
    value == expectedOutput
    resultBase == expectedBase

template testScanUnicode(input: string, expectedOutput: string) =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  var value: string
  lex.scanUnicode(lex.next, value)
  check:
    value == expectedOutput

template testBasicString(input: string, expectedOutput: string) =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  var value = lex.scanString(StringType.Basic)
  check:
    value == expectedOutput

template testLiteralString(input: string, expectedOutput: string) =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  var value = lex.scanString(StringType.Literal)
  check:
    value == expectedOutput

template testScanInt(input: string, expectedOutput, expectedBase, expectedSign: untyped) =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  var value: type expectedOutput
  let (sign, base) = lex.scanInt(value)
  check:
    value == expectedOutput
    sign  == expectedSign
    base  == expectedBase

template testScanName(input: string, expectedOutput: string) =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  var value: string
  lex.scanName(value)
  check:
    value == expectedOutput

template testScanKey(input: string, expectedOutput: untyped) =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  var value: type expectedOutput
  lex.scanKey(value)
  check:
    value == expectedOutput

template testTableName(input: string, expectedOutput: untyped,
                       expectedBracket = BracketType.single) =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  var value: type expectedOutput
  let bracket = lex.scanTableName(value)
  check:
    value == expectedOutput
    bracket == expectedBracket

template testScanBool(input: string, expectedOutput: untyped) =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  check:
    lex.scanBool() == expectedOutput

template testDecimalPart(input: string, expectedOutput: untyped) =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  var value: type expectedOutput
  lex.scanDecimalPart(value, Sign.None)
  check:
    when (type value) is string:
      value == expectedOutput
    else:
      abs(value - expectedOutput) < 1E-7

template testScanFrac(input: string, expectedOutput: untyped) =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  var value: type expectedOutput
  lex.scanFrac(value, Sign.None)
  check:
    when (type value) is string:
      value == expectedOutput
    else:
      abs(value - expectedOutput) < 1E-7

template testScanFloat(input: string, expectedOutput: untyped, special: static[bool] = false) =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  var value: type expectedOutput
  discard lex.scanFloat(value)
  when not special:
    check:
      when (type value) is string:
        value == expectedOutput
      else:
        abs(value - expectedOutput) < 1E-7
  else:
    check $value == $expectedOutput

template testDateTime(input: string, expectedOutput: untyped, zeroLead = false) =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  var value: type expectedOutput
  when expectedOutput is string:
    if zeroLead: value.add '0'
  lex.scanDateTime(value, zeroLead)
  check:
    value == expectedOutput

template testComment(input: string, expectedOutput: untyped) =
  var stream = unsafeMemoryInput(input)
  var lex = init(TomlLexer, stream)
  let output = lex.nonws(skipLf)
  check:
    output == expectedOutput

suite "numbers test suite":
  test "scanUint string":
    testScanUint("1234567890", "1234567890", base10, Leading.DenyZero)
    testScanUint("01234567890", "01234567890", base10, Leading.AllowZero)
    testScanUint("0", "0", base10, Leading.DenyZero)
    testScanUint("0", "0", base10, Leading.AllowZero)
    expect TomlError:
      testScanUint("01234567890", "1234567890", base10, Leading.DenyZero)

    testScanUint("1234567890", "1234567", base8, Leading.DenyZero)
    testScanUint("1234567890", "1", base2, Leading.DenyZero)
    testScanUint("1234567890abcdef", "1234567890abcdef", base16, Leading.DenyZero)

    testScanUint("0a1234567890", "0", base10, Leading.DenyZero)
    testScanUint("0a1234567890", "0", base10, Leading.AllowZero)

  test "scanUint int":
    testScanUint("1234567890", 1234567890'u64, base10, Leading.DenyZero)
    testScanUint("01234567890", 1234567890'u64, base10, Leading.AllowZero)
    testScanUint("0", 0'u64, base10, Leading.DenyZero)
    testScanUint("0", 0'u64, base10, Leading.AllowZero)
    expect TomlError:
      testScanUint("01234567890", 1234567890'u64, base10, Leading.DenyZero)

    testScanUint("1234567890", 342391'u64, base8, Leading.DenyZero)
    testScanUint("1234567890", 1'u64, base2, Leading.DenyZero)
    testScanUint("1234567890abcdef", 1311768467294899695'u64, base16, Leading.DenyZero)

    testScanUint("0a1234567890", 0'u64, base10, Leading.DenyZero)
    testScanUint("0a1234567890", 0'u64, base10, Leading.AllowZero)

  test "scanUint uint overflow":
    testScanUint("18446744073709551616", 0'u64, base10, Leading.DenyZero)
    testScanUint("18446744073709551615", 18446744073709551615'u64, base10, Leading.DenyZero)

  test "scanUint underscore int":
    testScanUint("123_4_5_6", 123456'u64, base10, Leading.DenyZero)
    testScanUint("0123_4_5_6", 123456'u64, base10, Leading.AllowZero)

    expect TomlError:
      testScanUint("_123_4_5_6", 123456'u64, base10, Leading.DenyZero)

    expect TomlError:
      testScanUint("_123_4_5_6", 123456'u64, base10, Leading.DenyZero)

    expect TomlError:
      testScanUint("123_4_5_6_", 123456'u64, base10, Leading.DenyZero)

    expect TomlError:
      testScanUint("123__4_5_6", 123456'u64, base10, Leading.DenyZero)

  test "scanUint underscore string":
    testScanUint("123_4_5_6", "123456", base10, Leading.DenyZero)
    testScanUint("0123_4_5_6", "0123456", base10, Leading.AllowZero)

    expect TomlError:
      testScanUint("_123_4_5_6", "123456", base10, Leading.DenyZero)

    expect TomlError:
      testScanUint("_123_4_5_6", "123456", base10, Leading.DenyZero)

    expect TomlError:
      testScanUint("123_4_5_6_", "123456", base10, Leading.DenyZero)

    expect TomlError:
      testScanUint("123__4_5_6", "123456", base10, Leading.DenyZero)

  test "scanEncoding int":
    testScanEncoding("b101", 5'u64, base2)
    testScanEncoding("o4567", 2423'u64, base8)
    testScanEncoding("xabcd", 43981'u64, base16)

    testScanEncoding("xabcdG", 43981'u64, base16)

    expect TomlError:
      testScanEncoding("yabcd", 43981'u64, base16)

    expect TomlError:
      testScanEncoding("1234", 43981'u64, base10)

  test "scanEncoding string":
    testScanEncoding("b101", "101", base2)
    testScanEncoding("o4567", "4567", base8)
    testScanEncoding("xabcd", "abcd", base16)

    testScanEncoding("xabcdG", "abcd", base16)

    expect TomlError:
      testScanEncoding("yabcd", "abcd", base16)

    expect TomlError:
      testScanEncoding("1234", "abcd", base10)

suite "string test suite":
  test "scanUnicode":
    expect TomlError:
      testScanUnicode("u0", "0")

    expect TomlError:
      testScanUnicode("u111", "0")

    expect TomlError:
      testScanUnicode("U0", "0")

    expect TomlError:
      testScanUnicode("U00110000", "0")

    expect TomlError:
      # surrogate pair
      testScanUnicode("uD800", "0")

    expect TomlError:
      # surrogate pair
      testScanUnicode("uDFFF", "0")

    expect TomlError:
      testScanUnicode("u\n0000", "\u0000")

    testScanUnicode("u0000", "\u0000")
    testScanUnicode("uD7FF", "\uD7FF")
    testScanUnicode("uE000", "\uE000")
    testScanUnicode("U0010FFFF", "\u{10FFFF}")

  test "scanString StringType.Basic":
    # first delimiter already eaten by parser
    testBasicString("\"", "")
    testBasicString("hello\"", "hello")
    testBasicString("\"\"hello\"\"\"", "hello")
    testBasicString("\"\"hel\\nlo\"\"\"", "hel\nlo")

    expect TomlError:
      testBasicString("he\nllo\"", "hello")

    expect TomlError:
      testBasicString("hello", "hello")

    expect TomlError:
      testBasicString("\"\"hel\nlo\"\"", "hel\nlo")

    testBasicString("\"\" \" \"\" \"\"\"", " \" \"\" ")

    testBasicString("hello\\uD7FF\"", "hello\uD7FF")
    testBasicString("hello\\U0010FFFF\"", "hello\u{10FFFF}")
    testBasicString("\\\"\\\\\\b\\f\\n\\r\\t\"", "\"\\\b\f\n\r\t")

    # 4 """" delimiter
    testBasicString("\"\"\\\"\\\\\\b\\f\\n\\r\\thel\nlo\"\"\"\"", "\"\\\b\f\n\r\thel\nlo\"")

    testBasicString("\"\"\\t\nhel\nlo\"\"\"", "\t\nhel\nlo")

    expect TomlError:
      testBasicString("\\xAB\"", "\xAB")

    testBasicString("Обычный текст в кодировке UTF-8\"", "Обычный текст в кодировке UTF-8")
    testBasicString("\"\"Обычный текст в кодировке UTF-8\"\"\"", "Обычный текст в кодировке UTF-8")

    expect TomlError:
      # nonws after backslash
      testBasicString("\"\"\n  foo \\ \\n\n  bar\"\"\"", "  foo \\n")

    const
      fox  = "The quick brown fox jumps over the lazy dog."
      fox2 = "\"\"The quick brown \\\n\n\nfox jumps over \\\nthe lazy dog.\"\"\""
      fox3 = "\"\"\\\nThe quick brown \\\nfox jumps over \\\nthe lazy dog.\\\n\"\"\""

    testBasicString(fox2, fox)
    testBasicString(fox3, fox)

  test "scanString StringType.Literal":
    # first delimiter already eaten by parser
    testLiteralString("\'", "")
    testLiteralString("hello\'", "hello")
    testLiteralString("\'\'hel\nlo\'\'\'", "hel\nlo")

    expect TomlError:
      testLiteralString("hel\nlo\'", "hello")

    expect TomlError:
      testLiteralString("hello", "hello")

    expect TomlError:
      testLiteralString("\'\'hel\nlo\'\'", "hel\nlo")

    testLiteralString("\'\' \' \'\' \'\'\'", r" ' '' ")
    testLiteralString("\'\'\\t\nhel\nlo\\uD7FF\'\'\'", "\\t\nhel\nlo\\uD7FF")

    testLiteralString("c:\\users\\user\'", r"c:\users\user")

    testLiteralString("Обычный текст в кодировке UTF-8\'", "Обычный текст в кодировке UTF-8")
    testLiteralString("\'\'Обычный текст в кодировке UTF-8\'\'\'", "Обычный текст в кодировке UTF-8")

    testLiteralString("\'\'Here are fifteen quotation marks: \"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\'\'\'",
      "Here are fifteen quotation marks: \"\"\"\"\"\"\"\"\"\"\"\"\"\"\"")

    testLiteralString("\'\'\'That\'s still pointless\', she said.\'\'\'",
      "\'That\'s still pointless\', she said.")

  test "skip first empty new line in multiline":
    testBasicString("\"\"\r\"\"\"", "")
    testBasicString("\"\"\n\"\"\"", "")
    testBasicString("\"\"\r\n\"\"\"", "")

suite "integer test suite":
  test "scanBool":
    testScanBool("true", true)
    testScanBool("false", false)

    expect TomlError:
      testScanBool("trur", true)

  test "scanInt int":
    testScanInt("0", 0'u64, base10, Sign.None)
    testScanInt("0 ", 0'u64, base10, Sign.None)
    testScanInt("+0", 0'u64, base10, Sign.Pos)
    testScanInt("+0 ", 0'u64, base10, Sign.Pos)

    testScanInt("1", 1'u64, base10, Sign.None)
    testScanInt("1 ", 1'u64, base10, Sign.None)
    testScanInt("+1", 1'u64, base10, Sign.Pos)
    testScanInt("+1 ", 1'u64, base10, Sign.Pos)

    testScanInt("12", 12'u64, base10, Sign.None)
    testScanInt("12 ", 12'u64, base10, Sign.None)
    testScanInt("+12", 12'u64, base10, Sign.Pos)
    testScanInt("+12 ", 12'u64, base10, Sign.Pos)

    testScanInt("-12", 12'u64, base10, Sign.Neg)
    testScanInt("-12 ", 12'u64, base10, Sign.Neg)
    testScanInt("-0", 0'u64, base10, Sign.Neg)
    testScanInt("-0 ", 0'u64, base10, Sign.Neg)

    # strange, but it will be handled by parser
    testScanInt("-12w", 12'u64, base10, Sign.Neg)
    testScanInt("+12w", 12'u64, base10, Sign.Pos)
    testScanInt("+0w", 0'u64, base10, Sign.Pos)
    testScanInt("-0w", 0'u64, base10, Sign.Neg)

    testScanInt("0xABCDEF", 11259375'u64, base16, Sign.None)
    testScanInt("0o7456", 3886'u64, base8, Sign.None)
    testScanInt("0b101", 5'u64, base2, Sign.None)

    testScanInt("0xAB_CD_EF", 11259375'u64, base16, Sign.None)
    testScanInt("0o74_56", 3886'u64, base8, Sign.None)
    testScanInt("0b10_1", 5'u64, base2, Sign.None)

    # hmm?
    testScanInt("-0xABCDEF", 0'u64, base10, Sign.Neg)
    testScanInt("-0o7456", 0'u64, base10, Sign.Neg)
    testScanInt("-0b101", 0'u64, base10, Sign.Neg)

    expect TomlError:
      testScanInt("00", 0'u64, base10, Sign.None)

    expect TomlError:
      testScanInt("01", 1'u64, base10, Sign.None)

    expect TomlError:
      testScanInt("- 0", 0'u64, base10, Sign.Neg)

    expect TomlError:
      testScanInt("+ 0", 0'u64, base10, Sign.Pos)

  test "scanInt string":
    testScanInt("0", "0", base10, Sign.None)
    testScanInt("0 ", "0", base10, Sign.None)
    testScanInt("+0", "0", base10, Sign.Pos)
    testScanInt("+0 ", "0", base10, Sign.Pos)

    testScanInt("1", "1", base10, Sign.None)
    testScanInt("1 ", "1", base10, Sign.None)
    testScanInt("+1", "1", base10, Sign.Pos)
    testScanInt("+1 ", "1", base10, Sign.Pos)

    testScanInt("12", "12", base10, Sign.None)
    testScanInt("12 ", "12", base10, Sign.None)
    testScanInt("+12", "12", base10, Sign.Pos)
    testScanInt("+12 ", "12", base10, Sign.Pos)

    testScanInt("-12", "12", base10, Sign.Neg)
    testScanInt("-12 ", "12", base10, Sign.Neg)
    testScanInt("-0", "0", base10, Sign.Neg)
    testScanInt("-0 ", "0", base10, Sign.Neg)

    # strange, but it will be handled by parser
    testScanInt("-12w", "12", base10, Sign.Neg)
    testScanInt("+12w", "12", base10, Sign.Pos)
    testScanInt("+0w", "0", base10, Sign.Pos)
    testScanInt("-0w", "0", base10, Sign.Neg)

    testScanInt("0xABCDEF", "ABCDEF", base16, Sign.None)
    testScanInt("0xabcdef", "abcdef", base16, Sign.None)
    testScanInt("0o7456", "7456", base8, Sign.None)
    testScanInt("0b101", "101", base2, Sign.None)

    testScanInt("0xAB_CD_EF", "ABCDEF", base16, Sign.None)
    testScanInt("0xab_cd_ef", "abcdef", base16, Sign.None)
    testScanInt("0o74_56", "7456", base8, Sign.None)
    testScanInt("0b10_1", "101", base2, Sign.None)

    # hmm?
    testScanInt("-0xABCDEF", "0", base10, Sign.Neg)
    testScanInt("-0o7456", "0", base10, Sign.Neg)
    testScanInt("-0b101", "0", base10, Sign.Neg)

    expect TomlError:
      testScanInt("00", "0", base10, Sign.None)

    expect TomlError:
      testScanInt("01", "1", base10, Sign.None)

    expect TomlError:
      testScanInt("- 0", "0", base10, Sign.Neg)

    expect TomlError:
      testScanInt("+ 0", "0", base10, Sign.Pos)

suite "name test suite":
  test "scanName":
    testScanName("\"botulinum\"", "botulinum")
    testScanName("botulinum", "botulinum")
    testScanName("\'botulinum\'", "botulinum")
    testScanName("123apple", "123apple")
    testScanName("apple123", "apple123")
    testScanName("_123apple", "_123apple")
    testScanName("-123apple", "-123apple")
    testScanName("123apple-", "123apple-")
    testScanName("123-apple", "123-apple")
    testScanName("123_apple", "123_apple")
    testScanName("-_-_-_", "-_-_-_")

    testScanName("\"123вapple\"", "123вapple")
    testScanName("\'123вapple\'", "123вapple")

    testScanName("\"appleв123\"", "appleв123")
    testScanName("\'appleв123\'", "appleв123")
    testScanName("\'\'", "")
    testScanName("\"\"", "")

    expect TomlError:
      testScanName("123вapple", "123")

    expect TomlError:
      testScanName("appleв123", "apple")

    expect TomlError:
      testScanName("\"\"\"appleв123\"\"\"", "appleв123")

    expect TomlError:
      testScanName("\'\'\'appleв123\'\'\'", "appleв123")

  test "scanKey string":
    testScanKey("family.\"genus\".\'species\'", "family.genus.species")

    expect TomlError:
      testScanKey("family.genus.", "family.genus.")

    testScanKey("3.\'\'", "3.")
    testScanKey("3.\"\"", "3.")

    testScanKey("3.14159", "3.14159")
    testScanKey("3 .14159", "3.14159")
    testScanKey("3. 14159", "3.14159")
    testScanKey("3 . 14159", "3.14159")

    testScanKey("3.14159", "3.14159")
    testScanKey("3\t.14159", "3.14159")
    testScanKey("3.\t14159", "3.14159")
    testScanKey("3\t.\t14159", "3.14159")
    testScanKey("3\t. 14159", "3.14159")
    testScanKey("3 .\t14159", "3.14159")

  test "scanKey seq[string]":
    testScanKey("family.genus.species", @["family", "genus", "species"])

    testScanKey("family.\"genus\".\'species\'", @["family", "genus", "species"])

    expect TomlError:
      testScanKey("family.genus.", @["family", "genus"])

    testScanKey("3.\'\'", @["3", ""])
    testScanKey("3.\"\"", @["3", ""])

    testScanKey("3.14159", @["3", "14159"])
    testScanKey("3 .14159", @["3", "14159"])
    testScanKey("3. 14159", @["3", "14159"])
    testScanKey("3 . 14159", @["3", "14159"])
    testScanKey("3\t.14159", @["3", "14159"])
    testScanKey("3.\t14159", @["3", "14159"])
    testScanKey("3 .\t14159", @["3", "14159"])
    testScanKey("3\t. 14159", @["3", "14159"])

  test "scanTableName string":
    testTableName("family.genus]", "family.genus")
    testTableName("[family.genus]]", "family.genus", BracketType.double)
    testTableName("family.genus] \n", "family.genus")
    testTableName("[family.genus]] \n", "family.genus", BracketType.double)

    expect TomlError:
      testTableName("family.genus", "family.genus")

    expect TomlError:
      testTableName("family.genus] x", "family.genus")

    expect TomlError:
      testTableName("family.genus] x", "family.genus", BracketType.double)

    expect TomlError:
      testTableName("[family.genus]", "family.genus", BracketType.double)

  test "scanTableName seq[string]":
    testTableName("family.genus]", @["family", "genus"])
    testTableName("[family.genus]]", @["family", "genus"], BracketType.double)
    testTableName("family.genus] \n", @["family", "genus"])
    testTableName("[family.genus]] \n", @["family", "genus"], BracketType.double)

    expect TomlError:
      testTableName("family.genus", @["family", "genus"])

    expect TomlError:
      testTableName("[family.genus]", @["family", "genus"], BracketType.double)

    expect TomlError:
      testTableName("family.genus]] x", @["family", "genus"], BracketType.double)

    expect TomlError:
      testTableName("family.genus]] x", @["family", "genus"])

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
    testScanFloat("+nan", Nan, true)
    testScanFloat("-nan", -Nan, true)

    testScanFloat("123E1", 123E1)
    testScanFloat("-123E1", -123E1)
    testScanFloat("+123E1", 123E1)

    testScanFloat("123E+1", 123E+1)
    testScanFloat("-123E+1", -123E+1)
    testScanFloat("+123E+1", 123E+1)

    testScanFloat("123E-1", 123E-1)
    testScanFloat("-123E-1", -123E-1)
    testScanFloat("+123E-1", 123E-1)

    expect TomlError:
      testScanFloat("+", 0'f64)

    expect TomlError:
      testScanFloat("-", -0'f64)

    testScanFloat("-123.123", -123.123'f64)
    testScanFloat("123.123", 123.123'f64)

suite "float test suite 4":
  test "scanFloat float":
    expect TomlError:
      testScanFloat("123.a", 123.0'f64)

    testScanFloat("123", 123'f32)
    testScanFloat("+123", 123'f32)
    testScanFloat("-123", -123'f32)

    expect TomlError:
      testScanFloat("+", 0'f32)

    expect TomlError:
      testScanFloat("-", -0'f32)

    testScanFloat("-123.123", -123.123'f32)
    testScanFloat("123.123", 123.123'f32)

    testScanFloat("-123.123E+1", -123.123E+1'f64)
    testScanFloat("-123.123E-1", -123.123E-1)
    testScanFloat("123.123E-1", 123.123E-1)
    testScanFloat("123.123E+1", 123.123E+1)

    expect TomlError:
      testScanFloat("123.a", 123.0'f32)

    testScanFloat("0e0", 0e0)
    testScanFloat("-0e0", -0e0)
    testScanFloat("+0e0", 0e0)

suite "date time test suite 1":
  test "scanDateTime string":
    testDateTime("07:32:01.999", "07:32:01.999")
    # truncate additional subsecond precision
    testDateTime("07:32:01.9999999", "07:32:01.999999")

    testDateTime("1979-05-27T07:32:00-08:01", "1979-05-27T07:32:00-08:01")
    testDateTime("1979-05-27T07:32:00+08:01", "1979-05-27T07:32:00+08:01")
    testDateTime("1979-05-27t07:32:00+08:01", "1979-05-27t07:32:00+08:01")
    testDateTime("1979-05-27 07:32:00+08:01", "1979-05-27 07:32:00+08:01")
    testDateTime("1979-05-27 07:32:00.666+08:01", "1979-05-27 07:32:00.666+08:01")
    testDateTime("1979-05-27 07:32:00.3333333+08:01", "1979-05-27 07:32:00.333333+08:01")
    testDateTime("1979-05-27T07:32:00z", "1979-05-27T07:32:00z")
    testDateTime("1979-05-27T07:32:00Z", "1979-05-27T07:32:00Z")

    expect TomlError:
      testDateTime("07:32:01.", "07:32:01.0")

    expect TomlError:
      testDateTime("07:32:0", "07:32:0")

    expect TomlError:
      testDateTime("07:32:", "07:32:")

    expect TomlError:
      testDateTime("07:32", "07:32")

    expect TomlError:
      testDateTime("07:3", "07:3")

    expect TomlError:
      testDateTime("07:", "07:")

    expect TomlError:
      testDateTime("1979-05-27T", "1979-05-27T")

    expect TomlError:
      testDateTime("1979-05-2", "1979-05-2")

    expect TomlError:
      testDateTime("1979-05-", "1979-05-")

    expect TomlError:
      testDateTime("1979-05", "1979-05")

    expect TomlError:
      testDateTime("1979-0", "1979-0")

    expect TomlError:
      testDateTime("1979-", "1979-")

    testDateTime("7:32:01.0", "07:32:01.0", zeroLead = true)
    testDateTime("979-05-27 07:32:01", "0979-05-27 07:32:01", zeroLead = true)

suite "date time test suite 2":
  test "scanDateTime toml":
    var x: TomlDateTime
    x.time = some(TomlTime(hour: 7, minute: 32, second: 1, subsecond: 999))
    testDateTime("07:32:01.999", x)
    x.time = some(TomlTime(hour: 7, minute: 32, second: 1, subsecond: 999999))
    testDateTime("07:32:01.9999999", x)

    x.date = some(TomlDate(
      year: 1979, month: 5, day:27
      ))
    x.time = some(TomlTime(hour: 7, minute: 32, second: 0, subsecond: 0))
    x.zone = some(TomlTimeZone(
      positiveShift: false,
      hourShift: 8,
      minuteShift: 1
      ))
    testDateTime("1979-05-27T07:32:00-08:01", x)

    x.zone = some(TomlTimeZone(
      positiveShift: true,
      hourShift: 8,
      minuteShift: 1
      ))
    testDateTime("1979-05-27T07:32:00+08:01", x)

    testDateTime("1979-05-27t07:32:00+08:01", x)
    testDateTime("1979-05-27 07:32:00+08:01", x)

    x.time = some(TomlTime(hour: 7, minute: 32, second: 0, subsecond: 666))
    testDateTime("1979-05-27 07:32:00.666+08:01", x)

    x.time = some(TomlTime(hour: 7, minute: 32, second: 0, subsecond: 333333))
    testDateTime("1979-05-27 07:32:00.3333333+08:01", x)

    x.zone = some(TomlTimeZone(
      positiveShift: true,
      hourShift: 0,
      minuteShift: 0
      ))
    x.time = some(TomlTime(hour: 7, minute: 32, second: 0, subsecond: 0))
    testDateTime("1979-05-27T07:32:00z", x)
    testDateTime("1979-05-27T07:32:00Z", x)

suite "date time test suite 3":
  test "scanDateTime toml":
    var x: TomlDateTime

    expect TomlError:
      testDateTime("07:32:01.", x)

    expect TomlError:
      testDateTime("07:32:0", x)

    expect TomlError:
      testDateTime("07:32:", x)

    expect TomlError:
      testDateTime("07:32", x)

    expect TomlError:
      testDateTime("07:3", x)

    expect TomlError:
      testDateTime("07:", x)

    expect TomlError:
      testDateTime("1979-05-27T", x)

    expect TomlError:
      testDateTime("1979-05-2", x)

    expect TomlError:
      testDateTime("1979-05-", x)

    expect TomlError:
      testDateTime("1979-05", x)

    expect TomlError:
      testDateTime("1979-0", x)

    expect TomlError:
      testDateTime("1979-", x)

    x.date = none(TomlDate)
    x.time = some(TomlTime(hour: 7, minute: 32, second: 0, subsecond: 0))
    x.zone = none(TomlTimezone)
    testDateTime("7:32:00", x, zeroLead = true)

    x.date = some(TomlDate(year: 979, month: 5, day:27))
    testDateTime("979-05-27 07:32:00", x, zeroLead = true)

suite "test toml v1.0.0-rc.2":
  test "zero leading exponent":
    testScanFloat("123E01", 123E1)
    testScanFloat("-123.123E+01", -123.123E+1'f64)
    testScanFloat("-123.123E-01", -123.123E-1)

  test "raw tab in string":
    testLiteralString("\t\'", "\t")
    testLiteralString("\'\'\t\'\'\'", "\t")
    testBasicString("\t\"", "\t")
    testBasicString("\"\"\t\"\"\"", "\t")

  test "control char not allowed in comments":
    testComment("# TOML doc\n[miaw]", '[')

    expect TomlError:
      testComment("# TOML \x01 doc\n[miaw]", '[')
