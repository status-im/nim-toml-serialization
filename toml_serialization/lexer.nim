# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  std/[strutils, strformat, options, tables, math, unicode],
  faststreams/inputs,
  types, private/utils

type
  TomlLexer* = object
    stream*: InputStream
    line*: int
    col*: int
    flags*: TomlFlags

  TomlReaderError* = object of TomlError
    line*, col*: int

  TomlFieldReadingError* = object of TomlReaderError
    field*: string
    error*: ref TomlError

  TomlErrorKind* = enum
    errNone               = "no error"
    errUnterminatedString = "unterminated string"
    errInvalidChar        = "invalid character in string, ord: "
    errUnderscoreDigit    = "underscore must be surrounded by digit"
    errForbidLeadingZero  = "leading zeroes are not allowed in integers"
    errIntegerOverflow    = "integer overflow"
    errIllegalChar        = "illegal character "
    errInvalidBool        = "invalid boolean value"
    errNegateUint         = "cannot negate unsigned integer"
    errInvalidUnicode     = "invalid Unicode codepoint, "
    errEmptyDecimalPart   = "decimal part empty"
    errMLStringName       = "multi line string not allowed in name"
    errMLStringEnum       = "multi line string not allowed as enum value"
    errNoDoubleBracket    = "\']]\' expected"
    errNoSingleBracket    = "\']\' expected"
    errExponentTooLarge   = "exponent too large"
    errUnknownIdent       = "unknown identifier"
    errInvalidDateTime    = "invalid date time"
    errDateTimeML         = "date time cannot span multiple lines"
    errKeyNameMissing     = "key name missing"
    errMissingFirstElement= "first array/table element missing"
    errUnterminatedArray  = "unterminatedArray"
    errUnterminatedTable  = "unterminatedTable"
    errRequireKey         = "require key"
    errDoubleBracket      = "double bracket not allowed"
    errDuplicateTableKey  = "duplicate table key not allowed: \'"
    errKeyNotFound        = "key not found: "
    errInvalidHex         = "invalid hex escape, "
    errExpectDoubleBracket= "expect table array for given key"

const
  CR   = '\r'
  LF*  = '\l'
  EOF* = '\0'
  TAB  = '\t'
  SPC  = ' '
  invalidCommentChar = {'\x00'..'\x08', '\x0A'..'\x1F', '\x7F'}

template readChar*(lex: TomlLexer): char =
  when nimvm:
    read(VMInputStream(lex.stream))
  else:
    char inputs.read(lex.stream)

template readable*(lex: TomlLexer): bool =
  when nimvm:
    readable(VMInputStream(lex.stream))
  else:
    lex.stream.readable()

template peekChar*(lex: TomlLexer): char =
  when nimvm:
    peekChar(VMInputStream(lex.stream))
  else:
    lex.stream.peek().char

proc lineInfo(lex: TomlLexer): (int, int) {.inline.} =
  (lex.line, lex.col)

template tryFmt(expr: untyped): string =
  try: expr
  except CatchableError as err: err.msg

method formatMsg*(err: ref TomlReaderError, filename: string): string
                 {.gcsafe, raises: [Defect].} =
  tryFmt: fmt"{filename}({err.line}, {err.col}) Error while reading TOML file: {err.msg}"

method formatMsg*(err: ref TomlFieldReadingError, filename: string): string
                 {.gcsafe, raises: [Defect].} =
  err.error.formatMsg(filename)

proc newTomlError*(line, col: int, msg: string): ref TomlError =
  result = newException(TomlError, "(" & $line &
                        ", " & $col & ")" & " " & msg)

template raiseTomlErr*(li: (int, int), kind: TomlErrorKind) =
  raise newTomlError(li[0], li[1], $kind)

template raiseTomlErr*(lex: TomlLexer, kind: TomlErrorKind) =
  raise newTomlError(lex.line, lex.col, $kind)

template raiseTomlErr*(lex: TomlLexer, msg: string) =
  raise newTomlError(lex.line, lex.col, msg)

template raiseInvalidChar*(lex: TomlLexer, c: char) =
  raiseTomlErr(lex, $errInvalidChar & $ord(c))

template raiseIllegalChar*(lex: TomlLexer, c: char) =
  raiseTomlErr(lex, $errIllegalChar & escape($c))

template raiseUnknownEscape*(lex: TomlLexer, c: char) =
  raiseTomlErr(lex, "unknown escape sequence \"\\" & c & "\"")

template raiseUnexpectedValue*(lex: TomlLexer, s: string) =
  raiseTomlErr(lex, "Expected valid '" & s & "' value")

template raiseUnexpectedField*(lex: TomlLexer, fieldName, typeName: string) =
  raiseTomlErr(lex, "Unexpected field '" &
                     fieldName & "' while deserializing '" &
                     typeName & "'")

template raiseInvalidUnicode*(lex: TomlLexer, s: string) =
  raiseTomlErr(lex, $errInvalidUnicode & s)

template raiseDigitCount*(lex: TomlLexer, expected, actual: int) =
  raiseTomlErr(lex, "Expected digits count '" &
                     $expected & "' but got '" & $actual & "'")

template raiseBadValue*(lex: TomlLexer, msg: string, value: SomeInteger) =
  raiseTomlErr(lex, msg & "(" & $value & ")")

template raiseExpectChar*(lex: TomlLexer, c: char) =
  raiseTomlErr(lex, "expected \'" & escape($c) & "\'")

template raiseNotTable(lex: TomlLexer, name: string) =
  raiseTomlErr(lex, "\"" & name & "\" is not a table")

template raiseNotArray(lex: TomlLexer, name: string) =
  raiseTomlErr(lex, "\"" & name & "\" is not an array")

template raiseKeyNotFound*(lex: TomlLexer, key: string) =
  raiseTomlErr(lex, $errKeyNotFound & "\'" & key & "\'")

template raiseInvalidHex*(lex: TomlLexer, s: string) =
  raiseTomlErr(lex, $errInvalidHex & s)

template raiseDuplicateTableKey*(lex: TomlLexer, s: string) =
  raiseTomlErr(lex, $errDuplicateTableKey & s & "\'")

proc init*(T: type TomlLexer, stream: InputStream, flags: TomlFlags = {}): T =
  T(stream: stream,
    line: 1,
    col: 1,
    flags: flags
   )

proc next*(lex: var TomlLexer): char =
  ## Return the next available char from the stream associate with
  ## the parser lex, or EOF if there are no characters left.
  if not lex.readable():
    return EOF

  result = lex.readChar()

  # Update the line and col number
  if result == LF:
    inc(lex.line)
    lex.col = 1
  elif result != CR:
    inc(lex.col)

template peek(): char =
  if not lex.readable(): EOF else: lex.peekChar()

template advance() =
  discard lex.next

template advancePeek(): untyped =
  advance()
  peek()

type
  LfSkipMode* = enum
    skipLf, skipNoLf

proc nonws*(lex: var TomlLexer, skip: static[LfSkipMode]): char =
  ## Note: this procedure does *not* consider a newline as a
  ## "whitespace". Since newlines are often mandatory in TOML files
  ## (e.g. after a key/value specification), we do not want to miss
  ## them...

  const whitespaces = when skip == skipLf:
                        {SPC, TAB, CR, LF}
                      else:
                        {SPC, TAB, CR}
  var next: char
  while true:
    next = peek
    if next == '#':
      # Skip the comment up to the newline, but do not jump over it
      while next != LF:
        if next == CR:
          next = advancePeek
          if next != LF:
            raiseIllegalChar(lex, next)
          else:
            break
        elif next in invalidCommentChar:
          raiseIllegalChar(lex, next)

        next = advancePeek

        if not lex.readable:
          # rase case
          break

    if next notin whitespaces: break
    advance

  result = next

type
  Leading {.pure.} = enum
    AllowZero, DenyZero

func charTo(T: type, c: char): T {.inline.} =
  case c
  of {'0'..'9'}: result = T(c) - T('0')
  of {'a'..'f'}: result = T(c) - T('a') + T(10)
  of {'A'..'F'}: result = T(c) - T('A') + T(10)
  else: doAssert(false, "should never executed")

func baseToDigits(base: NumberBase): set[char] {.inline.} =
  case base
  of base2: {'0', '1'}
  of base8: {'0'..'7'}
  of base10: strutils.Digits
  of base16: strutils.HexDigits

proc mulOverflow(a, b: uint64): bool =
  # Check if either of them is zero
  if a == 0'u64 or b == 0'u64:
    return false

  let res = a * b
  a != res div b

proc addOverflow(a, b: uint64): bool =
  if a == 0'u64 or b == 0'u64:
    return false

  let res = a + b
  res < a

proc scanUint[T](lex: var TomlLexer, value: var T, base: NumberBase,
                 leading = Leading.AllowZero) =
  ## scanUint only accepts `string` or `uint64` or `TomlVoid`

  var
    next: char
    firstPos = true
    wasUnderscore = false

  when T is uint64:
    let baseNum = (case base
                   of base2: 2'u64
                   of base8: 8'u64
                   of base10: 10'u64
                   of base16: 16'u64)

  let digits  = baseToDigits(base)

  while true:
    wasUnderscore = next == '_'
    next = peek
    if next == '_':
      if firstPos or wasUnderscore:
        raiseTomlErr(lex, errUnderscoreDigit)
      advance
      continue

    if next notin digits:
      if wasUnderscore:
        raiseTomlErr(lex, errUnderscoreDigit)
      break

    advance

    if leading == Leading.DenyZero:
      if next == '0' and firstPos:
        # TOML specifications forbid this
        let secondChar = peek
        if secondChar in Digits:
          raiseTomlErr(lex, errForbidLeadingZero)

    when T is string:
      value.add next
    elif T is uint64:
      if mulOverflow(value, baseNum):
        raiseTomlErr(lex, errIntegerOverflow)
      value = value * baseNum
      let digit = charTo(T, next)
      if addOverflow(value, digit):
        raiseTomlErr(lex, errIntegerOverflow)
      value = value + digit
    elif T is TomlVoid:
      discard
    else:
      {.fatal: "`scanUint` only accepts `string` or `uint64` or `TomlVoid`".}

    firstPos = false

proc scanDigits*[T](lex: var TomlLexer, value: var T,
                    base: NumberBase, maxDigits = high(int)): int =
  ## scanUint only accepts `string` or `int` or `TomlVoid`

  var next: char

  when T is SomeInteger:
    let baseNum = (case base
                   of base2: 2
                   of base8: 8
                   of base10: 10
                   of base16: 16)

  let digits  = baseToDigits(base)

  while true:
    next = peek

    if next notin digits:
      return

    inc result
    advance

    when T is string:
      value.add next
    elif T is SomeInteger:
      value = value * baseNum + charTo(T, next)
    elif T is TomlVoid:
      discard
    else:
      {.fatal: "`scanDigits` only accepts `string` or `SomeInteger` or `TomlVoid`".}

    if result == maxDigits:
      # consume the rest of digits
      while true:
        next = peek
        if next notin digits:
          break
        advance
      return

proc scanEncoding[T](lex: var TomlLexer, value: var T): NumberBase =
  let next = lex.next
  case next:
  of 'b':
    scanUint(lex, value, base2)
    base2
  of 'o':
    scanUint(lex, value, base8)
    base8
  of 'x':
    scanUint(lex, value, base16)
    base16
  else:
    raiseIllegalChar(lex, next)

proc scanUnicode[T](lex: var TomlLexer, kind: char, res: var T) =
  when T isnot (string or TomlVoid):
    {.fatal: "`scanUnicode` only accepts `string` or `TomlVoid`".}

  var code: int64
  let col = scanDigits(lex, code, base16)

  if kind == 'u' and col != 4:
    raiseInvalidUnicode(lex, "'u' must have 4 character value")
  if kind == 'U' and col != 8:
    raiseInvalidUnicode(lex, "'U' must have 8 character value")
  if code notin 0..0xD7FF and code notin 0xE000..0x10FFFF:
    raiseInvalidUnicode(lex, "must be a unicode scalar value")

  when T is string:
    res.add unicode.toUTF8(Rune(code))

proc scanHexEscape[T](lex: var TomlLexer, res: var T) =
  when T isnot (string or TomlVoid):
    {.fatal: "`scanHexEscape` only accepts `string` or `TomlVoid`".}

  if TomlHexEscape notin lex.flags:
    raiseInvalidHex(lex, "not supported by standard, please use `TomlHexEscape`")

  var code: int
  let col = scanDigits(lex, code, base16)
  if col != 2:
    raiseInvalidHex(lex, "'\\x' must have 2 character value")

  when T is string:
    res.add char(code)

proc scanEscapeChar[T](lex: var TomlLexer, esc: char, res: var T) =
  when T isnot (string or TomlVoid):
    {.fatal: "`scanEscapeChar` only accepts `string` or `TomlVoid`".}

  when T is string:
    case esc
    of 'b': res.add "\b"
    of 't': res.add "\t"
    of 'n': res.add "\n"
    of 'f': res.add "\f"
    of 'r': res.add "\r"
    of '\'': res.add "\'"
    of '\"': res.add "\""
    of '\\': res.add "\\"
    of 'x':
      scanHexEscape(lex, res)
    of 'u', 'U':
      scanUnicode(lex, esc, res)
    else:
      raiseUnknownEscape(lex, esc)
  else:
    case esc
    of 'b', 't', 'n', 'f', 'r', '\'', '\"', '\\':
      discard
    of 'x':
      scanHexEscape(lex, res)
    of 'u', 'U':
      scanUnicode(lex, esc, res)
    else:
      raiseUnknownEscape(lex, esc)

func stringDelimiter(kind: StringType): char {.inline.} =
  result = (case kind
            of StringType.Basic: '\"'
            of StringType.Literal: '\'')

proc scanMultiLineString[T](lex: var TomlLexer, res: var T, kind: static[StringType]) =
  when T isnot (string or TomlVoid):
    {.fatal: "`scanMultiLineString` only accepts `string` or `TomlVoid`".}

  ## This procedure parses strings enclosed within three consecutive
  ## sigle/double quotation marks. It assumes that all the quotation
  ## marks have already been consumed by the "lex" variable, which
  ## therefore is ready to read the first character of the string.

  const delimiter = stringDelimiter(kind)

  var
    isFirstChar = true
    next: char

  while true:
    next = peek

    # Skip the first newline, if it comes immediately after the
    # quotation marks
    if isFirstChar and (next == CR):
      isFirstChar = false
      next = advancePeek
      if next == LF:
        advance
      continue

    if isFirstChar and (next == LF):
      isFirstChar = false
      advance
      continue

    if next == delimiter:
      # Are we done?
      next = advancePeek
      if next == delimiter:
        next = advancePeek
        if next == delimiter:
          next = advancePeek
          # Done with this string
          if next == delimiter:
            advance
            when T is string:
              res.add delimiter
          return
        else:
          # Just got a double delimiter
          when T is string:
            res.add delimiter
            res.add delimiter
          continue
      else:
        # Just got a lone delimiter
        when T is string:
          res.add(delimiter)
        continue

    isFirstChar = false
    when kind == StringType.Basic:
      if next == '\\':
        # This can either be an escape sequence or a end-of-line char
        next = advancePeek
        if next in {LF, CR, SPC}:
          # We're at the end of a line: skip everything till the
          # next non-whitespace character
          while true:
            if next == LF:
              break
            elif next in {CR, SPC, TAB}:
              next = lex.next
            else:
              raiseIllegalChar(lex, next)

          while true:
            next = peek
            if next notin {CR, LF, SPC, TAB}:
              break
            advance

          continue
        else:
          # This is just an escape sequence (like "\t")
          lex.scanEscapeChar(lex.next, res)
          continue

    if next == EOF:
      raiseTomlErr(lex, errUnterminatedString)

    if ord(next) in {16, 31, 127}:
      raiseInvalidChar(lex, next)

    when T is string:
      res.add(next)
    advance

proc scanSingleLineString[T](lex: var TomlLexer, res: var T, kind: static[StringType]) =
  when T isnot (string or TomlVoid):
    {.fatal: "`scanSingleLineString` only accepts `string` or `TomlVoid`".}

  ## This procedure parses strings enclosed within single/double
  ## quotation marks. It assumes that the quotation mark has already
  ## been consumed by the "lex" variable, which therefore is ready
  ## to read the first character of the string.

  const delimiter = stringDelimiter(kind)

  var next: char
  while true:
    next = lex.next
    if next == delimiter:
      break

    if next in {CR, LF, EOF}:
      raiseTomlErr(lex, errUnterminatedString)

    if ord(next) in {16, 31, 127}:
      raiseInvalidChar(lex, next)

    when kind == StringType.Basic:
      if next == '\\':
        next = lex.next
        lex.scanEscapeChar(next, res)
        continue

    when T is string:
      res.add(next)

proc scanString*[T](lex: var TomlLexer, res: var T, kind: static[StringType]): bool =
  when T isnot (string or TomlVoid):
    {.fatal: "`scanString` only accepts `string` or `TomlVoid`".}

  ## This function assumes that "lex" has already consumed the
  ## first character (either \" or \' depends on `kind` param)
  ## returns true if multi line string

  const delimiter = stringDelimiter(kind)
  var next = peek
  if next == delimiter:
    next = advancePeek

    # We have two possibilities here:
    if next == delimiter:
      # (1) "long" multi-line strings.
      advance
      scanMultiLineString(lex, res, kind)
      return true

    # (2) the empty string
  else:
    scanSingleLineString(lex, res, kind)

proc scanString*(lex: var TomlLexer, kind: static[StringType]): string =
  result = newStringOfCap(defaultStringCapacity)
  discard scanString(lex, result, kind)

proc scanInt*[T](lex: var TomlLexer, value: var T): (Sign, NumberBase) =
  when T isnot (string or uint64):
    {.fatal: "`scanInt` only accepts `string` or `uint64`".}

  var
    next: char
    sign = Sign.None

  template zeroVal(): untyped =
    when T is string:
      value.add '0'
    (sign, base10)

  while true:
    next = peek
    case next:
    of '0':
      next = advancePeek
      if sign == Sign.None:
        if next in {'b', 'x', 'o'}:
          return (Sign.None, scanEncoding(lex, value))
        else:
          case next:
          of strutils.Whitespace:
            return zeroVal()
          of strutils.Digits:
            raiseTomlErr(lex, errForbidLeadingZero)
          else:
            # else is a sole 0
            return zeroVal()
      else:
        case next:
        of strutils.Whitespace:
          return zeroVal()
        else:
          # else is a sole 0
          return zeroVal()
    of strutils.Digits - {'0'}:
      scanUint(lex, value, base10, Leading.DenyZero)
      break
    of '+':
      advance
      sign = Sign.Pos
      continue
    of '-':
      advance
      sign = Sign.Neg
      continue
    else:
      raiseIllegalChar(lex, next)
    break

  result = (sign, base10)

proc scanName*[T](lex: var TomlLexer, res: var T) =
  when T isnot (string or TomlVoid):
    {.fatal: "`scanName` only accepts `string` or `TomlVoid`".}

  ## This parses the name of a key or a table

  var next = lex.nonws(skipNoLf)
  if next == '\"':
    advance
    if lex.scanString(res, StringType.Basic):
      raiseTomlErr(lex, errMLStringName)
    return

  elif next == '\'':
    advance
    if lex.scanString(res, StringType.Literal):
      raiseTomlErr(lex, errMLStringName)
    return

  while true:
    if (next in {'=', '.', '[', ']', EOF, SPC, TAB}):
      # Any of the above characters marks the end of the name
      break
    elif (next notin {'a'..'z', 'A'..'Z', '0'..'9', '_', '-'}):
      raiseIllegalChar(lex, next)
    else:
      when T is string:
        res.add(next)
    next = advancePeek

proc scanKey*[T](lex: var TomlLexer, res: var T) =
  when T isnot (string or seq[string] or TomlVoid):
    {.fatal: "`scanKey` only accepts `string` or `seq[string]` or `TomlVoid`".}

  when T is seq[string]:
    var partName = newStringOfCap(defaultStringCapacity)

  var next: char
  while true:
    when T is (string or TomlVoid):
      lex.scanName(res)
    else:
      partName.setLen(0)
      lex.scanName(partName)
      res.add partName

    next = lex.nonws(skipNoLf)
    if next == '.':
      advance
      next = lex.nonws(skipNoLf)
      if next in {'\'', '\"', '-', '_', 'a'..'z', 'A'..'Z', '0'..'9'}:
        when T is string:
          res.add '.'
      else:
        raiseIllegalChar(lex, next)

    else:
      break

type
  BracketType* {.pure.} = enum
    single, double

proc scanTableName*[T](lex: var TomlLexer, res: var T): BracketType =
  when T isnot (string or seq[string]):
    {.fatal: "`scanTableName` only accepts `string` or `seq[string]`".}

  ## This code assumes that '[' has already been consumed

  var next = peek
  if next == '[':
    advance
    result = BracketType.double
  else:
    result = BracketType.single

  lex.scanKey(res)

  case lex.nonws(skipNoLf)
  of ']':
    if result == BracketType.double:
      next = advancePeek
      if next != ']':
        raiseTomlErr(lex, errNoDoubleBracket)

    # We must check that there is nothing else in this line
    advance
    next = lex.nonws(skipNoLf)
    if next notin {LF, EOF}:
      raiseIllegalChar(lex, next)

    advance
  else:
    raiseTomlErr(lex, errNoSingleBracket)

proc scanBool*(lex: var TomlLexer): bool =
  var next = nonws(lex, skipLf)
  let li = lex.lineInfo # used for error messages

  case next
  of 't':
    advance
    # Is this "true"?
    if lex.next != 'r' or
       lex.next != 'u' or
       lex.next != 'e':
       raiseTomlErr(li, errInvalidBool)
    result = true

  of 'f':
    advance
    # Is this "false"?
    if lex.next != 'a' or
       lex.next != 'l' or
       lex.next != 's' or
       lex.next != 'e':
       raiseTomlErr(li, errInvalidBool)
    result = false

  else:
    raiseTomlErr(li, errInvalidBool)

proc scanDecimalPart[T](lex: var TomlLexer, value: var T, sign: Sign) =
  ## `T` should be `Somefloat`, or `string`, or `TomlVoid`

  var
    next: char
    firstPos = true
    wasUnderscore = false

  when T is SomeFloat:
    var
      invPowerOfTen = 10.0
      val = 0.0

  while true:
    wasUnderscore = next == '_'
    next = peek
    if next == '_':
      if firstPos or wasUnderscore:
        raiseTomlErr(lex, errUnderscoreDigit)
      advance
      continue
    if next notin strutils.Digits:
      if wasUnderscore:
        raiseTomlErr(lex, errUnderscoreDigit)
      if firstPos:
        raiseTomlErr(lex, errEmptyDecimalPart)
      break

    when T is SomeFloat:
      val = val + T(int(next) - int('0')) / invPowerOfTen
      invPowerOfTen *= 10
    elif T is string:
      value.add next
    elif T is TomlVoid:
      discard
    else:
      {.fatal: "`scanDecimalPart` only accepts `float` or `string` or `TomlVoid`".}

    firstPos = false
    advance

  when T is SomeFloat:
    if sign == Sign.Neg:
      value = value - val
    else:
      value = value + val

proc scanExponent[T](lex: var TomlLexer, value: var T) =
  when T isnot (SomeFloat or string or TomlVoid):
    {.fatal: "`scanFrac` only accepts `float` or `string` or `TomlVoid`".}

  when T is SomeFloat:
    var
      exponent = 0'u64
      sign = Sign.None

  var next = peek
  case next
  of '-':
    when T is string:
      value.add next
    elif T is SomeFloat:
      sign = Sign.Neg
    next = advancePeek
    if next notin strutils.Digits:
      raiseIllegalChar(lex, next)
  of '+':
    when T is string:
      value.add next
    elif T is SomeFloat:
      sign = Sign.Pos
    next = advancePeek
    if next notin strutils.Digits:
      raiseIllegalChar(lex, next)
  of strutils.Digits:
    discard
  else:
    raiseIllegalChar(lex, next)

  when T is SomeFloat:
    scanUint(lex, exponent, base10)
  else:
    scanUint(lex, value, base10)

  when T is SomeFloat:
    if exponent >= 308'u64:
      raiseTomlErr(lex, errExponentTooLarge)

    if sign == Sign.Neg:
      value = value / pow(10.0, exponent.float64)
    else:
      value = value * pow(10.0, exponent.float64)

proc scanFrac[T](lex: var TomlLexer, value: var T, sign: Sign) =
  when T isnot (SomeFloat or string or TomlVoid):
    {.fatal: "`scanFrac` only accepts `float` or `string` or `TomlVoid`".}

  scanDecimalPart(lex, value, sign)
  var next = peek
  if next in {'e', 'E'}:
    advance
    when T is string:
      value.add next
    scanExponent(lex, value)

proc addFrac*[T](lex: var TomlLexer, value: var T, sign: Sign) =
  when T is string:
    value.add '.'
    scanFrac(lex, value, sign)
  elif T is SomeFloat:
    scanFrac(lex, value, sign)
  elif T is TomlVoid:
    scanFrac(lex, value, sign)
  else:
    {.fatal: "`addFrac` only accepts `float` or `string` or `TomlVoid`".}

proc scanFloat*[T](lex: var TomlLexer, value: var T): Sign =
  when T isnot (SomeFloat or string):
    {.fatal: "`scanFloat` only accepts float or string".}

  var
    sign = Sign.None
    next: char

  while true:
    next = peek
    case next
    of '-':
      advance
      when T is string:
        value.add next
      sign = Sign.Neg
      continue
    of '+':
      advance
      when T is string:
        value.add next
      sign = Sign.Pos
      continue
    of 'i':
      let li = lex.lineInfo
      advance
      if lex.next != 'n' or
         lex.next != 'f':
         raiseTomlErr(li, errUnknownIdent)
      when T is string:
        value.add "inf"
      else:
        value = Inf
        if sign == Sign.Neg: value = -Inf
      return sign
    of 'n':
      let li = lex.lineInfo
      advance
      if lex.next != 'a' or
         lex.next != 'n':
         raiseTomlErr(li, errUnknownIdent)
      when T is string:
        value.add "nan"
      else:
        value = NaN
        if sign == Sign.Neg: value = -NaN
      return sign
    of strutils.Digits:
      break
    else:
      raiseIllegalChar(lex, next)
    break

  when T is SomeFloat:
    var intPart = 0'u64
    scanUint(lex, intPart, base10, Leading.DenyZero)
    value = T(intPart)
    if sign == Sign.Neg: value = -value
  else:
    scanUint(lex, value, base10, Leading.DenyZero)

  next = peek
  case next
  of '.':
    advance
    lex.addFrac(value, sign)
  of 'e', 'E':
    advance
    when T is string:
      value.add next
    lex.scanExponent(value)
  else:
    discard

  result = sign

proc scanStrictNum[T](lex: var TomlLexer, res: var T, minVal, maxVal, count: int, msg: string) =
  when T isnot (int or string or TomlVoid):
    {.fatal: "`scanStrictNum` only accepts `int` or `string` or `TomlVoid`".}

  var val: int
  let parsed = scanDigits(lex, val, base10)

  if parsed != count:
    lex.raiseDigitCount(count, parsed)

  if val < minVal or val > maxVal:
    lex.raiseBadValue(msg, val)

  when T is string:
    res.add intToStr(val, count)
  elif T is int:
    res = val

proc scanMinuteSecond*[T](lex: var TomlLexer, value: var T) =
  ## `scanTime` assume the two digits of hour already parsed
  when T isnot (TomlTime or string or TomlVoid):
    {.fatal: "`scanMinuteSecond` only accepts `TomlTime' or `string` or `TomlVoid`".}

  var
    next = lex.next
    line = lex.line

  if next != ':':
    lex.raiseExpectChar(':')

  when T is string:
    value.add next
    template num: untyped = value
  elif T is TomlVoid:
    template num: untyped = value
  else:
    var num: int

  scanStrictNum(lex, num, minVal = 0, maxVal = 59, count = 2,
                "number out of range for `minutes`")

  when T is TomlTime:
    value.minute = num

  next = peek
  if next != ':':
    if TomlHourMinute in lex.flags:
      return
    else:
      lex.raiseExpectChar(':')

  if lex.line != line:
    raiseTomlErr(lex, errDateTimeML)

  advance
  when T is string:
    value.add next

  # Parse the second. Note that seconds=60 *can* happen (leap second)
  scanStrictNum(lex, num, minVal = 0, maxVal = 60, count = 2,
                "number out of range for `seconds`")

  when T is TomlTime:
    value.second = num

  next = peek
  if next == '.':
    if lex.line != line:
      raiseTomlErr(lex, errDateTimeML)

    when T is string:
      value.add next

    next = advancePeek
    if next notin strutils.Digits:
      raiseIllegalChar(lex, next)

    # Toml spec says additional subsecond precision
    # should be truncated and not rounded
    when T is (string or TomlVoid):
      discard scanDigits(lex, value, base10, tomlSubsecondPrecision)
    elif T is TomlTime:
      discard scanDigits(lex, value.subsecond, base10, tomlSubsecondPrecision)

proc scanTime*[T](lex: var TomlLexer, value: var T) =
  var line = lex.line
  when T is (string or TomlVoid):
    template num: untyped = value
  else:
    var num : int

  scanStrictNum(lex, num, minVal = 0, maxVal = 23, count = 2,
                "number out of range for `hours`")

  if lex.line != line:
    raiseTomlErr(lex, errDateTimeML)

  when T is TomlTime:
    value.hour = num

  scanMinuteSecond(lex, value)

proc scanMonthDay*[T](lex: var TomlLexer, value: var T) =
  ## `scanMonthDay` assume the four digits of year already parsed
  when T isnot (TomlDate or string or TomlVoid):
    {.fatal: "`scanMonthDay` only accepts `TomlDate' or string or `TomlVoid`".}

  var
    next = lex.next
    line = lex.line

  if next != '-':
    lex.raiseExpectChar('-')

  when T is string:
    value.add next
    template num: untyped = value
  elif T is TomlVoid:
    template num: untyped = value
  else:
    var num : int

  scanStrictNum(lex, num, minVal = 1, maxVal = 12, count = 2,
                "number out of range for `month`")

  if lex.line != line:
    raiseTomlErr(lex, errDateTimeML)

  next = lex.next
  if next != '-':
    lex.raiseExpectChar('-')

  when T is string:
    value.add next
  elif T is TomlDate:
    value.month = num

  scanStrictNum(lex, num, minVal = 1, maxVal = 31, count = 2,
                "number out of range for `day`")

  when T is TomlDate:
    value.day = num

proc scanDate*[T](lex: var TomlLexer, value: var T) =
  var line = lex.line
  when T is (string or TomlVoid):
    template num: untyped = value
  else:
    var num : int

  scanStrictNum(lex, num, minVal = 0, maxVal = 9999, count = 4,
                "number out of range for `year`")

  if lex.line != line:
    raiseTomlErr(lex, errDateTimeML)

  when T is (string or TomlVoid):
    scanMonthDay(lex, value)
  else:
    value.year = num
    scanMonthDay(lex, value)

proc scanTimeZone*[T](lex: var TomlLexer, value: var T): bool =
  ## `scanTimeZone` assume the four digits of year already parsed
  when T isnot (TomlTimeZone or string or TomlVoid):
    {.fatal: "`scanTimeZone` only accepts `TomlTimeZone' or string or `TomlVoid`".}

  var
    line = lex.line
    next = peek

  when T is (string or TomlVoid):
    template num: untyped = value
  else:
    var num : int

  case next
  of 'z', 'Z':
    advance
    when T is string:
      value.add next
    elif T is TomlTimeZone:
      value.positiveShift = true
      value.hourShift = 0
      value.minuteShift = 0
    result = true
  of '-', '+':
    advance
    when T is string:
      value.add next
    elif T is TomlTimeZone:
      value.positiveShift = next == '+'

    scanStrictNum(lex, num, minVal = 0, maxVal = 23, count = 2,
                "number out of range for `zone hours`")

    if lex.line != line:
      raiseTomlErr(lex, errDateTimeML)

    next = lex.next
    if next != ':':
      lex.raiseExpectChar(':')

    when T is string:
      value.add next
    elif T is TomlTimeZone:
      value.hourShift = num

    scanStrictNum(lex, num, minVal = 0, maxVal = 59, count = 2,
                "number out of range for `zone minutes`")

    when T is TomlTimeZone:
      value.minuteShift = num

    result = true
  else:
    discard

proc scanLongDate*[T](lex: var TomlLexer, year: int, value: var T) =
  var line = lex.line

  when T is (string or TomlVoid):
    scanMonthDay(lex, value)
  else:
    var date = TomlDate(year: year)
    scanMonthDay(lex, date)
    value.date = some(date)

  if lex.line != line:
    # only date part without time
    return

  let delim = peek
  if delim notin {'t', 'T', ' '}:
    return

  advance
  when T is string:
    value.add delim
    scanTime(lex, value)
  elif T is TomlVoid:
    scanTime(lex, value)
  else:
    var time: TomlTime
    scanTime(lex, time)
    value.time = some(time)

  when T is (string or TomlVoid):
    discard scanTimeZone(lex, value)
  else:
    var zone: TomlTimeZone
    if scanTimeZone(lex, zone):
      value.zone = some(zone)

proc scanDateTime*[T](lex: var TomlLexer, value: var T, zeroLead = false) =
  when T isnot (TomlDateTime or string or TomlVoid):
    {.fatal: "`scanDateTime` only accepts `TomlDateTime' or string or `TomlVoid`".}

  var line = lex.line

  when T is (string or TomlVoid):
    let numDigit = scanDigits(lex, value, base10)
  else:
    var num: int
    let numDigit = scanDigits(lex, num, base10)

  let Z = zeroLead.int

  if numDigit == 4 - Z:
    if lex.line != line:
      raiseTomlErr(lex, errDateTimeML)

    when T is (string or TomlVoid):
      scanLongDate(lex, 0, value)
    else:
      scanLongDate(lex, num, value)

  elif numDigit == 2 - Z:
    if lex.line != line:
      raiseTomlErr(lex, errDateTimeML)

    when T is string:
      let num = (value[^2].int - '0'.int) * 10 + (value[^1].int - '0'.int)

    when T isnot TomlVoid:
      if num > 23:
        lex.raiseBadValue("number out of range for `hours`", num)

    when T is (string or TomlVoid):
      scanMinuteSecond(lex, value)
    else:
      var time = TomlTime(hour: num)
      scanMinuteSecond(lex, time)
      value.time = some(time)
  else:
    raiseTomlErr(lex, errInvalidDateTime)

proc toSigned*[T](lex: var TomlLexer, x: uint64): T =
  const maxT = uint64(high(T)) + 1'u64
  if x > maxT:
    raiseTomlErr(lex, errIntegerOverflow)
  elif x == maxT:
    low(T)
  else:
    T(-x.int64)

proc toIntVal(lex: var TomlLexer, x: uint64, sign: Sign): TomlValueRef =
  if sign != Neg and x > uint64(high(int64)):
    raiseTomlErr(lex, errIntegerOverflow)

  TomlValueRef(
    kind: TomlKind.Int,
    intVal: if sign == Neg: toSigned[int64](lex, x) else: x.int64
  )

proc parseNumOrDate*[T](lex: var TomlLexer, value: var T) =
  when T isnot (TomlValueRef or string or TomlVoid):
    {.fatal: "`parseNumOrDate` only accepts `TomlValueRef' or string or `TomlVoid`".}

  var
    next: char
    sign = Sign.None

  when T is TomlValueRef:
    var uintVal: uint64

  while true:
    next = peek
    case next:
    of '0':
      next = advancePeek
      if sign == Sign.None:
        if next in {'b', 'x', 'o'}:
          when T is (string or TomlVoid):
            discard scanEncoding(lex, value)
          else:
            value = TomlValueRef(kind: TomlKind.Int)
            discard scanEncoding(lex, uintVal)
            value.intVal = uintVal.int64
        else:
          # This must now be a float or a date/time, or a sole 0
          case next:
          of '.':
            advance
            when T is string:
              value.add '0'
              addFrac(lex, value, sign)
            elif T is TomlVoid:
              addFrac(lex, value, sign)
            else:
              value = TomlValueRef(kind: TomlKind.Float)
              addFrac(lex, value.floatVal, sign)
            return
          of strutils.Whitespace:
            when T is string:
              value.add '0'
            elif T is TomlValueRef:
              value = TomlValueRef(kind: TomlKind.Int, intVal: 0)
            return
          of strutils.Digits:
            # This must now be a date/time
            when T is string:
              value.add '0'
              scanDateTime(lex, value, zeroLead = true)
            elif T is TomlVoid:
              scanDateTime(lex, value, zeroLead = true)
            else:
              value = TomlValueRef(kind: TomlKind.DateTime)
              scanDateTime(lex, value.dateTime, zeroLead = true)
            return
          of 'e', 'E':
            advance
            when T is string:
              value.add '0'
              value.add next
              scanExponent(lex, value)
            elif T is TomlVoid:
              scanExponent(lex, value)
            else:
              value = TomlValueRef(kind: TomlKind.Float, floatVal: 0'f64)
              scanExponent(lex, value.floatVal)
            return
          else:
            # else is a sole 0
            when T is string:
              value.add '0'
            elif T is TomlValueRef:
              value = TomlValueRef(kind: TomlKind.Int, intVal: 0)
            return
      else:
        # This must now be a float, or a sole 0
        case next:
        of '.':
          advance
          when T is string:
            value.add '0'
            addFrac(lex, value, sign)
          elif T is TomlVoid:
            addFrac(lex, value, sign)
          else:
            value = TomlValueRef(kind: TomlKind.Float)
            if sign == Sign.Neg:
              value.floatVal = -value.floatVal
            addFrac(lex, value.floatVal, sign)
          return
        of strutils.Whitespace:
          when T is string:
            value.add '0'
          elif T is TomlValueRef:
            value = TomlValueRef(kind: TomlKind.Int, intVal: 0)
          return
        of 'e', 'E':
          advance
          when T is string:
            value.add '0'
            value.add next
            scanExponent(lex, value)
          elif T is TomlVoid:
            scanExponent(lex, value)
          else:
            value = TomlValueRef(kind: TomlKind.Float, floatVal: 0'f64)
            if sign == Sign.Neg:
              value.floatVal = -value.floatVal
            scanExponent(lex, value.floatVal)
          return
        else:
          # else is a sole 0
          when T is string:
            value.add '0'
          elif T is TomlValueRef:
            value = TomlValueRef(kind: TomlKind.Int, intVal: 0)
          return
    of strutils.Digits - {'0'}:
      advance
      # This might be a date/time, or an int or a float
      var
        digits = 1
        wasUnderscore = false

      when T is string:
        value.add next
      elif T is TomlValueRef:
        var curSum = uint64(next) - uint64('0')

      while true:
        next = peek
        if wasUnderscore and next notin strutils.Digits:
          raiseTomlErr(lex, errUnderscoreDigit)

        case next:
        of ':':
          if digits != 2:
            raiseTomlErr(lex, errInvalidDateTime)
          when T is (string or TomlVoid):
            scanMinuteSecond(lex, value)
          else:
            value = TomlValueRef(kind: TomlKind.DateTime)
            var time = TomlTime(hour: curSum.int)
            scanMinuteSecond(lex, time)
            value.dateTime.time = some(time)
          return
        of '-':
          if digits != 4:
            raiseTomlErr(lex, errInvalidDateTime)
          when T is (string or TomlVoid):
            scanLongDate(lex, 0, value)
          else:
            value = TomlValueRef(kind: TomlKind.DateTime)
            scanLongDate(lex, curSum.int, value.dateTime)
          return
        of '.':
          advance
          when T is (string or TomlVoid):
            addFrac(lex, value, sign)
          else:
            value = TomlValueRef(kind: TomlKind.Float, floatVal: float64(curSum))
            if sign == Sign.Neg:
              value.floatVal = -value.floatVal
            addFrac(lex, value.floatVal, sign)
          return
        of 'e', 'E':
          advance
          when T is string:
            value.add next
            scanExponent(lex, value)
          elif T is TomlVoid:
            scanExponent(lex, value)
          else:
            value = TomlValueRef(kind: TomlKind.Float, floatVal: float64(curSum))
            scanExponent(lex, value.floatVal)
            if sign == Sign.Neg:
              value.floatVal = -value.floatVal
          return
        of strutils.Digits:
          advance
          when T is string:
            value.add next
            inc digits
          elif T is TomlVoid:
            inc digits
          else:
            if mulOverflow(curSum, 10'u64):
              raiseTomlErr(lex, errIntegerOverflow)
            curSum = curSum * 10'u64
            let digit = (uint64(next) - uint64('0'))
            if addOverflow(curSum, digit):
              raiseTomlErr(lex, errIntegerOverflow)
            curSum = curSum + digit
            inc digits
          wasUnderscore = false
          continue
        of '_':
          advance
          wasUnderscore = true
          continue
        of strutils.Whitespace:
          when T is TomlValueRef:
            value = toIntVal(lex, curSum, sign)
          return
        else:
          when T is TomlValueRef:
            value = toIntVal(lex, curSum, sign)
          return
        break

    of '+':
      advance
      sign = Sign.Pos
      when T is string:
        value.add '+'
      continue
    of '-':
      advance
      sign = Sign.Neg
      when T is string:
        value.add '-'
      continue
    of 'i':
      advance
      let li = lex.lineInfo
      if lex.next != 'n' or
         lex.next != 'f':
         raiseTomlErr(li, errUnknownIdent)
      when T is string:
        value.add "inf"
      elif T is TomlValueRef:
        value = TomlValueRef(kind: TomlKind.Float, floatVal: Inf)
      return
    of 'n':
      advance
      let li = lex.lineInfo
      if lex.next != 'a' or
         lex.next != 'n':
         raiseTomlErr(li, errUnknownIdent)
      when T is string:
        value.add "nan"
      elif T is TomlValueRef:
        value = TomlValueRef(kind: TomlKind.Float, floatVal: NaN)
      return
    else:
      raiseIllegalChar(lex, next)
    break

proc parseValue*[T](lex: var TomlLexer, value: var T) {.raises: [Defect, IOError, TomlError].}

proc parseArray[T](lex: var TomlLexer, value: var T) =
  when T isnot (seq[TomlValueRef] or string or TomlVoid):
    {.fatal: "`parseArray` only accepts `TomlValueRef' or string or `TomlVoid`".}

  ## This procedure assumes that "lex" has already consumed the '['
  ## character

  var numElem = 0
  while true:
    var next = lex.nonws(skipLf)
    case next
    of ']':
      advance
      when T is string:
        value.add next
      return
    of EOF:
      raiseTomlErr(lex, errUnterminatedArray)
    of ',':
      advance
      if numElem == 0:
        # This happens with "[, 1, 2]", for instance
        raiseTomlErr(lex, errMissingFirstElement)

      # Check that this is not a terminating comma (like in
      #  "[b,]")
      next = lex.nonws(skipLf)
      if next == ']':
        advance
        when T is string:
          value.add next
        return
      else:
        when T is string:
          value.add ','
    else:
      when T is (string or TomlVoid):
        parseValue(lex, value)
      else:
        value.setLen(numElem + 1)
        parseValue(lex, value[numElem])

      inc numElem

proc parseInlineTable[T](lex: var TomlLexer, value: var T) =
  when T isnot (TomlTableRef or string or TomlVoid):
    {.fatal: "`parseInlineTable` only accepts `TomlTableRef' or string or `TomlVoid`".}

  ## This procedure assumes that "lex" has already consumed the '{'
  ## character

  var firstComma = true
  while true:
    var next = lex.nonws(skipNoLf)
    case next
    of '}':
      advance
      when T is string:
        value.add next
      return
    of EOF:
      raiseTomlErr(lex, errUnterminatedTable)
    of ',':
      advance
      if firstComma:
        raiseTomlErr(lex, errMissingFirstElement)

      next = lex.nonws(skipNoLf)
      if next == '}':
        if TomlInlineTableTrailingComma in lex.flags:
          advance
          when T is string:
            value.add '}'
          return
        else:
          raiseIllegalChar(lex, '}')
      else:
        when T is string:
          value.add ','
    of '\n':
      if TomlInlineTableNewline in lex.flags:
        advance
        continue
      else:
        raiseIllegalChar(lex, next)
    else:
      firstComma = false
      when T is (string or TomlVoid):
        scanKey(lex, value)
      else:
        var keys: seq[string]
        scanKey(lex, keys)
        if keys.len == 0:
          raiseTomlErr(lex, errRequireKey)

      next = lex.nonws(skipNoLf)
      if next != '=':
        raiseExpectChar(lex, '=')

      advance
      when T is string:
        value.add next
        parseValue(lex, value)
      elif T is TomlVoid:
        parseValue(lex, value)
      else:
        var curTable = value

        for i in 1 ..< keys.len:
          let key = keys[i]
          let deepestTable = TomlTableRef.new
          curTable[key] = TomlValueRef(
            kind: TomlKind.InlineTable,
            tableVal: deepestTable
            )
          curTable = deepestTable

        var val: TomlValueRef
        parseValue(lex, val)
        curTable[keys[^1]] = val

proc parseValue[T](lex: var TomlLexer, value: var T) =
  when T isnot (TomlValueRef or string or TomlVoid):
    {.fatal: "`parseValue` only accepts `TomlValueRef' or string or `TomlVoid`".}

  var next = lex.nonws(skipNoLf)
  case next
  of strutils.Digits, '+', '-', 'i', 'n':
    parseNumOrDate(lex, value)
  of 't', 'f':
    when T is string:
      let val = lex.scanBool
      value.add if val: "true" else: "false"
    elif T is TomlVoid:
      discard lex.scanBool
    else:
      let val = lex.scanBool
      value = TomlValueRef(kind: TomlKind.Bool, boolVal: val)
  of '\"':
    advance
    when T is (string or TomlVoid):
      discard scanString(lex, value, StringType.Basic)
    else:
      value = TomlValueRef(kind: TomlKind.String)
      discard scanString(lex, value.stringVal, StringType.Basic)
  of '\'':
    advance
    when T is (string or TomlVoid):
      discard scanString(lex, value, StringType.Literal)
    else:
      value = TomlValueRef(kind: TomlKind.String)
      discard scanString(lex, value.stringVal, StringType.Literal)
  of '[':
    advance
    # An array
    when T is string:
      value.add next
      parseArray(lex, value)
    elif T is TomlVoid:
      parseArray(lex, value)
    else:
      value = TomlValueRef(kind: TomlKind.Array)
      parseArray(lex, value.arrayVal)
  of '{':
    advance
    # An inline table
    when T is string:
      value.add next
      parseInlineTable(lex, value)
    elif T is TomlVoid:
      parseInlineTable(lex, value)
    else:
      value = TomlValueRef(kind: TomlKind.InlineTable, tableVal: TomlTableRef.new)
      parseInlineTable(lex, value.tableVal)
  else:
    raiseIllegalChar(lex, next)

proc newTableArray(size: int = 0): TomlValueRef =
  TomlValueRef(
    kind: TomlKind.Tables,
    tablesVal: newSeq[TomlTableRef](size)
  )

proc advanceToNextNestLevel(lex: var TomlLexer,
                            curTable: var TomlTableRef,
                            name: string) =

  var node = curTable[name]
  case node.kind
  of TomlKind.Table:
    curTable = node.tableVal
  of TomlKind.Tables:
    curTable = node.tablesVal[^1]
  else:
    raiseNotTable(lex, name)

proc createOrAppendTableArray(lex: var TomlLexer,
                              curTable: var TomlTableRef,
                              names: seq[string]) =

  # This is a table array entry (e.g. "[[entry]]")
  for idx, name in names:
    let lastTable = idx == names.high
    curTable[].withValue(name, node) do:
      # The element exists: is it of the right type?
      if lastTable:
        if node[].kind != TomlKind.Tables:
          raiseNotArray(lex, name)

        var newTable = TomlTableRef.new
        node[].tablesVal.add(newTable)
        curTable = newTable
      else:
        advanceToNextNestLevel(lex, curTable, name)
    do:
      # If this is the last name in the chain (e.g.,
      # "c" in "a.b.c"), its value should be an
      # array of tables, otherwise just a table
      if lastTable:
        var newValue = newTableArray(1)
        var newTable = TomlTableRef.new
        newValue.tablesVal[0] = newTable

        curTable[name] = newValue
        curTable = newTable
      else:
        var newValue = emptyTable()
        # Add the newly created object to the current table
        curTable[name] = newValue
        # Update the pointer to the current table
        curTable = newValue.tableVal

proc createTable(lex: var TomlLexer,
                 curTable: var TomlTableRef,
                 names: openArray[string],
                 dotted = false) =

  # This starts a new table (e.g. "[table]")
  for i, name in names:
    curTable[].withValue(name, val) do:
      if i == names.high and val[].kind == TomlKind.Table:
        if val[].tableVal.len == 0:
          raiseDuplicateTableKey(lex, name)
        elif not dotted:
          for value in val[].tableVal.values:
            if value.kind != TomlKind.Table:
              raiseDuplicateTableKey(lex, name)
      advanceToNextNestLevel(lex, curTable, name)
    do:
      # Add the newly created object to the current table
      var newVal = emptyTable()
      curTable[name] = newVal

      # Update the pointer to the current table
      curTable = newVal.tableVal

proc checkEol*(lex: var TomlLexer, line: int) =
  # new key val should start at next line
  let next = lex.nonws(skipLf)
  if next != EOF:
    if lex.line == line:
      raiseIllegalChar(lex, next)

proc parseKeyValue(lex: var TomlLexer, curTable: var TomlTableRef) =
  var pushTable = curTable
  var keys: seq[string]
  let line = lex.line

  scanKey(lex, keys)
  let key = keys.pop

  createTable(lex, curTable, keys, dotted = true)
  if curTable.hasKey(key):
    raiseDuplicateTableKey(lex, key)

  var next = lex.next
  if next != '=':
    raiseExpectChar(lex, '=')

  var newValue: TomlValueRef
  parseValue(lex, newValue)
  curTable[key] = newValue
  curTable = pushTable

  checkEol(lex, line)

proc parseToml*(lex: var TomlLexer): TomlValueRef =
  result = emptyTable()

  var
    next: char
    curTable = result.tableVal

  while true:
    next = lex.nonws(skipLf)
    case next
    of '[':
      advance
      var names: seq[string]
      let bracket = scanTableName(lex, names)

      curTable = result.tableVal
      if bracket == BracketType.double:
        createOrAppendTableArray(lex, curTable, names)
      else:
        createTable(lex, curTable, names)

    of '=':
      raiseTomlErr(lex, errKeyNameMissing)
    of '#', '.', ']':
      raiseIllegalChar(lex, next)
    of EOF:
      break
    else:
      # Everything else marks the presence of a "key = value" pattern
      parseKeyValue(lex, curTable)

proc parseKeyValue*(lex: var TomlLexer,
                   names, key: openArray[string],
                   tomlCase: TomlCase): bool =

  let line = lex.line

  var keys: seq[string]
  scanKey(lex, keys)
  let curKey = @names & keys

  var next = lex.next
  if next != '=':
    raiseExpectChar(lex, '=')

  if compare(curKey, key, tomlCase):
    return true

  var skipValue: TomlVoid
  parseValue(lex, skipValue)

  checkEol(lex, line)

proc parseKey*(key: string, tomlCase: TomlCase): seq[string] =
  var stream = unsafeMemoryInput(key)
  var lex = init(TomlLexer, stream)
  lex.scanKey(result)

proc parseToml*(lex: var TomlLexer, key: string, tomlCase: TomlCase): CodecState =
  ## move cursor to key position
  var
    next: char
    names: seq[string]
    keyList = parseKey(key, tomlCase)
    found = false

  while true:
    next = lex.nonws(skipLf)
    case next
    of '[':
      advance
      names.setLen(0)
      let bracket = scanTableName(lex, names)
      if bracket == BracketType.double:
        raiseTomlErr(lex, errDoubleBracket)

      if compare(keyList, names, tomlCase):
        found = true
        result = InsideRecord
        break

    of '=':
      raiseTomlErr(lex, errKeyNameMissing)
    of '#', '.', ']':
      raiseIllegalChar(lex, next)
    of EOF:
      break
    else:
      # Everything else marks the presence of a "key = value" pattern
      if parseKeyValue(lex, names, keyList, tomlCase):
        found = true
        result = ExpectValue
        break

  if not found:
    raiseKeyNotFound(lex, key)
