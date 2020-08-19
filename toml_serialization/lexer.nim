# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  strutils, unicode, options, tables, math,
  faststreams/inputs,
  types

type
  TomlLexer* = object
    stream*: InputStream
    line*: int
    col*: int
    pushBack: char
    pushCol: int
    pushLine: int
    flags: TomlFlags

  TomlReaderError* = object of TomlError
    line*, col*: int

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

const
  CR   = '\r'
  LF*  = '\l'
  EOF* = '\0'
  TAB  = '\t'
  SPC  = ' '
  invalidCommentChar = {'\x00'..'\x08', '\x0A'..'\x1F', '\x7F'}

template readChar(s: InputStream): char =
  char inputs.read(s)

template push*(lex: var TomlLexer, c: char) =
  lex.pushBack = c

template pushLineInfo*(lex: var TomlLexer) =
  lex.pushLine = lex.line
  lex.pushCol  = lex.col

template popLineInfo*(lex: var TomlLexer) =
  lex.pushLine = 0
  lex.pushCol  = 0

proc lineInfo(lex: var TomlLexer): (int, int) =
  if lex.pushLine != 0:
    result[0] = lex.pushLine
    lex.pushLine = 0
  else:
    result[0] = lex.line

  if lex.pushCol != 0:
    result[1] = lex.pushCol
    lex.pushCol = 0
  else:
    result[1] = lex.col

proc newTomlError*(lex: var TomlLexer, msg: string): ref TomlError =
  let (line, col) = lex.lineInfo
  result = newException(TomlError, "[" & $line &
                        ":" & $col & "]" & " " & msg)

template raiseTomlErr*(lex: TomlLexer, kind: TomlErrorKind) =
  raise(newTomlError(lex, $kind))

template raiseInvalidChar*(lex: TomlLexer, c: char) =
  raise(newTomlError(lex, $errInvalidChar & $ord(c)))

template raiseIllegalChar*(lex: TomlLexer, c: char) =
  raise(newTomlError(lex, $errIllegalChar & escape($c)))

template raiseUnknownEscape*(lex: TomlLexer, c: char) =
  raise(newTomlError(lex, "unknown escape sequence \"\\" & c & "\""))

template raiseUnexpectedValue*(lex: TomlLexer, s: string) =
  raise(newTomlError(lex, "Expected valid '" & s & "' value"))

template raiseUnexpectedField*(lex: TomlLexer, fieldName, typeName: string) =
  raise(newTomlError(lex, "Unexpected field '" &
                     fieldName & "' while deserializing '" &
                     typeName & "'"))

template raiseInvalidUnicode*(lex: TomlLexer, s: string) =
  raise(newTomlError(lex, $errInvalidUnicode & s))

template raiseDigitCount*(lex: TomlLexer, expected, actual: int) =
  raise(newTomlError(lex, "Expected digits count '" &
                     $expected & "' but got '" & $actual & "'"))

template raiseBadValue*(lex: TomlLexer, msg: string, value: SomeInteger) =
  raise(newTomlError(lex, msg & "(" & $value & ")"))

template raiseExpectChar*(lex: TomlLexer, c: char) =
  raise(newTomlError(lex, "expected \'" & escape($c) & "\'"))

template raiseNotTable(lex: TomlLexer, name: string) =
  raise(newTomlError(lex, "\"" & name & "\" is not a table"))

template raiseNotArray(lex: TomlLexer, name: string) =
  raise(newTomlError(lex, "\"" & name & "\" is not an array"))

template raiseKeyNotFound(lex: TomlLexer, key: string) =
  raise(newTomlError(lex, $errKeyNotFound & "\'" & key & "\'"))

template raiseInvalidHex*(lex: TomlLexer, s: string) =
  raise(newTomlError(lex, $errInvalidHex & s))

template raiseDuplicateTableKey*(lex: TomlLexer, s: string) =
  raise(newTomlError(lex, $errDuplicateTableKey & s & "\'"))

proc init*(T: type TomlLexer, stream: InputStream, flags: TomlFlags = {}): T =
  T(stream: stream,
    line: 1,
    col: 1,
    flags: flags
   )

proc next(lex: var TomlLexer): char =
  ## Return the next available char from the stream associate with
  ## the parser lex, or EOF if there are no characters left.

  if lex.pushBack != EOF:
    # If we've just read a character without having interpreted
    # it, just return it
    result = lex.pushBack
    lex.pushBack = EOF
  else:
    if not lex.stream.readable():
      return EOF

    result = lex.stream.readChar()

    # Update the line and col number
    if result == LF:
      inc(lex.line)
      lex.col = 1
    elif result != CR:
      inc(lex.col)

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
    next = lex.next
    if next == '#':
      # Skip the comment up to the newline, but do not jump over it
      while next != LF:
        if next == CR:
          if lex.next != LF:
            raiseIllegalChar(lex, next)
          else:
            lex.push LF
            break
        elif next in invalidCommentChar:
          raiseIllegalChar(lex, next)
        elif not lex.stream.readable:
          # rase case
          next = lex.next
          break
        next = lex.next

    if next notin whitespaces: break
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
    next = lex.next
    if next == '_':
      if firstPos or wasUnderscore:
        raiseTomlErr(lex, errUnderscoreDigit)
      continue

    if leading == Leading.DenyZero:
      if next == '0' and firstPos:
        # TOML specifications forbid this
        var upcomingChar = lex.next
        if upcomingChar in Digits:
          raiseTomlErr(lex, errForbidLeadingZero)
        else:
          lex.push(upcomingChar)

    if next notin digits:
      if wasUnderscore:
        raiseTomlErr(lex, errUnderscoreDigit)
      lex.push next
      break

    when T is string:
      value.add next
    elif T is uint64:
      value = value * baseNum + charTo(T, next)
    elif T is TomlVoid:
      discard
    else:
      {.fatal: "`scanUint` only accepts `string` or `uint64` or `TomlVoid`".}

    firstPos = false

proc scanDigits*[T](lex: var TomlLexer, value: var T, base: NumberBase): int =
  ## scanUint only accepts `string` or `int`

  var next: char

  when T is int:
    let baseNum = (case base
                   of base2: 2
                   of base8: 8
                   of base10: 10
                   of base16: 16)

  let digits  = baseToDigits(base)

  while true:
    next = lex.next

    if next notin digits:
      lex.push next
      break

    inc result
    when T is string:
      value.add next
    elif T is int:
      value = value * baseNum + charTo(T, next)
    elif T is TomlVoid:
      discard
    else:
      {.fatal: "`scanDigits` only accepts `string` or `int`".}

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

proc scanUnicode[T](lex: var TomlLexer, res: var T) =
  when T isnot (string or TomlVoid):
    {.fatal: "`scanUnicode` only accepts `string` or `TomlVoid`".}

  let kind = lex.next
  var code: int
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
      lex.push(esc)
      scanUnicode(lex, res)
    else:
      raiseUnknownEscape(lex, esc)
  else:
    case esc
    of 'b', 't', 'n', 'f', 'r', '\'', '\"', '\\':
      discard
    of 'x':
      scanHexEscape(lex, res)
    of 'u', 'U':
      lex.push(esc)
      scanUnicode(lex, res)
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
    next = lex.next

    # Skip the first newline, if it comes immediately after the
    # quotation marks
    if isFirstChar and (next == LF):
      isFirstChar = false
      continue

    if next == delimiter:
      # Are we done?
      next = lex.next
      if next == delimiter:
        next = lex.next
        if next == delimiter:
          next = lex.next
          # Done with this string
          if next == delimiter:
            when T is string:
              res.add delimiter
          else:
            lex.push next
          return
        else:
          # Just got a double delimiter
          when T is string:
            res.add delimiter
            res.add delimiter
          lex.push next
          continue
      else:
        # Just got a lone delimiter
        when T is string:
          res.add(delimiter)
        lex.push next
        continue

    isFirstChar = false
    when kind == StringType.Basic:
      if next == '\\':
        # This can either be an escape sequence or a end-of-line char
        next = lex.next
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

          while next in {CR, LF, SPC, TAB}:
            next = lex.next

          lex.push next
          continue
        else:
          # This is just an escape sequence (like "\t")
          lex.scanEscapeChar(next, res)
          continue

    if next == EOF:
      raiseTomlErr(lex, errUnterminatedString)

    if ord(next) in {16, 31, 127}:
      raiseInvalidChar(lex, next)

    when T is string:
      res.add(next)

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
  ## first character (either \" or \', which is passed in the
  ## "openChar" parameter).
  ## returns true if multi line string

  const delimiter = stringDelimiter(kind)
  var next = lex.next
  if next == delimiter:
    # We have two possibilities here: (1) the empty string, or (2)
    # "long" multi-line strings.
    next = lex.next
    if next == delimiter:
      scanMultiLineString(lex, res, kind)
      result = true
    else:
      # Empty string. This was easy!
      lex.push next
  else:
    lex.push next
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
    next = lex.next
    case next:
    of '0':
      next = lex.next
      if sign == Sign.None:
        if next in {'b', 'x', 'o'}:
          lex.push next
          return (Sign.None, scanEncoding(lex, value))
        else:
          case next:
          of strutils.Whitespace:
            lex.push next
            return zeroVal()
          of strutils.Digits:
            raiseTomlErr(lex, errForbidLeadingZero)
          else:
            # else is a sole 0
            return zeroVal()
      else:
        case next:
        of strutils.Whitespace:
          lex.push next
          return zeroVal()
        else:
          # else is a sole 0
          return zeroVal()
    of strutils.Digits - {'0'}:
      lex.push next
      scanUint(lex, value, base10, Leading.DenyZero)
      break
    of '+':
      sign = Sign.Pos
      continue
    of '-':
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
    if lex.scanString(res, StringType.Basic):
      raiseTomlErr(lex, errMLStringName)
    return

  elif next == '\'':
    if lex.scanString(res, StringType.Literal):
      raiseTomlErr(lex, errMLStringName)
    return

  lex.push next
  while true:
    next = lex.next
    if (next in {'=', '.', '[', ']', EOF, SPC, TAB}):
      # Any of the above characters marks the end of the name
      lex.push next
      break
    elif (next notin {'a'..'z', 'A'..'Z', '0'..'9', '_', '-'}):
      raiseIllegalChar(lex, next)
    else:
      when T is string:
        res.add(next)

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
      when T is string:
        let dot = next
      next = lex.nonws(skipNoLf)
      if next in {'\'', '\"', '-', '_', 'a'..'z', 'A'..'Z', '0'..'9'}:
        when T is string:
          res.add dot
      else:
        raiseIllegalChar(lex, next)
      lex.push next

    else:
      lex.push next
      break

type
  BracketType* {.pure.} = enum
    single, double

proc scanTableName*[T](lex: var TomlLexer, res: var T): BracketType =
  when T isnot (string or seq[string]):
    {.fatal: "`scanTableName` only accepts `string` or `seq[string]`".}

  ## This code assumes that '[' has already been consumed

  var next = lex.next
  if next == '[':
    result = BracketType.double
  else:
    lex.push next
    result = BracketType.single

  lex.scanKey(res)

  case lex.nonws(skipNoLf)
  of ']':
    if result == BracketType.double:
      next = lex.next
      if next != ']':
        raiseTomlErr(lex, errNoDoubleBracket)

    # We must check that there is nothing else in this line
    next = lex.nonws(skipNoLf)
    if next notin {LF, EOF}:
      raiseIllegalChar(lex, next)
  else:
    raiseTomlErr(lex, errNoSingleBracket)

proc scanBool*(lex: var TomlLexer): bool =
  var next = nonws(lex, skipLf)
  lex.pushLineInfo # used for error messages

  case next
  of 't':
    # Is this "true"?
    if lex.next != 'r' or
       lex.next != 'u' or
       lex.next != 'e':
       raiseTomlErr(lex, errInvalidBool)
    result = true

  of 'f':
    # Is this "false"?
    if lex.next != 'a' or
       lex.next != 'l' or
       lex.next != 's' or
       lex.next != 'e':
       raiseTomlErr(lex, errInvalidBool)
    result = false

  else:
    raiseTomlErr(lex, errInvalidBool)

  lex.popLineInfo

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
    next = lex.next
    if next == '_':
      if firstPos or wasUnderscore:
        raiseTomlErr(lex, errUnderscoreDigit)
      continue
    if next notin strutils.Digits:
      if wasUnderscore:
        raiseTomlErr(lex, errUnderscoreDigit)
      lex.push next
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

  var next = lex.next
  case next
  of '-':
    when T is string:
      value.add next
    elif T is SomeFloat:
      sign = Sign.Neg
    next = lex.next
    if next notin strutils.Digits:
      raiseIllegalChar(lex, next)
    lex.push next
  of '+':
    when T is string:
      value.add next
    elif T is SomeFloat:
      sign = Sign.Pos
    next = lex.next
    if next notin strutils.Digits:
      raiseIllegalChar(lex, next)
    lex.push next
  of strutils.Digits:
    lex.push next
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
  var next = lex.next
  if next in {'e', 'E'}:
    when T is string:
      value.add next
    scanExponent(lex, value)
  else:
    lex.push next

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
    next = lex.next
    case next
    of '-':
      when T is string:
        value.add next
      sign = Sign.Neg
      continue
    of '+':
      when T is string:
        value.add next
      sign = Sign.Pos
      continue
    of 'i':
      lex.pushLineInfo
      if lex.next != 'n' or
         lex.next != 'f':
         raiseTomlErr(lex, errUnknownIdent)
      when T is string:
        value.add "inf"
      else:
        value = Inf
        if sign == Sign.Neg: value = -Inf
      lex.popLineInfo
      return sign
    of 'n':
      lex.pushLineInfo
      if lex.next != 'a' or
         lex.next != 'n':
         raiseTomlErr(lex, errUnknownIdent)
      when T is string:
        value.add "nan"
      else:
        value = Nan
        if sign == Sign.Neg: value = -Nan
      lex.popLineInfo()
      return sign
    of strutils.Digits:
      lex.push next
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

  next = lex.next
  case next
  of '.':
    lex.addFrac(value, sign)
  of 'e', 'E':
    when T is string:
      value.add next
    lex.scanExponent(value)
  else:
    lex.push next

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

  if lex.line != line:
    raiseTomlErr(lex, errDateTimeML)

  next = lex.next
  if next != ':':
    if TomlHourMinute in lex.flags:
      lex.push next
      return
    else:
      lex.raiseExpectChar(':')

  when T is string:
    value.add next

  # Parse the second. Note that seconds=60 *can* happen (leap second)
  scanStrictNum(lex, num, minVal = 0, maxVal = 60, count = 2,
                "number out of range for `seconds`")

  when T is TomlTime:
    value.second = num

  next = lex.next
  if next == '.':
    if lex.line != line:
      raiseTomlErr(lex, errDateTimeML)

    when T is string:
      value.add next

    next = lex.next
    if next notin strutils.Digits:
      raiseIllegalChar(lex, next)
    else:
      lex.push next

    # Toml spec says additional subsecond precision
    # should be truncated and not rounded
    var subsecond: string
    when T isnot TomlVoid:
      let len = scanDigits(lex, subsecond, base10)
      if len > subsecondPrecision:
        subsecond.setLen(subsecondPrecision)
    else:
      discard scanDigits(lex, subsecond, base10)

    when T is string:
      value.add subsecond
    elif T is TomlTime:
      value.subsecond = parseInt(subsecond)
  else:
    lex.push next

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
    next = lex.next

  when T is (string or TomlVoid):
    template num: untyped = value
  else:
    var num : int

  case next
  of 'z', 'Z':
    when T is string:
      value.add next
    elif T is TomlTimeZone:
      value.positiveShift = true
      value.hourShift = 0
      value.minuteShift = 0
    result = true
  of '-', '+':
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
    elif T is TomlTimezone:
      value.hourShift = num

    scanStrictNum(lex, num, minVal = 0, maxVal = 59, count = 2,
                "number out of range for `zone minutes`")

    when T is TomlTimeZone:
      value.minuteShift = num

    result = true
  of EOF:
    discard
  else:
    lex.push next

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

  let delim = lex.next
  if delim notin {'t', 'T', ' '}:
    lex.push delim
    return

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

proc parseNumOrDate*[T](lex: var TomlLexer, value: var T) =
  when T isnot (TomlValueRef or string or TomlVoid):
    {.fatal: "`parseNumOrDate` only accepts `TomlValueRef' or string or `TomlVoid`".}

  var
    next: char
    sign = Sign.None

  when T is TomlValueRef:
    var uintVal: uint64

  while true:
    next = lex.next
    case next:
    of '0':
      next = lex.next
      if sign == Sign.None:
        if next in {'b', 'x', 'o'}:
          lex.push next
          when T is (string or TomlVoid):
            discard scanEncoding(lex, value)
          else:
            value = TomlValueRef(kind: TomlKind.Int)
            discard scanEncoding(lex, uintVal)
            value.intVal = uintVal.int
        else:
          # This must now be a float or a date/time, or a sole 0
          case next:
          of '.':
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
            lex.push next
            when T is string:
              value.add '0'
            elif T is TomlValueRef:
              value = TomlValueRef(kind: TomlKind.Int, intVal: 0)
            return
          of strutils.Digits:
            lex.push next
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
          lex.push next
          when T is string:
            value.add '0'
          elif T is TomlValueRef:
            value = TomlValueRef(kind: TomlKind.Int, intVal: 0)
          return
        of 'e', 'E':
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
      # This might be a date/time, or an int or a float
      var
        digits = 1
        wasUnderscore = false

      when T is string:
        value.add next
      elif T is TomlValueRef:
        var curSum = int64(next) - int64('0')

      while true:
        next = lex.next
        if wasUnderscore and next notin strutils.Digits:
          raiseTomlErr(lex, errUnderscoreDigit)

        case next:
        of ':':
          if digits != 2:
            raiseTomlErr(lex, errInvalidDateTime)
          lex.push next
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
          lex.push next
          when T is (string or TomlVoid):
            scanLongDate(lex, 0, value)
          else:
            value = TomlValueRef(kind: TomlKind.DateTime)
            scanLongDate(lex, curSum.int, value.dateTime)
          return
        of '.':
          when T is (string or TomlVoid):
            addFrac(lex, value, sign)
          else:
            value = TomlValueRef(kind: TomlKind.Float, floatVal: float64(curSum))
            if sign == Sign.Neg:
              value.floatVal = -value.floatVal
            addFrac(lex, value.floatVal, sign)
          return
        of 'e', 'E':
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
          when T is string:
            value.add next
            inc digits
          elif T is TomlVoid:
            inc digits
          else:
            try:
              curSum = curSum * 10'i64 + int64(next) - int64('0')
              inc digits
            except OverflowError:
              raiseTomlErr(lex, errIntegerOverflow)
          wasUnderscore = false
          continue
        of '_':
          wasUnderscore = true
          continue
        of strutils.Whitespace:
          lex.push next
          when T is TomlValueRef:
            value = TomlValueRef(
              kind: TomlKind.Int,
              intVal: if sign == Neg: -curSum else: curSum
            )
          return
        else:
          lex.push next
          when T is TomlValueRef:
            value = TomlValueRef(
              kind: TomlKind.Int,
              intVal: if sign == Neg: -curSum else: curSum
            )
          return
        break

    of '+':
      sign = Sign.Pos
      when T is string:
        value.add '+'
      continue
    of '-':
      sign = Sign.Neg
      when T is string:
        value.add '-'
      continue
    of 'i':
      lex.pushLineInfo
      if lex.next != 'n' or
         lex.next != 'f':
         raiseTomlErr(lex, errUnknownIdent)
      when T is string:
        value.add "inf"
      elif T is TomlValueRef:
        value = TomlValueRef(kind: TomlKind.Float, floatVal: Inf)
      lex.popLineInfo
      return
    of 'n':
      lex.pushLineInfo
      if lex.next != 'a' or
         lex.next != 'n':
         raiseTomlErr(lex, errUnknownIdent)
      when T is string:
        value.add "nan"
      elif T is TomlValueRef:
        value = TomlValueRef(kind: TomlKind.Float, floatVal: Nan)
      lex.popLineInfo
      return
    else:
      raiseIllegalChar(lex, next)
    break

proc parseValue*[T](lex: var TomlLexer, value: var T)

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
      when T is string:
        value.add next
      return
    of EOF:
      raiseTomlErr(lex, errUnterminatedArray)
    of ',':
      if numElem == 0:
        # This happens with "[, 1, 2]", for instance
        raiseTomlErr(lex, errMissingFirstElement)

      # Check that this is not a terminating comma (like in
      #  "[b,]")
      next = lex.nonws(skipLf)
      if next == ']':
        when T is string:
          value.add next
        return
      else:
        when T is string:
          value.add ','

      lex.push next
    else:
      lex.push next
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
      when T is string:
        value.add next
      return
    of EOF:
      raiseTomlErr(lex, errUnterminatedTable)
    of ',':
      if firstComma:
        raiseTomlErr(lex, errMissingFirstElement)

      when T is string:
        value.add ','

      next = lex.nonws(skipNoLf)
      if next == '}':
        raiseIllegalChar(lex, '}')

      lex.push next
    of '\n':
      if TomlInlineTableNewline in lex.flags:
        continue
      else:
        raiseIllegalChar(lex, next)
    else:
      firstComma = false
      lex.push next

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
    lex.push next
    parseNumOrDate(lex, value)
  of 't', 'f':
    lex.push next
    when T is string:
      let val = lex.scanBool
      value.add $val
    elif T is TomlVoid:
      discard lex.scanBool
    else:
      let val = lex.scanBool
      value = TomlValueRef(kind: TomlKind.Bool, boolVal: val)
  of '\"':
    when T is (string or TomlVoid):
      discard scanString(lex, value, StringType.Basic)
    else:
      value = TomlValueRef(kind: TomlKind.String)
      discard scanString(lex, value.stringVal, StringType.Basic)
  of '\'':
    when T is (string or TomlVoid):
      discard scanString(lex, value, StringType.Literal)
    else:
      value = TomlValueRef(kind: TomlKind.String)
      discard scanString(lex, value.stringVal, StringType.Literal)
  of '[':
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

proc emptyTable(): TomlValueRef =
  TomlValueRef(
    kind: TomlKind.Table,
    tableVal: TomlTableRef.new
  )

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

  # new key val should start at next line
  next = lex.nonws(skipLf)
  if next != EOF:
    if lex.line == line:
      raiseIllegalChar(lex, next)
  lex.push next

proc parseToml*(lex: var TomlLexer): TomlValueRef =
  result = emptyTable()

  var
    next: char
    curTable = result.tableVal

  while true:
    next = lex.nonws(skipLf)
    case next
    of '[':
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
    of '\0': # EOF
      break
    else:
      # Everything else marks the presence of a "key = value" pattern
      lex.push next
      parseKeyValue(lex, curTable)

func compare(a, b: openArray[string], tomlCase: TomlCase): bool =
  case tomlCase
  of TomlCaseSensitive, TomlCaseNim:
    result = a == b
  of TomlCaseInsensitive:
    if a.len != b.len: return false
    for i, x in a:
      if cmpIgnoreCase(x, b[i]) != 0:
        return false
    result = true

proc nimNormalize(x: string): string =
  # TODO: avoid realloc
  result = normalize(x)
  if x.len > 0:
    result[0] = x[0]

proc normalize(x: openArray[string]): seq[string] =
  for z in x:
    result.add nimNormalize(z)

proc parseKeyValue(lex: var TomlLexer,
                   names, key: openArray[string],
                   tomlCase: TomlCase): bool =

  let line = lex.line

  var keys: seq[string]
  scanKey(lex, keys)
  let curKey = if tomlCase == TomlCaseNim:
                 @names & normalize(keys)
               else:
                 @names & keys

  var next = lex.next
  if next != '=':
    raiseExpectChar(lex, '=')

  if compare(curKey, key, tomlCase):
    return true

  var skipValue: TomlVoid
  parseValue(lex, skipValue)

  # new key val should start at next line
  next = lex.nonws(skipLf)
  if next != EOF:
    if lex.line == line:
      raiseIllegalChar(lex, next)
  lex.push next

proc parseKey(key: string, tomlCase: TomlCase): seq[string] =
  var stream = unsafeMemoryInput(key)
  var lex = init(TomlLexer, stream)
  lex.scanKey(result)
  if tomlCase == TomlCaseNim:
    result = normalize(result)

proc parseToml*(lex: var TomlLexer, key: string, tomlCase: TomlCase) =
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
      names.setLen(0)
      let bracket = scanTableName(lex, names)
      if bracket == BracketType.double:
        raiseTomlErr(lex, errDoubleBracket)

      names = normalize(names)
      if compare(keyList, names, tomlCase):
        found = true
        break

    of '=':
      raiseTomlErr(lex, errKeyNameMissing)
    of '#', '.', ']':
      raiseIllegalChar(lex, next)
    of '\0': # EOF
      break
    else:
      # Everything else marks the presence of a "key = value" pattern
      lex.push next
      if parseKeyValue(lex, names, keyList, tomlCase):
        found = true
        break

  if not found:
    raiseKeyNotFound(lex, key)
