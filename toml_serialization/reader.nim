# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  enumutils, tables, strutils, typetraits, options,
  stew/[enums, objects],
  faststreams/inputs, serialization/[object_serialization, errors],
  types, lexer, private/[utils, array_reader]

export
  TomlReaderError, TomlFieldReadingError

type
  TomlReader* = object
    lex*: TomlLexer
    state: CodecState
    tomlCase: TomlCase

  GenericTomlReaderError* = object of TomlReaderError
    deserializedField*: string
    innerException*: ref CatchableError

proc assignLineNumber*(ex: ref TomlReaderError, r: TomlReader) =
  ex.line = r.lex.line
  ex.col = r.lex.col

proc handleReadException*(r: TomlReader,
                          Record: type,
                          fieldName: string,
                          field: auto,
                          err: ref CatchableError) =
  var ex = new GenericTomlReaderError
  ex.assignLineNumber(r)
  ex.deserializedField = fieldName
  ex.innerException = err
  raise ex

proc init*(T: type TomlReader,
           stream: InputStream,
           tomlCase: TomlCase,
           flags: TomlFlags = {}): T =
  result.lex = TomlLexer.init(stream, flags)
  result.state = TopLevel
  result.tomlCase = tomlCase

proc init*(T: type TomlReader,
           stream: InputStream,
           flags: TomlFlags = {}): T =
  TomlReader.init(stream, TomlCaseSensitive, flags)

proc moveToKey*(r: var TomlReader, key: string, tomlCase: TomlCase) =
  if key.len > 0:
    r.state = r.lex.parseToml(key, tomlCase)

const
  signedDigits = strutils.Digits + {'+', '-'}

template eatChar() =
  discard next(r.lex)

template expectChar(c: char, skip = skipLf) =
  if nonws(r.lex, skip) != c:
    raiseExpectChar(r.lex, c)
  eatChar

template expectChars(c: set[char], err: untyped, skip = skipLf) =
  if nonws(r.lex, skipLf) notin c:
    raiseTomlErr(r.lex, err)

template expectChars(c: set[char], skip = skipLf) =
  let next = nonws(r.lex, skipLf)
  if next notin c:
    raiseIllegalChar(r.lex, next)

template maybeChar(c: char, skip = skipLf) =
  if nonws(r.lex, skipLf) == c:
    eatChar

proc scanInt[T](r: var TomlReader, value: var T) =
  var x: uint64
  let (sign, _) = scanInt(r.lex, x)
  if sign == Neg:
    when value is SomeUnsignedInt:
      raiseTomlErr(r.lex, errNegateUint)
    else:
      value = toSigned[T](r.lex, x)
  else:
    value = T(x)

proc parseStringEnum(
    r: var TomlReader, T: type enum, next: char,
    normalizer: static[proc(s :string): string]): T =
  eatChar
  var s: string
  case next
  of '\"':
    if scanString(r.lex, s, StringType.Basic):
      raiseTomlErr(r.lex, errMLStringEnum)
  of '\'':
    if scanString(r.lex, s, StringType.Literal):
      raiseTomlErr(r.lex, errMLStringEnum)
  else:
    raiseIllegalChar(r.lex, next)
  try:
    result = genEnumCaseStmt(
      T, s, default = nil, ord(T.low), ord(T.high), normalizer)
  except ValueError as err:
    const typeName = typetraits.name(T)
    raiseUnexpectedValue(r.lex, typeName)

proc parseEnum*(
    r: var TomlReader, T: type enum,
    normalizer: static[proc(s :string): string]): T =
  var next = nonws(r.lex, skipLf)
  const typeName = typetraits.name(T)
  case T.enumStyle
  of EnumStyle.Numeric:
    if next in signedDigits:
      var n: uint64
      r.scanInt(n)
      if not result.checkedEnumAssign(n):
        raiseUnexpectedValue(r.lex, typeName)
    else:  # Also allow decoding by the enum value itself
      result = r.parseStringEnum(T, next, normalizer)
  of EnumStyle.AssociatedStrings:
    result = r.parseStringEnum(T, next, normalizer)

proc parseEnum*(r: var TomlReader, T: type enum): T =
  func normalizer(s: string): string =  # Strict comparison with values
    s
  r.parseEnum(T, normalizer)

proc parseValue*(r: var TomlReader): TomlValueRef =
  try:
    if r.state == TopLevel:
      result = parseToml(r.lex)
    else:
      parseValue(r.lex, result)
  except ValueError:
    const typeName = typetraits.name(type result)
    raiseUnexpectedValue(r.lex, typeName)

template parseInlineTable(r: var TomlReader, key: untyped, body: untyped) =
  let prevState = r.state
  var firstComma = true
  expectChar('{')

  while true:
    var next = nonws(r.lex, skipLf)
    case next
    of '}':
      eatChar
      break
    of EOF:
      raiseTomlErr(r.lex, errUnterminatedTable)
    of ',':
      eatChar
      if firstComma:
        raiseTomlErr(r.lex, errMissingFirstElement)

      next = nonws(r.lex, skipNoLf)
      if next == '}':
        if TomlInlineTableTrailingComma in r.lex.flags:
          eatChar
          break
        else:
          raiseIllegalChar(r.lex, ',')
    of '\n':
      if TomlInlineTableNewline in r.lex.flags:
        eatChar
        continue
      else:
        raiseIllegalChar(r.lex, next)
    else:
      key.setLen(0)
      scanKey(r.lex, key)
      expectChar('=')
      r.state = ExpectValue
      body
      r.state = prevState
      firstComma = false

template parseRecord(r: var TomlReader, key: untyped, body: untyped) =
  let prevState = r.state
  while true:
    var next = nonws(r.lex, skipLf)
    case next
    of '[', EOF:
      break
    of '#', '.', ']', '=':
      raiseIllegalChar(r.lex, next)
    else:
      key.setLen(0)
      scanKey(r.lex, key)
      expectChar('=')
      r.state = ExpectValue
      body
      r.state = prevState

template parseTable*(r: var TomlReader, key: untyped, body: untyped) =
  var `key` {.inject.}: string
  if r.state == ExpectValue:
    parseInlineTable(r, `key`, body)
  else:
    parseRecord(r, `key`, body)

template parseListImpl*(r: var TomlReader, index, body: untyped) =
  expectChar('[')
  while true:
    var next = nonws(r.lex, skipLf)
    case next
    of ']':
      eatChar
      break
    of EOF:
      raiseTomlErr(r.lex, errUnterminatedArray)
    of ',':
      eatChar
      if index == 0:
        # This happens with "[, 1, 2]", for instance
        raiseTomlErr(r.lex, errMissingFirstElement)

      # Check that this is not a terminating comma (like in
      #  "[b,]")
      next = nonws(r.lex, skipLf)
      if next == ']':
        eatChar
        break
    else:
      body
      inc index

template parseList*(r: var TomlReader, body: untyped) =
  var idx = 0
  parseListImpl(r, idx, body)

template parseList*(r: var TomlReader, i, body: untyped) =
  var `i` {.inject.} = 0
  parseListImpl(r, `i`, body)

proc skipTableBody(r: var TomlReader) =
  var skipValue: TomlVoid
  r.parseTable(key):
    discard key
    parseValue(r.lex, skipValue)

proc readValue*[T](r: var TomlReader, value: var T, numRead: int)
                  {.gcsafe, raises: [SerializationError, IOError, ValueError, Defect].} =
  mixin readValue

  when T is seq:
    value.setLen(numRead + 1)
    readValue(r, value[numRead])
  elif T is array:
    readValue(r, value[numRead])
  elif isOptionalInToml(T):
    if value.isNone:
      value = some default(typeof(value.get))
    readValue(r, value.get, numRead)
  else:
    const typeName = typetraits.name(T)
    {.error: "Failed to convert from TOML an unsupported type: " & typeName.}

proc decodeRecord[T](r: var TomlReader, value: var T) =
  mixin readValue

  const
    totalFields = T.totalSerializedFields
    arrayFields = T.totalArrayFields

  when  totalFields > 0:
    let fields = T.fieldReadersTable(TomlReader)

    when arrayFields > 0:
      var arrayReaders = T.arrayReadersTable(TomlReader)

    var
      expectedFieldPos = 0
      fieldsDone = 0
      fieldName = newStringOfCap(defaultStringCapacity)
      next: char
      prevState = r.state
      line = r.lex.line

    while true:
      fieldName.setLen(0)
      next = nonws(r.lex, skipLf)
      case next
      of '[':
        case r.state
        of TopLevel:
          r.state = InsideRecord
        of InsideRecord, ArrayOfTable:
          r.state = prevState
          break
        else:
          raiseIllegalChar(r.lex, next)
        eatChar
        let bracket = scanTableName(r.lex, fieldName)

        when arrayFields > 0:
          if bracket == BracketType.double:
            r.state = ArrayOfTable
        else:
          if bracket == BracketType.double:
            raiseTomlErr(r.lex, errDoubleBracket)

      of '=': raiseTomlErr(r.lex, errKeyNameMissing)
      of '#', '.', ']':
        raiseIllegalChar(r.lex, next)
      of EOF:
        break
      else:
        # Everything else marks the presence of a key
        if r.state notin {TopLevel, InsideRecord, ArrayOfTable}:
          raiseIllegalChar(r.lex, next)
        r.state = ExpectValue
        scanKey(r.lex, fieldName)
        expectChar('=')

      when arrayFields > 0:
        if r.state == ArrayOfTable:
          let reader = findArrayReader(arrayReaders, fieldName, r.tomlCase)
          if reader != BadArrayReader:
            reader.readArray(arrayReaders, value, r)
          elif TomlUnknownFields in r.lex.flags:
            r.skipTableBody
          else:
            const typeName = typetraits.name(T)
            raiseUnexpectedField(r.lex, fieldName, typeName)
          r.state = prevState
          continue

      when value is tuple:
        var reader = fields[][expectedFieldPos].reader
        expectedFieldPos += 1
      else:
        var reader = findFieldReader(fields[],
                      fieldName, expectedFieldPos, r.tomlCase)

      if reader != nil:
        try:
          reader(value, r)
        except TomlError as err:
          raise (ref TomlFieldReadingError)(field: fieldName, error: err)
        checkEol(r.lex, line)
        r.state = prevState
        inc fieldsDone
      elif TomlUnknownFields in r.lex.flags:
        # efficient skip, it doesn't produce any tokens
        var skipValue: TomlVoid
        parseValue(r.lex, skipValue)
        checkEol(r.lex, line)
        r.state = prevState
      else:
        const typeName = typetraits.name(T)
        raiseUnexpectedField(r.lex, fieldName, typeName)

      if fieldsDone >= totalFields:
        break

proc decodeInlineTable[T](r: var TomlReader, value: var T) =
  mixin readValue

  const totalFields = T.totalSerializedFields
  when  totalFields > 0:
    let fields = T.fieldReadersTable(TomlReader)
    var
      expectedFieldPos = 0
      fieldName = newStringOfCap(defaultStringCapacity)
      firstComma = true
      next: char

    expectChar('{', skipNoLf)

    while true:
      fieldName.setLen(0)
      next = nonws(r.lex, skipNoLf)
      case next
      of '}':
        eatChar
        break
      of EOF:
        raiseTomlErr(r.lex, errUnterminatedTable)
      of ',':
        eatChar
        if firstComma:
          raiseTomlErr(r.lex, errMissingFirstElement)

        next = nonws(r.lex, skipNoLf)
        if next == '}':
          if TomlInlineTableTrailingComma in r.lex.flags:
            eatChar
            break
          else:
            raiseIllegalChar(r.lex, ',')
      of '\n':
        if TomlInlineTableNewline in r.lex.flags:
          eatChar
          continue
        else:
          raiseIllegalChar(r.lex, next)
      else:
        firstComma = false
        scanKey(r.lex, fieldName)
        expectChar('=', skipNoLf)

        when value is tuple:
          var reader = fields[][expectedFieldPos].reader
          expectedFieldPos += 1
        else:
          var reader = findFieldReader(fields[],
                        fieldName, expectedFieldPos, r.tomlCase)

        if reader != nil:
          reader(value, r)
        elif TomlUnknownFields in r.lex.flags:
          # efficient skip, it doesn't produce any tokens
          var skipValue: TomlVoid
          parseValue(r.lex, skipValue)
        else:
          const typeName = typetraits.name(T)
          raiseUnexpectedField(r.lex, fieldName, typeName)

template getUnderlyingType*[T](_: Option[T]): untyped = T

proc readValue*[T](r: var TomlReader, value: var T)
                  {.gcsafe, raises: [SerializationError, IOError, Defect].} =
  mixin readValue

  when value is Option:
    # `readValue` from nim-serialization will suppress
    # compiler error when the underlying type
    # has `requiresInit` pragma.
    value = some(r.readValue(getUnderlyingType(value)))

  elif value is TomlValueRef:
    value = r.parseValue

  elif value is string:
    # every value can be deserialized as string
    parseValue(r.lex, value)

  elif value is TomlTime:
    expectChars(strutils.Digits, errInvalidDateTime)
    scanTime(r.lex, value)

  elif value is TomlDate:
    expectChars(strutils.Digits, errInvalidDateTime)
    scanDate(r.lex, value)

  elif value is TomlDateTime:
    expectChars(strutils.Digits, errInvalidDateTime)
    scanDateTime(r.lex, value)

  elif value is SomeInteger:
    expectChars(signedDigits)
    r.scanInt(value)

  elif value is SomeFloat:
    expectChars(signedDigits)
    discard scanFloat(r.lex, value)

  elif value is bool:
    value = scanBool(r.lex)

  elif value is enum:
    value = r.parseEnum(T)

  elif value is seq:
    r.parseList:
      let lastPos = value.len
      value.setLen(lastPos + 1)
      readValue(r, value[lastPos])

  elif value is array:
    expectChar('[')

    for i in low(value) ..< high(value):
      readValue(r, value[i])
      expectChar(',')

    readValue(r, value[high(value)])
    maybeChar(',')
    expectChar(']')

  elif value is (object or tuple):
    if r.state == ExpectValue:
      r.decodeInlineTable(value)
    else:
      r.decodeRecord(value)

  else:
    const typeName = typetraits.name(T)
    {.error: "Failed to convert from TOML an unsupported type: " & typeName.}

proc readTableArray*(r: var TomlReader, T: type, key: string, tomlCase: TomlCase): T
                        {.raises: [SerializationError, IOError, ValueError, Defect].} =
  mixin readValue

  ## move cursor to key position
  var
    next: char
    names: seq[string]
    keyList = parseKey(key, tomlCase)
    found = false

  when T is array:
    var i = 0

  while true:
    next = nonws(r.lex, skipLf)
    case next
    of '[':
      eatChar
      names.setLen(0)
      let bracket = scanTableName(r.lex, names)
      if not compare(keyList, names, tomlCase):
        continue

      if bracket == BracketType.single:
        raiseTomlErr(r.lex, errExpectDoubleBracket)

      found = true
      r.state = InsideRecord
      when T is array:
        if i < result.len:
          r.readValue(result[i])
          inc i
        else:
          break
      else:
        let lastPos = result.len
        result.setLen(lastPos + 1)
        r.readValue(result[lastPos])

    of '=':
      raiseTomlErr(r.lex, errKeyNameMissing)
    of '#', '.', ']':
      raiseIllegalChar(r.lex, next)
    of EOF:
      break
    else:
      # Everything else marks the presence of a "key = value" pattern
      if parseKeyValue(r.lex, names, keyList, tomlCase):
        raiseTomlErr(r.lex, errExpectDoubleBracket)

  if not found:
    raiseKeyNotFound(r.lex, key)

# these are helpers functions

proc parseNumber*(r: var TomlReader, value: var string): (Sign, NumberBase) =
  expectChars(signedDigits)
  scanInt(r.lex, value)

proc parseInt*(r: var TomlReader, T: type SomeInteger): T =
  expectChars(signedDigits)
  r.scanInt(result)

proc parseDateTime*(r: var TomlReader): TomlDateTime =
  expectChars(strutils.Digits, errInvalidDateTime)
  scanDateTime(r.lex, result)

proc parseTime*(r: var TomlReader): TomlTime =
  expectChars(strutils.Digits, errInvalidDateTime)
  scanTime(r.lex, result)

proc parseDate*(r: var TomlReader): TomlDate =
  expectChars(strutils.Digits, errInvalidDateTime)
  scanDate(r.lex, result)

proc parseString*(r: var TomlReader, value: var string): (bool, bool) =
  var next = nonws(r.lex, skipLf)
  if next == '\"':
    eatChar
    let ml = scanString(r.lex, value, StringType.Basic)
    return (ml, false)
  elif next == '\'':
    eatChar
    let ml = scanString(r.lex, value, StringType.Literal)
    return (ml, true)
  else:
    raiseIllegalChar(r.lex, next)

proc parseAsString*(r: var TomlReader): string =
  parseValue(r.lex, result)

proc parseFloat*(r: var TomlReader, value: var string): Sign =
  expectChars(signedDigits)
  scanFloat(r.lex, value)

template deserializeWithNormalizerInToml*(
    T: type[enum], normalizer: static[proc(s :string): string]) =
  proc readValue*(r: var TomlReader, value: var T) {.
      raises: [Defect, IOError, SerializationError].} =
    value = r.parseEnum(T, normalizer)
