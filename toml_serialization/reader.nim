# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  tables, strutils, typetraits, options,
  faststreams/inputs, serialization/[object_serialization, errors],
  types, lexer

type
  TomlReader* = object
    lex*: TomlLexer
    allowUnknownFields: bool
    level: int
    flags: TomlFlags

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
           flags: TomlFlags = {},
           allowUnknownFields = false): T =
  result.allowUnknownFields = allowUnknownFields
  result.lex = TomlLexer.init(stream, flags)
  result.flags = flags

proc moveToKey*(r: var TomlReader, key: string, tomlCase: TomlCase) =
  r.lex.parseToml(key, tomlCase)

proc setParsed[T: enum](e: var T, s: string) =
  e = parseEnum[T](s)

proc stringEnum[T: enum](r: var TomlReader, value: var T, s: string) =
  try:
    value.setParsed(s)
  except ValueError:
    const typeName = typetraits.name(T)
    raiseUnexpectedValue(r.lex, typeName)

proc topLevelObject[T](r: var TomlReader, value: var T) =
  mixin readValue

  const totalFields = T.totalSerializedFields
  when  totalFields > 0:
    let fields = T.fieldReadersTable(TomlReader)
    var
      expectedFieldPos = 0
      fieldsDone = 0
      fieldName = newStringOfCap(defaultStringCapacity)
      next: char

    while true:
      fieldName.setLen(0)
      next = nonws(r.lex, skipLf)
      case next
      of '[':
        let bracket = scanTableName(r.lex, fieldName)
        if bracket == BracketType.double:
          raiseTomlErr(r.lex, errDoubleBracket)
        if r.level > 0: break
      of '=': raiseTomlErr(r.lex, errKeyNameMissing)
      of '#', '.', ']':
        raiseIllegalChar(r.lex, next)
      of EOF:
        break
      else:
        # Everything else marks the presence of a key
        push(r.lex, next)
        scanKey(r.lex, fieldName)

        next = nonws(r.lex, skipLf)
        if next != '=':
          raiseExpectChar(r.lex, '=')

      when value is tuple:
        var reader = fields[][expectedFieldPos].reader
        expectedFieldPos += 1
      else:
        var reader = findFieldReader(fields[], fieldName, expectedFieldPos)

      if reader != nil:
        inc r.level
        reader(value, r)
        dec r.level
        inc fieldsDone
      elif r.allowUnknownFields:
        # efficient skip, it doesn't produce any tokens
        var skipValue: TomlVoid
        parseValue(r.lex, skipValue)
      else:
        const typeName = typetraits.name(T)
        raiseUnexpectedField(r.lex, fieldName, typeName)

      if fieldsDone >= totalFields:
        break

proc nestedObject[T](r: var TomlReader, value: var T) =
  mixin readValue

  const totalFields = T.totalSerializedFields
  when  totalFields > 0:
    let fields = T.fieldReadersTable(TomlReader)
    var
      expectedFieldPos = 0
      fieldName = newStringOfCap(defaultStringCapacity)
      firstComma = true

    var next = nonws(r.lex, skipNoLf)
    if next != '{':
      raiseExpectChar(r.lex, '{')

    while true:
      fieldName.setLen(0)
      next = nonws(r.lex, skipNoLf)
      case next
      of '}': break
      of EOF:
        raiseTomlErr(r.lex, errUnterminatedTable)
      of ',':
        if firstComma:
          raiseTomlErr(r.lex, errMissingFirstElement)

        next = nonws(r.lex, skipNoLf)
        if next == '}':
          raiseIllegalChar(r.lex, '}')

        push(r.lex, next)
      of '\n':
        if TomlInlineTableNewline in r.flags:
          continue
        else:
          raiseIllegalChar(r.lex, next)
      else:
        firstComma = false
        push(r.lex, next)
        scanKey(r.lex, fieldName)

        next = nonws(r.lex, skipNoLf)
        if next != '=':
          raiseExpectChar(r.lex, '=')

        when value is tuple:
          var reader = fields[][expectedFieldPos].reader
          expectedFieldPos += 1
        else:
          var reader = findFieldReader(fields[], fieldName, expectedFieldPos)

        if reader != nil:
          inc r.level
          reader(value, r)
          dec r.level
        elif r.allowUnknownFields:
          # efficient skip, it doesn't produce any tokens
          var skipValue: TomlVoid
          parseValue(r.lex, skipValue)
        else:
          const typeName = typetraits.name(T)
          raiseUnexpectedField(r.lex, fieldName, typeName)

template getUnderlyingType*[T](_: Option[T]): untyped = T

proc readValue*[T](r: var TomlReader, value: var T)
                  {.raises: [SerializationError, IOError, Defect].} =
  mixin readValue

  when value is Option:
    var z: getUnderlyingType(value)
    readValue(r, z)
    value = some(z)
  elif value is TomlValueRef:
    # top level differ from the level below
    try:
      if r.level == 0:
        value = parseToml(r.lex)
      else:
        parseValue(r.lex, value)
    except ValueError:
      const typeName = typetraits.name(T)
      raiseUnexpectedValue(r.lex, typeName)

  elif value is string:
    # every value can be deserialized as string
    parseValue(r.lex, value)

  elif value is TomlTime:
    var next = nonws(r.lex, skipLf)
    if next notin strutils.Digits:
      raiseTomlErr(r.lex, errInvalidDateTime)

    push(r.lex, next)
    try:
      scanTime(r.lex, value)
    except ValueError:
      raiseTomlErr(r.lex, errInvalidDateTime)

  elif value is TomlDate:
    var next = nonws(r.lex, skipLf)
    if next notin strutils.Digits:
      raiseTomlErr(r.lex, errInvalidDateTime)

    push(r.lex, next)
    scanDate(r.lex, value)

  elif value is TomlDateTime:
    var next = nonws(r.lex, skipLf)
    if next notin strutils.Digits:
      raiseTomlErr(r.lex, errInvalidDateTime)
    push(r.lex, next)
    try:
      scanDateTime(r.lex, value)
    except ValueError:
      raiseTomlErr(r.lex, errInvalidDateTime)

  elif value is SomeInteger:
    var next = nonws(r.lex, skipLf)
    if next notin strutils.Digits + {'+', '-'}:
      raiseIllegalChar(r.lex, next)

    push(r.lex, next)
    var xValue: uint64
    let (sign, _) = scanInt(r.lex, xValue)
    if sign == Neg:
      when value is SomeUnsignedInt:
        raiseTomlErr(r.lex, errNegateUint)
      else:
        try:
          value = -T(xValue)
        except OverflowError:
          raiseTomlErr(r.lex, errIntegerOverflow)
    else:
      value = T(xValue)

  elif value is SomeFloat:
    var next = nonws(r.lex, skipLf)

    if next notin strutils.Digits + {'+', '-'}:
      raiseIllegalChar(r.lex, next)

    push(r.lex, next)
    discard scanFloat(r.lex, value)

  elif value is bool:
    value = scanBool(r.lex)

  elif value is enum:
    var next = nonws(r.lex, skipLf)
    case next
    of '\"':
      var enumStr: string
      if scanString(r.lex, enumStr, StringType.Basic):
        raiseTomlErr(r.lex, errMLStringName)
      r.stringEnum(value, enumStr)
    of '\'':
      var enumStr: string
      if scanString(r.lex, enumStr, StringType.Literal):
        raiseTomlErr(r.lex, errMLStringName)
      r.stringEnum(value, enumStr)
    of strutils.Digits + {'+', '-'}:
      var enumInt: uint64
      push(r.lex, next)
      let (sign, _) = scanInt(r.lex, enumInt)
      try:
        if sign == Neg:
          value = T(-(enumInt.int))
        else:
          value = T(enumInt)
      except OverflowError:
        raiseTomlErr(r.lex, errIntegerOverflow)
    else: raiseIllegalChar(r.lex, next)

  elif value is seq:
    var next = nonws(r.lex, skipLf)
    if next != '[':
      raiseIllegalChar(r.lex, next)

    while true:
      next = nonws(r.lex, skipLf)
      case next
      of ']': break
      of EOF:
        raiseTomlErr(r.lex, errUnterminatedArray)
      of ',':
        if value.len == 0:
          # This happens with "[, 1, 2]", for instance
          raiseTomlErr(r.lex, errMissingFirstElement)

        # Check that this is not a terminating comma (like in
        #  "[b,]")
        next = nonws(r.lex, skipLf)
        if next == ']':
          break

        push(r.lex, next)
      else:
        push(r.lex, next)
        let lastPos = value.len
        value.setLen(lastPos + 1)
        readValue(r, value[lastPos])

  elif value is array:
    var next = nonws(r.lex, skipLf)
    if next != '[':
      raiseIllegalChar(r.lex, next)

    for i in low(value) ..< high(value):
      # TODO: don't ask. this makes the code compile
      if false: value[i] = value[i]
      readValue(r, value[i])
      next = nonws(r.lex, skipLf)
      if next != ',':
        raiseIllegalChar(r.lex, next)

    readValue(r, value[high(value)])
    next = nonws(r.lex, skipLf)
    if next != ']':
      raiseTomlErr(r.lex, errUnterminatedArray)

  elif value is (object or tuple):
    if r.level <= 1:
      var next = nonws(r.lex, skipLf)
      if next == '{':
        push(r.lex, next)
        r.nestedObject(value)
      else:
        push(r.lex, next)
        r.topLevelObject(value)
    else:
      r.nestedObject(value)

  else:
    const typeName = typetraits.name(T)
    {.error: "Failed to convert to TOML an unsupported type: " & typeName.}

# these are builtin functions

proc parseNumber*(r: var TomlReader, value: var string): (Sign, NumberBase) =
  var next = nonws(r.lex, skipLf)
  if next notin strutils.Digits + {'+', '-'}:
    raiseIllegalChar(r.lex, next)

  push(r.lex, next)
  scanInt(r.lex, value)

proc parseDateTime*(r: var TomlReader): TomlDateTime =
  var next = nonws(r.lex, skipLf)
  if next notin strutils.Digits:
    raiseTomlErr(r.lex, errInvalidDateTime)
  push(r.lex, next)

  try:
    scanDateTime(r.lex, result)
  except ValueError:
    raiseTomlErr(r.lex, errInvalidDateTime)

proc parseString*(r: var TomlReader, value: var string): (bool, bool) =
  var next = nonws(r.lex, skipLf)
  if next == '\"':
    let ml = scanString(r.lex, value, StringType.Basic)
    return (ml, false)
  elif next == '\'':
    let ml = scanString(r.lex, value, StringType.Literal)
    return (ml, true)
  else:
    raiseIllegalChar(r.lex, next)

proc parseAsString*(r: var TomlReader): string =
  parseValue(r.lex, result)

proc parseFloat*(r: var TomlReader, value: var string): Sign =
  var next = nonws(r.lex, skipLf)
  if next notin strutils.Digits + {'+', '-'}:
    raiseIllegalChar(r.lex, next)

  push(r.lex, next)
  scanFloat(r.lex, value)

proc parseTime*(r: var TomlReader): TomlTime =
  var next = nonws(r.lex, skipLf)
  if next notin strutils.Digits:
    raiseTomlErr(r.lex, errInvalidDateTime)

  push(r.lex, next)
  try:
    scanTime(r.lex, result)
  except ValueError:
    raiseTomlErr(r.lex, errInvalidDateTime)

proc parseDate*(r: var TomlReader): TomlDate =
  var next = nonws(r.lex, skipLf)
  if next notin strutils.Digits:
    raiseTomlErr(r.lex, errInvalidDateTime)

  push(r.lex, next)
  scanDate(r.lex, result)
