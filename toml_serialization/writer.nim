# toml-serialization
# Copyright (c) 2020-2026 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

{.push raises: [], gcsafe.}

import
  std/[typetraits, options, strutils, tables, unicode],
  faststreams/[outputs, textio],
  serialization,
  ./private/utils,
  ./types,
  ./desc,
  ./format

export
  desc, format

type
  TomlWriter*[Flavor = Toml] = object
    stream*: OutputStream
    level: int
    flags: TomlFlags
    hasPrettyOutput*: bool
    state: CodecState

proc init*(T: type TomlWriter,
           stream: OutputStream,
           flags: TomlFlags = {}): T =
  mixin flavorRuntimeFlags

  type
    Flavor = T.Flavor

  T(stream: stream,
    flags: flags + flavorRuntimeFlags(Flavor),
    state: TopLevel)

template append(x: untyped) =
  write w.stream, x

template append(x: BiggestInt, len: Positive) =
  writeInt w.stream, x, len

template indent =
  for i in 0 ..< w.level:
    append ' '

proc writeIterable*(w: var TomlWriter, collection: auto) {.raises: [IOError].} =
  mixin writeValue

  append '['

  if w.hasPrettyOutput:
    append '\n'
    w.level += 2
    indent()

  var first = true
  for e in collection:
    if not first:
      append ", "
      if w.hasPrettyOutput:
        append '\n'
        indent()

    w.writeValue(e)
    first = false

  if w.hasPrettyOutput:
    append '\n'
    w.level -= 2
    indent()

  append ']'

template writeArray*[T](w: var TomlWriter, elements: openArray[T]) =
  writeIterable(w, elements)

proc writeValue*(w: var TomlWriter, time: TomlTime) {.raises: [IOError].} =
  append(time.hour, 2)
  append ':'
  append(time.minute, 2)

  if TomlHourMinute in w.flags and time.second == 0 and time.subsecond == 0:
    return

  append ':'
  append(time.second, 2)
  if time.subsecond > 0:
    append '.'
    w.stream.writeText time.subsecond

proc writeValue*(w: var TomlWriter, date: TomlDate) {.raises: [IOError].} =
  append(date.year, 4)
  append '-'
  append(date.month, 2)
  append '-'
  append(date.day, 2)

proc writeValue*(w: var TomlWriter, x: TomlDateTime) {.raises: [IOError].} =
  if x.date.isSome:
    let date = x.date.get()
    writeValue(w, date)

  if x.time.isSome:
    let time = x.time.get()
    if x.date.isSome:
      append 'T'
    writeValue(w, time)

  if x.zone.isSome:
    let zone = x.zone.get()
    if zone.hourShift == 0 and
      zone.minuteShift == 0 and
      zone.positiveShift:
      append 'Z'
      return

    if zone.positiveShift:
      append '+'
    else:
      append '-'
    append(zone.hourShift, 2)
    append ':'
    append(zone.minuteShift, 2)

proc writeValue*(w: var TomlWriter, s: string) {.raises: [IOError].} =
  const
    lowEscape = {'\0'..'\31'} - {'\b', '\e', '\f', '\n', '\r', '\t'}
    highEscape = {'\127'..'\255'}

  append '\"'
  for c in runes(s):
    if c.int <= 255:
      case c.char
      of lowEscape, highEscape:
        if TomlHexEscape in w.flags:
          append "\\x"
          w.stream.toHex(c.int, 2)
        else:
          append "\\u"
          w.stream.toHex(c.int, 4)
      of '\b': append "\\b"
      of '\e': append "\\e"
      of '\f': append "\\f"
      of '\n': append "\\n"
      of '\r': append "\\r"
      of '\t': append "\\t"
      of '\'': append "\\\'"
      of '\"': append "\\\""
      of '\\': append "\\\\"
      else: append c.char
    else:
      if c.int < 0xFFFF:
        append "\\u"
        w.stream.toHex(c.int, 4)
      else:
        append "\\U"
        w.stream.toHex(c.int, 8)
  append '\"'

proc writeKey(w: var TomlWriter, s: string) {.raises: [IOError].}  =
  const
    shouldEscape = {'\0'..'\32', '.', '\"', '\'', '#', '\127'..'\255'}
  for c in runes(s):
    if c.int < 255:
      if c.char in shouldEscape:
        writeValue(w, s)
        return
    else:
      writeValue(w, s)
      return
  if s.len == 0:
    append "\"\""
  else:
    append s

proc writeKey(w: var TomlWriter, s: openArray[string]) {.raises: [IOError].} =
  for i, k in s:
    writeKey(w, k)
    if i < s.high:
      append '.'

proc writeKey(T: type TomlWriter,
              emptyTable: var seq[string],
              s: openArray[string]) {.raises: [IOError].} =
  var o = memoryOutput()
  var w = T.init(o)
  append '['
  writeKey(w, s)
  append "]\n"
  emptyTable.add o.getOutput(string)

proc writeToml(w: var TomlWriter,
               value: TomlValueRef,
               keyList: var seq[string],
               emptyTable: var seq[string],
               noKey: bool = false) {.raises: [IOError].}

proc writeInlineTable(w: var TomlWriter,
                      value: TomlValueRef,
                      keyList: var seq[string],
                      emptyTable: var seq[string],
                      noKey: bool) {.raises: [IOError].} =
  append '{'
  inc w.level
  let len = value.tableVal.len - 1
  var i = 0
  inc w.level
  for k, v in value.tableVal:
    writeKey(w, k)
    append " = "
    writeToml(w, v, keyList, emptyTable, noKey)
    if i < len:
      append ','
    inc i
  dec w.level
  append '}'
  dec w.level

template writeKeyValue(body: untyped) =
  if noKey:
    body
  else:
    indent()
    indent()
    writeKey(w, keyList)
    append '='
    body
    append '\n'

proc writeToml(w: var TomlWriter, value:
               TomlValueRef,
               keyList: var seq[string],
               emptyTable: var seq[string],
               noKey: bool = false) {.raises: [IOError].} =
  case value.kind
  of TomlKind.Int:
    writeKeyValue:
      w.stream.writeText value.intVal
  of TomlKind.Float:
    writeKeyValue:
      w.stream.writeText value.floatVal
  of TomlKind.Bool:
    writeKeyValue:
      append if value.boolVal: "true" else: "false"
  of TomlKind.DateTime:
    writeKeyValue:
      writeValue(w, value.dateTime)
  of TomlKind.String:
    writeKeyValue:
      writeValue(w, value.stringVal)
  of TomlKind.Array:
    writeKeyValue:
      append '['
      inc w.level
      for i, x in value.arrayVal:
        writeToml(w, x, keyList, emptyTable, true)
        if i < value.arrayVal.high:
          append ','
      dec w.level
      append ']'
  of TomlKind.Tables:
    for x in value.tablesVal:
      append "[["
      writeKey(w, keyList)
      append "]]\n"

      inc w.level
      # non array member come first to
      # prevent key collision
      for k, v in x:
        if v.kind == TomlKind.Tables:
          continue
        var newKeyList = @[k]
        writeToml(w, v, newKeyList, emptyTable, false)

      for k, v in x:
        if v.kind != TomlKind.Tables:
          continue
        keyList.add k
        writeToml(w, v, keyList, emptyTable, true)
        discard keyList.pop

      dec w.level

  of TomlKind.InlineTable:
    if w.level == 1 and w.state != ExpectValue:
      writeKey(w, keyList)
      append '='
    writeInlineTable(w, value, keyList, emptyTable, true)
    if w.level == 1 and w.state != ExpectValue:
      append'\n'

  of TomlKind.Table:
    if value.tableVal.len == 0 and keyList.len > 0:
      # empty table
      writeKey(typeof(w), emptyTable, keyList)

    for k, v in value.tableVal:
      if v.kind in {TomlKind.Table, TomlKind.Tables}:
        continue

      inc w.level
      keyList.add k
      writeToml(w, v, keyList, emptyTable, noKey)
      dec w.level
      discard keyList.pop

    for k, v in value.tableVal:
      if v.kind == TomlKind.Table:
        keyList.add k
        writeToml(w, v, keyList, emptyTable, noKey)
        discard keyList.pop

    for k, v in value.tableVal:
      if v.kind == TomlKind.Tables:
        keyList.add k
        writeToml(w, v, keyList, emptyTable, noKey)
        discard keyList.pop

proc writeFieldName(w: var TomlWriter, s: string) {.raises: [IOError].} =
  w.writeKey s
  append " = "

template writeArrayOfTable*[T](w: var TomlWriter, fieldName: string, list: openArray[T]) =
  mixin writeValue

  w.state = InsideRecord
  for val in list:
    append "[["
    append fieldName
    append "]]"
    append '\n'

    inc w.level
    w.writeValue(val)
    dec w.level
  w.state = prevState

template shouldWriteField*[FieldType](_: type Toml, field: FieldType): bool =
  ## Template to determine if a field should be written.
  true

proc writeRecordValue*[T](w: var TomlWriter, value: T) {.raises: [IOError].} =
  let prevState = w.state
  var firstField = true
  type RecordType = type value

  if w.state == ExpectValue:
    append '{'
    if TomlInlineTableNewline in w.flags:
      append '\n'

  value.enumInstanceSerializedFields(fieldName, field):
    type FieldType = type field

    template regularFieldWriter() {.used.} =
      inc w.level
      w.writeFieldIMPL(FieldTag[RecordType, fieldName], field, value)
      dec w.level
      w.state = prevState

      when FieldType isnot (object or tuple) or FieldType is TomlSpecial or isOptional(Toml, FieldType):
        append '\n'

    when FieldType isnot TomlVoid:
      case w.state
      of TopLevel:
        when FieldType is (object or tuple) and FieldType isnot TomlSpecial and not isOptional(Toml, FieldType):
          append '['
          append fieldName
          append ']'
          append '\n'
          w.state = InsideRecord
          regularFieldWriter()
        elif (FieldType is (seq or array)) and (FieldType isnot (TomlSpecial)) and isRecord(Toml, FieldType):
          writeArrayOfTable(w, fieldName, field)
        else:
          if shouldWriteField(Toml, field):
            w.writeFieldName(fieldName)
            w.state = ExpectValue
            regularFieldWriter()

      of ExpectValue:
        if not firstField:
          append ','
          if TomlInlineTableNewline in w.flags:
            append'\n'
          else:
            append ' '

        if TomlInlineTableNewline in w.flags:
          indent()
          indent()

        w.writeFieldName(fieldName)
        inc w.level
        w.writeFieldIMPL(FieldTag[RecordType, fieldName], field, value)
        dec w.level
        firstField = false

      of InsideRecord:
        indent()
        indent()
        w.state = ExpectValue
        w.writeFieldName(fieldName)
        inc w.level
        w.writeFieldIMPL(FieldTag[RecordType, fieldName], field, value)
        dec w.level
        w.state = prevState
        append '\n'

      else:
        discard

  if w.state == ExpectValue:
    if TomlInlineTableNewline in w.flags:
      append'\n'
      indent()
    append '}'
  elif w.state == InsideRecord:
    append '\n'

template writeValueObjectOrTuple(Flavor, w, value) =
  mixin flavorUsesAutomaticObjectSerialization

  const isAutomatic =
    flavorUsesAutomaticObjectSerialization(Flavor)

  when not isAutomatic:
    const typeName = typetraits.name(type value)
    {.error: "Please override writeValue for the " & typeName &
      " type (or import the module where the override is provided)".}

  writeRecordValue(w, value)

template checkAutoSerialization(Flavor: type, TypeClass: distinct type, value: typed, body: typed) =
  mixin flavorUsesAutomaticPrimitivesSerialization, flavorAutoSerializationWrite

  const
    autoSer = flavorUsesAutomaticPrimitivesSerialization(Flavor) or
              flavorAutoSerializationWrite(Flavor, TypeClass) or
              flavorAutoSerializationWrite(Flavor, typeof(value))

  when not autoSer:
    const
      typeName = typetraits.name(T)
      flavorName = typetraits.name(Flavor)
    {.error: flavorName & ": Please enable automatic serialization to write: " & typeName.}
  else:
    body

proc writeValue*(w: var TomlWriter, value: auto) {.raises: [IOError].} =
  mixin enumInstanceSerializedFields, writeValue, writeFieldIMPL, shouldWriteField

  type
    Flavor = TomlWriter.Flavor

  when value is TomlValueRef:
    checkAutoSerialization(Flavor, TomlValueRef, value):
      #doAssert(value.kind in {TomlKind.Table, TomlKind.InlineTable})
      var keyList = newSeqOfCap[string](5)
      var emptyTable = newSeqOfCap[string](5)
      writeToml(w, value, keyList, emptyTable)
      for k in emptyTable:
        append k

  elif value is Option:
    checkAutoSerialization(Flavor, typeof(value), value):
      if value.isSome:
        w.writeValue value.get

  elif value is bool:
    checkAutoSerialization(Flavor, bool, value):
      append if value: "true" else: "false"

  elif value is enum:
    checkAutoSerialization(Flavor, typeof(value), value):
      w.writeValue $value

  elif value is range:
    checkAutoSerialization(Flavor, typeof(value), value):
      type TVAL = type value
      when low(TVAL) < 0:
        w.stream.writeText int64(value)
      else:
        w.stream.writeText uint64(value)

  elif value is SomeInteger:
    checkAutoSerialization(Flavor, SomeInteger, value):
      w.stream.writeText value

  elif value is SomeFloat:
    checkAutoSerialization(Flavor, SomeFloat, value):
      w.stream.writeText value

  elif value is seq:
    checkAutoSerialization(Flavor, typeof(value), value):
      w.writeArray(value)

  elif value is (array or openArray):
    checkAutoSerialization(Flavor, typeof(value), value):
      w.writeArray(value)

  elif value is (object or tuple):
    writeValueObjectOrTuple(Flavor, w, value)

  else:
    const
      typeName = typetraits.name(value.type)
      flavorName = typetraits.name(Flavor)
    {.fatal: flavorName & ": Failed to convert to TOML an unsupported type: " & typeName.}
