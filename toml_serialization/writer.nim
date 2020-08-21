# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  typetraits, options, strutils, tables, unicode,
  faststreams/[outputs, textio], serialization,
  types, private/utils

type
  TomlWriter* = object
    stream*: OutputStream
    level: int
    flags: TomlFlags
    hasPrettyOutput*: bool
    state: CodecState

proc init*(T: type TomlWriter,
           stream: OutputStream,
           flags: TomlFlags = {}): T =
  result.stream = stream
  result.flags = flags
  result.state = TopLevel

template append(x: untyped) =
  write w.stream, x

template indent =
  for i in 0 ..< w.level:
    append ' '

proc writeIterable*(w: var TomlWriter, collection: auto) =
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

proc writeArray*[T](w: var TomlWriter, elements: openarray[T]) =
  writeIterable(w, elements)

proc writeValue*(w: var TomlWriter, time: TomlTime) =
  # TODO avoid alloc/using intToStr
  append intToStr(time.hour, 2)
  append ':'
  append intToStr(time.minute, 2)
  append ':'
  append intToStr(time.second, 2)
  if time.subsecond > 0:
    append '.'
    append $time.subsecond

proc writeValue*(w: var TomlWriter, date: TomlDate) =
  # TODO avoid alloc/using intToStr
  append intToStr(date.year, 4)
  append '-'
  append intToStr(date.month, 2)
  append '-'
  append intToStr(date.day, 2)

proc writeValue*(w: var TomlWriter, x: TomlDateTime) =
  # TODO avoid alloc/using intToStr
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
    append intToStr(zone.hourShift, 2)
    append ':'
    append intToStr(zone.minuteShift, 2)

proc writeValue*(w: var TomlWriter, s: string) =
  # TODO avoid alloc/using toHex
  const
    lowEscape = {'\0'..'\31'} - {'\b', '\n', '\t', '\f', '\r'}
    highEscape = {'\127'..'\255'}

  append '\"'
  for c in runes(s):
    if c.int < 255:
      case c.char
      of lowEscape, highEscape:
        append "\\u"
        append toHex(c.int, 4)
      of '\b': append "\\b"
      of '\t': append "\\t"
      of '\n': append "\\n"
      of '\f': append "\\f"
      of '\r': append "\\r"
      of '\'': append "\\\'"
      of '\"': append "\\\""
      of '\\': append "\\\\"
      else: append c.char
    else:
      if c.int < 0xFFFF:
        append "\\u"
        append toHex(c.int, 4)
      else:
        append "\\U"
        append toHex(c.int, 8)
  append '\"'

proc writeKey(w: var TomlWriter, s: string) =
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

proc writeKey(w: var TomlWriter, s: openArray[string]) =
  for i, k in s:
    writeKey(w, k)
    if i < s.high:
      append '.'

proc writeKey(emptyTable: var seq[string], s: openArray[string]) =
  var o = memoryOutput()
  var w = TomlWriter.init(o)
  append '['
  writeKey(w, s)
  append "]\n"
  emptyTable.add o.getOutput(string)

proc writeToml(w: var TomlWriter,
               value: TomlValueRef,
               keyList: var seq[string],
               emptyTable: var seq[string],
               noKey: bool = false)

proc writeInlineTable(w: var TomlWriter,
                      value: TomlValueRef,
                      keyList: var seq[string],
                      emptyTable: var seq[string],
                      noKey: bool) =
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
               noKey: bool = false) =
  case value.kind
  of TomlKind.Int:
    writeKeyValue:
      w.stream.writeText value.intVal
  of TomlKind.Float:
    writeKeyValue:
      append $value.floatVal
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
        var newkeyList = @[k]
        writeToml(w, v, newKeyList, emptyTable, false)

      for k, v in x:
        if v.kind != TomlKind.Tables:
          continue
        keyList.add k
        writeToml(w, v, keyList, emptyTable, true)
        discard keyList.pop

      dec w.level

  of TomlKind.InlineTable:
    if w.level == 1:
      writeKey(w, keyList)
      append '='
    writeInlineTable(w, value, keyList, emptyTable, true)
    if w.level == 1:
      append'\n'

  of TomlKind.Table:
    if value.tableVal.len == 0 and keyList.len > 0:
      # empty table
      writeKey(emptyTable, keyList)

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

proc writeFieldName(w: var TomlWriter, s: string) =
  w.writeKey s
  append " = "

proc writeValue*(w: var TomlWriter, value: auto) =
  mixin enumInstanceSerializedFields, writeValue, writeFieldIMPL

  when value is TomlValueRef:
    doAssert(value.kind == TomlKind.Table)
    var keyList = newSeqofCap[string](5)
    var emptyTable = newSeqofCap[string](5)
    writeToml(w, value, keyList, emptyTable)
    for k in emptyTable:
      append k

  elif value is bool:
    append if value: "true" else: "false"

  elif value is enum:
    w.stream.writeText ord(value)

  elif value is range:
    when low(value) < 0:
      w.stream.writeText int64(value)
    else:
      w.stream.writeText uint64(value)

  elif value is SomeInteger:
    w.stream.writeText value

  elif value is SomeFloat:
    # TODO Implement writeText for floats
    #      to avoid the allocation here:
    append $value

  elif value is (seq or array or openArray):
    w.writeArray(value)

  elif value is (object or tuple):
    let prevState = w.state
    type RecordType = type value
    if w.state == ExpectValue:
      append '}'
    value.enumInstanceSerializedFields(fieldName, field):
      type FieldType = type field
      case w.state
      of TopLevel:
        when FieldType is (object or tuple) and FieldType isnot (TomlSpecial):
          append '['
          append fieldName
          append ']'
          append '\n'
          w.state = InsideRecord
        else:
          w.writeFieldName(fieldName)
          w.state = ExpectValue

        w.writeFieldIMPL(FieldTag[RecordType, fieldName, FieldType], field, value)
        w.state = prevState

        when FieldType isnot (object or tuple) or FieldType is (TomlSpecial):
          append '\n'

      of ExpectValue:
        w.writeFieldName(fieldName)
        w.writeFieldIMPL(FieldTag[RecordType, fieldName, FieldType], field, value)

      of InsideRecord:
        w.writeFieldName(fieldName)
        w.writeFieldIMPL(FieldTag[RecordType, fieldName, FieldType], field, value)
        append '\n'

    if w.state == ExpectValue:
      append '}'
      append '\n'
    elif w.state == InsideRecord:
      append '\n'

  else:
    const typeName = typetraits.name(value.type)
    {.fatal: "Failed to convert to TOML an unsupported type: " & typeName.}
