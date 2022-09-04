# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  tables, options, math,
  serialization/errors,
  faststreams/inputs

export
  errors

const
  defaultStringCapacity* = 256
  tomlSubsecondPrecision* {.intdefine.} = 6
  tomlOrderedTable* {.booldefine.} = false

template TableForToml(A, B: type): type =
  when tomlOrderedTable:
    OrderedTable[A, B]
  else:
    Table[A, B]

type
  TomlError* = object of SerializationError

  Sign* {.pure.} = enum None, Pos, Neg

  NumberBase* = enum
    base10, base16, base8, base2

  StringType* {.pure.} = enum
    Basic   # Enclosed within double quotation marks
    Literal # Enclosed within single quotation marks

  TomlVoid* = object

  TomlCase* = enum
    TomlCaseSensitive
    TomlCaseInsensitive
    TomlCaseNim

  TomlFlag* = enum
    TomlInlineTableNewline
    TomlInlineTableTrailingComma
    TomlHexEscape     # allow \xHH escape sequence
    TomlHourMinute    # allow HH:MM format
    TomlUnknownFields # allow unknow fields

  TomlFlags* = set[TomlFlag]

  TomlKind* {.pure.} = enum
    Int,
    Float,
    Bool,
    DateTime,
    String,
    Array,
    Tables, # Array of Tables
    Table,
    InlineTable

  TomlDate* = object
    year*: int
    month*: int
    day*: int

  TomlTime* = object
    hour*: int
    minute*: int
    second*: int
    subsecond*: int

  TomlTimeZone* = object
    positiveShift*: bool
    hourShift*: int
    minuteShift*: int

  # it can be:
  # - time
  # - date
  # - date time
  # - date time zone
  TomlDateTime* = object
    date*: Option[TomlDate]
    time*: Option[TomlTime]
    zone*: Option[TomlTimeZone]

  TomlTable* = TableForToml(string, TomlValueRef)
  TomlTableRef* = ref TomlTable

  TomlValueRef* = ref TomlValue
  TomlValue* = object
    case kind*: TomlKind
    of TomlKind.Int:
      intVal*: int64
    of TomlKind.Float:
      floatVal*: float64
    of TomlKind.Bool:
      boolVal*: bool
    of TomlKind.DateTime:
      dateTime*: TomlDateTime
    of TomlKind.String:
      stringVal*: string
    # Array: immutable
    of TomlKind.Array:
      arrayVal*: seq[TomlValueRef]
    # Array of Tables: mutable
    of TomlKind.Tables:
      tablesVal*: seq[TomlTableRef]
    # Table: mutable
    # Inline Table: immutable
    of TomlKind.Table, TomlKind.InlineTable:
      tableVal*: TomlTableRef

when tomlOrderedTable:
  template withValue*(x: TomlTable, key: string,
                      value, body1, body2: untyped) =
    var valx = x.getOrDefault(key, nil)
    if valx.isNil:
      body2
    else:
      let value {.inject.} = addr(valx)
      body1

proc `==`*(a, b: TomlValueRef): bool {.noSideEffect.}

proc `==`*(a, b: TomlTableRef): bool {.noSideEffect.} =
  result = true
  if a.len != b.len:
    return false

  when tomlOrderedTable and (NimMajor,NimMinor,NimPatch) < (1,6,0):
    # https://github.com/nim-lang/Nim/issues/15750
    # workaround for Nim 1.2.0 and 1.4.0 or less
    if a.len == 0 and b.len == 0:
      return true

  result = a[] == b[]

proc `==`*(a, b: TomlValueRef): bool =
  const
    tableKind = {TomlKind.Table, TomlKind.InlineTable}

  if a.isNil:
    if b.isNil: return true
    return false

  if b.isNil or a.kind != b.kind:
    if not(a.kind in tableKind and b.kind in tableKind):
      return false

  case a.kind:
  of TomlKind.Int:
    result = a.intVal == b.intVal
  of TomlKind.Float:
    let fc = classify(a.floatVal)
    if fc != fcNormal:
      result = classify(b.floatVal) == fc
    else:
      result = (abs(a.floatVal - b.floatVal) < 1E-7)
  of TomlKind.Bool:
    result = a.boolVal == b.boolVal
  of TomlKind.DateTime:
    result = a.dateTime == b.dateTime
  of TomlKind.String:
    result = a.stringVal == b.stringVal
  of TomlKind.Array:
    result = a.arrayVal == b.arrayVal
  of TomlKind.Tables:
    if a.tablesVal.len != b.tablesVal.len:
      return false
    for i, val in a.tablesVal:
      if b.tablesVal[i] != val:
        return false
    return true
  of TomlKind.Table, TomlKind.InlineTable:
    result = a.tableVal == b.tableVal

method formatMsg*(err: ref TomlError, filename: string): string
                 {.gcsafe, raises: [Defect].} =
  filename & err.msg

proc `$`*(p: TomlDateTime): string =
  if p.date.isSome: result.add $p.date.get()
  if p.time.isSome: result.add $p.time.get()
  if p.zone.isSome: result.add $p.zone.get()

proc toUgly*(result: var string, p: TomlValueRef)

proc toUgly*(result: var string, p: TomlTableRef) =
  var comma = false
  result.add "{"
  for key, val in p:
    if comma: result.add ","
    else: comma = true
    result.add key
    result.add ":"
    result.toUgly val
  result.add "}"

proc toUgly(result: var string, p: TomlValueRef) =
  var comma = false
  case p.kind
  of TomlKind.String:
    result.add p.stringVal
  of TomlKind.Int:
    result.add $p.intVal
  of TomlKind.Float:
    result.add $p.floatVal
  of TomlKind.Bool:
    result.add $p.boolVal
  of TomlKind.Table, TomlKind.InlineTable:
    result.toUgly p.tableVal
  of TomlKind.Array:
    result.add "["
    for child in p.arrayVal:
      if comma: result.add ","
      else: comma = true
      result.toUgly child
    result.add "]"
  of TomlKind.DateTime:
    result.add $p.dateTime
  of TomlKind.Tables:
    result.add "[["
    for child in p.tablesVal:
      if comma: result.add ","
      else: comma = true
      result.toUgly child
    result.add "]]"

proc `$`*(p: TomlValueRef): string =
  toUgly(result, p)

type
  VMInputStream* = ref object of InputStream
    pos*: int
    data*: string

proc read*(s: VMInputStream): char =
  result = s.data[s.pos]
  inc s.pos

proc readable*(s: VMInputStream): bool =
  s.pos < s.data.len

proc peekChar*(s: VMInputStream): char =
  s.data[s.pos]
