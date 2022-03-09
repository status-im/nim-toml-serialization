# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  tables, options, math,
  serialization/errors

export
  errors

const
  defaultStringCapacity* = 256
  subsecondPrecision* {.intdefine.} = 6

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

  TomlTable* = Table[string, TomlValueRef]
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

proc `==`*(a, b: TomlValueRef): bool

proc `==`*(a, b: TomlTableRef): bool =
  result = true
  if a.len != b.len:
    return false
  for key, val in a:
    b[].withValue(key, node) do:
      result = node[] == val
    do:
      result = false

proc `==`(a, b: TomlValueRef): bool =
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
