# toml-serialization
# Copyright (c) 2026 Status Research & Development GmbH
# Licensed under either of
#  * Apache License, version 2.0, ([LICENSE-APACHE](LICENSE-APACHE))
#  * MIT license ([LICENSE-MIT](LICENSE-MIT))
# at your option.
# This file may not be copied, modified, or distributed except according to
# those terms.

{.push raises: [], gcsafe.}

import
  std/[json, tables],
  ../toml_serialization

type
  TomlFlavor = Toml_v100

proc jsonTag(tomlType: string, tomlValue: string): JsonNode =
  result = %{
    "type": %tomlType,
    "value": %tomlValue
  }

proc encodeDT(toml: TomlDateTime): string =
  type
    WR = TomlWriter[TomlFlavor]

  try:
    var
      o = memoryOutput()
      w = WR.init(o)
    w.writeValue(toml)
    return o.getOutput(string)
  except IOError:
    discard

func dtType(toml: TomlDateTime): string =
  if toml.date.isSome and toml.time.isSome and toml.zone.isNone:
    return "datetime-local"

  if toml.date.isSome and toml.time.isNone and toml.zone.isNone:
    return "date-local"

  if toml.date.isNone and toml.time.isSome and toml.zone.isNone:
    return "time-local"

  "datetime"

proc toJson(p: TomlValueRef, depth: int = 0): JsonNode

proc toJson(p: TomlTableRef, depth: int = 0): JsonNode =
  result = newJObject()
  for key, val in p:
    result[key] = val.toJson(depth + 1)

proc toJson(p: TomlValueRef, depth: int = 0): JsonNode =
  case p.kind
  of TomlKind.String:
    result = jsonTag("string", p.stringVal)
  of TomlKind.Int:
    result = jsonTag("integer", $p.intVal)
  of TomlKind.Float:
    result = jsonTag("float", $p.floatVal)
  of TomlKind.Bool:
    result = jsonTag("bool", $p.boolVal)
  of TomlKind.Table, TomlKind.InlineTable:
    result = p.tableVal.toJson(depth + 1)
  of TomlKind.Array:
    result = newJArray()
    for child in p.arrayVal:
      result.add toJson(child, depth + 1)
  of TomlKind.DateTime:
    result = jsonTag(dtType(p.dateTime), encodeDT(p.dateTime))
  of TomlKind.Tables:
    result = newJArray()
    for child in p.tablesVal:
      result.add toJson(child, depth + 1)

proc main() {.raises: {IOError}.} =
  setStdIoUnbuffered()

  try:
    let
      tomlData = stdin.readAll()
      res = TomlFlavor.decode(tomlData, TomlValueRef, flags = {TomlStrictComma})
      node = res.toJson()
    stdout.write(node.pretty())
  except SerializationError as exc:
    stderr.write("Serialization error: " & exc.msg)
    quit(1)
  except IOError as exc:
    stderr.write("IO error: " & exc.msg)
    quit(1)

main()
