# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  tables, typetraits, strutils,
  types, private/utils

proc innerValue(n: TomlValueRef, T: type): T =
  when T is (SomeInteger or SomeFloat):
    case n.kind
    of TomlKind.Int: result = T(n.intVal)
    of TomlKind.Float: result = T(n.floatVal)
    else: discard
  elif T is bool:
    if n.kind == TomlKind.Bool:
      result = n.boolVal
  elif T is string:
    if n.kind == TomlKind.String:
      result = n.stringVal
  elif T is TomlDateTime:
    if n.kind == TomlKind.DateTime:
      result = n.dateTime
  elif T is TomlDate:
    if n.kind == TomlKind.DateTime:
      if n.dateTime.date.isSome:
        result = n.dateDimte.date.get
  elif T is TomlTime:
    if n.kind == TomlKind.DateTime:
      if n.dateTime.time.isSome:
        result = n.dateDimte.time.get
  elif T is enum:
    case n.kind:
    of TomlKind.Int: result = T(n.intVal)
    of TomlKind.String: result = parseEnum[T](n.stringVal)
    else: discard
  elif T is seq:
    case n.kind
    of TomlKind.Array:
      result = n.arrayVal
    of TomlKind.Tables:
      result = n.tablesVal
    of TomlKind.Table, TomlKind.InlineTable:
      for x in values(n.tableVal):
        result.add x
    else: discard
  elif T is array:
    case n.kind
    of TomlKind.Array:
      for i, x in n.arrayVal:
        result[i] = x
    of TomlKind.Tables:
      for i, x in n.tablesVal:
        result[i] = x
    of TomlKind.Table, TomlKind.InlineTable:
      var i = 0
      for x in values(n.tableVal):
        result[i] = x
        inc i
    else: discard
  else:
    const typeName = typetraits.name(T)
    {.error: "Failed to convert to unsupported type: " & typeName & " from TOML".}

proc len*(n: TomlValueRef): int =
  ## If `n` is a `TomlKind.Array`, it returns the number of elements.
  ## If `n` is a `TomlKind.Table`, it returns the number of pairs.
  ## Else it returns 0.
  case n.kind
  of TomlKind.Array: result = n.arrayVal.len
  of TomlKind.Table: result = n.tableVal.len
  else: discard

proc `[]`*(node: TomlValueRef, name: string): TomlValueRef {.inline.} =
  ## Gets a field from a `TomlKind.Table`, which must not be nil.
  ## If the value at `name` does not exist, raises KeyError.
  assert(not isNil(node))
  assert(node.kind == TomlKind.Table)
  node.tableVal[name]

proc `[]`*(node: TomlValueRef, name: string, T: type): T {.inline.} =
  node[name].innerValue(T)

proc `[]`*(node: TomlValueRef, index: int): TomlValueRef {.inline.} =
  ## Gets the node at `index` in an Array. Result is undefined if `index`
  ## is out of bounds, but as long as array bound checks are enabled it will
  ## result in an exception.
  assert(not isNil(node))
  assert(node.kind == TomlKind.Array)
  node.arrayVal[index]

proc `[]`*(node: TomlValueRef, index: int, T: type): T {.inline.} =
  node[index].innerValue(T)

proc hasKey*(node: TomlValueRef, key: string): bool =
  ## Checks if `key` exists in `node`.
  assert(node.kind == TomlKind.Table)
  result = node.tableVal.hasKey(key)

proc contains*(node: TomlValueRef, key: string): bool =
  ## Checks if `key` exists in `node`.
  assert(node.kind == TomlKind.Table)
  node.tableVal.hasKey(key)

proc contains*(node: TomlValueRef, val: TomlValueRef): bool =
  ## Checks if `val` exists in array `node`.
  assert(node.kind == TomlKind.Array)
  find(node.arrayVal, val) >= 0

proc `[]=`*(obj: TomlValueRef, key: string, val: TomlValueRef) {.inline.} =
  ## Sets a field from a `TomlKind.Table`.
  assert(obj.kind == TomlKind.Table)
  obj.tableVal[key] = val

proc `{}`*(node: TomlValueRef, keys: varargs[string]): TomlValueRef =
  ## Traverses the node and gets the given value. If any of the
  ## keys do not exist, returns ``nil``. Also returns ``nil`` if one of the
  ## intermediate data structures is not an object.
  result = node
  for key in keys:
    if isNil(result) or result.kind != TomlKind.Table:
      return nil
    result = result.tableVal.getOrDefault(key)

proc getOrDefault*(node: TomlValueRef, key: string): TomlValueRef =
  ## Gets a field from a `node`. If `node` is nil or not an object or
  ## value at `key` does not exist, returns nil
  if not isNil(node) and node.kind == TomlKind.Table:
    result = node.tableVal.getOrDefault(key)

proc `{}=`*(node: TomlValueRef, keys: varargs[string], value: TomlValueRef) =
  ## Traverses the node and tries to set the value at the given location
  ## to ``value``. If any of the keys are missing, they are added.
  var node = node
  for i in 0..(keys.len-2):
    if not node.hasKey(keys[i]):
      node[keys[i]] = emptyTable()
    node = node[keys[i]]
  node[keys[keys.len-1]] = value

proc delete*(obj: TomlValueRef, key: string) =
  ## Deletes ``obj[key]``.
  assert(obj.kind == TomlKind.Table)
  if not obj.tableVal.hasKey(key):
    raise newException(IndexError, "key not in object")
  obj.tableVal.del(key)

proc copy*(p: TomlValueRef): TomlValueRef

proc copy*(p: TomlTableRef): TomlTableRef =
  result = TomlTableRef.new
  for key, val in pairs(p):
    result[key] = copy(val)

proc copy(p: TomlValueRef): TomlValueRef =
  ## Performs a deep copy of `p`.
  case p.kind
  of TomlKind.String:
    result = TomlValueRef(
      kind: TomlKind.String,
      stringVal: p.stringVal
    )
  of TomlKind.Int:
    result = TomlValueRef(
      kind: TomlKind.Int,
      intVal: p.intVal
    )
  of TomlKind.Float:
    result = TomlValueRef(
      kind: TomlKind.Float,
      floatVal: p.floatVal
    )
  of TomlKind.Bool:
    result = TomlValueRef(
      kind: TomlKind.Bool,
      boolVal: p.boolVal
    )
  of TomlKind.Table, TomlKind.InlineTable:
    result = TomlValueRef(kind: p.kind)
    result.tableVal = copy(p.tableVal)
  of TomlKind.Array:
    result = TomlValueRef(kind: TomlKind.Array)
    for x in items(p.arrayVal):
      result.arrayVal.add copy(x)
  of TomlKind.DateTime:
    result = TomlValueRef(kind: TomlKind.DateTime)
    result.dateTime = p.dateTime
  of TomlKind.Tables:
    result = TomlValueRef(kind: TomlKind.Tables)
    for x in items(p.tablesVal):
      result.tablesVal.add copy(x)
