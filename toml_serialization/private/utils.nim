# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  strutils,
  ../types,
  serialization/object_serialization,
  faststreams/outputs

type
  CodecState* = enum
    TopLevel
    InsideRecord
    ExpectValue
    ArrayOfTable

func cmpNimIdent(a, b: string): bool =
  if a.len == 0 and b.len == 0:
    return true
  if a.len == 0 or b.len == 0:
    return false
  if a[0] != b[0]:
    return false

  var i = 1
  var j = 1
  while true:
    while i < a.len and a[i] == '_': inc i
    while j < b.len and b[j] == '_': inc j
    var aa = if i < a.len: toLowerAscii(a[i]) else: '\0'
    var bb = if j < b.len: toLowerAscii(b[j]) else: '\0'
    let res = ord(aa) - ord(bb)
    if res != 0: return false
    # the characters are identical:
    if i >= a.len:
      # both cursors at the end:
      if j >= b.len: return true
      # not yet at the end of 'b':
      return false
    elif j >= b.len:
      return true
    inc i
    inc j

func compare*(a, b: openArray[string], tomlCase: TomlCase): bool =
  case tomlCase
  of TomlCaseSensitive:
    result = a == b
  of TomlCaseInsensitive:
    if a.len != b.len: return false
    for i, x in a:
      if cmpIgnoreCase(x, b[i]) != 0:
        return false
    result = true
  of TomlCaseNim:
    if a.len != b.len: return false
    for i, x in a:
      if not cmpNimIdent(x, b[i]):
        return false
    result = true

func compare*(a, b: string, tomlCase: TomlCase): bool =
  case tomlCase
  of TomlCaseSensitive:   a == b
  of TomlCaseInsensitive: cmpIgnoreCase(a, b) == 0
  of TomlCaseNim:         cmpNimIdent(a, b)

proc findFieldReader*(fieldsTable: FieldReadersTable,
                      fieldName: string,
                      expectedFieldPos: var int, tomlCase: TomlCase): auto =
  for i in expectedFieldPos ..< fieldsTable.len:
    if compare(fieldsTable[i].fieldName, fieldName, tomlCase):
      expectedFieldPos = i + 1
      return fieldsTable[i].reader

  for i in 0 ..< expectedFieldPos:
    if compare(fieldsTable[i].fieldName, fieldName, tomlCase):
      return fieldsTable[i].reader

  return nil

const
  digits = block:
    var z = ""
    for i in 0..99:
      if i < 10: z.add '0'
      z.add $i
    z

  maxLen = ($BiggestInt.high).len

proc writeInt*(s: OutputStream, x: BiggestInt, len: Positive) =
  var
    num: array[maxLen, char]
    pos = num.len
    numWritten = 0

  template prepend(c: char) =
    if numWritten < len:
      dec pos
      num[pos] = c
      inc numWritten

  var val = x
  while val > 99:
    let idx = (val mod 100) * 2
    val = val div 100

    prepend digits[idx + 1]
    prepend digits[idx]

  if val < 10:
    prepend char(ord('0') + val)
  else:
    let idx = val * 2
    prepend digits[idx + 1]
    prepend digits[idx]

  for _ in numWritten ..< len:
    prepend '0'

  write s, num.toOpenArray(pos, static(num.len - 1))

proc toHex*(s: OutputStream, x: BiggestInt, len: Positive) =
  const
    hexChars  = "0123456789ABCDEF"
    maxDigits = sizeof(x) * 2

  var
    hex: array[maxDigits, char]
    pos = hex.len
    n = x

  template prepend(c: char) =
    dec pos
    hex[pos] = c

  for _ in 0 ..< len:
    prepend(hexChars[int(n and 0xF)])
    n = n shr 4

  write s, hex.toOpenArray(pos, static(hex.len - 1))

proc emptyTable*(): TomlValueRef =
  TomlValueRef(
    kind: TomlKind.Table,
    tableVal: TomlTableRef.new
  )
