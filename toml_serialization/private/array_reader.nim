# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  ../types, ./utils,
  stew/shims/macros, stew/objects,
  serialization/[object_serialization, errors]

type
  ArrayReader*[RecordType, Reader] = tuple[
    numRead: int,
    fieldName: string,
    reader: proc (rec: var RecordType, reader: var Reader, idx: int)
                 {.gcsafe, nimcall, raises: [SerializationError, Defect].}
  ]

  ArrayReadersTable*[RecordType, Reader] = openArray[ArrayReader[RecordType, Reader]]

const
  BadArrayReader* = -1

proc totalArrayFieldsImpl(T: type): int =
  mixin enumAllSerializedFields

  enumAllSerializedFields(T):
    when isArrayLike(FieldType):
      inc result

template totalArrayFields*(T: type): int =
  (static(totalArrayFieldsImpl(T)))

proc makeArrayReadersTable(RecordType, Reader: distinct type, L: static[int]):
                           array[L, ArrayReader[RecordType, Reader]] =
  var fieldPos = 0
  enumAllSerializedFields(RecordType):
    when isArrayLike(FieldType):
      proc readArrayFieldImpl(obj: var RecordType, reader: var Reader, idx: int)
                             {.gcsafe, nimcall, raises: [SerializationError, Defect].} =
        mixin readValue

        when RecordType is tuple:
          const i = fieldName.parseInt

        try:
          {.gcsafe.}:
            when RecordType is tuple:
              reader.readValue(obj[i], idx)
            else:
              reader.readValue(field(obj, realFieldName), idx)
        except SerializationError as err:
          raise err
        except CatchableError as err:
          reader.handleReadException(
            `RecordType`,
            fieldName,
            when RecordType is tuple: obj[i] else: field(obj, realFieldName),
            err)

      result[fieldPos] = (0, fieldName, readArrayFieldImpl)
      inc fieldPos

template arrayReadersTable*(RecordType, Reader: distinct type): auto =
  mixin readValue
  const len = totalArrayFieldsImpl(RecordType)
  makeArrayReadersTable(RecordType, Reader, len)

proc findArrayReader*(fieldsTable: ArrayReadersTable,
                  fieldName: string, tomlCase: TomlCase): int =

  for i in 0 ..< fieldsTable.len:
    if compare(fieldsTable[i].fieldName, fieldName, tomlCase):
      return i

  result = BadArrayReader

template readArray*[T, Y](idx: int, fieldsTable: var ArrayReadersTable, rec: var T, r: var Y) =
  fieldsTable[idx].reader(rec, r, fieldsTable[idx].numRead)
  inc fieldsTable[idx].numRead
