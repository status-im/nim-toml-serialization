# toml-serialization
# Copyright (c) 2019-2026 Status Research & Development GmbH
# Licensed under either of
#  * Apache License, version 2.0, ([LICENSE-APACHE](LICENSE-APACHE))
#  * MIT license ([LICENSE-MIT](LICENSE-MIT))
# at your option.
# This file may not be copied, modified, or distributed except according to
# those terms.

{.push raises: [], gcsafe.}

import pkg/results, ../../toml_serialization/[reader, writer]

export results

template isOptional*[T](_: type Toml, U: distinct type Opt[T]): bool = true
template baseType*[T](_: type Toml, U: distinct type Opt[T]): type = T

template shouldWriteField*[T](_: type Toml, field: Opt[T]): bool =
  field.isSome

func isFieldExpected*[X](_: type Toml, T: distinct type Opt[X]): bool {.compileTime.} = false

proc writeValue*[T](w: var TomlWriter, value: Opt[T]) {.raises: [IOError].} =
  mixin writeValue

  if value.isSome:
    w.writeValue(value.get)

proc readValue*[T](
    r: var TomlReader, value: var Opt[T]
) {.raises: [IOError, SerializationError].} =
  mixin readValue

  value.ok r.readValue(T)

proc readValue*[T](
  r: var TomlReader, value: var Opt[T], numRead: int
) {.gcsafe, raises: [SerializationError, IOError].} =
  value.ok()
  readValue(r, value.get, numRead)
