# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  serialization, toml_serialization/[reader, writer, types]

export
  serialization, reader, writer, types

serializationFormat Toml,
                    Reader = TomlReader,
                    Writer = TomlWriter,
                    PreferedOutput = string,
                    mimeType = "application/toml"

template supports*(_: type Toml, T: type): bool =
  # The TOML format should support every type
  # when not at top level ... :)
  true

import
  typetraits

type
  # only objects | tuple | TomlValueRef allowed at top level
  TomlNotTopLevel* = SomeInteger or
    seq or SomeFloat or string or
    array or enum or bool or TomlDateTime

template decode*(_: type Toml,
                 input: string,
                 RecordType: type TomlNotTopLevel,
                 params: varargs[untyped]): auto =

  # TODO, this is duplicated only due to a Nim bug:
  # If `input` was `string|openarray[byte]`, it won't match `seq[byte]`

  const typeName = typetraits.name(type RecordType)
  {.fatal: "Toml.decode: \'" & typeName & "\' not allowed at top level Toml".}

template decode*(_: type Toml,
                 input: openarray[byte],
                 RecordType: type TomlNotTopLevel,
                 params: varargs[untyped]): auto =

  # TODO, this is duplicated only due to a Nim bug:
  # If `input` was `string|openarray[byte]`, it won't match `seq[byte]`

  const typeName = typetraits.name(type RecordType)
  {.fatal: "Toml.decode: \'" & typeName & "\' not allowed at top level Toml".}

template loadFile*(_: type Toml,
                   fileName: string,
                   RecordType: type TomlNotTopLevel,
                   params: varargs[untyped]): auto =

  const typeName = typetraits.name(type RecordType)
  {.fatal: "Toml.loadFile: \'" & typeName & "\' not allowed at top level Toml".}

template encode*(_: type Toml,
                 value: TomlNotTopLevel,
                 params: varargs[untyped]): auto =

  const typeName = typetraits.name(type value)
  {.fatal: "Toml.encode: \'" & typeName & "\' not allowed at top level Toml".}

template saveFile*(_: type Toml,
                   fileName: string,
                   value: TomlNotTopLevel,
                   params: varargs[untyped]) =

  const typeName = typetraits.name(type value)
  {.fatal: "Toml.saveFile: \'" & typeName & "\' not allowed at top level Toml".}
