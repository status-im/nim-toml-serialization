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


# override default behaviour when in keyed mode
import
  stew/shims/macros

template decode*(_: type Toml,
                 input: string,
                 RecordType: distinct type,
                 key: string,
                 tomlCase: TomlCase = TomlCaseSensitive,
                 params: varargs[untyped]): auto =

  # TODO, this is duplicated only due to a Nim bug:
  # If `input` was `string|openarray[byte]`, it won't match `seq[byte]`
  mixin init, ReaderType
  {.noSideEffect.}:
    # We assume that there are no side-effects here, because we are
    # using a `memoryInput`. The computed side-effects are coming
    # from the fact that the dynamic dispatch mechanisms used in
    # faststreams may be reading from a file or a network device.
    try:
      var stream = unsafeMemoryInput(input)
      var reader = unpackArgs(init, [TomlReader, stream, params])
      reader.moveToKey(key, tomlCase)
      reader.readValue(RecordType)
    except IOError:
      raise (ref Defect)() # memory inputs cannot raise an IOError

template decode*(_: type Toml,
                 input: openarray[byte],
                 RecordType: distinct type,
                 key: string,
                 tomlCase: TomlCase = TomlCaseSensitive,
                 params: varargs[untyped]): auto =

  # TODO, this is duplicated only due to a Nim bug:
  # If `input` was `string|openarray[byte]`, it won't match `seq[byte]`
  mixin init, ReaderType
  {.noSideEffect.}:
    # We assume that there are no side-effects here, because we are
    # using a `memoryInput`. The computed side-effects are coming
    # from the fact that the dynamic dispatch mechanisms used in
    # faststreams may be reading from a file or a network device.
    try:
      var stream = unsafeMemoryInput(input)
      var reader = unpackArgs(init, [TomlReader, stream, params])
      reader.moveToKey(key, tomlCase)
      reader.readValue(RecordType)
    except IOError:
      raise (ref Defect)() # memory inputs cannot raise an IOError

template loadFile*(Format: distinct type,
                   filename: string,
                   RecordType: distinct type,
                   key: string,
                   tomlCase: TomlCase = TomlCaseSensitive,
                   params: varargs[untyped]): auto =
  mixin init, ReaderType, readValue

  var stream = memFileInput(filename)
  try:
    var reader = unpackArgs(init, [TomlReader, stream, params])
    reader.moveToKey(key, tomlCase)
    reader.readValue(RecordType)
  finally:
    close stream
