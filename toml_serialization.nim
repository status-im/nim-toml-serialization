# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  serialization, toml_serialization/[reader, writer, types],
  toml_serialization/private/utils

export
  serialization, reader, writer, types

serializationFormat Toml,
                    mimeType = "application/toml"

Toml.setReader TomlReader
Toml.setWriter TomlWriter, PreferredOutput = string

template supports*(_: type Toml, T: type): bool =
  # The TOML format should support every type
  # when not at top level ... :)
  true

import
  typetraits

type
  # only objects | tuple | TomlValueRef allowed at top level
  TomlSpecial* = TomlDateTime or TomlDate or TomlTime
  TomlNotTopLevel* = SomeInteger or
    seq or SomeFloat or string or
    array or enum or bool or TomlSpecial

template tomlFatalImpl(fn,  R: untyped) =
  const typeName = typetraits.name(type R)
  {.fatal: "Toml." & astToStr(fn) & ": \'" & typeName &
    "\' not allowed at top level Toml, called from" &
    $instantiationInfo().}

template decode*(_: type Toml,
                 input: string,
                 RecordType: type TomlNotTopLevel,
                 params: varargs[untyped]): auto =
  # TODO, this is duplicated only due to a Nim bug:
  # If `input` was `string|openarray[byte]`, it won't match `seq[byte]`
  tomlFatalImpl(decode, RecordType)

template decode*(_: type Toml,
                 input: openarray[byte],
                 RecordType: type TomlNotTopLevel,
                 params: varargs[untyped]): auto =
  # TODO, this is duplicated only due to a Nim bug:
  # If `input` was `string|openarray[byte]`, it won't match `seq[byte]`
  tomlFatalImpl(decode, RecordType)

template loadFile*(_: type Toml,
                   fileName: string,
                   RecordType: type TomlNotTopLevel,
                   params: varargs[untyped]): auto =
  tomlFatalImpl(loadFile, RecordType)

template encode*(_: type Toml,
                 value: TomlNotTopLevel,
                 params: varargs[untyped]): auto =
  type RecordType = type value
  tomlFatalImpl(encode, RecordType)

template saveFile*(_: type Toml,
                   fileName: string,
                   value: TomlNotTopLevel,
                   params: varargs[untyped]) =
  type RecordType = type value
  tomlFatalImpl(saveFile, RecordType)

# override default behaviour when in keyed mode
import
  stew/shims/macros

template tomlDecodeImpl*(input: untyped,
                         RecordType: distinct type,
                         key: string,
                         tomlCase: TomlCase,
                         params: varargs[untyped]): auto =

  mixin init, ReaderType
  {.noSideEffect.}:
    # We assume that there are no side-effects here, because we are
    # using a `memoryInput`. The computed side-effects are coming
    # from the fact that the dynamic dispatch mechanisms used in
    # faststreams may be reading from a file or a network device.
    try:
      var stream = unsafeMemoryInput(input)
      var reader = unpackArgs(init, [TomlReader, stream, tomlCase, params])
      when RecordType is (seq or array) and uTypeIsRecord(RecordType):
        reader.readTableArray(RecordType, key, tomlCase)
      else:
        reader.moveToKey(key, tomlCase)
        reader.readValue(RecordType)
    except IOError:
      raise (ref Defect)() # memory inputs cannot raise an IOError

template decode*(_: type Toml, input: string,
                 RecordType: distinct type,
                 key: string, tomlCase: TomlCase,
                 params: varargs[untyped]): auto =
  tomlDecodeImpl(input, RecordType, key, tomlCase, params)

template decode*(_: type Toml, input: string,
                 RecordType: distinct type,
                 key: string, params: varargs[untyped]): auto =
  tomlDecodeImpl(input, RecordType, key, TomlCaseSensitive, params)

template decode*(_: type Toml, input: openarray[byte],
                 RecordType: distinct type,
                 key: string, tomlCase: TomlCase,
                 params: varargs[untyped]): auto =
  tomlDecodeImpl(input, RecordType, key, tomlCase, params)

template decode*(_: type Toml, input: openarray[byte],
                 RecordType: distinct type,
                 key: string, params: varargs[untyped]): auto =
  tomlDecodeImpl(input, RecordType, key, TomlCaseSensitive, params)


template tomlLoadImpl*(filename: string,
                       RecordType: distinct type,
                       key: string, tomlCase: TomlCase,
                       params: varargs[untyped]): auto =

  mixin init, ReaderType, readValue

  var stream = memFileInput(filename)
  try:
    var reader = unpackArgs(init, [TomlReader, stream, params])
    when RecordType is (seq or array) and uTypeIsRecord(RecordType):
      reader.readTableArray(RecordType, key, tomlCase)
    else:
      reader.moveToKey(key, tomlCase)
      reader.readValue(RecordType)
  finally:
    close stream

template loadFile*(_: type Toml, filename: string,
                   RecordType: distinct type,
                   key: string, tomlCase: TomlCase,
                   params: varargs[untyped]): auto =
  tomlLoadImpl(filename, RecordType, key, tomlCase, params)

template loadFile*(_: type Toml, filename: string,
                   RecordType: distinct type,
                   key: string, params: varargs[untyped]): auto =
  tomlLoadImpl(filename, RecordType, key, TomlCaseSensitive, params)
