# toml-serialization
# Copyright (c) 2026 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

{.push raises: [], gcsafe.}

import
  std/typetraits,
  stew/shims/macros,
  ./private/utils,
  ./[types, desc]

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

# override default behaviour when in keyed mode
template tomlDecodeImpl(Format: type SerializationFormat,
                        input: untyped,
                        RecordType: distinct type,
                        key: string,
                        tomlCase: TomlCase,
                        params: varargs[untyped]): auto =

  mixin init, Reader
  {.noSideEffect.}:
    # We assume that there are no side-effects here, because we are
    # using a `memoryInput`. The computed side-effects are coming
    # from the fact that the dynamic dispatch mechanisms used in
    # faststreams may be reading from a file or a network device.
    type
      TomlReaderType = Reader(Format)

    try:
      let stream = unsafeMemoryInput(input)
      var reader = unpackArgs(init, [TomlReaderType, stream, tomlCase, params])
      when RecordType is (seq or array) and isRecord(Toml, RecordType):
        reader.readTableArray(RecordType, key, tomlCase)
      else:
        reader.moveToKey(key, tomlCase)
        reader.readValue(RecordType)
    except IOError:
      raise (ref Defect)() # memory inputs cannot raise an IOError

template tomlLoadImpl(Format: type SerializationFormat,
                      filename: string,
                      RecordType: distinct type,
                      key: string, tomlCase: TomlCase,
                      params: varargs[untyped]): auto =
  mixin init, Reader, readValue
  type
    TomlReaderType = Reader(Format)

  var stream: InputStream
  when nimvm:
    let input = staticRead(filename)
    stream = unsafeMemoryInput(input)
  else:
    stream = memFileInput(filename)
  try:
    var reader = unpackArgs(init, [TomlReaderType, stream, params])
    when RecordType is (seq or array) and isRecord(Toml, RecordType):
      reader.readTableArray(RecordType, key, tomlCase)
    else:
      reader.moveToKey(key, tomlCase)
      reader.readValue(RecordType)
  finally:
    close stream

template setDecoder*(_: type Toml, Format: distinct type SerializationFormat) =
  template decode*(_: type Format,
                  input: string,
                  RecordType: type TomlNotTopLevel,
                  params: varargs[untyped]): auto =
    # TODO, this is duplicated only due to a Nim bug:
    # If `input` was `string|openArray[byte]`, it won't match `seq[byte]`
    tomlFatalImpl(decode, RecordType)

  template decode*(_: type Format,
                  input: openArray[byte],
                  RecordType: type TomlNotTopLevel,
                  params: varargs[untyped]): auto =
    # TODO, this is duplicated only due to a Nim bug:
    # If `input` was `string|openArray[byte]`, it won't match `seq[byte]`
    tomlFatalImpl(decode, RecordType)

  template loadFile*(_: type Format,
                    fileName: string,
                    RecordType: type TomlNotTopLevel,
                    params: varargs[untyped]): auto =
    tomlFatalImpl(loadFile, RecordType)

  template encode*(_: type Format,
                  value: TomlNotTopLevel,
                  params: varargs[untyped]): auto =
    type RecordType = type value
    tomlFatalImpl(encode, RecordType)

  template saveFile*(_: type Format,
                    fileName: string,
                    value: TomlNotTopLevel,
                    params: varargs[untyped]) =
    type RecordType = type value
    tomlFatalImpl(saveFile, RecordType)

  template decode*(T: type Format, input: string,
                  RecordType: distinct type,
                  key: string, tomlCase: TomlCase,
                  params: varargs[untyped]): auto =
    T.tomlDecodeImpl(input, RecordType, key, tomlCase, params)

  template decode*(T: type Format, input: string,
                  RecordType: distinct type,
                  key: string, params: varargs[untyped]): auto =
    T.tomlDecodeImpl(input, RecordType, key, TomlCaseSensitive, params)

  template decode*(T: type Format, input: openArray[byte],
                  RecordType: distinct type,
                  key: string, tomlCase: TomlCase,
                  params: varargs[untyped]): auto =
    T.tomlDecodeImpl(input, RecordType, key, tomlCase, params)

  template decode*(T: type Format, input: openArray[byte],
                  RecordType: distinct type,
                  key: string, params: varargs[untyped]): auto =
    T.tomlDecodeImpl(input, RecordType, key, TomlCaseSensitive, params)

  template decode*(T: type Format,
                  input: string,
                  RecordType: distinct type,
                  params: varargs[untyped]): auto =
    T.tomlDecodeImpl(input, RecordType, "", TomlCaseSensitive, params)

  template decode*(T: type Format,
                  input: openArray[byte],
                  RecordType: distinct type,
                  params: varargs[untyped]): auto =
    T.tomlDecodeImpl(input, RecordType, "", TomlCaseSensitive, params)


  template loadFile*(T: type Format, filename: string,
                    RecordType: distinct type,
                    key: string, tomlCase: TomlCase,
                    params: varargs[untyped]): auto =
    T.tomlLoadImpl(filename, RecordType, key, tomlCase, params)

  template loadFile*(T: type Format, filename: string,
                    RecordType: distinct type,
                    key: string, params: varargs[untyped]): auto =
    T.tomlLoadImpl(filename, RecordType, key, TomlCaseSensitive, params)

  template loadFile*(T: type Format, filename: string,
                    RecordType: distinct type,
                    tomlCase: TomlCase,
                    params: varargs[untyped]): auto =
    T.tomlLoadImpl(filename, RecordType, "", tomlCase, params)

  template loadFile*(T: type Format, filename: string,
                    RecordType: distinct type,
                    params: varargs[untyped]): auto =
    T.tomlLoadImpl(filename, RecordType, "", TomlCaseSensitive, params)
