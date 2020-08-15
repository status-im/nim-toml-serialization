# nim-toml-serialization

[![Build Status (Travis)](https://img.shields.io/travis/status-im/nim-toml-serialization/master.svg?label=Linux%20/%20macOS "Linux/macOS build status (Travis)")](https://travis-ci.org/status-im/nim-toml-serialization)
[![Windows build status (Appveyor)](https://img.shields.io/appveyor/ci/nimbus/nim-toml-serialization/master.svg?label=Windows "Windows build status (Appveyor)")](https://ci.appveyor.com/project/nimbus/nim-toml-serialization)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![License: Apache](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
![Stability: experimental](https://img.shields.io/badge/stability-experimental-orange.svg)

Flexible TOML serialization [not] relying on run-time type information.

## Overview
nim-toml-serialization is a member of [nim-serialization](https://github.com/status-im/nim-serialization)
family and provides several operation modes:

  - Decode into Nim data types without any intermediate steps using only a **subset** of TOML.
  - Decode into Nim data types mixed with `TomlValueRef` to parse any valid TOML value.
  - Decode into `TomlValueRef` from any valid TOML.
  - Encode Nim data types into a **subset** of TOML.
  - Encode `TomlValueRef` into full spec TOML.
  - Both encoder and decoder support `keyed` mode.

## Spec compliance
nim-toml-serialization implements [1.0.0-rc1](https://github.com/toml-lang/toml/releases/tag/v1.0.0-rc.1)
TOML spec and pass these test suites:

  - [iarna toml test suite](https://github.com/iarna/toml-spec-tests)
  - [burntsushi toml test suite](https://github.com/BurntSushi/toml-test)

## Non standard features
- TOML key comparison according to the spec is case sensitive and this is the default mode
  for both encoder/decoder. But nim-toml-serialization also support:

  - Case insensitive key comparison.
  - Nim ident sensitivity key comparison mode (only the first char is case sensitive).

  TOML key supports Unicode chars but the comparison mentioned above only applied to ASCII chars.

- TOML inline table disallow newline inside the table.
  nim-toml-serialization provide a switch to enable newline in inline table via `TomlInlineTableNewline`.

- TOML standard does not support xHH escape sequence, only uHHHH or UHHHHHHHH.
  Use `TomlHexEscape` to enable this feature, otherwise it will raise exception.

## Keyed mode
When decoding, only object, tuple or `TomlValueRef` allowed at top level.
All others Nim basic datatypes such as floats, ints, array, boolean must
be a value of a key.

nim-toml-serialization offers `keyed` mode decoding to overcome this limitation.

```toml
[server]
  name = "TOML Server"
  port = 8005
```

```Nim
var x = Toml.decode(rawToml, string, "server.name")
assert x == "TOML Server"

or

var y = Toml.decode(rawToml, string, "server.name", caseSensitivity)
```

where `caseSensitivity` is one of:
  - TomlCaseSensitive
  - TomlCaseInsensitive
  - TomlCaseNim

Key must be valid Toml basic-key, quoted-key, or dotted-key.

## Decoder
```
  type
    NimServer = object
      name: string
      port: int

    MixedServer = object
      name: TomlValueRef
      port: int

    StringServer = object
      name: string
      port: string

  # decode into native Nim
  var nim_native = Toml.decode(rawtoml, NimServer)

  # decode into mixed Nim + TomlValueRef
  var nim_mixed = Toml.decode(rawtoml, MixedServer)

  # decode any value into string
  var nim_string = Toml.decode(rawtoml, StringServer)

  # decode any valid TOML
  var toml_value = Toml.decode(rawtoml, TomlValueRef)
```

## Parse inline table with newline
```toml
# this is a non standard toml

server = {
  ip = "127.0.0.1",
  port = 8005,
  name = "TOML Server"
}
```

```Nim
  # turn on newline in inline table mode
  var x = Toml.decode(rawtoml, Server, flags = {TomlInlineTableNewline})
```

## Load and save
```Nim
  var server = Toml.loadFile("filename.toml", Server)
  var ip = Toml.loadFile("filename.toml", string, "server.ip")

  Toml.saveFile("filename.toml", server)
  Toml.saveFile("filename.toml", ip, "server.ip")
  Toml.saveFile("filename.toml", server, flags = {TomlInlineTableNewline})
```

## TOML we can['t] do
- Date Time.
  TOML date time format is described in [RFC 3339](https://tools.ietf.org/html/rfc3339).
  When parsing TOML date time, use `string`, `TomlDateTime`, or `TomlValueRef`.

- Heterogenous array.
  When parsing heterogenous array, use `string` or `TomlValueRef`.

- Floats.
  Floats should be implemented as IEEE 754 binary64 values.
  Standard TOML float are float64.
  When parsing floats, use `string` or `TomlValueRef` or `SomeFloat`.

- Integers.
  TOML integer is an 64 bit (signed long) range expected (−9,223,372,036,854,775,808 to 9,223,372,036,854,775,807).
  When parsing floats, use `string` or `SomeInteger`, or `TomlValueRef`.

- Array of tables.
  Currently array of tables only can be parsed via `TomlValueRef`.

- Dotted key.
  When parse into nim object, key must not a dotted key.
  Dotted key is supported via `keyed` decoding or `TomlValueRef`.

## Option[T]
  Option[T] works as usual.

## Bignum
TOML integer maxed at int64. But nim-toml-serialization can extend this to arbitrary precision bignum.
Parsing bignum is achieved via helper function `parseNumber`.

```Nim
# this is an example how to parse bignum with `parseNumber` and `stint`.

import stint, toml_serialization

proc readValue*(r: var TomlReader, value: var Uint256) =
  var z: string
  let (sign, base) = r.parseNumber(z)

  if sign == Sign.Neg:
    raiseTomlErr(r.lex, errNegateUint)

  case base
  of base10: value = parse(z, Uint256, 10)
  of base16: value = parse(z, Uint256, 16)
  of base8:  value = parse(z, Uint256, 8)
  of base2:  value = parse(z, Uint256, 2)

var z = Toml.decode("bignum = 1234567890_1234567890", Uint256, "bignum")
assert $z == "12345678901234567890"
```

## Helper functions
  - `parseNumber(r: var TomlReader, value: var string): (Sign, NumberBase)`
  - `parseDateTime(r: var TomlReader): TomlDateTime`
  - `parseString(r: var TomlReader, value: var string): (bool, bool)`
  - `parseAsString(r: var TomlReader): string`
  - `parseFloat(r: var TomlReader, value: var string): Sign`

`parseAsString` can parse any valid TOML value into Nim string including mixed array or inline table.
`parseString` return a tuple:
  - field 0:
    - false: is a single line string.
    - true: is a multi line string.
  - field 1:
    - false: is a basic string.
    - true: is a literal string.

`Sign` can be one of:
  - `Sign.None`
  - `Sign.Pos`
  - `Sign.Neg`

## Implementation specifics
TOMLTime contains subsecond field. The spec says the precision is implementation specific.
In nim-toml-serialization the default is 6 digits precision.
You can override this using compiler switch `-d:subsecondPrecision=numDigits`.

## Installation

You can install the developement version of the library through nimble with the following command
```
nimble install https://github.com/status-im/nim-toml-serialization@#master
```

or install latest release version
```
nimble install toml_serialization
```

## License

Licensed and distributed under either of

* MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT

or

* Apache License, Version 2.0, ([LICENSE-APACHEv2](LICENSE-APACHEv2) or http://www.apache.org/licenses/LICENSE-2.0)

at your option. This file may not be copied, modified, or distributed except according to those terms.

## Credits

 A portion of toml decoder was taken from PMunch's [`parsetoml`](https://github.com/NimParsers/parsetoml)
