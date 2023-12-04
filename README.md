# nim-toml-serialization

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![License: Apache](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![TOML](assets/badge-TOML.svg)](https://github.com/toml-lang/toml/releases/tag/1.0.0)
![Stability: experimental](https://img.shields.io/badge/stability-experimental-orange.svg)
![nimble](https://img.shields.io/badge/available%20on-nimble-yellow.svg?style=flat-square)
![Github action](https://github.com/status-im/nim-toml-serialization/workflows/CI/badge.svg)

Flexible TOML serialization [not] relying on run-time type information.

## Overview
nim-toml-serialization is a member of [nim-serialization](https://github.com/status-im/nim-serialization)
family and provides several operation modes:

  - Decode into Nim data types without any intermediate steps using only a **subset** of TOML.
    - Unlike typical lexer-based parser, nim-toml-serialization is very efficient because
      the parser converts text directly into Nim data types and uses no intermediate `token`.
  - Decode into Nim data types mixed with `TomlValueRef` to parse any valid TOML value.
    - Using `TomlValueRef` can offer more flexibility but also require more memory.
      If you can avoid using a dotted key, there is no reason to use `TomlValueRef`.
  - Decode into `TomlValueRef` from any valid TOML.
  - Encode Nim data types into a **subset** of TOML.
  - Encode `TomlValueRef` into full spec TOML.
  - Both encoder and decoder support `keyed` mode.
  - Allow skipping unknown fields using the `TomlUnknownFields` flag.
    - Skipping unknown fields is also done efficiently, with no token produced.
      But skipped fields should contain valid TOML values or the parser will raise an exception.
  - Since v0.2.1 you can choose to use `OrderedTable` instead of `Table` when parsing into `TomlValueRef`
    using `-d:tomlOrderedTable` compile time switch.
  - Since v0.2.3, compile time decode/loadFile is allowed. It means you can initialize a const value using
    `decode` or `loadFile`. It is also ok to use it inside a static block or other nim VM code.

## Spec compliance
nim-toml-serialization implements [v1.0.0](https://github.com/toml-lang/toml/releases/tag/1.0.0)
TOML spec and pass these test suites:

  - [iarna toml test suite](https://github.com/iarna/toml-spec-tests)
  - [burntsushi toml test suite](https://github.com/BurntSushi/toml-test)

## Nonstandard features
- TOML key comparison according to the spec is case sensitive and this is the default mode
  for both encoder/decoder. But nim-toml-serialization also supports:

  - Case insensitive key comparison.
  - Nim ident sensitivity key comparison mode (only the first char is case sensitive).

  TOML key supports Unicode chars but the comparison mentioned above only applies to ASCII chars.

- TOML inline table disallows newline inside the table.
  nim-toml-serialization provides a switch to enable newline in an inline table via `TomlInlineTableNewline`.

- TOML standard does not support xHH escape sequence, only uHHHH or UHHHHHHHH.
  Use `TomlHexEscape` to enable this feature otherwise it will raise an exception.

- TOML standard requires time in HH:MM:SS format, `TomlHourMinute` flags will allow HH:MM format.

## Keyed mode
When decoding, only objects, tuples or `TomlValueRef` are allowed at top level.
All other Nim basic datatypes such as floats, ints, arrays, and booleans must
be a value of a key.

nim-toml-serialization offers `keyed` mode decoding to overcome this limitation.
The parser can skip any non-matching key-value pair efficiently because
the parser produces no token but at the same time can validate the syntax correctly.

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

The key must be a valid Toml basic key, quoted key, or dotted key.

Gotcha:

```toml
server = { ip = "127.0.0.1", port = 8005, name = "TOML Server" }
```

It may be tempting to use keyed mode for the above example like this:
```nim
var x = Toml.decode(rawToml, string, "server.name")
```
But it won't work because the grammar of TOML makes it very difficult
to `exit` from the inline table parser in a clean way.

## Decoder
```nim
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
# This is a nonstandard toml

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

- Date.
  You can parse TOML date using `string`, `TomlDate`, `TomlDateTime`, or `TomlValueRef`.

- Time.
  You can parse TOML time using `string`, `TomlTime`, `TomlDateTime`, or `TomlValueRef`.

- Heterogenous array.
  When parsing a heterogenous array, use `string` or `TomlValueRef`.

- Floats.
  Floats should be implemented as IEEE 754 binary64 values.
  The standard TOML float is float64.
  When parsing floats, use `string` or `TomlValueRef` or `SomeFloat`.

- Integers.
  TOML integer is a 64-bit (signed long) range expected (−9,223,372,036,854,775,808 to 9,223,372,036,854,775,807).
  When parsing integers, use `string` or `SomeInteger`, or `TomlValueRef`.

- Array of tables.
  An array of tables can be parsed via `TomlValueRef` or parsed as a field of object.
  Parsing with keyed mode also works.

- Dotted key.
  When parsing into a nim object, the key must not be dotted.
  The dotted key is supported via `keyed` decoding or `TomlValueRef`.

## Option[T]
  Option[T] works as usual.

## Bignum
TOML integer maxed at int64. But nim-toml-serialization can extend this to arbitrary precision bignum.
Parsing bignum is achieved via the helper function `parseNumber`.

```Nim
# This is an example of how to parse bignum with `parseNumber` and `stint`.

import stint, toml_serialization

proc readValue*(r: var TomlReader, value: var Uint256) =
  try:
    var z: string
    let (sign, base) = r.parseNumber(z)

    if sign == Sign.Neg:
      raiseTomlErr(r.lex, errNegateUint)

    case base
    of base10: value = parse(z, Uint256, 10)
    of base16: value = parse(z, Uint256, 16)
    of base8:  value = parse(z, Uint256, 8)
    of base2:  value = parse(z, Uint256, 2)
  except ValueError as ex:
    raiseUnexpectedValue(r.lex, ex.msg)

var z = Toml.decode("bignum = 1234567890_1234567890", Uint256, "bignum")
assert $z == "12345678901234567890"
```

## Table
Decoding a table can be achieved via the `parseTable` template.
To parse the value, you can use one of the helper functions or use `readValue`.

The table can be used to parse the top-level value, regular table, and inline table like an object.

No built-in `readValue` for the table provided, you must overload it yourself depending on your need.

`Table` can be stdlib table, ordered table, table ref, or any table-like data type.

```Nim
proc readValue*(r: var TomlReader, table: var Table[string, int]) =
  parseTable(r, key):
    table[key] = r.parseInt(int)
```

## Sets and list-like
Similar to `Table`, sets and list or array-like data structure can be parsed using
`parseList` template. It comes in two flavors, indexed and non-indexed.

Built-in `readValue` for regular `seq` and `array` is implemented for you.
No built-in `readValue` for `set` or `set-like` is provided, you must overload it yourself depending on your need.

```nim
type
  HoldArray = object
    data: array[3, int]

  HoldSeq = object
    data: seq[int]

  WelderFlag = enum
    TIG
    MIG
    MMA

  Welder = object
    flags: set[WelderFlag]

proc readValue*(r: var TomlReader, value: var HoldArray) =
  # parseList with index, `i` can be any valid identifier
  r.parseList(i):
    value.data[i] = r.parseInt(int)

proc readValue*(r: var TomlReader, value: var HoldSeq) =
  # parseList without index
  r.parseList:
    let lastPos = value.data.len
    value.data.setLen(lastPos + 1)
    readValue(r, value.data[lastPos])

proc readValue*(r: var TomlReader, value: var Welder) =
  # populating set also okay
  r.parseList:
    value.flags.incl r.parseEnum(WelderFlag)
```

## Enums
There are no enums in TOML specification. The reader/decoder can parse both
the `ordinal` or `string` representation of an enum. While on the other hand,
the writer/encoder only has an `ordinal` built-in writer. But that is not a limitation,
you can always overload the `writeValue` to produce whatever representation of
the enum you need.

The `ordinal` representation of an enum is TOML integer. The `string` representation
is TOML `basic string` or `literal string`. Both multi-line basic strings(e.g. """TOML""") and
multi-line literal strings(e.g. '''TOML''') are not allowed for enum value.

```toml
# fruits.toml
fruit1 = "Apple"   # basic string
fruit2 = 1         # ordinal value
fruit3 = 'Orange'  # literal string
```

```Nim
type
  Fruits = enum
    Apple
    Banana
    Orange

  FruitBasket = object
    fruit1: Fruits
    fruit2: Fruits
    fruit3: Fruits

var x = Toml.loadFile("fruits.toml", FruitBasket)
assert x.fruit1 == Apple
assert x.fruit2 == Banana
assert x.fruit3 == Orange

# write enum output as a string
proc writeValue*(w: var TomlWriter, val: Fruits) =
  w.writeValue $val

let z = FruitBasket(fruit1: Apple, fruit2: Banana, fruit3: Orange)
let res = Toml.encode(z)
assert res == "fruit1 = \"Apple\"\nfruit2 = \"Banana\"\nfruit3 = \"Orange\"\n"
```

## Helper functions
  - `parseNumber(r: var TomlReader, value: var string): (Sign, NumberBase)`
  - `parseDateTime(r: var TomlReader): TomlDateTime`
  - `parseString(r: var TomlReader, value: var string): (bool, bool)`
  - `parseAsString(r: var TomlReader): string`
  - `parseFloat(r: var TomlReader, value: var string): Sign`
  - `parseTime(r: var TomlReader): TomlTime`
  - `parseDate(r: var TomlReader): TomlDate`
  - `parseValue(r: var TomlReader): TomlValueRef`
  - `parseEnum(r: var TomlReader, T: type enum): T`
  - `parseInt(r: var TomlReader, T: type SomeInteger): T`

`parseAsString` can parse any valid TOML value into a Nim string including a mixed array or inline table.

`parseString` returns a tuple:
  - field 0:
    - false: is a single line string.
    - true: is a multi-line string.
  - field 1:
    - false: is a basic string.
    - true: is a literal string.

`Sign` can be one of:
  - `Sign.None`
  - `Sign.Pos`
  - `Sign.Neg`

## Implementation specifics
TomlTime contains a subsecond field. The spec says the precision is implementation-specific.

In nim-toml-serialization the default is 6 digits precision.
Longer precision will be truncated by the parser.

You can override this using compiler switch `-d:tomlSubsecondPrecision=numDigits`.

## Installation

You can install the development version of the library through Nimble with the following command
```
nimble install https://github.com/status-im/nim-toml-serialization@#master
```

or install the latest release version
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

A portion of the toml decoder was taken from PMunch's [`parsetoml`](https://github.com/NimParsers/parsetoml)