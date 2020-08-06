# nim-toml-serialization

[![Build Status (Travis)](https://img.shields.io/travis/status-im/nim-toml-serialization/master.svg?label=Linux%20/%20macOS "Linux/macOS build status (Travis)")](https://travis-ci.org/status-im/nim-toml-serialization)
[![Windows build status (Appveyor)](https://img.shields.io/appveyor/ci/nimbus/nim-toml-serialization/master.svg?label=Windows "Windows build status (Appveyor)")](https://ci.appveyor.com/project/nimbus/nim-toml-serialization)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![License: Apache](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
![Stability: experimental](https://img.shields.io/badge/stability-experimental-orange.svg)

Flexible TOML serialization [not] relying on run-time type information.

## Overview
  nim-serialization
  general purpose toml parser
  compile time but not not compile time
  run time

## Spec Compliance
  1.0.0-rc1
  iarna test suite
  burnsushi test suite

## Encoder
  subset, full
    example

  newline in inline table
    example

  Toml.encode(T obj, "name", niit, subset/full)
    example

  Toml.encode(T obj, "name" + index?, niit, subset/full)
    example

## Decoder
  always pretty: depends on niit
    example

  subset, full
    example

  newline in inline table
    example

  Toml.decode(seq/string, T type, "name", niit, subset/full)
    example

  Toml.decode(seq/string, T type, "name" + index?, niit, subset/full)
    example

  Toml.decode(seq/string, T TomlTable, "name", niit)
    example

  Toml.decode(seq/string, T TomlTableArray, niit)
    example

## Load and Save
  load example
  save example

## TOML we can['t] do
  numerics

## Nim but [not] TOML
  Top level only tables
  and arrays of tables

## Option[T]
  manual override

## Bignum
  example uint256
  example uint1024

## TomlTableArray
  anything

## TomlTable
  single dict
  get one of array

## Builtins
  function  output

## Installation

You can install the developement version of the library through nimble with the following command
```
nimble install https://github.com/status-im/nim-toml-serialization@#master
```

or install latest release version
```
nimble install toml-serialization
```

## License

Licensed and distributed under either of

* MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT

or

* Apache License, Version 2.0, ([LICENSE-APACHEv2](LICENSE-APACHEv2) or http://www.apache.org/licenses/LICENSE-2.0)

at your option. This file may not be copied, modified, or distributed except according to those terms.
