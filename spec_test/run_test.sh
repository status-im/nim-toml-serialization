#!/usr/bin/env bash

set -e

cd "$(dirname "${BASH_SOURCE[0]}")"/..
REPO_DIR="${PWD}"
SPEC_DIR="${REPO_DIR}/spec_test"

VERSION="v2.2.0"
EXE=""

OS="$(nim --eval:'echo hostOS' --hints:off)"
CPU="$(nim --eval:'echo hostCPU' --hints:off)"

if [[ "$CPU" == "i386" ]]; then
  echo "cpu == $CPU not supported, skip spec test"
  exit 0
fi

if [[ "$OS" == "macosx" ]]; then
  OS="darwin"
fi

if [[ "$OS" == "windows" ]]; then
  EXE=".exe"
fi

GZ_FILE="toml-test-${VERSION}-${OS}-${CPU}${EXE}.gz"
URL="https://github.com/toml-lang/toml-test/releases/download/${VERSION}/${GZ_FILE}"
TOML_TEST="toml-test${EXE}"
ARCHIVE="${TOML_TEST}.gz"
DECODER_BIN="decoder${EXE}"

if [ ! -f "${SPEC_DIR}/${TOML_TEST}" ]; then
  echo "Downloading toml test binary"
  curl -L "${URL}" -o "${SPEC_DIR}/${ARCHIVE}"
  gunzip "${SPEC_DIR}/${ARCHIVE}"
fi

if [ ! -f "${SPEC_DIR}/${DECODER_BIN}" ]; then
  echo "Building decoder binary"
  nim c -d:release --hints:off "${SPEC_DIR}/decoder.nim"
fi

echo "Running TOML decoder test"

"${SPEC_DIR}/${TOML_TEST}" test -decoder="${SPEC_DIR}/decoder" \
  -skip="invalid/table/super-twice" \
  -skip="invalid/table/redefine-02" \
  -skip="invalid/table/duplicate-key-07" \
  -skip="invalid/table/append-with-dotted-keys-08" \
  -skip="invalid/table/append-with-dotted-keys-02" \
  -skip="invalid/table/append-with-dotted-keys-01" \
  -skip="invalid/control/bare-cr" \
  -skip="valid/table/array-implicit-and-explicit-after" \
  -skip="valid/spec-1.0.0/float-2" \
  -skip="valid/spec-1.0.0/float-0" \
  -skip="valid/inline-table/key-dotted-02" \
  -skip="valid/float/inf-and-nan" \
  -skip="valid/float/float" \
  -skip="valid/comment/tricky" \
  -skip="valid/array/open-parent-table"
