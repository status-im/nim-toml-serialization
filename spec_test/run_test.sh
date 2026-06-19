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
DECODER_v100_BIN="decoder_v100${EXE}"
DECODER_v110_BIN="decoder_v110${EXE}"

if [ ! -f "${SPEC_DIR}/${TOML_TEST}" ]; then
  echo "Downloading toml test binary"
  curl -L "${URL}" -o "${SPEC_DIR}/${ARCHIVE}"
  gunzip "${SPEC_DIR}/${ARCHIVE}"
  chmod +x "${SPEC_DIR}/${ARCHIVE}"
fi

if [ ! -f "${SPEC_DIR}/${DECODER_v100_BIN}" ]; then
  echo "Building decoder binary v100"
  nim c -d:release --hints:off -o:"${SPEC_DIR}/${DECODER_v100_BIN}" "${SPEC_DIR}/decoder.nim"
fi

echo "Running TOML v1.0.0 decoder test"

"${SPEC_DIR}/${TOML_TEST}" test -decoder="${SPEC_DIR}/${DECODER_v100_BIN}" \
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

if [ ! -f "${SPEC_DIR}/${DECODER_v110_BIN}" ]; then
  echo "Building decoder binary v110"
  nim c -d:release -d:toml_v110 --hints:off -o:"${SPEC_DIR}/${DECODER_v110_BIN}" "${SPEC_DIR}/decoder.nim"
fi

echo "Running TOML v1.1.0 decoder test"

"${SPEC_DIR}/${TOML_TEST}" test -toml="1.1" -decoder="${SPEC_DIR}/${DECODER_v110_BIN}" \
  -skip="valid/array/array" \
  -skip="valid/array/open-parent-table" \
  -skip="valid/comment/tricky" \
  -skip="valid/datetime/datetime" \
  -skip="valid/datetime/edge" \
  -skip="valid/datetime/local" \
  -skip="valid/datetime/local-time" \
  -skip="valid/datetime/no-seconds" \
  -skip="valid/example" \
  -skip="valid/float/float" \
  -skip="valid/float/inf-and-nan" \
  -skip="valid/inline-table/key-dotted-02" \
  -skip="valid/spec-1.1.0/common-12" \
  -skip="valid/spec-1.1.0/common-23" \
  -skip="valid/spec-1.1.0/common-25" \
  -skip="valid/spec-1.1.0/common-27" \
  -skip="valid/spec-1.1.0/common-28" \
  -skip="valid/spec-1.1.0/common-29" \
  -skip="valid/spec-1.1.0/common-30" \
  -skip="valid/spec-1.1.0/common-31" \
  -skip="valid/spec-1.1.0/common-33" \
  -skip="valid/spec-1.1.0/common-34" \
  -skip="valid/spec-example-1" \
  -skip="valid/spec-example-1-compact" \
  -skip="valid/string/hex-escape" \
  -skip="valid/table/array-implicit-and-explicit-after" \
  -skip="invalid/control/bare-cr" \
  -skip="invalid/control/multi-cr" \
  -skip="invalid/control/rawmulti-cr" \
  -skip="invalid/table/append-with-dotted-keys-01" \
  -skip="invalid/table/append-with-dotted-keys-02" \
  -skip="invalid/table/append-with-dotted-keys-08" \
  -skip="invalid/table/duplicate-key-07" \
  -skip="invalid/table/redefine-02" \
  -skip="invalid/table/super-twice"
