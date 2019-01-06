#!/bin/sh

set -e

dune build

_build/default/compiler/compiler.exe -target byte -o _build/output.byte $1
