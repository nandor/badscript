#!/bin/sh

dune build

_build/default/compiler/compiler.exe -target amd64 -o _build/output.S $1
gcc -c _build/output.S -o _build/output.o
gcc -c runtime/runtime.S -o _build/runtime.o

LD=`ld _build/output.o _build/runtime.o -lc -o _build/output 2>&1`
if [ $? -ne 0 ]; then
  echo $LD
  exit 1
fi

_build/output
