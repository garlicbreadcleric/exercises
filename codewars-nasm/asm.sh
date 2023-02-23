#!/bin/bash

function asm() {
    local name=$(basename $1 .asm)
    nasm -fmacho64 $1 -gdwarf -o "build/$name.o" && \
        ld -lc "build/$name.o" -o "build/$name" && \
        echo "./build/$name"
}

asm $@
