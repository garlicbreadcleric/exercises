; https://www.codewars.com/kata/5500d54c2ebe0a8e8a0003fd

section .text

global my_gcd
global _main

extern _printf

my_gcd:
    xor rax, rax
.loop:
    cmp rsi, 0
    jne .ne0
    mov rax, rdi
    jmp .ret
.ne0:
    mov rax, rdi
    mov rcx, rsi
    xor rdx, rdx
    div rcx
    mov rdi, rsi
    mov rsi, rdx
    jmp .loop
.ret:
    ret

%macro print_gcd 2
    mov rdi, %1
    mov rsi, %2
    call my_gcd

    mov rsi, %1
    lea rdi, [rel .gcd_str]
    mov rdx, %2
    mov rcx, rax
    mov rax, 0
    sub rsp, 8
    mov al, 0
    call _printf
    add rsp, 8

    mov rdi, 77
    mov rsi, 49
    call my_gcd
%endmacro

_main:
    print_gcd 28, 16
    print_gcd 77, 48
    print_gcd 678848322, 9958397878148688988

    mov rax, 0x02000001
    xor rdi, rdi
    syscall
.gcd_str:
    db "gcd(%lu, %lu) = %lu", 10, 0
