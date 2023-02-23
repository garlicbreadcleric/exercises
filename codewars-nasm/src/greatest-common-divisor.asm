; https://www.codewars.com/kata/5500d54c2ebe0a8e8a0003fd

section .text

global my_gcd
global _main

extern _printf
extern _puts

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

_main:
    mov rdi, 28
    mov rsi, 16
    call my_gcd

    mov rsi, rax
    lea rdi, [rel .gcd1_str]
    mov rax, 0
    sub rsp, 8
    mov al, 0
    call _printf
    add rsp, 8

    mov rdi, 77
    mov rsi, 49
    call my_gcd

    mov rsi, rax
    lea rdi, [rel .gcd2_str]
    mov rax, 0
    sub rsp, 8
    mov al, 0
    call _printf
    add rsp, 8
    
    mov rdi, 678848322
    mov rsi, 9958397878148688988
    call my_gcd

    mov rsi, rax
    lea rdi, [rel .gcd3_str]
    mov rax, 0
    sub rsp, 8
    mov al, 0
    call _printf
    add rsp, 8

    mov rax, 0x02000001
    xor rdi, rdi
    syscall
.gcd1_str:
    db "gcd(28, 16) = %d", 10, 0
.gcd2_str:
    db "gcd(77, 49) = %d", 10, 0
.gcd3_str:
    db "gcd(678848322, 9958397878148688988) = %d", 10, 0
