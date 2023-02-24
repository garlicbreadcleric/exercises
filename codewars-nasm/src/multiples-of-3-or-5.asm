; https://www.codewars.com/kata/514b92a657cdc65150000006

section .data

; Multiples less than or equal to 15.
mle15:
    dd  3, 5, 6, 9, 10, 12, 15
mul3or5_str:
    db  "mul3or5(%d) = %u", 10, 0

%define mle15_len 7

global _main
global mul3or5

extern _printf

section .text

mul3or5:
    xor eax, eax
    xor ebx, ebx                ; Base number.
    xor rcx, rcx                ; mle15 index.

.loop:
    cmp rcx, mle15_len * 4
    
    jl  .next_multiple
    
    xor rcx, rcx
    add ebx, 15

.next_multiple:
    mov edx, ebx
    lea r8, [rel mle15]
    add edx, dword [r8 + rcx]
    cmp edx, edi
    jge .ret
    add eax, edx
    add rcx, 4
    jmp .loop

.ret:
    ret

_main:
    mov         edi, 10
    call        mul3or5

    lea         rdi, [rel mul3or5_str]
    mov         rsi, 10
    mov         rdx, rax
    sub         rsp, 8
    call        _printf
    add         rsp, 8

    mov         rax, 0x02000001
    xor         rdi, rdi
    syscall