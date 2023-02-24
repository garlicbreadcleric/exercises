; https://www.codewars.com/kata/52b7ed099cdc285c300001cd

section .text

global _main
global sumintvls

extern _malloc
extern _printf

struc intvl
    .first:     resd    1
    .second:    resd    1
endstruc

sumintvls:
    call    sortintvls

    mov     r10, rsi                                            ; Max interval offset.
    imul    r10, intvl_size

    xor     rax, rax                                            ; Interval sum.
    xor     rbx, rbx                                            ; Interval pointer.

    xor     rcx, rcx                                            ; Current interval start.
    xor     rdx, rdx                                            ; Current interval end.
    xor     r8, r8                                              ; Previous interval start.
    xor     r9, r9                                              ; Previous interval end.

.loop:
    cmp     rbx, r10                                            ; If interval pointer is out of bounds,
    jae     .ret                                                ; break the loop.

    mov     ecx, dword [rdi + rbx + intvl.first]                ; Current interval start.
    mov     edx, dword [rdi + rbx + intvl.second]               ; Current interval end.

    cmp     rbx, 0                                              ; First interval doesn't have a previous one,
    je      .calculate_intvl_diff                               ; so skip previous check.

    mov     r8d, dword [rdi + rbx - intvl_size + intvl.first]   ; Previous interval start.
    mov     r9d, dword [rdi + rbx - intvl_size + intvl.second]  ; Previous interval end.

    cmp     edx, r9d                                            ; If current interval end is not less than previous interval end,
    jnl     .current_non_empty                                  ; then there's something to process.

    mov     dword [rdi + rbx + intvl.second], r9d               ; Otherwise, set current interval end to previous interval end,
    add     rbx, intvl_size                                     ; and continue to next interval.
    jmp     .loop

.current_non_empty:

    cmp     ecx, r9d                                            ; If current interval start is not less than previous interval end,
    jnl     .calculate_intvl_diff                               ; then it should be processed fully.

    mov     ecx, r9d                                            ; Otherwise, set current interval start to last interval end.

.calculate_intvl_diff:
    sub     edx, ecx
    add     rax, rdx

    add     rbx, intvl_size
    jmp     .loop

.ret:
    ret

sortintvls:
    mov     rax, rdi

    mov     r10, rsi                            ; Max interval offset.
    imul    r10, intvl_size

.outer_loop:
    mov     rcx, intvl_size                     ; Interval pointer.
.inner_loop:
    cmp     rcx, r10                            ; If interval pointer is out of bounds,
    jae     .ret                                ; break the loop.

    mov     r8, qword [rdi + rcx - intvl_size]  ; If previous interval start is not less than or equal
    mov     r9, qword [rdi + rcx]               ; to current element,
    cmp     r8d, r9d                            ; then
    jg      .swap_elements                      ; swap them.

    add     rcx, intvl_size
    jmp     .inner_loop
.swap_elements:
    mov     [rdi + rcx - intvl_size], r9
    mov     [rdi + rcx], r8
    jmp     .outer_loop

.ret:
    ret

%macro allocate 2
    mov     edi, %1_size * %2
    sub     rsp, 8
    call    _malloc
    add     rsp, 8
%endmacro

%macro push_intvl 2
    mov     dword [rax + intvl.first], %1
    mov     dword [rax + intvl.second], %2
    add     rax, intvl_size
%endmacro

_main:
    allocate    intvl, 3
    mov         rdi, rax

    push_intvl  1, 4
    push_intvl  7, 10
    push_intvl  3, 5

    mov         rsi, 3
    call        sumintvls

    lea         rdi, [rel .sumintvls_str]
    mov         rsi, rax
    sub         rsp, 8
    call        _printf
    add         rsp, 8

    mov         rax, 0x02000001
    xor         rdi, rdi
    syscall
.sumintvls_str:
    db "sum = %ld", 10, 0
