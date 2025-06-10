stseg segment para stack
  db 256 dup(?)
stseg ends

data segment para
    array dw 20 dup(?)     
    array_size dw 0        
    user_input db 7,?,7 dup ("$")
    choice dw 0
    number dw 0 
    len dw ?
    sum dw 0
    max_element dw ?
    
    menu_msg db 13,10,13,10,'We will work with array :)',13,10
             db '1. Enter size of array', 13,10
             db '2. Enter elements', 13,10
             db '3. Find sum of elements', 13,10
             db '4. Find maximum element', 13,10
             db '0. Exit', 13,10
             db 'Enter your choice: $'
    
    size_msg db 13,10,'Enter size of array (1-20): $'
    element_msg db 'Enter element (-1600 to 1600): $'
    sum_msg db 13,10,'Sum: $'
    max_msg db 13,10,'Max element: $'
    
    error_size_msg db 13,10,'Incorrect size!',13,10,'$'
    error_empty_msg db 13,10,'Array is empty. At first, enter nums',13,10,'$'
    error_choice_msg db 13,10,'Incorrect choice',13,10,'$'
    error_range_msg db 13,10,'Number out of range! Enter number from -1600 to 1600',13,10,'$'
    
    newline db 13,10,'$'
    
    overflow_message db 10,13,"Overflow!$"
    non_numeric_message db 10,13,"Non-numeric symbols!$"
    empty_string_message db 10,13,"Empty string!$"
    
data ends

code segment para
assume cs:code, ds:data, ss:stseg

main proc
    mov ax, data
    mov ds, ax

    main_loop:
        call show_menu         ; ???????? ????
        call enterNum          ; ???????? ????? ???????????
        mov dx, number         ; ?????????? ????? ? choice
        mov choice, dx

        ; ??????? ??????
        cmp choice, 0
        je exit_program
        cmp choice, 1
        je call_enter_array_size
        cmp choice, 2
        je call_input_array_elements
        cmp choice, 3
        je call_calc_sum
        cmp choice, 4
        je call_find_max

        ; ???? ??????? ???????????? ???????
        lea dx, error_choice_msg
        mov ah, 09h
        int 21h
        jmp main_loop

    call_enter_array_size:
        call enter_array_size
        jmp main_loop

    call_input_array_elements:
        call input_array_elements
        jmp main_loop

    call_calc_sum:
        call calc_sum
        jmp main_loop

    call_find_max:
        call find_max
        jmp main_loop

    exit_program:
        lea dx, newline
        mov ah, 09h
        int 21h

        mov ax, 4C00h
        int 21h


    empty_string_error:
        lea dx, empty_string_message
        mov ah, 09h
        int 21h
        jmp main_loop

    overflow_error:
        lea dx, overflow_message
        mov ah, 09h
        int 21h
        jmp main_loop

    non_numeric_error:
        lea dx, non_numeric_message
        mov ah, 09h
        int 21h
        jmp main_loop

main endp

show_menu proc
    lea dx, menu_msg
    mov ah, 09h
    int 21h
    ret
show_menu endp

enterNum proc
start_enterNum_proc:
    mov number, 0
    
    mov dx, offset user_input
    mov ah, 10
    int 21h
    
    mov cl, user_input+1 ; loop counter init. counter = number_length
    mov len, cx
    
    cmp len, 0
    jz empty_string_error
    
    ; loop
read_num_loop:
    ; as counter decrements in loop, we take address of last symbol of number then minus counter 
    ; so we pass from the left to the end of number
    mov bx, len
    add bx, offset user_input + 2
    sub bx, cx    
    
    ; check on non-numeric symbol
    cmp byte ptr [bx], "0"
    jl  need_to_check_minus  
    cmp byte ptr [bx], "9"
    jg  non_numeric_error
    jmp read_num
    
    ; if symbol is not numeric we should check if it is a minus if 
need_to_check_minus:
    cmp cx, len ; equality cx to len means we check first symbol
    jz minus_check
    jmp non_numeric_error
   
 minus_check:    
    cmp byte ptr [bx], "-"
    jz decrement_cx_if_minus
    jmp non_numeric_error
    
decrement_cx_if_minus:
    dec cx  ; decrement to transfer to next digit
    jmp read_num_loop
    
; if symbol we read is a digit, read it
read_num:
    ; manipulations with ax to leave only ascii-code of symbol
    ; and convert symbol to digit
    xor ax, ax
    mov ax, [bx]
    sub al, 48
    xor ah, ah
    
    ; converting to the number like in metodichka
    add number, ax
    mov ax, number
    cmp cx, 1   ; if we take last digit (counter then = 1), we don't have to multiply by 10 
    jz number_write
    mov dx, 10
    imul dx
    jo overflow_error
    
number_write:
    mov number, ax ; write result in number
    loop read_num_loop
    
    ; check if the first symbol of number is minus
    mov bx, offset user_input+2
    cmp byte ptr [bx], "-"
    jz make_negative ; in case yes, make it negative
    ret
    
make_negative:
    neg number
    ret
    
enterNum endp

enter_array_size proc
    lea dx, size_msg
    mov ah, 09h
    int 21h

    call enterNum
    mov dx, number
    mov array_size, dx

    cmp array_size, 1
    jl size_error
    cmp array_size, 20
    jg size_error
    ret

size_error:
    mov array_size, 0
    lea dx, error_size_msg
    mov ah, 09h
    int 21h
    ret
enter_array_size endp

input_array_elements proc
    cmp array_size, 0
    jle empty_error
    mov cx, array_size
    mov si, 0

    input_loop:
        lea dx, newline
        mov ah, 09h
        int 21h

        lea dx, element_msg
        mov ah, 09h
        int 21h

    input_retry:
        push cx
        push si
        call enterNum

        ; Check range: -1600 to 1600
        mov ax, number
        cmp ax, -1600
        jl range_error
        cmp ax, 1600
        jg range_error

        pop si
        pop cx
        mov ax, number
        mov bx, si
        shl bx, 1
        mov array[bx], ax
        inc si
        loop input_loop
        ret

    range_error:
        lea dx, error_range_msg
        mov ah, 09h
        int 21h
        lea dx, element_msg
        mov ah, 09h
        int 21h
        pop si
        pop cx
        jmp input_retry

    empty_error:
        lea dx, error_empty_msg
        mov ah, 09h
        int 21h
        ret
input_array_elements endp

calc_sum proc
    cmp array_size, 0
    jle empty_error_sum
    mov sum, 0
    mov cx, array_size
    mov si, 0

sum_loop:
    mov bx, si
    shl bx, 1
    mov ax, array[bx]
    add sum, ax
    inc si
    loop sum_loop

    lea dx, sum_msg
    mov ah, 09h
    int 21h

    push sum
    call printNum

    lea dx, newline
    mov ah, 09h
    int 21h
    ret

empty_error_sum:
    lea dx, error_empty_msg
    mov ah, 09h
    int 21h
    ret
calc_sum endp

find_max proc
    cmp array_size, 0
    jle empty_error_max

    mov ax, array[0]
    mov max_element, ax

    mov cx, array_size
    dec cx
    jz print_max

    mov si, 1
max_loop:
    mov bx, si
    shl bx, 1
    mov ax, array[bx]
    cmp ax, max_element
    jle skip_max
    mov max_element, ax
skip_max:
    inc si
    loop max_loop

print_max:
    lea dx, max_msg
    mov ah, 09h
    int 21h

    push max_element
    call printNum

    lea dx, newline
    mov ah, 09h
    int 21h
    ret

empty_error_max:
    lea dx, error_empty_msg
    mov ah, 09h
    int 21h
    ret
find_max endp


printNum proc
    ; prolog (watch the video to understand:)
    push bp
    mov bp, sp
    
    ; just copy it from metodichka
    mov bx, [bp+4]
    or bx,bx 
    jns m1 
    mov al,"-" 
    int 29h 
    neg bx 
m1:
    mov ax,bx 
    xor cx,cx 
    mov bx,10 
m2:
    xor dx,dx 
    div bx 
    add dl,'0' 
    push dx 
    inc cx 
    test ax,ax 
    jnz m2 
m3:
    pop ax 
    int 29h
    loop m3 
    
    ; epilog
    pop bp
    ret 2  ; 2 is mandatory! To delete from stack our local variable 
printNum endp

code ends
end main