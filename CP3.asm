assume cs:code, ds:data

data segment para
    introduce_message db "This program solves function",10,13, "depending on your X value :)", 10,13," (40x^2-23)/23, 0<=x<=7",10,13," 38x^3+5, x<=0",10,13," 126/x, x>7$"
    input_message db 10,13,10,13,"Enter X from -9 to 32767: $"
    choice_message db 10,13,"Continue? Yes(y) or no(another symbol): $"
    user_choice db 2,?,2 dup("$")
    user_input db 7,?,7 dup ("$")
    result_message db 10,13,"Result: $"
    x dw 0
    remainder dw 0
    number dw 0 
    len dw ?    ; number_length
    slash db "/$"
    space db " $"
    overflow_message db 10,13,"Overflow!$"
    non_numeric_message db 10,13,"Non-numeric symbols!$"
    empty_string_message db 10,13,"Empty string!$"
data ends

code segment para
start:
    mov ax, data
    mov ds, ax
    
    mov dx, offset introduce_message
    mov ah, 9h
    int 21h
    
enter_num:
    call enterNum
    
    cmp number, -9 
    jl overflowMessage ; if less than -9
    cmp number, 32767
    jg overflowMessage ; if more than 32767
    
    mov dx, number  ; initialize x
    mov x, dx       
    
    call calcNum  ; solve equation
    
    mov dx, offset result_message
    mov ah, 9
    int 21h
    
    push number ; take number as argument of procedure printNum (to understand how to take arguments into funcs/procs watch the video :)
    call printNum
    call printRemainder 
    
    ; user choice to continue
    mov dx, offset choice_message
    mov ah, 9h
    int 21h
    mov dx, offset user_choice
    mov ah, 10
    int 21h
    mov dh, [user_choice+2]
    cmp dh, "y"
    je enter_num
    
    mov ax, 4C00h
    int 21h

emptyStringMessage:
    mov dx, offset empty_string_message
    mov ah, 9h
    int 21h
    jmp enter_num  
    
overflowMessage:
    mov dx, offset overflow_message
    mov ah, 9h
    int 21h
    jmp enter_num
    
nonNumericMessage:
    mov dx, offset non_numeric_message
    mov ah, 9h
    int 21h
    jmp enter_num  
    

; num enter procedure
enterNum proc
start_enterNum_proc:
    mov number, 0

    mov dx, offset input_message
    mov ah, 9h
    int 21h
    
    mov dx, offset user_input
    mov ah, 10
    int 21h
    
    mov cl, user_input+1 ; loop counter init. counter = number_length
    mov len, cx
    
    cmp len, 0
    jz emptyStringMessage
    
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
    jg  nonNumericMessage
    jmp read_num
    
    ; if symbol is not numeric we should check if it is a minus if 
need_to_check_minus:
    cmp cx, len ; equality cx to len means we check first symbol
    jz minus_check
    jmp nonNumericMessage
   
 minus_check:    
    cmp byte ptr [bx], "-"
    jz decrement_cx_if_minus
    jmp nonNumericMessage
    
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
    jo overflowMessage 
    
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


; function solve procedure
calcNum proc
    cmp number, 0
    jg check_if_less_seven
    jmp second_equation

check_if_less_seven:
    cmp number, 7
    jng first_equation
    jmp third_equation 
    
first_equation:
    mov dx, number 
    mov ax, number
    imul ax ; squared number
    
    mov bx, 40
    imul bx
    
    mov dx, ax
    sub dx, 23
    
    mov ax, dx
    mov bx, x
    cwd 
    idiv bx
    mov number, ax
    mov remainder, dx
    ;call printRemainder 
    jmp exit_from_calc
    
second_equation:
    mov ax, number ; take now number as x to save the original value of x
    mov dx, x
    imul dx
    mov dx, x
    imul dx ; x^3
    
    mov dx, 38
    imul dx
    mov number, ax
 
    add number, 5
    mov remainder, 0
    jmp exit_from_calc
    
third_equation:
    mov ax, 126
    mov bx, x
    cwd 
    idiv bx
    
    mov number, ax
    mov remainder, dx
    ;call printRemainder 
    
exit_from_calc:
    ret
calcNum endp

; num print procedure
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


; print remainder
printRemainder proc
    cmp remainder, 0
    jne print_remainder
    jmp exit_from_remainder
    
print_remainder:
    mov dx, offset space
    mov ah, 9h
    int 21h
    
    push remainder
    call printNum
    
    mov dx, offset slash
    mov ah, 9h
    int 21h
    
    push x
    call printNum
exit_from_remainder:
    ret
    
printRemainder endp
    
code ends
end start