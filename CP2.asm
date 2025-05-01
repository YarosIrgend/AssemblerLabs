assume cs:code, ds:data, ss:stackSeg

data segment para
    introduce_message db "This program adds 134 to your number :)$"
    input_message db 10,13,"Enter number from -32768 to 32633: $"
    user_input db 7,?,7 dup ("$")
    result_message db 10,13,"Result: $"
    len dw ?    ; number_length
    number dw 0 
    overflow_message db 10,13,"Overflow!$"
    non_numeric_message db 10,13,"Non-numeric symbols!$"
    empty_string_message db 10,13,"Empty string!$"
data ends

stackSeg segment para stack
    db 64 dup ("stack")
stackSeg ends

code segment para
start:
    mov ax, data
    mov ds, ax
    
    mov dx, offset introduce_message
    mov ah, 9h
    int 21h
    
enter_num:
    call enterNum
    
    add number, 134
    jo overflowMessage ; if overflow after calculation
    
    call printNum
    
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


; num print procedure
printNum proc
    ; just copy it from metodichka
    mov dx, offset result_message
    mov ah, 9
    int 21h
    
    mov bx, number
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
    ret
printNum endp
    
code ends
end start