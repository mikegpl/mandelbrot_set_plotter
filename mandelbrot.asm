;-----------------------------------       draws mandelbrot set       ----------------------------------- 
;-----------------------------------  Michał Grabowski 'mkgrabowski'  ----------------------------------- 
;-----------------------------------    Informatyka IEiT, grupa 7     -----------------------------------
;-----------------------------------abandon all hope ye who enter here----------------------------------- 



;-----------------------------------              Data                ----------------------------------- 
data segment
    
    ; parser 
    parsedArguments             db 128  dup(?)  ; argtab
    numberOfArgs                db  0           ; number of args
    argLengths                  db  6   dup(0)  ; arg length tab
    ;/parser 

    ; video mode parameters
    xResolution                 dw  320         ; screen resolution for video mode
    yResolution                 dw  200
    ;/video mode parameters

    ; str to float conv section
    argBuffer                   db  30  dup(?)  ; for string to flat conversion
    leftFloat                   dw  ?           ; left side of . in float
    rightFloat                  dw  ?           ; right side of . in float
    rightLen                    dw  ?           ; length of right side of float
    ten                         dw  10d         ; for multiplying and dividing
    ;/str to float conv section

    ; FPU variables
    xMin                        dq  1.0         
    xMax                        dq  1.0
    yMin                        dq  1.0
    yMax                        dq  1.0
    pconst                      dq  ?
    qconst                      dq  ?
    float                       dq  1.0         ; for conversion
    p                           dq  ?           ; p, q for coordinate conversion
    q                           dq  ?
    max                         dq  4.0         ; for boundary condition
    wordBuffer                  dw  ?           ; for writing from CPU to FPU
    ;/FPU variables

    ; errors section
    error0          db  "Too few arguments.$"
    error1          db  "Too many arguments.$"
    error2          db  "Wrong number format - valid format - e.g. 123.321$"
    ;/errors section
data ends



;-----------------------------------              Code                ----------------------------------- 
code1 segment
.386                                ; pusha, popa, fpu flags to ax
.387                                ; coprocessor 80387
assume ss:stos1, ds:data

;###################################               Main               ################################### 

start:
    mov ax, seg data        
    mov ds, ax
    
    mov ax, seg w_stos  
    mov ss, ax
    mov sp, offset w_stos
    
    finit                           ; initialize fpu
    
    ; parse input
    call parse  

    ; draw mandelbrot set
    call drawmandelbrotset
    
    ; end the program and exit to command prompt
    call endsection



;###################################              Parser              ################################### 

parse:                              ; parse args, convert to float, validate

    call readfrombuffer
    call validate
    call writecoordinates
ret

readfrombuffer:                     ; read from PSP to arg buffer, save number and length of arguments

    pusha
    
    xor ax, ax                      ; used for reading from buffer (to al)
    xor bx, bx                      ; bh - argument counter, bl - agrument length counter
    xor cx, cx                      ; for loop counter cx=0
    xor dx, dx                      ; in dx offset of argLengths tab
    
    mov cl, es:[80h]                ; length of PSP buffer
    cmp cl, 1d                      ; check number of args
    JLE endreading                  ; if no arguments
    mov si, 82h                     ; first non space char in PSP buffer
    
    mov di, offset parsedArguments  ; in di (destination index) store offset of parsed arguments tab
    mov dx, offset argLengths       ; in dx store offset of argLengths to save length of each argument
    readloop:
        mov al, es:[si]             ; read char to al
        call checkifwhitespace
        cmp ah, 0d              
        JNE whspace             
        mov [di], al                ; write byte to parsedArguments
        inc di                      ; di+=1
        inc bl                      ; bl+=1 (arg number counter)
        push di             
            mov di, dx              ; set di to argLengths tab offset
            mov [di], bl            ; write current argument length
        pop di
            
        JMP endreadloop
        whspace:                    
            cmp bl, 0d              ; if white space after white space
            JE endreadloop      
            
            inc dx                  ; else if white space after regular character
            xor bl, bl              ; current argument length counter = 0
            inc bh                  ; inc arguments counter
                
        endreadloop:
        inc si                      ; si+=1, points to next char
    loop readloop

    mov di, offset numberOfArgs         
    mov [di], bh                    ; write number of args
    endreading:
    popa
ret
    
checkifwhitespace:                  ; return in ah 1 if al char is whitespace

    xor ah, ah      
    cmp al, ' '                     ; space
    JE setwspace
    cmp al, 9d                      ; tab
    JE setwspace    
    cmp al, 13d                     ; enter
    JE setwspace
    JMP wspaceend
    setwspace:          
    mov ah, 1d
    wspaceend:
ret

validate:                           ; checks if arguments number is right

    pusha

    mov dl, [numberOfArgs]
    cmp dl, 4d
    JB error0out                    ; too few arguments
    JA error1out                    ; too many arguments
    JMP exitvalidation

    error0out:
        mov dx, offset error0
        call errorhandler
    error1out:
        mov dx, offset error1
        call errorhandler

    exitvalidation:
    popa
ret

writecoordinates:                   ; write from parsed buffer to xMin, xMax, yMin, yMax

    pusha
    mov ax, offset xMin
    mov bx, 1
    call writetoarg

    mov ax, offset xMax
    mov bx, 2
    call writetoarg

    mov ax, offset yMin
    mov bx, 3
    call writetoarg

    mov ax, offset yMax
    mov bx, 4
    call writetoarg
    popa
ret

writetoarg:                         ; input - argument number in bx, argument offset in ax

    push ax
    push bx
    push cx
    push si

    push ax
    call getargument
    mov si, ax
    mov ch, [argLengths+bx-1]
    call stringtofloat
    fld qword ptr [float]
    pop ax
    mov bx, ax
    fstp qword ptr [bx]

    pop si
    pop cx
    pop bx
    pop ax
ret

getargument:                        ; input - in bl - argument number, output - in ax - offset of argument

    push si
    push bx
    push cx                             
    push dx                             

    mov cl, bl                      ; copy bl 
    mov si, offset argLengths           
    mov ax, offset parsedArguments

    getargloop:
        cmp cl, 1                   ; if first argument, return offset parsedArguments
        JE getargquit           

        xor dx, dx                  ; dx=0
        mov dl, byte ptr [si]       ; save in dl length of current argument
        add ax, dx                  ; offsetToReturn+=length of current argument
        inc si                      ; si+=1
        dec cl                      ; cx-=1
    JMP getargloop                  ; loop
    
    getargquit:
    pop dx
    pop cx
    pop bx
    pop si
ret

stringtofloat:                      ; converts number written as string to float, input: si- argument offset, ch - argument length

    push ax
    push bx
    push cx
    push dx     
    push si

    cmp ch, 3d                      ; each number should be written with at least 3 characters
    JB error2out
    xor cl, cl                      ; cl=0
    
    xor ax, ax                      ; bede trzymal tutaj liczbe binarna -> JA mnoze przez 10
    xor dx, dx                      ; negative number flag
    
    mov bl, byte ptr [si]
    cmp bl, "-"
    JNE leftloop
    mov dl, 1d                      ; if first char was minus
    inc si                          ; si+=1, read from next char
    inc cl                          ; cl+=1
    
    leftloop:
        cmp cl, ch
        JE error2out                ; if number ends before ., throw error
        xor bx, bx                  ; bx=0
        mov bl, byte ptr [si]       ; read ascii code of number to bl
        cmp bl, "."                 ; if this char is . 
        JE floatdotfound
        cmp bl, "0"         
        JB error2out                ; if < '0'
        cmp bl, "9"
        JA error2out                ; if > '9'
        push dx
        mul ten                     ; shift one digit to the left by multiplying by 10 
        pop dx
        sub bl, "0"                 ; ascii code to digit
        add ax, bx                  ; in bx current number is stored
        inc si                      ; si+=1
        inc cl                      ; cl+=1, I know how many chars were before .
    JMP leftloop
        
    floatdotfound:
        mov word ptr [leftFloat], ax; write left side of number
        inc si                      ; si+=1, points to first number after dot
        inc cl                      ; cl+=1
        
        cmp cl, 2d                  ; if dot was first char throw error
        JB error2out    
        cmp cl, ch                  ; if dot was last char throw error
        JE error2out
        
        xor ax, ax                  ; ax=0
        xor bx, bx                  ; bx=0
        
    rightloop:          
        push bx                     ; store bx on stack for later processing
        xor bx, bx                  ; bx=0
        mov bl, byte ptr [si]       ; read to bl number ascii code
        cmp bl, "0"         
        JB error2out                ; if < '0'
        cmp bl, "9"
        JA error2out                ; if > '9'
        push dx                     ; dx will be used when dividing 
            mul ten                 ; multiply ax by ten for shifting
        pop dx
        sub bl, "0"                 ; ascii code to digit
        add ax, bx                  ; in bx current number is stored 
        inc si                      ; si+=1
        pop bx                      ; get bx back for saving length after dot
        inc bx                      ; bx+=1
        inc cl
        cmp cl, ch                  
    JB rightloop
        
        mov word ptr [rightFloat], ax; write right side of number
        
        mov cx, bx                  ; divide right side for bx times by 10.0
    
        fild [leftFloat]            ; st(2) - integer part of number
        fild [ten]                  ; st(1) - 10.0
        fild [rightFloat]           ; st(0) - float part of number
        
    tofloatloop:
        fdiv st(0), st(1)           ; st(0) = st(0)/st(1)
        loop tofloatloop
        
        fadd st(0), st(2)           ; st(0) = st(0)+st(2)   add float and integer part
        
        cmp dl, 1d                  ; check if numer was negative
        JNE savenumber
        fchs                        ; number*=-1
    savenumber:
        fstp qword ptr [float]      ; store and pop st(0) 
        
        fsubp st(0), st(0)          ; free coprocesor stack
        fsubp st(0), st(0)          
        JMP exitstrtofloat
        
    error2out:
        mov dx, offset error2
        call errorhandler
    
    exitstrtofloat:     
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
ret

;###################################          Mandelbrot set          ################################### 

drawmandelbrotset:                  ; calculates points, draws mandelbrot set in video mode

    call entervideomode
    call getconstants
    call drawset
    call waitforkeystroke           
    call quitvideomode
ret

entervideomode:                     ; change to video mode

    push ax
    xor ah, ah
    mov al, 13d                     
    int 10h
    pop ax
ret

drawset:                            ; draws mandelbrot set based on calculated p, q and pixel value

    pusha
    pushf
    
    xor cx, cx                      ; set coordinates to (0,0)
    xor dx, dx
    
    loopY:
        cmp dx, 200d                ; if dx at end of column
        JE finishdrawing
        loopX:
            cmp cx, 320d            ; if cx at end of row
            JE endofrow             
            mov ax, cx
            call calculatep
            mov ax, dx
            call calculateq
            call calculatepixel
            cmp al, 1d
            JB black                ; if AL==0 draw black pixel
            mov al, 0Fh             ; set pixel color to white
            JMP drawpixel
        black:
            xor al, al              ; set pixel color to black
        drawpixel:
            mov ah, 0Ch
            int 10h
            inc cx
            JMP loopX               ; next pixel in row
        endofrow:
            xor cx, cx              ; back to first pixel in row
            inc dx                  ; dx+=1, go to next column
    JMP loopY
    
    finishdrawing:
    popf
    popa
ret


getconstants:
    
    fld qword ptr [xmax]            ; st(3) - xMax
    fld qword ptr [xmin]            ; st(2) - xMin
    fild word ptr [xResolution]     ; st(1) - horizontal resolution
    fldz

    fadd st(0), st(3)
    fsub st(0), st(2)
    fdiv st(0), st(1)
    fstp qword ptr [pconst]

    fstp st(0)
    fstp st(0)
    fstp st(0)
    fstp st(0)

    fld qword ptr [ymax]
    fld qword ptr [ymin]
    fild word ptr [yResolution]
    fldz

    fadd st(0), st(3)
    fsub st(0), st(2)
    fdiv st(0), st(1)
    fstp qword ptr [qconst]

    fstp st(0)
    fstp st(0)
    fstp st(0)
    fstp st(0)
ret


calculatep:                         ; input - in ax, coordinate, saves calculated value to memory
        
    mov word ptr wordBuffer, ax     ; write from ax to buffer, can't move directly from CPU to FPU
    fld qword ptr [xmin]            ; st(3) - xMin
    fld qword ptr [pconst]
    fild word ptr [wordBuffer]      ; st(1) - coordinate
    fldz                            ; st(0) - p=0
    
    fadd st(0), st(1)
    fmul st(0), st(2)
    fadd st(0), st(3)
    
    fstp qword ptr [p]              ; write p to memory and pop

    fsubp st(0), st(0)              ; free coprocessor stack
    fsubp st(0), st(0)
    fsubp st(0), st(0)
ret

calculateq:                         ; input - in ax, coordinate, saves calculated value to memory
    
    mov word ptr wordBuffer, ax     ; write from ax to buffer, can't move directly from CPU to FPU
    fld qword ptr [ymin]            ; st(3) - ymin
    fld qword ptr [qconst]
    fild word ptr [wordBuffer]      ; st(1) - coordinate
    fldz                            ; st(0) - q=0
    
    fadd st(0), st(1)
    fmul st(0), st(2)
    fadd st(0), st(3)
    
    fstp qword ptr [q]              ; write q to memory and pop

    fsubp st(0), st(0)              ; free coprocessor stack, could use fstp st(0)
    fsubp st(0), st(0)
    fsubp st(0), st(0)              ; coprocessor stack is empty
ret

calculatepixel:                     ; out - result in al

    push cx
    pushf

    fld qword ptr [p]               ; st(7) = p
    fld qword ptr [q]               ; st(6) = q
    fldz                            ; st(5) = 0 - for tmp
    fldz                            ; st(4) = 0 - for x
    fldz                            ; st(3) = 0 - for x*x
    fldz                            ; st(2) = 0 - for y
    fldz                            ; st(1) = 0 - for y*y
    fldz                            ; st(0) = 0 - for additional tmp variable
    
    mov cx, 1000d                   ; set loop counter for 1000 iterations
    
    calculateloop:
    ; tmp
    fsub st(0), st(0)               ; st(0) = 0
    fadd st(0), st(3)               ; st(0) = x*x
    fsub st(0), st(1)               ; st(0) = x*x-y*y
    fadd st(0), st(7)               ; st(0) = x*x-y*y+p
    fxch st(5)                      ; tmp = st(5) = x*x-y*y+p
    ; y
    fsub st(0), st(0)               ; st(0) = 0
    fadd st(0), st(4)               ; st(0) = x
    fmul st(0), st(2)               ; st(0) = x*y
    fadd st(0), st(0)               ; st(0) = 2*x*y
    fadd st(0), st(6)               ; st(0) = 2*x*y+q
    fxch st(2)                      ; st(2) = y =2*x*y+q
    ; x
    fsub st(0), st(0)               ; st(0) = 0
    fadd st(0), st(5)               ; st(0) = tmp
    fxch st(4)                      ; st(4) = x = tmp
    ; x*x + y*y
    fsub st(0), st(0)
    fadd st(0), st(4)               ; st(0) = x
    fmul st(0), st(0)               ; st(0) = x*x
    fxch st(3)                      ; st(3) = x*x
    fsub st(0), st(0)               ; st(0) = 0
    fadd st(0), st(2)               ; st(0) = y
    fmul st(0), st(0)               ; st(0) = y*y
    fxch st(1)                      ; st(1) = y*y
    fsub st(0), st(0)               ; st(0) = 0
    fadd st(0), st(3)               ; st(0) = x*x
    fadd st(0), st(1)               ; st(0) = x*x+y*y
    ; check exit condition
    fcom [max]
    fstsw ax                        ; write result to ax
    sahf                            ; from ah to flag register
    JA breakloop                    ; if st(0) > max break loop
    loop calculateloop              
    
    mov al, 1d                      ; all iterations performed
    JMP calculationexit

    breakloop:
    mov al, 0d                      ; <1000 iterations performed
    
    calculationexit:
    fsubp st(0), st(0)              ; free coprocessor stack
    fsubp st(0), st(0)
    fsubp st(0), st(0)
    fsubp st(0), st(0)
    fsubp st(0), st(0)
    fsubp st(0), st(0)
    fsubp st(0), st(0)
    fsubp st(0), st(0)

    popf
    pop cx
ret

waitforkeystroke:                   ; waits for keystroke

    push ax
    xor ax, ax
    int 16h
    pop ax
ret

quitvideomode:                      ; back from video mode
    push ax
    xor ah, ah
    mov al, 3d                      
    int 10h
    pop ax
ret

;###################################          Error handling          ###################################

errorhandler:                       ; args - error offset in dx

    call print
    call endsection
ret

;###################################          Print and exit          ################################### 

print:                              ;print variable from data segment
    mov ax, seg data                
    mov ds, ax                      ; move data segment adress to ds 
    mov ah, 9h                      ; interruption argument 9h (write)
    int 21h                         
ret 

endsection:                         ; end 
    mov ah, 04ch                    ; iterruption argument 4ch (quit)
    int 21h 
ret

code1 ends

;-----------------------------------              Stack               ----------------------------------- 
stos1 segment stack
    dw 250  dup (?)
    w_stos  dw  ?
stos1 ends

end start