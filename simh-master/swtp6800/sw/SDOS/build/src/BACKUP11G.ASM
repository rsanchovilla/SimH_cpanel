dynamicstring   ; dynamic string starts here (overwrites code)
constructdynamicstring$
        pula
        pulb
        staa    returnaddress
        stab    returnaddress+1
        sts     0                      makes source compatable for '00 & '09
        ldaa    0
        ldab    1
        deca                           allocate stack for basic program
        subb    #dynamicstring&$ff     maxlen=(end of str)-(start of str)
        sbca    #dynamicstring/256
        ldx     #dynamicstring         starting string address
        andb    #$80                   ensure multiple of 128 bytes
        staa    0,x                    set max len
        stab    1,x
        ldaa    #$ff                   build string descriptor
        psha                           push string count that says 'all'
        psha
        stx     0
        ldaa    0
        ldab    1
        pshb                           push string address
        psha
        des                            push one garbage byte
        ldaa    #1
        psha                           push string descriptor type
        clc                            say 'no errors'
        ldx     returnaddress          go back to basic
        jmp     0,x

returnaddress   rmb     2
        end     .progstart
