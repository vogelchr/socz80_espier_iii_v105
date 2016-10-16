; a more complex (and maybe more thorough?) memory tester
; load and run at 0xf000
; assumes 0x0000--0x1000 is ROM and does not need testing
; assumes 0xf000--0xffff is reliable SRAM (program, stack located in here)
;
; *****************************************************************************
; NOTE: the ROM now re-arranges things so the above is no longer true, so this
; program will not run on current hardware without modification.
; *****************************************************************************
;
; baseline SDRAM controller (v2) -- tests 1--7; timer values 0x0769A30F 0x0769A309 -- approx 124.4 seconds
; single word cache controller   -- tests 1--7; timer values 0x06784E21            -- approx 108.5 seconds (15.9 second improvement!)
; single word with byte valid    -- tests 1--7; timer values 0x062DDF2A            -- approx 103.7 seconds (4.8 second improvement!)
; single word, fixed, direct feed from sdram;   timer values 0x062A6B51            -- approx 103.4 seconds (0.3 second improvement! But also full write-through caching)
; fancy cache                                                0x062A6B50            -- looks like this is the best we can do given that we write almost 4 times the cache size on each pass.
;
; $ z80asm ./dram_tester.asm && ../tools/write_mem a.bin e800 /dev/ttyUSB1

UART0_STATUS:  equ 0x0000 ; [7: RX READY] [6: TX BUSY] [6 unused bits]
UART0_DATA:    equ 0x0001
FIRSTPAGE:     equ 0x00   ; start test at address 0x0000
LASTPAGE:      equ 0xe0   ; stop test at address 0xe000

        org 0xe800 ; bottom of RAM

        ; announce ourselves
        ld de, hello_msg
        call outstring

        ; select upcounter latch value
        ld a, 0x11
        out (0x11), a

goagain:
        ; reset hardware timer
        ld a, 0x01
        out (0x11), a

        ; run each test in turn
        call ram_test7
        call outnewline

        call ram_test5
        call outnewline

        call ram_test1
        call outnewline

        call ram_test2
        call outnewline

        call ram_test3
        call outnewline

        call ram_test4
        call outnewline

        call ram_test6
        call outnewline

        ; latch hardware timer value
        ld a, 0x02
        out (0x11), a

        ; announce that we're done, print the latched timer value, then start over.
        ld de, alldone_msg
        call outstring
        in a, (0x17)
        call outcharhex
        in a, (0x16)
        call outcharhex
        in a, (0x15)
        call outcharhex
        in a, (0x14)
        call outcharhex
        call outnewline

        jr goagain ;; or rst 0 to terminate after single pass

;------------------------------------------------------------------------------
ram_test1:
        ld a, 0
        ld (testbyte), a
        ; announce our intentions
        ld de, ram_test1_start_msg
        call outstring
rt1_nextvalue:
        ld a, (testbyte)
        call outcharhex
        ld a, 0x20
        call outchar
        ld l, 0
        ld h, FIRSTPAGE
        ld a, (testbyte)
        ld c, a
rt1_wnextbyte:
        ld (hl), c
        inc hl
        ld a, h
        cp LASTPAGE
        jr nz, rt1_wnextbyte
        ; we've finished our write pass, now loop through and read each byte back
        ;; ld a, 0x00         ; TEST CORRUPTION
        ;; ld (0x6666), a     ; TEST CORRUPTION
        ld l, 0
        ld h, FIRSTPAGE
rt1_rnextbyte:
        ld a, (hl)
        cp c
        jp nz, fail
        inc hl
        ld a, h
        cp LASTPAGE
        jr nz, rt1_rnextbyte
        ; we've finished our read pass too, now test the next byte value
        ld a, c
        inc a
        ld (testbyte), a
        cp 0
        ret z ; tested all byte values: done
        jr rt1_nextvalue

;------------------------------------------------------------------------------
ram_test2:
        ld a, 0
        ld (testbyte), a
        ; announce our intentions
        ld de, ram_test2_start_msg
        call outstring
rt2_nextvalue:
        ld a, (testbyte)
        call outcharhex
        ld a, 0x20
        call outchar
        ld l, 0
        ld h, FIRSTPAGE
        ld a, (testbyte)
        ld b, a
rt2_wnextbyte:
        ld a, l
        xor b
        ld (hl), a
        inc hl
        ld a, h
        cp LASTPAGE
        jr nz, rt2_wnextbyte
        ; we've finished our write pass, now loop through and read each byte back
        ;; ld a, 0x66         ; TEST CORRUPTION
        ;; ld (0x6666), a     ; TEST CORRUPTION
        ld l, 0
        ld h, FIRSTPAGE
rt2_rnextbyte:
        ld a, l
        xor b
        ld c, a
        ld a, (hl)
        cp c
        jp nz, fail
        inc hl
        ld a, h
        cp LASTPAGE
        jr nz, rt2_rnextbyte
        ; we've finished our read pass too, now test the next byte value
        ld a, b
        inc a
        ld (testbyte), a
        cp 0
        ret z ; tested all byte values: done
        jr rt2_nextvalue

;------------------------------------------------------------------------------
ram_test3:
        ld a, 0
        ld (testbyte), a
        ; announce our intentions
        ld de, ram_test3_start_msg
        call outstring
rt3_nextvalue:
        ld a, (testbyte)
        call outcharhex
        ld a, 0x20
        call outchar
        ld l, 0
        ld h, FIRSTPAGE
        ld a, (testbyte)
        ld b, a
rt3_wnextbyte:
        ld a, h
        xor b
        ld (hl), a
        inc hl
        ld a, h
        cp LASTPAGE
        jr nz, rt3_wnextbyte
        ; we've finished our write pass, now loop through and read each byte back
        ;; ld a, 0x66         ; TEST CORRUPTION
        ;; ld (0x6666), a     ; TEST CORRUPTION
        ld l, 0
        ld h, FIRSTPAGE
rt3_rnextbyte:
        ld a, h
        xor b
        ld c, a
        ld a, (hl)
        cp c
        jp nz, fail
        inc hl
        ld a, h
        cp LASTPAGE
        jr nz, rt3_rnextbyte
        ; we've finished our read pass too, now test the next byte value
        ld a, b
        inc a
        ld (testbyte), a
        cp 0
        ret z ; tested all byte values: done
        jr rt3_nextvalue

;------------------------------------------------------------------------------
ram_test4:
        ld a, 0
        ld (testbyte), a
        ; announce our intentions
        ld de, ram_test4_start_msg
        call outstring
rt4_nextvalue:
        ld a, (testbyte)
        call outcharhex
        ld a, 0x20
        call outchar
        ld l, 0
        ld h, FIRSTPAGE
        ld a, (testbyte)
        ld b, a
rt4_wnextbyte:
        ld a, h
        xor l
        xor b
        ld (hl), a
        inc hl
        ld a, h
        cp LASTPAGE
        jr nz, rt4_wnextbyte
        ; we've finished our write pass, now loop through and read each byte back
        ;; ld a, 0x00         ; TEST CORRUPTION
        ;; ld (0x6666), a     ; TEST CORRUPTION
        ld l, 0
        ld h, FIRSTPAGE
rt4_rnextbyte:
        ld a, h
        xor l
        xor b
        ld c, a
        ld a, (hl)
        cp c
        jp nz, fail
        inc hl
        ld a, h
        cp LASTPAGE
        jr nz, rt4_rnextbyte
        ; we've finished our read pass too, now test the next byte value
        ld a, b
        inc a
        ld (testbyte), a
        cp 0
        ret z ; tested all byte values: done
        jr rt4_nextvalue

;------------------------------------------------------------------------------
ram_test5:
        ; announce our intentions
        ld de, ram_test5_start_msg
        call outstring
        ld l, 0
        ld h, FIRSTPAGE
rt5_nextpage:
        ld a, h
        call outcharhex
        ld a, 0x20
        call outchar
rt5_wnextbyte:
        ld b, 0
rt5_wnextval:
        ld (hl), b
        ld a, (hl)
        cp b
        jp nz, fail_expectb
        djnz rt5_wnextval
        inc hl
        ld a, h ; check if h=LASTPAGE?
        cp LASTPAGE
        ret z ; all done
        ld a, l ; check if l=0?
        cp 0
        jr nz, rt5_wnextbyte
        jr rt5_nextpage

;------------------------------------------------------------------------------
ram_test6:
        ld a, 0
        ld (testbyte), a
        ; announce our intentions
        ld de, ram_test6_start_msg
        call outstring
rt6_nextvalue:
        ld a, (testbyte)
        call outcharhex
        ld a, 0x20
        call outchar
        ld l, 0
        ld h, LASTPAGE
        ld a, (testbyte)
        ld b, a
rt6_wnextbyte:
        dec hl ; we start at LASTPAGE so we have to decrement one byte before we proceed
        ld a, h
        xor l
        xor b
        ld (hl), a
        ld a, h
        cp FIRSTPAGE
        jr nz, rt6_wnextbyte
        ; in the final page we watch for the bottom byte
        ld a, l
        cp 1
        jr nz, rt6_wnextbyte
        ; we've finished our write pass, now loop through and read each byte back
        ;; ld a, 0x00         ; TEST CORRUPTION
        ;; ld (0x6666), a     ; TEST CORRUPTION
        ld l, 0
        ld h, LASTPAGE
rt6_rnextbyte:
        dec hl
        ld a, h
        xor l
        xor b
        ld c, a
        ld a, (hl)
        cp c
        jp nz, fail
        ld a, h
        cp FIRSTPAGE
        jr nz, rt6_rnextbyte
        ld a, l
        cp 1
        jr nz, rt6_rnextbyte
        ; we've finished our read pass too, now test the next byte value
        ld a, b
        inc a
        ld (testbyte), a
        cp 0
        ret z ; tested all byte values: done
        jr rt6_nextvalue

;------------------------------------------------------------------------------
ram_test7:
        ; announce our intentions
        ld de, ram_test7_start_msg
        call outstring
        ld l, 0
        ld h, LASTPAGE
        dec hl
rt7_nextpage:
        ld a, h
        call outcharhex
        ld a, 0x20
        call outchar
rt7_wnextbyte:
        ld b, 0
rt7_wnextval:
        ld (hl), b
        ld a, (hl)
        cp b
        jp nz, fail_expectb
        djnz rt7_wnextval
        dec hl
        ld a, h
        cp FIRSTPAGE-1
        ret z ; all done
        ld a, l ; check if l=0?
        cp 0xff
        jr nz, rt7_wnextbyte
        jr rt7_nextpage



; called when we hit a problem, with failing address in HL
; "fail" should be called with A = value read from RAM
;                              C = value expected
; "fail_expectb" just copies B to C first
fail_expectb:
        ld c, b
fail:
        ld b, a
        push bc
        push bc
        ld de, failmsg
        call outstring
        ld a, h
        call outcharhex
        ld a, l
        call outcharhex
        ld de, failmsg2
        call outstring
        pop bc
        ld a, b
        call outcharhex
        ld de, failmsg3
        call outstring
        pop bc
        ld a, c
        call outcharhex
        call outnewline
        rst 0 ; drop back to the monitor

; outstring: Print the string at (DE) until 0 byte is found
; destroys: AB DE
outstring:
        ld a, (de)     ; load next character
        and a          ; test if zero
        ret z          ; return when we find a 0 byte
        call outchar
        inc de         ; next char please
        jr outstring

; output a newline
outnewline:
        ld a, 0x0d   ; output newline
        call outchar
        ld a, 0x0a
        call outchar
        ret

; print the byte in A as a two-character hex value
; destroys ABC
outcharhex:
        ld c, a  ; copy value
        ; print the top nibble
        rra
        rra
        rra
        rra
        call outnibble
        ; print the bottom nibble
        ld a, c
        call outnibble
        ret

; print the nibble in the low four bits of A
outnibble:
        and 0x0f ; mask off low four bits
        cp 10
        jr c, numeral ; less than 10?
        add 0x07 ; start at 'A' (10+7+0x30=0x41='A')
numeral:add 0x30 ; start at '0' (0x30='0')
        call outchar
        ret

; outchar: Wait for UART TX idle, then print the char in A
; destroys: AB
outchar:
    ld b, a
    ; wait for transmitter to be idle
ocloop: in a, (UART0_STATUS)
    bit 6, a
    jr nz, ocloop   ; loop while busy
    ; now output the char to serial port
    ld a, b
    out (UART0_DATA), a
    ret

testbyte:
        db 0
hello_msg:
        db "RAM tester (2013-10-27 WRS)\r\n", 0
alldone_msg:
        db "Full pass completed: All OK. Timer 0x", 0
ram_test1_start_msg:
        db "Test 1: Constant value ", 0
ram_test2_start_msg:
        db "Test 2: Low address byte XOR ", 0
ram_test3_start_msg:
        db "Test 3: High address byte XOR ", 0
ram_test4_start_msg:
        db "Test 4: Both address bytes XOR ", 0
ram_test5_start_msg:
        db "Test 5: Write then immediate readback page ", 0
ram_test6_start_msg:
        db "Test 6: Reverse order, both address bytes XOR ", 0
ram_test7_start_msg:
        db "Test 7: Reverse order, write then immediate readback page ", 0
failmsg:
        db "*** IT HURTS AT ADDRESS ", 0
failmsg2:
        db ": READ=", 0
failmsg3:
        db ", EXPECTED=", 0
