; a YET MORE complex (and perhaps maybe more thorough?) memory tester
; load and run at 0xe800
; assumes 0xf000--0xffff is ROM and does not need testing
; assumes 0xe800--0xefff is reliable SRAM (program, stack located in here)
;
; baseline SDRAM controller (v2) -- tests 1--7; timer values                       -- approx ???.? seconds
; single word cache controller   -- tests 1--7; timer values                       -- approx ???.? seconds
; single word with byte valid    -- tests 1--7; timer values                       -- approx ???.? seconds
; single word, fixed, direct feed from sdram, code/stack in SRAM;   0x12A20763 0x12A20764 -- approx 312.6 seconds
; single word, fixed, direct feed from sdram, code/stack in DRAM;   0x25A8EB98            -- approx 631.8 seconds
; cache disabled, code/stack in DRAM;                               0x3CE8931F            -- approx 1021.9 seconds
; fancy 4K word cache, code/stack in SRAM                           0x12671A14            -- approx 308.8 seconds
; fancy 4K word cache, code/stack in DRAM                           0x129C5994            -- approx 312.2 seconds (takes 30.5% of the time of uncached equivalent)
; fancy 4K word cache, code/stack in SRAM, 128MHz(!)                0x0C3B6AAF            -- approx 205.2 seconds (takes 30.5% of the time of uncached equivalent)
; to get reproducible results just map 0xE000 as SRAM (physical 2001 is a page of SRAM) 
;
; $ z80asm ./dram_tester2.asm && ../../tools/write_mem a.bin e800 /dev/ttyUSB1

UART0_STATUS:  equ 0x0000 ; [7: RX READY] [6: TX BUSY] [6 unused bits]
UART0_DATA:    equ 0x0001
FIRSTPAGE:     equ 0x00   ; start test at address 0x0000 (has to be multiple of 0x10)
LASTPAGE:      equ 0xe0   ; stop test at address 0xe800 (has to be multiple of 0x10)

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

        ; run each test in turn, these tests use the standard calling method.
        call ram_test1
        call ram_test2
        call ram_test3
        call ram_test4
        
        ; these two are unmodified from the previous iteration of the tester so they don't print newlines automagically
        call ram_test5
        call outnewline
        call ram_test5r
        call outnewline

        ; fancypants PRNG tests ("WE GET THE MESSAGE, YOU ENJOY TESTING RAM")
        call ram_test6
        call ram_test7

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
        ld de, ram_test1_start_msg
        ld ix, test1_compute
        call testram_both
        ret

test1_compute:
        ld a, d
        jp (iy)

;------------------------------------------------------------------------------
ram_test2:
        ld de, ram_test2_start_msg
        ld ix, test2_compute
        call testram_both
        ret

test2_compute:
        ld a, l
        xor d
        jp (iy)

;------------------------------------------------------------------------------
ram_test3:
        ld de, ram_test3_start_msg
        ld ix, test3_compute
        call testram_both
        ret

test3_compute:
        ld a, h
        xor d
        jp (iy)

;------------------------------------------------------------------------------
ram_test4:
        ld de, ram_test4_start_msg
        ld ix, test3_compute
        call testram_both
        ret

test4_compute:
        ld a, h
        xor l
        xor d
        jp (iy)

testram_both:
        ; DE contains the name of the test
        push de
        push de
        ld de, forward_msg
        call outstring
        pop de ; name of test
        call outstring
        call testram
        call outnewline
        ld de, reverse_msg
        call outstring
        pop de ; name of test
        call outstring
        call rtestram
        call outnewline
        ret

; FORWARD ORDER
; test all bytes in RAM, writing one 4K page and then reading it back
; IX: points to address of function to calculate expected value at a given address
; IY: points to return address for expected value calcuation function
; HL: points to address in RAM being tested
;  D: seed value (ie current cycle, full test cycle run for each value 0 ... 255)
testram:
        ld d, 0
nextseed:
        ; announce the seed value
        ld a, d
        call outcharhex ; destroys ABC
        ld a, 0x20
        call outchar

        ; setup for the first page
        ld h, FIRSTPAGE
        ld l, 0

nextpage:
        ; set IY to our call return address
        ld iy, retw

        ; write 0x1000 bytes
next256w:
        ld b, 0
nextw:   
        ; compute byte value
        jp (ix)
retw:
        ld (hl), a ; write memory
        inc hl
        djnz nextw
        ; we've just been through our loop 256 times
        ; check if we've finished our page, ie HL & 0x0FFF == 0
        ld a, h
        and 0x0f
        or l
        jr nz, next256w
        ; finished writing this page

        ; We've written the whole page. Now read it back.
        ; First rewind HL to the start of the page.
        ld a, h  
        sub 16
        ld h, a
        ;
        ld iy, retr

        ; read 0x1000 bytes
next256r:
        ld b, 0
nextr:   
        ; compute byte value
        jp (ix) ; poor man's CALL (IX)
retr:
        ; expected value is now in A
        ld c, (hl)   ; read memory into C
        cp c         ; compare A (expected) and C (read)
        jp nz, fail
        inc hl
        djnz nextr
        ; we've just been through our loop 256 times
        ; check if we've finished our page, ie HL & 0x0FFF == 0
        ld a, h
        and 0x0f
        or l
        jr nz, next256r
        ; finished reading this page

        ; now check if we've done the last page
        ld a, LASTPAGE
        cp h
        jr nz, nextpage ; not at LASTPAGE yet? do the next page.

        ; full pass completed
        inc d      ; next seed
        ld a, d
        cp 0       ; tested all values?
        ret z      ; done
        jr nextseed

; REVERSE ORDER
; test all bytes in RAM, writing one 4K page and then reading it back
; IX: points to address of function to calculate expected value at a given address
; IY: points to return address for expected value calcuation function
; HL: points to address in RAM being tested
;  D: seed value (ie current cycle, full test cycle run for each value 0 ... 255)
rtestram:
        ld d, 0
rnextseed:
        ; announce the seed value
        ld a, d
        call outcharhex ; destroys ABC
        ld a, 0x20
        call outchar

        ; setup for the last page
        ld h, LASTPAGE
        ld l, 0

rnextpage:
        ; set IY to our call return address
        ld iy, rretw

        ; write 0x1000 bytes
rnext256w:
        ld b, 0
rnextw:   
        ; compute byte value
        dec hl
        jp (ix)
rretw:
        ld (hl), a ; write memory
        djnz rnextw
        ; we've just been through our loop 256 times
        ; check if we've finished our page, ie HL & 0x0FFF == 0
        ld a, h
        and 0x0f
        or l
        jr nz, rnext256w
        ; finished writing this page

        ; We've written the whole page. Now read it back.
        ; First rewind HL to the start of the page.
        ld a, h  
        add 16
        ld h, a
        ;
        ld iy, rretr

        ; read 0x1000 bytes
rnext256r:
        ld b, 0
rnextr:   
        ; compute byte value
        dec hl
        jp (ix) ; poor man's CALL (IX)
rretr:
        ; expected value is now in A
        ld c, (hl)   ; read memory into C
        cp c         ; compare A (expected) and C (read)
        jp nz, fail
        djnz rnextr
        ; we've just been through our loop 256 times
        ; check if we've finished our page, ie HL & 0x0FFF == 0
        ld a, h
        and 0x0f
        or l
        jr nz, rnext256r
        ; finished reading this page

        ; now check if we've done the last page
        ld a, FIRSTPAGE
        cp h
        jr nz, rnextpage ; not at FIRSTPAGE yet? do the next page.

        ; full pass completed
        inc d      ; next seed
        ld a, d
        cp 0       ; tested all values?
        ret z      ; done
        jr rnextseed


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
        jp nz, fail_reada_expectb
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
ram_test5r:
        ; announce our intentions
        ld de, ram_test5r_start_msg
        call outstring
        ld l, 0
        ld h, LASTPAGE
        dec hl
rt5r_nextpage:
        ld a, h
        call outcharhex
        ld a, 0x20
        call outchar
rt5r_wnextbyte:
        ld b, 0
rt5r_wnextval:
        ld (hl), b
        ld a, (hl)
        cp b
        jp nz, fail_reada_expectb
        djnz rt5r_wnextval
        dec hl
        ld a, h
        cp FIRSTPAGE-1
        ret z ; all done
        ld a, l ; check if l=0?
        cp 0xff
        jr nz, rt5r_wnextbyte
        jr rt5r_nextpage

;------------------------------------------------------------------------------
; pseudorandom write, linear read test
ram_test6:
        ld de, ram_test6_start_msg
        call outstring
        ld b, 0
rt6_nextpass:
        push bc ; protect B
        ld a, b
        call outcharhex
        ld a, 0x20
        call outchar
        pop bc
        ; scribble
        call writeprng
        ; read check
        push bc
        ld d, b ; reader expects pass number in D
        ld h, FIRSTPAGE
        ld l, 0
        ; code below here until the "pop bc" is adapted version of the combined tester above
pnext256r:
        ld b, 0
pnextr:   
        ld a, h
        xor l
        xor d
        ; expected value is now in A
        ld c, (hl)   ; read memory into C
        cp c         ; compare A (expected) and C (read)
        jp nz, fail
        inc hl
        djnz pnextr
        ; we've just been through our loop 256 times
        ; check if we've finished
        ld a, LASTPAGE
        cp h
        jr nz, pnext256r
        ; --- end of read test
        pop bc
        inc b
        ld a, b
        cp 0
        jr nz, rt6_nextpass
        call outnewline
        ret

;------------------------------------------------------------------------------
; pseudorandom write, pseudorandom read read test
ram_test7:
        ld de, ram_test7_start_msg
        call outstring
        ld b, 0
rt7_nextpass:
        push bc ; protect B
        ld a, b
        call outcharhex
        ld a, 0x20
        call outchar
        pop bc
        ; scribble
        call writeprng
        ; read check
        call readprng
        ; also do a linear read JUST TO BE SURE!
        push bc
        ld d, b ; reader expects pass number in D
        ld h, FIRSTPAGE
        ld l, 0
        ; code below here until the "pop bc" is adapted version of the combined tester above
ppnext256r:
        ld b, 0
ppnextr:   
        ld a, h
        xor l
        xor d
        ; expected value is now in A
        ld c, (hl)   ; read memory into C
        cp c         ; compare A (expected) and C (read)
        jp nz, fail
        inc hl
        djnz ppnextr
        ; we've just been through our loop 256 times
        ; check if we've finished
        ld a, LASTPAGE
        cp h
        jr nz, ppnext256r
        ; --- end of read test
        pop bc
        inc b
        ld a, b
        cp 0
        jr nz, rt7_nextpass
        call outnewline
        ret

; Write the entire address space using a PRNG to generate the addresses.
; PRNG generates all 2^16 addresses, ending with 0.
; Each byte is written with both address bytes XOR B
; We filter out those addresses outside our test range
writeprng:
        call rand16
        ; HL contains the next PRN -- check it is in range
        ld a, h
        cp LASTPAGE-FIRSTPAGE
        jr nc, writeprng ; too big -- next
        ; shift HL up to the base of RAM
        ld a, h
        ld e, a ; stash a copy for comparison later
        add FIRSTPAGE
        ld h, a
        ; compute H^L^B (A=H already)
        xor l
        xor b
        ld (hl), a ; write RAM
        ; when we have been through all other combinations the PRNG generates 0 as the terminal value (ie, gets back to the seed)
        ld a, e ; stashed before we modified HL
        or l
        jr nz, writeprng
        ret 

; Read the entire address space using a PRNG to generate the addresses.
; PRNG generates all 2^16 addresses, ending with 0.
; Each byte is read, expecting both address bytes XOR B
; We filter out those addresses outside our test range
readprng:
        call rand16
        ; HL contains the next PRN -- check it is in range
        ld a, h
        cp LASTPAGE-FIRSTPAGE
        jr nc, readprng ; too big -- next
        ; shift HL up to the base of RAM
        ld a, h
        ld e, a ; stash a copy for comparison later
        add FIRSTPAGE
        ld h, a
        ; compute H^L^B (A=H already)
        xor l
        xor b
        ld c, (hl) ; read RAM
        cp c
        jr nz, fail
        ; when we have been through all other combinations the PRNG generates 0 as the terminal value (ie, gets back to the seed)
        ld a, e ; stashed before we modified HL
        or l
        jr nz, readprng
        ret 

; generate a 16-bit random number
; period is 2^16
; destroys A DE HL; unharmed BC IX IY
rand16: ld  de, 0x0000 ; seed value, this is the final value we generate; note this instruction is modified in place.
        ld  a,d
        ld  h,e
        ld  l,253
        or  a
        sbc hl,de
        sbc a,0
        sbc hl,de
        ld  d,0
        sbc a,d
        ld  e,a
        sbc hl,de
        jr  nc,rand
        inc hl
rand:   ld  (rand16+1),hl  ; modifies the code in place. these 8-bitters are crazy!!!
        ret

; this is like "fail" but with different registers
;   A = value read from RAM
;   B = value expected
;  HL = address of failure
fail_reada_expectb:
        ld c, a
        ld a, b
        ; just drop through into fail
; called when we hit a problem; "fail" should be called with
;   C = value read from RAM
;   A = value expected (copied to B)
;  HL = address of failure
fail:
        ld b, a  ; b = value expected
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
        ld a, c
        call outcharhex
        ld de, failmsg3
        call outstring
        pop bc
        ld a, b
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
        db "RAM tester (2013-10-29 WRS)\r\n", 0
alldone_msg:
        db "Full pass completed: All OK. Timer 0x", 0
forward_msg:
        db "Forward ", 0
reverse_msg:
        db "Reverse ", 0
ram_test1_start_msg:
        db "Test 1: Constant value ", 0
ram_test2_start_msg:
        db "Test 2: Low address byte XOR ", 0
ram_test3_start_msg:
        db "Test 3: High address byte XOR ", 0
ram_test4_start_msg:
        db "Test 4: Both address bytes XOR ", 0
ram_test5_start_msg:
        db "Forward Test 5: Write then immediate readback page ", 0
ram_test5r_start_msg:
        db "Reverse Test 5: Write then immediate readback page ", 0
ram_test6_start_msg:
        db "Pseudorandom write, linear read, both address bytes XOR ", 0
ram_test7_start_msg:
        db "Pseudorandom write, pseudorandom read, both address bytes XOR ", 0
failmsg:
        db "*** IT HURTS AT ADDRESS ", 0
failmsg2:
        db ": READ=", 0
failmsg3:
        db ", EXPECTED=", 0
