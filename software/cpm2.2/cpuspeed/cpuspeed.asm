;
; WRS: zmac cpuspeed.asm && mv zout/cpuspeed.cim cpuspeed.com; rm -rf zout; ~/projects/socz80/software/cpm2.2/receive/sendmany cpuspeed.com
;

; this worked out more complex than I had expected!
; usage:
;     CPUSPEED      -- report current CPU speed
;     CPUSPEED nn   -- set CPU clock modulation register to nn, report new CPU speed

cpm            equ 0x0005
tfcb           equ 0x5c
dmabuf         equ 0x80
dmalen         equ 0x80
cwritestr      equ 0x09   ; write string pointed to by DE
cwritechar     equ 0x02   ; write character in E (with output mangling)
crawio         equ 0x06   ; raw console I/O
fopen          equ 15     ; open FCB
fdelete        equ 19     ; delete FCB
fcreate        equ 22     ; create FCB
fclose         equ 0x10   ; close FCB
fwrite         equ 0x15   ; write record
CPU_CLKMOD     equ 0xF0   ; clock modulation register
MMU_SELECT     equ 0xF8   ; page table entry selector
MMU_PERM       equ 0xFB   ; permission bits [6 unused bits] [WRITE] [READ]
MMU_FRAMEHI    equ 0xFC   ; high byte of frame number
MMU_FRAMELO    equ 0xFD   ; low byte of frame number
UART0_STATUS   equ 0x0000 ; [7  RX READY] [6  TX BUSY] [6 unused bits]
UART0_DATA     equ 0x0001

        org 0x100 ; CP/M start TPA
start:

        ld hl, tfcb
        inc hl ; skip first byte
        ld a, (hl)
        cp 0x20
        jr z, nospeed
        call parsehex
        ; desired speed now in register DE

        ; sanity check requested speed
        ld a, d
        cp 0
        jp nz, fail

        push de
        ld de, startstr
        ld c, cwritestr
        call cpm
        pop de

        call outde
        call crlf

        jr gospeed

nospeed:
        ; no speed specified on command line -- just report current speed
        ld d, 0
        in a, (CPU_CLKMOD)
        ld e, a

gospeed:
        push de ; stash requested speed
        ; compute resulting speed: 128,000 / (e+1) kHz
        push de
        ld de, speedstr
        ld c, cwritestr
        call cpm
        pop de
        ld a, e
        ; this is a bit dodgy; for high speeds we have a fixed table, for lower speeds
        ; we just compute it
        cp 0
        jr z, 128mhz
        ; for 32mhz and below we can compute it correctly.
        inc de ; DE=e+1
        ; set HLBC = 0x00 01 f4 00
        ld h, 0x00
        ld l, 0x01
        ld b, 0xf4
        ld c, 0x00
        call div32by16
        ; BC now contains resulting speed (in Hz), remainder in HL
        push bc
        pop hl ; move number into HL
        ld de, strbuf
        call Num2Dec
        ; strbuf now contains a 5 character string eg "02000" for 2.000MHz
        ld a, (strbuf)
        cp '0'
        jr z, c1
        call outchar
c1:     ld a, (strbuf+1)
        call outchar
        ld a, '.'
        call outchar
        ld a, (strbuf+2)
        call outchar
        ld a, (strbuf+3)
        call outchar
        ld a, (strbuf+4)
        call outchar
        jr donespeed
128mhz: ld de, 128mstr
        ld c, cwritestr
        call cpm
donespeed:
        ld de, mhzstr
        ld c, cwritestr
        call cpm
        call crlf

        ; program requested CPU speed
        pop de ; restore requested speed
        ld a, e
        out (CPU_CLKMOD), a

        ; announce success
        ld de, donestr
        ld c, cwritestr
        call cpm
        call crlf
        ; return to OS
        ret

fail:   ; oh dear, it went wrong
        ld de, failstr
        ld c, cwritestr
        call cpm
        call crlf
        ; return to OS
        ret

startstr: db "Setting CPU clock modulation register to $"
speedstr: db "CPU speed is now $"
mhzstr:   db " MHz$"
128mstr:  db "128.000$"
failstr:  db "Bad CPU speed requested (must be an 8-bit hex value).$"
donestr:  db "Done.$"
strbuf:   db "$$$$$$$$$$"

crlf:
        ld a, 13
        call outchar
        ld a, 10
        call outchar
        ret

outbc:
        ld a, b
        call outcharhex
        ld a, c
        call outcharhex
        ret
outde:
        ld a, d
        call outcharhex
        ld a, e
        call outcharhex
        ret
outhl:
        ld a, h
        call outcharhex
        ld a, l
        call outcharhex
        ret

outcharhex: ; print byte in A as two hex nibbles
        ; ld c, a  ; copy value
        push bc
        push af
        ; print the top nibble
        rra
        rra
        rra
        rra
        call outnibble
        ; print the bottom nibble
        pop af
        ; ld a, c
        call outnibble
        pop bc
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

outchar:
        push de
        push bc
        push hl
        push af
        ld e, a
        ld c, cwritechar
        call cpm
        pop af
        pop hl
        pop bc
        pop de
        ret

parsehex:
        ld d, 0
        ld e, 0
parsemorehex:
        ld a, (hl)
        cp 0x30 ; not a hex character?
        ret c
        ; convert to nibble
        cp 0x40 ; <= '9'?
        jr c, phnumeral
        sub 7
phnumeral: sub 0x30
        and 0xf
        ; A now contains the nibble at (hl) ie bits are 0000nnnn
        ; now we do:
        ; D = D << 4 | E << 4
        ; E = E << 4 | A
        ; remember rl shifts 9 bits total; 8 bits in the register plus the carry bit from flags register
        push bc ; don't clobber B
        ld b, 4 ; prepare to do this four times
phshift:
        and a   ; clear carry flag (so we shift zeros into the low four bits of E)
        rl e    ; rotate C register left one bit, top bit moves into carry flag
        rl d    ; rotate B register left one bit, carry flag moves into lower bit
        djnz phshift ; repeat four times please
        pop bc
        or e    ; combine C and A
        ld e, a ; move A into C
        inc hl
        jr parsemorehex

;;
;;next:   ld a, (hl)
;;        ld e, a
;;        ld c, cwritechar
;;        push af
;;        push hl
;;        call cpm
;;        pop hl
;;        pop af
;;        cp 0x20
;;        jr z, done
;;        inc hl
;;        jr next
;;done:   ld (hl), 0
;;        ; return to OS
;;        ret
;;        

; Integer divides HLBC by DE, returns result in BC and remainder in HL
; Clobbers A, F, IYH
; Found on Usenet
div32by16:
        ld iyh,16
div1:   rl c
        rl b
        adc hl,hl
        sbc a,a
        or a
        sbc hl,de
        sbc a,0
        jr nc,div2
        add hl,de
        scf
div2:   dec iyh
        jp nz,div1
        rl c
        rl b
        ld a,c
        cpl
        ld c,a
        ld a,b
        cpl
        ld b,a
        ret

; Input: HL = number to convert, DE = location of ASCII string
; Output: ASCII string at (DE)
Num2Dec:ld  bc,-10000
        call    Num1
        ld  bc,-1000
        call    Num1
        ld  bc,-100
        call    Num1
        ld  c,-10
        call    Num1
        ld  c,b
Num1:   ld  a,'0'-1
Num2:   inc a
        add hl,bc
        jr  c,Num2
        sbc hl,bc
        ld  (de),a
        inc de
        ret

        end start

