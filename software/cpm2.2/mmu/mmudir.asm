; version of MMU.ASM which just writes direct to the UART

cpm:           equ 0x0005
cwritestr:     equ 0x09   ; write string pointed to by DE
cwritechar:    equ 0x02   ; write character in E (with output mangling)
MMU_SELECT:    equ 0xF8   ; page table entry selector
MMU_PERM:      equ 0xFB   ; permission bits [6 unused bits] [WRITE] [READ]
MMU_FRAMEHI:   equ 0xFC   ; high byte of frame number
MMU_FRAMELO:   equ 0xFD   ; low byte of frame number
UART0_STATUS:  equ 0x00   ; [7: RX READY] [6: TX BUSY] [6 unused bits]
UART0_DATA:    equ 0x01

        org 0x100 ; CP/M start TPA

        ld e, '!'
        call outchar
        ld e, 13
        call outchar
        ld e, 10
        call outchar

nextframe:
        ; print info on this frame
        ld a, (mmu_frame)
        out (MMU_SELECT), a
        call outcharhex

        ld e, '='
        call outchar

        in a, (MMU_PERM)
        call outcharhex

        ld e, ','
        call outchar
        ld e, ' '
        call outchar

        in a, (MMU_FRAMEHI)
        call outcharhex

        in a, (MMU_FRAMELO)
        call outcharhex

        ld e, '\r'
        call outchar
        ld e, '\n'
        call outchar

        ; setup for next
        ld a, (mmu_frame)
        inc a
        ld (mmu_frame), a
        cp 16
        jr nz, nextframe

        rst 0 ; done

startstr:  db "MMU state:\r\n$"
mmu_frame: db 0

outchar: ; print single char in register E
dbgout:     ; wait tx idle
    in a, (UART0_STATUS)
    bit 6, a
    jr nz, dbgout
    ; GO GO
    ld a, e
    out (UART0_DATA), a
dbgwait:    ; wait tx idle again
    in a, (UART0_STATUS)
    bit 6, a
    jr nz, dbgwait
    ret


outcharhex: ; print byte in A as two hex nibbles
        ; ld c, a  ; copy value
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
        ret

; print the nibble in the low four bits of A
outnibble:
        and 0x0f ; mask off low four bits
        cp 10
        jr c, numeral ; less than 10?
        add 0x07 ; start at 'A' (10+7+0x30=0x41='A')
numeral:add 0x30 ; start at '0' (0x30='0')
        ld e, a
        call outchar
        ret

