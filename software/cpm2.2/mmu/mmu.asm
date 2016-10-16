cpm:           equ 0x0005
cwritestr:     equ 0x09   ; write string pointed to by DE
cwritechar:    equ 0x02   ; write character in E (with output mangling)
MMU_SELECT:    equ 0xF8   ; page table entry selector
MMU_PERM:      equ 0xFB   ; permission bits [6 unused bits] [WRITE] [READ]
MMU_FRAMEHI:   equ 0xFC   ; high byte of frame number
MMU_FRAMELO:   equ 0xFD   ; low byte of frame number

        org 0x100 ; CP/M start TPA

        ld de, startstr
        ld c, cwritestr
        call cpm

nextframe:
        ; print info on this frame
        ld a, (mmu_frame)
        out (MMU_SELECT), a
        call outcharhex

        ld e, '='
        ld c, cwritechar
        call cpm

        ld a, (mmu_frame)
        out (MMU_SELECT), a
        in a, (MMU_PERM)
        call outcharhex

        ld e, ','
        ld c, cwritechar
        call cpm
        ld e, ' '
        ld c, cwritechar
        call cpm

        ld a, (mmu_frame)
        out (MMU_SELECT), a
        in a, (MMU_FRAMEHI)
        ld h, a
        in a, (MMU_FRAMELO)
        ld l, a

        ld a, h
        push hl
        call outcharhex
        pop hl
        ld a, l
        call outcharhex

        ld e, '\r'
        ld c, cwritechar
        call cpm
        ld e, '\n'
        ld c, cwritechar
        call cpm

        ; setup for next
        ld a, (mmu_frame)
        inc a
        ld (mmu_frame), a
        cp 16
        jr nz, nextframe

        rst 0 ; done

startstr:  db "MMU state:\r\n$"
mmu_frame: db 0

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
        ld c, cwritechar
        call cpm
        ret

