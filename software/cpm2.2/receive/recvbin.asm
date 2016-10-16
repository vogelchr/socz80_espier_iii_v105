;
; WRS: z80asm receive.asm -o r2.com ; sendmany r2.com
; then on the target do era receive.com; ren receive.com=r2.com
;

cpm:           equ 0x0005
tfcb:          equ 0x5c
dmabuf:        equ 0x80
dmalen:        equ 0x80
cwritestr:     equ 0x09   ; write string pointed to by DE
cwritechar:    equ 0x02   ; write character in E (with output mangling)
crawio:        equ 0x06   ; raw console I/O
fopen:         equ 15     ; open FCB
fdelete:       equ 19     ; delete FCB
fcreate:       equ 22     ; create FCB
fclose:        equ 0x10   ; close FCB
fwrite:        equ 0x15   ; write record
MMU_SELECT:    equ 0xF8   ; page table entry selector
MMU_PERM:      equ 0xFB   ; permission bits [6 unused bits] [WRITE] [READ]
MMU_FRAMEHI:   equ 0xFC   ; high byte of frame number
MMU_FRAMELO:   equ 0xFD   ; low byte of frame number
UART0_STATUS:  equ 0x0000 ; [7: RX READY] [6: TX BUSY] [6 unused bits]
UART0_DATA:    equ 0x0001


        org 0x100 ; CP/M start TPA

        ld de, startstr
        ld c, cwritestr
        call cpm

        ; erase any existing file
        ld de, tfcb
        ld c, fdelete
        call cpm

        ; create new file
        ld de, tfcb
        ld c, fcreate
        call cpm
        cp 0xff
        jp z, openfail


nextblock:
        ld hl, dmabuf
        ; wait for framing byte
        call readraw
        cp '!' ; eof?
        jr z, maineof
        cp '@'
        jr nz, badframing
nextbyte:
        ; push hl
        call readraw
        ; pop hl
        ld (hl), a ; write byte

        ld a, (eof)
        cp 0
        jr nz, nomore

        inc hl
        ld a, l
        cp 0 ; reached 0x100?
        jr nz, nextbyte

writeblk:
        ld de, tfcb
        ld c, fwrite
        call cpm
        cp 0x00
        jr nz, writefail

        ld e, '.'
        ld c, cwritechar
        call cpm

        ld a, (eof)
        cp 0
        jr z, nextblock

closefile:
        ld de, tfcb
        ld c, fclose
        call cpm
        cp 0xff
        jp z, openfail ; not quite, but yeah

        ret

nomore:
        push hl
        ld e, '!'
        ld c, cwritechar
        call cpm
        pop hl
        ; if we've not started this block, we're done
        ld a, l
        cp 0x80
        jr z, closefile
padblock:
        ; otherwise finish off the final block with 00
        xor a
        ld (hl), a
        inc hl
        ld a, l
        cp 0 ; end of block?
        jr nz, padblock
        ; now write it out
        jr writeblk

maineof:
        ld a, 1
        ld (eof), a
        jr closefile

writefail:
        call outcharhex
        ld de, writefailstr
        call cpm
        rst 0

openfail:   ; opening file didn't work :(
        ld de, openfailstr
        ld c, cwritestr
        call cpm
        rst 0

badframing:
        ld de, lostframing
        ld c, cwritestr
        call cpm
        rst 0

openfailstr: db "openfail :(\r\n$"
startstr:  db "Writing file:\r\n$"
writefailstr: db " <-- write err\r\n$"
lostframing:  db "Lost framing.\r\n$"

; myfcb:
;         db 0    ; must be zero
;         db "FILENAMEBIN"
;         db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

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

readhexbyte:
        ; first nibble
        call readraw
        ; check eof
        cp '!'
        jr z, readdone
        ; first nibble in A
        cp 0x40 ; <= '9'?
        jr c, rbnum1
        sub 7
rbnum1: sub 0x30
        and 0xf
        ; A now contains the first nibble
        ; shift left four bits
        and a
        rla
        rla
        rla
        rla
        ld d, a ; stash
        ; second nibble
        call readraw
        ; check eof
        cp '!'
        jr z, readdone
        ; second nibble in A
        cp 0x40 ; <= '9'?
        jr c, rbnum2
        sub 7
rbnum2: sub 0x30
        and 0xf
        ; A now contains the second nibble
        or d ; merge in first nibble
        ret
readdone:
        ld a, 1
        ld (eof), a
        xor a
        ret

readraw:
        ; this bit does not use CP/M. bad me.
        in a, (UART0_STATUS)
        bit 7, a
        jr z, readraw
        in a, (UART0_DATA)
        ret

        ; end of file flag
eof:    db 0
