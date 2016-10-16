;
; WRS: z80asm trapdoor.asm -o trapdoor.com ; sendmany trapdoor.com
;
; A CP/M program which attempts to escape to the ROM while preserving
; machine state. Useful for debugging. It works by copying itself to
; 0x1000, remapping the ROM in at 0x0000 through 0x1000 as it would
; be at power on, and then running the ROM. This is not ideal because
; the ROM then puts the first page of SDRAM back in address zero. It
; would be better to map the ROM at 0xF000 and jump into the ROM at
; the correct location for it to leave the MMU alone.
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

        jp letsgo ; keep UZI happy
letsgo:

        ; copy ourselves from 0x100 up to 0x1000
copystart:
        di
        ld de, 0x1000
        ld hl, 0x100
        ld bc, copyend-copystart
        ldir
        jp page1
        org 0x1000+$-0x100
page1:  xor a
        out (MMU_SELECT), a
        out (MMU_FRAMELO), a
        ld a, 0x20
        out (MMU_FRAMEHI), a
        jp 0x0000
copyend:
