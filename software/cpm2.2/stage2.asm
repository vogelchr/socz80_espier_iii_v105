; stage1 is in the ROM; it loads the first 4096 bytes from the disk into RAM
; from 0x1000 upwards, then jumps to 0x1000, with the first physical page of 
; the boot disk in HL.

UART0_STATUS:  equ 0x0000 ; [7: RX READY] [6: TX BUSY] [6 unused bits]
UART0_DATA:    equ 0x0001
MMU_PAGESEL:   equ 0xF8
MMU_PERM:      equ 0xFB
MMU_FRAMEHI:   equ 0xFC
MMU_FRAMELO:   equ 0xFD

            org 0x1000 ; load address
start:      ; unmap the ROM and replace it with RAM
            ld a, 0xf
            out (MMU_PAGESEL), a
            out (MMU_FRAMELO), a
            xor a
            out (MMU_FRAMEHI), a
            ; ROM has left the building

            ; stash the boot disk first physical page address
            ld (diskpage), hl

            ; move our stack
            ld hl, stackptr
            ld sp, hl

            ; say hello
            ld hl, loadmsg
            call strout
            ld hl, (loadaddr)
            call outwordhex
            ld hl, frommsg
            call strout

            ; recover disk address
            ld hl, (diskpage)

nextpage:   inc hl                  ; OS image starts as diskpage+1
            call outwordhex         ; print page number
            ld c, 0x20              ; print space
            call conout
            ld a, 0x2               ; map disk page at 0x2000
            out (MMU_PAGESEL), a
            ld a, h
            out (MMU_FRAMEHI), a
            ld a, l
            out (MMU_FRAMELO), a
            push hl                 ; save page number
            ld de, (loadaddr)       ; retrieve target address
            ld hl, 0x2000           ; source address
            ld bc, 0x1000           ; transfer 4KB
            ldir                    ; copy copy copy
            ld (loadaddr), de       ; save target address
            pop hl                  ; restore page number
            ; more, sir?
            ld a, (pagecount)
            dec a
            ld (pagecount), a
            jr nz, nextpage

            ; map back RAM
            ld a, 0x2
            ld a, 0x2
            out (MMU_PAGESEL), a
            out (MMU_FRAMELO), a
            xor a
            out (MMU_FRAMEHI), a

            ; announce jump
            ld hl, jmpmsg
            call strout

            ; read the boot vector (written into the last four bytes of our 4K boot sector)
            ld ix, (bootvector)
            ld hl, (bootvector)
            call outwordhex
            ld hl, excited
            call strout
            ld hl, (diskpage) ; tell the OS where to find the boot disk
            jp (ix) ; boot

; functions

conout:     ; write chracter from C to console
            in a, (UART0_STATUS)
            bit 6, a
            jr nz, conout ; loop again if transmitter is busy
            ld a, c
            out (UART0_DATA), a ; transmit character
            ret

strout:     ; print string pointed to by HL
            ld a, (hl)
            cp 0
            ret z
            ld c, a
            call conout
            inc hl
            jr strout

outwordhex: ; print the word in HL as a four-char hex value
            ld a, h
            call outcharhex
            ld a, l
            call outcharhex
            ret

outcharhex: ; print byte in A as two-char hex value
            ld d, a ; copy
            rra
            rra
            rra
            rra
            call outnibble
            ld a, d
            call outnibble
            ret

outnibble:  and 0x0f
            cp 10
            jr c, numeral
            add 0x07
numeral:    add 0x30
            ld c, a
            call conout
            ret

; data
diskpage:   dw 0
loadmsg:    db "RAM disk bootstrap loading to ", 0 
frommsg:    db " from pages ", 0
jmpmsg:     db "... jump to ", 0
excited:    db "!\r\n", 0

; pad to 4KB
pad:        ds start+0x1000-$-10

stackptr:   ; put our stack below config

; configuration -- can be overridden easily since it's at the end of the sector.
; if you add more variables, add them here (ie maintain addresses of existing stuff) and adjust padding.
loadaddr:   dw 0xe000  ; address to start loading, must be a multiple of 0x1000 (pad the start of your payload if you need otherwise)
pagecount:  db 0x02    ; number of 4KB pages to read
            db 0x00    ; ignored, must be zero (high byte for future 16-bit page count)
bootvector: dw 0xfa00  ; virtual address to load at
bootstrap:  db 0xba, 0xbe  ; ROM checks for this code to indicate bootability
myorg:      dw 0x1000  ; ROM will load our 4K page to this address in memory
