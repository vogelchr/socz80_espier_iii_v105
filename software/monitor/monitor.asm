; 2013-10-20: A simple but working ROM monitor program for the great Z80 system-on-FPGA project.
; This is my first Z80 assembler program so ... be kind.

; Wishlist:
;    dm could print an extra space after 8th char, and chars in ASCII on the right hand side, a la hexdump -C

; hardware register addresses
UART0_STATUS:  equ 0x00 ; [7: RX READY] [6: TX BUSY] [6 unused bits]
UART0_DATA:    equ 0x01
UART1_STATUS:  equ 0x28
UART1_DATA:    equ 0x29
STACK_INIT:    equ 0xF000 ; stack (grows downwards, runtime movable)
INPUT_BUFFER:  equ 0xEF00 ; input buffer (grows upwards, runtime movable)

SPI_CHIPSELECT: equ 0x18
SPI_STATUS:     equ 0x19
SPI_TX:         equ 0x1a
SPI_RX:         equ 0x1b
SPI_DIVISOR:    equ 0x1c
GPIO_INPUT:     equ 0x20
GPIO_OUTPUT:    equ 0x21

MMU_SELECT:    equ 0xF8   ; page table entry selector
MMU_PAGE17:    equ 0xFA   ; magic I/O port that accesses physical memory
MMU_PERM:      equ 0xFB   ; permission bits [6 unused bits] [WRITE] [READ]
MMU_FRAMEHI:   equ 0xFC   ; high byte of frame number
MMU_FRAMELO:   equ 0xFD   ; low byte of frame number
MMU_PTR_VAL0:  equ 0xFC
MMU_PTR_VAL1:  equ 0xFD
MMU_PTR_VAL2:  equ 0xFE
MMU_PTR_VAL3:  equ 0xFF

FLASH_MAN:     equ 0xc2
FLASH_DEV1:    equ 0x20
FLASH_DEV2:    equ 0x17

RAM_MB: equ 8

        ; on reset, the MMU maps us at 0x0000 for 4K,
        ; however it's really useful to have access to this
        ; first page because it's where we can put programs
        ; for CP/M to run, etc. So we remap ourselves up to
        ; 0xf000 and run from there.

        org 0x0000 ; early ROM location (also Z80 reset vector)

        ; test if we're cold booting -- have to do this reasonably quickly,
        ; before the SDRAM controller has initialised itself (~0.1msec)
        in a, (GPIO_INPUT)
        ld b, a ; stash that for now

        di ; disable interrupts (for those arriving via RST 0 rather than CPU reset)
        ; map page at 0xF000 to 0x2000000 physical (4K SRAM ROM)
        ld a, 0xf
        out (MMU_SELECT), a
        ld a, 0x20
        out (MMU_FRAMEHI), a
        xor a
        out (MMU_FRAMELO), a
        ld a, 0x03
        out (MMU_PERM), a
        ; jump to monitor in new location
        jp boot
        org 0xF000 + $
boot:
        ; use MMU to map SDRAM into page 0
        xor a
        out (MMU_SELECT), a
        out (MMU_FRAMEHI), a
        out (MMU_FRAMELO), a
        ld a, 0x03 ; map it read/write
        out (MMU_PERM), a

        ld a, b ; recover the coldboot flag
        and 0x80 ; had the SDRAM controller initialisation completed?
        jr z, warmboot ; yes! it's a warm boot then.

        ; no; it's a cold boot
        ld sp, 0x1000

        ld hl, coldbootmsg
        call outstring

        ; we're going to zero out RAM, each 4K page in turn
        ld hl, 0x0

        ; use frame starting at 0x1000
        ld a, 1
        out (MMU_SELECT), a
        ld a, 0x03
        out (MMU_PERM), a

        ; for each page ...
cbnextpage:
        ; map it in
        ld a, h
        out (MMU_FRAMEHI), a
        out (GPIO_OUTPUT), a ; also display code on LEDs
        ld a, l
        out (MMU_FRAMELO), a
        push hl ; stash page number

        ; write zeroes to the 4K page mapped at 0x1000....0x1fff
        ld hl, 0x1000
        ld de, 0x1001
        ld bc, 0x0fff
        ld (hl), 0 ; fill memory with with 0s
        ldir

        pop hl ; recover page number (we overwrite it in cycle 0, but with 0s)
        inc hl
        ld a, l
        cp 0
        jr nz, cbnextpage
        ld a, h
        call outnibble       ; print char after each MB zeroed
        ld a, h
        cp RAM_MB ; test if we're up to system RAM size?
        jr nz, cbnextpage

        call outnewline

        ; turn off LEDs
        xor a
        out (GPIO_OUTPUT), a

        ; restore MMU config
        xor a
        out (MMU_FRAMEHI), a
        inc a
        out (MMU_FRAMELO), a

        ; fall through to warm boot process
warmboot:
        ld sp, STACK_INIT   ; load stack pointer to point to 1 byte past top of memory
        ld iy, INPUT_BUFFER ; set default input buffer location 256 bytes below top of stack

        ; put a reset vector in place to jump back into us
        ld a, 0xc3 ; jmp instruction
        ld (0), a
        ld hl, boot
        ld (1), hl

        ; turn off bright LEDs!
        xor a
        out (GPIO_OUTPUT), a

        ; print our greeting
        ld hl, greeting
        call outstring

        ; flush UART FIFO
fifoflush:
        in a, (UART0_DATA)     ; read byte from UART
        in a, (UART0_STATUS)   ; check status
        bit 7, a               ; test if data waiting
        jr nz, fifoflush       ; keep flushing if there is

monitor_loop:
        ld hl, monitor_prompt
        call outstring

        push iy ; HL=IY
        pop hl
        call instring
        call outnewline

        ld ix, cmd_table
testnextcmd:
        push iy ; HL=IY
        pop hl
        ; test for end of string
        ld a, (hl)
        cp 0
        jr z, monitor_loop
        ; load string address
        ld e, (ix+0)
        ld d, (ix+1)
        call cmdcompare
        jp z, runcmd
        ; advance ix to point to next command
        ld bc, 4
        add ix, bc
        ; list is terminated with a zero, check for that
        ld a, (ix+0)
        or (ix+1)
        cp 0
        jr nz, testnextcmd
        ; end of list but no command found
        ld hl, what_msg
        call outstring
        jr monitor_loop
runcmd: ; run command at ix+2, ix+3
        ld e, (ix+2)
        ld d, (ix+3)
        push de
        pop ix
        jp (ix)

do_help:
        ld hl, help_msg
        call outstring
        jp monitor_loop

; parse command line to display I/O register contents. call with (HL) pointing into input buffer after "in "
do_in:
        call skipspaces
        call parsehexorfail
        ; DE now contains the I/O address to read
        push de
        call outde
        ld a, 0x3d
        call outchar
        ld a, 0x20
        call outchar
        pop bc
        in a, (c) ; asserts full 16-bit address bus
        call outcharhex
        call outnewline
        call skipspaces
        ld a, (hl)
        cp 0
        jp z, monitor_loop  ; don't use ret, we get here by a jump and not a call.
        jr do_in  ; next address

; write to I/O register
do_out:
        call skipspaces
        call parsehexorfail
        ; DE now contains the I/O address
        push de
nextio:
        call skipspaces
        call parsehexorfail
        ; DE now contains the value to write
        ld a, d ; check D=0
        cp 0
        jr nz, invalid
        pop bc ; I/O address
        out (c), e
        inc bc ; next I/O address
        call skipspaces
        ld a, (hl)
        cp 0
        jp z, monitor_loop
        push bc
        jr nextio

; write to memory command
do_wm:
        call skipspaces
        call parsehexorfail
        push de
        pop ix    ; put the memory address in ix
wm_nextbyte:
        call skipspaces
        ld a, (hl)
        cp 0
        jp z, monitor_loop ; end of string; go back to the monitor
        call parsehex
        ; data value to write to now in DE
        ; confirm D byte is zero ie we're dealing with a single byte
        ld a, d
        cp 0
        jr nz, invalid
        ; do the write
        ld (ix+0), e
        ; move to next memory address for the next byte
        inc ix
        jr wm_nextbyte

invalid:
        ld hl, invalid_msg
        call outstring
        jp monitor_loop ; don't use ret

; parse command line to display memory contents. call with (HL) pointing into input buffer after "dm "
do_dm:
        call skipspaces
        call parsehexorfail
        push de ; start address
        call skipspaces
        call parsehex
        ld a, e
        or d
        cp 0
        jr nz, dmgo
        ; ok DE=0, which is a problem. we'll dump 64K -- takes a while!
        ld e, 1 ; force DE=0x0001
dmgo:
        pop  hl ; start address
        call dumpmem
        jp monitor_loop ; don't use ret, we get here by a jump and not a call.

; display memory starting at (HL) length DE
dumpmem:
        ; start by writing the memory address
        call outhl
        ld a, 0x3a
        call outchar

        ; now print the bytes from memory separated by spaces
dumpnb: ld a, 0x20
        call outchar
        ld a, (hl)
        call outcharhex

        ; next byte
        inc hl
        dec de

        ; got to the end?
        ld a, e
        or d
        jr z, dumpdone

        ; newline?
        ld a, l
        and 0x0f
        cp 0
        jr nz, dumpnb

dumpnl: call outnewline
        jr dumpmem

dumpdone:
        call outnewline
        ret

; jump to given address
do_run:
        call skipspaces
        call parsehexorfail
        ; address to jump to is now in (DE)
        ex de, hl ; put it in HL where we can use it
        ld bc, warmboot
        push bc  ; push return address (monitor entry point)
        jp (hl)
        ; we never get here.

do_rboot:
        ; boot from RAM disk
        call skipspaces
        call parsehexorfail ; get physical page address in DE
        push de
        ; just for fun, let's use the 17th page!
        ld a, 0xff
        out (MMU_SELECT), a
        ; load address: page address 1234 = physical address 01 23 40 00
        ld a, d
        ld b, 4
rb1a:   srl a
        djnz rb1a ; repeat four times
        out (MMU_PTR_VAL0), a
        ld b, 4
        ld a, d
        ld c, e
rb1b:   sla c   ; shift B left one bit, top bit to carry, 0 to bottom bit
        rla     ; shift A left one bit, copy carry to bit 0
        djnz rb1b ; repeat four times
        out (MMU_PTR_VAL1), a
        ; we want to load from the specified address plus 0xFFC ... 0xFFF in order to get the
        ; 16-bit bootstrap magic number and 16-bit load/jump address into HL
        ld a, c
        or 0x0f
        out (MMU_PTR_VAL2), a
        ld a, 0xfc
        out (MMU_PTR_VAL3), a
        ; first read in the signature, should be be BA BE
        in a, (MMU_PAGE17)
        cp 0xBA
        jr nz, rbfail
        in a, (MMU_PAGE17)
        cp 0xBE
        jr nz, rbfail
        ; now read in that load address
        in a, (MMU_PAGE17)
        ld l, a
        in a, (MMU_PAGE17)
        ld h, a
        ; set the pointer to the correct address
        ld a, c
        out (MMU_PTR_VAL2), a
        xor a
        out (MMU_PTR_VAL3), a
        ; print out our intentions
        push hl
        ld hl, rboot_msg
        call outstring
        pop hl
        push hl ; stash load/jump address
        call outhl
        call outnewline
        ; now read the page of memory in
        ld c, MMU_PAGE17
        ld d, 16
        ld b, 0
rb1c:   inir ; read 256 bytes from port MMU_PAGE17 to (HL), incrementing HL as we go
        dec d
        jr nz, rb1c ; repeat until 16*256=0x1000 bytes read
        pop ix ; load/boot vector
        pop hl ; we enter the bootstrap with base page of RAM disk in HL
        jp (ix) ; jump into bootstrap
        ; bootstrap can never return
rbfail: ld hl, rboot_fail_msg
        call outstring
        jp monitor_loop

        ; before we had "the 17th page" we just mapped it in and copied:
        ; in some ways this is much simpler!! but it doesn't help us when
        ; we want to load somewhere other than 0x1000.
        ; ; we're going to read this page in at 0x1000, so let's map it at 0x2000
        ; ld a, 2
        ; out (MMU_SELECT), a
        ; ld a, d
        ; out (MMU_FRAMEHI), a
        ; ld a, e
        ; out (MMU_FRAMELO), a
        ; ld de, 0x1000 ; write to 0x1000
        ; ld hl, 0x2000 ; read from mapped page at 0x2000
        ; ld bc, 0x1000 ; 4KB
        ; ldir ; copy copy copy
        ; ; put RAM back
        ; ld a, 2
        ; out (MMU_FRAMELO), a
        ; xor a
        ; out (MMU_FRAMEHI), a
        ; pop hl ; we enter the bootstrap with base page of RAM disk in HL
        ; jp 0x1000 ; jump into bootstrap

do_mmu:
        ; show MMU state
        ld hl, mmu_header_msg
        call outstring
        in a, (MMU_SELECT)
        push af ; save current MMU_SELECT register value (ie, stop confusing Will)
        ld e, 0
mmunextpage:
        ld a, e
        out (MMU_SELECT), a
        rlca
        rlca
        rlca
        rlca
        ld d, a
        ; page start address
        call outcharhex
        ld a, 0
        call outcharhex
        ld a, '-'
        call outchar
        ; page end address
        ld a, d
        or 0x0f
        call outcharhex
        ld a, 0xff
        call outcharhex
        ld a, '\t'
        call outchar
        ; physical address
        in a, (MMU_FRAMEHI)
        call outcharhex
        in a, (MMU_FRAMELO)
        call outcharhex
        ld a, 0
        call outcharhex
        ld a, '0'
        call outchar
        ld a, '-'
        call outchar
        in a, (MMU_FRAMEHI)
        call outcharhex
        in a, (MMU_FRAMELO)
        call outcharhex
        ld a, 0xff
        call outcharhex
        ld a, 'F'
        call outchar
        ld a, '\t'
        call outchar
        ld a, '\t'
        call outchar
        ; permissions
        in a, (MMU_PERM)
        ld d, a
        call outcharhex
        ld a, ' '
        call outchar
        ld a, d
        and 0x02
        jr z,testread
        ld hl, mmu_write_msg
        call outstring
testread:
        ld a, d
        and 0x01
        jr z, mmudone
        ld hl, mmu_read_msg
        call outstring
mmudone:
        call outnewline
        inc e
        ld a, e
        cp 0x10
        jp nz, mmunextpage
        ; finally print the MMU pointer
        ld hl, mmu_ptr_msg
        call outstring
        ld a, 0xFF
        out (MMU_SELECT), a
        in a, (MMU_PTR_VAL0)
        call outcharhex
        in a, (MMU_PTR_VAL1)
        call outcharhex
        in a, (MMU_PTR_VAL2)
        call outcharhex
        in a, (MMU_PTR_VAL3)
        call outcharhex
        call outnewline
        ; tidy up
        pop af
        out (MMU_SELECT), a ; restore MMU select register
        jp monitor_loop

do_cp:
        ; copy memory
        call skipspaces
        call parsehexorfail
        ; src is now in DE
        push de
        call skipspaces
        call parsehexorfail
        ; dst is now in DE
        push de
        call skipspaces
        call parsehexorfail
        ; len is now in DE
        push de
        ; set up for LDIR:
        ; DE dst
        ; HL src
        ; BC count
        pop bc
        pop de
        pop hl
        ldir ; copy copy copy!
        jp monitor_loop

do_sp:
        call skipspaces
        ld a, (hl) ; check if we're at the end of string?
        cp 0
        jr z, showsp
        ; if not, load SP first
        call parsehex
        ; new SP is now in DE
        push de
        pop hl
        ld sp, hl
showsp:
        ; you can't read SP directly but you can add it to HL (go figure)
        ld hl, sp_msg
        call outstring
        ld hl, 0
        add hl, sp
        call outhl
        call outnewline
        jp monitor_loop

do_buf:
        call skipspaces
        ld a, (hl) ; check if we're at the end of string?
        cp 0
        jr z, showbuf
        ; if not, load SP first
        call parsehex
        ; new SP is now in DE
        push de
        pop iy
showbuf:
        ld hl, buf_msg
        call outstring
        push iy
        pop hl
        call outhl
        call outnewline
        jp monitor_loop

do_rerase:
        call skipspaces
        ld de, 0x200 ; default disk base
        ld a, (hl)   ; end of string?
        cp 0
        jr z, erasewarn
        call parsehex
erasewarn:
        ; target page in DE
        ld hl, erasewarn_msg
        call outstring
        call outde
        call plzconfirm
        ; if plzconfirm returns, we're good to go.
        ; start address remains in DE
        ; store MMU state
        ld a, 0x0d
        out (MMU_SELECT), a ; select Dxxx frame
        in a, (MMU_FRAMEHI)
        ld b, a
        in a, (MMU_FRAMELO)
        ld c, a
        push bc ; store MMU state on the stack
        call outnewline
        ld bc, 0x200 ; put length in bc
        ex de, hl ; put first page in hl
ernextpage:
        ; print page number
        call outhl
        ld a, l
        and 0x0f
        cp 0x0f
        jr nz, erspace
        ; print CR LF
        call outnewline
        jr erwritepage ; skip space
erspace:    ; print a space
        ld a, 0x20
        call outchar
erwritepage:; map and write ("erase") page
        ; map page
        ld a, h
        out (MMU_FRAMEHI), a
        ld a, l
        out (MMU_FRAMELO), a
        push hl ; stash page number
        push bc ; stash counter
        ; write page
        ld hl, 0xd000  ; first byte (source address)
        ld de, 0xd001  ; second byte (target address)
        ld bc, 0x0fff  ; block size - 1
        ld a, 0xe5
        ld (hl), a     ; write first byte
        ldir           ; copy copy copy!
        pop bc ; recover counter
        pop hl ; recover page number
        inc hl ; next page
        dec bc ; one less to go
        ; test for last page
        ld a, b
        or c
        cp 0
        jr nz, ernextpage
        ; done!
        call outnewline
fixmmu:
        ; restore MMU to default state
        pop bc
        ld a, b
        out (MMU_FRAMEHI), a
        ld a, c
        out (MMU_FRAMELO), a
        jp monitor_loop

plzconfirm:
        ld hl, genwarn_msg
        call outstring
erasewaitkey:
        call incharwait
        and 0xdf ; convert to upper case
        cp 'Y'
        ret z
        cp 'N'
        jr nz, erasewaitkey
        call outnewline
        pop hl ; remove return address from stack (we were call'ed but won't ret)
        jp monitor_loop

do_rread:
        push hl
        call flash_initialise ; check flash is present and matches our expectations
        pop hl
        call skipspaces
        ld bc, 0x200 ; default flash base
        ld de, 0x200 ; default ramdisk base
        ld a, (hl) ; end of string?
        cp 0
        jr z, readwarn
        call parsehex
        ; ramdisk base now in DE
        call skipspaces
        ld a, (hl) ; end of string?
        cp 0
        jr z, readwarn
        push de ; push disk base
        call parsehex
        ; flash base now in DE
        ; quick sanity check
        ld a, d
        and 0xf0
        cp 0
        jp nz, invalid
        push de
        pop bc ; put flash base in BC
        pop de ; recover ramdisk base in DE
readwarn:
        ; target memory page in de
        ; target flash address in bc ... *but* still needs shifting left four bits.
        ld hl, readwarn1_msg
        call outstring
        call outde
        ld hl, readwarn2_msg
        call outstring
        call outbc
        call plzconfirm
        ; now we can do the read
        call outnewline
        ; shift BC four bits left
        ld a, 4
readsh: and a ; clear carry flag
        rl c  ; shift C left one bit, top bit to carry
        rl b  ; shift B left one bit, shift in carry bit
        dec a
        cp 0
        jr nz, readsh
        ; BC contains correct flash address now
        ld a, 0x0d
        out (MMU_SELECT), a ; select Dxxx frame
        in a, (MMU_FRAMEHI)
        ld h, a
        in a, (MMU_FRAMELO)
        ld l, a
        push hl ; store MMU state on the stack
        push bc ; move flash address into HL
        pop hl
        ld bc, 0x200 ; put disk length (4K pages) in BC
        ; now:
        ;  BC -- number of pages remaining to copy
        ;  DE -- ram page number
        ;  HL -- flash page number
readnextpage:
        ; tell them what page we're on
        call outde
        ld a, e
        and 0x0f
        cp 0x0f
        jr nz, rnpspace
        call outnewline
        jr readcopypage
rnpspace:
        ld a, ' '
        call outchar
readcopypage:
        ; now do the actual copying
        ; map the page in
        ld a, d
        out (MMU_FRAMEHI), a
        ld a, e
        out (MMU_FRAMELO), a
        push de ; save ram page number
        push hl ; save flash page number
        call spi_start
        ld a, 0x0b ; fast read command
        out (SPI_TX), a
        ; now we send the memory address
        call flash_sendaddress
        out (SPI_TX), a ; dummy byte while fast read fills pipeline or whatever
        ld hl, 0xd000 ; target address
readnextbyte:
        out (SPI_TX), a ; send dummy byte
        in a, (SPI_RX)
        ld (hl), a
        inc hl
        ld a, h
        cp 0xe0 ; when we get to 0xe000 we're done
        jr nz, readnextbyte
        call spi_end
        ; move flash address forward 16x256 bytes ie 4KB
        pop hl
        ld de, 16
        add hl, de
        pop de ; recover ram page number
        inc de ; next page
        dec bc ; one down
        ld a, b
        or c
        jr nz, readnextpage
        ; ok we're done
        call outnewline
        jp fixmmu

flash_sendaddress:
        ld a, h
        out (SPI_TX), a ; address byte 0
        ld a, l
        out (SPI_TX), a ; address byte 1
        xor a
        out (SPI_TX), a ; address byte 2 (always 0 for our code)
        ret

do_rwrite:
        push hl
        call flash_initialise ; check flash is present and matches our expectations
        pop hl
        call skipspaces
        ld bc, 0x200 ; default flash base
        ld de, 0x200 ; default ramdisk base
        ld a, (hl) ; end of string?
        cp 0
        jr z, writewarn
        call parsehex
        ; ramdisk base now in DE
        call skipspaces
        ld a, (hl) ; end of string?
        cp 0
        jr z, writewarn
        push de ; push disk base
        call parsehex
        ; flash base now in DE
        ; quick sanity check
        ld a, d
        and 0xf0
        cp 0
        jp nz, invalid
        push de
        pop bc ; put flash base in BC
        pop de ; recover ramdisk base in DE
writewarn:
        ; target memory page in de
        ; target flash address in bc ... *but* still needs shifting left four bits.
        ld hl, writewarn1_msg
        call outstring
        call outde
        ld hl, writewarn2_msg
        call outstring
        call outbc
        call plzconfirm
        ; now we can do the read
        call outnewline
        ; shift BC four bits left
        ld a, 4
writesh: and a ; clear carry flag
        rl c  ; shift C left one bit, top bit to carry
        rl b  ; shift B left one bit, shift in carry bit
        dec a
        cp 0
        jr nz, writesh
        ; BC contains correct flash address now
        ld a, 0x0d
        out (MMU_SELECT), a ; select Dxxx frame
        in a, (MMU_FRAMEHI)
        ld h, a
        in a, (MMU_FRAMELO)
        ld l, a
        push hl ; store MMU state on the stack
        push bc ; move flash address into HL
        pop hl
        ld bc, 0x200 ; put disk length (4K pages) in BC -- should be 0x200
        ; now:
        ;   BC -- number of pages remaining to copy
        ;   DE -- ram page number
        ;   HL -- flash page number
writenextpage:
        ; tell them what page we're on
        call outde
        ld a, e
        and 0x0f
        cp 0x0f
        jr nz, wnpspace
        call outnewline
        jr writecopypage
wnpspace:
        ld a, ' '
        call outchar
writecopypage:
        ; first we verify the content of the flash to see if it matches what we have
        push de ; store RAM page number
        ld a, d
        out (MMU_FRAMEHI), a
        ld a, e
        out (MMU_FRAMELO), a
        call flash_verify_4k
        cp 0x00 ; identical?
        jr z, writepagedone ; skip write (yay)
        ; ok, now do the write
        push hl ; store flash page number
        call flash_prepare_write
        call spi_start
        ld a, 0x20 ; sector (4K) erase command
        out (SPI_TX), a
        call flash_sendaddress
        call spi_end
        ; we program the sector in 16 sets of 256 bytes, 16*256=4096.
        ld de, 0xd000 ; start address in mapped RAM
ppnext256:
        call flash_prepare_write
        call spi_start
        ld a, 0x02 ; page program command
        out (SPI_TX), a
        call flash_sendaddress
        ; now send 256 bytes of data
ppnextbyte:
        ld a, (de)
        out (SPI_TX), a
        inc de
        ld a, e
        cp 0
        jr nz, ppnextbyte
        call spi_end
        inc hl ; advance flash address by 256 bytes
        ld a, d
        cp 0xe0 ; made it to the end of mapped pages yet?
        jr nz, ppnext256
        ; now we verify the 4K that we just wrote
        pop hl ; recover original flash page number
        call flash_finish_write
        call flash_verify_4k
        cp 0x00 ; identical?
        jr z, writepagedone
        ; they differ :(
        call outnewline
        ld hl, verifybad_msg
        call outstring
        call invalid ; sorry, you lose.
writepagedone:
        ; advance flash address by 4KB
        ld de, 16
        add hl, de
        pop de ; recover RAM page number
        inc de ; next page
        dec bc ; one down
        ld a, b
        or c
        jr nz, writenextpage
        ; ok we're done
        call outnewline
        jp fixmmu

flash_verify_4k:
        ; compare flash at page HL against RAM at 0xd000 for 4K bytes
        push hl
        call spi_start
        ld a, 0x0b ; fast read command
        out (SPI_TX), a
        call flash_sendaddress
        out (SPI_TX), a ; dummy byte for fast read cmd pipeline
        ld hl, 0xd000
vrnext: out (SPI_TX), a ; dummy write
        in a, (SPI_RX)
        cp (hl)
        jr nz, verifyfail
        inc hl
        ld a, h
        cp 0xe0
        jr nz, vrnext
verifypass:
        call spi_end
        pop hl
        ld a, 0x00 ; A=0 -> identical
        ret
verifyfail:
        call spi_end
        pop hl
        ld a, 0xff ; A=FF -> non-identical
        ret

flash_initialise:
        call spi_init
        ld hl, type_check_msg
        call outstring
        call spi_start
        ld a, 0x9f ; identify/RDID command
        out (SPI_TX), a ; send command
        xor a
        out (SPI_TX), a ; send three address bytes
        out (SPI_TX), a
        out (SPI_TX), a
        ; read out ID value (3 bytes)
        out (SPI_TX), a
        in a, (SPI_RX)
        ld h, a
        out (SPI_TX), a
        in a, (SPI_RX)
        ld l, a
        out (SPI_TX), a
        in a, (SPI_RX)
        ld e, a
        call spi_end

        ; print type
        call outhl
        ld a, e
        call outcharhex

        ; check value received
        ld a, h
        cp FLASH_MAN
        jr nz, type_check_fail
        ld a, l
        cp FLASH_DEV1
        jr nz, type_check_fail
        ld a, e
        cp FLASH_DEV2
        jr nz, type_check_fail

        ; type is OK!
        ld hl, type_check_ok_msg
        call outstring
        ret
type_check_fail:
        ld hl, type_check_fail_msg
        call outstring
        jp monitor_loop ; give up

spi_init:
        ld a, 1 ; SPI CLK is sysclk/2*(1+n) ie for 128MHz clock, divisor 0 is 64MHz, 1 is 32MHz, 3 is 16MHz etc
        out (SPI_DIVISOR), a
        ; fall through to spi_end
spi_end:
        ld a, 0xff
        out (SPI_CHIPSELECT), a
        ret

spi_start:
        ld a, 0xfe ; flash is on CS0.
        out (SPI_CHIPSELECT), a
        ret

flash_write_enable:
        call spi_start
        ld a, 0x06
        out (SPI_TX), a
        call spi_end

flash_read_status_register:
        call spi_start
        ld a, 0x05
        out (SPI_TX), a
        out (SPI_TX), a
        call spi_end
        IN a, (SPI_RX)
        ret ; status in A

flash_finish_write:
        call flash_read_status_register
        bit 0, a ; check WIP (write in progress)
        ret z    ; return if write done
        jr flash_finish_write ; check again

flash_prepare_write:
        call flash_read_status_register
        bit 0, a ; check WIP (write in progress)
        jr nz, flash_prepare_write ; wait until no longer writing
        bit 1, a ; check WEL (write enable latch)
        ret nz   ; return if set
        call flash_write_enable ; enable writes
        jr flash_prepare_write  ; check again


; compare strings at (HL) and (DE), input buffer in (HL) and command in (DE).
; return with flags NZ -> inequality
; return with flags  Z -> equal ie string at (DE) is a prefix of (HL)
cmdcompare:
        ld a, (de)
        cp 0
        ret z    ; end of string at (DE) -> return zero flag
        cp (hl)
        ret nz   ; non-matching -> return nonzero flag
        inc de
        inc hl
        jr cmdcompare

; outstring: Print the string at (HL) until 0 byte is found
; destroys: AF HL
outstring:
        ld a, (hl)     ; load next character
        and a          ; test if zero
        ret z          ; return when we find a 0 byte
        call outchar
        inc hl         ; next char please
        jr outstring

; print the string at (HL) in hex (continues until 0 byte seen)
outstringhex:
        ld a, (hl)     ; load next character
        and a          ; test if zero
        ret z          ; return when we find a 0 byte
        call outcharhex
        ld a, 0x20 ; space
        call outchar
        inc hl         ; next char please
        jr outstringhex

; output a newline
outnewline:
        ld a, 0x0d   ; output newline
        call outchar
        ld a, 0x0a
        call outchar
        ret

outhl:  ; prints HL in hex. Destroys AF.
        ld a, h
        call outcharhex
        ld a, l
        call outcharhex
        ret

outbc:  ; prints BC in hex. Destroys AF.
        ld a, b
        call outcharhex
        ld a, c
        call outcharhex
        ret

outde:  ; prints DE in hex. Destroys AF.
        ld a, d
        call outcharhex
        ld a, e
        call outcharhex
        ret

; print the byte in A as a two-character hex value
outcharhex:
        push bc
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

; outchar: Wait for UART TX idle, then print the char in A
; destroys: AF
outchar:
        push bc
        ld b, a
        ; wait for transmitter to be idle
ocloop: in a, (UART0_STATUS)
        bit 6, a
        jr nz, ocloop   ; loop while busy

        ; now output the char to serial port
        ld a, b
        out (UART0_DATA), a
        pop bc
        ret

; instring: Read a typed string into (HL) terminate with 0 byte when CR seen (leaves (HL) pointing at the 0 byte)
;           Returns length of string (excluding 0 terminator) in C
;           Special handling for newline, backspace and non-printing characters.
;           If first character received is '@' it is not inserted into the buffer and the buffer is not echoed back (for bulk data load etc)
; DOES NOT PERFORM BUFFER LENGTH CHECKING!
instring:
        ld c, 0        ; we use c to remember our string length
        call incharwait
        cp '@'
        jr nz, gotchar
        ; we've got an @ so we should do a bulk load
        call outchar
        jr instringbulk
instringloop:
        call incharwait ; reads single byte into A
gotchar:; test for cr/lf
        cp 0x0d
        jr z, cr
        cp 0x0a
        jr z, cr
        ; test for backspace
        cp 0x08
        jr z, backspace
        cp 0x7f
        jr z, backspace
        ; test for non-printing characters
        cp 0x20 ; < 0x20?
        jp c, instringloop 
        cp 0x7f ; > 0x7f?
        jp nc, instringloop
        ; store the character in the buffer
        ld (hl), a
        inc hl
        inc c
        call outchar ; echo back the character typed
        jr instringloop
backspace:
        ld a, c
        cp 0
        jr z, instringloop ; cannot backspace past the start
        dec hl
        dec c
        ld a, 0x08 ; move back
        call outchar
        ld a, 0x20 ; print space
        call outchar
        ld a, 0x08 ; move back again
        call outchar
        jr instringloop
cr:     
        ld a, 0
        ld (hl), a
        ret

; a fast bulk version of instring
; does not echo back characters received after the first space, in order to avoid overflowing the rx fifo
; does not perform full terminal handling
instringbulk:
        call incharwait
        cp 0x0d
        jr z, cr
        cp 0x0a
        jr z, cr
        ld (hl), a
        inc hl
        cp 0x20
        jr z, isbspace
        call outchar
        jr instringbulk
isbspace:
        call incharwait
        cp 0x0d
        jr z, cr
        cp 0x0a
        jr z, cr
        ld (hl), a
        inc hl
        jr isbspace



; incharwait: Wait for UART RX, return character read in A
; destroys: A
incharwait:
        in a, (UART0_STATUS)
        bit 7, a
        jr z, incharwait   ; loop while no character received
        in a, (UART0_DATA)
        ret

; advance HL until it no longer points at a space
skipspaces:
        ld a, (hl)
        cp 0x20
        ret nz
        inc hl
        jr skipspaces

; convert character in A to upper case
toupper:
        cp 0x61    ; < "a" ?
        ret c
        cp 0x7b
        ret nc
        and 0x5f
        ret

; if at end of string, jump to invalid, else continue
; and run parsehex
parsehexorfail:
        ld a, (hl)
        cp 0
        jp z, invalid
        ; fall through to parsehex
; read hex digits pointed to by hl, load into DE
; does not work when fed with non-hex digits!!
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

coldbootmsg:        db "\r\nCold boot: zeroing RAM ", 0
greeting:           db "\r"
                    db "                    ___   ___  \r\n"
                    db " ___  ___   ___ ___( _ ) / _ \\ \r\n"
                    db "/ __|/ _ \\ / __|_  / _ \\| | | |\r\n"
                    db "\\__ \\ (_) | (__ / / (_) | |_| |\r\n"
                    db "|___/\\___/ \\___/___\\___/ \\___/ \r\n"
                    db "Z80 ROM Monitor (Will Sowerbutts, 2013-12-12)\r\n", 0
monitor_prompt:     db "Z80> ", 0
what_msg:           db "Error reduces\r\nYour expensive computer\r\nTo a simple stone.\r\n", 0
invalid_msg:        db "Errors have occurred.\r\nWe won't tell you where or why.\r\nLazy programmers.\r\n", 0 
mmu_header_msg:     db "Virtual (F8)\tPhysical (FC FD)\tFlags (FB)\r\n", 0
mmu_read_msg:       db "READ ", 0
mmu_write_msg:      db "WRITE ", 0
mmu_ptr_msg:        db "17th Page Pointer (FA) = ",0
rboot_msg:          db "Loading stage 2 bootstrap from RAM disk to ", 0
rboot_fail_msg:     db "Bad magic number. Gentlemen, please check your RAM disks.\r\n", 0
sp_msg:             db "SP=", 0
buf_msg:            db "BUF=", 0
type_check_msg:     db "Checking SPI flash type: ", 0
type_check_ok_msg:   db " (OK)\r\n", 0
type_check_fail_msg: db " FAIL! :(\r\n", 0
erasewarn_msg:      db "Erase RAM disk starting at page ", 0
readwarn1_msg:      db "Read RAM disk starting at page ", 0
readwarn2_msg:      db " from flash page ", 0
writewarn1_msg:     db "Write RAM disk starting at page ", 0
writewarn2_msg:     db " to flash page ", 0
genwarn_msg:        db " (y/n)?", 0
verifybad_msg:      db "Flash write verify failed :(\r\n", 0
help_msg:           db "Commands:\r\n"
                    db "\tdm addr [len]\t\t\tdisplay memory contents from addr for len (default 1) bytes\r\n"
                    db "\twm addr val [val...]\t\twrite bytes to memory starting at addr\r\n"
                    db "\tcp src dst len\t\t\tcopy len bytes from src to dst\r\n"
                    db "\trun addr\t\t\trun code at addr\r\n"
                    db "\tin addr\t\t\t\tread I/O port at addr, display result\r\n"
                    db "\tout addr val [val...]\t\twrite I/O port at addr with val\r\n"
                    db "\tmmu\t\t\t\tshow MMU state\r\n"
                    db "\tsp [addr]\t\t\tshow stack pointer (and set to addr)\r\n"
                    db "\tbuf [addr]\t\t\tshow input buffer (and set to addr)\r\n"
                    db "\trboot page\t\t\tBoot from RAM disk\r\n"
                    db "\trerase [page]\t\t\tErase RAM disk\r\n"
                    db "\trread [page] [flashpage]\tRead RAM disk from SPI flash\r\n"
                    db "\trwrite [page] [flashpage]\tWrite RAM disk to SPI flash\r\n"
                    db "\t@[cmd]\t\t\t\tPerform command without echo or terminal handling (bulk operations)\r\n"
                    db 0
cmd_rboot:          db "rboot ", 0
cmd_buf:            db "buf", 0   ; no space at end (arg is optional)
cmd_cp:             db "cp ", 0
cmd_dm:             db "dm ", 0
cmd_help:           db "help", 0
cmd_help2:          db "?", 0
cmd_in:             db "in ", 0
cmd_mmu:            db "mmu",0
cmd_out:            db "out ", 0
cmd_run:            db "run ", 0
cmd_sp:             db "sp",0     ; no space at end (arg is optional)
cmd_wm:             db "wm ", 0
cmd_rerase:         db "rerase", 0
cmd_rread:          db "rread", 0
cmd_rwrite:         db "rwrite", 0
cmd_table:          
                    dw cmd_cp, do_cp
                    dw cmd_dm, do_dm
                    dw cmd_help, do_help
                    dw cmd_help2, do_help
                    dw cmd_in, do_in
                    dw cmd_mmu, do_mmu
                    dw cmd_rboot, do_rboot
                    dw cmd_out, do_out
                    dw cmd_run, do_run
                    dw cmd_wm, do_wm
                    dw cmd_sp, do_sp
                    dw cmd_buf, do_buf
                    dw cmd_rerase, do_rerase
                    dw cmd_rread, do_rread
                    dw cmd_rwrite, do_rwrite
                    dw 0 ; terminate command table

; pad to 4K
                    ds 0x10000 - $, 0xfe  ; this will be negative when the ROM exceeds 4K so the assembler will alert us to our excess.
