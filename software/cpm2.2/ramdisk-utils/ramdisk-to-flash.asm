; 2013-11-06 WRS
; Talk to the SPI flash chip.
;
; z80asm ./spi-id.asm && ../tools/write_mem a.bin 1000 /dev/ttyUSB1

;
; OBSOLETE -- the SPI is now on dedicated master hardware, not GPIO,
; and the commands are built into the ROM monitor.
;

UART0_STATUS:  equ 0x0000 ; [7: RX READY] [6: TX BUSY] [6 unused bits]
UART0_DATA:    equ 0x0001
MMU_PAGESEL:   equ 0xF8
MMU_PERM:      equ 0xFB
MMU_FRAMEHI:   equ 0xFC
MMU_FRAMELO:   equ 0xFD

SPI_CHIPSELECT: equ 0x18
SPI_STATUS:     equ 0x19
SPI_TX:         equ 0x1a
SPI_RX:         equ 0x1b
SPI_DIVISOR:    equ 0x1c

; GPIO_IN:       equ 0x18
; GPIO_OUT:      equ 0x19
; BIT_CS:        equ 2 ; val 4
; BIT_CLK:       equ 1 ; val 2
; BIT_MOSI:      equ 0 ; val 1
; BIT_MISO:      equ 0 ; val 1
FLASH_MAN:     equ 0xc2
FLASH_DEV1:    equ 0x20
FLASH_DEV2:    equ 0x17

org 0x1000
            jp start

; program configuration
ram_start_page: dw 0x0200 ; 2MB (in 4K MMU pages)
ram_end_page:   dw 0x0400 ; 4MB (in 4K MMU pages) -- we stop at the byte BEFORE this page
flash_start:    dw 0x4000 ; 2MB (in 256 byte flash pages)
format_char:    db  0xe5  ; this is what CP/M expects to be written to a formatted floppy.

start:
            call spi_init
            ld hl, startmsg
            call strout

            ; just check the flash is present and talking to us
            call flash_check_type

            ; main menu
mainmenu:   ld hl, menumsg
            call strout
            call conin
            and 0xDF     ; convert keypress to upper case
            cp 'E'
            jp z, erase_ramdisk
            cp 'R'
            jp z, flash_to_ramdisk
            cp 'W'
            jp z, ramdisk_to_flash
            cp 'Q'
            ret z
            jr mainmenu

erase_ramdisk:
            call savemmu
            ld hl, erasingmsg
            call strout
            ld hl, (ram_start_page)
ernextpage:
            ; print page number
            call outwordhex
            ld a, l
            and 0x0f
            cp 0x0f
            jr nz, erspace
            ; print CR LF
            ld c, 0x0d
            call conout
            ld c, 0x0a
            call conout
            jr erwritepage ; skip space
erspace:    ; print a space
            ld c, 0x20
            call conout
erwritepage:; map and write ("erase") page
            ; map page
            ld a, 0x0d
            out (MMU_PAGESEL), a ; select Dxxx frame
            ld a, h
            out (MMU_FRAMEHI), a
            ld a, l
            out (MMU_FRAMELO), a
            push hl ; stash page number
            ; write page
            ld hl, 0xd000  ; first byte (source address)
            ld de, 0xd001  ; second byte (target address)
            ld bc, 0x0fff  ; block size - 1
            ld a, (format_char)
            ld (hl), a     ; write first byte
            ldir           ; copy copy copy!
            pop hl ; recover page number
            inc hl ; next page
            ; test for last page
            ld a, (ram_end_page+1)
            cp h
            jr nz, ernextpage
            ld a, (ram_end_page)
            cp l
            jr nz, ernextpage
            ; done!
            call restoremmu
            jp mainmenu

flash_to_ramdisk:
            call savemmu
            ld hl, readingmsg
            call strout
            ld hl, (ram_start_page)
            ld de, (flash_start)
wrnextpage:
            ; print page number
            call outwordhex

            ld a, l
            and 0x0f
            cp 0x0f
            jr nz, wrspace
            ld c, '\r'
            call conout
            ld c, '\n'
            jr wrcopypage
wrspace:    ld c, ' '
wrcopypage: call conout
            ; map the page in
            ld a, 0x0d
            out (MMU_PAGESEL), a
            ld a, h
            out (MMU_FRAMEHI), a
            ld a, l
            out (MMU_FRAMELO), a
            push hl ; save page number for later
            push de ; save flash addr for later
            ld hl, 0xd000 ; set memory address for flash write routine
            call flash_read_4k
            ; move flash address forward 16 x 256 bytes ie 4KB
            pop hl
            ld de, 16
            add hl, de
            ex de, hl
            pop hl ; restore page number
            inc hl ; next page
            ; test for last page
            ld a, (ram_end_page+1)
            cp h
            jr nz, wrnextpage
            ld a, (ram_end_page)
            cp l
            jr nz, wrnextpage
            ; done
            call restoremmu
            ld hl, donemsg
            call strout
            jp mainmenu

ramdisk_to_flash:
            ; prepare for reading our memory
            call savemmu
            ld hl, writingmsg
            call strout
            ld hl, (ram_start_page)
            ld de, (flash_start)
rdnextpage:
            ; print page number
            call outwordhex
            ld a, l
            and 0x0f
            cp 0x0f
            jr nz, rdspace
            ld c, '\r'
            call conout
            ld c, '\n'
            jr rdcopypage
rdspace:    ld c, ' '
rdcopypage: call conout
            ; map the page in
            ld a, 0x0d
            out (MMU_PAGESEL), a
            ld a, h
            out (MMU_FRAMEHI), a
            ld a, l
            out (MMU_FRAMELO), a
            push hl ; save page number for later
            push de ; save flash address for later
            ld hl, 0xd000 ; set memory address for flash write routine
            call flash_write_4k
            pop hl ; restore flash address but to hl while we add to it
            ; advance flash address
            ld de, 16 ; 16 x 256 byte pages = 4K
            add hl, de
            ex de, hl ; put the flash address back into de.
            pop hl ; restore page number
            inc hl ; next 4K page
            ; test for last page
            ld a, (ram_end_page+1)
            cp h
            jr nz, rdnextpage
            ld a, (ram_end_page)
            cp l
            jr nz, rdnextpage
            ; all done
            call restoremmu
            ld hl, donemsg
            call strout
            jp mainmenu

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
            IN a, (SPI_RX)
            ld c, a
            call spi_end
            ret ; status in C

flash_finish_write:
            call flash_read_status_register
            bit 0, c ; check WIP (write in progress)
            ret z    ; return if write done
            jr flash_finish_write ; check again


flash_prepare_write:
            call flash_read_status_register
            bit 0, c ; check WIP (write in progress)
            jr nz, flash_prepare_write ; wait until no longer writing
            bit 1, c ; check WEL (write enable latch)
            ret nz   ; return if set
            call flash_write_enable ; enable writes
            jr flash_prepare_write  ; check again

; read the 4K page which flash_addr points at into RAM from (HL) onwards.
flash_read_4k:
            call flash_copy_address_to_tmp
            call spi_start
            ld a, 0x0b  ; fast read command
            out (SPI_TX), a
            call flash_send_address_tmp
            ld a, 0              ; dummy byte
            out (SPI_TX), a
            ld de, 4096          ; 4K bytes to read
frnext:     out (SPI_TX), a
            in a, (SPI_RX)
            ld (hl), a
            inc hl               ; advance memory ptr
            dec de               ; drop DE
            ld a, d
            or e
            jp nz, frnext        ; there's more!
            call spi_end         ; we're done
            ret

flash_copy_address_to_tmp:
            ld a, d
            ld (flash_addr_tmp), a
            ld a, e
            ld (flash_addr_tmp+1), a
            ret

flash_send_address_tmp:
            ld a, (flash_addr_tmp)
            out (SPI_TX), a
            ld a, (flash_addr_tmp+1)
            out (SPI_TX), a
            xor a                 ; final address byte is always 0 (start of page)
            out (SPI_TX), a
            ret

flash_address_tmp_nextpage:
            ld a, (flash_addr_tmp+1)
            inc a
            ld (flash_addr_tmp+1), a
            ret nc
            ld a, (flash_addr_tmp)
            inc a
            ld (flash_addr_tmp), a
            ret

; erase, write and verify the 4K page with address (DE) from RAM starting at (HL) for 4096 bytes.
; we test the contents of the flash first as we can avoid writes where the data in flash is identical to RAM (which is commonly the case in our application)
flash_write_4k:
            ; compare data in flash to data in RAM first
            push de
            push hl
            call flash_verify_4k
            pop hl
            pop de
            cp 0x00 ; content in flash is identical already?
            ret z   ; skip copy
            push de ; save address for later
            push hl ; save address for later
            call flash_copy_address_to_tmp
            call flash_prepare_write
            ; erase sector
            call spi_start
            ld a, 0x20 ; sector erase command
            out (SPI_TX), a
            call flash_send_address_tmp
            call spi_end
            ; the flash supports programming in 256 byte "pages", so this will take us 16 iterations
            ld de, 4096
ppnextpage: call flash_prepare_write ; wait for previous write/erase to complete
            call spi_start
            ld a, 0x02 ; page program command
            out (SPI_TX), a
            call flash_send_address_tmp
            ; now the data
ppnextbyte: ld a, (hl)
            out (SPI_TX), a
            inc hl
            dec de
            ld a, e
            cp 0
            jr nz, ppnextbyte
            call spi_end
            ; hooray, one more page programmed!
            ; advance address in flash
            call flash_address_tmp_nextpage
            ld a, d
            or e
            cp 0
            jr nz, ppnextpage ; DE != 0, more to do.
            ; hooray, all 4K programmed!
            ; wait for program to complete
            call flash_finish_write
            ; now verify what we just wrote
            pop hl ; recover original memory address
            pop de ; recover original flash address
            call flash_verify_4k
            cp 0x00 ; identical?
            ret z
            jp verify_fail

; compare flash page (DE) with RAM starting at (HL) for 4096 bytes.
; return with A=00 if content is identical, or A=FF if they differ
; destroys BC DE HL.
flash_verify_4k:
            call flash_copy_address_to_tmp
            ld de, 4096
            call spi_start
            ld a, 0x0b           ; fast read command
            out (SPI_TX), a
            call flash_send_address_tmp
            ld a, 0              ; dummy byte required by fast read
            out (SPI_TX), a
vrnext:     out (SPI_TX), a      ; transmit dummy byte so we can read the next byte in
            in a, (SPI_RX)
            cp (hl)
            jr nz, differs
            inc hl               ; advance memory ptr
            dec de               ; drop DE
            ld a, d
            or e
            jr nz, vrnext        ; there's more!
            ld h, 0x00           ; the same
            jr vrdone
differs:    
            ld h, 0xff
vrdone:     call spi_end         ; we're done
            ld a, h              ; recover return code
            ret

flash_check_type:
            ld hl, type_check_msg
            call strout
            call spi_start
            ld a, 0x9f       ; identify/RDID command
            out (SPI_TX), a
            xor a
            out (SPI_TX), a  ; 24-bit address, all zeroes
            out (SPI_TX), a
            out (SPI_TX), a

            out (SPI_TX), a ; dummy byte
            in a, (SPI_RX)
            ld h, a

            out (SPI_TX), a ; dummy byte
            in a, (SPI_RX)
            ld l, a

            out (SPI_TX), a ; dummy byte
            in a, (SPI_RX)
            ld e, a

            call spi_end

            ; print HL then E
            call outwordhex
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

            ld hl, type_check_ok_msg
            call strout
            ret

type_check_fail:
            ld hl, type_check_fail_msg
            call strout
            ; jp flash_check_type ; try again?
            rst 0 ; back to the monitor

verify_fail:
            call spi_end
            ld hl, verify_fail_msg
            call strout
            rst 0

spi_init:   
            ld a, 1               ; SPI clock is divided by 2*(a+1), ie 128/4 = 32MHz
            out (SPI_DIVISOR), a
spi_end:
            ld a, 0xff
            out (SPI_CHIPSELECT), a ; raise CS
            ret

spi_start:  xor a
            out (SPI_CHIPSELECT), a
            ret

conin:      ; read character from console into A; wait if no character ready
            in a, (UART0_STATUS)
            bit 7, a
            jr z, conin ; keep waiting if no character ready
            in a, (UART0_DATA) ; read character
            ret

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
            push de
            ld a, h
            call outcharhex
            ld a, l
            call outcharhex
            pop de
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

savemmu:    ld a, 0x0d
            out (MMU_PAGESEL), a ; select page Dxxx
            ; store current MMU state
            in a, (MMU_PERM)
            ld (mmutemp0), a
            in a, (MMU_FRAMEHI)
            ld (mmutemp1), a
            in a, (MMU_FRAMELO)
            ld (mmutemp2), a
            ret

restoremmu: ; put MMU mapping for frame 0xD000 back as it was
            ld a, 0x0d
            out (MMU_PAGESEL), a
            ld a, (mmutemp0)
            out (MMU_PERM), a
            ld a, (mmutemp1)
            out (MMU_FRAMEHI), a
            ld a, (mmutemp2)
            out (MMU_FRAMELO), a
            ret

startmsg:            db "Copy RAM disk to/from SPI Flash\r\n", 0
type_check_msg:      db "Checking SPI flash type: ", 0
type_check_ok_msg:   db " OK!\r\n", 0
type_check_fail_msg: db " That's not what I expected! :(\r\n", 0
verify_fail_msg:     db "Flash verify failed\r\n", 0
donemsg:             db "Copy complete.\r\n", 0
erasingmsg:          db "Formatting RAM disk ...\r\n", 0
writingmsg:          db "Writing to flash ...\r\n", 0
readingmsg:          db "Reading from flash ...\r\n", 0

menumsg:             db "Main menu:\r\n"
                     db "E) erase (format) RAM disk contents\r\n"
                     db "W) write RAM disk to flash\r\n"
                     db "R) read RAM disk from flash\r\n"
                     db "Q) quit back to the monitor\r\n", 0
                     ; db "F) set flash start address\r\n", 0
                     ; db "M) set memory start address\r\n", 0
                     ; db "L) set memory length\r\n", 0

; variables
flash_addr_tmp:      db 0, 0        ; address for flash memory operations (internal)
mmutemp0:            db 0x55
mmutemp1:            db 0x55
mmutemp2:            db 0x55

