; SOCZ80 MP/M 2.1 LDRBIOS (largely a copy of my CPM/2.2 BIOS)
; 2013-11-19 Will Sowerbutts
; use zmac to assemble this

.z80 ; prefer z80 mnemonics

UART0_STATUS   equ 0x0000 ; [7: RX READY] [6: TX BUSY] [6 unused bits]
UART0_DATA     equ 0x0001
MMU_PAGESEL    equ 0xF8
MMU_PERM       equ 0xFB
MMU_FRAMEHI    equ 0xFC
MMU_FRAMELO    equ 0xFD

            .org 01700h ; immediately after MPMLDR

; jump vector table used by CP/M
            jp boot         ; cold start
wboote:     jp wboot        ; warm start
            jp const        ; console status
            jp conin        ; console character in
            jp conout       ; console character out
            jp list         ; list character out
            jp punch        ; punch character out
            jp reader       ; reader character out
            jp home         ; move disk head to home position
            jp seldsk       ; select disk
            jp seltrk       ; set track number
            jp setsec       ; set setor number
            jp setdma       ; set DMA address
            jp read         ; read disk
            jp write        ; write disk
            jp listst       ; return list status
            jp sectran      ; sector translate

; disk parameter header (16 bytes for each drive), see page 6-28 in CP/M 2.2 operating system manual
dpbase:
            ; disk 0 (A)
            dw 0            ; sector translation table (0 = no translation)
            dw 0            ; must be 0
            dw 0            ; must be 0
            dw 0            ; must be 0
            dw dirbf        ; DIRBUF (address of shared 128 byte scratch pad area)
            dw dpblk        ; DPB (disk parameter block)
            dw chk00        ; CSV (unique scratch pad used to check for changed disks)
            dw alv00        ; ALV (unique scratch pad for allocation information)
            ; end of disk 0

            ; disk 1 (B)
            dw 0            ; sector translation table (0 = no translation)
            dw 0            ; must be 0
            dw 0            ; must be 0
            dw 0            ; must be 0
            dw dirbf        ; DIRBUF (address of shared 128 byte scratch pad area)
            dw dpblk        ; DPB (disk parameter block)
            dw chk01        ; CSV (unique scratch pad used to check for changed disks)
            dw alv01        ; ALV (unique scratch pad for allocation information)
            ; end of disk 1

ndisks      equ 2           ; number of disks we defined

; disk parameter block (can be shared by all disks with same configuration)
; my RAM disks are each 2MB in size;
;     track is 16384 bytes (128 x 128 sectors per track)
;     there are 128 tracks (128 x 128 x 128 = 2MB)
;     track 0 is reserved for the operating system, so there are 2048-16 = 2032 user sectors, and 16KB reserved for OS boot storage (which is plenty)
; using 1K sectors here resulted in a FUCK UP. don't do that! with 4K sectors it seems much happier.
dpblk:
            dw 128          ; SPT: number of 128 byte sectors per track
            db 5            ; BSH: block shift factor (see manual for table)
            db 31           ; BLM: block mask (see manual for table)
            db 1            ; EXM: extent mask (see manual for table, using entries marked N/A turns out to be a bad idea!)
            dw 508          ; DSM: (disk bytes / block bytes) - 1, change alv00/01 etc if you change this; this is the number of the last sector on the disk, excluding system tracks (ie more system tracks -> this gets smaller)
            dw 511          ; DRM: directory max entries - 1
            db 0xf0         ; AL0: directory sector allocation bitmask byte 0
            db 0x00         ; AL1: directory sector allocation bitmask byte 1
            dw 0            ; CKS: check size (change chk00/01 etc if you change this)
            dw 1            ; OFF: track offset (number of system tracks)

; bios functions follow
boot:       
wboot:
gocpm:
            ld hl, ouchithurts
            call strout
wefailed:   jr wefailed

const:      ; read console status
            in a, (UART0_STATUS)
            bit 7, a
            jr z, connotready
            ; we're ready
            ld a, 0xff
            ret
connotready:
            xor a
            ret

conin:      ; read character from console into A; wait if no character ready
            in a, (UART0_STATUS)
            bit 7, a
            jr z, conin ; keep waiting if no character ready
            in a, (UART0_DATA) ; read character
            ; fix backspace
            cp 0x7f ; backspace?
            ret nz
            ld a, 8 ; ctrl-h
            ret

conout:     ; write chracter from C to console
            in a, (UART0_STATUS)
            bit 6, a
            jr nz, conout ; loop again if transmitter is busy
            ld a, c
            out (UART0_DATA), a ; transmit character
            ret

list:       ; write character to listing device (we don't have one!)
            ret

listst:     ; return list device status
            xor a       ; 0 = not ready
            ret

punch:      ; write character to punch device (we don't have one!)
            ret

reader:     ; read character from reader device (which we don't have)
            ld a, 0x1a ; end of file
            ret

seldsk:     ; select disk indicated by register C
            ;; push bc
            ;; push bc
            ;; ld hl, seldskmsg
            ;; call strout
            ;; pop bc
            ;; ld a, c
            ;; call outcharhex
            ;; call crlf
            ;; pop bc

            ld hl, 0    ; return code 0 indicates error
            ld a, c
            cp ndisks
            ret nc      ; return (with error code) if C >= ndisks ie illegal drive
            ld (curdisk), a ; store current disk
            ; compute proper disk parameter header address in HL
            ld l, c
            ld h, 0
            add hl, hl ; *2
            add hl, hl ; *4
            add hl, hl ; *8
            add hl, hl ; *16
            ; now HL = disk number * 16
            ld de, dpbase
            add hl, de ; HL = dpbase + disk number * 16
            ret

home:       ld c, 0
            ; fall through into seltrk
seltrk:     ; set track given by register BC
            ; push bc
            ; push bc
            ; ld hl, seltrkmsg
            ; call strout
            ; pop bc
            ; ld a, c
            ; call outcharhex
            ; ld c, 0x20
            ; call conout
            ; pop bc

            ld a, c
            ld (curtrack), a
            ret

setsec:     ; set sector given by register BC
            ; push bc
            ; push bc
            ; ld hl, setsecmsg
            ; call strout
            ; pop bc
            ; ld a, c
            ; call outcharhex
            ; ld c, 0x20
            ; call conout
            ; pop bc

            ld a, c
            ld (cursector), a
            ret

sectran:    ; logical to physical sector translation
            ; HL=BC ie 1:1 mapping (no translation)
            ld h, b
            ld l, c
            ret

setdma:     ; set DMA address given by BC
            ;;; push bc
            ;;; push bc
            ;;; ld hl, setdmamsg
            ;;; call strout
            ;;; pop hl
            ;;; call outwordhex
            ;;; ld c, 0x20
            ;;; call conout
            ;;; pop bc

            ld (curdmaaddr), bc ; may need to xfer to HL first?
            ret

read:       ; read from our RAM disk
;              ld hl, readmsg
;              call strout
;              ld a, (curdisk)
;              call outcharhex
;              ld a, (curtrack)
;              call outcharhex
;              ld a, (cursector)
;              call outcharhex
;              ld c, '@'
;              call conout
;              ld a, (curdmaaddr+1)
;              ld c, a
;              call outcharhex
;              ld a, (curdmaaddr)
;              ld c, a
;              call outcharhex
            call mapmmu
;              ld c, ']'
;              call conout
            ; HL now points to the location where our data is stored
            ; DE now points at the DMA buffer
docopy:     ld bc, 0x80 ; transfer 128 bytes
            ldir ; copy copy copy!
            call unmapmmu ; put MMU back as it was
            xor a ; A=0: success
            ret

write:      ; write to our RAM disk
;              push bc
;              push hl
;              push de
;              ld hl, writemsg
;              call strout
;              ld a, (curdisk)
;              call outcharhex
;              ld a, (curtrack)
;              call outcharhex
;              ld a, (cursector)
;              call outcharhex
;              ld c, '@'
;              call conout
;              ld a, (curdmaaddr+1)
;              ld c, a
;              call outcharhex
;              ld a, (curdmaaddr)
;              ld c, a
;              call outcharhex
            call mapmmu
;              ld c, ']'
;              call conout

            ; HL now points to the location where our data is stored
            ; DE points to the DMA buffer
            ex de, hl ; swap HL/DE
            jr docopy

mapmmu:     ; use MMU to map the physical page corresponding to the drive data
            ;
            ; CPM address = drive number / 7 bit track / 7 bit sector / 7 bits byte offset
            ; Physical address:
            ;   bits 22-21: drive number+1 (2 bits)
            ;   bits 20-14: track number (7 bits)
            ;   bits 13-7:  sector number (7 bits)
            ;   bits 6-0:   byte offset (7 bits)
            ; CPM:   DDTTTTTTTSSSSSSSOOOOOOO  (2 bit drive, 7 bit track, 7 bit sector, 7 bit offset)
            ; Phys:  PPPPPPPPPPPOOOOOOOOOOOO  (11 bit page frame number, 12 bit page offset)
            ; MMU:   HHHLLLLLLLL              (3 bit hi, 8 bit low, low 12 bits come from logical address accessed)

            ; start by picking where in our address space to map the disk memory
            ; we know PC is in Fxxx so we can't use that
            ; we have to avoid SP and the target DMA address
            ; SP >= 0x8000 -> use 0x2000 unless DMA is in 0x2000 in which case use 0x4000
            ; SP  < 0x8000 -> use 0xA000 unless DMA is in 0xA000 in which case use 0xC000

            ld hl, 0    ; you can't read sp ...
            add hl, sp  ; but you can add it to hl!
            ; re-use l register
            ld a, (curdmaaddr+1)
            and 0xf0
            ld l, a     ; store top 4 bits of DMA address in L
            ld a, h     ; test top bit of SP
            and 0x80
            jr z, use_a0   ; top bit of SP is not set, SP is in low 32K
            ; top bit of SP is set, SP is in high 32K
            ld a, l
            cp 0x20
            jr z, use_40
            ld a, 0x2
            jr foundframe
use_40:     ld a, 0x4
            jr foundframe
use_a0:     ld a, l
            cp 0xa0
            jr z, use_c0
            ld a, 0xa
            jr foundframe
use_c0:     ld a, 0xc
            ; fall through to foundframe
foundframe: ; selected frame in register a
            out (MMU_PAGESEL), a ; select page frame
            and a ; clear carry flag (for rla)
            rla   ; shift left four bits
            rla
            rla
            rla
            ld h, a              ; set the top four bits of h
            ; store current MMU state
            in a, (MMU_PERM)
            ld (mmutemp0), a
            in a, (MMU_FRAMEHI)
            ld (mmutemp1), a
            in a, (MMU_FRAMELO)
            ld (mmutemp2), a
            ; now compute MMU_FRAMEHI value (physical address of RAM disk sector)
            ld a, (curdisk)
            inc a             ; we use disk+1 so first disk starts at physical address 2MB
            sla a             ; shift A left one position, load 0 into bit 0
            ld b, a
            ld a, (curtrack)
            and 0x40          ; test top bit of 7-bit track number
            jr z, framehiready
            inc b             ; load top bit of track into B
framehiready:
            ld a, b
            out (MMU_FRAMEHI), a
;               ld d, a
            ; now compute MMU_FRAMELO value (physical address of RAM disk sector)
            ld a, (curtrack)
            sla a
            sla a
            ld b, a         ; the bottom 6 bits of the track number are now in the top 6 bits of B, with 00 in the low two bits
            ld a, (cursector) ; we only want the top two bits of this 7-bit number
            srl a
            srl a
            srl a
            srl a
            srl a
            and 0x03 ; mask off the low two bits (potentially cursector had the top bit set I suppose?)
            or b     ; merge in the 6 bits of track number
            out (MMU_FRAMELO), a
;               ld e, a
;               ld c, ';'
;               call conout
;               ex de, hl
;               push de
;               call outwordhex
;               pop de
;               ex de, hl
;               ld c, '.'
;               call conout
            ; brilliant, now it's mapped! finally compute HL to point to the start of the data buffer
            ld l, 0
            ld a, (cursector)
            srl a            ; shift A right one bit, bottom bit goes into carry flag
            jr nc, lready    ; test carry flag and ...
            ld l, 0x80       ; ... bump up L to the next sector if it was 1
lready:     and 0x0f         ; we only want four bits
            or h             ; merge in the frame number computed earlier
            ld h, a          ; store it in H
;               call outwordhex
            ld de, (curdmaaddr) ; load DE with source/target address
            ret

unmapmmu:   ; put MMU mapping for frame back as it was
            ld a, (mmutemp0)
            out (MMU_PERM), a
            ld a, (mmutemp1)
            out (MMU_FRAMEHI), a
            ld a, (mmutemp2)
            out (MMU_FRAMELO), a
            ret

;---------------------------------------------------------------------------------------------------------------
; debug functions (ideally to be removed in final version, if we ever get that far!)
strout:     ; print string pointed to by HL
            ld a, (hl)
            cp 0
            ret z
            ld c, a
            call conout
            inc hl
            jr strout

;;;; outwordhex: ; print the word in HL as a four-char hex value
;;;;             ld a, h
;;;;             call outcharhex
;;;;             ld a, l
;;;;             call outcharhex
;;;;             ret
;;;; 
;;;; outcharhex: ; print byte in A as two-char hex value
;;;;             ld d, a ; copy
;;;;             rra
;;;;             rra
;;;;             rra
;;;;             rra
;;;;             call outnibble
;;;;             ld a, d
;;;;             call outnibble
;;;;             ret
;;;; 
;;;; crlf:
;;;;             ld c, 0x0d
;;;;             call conout
;;;;             ld c, 0x0a
;;;;             call conout
;;;;             ret
;;;; 
;;;; outnibble:  and 0x0f
;;;;             cp 10
;;;;             jr c, numeral
;;;;             add 0x07
;;;; numeral:    add 0x30
;;;;             ld c, a
;;;;             call conout
;;;;             ret

ouchithurts: db "oops", 0

;---------------------------------------------------------------------------------------------------------------

; scratch RAM used by BIOS
curdisk:    db 0x11
curtrack:   db 0x22
cursector:  db 0x33
curdmaaddr: dw 0x4444
mmutemp0:   db 0x55
mmutemp1:   db 0x66
mmutemp2:   db 0x77

; scratch RAM used by BDOS/MPMLDR
dirbf:      ds 128           ; directory scratch area
alv00:      ds 64            ; allocation vector for disk 0, must be (DSM/8)+1 bytes
alv01:      ds 64            ; allocation vector for disk 1, must be (DSM/8)+1 bytes
chk00:      ds 0             ; check vector for disk 0 (must be CKS bytes long)
chk01:      ds 0             ; check vector for disk 1 (must be CKS bytes long)

; tell zmac we're done, and the start address for this is "boot" (although this is nonsense of course)
 .end boot
