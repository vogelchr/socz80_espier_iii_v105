***************************************************************************
**                                                                       **
** Important: This is a fork of William R. Sowerbutts' socZ80            **
**            by Christian Vogel <vogelchr@vogel.cx> to support a cheap  **
**            Spartan6 board. Most of my changes have been rather        **
**            trivial, but any problems in the design have probably      **
**            been introduced by me, not by William :-).                 **
**                                                                       **
***************************************************************************

Changes to the original design:

   - works on a cheap Spartan6 board with labels in chinese, marking
     on the bottom left corner: ESPIER_III V105

  - uses a Winbond W9864G6KH-5 DRAM (1M, 4banks, 16bit)

   - monitor.asm : changed RDID command sequence to work with the
       Winbond W25Q32B SPI flash(es) on the ESPIER board

   - project file is       socz80_espier_iii_v105.xise
   - toplevel .vhd file is top_level_espier_iii_v105.vhd
   - user-constraints are  espier_iii_V105.ucf


ESPIER_III V105 board

 +----------------------------------------------------------------+
 |zrtech        [Seven-Segment]                                   |
 [::] J1          D5 D4 D3 D2 D1                                  |
 [:1] JTAG           Leds                                         |
 |                                                                |
 [::] J3                                                          |
 [::]                                                             |
 [::]                                                             |
 [::]                                                             |
 [:1]                   Buttons                             USB   |
 |     (Beep)     [S4]   [S3]   [S2]   [S1]         [On/Off]      |
 +----------------------------------------------------------------+
                  Reset Console  User1  User0 
                        gpio[2] gpio[1] gpio[0]

Buttons are (from left to right):
    * Reset    (push to reset Z80 system)
    * Console  (keep pushed during reset to have console on J3 S57N/S58P
    * User 1
    * User 0

S3, S2 and S1 are available as inputs 2..0 in the GPIO inputs.
LEDs D5..D2 are gpio outputs 3..0, D1 is always on (power LED).

The 7-segment display can display any hex number, high-byte is
io-register 0x38, low-byte is 0x39, decimal digits are 0x3a.

---- Original README by Willaim follows: ----------------------------------

socZ80 by William R Sowerbutts <will@sowerbutts.com> 

http://sowerbutts.com/socz80/ -- 2014-04-30 release (git 33e313f7)

Target platform is the Papilio Pro FPGA board.

Everything I have written for this project is distributed under the GNU
General Public License version 3 (see "LICENSE.txt" included in this
distribution)


== Get started ==

Connect to the Papilio UART at 115200bps 8N1, no flow control. Load the FPGA
bitstream "papilio_pro_spartan6.bit" with "papilio-prog -f" as usual. You
should see the system boot on the serial console. Type "help". Play around.

To upload and run a simple program, type (or perhaps copy and paste!)

wm 8000 21 11 80 DB 00 CB 77 20 FA 7E A7 C8 23 D3 01 18
wm 8010 F2 49 20 74 68 69 6E 6B 20 49 27 6D 20 61 6C 69
wm 8020 76 65 20 61 6E 64 20 79 6F 75 20 61 72 65 20 74
wm 8030 68 65 20 6D 61 63 68 69 6E 65 2E 0D 0A 00
run 8000

It may help to read the documentation below before you try to continue with the
next steps. Or just dive in.

If you want to run the software I've ported, I've supplied three RAM disks for
this purpose. The first two are CP/M formatted (A: and B:) while the third is
UZI formatted.

You will probably want to load the images into RAM, then write a copy to the
flash ROM on the board so that next time you can load them from ROM in 2
seconds rather than 10 minutes over the serial line.

Note that all my tools are written in Python and so should run under most
operating systems, but I use Linux and consequently there will be a number of
Linux assumptions baked into them.

At your command line, use the "write_phys_mem" tool to write each disk image
file to memory:

./software/tools/write_phys_mem ./disk-images/disk-200000.img 200000 /dev/ttyUSB1
./software/tools/write_phys_mem ./disk-images/disk-400000.img 400000 /dev/ttyUSB1
./software/tools/write_phys_mem ./disk-images/disk-600000.img 600000 /dev/ttyUSB1

Then at the monitor prompt on the machine, use "rwrite" to save the images to ROM:

  rwrite 200 200
  rwrite 400 400
  rwrite 600 600

After you hard reset the machine (power cycle, or with the button on the Papilio
Pro board) you can reload the disk images from the flash ROM with:

  rread 200 200
  rread 400 400
  rread 600 600

The papilio-prog tool does not overwrite this part of the  flash ROM when it
programs a new design into the flash so the images should remain safe.

After loading the disk images with rread, try these fun things:

Type "rboot 200" in the monitor to boot CP/M. There are various things on A:
and on B: you should find Zork (of course) as well as some other stuff. In user
area 1 on B: you'll find BBCBASIC.

While CP/M is running, type "mpmldr" to boot into MP/M-II. At this point the
second UART should come to life and you can multitask on both consoles.

Alternatively, while CP/M is running, type "uzi 1" to boot into UZI. Again the
second UART should come to life. Log in as "root" with password "root" (this is
the second most secure password in the world, beaten only by "password100") and
have a play. The program "/root/life" is an UZI port of the MINIX version of
game of life. "life -d" allows you to draw before it starts cycling. Ctrl+C
will quit, of course. "cd /root; zork1.com" will run the CP/M version of Zork
under CP/M emulation. "cc.com" is Hi-Tech C compiler (again under emulation).

At this stage, you're on your own. Have fun! Let me know what you do with it!
My email address is above.


== Specification ==

  - Z80 compatible T80 CPU core at 128MHz
  - 4KB paged MMU
  - 8MB DRAM with 16KB direct-mapped cache (using block RAM)
  - 4KB ROM, 4KB SRAM (using block RAM)
  - Two UARTs with receive FIFO (optional hardware flow control)
  - 1MHz Timer
  - SPI master connected to SPI flash ROM
  - SPI master connected to optional SD card socket
  - GPIO


== Physical memory map ==

  0x0000000 ... 0x07FFFFF    8MB SDRAM, cached
  0x0800000 ... 0x0FFFFFF    8MB SDRAM, cached (aliased -- ie 0x0800000 addresses the same byte as 0x0000000 etc)
  0x1000000 ... 0x17FFFFF    8MB SDRAM, uncached
  0x1800000 ... 0x1FFFFFF    8MB SDRAM, uncached (aliased)
  0x2000000 ... 0x2000FFF    4KB ROM (contains the monitor code)
  0x2001000 ... 0x2001FFF    4KB SRAM
  0x2002000 ... 0x3FFFFFF    unused (for future expansion)


== IO port map ==

All peripherals have a block of 8 IO ports

  0x00 ... 0x07              System console UART
  0x10 ... 0x17              Timer
  0x18 ... 0x1F              SPI master connected to PPro flash ROM
  0x20 ... 0x27              GPIO on IO pins (I use this for LEDs)
  0x27 ... 0x2F              Second UART on IO pins (includes hardware flow control lines)
  0x30 ... 0x37              SPI master on IO pins (I use this for an SD card socket)
  0x38 ... 0xEF              unused (for future expansion)
  0xF0 ... 0xF7              CPU clock modulator
  0xF8 ... 0xFF              MMU


== FPGA IO pins used by my PPro IO expansion board ==

  P131 -- Reset button (momentary switch which connects this line to 3.3V to request reset)
  P126 -- LED1 (output)
  P124 -- LED2 (output)
  P127 -- LED3 (output)
  P123 -- LED4 (output)
  P118 -- UART1 TX (output)
  P119 -- UART1 RTS (output)
  P120 -- UART1 RX (input)
  P121 -- UART1 CTS (input)
  P134 -- Console select jumper (pull this line to GND to make UART1 the system console)
  P116 -- SD card socket SPI clock (output)
  P114 -- SD card socket SPI chip select line (output)
  P115 -- SD card socket SPI MOSI (output)
  P117 -- SD card socket SPI MISO (input)

The IO expansion board is entirely optional and the system functions fully
without it.


== Reset ==

The Papilio Pro board has a "SW1" button which resets the entire FPGA and
reloads the design from the flash ROM. The problem with this is that the SDRAM
controller does not run when the FPGA is in this state, and consequently the
SDRAM is not refreshed and the contents starts to "leak" away. This has caused
me to accidentally corrupt my RAM disk on more than once occasion, so my
monitor ROM now detects a cold boot and deliberately zeroes the entire SDRAM.

I use a microswitch which momentarily connects 3.3V to P131 on the FPGA to
request a CPU reset. This resets the CPU, MMU and (some!) peripherals without 
losing the SDRAM contents.

Because you may not have a switch hooked up to P131, and because it's useful to
be able to trigger a reset from the system console, if you send the special
sequence "!~!~!~" to the system console it will trigger a reset, exactly as if
the reset button were pushed. The UART contains special hardware to watch for
this sequence so it will operate even if the CPU is halted or wedged.


== Serial ports ==

There are two UARTs. Both run at 115200bps 8N1. Both have 1KB receive FIFOs.
One is connected to the FTDI USB chip. This port has no flow control. The
second is connected to four IO pins (as described above) and implements RTS/CTS
hardware flow control. I have this connected to a MAX3232 for use with a serial
terminal. The serial terminal is very slow compared to the PC so hardware flow
control is essential (I would prefer to have it on the FTDI interface also).

By default, the FTDI is the system console. The monitor and OS talk to this
port. In MP/M and UZI the second serial port is also used as a console.

Note that the hardware flow control means that the system will not transmit at
all on the IO pin UART until you enable RTS/CTS flow control.

If you pull P134 to ground when the system is reset, the two UARTs are swapped
over and the IO pins become the system console. I have a jumper on my IO board
for this purpose.

Sending the special sequence "!~!~!~" on the FTDI port will reset the system as
described above.


== System state on reset ==

On reset the MMU is set up to translate memory as follows:

  Frame 0 -- physical 0x2000000 -- system ROM
  Frame 1 -- physical 0x0001000 -- cached SDRAM
  Frame 2 -- physical 0x0002000 -- cached SDRAM
  Frame 3 -- physical 0x0003000 -- cached SDRAM
   ... more SDRAM ...
  Frame E -- physical 0x000E000 -- cached SDRAM
  Frame F -- physical 0x2001000 -- SRAM

The Z80 is reset as normal and will start executing the system ROM at address
zero. The system ROM code remaps itself to the top frame of memory (ie 0xF000
upwards), jumps into itself in the new location, and then maps the first page
of SDRAM into frame 0. This results in the bottom 60KB of memory being directly
mapped into SDRAM with the ROM occupying the top 4KB. The input buffer for
characters typed by the user is set to 0xEF00 and the stack pointer is set to
0xF000. This is the system state when control is passed to the user.


== ROM Monitor commands ==

Type "help" for a list of commands:

        dm addr [len]                   display memory contents from addr for len (default 1) bytes
        wm addr val [val...]            write bytes to memory starting at addr
        cp src dst len                  copy len bytes from src to dst
        run addr                        run code at addr
        in addr                         read I/O port at addr, display result
        out addr val [val...]           write I/O port at addr with val
        mmu                             show MMU state
        sp [addr]                       show stack pointer (and set to addr)
        buf [addr]                      show input buffer (and set to addr)
        rboot page                      Boot from RAM disk
        rerase [page]                   Erase RAM disk
        rread [page] [flashpage]        Read RAM disk from SPI flash
        rwrite [page] [flashpage]       Write RAM disk to SPI flash
        @[cmd]                          Perform command without echo or terminal handling (bulk operations)

A few commands bear a little explanation;

"mmu" will display the current MMU state. To change it you need to use the
"out" command to write to the MMU registers. For example to remap frame 1 (ie
virtual addresses 0x1000 to 0x1FFF) to contain the page of SDRAM starting at
1MB you would do:
   out f8 1       (select page 1)
   out fc 1 0     (write 0x01 to port 0xFC, then 0x00 to port 0xFD, ie page 0x0100, 0x0100000 physical)

"buf" and "sp" will print the address at which the console input buffer and
stack pointer are set. You can provide an argument to move these pointers, eg
"sp 2000" will set the SP register to 2000 (remember the stack grows downwards)

"@" is a special prefix which you can apply to a command. The command behaves
exactly as before but no characters are echoed to the console after you type
"@" and the command. This may seem odd but it's useful for bulk operations over
the serial line -- it prevents the ROM from attempting to transmit more
characters than it receives (which can happen if you echo what was received,
plus results, plus the prompt) and thus ensures that it cannot fall behind when
processing data at full line rate. Useful mainly when driving the console via a
script.

"rread", "rwrite", "rerase" are for managing RAM disks. My convention is to use
three RAM disks each 2MB in size starting at 2MB, 4MB and 6MB in SDRAM. The
start/end address for RAM disk operations can be freely specified (it is
expressed as a number of 4KB pages) but the length is fixed at 2MB.

"rerase" will erase the contents of the RAM disk by writing 0xE5 to every byte.
Apparently this is what CP/M expects a newly formatted disk to look like. The
start page must be specified, in 4KB pages, ie "rerase 200" will erase the
first disk (starting at 2MB) while "rerase 400" and "rerase 600" will erase the
second and third.

To persist RAM disk contents across power cycles the "rread" and "rwrite"
commands are used. The Papilio Pro has an 8MByte SPI flash chip on board, but
less than 1MB is used to hold the FPGA design bitstream. I store my RAM disks
in the remaining space. For simplicity I write the disk at 2MB in RAM to offset
2MB in the flash ROM, etc.

"rread" will load a RAM disk from this flash ROM. The first argument is the
first page number of the RAM disk, the second argument is the first page number
in the flash ROM. I typically do "rread 200 200" to load my first RAM disk,
"rread 400 400" and "rread 600 600" for the other two.

"rwrite" is the reverse of "rread", ie it copies the RAM contents out to the
ROM. To save wear on the flash and to speed up the operation it compares the
RAM and flash contents and only erases and rewrites pages that have changed.

"rboot" attempts to boot from a RAM disk, it's the first stage of a multi-stage
bootstrap which loads a second stage from the RAM disk and passes control to
it. The first 4KB of the RAM disk contains this second stage. The final four
bytes of this 4KB (ie offsets 0xFFFC through 0xFFFF on the disk) are inspected
by "rboot"; the first two bytes must be 0xBA, 0xBE to indicate that a boostrap
is present. The next two bytes contain the address in RAM at which the
bootstrap exepcts to be loaded -- for my CP/M bootstrap this is 0x1000. If
rboot finds the expected signature it reads the first 4KB from the RAM disk at
the requested address, stores the page number of the first page of the RAM disk
in the HL register (so the second stage also knows which RAM disk is being
booted from) and then jumps to the requested address. 

The second stage bootstrap is reasonably flexible, it can be configured just by
changing a few values (stored near the end of its 4KB memory page) which tell
it how many additional 4KB pages to read from the RAM disk, what address to
load them to, and where to jump to start the program. For CP/M it reads the
next two 4KB pages and loads them from 0xE000 before finally jumping to the
CP/M entry vector at 0xFA00. The other operating systems I've ported (MP/M,
UZI) use CP/M as a glorified bootstrap but could in principle boot directly in
the same way.


== Storage ==

The CP/M BIOS and MP/M XIOS support only RAM disks (two disks of 2MB each,
starting at physical address 2MB and 4MB).  UZI also supports two RAM disks of
2MB each, starting at 4MB and 6MB in RAM, and additionally supports an optional
SD card socket.


== Tools for working with the monitor ==

I have some python scripts in software/tools that I find helpful. You'll need
the python serial module ("apt-get install python-serial" in Debian/Ubuntu).
Some scripts work with the monitor, in other words they take control of the
serial port and talk to the monitor program directly. The two "write" tools do
not need to read from the serial port so if your terminal application doesn't
open it exclusively you can leave it running while you upload a file.  "putty"
under linux is one that does share, "screen" does not (but you can easily hack
the source code so it does).

To use these, just reset the system to the monitor (with !~!~!~, not a full
FPGA reset -- unless you want the RAM blanked) and then run the program.

write_mem: Generate a series of "wm" commands for the monitor. When executed,
these commands result in the contents of a given file being written into memory
at a given address. The address is a virtual address, as translated by the MMU.
The file contains just the bytes to be loaded into memory (raw binary data).

Syntax: write_mem <filename> <start> [<port> [<runaddr>]]

  filename: the input filename

  start: the virtual address of the first byte to load (in hex)

  (optional) port: the name of the serial port

  (optional) runaddr: entry address (in hex)


By default the commands are just printed to the output. You may optionally
specify a serial port, in which case the commands are also sent to the port; a
final "run <addr>" command is also sent to jump to the first loaded address,
but the final newline is not sent to the port so you can choose whether and
when to run it. If the first loaded address is not the correct entry vector you
may specify that as a final argument.

This is really useful when testing short programs. I load my terminal program
and connect to the monitor. Then in another window I edit and assemble my
program, then send it to the port with write_mem. I don't even have to quit my
terminal program, just hit enter when write_mem has loaded it:

  write_mem /tmp/hello.bin 8000 /dev/ttyUSB1

The commands generated and send to the port are just normal monitor commands:

@wm 8000 21 11 80 DB 00 CB 77 20 FA 7E A7 C8 23 D3 01 18
@wm 8010 F2 49 20 74 68 69 6E 6B 20 49 27 6D 20 61 6C 69
@wm 8020 76 65 20 61 6E 64 20 79 6F 75 20 61 72 65 20 74
@wm 8030 68 65 20 6D 61 63 68 69 6E 65 2E 0D 0A 00
run 8000


read_phys_mem: Reads a chunk of memory by physical address (ie, not translated
by the MMU). Writes out a binary file to disk. I use this for taking backups of
my RAM disks. Because it has to read and write from the serial port, you must
quit your terminal application to use this.

Internally this program generates a series of "out" and "dm" commands to have
the monitor reprogram the MMU and examine the contents of memory.

Syntax: read_phys_mem <filename> <start> <length> <port>

  filename: the output filename

  start: the physical address of the first byte (in hex)

  length: number of bytes to read (in hex)

  port: the name of the serial port

Example: Copy the second RAM disk (starting at 4MB, length 2MB):
  read_phys_mem /tmp/ramdisk2.bin 400000 200000 /dev/ttyUSB1

Backing up a RAM disk like this takes about 10.5 minutes.


write_phys_mem: Writes a chunk of memory by physical address (not translated by
the MMU). This is (obviously) the reverse of read_phys_mem.

It is not necessary to specify the length, all bytes in the file are sent.

If an optional delta file is named, this file is assumed to be the current
contents of that region of memory. THERE IS NO CHECK THAT THIS IS REALLY THE
CASE, so you must be sure! The tool will send only the differences. This can be
really useful if you have a disk image loaded in RAM already and an exact copy
on file, and want to overwrite it with a slightly modified version -- this can
save a lot of time.

Syntax: write_phys_mem <filename> <start> <port> <deltafile>

  filename: the output filename

  start: the physical address of the first byte (in hex)

  port: the name of the serial port

  deltafile: the current contents of memory, if known

Example: Restore a copy the second RAM disk (starting at 4MB):
  write_phys_mem /tmp/ramdisk2.bin 400000


== Tools for transferring files to CP/M ==

The tools in software/cpm2.2/receive are for transferring files to CP/M. They
work in a similar way to the monitor tools (ie they just dump commands and data
to the serial port), but instead of the monitor interpreting these there are a
couple of CP/M programs to handle the receive side.

receive.asm -- this assembles to a CP/M executable that will read file data
on the standard input and write it to a file on the filesystem. The file data
is ASCII encoded -- it sends each byte as an ASCII hex value. There is some
framing to check for dropped characters (this was written before I had a
receive FIFO on the UART). The counterpart python program to send files is
"sendfile" in the same directory.

recvbin.asm -- same deal but it uses a binary encoding, twice the speed!
Counterpart python script is "sendbin".

sendmany -- this is a little utility shell script. You set the machine up
with CP/M running and then you can do (eg) "sendmany *.com" to send a bunch
of files at once. It sends commands to CP/M, waits a short while, then sends
the file data, then repeats the process for the next file. You will need to
adjust the script since it includes the full path to sendbin. This uses the
binary encoding. There is another script "sendslow" that uses the old ASCII
encoding.

There is of course a bootstrapping problem here--how do you get receive.com
or recvbin.com onto the system in the first place? In your case I've provided
a RAM disk image, but I got them onto the system in the first place by
writing them to memory at 0x0100 upwards with "wm" in the monitor (ie into
the CP/M TPA), then booting CP/M, and then using the "SAVE" command in CP/M
to write them from the TPA out to disk. My bootstrap is designed to avoid
clobbering memory below 0x1000 for this purpose. It's also why I reworked the
ROM to remap itself to 0xF000 and map RAM back in at 0x0000.


== Rebuilding the FPGA bitstream ==

To rebuild the FPGA from source you must first install the Xilinx ISE tools 
(I use the no-cost WebPack edition, version 14.7).

Run "ise" and load the project file "socz80.xise" from the "fpga" directory.
Select the "top_level" module in the hierarchy, then run the "Generate 
programming file" process. It will produce a new bitstream file named 
"fpga/work/top_level.bit"

If you just want to replace the monitor ROM contents you do not need to
resynthesise the entire design, instead you can just rewrite the ROM contents
in an existing bitstream file. The "build" script in "software/monitor"
contains the commands to do this using the Xilinx "data2mem" tool. It 
requires a bitstream file and the corresponding "socz80_bd.bmm" file which 
identifies which BRAMs on the FPGA are used to store the ROM program. This
makes the ROM development cycle very fast indeed.
