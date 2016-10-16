//
// zi80dis.cpp - extract disassembly and timing information for Z-80 and 8080 instructions
//
// Standard Z-80 mnemonics are used with extensions for undocumented opcodes.
//		sl1				cb28 - cb2f
//		pfix			DD preceeding DD or FD
//		pfiy			FD preceeding DD of FD
//		ixh,ixl			low, high bytes of IX register
//		iyh,iyl			low, high bytes of IY register
//		ednop			ED with no effect
//		in (c)			ed70
//		out (c),0		ed71
//		srop (ix+d),r	undocumented IX/IY shift/rotate instructions

#include <stdio.h>
#include <string.h>

#include "zi80dis.h"

// TODO: The 8080 extensions are bogus; dunno if they're 8085A or what, exactly.
// Not harmful, but wonder where they come from?

static char undefined[] = "undefined";
static char ednop[] = "ednop";
static char pfix[] = "pfix";

struct Opcode {
	const char	*name;
	int			args;
	int			tstates;
	int			tstates8080;
};

// Opcode args if name == 0
// 0 - CB
// 1 - DD|FD
// 2 - ED
// 3 - DD|FD CB

// other Opcode args
#define BC		(4)		/* byte constant */
#define WC		(5)		/* word constant */
#define ADDR	(6)		/* 2 byte memory address */
#define JR		(7)		/* 1 byte relative jump */
#define IND		(8)		/* IX/IY index offset */
#define IND_1	(9)		/* IX/IY index offset and literal byte */
#define CALL	(10)	/* 2 byte memory address as done by call */
#define RST		(11)	/* rst call, addr is inst & 0x38 */
#define PORT	(12)	/* I/O port */
#define UNBR	(256)	/* bit to indicate processor flow goes elsewhere */
#define UNDOC	(512)	/* undocumented instruction */
#define NOEFF	(1024)	/* DD/FD prefix has no effect */

#define FLGMSK	(~255)

// TODO - add flag for repeated instructions

#define VT(low, high)	(((low) << 8) | (high))

static Opcode major[256] = {
	{ "nop",			0,				 4,  4 }, /* 00 */
	{ "ld bc,%s",		WC,				10, 10 }, /* 01 */
	{ "ld (bc),a",		0,				 7,  7 }, /* 02 */
	{ "inc bc",			0,				 6,  5 }, /* 03 */
	{ "inc b",			0,				 4,  5 }, /* 04 */
	{ "dec b",			0,				 4,  5 }, /* 05 */
	{ "ld b,%s",		BC,				 7,  7 }, /* 06 */
	{ "rlca",			0,				 4,  4 }, /* 07 */

	{ "ex af,af'",		0,				 4,  4 }, /* 08 */
	{ "add hl,bc",		0,				11, 10 }, /* 09 */
	{ "ld a,(bc)",		0,				 7,  7 }, /* 0a */
	{ "dec bc",			0,				 6,  5 }, /* 0b */
	{ "inc c",			0,				 4,  5 }, /* 0c */
	{ "dec c",			0,				 4,  5 }, /* 0d */
	{ "ld c,%s",		BC,				 7,  7 }, /* 0e */
	{ "rrca",			0,				 4,  4 }, /* 0f */

	{ "djnz %s",		JR,		 VT(8, 13),  4 }, /* 10 */
	{ "ld de,%s",		WC,				10, 10 }, /* 11 */
	{ "ld (de),a",		0,				 7,  7 }, /* 12 */
	{ "inc de",			0,				 6,  5 }, /* 13 */
	{ "inc d",			0,				 4,  5 }, /* 14 */
	{ "dec d",			0,				 4,  5 }, /* 15 */
	{ "ld d,%s",		BC,				 7,  7 }, /* 16 */
	{ "rla",			0,				 4,  4 }, /* 17 */

	{ "jr %s",			JR | UNBR,		12,  4 }, /* 18 */
	{ "add hl,de",		0,				11, 10 }, /* 19 */
	{ "ld a,(de)",		0,				 7,  7 }, /* 1a */
	{ "dec de",			0,				 6,  5 }, /* 1b */
	{ "inc e",			0,				 4,  5 }, /* 1c */
	{ "dec e",			0,				 4,  5 }, /* 1d */
	{ "ld e,%s",		BC,				 7,  7 }, /* 1e */
	{ "rra",			0,				 4,  4 }, /* 1f */

	{ "jr nz,%s",		JR,		 VT(7, 12),  4 }, /* 20 */
	{ "ld hl,%s",		WC,				10, 10 }, /* 21 */
	{ "ld (%s),hl",		ADDR,			16, 16 }, /* 22 */
	{ "inc hl",			0,				 6,  5 }, /* 23 */
	{ "inc h",			0,				 4,  5 }, /* 24 */
	{ "dec h",			0,				 4,  5 }, /* 25 */
	{ "ld h,%s",		BC,				 7,  7 }, /* 26 */
	{ "daa",			0,				 4,  4 }, /* 27 */

	{ "jr z,%s",		JR,		 VT(7, 12),  4 }, /* 28 */
	{ "add hl,hl",		0,				11, 10 }, /* 29 */
	{ "ld hl,(%s)",		ADDR,			16, 16 }, /* 2a */
	{ "dec hl",			0,				 6,  5 }, /* 2b */
	{ "inc l",			0,				 4,  5 }, /* 2c */
	{ "dec l",			0,				 4,  5 }, /* 2d */
	{ "ld l,%s",		BC,				 7,  7 }, /* 2e */
	{ "cpl",			0,				 4,  4 }, /* 2f */

	{ "jr nc,%s",		JR,		 VT(7, 12),  4 }, /* 30 */
	{ "ld sp,%s",		WC,				10, 10 }, /* 31 */
	{ "ld (%s),a",		ADDR,			13, 13 }, /* 32 */
	{ "inc sp",			0,				 6,  5 }, /* 33 */
	{ "inc (hl)",		0,				11, 10 }, /* 34 */
	{ "dec (hl)",		0,				11, 10 }, /* 35 */
	{ "ld (hl),%s",		BC,				10, 10 }, /* 36 */
	{ "scf",			0,				 4,  4 }, /* 37 */

	{ "jr c,%s",		JR,		 VT(7, 12),  4 }, /* 38 */
	{ "add hl,sp",		0,				11, 10 }, /* 39 */
	{ "ld a,(%s)",		ADDR,			13, 13 }, /* 3a */
	{ "dec sp",			0,				 6,  5 }, /* 3b */
	{ "inc a",			0,				 4,  5 }, /* 3c */
	{ "dec a",			0,				 4,  5 }, /* 3d */
	{ "ld a,%s",		BC,				 7,  7 }, /* 3e */
	{ "ccf",			0,				 4,  4 }, /* 3f */

	{ "ld b,b",			0,				 4,  5 }, /* 40 */
	{ "ld b,c",			0,				 4,  5 }, /* 41 */
	{ "ld b,d",			0,				 4,  5 }, /* 42 */
	{ "ld b,e",			0,				 4,  5 }, /* 43 */
	{ "ld b,h",			0,				 4,  5 }, /* 44 */
	{ "ld b,l",			0,				 4,  5 }, /* 45 */
	{ "ld b,(hl)",		0,				 7,  7 }, /* 46 */
	{ "ld b,a",			0,				 4,  5 }, /* 47 */

	{ "ld c,b",			0,				 4,  5 }, /* 48 */
	{ "ld c,c",			0,				 4,  5 }, /* 49 */
	{ "ld c,d",			0,				 4,  5 }, /* 4a */
	{ "ld c,e",			0,				 4,  5 }, /* 4b */
	{ "ld c,h",			0,				 4,  5 }, /* 4c */
	{ "ld c,l",			0,				 4,  5 }, /* 4d */
	{ "ld c,(hl)",		0,				 7,  7 }, /* 4e */
	{ "ld c,a",			0,				 4,  5 }, /* 4f */

	{ "ld d,b",			0,				 4,  5 }, /* 50 */
	{ "ld d,c",			0,				 4,  5 }, /* 51 */
	{ "ld d,d",			0,				 4,  5 }, /* 52 */
	{ "ld d,e",			0,				 4,  5 }, /* 53 */
	{ "ld d,h",			0,				 4,  5 }, /* 54 */
	{ "ld d,l",			0,				 4,  5 }, /* 55 */
	{ "ld d,(hl)",		0,				 7,  7 }, /* 56 */
	{ "ld d,a",			0,				 4,  5 }, /* 57 */

	{ "ld e,b",			0,				 4,  5 }, /* 58 */
	{ "ld e,c",			0,				 4,  5 }, /* 59 */
	{ "ld e,d",			0,				 4,  5 }, /* 5a */
	{ "ld e,e",			0,				 4,  5 }, /* 5b */
	{ "ld e,h",			0,				 4,  5 }, /* 5c */
	{ "ld e,l",			0,				 4,  5 }, /* 5d */
	{ "ld e,(hl)",		0,				 7,  7 }, /* 5e */
	{ "ld e,a",			0,				 4,  5 }, /* 5f */

	{ "ld h,b",			0,				 4,  5 }, /* 60 */
	{ "ld h,c",			0,				 4,  5 }, /* 61 */
	{ "ld h,d",			0,				 4,  5 }, /* 62 */
	{ "ld h,e",			0,				 4,  5 }, /* 63 */
	{ "ld h,h",			0,				 4,  5 }, /* 64 */
	{ "ld h,l",			0,				 4,  5 }, /* 65 */
	{ "ld h,(hl)",		0,				 7,  7 }, /* 66 */
	{ "ld h,a",			0,				 4,  5 }, /* 67 */

	{ "ld l,b",			0,				 4,  5 }, /* 68 */
	{ "ld l,c",			0,				 4,  5 }, /* 69 */
	{ "ld l,d",			0,				 4,  5 }, /* 6a */
	{ "ld l,e",			0,				 4,  5 }, /* 6b */
	{ "ld l,h",			0,				 4,  5 }, /* 6c */
	{ "ld l,l",			0,				 4,  5 }, /* 6d */
	{ "ld l,(hl)",		0,				 7,  7 }, /* 6e */
	{ "ld l,a",			0,				 4,  5 }, /* 6f */

	{ "ld (hl),b",		0,				 7,  7 }, /* 70 */
	{ "ld (hl),c",		0,				 7,  7 }, /* 71 */
	{ "ld (hl),d",		0,				 7,  7 }, /* 72 */
	{ "ld (hl),e",		0,				 7,  7 }, /* 73 */
	{ "ld (hl),h",		0,				 7,  7 }, /* 74 */
	{ "ld (hl),l",		0,				 7,  7 }, /* 75 */
	{ "halt",			0,				 4,  7 }, /* 76 */
	{ "ld (hl),a",		0,				 7,  7 }, /* 77 */

	{ "ld a,b",			0,				 4,  5 }, /* 78 */
	{ "ld a,c",			0,				 4,  5 }, /* 79 */
	{ "ld a,d",			0,				 4,  5 }, /* 7a */
	{ "ld a,e",			0,				 4,  5 }, /* 7b */
	{ "ld a,h",			0,				 4,  5 }, /* 7c */
	{ "ld a,l",			0,				 4,  5 }, /* 7d */
	{ "ld a,(hl)",		0,				 7,  7 }, /* 7e */
	{ "ld a,a",			0,				 4,  5 }, /* 7f */

	{ "add a,b",		0,				 4,  4 }, /* 80 */
	{ "add a,c",		0,				 4,  4 }, /* 81 */
	{ "add a,d",		0,				 4,  4 }, /* 82 */
	{ "add a,e",		0,				 4,  4 }, /* 83 */
	{ "add a,h",		0,				 4,  4 }, /* 84 */
	{ "add a,l",		0,				 4,  4 }, /* 85 */
	{ "add a,(hl)",		0,				 7,  7 }, /* 86 */
	{ "add a,a",		0,				 4,  4 }, /* 87 */

	{ "adc a,b",		0,				 4,  4 }, /* 88 */
	{ "adc a,c",		0,				 4,  4 }, /* 89 */
	{ "adc a,d",		0,				 4,  4 }, /* 8a */
	{ "adc a,e",		0,				 4,  4 }, /* 8b */
	{ "adc a,h",		0,				 4,  4 }, /* 8c */
	{ "adc a,l",		0,				 4,  4 }, /* 8d */
	{ "adc a,(hl)",		0,				 7,  7 }, /* 8e */
	{ "adc a,a",		0,				 4,  4 }, /* 8f */

	{ "sub b",			0,				 4,  4 }, /* 90 */
	{ "sub c",			0,				 4,  4 }, /* 91 */
	{ "sub d",			0,				 4,  4 }, /* 92 */
	{ "sub e",			0,				 4,  4 }, /* 93 */
	{ "sub h",			0,				 4,  4 }, /* 94 */
	{ "sub l",			0,				 4,  4 }, /* 95 */
	{ "sub (hl)",		0,				 7,  7 }, /* 96 */
	{ "sub a",			0,				 4,  4 }, /* 97 */

	{ "sbc a,b",		0,				 4,  4 }, /* 98 */
	{ "sbc a,c",		0,				 4,  4 }, /* 99 */
	{ "sbc a,d",		0,				 4,  4 }, /* 9a */
	{ "sbc a,e",		0,				 4,  4 }, /* 9b */
	{ "sbc a,h",		0,				 4,  4 }, /* 9c */
	{ "sbc a,l",		0,				 4,  4 }, /* 9d */
	{ "sbc a,(hl)",		0,				 7,  7 }, /* 9e */
	{ "sbc a,a",		0,				 4,  4 }, /* 9f */

	{ "and b",			0,				 4,  4 }, /* a0 */
	{ "and c",			0,				 4,  4 }, /* a1 */
	{ "and d",			0,				 4,  4 }, /* a2 */
	{ "and e",			0,				 4,  4 }, /* a3 */
	{ "and h",			0,				 4,  4 }, /* a4 */
	{ "and l",			0,				 4,  4 }, /* a5 */
	{ "and (hl)",		0,				 7,  7 }, /* a6 */
	{ "and a",			0,				 4,  4 }, /* a7 */

	{ "xor b",			0,				 4,  4 }, /* a8 */
	{ "xor c",			0,				 4,  4 }, /* a9 */
	{ "xor d",			0,				 4,  4 }, /* aa */
	{ "xor e",			0,				 4,  4 }, /* ab */
	{ "xor h",			0,				 4,  4 }, /* ac */
	{ "xor l",			0,				 4,  4 }, /* ad */
	{ "xor (hl)",		0,				 7,  7 }, /* ae */
	{ "xor a",			0,				 4,  4 }, /* af */

	{ "or b",			0,				 4,  4 }, /* b0 */
	{ "or c",			0,				 4,  4 }, /* b1 */
	{ "or d",			0,				 4,  4 }, /* b2 */
	{ "or e",			0,				 4,  4 }, /* b3 */
	{ "or h",			0,				 4,  4 }, /* b4 */
	{ "or l",			0,				 4,  4 }, /* b5 */
	{ "or (hl)",		0,				 7,  7 }, /* b6 */
	{ "or a",			0,				 4,  4 }, /* b7 */

	{ "cp b",			0,				 4,  4 }, /* b8 */
	{ "cp c",			0,			 	 4,  4 }, /* b9 */
	{ "cp d",			0,				 4,  4 }, /* ba */
	{ "cp e",			0,				 4,  4 }, /* bb */
	{ "cp h",			0,				 4,  4 }, /* bc */
	{ "cp l",			0,				 4,  4 }, /* bd */
	{ "cp (hl)",		0,				 7,  7 }, /* be */
	{ "cp a",			0,				 4,  4 }, /* bf */

	{ "ret nz",			0,		 VT(5, 11), VT(5, 11) }, /* c0 */
	{ "pop bc",			0,				10, 10 }, /* c1 */
	{ "jp nz,%s",		ADDR,			10, 10 }, /* c2 */
	{ "jp %s",			ADDR | UNBR,	10, 10 }, /* c3 */
	{ "call nz,%s",		CALL,   VT(10, 17), VT(11, 17) }, /* c4 */
	{ "push bc",		0,				11, 11 }, /* c5 */
	{ "add a,%s",		BC,				 7,  7 }, /* c6 */
	{ "rst %s",			RST,			11, 11 }, /* c7 */

	{ "ret z",			0,		 VT(5, 11), VT(5, 11) }, /* c8 */
	{ "ret",			0 | UNBR,		10, 10 }, /* c9 */
	{ "jp z,%s",		ADDR,			10, 10 }, /* ca */
	{ 0,				0,				 0, 10 }, /* cb */
	{ "call z,%s",		CALL,	VT(10, 17), VT(11, 17) }, /* cc */
	{ "call %s",		CALL,			17, 17 }, /* cd */
	{ "adc a,%s",		BC,				 7,  7 }, /* ce */
	{ "rst %s",			RST,			11, 11 }, /* cf */

	{ "ret nc",			0,		 VT(5, 11), VT(5, 11) }, /* d0 */
	{ "pop de",			0,				10, 10 }, /* d1 */
	{ "jp nc,%s",		ADDR,			10, 10 }, /* d2 */
	{ "out (%s),a",		PORT,			11, 10 }, /* d3 */
	{ "call nc,%s",		CALL,	VT(10, 17), VT(11, 17) }, /* d4 */
	{ "push de",		0,				11, 11 }, /* d5 */
	{ "sub %s",			BC,				 7,  7 }, /* d6 */
	{ "rst %s",			RST,			11, 11 }, /* d7 */

	{ "ret c",			0,		 VT(5, 11), VT(5, 11) }, /* d8 */
	{ "exx",			0,				 4, 10 }, /* d9 */
	{ "jp c,%s",		ADDR,			10, 10 }, /* da */
	{ "in a,(%s)",		PORT,			11, 10 }, /* db */
	{ "call c,%s",		CALL,	VT(10, 17), VT(11, 17) }, /* dc */
	{ 0,				1,				 0, 17 }, /* dd */
	{ "sbc a,%s",		BC,				 7,  7 }, /* de */
	{ "rst %s",			RST,			11, 11 }, /* df */

	{ "ret po",			0,		 VT(5, 11), VT(5, 11)}, /* e0 */
	{ "pop hl",			0,				10, 10 }, /* e1 */
	{ "jp po,%s",		ADDR,			10, 10 }, /* e2 */
	{ "ex (sp),hl",		0,				19, 18 }, /* e3 */
	{ "call po,%s",		CALL,	VT(10, 17), VT(11, 17)}, /* e4 */
	{ "push hl",		0,				11, 11 }, /* e5 */
	{ "and %s",			BC,				 7,  7 }, /* e6 */
	{ "rst %s",			RST,			11, 11 }, /* e7 */

	{ "ret pe",			0,		 VT(5, 11), VT(5, 11) }, /* e8 */
	{ "jp (hl)",		0 | UNBR,		 4,  5 }, /* e9 */
	{ "jp pe,%s",		ADDR,			10, 10 }, /* ea */
	{ "ex de,hl",		0,				 4,  5 }, /* eb */
	{ "call pe,%s",		CALL,	VT(10, 17), VT(11, 17) }, /* ec */
	{ 0,				2,				 0, 17 }, /* ed */
	{ "xor %s",			BC,				 7,  7 }, /* ee */
	{ "rst %s",			RST,			11, 11 }, /* ef */

	{ "ret p",			0,		 VT(5, 11), VT(5, 11) }, /* f0 */
	{ "pop af",			0,				10, 10 }, /* f1 */
	{ "jp p,%s",		ADDR,			10, 10 }, /* f2 */
	{ "di",				0,				 4,  4 }, /* f3 */
	{ "call p,%s",		CALL,	VT(10, 17), VT(11, 17) }, /* f4 */
	{ "push af",		0,				11, 11 }, /* f5 */
	{ "or %s",			BC,				 7,  7 }, /* f6 */
	{ "rst %s",			RST,			11, 11 }, /* f7 */

	{ "ret m",			0,		 VT(5, 11), VT(5, 11) }, /* f8 */
	{ "ld sp,hl",		0,				 6,  5 }, /* f9 */
	{ "jp m,%s",		ADDR,			10, 10 }, /* fa */
	{ "ei",				0,				 4,  4 }, /* fb */
	{ "call m,%s",		CALL,	VT(10, 17), VT(11, 17) }, /* fc */
	{ 0,				1,				 0, 17 }, /* fd */
	{ "cp %s",			BC,				 7,  7 }, /* fe */
	{ "rst %s",			RST,			11, 11 }, /* ff */
};

static Opcode minor[3][256] = {
  {							/* cb */
	{ "rlc b",			0,				 8 }, /* cb00 */
	{ "rlc c",			0,				 8 }, /* cb01 */
	{ "rlc d",			0,				 8 }, /* cb02 */
	{ "rlc e",			0,				 8 }, /* cb03 */
	{ "rlc h",			0,				 8 }, /* cb04 */
	{ "rlc l",			0,				 8 }, /* cb05 */
	{ "rlc (hl)",		0,				15 }, /* cb06 */
	{ "rlc a",			0,				 8 }, /* cb07 */

	{ "rrc b",			0,				 8 }, /* cb08 */
	{ "rrc c",			0,				 8 }, /* cb09 */
	{ "rrc d",			0,				 8 }, /* cb0a */
	{ "rrc e",			0,				 8 }, /* cb0b */
	{ "rrc h",			0,				 8 }, /* cb0c */
	{ "rrc l",			0,				 8 }, /* cb0d */
	{ "rrc (hl)",		0,				15 }, /* cb0e */
	{ "rrc a",			0,				 8 }, /* cb0f */

	{ "rl b",			0,				 8 }, /* cb10 */
	{ "rl c",			0,				 8 }, /* cb11 */
	{ "rl d",			0,				 8 }, /* cb12 */
	{ "rl e",			0,				 8 }, /* cb13 */
	{ "rl h",			0,				 8 }, /* cb14 */
	{ "rl l",			0,				 8 }, /* cb15 */
	{ "rl (hl)",		0,				15 }, /* cb16 */
	{ "rl a",			0,				 8 }, /* cb17 */

	{ "rr b",			0,				 8 }, /* cb18 */
	{ "rr c",			0,				 8 }, /* cb19 */
	{ "rr d",			0,				 8 }, /* cb1a */
	{ "rr e",			0,				 8 }, /* cb1b */
	{ "rr h",			0,				 8 }, /* cb1c */
	{ "rr l",			0,				 8 }, /* cb1d */
	{ "rr (hl)",		0,				15 }, /* cb1e */
	{ "rr a",			0,				 8 }, /* cb1f */

	{ "sla b",			0,				 8 }, /* cb20 */
	{ "sla c",			0,				 8 }, /* cb21 */
	{ "sla d",			0,				 8 }, /* cb22 */
	{ "sla e",			0,				 8 }, /* cb23 */
	{ "sla h",			0,				 8 }, /* cb24 */
	{ "sla l",			0,				 8 }, /* cb25 */
	{ "sla (hl)",		0,				15 }, /* cb26 */
	{ "sla a",			0,				 8 }, /* cb27 */

	{ "sra b",			0,				 8 }, /* cb28 */
	{ "sra c",			0,				 8 }, /* cb29 */
	{ "sra d",			0,				 8 }, /* cb2a */
	{ "sra e",			0,				 8 }, /* cb2b */
	{ "sra h",			0,				 8 }, /* cb2c */
	{ "sra l",			0,				 8 }, /* cb2d */
	{ "sra (hl)",		0,				15 }, /* cb2e */
	{ "sra a",			0,				 8 }, /* cb2f */

	{ "sl1 b",			UNDOC,			 8 }, /* cb30 */
	{ "sl1 c",			UNDOC,			 8 }, /* cb31 */
	{ "sl1 d",			UNDOC,			 8 }, /* cb32 */
	{ "sl1 e",			UNDOC,			 8 }, /* cb33 */
	{ "sl1 h",			UNDOC,			 8 }, /* cb34 */
	{ "sl1 l",			UNDOC,			 8 }, /* cb35 */
	{ "sl1 (hl)",		UNDOC,			15 }, /* cb36 */
	{ "sl1 a",			UNDOC,			 8 }, /* cb37 */

	{ "srl b",			0,				 8 }, /* cb38 */
	{ "srl c",			0,				 8 }, /* cb39 */
	{ "srl d",			0,				 8 }, /* cb3a */
	{ "srl e",			0,				 8 }, /* cb3b */
	{ "srl h",			0,				 8 }, /* cb3c */
	{ "srl l",			0,				 8 }, /* cb3d */
	{ "srl (hl)",		0,				15 }, /* cb3e */
	{ "srl a",			0,				 8 }, /* cb3f */

	{ "bit 0,b",		0,				 8 }, /* cb40 */
	{ "bit 0,c",		0,				 8 }, /* cb41 */
	{ "bit 0,d",		0,				 8 }, /* cb42 */
	{ "bit 0,e",		0,				 8 }, /* cb43 */
	{ "bit 0,h",		0,				 8 }, /* cb44 */
	{ "bit 0,l",		0,				 8 }, /* cb45 */
	{ "bit 0,(hl)",		0,				12 }, /* cb46 */
	{ "bit 0,a",		0,				 8 }, /* cb47 */

	{ "bit 1,b",		0,				 8 }, /* cb48 */
	{ "bit 1,c",		0,				 8 }, /* cb49 */
	{ "bit 1,d",		0,				 8 }, /* cb4a */
	{ "bit 1,e",		0,				 8 }, /* cb4b */
	{ "bit 1,h",		0,				 8 }, /* cb4c */
	{ "bit 1,l",		0,				 8 }, /* cb4d */
	{ "bit 1,(hl)",		0,				12 }, /* cb4e */
	{ "bit 1,a",		0,				 8 }, /* cb4f */

	{ "bit 2,b",		0,				 8 }, /* cb50 */
	{ "bit 2,c",		0,				 8 }, /* cb51 */
	{ "bit 2,d",		0,				 8 }, /* cb52 */
	{ "bit 2,e",		0,				 8 }, /* cb53 */
	{ "bit 2,h",		0,				 8 }, /* cb54 */
	{ "bit 2,l",		0,				 8 }, /* cb55 */
	{ "bit 2,(hl)",		0,				12 }, /* cb56 */
	{ "bit 2,a",		0,				 8 }, /* cb57 */

	{ "bit 3,b",		0,				 8 }, /* cb58 */
	{ "bit 3,c",		0,				 8 }, /* cb59 */
	{ "bit 3,d",		0,				 8 }, /* cb5a */
	{ "bit 3,e",		0,				 8 }, /* cb5b */
	{ "bit 3,h",		0,				 8 }, /* cb5c */
	{ "bit 3,l",		0,				 8 }, /* cb5d */
	{ "bit 3,(hl)",		0,				12 }, /* cb5e */
	{ "bit 3,a",		0,				 8 }, /* cb5f */

	{ "bit 4,b",		0,				 8 }, /* cb60 */
	{ "bit 4,c",		0,				 8 }, /* cb61 */
	{ "bit 4,d",		0,				 8 }, /* cb62 */
	{ "bit 4,e",		0,				 8 }, /* cb63 */
	{ "bit 4,h",		0,				 8 }, /* cb64 */
	{ "bit 4,l",		0,				 8 }, /* cb65 */
	{ "bit 4,(hl)",		0,				12 }, /* cb66 */
	{ "bit 4,a",		0,				 8 }, /* cb67 */

	{ "bit 5,b",		0,				 8 }, /* cb68 */
	{ "bit 5,c",		0,				 8 }, /* cb69 */
	{ "bit 5,d",		0,				 8 }, /* cb6a */
	{ "bit 5,e",		0,				 8 }, /* cb6b */
	{ "bit 5,h",		0,				 8 }, /* cb6c */
	{ "bit 5,l",		0,				 8 }, /* cb6d */
	{ "bit 5,(hl)",		0,				12 }, /* cb6e */
	{ "bit 5,a",		0,				 8 }, /* cb6f */

	{ "bit 6,b",		0,				 8 }, /* cb70 */
	{ "bit 6,c",		0,				 8 }, /* cb71 */
	{ "bit 6,d",		0,				 8 }, /* cb72 */
	{ "bit 6,e",		0,				 8 }, /* cb73 */
	{ "bit 6,h",		0,				 8 }, /* cb74 */
	{ "bit 6,l",		0,				 8 }, /* cb75 */
	{ "bit 6,(hl)",		0,				12 }, /* cb76 */
	{ "bit 6,a",		0,				 8 }, /* cb77 */

	{ "bit 7,b",		0,				 8 }, /* cb78 */
	{ "bit 7,c",		0,				 8 }, /* cb79 */
	{ "bit 7,d",		0,				 8 }, /* cb7a */
	{ "bit 7,e",		0,				 8 }, /* cb7b */
	{ "bit 7,h",		0,				 8 }, /* cb7c */
	{ "bit 7,l",		0,				 8 }, /* cb7d */
	{ "bit 7,(hl)",		0,				12 }, /* cb7e */
	{ "bit 7,a",		0,				 8 }, /* cb7f */

	{ "res 0,b",		0,				 8 }, /* cb80 */
	{ "res 0,c",		0,				 8 }, /* cb81 */
	{ "res 0,d",		0,				 8 }, /* cb82 */
	{ "res 0,e",		0,				 8 }, /* cb83 */
	{ "res 0,h",		0,				 8 }, /* cb84 */
	{ "res 0,l",		0,				 8 }, /* cb85 */
	{ "res 0,(hl)",		0,				15 }, /* cb86 */
	{ "res 0,a",		0,				 8 }, /* cb87 */

	{ "res 1,b",		0,				 8 }, /* cb88 */
	{ "res 1,c",		0,				 8 }, /* cb89 */
	{ "res 1,d",		0,				 8 }, /* cb8a */
	{ "res 1,e",		0,				 8 }, /* cb8b */
	{ "res 1,h",		0,				 8 }, /* cb8c */
	{ "res 1,l",		0,				 8 }, /* cb8d */
	{ "res 1,(hl)",		0,				15 }, /* cb8e */
	{ "res 1,a",		0,				 8 }, /* cb8f */

	{ "res 2,b",		0,				 8 }, /* cb90 */
	{ "res 2,c",		0,				 8 }, /* cb91 */
	{ "res 2,d",		0,				 8 }, /* cb92 */
	{ "res 2,e",		0,				 8 }, /* cb93 */
	{ "res 2,h",		0,				 8 }, /* cb94 */
	{ "res 2,l",		0,				 8 }, /* cb95 */
	{ "res 2,(hl)",		0,				15 }, /* cb96 */
	{ "res 2,a",		0,				 8 }, /* cb97 */

	{ "res 3,b",		0,				 8 }, /* cb98 */
	{ "res 3,c",		0,				 8 }, /* cb99 */
	{ "res 3,d",		0,				 8 }, /* cb9a */
	{ "res 3,e",		0,				 8 }, /* cb9b */
	{ "res 3,h",		0,				 8 }, /* cb9c */
	{ "res 3,l",		0,				 8 }, /* cb9d */
	{ "res 3,(hl)",		0,				15 }, /* cb9e */
	{ "res 3,a",		0,				 8 }, /* cb9f */

	{ "res 4,b",		0,				 8 }, /* cba0 */
	{ "res 4,c",		0,				 8 }, /* cba1 */
	{ "res 4,d",		0,				 8 }, /* cba2 */
	{ "res 4,e",		0,				 8 }, /* cba3 */
	{ "res 4,h",		0,				 8 }, /* cba4 */
	{ "res 4,l",		0,				 8 }, /* cba5 */
	{ "res 4,(hl)",		0,				15 }, /* cba6 */
	{ "res 4,a",		0,				 8 }, /* cba7 */

	{ "res 5,b",		0,				 8 }, /* cba8 */
	{ "res 5,c",		0,				 8 }, /* cba9 */
	{ "res 5,d",		0,				 8 }, /* cbaa */
	{ "res 5,e",		0,				 8 }, /* cbab */
	{ "res 5,h",		0,				 8 }, /* cbac */
	{ "res 5,l",		0,				 8 }, /* cbad */
	{ "res 5,(hl)",		0,				15 }, /* cbae */
	{ "res 5,a",		0,				 8 }, /* cbaf */

	{ "res 6,b",		0,				 8 }, /* cbb0 */
	{ "res 6,c",		0,				 8 }, /* cbb1 */
	{ "res 6,d",		0,				 8 }, /* cbb2 */
	{ "res 6,e",		0,				 8 }, /* cbb3 */
	{ "res 6,h",		0,				 8 }, /* cbb4 */
	{ "res 6,l",		0,				 8 }, /* cbb5 */
	{ "res 6,(hl)",		0,				15 }, /* cbb6 */
	{ "res 6,a",		0,				 8 }, /* cbb7 */

	{ "res 7,b",		0,				 8 }, /* cbb8 */
	{ "res 7,c",		0,				 8 }, /* cbb9 */
	{ "res 7,d",		0,				 8 }, /* cbba */
	{ "res 7,e",		0,				 8 }, /* cbbb */
	{ "res 7,h",		0,				 8 }, /* cbbc */
	{ "res 7,l",		0,				 8 }, /* cbbd */
	{ "res 7,(hl)",		0,				15 }, /* cbbe */
	{ "res 7,a",		0,				 8 }, /* cbbf */

	{ "set 0,b",		0,				 8 }, /* cbc0 */
	{ "set 0,c",		0,				 8 }, /* cbc1 */
	{ "set 0,d",		0,				 8 }, /* cbc2 */
	{ "set 0,e",		0,				 8 }, /* cbc3 */
	{ "set 0,h",		0,				 8 }, /* cbc4 */
	{ "set 0,l",		0,				 8 }, /* cbc5 */
	{ "set 0,(hl)",		0,				15 }, /* cbc6 */
	{ "set 0,a",		0,				 8 }, /* cbc7 */

	{ "set 1,b",		0,				 8 }, /* cbc8 */
	{ "set 1,c",		0,				 8 }, /* cbc9 */
	{ "set 1,d",		0,				 8 }, /* cbca */
	{ "set 1,e",		0,				 8 }, /* cbcb */
	{ "set 1,h",		0,				 8 }, /* cbcc */
	{ "set 1,l",		0,				 8 }, /* cbcd */
	{ "set 1,(hl)",		0,				15 }, /* cbce */
	{ "set 1,a",		0,				 8 }, /* cbcf */

	{ "set 2,b",		0,				 8 }, /* cbd0 */
	{ "set 2,c",		0,				 8 }, /* cbd1 */
	{ "set 2,d",		0,				 8 }, /* cbd2 */
	{ "set 2,e",		0,				 8 }, /* cbd3 */
	{ "set 2,h",		0,				 8 }, /* cbd4 */
	{ "set 2,l",		0,				 8 }, /* cbd5 */
	{ "set 2,(hl)",		0,				15 }, /* cbd6 */
	{ "set 2,a",		0,				 8 }, /* cbd7 */

	{ "set 3,b",		0,				 8 }, /* cbd8 */
	{ "set 3,c",		0,				 8 }, /* cbd9 */
	{ "set 3,d",		0,				 8 }, /* cbda */
	{ "set 3,e",		0,				 8 }, /* cbdb */
	{ "set 3,h",		0,				 8 }, /* cbdc */
	{ "set 3,l",		0,				 8 }, /* cbdd */
	{ "set 3,(hl)",		0,				15 }, /* cbde */
	{ "set 3,a",		0,				 8 }, /* cbdf */

	{ "set 4,b",		0,				 8 }, /* cbe0 */
	{ "set 4,c",		0,				 8 }, /* cbe1 */
	{ "set 4,d",		0,				 8 }, /* cbe2 */
	{ "set 4,e",		0,				 8 }, /* cbe3 */
	{ "set 4,h",		0,				 8 }, /* cbe4 */
	{ "set 4,l",		0,				 8 }, /* cbe5 */
	{ "set 4,(hl)",		0,				15 }, /* cbe6 */
	{ "set 4,a",		0,				 8 }, /* cbe7 */

	{ "set 5,b",		0,				 8 }, /* cbe8 */
	{ "set 5,c",		0,				 8 }, /* cbe9 */
	{ "set 5,d",		0,				 8 }, /* cbea */
	{ "set 5,e",		0,				 8 }, /* cbeb */
	{ "set 5,h",		0,				 8 }, /* cbec */
	{ "set 5,l",		0,				 8 }, /* cbed */
	{ "set 5,(hl)",		0,				15 }, /* cbee */
	{ "set 5,a",		0,				 8 }, /* cbef */

	{ "set 6,b",		0,				 8 }, /* cbf0 */
	{ "set 6,c",		0,				 8 }, /* cbf1 */
	{ "set 6,d",		0,				 8 }, /* cbf2 */
	{ "set 6,e",		0,				 8 }, /* cbf3 */
	{ "set 6,h",		0,				 8 }, /* cbf4 */
	{ "set 6,l",		0,				 8 }, /* cbf5 */
	{ "set 6,(hl)",		0,				15 }, /* cbf6 */
	{ "set 6,a",		0,				 8 }, /* cbf7 */

	{ "set 7,b",		0,				 8 }, /* cbf8 */
	{ "set 7,c",		0,				 8 }, /* cbf9 */
	{ "set 7,d",		0,				 8 }, /* cbfa */
	{ "set 7,e",		0,				 8 }, /* cbfb */
	{ "set 7,h",		0,				 8 }, /* cbfc */
	{ "set 7,l",		0,				 8 }, /* cbfd */
	{ "set 7,(hl)",		0,				15 }, /* cbfe */
	{ "set 7,a",		0,				 8 }, /* cbff */
  },
  {							/* dd fd */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd00 fd00 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd01 fd01 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd02 fd02 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd03 fd03 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd04 fd04 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd05 fd05 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd06 fd06 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd07 fd07 */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd08 fd08 */
	{ "add ix,bc",		0,				15 }, /* dd09 fd09 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd0a fd0a */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd0b fd0b */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd0c fd0c */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd0d fd0d */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd0e fd0e */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd0f fd0f */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd10 fd10 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd11 fd11 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd12 fd12 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd13 fd13 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd14 fd14 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd15 fd15 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd16 fd16 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd17 fd17 */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd18 fd18 */
	{ "add ix,de",		0,				15 }, /* dd19 fd19 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd1a fd1a */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd1b fd1b */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd1c fd1c */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd1d fd1d */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd1e fd1e */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd1f fd1f */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd20 fd20 */
	{ "ld ix,%s",		WC,				14 }, /* dd21 fd21 */
	{ "ld (%s),ix",		ADDR,			20 }, /* dd22 fd22 */
	{ "inc ix",			0,				10 }, /* dd23 fd23 */
	{ "inc ixh",		UNDOC,			 8 }, /* dd24 fd24 */
	{ "dec ixh",		UNDOC,			 8 }, /* dd25 fd25 */
	{ "ld ixh,%s",		BC,				11 }, /* dd26 fd26 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd27 fd27 */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd28 fd28 */
	{ "add ix,ix",		0,				15 }, /* dd29 fd29 */
	{ "ld ix,(%s)",		ADDR,			20 }, /* dd2a fd2a */
	{ "dec ix",			0,				10 }, /* dd2b fd2b */
	{ "inc ixl",		UNDOC,			 8 }, /* dd2c fd2c */
	{ "dec ixl",		UNDOC,			 8 }, /* dd2d fd2d */
	{ "ld ixl,%s",		BC,				11 }, /* dd2e fd2e */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd2f fd2f */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd30 fd30 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd31 fd31 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd32 fd32 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd33 fd33 */
	{ "inc (ix%s)",		IND,			23 }, /* dd34 fd34 */
	{ "dec (ix%s)",		IND,			23 }, /* dd35 fd35 */
	{ "ld (ix%s),%s",	IND_1,			19 }, /* dd36 fd36 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd37 fd37 */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd38 fd38 */
	{ "add ix,sp",		0,				15 }, /* dd39 fd39 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd3a fd3a */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd3b fd3b */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd3c fd3c */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd3d fd3d */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd3e fd3e */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd3f fd3f */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd40 fd40 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd41 fd41 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd42 fd42 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd43 fd43 */
	{ "ld b,ixh",		UNDOC,			 8 }, /* dd44 fd44 */
	{ "ld b,ixl",		UNDOC,			 8 }, /* dd45 fd45 */
	{ "ld b,(ix%s)",	IND,			19 }, /* dd46 fd46 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd47 fd47 */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd48 fd48 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd49 fd49 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd4a fd4a */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd4b fd4b */
	{ "ld c,ixh",		UNDOC,			 8 }, /* dd4c fd4c */
	{ "ld c,ixl",		UNDOC,			 8 }, /* dd4d fd4d */
	{ "ld c,(ix%s)",	IND,			19 }, /* dd4e fd4e */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd4f fd4f */
	
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd50 fd50 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd51 fd51 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd52 fd52 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd53 fd53 */
	{ "ld d,ixh",		UNDOC,			 8 }, /* dd54 fd54 */
	{ "ld d,ixl",		UNDOC,			 8 }, /* dd55 fd55 */
	{ "ld d,(ix%s)",	IND,			19 }, /* dd56 fd56 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd57 fd57 */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd58 fd58 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd59 fd59 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd5a fd5a */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd5b fd5b */
	{ "ld e,ixh",		UNDOC,			 8 }, /* dd5c fd5c */
	{ "ld e,ixl",		UNDOC,			 8 }, /* dd5d fd5d */
	{ "ld e,(ix%s)",	IND,			19 }, /* dd5e fd5e */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd5f fd5f */
	
	{ "ld ixh,b",		UNDOC,			 8 }, /* dd60 fd60 */
	{ "ld ixh,c",		UNDOC,			 8 }, /* dd61 fd61 */
	{ "ld ixh,d",		UNDOC,			 8 }, /* dd62 fd62 */
	{ "ld ixh,e",		UNDOC,			 8 }, /* dd63 fd63 */
	{ "ld ixh,ixh",		UNDOC,			 8 }, /* dd64 fd64 */
	{ "ld ixh,ixl",		UNDOC,			 8 }, /* dd65 fd65 */
	{ "ld h,(ix%s)",	IND,			19 }, /* dd66 fd66 */
	{ "ld ixh,a",		UNDOC,			 8 }, /* dd67 fd67 */

	{ "ld ixl,b",		UNDOC,			 8 }, /* dd68 fd68 */
	{ "ld ixl,c",		UNDOC,			 8 }, /* dd69 fd69 */
	{ "ld ixl,d",		UNDOC,			 8 }, /* dd6a fd6a */
	{ "ld ixl,e",		UNDOC,			 8 }, /* dd6b fd6b */
	{ "ld ixl,ixh",		UNDOC,			 8 }, /* dd6c fd6c */
	{ "ld ixl,ixl",		UNDOC,			 8 }, /* dd6d fd6d */
	{ "ld l,(ix%s)",	IND,			19 }, /* dd6e fd6e */
	{ "ld ixl,a",		UNDOC,			 8 }, /* dd6f fd6f */
	
	{ "ld (ix%s),b",	IND,			19 }, /* dd70 fd70 */
	{ "ld (ix%s),c",	IND,			19 }, /* dd71 fd71 */
	{ "ld (ix%s),d",	IND,			19 }, /* dd72 fd72 */
	{ "ld (ix%s),e",	IND,			19 }, /* dd73 fd73 */
	{ "ld (ix%s),h",	IND,			19 }, /* dd74 fd74 */
	{ "ld (ix%s),l",	IND,			19 }, /* dd75 fd75 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd76 fd76 */
	{ "ld (ix%s),a",	IND,			19 }, /* dd77 fd77 */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd78 fd78 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd79 fd79 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd7a fd7a */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd7b fd7b */
	{ "ld a,ixh",		UNDOC,			 8 }, /* dd7c fd7c */
	{ "ld a,ixl",		UNDOC,			 8 }, /* dd7d fd7d */
	{ "ld a,(ix%s)",	IND,			19 }, /* dd7e fd7e */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd7f fd7f */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd80 fd80 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd81 fd81 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd82 fd82 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd83 fd83 */
	{ "add a,ixh",		UNDOC,			 8 }, /* dd84 fd84 */
	{ "add a,ixl",		UNDOC,			 8 }, /* dd85 fd85 */
	{ "add a,(ix%s)",	IND,			19 }, /* dd86 fd86 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd87 fd87 */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd88 fd88 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd89 fd89 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd8a fd8a */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd8b fd8b */
	{ "adc a,ixh",		UNDOC,			 8 }, /* dd8c fd8c */
	{ "adc a,ixl",		UNDOC,			 8 }, /* dd8d fd8d */
	{ "adc a,(ix%s)",	IND,			19 }, /* dd8e fd8e */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd8f fd8f */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd90 fd90 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd91 fd91 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd92 fd92 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd93 fd93 */
	{ "sub ixh",		UNDOC,			 8 }, /* dd94 fd94 */
	{ "sub ixl",		UNDOC,			 8 }, /* dd95 fd95 */
	{ "sub (ix%s)",		IND,			19 }, /* dd96 fd96 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd97 fd97 */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd98 fd98 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd99 fd99 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd9a fd9a */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd9b fd9b */
	{ "sbc ixh",		UNDOC,			 8 }, /* dd9c fd9c */
	{ "sbx ixl",		UNDOC,			 8 }, /* dd9d fd9d */
	{ "sbc a,(ix%s)",	IND,			19 }, /* dd9e fd9e */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dd9f fd9f */
	
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dda0 fda0 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dda1 fda1 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dda2 fda2 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dda3 fda3 */
	{ "and ixh",		UNDOC,			 8 }, /* dda4 fda4 */
	{ "and ixl",		UNDOC,			 8 }, /* dda5 fda5 */
	{ "and (ix%s)",		IND,			19 }, /* dda6 fda6 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dda7 fda7 */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* dda8 fda8 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dda9 fda9 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddaa fdaa */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddab fdab */
	{ "xor ixh",		UNDOC,			 8 }, /* ddac fdac */
	{ "xor ixl",		UNDOC,			 8 }, /* ddad fdad */
	{ "xor (ix%s)",		IND,			19 }, /* ddae fdae */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddaf fdaf */
	
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddb0 fdb0 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddb1 fdb1 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddb2 fdb2 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddb3 fdb3 */
	{ "or ixh",			UNDOC,			 8 }, /* ddb4 fdb4 */
	{ "or ixl",			UNDOC,			 8 }, /* ddb5 fdb5 */
	{ "or (ix%s)",		IND,			19 }, /* ddb6 fdb6 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddb7 fdb7 */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddb8 fdb8 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddb9 fdb9 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddba fdba */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddbb fdbb */
	{ "cp ixh",			UNDOC,			 8 }, /* ddbc fdbc */
	{ "cp ixl",			UNDOC,			 8 }, /* ddbd fdbd */
	{ "cp (ix%s)",		IND,			19 }, /* ddbe fdbe */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddbf fdbf */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddc0 fdc0 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddc1 fdc1 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddc2 fdc2 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddc3 fdc3 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddc4 fdc4 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddc5 fdc5 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddc6 fdc6 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddc7 fdc7 */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddc8 fdc8 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddc9 fdc9 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddca fdca */
	{ "dd cb",			3,				 0 }, /* ddcb fdcb */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddcc fdcc */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddcd fdcd */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddce fdce */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddcf fdcf */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddd0 fdd0 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddd1 fdd1 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddd2 fdd2 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddd3 fdd3 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddd4 fdd4 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddd5 fdd5 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddd6 fdd6 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddd7 fdd7 */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddd8 fdd8 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddd9 fdd9 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddda fdda */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dddb fddb */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dddc fddc */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dddd fddd */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddde fdde */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dddf fddf */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* dde0 fde0 */
	{ "pop ix",			0,				14 }, /* dde1 fde1 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dde2 fde2 */
	{ "ex (sp),ix",		0,				23 }, /* dde3 fde3 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dde4 fde4 */
	{ "push ix",		0,				15 }, /* dde5 fde5 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dde6 fde6 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dde7 fde7 */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* dde8 fde8 */
	{ "jp (ix)",		0 | UNBR,		 8 }, /* dde9 fde9 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddea fdea */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* eb */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddec fdec */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* dded fded */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddee fdee */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddef fdef */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddf0 fdf0 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddf1 fdf1 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddf2 fdf2 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddf3 fdf3 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddf4 fdf4 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddf5 fdf5 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddf6 fdf6 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddf7 fdf7 */

	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddf8 fdf8 */
	{ "ld sp,ix",		0,				10 }, /* ddf9 fdf9 */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddfa fdfa */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddfb fdfb */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddfc fdfc */
	{ pfix,				UNDOC | NOEFF, 	 4 }, /* ddfd fdfd */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddfe fdfe */
	{ pfix,				UNDOC | NOEFF,	 4 }, /* ddff fdff */
  },
  {						/* ed */
	{ ednop,			UNDOC,			 8 }, /* ed00 */
	{ ednop,			UNDOC,			 8 }, /* ed01 */
	{ ednop,			UNDOC,			 8 }, /* ed02 */
	{ ednop,			UNDOC,			 8 }, /* ed03 */
	{ ednop,			UNDOC,			 8 }, /* ed04 */
	{ ednop,			UNDOC,			 8 }, /* ed05 */
	{ ednop,			UNDOC,			 8 }, /* ed06 */
	{ ednop,			UNDOC,			 8 }, /* ed07 */

	{ ednop,			UNDOC,			 8 }, /* ed08 */
	{ ednop,			UNDOC,			 8 }, /* ed09 */
	{ ednop,			UNDOC,			 8 }, /* ed0a */
	{ ednop,			UNDOC,			 8 }, /* ed0b */
	{ ednop,			UNDOC,			 8 }, /* ed0c */
	{ ednop,			UNDOC,			 8 }, /* ed0d */
	{ ednop,			UNDOC,			 8 }, /* ed0e */
	{ ednop,			UNDOC,			 8 }, /* ed0f */

	{ ednop,			UNDOC,			 8 }, /* ed10 */
	{ ednop,			UNDOC,			 8 }, /* ed11 */
	{ ednop,			UNDOC,			 8 }, /* ed12 */
	{ ednop,			UNDOC,			 8 }, /* ed13 */
	{ ednop,			UNDOC,			 8 }, /* ed14 */
	{ ednop,			UNDOC,			 8 }, /* ed15 */
	{ ednop,			UNDOC,			 8 }, /* ed16 */
	{ ednop,			UNDOC,			 8 }, /* ed17 */

	{ ednop,			UNDOC,			 8 }, /* ed18 */
	{ ednop,			UNDOC,			 8 }, /* ed19 */
	{ ednop,			UNDOC,			 8 }, /* ed1a */
	{ ednop,			UNDOC,			 8 }, /* ed1b */
	{ ednop,			UNDOC,			 8 }, /* ed1c */
	{ ednop,			UNDOC,			 8 }, /* ed1d */
	{ ednop,			UNDOC,			 8 }, /* ed1e */
	{ ednop,			UNDOC,			 8 }, /* ed1f */

	{ ednop,			UNDOC,			 8 }, /* ed20 */
	{ ednop,			UNDOC,			 8 }, /* ed21 */
	{ ednop,			UNDOC,			 8 }, /* ed22 */
	{ ednop,			UNDOC,			 8 }, /* ed23 */
	{ ednop,			UNDOC,			 8 }, /* ed24 */
	{ ednop,			UNDOC,			 8 }, /* ed25 */
	{ ednop,			UNDOC,			 8 }, /* ed26 */
	{ ednop,			UNDOC,			 8 }, /* ed27 */

	{ ednop,			UNDOC,			 8 }, /* ed28 */
	{ ednop,			UNDOC,			 8 }, /* ed29 */
	{ ednop,			UNDOC,			 8 }, /* ed2a */
	{ ednop,			UNDOC,			 8 }, /* ed2b */
	{ ednop,			UNDOC,			 8 }, /* ed2c */
	{ ednop,			UNDOC,			 8 }, /* ed2d */
	{ ednop,			UNDOC,			 8 }, /* ed2e */
	{ ednop,			UNDOC,			 8 }, /* ed2f */

	{ ednop,			UNDOC,			 8 }, /* ed30 */
	{ ednop,			UNDOC,			 8 }, /* ed31 */
	{ ednop,			UNDOC,			 8 }, /* ed32 */
	{ ednop,			UNDOC,			 8 }, /* ed33 */
	{ ednop,			UNDOC,			 8 }, /* ed34 */
	{ ednop,			UNDOC,			 8 }, /* ed35 */
	{ ednop,			UNDOC,			 8 }, /* ed36 */
	{ ednop,			UNDOC,			 8 }, /* ed37 */

	{ ednop,			UNDOC,			 8 }, /* ed38 */
	{ ednop,			UNDOC,			 8 }, /* ed39 */
	{ ednop,			UNDOC,			 8 }, /* ed3a */
	{ ednop,			UNDOC,			 8 }, /* ed3b */
	{ ednop,			UNDOC,			 8 }, /* ed3c */
	{ ednop,			UNDOC,			 8 }, /* ed3d */
	{ ednop,			UNDOC,			 8 }, /* ed3e */
	{ ednop,			UNDOC,			 8 }, /* ed3f */

	{ "in b,(c)",		0,				12 }, /* ed40 */
	{ "out (c),b",		0,				12 }, /* ed41 */
	{ "sbc hl,bc",		0,				15 }, /* ed42 */
	{ "ld (%s),bc",		ADDR,			20 }, /* ed43 */
	{ "neg",			0,				 8 }, /* ed44 */
	{ "retn",			0 | UNBR,		14 }, /* ed45 */
	{ "im 0",			0,				 8 }, /* ed46 */
	{ "ld i,a",			0,				 9 }, /* ed47 */

	{ "in c,(c)",		0,				12 }, /* ed48 */
	{ "out (c),c",		0,				12 }, /* ed49 */
	{ "adc hl,bc",		0,				15 }, /* ed4a */
	{ "ld bc,(%s)",		ADDR,			20 }, /* ed4b */
	{ "neg",			UNDOC,			 8 }, /* ed4c */
	{ "reti",			0 | UNBR,		14 }, /* ed4d */
	{ "im 0",			UNDOC,			 8 }, /* ed4e */
	{ "ld r,a",			0,				 9 }, /* ed4f */

	{ "in d,(c)",		0,				12 }, /* ed50 */
	{ "out (c),d",		0,				12 }, /* ed51 */
	{ "sbc hl,de",		0,				15 }, /* ed52 */
	{ "ld (%s),de",		ADDR,			20 }, /* ed53 */
	{ "neg",			UNDOC,			 8 }, /* ed54 */
	{ "retn",			UNDOC | UNBR,	14 }, /* ed55 */
	{ "im 1",			0,				 8 }, /* ed56 */
	{ "ld a,i",			0,				 9 }, /* ed57 */

	{ "in e,(c)",		0,				12 }, /* ed58 */
	{ "out (c),e",		0,				12 }, /* ed59 */
	{ "adc hl,de",		0,				15 }, /* ed5a */
	{ "ld de,(%s)",		ADDR,			20 }, /* ed5b */
	{ "neg",			UNDOC,			 8 }, /* ed5c */
	{ "retn",			UNDOC | UNBR,	14 }, /* ed5d */
	{ "im 2",			0,				 8 }, /* ed5e */
	{ "ld a,r",			0,				 9 }, /* ed5f */

	{ "in h,(c)",		0,				12 }, /* ed60 */
	{ "out (c),h",		0,				12 }, /* ed61 */
	{ "sbc hl,hl",		0,				15 }, /* ed62 */
	{ "ld (%s),hl",		ADDR,			20 }, /* ed63 */
	{ "neg",			UNDOC,			 8 }, /* ed64 */
	{ "retn",			UNDOC | UNBR,	14 }, /* ed65 */
	{ "im 0",			UNDOC,			 8 }, /* ed66 */
	{ "rrd",			0,				18 }, /* ed67 */

	{ "in l,(c)",		0,				12 }, /* ed68 */
	{ "out (c),l",		0,				12 }, /* ed69 */
	{ "adc hl,hl",		0,				15 }, /* ed6a */
	{ "ld hl,(%s)",		ADDR,			20 }, /* ed6b */
	{ "neg",			UNDOC,			 8 }, /* ed6c */
	{ "retn",			UNDOC | UNBR,	14 }, /* ed6d */
	{ "im 0",			UNDOC,			 8 }, /* ed6e */
	{ "rld",			0,				18 }, /* ed6f */

	{ "in (c)",			UNDOC,			12 }, /* ed70 */
	{ "out (c),0",		UNDOC,			12 }, /* ed71 */
	{ "sbc hl,sp",		0,				15 }, /* ed72 */
	{ "ld (%s),sp",		ADDR,			20 }, /* ed73 */
	{ "neg",			UNDOC,			 8 }, /* ed74 */
	{ "retn",			UNDOC | UNBR,	14 }, /* ed75 */
	{ "im 1",			UNDOC,			 8 }, /* ed76 */
	{ ednop,			UNDOC,			 8 }, /* ed77 */

	{ "in a,(c)",		0,				12 }, /* ed78 */
	{ "out (c),a",		0,				12 }, /* ed79 */
	{ "adc hl,sp",		0,				15 }, /* ed7a */
	{ "ld sp,(%s)",		ADDR,			20 }, /* ed7b */
	{ "neg",			UNDOC,			 8 }, /* ed7c */
	{ "retn",			UNDOC | UNBR,	14 }, /* ed7d */
	{ "im 2",			UNDOC,			 8 }, /* ed7e */
	{ ednop,			UNDOC,			 8 }, /* ed7f */

	{ ednop,			UNDOC,			 8 }, /* ed80 */
	{ ednop,			UNDOC,			 8 }, /* ed81 */
	{ ednop,			UNDOC,			 8 }, /* ed82 */
	{ ednop,			UNDOC,			 8 }, /* ed83 */
	{ ednop,			UNDOC,			 8 }, /* ed84 */
	{ ednop,			UNDOC,			 8 }, /* ed85 */
	{ ednop,			UNDOC,			 8 }, /* ed86 */
	{ ednop,			UNDOC,			 8 }, /* ed87 */

	{ ednop,			UNDOC,			 8 }, /* ed88 */
	{ ednop,			UNDOC,			 8 }, /* ed89 */
	{ ednop,			UNDOC,			 8 }, /* ed8a */
	{ ednop,			UNDOC,			 8 }, /* ed8b */
	{ ednop,			UNDOC,			 8 }, /* ed8c */
	{ ednop,			UNDOC,			 8 }, /* ed8d */
	{ ednop,			UNDOC,			 8 }, /* ed8e */
	{ ednop,			UNDOC,			 8 }, /* ed8f */

	{ ednop,			UNDOC,			 8 }, /* ed90 */
	{ ednop,			UNDOC,			 8 }, /* ed91 */
	{ ednop,			UNDOC,			 8 }, /* ed92 */
	{ ednop,			UNDOC,			 8 }, /* ed93 */
	{ ednop,			UNDOC,			 8 }, /* ed94 */
	{ ednop,			UNDOC,			 8 }, /* ed95 */
	{ ednop,			UNDOC,			 8 }, /* ed96 */
	{ ednop,			UNDOC,			 8 }, /* ed97 */

	{ ednop,			UNDOC,			 8 }, /* ed98 */
	{ ednop,			UNDOC,			 8 }, /* ed99 */
	{ ednop,			UNDOC,			 8 }, /* ed9a */
	{ ednop,			UNDOC,			 8 }, /* ed9b */
	{ ednop,			UNDOC,			 8 }, /* ed9c */
	{ ednop,			UNDOC,			 8 }, /* ed9d */
	{ ednop,			UNDOC,			 8 }, /* ed9e */
	{ ednop,			UNDOC,			 8 }, /* ed9f */

	{ "ldi",			0,				16 }, /* eda0 */
	{ "cpi",			0,				16 }, /* eda1 */
	{ "ini",			0,				16 }, /* eda2 */
	{ "outi",			0,				16 }, /* eda3 */
	{ ednop,			UNDOC,			 8 }, /* eda4 */
	{ ednop,			UNDOC,			 8 }, /* eda5 */
	{ ednop,			UNDOC,			 8 }, /* eda6 */
	{ ednop,			UNDOC,			 8 }, /* eda7 */

	{ "ldd",			0,				16 }, /* eda8 */
	{ "cpd",			0,				16 }, /* eda9 */
	{ "ind",			0,				16 }, /* edaa */
	{ "outd",			0,				16 }, /* edab */
	{ ednop,			UNDOC,			 8 }, /* edac */
	{ ednop,			UNDOC,			 8 }, /* edad */
	{ ednop,			UNDOC,			 8 }, /* edae */
	{ ednop,			UNDOC,			 8 }, /* edaf */

	{ "ldir",			0,		 VT(16, 21)}, /* edb0 */
	{ "cpir",			0,		 VT(16, 21)}, /* edb1 */
	{ "inir",			0,		 VT(16, 21)}, /* edb2 */
	{ "otir",			0,		 VT(16, 21)}, /* edb3 */
	{ ednop,			UNDOC,			 8 }, /* edb4 */
	{ ednop,			UNDOC,			 8 }, /* edb5 */
	{ ednop,			UNDOC,			 8 }, /* edb6 */
	{ ednop,			UNDOC,			 8 }, /* edb7 */

	{ "lddr",			0,		 VT(16, 21)}, /* edb8 */
	{ "cpdr",			0,		 VT(16, 21)}, /* edb9 */
	{ "indr",			0,		 VT(16, 21)}, /* edba */
	{ "otdr",			0,		 VT(16, 21)}, /* edbb */
	{ ednop,			UNDOC,			 8 }, /* edbc */
	{ ednop,			UNDOC,			 8 }, /* edbd */
	{ ednop,			UNDOC,			 8 }, /* edbe */
	{ ednop,			UNDOC,			 8 }, /* edbf */

	{ ednop,			UNDOC,			 8 }, /* edc0 */
	{ ednop,			UNDOC,			 8 }, /* edc1 */
	{ ednop,			UNDOC,			 8 }, /* edc2 */
	{ ednop,			UNDOC,			 8 }, /* edc3 */
	{ ednop,			UNDOC,			 8 }, /* edc4 */
	{ ednop,			UNDOC,			 8 }, /* edc5 */
	{ ednop,			UNDOC,			 8 }, /* edc6 */
	{ ednop,			UNDOC,			 8 }, /* edc7 */

	{ ednop,			UNDOC,			 8 }, /* edc8 */
	{ ednop,			UNDOC,			 8 }, /* edc9 */
	{ ednop,			UNDOC,			 8 }, /* edca */
	{ ednop,			UNDOC,			 8 }, /* edcb */
	{ ednop,			UNDOC,			 8 }, /* edcc */
	{ ednop,			UNDOC,			 8 }, /* edcd */
	{ ednop,			UNDOC,			 8 }, /* edce */
	{ ednop,			UNDOC,			 8 }, /* edcf */

	{ ednop,			UNDOC,			 8 }, /* edd0 */
	{ ednop,			UNDOC,			 8 }, /* edd1 */
	{ ednop,			UNDOC,			 8 }, /* edd2 */
	{ ednop,			UNDOC,			 8 }, /* edd3 */
	{ ednop,			UNDOC,			 8 }, /* edd4 */
	{ ednop,			UNDOC,			 8 }, /* edd5 */
	{ ednop,			UNDOC,			 8 }, /* edd6 */
	{ ednop,			UNDOC,			 8 }, /* edd7 */

	{ ednop,			UNDOC,			 8 }, /* edd8 */
	{ ednop,			UNDOC,			 8 }, /* edd9 */
	{ ednop,			UNDOC,			 8 }, /* edda */
	{ ednop,			UNDOC,			 8 }, /* eddb */
	{ ednop,			UNDOC,			 8 }, /* eddc */
	{ ednop,			UNDOC,			 8 }, /* eddd */
	{ ednop,			UNDOC,			 8 }, /* edde */
	{ ednop,			UNDOC,			 8 }, /* eddf */

	{ ednop,			UNDOC,			 8 }, /* ede0 */
	{ ednop,			UNDOC,			 8 }, /* ede1 */
	{ ednop,			UNDOC,			 8 }, /* ede2 */
	{ ednop,			UNDOC,			 8 }, /* ede3 */
	{ ednop,			UNDOC,			 8 }, /* ede4 */
	{ ednop,			UNDOC,			 8 }, /* ede5 */
	{ ednop,			UNDOC,			 8 }, /* ede6 */
	{ ednop,			UNDOC,			 8 }, /* ede7 */

	{ ednop,			UNDOC,			 8 }, /* ede8 */
	{ ednop,			UNDOC,			 8 }, /* ede9 */
	{ ednop,			UNDOC,			 8 }, /* edea */
	{ ednop,			UNDOC,			 8 }, /* edeb */
	{ ednop,			UNDOC,			 8 }, /* edec */
	{ ednop,			UNDOC,			 8 }, /* eded */
	{ ednop,			UNDOC,			 8 }, /* edee */
	{ ednop,			UNDOC,			 8 }, /* edef */

	{ ednop,			UNDOC,			 8 }, /* edf0 */
	{ ednop,			UNDOC,			 8 }, /* edf1 */
	{ ednop,			UNDOC,			 8 }, /* edf2 */
	{ ednop,			UNDOC,			 8 }, /* edf3 */
	{ ednop,			UNDOC,			 8 }, /* edf4 */
	{ ednop,			UNDOC,			 8 }, /* edf5 */
	{ ednop,			UNDOC,			 8 }, /* edf6 */
	{ ednop,			UNDOC,			 8 }, /* edf7 */

	{ ednop,			UNDOC,			 8 }, /* edf8 */
	{ ednop,			UNDOC,			 8 }, /* edf9 */
	{ ednop,			UNDOC,			 8 }, /* edfa */
	{ ednop,			UNDOC,			 8 }, /* edfb */
	{ ednop,			UNDOC,			 8 }, /* edfc */
	{ ednop,			UNDOC,			 8 }, /* edfd */
	{ ednop,			UNDOC,			 8 }, /* edfe */
	{ ednop,			UNDOC,			 8 }, /* edff */
  }
};

struct OpcodePatch {
	int		op;
	Opcode	code;
};

// TODO: reminder: where to these come from?

#if 0
static OpcodePatch change8080[] = {
	{ 0x08, { "sub hl,bc",		0,				15 } },
	{ 0x10, { "asr hl", 		0,				 8 } },
	{ 0x18, { "lsl de", 		0,				 8 } },
	{ 0x20, { undefined, 		0,				 0 } },	// 8085 rim
	{ 0x28, { "ld de,hl+%s",	BC,				13 } },
	{ 0x30, { undefined, 		0,				 0 } },	// 8080 sim
	{ 0x38, { "ld de,sp+%s",	BC,				13 } },
	{ 0xcb, { "rst v,40h",		RST,			11 } },
	{ 0xd9, { "ld (de),hl",		0,				17 } },
	{ 0xed, { "ld hl,(de)",		0,				17 } },
	{ 0xdd, { "jp nx5,%s",		ADDR | UNBR,	10 } },
	{ 0xfd, { "jp x5,%s",		ADDR | UNBR,	10 } }
};
#endif

Zi80dis::Zi80dis()
{
	m_processor = procZ80;
	m_assemblerable = true;
	m_dollarhex = false;
	m_undoc = false;
}

void Zi80dis::SetProcessor(enum Zi80dis::Processor proc)
{
	m_processor = proc;
}

void Zi80dis::SetAssemblerable(bool assemblerable)
{
	m_assemblerable = assemblerable;
}

void Zi80dis::SetDollarHex(bool dollarhex)
{
	m_dollarhex = dollarhex;
	if (m_dollarhex)
		m_assemblerable = false;
}

void Zi80dis::SetUndocumented(bool undoc)
{
	m_undoc = undoc;
}

bool Zi80dis::IsUndefined()
{
	return m_minT == 0;
}

#define SIGN_EXTEND(byte)	(((byte) & 0x80) ? -(((byte) ^ 255) + 1) : (byte))

void Zi80dis::Disassemble(const unsigned char *mem, int pc, bool memPointsToInstruction)
{
	m_minT = 0;
	m_maxT = 0;
	m_ocf = 0;
	m_min8080T = 0;
	m_max8080T = 0;
	m_length = 0;

#define FETCH_TO(var) var = memPointsToInstruction	\
		? mem[m_length] 							\
		: mem[(pc + m_length) & 0xffff];			\
	m_length++

	int FETCH_TO(op);
	int op0 = op;
	const Opcode *code = &major[op];
	int args;

	if (code->tstates8080 > 256)
	{
		m_min8080T += code->tstates8080 >> 8;
		m_max8080T += code->tstates8080 & 0xff;
	}
	else
	{
		m_min8080T += code->tstates8080;
		m_max8080T = m_min8080T;
	}

	m_ocf++;

	if (m_processor == proc8080)
	{
#if 0
		// Check if there are replacements for 8080 instructions.

		for (int i = 0; i < sizeof(change8080) / sizeof(change8080[0]); i++)
		{
			if (op == change8080[i].op)
			{
				code = &change8080[i].code;
				break;
			}
		}
#endif
		args = code->args;
	}
	else if (!major[op].name)
	{
		args = major[op0].args;
		FETCH_TO(op);
		// Check DD/FD prefix specially for no effect.
		if (args == 1 && (minor[args][op].args & NOEFF))
		{
			m_length--;
			code = &minor[args][op];
		}
		else
		{
			m_ocf++; // book says at most 2 op code fetches even for DDCB (TODO: really?)
			while (!minor[args][op].name)
			{
				args = minor[args][op].args;
				FETCH_TO(op);
			}
			code = &minor[args][op];
		}
	}

	m_neverNext = (code->args & UNBR) != 0;
	m_stableMask = (1 << m_length) - 1;	// opcodes are stable
	m_numArg = 0;

	if (!m_undoc && (code->args & UNDOC))
	{
		strcpy(m_format, undefined);
		return;
	}

	switch (code->args & ~FLGMSK)
	{
	case 3:	/* DD CB or FD CB instructions */
		m_stableMask &= ~4;	// Offset at byte 3 is not stable.
		m_stableMask |= 8;	// Opcode at byte 4 is stable.
		m_numArg = 1;
		FETCH_TO(op);
		m_arg[0] = SIGN_EXTEND(op);
		m_argType[0] = Zi80dis::argIndex;
		FETCH_TO(op);
		code = &minor[0][op];
		
		{
			// Find (hl) in normal instruction description.
			const char *left = strchr(code->name, '(');
			if (!left) {
				if (!m_undoc)
				{
					strcpy(m_format, undefined); // No (hl), don't want undocumented instructions.
				}
				else
				{
					left = strchr(code->name, '\0') - 1;
				}
			}

			if (left)
			{
					sprintf(m_format, "%.*s(ix%%s)", (int)(left - code->name), code->name);
					if (*left != '(' && strncmp(code->name, "bit", 3) != 0)
					{
						strcat(m_format, ",");
						strcat(m_format, left);
					}
					m_minT = 8;
					m_maxT = 8;
			}
		}
		break;
	case IND:
		m_numArg = 1;
		FETCH_TO(op);
		m_arg[0] = SIGN_EXTEND(op);
		m_argType[0] = argIndex;
		strcpy(m_format, code->name);
		break;
	case IND_1:
		m_numArg = 2;
		FETCH_TO(op);
		m_arg[0] = SIGN_EXTEND(op);
		m_argType[0] = argIndex;
		FETCH_TO(op);
		m_arg[1] = op;
		m_argType[1] = argByte;
		strcpy(m_format, code->name);
		break;
	case PORT:
		m_numArg = 1;
		FETCH_TO(op);
		m_arg[0] = op;
		m_argType[0] = argPort;
		strcpy(m_format, code->name);
		break;
	case 0:
		strcpy(m_format, code->name);
		break;
	case BC:
		m_numArg = 1;
		FETCH_TO(op);
		m_arg[0] = op;
		m_argType[0] = argByte;
		strcpy(m_format, code->name);
		break;
	case WC:
		m_numArg = 1;
		FETCH_TO(op);
		m_arg[0] = op;
		FETCH_TO(op);
		m_arg[0] |= op << 8;
		m_argType[0] = argWord;
		strcpy(m_format, code->name);
		break;
	case ADDR:
		m_numArg = 1;
		FETCH_TO(op);
		m_arg[0] = op;
		FETCH_TO(op);
		m_arg[0] |= op << 8;
		m_argType[0] = argAddress;
		strcpy(m_format, code->name);
		break;
	case CALL:
		m_numArg = 1;
		FETCH_TO(op);
		m_arg[0] = op;
		FETCH_TO(op);
		m_arg[0] |= op << 8;
		m_argType[0] = argCall;
		strcpy(m_format, code->name);
		break;
	case RST:
		m_numArg = 1;
		m_arg[0] = op0 & 0x38;
		m_argType[0] = argRst;
		strcpy(m_format, code->name);
		break;
	case JR:
		m_numArg = 1;
		FETCH_TO(op);
		m_arg[0] = (pc + m_length + SIGN_EXTEND(op)) & 0xffff;
		m_argType[0] = argRelative;
		strcpy(m_format, code->name);
		break;
	}

	// Translate ix to iy if necessary.
	if (op0 == 0xfd)
	{
		for (int i = 0; m_format[i]; i++)
		{
			if (m_format[i] == 'i' && m_format[i + 1] == 'x')
			{
				m_format[i + 1] = 'y';
			}
		}
	}

	if (code->tstates > 256)
	{
		m_minT += code->tstates >> 8;
		m_maxT += code->tstates & 0xff;
	}
	else
	{
		m_minT += code->tstates;
		m_maxT = m_minT;
	}
}

void Zi80dis::Format(char *output)
{
	char arg[2][16];

	for (int i = 0; i < m_numArg; i++)
	{
		switch (m_argType[i])
		{
		case argWord:
		case argAddress:
		case argRelative:
		case argCall:
		case argRst:
			if (m_assemblerable)
			{
				sprintf(arg[i], "0%04xh", m_arg[i]);
			}
			else if (m_dollarhex)
			{
				sprintf(arg[i], "$%04x", m_arg[i]);
			}
			else
			{
				sprintf(arg[i], "%04xh", m_arg[i]);
			}
			break;

		case argByte:
		case argPort:
			if (m_assemblerable)
			{
				sprintf(arg[i], "0%02xh", m_arg[i]);
			}
			else if (m_dollarhex)
			{
				sprintf(arg[i], "$%02x", m_arg[i]);
			}
			else
			{
				sprintf(arg[i], "%02xh", m_arg[i]);
			}
			break;

		case argIndex:
			sprintf(arg[i], m_dollarhex ? "%c$%02x" : "%c%02xh",
				(m_arg[i] & 0x80) ? '-' : '+',
				(m_arg[i] & 0x80) ? (m_arg[i] & 255 ^ 255) + 1 : m_arg[i]);
			break;
		}
	}

	sprintf(output, m_format, arg[0], arg[1]);
}

extern "C" void zi_tstates(const unsigned char *inst,
	int *low, int *high, int *ocf, int *low8080, int *high8080)
{
	Zi80dis dis;

	dis.SetUndocumented(true);
	dis.Disassemble(inst, 0, true);

	if (low) *low = dis.m_minT;
	if (high) *high = dis.m_maxT;
	if (ocf) *ocf = dis.m_ocf;
	if (low8080) *low8080 = dis.m_min8080T;
	if (high8080) *high8080 = dis.m_max8080T;
}
