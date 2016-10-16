%{
// GWP - keep track of version via hand-maintained date stamp.
#define VERSION "19sep2013"

/*
 *  zmac -- macro cross-assembler for the Zilog Z80 microprocessor
 *
 *  Bruce Norskog	4/78
 *
 *  Last modification  1-18-87 by cdk
 *  This assembler is modeled after the Intel 8080 macro cross-assembler
 *  for the Intel 8080 by Ken Borgendale.  The major features are:
 *	1.  Full macro capabilities
 *	2.  Conditional assembly
 *	3.  A very flexible set of listing options and pseudo-ops
 *	4.  Symbol table output
 *	5.  Error report
 *	6.  Elimination of sequential searching
 *	7.  Commenting of source
 *	8.  Facilities for system definiton files
 *
 * Revision History:
 *
 * jrp	3-8-82	Converted to run on Vax, updated syntax to conform better
 *		to the Zilog standard.
 *
 * jrp	3-15-82	Added underscore as a character type in the lex table
 *		'numpart' (0x5F).
 *
 *		Changed maximum number of characters in a label to 15
 *		from 7. Note that 'putsymtab' uses this value inside
 *		of a quoted string, so we use 15.
 *
 * jrp	2-15-83	Fixed 'getlocal' to return better local labels. It used
 *		to crash after 6 invocations.
 *
 * jrp	6-7-83	Fixed bug in the ADD IX,... instruction.
 *
 * jrp	5-11-84	Added code to print unused labels out with the symbol table
 *		Also sped up the macro processor by using stdio.
 *
 * jrp 5-22-84	Added include files ala ormac
 *
 * jrp 8-27-84	Added PHASE/DEPHASE commands
 *
 * cdk 9-20-86	Converted to run on a Pyramid.  This meant changing yylval
 *		to be a %union, and then putting in the appropriate
 *		typecasts where ints are pointers are used interchangeably.
 *		The current version still probably won't run on machines where
 *		sizeof(int) != sizeof(char *).
 *		Also changed emit() to use varargs, and got rid of the
 *		old style = in front of yacc action code.
 *			-Colin Kelley  vu-vlsi!colin
 *
 * cdk 10-2-86	Added some more typecasts to keep lint a little happier.
 *		Removed several unused variables.  Changed most vars
 *		declared as char to int, since many of them were being
 *		compared with -1!  I still don't know what's going on with
 *		est[][] being malloc'd and free'd everywhere...it looks pretty
 *		fishy...
 *
 * cdk 1-18-87  Added MIO code to emulate 'mfile' using malloc()'d memory.
 *		This was needed to get the code to work when compiled under
 *		MSC 4.0 on a PC, and it's probably faster anyway.
 *
 * cdk 2-5-87	Added 'cmp' as a synonym for 'cp', 'jmp' as a synonym for
 *		'jp', and added tolerance of accumulator specification for arithmetic
 *		and logical instructions.  (For example, 'or a,12' is now accepted,
 *		same as 'or 12'.)
 *
 * gwp 12-29-08	Changes to allow compilation with modern C compiler and using bison
 *		as the .y to .c converter.  assert, tstate pseudo-ops.
 *		t(), tilo(), tihi() functions.  ==, <=, >=, !=, !, <, > operators.
 *		-c to turn cycle counts off in listing.  Usage, -h and version.
 *
 * gwp 9-26-10	Add ocf() and setocf to track and set op code fetch counts.
 *		Add sett as an alias for tstate
 *
 * gwp 12-30-11	Add undocumented instructions sl1, pfix, pfiy, in (c), out (c),0
 *		bit/set/res (ixy+d),reg and ld/inc/dec ixylh.
 *
 * gwp 2-8-12   Increase MAXIFS massively due to massive lowt macro
 *
 * gwp 2-11-12  Support 32 bit constants.  '%' alias for MOD.  Add defd, dword.
 *		lo(x) and hi(x) for easy low and high byte extraction.  Allow
 *		filenames longer than 15 characters.  All output to "zout" subdirectory
 *		of source file.
 *
 * gwp 2-15-12	Perform multiple passes while equates are changing.  Support
 *		.label for temporary label definitions and _label for file
 *		scoped labels.  Allow '.' within labels.  Assert listing bugfix.
 *
 * gwp 4-27-12	Implement $ prefixed hex constants and double-quoted strings.
 *
 * gwp 6-30-12	Minor changes to allow compilation with gcc.
 *
 * gwp 9-05-12	incbin added.
 *
 * gwp 11-24-12	Fix macro expansion bug when symbol larger than MAXSYMBOLSIZE
 *		due to file name prepending when symbol starts with '_'.
 *
 * gwp 12-04-12	Optional JR promotion and JP demotion errors.  Output a warning
 *		if no execute address given.  Output ".bds" file to enable easy
 *		simple source level debugging.
 *
 * gwp 4-14-13	Parens in expressions, else, .pseudo, full set of C operators
 *		with conventional precedence and various aliases and code
 *		changes to make source similar to zmac 1.3 on internet.
 *
 * gwp 5-5-13	.cmd,.cas,.lcas,.bin output.  dc (both MACRO-80 and EDAS types).
 *		lo, hi renamed to low, high and make unary operators.  Allow
 *		label::, placeholder public, extern declarations.  Bug fixes
 *		in defs, t, ocf, tihi, tilo in phase mode.  Initial support
 *		for -I include directories. 0x hex constants. --mras flag for
 *		limited MRAS compatibility (allows $ in labels, $? to start
 *		labels).
 *
 * gwp 4-6-13	--rel for .rel (Microsoft linker) output and extern, public,
 *		aseg, cseg, dseg in support (big emit + expression revamp).
 *		-I follows C preprocessor convention, output relative to
 *		current directory.  8080 mnemonics, .z80, .8080, -z, -8.
 *		Change .bin to .cim.  Warn on labels not in first column.
 *
 * gwp 8-11-13	Allow $ to start identifiers and do '$' dropping when macro
 *              parsed so we no longer need to drop '$' in identifiers. 
 *              Even $FCB allowed, with warning.  Add --zmac for backwards
 *		compatibility with zmac.  ` now used for joining in macros.
 *		Most reserved words can be used as labels or variables.
 *		Free-form title, name, comment, subttl parsing.  Allow #arg
 *		for macro arguments (in --mras mode).  Support <CR> delimited
 *		files.  Add .ams output.  Integrate documentation (--doc).
 */

#define MIO		/* use emulation routines from mio.c */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>

#ifdef WIN32
#include <windows.h>	// just for colouring the output
#endif

#include "zi80dis.h"

#ifdef vax11c
#define unlink(filename) delete(filename)
#endif

#ifdef MIO
FILE *mfopen();
#else
#define mfopen(filename,mode) fopen(filename,mode)
#define mfclose(filename,mode) fclose(filename,mode) 
#define mfputc(c,f) putc(c,f)
#define mfgetc(f) getc(f)
#define mfseek(f,loc,origin) fseek(f,loc,origin)
#define mfread(ptr,size,nitems,f) fread(ptr,size,nitems,f)
#define mfwrite(ptr,size,nitems,f) fread(ptr,size,nitems,f)
#endif /* MIO */

/*
 * DEBUG turns on pass reporting.
 * Macro debug and Token debug enables.
#define	DEBUG
#define	M_DEBUG	
#define	T_DEBUG
 */

#define ITEMTABLESIZE	10000
#define TEMPBUFSIZE	(1000+MAXSYMBOLSIZE)
#define LINEBUFFERSIZE	1000
#define EMITBUFFERSIZE	200
#define MAXSYMBOLSIZE	40
#define IFSTACKSIZE	20
// GWP - I use lots of if's with my lowt macro
#define MAXIFS		65536
#define TITLELEN	50
#define BINPERLINE	16
#define	PARMMAX		25
#define MAXEXP		25
#define SYMMAJIC	07203
#define	NEST_IN		32
#define MAXPASS		32
#define MAXINCPATH	32


void yyerror(char *err)
{}		/* we will do our own error printing */

struct	item	{
	char	*i_string;
	int	i_value;
	int	i_token;
	int	i_uses;
	int	i_scope;
	int	i_chain;
};

#define SCOPE_NONE	(0)
#define SCOPE_PROGRAM	(1)
#define SCOPE_DATA	(2)
#define SCOPE_PUBLIC	(4)
#define SCOPE_EXTERNAL	(8)
#define SCOPE_NORELOC	(16)

#define SCOPE_SEGMASK	(3)
#define SCOPE_SEG(s)	((s) & SCOPE_SEGMASK)

struct expr {
	int e_value;
	int e_scope;
	int e_token;
	struct item *e_item;
	struct expr *e_left;
	struct expr *e_right;
};

#define EXPR_SEG(e)	SCOPE_SEG(e->e_scope)

FILE	*fout,
	*fbuf,
	*fbds,
	*fcmd,
	*fcas,
	*flcas,
	*fcim,
	*fams,
	*frel,
	*fin[NEST_IN],
	*now_file ;

int	pass2;	/*set when pass one completed*/
int	outpass; 	// set when we decide to stop doing passes */
int	passfail;	// set when an error means passes should not continue
int	passretry;	// set when an inconsistency will require another pass
int	dollarsign ;	/* location counter */
int	olddollar ;	/* kept to put out binary */
int	oldothdollar;	// output address of next .cmd/.cas/.lcas block
int	emit_addr;	// were code and data are being placed in memory
int	tstates;	// cumulative T-states
int	ocf;		// cumulative op code fetches
int	llseq;		// local label sequence number
int	mras;		// MRAS semi-compatibility mode
int	zcompat;	// Original zmac compatibility mode
char	modstr[8];	// Replacement string for '?' in labels when MRAS compatible
int	relopt;		// Only output .rel files.
char	progname[8];	// Program name for .rel output
int	note_depend;	// Print names of files included
int	firstcol;
int	logcol;
int	coloncnt;

// Include file search path
char	*incpath[MAXINCPATH];
int	incpath_cnt;

/* program counter save for PHASE/DEPHASE */
int	phdollar, phbegin, phaseflag ;

char	*src_name[NEST_IN] ;
int	linein[NEST_IN] ;
int	now_in ;


// These first 5 errors are singled out in listerr1() for reasons I don't
// quite understand.
#define bflag	0	/* balance error */
#define eflag	1	/* expression error */
#define fflag	2	/* syntax error */
#define iflag	3	/* bad digits */
#define mflag	4	/* multiply defined */

#define pflag	5	/* phase error */
#define uflag	6	/* undeclared used */
#define vflag	7	/* value out of range */
#define oflag	8	/* phase/dephase error */
#define aflag	9	/* assert failed */
#define jflag	10	/* JP could be JR */
#define rflag	11	/* expression not relocatable */
#define gflag	12	/* incorrect register */
#define zflag	13	/* Z-80 instruction */

#define FIRSTWARN	warn_hex
#define	warn_hex	14
#define warn_notimpl	15

#define FLAGS	16	/* number of errors and warnings */

char	err[FLAGS];
int	keeperr[FLAGS];
char	errlet[FLAGS]="BEFIMPUVOAJRGZHN";
char	*errname[FLAGS]={
	"Balance",
	"Expression",
	"Syntax",
	"Digit",
	"Mult. def.",
	"Phase",
	"Undeclared",
	"Value",
	"Phase/Dephase",
	"Assertion failure",
	"Use JR",
	"Not relocatable",
	"Register usage",
	"Z-80 instruction in 8080 mode",
	"$hex constant interpreted as symbol",
	"Not implemented"
};


unsigned char inpbuf[LINEBUFFERSIZE];
unsigned char *inpptr;

char	linebuf[LINEBUFFERSIZE];
char	*lineptr;
char	*linemax = linebuf+LINEBUFFERSIZE;

char	outbin[BINPERLINE];
char	*outbinp = outbin;
char	*outbinm = outbin+BINPERLINE;

char	outoth[256];
int	outoth_cnt = 0;

char	emitbuf[EMITBUFFERSIZE];
char	*emitptr;

char	ifstack[IFSTACKSIZE];
char	*ifptr;
char	*ifstmax = ifstack+IFSTACKSIZE-1;


char	expif[MAXIFS];
char	*expifp;
char	*expifmax = expif+MAXIFS;

char	hexadec[] = "0123456789ABCDEF" ;


int	nitems;
int	linecnt;
int	nbytes;
int	invented;
int	npass;
int	njrpromo;


char	tempbuf[TEMPBUFSIZE];
char	*tempmax = tempbuf+TEMPBUFSIZE-1;

char	inmlex;
char	arg_flag;
int	parm_number;
int	exp_number;
char	symlong[] = "Symbol/number too long";
int	raw;

int	disp;
#define FLOC	PARMMAX
#define TEMPNUM	PARMMAX+1
union exprec {
	char *param;
	int value;
};
union exprec	*est;
union exprec	*est2;
union exprec	*expstack[MAXEXP];
int	expptr;

int	floc;
int	mfptr;
FILE	*mfile;

char	*writesyms;


char	*title;
char	titlespace[TITLELEN];
char	*timp;
char	*sourcef;
/* changed to cope with filenames longer than 14 chars -rjm 1998-12-15 */
char	src[1024];
char	bin[1024];
char	mtmp[1024];
char	listf[1024];
char	bds[1024];
char	oth[1024];

char	bopt = 1,
	copt = 1,	/* cycle counts in listings by default */
	edef = 1,
	eopt = 1,
	fdef = 0,
	fopt = 0,
	gdef = 1,
	gopt = 1,
	iopt = 0 ,	/* list include files */
	jopt = 0,
	JPopt = 0,
	lstoff = 0,
	lston = 0,	/* flag to force listing on */
	lopt = 0,
	mdef = 0,
	mopt = 0,
	nopt = 1 ,	/* line numbers on as default */
	oopt = 0,
	popt = 1,	/* form feed as default page eject */
	sopt = 0,	/* turn on symbol table listing */
	topt = 1,	/* terse, only error count to terminal */
	printer_output = 0, // GWP - printer style output
	z80,
	saveopt;

char default_jopt, default_JPopt, default_z80 = 1;

char	xeq_flag = 0;
int	xeq;

time_t	now;
int	line;
int	page = 1;

struct stab {
	char	t_name[MAXSYMBOLSIZE+1];
	int	t_value;
	int	t_token;
};

// GWP - support for cycle count tracking (and opens door for efficient .cmd, etc. output)

unsigned char memory[1 << 16];
char memflag[1 << 16];
enum {
	MEM_DATA = 1,
	MEM_INST = 2,
	MEM_T_SET = 4
};
int tstatesum[1 << 16];
int ocfsum[1 << 16];

// GWP - expression handling extensions for .rel output.
void advance_segment(int step);
void expr_reloc_check(struct expr *ex);
void expr_number_check(struct expr *ex);
void expr_scope_same(struct expr *ex1, struct expr *ex2);
void expr_word_check(struct expr *ex);
int is_number(struct expr *ex);
int is_external(struct expr *ex);
struct expr *expr_num(int value);
struct expr *expr_alloc(void);
struct expr *expr_op(struct expr *left, int token, struct expr *right, int value);
struct expr *expr_op_sc(struct expr *left, int token, struct expr *right, int value);
void expr_free(struct expr *ex);
int can_extend_link(struct expr *ex);
void extend_link(struct expr *ex);
void putrelop(int op);
#define RELOP_BYTE	(1)
#define RELOP_WORD	(2)
#define RELOP_HIGH	(3)
#define RELOP_LOW	(4)
#define RELOP_NOT	(5)
#define RELOP_NEG	(6)
#define RELOP_SUB	(7)
#define RELOP_ADD	(8)
#define RELOP_MUL	(9)
#define RELOP_DIV	(10)
#define RELOP_MOD	(11)
struct item *locate(char *name);
// Data descriptions for emit()
#define E_CODE		(0)
#define E_DATA		(1)
#define E_CODE8		(2)
#define E_CODE16	(3)
int segment;
#define SEG_ABS		(0)
#define SEG_CODE	(1)
#define SEG_DATA	(2)
int seg_pos[4]; // may eventually support SEG_COMMON
int seg_size[4];
int rel_main;
int segchange;

/*
 *  push back character
 */
int	peekc;
int	nextline_peek;

/* function prototypes */
void error(char *as);
void usage(char *msg, char *param);
void help();
void erreport();
void errorprt(int errnum);
void mlex(char *look);
void popsi();
void suffix(char *str, char *suff);
char *basename(char *filename);
char *getsuffix(char *str);
void outpath(char *out, char *src, char *suff);
void casname(char *out, char *src);
void putm(int c);
void insymtab(char *name);
void outsymtab(char *name);
void putsymtab();
void clear();
void setmem(int addr, int value, int type);
void setvars();
void flushbin();
void flushoth();
void lineout();
void puthex(int byte, FILE *buf);
void putcas(int byte);
void putrelbits(int count, int bits);
void putrel(int byte);
void putrelname(char *str);
void putrelextaddr(int extaddr);
void putrelcmd(int cmd);
void putrelsegref(int scope, int addr);
void flushrel(void);
void lsterr1();
void lsterr2(int lst);
void copyname(char *st1, char *st2);
void next_source(char *sp);
void incbin(char *filename);
void dc(int count, int value);
char *getmraslocal();

#define RELCMD_PUBLIC	(0)
#define RELCMD_COMMON	(1)
#define RELCMD_PROGNAME	(2)
#define RELCMD_LIBLOOK	(3)
#define RELCMD_EXTLINK	(4)
#define RELCMD_COMSIZE	(5)
#define RELCMD_EXTCHAIN	(6)
#define RELCMD_PUBVALUE	(7)
#define RELCMD_EXTMINUS	(8)
#define RELCMD_EXTPLUS	(9)
#define RELCMD_DATASIZE	(10)
#define RELCMD_SETLOC	(11)
#define RELCMD_CODESIZE	(13)
#define RELCMD_ENDMOD	(14)
#define RELCMD_ENDPROG	(15)

/*
 *  add a character to the output line buffer
 */
int addtoline(int ac)
{
	/* check for EOF from stdio */
	if (ac == -1)
		ac = 0 ;
	if (inmlex)
		return(ac);
	if (lineptr >= linemax)
		error("line buffer overflow");
	*lineptr++ = ac;
	return(ac);
}

/*
 *  put values in buffer for outputing
 */

void emit(int bytes, int desc, struct expr *data, ...)
{
	int type, i, args, dsize;
	va_list ap;

	if (relopt && segchange) {
		segchange = 0;
		putrelcmd(RELCMD_SETLOC);
		putrelsegref(segment, seg_pos[segment]);
	}

	// External references only supported in .rel output.
	if (!relopt && data && (data->e_scope & SCOPE_EXTERNAL))
		err[uflag]++;

	va_start(ap, data);

	type = desc == E_DATA ? MEM_DATA : MEM_INST;

	// Check emit is not adding instructions to the buffer.
	if (desc != E_DATA && emitptr != emitbuf)
		fprintf(stderr, "internal inconsistency in t-state counting\n");

	dsize = 0;
	args = bytes;
	if (desc == E_DATA) {
		args = 0;
		dsize = bytes;
	}
	else if (desc == E_CODE16)
		dsize = 2;
	else if (desc == E_CODE8)
		dsize = 1;

	for (i = 0; i < args; i++)
	{
		if (emitptr >= &emitbuf[EMITBUFFERSIZE])
			error("emit buffer overflow");
		else {
			int addr = (emit_addr + (emitptr - emitbuf)) & 0xffff;
			*emitptr = va_arg(ap, int);
			if (segment == SEG_CODE) 
				setmem(addr, *emitptr, type);
			putrel(*emitptr);
			emitptr++;
		}
	}

	va_end(ap);

	for (i = 0; i < dsize; i++) {
		int addr = (emit_addr + (emitptr - emitbuf)) & 0xffff;
		*emitptr = data->e_value >> (i * 8);
		if (segment == SEG_CODE)
			setmem(addr, *emitptr, type);
		emitptr++;
	}

	advance_segment(args + dsize);

	if (desc != E_DATA)
	{
		int eaddr = emit_addr, low, fetch, low8080, addr_after;

		// emitbuf is OK since this only happens with single emits

		if (!z80) {
			// Check for invalid 8080 instructions.
			int op = emitbuf[0] & 0xff;
			if (op == 0x08 || op == 0x10 || op == 0x18 ||
			    op == 0x20 || op == 0x28 || op == 0x30 ||
			    op == 0x38 || op == 0xCB || op == 0xD9 ||
			    op == 0xDD || op == 0xED || op == 0xFD)
			{
				err[zflag]++;
			}
		}

		zi_tstates(emitbuf, &low, 0, &fetch, &low8080, 0);
		if (!z80)
			low = low8080;

		// Sanity check
		if (low <= 0)
		{
			fprintf(stderr, "undefined instruction on %02x %02x (assembler or diassembler broken)\n",
				emitbuf[0], emitbuf[1]);
		}

		// Special case to catch promotion of djnz to DEC B JP NZ
		// Even update the tstatesum[] counter though that seems to
		// me to be above and beyond.
		if (emitbuf[0] == 5 && args == 2) {
			tstatesum[eaddr] = tstates;
			ocfsum[eaddr] = ocf;
			memflag[eaddr] |= MEM_T_SET;
			eaddr++;
			tstates += low;
			ocf += fetch;
			low = 10;
			// still 1 fetch
		}

		// Double setting of both sides of tstatesum[] seems like too
		// much, but must be done in the isolated instruction case:
		// org x ; inc a ; org y

		tstatesum[eaddr] = tstates;
		ocfsum[eaddr] = ocf;
		memflag[eaddr] |= MEM_T_SET;

		// Well, OK, should we default to high or low???
		// Guess it should be whatever makes sense for you
		// to get here which, generally, is the low.

		// low it is.

		tstates += low;
		ocf += fetch;

		addr_after = (emit_addr + (emitptr - emitbuf)) & 0xffff;

		tstatesum[addr_after] = tstates;
		ocfsum[addr_after] = ocf;
		memflag[addr_after] |= MEM_T_SET;
	}

	if (relopt && outpass && dsize > 0) {
		if (dsize == 1) {
			if (is_number(data))
				putrel(data->e_value);
			else if (can_extend_link(data)) {
				extend_link(data);
				putrelop(RELOP_BYTE);
				putrel(0);
			}
			else {
				err[rflag]++;

				putrel(0);
			}
		}
		else if (dsize == 2) {
			int handled = 0;
			if (data->e_scope & SCOPE_EXTERNAL) {
				struct item *var = 0;
				int offset = 0;
				// Simple external reference.
				if (is_external(data))
					var = data->e_item;
				else if (is_external(data->e_left) &&
					data->e_token == '+' &&
					is_number(data->e_right))
				{
					var = data->e_left->e_item;
					offset = data->e_right->e_value;
				}
				else if (is_number(data->e_left) &&
					data->e_token == '+' &&
					is_external(data->e_right))
				{
					offset = data->e_left->e_value;
					var = data->e_right->e_item;
				}
				else if (is_external(data->e_left) &&
					data->e_token == '-' &&
					is_number(data->e_right))
				{
					var = data->e_left->e_item;
					offset = data->e_right->e_value;
				}

				if (var && offset) {
					putrelcmd(data->e_token == '-' ?
						RELCMD_EXTMINUS : RELCMD_EXTPLUS);
					// Theoretically we could put a
					// program or data relative value here...
					putrelsegref(SEG_ABS, offset);
				}

				if (var) {
					if (var->i_chain == 0) {
						putrel(0);
						putrel(0);
					}
					else {
						putrelbits(1, 1);
						putrelextaddr(var->i_chain);
					}
					var->i_chain = (segment << 16) |
						((dollarsign + args) & 0xffff);
					handled = 1;
				}
			}
			else if ((data->e_scope & ~SCOPE_PUBLIC) == 0) {
				// nice constant value
				putrel(data->e_value);
				putrel(data->e_value >> 8);
				handled = 1;
			}
			else if (!(data->e_scope & SCOPE_NORELOC)) {
				// relocatable value
				putrelbits(1, 1);
				putrelbits(2, data->e_scope);
				putrelbits(8, data->e_value);
				putrelbits(8, data->e_value >> 8);
				handled = 1;
			}

			if (!handled) {
				if (can_extend_link(data)) {
					extend_link(data);
					putrelop(RELOP_WORD);
					putrel(0);
					putrel(0);
				}
				else {
					err[rflag]++;
					putrel(data->e_value);
					putrel(data->e_value >> 8);
				}
			}
		}
		else if (dsize == 4) {
			// Only numbers are allowed.
			if (data->e_scope != 0) {
				err[vflag]++;
				if (data->e_scope & SCOPE_NORELOC)
					err[rflag]++;
			}
			for (i = 0; i < dsize; i++)
				putrel(data->e_value >> (i * 8));
		}
		else
			error("internal dsize error");
	}
}

#define ET_NOARG_DISP	(0)
#define ET_NOARG	(1)
#define ET_BYTE		(2)
#define ET_WORD		(5)

void emit1(int opcode, int regvalh, struct expr *data, int type)
{
	if (type == ET_BYTE && (data->e_value < -128 || data->e_value > 255))
		err[vflag]++;

	if (regvalh & 0x10000) {
		switch (type) {
		case ET_NOARG_DISP:
			emit(2, E_CODE, 0, regvalh >> 8, opcode);
			break;
		case ET_BYTE:
			emit(2, E_CODE8, data, regvalh >> 8, opcode);
			break;
		}
	}
	else if (regvalh & 0x8000) {
		switch (type) {
		case ET_NOARG_DISP:
			if (opcode & 0x8000)
				emit(4, E_CODE, 0, regvalh >> 8, opcode >> 8, disp, opcode);
			else
				emit(3, E_CODE, 0, regvalh >> 8, opcode, disp);
			break;
		case ET_NOARG:
			emit(2, E_CODE, 0, regvalh >> 8, opcode);
			break;
		case ET_BYTE:
			emit(3, E_CODE8, data, regvalh >> 8, opcode, disp);
			break;
		case ET_WORD:
			emit(2, E_CODE16, data, regvalh >> 8, opcode);
		}
	} else
		switch(type) {
		case ET_NOARG_DISP:
		case ET_NOARG:
			if (opcode & 0100000)
				emit(2, E_CODE, 0, opcode >> 8, opcode);
			else
				emit(1, E_CODE, 0, opcode);
			break;
		case ET_BYTE:
			emit(1, E_CODE8, data, opcode);
			break;
		case ET_WORD:
			if (opcode & 0100000)
				emit(2, E_CODE16, data, opcode >> 8, opcode);
			else
				emit(1, E_CODE16, data, opcode);
		}
}




void emitdad(int rp1,int rp2)
{
	if (rp1 & 0x8000)
		emit(2, E_CODE, 0, rp1 >> 8, rp2 + 9);
	else
		emit(1, E_CODE, 0, rp2 + 9);
}


void emitjr(int opcode, struct expr *dest)
{
	int disp = dest->e_value - dollarsign - 2;

	if (dest->e_scope & SCOPE_NORELOC)
		err[rflag]++;

	// Can't relative jump to other segments or an external
	if (((dest->e_scope & (SCOPE_SEGMASK | SCOPE_EXTERNAL)) != segment ||
		disp > 127 || disp < -128) && z80)
	{
		if (jopt) {
			njrpromo++;
			switch (opcode) {
			case 0x10: // DJNZ
				emit(2, E_CODE16, dest, 0x05, 0xC2); // DEC B, JP NZ
				break;
			case 0x18: // JR
				emit(1, E_CODE16, dest, 0xC3); // JP
				break;
			case 0x20: // JR NZ
				emit(1, E_CODE16, dest, 0xC2); // JP NZ
				break;
			case 0x28: // JR Z
				emit(1, E_CODE16, dest, 0xCA); // JP Z
				break;
			case 0x30: // JR NC
				emit(1, E_CODE16, dest, 0xD2); // JP NC
				break;
			case 0x38: // JR C
				emit(1, E_CODE16, dest, 0xDA); // JP C
				break;
			default:
				err[vflag]++;	// shouldn't happen!
				expr_free(dest);
				break;
			}
		}
		else {
			err[vflag]++;
			expr_free(dest);
		}
	}
	else {
		emit(2, E_CODE, 0, opcode, disp);
		expr_free(dest);
	}
}

void checkjp(int op, struct expr *dest)
{
	op &= 0x030;
	// Only applies to Z-80 output and if JP optimization checking is on.
	// JR only has z, nz, nc, c
	// A jump to the current segment might have been optimizable
	if (z80 && JPopt && (op == 0 || op == 010 || op == 020 || op == 030) &&
		(dest->e_scope & (SCOPE_SEGMASK | SCOPE_EXTERNAL)) == segment)
	{
		int disp = dest->e_value - dollarsign - 2;
		if (disp >= -128 && disp <= 127)
			err[jflag]++;
	}
}

/*
 *  put out a byte of binary 
 */
void putbin(int v)
{
	if(!outpass || !bopt) return;
	*outbinp++ = v;
	if (outbinp >= outbinm) flushbin();

	outoth[outoth_cnt++] = v;
	if (outoth_cnt == 256) flushoth();
}



/*
 *  output one line of binary in INTEL standard form
 */
void flushbin()
{
	char *p;
	int check=outbinp-outbin;

	if (!outpass || !bopt)
		return;
	nbytes += check;
	if (check) {
		putc(':', fbuf);
		puthex(check, fbuf);
		puthex(olddollar>>8, fbuf);
		puthex(olddollar, fbuf);
		puthex(0, fbuf);
		check += (olddollar >> 8) + olddollar;
		olddollar += (outbinp-outbin);
		for (p=outbin; p<outbinp; p++) {
			puthex(*p, fbuf);
			check += *p;
		}
		puthex(256-check, fbuf);
		putc('\n', fbuf);
		outbinp = outbin;
	}
}



/*
 *  put out one byte of hex
 */
void puthex(int byte, FILE *buf)
{
	putc(hexadec[(byte >> 4) & 017], buf);
	putc(hexadec[byte & 017], buf);
}

void flushoth()
{
	int i, checksum;

	if (!outpass || !bopt || outoth_cnt == 0)
		return;

	fprintf(fcmd, "%c%c%c%c", 1, outoth_cnt + 2, oldothdollar, oldothdollar >> 8);
	fwrite(outoth, outoth_cnt, 1, fcmd);

	putcas(0x3c);
	putcas(outoth_cnt);
	putcas(oldothdollar);
	putcas(oldothdollar >> 8);
	checksum = oldothdollar + (oldothdollar >> 8);
	for (i = 0; i < outoth_cnt; i++) {
		putcas(outoth[i]);
		checksum += outoth[i];
	}
	putcas(checksum);

	oldothdollar += outoth_cnt;
	outoth_cnt = 0;
}

int casbit, casbitcnt = 0;

void putcas(int byte)
{
	fputc(byte, flcas);

	// Buffer 0 stop bit and the 8 data bits.
	casbit = (casbit << 9) | (byte & 0xff);
	casbitcnt += 9;
	while (casbitcnt >= 8) {
		casbitcnt -= 8;
		fputc(casbit >> casbitcnt, fcas);
	}
}

void casname(char *out, char *src)
{
	char *base = basename(src);
	int i;

	out[0] = 'N';
	for (i = 1; i < 6; i++)
		out[i] = ' ';

	for (i = 0; *base && i < 6; base++) {
		if (strcmp(base, ".z") == 0 || strcmp(base, ".Z") == 0)
			break;

		if (*base >= 'a' && *base <= 'z') {
			*out++ = *base - ('a' - 'A');
			i++;
		}
		else if (*base >= 'A' && *base <= 'Z') {
			*out++ = *base;
			i++;
		}
	}
}

int relbit, relbitcnt = 0;

void putrelbits(int count, int bits)
{
	if (!outpass || !relopt)
		return;

	relbit <<= count;
	relbit |= bits & ((1 << count) - 1);
	relbitcnt += count;

	while (relbitcnt >= 8) {
		relbitcnt -= 8;
		fputc(relbit >> relbitcnt, frel);
	}
}

void putrel(int byte)
{
	// Add 0 bit indicating byte to load at next counter
	putrelbits(1, 0);
	// Add byte to load
	putrelbits(8, byte);
}

void putrelname(char *str)
{
	int len = strlen(str);

	// .rel file format can do strings 7 long but for compatibility
	// we restrict them to 6.  I believe this is important because
	// extended link functions require a character when they wish to
	// operate on an external symbol.
	if (len > 6)
		len = 6;
	putrelbits(3, len);
	while (len-- > 0) {
		int ch = *str++;
		if (ch >= 'a' && ch <= 'z')
			ch -= 'a' - 'A';
		putrelbits(8, ch);
	}
}

void putrelsegref(int scope, int addr)
{
	putrelbits(2, scope);
	putrelbits(8, addr);
	putrelbits(8, addr >> 8);
}

void putrelextaddr(int extaddr)
{
	putrelsegref(extaddr >> 16, extaddr);
}


void putrelcmd(int relcmd)
{
	putrelbits(1, 1);
	putrelbits(2, 0);
	putrelbits(4, relcmd);
}

void flushrel(void)
{
	if (relbitcnt > 0)
		putrelbits(8 - relbitcnt, 0);

	if (relopt)
		fflush(frel);
}

/*
 *  put out a line of output -- also put out binary
 */
void list(int optarg)
{
	char *	p;
	int	i;
	int  lst;
	char seg = ' ';

	if (!expptr)
		linecnt++;
	addtoline('\0');
	if (outpass) {
		lst = iflist();
		if (lst) {
			lineout();
			if (nopt)
				fprintf(fout, "%4d:", linein[now_in]);

			if (copt)
			{
			    if (emitptr > emitbuf && (memflag[emit_addr] & MEM_INST))
			    {
			        int low, high, fetch, low8080, high8080;
			        zi_tstates(memory + emit_addr, &low, &high, &fetch, &low8080, &high8080);
				if (!z80) {
					low = low8080;
					high = high8080;
				}

				// Special case to catch promotion of djnz to DEC B JP NZ
				if (memory[emit_addr] == 5 && emitptr - emitbuf == 4) {
					low += 10;
					high += 10;
				}

			    	fprintf(fout, nopt ? "%5d" : "%4d", tstatesum[emit_addr]);

				fprintf(fout, "+%d", low);
				if (low != high)
				    fprintf(fout, "+%d", high - low);
			    }
			    else
			    {
			        fprintf(fout, nopt ? "%5s-" : "%4s-", "");
			    }
			}

			if (nopt || copt)
				fprintf(fout, "\t");

			puthex(optarg >> 8, fout);
			puthex(optarg, fout);
			if (relopt)
				seg = " '\"!"[segment];
			fputc(seg, fout);
			fputc(' ', fout);
			for (p = emitbuf; (p < emitptr) && (p - emitbuf < 4); p++) {
				puthex(*p, fout);
			}
			for (i = 4 - (p-emitbuf); i > 0; i--)
				fputs("  ", fout);

			putc('\t', fout);
			fputs(linebuf, fout);
		}

		if (bopt) {
			fprintf(fbds, "%04x %04x d ", dollarsign, emit_addr);
			for (p = emitbuf; p < emitptr; p++)
				fprintf(fbds, "%02x", *p & 0xff);
			fprintf(fbds, "\n");
			fprintf(fbds, "%04x %04x s %s", dollarsign, emit_addr, linebuf);

			for (p = emitbuf; p < emitptr; p++)
				putbin(*p);
		}


		p = emitbuf+4;
		while (lst && gopt && p < emitptr) {
			lineout();
			if (nopt) putc('\t', fout);
			fputs("      ", fout);
			for (i = 0; (i < 4) && (p < emitptr);i++) {
				puthex(*p, fout);
				p++;
			}
			putc('\n', fout);
		}

		lsterr2(lst);
	} else
		lsterr1();
	dollarsign += emitptr - emitbuf;
	emit_addr += emitptr - emitbuf;
	emitptr = emitbuf;
	lineptr = linebuf;
	advance_segment(emitptr - emitbuf);
}



/*
 *  keep track of line numbers and put out headers as necessary
 */
void lineout()
{
	if (!printer_output)
		return;

	if (line == 60) {
		if (popt)
			putc('\014', fout);	/* send the form feed */
		else
			fputs("\n\n\n\n\n", fout);
		line = 0;
	}
	if (line == 0) {
		fprintf(fout, "\n\n%s %s\t%s\t Page %d\n\n\n",
			&timp[4], &timp[20], title, page++);
		line = 4;
	}
	line++;
}


/*
 *  cause a page eject
 */
void eject()
{
	if (printer_output)
		return;

	if (outpass && iflist()) {
		if (popt) {
			putc('\014', fout);	/* send the form feed */
		} else {
			while (line < 65) {
				line++;
				putc('\n', fout);
			}
		}
	}
	line = 0;
}


/*
 *  space n lines on the list file
 */
void space(int n)
{
	int	i ;
	if (outpass && iflist())
		for (i = 0; i<n; i++) {
			lineout();
			putc('\n', fout);
		}
}

/*
 *  Error handling - pass 1
 */
void lsterr1()
{
	int i;
	for (i = 0; i <= mflag; i++)
		if (err[i]) {
			if (topt)
				errorprt(i);
			passfail = 1;
			err[i] = 0;
		}
}


/*
 *  Error handling - pass 2.
 */
void lsterr2(int lst)
{
	int i;
	for (i=0; i<FLAGS; i++)
		if (err[i]) {
			if (i < FIRSTWARN)
				passfail = 1;
			if (lst) {
				lineout();
				fprintf(fout, "%c %s %s\n",
					errlet[i], errname[i],
					i < FIRSTWARN ? "error" : "warning");
			}
			err[i] = 0;
			keeperr[i]++;
			if (i > mflag && topt)
				errorprt(i);
		}

	fflush(fout);	/*to avoid putc(har) mix bug*/
}

/*
 *  print diagnostic to error terminal
 */
void errorprt(int errnum)
{
	fprintf(stderr,"%s(%d) : %s %s\n%s\n",
		src_name[now_in], linein[now_in], errname[errnum],
			errnum < FIRSTWARN ? "error" : "warning", linebuf) ;
	fflush(stderr) ;
	return ;
}


/*
 *  list without address -- for comments and if skipped lines
 */
void list1()
{
	int lst;

	addtoline('\0');
	lineptr = linebuf;
	if (!expptr) linecnt++;
	if (outpass) {
		if (lst = iflist()) {
			lineout();
			if (nopt)
				fprintf(fout, "%4d:\t", linein[now_in]);
			if (copt)
				fprintf(fout, "\t");
			fprintf(fout, "\t\t%s", linebuf);
			lsterr2(lst);
		}
		if (bopt)
			fprintf(fbds, "%04x %04x s %s", dollarsign, emit_addr, linebuf);
	}
	else
		lsterr1();
}


/*
 *  see if listing is desired
 */
int iflist()
{
	int i, j;

	if (lston)
		return(1) ;
	if (lopt)
		return(0);
	if (*ifptr && !fopt)
		return(0);
	if (!lstoff && !expptr)
		return(1);
	j = 0;
	for (i=0; i<FLAGS; i++)
		if (err[i])
			j++;
	if (expptr)
		return(mopt || j);
	if (eopt && j)
		return(1);
	return(0);
}

// GWP - This avoids an apparent bug in bison as it tries to start out the
// Not needed under gcc which defines __STDC__ so I guard it to prevent
// warnings.
// yyparse() function with yyparse() ; { }.
#ifndef __STDC__
#define __STDC__
#endif

#define PSTITL	(0)	/* title */
#define PSRSYM	(1)	/* rsym */
#define PSWSYM	(2)	/* wsym */
#define PSINC	(3)	/* include file */

#define SPTITL	(0)	/* title */
#define SPSBTL	(1)	/* sub title */
#define SPNAME	(2)	/* name */
#define SPCOM	(3)	/* comment */

%}

%union	{
	struct expr *exprptr;
	struct item *itemptr;
	int ival;
	char *cval;
	}

%token <cval> STRING
%token <itemptr> NOOPERAND
%token <itemptr> ARITHC
%token ADD
%token <itemptr> LOGICAL
%token <itemptr> AND
%token <itemptr> OR
%token <itemptr> XOR
%token <ival> ANDAND
%token <ival> OROR
%token <itemptr> BIT
%token CALL
%token <itemptr> INCDEC
%token <itemptr> DJNZ
%token EX
%token <itemptr> IM
%token PHASE
%token DEPHASE
%token <itemptr> TK_IN
%token <itemptr> JR
%token LD
%token <itemptr> TK_OUT
%token <itemptr> PUSHPOP
%token <itemptr> RET
%token <itemptr> SHIFT
%token <itemptr> RST
%token <itemptr> REGNAME
%token <itemptr> IXYLH
%token <itemptr> ACC
%token <itemptr> C
%token <itemptr> RP
%token <itemptr> HL
%token <itemptr> INDEX
%token <itemptr> AF
%token AFp
%token <itemptr> SP
%token <itemptr> MISCREG
%token <itemptr> COND
%token <itemptr> SPCOND
%token <ival> NUMBER
%token <itemptr> UNDECLARED
%token END
%token ORG
%token ASSERT
%token TSTATE
%token <ival> T
%token <ival> TILO
%token <ival> TIHI
%token SETOCF
%token <ival> OCF
%token <ival> LOW
%token <ival> HIGH
%token DC
%token DEFB
%token DEFD
%token DEFS
%token DEFW
%token EQU
%token DEFL
%token <itemptr> LABEL
%token <itemptr> EQUATED
%token <itemptr> WASEQUATED
%token <itemptr> DEFLED
%token <itemptr> MULTDEF
%token <ival> MOD
%token <ival> SHL
%token <ival> SHR
%token <ival> NOT
%token <ival> LT
%token <ival> GT
%token <ival> LE
%token <ival> GE
%token <ival> NE
%token IF_TK
%token ELSE_TK
%token ENDIF_TK
%token <itemptr> ARGPSEUDO
%token <itemptr> INCBIN
%token <itemptr> LIST
%token <itemptr> MINMAX
%token MACRO
%token <itemptr> MNAME
%token <itemptr> OLDMNAME
%token ARG
%token ENDM
%token MPARM
%token <ival> ONECHAR
%token <ival> TWOCHAR
%token JRPROMOTE
%token JPERROR
%token PUBLIC
%token EXTRN
%token MRAS_MOD
%token <itemptr> SETSEG INSTSET
%token LXI DAD STAX STA SHLD LDAX LHLD LDA MVI MOV
%token <itemptr> INXDCX INRDCR PSW JUMP8 JP CALL8 ALUI8
%token <itemptr> SPECIAL
%token RAWTOKEN LOCAL

%type <itemptr> label.part symbol
%type <ival> allreg reg evenreg ixylhreg realreg mem memxy pushable bcdesp bcdehlsp mar condition
%type <ival> spcondition
%type <exprptr> noparenexpr parenexpr expression lxexpression
%type <ival> maybecolon maybeocto
%type <ival> evenreg8 reg8 m pushable8

%right '?' ':'
%left OROR
%left ANDAND
%left '|' OR
%left '^' XOR
%left '&' AND
%left '=' NE
%left '<' '>' LT GT LE GE
%left SHL SHR
%left '+' '-'
%left '*' '/' '%' MOD
%right '!' '~' NOT UNARY

%{
char  *cp;
int  i;
%}

%%

statements:
	/* Empty file! */
|
	statements statement
;


statement:
	label.part '\n'	{ 
		// An identfier without a colon all by itself on a line
		// will be interpreted as a label.  But there's a very
		// good chance it is a misspelling of an instruction or
		// pseudo-op name creating silent errors.  Since the condition
		// is unusual we print a warning.  Unless it is followed by
		// a colon in which case there's no ambiguity.
		if ($1 && !firstcol && coloncnt == 0 && outpass) {
			fprintf(stderr, "%s(%d): warning: '%s' treated as label (instruction typo?)\n",
				src_name[now_in], linein[now_in], $1->i_string);
			fprintf(stderr, "\tAdd a colon or move to first column to stop this warning.\n");
		}

		if ($1) list(dollarsign);
		else  list1();
	}
|
	label.part operation '\n' {
		list(dollarsign);
	}
|
	symbol EQU expression '\n' {
		expr_reloc_check($3);
		switch($1->i_token) {
		case UNDECLARED: case WASEQUATED:
			if ($1->i_token == WASEQUATED &&
				($1->i_value != $3->e_value ||
				 (($1->i_scope ^ $3->e_scope) & SCOPE_SEGMASK)))
			{
				if (outpass)
					err[pflag]++;
				else
					passretry = 1;
			}

			$1->i_token = EQUATED;
			$1->i_value = $3->e_value;
			$1->i_scope |= $3->e_scope;
			break;
		default:
			// m80 allows multiple equates as long as the value
			// does not change.  So does newer zmac.
			if ($1->i_value != $3->e_value ||
				(($1->i_scope ^ $3->e_scope) & SCOPE_SEGMASK))
			{
				err[mflag]++;
				$1->i_token = MULTDEF;
			}
		}

		list($3->e_value);
		expr_free($3);
	}
|
	symbol DEFL expression '\n' {
		expr_reloc_check($3);
		switch($1->i_token) {
		case UNDECLARED: case DEFLED:
			$1->i_token = DEFLED;
			$1->i_value = $3->e_value;
			$1->i_scope = ($1->i_scope & SCOPE_SEGMASK) | $3->e_scope;
			break;
		default:
			err[mflag]++;
			$1->i_token = MULTDEF;
		}
		list($3->e_value);
		expr_free($3);
	}
|
	symbol MINMAX expression ',' expression '\n' {
		int val3 = $3->e_value;
		int val5 = $5->e_value;
		expr_reloc_check($3);
		expr_reloc_check($5);
		expr_scope_same($3, $5);
		switch ($1->i_token) {
		case UNDECLARED: case DEFLED:
			$1->i_token = DEFLED;
			$1->i_scope |= $3->e_scope;
			if ($2->i_value)	/* max */
				list($1->i_value = (val3 > val5? val3:val5));
			else list($1->i_value = (val3 < val5? val3:val5));
			break;
		default:
			err[mflag]++;
			$1->i_token = MULTDEF;
			list($1->i_value);
		}
		expr_free($3);
		expr_free($5);
	}
|
	IF_TK expression '\n' {
		expr_number_check($2);
		if (ifptr >= ifstmax)
			error("Too many ifs");
		else {
			if (pass2) {
				*++ifptr = *expifp++;
				if (*ifptr != !($2->e_value)) err[pflag]++;
			} else {
				if (expifp >= expifmax)
					error("Too many ifs!");
				*expifp++ = !($2->e_value);
				*++ifptr = !($2->e_value);
			}
		}
		saveopt = fopt;
		fopt = 1;
		list($2->e_value);
		fopt = saveopt;
		expr_free($2);
	}
|
	ELSE_TK '\n' {
		/* FIXME: it would be nice to spot repeated ELSEs, but how? */
		*ifptr = !*ifptr;
		saveopt = fopt;
		fopt = 1;
		list1();
		fopt = saveopt;
	}
|
	ENDIF_TK '\n' {
		if (ifptr == ifstack) err[bflag]++;
		else --ifptr;
		list1();
	}
|
	label.part END '\n' {
		list(dollarsign);
		peekc = 0;
	}
|
	label.part END expression '\n' {
		expr_reloc_check($3);
		xeq_flag++;
		xeq = $3->e_value & 0xffff;
		list($3->e_value);
		peekc = 0;
		rel_main = (($3->e_scope & SCOPE_SEGMASK) << 16) | xeq;
		expr_free($3);
	}
|
	label.part DEFS expression '\n' {
		expr_number_check($3);
		if ($3->e_value < 0) err[vflag]++;
		if ($3->e_value > 0) {
			if (!phaseflag) {
				list(dollarsign);
				flushbin();
				flushoth();
				dollarsign += $3->e_value;
				olddollar += $3->e_value;
				oldothdollar += $3->e_value;
				emit_addr += $3->e_value;
				advance_segment($3->e_value);
				putrelcmd(RELCMD_SETLOC);
				putrelsegref(segment, seg_pos[segment]);
			}
			else
				dc($3->e_value, 0);
		}
		else
			list1();

		expr_free($3);
	}
|
	label.part DEFS expression ',' expression '\n' {
		expr_number_check($3);
		expr_number_check($5);
		if ($3->e_value < 0) err[vflag]++;
		if ($5->e_value < -128 || $5->e_value > 127) err[vflag]++;
		if ($3->e_value > 0) {
			dc($3->e_value, $5->e_value);
		}
		else
			list1();

		expr_free($3);
		expr_free($5);
	}
|
	label.part DC ONECHAR '\n' { emit(1, E_DATA, expr_num($3 | 0x80)); list(dollarsign); }
|
	label.part DC TWOCHAR '\n' { emit(1, E_DATA, expr_num($3)); emit(1, E_DATA, expr_num(($3 >> 8) | 0x80)); list(dollarsign); }
|
	label.part DC STRING '\n'
		{
			for (cp = $3; *cp != '\0'; cp++)
				if (!cp[1])
					emit(1, E_DATA, expr_num(*cp | 0x80));
				else
					emit(1, E_DATA, expr_num(*cp));

			list(dollarsign);
		}
|
	label.part DC expression ',' expression '\n'
		{
			expr_number_check($3);
			expr_number_check($5);
			dc($3->e_value, $5->e_value);
			expr_free($3);
			expr_free($5);
		}
|
	ARGPSEUDO arg_on ARG arg_off '\n' {
		list1();
		switch ($1->i_value) {

		case PSTITL:	/* title */
			lineptr = linebuf;
			cp = tempbuf;
			title = titlespace;
			while ((*title++ = *cp++) && (title < &titlespace[TITLELEN]));
			*title = 0;
			title = titlespace;
			break;

		case PSRSYM:	/* rsym */
			if (pass2) break;
			insymtab(tempbuf);
			break;

		case PSWSYM:	/* wsym */
			writesyms = malloc(strlen(tempbuf)+1);
			strcpy(writesyms, tempbuf);
			break;
		case PSINC:	/* include file */
			next_source(tempbuf) ;
			break ;
		}
	}
|
	ARGPSEUDO arg_on arg_off '\n' {
		fprintf(stderr, "Missing argument of '%s'\n", $1->i_string);
		err[fflag]++;
		list(dollarsign);
	}
|
	label.part INCBIN arg_on ARG arg_off '\n' {
		incbin(tempbuf);
	}
|
	SPECIAL raw_on RAWTOKEN {
		int quote = 0;
		char *p, *q;
		switch ($1->i_value) {
		case SPTITL:
			cp = tempbuf;
			title = titlespace;
			if (*cp == '\'' || *cp == '"')
				quote = *cp++;
			while ((*title++ = *cp++) && (title < &titlespace[TITLELEN]));
			if (quote && title > titlespace + 1 && title[-2] == quote)
				title[-2] = '\0';
			title = titlespace;
			list1();
			break;
		case SPSBTL:
			err[warn_notimpl]++;
			list1();
			break;
		case SPNAME:
			// Drop surrounding ('') if present
			p = tempbuf;
			q = strchr(tempbuf, '\0') - 1;
			if (*p == '(' && *q == ')' && q > p) p++, q--;
			if (*p == '\'' && *q == '\'' && q > p) p++, q--;
			q[1] = '\0';
			strncpy(progname, p, sizeof progname);
			progname[sizeof progname - 1] = '\0';
			list1();
			break;
		case SPCOM:
			quote = *tempbuf;
			list1();
			for (;;) {
				raw = 1;
				yychar = yylex();
				list1();
				if (yychar == 0)
					break;
				if (*tempbuf == quote) {
					yychar = yylex();
					break;
				}
			}
			break;
		}
	}
|
	LIST '\n' {
		goto dolopt; }
|
	LIST expression '\n' {
		int enable = $2->e_value;
		expr_number_check($2);
		expr_free($2);
		goto doloptA;
	dolopt:
		enable = 1;
	doloptA:
		linecnt++;
		if (outpass) {
			lineptr = linebuf;
			switch ($1->i_value) {
			case 0:	/* list */
				if (enable < 0) lstoff = 1;
				if (enable > 0) lstoff = 0;
				break;

			case 1:	/* eject */
				if (enable) eject();
				break;

			case 2:	/* space */
				if ((line + enable) > 60) eject();
				else space(enable);
				break;

			case 3:	/* elist */
				eopt = edef;
				if (enable < 0) eopt = 0;
				if (enable > 0) eopt = 1;
				break;

			case 4:	/* fopt */
				fopt = fdef;
				if (enable < 0) fopt = 0;
				if (enable > 0) fopt = 1;
				break;

			case 5:	/* gopt */
				gopt = gdef;
				if (enable < 0) gopt = 1;
				if (enable > 0) gopt = 0;
				break;

			case 6: /* mopt */
				mopt = mdef;
				if (enable < 0) mopt = 0;
				if (enable > 0) mopt = 1;
			}
		}
	}
|
	JRPROMOTE expression '\n' {
		expr_number_check($2);
		jopt = !!$2->e_value;
		list1();
		expr_free($2);
	}
|
	JPERROR expression '\n' {
		expr_number_check($2);
		JPopt = !!$2->e_value;
		list1();
		expr_free($2);
	}
|
	PUBLIC public.list '\n' {
		list1();
	}
|
	EXTRN extrn.list '\n' {
		list1();
	}
|
	MRAS_MOD '\n' {
		char *p = strchr(modstr, '\0') - 1;
		for (; p >= modstr; p--) {
			(*p)++;
			if (*p < 'Z')
				break;
			*p = 'A';
		}
		list1();
	}
|
	SETSEG '\n' {
		if (relopt && segment != $1->i_value) {
			segment = $1->i_value;
			segchange = 1;
			dollarsign = seg_pos[$1->i_value];
		}
		list1();
	}
|
	INSTSET '\n' {
		z80 = $1->i_value;
		list1();
	}
|
	UNDECLARED MACRO parm.list '\n' locals {
		$1->i_token = MNAME;
		$1->i_value = mfptr;
#ifdef M_DEBUG
		fprintf (stderr, "[UNDECLARED MACRO %s]\n", $1->i_string);
#endif
		mfseek(mfile, (long)mfptr, 0);
		cp = 0;

		// Because of locals the parser has to look ahead.
		// We'll have buffered that as we usually do so just a
		// matter of picking that up and cancelling any look-ahead.
		*lineptr = '\0';
		cp = strchr(linebuf, '\n');
		if (cp) {
			cp++;
			peekc = -1;
		}
		yychar = YYEMPTY;

		list1();
		mlex(cp);
		parm_number = 0;
	}
|
	OLDMNAME MACRO {
		$1->i_token = MNAME;
#ifdef M_DEBUG
		fprintf (stderr, "[OLDNAME MACRO %s]\n", $1->i_string);
#endif
		while (yychar != ENDM && yychar) {
			while (yychar != '\n' && yychar)
				yychar = yylex();
			list1();
			yychar = yylex();
		}
		while (yychar != '\n' && yychar) yychar = yylex();
		list1();
		yychar = yylex();
	}
|
	label.part MNAME al arg.list '\n' {
#ifdef M_DEBUG
		fprintf (stderr, "[MNAME %s]\n", $2->i_string);
#endif
		$2->i_uses++ ;
		arg_flag = 0;
		parm_number = 0;
		list(dollarsign);
		expptr++;
		est = est2;
		est2 = NULL; // GWP - this may leak, but it avoids double-free crashes
		est[FLOC].value = floc;
		est[TEMPNUM].value = exp_number++;
		floc = $2->i_value;
		mfseek(mfile, (long)floc, 0);
	}
|
	error {
		err[fflag]++;
		arg_flag = 0;
		parm_number = 0;

		if (est2)
		{
			int i;
			for (i=0; i<PARMMAX; i++) {
				if (est2[i].param) {
#ifdef M_DEBUG
	fprintf (stderr, "[Freeing2 arg%u(%p)]\n", i, est2[i].param),
#endif
					free(est2[i].param);
				}
			}
			free(est2);
			est2 = NULL;
		}

		while(yychar != '\n' && yychar != '\0') yychar = yylex();
		list(dollarsign);
		yyclearin;yyerrok;
	}
;

maybecolon:
	/* empty */ { $$ = 0; }
|
	':' { $$ = 1; }
|
	':' ':' { $$ = 2; }
;

label.part:
	/*empty*/
	 {	$$ = NULL;	}
|
	symbol maybecolon {
		coloncnt = $2;
		$1->i_scope |= segment;
		if ($2 == 2)
			$1->i_scope |= SCOPE_PUBLIC;

		if ($1->i_string[0] != '.')
			llseq++;

		switch($1->i_token) {
		case UNDECLARED:
			if (pass2)
				err[pflag]++;
			else {
				$1->i_token = LABEL;
				$1->i_value = dollarsign;
			}
			break;
		case LABEL:
			if (!pass2) {
				$1->i_token = MULTDEF;
				err[mflag]++;
			} else if ($1->i_value != dollarsign) {
				// XXX - perhaps only allow retrys if JR promotions are in play?
				if (outpass) {
					if (!passfail)
						err[pflag]++;
				}
				else {
					$1->i_value = dollarsign;
					passretry = 1;
				}
			}
			break;
		default:
			err[mflag]++;
			$1->i_token = MULTDEF;
		}
	}
;

public.list:
	public.part
|
	public.list ',' public.part
;

public.part:
	symbol {
		$1->i_scope |= SCOPE_PUBLIC;
		if (pass2) {
			if ($1->i_token == UNDECLARED) {
				err[uflag]++;
			}
		}
	}
;

extrn.list:
	extrn.part
|
	extrn.list ',' extrn.part
;

extrn.part:
	symbol {
		if (pass2 && $1->i_scope != SCOPE_NONE && !($1->i_scope & SCOPE_EXTERNAL)) {
			fprintf(stderr, "Label scope change\n");
			err[fflag]++;
		}
		$1->i_scope |= SCOPE_EXTERNAL;
		if (pass2) {
			if ($1->i_token != UNDECLARED) {
				fprintf(stderr, "External label defined locally.\n");
				err[fflag]++;
			}
		}
	}
;

operation:
	NOOPERAND
		{ emit1($1->i_value, 0, 0, ET_NOARG); }
|	
	NOOPERAND expression
		{
			// XXX - maybe splitting out CPI is better?
			if (!z80 && $1->i_value == 0166641)
				emit1(0376, 0, $2, ET_BYTE);
			else
				err[fflag]++;
		}
|
	SHIFT
		{
			if (!z80 && $1->i_value < 2)
				emit(1, E_CODE, 0, 007 | ($1->i_value << 3));
			else
				err[fflag]++;
		}
|
	JP expression
	{
		if (z80 || $1->i_value == 0303) {
			checkjp(0, $2);
			emit(1, E_CODE16, $2, 0303);
		}
		else
			// can't optimize jump on plus
			emit(1, E_CODE16, $2, 0362);
	}
|
	CALL expression
		{	emit(1, E_CODE16, $2, 0315);	}
|
	RST	expression
	{
		// accepts rst 0-7 or rst 0,8,16,...,56
		int vec = $2->e_value;
		expr_number_check($2);
		if ((vec > 7 || vec < 0) && (vec & ~(7 << 3)))
			err[vflag]++;
		if (vec > 7) vec >>= 3;
		emit(1, E_CODE, 0, $1->i_value + ((vec & 7) << 3));
		expr_free($2);
	}
|
	ALUI8 expression
		{ emit1($1->i_value, 0, $2, ET_BYTE); }
|
	ADD expression
		{ emit1(0306, 0, $2, ET_BYTE); }
|
	ADD ACC ',' expression
		{ emit1(0306, 0, $4, ET_BYTE); }
|
	ARITHC expression
		{ emit1(0306 + ($1->i_value << 3), 0, $2, ET_BYTE); }
|
	ARITHC ACC ',' expression
		{ emit1(0306 + ($1->i_value << 3), 0, $4, ET_BYTE); }
|
	LOGICAL expression
		{
			if (!z80 && $1->i_value == 7)
				emit(1, E_CODE16, $2, 0364);
			else
				emit1(0306 | ($1->i_value << 3), 0, $2, ET_BYTE);
		}
|
	AND expression
		{ emit1(0306 | ($1->i_value << 3), 0, $2, ET_BYTE); }
|
	OR expression
		{ emit1(0306 | ($1->i_value << 3), 0, $2, ET_BYTE); }
|
	XOR expression
		{ emit1(0306 | ($1->i_value << 3), 0, $2, ET_BYTE); }
|
	LOGICAL ACC ',' expression	/* -cdk */
		{ emit1(0306 | ($1->i_value << 3), 0, $4, ET_BYTE); }
|
	AND ACC ',' expression	/* -cdk */
		{ emit1(0306 | ($1->i_value << 3), 0, $4, ET_BYTE); }
|
	OR ACC ',' expression	/* -cdk */
		{ emit1(0306 | ($1->i_value << 3), 0, $4, ET_BYTE); }
|
	XOR ACC ',' expression	/* -cdk */
		{ emit1(0306 | ($1->i_value << 3), 0, $4, ET_BYTE); }
|
	ADD allreg
		{ emit1(0200 + ($2 & 0377), $2, 0, ET_NOARG_DISP); }
|
	ADD ACC ',' allreg
		{ emit1(0200 + ($4 & 0377), $4, 0, ET_NOARG_DISP); }
|
	ADD m
		{ emit(1, E_CODE, 0, 0206); }
|
	ARITHC allreg
		{ emit1(0200 + ($1->i_value << 3) + ($2 & 0377), $2, 0, ET_NOARG_DISP); }
|
	ARITHC ACC ',' allreg
		{ emit1(0200 + ($1->i_value << 3) + ($4 & 0377), $4, 0, ET_NOARG_DISP); }
|
	ARITHC m
		{ emit1(0200 + ($1->i_value << 3) + ($2 & 0377), $2, 0, ET_NOARG_DISP); }
|
	LOGICAL allreg
		{ emit1(0200 + ($1->i_value << 3) + ($2 & 0377), $2, 0, ET_NOARG_DISP); }
|
	LOGICAL m
		{ emit1(0200 + ($1->i_value << 3) + ($2 & 0377), $2, 0, ET_NOARG_DISP); }
|
	AND allreg
		{ emit1(0200 + ($1->i_value << 3) + ($2 & 0377), $2, 0, ET_NOARG_DISP); }
|
	OR allreg
		{ emit1(0200 + ($1->i_value << 3) + ($2 & 0377), $2, 0, ET_NOARG_DISP); }
|
	XOR allreg
		{ emit1(0200 + ($1->i_value << 3) + ($2 & 0377), $2, 0, ET_NOARG_DISP); }
|
	LOGICAL ACC ',' allreg		/* -cdk */
		{ emit1(0200 + ($1->i_value << 3) + ($4 & 0377), $4, 0, ET_NOARG_DISP); }
|
	AND ACC ',' allreg		/* -cdk */
		{ emit1(0200 + ($1->i_value << 3) + ($4 & 0377), $4, 0, ET_NOARG_DISP); }
|
	OR ACC ',' allreg		/* -cdk */
		{ emit1(0200 + ($1->i_value << 3) + ($4 & 0377), $4, 0, ET_NOARG_DISP); }
|
	XOR ACC ',' allreg		/* -cdk */
		{ emit1(0200 + ($1->i_value << 3) + ($4 & 0377), $4, 0, ET_NOARG_DISP); }
|
	SHIFT reg
		{ emit1(0145400 + ($1->i_value << 3) + ($2 & 0377), $2, 0, ET_NOARG_DISP); }
|
	SHIFT memxy ',' realreg
		{ emit1(0xCB00 + ($1->i_value << 3) + ($4 & 0377), $2, 0, ET_NOARG_DISP); }
|
	INCDEC	allreg
		{ emit1($1->i_value + (($2 & 0377) << 3) + 4, $2, 0, ET_NOARG_DISP); }
|
	INRDCR	reg8
		{ emit1($1->i_value + (($2 & 0377) << 3) + 4, $2, 0, ET_NOARG_DISP); }
|
	ARITHC HL ',' bcdehlsp
		{ if ($1->i_value == 1)
				emit(2,E_CODE,0,0355,0112+$4);
			else
				emit(2,E_CODE,0,0355,0102+$4);
		}
|
	ADD mar ',' bcdesp
		{ emitdad($2,$4); }
|
	ADD mar ',' mar
		{
			if ($2 != $4) {
				fprintf(stderr,"ADD mar, mar error\n");
				err[gflag]++;
			}
			emitdad($2,$4);
		}
|
	DAD evenreg8 { emitdad(040, $2); }
|
	INCDEC evenreg
		{ emit1(($1->i_value << 3) + ($2 & 0377) + 3, $2, 0, ET_NOARG); }
|
	INXDCX evenreg8
		{ emit1(($1->i_value << 3) + ($2 & 0377) + 3, $2, 0, ET_NOARG); }
|
	PUSHPOP pushable
		{ emit1($1->i_value + ($2 & 0377), $2, 0, ET_NOARG); }
|
	PUSHPOP pushable8
		{ emit1($1->i_value + ($2 & 0377), $2, 0, ET_NOARG); }
|
	BIT expression ',' reg
		{
			int bit = $2->e_value;
			expr_number_check($2);
			expr_free($2);
			if (bit < 0 || bit > 7)
				err[vflag]++;
			emit1($1->i_value + ((bit & 7) << 3) + ($4 & 0377), $4, 0, ET_NOARG_DISP);
		}
|
	BIT expression ',' memxy ',' realreg
		{
			int bit = $2->e_value;
			expr_number_check($2);
			expr_free($2);
			if (bit < 0 || bit > 7)
				err[vflag]++;
			emit1($1->i_value + ((bit & 7) << 3) + ($6 & 0377), $4, 0, ET_NOARG_DISP);
		}
|
	JP condition ',' expression
	{
		checkjp($2, $4);
		emit(1, E_CODE16, $4, 0302 + $2);
	}
|
	JUMP8 expression
	{
		checkjp($1->i_value, $2);
		emit(1, E_CODE16, $2, $1->i_value);
	}
|
	JP '(' mar ')'
		{ emit1(0351, $3, 0, ET_NOARG); }
|
	CALL condition ',' expression
		{ emit(1, E_CODE16, $4, 0304 + $2); }
|
	CALL8 expression
		{ emit(1, E_CODE16, $2, $1->i_value); }
|
	JR expression
		{ emitjr(030,$2); }
|
	JR spcondition ',' expression
		{ emitjr($1->i_value + $2, $4); }
|
	DJNZ expression
		{ emitjr($1->i_value, $2); }
|
	RET
		{ emit(1, E_CODE, 0, $1->i_value); }
|
	RET condition
		{ emit(1, E_CODE, 0, 0300 + $2); }
|
	LD allreg ',' allreg
		{
			// Many constraints on byte access to IX/IY registers.
			if (($2 | $4) >> 16) {
				int a = $2;
				int b = $4;

				// Only ixh,ixh; ixh,ixl; ixl,ixh; ixl,ixl allowed.
				if (a >> 16 && b >> 16) {
					if (a >> 8 != b >> 8) {
						fprintf(stderr, "LD cannot move between ix and iy\n");
						err[gflag]++;
					}
				}
				else {
					int c = b >> 16 ? a : b;
					// No access to h, l, (hl), (ix), (iy)
					if (c == 4 || c == 5 || (c & 0xff) == 6) {
						fprintf(stderr, "LD cannot combine i/xy/lh and h,l,(hl),(ix) or (iy).\n");
						err[gflag]++;
					}
				}
			}

			if (($2 & 0377) == 6 && ($4 & 0377) == 6) {
				fprintf(stderr,"LD reg, reg error: can't do memory to memory\n");
				err[gflag]++;
			}
			emit1(0100 + (($2 & 7) << 3) + ($4 & 7),$2 | $4, 0, ET_NOARG_DISP);
		}
|
	MOV reg8 ',' reg8
		{
			if ($2 == 6 && $4 == 6) err[gflag]++;
			emit1(0100 + (($2 & 7) << 3) + ($4 & 7),$2 | $4, 0, ET_NOARG_DISP);
		}
|
	LD allreg ',' noparenexpr
		{ emit1(6 + (($2 & 0377) << 3), $2, $4, ET_BYTE); }
|
	MVI reg8 ',' expression
		{ emit1(6 + (($2 & 0377) << 3), $2, $4, ET_BYTE); }
|
	LD allreg ',' '(' RP ')'
		{	if ($2 != 7) {
				fprintf(stderr,"LD reg, (RP) error\n");
				err[gflag]++;
			}
			else emit(1, E_CODE, 0, 012 + $5->i_value);
		}
|
	LDAX realreg
		{
			if ($2 != 0 && $2 != 2) err[gflag]++;
			emit(1, E_CODE, 0, 012 + ($2 << 3));
		}
|
	LD allreg ',' parenexpr
		{
			if ($2 != 7) {
				fprintf(stderr,"LD reg, (expr) error: A only valid destination\n");
				err[gflag]++;
			}
			else {
				expr_word_check($4);
				emit(1, E_CODE16, $4, 072);
			}
		}
|
	LDA expression
		{
			expr_word_check($2);
			emit(1, E_CODE16, $2, 072);
		}
|
	LD '(' RP ')' ',' ACC
		{ emit(1, E_CODE, 0, 2 + $3->i_value); }
|
	STAX realreg
		{
			if ($2 != 0 && $2 != 2) err[gflag]++;
			emit(1, E_CODE, 0, 2 + ($2 << 3));
		}
|
	LD parenexpr ',' ACC
		{
			expr_word_check($2);
			emit(1, E_CODE16, $2, 062);
		}
|
	STA expression
		{
			expr_word_check($2);
			emit(1, E_CODE16, $2, 062);
		}
|
	LD allreg ',' MISCREG
		{
			if ($2 != 7) {
				fprintf(stderr,"LD reg, MISCREG error: A only valid destination\n");
				err[gflag]++;
			}
			else emit(2, E_CODE, 0, 0355, 0127 + $4->i_value);
		}
|
	LD MISCREG ',' ACC
		{ emit(2, E_CODE, 0, 0355, 0107 + $2->i_value); }
|
	LD evenreg ',' lxexpression
		{
			expr_word_check($4);
			emit1(1 + ($2 & 060), $2, $4, ET_WORD);
		}
|
	LXI evenreg8 ',' lxexpression
		{
			expr_word_check($4);
			emit1(1 + ($2 & 060), $2, $4, ET_WORD);
		}
|
	LD evenreg ',' parenexpr
		{
			expr_word_check($4);
			if (($2 & 060) == 040)
				emit1(052, $2, $4, ET_WORD);
			else
				emit(2, E_CODE16, $4, 0355, 0113 + $2);
		}
|
	LHLD expression
		{
			expr_word_check($2);
			emit1(052, 040, $2, ET_WORD);
		}
|
	LD parenexpr ',' evenreg
		{
			expr_word_check($2);
			if (($4 & 060) == 040)
				emit1(042, $4, $2, ET_WORD);
			else
				emit(2, E_CODE16, $2, 0355, 0103 + $4);
		}
|
	SHLD expression
		{
			expr_word_check($2);
			emit1(042, 040, $2, ET_WORD);
		}
|
	LD evenreg ',' mar
		{
			if ($2 != 060) {
				fprintf(stderr,"LD evenreg error\n");
				err[gflag]++;
			}
			else
				emit1(0371, $4, 0, ET_NOARG);
		}
|
	EX RP ',' HL
		{
			if ($2->i_value != 020) {
				fprintf(stderr,"EX RP, HL error\n");
				err[gflag]++;
			}
			else
				emit(1, E_CODE, 0, 0353);
		}
|
	EX AF ',' AFp
		{ emit(1, E_CODE, 0, 010); }
|
	EX '(' SP ')' ',' mar
		{ emit1(0343, $6, 0, ET_NOARG); }
|
	TK_IN realreg ',' parenexpr
		{
			if ($2 != 7) {
				fprintf(stderr,"IN reg, (expr) error\n");
				err[gflag]++;
			}
			else	{
				if ($4->e_value < 0 || $4->e_value > 255)
					err[vflag]++;
				emit(1, E_CODE8, $4, $1->i_value);
			}
		}
|
	TK_IN expression
		{
			if ($2->e_value < 0 || $2->e_value > 255)
				err[vflag]++;
			emit(1, E_CODE8, $2, $1->i_value);
		}
|
	TK_IN realreg ',' '(' C ')'
		{ emit(2, E_CODE, 0, 0355, 0100 + ($2 << 3)); }
|
	TK_IN 'F' ',' '(' C ')'
		{ emit(2, E_CODE, 0, 0355, 0160); }
|
	TK_IN '(' C ')'
		{ emit(2, E_CODE, 0, 0355, 0160); }
|
	TK_OUT parenexpr ',' ACC
		{
			if ($2->e_value < 0 || $2->e_value > 255)
				err[vflag]++;
			emit(1, E_CODE8, $2, $1->i_value);
		}
|
	TK_OUT expression
		{
			if ($2->e_value < 0 || $2->e_value > 255)
				err[vflag]++;
			emit(1, E_CODE8, $2, $1->i_value);
		}
|
	TK_OUT '(' C ')' ',' realreg
		{ emit(2, E_CODE, 0, 0355, 0101 + ($6 << 3)); }
|
	TK_OUT '(' C ')' ',' expression
		{
			expr_number_check($6);
			if ($6->e_value != 0) {
				fprintf(stderr, "Can only output 0 to port C with OUT\n");
				err[vflag]++;
			}
			expr_free($6);

			emit(2, E_CODE8, 0, 0355, 0101 + (6 << 3));
		}
|
	IM expression
		{
			int im = $2->e_value;
			expr_number_check($2);
			expr_free($2);
			if (im > 2 || im < 0)
				err[vflag]++;
			else
				emit(2, E_CODE, 0, $1->i_value >> 8, $1->i_value + ((im + (im > 0)) << 3));
		}
|
	PHASE expression
		{
			expr_number_check($2);
			if (phaseflag) {
				err[oflag]++;
			} else {
				phaseflag = 1;
				phdollar = dollarsign;
				dollarsign = $2->e_value;
				phbegin = dollarsign;
			}
			expr_free($2);
		}
|
	DEPHASE
		{
			if (!phaseflag) {
				err[oflag]++;
			} else {
				phaseflag = 0;
				dollarsign = phdollar + dollarsign - phbegin;
			}
		}
|
	ORG expression
		{
			expr_reloc_check($2);
			// Cannot org to the other segment (but absolute values are OK)
			if (relopt && segment && ($2->e_scope & SCOPE_SEGMASK) != segment)
				err[rflag]++;
			if (phaseflag) {
				err[oflag]++;
				dollarsign = phdollar + dollarsign - phbegin;
				phaseflag = 0;
			}
			if ($2->e_value-dollarsign) {
				flushbin();
				flushoth();
				olddollar = $2->e_value;
				oldothdollar = $2->e_value;
				dollarsign = $2->e_value;
				emit_addr = $2->e_value;
				seg_pos[segment] = dollarsign;
				if (seg_pos[segment] > seg_size[segment])
					seg_size[segment] = seg_pos[segment];
				putrelcmd(RELCMD_SETLOC);
				putrelsegref(segment, seg_pos[segment]);
				segchange = 0;
			}
			expr_free($2);
		}
|
	ASSERT expression
		{
			expr_number_check($2);
			if (outpass && !$2->e_value)
			{
				err[aflag]++;
			}
			expr_free($2);
		}
|
	TSTATE expression
		{
			expr_number_check($2);
			tstates = $2->e_value;
			tstatesum[emit_addr] = tstates;
			expr_free($2);
		}
|
	SETOCF expression
		{
			expr_number_check($2);
			ocf = $2->e_value;
			ocfsum[emit_addr] = ocf;
			expr_free($2);
		}
|
	DEFB db.list
|
	DEFW dw.list
|
	DEFD dd.list
|
	ENDM
;


parm.list:
|
	parm.element
|
	parm.list ',' parm.element
;

maybeocto: { $$ = 0; } | '#' { $$ = 1; };

parm.element:
	maybeocto UNDECLARED
		{
			$2->i_token = MPARM;
			if (parm_number >= PARMMAX)
				error("Too many parameters");
			$2->i_value = parm_number++;
			$2->i_scope = $1;
			$2->i_chain = 0;
		}
;

locals:
|
	LOCAL	local.list '\n' { list1(); }
;

local.list:
|
	local.element
|
	local.list ',' local.element
;

local.element:
	UNDECLARED
		{
			$1->i_token = MPARM;
			if (parm_number >= PARMMAX)
				error("Too many parameters");
			$1->i_value = parm_number++;
			$1->i_scope = 0;
			$1->i_chain = 1;
		}
;

arg.list:
	/* empty */
|
	arg.element
|
	arg.list ',' arg.element
;


arg.element:
	ARG
		{
			cp = malloc(strlen(tempbuf)+1);
#ifdef M_DEBUG
			fprintf (stderr, "[Arg%u(%p): %s]\n", parm_number, cp, tempbuf);
#endif
			est2[parm_number++].param = cp;
			strcpy(cp, tempbuf);
		}
;
allreg:
	reg
|
	ixylhreg
;
reg:
	realreg
|
	mem
;
ixylhreg:
	IXYLH
		{
			$$ = $1->i_value;
		}
;
reg8:
	realreg
|
	m
;
m:
	COND { if ($1->i_value != 070) err[gflag]++; $$ = 6; }
;
realreg:
	REGNAME
		{
			$$ = $1->i_value;
		}
|
	ACC
		{
			$$ = $1->i_value;
		}
|
	C
		{
			$$ = $1->i_value;
		}
;
mem:
	'(' HL ')'
		{
			$$ = 6;
		}
|
	memxy
;
memxy:
	'(' INDEX expression ')'
		{
			expr_number_check($3);
			disp = $3->e_value;
			expr_free($3);
			$$ = ($2->i_value & 0177400) | 6;
			if (disp > 127 || disp < -128)
				err[vflag]++;
		}
|
	'(' INDEX ')'
		{
			disp = 0;
			$$ = ($2->i_value & 0177400) | 6;
		}
;
evenreg:
	bcdesp
|
	mar
;
evenreg8:
	realreg	{ if ($1 & 1) err[gflag]++; $$ = $1 << 3; }
|
	SP { $$ = $1->i_value; }
;
pushable:
	RP
		{
			$$ = $1->i_value;
		}
|
	AF
		{
			$$ = $1->i_value;
		}
|
	mar
;
pushable8:
	realreg	{ if ($1 & 1) err[gflag]++; $$ = $1 << 3; }
|
	PSW { $$ = $1->i_value; }
;
bcdesp:
	RP
		{
			$$ = $1->i_value;
		}
|
	SP
		{
			$$ = $1->i_value;
		}
;
bcdehlsp:
	bcdesp
|
	HL
		{
			$$ = $1->i_value;
		}
;
mar:
	HL
		{
			$$ = $1->i_value;
		}
|
	INDEX
		{
			$$ = $1->i_value;
		}
;
condition:
	spcondition
|
	COND
		{
			$$ = $1->i_value;
		}
;
spcondition:
	SPCOND
		{
			$$ = $1->i_value;
		}
|
	C
		{	$$ = 030;	}
;
db.list:
	db.list.element
|
	db.list ',' db.list.element
;
db.list.element:
	TWOCHAR
		{
			emit(1, E_DATA, expr_num($1));
			emit(1, E_DATA, expr_num($1>>8));
		}
|
	STRING
		{
			cp = $1;
			while (*cp != '\0')
				emit(1,E_DATA,expr_num(*cp++));
		}
|
	expression
		{
			if (is_number($1) && ($1->e_value < -128 || $1->e_value > 255))
				err[vflag]++;
			emit(1, E_DATA, $1);
		}
;


dw.list:
	dw.list.element
|
	dw.list ',' dw.list.element
;


dw.list.element:
	expression
		{
			if ($1->e_value < -32768 || $1->e_value > 65535) {
				err[vflag]++;
			}
			emit(2, E_DATA, $1);
		}
;

dd.list:
	dd.list.element
|
	dd.list ',' dd.list.element
;


dd.list.element:
	expression
		{
			// Can't check overflow as I only have 32 bit ints.
			emit(4, E_DATA, $1);
		}
;



lxexpression:
	noparenexpr
|
	TWOCHAR
		{
			$$ = expr_num($1);
		}
;

expression:
	parenexpr
|
	noparenexpr
;

parenexpr:
	'(' expression ')'
		{	$$ = $2;	}
;

noparenexpr:
	error
		{
			err[eflag]++;
			$$ = expr_num(0);
		}
|
	LABEL
		{
			$$ = expr_alloc();
			$$->e_token = 'v';
			$$->e_item = $1;
			$$->e_scope = $1->i_scope;
			$$->e_value = $1->i_value;
			$1->i_uses++;
		}
|
	NUMBER
		{
			$$ = expr_num($1);
		}
|
	ONECHAR
		{
			$$ = expr_num($1);
		}
|
	EQUATED
		{
			$$ = expr_alloc();
			$$->e_token = 'v';
			$$->e_item = $1; // not necessary
			$$->e_scope = $1->i_scope;
			$$->e_value = $1->i_value;
		}
|
	WASEQUATED
		{
			$$ = expr_alloc();
			$$->e_token = 'v';
			$$->e_item = $1; // not necessary
			$$->e_scope = $1->i_scope;
			$$->e_value = $1->i_value;
		}
|
	DEFLED
		{
			$$ = expr_alloc();
			$$->e_token = 'v';
			$$->e_item = $1; // not necessary
			$$->e_scope = $1->i_scope;
			$$->e_value = $1->i_value;
		}
|
	'$'
		{
			$$ = expr_num(dollarsign);
			$$->e_scope = segment;
		}
|
	UNDECLARED
		{
			$$ = expr_alloc();
			$$->e_token = 'u';
			$$->e_item = $1;
			$$->e_scope = $1->i_scope;
			$$->e_value = 0;

			if (!($1->i_scope & SCOPE_EXTERNAL)) {
				err[uflag]++;
			}
		}
|
	MULTDEF
		{
			$$ = expr_alloc();
			$$->e_token = 'm';
			$$->e_item = $1;
			$$->e_scope = $1->i_scope;
			$$->e_value = 0;
		}
|
	expression '+' expression
		{
			$$ = expr_op($1, '+', $3, $1->e_value + $3->e_value);

			// Can't operate on external labels.
			// But we can add constants to any scope.
			if (!(($1->e_scope | $3->e_scope) & SCOPE_EXTERNAL) &&
				(($1->e_scope && SCOPE_SEGMASK) == 0 ||
				($3->e_scope && SCOPE_SEGMASK) == 0))
			{
				$$->e_scope &= ~(SCOPE_NORELOC | SCOPE_SEGMASK);
				$$->e_scope |= ($1->e_scope | $3->e_scope & SCOPE_SEGMASK);
			}
		}
|
	expression '-' expression
		{
			$$ = expr_op_sc($1, '-', $3, $1->e_value - $3->e_value);

			// But we can subtract a constant.
			if (!(($1->e_scope | $3->e_scope) & SCOPE_EXTERNAL) &&
				(($3->e_scope & SCOPE_SEGMASK) == 0))
			{
				$$->e_scope &= ~(SCOPE_NORELOC | SCOPE_SEGMASK);
				$$->e_scope |= ($1->e_scope & SCOPE_SEGMASK);
			}
		}
|
	expression '/' expression
		{
			int val = 0;
			if ($3->e_value == 0)
				err[eflag]++;
			else
				val = $1->e_value / $3->e_value;

			$$ = expr_op($1, '/', $3, val);
		}
|
	expression '*' expression
		{	$$ = expr_op($1, '*', $3, $1->e_value * $3->e_value); }
|
	expression '%' expression
		{
			int val;
		domod:
			val = 0;
			if ($3->e_value == 0)
				err[eflag]++;
			else
				val = $1->e_value % $3->e_value;

			$$ = expr_op($1, '%', $3, val);
		}
|
	expression MOD expression
		{	goto domod;	}
|
	expression '&' expression
		{	$$ = expr_op($1, '&', $3, $1->e_value & $3->e_value); }
|
	expression AND expression
		{	$$ = expr_op($1, '&', $3, $1->e_value & $3->e_value); }
|
	expression '|' expression
		{	$$ = expr_op($1, '|', $3, $1->e_value | $3->e_value); }
|
	expression OR expression
		{	$$ = expr_op($1, '|', $3, $1->e_value | $3->e_value); }
|
	expression '^' expression
		{	$$ = expr_op($1, '^', $3, $1->e_value ^ $3->e_value); }
|
	expression XOR expression
		{	$$ = expr_op($1, '^', $3, $1->e_value ^ $3->e_value); }
|
	expression SHL expression
		{	$$ = expr_op($1, $2, $3, $1->e_value << $3->e_value); }
|
	expression SHR expression
		{
			int val = $3->e_value == 0 ? $1->e_value : (($1->e_value >> 1) & ((0x7fff << 16) | 0xffff)) >> ($3->e_value - 1);
			$$ = expr_op($1, $2, $3, val);
		}
|
	expression '<' expression
		{	$$ = expr_op_sc($1, '<', $3, $1->e_value < $3->e_value); }
|
	expression '=' expression
		{	$$ = expr_op_sc($1, '=', $3, $1->e_value == $3->e_value); }
|
	expression '>' expression
		{	$$ = expr_op_sc($1, '>', $3, $1->e_value > $3->e_value); }
|
	expression LT expression
		{	$$ = expr_op_sc($1, $2, $3, $1->e_value < $3->e_value); }
|
	expression GT expression
		{	$$ = expr_op_sc($1, $2, $3, $1->e_value > $3->e_value); }
|
	expression LE expression
		{	$$ = expr_op_sc($1, $2, $3, $1->e_value <= $3->e_value); }
|
	expression GE expression
		{	$$ = expr_op_sc($1, $2, $3, $1->e_value >= $3->e_value); }
|
	expression NE expression
		{	$$ = expr_op_sc($1, $2, $3, $1->e_value != $3->e_value); }
|
	expression ANDAND expression
		{	$$ = expr_op($1, $2, $3, $1->e_value && $3->e_value); }
|
	expression OROR expression
		{	$$ = expr_op($1, $2, $3, $1->e_value || $3->e_value); }
|
	expression '?' expression ':' expression
		{
			expr_number_check($1);
			if ($1->e_value) {
				$$ = $3;
				expr_free($5);
			}
			else {
				$$ = $5;
				expr_free($3);
			}
			expr_free($1);
		}
|
	'[' expression ']'
		{	$$ = $2;	}
|
	NOT expression
		{	$$ = expr_op($2, '~', 0, ~$2->e_value);	}
|
	'~' expression
		{	$$ = expr_op($2, '~', 0, ~$2->e_value);	}
|
	'!' expression
		{	$$ = expr_op($2, '!', 0, !$2->e_value);	}
|
	'+' expression %prec UNARY
		{	$$ = $2; /* no effect */	}
|
	'-' expression %prec UNARY
		{	$$ = expr_op($2, '-', 0, -$2->e_value);	}
|
	T expression %prec UNARY
		{
			expr_reloc_check($2);
			$$ = expr_num(tstatesum[phaseaddr($2->e_value)]);
			expr_free($2);
		}
|
	TILO expression %prec UNARY
		{
			int low, low8080, fetch;
			expr_reloc_check($2);
			zi_tstates(memory + phaseaddr($2->e_value), &low, 0, &fetch, &low8080, 0);
			$$ = expr_num(z80 ? low : low8080);
			expr_free($2);
		}
|
	TIHI expression %prec UNARY
		{
			int high, high8080, fetch;
			expr_reloc_check($2);
			zi_tstates(memory + phaseaddr($2->e_value), 0, &high, &fetch, 0, &high8080);
			$$ = expr_num(z80 ? high : high8080);
			expr_free($2);
		}
|
	OCF expression %prec UNARY
		{
			expr_reloc_check($2);
			$$ = expr_num(ocfsum[phaseaddr($2->e_value)]);
			expr_free($2);
		}
|
	LOW expression %prec UNARY
		{
			$$ = expr_op($2, $1, 0, $2->e_value & 0xff);
		}
|
	HIGH expression %prec UNARY
		{
			$$ = expr_op($2, $1, 0, ($2->e_value >> 8) & 0xff);
		}
;

symbol:
	UNDECLARED
|
	LABEL
|
	MULTDEF
|
	EQUATED
|
	WASEQUATED
|
	DEFLED
;


al:
	{ int i;
		if (expptr >= MAXEXP)
			error("Macro expansion level");
		est2 = (union exprec *) malloc((PARMMAX + 4) * sizeof *est2);
		expstack[expptr] = est2;
		for (i=0; i<PARMMAX; i++)
			est2[i].param = 0;
		arg_flag++;
	}
;


arg_on:
	{	arg_flag++;	}
;

arg_off:
		{	arg_flag = 0;	}
;

raw_on: { raw = 1; }

%%
/*extern int	yylval;*/

#define F_END	0
#define OTHER	1
#define SPACE	2
#define DIGIT	3
#define LETTER	4
#define STARTER 5
#define DOLLAR	6


/*
 *  This is the table of character classes.  It is used by the lexical
 *  analyser. (yylex())
 */
char	charclass[] = {
	F_END,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,
	OTHER,	SPACE,	OTHER,	OTHER,	OTHER,	SPACE,	OTHER,	OTHER,
	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,
	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,
	SPACE,	OTHER,	OTHER,	OTHER,	DOLLAR,	OTHER,	OTHER,	OTHER,	//  !"#$%&'
	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	STARTER,OTHER,	// ()*+,-./
	DIGIT,	DIGIT,	DIGIT,	DIGIT,	DIGIT,	DIGIT,	DIGIT,	DIGIT,	// 01234567
	DIGIT,	DIGIT,	OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	STARTER,// 89:;<=>?
	STARTER,LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER,	// @ABCDEFG
	LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER,	// HIJKLMNO
	LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER,	// PQRSTUVW
	LETTER, LETTER, LETTER, OTHER,	OTHER,	OTHER,	OTHER,	LETTER,	// XYZ[\]^_
	OTHER,	LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER,	// `abcdefg
	LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER,	// hijklmno
	LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER,	// pqrstuvw
	LETTER, LETTER, LETTER, OTHER,	OTHER,	OTHER,	OTHER,	OTHER,	// xyz{|}~
};


/*
 *  the following table tells which characters are parts of numbers.
 *  The entry is non-zero for characters which can be parts of numbers.
 */
char	numpart[] = {
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	'0',	'1',	'2',	'3',	'4',	'5',	'6',	'7',
	'8',	'9',	0,	0,	0,	0,	0,	0,
	0,	'A',	'B',	'C',	'D',	'E',	'F',	0,
	'H',	0,	0,	0,	0,	0,	0,	'O',
	0,	'Q',	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	'a',	'b',	'c',	'd',	'e',	'f',	0,
	'h',	0,	0,	0,	0,	0,	0,	'o',
	0,	'q',	0,	0,	0,	0,	0,	0,
	'x',	0,	0,	0,	0,	0,	0,	0,
	0};




/*
 *  the following table is a list of assembler mnemonics;
 *  for each mnemonic the associated machine-code bit pattern
 *  and symbol type are given.
 *
 *  The i_uses field is overloaded to indicate the possible uses for
 *  a token.
 */

#define VERB	(1)	/* opcode or psuedo-op */
#define I8080	(2)	/* used in 8080 instructions */
#define Z80	(4)	/* used in Z80 instructions */
#define UNDOC	(8)	/* used only in undocumented instructions */
#define TERM	(16)	/* can appear in expressions (not all marked) */

struct	item	keytab[] = {
	{"*mod",	0,	MRAS_MOD,	VERB },
	{".8080",	0,	INSTSET,	VERB },
	{"a",		7,	ACC,		I8080 | Z80 },
	{"aci",		0316,	ALUI8,		VERB | I8080 },
	{"adc",		1,	ARITHC,		VERB | I8080 | Z80  },
	{"add",		0,	ADD,		VERB | I8080 | Z80  },
	{"adi",		0306,	ALUI8,		VERB | I8080 },
	{"af",		060,	AF,		Z80 },
	{"ana",		4,	ARITHC,		VERB | I8080},
	{"and",		4,	AND,		VERB | Z80 | TERM },
	{"ani",		0346,	ALUI8,		VERB | I8080 },
	{".ascii",	0,	DEFB,		VERB },
	{".aseg",	SEG_ABS,SETSEG,		VERB },
	{".aset",	0,	DEFL,		VERB },
	{".assert",	0,	ASSERT,		VERB },
	{"b",		0,	REGNAME,	I8080 | Z80 },
	{"bc",		0,	RP,		Z80 },
	{"bit",		0145500,BIT,		VERB | Z80 },
	{".block",	0,	DEFS,		VERB },
	{".byte",	0,	DEFB,		VERB },
	{"c",		1,	C,		I8080 | Z80 },
	{"call",	0315,	CALL,		VERB | I8080 | Z80 },
	{"cc",		0334,	CALL8,		VERB | I8080 },
	{"ccf",		077,	NOOPERAND,	VERB | Z80 },
	{"cm",		0374,	CALL8,		VERB | I8080 },
	{"cma",		057,	NOOPERAND,	VERB | I8080 },
	{"cmc",		077,	NOOPERAND,	VERB | I8080 },
	{"cmp",		7,	LOGICAL,	VERB | I8080 },
	{"cnc",		0324,	CALL8,		VERB | I8080 },
	{"cnz",		0304,	CALL8,		VERB | I8080 },
	{".comment",	SPCOM,	SPECIAL,	VERB },
	{".cond",	0,	IF_TK,		VERB },
	{"cp",		7,	LOGICAL,	VERB | I8080 | Z80 },
	{"cpd",		0166651,NOOPERAND,	VERB | Z80 },
	{"cpdr",	0166671,NOOPERAND,	VERB | Z80 },
	{"cpe",		0354,	CALL8,		VERB | I8080 },
	{"cpi",		0166641,NOOPERAND,	VERB | I8080 | Z80 },
	{"cpir",	0166661,NOOPERAND,	VERB | Z80 },
	{"cpl",		057,	NOOPERAND,	VERB | Z80 },
	{"cpo",		0344,	CALL8,		VERB | I8080 },
	{".cseg",	SEG_CODE,SETSEG,	VERB },
	{"cz",		0314,	CALL8,		VERB | I8080 },
	{"d",		2,	REGNAME,	I8080 | Z80 },
	{"daa",		0047,	NOOPERAND,	VERB | I8080 | Z80 },
	{"dad",		0,	DAD,		VERB | I8080 },
	{".db",		0,	DEFB,		VERB },
	{".dc",		0,	DC,		VERB },
	{"dcr",		1,	INRDCR,		VERB | I8080 },
	{"dcx",		1,	INXDCX,		VERB | I8080 },
	{"de",		020,	RP,		Z80 },
	{"dec",		1,	INCDEC,		VERB | I8080 | Z80 },
	{".defb",	0,	DEFB,		VERB },
	{".defd",	0,	DEFD,		VERB },
	{".defl",	0,	DEFL,		VERB },
	{".defm",	0,	DEFB,		VERB },
	{".defs",	0,	DEFS,		VERB },
	{".defw",	0,	DEFW,		VERB },
	{".dephase",	0,	DEPHASE,	VERB },
	{"di",		0363,	NOOPERAND,	VERB | I8080 | Z80 },
	{"djnz",	020,	DJNZ,		VERB | Z80 },
	{".ds",		0,	DEFS,		VERB },
	{".dseg",	SEG_DATA,SETSEG,	VERB },
	{".dw",		0,	DEFW,		VERB },
	{".dword",	0,	DEFD,		VERB },
	{"e",		3,	REGNAME,	I8080 | Z80 },
	{"ei",		0373,	NOOPERAND,	VERB | I8080 | Z80 },
	{".eject",	1,	LIST,		VERB },
	{".elist",	3,	LIST,		VERB },
	{".else",	0,	ELSE_TK,	VERB },
	{".end",	0,	END,		VERB },
	{".endc",	0,	ENDIF_TK,	VERB },
	{".endif",	0,	ENDIF_TK,	VERB },
	{".endm", 	0,	ENDM,		VERB },
	{".entry",	0,	PUBLIC,		VERB },
	{"eq",		0,	'=',		0 },
	{".equ",	0,	EQU,		VERB },
	{"ex",		0,	EX,		VERB | Z80 },
	{".ext",	0,	EXTRN,		VERB },
	{".extern",	0,	EXTRN,		VERB },
	{".extrn",	0,	EXTRN,		VERB },
	{"exx",		0331,	NOOPERAND,	VERB | Z80 },
	{".flist",	4,	LIST,		VERB },
	{"ge",		0,	GE,		0 },
	{".glist",	5,	LIST,		VERB },
	{".global",	0,	PUBLIC,		VERB },
	{"gt",		0,	GT,		0 },
	{"h",		4,	REGNAME,	I8080 | Z80 },
	{"halt",	0166,	NOOPERAND,	VERB | Z80 },
	{"high",	0,	HIGH,		0 },
	{"hl",		040,	HL,		Z80 },
	{"hlt",		0166,	NOOPERAND,	VERB | I8080 },
	{"i",		0,	MISCREG,	Z80 },
	{".if",		0,	IF_TK,		VERB },
	{"im",		0166506,IM,		VERB | Z80 },
	{"in",		0333,	TK_IN,		VERB | I8080 | Z80 },
	{"inc",		0,	INCDEC,		VERB | Z80 },
	{".incbin", 	0, 	INCBIN,		VERB },
	{".include",	PSINC,	ARGPSEUDO,	VERB },
	{"ind",		0166652,NOOPERAND,	VERB | Z80 },
	{"indr",	0166672,NOOPERAND,	VERB | Z80 },
	{"ini",		0166642,NOOPERAND,	VERB | Z80 },
	{"inir",	0166662,NOOPERAND,	VERB | Z80 },
	{"inr",		0,	INRDCR,		VERB | I8080 },
	{"inx",		0,	INXDCX,		VERB | I8080 },
	{"ix",		0156440,INDEX,		Z80 },
	{"ixh",		0x1DD04,IXYLH,		Z80 | UNDOC },
	{"ixl",		0x1DD05,IXYLH,		Z80 | UNDOC },
	{"iy",		0176440,INDEX,		Z80 },
	{"iyh",		0x1FD04,IXYLH,		Z80 | UNDOC },
	{"iyl",		0x1FD05,IXYLH,		Z80 | UNDOC },
	{"jc",		0332,	JUMP8,		VERB | I8080 },
	{"jm",		0372,	JUMP8,		VERB | I8080 },
	{"jmp",		0303,	JP,		VERB | I8080 },
	{"jnc",		0322,	JUMP8,		VERB | I8080 },
	{"jnz",		0302,	JUMP8,		VERB | I8080 },
	{"jp",		0,	JP,		VERB | I8080 | Z80 },
	{"jpe",		0352,	JUMP8,		VERB | I8080 },
	{".jperror",	0,	JPERROR,	VERB },
	{"jpo",		0342,	JUMP8,		VERB | I8080 },
	{"jr",		040,	JR,		VERB | Z80 },
	{".jrpromote",	0,	JRPROMOTE,	VERB },
	{"jz",		0312,	JUMP8,		VERB | I8080 },
	{"l",		5,	REGNAME,	I8080 | Z80 },
	{"ld",		0,	LD,		VERB | Z80 },
	{"lda",		0,	LDA,		VERB | I8080 },
	{"ldax",	0,	LDAX,		VERB | I8080 },
	{"ldd",		0166650,NOOPERAND,	VERB | Z80 },
	{"lddr",	0166670,NOOPERAND,	VERB | Z80 },
	{"ldi",		0166640,NOOPERAND,	VERB | Z80 },
	{"ldir",	0166660,NOOPERAND,	VERB | Z80 },
	{"le",		0,	LE,		0 },
	{"lhld",	0,	LHLD,		VERB | I8080 },
	{".list",	0,	LIST,		VERB },
	{".local",	0,	LOCAL,		VERB },
	{"low",		0,	LOW,		0 },
	{"lt",		0,	LT,		0 },
	{"lxi",		0,	LXI,		VERB | I8080 },
	{"m",		070,	COND,		I8080 | Z80 },
	{".macro",	0,	MACRO,		VERB },
	{".max",	1,	MINMAX,		VERB },
	{".min",	0,	MINMAX,		VERB },
	{".mlist",	6,	LIST,		VERB },
	{"mod",		0,	MOD,		0 },
	{"mov",		0,	MOV,		VERB | I8080 },
	{"mvi",		0,	MVI,		VERB | I8080 },
	{".name",	SPNAME,	SPECIAL,	VERB },
	{"nc",		020,	SPCOND,		0 },
	{"ne",		0,	NE,		0 },
	{"neg",		0166504,NOOPERAND,	VERB | Z80 },
	{".nolist",	-1,	LIST,		VERB },
	{"nop",		0,	NOOPERAND,	VERB | I8080 | Z80 },
	{"not",		0,	NOT,		0 },
	{"nv",		040,	COND,		Z80 },
	{"nz",		0,	SPCOND,		Z80 },
	{"ocf",		0,	OCF,		0 },
	{"or",		6,	OR,		VERB | Z80 | TERM },
	{"ora",		6,	LOGICAL,	VERB | I8080 },
	{".org",	0,	ORG,		VERB },
	{"ori",		0366,	ALUI8,		VERB | I8080 },
	{"otdr",	0166673,NOOPERAND,	VERB | Z80 },
	{"otir",	0166663,NOOPERAND,	VERB | Z80 },
	{"out",		0323,	TK_OUT,		VERB | I8080 | Z80 },
	{"outd",	0166653,NOOPERAND,	VERB | Z80 },
	{"outi",	0166643,NOOPERAND,	VERB | Z80 },
	{"p",		060,	COND,		Z80 },
	{".page",	1,	LIST,		VERB },
	{"pchl",	0351,	NOOPERAND,	VERB | I8080 },
	{"pe",		050,	COND,		Z80 },
	{"pfix",	0xdd,	NOOPERAND,	VERB | Z80 | UNDOC },
	{"pfiy",	0xfd,	NOOPERAND,	VERB | Z80 | UNDOC },
	{".phase",	0,	PHASE,		VERB },
	{"po",		040,	COND,		Z80 },
	{"pop",		0301,	PUSHPOP,	VERB | I8080 | Z80 },
	{"psw", 	060,	PSW,		I8080 },
	{".public",	0,	PUBLIC,		VERB },
	{"push",	0305,	PUSHPOP,	VERB | I8080 | Z80 },
	{"r",		010,	MISCREG,	Z80 },
	{"ral",		027,	NOOPERAND,	VERB | I8080 },
	{"rar",		037,	NOOPERAND,	VERB | I8080 },
	{"rc",		0330,	NOOPERAND,	VERB | I8080 },
	{".read",	PSINC,	ARGPSEUDO,	VERB },
	{"res",		0145600,BIT,		VERB | Z80 },
	{"ret",		0311,	RET,		VERB | I8080 | Z80 },
	{"reti",	0166515,NOOPERAND,	VERB | Z80 },
	{"retn",	0166505,NOOPERAND,	VERB | Z80 },
	{"rl",		2,	SHIFT,		VERB | Z80 },
	{"rla",		027,	NOOPERAND,	VERB | Z80 },
	{"rlc",		0,	SHIFT,		VERB | I8080 | Z80 },
	{"rlca",	07,	NOOPERAND,	VERB | Z80 },
	{"rld",		0166557,NOOPERAND,	VERB | Z80 },
	{"rm",		0370,	NOOPERAND,	VERB | I8080 },
	{".rmem",	0,	DEFS,		VERB },
	{"rnc",		0320,	NOOPERAND,	VERB | I8080 },
	{"rnz",		0300,	NOOPERAND,	VERB | I8080 },
	{"rp",		0360,	NOOPERAND,	VERB | I8080 },
	{"rpe",		0350,	NOOPERAND,	VERB | I8080 },
	{"rpo",		0340,	NOOPERAND,	VERB | I8080 },
	{"rr",		3,	SHIFT,		VERB | Z80 },
	{"rra",		037,	NOOPERAND,	VERB | Z80 },
	{"rrc",		1,	SHIFT,		VERB | I8080 | Z80 },
	{"rrca",	017,	NOOPERAND,	VERB | Z80 },
	{"rrd",		0166547,NOOPERAND,	VERB | Z80 },
	{"rst",		0307,	RST,		VERB | I8080 | Z80 },
	{".rsym",	PSRSYM,	ARGPSEUDO,	VERB },
	{"rz",		0310,	NOOPERAND,	VERB | I8080 },
	{"sbb",		3,	ARITHC,		VERB | I8080 },
	{"sbc",		3,	ARITHC,		VERB | Z80 },
	{"sbi",		0336,	ALUI8,		VERB | I8080 },
	{"scf",		067,	NOOPERAND,	VERB | Z80 },
	{"set",		0145700,BIT,		VERB | Z80 },
	{".setocf",	0,	SETOCF,		VERB },
	{".sett",	0,	TSTATE,		VERB },
	{"shl",		0,	SHL,		TERM },
	{"shld",	0,	SHLD,		VERB | I8080 },
	{"shr",		0,	SHR,		TERM },
	{"sl1",		6,	SHIFT,		VERB | Z80 | UNDOC },
	{"sla",		4,	SHIFT,		VERB | Z80 },
	{"sll",		6,	SHIFT,		VERB | Z80 },
	{"sp",		060,	SP,		I8080 | Z80 },
	{".space",	2,	LIST,		VERB },
	{"sphl",	0371,	NOOPERAND,	VERB | I8080 },
	{"sra",		5,	SHIFT,		VERB | Z80 },
	{"srl",		7,	SHIFT,		VERB | Z80 },
	{"sta",		0,	STA,		VERB | I8080 },
	{"stax",	0,	STAX,		VERB | I8080 },
	{"stc",		067,	NOOPERAND,	VERB | I8080 },
	{"sub",		2,	LOGICAL,	VERB | I8080 | Z80 },
	{".subttl",	SPSBTL,	SPECIAL,	VERB },
	{"sui",		0326,	ALUI8,		VERB | I8080 },
	{"t",		0,	T,		0 },
	{".text",	0,	DEFB,		VERB },
	{"tihi",	0,	TIHI,		0 },
	{"tilo",	0,	TILO,		0 },
	{".title",	SPTITL,	SPECIAL,	VERB },
	{".tstate",	0,	TSTATE,		VERB },
	{"v",		050,	COND,		Z80 },
	{".word",	0,	DEFW,		VERB },
	{".wsym",	PSWSYM,	ARGPSEUDO,	VERB },
	{"xchg",	0353,	NOOPERAND,	VERB | I8080 },
	{"xor",		5,	XOR,		VERB | Z80 | TERM },
	{"xra",		5,	LOGICAL,	VERB | I8080 },
	{"xri",		0356,	ALUI8,		VERB | I8080 },
	{"xthl",	0343,	NOOPERAND,	VERB | I8080 },
	{"z",		010,	SPCOND,		Z80 },
	{".z80",	1,	INSTSET,	VERB },
};

/*
 *  user-defined items are tabulated in the following table.
 */

struct item	itemtab[ITEMTABLESIZE];
struct item	*itemmax = itemtab+ITEMTABLESIZE;





/*
 *  lexical analyser, called by yyparse.
 */
int yylex()
{
	int c;
	char *p;
	int radix;
	int sep;
	char *d0, *dn;
	int exclude, include, overflow;

	if (arg_flag)
		return(getarg());

	if (raw) {
		int skip = 1;
		p = tempbuf;
		while ((c = nextchar()) != '\n' && c) {
			if (p >= tempmax) {
				*p = '\0';
				printf("was parsing '%s'\n", tempbuf);
				error(symlong);
			}
			if (!skip || charclass[c] != SPACE) {
				*p++ = c;
				skip = 0;
			}
		}
		if (c == 0)
			peekc = c;

		*p-- = '\0';

		while (p >= tempbuf && charclass[*p] == SPACE)
			*p-- = '\0';

		raw = 0;

		return RAWTOKEN;
	}

for (;;) switch(charclass[c = nextchar()]) {
	case F_END:
		if (expptr) {
			popsi();
			continue;
		} else return(0);

	case SPACE:
		while (charclass[c = nextchar()] == SPACE)
			;
		peekc = c;
		logcol++;
		break;
	case LETTER:
	case STARTER:
	case DIGIT:
	case DOLLAR:
	spectok:
		firstcol = getcol() == 1;

		radix = -1; // might be a number
		p = tempbuf;
		do {
			if (p >= tempmax) {
				*tempmax = '\0';
				printf("was parsing '%s'\n", tempbuf);
				error(symlong);
			}
			*p = (c >= 'A' && c <= 'Z') ? c + 'a' - 'A' : c;
			if (mras && *p == '?') {
				char *q;

				radix = 0; // can't be a number even if it looks like it

				if (expptr)
					q = getmraslocal();
				else
					for (q = modstr; *q == '@'; q++)
						;

				if (*q) {
					strcpy(p, q);
					p = strchr(p, '\0') - 1;
				}
				else
					*p = '?';
			}
			p++;
			c = nextchar();
		} while	(charclass[c]==LETTER || charclass[c]==DIGIT ||
			charclass[c]==STARTER || charclass[c]==DOLLAR);

		*p = '\0';
		// Special case for AF'
		if (c == '\'' && strcmp(tempbuf, "af") == 0)
			return AFp;

		peekc = c;

		// Pass off '?' (XXX but, technically, should only be done in expression context)
		if (strcmp(tempbuf, "?") == 0)
			return '?';

		// Pass off '$'
		if (strcmp(tempbuf, "$") == 0)
			return '$';

		// Look ahead at what we have.
		while (charclass[c] == SPACE)
			c = nextchar();

		peekc = c;

		//printf("%d %s\n", logcol, tempbuf);
		// If logcol == 0 then if c == ':' we're a label for sure.
		// If logcol == 1 if c == ':' we're a label, change logcol
		//    otherwise we're op or pseudo
		// If logcol == 0 and c == '\n' or ';' then we're alone so
		//	we give tokenization a chance otherwise label
		// If logcol >= 2 we're in the arguments
		//
		// There is quite a lot of unrealziaed scope for error
		// detection and helpful warnings.

		 // Default to any tokenization.
		exclude = 0;
		include = 0;

		if (logcol >= 2) {
			exclude = VERB;
			include = TERM;
		}
		else if (logcol == 0 && c != ';' && c != '\n')
			exclude = VERB;
		else if (logcol == 1 && c == ':') {
			exclude = VERB;
			logcol = 0;
		}

		logcol++;

		// Look for possible numbers.
		// 0x<hex> $<hex> <hex>h <octal>o <octal>q <binary>b
		// <decimal> <decimal>d
		// Suffix formats must start with 0-9.

		if (radix) {
			if (tempbuf[0] == '0' && tempbuf[1] == 'x' && tempbuf[2]) {
				radix = 16;
				d0 = tempbuf + 2;
				dn = p;
			} else if (tempbuf[0] == '$') {
				radix = 16;
				d0 = tempbuf + 1;
				dn = p;
			}
			else if (tempbuf[0] >= '0' && tempbuf[0] <= '9') {
				d0 = tempbuf;
				dn = p - 1;
				switch (*dn) {
				case 'o':
				case 'q':
					radix = 8;
					break;
				case 'd':
					radix = 10;
					break;
				case 'h':
					radix = 16;
					break;
				case 'b':
					radix = 2;
					break;
				default:
					radix = 10;
					dn++;
				}
			}
		}

		// We may have a number on our hands.
		if (radix > 0) {
			overflow = 0;
			yylval.ival = 0;

			for (; d0 < dn; d0++) {
				unsigned int ovchk = (unsigned int)yylval.ival;
				c = *d0 - (*d0 > '9' ? ('a' - 10) : '0');
				if (c < 0 || c >= radix) {
					radix = 0;
					break;
				}
				if (ovchk * radix / radix != ovchk)
					overflow = 1;

				yylval.ival *= radix;
				yylval.ival += c;
			}
		}

		// If we're in the first logical column and the token starts with
		// '$' then we'll force it to be a label even though it could be
		// a $hex constant. This will allow $FCB as a label.
		// Thus we must also allow symbol lookup a chance to override number
		// parsing if we start with a '$'.

		if (tempbuf[0] == '$') {
			if (logcol == 1 || locate(tempbuf)->i_token) {
				if (radix > 0)
					err[warn_hex]++;
				radix = 0;
			}
		}

		if (radix > 0) {
			// Might be line skipping time, though.
			if (*ifptr)
				return skipline(c);

			if (overflow) {
				err[iflag]++;
				yylval.ival = 0;
			}
			return NUMBER;
		}

		// Too late to do '$' concatenation of numbers.  But zmac
		// didn't allow for that previously at any rate.
		if (zcompat) {
			char *q = tempbuf;
			// Normal zmac operation requires we ignore $ in identifiers
			for (p = q; *p; p++)
				if (*p != '$')
					*q++ = *p;

			*q = '\0';
			p = q;
		}

		// GWP - boy, this should be a warning or error
		if (p - tempbuf > MAXSYMBOLSIZE) {
			p = tempbuf + MAXSYMBOLSIZE;
			*p = '\0';
		}

		return tokenofitem(UNDECLARED, exclude, include);

	default:
		if (*ifptr)
			return(skipline(c));

		if (mras && getcol() == 1 && c == '*')
			goto spectok;

		switch(c) {
		int corig;
		case ':':
			if (logcol == 1) {
				// Make sure "label:ret", "label: ret",
				// "label: :ret", "label: : ret" work out OK.
				// But stop fooling around once we've done the VERB
				peekc = nextchar();
				if (charclass[peekc] == SPACE)
					logcol--;
			}
			return c;
		case ';':
			return(skipline(c));
		case '\'':
		case '"':
			sep = c;
			p = tempbuf;
			p[1] = 0;
			do	switch(c = nextchar())	{
			case '\0':
			case '\n':
				err[bflag]++;
				goto retstring;
			default:
				if (c == sep && (c = nextchar()) != sep) {
				retstring:
					peekc = c;
					*p = '\0';
					if ((p-tempbuf) >2) {
						yylval.cval = tempbuf;
						return(STRING);
					} else if (p-tempbuf == 2)	{
						p = tempbuf;
						yylval.ival = *p++ ;
						yylval.ival |= *p<<8;
						return(TWOCHAR);
					} else	{
						p = tempbuf;
						yylval.ival = *p++;
						return(ONECHAR);
					}
				}
				*p++ = c;
			} while (p < tempmax);
			/*
			 *  if we break out here, our string is longer than
			 *  our input line
			 */
			error("string buffer overflow");
		case '<':
			corig = c;
			switch (c = nextchar ()) {
			case '=':
				return LE;
			case '<':
				return SHL;
			case '>':
				return NE;
			default:
				peekc = c;
				return corig;
			}
			/* break; suppress "unreachable" warning for tcc */
		case '>':
			corig = c;
			switch (c = nextchar ()) {
			case '=':
				return GE;
			case '>':
				return SHR;
			default:
				peekc = c;
				return corig;
			}
			/* break; suppress "unreachable" warning for tcc */
		case '!':
			corig = c;
			switch (c = nextchar ()) {
			case '=':
				return NE;
			default:
				peekc = c;
				return corig;
			}
			/* break; suppress "unreachable" warning for tcc */
		case '=':
			corig = c;
			switch (c = nextchar ()) {
			case '=':
				return '=';
			default:
				peekc = c;
				return corig;
			}
			/* break; suppress "unreachable" warning for tcc */

		case '&':
			corig = c;
			if ((c = nextchar()) == '&')
				return ANDAND;
			else {
				peekc = c;
				return corig;
			}
			/* break; suppress "unreachable" warning for tcc */
		case '|':
			corig = c;
			if ((c = nextchar()) == '|')
				return OROR;
			else {
				peekc = c;
				return corig;
			}
			/* break; suppress "unreachable" warning for tcc */
		default:
			return(c);
		}
	}
}

// Verify keytab is in alphabetical order.
int check_keytab()
{
	int i;
	char *prev;

	for (i = 0; i < sizeof(keytab) / sizeof(keytab[0]); i++) {
		char *next = keytab[i].i_string;
		next += *next == '.';
		if (i != 0) {
			if (strcmp(prev, next) >= 0) {
				printf("keytab error: %s >= %s\n", prev, next);
				return 0;
			}
		}
		prev = next;
	}

	printf("keytab OK\n");

	return 1;
}


struct item *keyword(char *name)
{
	int  r, l, u;
	struct item *ip;

	/*
	 *  binary search
	 */
	l = 0;
	u = (sizeof keytab/sizeof keytab[0])-1;
	while (l <= u) {
		char *key;
		i = (l+u)/2;
		ip = &keytab[i];
		key = ip->i_string;
		r = strcmp(name + (name[0] == '.'), key + (key[0] == '.'));
		if (r == 0) {
			// Do not allow ".foo" to match "foo"
			if (name[0] == '.' && key[0] != '.')
				break;

			return ip;
		}
		if (r < 0)
			u = i-1;
		else
			l = i+1;
	}

	return 0;
}

struct item *locate(char *name)
{
	struct item *ip;
	/*
	 *  hash into item table
	 */
	int hash = 0;
	char *p = name;
	while (*p) hash += *p++;
	hash %= ITEMTABLESIZE;
	ip = &itemtab[hash];

	for (;;) {
		if (ip->i_token == 0)
			break;
		if (strcmp(name, ip->i_string) == 0)
			break;
		if (++ip >= itemmax)
			ip = itemtab;
	}

	return ip;
}

/*
 *  return the token associated with the string pointed to by
 *  tempbuf.  if no token is associated with the string, associate
 *  deftoken with the string and return deftoken.
 *  in either case, cause yylval to point to the relevant
 *  symbol table entry.
 *
 *  Only keys not matching the keyexclude will be returned allowing
 *  context-dependent tokenization.  Unless they match keyinclude.
 */

int tokenofitem(int deftoken, int keyexclude, int keyinclude)
{
	char *p;
	struct item *ip;
	int  i;

#ifdef T_DEBUG
	fputs("'tokenofitem entry'	", stderr) ;
	fputs(tempbuf, stderr) ;
#endif

	ip = keyword(tempbuf);
	if (ip) {
		if (ip->i_uses & keyinclude)
			goto found;

		if (!(ip->i_uses & keyexclude))
			goto found;
	}

	// This is really my own thing rather than old zmac, but zmac
	// didn't support it and it does depend on '$' crushing a bit.
	if (zcompat) {
	    // '_' prefixed labels are local to the file
	    if (tempbuf[0] == '_') {
		    strcat(tempbuf, "$");
		    strcat(tempbuf, basename(src_name[now_in]));
	    }

	    // '.' prefixed labels are local between labels
	    if (tempbuf[0] == '.') {
		    char *p = tempbuf;
		    while (*p) p++;
		    sprintf(p, "$%d", llseq);
	    }
	}

	ip = locate(tempbuf);

	if (ip->i_token)
		goto found;

	if (!deftoken) {
		i = 0 ;
		goto token_done ;
	}
	if (++nitems > ITEMTABLESIZE-20)
		error("item table overflow");
	ip->i_string = malloc(strlen(tempbuf)+1);
	ip->i_token = deftoken;
	ip->i_uses = 0;
	strcpy(ip->i_string, tempbuf);

found:
	if (*ifptr) {
		if (ip->i_token == ENDIF_TK) {
			i = ENDIF_TK;
			goto token_done ;
		}
		if (ip->i_token == ELSE_TK) {
			/* We must only honour the ELSE if it is not
			   in a nested failed IF/ELSE */
			char forbid = 0;
			char *ifstackptr;
			for (ifstackptr = ifstack; ifstackptr != ifptr; ++ifstackptr) {
				if (*ifstackptr) {
					forbid = 1;
					break;
				}
			}
			if (!forbid) {
				i = ELSE_TK;
				goto token_done;
			}
		}
		if (ip->i_token == IF_TK) {
			if (ifptr >= ifstmax)
				error("Too many ifs");
			else *++ifptr = 1;
		}
		i = skipline(' ');
		goto token_done ;
	}
	yylval.itemptr = ip;
	i = ip->i_token;
token_done:
#ifdef T_DEBUG
	fputs("\t'tokenofitem exit'\n", stderr) ;
#endif
	return(i) ;
}


/*
 *  interchange two entries in the item table -- used by custom_qsort
 */
void interchange(int i, int j)
{
	struct item *fp, *tp;
	struct item temp;

	fp = &itemtab[i];
	tp = &itemtab[j];
	temp.i_string = fp->i_string;
	temp.i_value = fp->i_value;
	temp.i_token = fp->i_token;
	temp.i_uses = fp->i_uses;
	temp.i_scope = fp->i_scope;
	temp.i_chain = fp->i_chain;

	fp->i_string = tp->i_string;
	fp->i_value = tp->i_value;
	fp->i_token = tp->i_token;
	fp->i_uses = tp->i_uses;
	fp->i_scope = tp->i_scope;
	fp->i_chain = tp->i_chain;

	tp->i_string = temp.i_string;
	tp->i_value = temp.i_value;
	tp->i_token = temp.i_token;
	tp->i_uses = temp.i_uses;
	tp->i_scope = temp.i_scope;
	tp->i_chain = temp.i_chain;
}



/*
 *  quick sort -- used by putsymtab to sort the symbol table
 */
void custom_qsort(int m, int n)
{
	int  i, j;

	if (m < n) {
		i = m;
		j = n+1;
		for (;;) {
			do i++; while(strcmp(itemtab[i].i_string,
					itemtab[m].i_string) < 0);
			do j--; while(strcmp(itemtab[j].i_string,
					itemtab[m].i_string) > 0);
			if (i < j) interchange(i, j); else break;
		}
		interchange(m, j);
		custom_qsort(m, j-1);
		custom_qsort(j+1, n);
	}
}

int getcol()
{
	return inpptr - inpbuf;
}

/*
 *  get the next character
 */
int nextchar()
{
	int c, ch;
	unsigned char *p;
	char *getlocal();

	if (peekc != -1) {
		c = peekc;
		peekc = -1;
		return c;
	}

	if (inpptr) {
		// Double nul indicates EOF for macros
		if (expptr && inpptr[0] == '\0' && inpptr[1] == '\0') {
			inpptr = 0;
			return 0;
		}

		if (!expptr && getcol() == 0) {
			void analyze_inpbuf(void);
			linein[now_in]++;
			analyze_inpbuf();
		}

		c = *inpptr++;

		addtoline(c);

		if (*inpptr == '\0')
			inpptr = 0;

		return c;
	}

	inpptr = inpbuf;
	logcol = 0;
	p = inpbuf;

	// XXX - should check for input line overflow!

	// If invoking a macro then pull the next line from it.
	if (expptr) {
		for (;;) {
			ch = getm();

			if (ch == '\1') { /* expand argument */
				ch = getm() - 'A';
				if (ch >= 0 && ch < PARMMAX && est[ch].param) {
					strcpy(p, est[ch].param);
					p = strchr(p, '\0');
				}
			}
			else if (ch == '\2') {	/*  local symbol  */
				ch = getm() - 'A';
				if (ch >= 0 && ch < PARMMAX && est[ch].param)
					strcpy(p, est[ch].param);
				else
					strcpy(p, getlocal(ch, est[TEMPNUM].value));

				p = strchr(p, '\0');
			}
			else {
				if (ch == 0)
					break;

				*p++ = ch;

				if (ch == '\n')
					break;
			}
		}
		*p = '\0';
		p[1] = ch;
	}
	else {
		if (nextline_peek != -1) {
			*p++ = nextline_peek;
			nextline_peek = -1;
		}
		for (;;) {
			ch = getc(now_file);

			if (ch == '\r') {
				nextline_peek = getc(now_file);
				if (nextline_peek == '\n')
					nextline_peek = -1;
				else if (nextline_peek == EOF) {
					*p++ = '\n';
					nextline_peek = -1;
					ch = EOF;
					break;
				}
				ch = '\n';
			}

			if (ch == EOF)
				break;

			*p++ = ch;

			if (ch == '\n') 
				break;
		}

		*p = '\0';

		/* if EOF, check for include file */
		if (ch == EOF) {
			if (now_in) {
				fclose(fin[now_in]) ;
				free(src_name[now_in]);
				now_file = fin[--now_in];
			}
			else if (p == inpbuf)
				return 0;
	
			if (linein[now_in] < 0) {
				lstoff = 1;
				linein[now_in] = -linein[now_in];
			} else {
				lstoff = 0 ;
			}

			if (outpass) {
				if (iflist()) {
					lineout();
					fprintf(fout, "**** %s ****\n", src_name[now_in]) ;
				}
				if (bopt)
					fprintf(fbds, "%04x %04x f %s\n", dollarsign, emit_addr, src_name[now_in]);
			}

			if (p != inpbuf) {
				*p++='\n';
				*p = '\0';
			}
			else
				inpptr = 0;
		}
	}

	return nextchar();
}

char *skipspace(char *p)
{
	while (charclass[*p] == SPACE)
		p++;

	return p;
}

// Look at inpbuf and try to determine what logical column we are starting
// at.  We could put all of the work in here and keep yylex simple but for
// now we share the load.

void analyze_inpbuf(void)
{
	int cc;
	char *p, *q;
	struct item *ip;

	// Default if we find nothing to override
	logcol = 0;

	// We'll only worry about one case for now.  When you start with
	// whitespace yet there are 3 columns.  If so then we change logcol
	// to -1 to compensate.  If the 2nd column is a VERB.

	// Start by recognizing space and skipping or aborting if not found
	p = inpbuf;
	if (charclass[*p] != SPACE)
		return;

	p = skipspace(p);

	// Now skip over a token or abort if we don't find one

	cc = charclass[*p];
	if (cc != LETTER && cc != STARTER && cc != DIGIT && cc != DOLLAR)
		return;

	for (;;) {
		cc = charclass[*p];
		if (cc == LETTER || cc == STARTER || cc == DIGIT || cc == DOLLAR)
			p++;
		else
			break;
	}

	// We could skip space-separated colons now, but if we see a colon
	// the issue has been decided to do that because it is easier.
	if (*p == ':')
		return;

	p = skipspace(p);

	// Another token to skip past.
	// But we need to examine it to see if it is a verb.

	cc = charclass[*p];
	if (cc != LETTER && cc != STARTER && cc != DIGIT && cc != DOLLAR)
		return;

	q = p;
	for (;;) {
		cc = charclass[*p];
		if (cc == LETTER || cc == STARTER || cc == DIGIT || cc == DOLLAR)
			p++;
		else
			break;
	}

	// Must have space to skip over
	if (charclass[*p] != SPACE)
		return;

	// This 2nd token must be a verb.
	cc = *p;
	*p = '\0';
	ip = keyword(q);
	*p = cc;
	if (!ip || !(ip->i_uses & VERB))
		return;

	// Now skip over space.  If there's anything but a comment or end
	// of the line then we've may have 3 logical columns.
	// "ld a, 5" can throw that off, but we've done the verb check.

	p = skipspace(p);

	if (*p != ';' && *p != '\n' && *p != '\0')
		logcol--;
}


/*
 *  skip to rest of the line -- comments and if skipped lines
 */
int skipline(int ac)
{
	int  c;

	c = ac;
	while (c != '\n' && c != '\0')
		c = nextchar();
	return('\n');
}

void add_incpath(char *dir)
{
	char *p;

	if (incpath_cnt >= MAXINCPATH) {
		fprintf(stderr, "Sorry, can only handle %d include paths\n", MAXINCPATH);
		exit(1);
	}

	p = malloc(strlen(dir) + 1);
	strcpy(p, dir);

	incpath[incpath_cnt++] = dir;
}

FILE *open_incpath(char *filename, char *mode)
{
	char quote;
	int i;
	char path[1024];
	FILE *fp;

	// Due to the way parsing works the string can be specified
	// without quotes or will allow quotes but include them.  Instead
	// of fooling with the parsing I just strip the quotes.  I think
	// you can still include a file that starts with a single or double
	// quote by quoting it, but that's an awful thing to do to yourself.

	quote = *filename;
	if (quote == '"' || quote == '\'') {
		strcpy(filename, filename + 1);
		if (strrchr(filename, quote))
			*strrchr(filename, quote) = '\0';
	}

	// First look for included file in same directory as source file.

	strcpy(path, src_name[now_in]);
	*basename(path) = '\0';
	strcat(path, filename);
	fp = fopen(path, mode);
	if (fp) {
		if (note_depend && outpass)
			printf("%s\n", path);
		return fp;
	}

	for (i = 0; i < incpath_cnt; i++) {
		sprintf(path, "%s/%s", incpath[i], filename);
		fp = fopen(path, mode);
		if (fp) {
			if (note_depend && outpass)
				printf("%s\n", path);
			return fp;
		}
	}

	if (note_depend && outpass)
		printf("%s\n", filename);

	return fopen(filename, mode);
}

void version()
{
	fprintf(stderr, "zmac version " VERSION "\n");
}

//
// Print out a usage message and exit.
//
void usage(char *msg, char *param)
{
	fprintf(stderr, msg, param);
	fprintf(stderr, "\n");
	version();
	fprintf(stderr, "usage: zmac [-8bcefghijJlLmnopstz] [-I dir] file[.z]\n");
	fprintf(stderr, "other opts: --rel --mras --zmac --dep --help --doc --version\n");
	fprintf(stderr, "  zmac -h for more detail about options.\n");
	exit(1);
}

void help()
{
	version();

	fprintf(stderr, "\t--version show version number\n");
	fprintf(stderr, "\t--help\tshow this help message\n");
	fprintf(stderr, "\t-8\tuse 8080 interpretation of mnemonics\n");
	fprintf(stderr, "\t-b\tno binary (.hex,.cmd,.cas, etc.) output\n");
	fprintf(stderr, "\t-c\tno cycle counts in listing\n");
	fprintf(stderr, "\t-e\terror list only\n");
	fprintf(stderr, "\t-f\tprint if skipped lines\n");
	fprintf(stderr, "\t-g\tdo not list extra code\n");
	fprintf(stderr, "\t-h\tshow this information about options and quit\n");
	fprintf(stderr, "\t-i\tdo not list include files\n");
	fprintf(stderr, "\t-I dir\tadd 'dir' to include file search path\n");
	fprintf(stderr, "\t-j\tpromote relative jumps to absolute as needed\n");
	fprintf(stderr, "\t-J\twarn when a jump could be relative\n");
	fprintf(stderr, "\t-l\tno list\n");
	fprintf(stderr, "\t-L\tforce listing of everything\n");
	fprintf(stderr, "\t-m\tprint macro expansions\n");
	fprintf(stderr, "\t-n\tput line numbers off\n");
	fprintf(stderr, "\t-o\tlist to standard output\n");
	fprintf(stderr, "\t-p\tput out four \\n's for eject\n");
	fprintf(stderr, "\t-s\tdon't produce a symbol list\n");
	fprintf(stderr, "\t-t\toutput error count instead of list of errors\n");
	fprintf(stderr, "\t-z\tuse Z-80 interpretation of mnemonics\n");
	fprintf(stderr, "\t--dep\tlist files included\n");
	fprintf(stderr, "\t--mras\tlimited MRAS/EDAS compatibility\n");
	fprintf(stderr, "\t--rel\toutput .rel file only\n");
	fprintf(stderr, "\t--zmac\tcompatibility with original zmac\n");
	fprintf(stderr, "\t--doc\toutput documentation as HTML file\n");

	exit(0);
}

int main(int argc, char *argv[])
{
	struct item *ip;
	int  i;
	int  files;
#ifdef DBUG
	extern  yydebug;
#endif

	fout = stdout ;
	fin[0] = stdin ;
	now_file = stdin ;
	files = 0;

	// Special flag for unit testing.
	if (argc > 1 && strcmp(argv[1], "--test") == 0)
		exit(!check_keytab());

	for (i=1; i<argc; i++) {
		int skip = 0;
		if (strcmp(argv[i], "--mras") == 0) {
			mras = 1;
			continue;
		}

		if (strcmp(argv[i], "--rel") == 0) {
			relopt = 1;
			bopt = 0;
			continue;
		}

		if (strcmp(argv[i], "--zmac") == 0) {
			zcompat = 1;
			continue;
		}

		if (strcmp(argv[i], "--dep") == 0) {
			note_depend = 1;
			continue;
		}

		if (strcmp(argv[i], "--help") == 0) {
			help();
			continue;
		}

		if (strcmp(argv[i], "--doc") == 0) {
			extern void doc(void);
			doc();
			exit(0);
			continue; // not reached
		}

		if (strcmp(argv[i], "--version") == 0) {
			version();
			exit(0);
			continue; // not reached
		}

		if (*argv[i] == '-') while (*++argv[i]) {
			switch(*argv[i]) {

			case '8':	/* Equivalent to .8080 */
				default_z80 = 0;
				continue;

			case 'b':	/*  no binary  */
				bopt = 0;
				continue;

			case 'c':	/*  no cycle counts in listing */
				copt-- ;
				continue;

#ifdef DBUG
			case 'd':	/*  debug  */
				yydebug++;
				continue;
#endif

			case 'e':	/*  error list only  */
				eopt = 0;
				edef = 0;
				continue;

			case 'f':	/*  print if skipped lines  */
				fopt++;
				fdef++;
				continue;

			case 'g':	/*  do not list extra code  */
				gopt = 0;
				gdef = 0;
				continue;

			case 'h':
				help();
				continue;

			case 'i':	/* do not list include files */
				iopt = 1 ;
				continue ;

			case 'I':
				if (argv[i][1])
					add_incpath(argv[i] + 1);
				else {
					i++;
					if (i < argc)
						add_incpath(argv[i]);
					else
						usage("missing argument to -I option", 0);
				}
				skip = 1;
				break;

			case 'l':	/*  no list  */
				lopt++;
				continue;

			case 'L':	/*  force listing of everything */
				lston++;
				continue;

			case 'j':	// promote relative jumps to absolute as needed
				default_jopt = 1;
				continue;

			case 'J':	// error when JR instructions could replace JP
				default_JPopt = 1;
				continue;

			case 'm':	/*  print macro expansions  */
				mdef++;
				mopt++;
				continue;

			case 'n':	/*  put line numbers off */
				nopt-- ;
				continue;

			case 'o':	/*  list to standard output  */
				oopt++;
				continue;

			case 'p':	/*  put out four \n's for eject */
				popt-- ;
				continue;

			case 'P':	// GWP - printer style output (headers, page separation, etc.)
				printer_output = 1;
				continue;

			case 's':	/*  don't produce a symbol list  */
				sopt++;
				continue;

			case 't':	/*  output only number of errors */
				topt = 0;
				continue;

			case 'z':	/* Equivalent to .z80 */
				default_z80 = 1;
				continue;

			default:	/*  error  */
				usage("Unknown option", 0);

			}
			if (skip)
				break;
		}
		else if (files++ == 0) {
			sourcef = argv[i];
			strcpy(src, sourcef);
			if ((now_file = fopen(src, "r")) == NULL) {
				if (!*getsuffix(src))
					suffix(src, ".z");
				if ((now_file = fopen(src, "r")) == NULL)
					usage("Cannot open source file '%s'", src);
			}
			now_in = 0;
			fin[now_in] = now_file ;
			src_name[now_in] = src ;
		} else if (files)
			usage("Too many arguments", 0);
	}


	if (files == 0)
		usage("No source file", 0);

	{
		char outdir[1025];
		outpath(outdir, sourcef, 0);
#ifdef WIN32
		_mkdir(outdir);
#else
		mkdir(outdir, 0777);
#endif
	}

	if (bopt) {
		outpath(bds, sourcef, ".bds");
		fbds = fopen(bds, "w");
		if (fbds == NULL)
			error("Cannot create .bds file");

		fprintf(fbds, "binary-debuggable-source\n");

		outpath(oth, sourcef, ".cmd");
		fcmd = fopen(oth, "wb");
		if (fcmd == NULL)
			error("Cannot create .cmd file");

		outpath(oth, sourcef, ".cas");
		fcas = fopen(oth, "wb");
		if (fcas == NULL)
			error("Cannot create .cas file");

		outpath(oth, sourcef, ".lcas");
		flcas = fopen(oth, "wb");
		if (flcas == NULL)
			error("Cannot create .lcas file");

		// Tape header
		for (i = 0; i < 255; i++) {
			fputc(0, flcas);
			fputc(0x55, fcas);
		}
		fputc(0xA5, flcas);
		fputc(0x7F, fcas);
		casname(oth, sourcef);
		putcas(0x55);
		for (i = 0; i < 6; i++)
			putcas(oth[i]);

		outpath(oth, sourcef, ".cim");
		fcim = fopen(oth, "wb");
		if (fcim == NULL)
			error("Cannot create .cim file");

		outpath(oth, sourcef, ".ams");
		fams = fopen(oth, "wb");
		if (fams == NULL)
			error("Cannot create .ams file");

		outpath(bin, sourcef, ".hex");
#ifdef MSDOS
		if (( fbuf = fopen(bin, "wb")) == NULL)
#else
		if (( fbuf = fopen(bin, "w")) == NULL)
#endif
			error("Cannot create .hex file");
	}
	else if (relopt) {
		outpath(oth, sourcef, ".rel");
		frel = fopen(oth, "wb");
		if (frel == NULL)
			error("Cannot create .rel file");

		strncpy(progname, basename(sourcef), sizeof progname);
		progname[sizeof progname - 1] = '\0';
	}
	if (!lopt && !oopt) {
		outpath(listf, sourcef, ".lst");
		if ((fout = fopen(listf, "w")) == NULL)
			error("Cannot create list file");
	} else
		fout = stdout ;
	outpath(mtmp, sourcef, ".tmp");
#ifdef MSDOS
	mfile = mfopen(mtmp,"w+b") ;
#else
	mfile = mfopen(mtmp,"w+") ;
#endif
	if (mfile == NULL) {
		error("Cannot create temp file");
	}
	/*unlink(mtmp);*/

	/*
	 *  get the time
	 */
	time(&now);
	timp = ctime(&now);
	timp[16] = 0;
	timp[24] = 0;

	title = sourcef;
	/*
	 * pass 1
	 */
#ifdef DEBUG
	fputs("DEBUG-pass 1\n", stderr) ;
#endif
	clear();
	setvars();
	outpass = 0;
	yyparse();

	// GWP - errors should stop us, but the listing is very useful.

	pass2++;

	for (npass = 2; npass < MAXPASS; npass++) {
		if (passfail || npass == MAXPASS - 1)
			outpass = 1;

		if (outpass) {
			putrelcmd(RELCMD_PROGNAME);
			putrelname(progname);
		}

		ip = &itemtab[-1];
		while (++ip < itemmax) {
			// Output list of public labels.  m80 will let
			// equates and aseg values be public so we do, too.
			if (outpass && ip->i_token && (ip->i_scope & SCOPE_PUBLIC)) {
				putrelcmd(RELCMD_PUBLIC);
				putrelname(ip->i_string);
			}

			/* reset use count */
			ip->i_uses = 0 ;

			/* set macro names, equated and defined names */
			switch	(ip->i_token) {
			case MNAME:
				ip->i_token = OLDMNAME;
				break;

			case EQUATED:
				ip->i_token = WASEQUATED;
				break;

			case DEFLED:
				if (zcompat)
					ip->i_token = UNDECLARED;
				break;
			}
		}

		if (outpass) {
			// m80 outputs data size as an absolute value, but
			// code size as code segment relative.  Odd, but
			// I'll follow suit.
			putrelcmd(RELCMD_DATASIZE);
			putrelsegref(SEG_ABS, seg_size[SEG_DATA]);

			putrelcmd(RELCMD_CODESIZE);
			putrelsegref(SEG_CODE, seg_size[SEG_CODE]);
		}

		// In case we hit 'end' inside an included file
		while (now_in > 0) {
			fclose(fin[now_in]);
			free(src_name[now_in]);
			now_file = fin[--now_in];
		}
		setvars();
		fseek(now_file, (long)0, 0);

	#ifdef DEBUG
		fprintf(stderr, "DEBUG- pass %d\n", npass) ;
	#endif

		yyparse();

		if (outpass || passfail)
			break;

		if (!passretry)
			outpass = 1;
	}

	if (bopt) {
		flushbin();
		flushoth();
		putc(':', fbuf);
		if (xeq_flag) {
			puthex(0, fbuf);
			puthex(xeq >> 8, fbuf);
			puthex(xeq, fbuf);
			puthex(1, fbuf);
			puthex(255-(xeq >> 8)-xeq, fbuf);
			fprintf(fcmd, "%c%c%c%c", 2, 2, xeq, xeq >> 8);
			fflush(fcmd);
			putcas(0x78);
			putcas(xeq);
			putcas(xeq >> 8);
		} else
			for	(i = 0; i < 10; i++)
				putc('0', fbuf);
		putc('\n', fbuf);
		fflush(fbuf);
		// "Play Cas" seems to require trailing zeros to work
		// properly.  And we need to output at least one zero byte
		// to flush out the final high speed bits.
		for (i = 0; i < 6; i++)
			putcas(0);
	}

	if (relopt) {
		struct item *ip;
		// Output external symbols and value of public symbols
		for (ip = itemtab; ip < itemmax; ip++) {
			if (ip->i_token == UNDECLARED && (ip->i_scope && SCOPE_EXTERNAL)) {
				putrelcmd(RELCMD_EXTCHAIN);
				// Chain value will have top two bits set appropriately
				putrelextaddr(ip->i_chain);
				putrelname(ip->i_string);
			}
			if (ip->i_scope & SCOPE_PUBLIC)
			{
				putrelcmd(RELCMD_PUBVALUE);
				putrelsegref(ip->i_scope, ip->i_value);
				putrelname(ip->i_string);
			}
		}

		// End module, entry address if any
		putrelcmd(RELCMD_ENDMOD);
		putrelextaddr(rel_main);
		flushrel(); // byte alignment expected after end module

		// End .rel file
		putrelcmd(RELCMD_ENDPROG);
		flushrel();
	}

	if (xeq_flag == 0) {
#if WIN32
		CONSOLE_SCREEN_BUFFER_INFO inf;
		HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
		GetConsoleScreenBufferInfo(hOut, &inf);
		SetConsoleTextAttribute(hOut, FOREGROUND_RED|FOREGROUND_GREEN|FOREGROUND_INTENSITY);
#endif
		fprintf(stderr, "Warning: no entry address (forgot \"end label\")\n");
		fflush(stderr);
#if WIN32
		SetConsoleTextAttribute(hOut, inf.wAttributes);
#endif
	}
	else if (bopt) {
		fprintf(fbds, "%04x e\n", xeq);
	}

	if (bopt) {
		int low = 0;
		int high = sizeof(memory) - 1;
		int chk;
		int filelen;
		char leafname[] = "FILENAMEBIN";

		while (low < sizeof(memory) && (memflag[low] & (MEM_INST | MEM_DATA)) == 0)
			low++;

		while (high >= 0 && (memflag[high] & (MEM_INST | MEM_DATA)) == 0)
			high--;

		if (high >= low)
			fwrite(memory + low, high + 1 - low, 1, fcim);

		// AMSDOS binary file output (A for Amstrad, code from zmac 1.3)
		filelen = (high + 1) - low;

		chk = 0;
		putc(0, fams);
		for (i = 0; i < 11; i++) {
			putc(leafname[i], fams);
			chk += leafname[i];
		}
		for (i = 0; i < 6; i++)
			putc(0, fams);

		putc(2, fams); // Unprotected binary
		chk += 2;
		putc(0, fams);
		putc(0, fams);
		putc(low & 0xff, fams);
		chk += low & 0xff;
		putc(low >> 8, fams);
		chk += low >> 8;
		putc(0, fams);
		putc(filelen & 0xff, fams);
		chk += filelen & 0xff;
		putc(filelen >> 8, fams);
		chk += filelen >> 8;
		putc(xeq & 0xff, fams);
		chk += xeq & 0xff;
		putc(xeq >> 8, fams);
		chk += xeq >> 8;
		for (i = 28; i < 64; i++)
			putc(0, fams);

		putc(filelen & 0xff, fams);
		chk += filelen & 0xff;
		putc(filelen >> 8, fams);
		chk += filelen >> 8;
		putc(0, fams); // this would be used if filelen > 64K
		putc(chk & 0xff, fams);
		putc(chk >> 8, fams);

		for (i = 69; i < 128; i++)
			putc(0, fams);

		if (filelen > 0)
			fwrite(memory + low, filelen, 1, fams);

		if (filelen & 0x7f)
			putc(0x1a, fams); // CP/M EOF character
	}

	if (bopt) {
		struct item *tp;

		for (tp = itemtab; tp < itemmax; tp++) {
			if (tp->i_token == LABEL)
				fprintf(fbds, "%04x a %s\n", tp->i_value, tp->i_string);
		}
	}

	if (!lopt)
		fflush(fout);
	if (writesyms)
		outsymtab(writesyms);
	if (eopt)
		erreport();
	if (!lopt && !sopt)
		putsymtab();
	if (!lopt) {
		eject();
		fflush(fout);
	}
	// GWP - some things (like balance errors in macro definitions) do
	// not show up until you use them.  So just in case we print an error
	// count here as not to confuse the programmer who is unlikely to check
	// the listing for errors if none are shown in the command window.
	if (counterr() > 0)
		fprintf(stderr, "%d errors (see listing if no diagnostics appeared here)\n", counterr());
	if (countwarn() > 0)
		fprintf(stderr, "%d warnings (see listing if no diagnostics appeared here)\n", countwarn());
	exit(counterr() > 0);
}


/*
 *  set some data values before each pass
 */
void setvars()
{
	int  i;

	peekc = -1;
	inpptr = 0;
	nextline_peek = -1;
	raw = 0;
	linein[now_in] = linecnt = 0;
	exp_number = 0;
	emitptr = emitbuf;
	lineptr = linebuf;
	ifptr = ifstack;
	expifp = expif;
	*ifptr = 0;
	dollarsign = 0;
	emit_addr = 0;
	olddollar = 0;
	oldothdollar = 0;
	phaseflag = 0;
	for (i=0; i<FLAGS; i++) err[i] = 0;
	tstates = 0;
	ocf = 0;
	llseq = 0;
	passfail = 0;
	passretry = 0;
	njrpromo = 0;
	jopt = default_jopt;
	JPopt = default_JPopt;
	strcpy(modstr, "@@@@");
	segment = SEG_CODE;
	memset(seg_pos, 0, sizeof(seg_pos));
	memset(seg_size, 0, sizeof(seg_size));
	segchange = 0;
	z80 = default_z80;
}

//
// Clear out cycle counts and memory.
//

void clear()
{
	int i;

	for (i = 0; i < sizeof(memory) / sizeof(memory[0]); i++)
	{
		memory[i] = 0;
		memflag[i] = 0;
		tstatesum[i] = 0;
	}
}

void setmem(int addr, int value, int type)
{
	value &= 0xff;
	if (memory[addr] != value) {
		if (outpass) {
			if (!passfail)
				err[pflag]++;
		}
		else
			passretry = 1;
	}

	memory[addr] = value;
	memflag[addr] |= type;
}

/*
 *  print out an error message and die
 */
void error(char *as)
{
	*linemax = 0;
	fprintf(fout, "%s\n", linebuf);
	fflush(fout);
	fprintf(stderr, "%s\n", as) ;
	exit(1);
}


/*
 *  output the symbol table
 */
void putsymtab()
{
	struct item *tp, *fp;
	int  i, j, k, t, rows;
	char c, c1, seg = ' ';
	int numcol = printer_output ? 4 : 1;

	if (!nitems)
		return;

	/* compact the table so unused and UNDECLARED entries are removed */
	tp = &itemtab[-1];
	for (fp = itemtab; fp<itemmax; fp++) {
		if (fp->i_token == UNDECLARED && !(fp->i_scope && SCOPE_EXTERNAL)) {
			nitems--;
			continue;
		}
		if (fp->i_token == 0)
			continue;
		tp++;
		if (tp != fp) {
			tp->i_string = fp->i_string;
			tp->i_value = fp->i_value;
			tp->i_token = fp->i_token;
			tp->i_uses = fp->i_uses;
			tp->i_scope = fp->i_scope;
			tp->i_chain = fp->i_chain;
		}
	}

	tp++;
	tp->i_string = "{";	/* } */

	/*  sort the table */
	custom_qsort(0, nitems-1);

	title = "**  Symbol Table  **";

	rows = (nitems+numcol-1) / numcol;
	if (rows+5+line > 60)
		eject();
	lineout();
	fprintf(fout,"\n\n\nSymbol Table:\n\n") ;
	line += 4;

	for (i=0; i<rows; i++) {
		for(j=0; j<numcol; j++) {
			k = rows*j+i;
			if (k < nitems) {
				tp = &itemtab[k];
				t = tp->i_token;
				c = ' ' ;
				if (t == EQUATED || t == DEFLED)
					c = '=' ;
				if (tp->i_uses == 0)
					c1 = '+' ;
				else
					c1 = ' ' ;

				// GWP - decided I don't care about uses
				// even if it were accurate.
				// TODO: Should use maxsymbol size in there,
				// but makes output harder to read.

				fprintf(fout, "%-15s%c", tp->i_string, c);

				if (relopt)
					seg = " '\"!"[tp->i_scope & SCOPE_SEGMASK];

				if (tp->i_value >> 16)
					fprintf(fout, "%8x%c", tp->i_value, seg);
				else
					fprintf(fout, "%4x%c    ", tp->i_value & 0xffff, seg);

				if (tp->i_scope & SCOPE_EXTERNAL)
					fprintf(fout, " (extern)");

				if (tp->i_scope & SCOPE_PUBLIC)
					fprintf(fout, " (public)");
			}
		}
		lineout();
		putc('\n', fout);
	}
}




/*
 *  put out error report
 */
void erreport()
{
	int i, numerr, numwarn;

	if (line > 49) eject();
	lineout();
	numerr = 0;
	for (i=0; i<FIRSTWARN; i++) numerr += keeperr[i];
	numwarn = 0;
	for (i = FIRSTWARN; i < FLAGS; i++) numwarn += keeperr[i];
	if (numerr || numwarn) {
		fputs("\n\n\nError + Warning report:\n\n", fout);
		fprintf(fout, "%6d errors\n", numerr);
		fprintf(fout, "%6d warnings\n", numwarn);
		line += 6;
	} else {
		fputs("\n\n\nStatistics:\n", fout);
		line += 3;
	}

	for (i=0; i<FLAGS; i++)
		if (keeperr[i]) {
			lineout();
			fprintf(fout, "%6d %c -- %s %s\n",
				keeperr[i], errlet[i], errname[i],
				i < FIRSTWARN ? "error" : "warnings");
		}

	if (line > 52) eject();
	lineout();
	fprintf(fout, "\n%6d\tpasses\n", npass);
	fprintf(fout, "%6d\tjr promotions\n", njrpromo);
	fprintf(fout, "%6d\tsymbols\n", nitems);
	fprintf(fout, "%6d\tbytes\n", nbytes);
	line += 4;
	if (mfptr) {
		if (line > 53) eject();
		lineout();
		fprintf(fout, "\n%6d\tmacro calls\n", exp_number);
		fprintf(fout, "%6d\tmacro bytes\n", mfptr);
		fprintf(fout, "%6d\tinvented symbols\n", invented/2);
		line += 3;
	}
}

/*
 * count errors (GWP - added to set exit code)
 */
int counterr()
{
	int i, numerr = 0;
	for (i=0; i<FIRSTWARN; i++) numerr += keeperr[i];
	return numerr;
}

// Count warnings
int countwarn()
{
	int i, numwarn = 0;
	for (i = FIRSTWARN; i < FLAGS; i++)
		numwarn += keeperr[i];
	return numwarn;
}

char *mlook;

int nextmac()
{
	int ch;

	if (mlook) {
		if (*mlook)
			ch = *mlook++;
		else
			mlook = 0;
	}

	if (!mlook)
		ch = nextchar();

	return ch;
}

/*
 *  lexical analyser for macro definition
 */
void mlex(char *look)
{
	char  *p;
	int  c;
	int  t;
	int octo;
	int zcompat_save;
	char symbuf[TEMPBUFSIZE];

	/*
	 *  move text onto macro file, changing formal parameters
	 */
#ifdef	M_DEBUG
	fprintf(stderr,"enter 'mlex'\n") ;
#endif
	inmlex++;

	mlook = look;

	c = nextmac();
for (;;) {
	octo = 0;
	if (c == '#') {
		c = nextmac();
		if (charclass[c] != STARTER && charclass[c] != LETTER) {
			putm('#');
			continue;
		}
		octo = 1;
	}

	switch(charclass[c]) {

	case DIGIT:
		while (numpart[c]) {
			putm(c);
			c = nextmac();
		}
		continue;

	case STARTER:
	case LETTER:
		t = 0;
		p = symbuf;
		do {
			if (p >= tempmax) {
				*tempmax = '\0';
				printf("was parsing '%s' in macro definition\n", tempbuf);
				error(symlong);
			}
			*p++ = c;
			if (t < MAXSYMBOLSIZE)
				tempbuf[t++] = (c >= 'A' && c <= 'Z')  ?
					c+'a'-'A' : c;
			c = nextmac();
		} while	(charclass[c]==LETTER || charclass[c]==DIGIT || charclass[c]==STARTER);

		tempbuf[t] = 0;
		*p++ = '\0';
		p = symbuf;
		// Bit of dancing to allow VERBs as parameters.
		// We could allow anything if parm.element accepted more.
		// Can't allow token expansion at this point.  Doesn't make
		// sense and could screw things up.
		zcompat_save = zcompat;
		zcompat = 0;
		t = tokenofitem(0, 0, 0);
		if (t != ENDM)
			t = tokenofitem(0, VERB, 0);

		zcompat = zcompat_save;

		if (t == MPARM) {
			if (octo != yylval.itemptr->i_scope)
				t = MPARM + 1;
			else
				octo = 0;
		}

		if (octo) { putm('#'); octo = 0; }

		if (t != MPARM) {
			for (p = symbuf; *p; p++)
				putm(*p);
		}
		else {
			if (*(yylval.itemptr->i_string) == '?' || yylval.itemptr->i_chain)
				putm('\2');
			else
				putm('\1');
			putm(yylval.itemptr->i_value + 'A');
		}
		if (t == ENDM) goto done;
		continue;

	case F_END:
		printf("Warning: macro went until end of file.\n");
		if (expptr) {
			popsi();
			c = nextmac();
			continue;
		}

		goto done;

	default:
		if (c == '\n') {
			linecnt++;
		}
		if (c != '\1' && c != '`') putm(c);
		c = nextmac();
	}
}

	/*
	 *  finish off the file entry
	 */
done:
	while(c != EOF && c != '\n' && c != '\0') c = nextmac();
	linecnt++;
	putm('\n');
	putm('\n');
	putm(0);

	for (c=0; c<ITEMTABLESIZE; c++)
		if (itemtab[c].i_token == MPARM) {
			itemtab[c].i_token = UNDECLARED;
		}
	inmlex = 0;
#ifdef	M_DEBUG
	fprintf(stderr,"exit 'mlex'\n") ;
#endif
}



/*
 *  lexical analyser for the arguments of a macro call
 */
int getarg()
{
	int c;
	char *p;
	static int comma;
	int quote;

	*tempbuf = 0;
	yylval.cval = tempbuf;
	while(charclass[c = nextchar()] == SPACE);

	switch(c) {

	case '\0':
		popsi();
	case '\n':
	case ';':
		comma = 0;
		return(skipline(c));

	case ',':
		if (comma) {
			comma = 0;
			return(',');
		}
		else {
			comma++;
			return(ARG);
		}

	case '\'':
	case '\"':
		quote = c;
		p = tempbuf;
		if (!zcompat)
			*p++ = c;

		do {
			c = nextchar();
			if (c == '\0' || c == '\n') {
				peekc = c;
				*p = 0;
				err[bflag]++;
				return ARG;
			}
			else if (c == quote) {
				if ((c = nextchar()) != quote) {
					if (!zcompat)
						*p++ = quote;
					peekc = c;
					*p = '\0';
					comma++;
					return ARG;
				}
			}
			else
				*p++ = c;
		} while (p < tempmax);
		*tempmax = '\0';
		printf("was parsing macro argument '%s'\n", tempbuf);
		error(symlong);
		return 0; // not reached

	default:  /* unquoted string */
		p = tempbuf;
		peekc = c;
		do switch(c = nextchar()) {
			case '\0':
			case '\n':
			case '\t':
			case ' ':
			case ',':
				peekc = c;
				*p = '\0';
				comma++;
				return(ARG);
			default:
				*p++ = c;
		} while (p < tempmax);
		error("macro argument too long");
		return 0; // not reached
	}
}





/*
 *  add a suffix to a string
 */
void suffix(char *str, char *suff)
{
	strcpy(getsuffix(str), suff);
}

char *basename(char *filename)
{
	char *base, *p;

	base = filename;
	for (p = filename; *p; p++) {
		if (*p == '/' || *p == '\\') {
			base = p + 1;
		}
	}

	return base;
}

char *getsuffix(char *str)
{
	char *suffix = 0;
	str = basename(str);
	for (; *str; str++) {
		if (*str == '.')
			suffix = str;
	}
	return suffix ? suffix : str;
}

// Construct output file given input path.
// Essentially files for "file.z" are sent to "zout/file.suffix".
// And for "dir/file.z" they are "zout/file.suffix"

void outpath(char *out, char *src, char *suff)
{
	strcpy(out, "zout");
	if (!suff)
		return;

	strcat(out, "/");
	strcat(out, basename(src));
	suffix(out, suff);
}


/*
 *  put out a byte to the macro file, keeping the offset
 */
void putm(int c)
{
	mfptr++;
	mfputc(c, mfile);
}



/*
 *  get a byte from the macro file
 */
int getm()
{
	int ch;

	floc++;
	ch = mfgetc(mfile);
	if (ch == EOF) {
		ch = 0;
		fprintf(stderr, "bad macro read\n");
	}
	return ch;
}



/*
 *  pop standard input
 */
void popsi()
{
	int  i;

	for (i=0; i<PARMMAX; i++) {
		if (est[i].param) free(est[i].param);
	}
	floc = est[FLOC].value;
	free(est);
	expptr--;
	est = expptr ? expstack[expptr-1] : 0;
	mfseek(mfile, (long)floc, 0);
	if (lineptr > linebuf) lineptr--;
}



/*
 *  return a unique name for a local symbol
 *  c is the parameter number, n is the macro number.
 */

char *getlocal(int c, int n)
{
	static char local_label[10];

	invented++;
	if (c >= 26)
		c += 'a' - '0';
	sprintf(local_label, "?%c%04d", c+'a', n) ;
	return(local_label);
}

char *getmraslocal()
{
	static char mras_local[32];
	char *p = mras_local + sizeof mras_local - 1;
	int n = est[TEMPNUM].value;

	*p = '\0';
	for (; n > 0; n /= 26)
		*--p = 'A' + n % 26;


	return p;
}


/*
 *  read in a symbol table
 */
void insymtab(char *name)
{
	struct stab *t;
	int  s, i;
	FILE *sfile;

	t = (struct stab *) tempbuf;
	if (!(sfile = fopen(name, "rb")))
		return;
	fread((char *)t, 1, sizeof *t, sfile);
	if (t->t_value != SYMMAJIC)
		return;

	s = t->t_token;
	for (i=0; i<s; i++) {
		fread((char *)t, 1, sizeof *t, sfile);
		if (tokenofitem(UNDECLARED, 0, 0) != UNDECLARED)
			continue;
		yylval.itemptr->i_token = t->t_token;
		yylval.itemptr->i_value = t->t_value;
		if (t->t_token == MACRO)
			yylval.itemptr->i_value += mfptr;
	}

	while ((s = fread(tempbuf, 1, TEMPBUFSIZE, sfile)) > 0) {
		mfptr += s;
		mfwrite(tempbuf, 1, s, mfile) ;
	}
	fclose(sfile);
}



/*
 *  write out symbol table
 */
void outsymtab(char *name)
{
	struct stab *t;
	struct item *ip;
	int  i;
	FILE *sfile;

	t = (struct stab *) tempbuf;
	if (!(sfile = fopen(name, "wb")))
		return;
	for (ip=itemtab; ip<itemmax; ip++) {
		if (ip->i_token == UNDECLARED) {
			ip->i_token = 0;
			nitems--;
		}
	}

	copyname(title, (char *)t);
	t->t_value = SYMMAJIC;
	t->t_token = nitems;
	fwrite((char *)t, 1, sizeof *t, sfile);

	for (ip=itemtab; ip<itemmax; ip++) {
		if (ip->i_token != 0) {
			t->t_token = ip->i_token;
			t->t_value = ip->i_value;
			copyname(ip->i_string, (char *)t);
			fwrite((char *)t, 1, sizeof *t, sfile);
		}
	}

	mfseek(mfile, (long)0, 0);
	while((i = mfread(tempbuf, 1, TEMPBUFSIZE, mfile) ) > 0)
		fwrite(tempbuf, 1, i, sfile);

	fclose(sfile);
}



/*
 *  copy a name into the symbol file
 */
void copyname(char *st1, char *st2)
{
	char  *s1, *s2;
	int  i;

	i = (MAXSYMBOLSIZE+2) & ~01;
	s1 = st1;
	s2 = st2;

	while((*s2++ = *s1++)) i--;		/* -Wall-ishness :-) -RJM */
	while(--i > 0) *s2++ = '\0';
}

/* get the next source file */
void next_source(char *sp)
{

	if(now_in == NEST_IN -1)
		error("Too many nested includes") ;
	if ((now_file = open_incpath(sp, "r")) == NULL) {
		char ebuf[1024] ;
		sprintf(ebuf,"Can't open include file: %s", sp) ;
		error(ebuf) ;
	}
	if (outpass && iflist()) {
		lineout() ;
		fprintf(fout, "**** %s ****\n",sp) ;
	}

	if (outpass && bopt)
		fprintf(fbds, "%04x %04x f %s\n", dollarsign, emit_addr, sp);

	/* save the list control flag with the current line number */
	if (lstoff)
		linein[now_in] = - linein[now_in] ;

	/* no list if include files are turned off */
	lstoff |= iopt ;

	/* save the new file descriptor. */
	fin[++now_in] = now_file ;
	/* start with line 0 */
	linein[now_in] = 0 ;
	/* save away the file name */
	src_name[now_in] = malloc(strlen(sp)+1) ;
	strcpy(src_name[now_in],sp) ;
}

int phaseaddr(int addr)
{
	if (!phaseflag)
		return addr;

	if (addr < phbegin || addr > dollarsign) {
		err[vflag]++;
		if (pass2)
			fprintf(stderr, "$%04x outside current phase area\n", addr);
		return 0;
	}

	return phdollar + (addr - phbegin);
}

// Include contents of named file as binary data.
void incbin(char *filename)
{
	FILE *fp = open_incpath(filename, "rb");
	int ch;
	int start = dollarsign;
	int last = start;

	if (!fp) {
		char ebuf[1024];
		sprintf(ebuf, "Can't binary include file: %s", filename);
		error(ebuf);
		return;
	}

	// Avoid emit() because it has a small buffer and it'll spam the listing.
	while ((ch = fgetc(fp)) != EOF) {
		if (segment == SEG_CODE)
			setmem(emit_addr, ch, MEM_DATA);
		emit_addr++;
		emit_addr &= 0xffff;
		last = dollarsign;
		dollarsign++;
		dollarsign &= 0xffff;

		putbin(ch);
		putrel(ch);
	}

	fclose(fp);

	// Do our own list() work as we emit bytes manually.

	addtoline('\0');

	if (outpass && iflist()) {
		lineout();

		if (nopt)
			fprintf(fout, "%4d:", linein[now_in]);

		if (copt)
		        fprintf(fout, nopt ? "%5s-" : "%4s-", "");

		if (nopt || copt)
			fprintf(fout, "\t");

		puthex(start >> 8, fout);
		puthex(start, fout);
		fprintf(fout, " .. ");
		puthex(last >> 8, fout);
		puthex(last, fout);

		putc('\t', fout);

		fputs(linebuf, fout);

		lineptr = linebuf;
	}
}

void dc(int count, int value)
{
	int start = dollarsign;

	// Avoid emit() because it has a small buffer and it'll spam the listing.
	while (count-- > 0) {
		if (segment == SEG_CODE)
			setmem(emit_addr, value, MEM_DATA);
		emit_addr++;
		emit_addr &= 0xffff;
		dollarsign++;
		dollarsign &= 0xffff;

		putbin(value);
		putrel(value);
	}

	// Do our own list() work as we emit bytes manually.

	addtoline('\0');

	if (outpass && iflist()) {
		lineout();

		if (nopt)
			fprintf(fout, "%4d:", linein[now_in]);

		if (copt)
		        fprintf(fout, nopt ? "%5s-" : "%4s-", "");

		if (nopt || copt)
			fprintf(fout, "\t");

		puthex(start >> 8, fout);
		puthex(start, fout);
		fprintf(fout, " .. ");
		puthex((dollarsign - 1) >> 8, fout);
		puthex((dollarsign - 1), fout);
		putc(' ', fout);
		puthex(value, fout);
		putc('\t', fout);
		fputs(linebuf, fout);
		lsterr2(1);

		lineptr = linebuf;
	}
	else
		lsterr1();
}

void advance_segment(int step)
{
	int top = seg_pos[segment] += step;
	seg_pos[segment] &= 0xffff;
	if (top >= 0x10000)
		top = 0xffff;

	if (top > seg_size[segment])
		seg_size[segment] = top;
}

void expr_reloc_check(struct expr *ex)
{
	if (!relopt) return;
	if (ex->e_scope & (SCOPE_EXTERNAL | SCOPE_NORELOC))
		err[rflag]++;
}

void expr_number_check(struct expr *ex)
{
	if (!relopt) return;
	expr_reloc_check(ex);
	if (ex->e_scope & SCOPE_SEGMASK)
		err[rflag]++;
}

void expr_scope_same(struct expr *ex1, struct expr *ex2)
{
	if (!relopt) return;
	if ((ex1->e_scope & SCOPE_SEGMASK) != (ex2->e_scope & SCOPE_SEGMASK))
		err[rflag]++;
}

void expr_word_check(struct expr *ex)
{
	if (ex->e_value < -32768 || ex->e_value > 65535) {
		err[vflag]++;
	}
}

int is_number(struct expr *ex)
{
	return ex && (ex->e_scope & ~SCOPE_PUBLIC) == 0;
}

int is_external(struct expr *ex)
{
	return ex && (ex->e_scope & SCOPE_EXTERNAL) && !ex->e_left && !ex->e_right &&
		ex->e_item;
}

struct expr *expr_alloc(void)
{
	struct expr *ex = malloc(sizeof *ex);

	ex->e_value = 0;
	ex->e_scope = 0;
	ex->e_token = 0;
	ex->e_item = 0;
	ex->e_left = 0;
	ex->e_right = 0;

	return ex;
}

struct expr *expr_num(int value)
{
	struct expr *ex = expr_alloc();
	ex->e_value = value;
	ex->e_token = '0';

	return ex;
}

// Expression consruction for operators that subtract/compare.
// They produce a valid result if operating on numbers in the same segment.
struct expr *expr_op_sc(struct expr *left, int token, struct expr *right, int value)
{
	struct expr *ex = expr_op(left, token, right, value);

	if (!(ex->e_scope & SCOPE_EXTERNAL) &&
		((left->e_scope ^ right->e_scope) & SCOPE_SEGMASK) == 0)
	{
		// Result relocatable and a simple number
		ex->e_scope &= ~(SCOPE_NORELOC | SCOPE_SEGMASK);
	}

	return ex;
}

struct expr *expr_op(struct expr *left, int token, struct expr *right, int value)
{
	struct expr *ex = expr_alloc();

	ex->e_value = value;
	ex->e_token = token;
	ex->e_left = left;
	ex->e_right = right;

	// Combining two numbers will be fine as long as they're not
	// flagged as external or already not relocatable.  In which case
	// it is up to the particular operator to allow the value
	// to become valid.

	ex->e_scope = left->e_scope;
	if (left->e_scope & SCOPE_SEGMASK)
		ex->e_scope |= SCOPE_NORELOC;
	if (right) {
		ex->e_scope |= right->e_scope;
		if (right->e_scope & SCOPE_SEGMASK)
			ex->e_scope |= SCOPE_NORELOC;
	}

	return ex;
}

void expr_free(struct expr *ex)
{
	if (!ex)
		return;

	expr_free(ex->e_left);
	expr_free(ex->e_right);
	free(ex);
}

int synth_op(struct expr *ex, int gen)
{
	if (ex->e_token == '&' && is_number(ex->e_right) &&
		ex->e_right->e_value == 255)
	{
		if (gen) {
			extend_link(ex->e_left);
			putrelop(RELOP_LOW);
			return 1;
		}
		return can_extend_link(ex->e_left);
	}

	return 0;
}

int link_op(struct expr *ex)
{
	if (!ex)
		return 0;

	switch (ex->e_token) {
	case HIGH: return RELOP_HIGH;
	case LOW: return RELOP_LOW;
	case '~': return RELOP_NOT;
	case '-': return !ex->e_right ? RELOP_NEG : RELOP_SUB;
	case '+': return RELOP_ADD;
	case '*': return RELOP_MUL;
	case '/': return RELOP_DIV;
	case '%': return RELOP_MOD;
	default: return 0;
	}
}

int can_extend_link(struct expr *ex)
{
	if (!ex)
		return 1;

	// If we have a value available then we're good.
	if (!(ex->e_scope & SCOPE_NORELOC))
		return 1;

	// Might be able to synthesize the operation.
	if (synth_op(ex, 0))
		return 1;

	// Otherwise, the operator must be supported and the children
	// must be linkable.

	return link_op(ex) && can_extend_link(ex->e_left) && can_extend_link(ex->e_right);
}

void extend_link(struct expr *ex)
{
	int op;

	if (!ex)
		return;

	if (synth_op(ex, 1))
		return;

	extend_link(ex->e_left);
	extend_link(ex->e_right);

	op = link_op(ex);
	if (op) {
		putrelop(op);
		return;
	}

	putrelcmd(RELCMD_EXTLINK);

	if (is_external(ex)) {
		char *str = ex->e_item->i_string;
		int len = strlen(str);
		if (len > 6)
			len = 6;
		putrelbits(3, 1 + len);
		putrelbits(8, 'B');
		while (len-- > 0) {
			int ch = *str++;
			if (ch >= 'a' && ch <= 'z')
				ch -= 'a' - 'A';
			putrelbits(8, ch);
		}
	}
	else {
		putrelbits(3, 4);
		putrelbits(8, 'C');
		putrelbits(8, ex->e_scope & SCOPE_SEGMASK);
		putrelbits(8, ex->e_value);
		putrelbits(8, ex->e_value >> 8);
	}
}

void putrelop(int op)
{
	putrelcmd(RELCMD_EXTLINK);

	putrelbits(3, 2);
	putrelbits(8, 'A');
	putrelbits(8, op);
}
