/* dcyphr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;

/* $Procedure      DCYPHR ( Decypher the meaning of an IOSTAT code) */
/* Subroutine */ int dcyphr_(integer *iostat, logical *found, char *diagns, 
	ftnlen diagns_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static integer lbnd, ubnd;
    static char attr[32*2];
    static logical next;
    static integer n;
    static logical alpha;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    static logical pc, hp;
    static char messge[800*90];
    extern /* Subroutine */ int pltfrm_(integer *, integer *, char *, ftnlen);
    static logical sgi, vax, sun;

/* $ Abstract */

/*     Given an IOSTAT code returned by a read, write, open, */
/*     inquire, or close statement, this routine returns a */
/*     brief text description of the problem. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*      None. */

/* $ Keywords */

/*       ERROR */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      IOSTAT     I   The value of IOSTAT returned by a FORTRAN function */
/*      FOUND      O   TRUE if the value of IOSTAT was found */
/*      DIAGNS     O   A string describing the meaning of IOSTAT */

/* $ Detailed_Input */

/*     IOSTAT      is the non-zero value of IOSTAT returned by */
/*                 some intrinsic FORTRAN I/O facility such as */
/*                 OPEN, INQUIRE, READ, WRITE, or CLOSE. */

/* $ Detailed_Output */

/*     FOUND       is set to TRUE if the value of IOSTAT was found, */
/*                 otherwise it is returned as false. */

/*     DIAGNS      is a string that describes the meaning of IOSTAT. */
/*                 you should declare DIAGNS to be at least */
/*                 CHARACTER*(800) to ensure that the message will */
/*                 fit into DIAGNS. */

/* $ Parameters */

/*      None. */

/* $ Files */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/*     1) If the meaning of IOSTAT is not available within this */
/*        routine, DIAGNS will be returned with a string of the */
/*        form: */

/*           The value of IOSTAT was #.  The meaning of this */
/*           value is not available via the SPICE system. */
/*           Please consult your FORTRAN manual for the */
/*           meaning of this code. */

/*        where the character '#' will be replaced by a string */
/*        giving the input value of IOSTAT. */

/* $ Particulars */

/*     This routine is a utility for aiding in the construction */
/*     of messages relating to the failure of FORTRAN I/O. */

/* $ Examples */

/*     Suppose that you get a positive value of IOSTAT as the */
/*     result of a FORTRAN I/O statement and that you'd like to */
/*     present a descriptive diagnostic. */

/*        CALL DCYPHR ( IOSTAT, DIAGNS ) */
/*        WRITE (*,*) DIAGNS */


/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*      H.A. Neilan     (JPL) */
/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/* -    SPICELIB Version 1.0.0, 21-APR-1994 (HAN) (WLT) */


/* -& */
/* $ Index_Entries */

/*     Get the meaning of an IOSTAT value. */

/* -& */
    if (first) {
	pltfrm_(&c__2, &n, attr, (ftnlen)32);
	next = first && s_cmp(attr, "NEXT ", (ftnlen)32, (ftnlen)5) == 0;
	hp = first && s_cmp(attr, "HP   ", (ftnlen)32, (ftnlen)5) == 0;
	sun = first && s_cmp(attr, "SUN  ", (ftnlen)32, (ftnlen)5) == 0;
	sgi = first && s_cmp(attr, "SGI  ", (ftnlen)32, (ftnlen)5) == 0;
	vax = first && s_cmp(attr, "VAX  ", (ftnlen)32, (ftnlen)5) == 0;
	pc = first && s_cmp(attr, "PC   ", (ftnlen)32, (ftnlen)5) == 0;
	alpha = first && s_cmp(attr, "ALPHA", (ftnlen)32, (ftnlen)5) == 0;
	first = FALSE_;
    }
    if (next) {
	lbnd = 9999;
	ubnd = 10032;
	s_copy(messge, "The file is not open for reading.", (ftnlen)800, (
		ftnlen)33);
	s_copy(messge + 800, "The file is not open for writing.", (ftnlen)800,
		 (ftnlen)33);
	s_copy(messge + 1600, "The file was not found.", (ftnlen)800, (ftnlen)
		23);
	s_copy(messge + 2400, "The record length specified was negative or 0."
		, (ftnlen)800, (ftnlen)46);
	s_copy(messge + 3200, "I/O buffer allocation failed.", (ftnlen)800, (
		ftnlen)29);
	s_copy(messge + 4000, "The iolist specifier was bad.", (ftnlen)800, (
		ftnlen)29);
	s_copy(messge + 4800, "The format string is in error.", (ftnlen)800, (
		ftnlen)30);
	s_copy(messge + 5600, "The repeat count is illegal.", (ftnlen)800, (
		ftnlen)28);
	s_copy(messge + 6400, "The hollerith count exceeds remaining format "
		"string.", (ftnlen)800, (ftnlen)52);
	s_copy(messge + 7200, "The format string is missing an opening \"(\"."
		, (ftnlen)800, (ftnlen)44);
	s_copy(messge + 8000, "The format string has unmatched parentheses.", 
		(ftnlen)800, (ftnlen)44);
	s_copy(messge + 8800, "The format string has unmatched quotes.", (
		ftnlen)800, (ftnlen)39);
	s_copy(messge + 9600, "A format descriptor is non-repeatable.", (
		ftnlen)800, (ftnlen)38);
	s_copy(messge + 10400, "The program attempted to read past end of th"
		"e file.", (ftnlen)800, (ftnlen)51);
	s_copy(messge + 11200, "The file specification was bad.", (ftnlen)800,
		 (ftnlen)31);
	s_copy(messge + 12000, "The format group table overflowed.", (ftnlen)
		800, (ftnlen)34);
	s_copy(messge + 12800, "An illegal character was present in numeric "
		"input.", (ftnlen)800, (ftnlen)50);
	s_copy(messge + 13600, "No record was specified while using direct a"
		"ccess I/O.", (ftnlen)800, (ftnlen)54);
	s_copy(messge + 14400, "The maximum record number was exceeded.", (
		ftnlen)800, (ftnlen)39);
	s_copy(messge + 15200, "An illegal file type was supplied for use wi"
		"th namelist directed I/O", (ftnlen)800, (ftnlen)68);
	s_copy(messge + 16000, "An illegal input for namelist directed I/O w"
		"as encountered.", (ftnlen)800, (ftnlen)59);
	s_copy(messge + 16800, "A variable is not present in the current nam"
		"elist.", (ftnlen)800, (ftnlen)50);
	s_copy(messge + 17600, "A variable type or size does not match edit "
		"descriptor.", (ftnlen)800, (ftnlen)55);
	s_copy(messge + 18400, "An llegal direct access record number was us"
		"ed.", (ftnlen)800, (ftnlen)47);
	s_copy(messge + 19200, "An internal file was used illegally.", (
		ftnlen)800, (ftnlen)36);
	s_copy(messge + 20000, "The OPEN specifiere \"RECL=\" is only valid "
		"for direct access files", (ftnlen)800, (ftnlen)65);
	s_copy(messge + 20800, "The Open specifiere \"BLOCK=\" is only valid"
		" for unformatted sequential files.", (ftnlen)800, (ftnlen)76);
	s_copy(messge + 21600, "The program was unable to truncate the file "
		"after rewind, backspace,or endfile.", (ftnlen)800, (ftnlen)79)
		;
	s_copy(messge + 22400, "It's illegal to use formatted I/O on an enti"
		"re structure.", (ftnlen)800, (ftnlen)57);
	s_copy(messge + 23200, "An illegal (negative) unit was specified.", (
		ftnlen)800, (ftnlen)41);
	s_copy(messge + 24000, "The specifications in a RE-OPEN do not match"
		" aprevious OPEN.", (ftnlen)800, (ftnlen)60);
	s_copy(messge + 24800, "An implicit OPEN can not be used for direct "
		"access files.", (ftnlen)800, (ftnlen)57);
	s_copy(messge + 25600, "The file already exists. It cannot be opened"
		" as a new file.", (ftnlen)800, (ftnlen)59);
    } else if (sun) {
	lbnd = 99;
	ubnd = 126;
	s_copy(messge, "The format string is in error.", (ftnlen)800, (ftnlen)
		30);
	s_copy(messge + 800, "The unit number is illegal.", (ftnlen)800, (
		ftnlen)27);
	s_copy(messge + 1600, "The logical unit was opened for unformatted I"
		"/O, not formatted.", (ftnlen)800, (ftnlen)63);
	s_copy(messge + 2400, "The logical unit was opened for formatted I/O"
		", not unformatted.", (ftnlen)800, (ftnlen)63);
	s_copy(messge + 3200, "The logical unit was opened for sequential ac"
		"cess, or the logical record length was specified as zero.", (
		ftnlen)800, (ftnlen)102);
	s_copy(messge + 4000, "The logical unit was opened for direct I/O, n"
		"ot sequential.", (ftnlen)800, (ftnlen)59);
	s_copy(messge + 4800, "The program was unable to backspace the file.",
		 (ftnlen)800, (ftnlen)45);
	s_copy(messge + 5600, "The format specified a left tab beyond the be"
		"ginning of an internal input record.", (ftnlen)800, (ftnlen)
		81);
	s_copy(messge + 6400, "The system cannot return status information a"
		"bout the file. Perhaps the directory is unreadable.", (ftnlen)
		800, (ftnlen)96);
	s_copy(messge + 7200, "Repeat counts in list-directed I/O must be fo"
		"llowed by an asterisk with no blank spaces.", (ftnlen)800, (
		ftnlen)88);
	s_copy(messge + 8000, "The program attempted to read past the end of"
		" a record.", (ftnlen)800, (ftnlen)55);
	s_copy(messge + 8800, "The program was unable to truncate an externa"
		"l sequential file on close, backspace, or rewind.", (ftnlen)
		800, (ftnlen)94);
	s_copy(messge + 9600, "The list input is incomprehensible.", (ftnlen)
		800, (ftnlen)35);
	s_copy(messge + 10400, "The library dynamically creates buffers for "
		"internal use. The program is too big, and thus ran out of fr"
		"ee space.", (ftnlen)800, (ftnlen)113);
	s_copy(messge + 11200, "The logical unit was not open.", (ftnlen)800, 
		(ftnlen)30);
	s_copy(messge + 12000, "An unexpected character was encountered. Som"
		"e format conversions cannot tolerate nonnumeric data.", (
		ftnlen)800, (ftnlen)97);
	s_copy(messge + 12800, "Logical data must be true or false.", (ftnlen)
		800, (ftnlen)35);
	s_copy(messge + 13600, "The program tried to open an existing file w"
		"ith \"STATUS = NEW\".", (ftnlen)800, (ftnlen)63);
	s_copy(messge + 14400, "The program tried to open a nonexistent file"
		" with \"STATUS=OLD\".", (ftnlen)800, (ftnlen)63);
	s_copy(messge + 15200, "The program caused an unknown system error. "
		"Contact your system administrator!", (ftnlen)800, (ftnlen)78);
	s_copy(messge + 16000, "Direct access of a file requires seek abilit"
		"y. Sequential unformatted I/O and tabbing left also require "
		"seek ability.", (ftnlen)800, (ftnlen)117);
	s_copy(messge + 16800, "An illegal argument was specified in the sta"
		"tement.", (ftnlen)800, (ftnlen)51);
	s_copy(messge + 17600, "The repeat count for list-directed input mus"
		"t be a positive integer.", (ftnlen)800, (ftnlen)68);
	s_copy(messge + 18400, "An illegal operation was attempted on the de"
		"vice associated with the unit.", (ftnlen)800, (ftnlen)74);
	s_copy(messge + 19200, "The program tried to open too many files. Th"
		"e limit is 64.", (ftnlen)800, (ftnlen)58);
	s_copy(messge + 20000, "The logical unit was not open.", (ftnlen)800, 
		(ftnlen)30);
	s_copy(messge + 20800, "A namelist read encountered an invalid data "
		"item.", (ftnlen)800, (ftnlen)49);
    } else if (hp) {
	lbnd = 899;
	ubnd = 989;
	s_copy(messge, "Error in format. Format specification does not start"
		" with a left parenthesis or end with a right parenthesis, or"
		" contains unrecognizable code or string; format specificatio"
		"n is too long for library internal buffer. Change the format"
		" specification to proper syntax; split the format specificat"
		"ions into several statements. ", (ftnlen)800, (ftnlen)322);
	s_copy(messge + 800, "I/O with illegal unit number attempted. Negati"
		"ve unit number was used in an I/O statement. Use integers gr"
		"eater than or equal to 0 for an I/O number. ", (ftnlen)800, (
		ftnlen)150);
	s_copy(messge + 1600, "Formatted I/O attempted on unformatted file. "
		"Formatted I/O was attempted on a file opened for unformatted"
		" I/O. Open the file for formatted I/O; do unformatted I/O on"
		" this file. ", (ftnlen)800, (ftnlen)177);
	s_copy(messge + 2400, "Unformatted I/O attempted on formatted file. "
		"Unformatted I/O was attempted on a file opened for formatted"
		" I/O. Open the file for unformatted I/O; do formatted I/O on"
		" this file. ", (ftnlen)800, (ftnlen)177);
	s_copy(messge + 3200, "Direct I/O attempted on sequential file. Dire"
		"ct operation attempted on sequential file; direct operation "
		"attempted on opened file connected to a terminal. Use sequen"
		"tial operations on this file; open file for direct access; d"
		"o not do direct I/O on a file connected to a terminal. ", (
		ftnlen)800, (ftnlen)280);
	s_copy(messge + 4000, "Error in list- or name-directed read of logic"
		"al data. Found repeat value, but no asterisk; first characte"
		"r after optional decimal point was not \"T\" or \"F\". Chang"
		"e input data to correspond to syntax expected by list-direct"
		"ed input of logicals; use input statement that corresponds t"
		"o syntax of input data. ", (ftnlen)800, (ftnlen)305);
	s_copy(messge + 4800, "Illegal sequential I/O to tty attempted1. Exe"
		"cuted a BACKSPACE, REWIND, formatted READ, or formatted WRIT"
		"E, on this sequential file or device. Use a file or device t"
		"hat is considered blocked in HP-UX. ", (ftnlen)800, (ftnlen)
		201);
	s_copy(messge + 5600, "List- or name-directed read of character data"
		" attempted. Found repeat value, but no asterisk; character n"
		"ot delimited by quotation marks. Change input data to corres"
		"pond to syntax expected by list-directed input of characters"
		"; use input statement that corresponds to syntax of input da"
		"ta. ", (ftnlen)800, (ftnlen)289);
	s_copy(messge + 6400, "Open of file with bad path-name attempted. Tr"
		"ied to open a file that the system would not allow for one o"
		"f the following reasons: 1.  A component of the path prefix "
		"is not a directory. 2.  The named file does not exist. 3.  S"
		"earch permission is denied for a component of the path prefi"
		"x. Correct the path-name to invoke the file intended; check "
		"that the file is not corrupt; be sure that search permission"
		"s are set properly. ", (ftnlen)800, (ftnlen)425);
	s_copy(messge + 7200, "Sequential I/O attempted on direct file. Atte"
		"mpted a BACKSPACE, REWIND, or ENDFILE on a direct file. Open"
		" the file for sequential access; do not use BACKSPACE, REWIN"
		"D, or ENDFILE. ", (ftnlen)800, (ftnlen)180);
	s_copy(messge + 8000, "Access past end of record attempted. Tried to"
		" do I/O on record of a file past beginning or end of record."
		" Perform I/O operation within bounds of the record; increase"
		" record length. ", (ftnlen)800, (ftnlen)181);
	s_copy(messge + 8800, "Recursive I/O attempted1. An I/O specifier or"
		" item in an I/O list attempted to do I/O (that is, calls to "
		"functions that do I/O). Remove calls to functions that do I/"
		"O from the specifier/list item; remove I/O statements from t"
		"he function called by the specifier/list item. ", (ftnlen)800,
		 (ftnlen)272);
	s_copy(messge + 9600, "Error in list- or name-directed read of compl"
		"ex data. While reading complex data, one of the following pr"
		"oblems has occurred: 1.  No left parenthesis or no repeat va"
		"lue. 2.  Found repeat value, but no asterisk. 3.  No comma a"
		"fter real part. 4.  No closing right parenthesis. Change inp"
		"ut data to correspond to syntax expected by list-directed in"
		"put of complex numbers; use input statement corresponding to"
		" syntax of input data. ", (ftnlen)800, (ftnlen)428);
	s_copy(messge + 10400, "Out of free space. Library cannot store file"
		" name (from OPEN statement) or characters read (from list-di"
		"rected read). Use shorter file name or read fewer characters"
		"; use fewer file names or read fewer character strings. ", (
		ftnlen)800, (ftnlen)220);
	s_copy(messge + 11200, "Access of unconnected unit attempted. Unit s"
		"pecified in I/O statement has not previously been connected "
		"to anything. Connect unit (that is, OPEN it) before attempti"
		"ng I/O on it; perform I/O on another, already connected, uni"
		"t. ", (ftnlen)800, (ftnlen)227);
	s_copy(messge + 12000, "Read unexpected character. While reading an "
		"integer, read a character that was not a digit, \"+\", \"-\""
		", comma, end-of-line or blank; while reading a real number, "
		"read a character that was not a digit, \"+\", \"-\", comma, "
		"end-of-line, blank, \"d\", \"D\", \"e\", \"E\", or period. R"
		"emove from input data any characters that are illegal in int"
		"egers or real numbers. ", (ftnlen)800, (ftnlen)351);
	s_copy(messge + 12800, "Error in read of logical data. A blank was r"
		"ead when logical data was expected. Change input data to cor"
		"respond to syntax expected when reading logical data; use in"
		"put statement corresponding to syntax of input data. ", (
		ftnlen)800, (ftnlen)217);
	s_copy(messge + 13600, "Open with named scratch file attempted. Exec"
		"uted OPEN statement with STATUS='SCRATCH', but also named th"
		"e file (FILE= filename). Either open file with STATUS='SCRAT"
		"CH', or name the file in an OPEN statement, but not both. ", (
		ftnlen)800, (ftnlen)222);
	s_copy(messge + 14400, "Open of existing file with STATUS='NEW' atte"
		"mpted. Executed OPEN statement with STATUS='NEW', but file a"
		"lready exists. Use OPEN without STATUS specifier, or with ST"
		"ATUS='OLD', or STATUS='UNKNOWN'. ", (ftnlen)800, (ftnlen)197);
	s_copy(messge + 15200, "The value of IOSTAT was 919.  No explanation"
		" is provided in the HP documentation for this value of IOSTA"
		"T. . ", (ftnlen)800, (ftnlen)109);
	s_copy(messge + 16000, "Open of file connected to different unit att"
		"empted. Executed OPEN statement with file name that is alrea"
		"dy associated with a UNIT specifier. Use an OPEN statement w"
		"ith a file name that is not connected to a unit name; open t"
		"he connected file to the same unit name. ", (ftnlen)800, (
		ftnlen)265);
	s_copy(messge + 16800, "Unformatted open with BLANK specifier attemp"
		"ted. OPEN statement specified FORM='UNFORMATTED' and BLANK= "
		"xx. Use either FORM='FORMATTED' or BLANK= xx, but not both, "
		"when opening files. ", (ftnlen)800, (ftnlen)184);
	s_copy(messge + 17600, "I/O on illegal record attempted. Attempted t"
		"o read a record of a formatted or unformatted direct file th"
		"at is beyond the current end-of-file. Read records that are "
		"within the bounds of the file. ", (ftnlen)800, (ftnlen)195);
	s_copy(messge + 18400, "Open with illegal FORM specifier attempted. "
		"FORM specifier did not begin with \"F\", \"f\", \"U\", or \"u"
		"\". Use either 'FORMATTED' or 'UNFORMATTED' for the FORM spe"
		"cifier in an OPEN statement. ", (ftnlen)800, (ftnlen)186);
	s_copy(messge + 19200, "Close of scratch file with STATUS='KEEP' att"
		"empted. The file specified in the CLOSE statement was previo"
		"usly opened with 'SCRATCH' specified in the STATUS specifier"
		". Open the file with a STATUS other than 'SCRATCH'; do not s"
		"pecify STATUS='KEEP' in the CLOSE statement for this scratch"
		" file. ", (ftnlen)800, (ftnlen)291);
	s_copy(messge + 20000, "Open with illegal STATUS specifier attempted"
		". STATUS specifier did not begin with \"O\", \"o\", \"N\","
		" \"n\", \"S\", \"s\", \"U\", or \"u\". Use 'OLD', 'NEW', 'SC"
		"RATCH', or 'UNKNOWN' for the STATUS specifier in OPEN statem"
		"ent. ", (ftnlen)800, (ftnlen)211);
	s_copy(messge + 20800, "Close with illegal STATUS specifier attempte"
		"d. STATUS specifier did not begin with \"K\", \"k\", \"D\", "
		"or \"d\". statement. ", (ftnlen)800, (ftnlen)117);
	s_copy(messge + 21600, "Open with illegal ACCESS specifier attempted"
		". ACCESS specifier did not begin with \"S\", \"s\", \"D\", o"
		"r \"d\". Use 'SEQUENTIAL' or 'DIRECT' for the ACCESS specifi"
		"er in an OPEN statement. ", (ftnlen)800, (ftnlen)181);
	s_copy(messge + 22400, "Open of sequential file with RECL specifier "
		"attempted. OPEN statement had both ACCESS='SEQUENTIAL' and R"
		"ECL= xx specified. Omit RECL specifier; specify ACCESS='DIRE"
		"CT'. ", (ftnlen)800, (ftnlen)169);
	s_copy(messge + 23200, "Open of direct file with no RECL specifier a"
		"ttempted. OPEN statement has ACCESS='DIRECT', but no RECL sp"
		"ecifier. Add RECL specifier; specify ACCESS='SEQUENTIAL'. or"
		" Open of direct file with no RECL or RECL=0 attempted1 OPEN "
		"statement has ACCESS='DIRECT', but no RECL specifier. Add RE"
		"CL specifier; specify ACCESS='SEQUENTIAL'. ", (ftnlen)800, (
		ftnlen)327);
	s_copy(messge + 24000, "Open with RECL less than 1 attempted. RECL s"
		"pecifier in OPEN statement was less than or equal to zero. U"
		"se a positive number for RECL specifier in OPEN statement. o"
		"r Open with RECL less than zero attempted. RECL specifier in"
		" OPEN statement was less than or equal to zero. Use a positi"
		"ve number for RECL specifier in OPEN statement. ", (ftnlen)
		800, (ftnlen)332);
	s_copy(messge + 24800, "Open with illegal BLANK specifier attempted."
		" BLANK specifier did not begin with \"N\", \"n\", \"Z\", or"
		" \"z\". Use 'NULL' or 'ZERO' for BLANK specifier in OPEN sta"
		"tement. ", (ftnlen)800, (ftnlen)163);
	s_copy(messge + 25600, "Too many units open at once. The program att"
		"empted to have greater than 60 files open at once. Close a p"
		"resently open file before opening another. ", (ftnlen)800, (
		ftnlen)147);
	s_copy(messge + 26400, "End of file encountered. Attempted to read b"
		"eyond the end of a sequential file. Read records that are wi"
		"thin bounds of the file. ", (ftnlen)800, (ftnlen)129);
	s_copy(messge + 27200, "The value of IOSTAT was 934.  No explanation"
		" is provided in the HP documentation for this value of IOSTA"
		"T. ", (ftnlen)800, (ftnlen)107);
	s_copy(messge + 28000, "Internal library error. A rare software erro"
		"r has occurred. Report the error. ", (ftnlen)800, (ftnlen)78);
	s_copy(messge + 28800, "The value of IOSTAT was 936.  No explanation"
		" is provided in the HP documentation for this value of IOSTA"
		"T. ", (ftnlen)800, (ftnlen)107);
	s_copy(messge + 29600, "Access of record <=0 attempted. Access of di"
		"rect file specifier REC= negative number or 0. Use an intege"
		"r greater than 0 in the REC= specifier. ", (ftnlen)800, (
		ftnlen)144);
	s_copy(messge + 30400, "List I/O of unknown type attempted. An inter"
		"nal error has occurred. Report the error. ", (ftnlen)800, (
		ftnlen)86);
	s_copy(messge + 31200, "Open of inaccessible file attempted. When op"
		"ening a file with STATUS='OLD', component of the path is not"
		" a directory, the named file does not exist, or the path poi"
		"nts outside a process or allocated address space. Use legal "
		"pathname; insure existence of file; or open with STATUS='NEW"
		"'. ", (ftnlen)800, (ftnlen)287);
	s_copy(messge + 32000, "Open attempted. Too many files open; file pe"
		"rmissions do not allow access. Close some files before openi"
		"ng more; change read/write access of file to allow open. ", (
		ftnlen)800, (ftnlen)161);
	s_copy(messge + 32800, "Error in sequential unformatted read. Attemp"
		"t to prepare file for sequential unformatted read failed. Us"
		"e existing, non-corrupt file and be sure the system is not c"
		"orrupt. ", (ftnlen)800, (ftnlen)172);
	s_copy(messge + 33600, "Error in list- or name-directed read. System"
		" detected error while trying to do list read. Be sure system"
		" and file are not corrupt. ", (ftnlen)800, (ftnlen)131);
	s_copy(messge + 34400, "Error in direct formatted read. System encou"
		"ntered problem while reading a character from specified exte"
		"rnal file. Be sure file and system are not corrupt. ", (
		ftnlen)800, (ftnlen)156);
	s_copy(messge + 35200, "Error in direct unformatted I/O. System foun"
		"d error while concluding direct unformatted I/O call. Be sur"
		"e file and system are not corrupt. ", (ftnlen)800, (ftnlen)
		139);
	s_copy(messge + 36000, "Error in formatted I/O. System found error w"
		"hile reading or writing formatted data; usually means more c"
		"haracters were requested than exist in a record. Be sure for"
		"mat matches data.  Be sure file and system are not corrupt. ",
		 (ftnlen)800, (ftnlen)224);
	s_copy(messge + 36800, "Error in list I/O. List I/O was attempted on"
		" an unformatted file. Do list I/O on formatted file. ", (
		ftnlen)800, (ftnlen)97);
	s_copy(messge + 37600, "Edit descriptor not compatible with type of "
		"item. Use an edit descriptor that is compatible with the dat"
		"a item; use a data item that is compatible with the edit des"
		"criptor. ", (ftnlen)800, (ftnlen)173);
	s_copy(messge + 38400, "Write to write-protected file attempted. Cha"
		"nge write protection bit to allow write; do not write to thi"
		"s file. ", (ftnlen)800, (ftnlen)112);
	s_copy(messge + 39200, "Read from read-protected file attempted. Cha"
		"nge read protection bit to allow read; do not read from this"
		" file. ", (ftnlen)800, (ftnlen)111);
	s_copy(messge + 40000, "Value out of range. An index to an array or "
		"substring reference was outside of the declared limits. Chec"
		"k all indexes to arrays and substrings. ", (ftnlen)800, (
		ftnlen)144);
	s_copy(messge + 40800, "Label out of bounds in assigned GOTO. The va"
		"lue of the variable did not correspond to any of the labels "
		"in the list in an assigned GOTO statement. Check for a possi"
		"ble logic error in the program or an incorrect list in the a"
		"ssigned GOTO statement. ", (ftnlen)800, (ftnlen)248);
	s_copy(messge + 41600, "Zero increment value in DO loop. A DO loop w"
		"ith a zero increment has produced an infinite loop. Check fo"
		"r a logic error in the program. ", (ftnlen)800, (ftnlen)136);
	s_copy(messge + 42400, "No repeatable edit descriptor in format stat"
		"ement. A repeat count was given for an edit descriptor that "
		"does not allow repetition. Add at least one repeatable edit "
		"descriptor to the format statement. ", (ftnlen)800, (ftnlen)
		200);
	s_copy(messge + 43200, "Illegal use of empty format attempted. An em"
		"pty format specification, (), was used with the list items s"
		"pecified. Remove the items from I/O list; fill in the format"
		" specifications with the appropriate format descriptors. ", (
		ftnlen)800, (ftnlen)221);
	s_copy(messge + 44000, "Open with no FILE= and STATUS 'OLD' or 'NEW'"
		" attempted. Status 'NEW' or 'OLD' was attempted and FILE= wa"
		"s not specified. Change the STATUS specifier to 'SCRATCH' or"
		" 'UNKNOWN'; add the file specifier. ", (ftnlen)800, (ftnlen)
		200);
	s_copy(messge + 44800, "The value of IOSTAT was 956.  No explanation"
		" is provided in the HP documentation for this value of IOSTA"
		"T. ", (ftnlen)800, (ftnlen)107);
	s_copy(messge + 45600, "Format descriptor incompatible with numeric "
		"item in I/O list. A numeric item in the I/O list was matched"
		" with a nonnumeric format descriptor. Match format descripto"
		"rs to I/O list. or File could not be truncated. Physical len"
		"gth of file could not be forced to match the logical length. "
		, (ftnlen)800, (ftnlen)285);
	s_copy(messge + 46400, "Format descriptor incompatible with characte"
		"r item in I/O list. A character item in the I/O list was mat"
		"ched with a format descriptor other than \"A\" or \"R\". Mat"
		"ch format descriptors to I/O list. or Unexpected character i"
		"n NAMELIST read. An illegal character was found in NAMELIST-"
		"directed input. Be sure input data conforms to the syntax ru"
		"les for NAMELIST-directed input. ", (ftnlen)800, (ftnlen)373);
	s_copy(messge + 47200, "Format descriptor incompatible with logical "
		"item in I/O list. A logical item in the I/O list was matched"
		" with a format descriptor other than \"L\". Match format des"
		"criptors to I/O list. or Illegal subscript/substring in NAME"
		"LIST read. An invalid subscript or substring specifier was f"
		"ound in NAMELIST-directed input. Possible causes:  bad synta"
		"x, subscript/substring component out-of-bounds, wrong number"
		" of subscripts, substring on non-CHARACTER variable. Check i"
		"nput data for syntax errors.  Be sure subscript/substring sp"
		"ecifiers are correct for data type. ", (ftnlen)800, (ftnlen)
		558);
	s_copy(messge + 48000, "Format error: Missing starting left parenthe"
		"sis. Format did not begin with a left parenthesis. Begin for"
		"mat with a left parenthesis. or Too many values in NAMELIST "
		"read. Too many input values were found during a NAMELIST-dir"
		"ected READ. This message will be generated by attempts to fi"
		"ll variables beyond their memory limits. Remove excess value"
		"s from input data. ", (ftnlen)800, (ftnlen)363);
	s_copy(messge + 48800, "Variable not in NAMELIST group. A variable n"
		"ame was encountered in the input stream which was not declar"
		"ed as part of the current NAMELIST group. Check input data w"
		"ith NAMELIST group declaration for differences. Format error"
		": Invalid format descriptor. Format descriptor did not begin"
		" with a character that can start a legal format descriptor. "
		"Specify correct format descriptor. ", (ftnlen)800, (ftnlen)
		379);
	s_copy(messge + 49600, "Unexpected character found following a numbe"
		"r in the format string. Format error:  Character in the set "
		"IFEDGMNK@OLAR(PHX expected and not found. Specify correct fo"
		"rmat descriptor to follow number. or NAMELIST I/O attempted "
		"on unformatted file1 An illegal NAMELIST I/O operation was a"
		"ttempted on an unformatted file. OPEN file with FORM='FORMAT"
		"TED'. ", (ftnlen)800, (ftnlen)350);
	s_copy(messge + 50400, "Format error: Trying to scale unscalable for"
		"mat specifier. The specifier being scaled is not \"F\", \""
		"E\", \"D\", \"M\", \"N\", or \"G\". Scale only specifiers fo"
		"r floating-point I/O. or COUNT exceeds buffer length in ENCO"
		"DE/DECODE1 The count of characters to be transferred exceeds"
		" the internal buffer length. Either transfer fewer character"
		"s or use a larger buffer. ", (ftnlen)800, (ftnlen)356);
	s_copy(messge + 51200, "Format error: Parentheses too deeply nested."
		" Too many left parentheses for the format processor to stack"
		". Nest parentheses less deeply. ", (ftnlen)800, (ftnlen)136);
	s_copy(messge + 52000, "Format error: Invalid tab specifier. A speci"
		"fier beginning with \"T\" is not a correct tab specifier. Co"
		"rrect the specifier beginning with \"T\". ", (ftnlen)800, (
		ftnlen)142);
	s_copy(messge + 52800, "Format error: Invalid blank specifier. A spe"
		"cifier beginning with \"B\" did not have \"N\" or \"Z\" as t"
		"he next character. Correct the specifier beginning with \""
		"B\". ", (ftnlen)800, (ftnlen)159);
	s_copy(messge + 53600, "Format error: Specifier expected but end of "
		"format found. The end of the format was reached when another"
		" specifier was expected. Check the end of the format for a c"
		"ondition that would lead the processor to look for another s"
		"pecifier (possibly a missing right parenthesis). ", (ftnlen)
		800, (ftnlen)273);
	s_copy(messge + 54400, "Format error: Missing separator. Other speci"
		"fier found when /, :, or ) expected. Insert separator where "
		"needed. ", (ftnlen)800, (ftnlen)112);
	s_copy(messge + 55200, "Format error: Digit expected. Number not fou"
		"nd following format descriptor requiring a field width. Spec"
		"ify field width where required. ", (ftnlen)800, (ftnlen)136);
	s_copy(messge + 56000, "Format error: Period expected in floating po"
		"int format descriptor. No period was found to specify the nu"
		"mber of decimal places in an \"F\", \"G\", \"E\", or \"D\" f"
		"ormat descriptor. Specify the number of decimal places for t"
		"he field. ", (ftnlen)800, (ftnlen)226);
	s_copy(messge + 56800, "Format error: Unbalanced parentheses. More r"
		"ight parentheses than left parentheses were found. Correct f"
		"ormat so parentheses balance. ", (ftnlen)800, (ftnlen)134);
	s_copy(messge + 57600, "Format error: Invalid string in format. Stri"
		"ng extends past the end of the format or is too long for buf"
		"fer. Check for unbalanced quotation mark or for \"H\" format"
		" count too large; or break up long string. ", (ftnlen)800, (
		ftnlen)205);
	s_copy(messge + 58400, "Record length different in subsequent OPEN. "
		"Record length specified in redundant OPEN conflicted with th"
		"e value as opened. Only BLANK= specifier may be changed by a"
		" redundant OPEN. ", (ftnlen)800, (ftnlen)181);
	s_copy(messge + 59200, "Record accessed past end of internal file re"
		"cord (variable). An attempt was made to transfer more charac"
		"ters than internal file length. Match READ or WRITE with int"
		"ernal file size. ", (ftnlen)800, (ftnlen)181);
	s_copy(messge + 60000, "Illegal new file number requested in fset fu"
		"nction. The file number requested to be set was not a legal "
		"file system file number. Check that the OPEN succeeded and t"
		"he file number is correct. ", (ftnlen)800, (ftnlen)191);
	s_copy(messge + 60800, "Unexpected character in \"NAMELIST\" read. A"
		"n illegal character was found in NAMELIST-directed input. Be"
		" sure input data conforms to the syntax rules for \"NAMELIS"
		"T\"-directed input; remove illegal character from data. ", (
		ftnlen)800, (ftnlen)215);
	s_copy(messge + 61600, "Illegal subscript or substring in \"NAMELIS"
		"T\" read. An invalid subscript or substring specifier was fo"
		"und in NAMELIST-directed input.  Possible causes:  bad synta"
		"x, subscript/substring component out-of-bounds, wrong number"
		" of subscripts, substring on non-CHARACTER variable. Check i"
		"nput data for syntax errors.  Be sure subscript/substring sp"
		"ecifiers are correct for data type; specify only array eleme"
		"nts within the bounds of the array being read. ", (ftnlen)800,
		 (ftnlen)448);
	s_copy(messge + 62400, "Too many values in \"NAMELIST\" read. Too ma"
		"ny input values were found during a NAMELIST-directed READ. "
		"This message will be generated by attempts to fill variables"
		" beyond their memory limits. Supply only as many values as t"
		"he length of the array. ", (ftnlen)800, (ftnlen)246);
	s_copy(messge + 63200, "Variable not in \"NAMELIST\" group. A variab"
		"le name was encountered in the input stream which was not de"
		"clared as part of the current NAMELIST group. Read only the "
		"variables in this NAMELIST. ", (ftnlen)800, (ftnlen)190);
	s_copy(messge + 64000, "\"NAMELIST\" I/O attempted on unformatted fi"
		"le. An illegal NAMELIST I/O operation was attempted on an un"
		"formatted (binary) file. OPEN file with FORM='FORMATTED'; us"
		"e NAMELIST I/O only on formatted files. ", (ftnlen)800, (
		ftnlen)202);
	s_copy(messge + 64800, "Value out of range in numeric read. Value re"
		"ad for the numeric item is too big or too small. Read only t"
		"he values that fit in the range of the numeric type being re"
		"ad. ", (ftnlen)800, (ftnlen)168);
	s_copy(messge + 65600, "The value of IOSTAT was 982.  No explanation"
		" is provided in the HP documentation for this value of IOSTA"
		"T. ", (ftnlen)800, (ftnlen)107);
	s_copy(messge + 66400, "The value of IOSTAT was 983.  No explanation"
		" is provided in the HP documentation for this value of IOSTA"
		"T. ", (ftnlen)800, (ftnlen)107);
	s_copy(messge + 67200, "The value of IOSTAT was 984.  No explanation"
		" is provided in the HP documentation for this value of IOSTA"
		"T. ", (ftnlen)800, (ftnlen)107);
	s_copy(messge + 68000, "The value of IOSTAT was 985.  No explanation"
		" is provided in the HP documentation for this value of IOSTA"
		"T. ", (ftnlen)800, (ftnlen)107);
	s_copy(messge + 68800, "The value of IOSTAT was 986.  No explanation"
		" is provided in the HP documentation for this value of IOSTA"
		"T. ", (ftnlen)800, (ftnlen)107);
	s_copy(messge + 69600, "The value of IOSTAT was 987.  No explanation"
		" is provided in the HP documentation for this value of IOSTA"
		"T. ", (ftnlen)800, (ftnlen)107);
	s_copy(messge + 70400, "The value of IOSTAT was 988.  No explanation"
		" is provided in the HP documentation for this value of IOSTA"
		"T. ", (ftnlen)800, (ftnlen)107);
	s_copy(messge + 71200, "`Illegal FORTRAN NLS call: FORTRAN source co"
		"de must be compiled with -Y. The FORTRAN source file was not"
		" compiled with the -Y option and NLS features were used. The"
		" problem is critical enough that program execution cannot co"
		"ntinue. ", (ftnlen)800, (ftnlen)232);
    } else if (sgi) {
	lbnd = 99;
	ubnd = 169;
	s_copy(messge, "error in format ", (ftnlen)800, (ftnlen)16);
	s_copy(messge + 800, "out of space for unit table ", (ftnlen)800, (
		ftnlen)28);
	s_copy(messge + 1600, "formatted i/o not allowed ", (ftnlen)800, (
		ftnlen)26);
	s_copy(messge + 2400, "unformatted i/o not allowed ", (ftnlen)800, (
		ftnlen)28);
	s_copy(messge + 3200, "direct i/o not allowed ", (ftnlen)800, (ftnlen)
		23);
	s_copy(messge + 4000, "sequential i/o not allowed ", (ftnlen)800, (
		ftnlen)27);
	s_copy(messge + 4800, "can't backspace file ", (ftnlen)800, (ftnlen)
		21);
	s_copy(messge + 5600, "null file name ", (ftnlen)800, (ftnlen)15);
	s_copy(messge + 6400, "can't stat file ", (ftnlen)800, (ftnlen)16);
	s_copy(messge + 7200, "unit not connected ", (ftnlen)800, (ftnlen)19);
	s_copy(messge + 8000, "off end of record ", (ftnlen)800, (ftnlen)18);
	s_copy(messge + 8800, "truncation failed in end file ", (ftnlen)800, (
		ftnlen)30);
	s_copy(messge + 9600, "incomprehensible list input ", (ftnlen)800, (
		ftnlen)28);
	s_copy(messge + 10400, "out of free space ", (ftnlen)800, (ftnlen)18);
	s_copy(messge + 11200, "unit not connected ", (ftnlen)800, (ftnlen)19)
		;
	s_copy(messge + 12000, "read unexpected character ", (ftnlen)800, (
		ftnlen)26);
	s_copy(messge + 12800, "blank logical input field ", (ftnlen)800, (
		ftnlen)26);
	s_copy(messge + 13600, "bad variable type ", (ftnlen)800, (ftnlen)18);
	s_copy(messge + 14400, "bad namelist name ", (ftnlen)800, (ftnlen)18);
	s_copy(messge + 15200, "variable not in namelist ", (ftnlen)800, (
		ftnlen)25);
	s_copy(messge + 16000, "no end record ", (ftnlen)800, (ftnlen)14);
	s_copy(messge + 16800, "namelist subscript out of range ", (ftnlen)
		800, (ftnlen)32);
	s_copy(messge + 17600, "negative repeat count ", (ftnlen)800, (ftnlen)
		22);
	s_copy(messge + 18400, "illegal operation for unit ", (ftnlen)800, (
		ftnlen)27);
	s_copy(messge + 19200, "off beginning of record ", (ftnlen)800, (
		ftnlen)24);
	s_copy(messge + 20000, "no * after repeat count ", (ftnlen)800, (
		ftnlen)24);
	s_copy(messge + 20800, "'new' file exists ", (ftnlen)800, (ftnlen)18);
	s_copy(messge + 21600, "can't find 'old' file ", (ftnlen)800, (ftnlen)
		22);
	s_copy(messge + 22400, "unknown system error ", (ftnlen)800, (ftnlen)
		21);
	s_copy(messge + 23200, "requires seek ability ", (ftnlen)800, (ftnlen)
		22);
	s_copy(messge + 24000, "illegal argument ", (ftnlen)800, (ftnlen)17);
	s_copy(messge + 24800, "duplicate key value on write ", (ftnlen)800, (
		ftnlen)29);
	s_copy(messge + 25600, "indexed file not open ", (ftnlen)800, (ftnlen)
		22);
	s_copy(messge + 26400, "bad isam argument ", (ftnlen)800, (ftnlen)18);
	s_copy(messge + 27200, "bad key description ", (ftnlen)800, (ftnlen)
		20);
	s_copy(messge + 28000, "too many open indexed files ", (ftnlen)800, (
		ftnlen)28);
	s_copy(messge + 28800, "corrupted isam file ", (ftnlen)800, (ftnlen)
		20);
	s_copy(messge + 29600, "isam file not opened for exclusive access ", (
		ftnlen)800, (ftnlen)42);
	s_copy(messge + 30400, "record locked ", (ftnlen)800, (ftnlen)14);
	s_copy(messge + 31200, "key already exists ", (ftnlen)800, (ftnlen)19)
		;
	s_copy(messge + 32000, "cannot delete primary key ", (ftnlen)800, (
		ftnlen)26);
	s_copy(messge + 32800, "beginning or end of file reached ", (ftnlen)
		800, (ftnlen)33);
	s_copy(messge + 33600, "cannot find requested record ", (ftnlen)800, (
		ftnlen)29);
	s_copy(messge + 34400, "current record not defined ", (ftnlen)800, (
		ftnlen)27);
	s_copy(messge + 35200, "isam file is exclusively locked ", (ftnlen)
		800, (ftnlen)32);
	s_copy(messge + 36000, "filename too long ", (ftnlen)800, (ftnlen)18);
	s_copy(messge + 36800, "cannot create lock file ", (ftnlen)800, (
		ftnlen)24);
	s_copy(messge + 37600, "record too long ", (ftnlen)800, (ftnlen)16);
	s_copy(messge + 38400, "key structure does not match file structure ",
		 (ftnlen)800, (ftnlen)44);
	s_copy(messge + 39200, "direct access on an indexed file not allowed "
		, (ftnlen)800, (ftnlen)45);
	s_copy(messge + 40000, "keyed access on a sequential file not allowe"
		"d ", (ftnlen)800, (ftnlen)46);
	s_copy(messge + 40800, "keyed access on a relative file not allowed ",
		 (ftnlen)800, (ftnlen)44);
	s_copy(messge + 41600, "append access on an indexed file not allowed "
		, (ftnlen)800, (ftnlen)45);
	s_copy(messge + 42400, "must specify record length ", (ftnlen)800, (
		ftnlen)27);
	s_copy(messge + 43200, "key field value type does not match key type "
		, (ftnlen)800, (ftnlen)45);
	s_copy(messge + 44000, "character key field value length too long ", (
		ftnlen)800, (ftnlen)42);
	s_copy(messge + 44800, "fixed record on sequential file not allowed ",
		 (ftnlen)800, (ftnlen)44);
	s_copy(messge + 45600, "variable records allowed only on unformatted"
		" sequential file ", (ftnlen)800, (ftnlen)61);
	s_copy(messge + 46400, "stream records allowed only on formatted seq"
		"uential file ", (ftnlen)800, (ftnlen)57);
	s_copy(messge + 47200, "maximum number of records in direct access f"
		"ile exceeded ", (ftnlen)800, (ftnlen)57);
	s_copy(messge + 48000, "attempt to write to a readonly file ", (
		ftnlen)800, (ftnlen)36);
	s_copy(messge + 48800, "must specify key descriptions ", (ftnlen)800, 
		(ftnlen)30);
	s_copy(messge + 49600, "carriage control not allowed for unformatted"
		" units ", (ftnlen)800, (ftnlen)51);
	s_copy(messge + 50400, "indexed files only ", (ftnlen)800, (ftnlen)19)
		;
	s_copy(messge + 51200, "cannot use on indexed file ", (ftnlen)800, (
		ftnlen)27);
	s_copy(messge + 52000, "cannot use on indexed or append file ", (
		ftnlen)800, (ftnlen)37);
	s_copy(messge + 52800, "error in closing file ", (ftnlen)800, (ftnlen)
		22);
	s_copy(messge + 53600, "invalid code in format specification ", (
		ftnlen)800, (ftnlen)37);
	s_copy(messge + 54400, "invalid record number in direct access file ",
		 (ftnlen)800, (ftnlen)44);
	s_copy(messge + 55200, "cannot have endfile record on non-sequential"
		" file ", (ftnlen)800, (ftnlen)50);
    } else if (vax) {
	lbnd = 0;
	ubnd = 68;
	s_copy(messge, "Not a Fortran-specific error. ", (ftnlen)800, (ftnlen)
		30);
	s_copy(messge + 800, "No diagnostics are available other than the va"
		"lue of IOSTAT is 2 ", (ftnlen)800, (ftnlen)65);
	s_copy(messge + 1600, "No diagnostics are available other than the v"
		"alue of IOSTAT is 3 ", (ftnlen)800, (ftnlen)65);
	s_copy(messge + 2400, "No diagnostics are available other than the v"
		"alue of IOSTAT is 4 ", (ftnlen)800, (ftnlen)65);
	s_copy(messge + 3200, "No diagnostics are available other than the v"
		"alue of IOSTAT is 5 ", (ftnlen)800, (ftnlen)65);
	s_copy(messge + 4000, "No diagnostics are available other than the v"
		"alue of IOSTAT is 6 ", (ftnlen)800, (ftnlen)65);
	s_copy(messge + 4800, "No diagnostics are available other than the v"
		"alue of IOSTAT is 7 ", (ftnlen)800, (ftnlen)65);
	s_copy(messge + 5600, "No diagnostics are available other than the v"
		"alue of IOSTAT is 8 ", (ftnlen)800, (ftnlen)65);
	s_copy(messge + 6400, "No diagnostics are available other than the v"
		"alue of IOSTAT is 9 ", (ftnlen)800, (ftnlen)65);
	s_copy(messge + 7200, "No diagnostics are available other than the v"
		"alue of IOSTAT is 10 ", (ftnlen)800, (ftnlen)66);
	s_copy(messge + 8000, "No diagnostics are available other than the v"
		"alue of IOSTAT is 11 ", (ftnlen)800, (ftnlen)66);
	s_copy(messge + 8800, "No diagnostics are available other than the v"
		"alue of IOSTAT is 12 ", (ftnlen)800, (ftnlen)66);
	s_copy(messge + 9600, "No diagnostics are available other than the v"
		"alue of IOSTAT is 13 ", (ftnlen)800, (ftnlen)66);
	s_copy(messge + 10400, "No diagnostics are available other than the "
		"value of IOSTAT is 14 ", (ftnlen)800, (ftnlen)66);
	s_copy(messge + 11200, "No diagnostics are available other than the "
		"value of IOSTAT is 15 ", (ftnlen)800, (ftnlen)66);
	s_copy(messge + 12000, "No diagnostics are available other than the "
		"value of IOSTAT is 16 ", (ftnlen)800, (ftnlen)66);
	s_copy(messge + 12800, "Syntax error in NAMELIST input. ", (ftnlen)
		800, (ftnlen)32);
	s_copy(messge + 13600, "Too many values for NAMELIST variable. ", (
		ftnlen)800, (ftnlen)39);
	s_copy(messge + 14400, "Invalid reference to variable in NAMELIST in"
		"put. ", (ftnlen)800, (ftnlen)49);
	s_copy(messge + 15200, "REWIND error. ", (ftnlen)800, (ftnlen)14);
	s_copy(messge + 16000, "Duplicate file specifications. ", (ftnlen)800,
		 (ftnlen)31);
	s_copy(messge + 16800, "Input record too long. ", (ftnlen)800, (
		ftnlen)23);
	s_copy(messge + 17600, "BACKSPACE error ", (ftnlen)800, (ftnlen)16);
	s_copy(messge + 18400, "End-of-file during read. ", (ftnlen)800, (
		ftnlen)25);
	s_copy(messge + 19200, "Record number outside range. ", (ftnlen)800, (
		ftnlen)29);
	s_copy(messge + 20000, "OPEN or DEFINE FILE required. ", (ftnlen)800, 
		(ftnlen)30);
	s_copy(messge + 20800, "Too many records in IO statement. ", (ftnlen)
		800, (ftnlen)34);
	s_copy(messge + 21600, "CLOSE error. ", (ftnlen)800, (ftnlen)13);
	s_copy(messge + 22400, "File not found. ", (ftnlen)800, (ftnlen)16);
	s_copy(messge + 23200, "Open failure. ", (ftnlen)800, (ftnlen)14);
	s_copy(messge + 24000, "Mixed file access modes. ", (ftnlen)800, (
		ftnlen)25);
	s_copy(messge + 24800, "Invalid logical unit number. ", (ftnlen)800, (
		ftnlen)29);
	s_copy(messge + 25600, "ENDFILE error. ", (ftnlen)800, (ftnlen)15);
	s_copy(messge + 26400, "Unit already open. ", (ftnlen)800, (ftnlen)19)
		;
	s_copy(messge + 27200, "Segmented record format error. ", (ftnlen)800,
		 (ftnlen)31);
	s_copy(messge + 28000, "Attempt to access non-existent record. ", (
		ftnlen)800, (ftnlen)39);
	s_copy(messge + 28800, "Inconsistent record length. ", (ftnlen)800, (
		ftnlen)28);
	s_copy(messge + 29600, "Error during write. ", (ftnlen)800, (ftnlen)
		20);
	s_copy(messge + 30400, "Error during read. ", (ftnlen)800, (ftnlen)19)
		;
	s_copy(messge + 31200, "Recursive IO operation. ", (ftnlen)800, (
		ftnlen)24);
	s_copy(messge + 32000, "Insufficient virtual memory. ", (ftnlen)800, (
		ftnlen)29);
	s_copy(messge + 32800, "No such device. ", (ftnlen)800, (ftnlen)16);
	s_copy(messge + 33600, "File name specification error. ", (ftnlen)800,
		 (ftnlen)31);
	s_copy(messge + 34400, "Inconsistent record type. ", (ftnlen)800, (
		ftnlen)26);
	s_copy(messge + 35200, "Keyword value error in OPEN statement. ", (
		ftnlen)800, (ftnlen)39);
	s_copy(messge + 36000, "Inconsistent OPENCLOSE parameters. ", (ftnlen)
		800, (ftnlen)35);
	s_copy(messge + 36800, "Write to READONLY file. ", (ftnlen)800, (
		ftnlen)24);
	s_copy(messge + 37600, "Invalid argument to Fortran Run-Time Library"
		". ", (ftnlen)800, (ftnlen)46);
	s_copy(messge + 38400, "Invalid key specification. ", (ftnlen)800, (
		ftnlen)27);
	s_copy(messge + 39200, "Inconsistent key change or duplicate key. ", (
		ftnlen)800, (ftnlen)42);
	s_copy(messge + 40000, "Inconsistent file organization. ", (ftnlen)
		800, (ftnlen)32);
	s_copy(messge + 40800, "Specified record locked. ", (ftnlen)800, (
		ftnlen)25);
	s_copy(messge + 41600, "No current record. ", (ftnlen)800, (ftnlen)19)
		;
	s_copy(messge + 42400, "REWRITE error. ", (ftnlen)800, (ftnlen)15);
	s_copy(messge + 43200, "DELETE error. ", (ftnlen)800, (ftnlen)14);
	s_copy(messge + 44000, "UNLOCK error. ", (ftnlen)800, (ftnlen)14);
	s_copy(messge + 44800, "FIND error. ", (ftnlen)800, (ftnlen)12);
	s_copy(messge + 45600, "No diagnostics are available other than the "
		"value of IOSTAT is 58 ", (ftnlen)800, (ftnlen)66);
	s_copy(messge + 46400, "List-directed IO syntax error. ", (ftnlen)800,
		 (ftnlen)31);
	s_copy(messge + 47200, "Infinite format loop. ", (ftnlen)800, (ftnlen)
		22);
	s_copy(messge + 48000, "Formatvariable-type mismatch. ", (ftnlen)800, 
		(ftnlen)30);
	s_copy(messge + 48800, "Syntax error in format. ", (ftnlen)800, (
		ftnlen)24);
	s_copy(messge + 49600, "Output conversion error. ", (ftnlen)800, (
		ftnlen)25);
	s_copy(messge + 50400, "Input conversion error. ", (ftnlen)800, (
		ftnlen)24);
	s_copy(messge + 51200, "No diagnostics are available other than the "
		"value of IOSTAT is 65 ", (ftnlen)800, (ftnlen)66);
	s_copy(messge + 52000, "Output statement overflows record. ", (ftnlen)
		800, (ftnlen)35);
	s_copy(messge + 52800, "Input statement requires too much data. ", (
		ftnlen)800, (ftnlen)40);
	s_copy(messge + 53600, "Variable format expression value error. ", (
		ftnlen)800, (ftnlen)40);
    } else if (pc) {
	lbnd = 2;
	ubnd = 1;
    } else {
	lbnd = 2;
	ubnd = 1;
    }
    if (*iostat > lbnd && *iostat <= ubnd) {
	s_copy(diagns, messge + ((i__1 = *iostat - lbnd - 1) < 90 && 0 <= 
		i__1 ? i__1 : s_rnge("messge", i__1, "dcyphr_", (ftnlen)1120))
		 * 800, diagns_len, (ftnlen)800);
	*found = TRUE_;
    } else {
	s_copy(diagns, "The value of IOSTAT was #.  The meaning of this valu"
		"e is not available via the SPICE system. Please consult your"
		" FORTRAN manual for the meaning of this code.", diagns_len, (
		ftnlen)157);
	repmi_(diagns, "#", iostat, diagns, diagns_len, (ftnlen)1, diagns_len)
		;
	*found = FALSE_;
    }
    return 0;
} /* dcyphr_ */

