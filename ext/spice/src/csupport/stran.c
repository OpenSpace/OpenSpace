/* stran.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;
static integer c__12 = 12;

/* $Procedure     STRAN */
/* Subroutine */ int stran_0_(int n__, char *input, char *output, logical *
	tran, ftnlen input_len, ftnlen output_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_indx(char *, char *, 
	    ftnlen, ftnlen), s_rnge(char *, integer, char *, integer), i_len(
	    char *, ftnlen);

    /* Local variables */
    static integer ldef, leno, vdim, slot, lout, lsym, ptrs[810], i__, j;
    extern integer cardc_(char *, ftnlen);
    static integer l, n;
    static logical check[200];
    extern logical batch_(void);
    static integer place;
    extern /* Subroutine */ int lcase_(char *, char *, ftnlen, ftnlen);
    static char delim[1];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static integer nname;
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    static char names[32*206];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen), 
	    geteq_(char *, ftnlen);
    extern integer ncpos_(char *, char *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int nthwd_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen);
    static char symbl[33];
    static integer psize;
    extern integer rtrim_(char *, ftnlen);
    static logical checkd[200];
    extern logical failed_(void);
    static char alphab[32];
    extern /* Subroutine */ int getdel_(char *, ftnlen);
    extern logical matchm_(char *, char *, char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen);
    static char buffer[256*52];
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen), 
	    lastnb_(char *, ftnlen);
    static logical gotone;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), repsub_(char *, integer *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen);
    static char equote[1];
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    static char resvrd[32*12], symbol[33], pattrn[80];
    static integer nxtchr;
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen), rdstmn_(char *, char *, char *, ftnlen, ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int sbget_1__(char *, char *, integer *, char *, 
	    char *, integer *, ftnlen, ftnlen, ftnlen, ftnlen), nthuqw_(char *
	    , integer *, char *, char *, integer *, ftnlen, ftnlen, ftnlen);
    static char myprmt[80];
    extern /* Subroutine */ int sbrem_1__(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    static integer lsttry;
    extern /* Subroutine */ int sbset_1__(char *, char *, char *, integer *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen);
    static char def[1024];
    static integer loc;
    static char key[32];
    static logical new__;
    extern /* Subroutine */ int sbinit_1__(integer *, integer *, integer *, 
	    char *, integer *, char *, ftnlen, ftnlen);

/* $ Abstract */

/*     Translate the symbols in an input string. */

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

/* $ Keywords */

/*     PARSE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     INPUT      I   Input string containing symbols to be translated. */
/*     OUTPUT     O   Output string, with all symbols translated. */

/* $ Detailed_Input */

/*     INPUT      is the input string to be translated. INPUT may contain */
/*                any number of known symbols. */


/* $ Detailed_Output */

/*     OUTPUT     is the translation of the input string. The first */
/*                of the symbols in INPUT will have been translated. */
/*                When INPUT is either a DEFINE or an UNDEFINE command, */
/*                OUTPUT is blank. */

/*                OUTPUT may overwrite INPUT. */

/* $ Input_Files */

/*     None. */

/* $ Output_Files */

/*     None. */

/* $ Input_Output_Common */

/*     None. */

/* $ Exceptions */

/*     The following exceptions are detected by this routine: */

/*     1)  Attempt to define or undefine a symbol that does */
/*         not begin with a letter. */

/*     2)  Attempt to define or undefine a symbol that ends with */
/*         a question mark '?' . */

/*     3)  Failure to specify a symbol to define or undefine. */

/*     4)  Attempting to define a reserved word.  The reserved */
/*         words are: */

/*            'START' */
/*            'STOP' */
/*            'EXIT' */
/*            'INQUIRE' */
/*            'SHOW' */
/*            'DEFINE' */
/*            'SHOW' */
/*            'UNDEFINE' */
/*            'HELP' */

/*      In all of the above cases OUTPUT is set to blank and TRAN to */
/*      FALSE.  No new symbol is placed in the table of symbol */
/*      definitions. */

/*      In all of these cases the error BAD_SYMBOL_SPC is signalled. */

/*      5) Recursive symbol definitions are detected and disallowed. */
/*         A long error message diagnosing the problem is set and */
/*         the error RECURSIVE_SYMBOL is signalled. */

/*      5) Overflow of the input command caused by symbol resolution. */

/*         In this case the OUTPUT is left at the state it had reached */
/*         prior to the overflow condition and TRAN is returned as */
/*         FALSE. The error SYMBOL_OVERFLOW is signalled. */

/* $ Detailed_Description */

/*     A new symbol may be defined with the DEFINE command. The */
/*     syntax is: */

/*            DEFINE  <symbol>  <definition> */

/*     where <symbol> is a valid symbol name and <definition> is any */
/*     valid definition. The DEFINE command, the symbol name, and the */
/*     definition are delimited by blanks. */

/*     When a symbol is defined, the symbol and definition are inserted */
/*     into the symbol table. */

/*     An existing symbol may be removed from the table with the */
/*     UNDEFINE command. The syntax is: */

/*            UNDEFINE <symbol> */

/*     where <symbol> is the name of an existing symbol. The UNDEFINE */
/*     command and the symbol name are delimited by blanks. */

/*     If the input string does not contain a definition statement, */
/*     STRANS searches the input string for potential symbol names. */
/*     When a valid symbol is encountered, it is removed from the */
/*     string and replaced by the corresponding definition. This */
/*     continues until no untranslated symbols remain. */

/* $ Examples */

/*     Suppose that we are given the following definitions: */

/*            DEFINE  BODIES      PLANET AND SATS */
/*            DEFINE  EUROPA      502 */
/*            DEFINE  GANYMEDE    503 */
/*            DEFINE  IO          501 */
/*            DEFINE  JUPITER     599 */
/*            DEFINE  PLANET      JUPITER */
/*            DEFINE  CALLISTO    504 */
/*            DEFINE  SATS        IO EUROPA GANYMEDE CALLISTO */

/*      Then the string 'BODIES AND SOULS' would translate, */
/*      at various stages, to: */

/*           'PLANET AND SATS AND SOULS' */

/*           'JUPITER AND SATS AND SOULS' */

/*           '599 AND SATS AND SOULS' */

/*           '599 AND IO EUROPA GANYMEDE CALLISTO AND SOULS' */

/*           '599 AND 501 EUROPA GANYMEDE CALLISTO AND SOULS' */

/*           '599 AND 501 502 GANYMEDE CALLISTO AND SOULS' */

/*           '599 AND 501 502 503 CALLISTO AND SOULS' */

/*           '599 AND 501 502 503 504 AND SOULS' */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     I. M. Underwood (JPL) */

/* $ Version_and_Date */

/*     Version 1.2.0 29-Aug-1996 (WLT) */

/*        Fixed the error message for the case in which someone */
/*        tries to create a symbol that is more than 32 characters */
/*        in length. */

/*     Version 1.1, 14-SEP-1995 */

/*        Reference to unused variable WORD deleted. */

/*     Version 1,    8-SEP-1986 */

/* -& */
/*     SPICELIB Functions */


/*     Other supporting functions */


/*     The following parameters are used to define our table */
/*     of symbol translations. */


/*     Longest allowed symbol name is given by WDSIZE */


/*     Maximum number of allowed symbols is MAXN */


/*     The longest we expect any symbol to be is MAXL characters */


/*     The average number of characters per symbol is AVGL */


/*     Finally, here are the arrays used to hold the symbol translations. */


/*     Here's the storage we need for the reserved words. */

    switch(n__) {
	case 1: goto L_sympat;
	case 2: goto L_symget;
	}


/*     Set up all of the data structures and special strings in */
/*     the first pass through the routine. */

    if (return_()) {
	return 0;
    }
    chkin_("STRAN", (ftnlen)5);
    if (first) {
	first = FALSE_;
	vdim = 51;
	psize = 804;
	nname = 200;
	sbinit_1__(&nname, &psize, &vdim, names, ptrs, buffer, (ftnlen)32, (
		ftnlen)256);
	s_copy(resvrd, "START", (ftnlen)32, (ftnlen)5);
	s_copy(resvrd + 32, "STOP", (ftnlen)32, (ftnlen)4);
	s_copy(resvrd + 64, "EXIT", (ftnlen)32, (ftnlen)4);
	s_copy(resvrd + 96, "INQUIRE", (ftnlen)32, (ftnlen)7);
	s_copy(resvrd + 128, "SHOW", (ftnlen)32, (ftnlen)4);
	s_copy(resvrd + 160, "DEFINE", (ftnlen)32, (ftnlen)6);
	s_copy(resvrd + 192, "SHOW", (ftnlen)32, (ftnlen)4);
	s_copy(resvrd + 224, "UNDEFINE", (ftnlen)32, (ftnlen)8);
	s_copy(resvrd + 256, "HELP", (ftnlen)32, (ftnlen)4);
	s_copy(resvrd + 288, "RECALL", (ftnlen)32, (ftnlen)6);
	s_copy(resvrd + 320, "DO", (ftnlen)32, (ftnlen)2);
	s_copy(resvrd + 352, "EDIT", (ftnlen)32, (ftnlen)4);
	s_copy(alphab, "ABCDEFGHIJKLMNOPQRSTUVWXYZ", (ftnlen)32, (ftnlen)26);
    }

/*     Find out what the special marker character is for suppressing */
/*     symbol evaluation. */

    geteq_(equote, (ftnlen)1);

/*     Is this a definition statement? The presence of DEFINE, INQUIRE or */
/*     UNDEFINE at the beginning of the string will confirm this. */

    nthwd_(input, &c__1, key, &loc, input_len, (ftnlen)32);
    ucase_(key, key, (ftnlen)32, (ftnlen)32);

/*     The keyword must be followed by a valid symbol name. */

    if (s_cmp(key, "DEFINE", (ftnlen)32, (ftnlen)6) == 0 || s_cmp(key, "INQU"
	    "IRE", (ftnlen)32, (ftnlen)7) == 0 || s_cmp(key, "UNDEFINE", (
	    ftnlen)32, (ftnlen)8) == 0) {
	nthwd_(input, &c__2, symbl, &loc, input_len, (ftnlen)33);
	ucase_(symbl, symbol, (ftnlen)33, (ftnlen)33);
	l = rtrim_(symbol, (ftnlen)33);
	if (s_cmp(symbol, " ", (ftnlen)33, (ftnlen)1) == 0) {
	    s_copy(output, " ", output_len, (ftnlen)1);
	    *tran = FALSE_;
	    setmsg_("The \"#\" command must be followed by the name of the s"
		    "ymbol that you want to #. ", (ftnlen)79);
	    errch_("#", key, (ftnlen)1, (ftnlen)32);
	    lcase_(key, key, (ftnlen)32, (ftnlen)32);
	    errch_("#", key, (ftnlen)1, (ftnlen)32);
	    sigerr_("BAD_SYMBOL_SPEC", (ftnlen)15);
	    chkout_("STRAN", (ftnlen)5);
	    return 0;
	} else if (i_indx(alphab, symbol, (ftnlen)32, (ftnlen)1) == 0) {
	    s_copy(output, " ", output_len, (ftnlen)1);
	    *tran = FALSE_;
	    lcase_(key, key, (ftnlen)32, (ftnlen)32);
	    setmsg_("You cannot # \"#\".  Symbols must begin with a letter ("
		    "A-Z) ", (ftnlen)58);
	    errch_("#", key, (ftnlen)1, (ftnlen)32);
	    errch_("#", symbol, (ftnlen)1, (ftnlen)33);
	    sigerr_("BAD_SYMBOL_SPEC", (ftnlen)15);
	    chkout_("STRAN", (ftnlen)5);
	    return 0;
	} else if (l > 32) {
	    s_copy(output, " ", output_len, (ftnlen)1);
	    *tran = FALSE_;
	    lcase_(key, key, (ftnlen)32, (ftnlen)32);
	    setmsg_("You cannot # \"#...\".  Symbols may not be longer than "
		    "32 characters in length.", (ftnlen)77);
	    errch_("#", key, (ftnlen)1, (ftnlen)32);
	    errch_("#", symbol, (ftnlen)1, (ftnlen)33);
	    sigerr_("BAD_SYMBOL_SPEC", (ftnlen)15);
	    chkout_("STRAN", (ftnlen)5);
	    return 0;
	} else if (*(unsigned char *)&symbol[l - 1] == '?') {
	    s_copy(output, " ", output_len, (ftnlen)1);
	    *tran = FALSE_;
	    lcase_(key, key, (ftnlen)32, (ftnlen)32);
	    setmsg_("You cannot # \"#\".  Symbols may not end with a questio"
		    "n mark '?'. ", (ftnlen)65);
	    errch_("#", key, (ftnlen)1, (ftnlen)32);
	    errch_("#", symbol, (ftnlen)1, (ftnlen)33);
	    sigerr_("BAD_SYMBOL_SPEC", (ftnlen)15);
	    chkout_("STRAN", (ftnlen)5);
	    return 0;
	} else if ((s_cmp(key, "DEFINE", (ftnlen)32, (ftnlen)6) == 0 || s_cmp(
		key, "INQUIRE", (ftnlen)32, (ftnlen)7) == 0) && isrchc_(
		symbol, &c__12, resvrd, (ftnlen)33, (ftnlen)32) > 0) {
	    s_copy(output, " ", output_len, (ftnlen)1);
	    *tran = FALSE_;
	    setmsg_("The word '#' is a reserved word. You may not redefine i"
		    "t. ", (ftnlen)58);
	    errch_("#", symbol, (ftnlen)1, (ftnlen)33);
	    sigerr_("BAD_SYMBOL_SPEC", (ftnlen)15);
	    chkout_("STRAN", (ftnlen)5);
	    return 0;
	}
    }
    if (s_cmp(key, "INQUIRE", (ftnlen)32, (ftnlen)7) == 0) {

/*        First of all we, can only INQUIRE for symbol definitions */
/*        if the program is not running in "batch" mode. */

	if (batch_()) {
	    setmsg_("You've attempted to INQUIRE for the value of a symbol w"
		    "hile the program is running in \"batch\" mode. You can I"
		    "NQUIRE for a symbol value only if you are running in INT"
		    "ERACTIVE mode. ", (ftnlen)180);
	    sigerr_("WRONG_MODE", (ftnlen)10);
	    chkout_("STRAN", (ftnlen)5);
	    return 0;
	}

/*        See if there is anything following the symbol that is */
/*        to be defined.  This will be used as our prompt value. */

/* Computing MAX */
	i__3 = loc + l;
	i__1 = loc + l, i__2 = ncpos_(input, " ", &i__3, input_len, (ftnlen)1)
		;
	nxtchr = max(i__1,i__2);
	if (s_cmp(input + (nxtchr - 1), " ", input_len - (nxtchr - 1), (
		ftnlen)1) != 0) {
	    s_copy(myprmt, input + (nxtchr - 1), (ftnlen)80, input_len - (
		    nxtchr - 1));
	} else {
	    s_copy(myprmt, "Enter definition for", (ftnlen)80, (ftnlen)20);
	    suffix_(symbol, &c__1, myprmt, (ftnlen)33, (ftnlen)80);
	    suffix_(">", &c__1, myprmt, (ftnlen)1, (ftnlen)80);
	}
	getdel_(delim, (ftnlen)1);
	rdstmn_(myprmt, delim, def, (ftnlen)80, (ftnlen)1, (ftnlen)1024);
	sbset_1__(symbol, def, names, ptrs, buffer, (ftnlen)33, (ftnlen)1024, 
		(ftnlen)32, (ftnlen)256);
    }

/*     If this is a definition, and the symbol already exists in the */
/*     symbol table, simply replace the existing definition with the */
/*     string following the symbol name. If this is a new symbol, */
/*     find the first symbol in the list that should follow the new */
/*     one. Move the rest of the symbols back, and insert the new one */
/*     at this point. */

    if (s_cmp(key, "DEFINE", (ftnlen)32, (ftnlen)6) == 0) {
/* Computing MAX */
	i__3 = loc + l;
	i__1 = loc + l, i__2 = ncpos_(input, " ", &i__3, input_len, (ftnlen)1)
		;
	nxtchr = max(i__1,i__2);
	sbset_1__(symbol, input + (nxtchr - 1), names, ptrs, buffer, (ftnlen)
		33, input_len - (nxtchr - 1), (ftnlen)32, (ftnlen)256);
    }
    if (s_cmp(key, "DEFINE", (ftnlen)32, (ftnlen)6) == 0 || s_cmp(key, "INQU"
	    "IRE", (ftnlen)32, (ftnlen)7) == 0) {
	if (failed_()) {
	    chkout_("STRAN", (ftnlen)5);
	    return 0;
	}

/*        Now check for a recursive definition.  To do this we have */
/*        two parallel arrays to the NAMES array of the string */
/*        buffer.  The first array CHECK is used to indicate that */
/*        in the course of the definition resolution of the */
/*        new symbol, another symbol shows up.  The second array */
/*        called CHECKD indicats whether or not we have examined this */
/*        existing symbol to see if contains the newly created */
/*        symbol as part of its definition. */

/*        So far we have nothing to check and haven't checked anything. */

	n = cardc_(names, (ftnlen)32);
	i__1 = n;
	for (j = 1; j <= i__1; ++j) {
	    check[(i__2 = j - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge("check", 
		    i__2, "stran_", (ftnlen)545)] = FALSE_;
	    checkd[(i__2 = j - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge("checkd",
		     i__2, "stran_", (ftnlen)546)] = FALSE_;
	}

/*        Find the location of our new symbol in the NAMES cell. */

	place = isrchc_(symbol, &n, names + 192, (ftnlen)33, (ftnlen)32);
	new__ = TRUE_;
	while(new__) {

/*           Look up the definition currently associated with */
/*           the symbol we are checking. */

	    sbget_1__(symbol, names, ptrs, buffer, def, &i__, (ftnlen)33, (
		    ftnlen)32, (ftnlen)256, (ftnlen)1024);
	    j = 1;
	    nthuqw_(def, &j, equote, symbol, &loc, (ftnlen)1024, (ftnlen)1, (
		    ftnlen)33);
	    while(loc > 0) {
		ucase_(symbol, symbol, (ftnlen)33, (ftnlen)33);
		slot = isrchc_(symbol, &n, names + 192, (ftnlen)33, (ftnlen)
			32);

/*              If the word is located in the same place as the */
/*              symbol we've just defined, we've introduced */
/*              a recursive symbol definition.  Remove this */
/*              symbol and diagnose the error. */

		if (slot == place) {
		    s_copy(output, " ", output_len, (ftnlen)1);
		    *tran = FALSE_;
		    s_copy(symbol, names + (((i__1 = place + 5) < 206 && 0 <= 
			    i__1 ? i__1 : s_rnge("names", i__1, "stran_", (
			    ftnlen)582)) << 5), (ftnlen)33, (ftnlen)32);
		    sbrem_1__(symbol, names, ptrs, buffer, (ftnlen)33, (
			    ftnlen)32, (ftnlen)256);
		    setmsg_("The definition of '#' is recursive.  Recursivel"
			    "y defined symbol definitions are not allowed. ", (
			    ftnlen)93);
		    errch_("#", symbol, (ftnlen)1, (ftnlen)33);
		    sigerr_("RECURSIVE_SYMBOL", (ftnlen)16);
		    chkout_("STRAN", (ftnlen)5);
		    return 0;
		} else if (slot > 0) {

/*                 Otherwise if this word is in the names list */
/*                 we may need to check this symbol to see if */
/*                 it lists the just defined symbol in its definition. */

		    if (checkd[(i__1 = slot - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("checkd", i__1, "stran_", (ftnlen)602)]) {
			check[(i__1 = slot - 1) < 200 && 0 <= i__1 ? i__1 : 
				s_rnge("check", i__1, "stran_", (ftnlen)603)] 
				= FALSE_;
		    } else {
			check[(i__1 = slot - 1) < 200 && 0 <= i__1 ? i__1 : 
				s_rnge("check", i__1, "stran_", (ftnlen)605)] 
				= TRUE_;
		    }
		}

/*              Locate the next unquoted word in the definition. */

		++j;
		nthuqw_(def, &j, equote, symbol, &loc, (ftnlen)1024, (ftnlen)
			1, (ftnlen)33);
	    }

/*           See if there are any new items to check.  If there */
/*           are create a new value for symbol, and mark the */
/*           new item as being checked. */

	    new__ = FALSE_;
	    i__1 = n;
	    for (j = 1; j <= i__1; ++j) {
		if (check[(i__2 = j - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"check", i__2, "stran_", (ftnlen)625)] && ! new__) {
		    s_copy(symbol, names + (((i__2 = j + 5) < 206 && 0 <= 
			    i__2 ? i__2 : s_rnge("names", i__2, "stran_", (
			    ftnlen)626)) << 5), (ftnlen)33, (ftnlen)32);
		    check[(i__2 = j - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			    "check", i__2, "stran_", (ftnlen)627)] = FALSE_;
		    checkd[(i__2 = j - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			    "checkd", i__2, "stran_", (ftnlen)628)] = TRUE_;
		    new__ = TRUE_;
		}
	    }
	}

/*        If we get to this point, we have a new non-recursively */
/*        defined symbol. */

	s_copy(output, " ", output_len, (ftnlen)1);
	*tran = FALSE_;
	chkout_("STRAN", (ftnlen)5);
	return 0;
    }

/*     If this is a deletion, and the symbol already exists in the */
/*     symbol table, simply move the symbols that follow toward the */
/*     front of the table. */

    if (s_cmp(key, "UNDEFINE", (ftnlen)32, (ftnlen)8) == 0) {
	sbrem_1__(symbol, names, ptrs, buffer, (ftnlen)33, (ftnlen)32, (
		ftnlen)256);
	s_copy(output, " ", output_len, (ftnlen)1);
	*tran = FALSE_;
	chkout_("STRAN", (ftnlen)5);
	return 0;
    }

/*     This is not a definition statement. Look for potential symbols. */
/*     Try to resolve the first symbol in the string by substituting the */
/*     corresponding definition for the existing symbol. */

    s_copy(output, input, output_len, input_len);
    *tran = FALSE_;
    j = 1;
    nthuqw_(output, &j, equote, symbol, &loc, output_len, (ftnlen)1, (ftnlen)
	    33);
    while(! (*tran) && s_cmp(symbol, " ", (ftnlen)33, (ftnlen)1) != 0) {
	ucase_(symbol, symbol, (ftnlen)33, (ftnlen)33);
	sbget_1__(symbol, names, ptrs, buffer, def, &i__, (ftnlen)33, (ftnlen)
		32, (ftnlen)256, (ftnlen)1024);
	if (i__ > 0) {
	    lsym = lastnb_(symbol, (ftnlen)33);
	    ldef = lastnb_(def, (ftnlen)1024) + 1;
	    lout = lastnb_(output, output_len);
	    leno = i_len(output, output_len);
	    if (lout - lsym + ldef > leno) {
		*tran = FALSE_;
		setmsg_("As a result of attempting to resolve the symbols in"
			" the input command, the command has overflowed the a"
			"llocated memory. This is may be due to unintentional"
			"ly using symbols that you had not intended to use.  "
			"You may protect portions of your string from symbol "
			"evaluation by enclosing that portion of your string "
			"between the character # as in 'DO #THIS PART WITHOUT"
			" SYMBOLS#' . ", (ftnlen)376);
		errch_("#", equote, (ftnlen)1, (ftnlen)1);
		errch_("#", equote, (ftnlen)1, (ftnlen)1);
		errch_("#", equote, (ftnlen)1, (ftnlen)1);
		sigerr_("SYMBOL_OVERFLOW", (ftnlen)15);
		chkout_("STRAN", (ftnlen)5);
		return 0;
	    }
	    i__1 = loc + lsym - 1;
	    repsub_(output, &loc, &i__1, def, output, output_len, ldef, 
		    output_len);
	    *tran = TRUE_;
	} else {
	    ++j;
	}
	nthuqw_(output, &j, equote, symbol, &loc, output_len, (ftnlen)1, (
		ftnlen)33);
    }
    chkout_("STRAN", (ftnlen)5);
    return 0;

/*     The following entry point allows us to set up a search */
/*     of defined symbols that match a wild-card pattern.  It must */
/*     be called prior to getting any symbol definitions. */


L_sympat:
    lsttry = 0;
    s_copy(pattrn, input, (ftnlen)80, input_len);
    return 0;

/*     The following entry point fetches the next symbol and its */
/*     definition for the next SYMBOL whose name */
/*     matches a previously supplied template via the entry point */
/*     above --- SYMPAT. */

/*     If there is no matching symbol, we get back blanks.  Note */
/*     that no translation of the definition is performed. */


L_symget:
    s_copy(input, " ", input_len, (ftnlen)1);
    s_copy(output, " ", output_len, (ftnlen)1);
    n = cardc_(names, (ftnlen)32);
    while(lsttry < n) {
	++lsttry;
	gotone = matchm_(names + (((i__1 = lsttry + 5) < 206 && 0 <= i__1 ? 
		i__1 : s_rnge("names", i__1, "stran_", (ftnlen)767)) << 5), 
		pattrn, "*", "%", "~", "|", (ftnlen)32, (ftnlen)80, (ftnlen)1,
		 (ftnlen)1, (ftnlen)1, (ftnlen)1);
	if (gotone) {
	    s_copy(symbol, names + (((i__1 = lsttry + 5) < 206 && 0 <= i__1 ? 
		    i__1 : s_rnge("names", i__1, "stran_", (ftnlen)771)) << 5)
		    , (ftnlen)33, (ftnlen)32);
	    s_copy(input, names + (((i__1 = lsttry + 5) < 206 && 0 <= i__1 ? 
		    i__1 : s_rnge("names", i__1, "stran_", (ftnlen)772)) << 5)
		    , input_len, (ftnlen)32);
	    sbget_1__(symbol, names, ptrs, buffer, output, &i__, (ftnlen)33, (
		    ftnlen)32, (ftnlen)256, output_len);
	    return 0;
	}
    }
    return 0;
} /* stran_ */

/* Subroutine */ int stran_(char *input, char *output, logical *tran, ftnlen 
	input_len, ftnlen output_len)
{
    return stran_0_(0, input, output, tran, input_len, output_len);
    }

/* Subroutine */ int sympat_(char *input, ftnlen input_len)
{
    return stran_0_(1, input, (char *)0, (logical *)0, input_len, (ftnint)0);
    }

/* Subroutine */ int symget_(char *input, char *output, ftnlen input_len, 
	ftnlen output_len)
{
    return stran_0_(2, input, output, (logical *)0, input_len, output_len);
    }

