/* zzphsh.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZPHSH (Private---kernel pool hash function) */
integer zzphsh_0_(int n__, char *word, integer *m, integer *m2, ftnlen 
	word_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer ret_val, i__1, i__2, i__3, i__4;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), i_len(char *, ftnlen);

    /* Local variables */
    static integer base, f, i__, blank;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    static integer length;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    static integer divisr;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    static integer val[129];

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This is an umbrella routine for the kernel pool hash function. */
/*     It should never be called directly. */

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

/*       PRIVATE UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  Entry point */
/*      --------  ---  -------------------------------------------------- */
/*      WORD       I   ZZHASH */
/*      M          I   ZZSHSH */

/*      The function returns zero. */

/* $ Detailed_Input */

/*     See individual entry points. */

/* $ Detailed_Output */

/*     The function ZZPHSH should never be called. However, it returns */
/*     the value zero. */

/* $ Parameters */

/*      None. */

/* $ Files */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This routine is an umbrella for the kernel pool hash function */
/*     ZZHASH, ZZHASH2 and the set up routine ZZSHSH. */

/* $ Examples */

/*     To make use of the ZZHAS hash function you must first call ZZSHSH */
/*     somewhere in your program. The value returned by ZZSHSH has */
/*     no meaning.  You can assign it to any temporary variable you */
/*     happen to have lying around. */

/*        I = ZZSHSH ( M ) */

/*           ...any other set up code... */

/*        LOOKAT = ZZHASH ( WORD ) */


/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 21-NOV-2006 (EDW)(BVS) */

/*        Replaced ICHAR('\\') expression with parameter */
/*        BSLASH, the parameter set to the ASCII value */
/*        of the backslash character, 92. */

/* -    SPICELIB Version 1.1.0, 14-SEP-2005 (EDW) */

/*        Added function ZZHASH2. Operation matches */
/*        that of ZZHASH with the exception that ZZHASH2 */
/*        accepts the divisor value, M, as an input. */

/* -    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT) */

/* -& */

/*     Entry Points */


/*     Local Variables. */

    switch(n__) {
	case 1: goto L_zzshsh;
	case 2: goto L_zzhash;
	case 3: goto L_zzhash2;
	}


/*     We do not diagnose a bogus call since this is a private routine. */

    ret_val = 0;
    return ret_val;
/* $Procedure      ZZSHSH (Private---Set up hash function) */

L_zzshsh:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine sets up the kernel pool hash function.  Call it */
/*     once per program execution. */

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

/*       PRIVATE UTILITY */

/* $ Declarations */

/*     INTEGER               M */

/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      M          I   Modulus used for the hash function */

/*      The function returns 0. */

/* $ Detailed_Input */

/*     M           is the modulus of the hashing function.  It is */
/*                 recommended that this be a prime number. */

/* $ Detailed_Output */

/*     The function returns the value zero (0). */

/* $ Parameters */

/*      None. */

/* $ Files */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This entry point sets up the modulus used for hashing input */
/*     strings.  It should be called once by an initialization */
/*     branch of the kernel pool. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 21-NOV-2006 (EDW)(BVS) */

/*        Replaced ICHAR('\\') expression with parameter */
/*        BSLASH, the parameter set to the ASCII value */
/*        of the backslash character, 92. */

/* -    SPICELIB Version 1.1.0, 06-JUL-2005 (EDW) */

/*        Added punctuation marks to array of allowed */
/*        characters. The function can process any */
/*        character with ASCII decimal value 33 to 122. */

/* -    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT) */

/* -& */
    divisr = *m;
    if (first) {
	first = FALSE_;
	base = 68;
	blank = ' ';
	for (i__ = 0; i__ <= 128; ++i__) {
	    val[(i__1 = i__) < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		    "zzphsh_", (ftnlen)295)] = 0;
	}
	val[(i__1 = '0') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)298)] = 1;
	val[(i__1 = '1') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)299)] = 2;
	val[(i__1 = '2') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)300)] = 3;
	val[(i__1 = '3') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)301)] = 4;
	val[(i__1 = '4') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)302)] = 5;
	val[(i__1 = '5') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)303)] = 6;
	val[(i__1 = '6') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)304)] = 7;
	val[(i__1 = '7') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)305)] = 8;
	val[(i__1 = '8') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)306)] = 9;
	val[(i__1 = '9') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)307)] = 10;
	val[(i__1 = 'A') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)308)] = 11;
	val[(i__1 = 'B') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)309)] = 12;
	val[(i__1 = 'C') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)310)] = 13;
	val[(i__1 = 'D') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)311)] = 14;
	val[(i__1 = 'E') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)312)] = 15;
	val[(i__1 = 'F') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)313)] = 16;
	val[(i__1 = 'G') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)314)] = 17;
	val[(i__1 = 'H') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)315)] = 18;
	val[(i__1 = 'I') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)316)] = 19;
	val[(i__1 = 'J') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)317)] = 20;
	val[(i__1 = 'K') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)318)] = 21;
	val[(i__1 = 'L') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)319)] = 22;
	val[(i__1 = 'M') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)320)] = 23;
	val[(i__1 = 'N') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)321)] = 24;
	val[(i__1 = 'O') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)322)] = 25;
	val[(i__1 = 'P') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)323)] = 26;
	val[(i__1 = 'Q') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)324)] = 27;
	val[(i__1 = 'R') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)325)] = 28;
	val[(i__1 = 'S') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)326)] = 29;
	val[(i__1 = 'T') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)327)] = 30;
	val[(i__1 = 'U') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)328)] = 31;
	val[(i__1 = 'V') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)329)] = 32;
	val[(i__1 = 'W') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)330)] = 33;
	val[(i__1 = 'X') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)331)] = 34;
	val[(i__1 = 'Y') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)332)] = 35;
	val[(i__1 = 'Z') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)333)] = 36;
	val[(i__1 = '-') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)334)] = 37;
	val[(i__1 = '_') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)335)] = 38;
	val[(i__1 = '.') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)336)] = 39;
	val[(i__1 = '/') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)337)] = 40;
	val[(i__1 = '!') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)338)] = 41;
	val[(i__1 = '@') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)339)] = 42;
	val[(i__1 = '#') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)340)] = 43;
	val[(i__1 = '$') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)341)] = 44;
	val[(i__1 = '%') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)342)] = 45;
	val[(i__1 = '^') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)343)] = 46;
	val[(i__1 = '&') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)344)] = 47;
	val[(i__1 = '*') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)345)] = 48;
	val[(i__1 = '(') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)346)] = 49;
	val[(i__1 = ')') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)347)] = 50;
	val[(i__1 = '+') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)348)] = 51;
	val[(i__1 = '=') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)349)] = 52;
	val[(i__1 = '[') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)350)] = 53;
	val[(i__1 = '{') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)351)] = 54;
	val[(i__1 = ']') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)352)] = 55;
	val[(i__1 = '}') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)353)] = 56;
	val[(i__1 = '|') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)354)] = 57;
	val[92] = 58;
	val[(i__1 = ':') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)356)] = 59;
	val[(i__1 = ';') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)357)] = 60;
	val[(i__1 = '<') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)358)] = 61;
	val[(i__1 = ',') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)359)] = 62;
	val[(i__1 = '>') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)360)] = 63;
	val[(i__1 = '?') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)361)] = 64;

/*        Note, ICHAR('''') returns the ASCII */
/*        value for the single quote -> ' */

	val[(i__1 = '\'') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)367)] = 65;
	val[(i__1 = '"') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)368)] = 66;
	val[(i__1 = '`') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)369)] = 67;
	val[(i__1 = '~') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)370)] = 68;
	val[(i__1 = 'a') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)373)] = val[(i__2 = 'A') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)373)];
	val[(i__1 = 'b') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)374)] = val[(i__2 = 'B') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)374)];
	val[(i__1 = 'c') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)375)] = val[(i__2 = 'C') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)375)];
	val[(i__1 = 'd') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)376)] = val[(i__2 = 'D') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)376)];
	val[(i__1 = 'e') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)377)] = val[(i__2 = 'E') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)377)];
	val[(i__1 = 'f') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)378)] = val[(i__2 = 'F') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)378)];
	val[(i__1 = 'g') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)379)] = val[(i__2 = 'G') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)379)];
	val[(i__1 = 'h') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)380)] = val[(i__2 = 'H') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)380)];
	val[(i__1 = 'i') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)381)] = val[(i__2 = 'I') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)381)];
	val[(i__1 = 'j') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)382)] = val[(i__2 = 'J') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)382)];
	val[(i__1 = 'k') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)383)] = val[(i__2 = 'K') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)383)];
	val[(i__1 = 'l') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)384)] = val[(i__2 = 'L') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)384)];
	val[(i__1 = 'm') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)385)] = val[(i__2 = 'M') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)385)];
	val[(i__1 = 'n') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)386)] = val[(i__2 = 'N') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)386)];
	val[(i__1 = 'o') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)387)] = val[(i__2 = 'O') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)387)];
	val[(i__1 = 'p') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)388)] = val[(i__2 = 'P') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)388)];
	val[(i__1 = 'q') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)389)] = val[(i__2 = 'Q') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)389)];
	val[(i__1 = 'r') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)390)] = val[(i__2 = 'R') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)390)];
	val[(i__1 = 's') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)391)] = val[(i__2 = 'S') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)391)];
	val[(i__1 = 't') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)392)] = val[(i__2 = 'T') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)392)];
	val[(i__1 = 'u') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)393)] = val[(i__2 = 'U') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)393)];
	val[(i__1 = 'v') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)394)] = val[(i__2 = 'V') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)394)];
	val[(i__1 = 'w') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)395)] = val[(i__2 = 'W') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)395)];
	val[(i__1 = 'x') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)396)] = val[(i__2 = 'X') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)396)];
	val[(i__1 = 'y') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)397)] = val[(i__2 = 'Y') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)397)];
	val[(i__1 = 'z') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)398)] = val[(i__2 = 'Z') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)398)];
    }
    ret_val = 0;
    return ret_val;
/* $Procedure      ZZHASH (Private --- Hash function) */

L_zzhash:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine computes the hash value associated with a kernel */
/*     pool variable name. */

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

/*       PRIVATE UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         WORD */

/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      WORD       I   A left justified string of characters. */

/*      The function returns the hash value associated with WORD. */

/* $ Detailed_Input */

/*     WORD        is a left justified string of characters.  Nominally */
/*                 this is the name of some kernel pool variable. */

/* $ Detailed_Output */

/*     The function returns the hash value of WORD */

/* $ Parameters */

/*      None. */

/* $ Files */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This routine computes the hash value of a left justified */
/*     string of characters.  It is critical that the string be */
/*     left justified.  All non-left justified strings map to the */
/*     same value 0. */

/* $ Examples */

/*     See POOL. */

/* $ Restrictions */

/*     1) If the has value calculates to a negative value, an error */
/*        signals. Such a signal should never occur. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2005 (EDW) */

/*        Added error test to catch non-positive hash values. */

/* -    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT) */

/* -& */
    f = 0;
    length = i_len(word, word_len);
    i__1 = length;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (*(unsigned char *)&word[i__ - 1] == blank) {
	    ret_val = f * base % divisr + 1;
	    return ret_val;
	}
/* Computing MIN */
	i__3 = 128, i__4 = *(unsigned char *)&word[i__ - 1];
	f = val[(i__2 = min(i__3,i__4)) < 129 && 0 <= i__2 ? i__2 : s_rnge(
		"val", i__2, "zzphsh_", (ftnlen)530)] + f * base;
	f %= divisr;
    }
    ret_val = f * base % divisr + 1;

/*     A non-positive value for ZZHASH indicates a serious problem. */

    if (ret_val < 0) {
	setmsg_("The ZZHASH function calculated a non-positive value for str"
		"ing $1. Contact NAIF", (ftnlen)79);
	errch_("$1", word, (ftnlen)2, word_len);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	return ret_val;
    }
    return ret_val;
/* $Procedure ZZHASH2 (Private --- Hash function) */

L_zzhash2:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine computes the hash value corresponding to an string */
/*     given a particular  divisor value (M2). */

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

/*       PRIVATE UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         WORD */

/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      WORD       I   A left justified string of characters. */
/*      M2         I   Modulus used for the hash function */

/*      The function returns the hash value associated with WORD. */

/* $ Detailed_Input */

/*     WORD        is a left justified string of characters. */

/*     M2          the modulus of the hashing function. This value */
/*                 defines the spread of the hash values, that */
/*                 spread covering the interval [0, M2-1]. The larger */
/*                 the value, the less the chance of a hash key */
/*                 collision. The user should always chose a prime */
/*                 for M2. */

/* $ Detailed_Output */

/*     The function returns the hash value of WORD as computed using */
/*     M2 as the M divisor. */

/* $ Parameters */

/*      None. */

/* $ Files */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This routine computes the hash value of a left justified */
/*     string of characters.  It is critical that the string be */
/*     left justified.  All non-left justified strings map to the */
/*     same value 0. */

/* $ Examples */

/*     1) If the has value calculates to a negative value, an error */
/*        signals. Such a signal should never occur. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */
/*      E.D. Wright     (JPL) */

/* $ Literature_References */

/*     1)  Knuth, Donald E. "The Art of Computer Programming, Volume */
/*         3/Sorting and Searching 2nd Edition" 1997, pp 513-521. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 14-SEP-2005 (EDW) */

/* -& */
    f = 0;
    length = i_len(word, word_len);
    i__1 = length;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (*(unsigned char *)&word[i__ - 1] == blank) {
	    ret_val = f * base % *m2 + 1;
	    return ret_val;
	}
/* Computing MIN */
	i__3 = 128, i__4 = *(unsigned char *)&word[i__ - 1];
	f = val[(i__2 = min(i__3,i__4)) < 129 && 0 <= i__2 ? i__2 : s_rnge(
		"val", i__2, "zzphsh_", (ftnlen)682)] + f * base;
	f %= *m2;
    }
    ret_val = f * base % *m2 + 1;

/*     A non-positive value for ZZHASH2 indicates a serious problem. */

    if (ret_val < 0) {
	setmsg_("The ZZHASH2 function calculated a non-positive value for st"
		"ring $1. Contact NAIF", (ftnlen)80);
	errch_("$1", word, (ftnlen)2, word_len);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	return ret_val;
    }
    return ret_val;
} /* zzphsh_ */

integer zzphsh_(char *word, integer *m, integer *m2, ftnlen word_len)
{
    return zzphsh_0_(0, word, m, m2, word_len);
    }

integer zzshsh_(integer *m)
{
    return zzphsh_0_(1, (char *)0, m, (integer *)0, (ftnint)0);
    }

integer zzhash_(char *word, ftnlen word_len)
{
    return zzphsh_0_(2, word, (integer *)0, (integer *)0, word_len);
    }

integer zzhash2_(char *word, integer *m2, ftnlen word_len)
{
    return zzphsh_0_(3, word, (integer *)0, m2, word_len);
    }

