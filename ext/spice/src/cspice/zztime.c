/* zztime.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;
static integer c__12 = 12;

/* $Procedure      ZZTIME ( Private --- time parsing utilities ) */
logical zztime_0_(int n__, char *string, char *transl, char *letter, char *
	error, char *pic, doublereal *tvec, integer *b, integer *e, logical *
	l2r, logical *yabbrv, ftnlen string_len, ftnlen transl_len, ftnlen 
	letter_len, ftnlen error_len, ftnlen pic_len)
{
    /* Initialized data */

    static integer size = 0;
    static logical first = TRUE_;
    static char months[3*12] = "JAN" "FEB" "MAR" "APR" "MAY" "JUN" "JUL" 
	    "AUG" "SEP" "OCT" "NOV" "DEC";

    /* System generated locals */
    integer i__1, i__2, i__3;
    logical ret_val;

    /* Builtin functions */
    integer i_len(char *, ftnlen), s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_indx(char *, char *, ftnlen, ftnlen), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    static integer case__, begs[32], kind, nsec, ends[32];
    static logical ampm;
    static integer nday, item, from;
    extern integer cpos_(char *, char *, integer *, ftnlen, ftnlen);
    static integer last, nmin, nmon;
    static char this__[1];
    static integer ndoy, next;
    extern integer posr_(char *, char *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int zzrepsub_(char *, integer *, integer *, char *
	    , char *, ftnlen, ftnlen, ftnlen);
    static integer f[95];
    extern /* Subroutine */ int zzinssub_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    static integer i__, j, k, l[95];
    static logical check;
    static integer r__, blank, w, nchar;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char recog[12*70];
    static integer pbegs[32];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    static char names[32*95], class__[1*70];
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen);
    static integer pends[32], value;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    static integer nyear, width[70];
    static char wkday[12*3*2];
    static integer pfrom;
    static char mnmrk[12*3*2], month[3];
    static integer nhour;
    extern integer rtrim_(char *, ftnlen);
    static char myerr[160];
    static integer pnext, p1, p2;
    extern /* Subroutine */ int lx4uns_(char *, integer *, integer *, integer 
	    *, ftnlen);
    static integer to;
    extern logical samchi_(char *, integer *, char *, integer *, ftnlen, 
	    ftnlen);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static char spcial[12];
    extern logical samsbi_(char *, integer *, integer *, char *, integer *, 
	    integer *, ftnlen, ftnlen);
    static char messge[160];
    extern /* Subroutine */ int nparsd_(char *, doublereal *, char *, integer 
	    *, ftnlen, ftnlen), sigerr_(char *, ftnlen), nparsi_(char *, 
	    integer *, char *, integer *, ftnlen, ftnlen), chkout_(char *, 
	    ftnlen), prefix_(char *, integer *, char *, ftnlen, ftnlen);
    static char pictur[160];
    static integer mnsize[2];
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    static integer wksize[2];
    extern /* Subroutine */ int zzmkpc_(char *, integer *, integer *, char *, 
	    char *, ftnlen, ftnlen, ftnlen);
    static logical did;
    static integer njd, get;
    static char rep[32];
    static doublereal hms[3];
    static logical got;
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);
    static integer pto, ptr, put;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This is an umbrella routine for a collection of entry points */
/*     to the time parsing utility functions. */

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

/*      TIME --- Private */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  Entry Points */
/*     --------  ---  -------------------------------------------------- */
/*     STRING    I/O  ZZUNPCK ZZCMBT ZZGREP ZZISPT  ZZSUBT ZZTOKNS ZZVALT */
/*     TRANSL     I   ZZUNPCK ZZSUBT */
/*     LETTER     I   ZZCMBT  ZZIST   ZZNOTE  ZZREMT ZZVALT */
/*     ERROR      O   ZZUNPCK ZZTOKNS */
/*     TVEC       O   ZZUNPCK */
/*     B          O   ZZISPT  ZZNOTE  ZZVALT */
/*     E          O   ZZISPT  ZZNOTE  ZZUNPCK ZZVALT */
/*     L2R        I   ZZCMBT  ZZSUBT */
/*     YABBRV     I   ZZUNPCK */

/* $ Detailed_Input */

/*     See Individual Entry Points. */

/* $ Detailed_Output */

/*     See Individual Entry Points. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If ZZTIME is called directly the error 'SPICE(BOGUSENTRY)' */
/*         is signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine serves as an umbrella for a collection of */
/*     related entry points that are used to parse time strings. */

/*     Normal usage is to first call ZZTOKNS to create an internal */
/*     representation for a time string.  This internal representations */
/*     maintains a list of identified substrings from the original */
/*     input time string.  For example the call to ZZTOKNS using */
/*     the string */

/*       '1996 JAN 25 12:18:19.199' */
/*        123456789012345678901234 */

/*     yields the following internal representation: */

/*       'ibmbibi:i:i.i' */

/*     where the individual tokens correspond to the substrings */
/*     indicated in the following table: */

/*       Identifier    Substring          meaning */
/*       ----------    -------------      ---------------- */
/*         i           from 01 to 04      unsigned integer */
/*         b           from 05 to 05      blanks or tab */
/*         m           from 06 to 08      month */
/*         b           from 09 to 09      blanks or tab */
/*         i           from 10 to 11      unsigned integer */
/*         b           from 12 to 12      blank or tab */
/*         i           from 13 to 14      unsigned integer */
/*         :           from 15 to 15      colon */
/*         i           from 16 to 17      unsigned integer */
/*         :           from 18 to 18      colon */
/*         i           from 19 to 20      unsigned integer */
/*         .           from 21 to 21      decimal point */
/*         i           from 22 to 24      unsigned integer */

/*     These substrings may be combined and reidentified, removed */
/*     or re-identified using the various entry points listed here: */

/*     ZZCMBT   combine several tokens into a single token */
/*              for example you might scan right to left and replace */
/*              the token sequence i.i by n (for number).  In this */
/*              case the substring boundaries of n would be from 19 */
/*              to 24. */

/*     ZZGREP   returns the current internal representation */
/*              in the case above 'ibmbibi:i:i.i' */


/*     ZZISPT   returns TRUE if a pair of letters from a list are */
/*              present in the internal representation. This is */
/*              used primarily to detect erroneous substrings such */
/*              as ',,' or ':,' */

/*     ZZIST    Return TRUE if a particular letter is present in the */
/*              string. */

/*     ZZNOTE   Returns the substring boundaries associated with */
/*              a letter and removes the letter from the internal */
/*              representation.  This is used primarily for calendar */
/*              string modifiers such as 'B.C.', 'A.D.' etc. */

/*     ZZREMT   remove a letter from the internal representation. */
/*              In the input example you might remove all white space */
/*              markers. */

/*     ZZSUBT   substitute a different letter for one listed in the */
/*              input one for one. For example after removing blanks */
/*              you might substitute YmD for imi. */


/*     ZZVALT   replace an integer by a new marker if the integer */
/*              lies withing a particular range.  For example */
/*              you might replace any integer between 1000 and 10000 */
/*              by Y (for year). */

/*     Once all substitutions and removals have been performed that */
/*     can be made, the entry point ZZUNPCK allows you to extract */
/*     year(Y), month(m), day or month(D), day of year (y), hours(H), */
/*     minutes(M) and seconds(S) from the input string */

/* $ Examples */

/*     See TPARTV. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     B.V. Semenov    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.5.0, 08-MAR-2009 (NJB) */

/*        Bug fix: in entry point ZZTOKNS, changed upper */
/*        bound used to detect non-printing characters from 128 */
/*        to 126. */

/*        Bug fix: added error handling to this routine. Header */
/*        already referred to SPICE(BOGUSENTRY) error, but no */
/*        such error was signaled. */

/*        Changed upper bound of arrays NAMES, F, and L from 128 */
/*        to 126. */

/*        Re-ordered header sections in various entry points. */

/* -    SPICELIB Version 1.4.0, 27-OCT-2006 (BVS) */

/*        Fixed the bug in the ZZTOKNS entry that in the case of a one */
/*        character long blank input time string caused the TO variable */
/*        be set to the value greater than the string length, triggering */
/*        an OUT OF BOUNDS runtime error on HP. Added to ZZTOKNS a */
/*        separate check for the blank input strings. */

/* -    SPICELIB Version 1.3.0, 13-Nov-2000 (WLT) */

/*        Changed the call to EQSTR to a call to SAMSBI so as to */
/*        guard against overflowing strings. */

/* -    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT) */

/*        The main routine (which should never be called) now returns */
/*        the value .FALSE. */

/* -    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT) */

/*        Added a RETURN statement at the end of the main routine. */
/*        Enhanced error message for the case when the input string */
/*        to ZZTOKNS has a non-printing character. */

/* -    SPICELIB Version 1.0.0, 8-APR-1996 (WLT) */


/* -& */

/*     Entry points */


/*     Spicelib Functions */


/*     Standard Parameters */


/*     LOWER */
/*     UPPER */
/*     MIXED */


/*     FULL */
/*     SHORT */


/*     Representation Variables. */


/*     Token Recognition Variables. */

/*     At the moment there are 53 recognized substrings, we */
/*     make room for 70 just so we won't have to increase */
/*     the parameter NRECOG soon. */

    /* Parameter adjustments */
    if (tvec) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_zzcmbt;
	case 2: goto L_zzgrep;
	case 3: goto L_zzispt;
	case 4: goto L_zzist;
	case 5: goto L_zznote;
	case 6: goto L_zzremt;
	case 7: goto L_zzsubt;
	case 8: goto L_zztokns;
	case 9: goto L_zzunpck;
	case 10: goto L_zzvalt;
	}

    ret_val = FALSE_;
    chkin_("ZZTIME", (ftnlen)6);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZTIME", (ftnlen)6);
    return ret_val;
/* $Procedure      ZZCMBT ( Private --- combine tokens ) */

L_zzcmbt:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Combine several token representatives into a single token. */

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

/*      TIME --- PRIVATE */


/* $ Declarations */

/*     CHARACTER*(*)         STRING */
/*     CHARACTER*(1)         LETTER */
/*     LOGICAL               L2R */

/* $ Brief_I/O */
/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   A sequence of tokens to be combined. */
/*     LETTER     I   The replacement token for the combination */
/*     L2R        I   If TRUE scan left to right, else scan right to left */

/*     The function returns TRUE is a combination was performed. */

/* $ Detailed_Input */

/*     STRING     is a sequence of tokens to look for in the */
/*                stored internal representation. */

/*     LETTER     is the replacement token to insert for STRING. */

/*                If letter is a blank, the combination is simply */
/*                replaced by a blank. */

/*     L2R        is a logical.  If TRUE, the internal representation */
/*                is scanned left to right.  If FALSE, the internal */
/*                representation is scanned right to left. */

/* $ Detailed_Output */

/*     The function returns TRUE if a combination is performed. */
/*     Otherwise it returns FALSE. */

/*     Note that the most important action of this function is a */
/*     side-effect.  The internal representation of a time string */
/*     is modified to reflect the requested token combination. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error Free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This function allows you to alter the internal representation */
/*     of a time string by combining two or more tokens into a single */
/*     token. */

/* $ Examples */

/*     See TPARTV */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB) */

/*        Re-ordered header sections. */

/* -    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT) */

/*        The main routine (which should never be called) now returns */
/*        the value .FALSE. */

/* -    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT) */

/*        Added a RETURN statement at the end of the main routine. */
/*        Enhanced error message for the case when the input string */
/*        to ZZTOKNS has a non-printing character. */

/* -    SPICELIB Version 1.0.0, 4-APR-1996 (WLT) */


/* -& */

/*     So far we haven't combined anything. */

    did = FALSE_;

/*     Look for the substring either looking from the */
/*     left (L2R is YES) or from the right (L2R is NO). */

    if (*l2r) {
	from = pos_(rep, string, &c__1, size, string_len);
    } else {
	from = posr_(rep, string, &size, size, string_len);
    }
    to = from + i_len(string, string_len) - 1;
    if (from > 0) {
	did = TRUE_;
	ends[(i__1 = from - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", i__1,
		 "zztime_", (ftnlen)582)] = ends[(i__2 = to - 1) < 32 && 0 <= 
		i__2 ? i__2 : s_rnge("ends", i__2, "zztime_", (ftnlen)582)];
	pends[(i__1 = from - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("pends", 
		i__1, "zztime_", (ftnlen)583)] = pends[(i__2 = to - 1) < 32 &&
		 0 <= i__2 ? i__2 : s_rnge("pends", i__2, "zztime_", (ftnlen)
		583)];
	put = from + 1;
	next = to + 1;

/*        Perform the substitution in the representation */

	zzrepsub_(rep, &from, &to, letter, rep, (ftnlen)32, (ftnlen)1, (
		ftnlen)32);

/*        Now update the begins and ends of tokens in the original */
/*        string. */

	i__1 = size;
	for (get = next; get <= i__1; ++get) {
	    begs[(i__2 = put - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("begs", 
		    i__2, "zztime_", (ftnlen)597)] = begs[(i__3 = get - 1) < 
		    32 && 0 <= i__3 ? i__3 : s_rnge("begs", i__3, "zztime_", (
		    ftnlen)597)];
	    ends[(i__2 = put - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("ends", 
		    i__2, "zztime_", (ftnlen)598)] = ends[(i__3 = get - 1) < 
		    32 && 0 <= i__3 ? i__3 : s_rnge("ends", i__3, "zztime_", (
		    ftnlen)598)];
	    pbegs[(i__2 = put - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("pbegs", 
		    i__2, "zztime_", (ftnlen)599)] = pbegs[(i__3 = get - 1) < 
		    32 && 0 <= i__3 ? i__3 : s_rnge("pbegs", i__3, "zztime_", 
		    (ftnlen)599)];
	    pends[(i__2 = put - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("pends", 
		    i__2, "zztime_", (ftnlen)600)] = pends[(i__3 = get - 1) < 
		    32 && 0 <= i__3 ? i__3 : s_rnge("pends", i__3, "zztime_", 
		    (ftnlen)600)];
	    ++put;
	}
	size = size - i_len(string, string_len) + 1;
    }
    ret_val = did;
    return ret_val;
/* $Procedure      ZZGREP ( Private --- get representation ) */

L_zzgrep:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Return the internal representation of the time string. */

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

/*      TIME --- PRIVATE */


/* $ Declarations */

/*     CHARACTER*(*)         STRING */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     O   The current representation of tokenized time */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     STRING     is the current internal tokenized representation of */
/*                the time string that was last supplied to ZZTIME */
/*                via the entry point ZZTOKNS. */

/*     The function returns TRUE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This returns the current internal representation of the */
/*     tokenized time string. The function always returns the */
/*     value TRUE. */

/* $ Examples */

/*     See TPARTV. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT) */

/*        The main routine (which should never be called) now returns */
/*        the value .FALSE. */

/* -    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT) */

/*        Added a RETURN statement at the end of the main routine. */
/*        Enhanced error message for the case when the input string */
/*        to ZZTOKNS has a non-printing character. */

/* -    SPICELIB Version 1.0.0, 4-APR-1996 (WLT) */


/* -& */
    s_copy(string, rep, string_len, (max(1,size)));
    ret_val = TRUE_;
    return ret_val;
/* $Procedure      ZZISPT ( Private --- is pair of tokens ) */

L_zzispt:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Determine if there is a pair of consecutive tokens from */
/*     a user specified list of tokens. */

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

/*      TIME --- PRIVATE */

/* $ Declarations */

/*     CHARACTER*(*)         STRING */
/*     INTEGER               B */
/*     INTEGER               E */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   a list of tokens to search for. */
/*     B          O   the beginning of the first matching token */
/*     E          O   the ending of the last matching token. */

/*     The function returns TRUE if a pair is found. */

/* $ Detailed_Input */

/*     STRING     is a character string that gives a list of tokens */
/*                to search for in a string. */

/* $ Detailed_Output */

/*     B          is the location in the original time string supplied */
/*                to ZZTOKNS of the beginning a pair of consecutive */
/*                tokens from the list specified by STRING. */

/*     E          is the location in the original time string supplied */
/*                to ZZTOKENS of the end a pair of consecutive */
/*                tokens from the list specified by STRING. */

/*     The function returns the TRUE is a consecutive pair of tokens */
/*     from STRING is located. Otherwise it returns FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error Free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine exists primarily to assist in the diagnosis */
/*     of consecutive delimiters in a time string. */

/* $ Examples */

/*     See TPARTV */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB) */

/*        Re-ordered header sections. */

/* -    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT) */

/*        The main routine (which should never be called) now returns */
/*        the value .FALSE. */

/* -    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT) */

/*        Added a RETURN statement at the end of the main routine. */
/*        Enhanced error message for the case when the input string */
/*        to ZZTOKNS has a non-printing character. */

/* -    SPICELIB Version 1.0.0, 4-APR-1996 (WLT) */


/* -& */
    did = FALSE_;
    from = cpos_(rep, string, &c__1, (ftnlen)32, string_len);
    while(from > 0) {
	if (from < size) {
	    to = from + 1;
	    did = i_indx(string, rep + (to - 1), string_len, (ftnlen)1) > 0;
	} else {
	    *b = 0;
	    *e = 0;
	    ret_val = FALSE_;
	    return ret_val;
	}
	if (did) {
	    *b = begs[(i__1 = from - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge(
		    "begs", i__1, "zztime_", (ftnlen)896)];
	    *e = ends[(i__1 = to - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends"
		    , i__1, "zztime_", (ftnlen)897)];
	    ret_val = TRUE_;
	    return ret_val;
	}
	from = cpos_(rep, string, &to, (ftnlen)32, string_len);
    }
    *b = 0;
    *e = 0;
    ret_val = FALSE_;
    return ret_val;
/* $Procedure      ZZIST ( Private --- is there a token ) */

L_zzist:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Determine if a token is present in the internal representation */
/*     of a tokenized time string. */

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

/*      TIME --- PRIVATE */


/* $ Declarations */

/*     CHARACTER*(1)         LETTER */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LETTER     I */

/*     The function returns */

/* $ Detailed_Input */

/*     LETTER     is a token to look for in the tokenized representation */
/*                of a time string. */

/* $ Detailed_Output */

/*     The function returns TRUE is LETTER is present in the internal */
/*     representation of the last time string passed to ZZTOKNS. */
/*     Otherwise it returns FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine determines whether or not a particular token */
/*     is present in a tokenized representation of a time. */

/* $ Examples */

/*     See TPARTV */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT) */

/*        The main routine (which should never be called) now returns */
/*        the value .FALSE. */

/* -    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT) */

/*        Added a RETURN statement at the end of the main routine. */
/*        Enhanced error message for the case when the input string */
/*        to ZZTOKNS has a non-printing character. */

/* -    SPICELIB Version 1.0.0, 4-APR-1996 (WLT) */


/* -& */
    ret_val = i_indx(rep, letter, size, (ftnlen)1) > 0;
    return ret_val;
/* $Procedure      ZZNOTE ( Private --- note the existence and remove ) */

L_zznote:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Return the beginning and ending of a token in a time string */
/*     and remove the token from the internal representation. */

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

/*      TIME --- PRIVATE */

/* $ Declarations */

/*     CHARACTER*(1)         LETTER */
/*     INTEGER               B */
/*     INTEGER               E */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LETTER     I   a token to look for in the internal representation */
/*     B          O   is the beginning of the token */
/*     E          O   is the end of the token. */

/*     The function returns TRUE if the token is located. */

/* $ Detailed_Input */

/*     LETTER     is a token to look for and remove from the */
/*                current tokenization of a time string. */

/*                If located the token is removed from the string. */

/*                Note that this simply finds the first matching */
/*                token.  If others are present they are not */
/*                affected. */

/* $ Detailed_Output */

/*     B          is the beginning of the requested token if it */
/*                was found.  Otherwise B is zero. */

/*     E          is the ending of the requested token if it was */
/*                found.  Otherwise E is zero. */

/*     The function returns the value TRUE if the token is located. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Look up and remove a token from the internal representation */
/*     of a time string.  This is useful in removing modifiers */
/*     from a string (such as the ERA of an epoch, AM/PM of a time */
/*     etc.) */

/* $ Examples */

/*     See TPARTV */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB) */

/*        Re-ordered header sections. */

/* -    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT) */

/*        The main routine (which should never be called) now returns */
/*        the value .FALSE. */

/* -    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT) */

/*        Added a RETURN statement at the end of the main routine. */
/*        Enhanced error message for the case when the input string */
/*        to ZZTOKNS has a non-printing character. */

/* -    SPICELIB Version 1.0.0, 4-APR-1996 (WLT) */


/* -& */
    put = i_indx(rep, letter, (ftnlen)32, (ftnlen)1);
    if (put > 0) {
	*b = begs[(i__1 = put - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		i__1, "zztime_", (ftnlen)1185)];
	*e = ends[(i__1 = put - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		i__1, "zztime_", (ftnlen)1186)];
	next = put + 1;
	i__1 = size;
	for (get = next; get <= i__1; ++get) {
	    begs[(i__2 = put - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("begs", 
		    i__2, "zztime_", (ftnlen)1192)] = begs[(i__3 = get - 1) < 
		    32 && 0 <= i__3 ? i__3 : s_rnge("begs", i__3, "zztime_", (
		    ftnlen)1192)];
	    ends[(i__2 = put - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("ends", 
		    i__2, "zztime_", (ftnlen)1193)] = ends[(i__3 = get - 1) < 
		    32 && 0 <= i__3 ? i__3 : s_rnge("ends", i__3, "zztime_", (
		    ftnlen)1193)];
	    pbegs[(i__2 = put - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("pbegs", 
		    i__2, "zztime_", (ftnlen)1194)] = pbegs[(i__3 = get - 1) <
		     32 && 0 <= i__3 ? i__3 : s_rnge("pbegs", i__3, "zztime_",
		     (ftnlen)1194)];
	    pends[(i__2 = put - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("pends", 
		    i__2, "zztime_", (ftnlen)1195)] = pends[(i__3 = get - 1) <
		     32 && 0 <= i__3 ? i__3 : s_rnge("pends", i__3, "zztime_",
		     (ftnlen)1195)];
	    *(unsigned char *)&rep[put - 1] = *(unsigned char *)&rep[get - 1];
	    ++put;
	}
	s_copy(rep + (size - 1), " ", 32 - (size - 1), (ftnlen)1);
	--size;
	did = TRUE_;
    } else {
	*b = 0;
	*e = 0;
	did = FALSE_;
    }
    ret_val = did;
    return ret_val;
/* $Procedure      ZZREMT ( Private --- remove token ) */

L_zzremt:
/* $ Abstract */

/*    SPICE Private routine intended solely for the support of SPICE */
/*    routines.  Users should not call this routine directly due */
/*    to the volatile nature of this routine. */

/*    Remove a specified token from the internal representation */

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

/*     None. */

/* $ Keywords */

/*     TIME --- Private */


/* $ Declarations */

/*     CHARACTER*(1)         LETTER */

/* $ Brief_I/O */
/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LETTER     I   token to remove from the internal representation. */

/*     The function returns TRUE if any tokens are removed. */

/* $ Detailed_Input */

/*     LETTER     is a token to be removed from the internal */
/*                representation of a tokenized time string. */
/*                All instances of LETTER will be removed from */
/*                the internal representation. */

/* $ Detailed_Output */

/*     The function returns TRUE if any instance of LETTER is removed */
/*     from the internal representation of a tokenized time string. */
/*     If no instances are removed the function returns FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is used to remove various delimiters that */
/*     appear in a tokenized time string (although it could be */
/*     used to remove any token from a tokenized time string). */

/* $ Examples */

/*     See TPARTV */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB) */

/*        Re-ordered header sections. */

/* -    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT) */

/*        The main routine (which should never be called) now returns */
/*        the value .FALSE. */

/* -    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT) */

/*        Added a RETURN statement at the end of the main routine. */
/*        Enhanced error message for the case when the input string */
/*        to ZZTOKNS has a non-printing character. */

/* -    SPICELIB Version 1.0.0, 4-APR-1996 (WLT) */


/* -& */
    put = 0;
    did = FALSE_;
    i__1 = size;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (*(unsigned char *)&rep[i__ - 1] != *(unsigned char *)letter) {
	    ++put;
	    *(unsigned char *)&rep[put - 1] = *(unsigned char *)&rep[i__ - 1];
	    begs[(i__2 = put - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("begs", 
		    i__2, "zztime_", (ftnlen)1350)] = begs[(i__3 = i__ - 1) < 
		    32 && 0 <= i__3 ? i__3 : s_rnge("begs", i__3, "zztime_", (
		    ftnlen)1350)];
	    ends[(i__2 = put - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("ends", 
		    i__2, "zztime_", (ftnlen)1351)] = ends[(i__3 = i__ - 1) < 
		    32 && 0 <= i__3 ? i__3 : s_rnge("ends", i__3, "zztime_", (
		    ftnlen)1351)];
	    pbegs[(i__2 = put - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("pbegs", 
		    i__2, "zztime_", (ftnlen)1352)] = pbegs[(i__3 = i__ - 1) <
		     32 && 0 <= i__3 ? i__3 : s_rnge("pbegs", i__3, "zztime_",
		     (ftnlen)1352)];
	    pends[(i__2 = put - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("pends", 
		    i__2, "zztime_", (ftnlen)1353)] = pends[(i__3 = i__ - 1) <
		     32 && 0 <= i__3 ? i__3 : s_rnge("pends", i__3, "zztime_",
		     (ftnlen)1353)];
	} else {
	    did = TRUE_;
	}
    }
    size = put;
    if (put == 0) {
	s_copy(rep, " ", (ftnlen)32, (ftnlen)1);
    } else if (put < i_len(rep, (ftnlen)32)) {
	i__1 = put;
	s_copy(rep + i__1, " ", 32 - i__1, (ftnlen)1);
    }
    ret_val = did;
    return ret_val;
/* $Procedure      ZZSUBT ( Private --- substitute tokens ) */

L_zzsubt:
/* $ Abstract */

/*    SPICE Private routine intended solely for the support of SPICE */
/*    routines.  Users should not call this routine directly due */
/*    to the volatile nature of this routine. */

/*    Substitute one token for another in the internal representation */
/*    of a tokenized time string. */

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

/*     None. */

/* $ Keywords */

/*     TIME --- Private */


/* $ Declarations */

/*     IMPLICIT NONE */
/*     CHARACTER*(*)         STRING */
/*     CHARACTER*(*)         TRANSL */
/*     LOGICAL               L2R */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   token pattern to look for. */
/*     TRANSL     I   token replacement pattern. */
/*     L2R        I   direction to scan internal representation. */

/*     The function returns TRUE is a substitution is performed. */

/* $ Detailed_Input */

/*     STRING     is a string of tokens to look for in the internal */
/*                representation of a tokenized time string. */

/*                Only the first occurrence of STRING will be modified. */

/*                If the first character in STRING is '<', (and string */
/*                is more than 1 character in length) substitutions */
/*                will be performed in the4 tokenized string only if */
/*                STRING exactly matches the tokenized string */
/*                starting at the left most character. */

/*                If the last character in STRING is '>' (and string */
/*                is more than 1 character in length) substitutions */
/*                will be performed in the4 tokenized string only if */
/*                STRING exactly matches the tokenized string */
/*                ending at the right most character. */

/*                If first and last character of STRING are '<' and '>' */
/*                respectively, the first case above is applied and the */
/*                greater than character ('>') is regarded as just */
/*                another character. */

/*     TRANSL     is a sequence of replacement tokens to substitute */
/*                in place of STRING. */

/*     L2R        is a logical flag.  If L2R is TRUE, the internal */
/*                representation is scanned from left to right. If */
/*                L2R is FALSE, the internal representation is scanned */
/*                from right to left. */

/* $ Detailed_Output */

/*     The function returns TRUE if a substitution is performed. */
/*     Otherwise it returns FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine searchs for the first instance of a specified */
/*     pattern in the internal representation of a tokenized */
/*     time string.  If the pattern is found, it is replaced */
/*     by that value of TRANSL.  Only one pattern substitution */
/*     is performed per call to this function. */

/* $ Examples */

/*     See TPARTV */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB) */

/*        Re-ordered header sections. */

/* -    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT) */

/*        The main routine (which should never be called) now returns */
/*        the value .FALSE. */

/* -    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT) */

/*        Added a RETURN statement at the end of the main routine. */
/*        Enhanced error message for the case when the input string */
/*        to ZZTOKNS has a non-printing character. */

/* -    SPICELIB Version 1.0.0, 4-APR-1996 (WLT) */


/* -& */

/*     So far we haven't combined anything. */

    did = FALSE_;
    k = i_len(string, string_len);

/*     We have two special cases to deal with. */

    if (*(unsigned char *)string == '<' && k > 1) {
/* Computing MIN */
	i__1 = k - 1;
	to = min(i__1,size);
	from = 1;
	if (s_cmp(string + 1, rep + (from - 1), k - 1, to - (from - 1)) == 0) 
		{
	    s_copy(rep + (from - 1), transl, to - (from - 1), transl_len);
	    ret_val = TRUE_;
	} else {
	    ret_val = FALSE_;
	}
	return ret_val;
    } else if (*(unsigned char *)&string[k - 1] == '>' && k > 1) {
/* Computing MAX */
	i__1 = 1, i__2 = size - k + 2;
	from = max(i__1,i__2);
	to = size;
	if (s_cmp(string, rep + (from - 1), k - 1, to - (from - 1)) == 0) {
	    s_copy(rep + (from - 1), transl, to - (from - 1), transl_len);
	    ret_val = TRUE_;
	} else {
	    ret_val = FALSE_;
	}
	return ret_val;
    }

/*     Look for the substring either looking from the */
/*     left (L2R is YES) or from the right (L2R is NO). */

    if (*l2r) {
	from = pos_(rep, string, &c__1, (ftnlen)32, string_len);
    } else {
	from = posr_(rep, string, &size, (ftnlen)32, string_len);
    }
    to = from + i_len(transl, transl_len) - 1;
    if (from > 0) {
	did = TRUE_;
	s_copy(rep + (from - 1), transl, to - (from - 1), transl_len);
    }
    ret_val = did;
    return ret_val;
/* $Procedure      ZZTOKNS ( Private --- Time Tokens ) */

L_zztokns:
/* $ Abstract */

/*    SPICE Private routine intended solely for the support of SPICE */
/*    routines.  Users should not call this routine directly due */
/*    to the volatile nature of this routine. */

/*    Construct an internal tokenized representation of STRING. */

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

/*     None. */

/* $ Keywords */

/*     TIME --- PRIVATE */

/* $ Declarations */

/*     IMPLICIT NONE */
/*     CHARACTER*(*)         STRING */
/*     CHARACTER*(*)         ERROR */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   A time string to be tokenized and internalized. */
/*     ERROR      O   A diagnostic message */

/*     The function returns TRUE is STRING can be tokenized. */

/* $ Detailed_Input */

/*     STRING     is a string that is intended to represent some */
/*                epoch and that needs parsing. */

/* $ Detailed_Output */

/*     ERROR      is a diagnostic message that is returned if a */
/*                problem occurs while trying to tokenize the */
/*                input time string.  If no problems arise, ERROR */
/*                will be returned as a blank. */

/*     The function returns TRUE if the input string can be successfully */
/*     tokenized.  If a problem arises, the function returns FALSE */
/*     and diagnostic is returned in ERROR. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is the first step in parsing a time string.  The */
/*     string is examined for integers, month, weekdays, time systems */
/*     time zones, eras, am/pm and various separators.  This */
/*     representation is maintained and manipulated by the */
/*     companion entry points in ZZTIME. */

/*     The various recognized tokens represented by this routine */
/*     are: */

/*        '    --- the quote character (year abbreviation) */
/*        ,    --- a comma  (delimiter) */
/*        -    --- a dash   (delimiter) */
/*        .    --- a period (delimiter) */
/*        /    --- a slash  (delimiter) */
/*        :    --- a colon  (delimiter) */
/*        N    --- AM/PM marker */
/*        O    --- UTC+  marker */
/*        Z    --- US Time Zone Marker */
/*        [    --- left parenthesis marker */
/*        ]    --- right parenthesis marker */
/*        b    --- stands for blanks, or tabs (delimiter) */
/*        d    --- day of year marker (delimiter) */
/*        e    --- era marker */
/*        j    --- julian date system marker */
/*        m    --- month marker */
/*        o    --- UTC- marker */
/*        s    --- time system marker */
/*        t    --- the "T" marker used in ISO formats. */
/*        w    --- the weekday marker */
/*        i    --- unsigned integer marker */

/*     Using the other entry points in ZZTIME, these markers are */
/*     gradually removed and transformed to more meaningful markers. */

/* $ Examples */

/*     See TPARTV */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     B.V. Semenov    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.5.0, 08-MAR-2009 (NJB) */

/*        Bug fix: changed upper bound used to detect */
/*        non-printing characters from 128 to 126. */

/*        Re-ordered header sections. */

/* -    SPICELIB Version 1.3.0, 27-OCT-2006 (BVS) */

/*        Fixed the bug that in the case of a one character long blank */
/*        input time string caused the TO variable be set to the value */
/*        greater than the string length, triggering an OUT OF BOUNDS */
/*        runtime error on HP. Added a separate up-front check for the */
/*        blank input string. */

/* -    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT) */

/*        The main routine (which should never be called) now returns */
/*        the value .FALSE. */

/* -    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT) */

/*        Added a RETURN statement at the end of the main routine. */
/*        Enhanced error message for the case when the input string */
/*        to ZZTOKNS has a non-printing character. */

/* -    SPICELIB Version 1.0.0, 4-APR-1996 (WLT) */


/* -& */

/*     The first time in this routine we initialize our "tokenizing" */
/*     table. */

    if (first) {
	first = FALSE_;
	blank = ' ';

/*        Below is the list of recognized substrings.  The basic */
/*        pattern here is to find the block of special tokens */
/*        that begin with a particular character.  Insert into */
/*        that block the lines of code below */

/*        I              =  I + 1 */
/*        F( ICHAR('letter')) =  I */
/*        RECOG(I)       = 'the full substring that's recognized ' */
/*        WIDTH(I)       =  number of characters required to match */
/*        CLASS(I)       = 'the classification of this substring' */
/*        L( ICHAR('b')) =  I */

/*        Note matching is performed from the first string in the */
/*        group to the last. */


	for (i__ = 32; i__ <= 126; ++i__) {
	    f[(i__1 = i__ - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		    "zztime_", (ftnlen)1800)] = 0;
	    l[(i__1 = i__ - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		    "zztime_", (ftnlen)1801)] = -1;
	    s_copy(names + (((i__1 = i__ - 32) < 95 && 0 <= i__1 ? i__1 : 
		    s_rnge("names", i__1, "zztime_", (ftnlen)1802)) << 5), 
		    "substring", (ftnlen)32, (ftnlen)9);
	}
	s_copy(names + (((i__1 = '\'' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1806)) << 5), "\"Year Abbr"
		"eviation Mark\"", (ftnlen)32, (ftnlen)24);
	s_copy(names + (((i__1 = ',' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1807)) << 5), "comma", (
		ftnlen)32, (ftnlen)5);
	s_copy(names + (((i__1 = '-' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1808)) << 5), "dash", (
		ftnlen)32, (ftnlen)4);
	s_copy(names + (((i__1 = '.' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1809)) << 5), "period", (
		ftnlen)32, (ftnlen)6);
	s_copy(names + (((i__1 = '/' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1810)) << 5), "slash", (
		ftnlen)32, (ftnlen)5);
	s_copy(names + (((i__1 = ':' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1811)) << 5), "colon", (
		ftnlen)32, (ftnlen)5);
	s_copy(names + (((i__1 = 'D' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1812)) << 5), "Day of Month"
		, (ftnlen)32, (ftnlen)12);
	s_copy(names + (((i__1 = 'H' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1813)) << 5), "Hour", (
		ftnlen)32, (ftnlen)4);
	s_copy(names + (((i__1 = 'M' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1814)) << 5), "Minute", (
		ftnlen)32, (ftnlen)6);
	s_copy(names + (((i__1 = 'N' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1815)) << 5), "AM/PM indic"
		"ator", (ftnlen)32, (ftnlen)15);
	s_copy(names + (((i__1 = 'O' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1816)) << 5), "UTC-Offset "
		"indicator", (ftnlen)32, (ftnlen)20);
	s_copy(names + (((i__1 = 'S' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1817)) << 5), "Second", (
		ftnlen)32, (ftnlen)6);
	s_copy(names + (((i__1 = 'Y' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1818)) << 5), "Year", (
		ftnlen)32, (ftnlen)4);
	s_copy(names + (((i__1 = 'Z' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1819)) << 5), "Time-Zone i"
		"ndicator", (ftnlen)32, (ftnlen)19);
	s_copy(names + (((i__1 = '[' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1820)) << 5), "Left Parent"
		"hesis", (ftnlen)32, (ftnlen)16);
	s_copy(names + (((i__1 = ']' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1821)) << 5), "Right Paren"
		"thesis", (ftnlen)32, (ftnlen)17);
	s_copy(names + (((i__1 = 'b' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1822)) << 5), "White Space",
		 (ftnlen)32, (ftnlen)11);
	s_copy(names + (((i__1 = 'd' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1823)) << 5), "Day-of-Year"
		" indicator", (ftnlen)32, (ftnlen)21);
	s_copy(names + (((i__1 = 'e' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1824)) << 5), "Era", (
		ftnlen)32, (ftnlen)3);
	s_copy(names + (((i__1 = 'i' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1825)) << 5), "Integer", (
		ftnlen)32, (ftnlen)7);
	s_copy(names + (((i__1 = 'j' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1826)) << 5), "Julian Date"
		" indicator", (ftnlen)32, (ftnlen)21);
	s_copy(names + (((i__1 = 'm' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1827)) << 5), "Month", (
		ftnlen)32, (ftnlen)5);
	s_copy(names + (((i__1 = 'n' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1828)) << 5), "Decimal Num"
		"ber", (ftnlen)32, (ftnlen)14);
	s_copy(names + (((i__1 = 'o' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1829)) << 5), "UTC-Offset "
		"indicator", (ftnlen)32, (ftnlen)20);
	s_copy(names + (((i__1 = 's' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1830)) << 5), "Time System"
		" specification", (ftnlen)32, (ftnlen)25);
	s_copy(names + (((i__1 = 't' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1831)) << 5), "ISO Time Se"
		"parator", (ftnlen)32, (ftnlen)18);
	s_copy(names + (((i__1 = 'w' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1832)) << 5), "Weekday", (
		ftnlen)32, (ftnlen)7);
	s_copy(names + (((i__1 = 'y' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "zztime_", (ftnlen)1833)) << 5), "Day of Year",
		 (ftnlen)32, (ftnlen)11);
	s_copy(mnmrk, "month", (ftnlen)12, (ftnlen)5);
	s_copy(mnmrk + 12, "MONTH", (ftnlen)12, (ftnlen)5);
	s_copy(mnmrk + 24, "Month", (ftnlen)12, (ftnlen)5);
	s_copy(mnmrk + 36, "mon", (ftnlen)12, (ftnlen)3);
	s_copy(mnmrk + 48, "MON", (ftnlen)12, (ftnlen)3);
	s_copy(mnmrk + 60, "Mon", (ftnlen)12, (ftnlen)3);
	s_copy(wkday, "weekday", (ftnlen)12, (ftnlen)7);
	s_copy(wkday + 12, "WEEKDAY", (ftnlen)12, (ftnlen)7);
	s_copy(wkday + 24, "Weekday", (ftnlen)12, (ftnlen)7);
	s_copy(wkday + 36, "wkd", (ftnlen)12, (ftnlen)3);
	s_copy(wkday + 48, "WKD", (ftnlen)12, (ftnlen)3);
	s_copy(wkday + 60, "Wkd", (ftnlen)12, (ftnlen)3);

/*        Length of the items Month, Mon, weekday, wkd */

	wksize[0] = 7;
	wksize[1] = 3;
	mnsize[0] = 5;
	mnsize[1] = 3;
	i__ = 0;

/*        Tokens beginning with ' ' */

	++i__;
	f[(i__1 = ' ' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)1861)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)1862)) * 12, " ", (ftnlen)
		12, (ftnlen)1);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)1863)] = 1;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)1864)] = 'b';
	l[(i__1 = ' ' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)1865)] = i__;

/*        Tokens beginning with '(' */

	++i__;
	f[(i__1 = '(' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)1871)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)1872)) * 12, "(", (ftnlen)
		12, (ftnlen)1);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)1873)] = 1;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)1874)] = '[';
	l[(i__1 = '(' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)1875)] = i__;

/*        Tokens beginning with ')' */

	++i__;
	f[(i__1 = ')' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)1880)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)1881)) * 12, ")", (ftnlen)
		12, (ftnlen)1);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)1882)] = 1;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)1883)] = ']';
	l[(i__1 = ')' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)1884)] = i__;

/*        Tokens beginning with ',' */

	++i__;
	f[(i__1 = ',' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)1889)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)1890)) * 12, ",", (ftnlen)
		12, (ftnlen)1);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)1891)] = 1;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)1892)] = ',';
	l[(i__1 = ',' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)1893)] = i__;

/*        Tokens beginning with '-' */

	++i__;
	f[(i__1 = '-' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)1899)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)1900)) * 12, "-", (ftnlen)
		12, (ftnlen)1);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)1901)] = 1;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)1902)] = '-';
	l[(i__1 = '-' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)1903)] = i__;

/*        Tokens beginning with '.' */

	++i__;
	f[(i__1 = '.' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)1909)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)1910)) * 12, ".", (ftnlen)
		12, (ftnlen)1);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)1911)] = 1;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)1912)] = '.';
	l[(i__1 = '.' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)1913)] = i__;

/*        Tokens beginning with '/' */

	++i__;
	f[(i__1 = '/' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)1919)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)1920)) * 12, "//", (ftnlen)
		12, (ftnlen)2);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)1921)] = 2;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)1922)] = 'd';
	l[(i__1 = '/' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)1923)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)1926)) * 12, "/", (ftnlen)
		12, (ftnlen)1);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)1927)] = 1;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)1928)] = '/';
	l[(i__1 = '/' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)1929)] = i__;

/*        Tokens beginning with ':' */

	++i__;
	f[(i__1 = ':' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)1934)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)1935)) * 12, "::", (ftnlen)
		12, (ftnlen)2);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)1936)] = 2;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)1937)] = 'd';
	l[(i__1 = ':' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)1938)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)1941)) * 12, ":", (ftnlen)
		12, (ftnlen)1);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)1942)] = 1;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)1943)] = ':';
	l[(i__1 = ':' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)1944)] = i__;

/*        Tokens beginning with 'A' */

	++i__;
	f[(i__1 = 'A' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)1950)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)1951)) * 12, "A.D.", (
		ftnlen)12, (ftnlen)4);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)1952)] = 4;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)1953)] = 'e';
	l[(i__1 = 'A' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)1954)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)1957)) * 12, "AD", (ftnlen)
		12, (ftnlen)2);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)1958)] = 2;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)1959)] = 'e';
	l[(i__1 = 'A' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)1960)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)1963)) * 12, "A.M.", (
		ftnlen)12, (ftnlen)4);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)1964)] = 4;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)1965)] = 'N';
	l[(i__1 = 'A' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)1966)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)1969)) * 12, "AM", (ftnlen)
		12, (ftnlen)2);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)1970)] = 2;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)1971)] = 'N';
	l[(i__1 = 'A' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)1972)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)1975)) * 12, "APRIL", (
		ftnlen)12, (ftnlen)5);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)1976)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)1977)] = 'm';
	l[(i__1 = 'A' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)1978)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)1981)) * 12, "AUGUST", (
		ftnlen)12, (ftnlen)6);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)1982)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)1983)] = 'm';
	l[(i__1 = 'A' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)1984)] = i__;

/*        Tokens beginning with 'B' */

	++i__;
	f[(i__1 = 'B' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)1990)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)1991)) * 12, "B.C.", (
		ftnlen)12, (ftnlen)4);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)1992)] = 4;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)1993)] = 'e';
	l[(i__1 = 'B' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)1994)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)1997)) * 12, "BC", (ftnlen)
		12, (ftnlen)2);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)1998)] = 2;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)1999)] = 'e';
	l[(i__1 = 'B' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2000)] = i__;

/*        Tokens beginning with 'C' */

	++i__;
	f[(i__1 = 'C' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)2006)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2007)) * 12, "CDT", (ftnlen)
		12, (ftnlen)3);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2008)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2009)] = 'Z';
	l[(i__1 = 'C' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2010)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2013)) * 12, "CST", (ftnlen)
		12, (ftnlen)3);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2014)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2015)] = 'Z';
	l[(i__1 = 'C' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2016)] = i__;

/*        Tokens beginning with 'D' */

	++i__;
	f[(i__1 = 'D' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)2022)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2023)) * 12, "DECEMBER", (
		ftnlen)12, (ftnlen)8);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2024)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2025)] = 'm';
	l[(i__1 = 'D' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2026)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2029)) * 12, "D+", (ftnlen)
		12, (ftnlen)2);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2030)] = 2;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2031)] = 'E';
	l[(i__1 = 'D' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2032)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2035)) * 12, "D-", (ftnlen)
		12, (ftnlen)2);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2036)] = 2;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2037)] = 'E';
	l[(i__1 = 'D' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2038)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2041)) * 12, "D", (ftnlen)
		12, (ftnlen)1);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2042)] = 1;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2043)] = 'E';
	l[(i__1 = 'D' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2044)] = i__;

/*        Tokens beginning with 'E' */

	++i__;
	f[(i__1 = 'E' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)2053)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2054)) * 12, "EDT", (ftnlen)
		12, (ftnlen)3);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2055)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2056)] = 'Z';
	l[(i__1 = 'E' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2057)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2060)) * 12, "EST", (ftnlen)
		12, (ftnlen)3);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2061)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2062)] = 'Z';
	l[(i__1 = 'E' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2063)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2066)) * 12, "E+", (ftnlen)
		12, (ftnlen)2);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2067)] = 2;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2068)] = 'E';
	l[(i__1 = 'E' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2069)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2072)) * 12, "E-", (ftnlen)
		12, (ftnlen)2);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2073)] = 2;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2074)] = 'E';
	l[(i__1 = 'E' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2075)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2078)) * 12, "E", (ftnlen)
		12, (ftnlen)1);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2079)] = 1;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2080)] = 'E';
	l[(i__1 = 'E' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2081)] = i__;

/*        Tokens beginning with 'F' */

	++i__;
	f[(i__1 = 'F' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)2088)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2089)) * 12, "FEBRUARY", (
		ftnlen)12, (ftnlen)8);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2090)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2091)] = 'm';
	l[(i__1 = 'F' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2092)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2095)) * 12, "FRIDAY", (
		ftnlen)12, (ftnlen)6);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2096)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2097)] = 'w';
	l[(i__1 = 'F' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2098)] = i__;

/*        Tokens beginning with 'J' */

	++i__;
	f[(i__1 = 'J' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)2104)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2105)) * 12, "JANUARY", (
		ftnlen)12, (ftnlen)7);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2106)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2107)] = 'm';
	l[(i__1 = 'J' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2108)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2111)) * 12, "JD", (ftnlen)
		12, (ftnlen)2);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2112)] = 2;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2113)] = 'j';
	l[(i__1 = 'J' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2114)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2117)) * 12, "JULY", (
		ftnlen)12, (ftnlen)4);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2118)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2119)] = 'm';
	l[(i__1 = 'J' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2120)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2123)) * 12, "JUNE", (
		ftnlen)12, (ftnlen)4);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2124)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2125)] = 'm';
	l[(i__1 = 'J' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2126)] = i__;

/*        Tokens beginning with 'M' */

	++i__;
	f[(i__1 = 'M' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)2132)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2133)) * 12, "MARCH", (
		ftnlen)12, (ftnlen)5);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2134)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2135)] = 'm';
	l[(i__1 = 'M' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2136)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2139)) * 12, "MAY", (ftnlen)
		12, (ftnlen)3);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2140)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2141)] = 'm';
	l[(i__1 = 'M' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2142)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2145)) * 12, "MDT", (ftnlen)
		12, (ftnlen)3);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2146)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2147)] = 'Z';
	l[(i__1 = 'M' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2148)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2151)) * 12, "MONDAY", (
		ftnlen)12, (ftnlen)6);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2152)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2153)] = 'w';
	l[(i__1 = 'M' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2154)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2157)) * 12, "MST", (ftnlen)
		12, (ftnlen)3);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2158)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2159)] = 'Z';
	l[(i__1 = 'M' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2160)] = i__;

/*        Tokens beginning with 'N' */

	++i__;
	f[(i__1 = 'N' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)2166)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2167)) * 12, "NOVEMBER", (
		ftnlen)12, (ftnlen)8);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2168)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2169)] = 'm';
	l[(i__1 = 'N' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2170)] = i__;

/*        Tokens beginning with 'O' */

	++i__;
	f[(i__1 = 'O' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)2176)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2177)) * 12, "OCTOBER", (
		ftnlen)12, (ftnlen)7);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2178)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2179)] = 'm';
	l[(i__1 = 'O' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2180)] = i__;

/*        Tokens beginning with 'P' */

	++i__;
	f[(i__1 = 'P' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)2186)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2187)) * 12, "P.M.", (
		ftnlen)12, (ftnlen)4);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2188)] = 4;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2189)] = 'N';
	l[(i__1 = 'P' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2190)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2193)) * 12, "PDT", (ftnlen)
		12, (ftnlen)3);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2194)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2195)] = 'Z';
	l[(i__1 = 'P' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2196)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2199)) * 12, "PM", (ftnlen)
		12, (ftnlen)2);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2200)] = 2;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2201)] = 'N';
	l[(i__1 = 'P' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2202)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2205)) * 12, "PST", (ftnlen)
		12, (ftnlen)3);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2206)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2207)] = 'Z';
	l[(i__1 = 'P' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2208)] = i__;

/*        Tokens beginning with 'S' */

	++i__;
	f[(i__1 = 'S' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)2214)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2215)) * 12, "SATURDAY", (
		ftnlen)12, (ftnlen)8);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2216)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2217)] = 'w';
	l[(i__1 = 'S' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2218)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2221)) * 12, "SEPTEMBER", (
		ftnlen)12, (ftnlen)9);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2222)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2223)] = 'm';
	l[(i__1 = 'S' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2224)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2227)) * 12, "SUNDAY", (
		ftnlen)12, (ftnlen)6);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2228)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2229)] = 'w';
	l[(i__1 = 'S' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2230)] = i__;

/*        Tokens beginning with 'T' */

	++i__;
	f[(i__1 = 'T' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)2236)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2237)) * 12, "TDB", (ftnlen)
		12, (ftnlen)3);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2238)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2239)] = 's';
	l[(i__1 = 'T' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2240)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2243)) * 12, "TDT", (ftnlen)
		12, (ftnlen)3);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2244)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2245)] = 's';
	l[(i__1 = 'T' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2246)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2249)) * 12, "THURSDAY", (
		ftnlen)12, (ftnlen)8);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2250)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2251)] = 'w';
	l[(i__1 = 'T' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2252)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2255)) * 12, "TUESDAY", (
		ftnlen)12, (ftnlen)7);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2256)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2257)] = 'w';
	l[(i__1 = 'T' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2258)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2261)) * 12, "T", (ftnlen)
		12, (ftnlen)1);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2262)] = 1;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2263)] = 't';
	l[(i__1 = 'T' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2264)] = i__;

/*        Tokens beginning with 'U' */

	++i__;
	f[(i__1 = 'U' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)2270)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2271)) * 12, "UTC+", (
		ftnlen)12, (ftnlen)4);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2272)] = 4;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2273)] = 'O';
	l[(i__1 = 'U' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2274)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2277)) * 12, "UTC-", (
		ftnlen)12, (ftnlen)4);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2278)] = 4;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2279)] = 'o';
	l[(i__1 = 'U' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2280)] = i__;
	++i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2283)) * 12, "UTC", (ftnlen)
		12, (ftnlen)3);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2284)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2285)] = 's';
	l[(i__1 = 'U' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2286)] = i__;

/*        Tokens beginning with '''' */

	++i__;
	f[(i__1 = '\'' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)2291)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2292)) * 12, "'", (ftnlen)
		12, (ftnlen)1);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2293)] = 1;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2294)] = '\'';
	l[(i__1 = '\'' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2295)] = i__;

/*        Tokens beginning with 'W' */

	++i__;
	f[(i__1 = 'W' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("f", i__1, 
		"zztime_", (ftnlen)2300)] = i__;
	s_copy(recog + ((i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
		"recog", i__1, "zztime_", (ftnlen)2301)) * 12, "WEDNESDAY", (
		ftnlen)12, (ftnlen)9);
	width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge("width", 
		i__1, "zztime_", (ftnlen)2302)] = 3;
	*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 :
		 s_rnge("class", i__1, "zztime_", (ftnlen)2303)] = 'w';
	l[(i__1 = 'W' - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge("l", i__1, 
		"zztime_", (ftnlen)2304)] = i__;
    }

/*     If the input string is blank, return with an error message. */

    if (s_cmp(string, " ", string_len, (ftnlen)1) == 0) {
	s_copy(error, "The input time string is blank.", error_len, (ftnlen)
		31);
	ret_val = FALSE_;
	return ret_val;
    }

/*     OK. Initializations are out of the way.  We now take */
/*     apart the string. */

    did = FALSE_;
    s_copy(error, " ", error_len, (ftnlen)1);
    s_copy(rep, " ", (ftnlen)32, (ftnlen)1);
    s_copy(pictur, " ", (ftnlen)160, (ftnlen)1);
    size = 0;
    next = 1;
    pnext = 1;
    put = 0;
    ampm = FALSE_;
    last = rtrim_(string, string_len);
    while(next <= last) {

/*        FROM and NEXT point to parts of the string, PFROM and PNEXT */
/*        point to parts of the picture we will construct. */

	from = next;
	pfrom = pnext;
	item = *(unsigned char *)&string[next - 1];

/*        First we try to find an unsigned integer in the string. */

	lx4uns_(string, &from, &to, &nchar, last);
	if (nchar > 0) {

/*           We found an unsigned integer, add a letter to the */
/*           internal representation, note the begin and end */
/*           of the token and set NEXT to the first character */
/*           beyond this token. */

	    ++put;
	    *(unsigned char *)&rep[put - 1] = 'i';
	    begs[(i__1 = put - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		    i__1, "zztime_", (ftnlen)2354)] = from;
	    ends[(i__1 = put - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		    i__1, "zztime_", (ftnlen)2355)] = to;
	    next = to + 1;
	    pto = pfrom + nchar - 1;
	    pnext = pto + 1;
	    s_copy(pictur + (pfrom - 1), string + (from - 1), pto - (pfrom - 
		    1), to - (from - 1));
	    pbegs[(i__1 = put - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("pbegs", 
		    i__1, "zztime_", (ftnlen)2360)] = pfrom;
	    pends[(i__1 = put - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("pends", 
		    i__1, "zztime_", (ftnlen)2361)] = pto;
	} else if (item == blank) {

/*           We have a blank.  We lump all consecutive */
/*           blanks together as one big fat blank. */

	    ++put;
	    to = from;
	    begs[(i__1 = put - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		    i__1, "zztime_", (ftnlen)2370)] = from;
	    *(unsigned char *)&rep[put - 1] = 'b';
	    while(item == blank && to <= last) {
		++to;
		if (to <= last) {
		    item = *(unsigned char *)&string[to - 1];
		}
	    }
	    next = to;
	    --to;
	    ends[(i__1 = put - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		    i__1, "zztime_", (ftnlen)2384)] = to;
	    pto = pfrom + to - from;
	    pnext = pto + 1;
	    s_copy(pictur + (pfrom - 1), string + (from - 1), pto - (pfrom - 
		    1), to - (from - 1));
	    pbegs[(i__1 = put - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("pbegs", 
		    i__1, "zztime_", (ftnlen)2389)] = pfrom;
	    pends[(i__1 = put - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("pends", 
		    i__1, "zztime_", (ftnlen)2390)] = pto;
	} else if (item == 9) {

/*           We've got a tab character, we treat tabs as */
/*           blanks. */

	    ++put;
	    *(unsigned char *)&rep[put - 1] = 'b';
	    begs[(i__1 = put - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		    i__1, "zztime_", (ftnlen)2399)] = from;
	    ends[(i__1 = put - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		    i__1, "zztime_", (ftnlen)2400)] = from;
	    ++next;
	    pto = pfrom;
	    pnext = pto + 1;
	    s_copy(pictur + (pfrom - 1), " ", pto - (pfrom - 1), (ftnlen)1);
	    pbegs[(i__1 = put - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("pbegs", 
		    i__1, "zztime_", (ftnlen)2406)] = pfrom;
	    pends[(i__1 = put - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("pends", 
		    i__1, "zztime_", (ftnlen)2407)] = pfrom;
	} else if (item < 32 || item > 126) {

/*           This is a non-printing character.  This is */
/*           regarded as an error. */

	    s_copy(error, string, error_len, string_len);
	    zzinssub_(error, "<", &next, error, error_len, (ftnlen)1, 
		    error_len);

/*           Overwrite the non-printing character with a */
/*           closing angle bracket. */

	    if (next < i_len(error, error_len)) {
		i__1 = next;
		s_copy(error + i__1, ">", next + 1 - i__1, (ftnlen)1);
	    }
	    prefix_("There is a non-printing, non-tab character (ASCII #) at"
		    " position # of the time string: ", &c__1, error, (ftnlen)
		    87, error_len);
	    repmi_(error, "#", &item, error, error_len, (ftnlen)1, error_len);
	    repmi_(error, "#", &next, error, error_len, (ftnlen)1, error_len);
	    ret_val = FALSE_;
	    return ret_val;
	} else {

/*           This has to be one of the known types or we */
/*           have an unknown component in the string.  We've constructed */
/*           a "parsing" table for handling these special cases. */
/*           This table uses the first letter of the string */
/*           to begin a search.  We get that code and force it */
/*           into a suitable range. */

	    ucase_(string + (next - 1), this__, (ftnlen)1, (ftnlen)1);
	    item = *(unsigned char *)this__;
	    from = next;
	    check = TRUE_;
	    i__ = f[(i__1 = item - 32) < 95 && 0 <= i__1 ? i__1 : s_rnge(
		    "f", i__1, "zztime_", (ftnlen)2451)];
	    while(check && i__ <= l[(i__1 = item - 32) < 95 && 0 <= i__1 ? 
		    i__1 : s_rnge("l", i__1, "zztime_", (ftnlen)2453)]) {
		w = width[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 : s_rnge(
			"width", i__1, "zztime_", (ftnlen)2455)];
		to = from + w - 1;
		got = samsbi_(string, &from, &to, recog + ((i__1 = i__ - 1) < 
			70 && 0 <= i__1 ? i__1 : s_rnge("recog", i__1, "zzti"
			"me_", (ftnlen)2458)) * 12, &c__1, &w, string_len, (
			ftnlen)12);
		if (got) {

/*                 We have a match.  If it is the match of a month */
/*                 or day of the week, we keep looking for the */
/*                 end of the match. */

		    if (*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 && 0 
			    <= i__1 ? i__1 : s_rnge("class", i__1, "zztime_", 
			    (ftnlen)2466)] == 'm' || *(unsigned char *)&
			    class__[(i__2 = i__ - 1) < 70 && 0 <= i__2 ? i__2 
			    : s_rnge("class", i__2, "zztime_", (ftnlen)2466)] 
			    == 'w') {
			s_copy(spcial, recog + ((i__1 = i__ - 1) < 70 && 0 <= 
				i__1 ? i__1 : s_rnge("recog", i__1, "zztime_",
				 (ftnlen)2470)) * 12, (ftnlen)12, (ftnlen)12);
			r__ = rtrim_(spcial, (ftnlen)12);
			++w;
			++to;
			while(samchi_(string, &to, spcial, &w, string_len, 
				r__)) {
			    ++w;
			    ++to;
			}
			--to;
			if (w > r__) {
			    kind = 1;
			} else {
			    kind = 2;
			}
			if (*(unsigned char *)this__ != *(unsigned char *)&
				string[next - 1]) {
			    case__ = 1;
			} else if (s_cmp(string + (next - 1), spcial, (ftnlen)
				3, (ftnlen)3) == 0) {
			    case__ = 2;
			} else {
			    case__ = 3;
			}
			if (*(unsigned char *)&class__[(i__1 = i__ - 1) < 70 
				&& 0 <= i__1 ? i__1 : s_rnge("class", i__1, 
				"zztime_", (ftnlen)2497)] == 'm') {
			    pto = pfrom + mnsize[(i__1 = kind - 1) < 2 && 0 <=
				     i__1 ? i__1 : s_rnge("mnsize", i__1, 
				    "zztime_", (ftnlen)2499)] - 1;
			    pnext = pto + 1;
			    s_copy(pictur + (pfrom - 1), mnmrk + ((i__1 = 
				    case__ + kind * 3 - 4) < 6 && 0 <= i__1 ? 
				    i__1 : s_rnge("mnmrk", i__1, "zztime_", (
				    ftnlen)2501)) * 12, pto - (pfrom - 1), (
				    ftnlen)12);
			} else {
			    pto = pfrom + wksize[(i__1 = kind - 1) < 2 && 0 <=
				     i__1 ? i__1 : s_rnge("wksize", i__1, 
				    "zztime_", (ftnlen)2505)] - 1;
			    pnext = pto + 1;
			    s_copy(pictur + (pfrom - 1), wkday + ((i__1 = 
				    case__ + kind * 3 - 4) < 6 && 0 <= i__1 ? 
				    i__1 : s_rnge("wkday", i__1, "zztime_", (
				    ftnlen)2507)) * 12, pto - (pfrom - 1), (
				    ftnlen)12);
			}
		    } else if (*(unsigned char *)&class__[(i__1 = i__ - 1) < 
			    70 && 0 <= i__1 ? i__1 : s_rnge("class", i__1, 
			    "zztime_", (ftnlen)2511)] == 'e') {
			pto = pfrom + 2;
			pnext = pto + 1;
			if (*(unsigned char *)&string[from - 1] == *(unsigned 
				char *)this__) {
			    s_copy(pictur + (pfrom - 1), "ERA", pto - (pfrom 
				    - 1), (ftnlen)3);
			} else {
			    s_copy(pictur + (pfrom - 1), "era", pto - (pfrom 
				    - 1), (ftnlen)3);
			}
		    } else if (*(unsigned char *)&class__[(i__1 = i__ - 1) < 
			    70 && 0 <= i__1 ? i__1 : s_rnge("class", i__1, 
			    "zztime_", (ftnlen)2522)] == 'N') {
			pto = pfrom + 3;
			pnext = pto + 1;
			if (*(unsigned char *)&string[from - 1] == *(unsigned 
				char *)this__) {
			    s_copy(pictur + (pfrom - 1), "AMPM", pto - (pfrom 
				    - 1), (ftnlen)4);
			} else {
			    s_copy(pictur + (pfrom - 1), "ampm", pto - (pfrom 
				    - 1), (ftnlen)4);
			}
			ampm = TRUE_;
		    } else {
			pto = pfrom + to - from;
			pnext = pto + 1;
			s_copy(pictur + (pfrom - 1), string + (from - 1), pto 
				- (pfrom - 1), to - (from - 1));
		    }
		    ++put;
		    *(unsigned char *)&rep[put - 1] = *(unsigned char *)&
			    class__[(i__1 = i__ - 1) < 70 && 0 <= i__1 ? i__1 
			    : s_rnge("class", i__1, "zztime_", (ftnlen)2543)];
		    begs[(i__1 = put - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge(
			    "begs", i__1, "zztime_", (ftnlen)2544)] = from;
		    ends[(i__1 = put - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge(
			    "ends", i__1, "zztime_", (ftnlen)2545)] = to;
		    pbegs[(i__1 = put - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge(
			    "pbegs", i__1, "zztime_", (ftnlen)2546)] = pfrom;
		    pends[(i__1 = put - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge(
			    "pends", i__1, "zztime_", (ftnlen)2547)] = pto;
		    check = FALSE_;
		    next = to + 1;
		}
		++i__;
	    }

/*           If we reach the end of the loop and CHECK is still */
/*           set to TRUE, we have a bit of unrecognizable string. */

	    if (check) {
		s_copy(error, string, error_len, string_len);
		i__1 = from + 1;
		zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, 
			error_len);
		zzinssub_(error, "<", &from, error, error_len, (ftnlen)1, 
			error_len);
		prefix_("The input string contains an unrecognizable substri"
			"ng beginning at the character marked by <#>: \"", &
			c__0, error, (ftnlen)97, error_len);
		suffix_("\"", &c__0, error, (ftnlen)1, error_len);
		repmc_(error, "#", string + (from - 1), error, error_len, (
			ftnlen)1, (ftnlen)1, error_len);
		ret_val = FALSE_;
		return ret_val;
	    }
	}
    }
    size = put;
    ret_val = TRUE_;
    return ret_val;
/* $Procedure      ZZUNPCK ( Private --- Unpack a time string ) */

L_zzunpck:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Unpack the time string and parse its components using the */
/*     stored internal representation. */

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

/*      TIME --- PRIVATE */

/* $ Declarations */

/*     IMPLICIT NONE */
/*     CHARACTER*(*)         STRING */
/*     LOGICAL               YABBRV */
/*     DOUBLE PRECISION      TVEC ( * ) */
/*     INTEGER               E */
/*     CHARACTER*(*)         TRANSL */
/*     CHARACTER*(*)         ERROR */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   is a time string that has been tokenized. */
/*     YABBRV     I   has the year been abbreviated. */
/*     TVEC       O   is a vector of time components */
/*     E          O   is the actual number of components present */
/*     TRANSL     O   is the type TVEC ( YMD or YD ) */
/*     PIC        O   is a picture of the format used for the time string */
/*     ERROR      O   a diagnostic of any problems */

/*     The function returns TRUE if the string was unpacked completely. */

/* $ Detailed_Input */

/*     STRING     is the original string from which the current */
/*                internal representation was derived. */

/*     YABBRV     is a logical that indicates whether or not an */
/*                abbreviated year was encountered in the string. */
/*                YABBRV is TRUE if such an abbreviation was present */
/*                otherwise it is FALSE. */

/* $ Detailed_Output */

/*     TVEC       is a double precision array of the parsed time */
/*                components.  TVEC will have either 5 or 6 values */
/*                depending upon whether the string is Year, Month, */
/*                and Day of Month, or Year and Day of Year. */

/*     E          is the actual number of components that were */
/*                present in the internal representation. */

/*                If STRING cannot be fully resolved, E is returned */
/*                as a zero. */

/*     TRANSL     is the type of time vector.  The value will be */
/*                'YD' (day of year) or 'YMD' (Year, Month, Day). */

/*                If STRING cannot be fully resolved, TRANSL is */
/*                returned as a blank. */

/*     PIC        is a picture of the time format corresponding the */
/*                the time string in the last call to ZZTOKNS. */

/*                If some part of the input string can't be identified */
/*                PIC is returned as a blank.  Note that there is a */
/*                distinction between recognizable and parsable. */
/*                The input string must be unambiguous to be parsable, */
/*                However, even if a string is ambiguous it may */
/*                correspond to a legitimate format picture.  Since */
/*                occasionally, that's what you want (an ambiguous */
/*                format), we allow it in PIC. */

/*     ERROR      is a diagnostic that indicates some problem in */
/*                resolving STRING.  If no problems occur ERROR */
/*                is returned as a blank. */

/*     The function returns TRUE if STRING was successfully unpacked. */
/*     That is the string is parsed and is unambiguously recognized. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is the last routine that will normally be */
/*     called by a time parsing routine.  This call should be */
/*     made after all combinations, replacements and removals */
/*     that make sense to perform have been made. */

/* $ Examples */

/*     See TPARTV */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB) */

/*        Re-ordered header sections. */

/* -    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT) */

/*        The main routine (which should never be called) now returns */
/*        the value .FALSE. */

/* -    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT) */

/*        Added a RETURN statement at the end of the main routine. */
/*        Enhanced error message for the case when the input string */
/*        to ZZTOKNS has a non-printing character. */

/* -    SPICELIB Version 1.0.0, 4-APR-1996 (WLT) */


/* -& */
    nyear = 0;
    nmon = 0;
    nday = 0;
    nhour = 0;
    nmin = 0;
    nsec = 0;
    ndoy = 0;
    njd = 0;
    *e = 0;
    s_copy(transl, " ", transl_len, (ftnlen)1);
    hms[0] = 0.;
    hms[1] = 0.;
    hms[2] = 0.;
    for (i__ = size; i__ >= 1; --i__) {
	item = *(unsigned char *)&rep[i__ - 1];
	j = begs[(i__1 = i__ - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		i__1, "zztime_", (ftnlen)2783)];
	k = ends[(i__1 = i__ - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		i__1, "zztime_", (ftnlen)2784)];
	if (item == 'Y') {
	    ++nyear;
	    ++(*e);
	    nparsd_(string + (j - 1), tvec, error, &ptr, k - (j - 1), 
		    error_len);
	    if (*yabbrv) {
		zzrepsub_(pictur, &pbegs[(i__1 = i__ - 1) < 32 && 0 <= i__1 ? 
			i__1 : s_rnge("pbegs", i__1, "zztime_", (ftnlen)2793)]
			, &pends[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : 
			s_rnge("pends", i__2, "zztime_", (ftnlen)2793)], 
			"YR", pictur, (ftnlen)160, (ftnlen)2, (ftnlen)160);
	    } else {
		zzrepsub_(pictur, &pbegs[(i__1 = i__ - 1) < 32 && 0 <= i__1 ? 
			i__1 : s_rnge("pbegs", i__1, "zztime_", (ftnlen)2796)]
			, &pends[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : 
			s_rnge("pends", i__2, "zztime_", (ftnlen)2796)], 
			"YYYY", pictur, (ftnlen)160, (ftnlen)4, (ftnlen)160);
	    }
	} else if (item == 'm') {
	    ++nmon;
	    ++(*e);
	    ucase_(string + (j - 1), month, k - (j - 1), (ftnlen)3);
	    value = isrchc_(month, &c__12, months, (ftnlen)3, (ftnlen)3);
	    if (value == 0) {
		nparsd_(string + (j - 1), &tvec[1], error, &ptr, k - (j - 1), 
			error_len);
		zzrepsub_(pictur, &pbegs[(i__1 = i__ - 1) < 32 && 0 <= i__1 ? 
			i__1 : s_rnge("pbegs", i__1, "zztime_", (ftnlen)2810)]
			, &pends[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : 
			s_rnge("pends", i__2, "zztime_", (ftnlen)2810)], 
			"MM", pictur, (ftnlen)160, (ftnlen)2, (ftnlen)160);
	    } else {
		tvec[1] = (doublereal) value;
	    }
	} else if (item == 'D') {
	    ++nday;
	    ++(*e);
	    nparsd_(string + (j - 1), &tvec[2], error, &ptr, k - (j - 1), 
		    error_len);
	    zzmkpc_(pictur, &pbegs[(i__1 = i__ - 1) < 32 && 0 <= i__1 ? i__1 :
		     s_rnge("pbegs", i__1, "zztime_", (ftnlen)2823)], &pends[(
		    i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("pends",
		     i__2, "zztime_", (ftnlen)2823)], "DD", string + (j - 1), 
		    (ftnlen)160, (ftnlen)2, k - (j - 1));
	} else if (item == 'y') {
	    ++ndoy;
	    ++(*e);
	    nparsd_(string + (j - 1), &tvec[1], error, &ptr, k - (j - 1), 
		    error_len);
	    zzmkpc_(pictur, &pbegs[(i__1 = i__ - 1) < 32 && 0 <= i__1 ? i__1 :
		     s_rnge("pbegs", i__1, "zztime_", (ftnlen)2833)], &pends[(
		    i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("pends",
		     i__2, "zztime_", (ftnlen)2833)], "DOY", string + (j - 1),
		     (ftnlen)160, (ftnlen)3, k - (j - 1));
	} else if (item == 'H') {
	    ++nhour;
	    ++(*e);
	    nparsd_(string + (j - 1), hms, error, &ptr, k - (j - 1), 
		    error_len);

/*           We have to handle the hour component based on the */
/*           presence of the AM/PM mark in the picture.  We earlier */
/*           set up the logical AMPM to indicate its presence. */

	    if (ampm) {
		zzmkpc_(pictur, &pbegs[(i__1 = i__ - 1) < 32 && 0 <= i__1 ? 
			i__1 : s_rnge("pbegs", i__1, "zztime_", (ftnlen)2849)]
			, &pends[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : 
			s_rnge("pends", i__2, "zztime_", (ftnlen)2849)], 
			"AP", string + (j - 1), (ftnlen)160, (ftnlen)2, k - (
			j - 1));
	    } else {
		zzmkpc_(pictur, &pbegs[(i__1 = i__ - 1) < 32 && 0 <= i__1 ? 
			i__1 : s_rnge("pbegs", i__1, "zztime_", (ftnlen)2854)]
			, &pends[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : 
			s_rnge("pends", i__2, "zztime_", (ftnlen)2854)], 
			"HR", string + (j - 1), (ftnlen)160, (ftnlen)2, k - (
			j - 1));
	    }
	} else if (item == 'M') {
	    ++nmin;
	    ++(*e);
	    nparsd_(string + (j - 1), &hms[1], error, &ptr, k - (j - 1), 
		    error_len);
	    zzmkpc_(pictur, &pbegs[(i__1 = i__ - 1) < 32 && 0 <= i__1 ? i__1 :
		     s_rnge("pbegs", i__1, "zztime_", (ftnlen)2866)], &pends[(
		    i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("pends",
		     i__2, "zztime_", (ftnlen)2866)], "MN", string + (j - 1), 
		    (ftnlen)160, (ftnlen)2, k - (j - 1));
	} else if (item == 'S') {
	    ++nsec;
	    ++(*e);
	    nparsd_(string + (j - 1), &hms[2], error, &ptr, k - (j - 1), 
		    error_len);
	    zzmkpc_(pictur, &pbegs[(i__1 = i__ - 1) < 32 && 0 <= i__1 ? i__1 :
		     s_rnge("pbegs", i__1, "zztime_", (ftnlen)2876)], &pends[(
		    i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("pends",
		     i__2, "zztime_", (ftnlen)2876)], "SC", string + (j - 1), 
		    (ftnlen)160, (ftnlen)2, k - (j - 1));
	} else if (item == 'J') {
	    ++njd;
	    ++(*e);
	    nparsd_(string + (j - 1), tvec, error, &ptr, k - (j - 1), 
		    error_len);
	    zzmkpc_(pictur, &pbegs[(i__1 = i__ - 1) < 32 && 0 <= i__1 ? i__1 :
		     s_rnge("pbegs", i__1, "zztime_", (ftnlen)2886)], &pends[(
		    i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("pends",
		     i__2, "zztime_", (ftnlen)2886)], "JULIAND", string + (j 
		    - 1), (ftnlen)160, (ftnlen)7, k - (j - 1));
	} else if (item == 'i') {
	    s_copy(error, string, error_len, string_len);
	    i__1 = k + 1;
	    zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, 
		    error_len);
	    zzinssub_(error, "<", &j, error, error_len, (ftnlen)1, error_len);
	    prefix_("The meaning of the integer <#> could not be determined:"
		    " '", &c__1, error, (ftnlen)57, error_len);
	    suffix_("'", &c__0, error, (ftnlen)1, error_len);
	    repmc_(error, "#", string + (j - 1), error, error_len, (ftnlen)1, 
		    k - (j - 1), error_len);
	    *e = 0;
	    s_copy(pic, " ", pic_len, (ftnlen)1);
	    ret_val = FALSE_;
	    return ret_val;
	} else if (item == 'n') {
	    s_copy(error, string, error_len, string_len);
	    i__1 = k + 1;
	    zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, 
		    error_len);
	    zzinssub_(error, "<", &j, error, error_len, (ftnlen)1, error_len);
	    prefix_("The meaning of the decimal number <#> could not be dete"
		    "rmined: ", &c__1, error, (ftnlen)63, error_len);
	    repmc_(error, "#", string + (j - 1), error, error_len, (ftnlen)1, 
		    k - (j - 1), error_len);
	    *e = 0;
	    s_copy(pic, " ", pic_len, (ftnlen)1);
	    ret_val = FALSE_;
	    return ret_val;
	} else {
	    s_copy(error, string, error_len, string_len);
	    i__1 = k + 1;
	    zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, 
		    error_len);
	    zzinssub_(error, "<", &j, error, error_len, (ftnlen)1, error_len);
	    prefix_("An unexpected # (\"#\") was encountered in the time str"
		    "ing: ", &c__1, error, (ftnlen)58, error_len);
	    repmc_(error, "#", names + (((i__1 = item - 32) < 95 && 0 <= i__1 
		    ? i__1 : s_rnge("names", i__1, "zztime_", (ftnlen)2934)) 
		    << 5), error, error_len, (ftnlen)1, (ftnlen)32, error_len)
		    ;
	    repmc_(error, "#", string + (j - 1), error, error_len, (ftnlen)1, 
		    k - (j - 1), error_len);
	    s_copy(pic, " ", pic_len, (ftnlen)1);
	    *e = 0;
	    ret_val = FALSE_;
	    return ret_val;
	}
    }

/*     Ok.  Check the counts of substrings to make sure everything */
/*     looks ok. If so move the HMS into the appropriate slots */
/*     in TVEC, set the kind of TVEC, set the function value to YES, */
/*     and RETURN.  Note regardless of the correctness of the parsing */
/*     we have a legitimate format picture at this point so we keep it. */

    s_copy(pic, pictur, pic_len, (ftnlen)160);
    if (nyear == 1 && nmon == 1 && nday == 1 && ndoy == 0 && njd == 0 && 
	    nhour <= 1 && nmin <= nhour && nsec <= nmin) {
	tvec[3] = hms[0];
	tvec[4] = hms[1];
	tvec[5] = hms[2];
	s_copy(transl, "YMD", transl_len, (ftnlen)3);
	ret_val = TRUE_;
	return ret_val;
    } else if (nyear == 1 && nmon == 0 && nday == 0 && njd == 0 && ndoy == 1 
	    && nhour <= 1 && nmin <= nhour && nsec <= nmin) {
	tvec[2] = hms[0];
	tvec[3] = hms[1];
	tvec[4] = hms[2];
	s_copy(transl, "YD", transl_len, (ftnlen)2);
	ret_val = TRUE_;
	return ret_val;
    } else if (nyear == 0 && nmon == 0 && nday == 0 && njd == 1 && ndoy == 0 
	    && nhour <= 0 && nmin <= 0 && nsec <= 0) {
	s_copy(transl, "JD", transl_len, (ftnlen)2);
	ret_val = TRUE_;
	return ret_val;
    }

/*     If we're still here, there is some kind of an error */
/*     in the input string.   There are a lot of possible */
/*     problems. */

    *e = 0;
    if (nyear == 0 && nday == 0 && njd == 0 && ndoy == 0 && nhour == 0 && 
	    nmin == 0 && nsec == 0) {
	s_copy(error, "No numeric components were supplied in the time strin"
		"g. ", error_len, (ftnlen)56);
    } else if (njd == 1) {
	s_copy(error, "The string possesses calendar components in addition "
		"to Julian Date specifier. ", error_len, (ftnlen)79);
    } else if (njd > 1) {
	s_copy(error, "There is more than one Julian Date specified in the e"
		"poch string. ", error_len, (ftnlen)66);
    } else if (nyear == 0) {
	s_copy(error, "The year associated with the calendar string \"#\" co"
		"uld not be identified. ", error_len, (ftnlen)74);
	repmc_(error, "#", string, error, error_len, (ftnlen)1, string_len, 
		error_len);
    } else if (nyear > 1) {
	s_copy(error, string, error_len, string_len);
	s_copy(messge, "Two substrings indicating a calendar year were ident"
		"ified in the input time string <#> and <#>: \"", (ftnlen)160, 
		(ftnlen)97);
	p1 = pos_(rep, "Y", &c__1, (ftnlen)32, (ftnlen)1);
	i__1 = p1 + 1;
	p2 = pos_(rep, "Y", &i__1, (ftnlen)32, (ftnlen)1);
	j = begs[(i__1 = p2 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		i__1, "zztime_", (ftnlen)3053)];
	k = ends[(i__1 = p2 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		i__1, "zztime_", (ftnlen)3054)];
	i__1 = k + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &j, error, error_len, (ftnlen)1, error_len);
	repmc_(messge, "#", string + (j - 1), messge, (ftnlen)160, (ftnlen)1, 
		k - (j - 1), (ftnlen)160);
	j = begs[(i__1 = p1 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		i__1, "zztime_", (ftnlen)3060)];
	k = ends[(i__1 = p1 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		i__1, "zztime_", (ftnlen)3061)];
	i__1 = k + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &j, error, error_len, (ftnlen)1, error_len);
	repmc_(messge, "#", string + (j - 1), messge, (ftnlen)160, (ftnlen)1, 
		k - (j - 1), (ftnlen)160);
	prefix_(messge, &c__1, error, (ftnlen)160, error_len);
	suffix_("\"", &c__0, error, (ftnlen)1, error_len);
    } else if (nmon > 0 && ndoy > 0) {
	s_copy(error, string, error_len, string_len);
	s_copy(messge, "Both a day of year and month were identified in the "
		"input string. \"", (ftnlen)160, (ftnlen)67);
/* Computing MAX */
	i__1 = pos_(rep, "m", &c__1, (ftnlen)32, (ftnlen)1), i__2 = pos_(rep, 
		"y", &c__1, (ftnlen)32, (ftnlen)1);
	p2 = max(i__1,i__2);
/* Computing MIN */
	i__1 = pos_(rep, "m", &c__1, (ftnlen)32, (ftnlen)1), i__2 = pos_(rep, 
		"y", &c__1, (ftnlen)32, (ftnlen)1);
	p1 = min(i__1,i__2);
	j = begs[(i__1 = p2 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		i__1, "zztime_", (ftnlen)3083)];
	k = ends[(i__1 = p2 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		i__1, "zztime_", (ftnlen)3084)];
	i__1 = k + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &j, error, error_len, (ftnlen)1, error_len);
	j = begs[(i__1 = p1 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		i__1, "zztime_", (ftnlen)3089)];
	k = ends[(i__1 = p1 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		i__1, "zztime_", (ftnlen)3090)];
	i__1 = k + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &j, error, error_len, (ftnlen)1, error_len);
	prefix_(messge, &c__1, error, (ftnlen)160, error_len);
	suffix_("\"", &c__0, error, (ftnlen)1, error_len);
    } else if (nmon > 1) {
	s_copy(error, string, error_len, string_len);
	s_copy(messge, "Two substrings indicating a calendar month were iden"
		"tified in the input time string <#> and <#>: \"", (ftnlen)160,
		 (ftnlen)98);
	p1 = pos_(rep, "m", &c__1, (ftnlen)32, (ftnlen)1);
	i__1 = p1 + 1;
	p2 = pos_(rep, "m", &i__1, (ftnlen)32, (ftnlen)1);
	j = begs[(i__1 = p2 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		i__1, "zztime_", (ftnlen)3109)];
	k = ends[(i__1 = p2 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		i__1, "zztime_", (ftnlen)3110)];
	i__1 = k + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &j, error, error_len, (ftnlen)1, error_len);
	repmc_(messge, "#", string + (j - 1), messge, (ftnlen)160, (ftnlen)1, 
		k - (j - 1), (ftnlen)160);
	j = begs[(i__1 = p1 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		i__1, "zztime_", (ftnlen)3116)];
	k = ends[(i__1 = p1 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		i__1, "zztime_", (ftnlen)3117)];
	i__1 = k + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &j, error, error_len, (ftnlen)1, error_len);
	repmc_(messge, "#", string + (j - 1), messge, (ftnlen)160, (ftnlen)1, 
		k - (j - 1), (ftnlen)160);
	prefix_(messge, &c__1, error, (ftnlen)160, error_len);
	suffix_("\"", &c__0, error, (ftnlen)1, error_len);
    } else if (ndoy > 1) {
	s_copy(error, string, error_len, string_len);
	s_copy(messge, "Two substrings indicating a day of year were identif"
		"ied in the input time string <#> and <#>: \"", (ftnlen)160, (
		ftnlen)95);
	p1 = pos_(rep, "y", &c__1, (ftnlen)32, (ftnlen)1);
	i__1 = p1 + 1;
	p2 = pos_(rep, "y", &i__1, (ftnlen)32, (ftnlen)1);
	j = begs[(i__1 = p2 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		i__1, "zztime_", (ftnlen)3137)];
	k = ends[(i__1 = p2 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		i__1, "zztime_", (ftnlen)3138)];
	i__1 = k + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &j, error, error_len, (ftnlen)1, error_len);
	repmc_(messge, "#", string + (j - 1), messge, (ftnlen)160, (ftnlen)1, 
		k - (j - 1), (ftnlen)160);
	j = begs[(i__1 = p1 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		i__1, "zztime_", (ftnlen)3144)];
	k = ends[(i__1 = p1 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		i__1, "zztime_", (ftnlen)3145)];
	i__1 = k + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &j, error, error_len, (ftnlen)1, error_len);
	repmc_(messge, "#", string + (j - 1), messge, (ftnlen)160, (ftnlen)1, 
		k - (j - 1), (ftnlen)160);
	prefix_(messge, &c__1, error, (ftnlen)160, error_len);
	suffix_("\"", &c__0, error, (ftnlen)1, error_len);
    } else if (nday > 1) {
	s_copy(error, string, error_len, string_len);
	s_copy(messge, "Two substrings indicating a day of month were identi"
		"fied in the input time string <#> and <#>: \"", (ftnlen)160, (
		ftnlen)96);
	p1 = pos_(rep, "D", &c__1, (ftnlen)32, (ftnlen)1);
	i__1 = p1 + 1;
	p2 = pos_(rep, "D", &i__1, (ftnlen)32, (ftnlen)1);
	j = begs[(i__1 = p2 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		i__1, "zztime_", (ftnlen)3165)];
	k = ends[(i__1 = p2 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		i__1, "zztime_", (ftnlen)3166)];
	i__1 = k + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &j, error, error_len, (ftnlen)1, error_len);
	repmc_(messge, "#", string + (j - 1), messge, (ftnlen)160, (ftnlen)1, 
		k - (j - 1), (ftnlen)160);
	j = begs[(i__1 = p1 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		i__1, "zztime_", (ftnlen)3172)];
	k = ends[(i__1 = p1 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		i__1, "zztime_", (ftnlen)3173)];
	i__1 = k + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &j, error, error_len, (ftnlen)1, error_len);
	repmc_(messge, "#", string + (j - 1), messge, (ftnlen)160, (ftnlen)1, 
		k - (j - 1), (ftnlen)160);
	prefix_(messge, &c__1, error, (ftnlen)160, error_len);
	suffix_("\"", &c__0, error, (ftnlen)1, error_len);
    } else if (nhour > 1) {
	s_copy(error, string, error_len, string_len);
	s_copy(messge, "Two substrings representing an hour of the day were "
		"identified in the input time string <#> and <#>: \"", (ftnlen)
		160, (ftnlen)102);
	p1 = pos_(rep, "H", &c__1, (ftnlen)32, (ftnlen)1);
	i__1 = p1 + 1;
	p2 = pos_(rep, "H", &i__1, (ftnlen)32, (ftnlen)1);
	j = begs[(i__1 = p2 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		i__1, "zztime_", (ftnlen)3193)];
	k = ends[(i__1 = p2 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		i__1, "zztime_", (ftnlen)3194)];
	i__1 = k + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &j, error, error_len, (ftnlen)1, error_len);
	repmc_(messge, "#", string + (j - 1), messge, (ftnlen)160, (ftnlen)1, 
		k - (j - 1), (ftnlen)160);
	j = begs[(i__1 = p1 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		i__1, "zztime_", (ftnlen)3200)];
	k = ends[(i__1 = p1 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		i__1, "zztime_", (ftnlen)3201)];
	i__1 = k + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &j, error, error_len, (ftnlen)1, error_len);
	repmc_(messge, "#", string + (j - 1), messge, (ftnlen)160, (ftnlen)1, 
		k - (j - 1), (ftnlen)160);
	prefix_(messge, &c__1, error, (ftnlen)160, error_len);
	suffix_("\"", &c__0, error, (ftnlen)1, error_len);
    } else if (nmin > 1) {
	s_copy(error, string, error_len, string_len);
	s_copy(messge, "Two substrings representing minutes of the hour were"
		" identified in the input time string <#> and <#>: \"", (
		ftnlen)160, (ftnlen)103);
	p1 = pos_(rep, "M", &c__1, (ftnlen)32, (ftnlen)1);
	i__1 = p1 + 1;
	p2 = pos_(rep, "M", &i__1, (ftnlen)32, (ftnlen)1);
	j = begs[(i__1 = p2 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		i__1, "zztime_", (ftnlen)3221)];
	k = ends[(i__1 = p2 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		i__1, "zztime_", (ftnlen)3222)];
	i__1 = k + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &j, error, error_len, (ftnlen)1, error_len);
	repmc_(messge, "#", string + (j - 1), messge, (ftnlen)160, (ftnlen)1, 
		k - (j - 1), (ftnlen)160);
	j = begs[(i__1 = p1 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		i__1, "zztime_", (ftnlen)3228)];
	k = ends[(i__1 = p1 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		i__1, "zztime_", (ftnlen)3229)];
	i__1 = k + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &j, error, error_len, (ftnlen)1, error_len);
	repmc_(messge, "#", string + (j - 1), messge, (ftnlen)160, (ftnlen)1, 
		k - (j - 1), (ftnlen)160);
	prefix_(messge, &c__1, error, (ftnlen)160, error_len);
	suffix_("\"", &c__0, error, (ftnlen)1, error_len);
    } else if (nsec > 1) {
	s_copy(error, string, error_len, string_len);
	s_copy(messge, "Two substrings representing seconds were identified "
		"in the input time string <#> and <#>: \"", (ftnlen)160, (
		ftnlen)91);
	p1 = pos_(rep, "S", &c__1, (ftnlen)32, (ftnlen)1);
	i__1 = p1 + 1;
	p2 = pos_(rep, "S", &i__1, (ftnlen)32, (ftnlen)1);
	j = begs[(i__1 = p2 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		i__1, "zztime_", (ftnlen)3249)];
	k = ends[(i__1 = p2 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		i__1, "zztime_", (ftnlen)3250)];
	i__1 = k + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &j, error, error_len, (ftnlen)1, error_len);
	repmc_(messge, "#", string + (j - 1), messge, (ftnlen)160, (ftnlen)1, 
		k - (j - 1), (ftnlen)160);
	j = begs[(i__1 = p1 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("begs", 
		i__1, "zztime_", (ftnlen)3256)];
	k = ends[(i__1 = p1 - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ends", 
		i__1, "zztime_", (ftnlen)3257)];
	i__1 = k + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &j, error, error_len, (ftnlen)1, error_len);
	repmc_(messge, "#", string + (j - 1), messge, (ftnlen)160, (ftnlen)1, 
		k - (j - 1), (ftnlen)160);
	prefix_(messge, &c__1, error, (ftnlen)160, error_len);
	suffix_("\"", &c__0, error, (ftnlen)1, error_len);
    } else if (ndoy == 0 && nmon == 0) {
	s_copy(error, "Neither a month nor day of year could be identified i"
		"n the input time string: \"#\" ", error_len, (ftnlen)82);
	repmc_(error, "#", string, error, error_len, (ftnlen)1, string_len, 
		error_len);
    } else if (nmon == 1 && nday == 0) {
	s_copy(error, "A month was identified in the time string \"#\", but "
		"a day of month could not be identified. ", error_len, (ftnlen)
		91);
	repmc_(error, "#", string, error, error_len, (ftnlen)1, string_len, 
		error_len);
    } else if (nmon == 0 && nday == 1) {
	s_copy(error, "A day of month was identified in the time string \""
		"#\", but the month it belongs to could not be identified. ", 
		error_len, (ftnlen)107);
	repmc_(error, "#", string, error, error_len, (ftnlen)1, string_len, 
		error_len);
    } else if (nmin > nhour) {
	s_copy(error, "A minutes components of the time  was identified in t"
		"he time string \"#\", but the hours component could not be i"
		"dentified. ", error_len, (ftnlen)122);
	repmc_(error, "#", string, error, error_len, (ftnlen)1, string_len, 
		error_len);
    } else if (nsec > nmin) {
	s_copy(error, "A seconds components of the time was identified in th"
		"e time string \"#\", but the minutes component could not be "
		"identified. ", error_len, (ftnlen)123);
	repmc_(error, "#", string, error, error_len, (ftnlen)1, string_len, 
		error_len);
    }
    ret_val = FALSE_;
    return ret_val;
/* $Procedure      ZZVALT ( Private --- Value Based Tokens ) */

L_zzvalt:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Examine the value of an integer token and if it is within the */
/*     range from B to E replace the token with the new token */
/*     specified by LETTER. */

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

/*      TIME --- PRIVATE */

/* $ Declarations */

/*     IMPLICIT NONE */
/*     CHARACTER*(*)         STRING */
/*     INTEGER               B */
/*     INTEGER               E */
/*     CHARACTER*(1)         LETTER */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   Original time string. */
/*     B          I   Lower bound of value range */
/*     E          I   Upper bound of value range */
/*     LETTER     I   New token if integer is within range. */

/*     The function returns TRUE if any substitutions are performed. */

/* $ Detailed_Input */

/*     STRING     is an original time string as last supplied to ZZTOKNS. */

/*     B          is the lower bound of some test range of integers */

/*     E          is the upper bound of some test range of integers */

/*     LETTER     is the new token value to put in place of 'i' if */
/*                the value of the integer is between B and E */
/*                (inclusive). */
/* $ Detailed_Output */

/*     The function returns TRUE if any substitutions are performed.. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This function replaces every occurrence of 'i' in the internal */
/*     representation by the value LETTER if the numerical value */
/*     of the token corresponding to 'i' is between B and E. */

/*     This is used primarily to identify YEAR tokens in a time */
/*     string. */

/* $ Examples */

/*     See TPARTV */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB) */

/*        Re-ordered header sections. */

/* -    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT) */

/*        The main routine (which should never be called) now returns */
/*        the value .FALSE. */

/* -    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT) */

/*        Added a RETURN statement at the end of the main routine. */
/*        Enhanced error message for the case when the input string */
/*        to ZZTOKNS has a non-printing character. */

/* -    SPICELIB Version 1.0.0, 4-APR-1996 (WLT) */


/* -& */

/*     So far no translations have been performed. */

    did = FALSE_;

/*     Examine each token to see if it is an integer. */

    i__1 = size;
    for (i__ = 1; i__ <= i__1; ++i__) {
	item = *(unsigned char *)&rep[i__ - 1];
	if (item == 'i') {

/*           We've got an integer.  Parse it to see if it */
/*           is in the specified range. */

	    j = begs[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("begs"
		    , i__2, "zztime_", (ftnlen)3469)];
	    k = ends[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("ends"
		    , i__2, "zztime_", (ftnlen)3470)];
	    nparsi_(string + (j - 1), &value, myerr, &ptr, k - (j - 1), (
		    ftnlen)160);
	    if (ptr == 0 && value >= *b && value <= *e) {
		*(unsigned char *)&rep[i__ - 1] = *(unsigned char *)letter;
		did = TRUE_;
	    }
	}
    }
    ret_val = did;
    return ret_val;
} /* zztime_ */

logical zztime_(char *string, char *transl, char *letter, char *error, char *
	pic, doublereal *tvec, integer *b, integer *e, logical *l2r, logical *
	yabbrv, ftnlen string_len, ftnlen transl_len, ftnlen letter_len, 
	ftnlen error_len, ftnlen pic_len)
{
    return zztime_0_(0, string, transl, letter, error, pic, tvec, b, e, l2r, 
	    yabbrv, string_len, transl_len, letter_len, error_len, pic_len);
    }

logical zzcmbt_(char *string, char *letter, logical *l2r, ftnlen string_len, 
	ftnlen letter_len)
{
    return zztime_0_(1, string, (char *)0, letter, (char *)0, (char *)0, (
	    doublereal *)0, (integer *)0, (integer *)0, l2r, (logical *)0, 
	    string_len, (ftnint)0, letter_len, (ftnint)0, (ftnint)0);
    }

logical zzgrep_(char *string, ftnlen string_len)
{
    return zztime_0_(2, string, (char *)0, (char *)0, (char *)0, (char *)0, (
	    doublereal *)0, (integer *)0, (integer *)0, (logical *)0, (
	    logical *)0, string_len, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)
	    0);
    }

logical zzispt_(char *string, integer *b, integer *e, ftnlen string_len)
{
    return zztime_0_(3, string, (char *)0, (char *)0, (char *)0, (char *)0, (
	    doublereal *)0, b, e, (logical *)0, (logical *)0, string_len, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

logical zzist_(char *letter, ftnlen letter_len)
{
    return zztime_0_(4, (char *)0, (char *)0, letter, (char *)0, (char *)0, (
	    doublereal *)0, (integer *)0, (integer *)0, (logical *)0, (
	    logical *)0, (ftnint)0, (ftnint)0, letter_len, (ftnint)0, (ftnint)
	    0);
    }

logical zznote_(char *letter, integer *b, integer *e, ftnlen letter_len)
{
    return zztime_0_(5, (char *)0, (char *)0, letter, (char *)0, (char *)0, (
	    doublereal *)0, b, e, (logical *)0, (logical *)0, (ftnint)0, (
	    ftnint)0, letter_len, (ftnint)0, (ftnint)0);
    }

logical zzremt_(char *letter, ftnlen letter_len)
{
    return zztime_0_(6, (char *)0, (char *)0, letter, (char *)0, (char *)0, (
	    doublereal *)0, (integer *)0, (integer *)0, (logical *)0, (
	    logical *)0, (ftnint)0, (ftnint)0, letter_len, (ftnint)0, (ftnint)
	    0);
    }

logical zzsubt_(char *string, char *transl, logical *l2r, ftnlen string_len, 
	ftnlen transl_len)
{
    return zztime_0_(7, string, transl, (char *)0, (char *)0, (char *)0, (
	    doublereal *)0, (integer *)0, (integer *)0, l2r, (logical *)0, 
	    string_len, transl_len, (ftnint)0, (ftnint)0, (ftnint)0);
    }

logical zztokns_(char *string, char *error, ftnlen string_len, ftnlen 
	error_len)
{
    return zztime_0_(8, string, (char *)0, (char *)0, error, (char *)0, (
	    doublereal *)0, (integer *)0, (integer *)0, (logical *)0, (
	    logical *)0, string_len, (ftnint)0, (ftnint)0, error_len, (ftnint)
	    0);
    }

logical zzunpck_(char *string, logical *yabbrv, doublereal *tvec, integer *e, 
	char *transl, char *pic, char *error, ftnlen string_len, ftnlen 
	transl_len, ftnlen pic_len, ftnlen error_len)
{
    return zztime_0_(9, string, transl, (char *)0, error, pic, tvec, (integer 
	    *)0, e, (logical *)0, yabbrv, string_len, transl_len, (ftnint)0, 
	    error_len, pic_len);
    }

logical zzvalt_(char *string, integer *b, integer *e, char *letter, ftnlen 
	string_len, ftnlen letter_len)
{
    return zztime_0_(10, string, (char *)0, letter, (char *)0, (char *)0, (
	    doublereal *)0, b, e, (logical *)0, (logical *)0, string_len, (
	    ftnint)0, letter_len, (ftnint)0, (ftnint)0);
    }

