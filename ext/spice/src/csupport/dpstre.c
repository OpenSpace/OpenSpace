/* dpstre.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure      DPSTRE ( Double Precision Number to Character ) */
/* Subroutine */ int dpstre_(doublereal *x, integer *sigdig, char *string, 
	ftnlen string_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer maxsav = 14;
    static char fmtstr[10] = "(1PE20.13)";

    /* System generated locals */
    integer i__1, i__2;
    icilist ici__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfi(icilist *), do_fio(integer *, char *, ftnlen), e_wsfi(void)
	    ;

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), errdp_(char *, doublereal *, ftnlen), 
	    dpstr_(doublereal *, integer *, char *, ftnlen);
    integer maxsig;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    char numstr[40];

/* $ Abstract */

/*     Take a double precision number and convert it to an equivalent */
/*     character string representation (base 10). */

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

/*     CHARACTER */
/*     CONVERSION */
/*     PARSING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     X          I   A double precision number */
/*     SIGDIG     I   The number of significant digits placed in output */
/*     STRING     O   A character string representation of X */

/* $ Detailed_Input */

/*     X          is a double precision number. */

/*     SIGDIG     is the number of significant digits that are desired */
/*                for the output string. */

/* $ Detailed_Output */


/*     STRING     is a character representation of X to the number of */
/*                significant digits specified by SIGDIG.  The number of */
/*                spaces required to return the requested character */
/*                string is SIGDIG + 6.  If STRING is not declared to */
/*                have adequate length, the number returned will be */
/*                truncated on the right. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     If SIGDIG is less than one, this routine returns one significant */
/*     digit in the output string. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine computes an approximate character representation of */
/*     the input DP number X. The minimum number of significant digits */
/*     returned is 1. The maximum number of significant digits returned */
/*     is 33. */

/*     For the numbers of significant digits less or equal to 14 this */
/*     routines calls DPSTR. */

/*     For the numbers of significant digits between 15 and 33 this */
/*     routines uses the following FORTRAN write statement */

/*         WRITE ( STRING, FMT=(1PEXX.YY) ) */

/*     where XX = (SIGDIG + 6) and YY = (SIGDIG - 1). */

/*     For the numbers of significant digits greater than 33 this */
/*     routines uses the following FORTRAN write statement */

/*         WRITE ( STRING, FMT=(1PE39.32) ) */

/*     as if the number of significant digits was 33. */

/* $ Examples */

/*     This example program prints PI with 3, 14, 17 and 22 significant */
/*     digits: */

/*           DOUBLE PRECISION      PI */
/*           CHARACTER*(80)     TEXT */

/*           CALL DPSTRE( PI(),  3, TEXT ) */
/*           CALL TOSTDO( TEXT ) */
/*           CALL DPSTRE( PI(), 14, TEXT ) */
/*           CALL TOSTDO( TEXT ) */
/*           CALL DPSTRE( PI(), 17, TEXT ) */
/*           CALL TOSTDO( TEXT ) */
/*           CALL DPSTRE( PI(), 22, TEXT ) */
/*           CALL TOSTDO( TEXT ) */

/*           END */

/*     When compiled with 32bit GFORTRAN on a Linux box it produces */
/*     the following output: */

/*            3.14E+00 */
/*            3.1415926535898E+00 */
/*            3.14159265358979312E+00 */
/*            3.141592653589793115998E+00 */

/* $ Restrictions */

/*     The maximum number of significant digits returned is 33. */

/*     If the output string is not declared to be adequately large */
/*     (at least SIGDIG + 6), the numeric string will be truncated */
/*     on the right. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SUPPORT Version 1.0.0, 27-JAN-2012 (BVS) */

/* -& */
/* $ Index_Entries */

/*     d.p. number to character */

/* -& */

/*     Local parameters. */

/*     The maximum number of allowed significant digits is set to 33 */
/*     (=16*2+1). This is an arbitrarily picked value because FORTRAN */
/*     doesn't seen to have a limit. But we do need a limit to make sure */
/*     that the formatted WRITE does not overflow the local buffer */
/*     string. */



/*     Format template. */


/*     Local variables */


/*     Saved variables. */


/*     Initial values. */


/*     Reset the input number of significant digits if it is outside of */
/*     the allowed range (1 to 33). */

/* Computing MIN */
    i__1 = 33, i__2 = max(1,*sigdig);
    maxsig = min(i__1,i__2);

/*     If the number of significant digits is less then or equal to 14, */
/*     outsource conversion to DPSTR. */

    if (maxsig <= 14) {
	dpstr_(x, &maxsig, string, string_len);
    } else {

/*        The number of significant digits is greater than 14. Make */
/*        output format. Do it only for the first call or if the */
/*        previous call had a different number of significant digits. */
/*        Otherwise, use the SAVEd format string from the previous */
/*        call. */

	if (first || maxsig != maxsav) {
	    s_copy(fmtstr, "(1PE#.#)", (ftnlen)10, (ftnlen)8);
	    i__1 = maxsig + 6;
	    repmi_(fmtstr, "#", &i__1, fmtstr, (ftnlen)10, (ftnlen)1, (ftnlen)
		    10);
	    i__1 = maxsig - 1;
	    repmi_(fmtstr, "#", &i__1, fmtstr, (ftnlen)10, (ftnlen)1, (ftnlen)
		    10);
	    maxsav = maxsig;
	    first = FALSE_;
	}

/*        Use WRITE to create a temporary output string. This string is */
/*        declared to have enough room for any allowed numbers of */
/*        significant digits. We should not get any errors. */

	ici__1.icierr = 1;
	ici__1.icirnum = 1;
	ici__1.icirlen = 40;
	ici__1.iciunit = numstr;
	ici__1.icifmt = fmtstr;
	iostat = s_wsfi(&ici__1);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_fio(&c__1, (char *)&(*x), (ftnlen)sizeof(doublereal));
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = e_wsfi();
L100001:

/*        This is fail safe check. Since we made the format string */
/*        ourselves and declared the output string with enough room we */
/*        should never hit it. But we do this check anyway, just in */
/*        case. */

	if (iostat != 0) {
	    chkin_("DPSTRE", (ftnlen)6);
	    setmsg_("Bug. FORTRAN WRITE failed; number = #; format = #; IOST"
		    "AT = #", (ftnlen)61);
	    errdp_("#", x, (ftnlen)1);
	    errch_("#", fmtstr, (ftnlen)1, (ftnlen)10);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(BUGWRITEFAILED)", (ftnlen)21);
	    chkout_("DPSTRE", (ftnlen)6);
	    return 0;
	}

/*        NOTE 1: should we also check for '*'s? */

/*        NOTE 2: should we check for 'E' in the string for cases of */
/*                output FORTRAN WRITE for numbers greater than 1D100? */
/*                (In this case GFORTRAN leaves E out and prints */
/*                pi*1D101 like this -3.14+101.) */


/*        Assign output string. */

	s_copy(string, numstr, string_len, (ftnlen)40);
    }
    return 0;
} /* dpstre_ */

