/* texpyr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      TEXPYR ( Time --- Expand year ) */
/* Subroutine */ int texpyr_0_(int n__, integer *year)
{
    /* Initialized data */

    static integer centry = 1900;
    static integer lbound = 1969;

/* $ Abstract */

/*    Expand an abbreviated year to a full year specification. */

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

/*     TIME */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     YEAR      I/O  The year of some epoch abbreviated/expanded. */

/* $ Detailed_Input */

/*     YEAR      is an "abbreviated year."  In other words the 98 of */
/*               1998,  05 of 2005, etc. */

/* $ Detailed_Output */

/*     YEAR      is the expansion of the abbreviated year according */
/*               to the lower bound established in the entry point */
/*               TSETYR.  By default if YEAR is 69 to 99, the output */
/*               is 1900 + the input value of YEAR.  If YEAR is 0 to 68 */
/*               the output value of YEAR is 2000 + the input value of */
/*               YEAR. */

/*               See the entry point TSETRY to modify this behavior. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) If on input YEAR is not in the inclusive interval from */
/*        0 to 99, YEAR is returned unchanged. */

/* $ Particulars */

/*     This routine allows all of the SPICE time subsystem to handle */
/*     uniformly the expansion of "abbreviated" years.  (i.e. the */
/*     remainder after dividing the actual year by 100). */

/*     By using this routine together with the routine TSETYR you */
/*     can recover the actual year to associate with an abbreviation. */

/*     The default behavior is as follows */

/*     YEAR input      YEAR Output */
/*     ----------      ----------- */
/*     00              2000 */
/*     01              2001 */
/*      .                . */
/*      .                . */
/*      .                . */
/*     66              2066 */
/*     67              2067 */
/*     68              2068 */
/*     69              1969 */
/*      .                . */
/*      .                . */
/*      .                . */
/*     99              1999 */


/* $ Examples */

/*     Suppose that you use TPARTV to parse time strings and that */
/*     you want to treat years components in the range from 0 to 99 */
/*     as being abbreviations for years in the range from */
/*     1980 to 2079 (provided that the years are not modified by */
/*     an ERA substring).  The code fragment below shows how you */
/*     could go about this. */

/*        Early in your application set up the lower bound for the */
/*        expansion of abbreviated years. */

/*        CALL TSETYR ( 1980 ) */


/*        After calling TPARTV or some other suitable parsing routine */
/*        get the integer value of the year. */

/*        YEAR = NINT( TVEC(1) ) */

/*        Having satisfied yourself that the year does not represent */
/*        a year in the range from 99 to 1 B.C. or in the range */
/*        from 1 to 99 A.D.  Expand Year to the appropriate value. */

/*        IF ( YEAR .LT. 100 ) THEN */

/*           CALL TEXPYR ( YEAR ) */

/*        END IF */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 18-NOV-1997 (WLT) */

/*        The default century was change from 1950-2049 to 1969-2068 */

/* -    SPICELIB Version 1.0.0, 8-APR-1996 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Expand an abbreviated year to a fully specified year. */

/* -& */
    switch(n__) {
	case 1: goto L_tsetyr;
	}

    if (*year >= 100 || *year < 0) {
	return 0;
    }
    *year += centry;
    if (*year < lbound) {
	*year += 100;
    }
    return 0;
/* $Procedure      TSETYR ( Time --- set year expansion boundaries ) */

L_tsetyr:
/* $ Abstract */

/*    Set the lower bound on the 100 year range */

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

/*     TIME */

/* $ Declarations */

/*     INTEGER               YEAR */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     YEAR       I   Lower bound on the 100 year interval of expansion */

/* $ Detailed_Input */

/*     YEAR       is the year associated with the lower bound on all */
/*                year expansions computed by TEXPYR.  For example */
/*                if YEAR is 1980, then the range of years that */
/*                can be abbreviated is from 1980 to 2079. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) If YEAR is less than 1 no action is taken */

/* $ Particulars */

/*     This entry point allows you to set the range to which years */
/*     abbreviated to the last two digits will be expanded. The input */
/*     supplied to this routine represents the lower bound of the */
/*     expansion interval.  The upper bound of the expansion interval */
/*     is YEAR + 99. */

/*     The default expansion interval is from 1969 to 2068. */

/* $ Examples */

/*     See the main routine TEXPYR. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 18-NOV-1997 (WLT) */

/*        The default century was change from 1950-2049 to 1969-2068 */

/* -    SPICELIB Version 1.0.0, 8-APR-1996 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Set the interval of expansion for abbreviated years */

/* -& */
    centry = *year / 100 * 100;
    lbound = *year;
    return 0;
} /* texpyr_ */

/* Subroutine */ int texpyr_(integer *year)
{
    return texpyr_0_(0, year);
    }

/* Subroutine */ int tsetyr_(integer *year)
{
    return texpyr_0_(1, year);
    }

