/* getelm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure GETELM ( Get the components from two-line elements) */
/* Subroutine */ int getelm_(integer *frstyr, char *lines, doublereal *epoch, 
	doublereal *elems, ftnlen lines_len)
{
    extern /* Subroutine */ int zzgetelm_(integer *, char *, doublereal *, 
	    doublereal *, logical *, char *, ftnlen, ftnlen), chkin_(char *, 
	    ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    char error[256];
    logical ok;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*    Given a the "lines" of a two-line element set, parse the */
/*    lines and return the elements in units suitable for use */
/*    in SPICE software. */

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

/*     PARSING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FRSTYR     I   year of earliest representable two-line elements */
/*     LINES      I   a pair of "lines" containing two-line elements */
/*     EPOCH      O   The epoch of the elements in seconds past J2000 */
/*     ELEMS      O   The elements converted to SPICE units. */

/* $ Detailed_Input */

/*     FRSTYR    is the first year possible for two line elements. */
/*               Since two line elements allow only two digits for */
/*               the year, some conventions must be followed concerning */
/*               which century the two digits refer to .  FRSTYR */
/*               is the year of the earliest representable elements. */
/*               The two-digit year is mapped to the year in */
/*               the interval from FRSTYR to FRSTYR + 99 that */
/*               has the same last two digits as the two digit */
/*               year in the element set.  For example if FRSTYR */
/*               is set to 1960  then the two digit years are mapped */
/*               as shown in the table below: */

/*               Two-line         Maps to */
/*               element year */
/*                  00            2000 */
/*                  01            2001 */
/*                  02            2002 */
/*                   .              . */
/*                   .              . */
/*                   .              . */
/*                  58            2058 */
/*                  59            2059 */
/*                 -------------------- */
/*                  60            1960 */
/*                  61            1961 */
/*                  62            1962 */
/*                   .              . */
/*                   .              . */
/*                   .              . */
/*                  99            1999 */

/*                Note that if Space Command should decide to represent */
/*                years in 21st century as 100 + the last two digits */
/*                of the year (for example: 2015 is represented as 115) */
/*                instead of simply dropping the first two digits of */
/*                the year, this routine will correctly map the year */
/*                as long as you set FRSTYR to some value between 1900 */
/*                and 1999. */

/*     LINES      is a pair of lines of text that comprise a Space */
/*                command ``two-line element'' set.  These text lines */
/*                should be the same as they are presented in the */
/*                two-line element files available from Space Command */
/*                (formerly NORAD). Below is an example of a two-line */
/*                set for TOPEX. */

/*  TOPEX */
/*  1 22076U 92052A   97173.53461370 -.00000038  00000-0  10000-3 0   594 */
/*  2 22076  66.0378 163.4372 0008359 278.7732  81.2337 12.80930736227550 */


/* $ Detailed_Output */

/*     EPOCH      is the epoch of the two line elements supplied via */
/*                the input array LINES.  Epoch is returned in TDB */
/*                seconds past J2000. */

/*     ELEMS      is an array containing the elements from the two line */
/*                set supplied via the array LINES.  The elements are */
/*                in units suitable for use by the SPICE routine */
/*                EV2LIN. */

/*                Also note that the elements XNDD6O and BSTAR */
/*                incorporate the exponential factor present in the */
/*                input two line elements in LINES.  (See particulars */
/*                below. */

/*                    ELEMS (  1 ) = XNDT2O in radians/minute**2 */
/*                    ELEMS (  2 ) = XNDD6O in radians/minute**3 */
/*                    ELEMS (  3 ) = BSTAR */
/*                    ELEMS (  4 ) = XINCL  in radians */
/*                    ELEMS (  5 ) = XNODEO in radians */
/*                    ELEMS (  6 ) = EO */
/*                    ELEMS (  7 ) = OMEGAO in radians */
/*                    ELEMS (  8 ) = XMO    in radians */
/*                    ELEMS (  9 ) = XNO    in radians/minute */
/*                    ELEMS ( 10 ) = EPOCH of the elements in seconds */
/*                                   past ephemeris epoch J2000. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     You must have loaded a SPICE leapseconds kernel into the */
/*     kernel pool prior to caling this routine. */

/* $ Exceptions */

/*     1) If an error occurs while trying to parse the two-line element */
/*        set, the error 'SPICE(BADTLE)' signals. */

/* $ Particulars */

/*     This routine passes a Space Command Two-line element set */
/*     to the parsing routine ZZGETELM.  Input elements have the */
/*     form: */

/*  1 22076U 92052A   97173.53461370 -.00000038  00000-0  10000-3 0   594 */
/*  2 22076  66.0378 163.4372 0008359 278.7732  81.2337 12.80930736227550 */
/*  ^ */
/*  123456789012345678901234567890123456789012345678901234567890123456789 */
/*           1         2         3         4         5         6 */

/* $ Examples */

/*     Suppose you have a set of two-line elements and an array */
/*     containing the related geophysical constants necessary */
/*     to evaluate a state.  The example below shows how you */
/*     can use this routine together with the routine EV2LIN to */
/*     propagate a state to an epoch of interest. */


/*        The parameters below will make it easier to make assignments */
/*        to the array GEOPHS required by EV2LIN. */

/*        J2  --- location of J2 */
/*        J3  --- location of J3 */
/*        J4  --- location if J4 */
/*        KE  --- location of KE = sqrt(GM) in eart-radii**1.5/MIN */
/*        QO  --- location of upper bound of atmospheric model in KM */
/*        SO  --- location of lower bound of atmospheric model in KM */
/*        ER  --- location of earth equatorial radius in KM. */
/*        AE  --- location of distance units/earth radius */

/*        PARAMETER           ( J2 = 1 ) */
/*        PARAMETER           ( J3 = 2 ) */
/*        PARAMETER           ( J4 = 3 ) */
/*        PARAMETER           ( KE = 4 ) */
/*        PARAMETER           ( QO = 5 ) */
/*        PARAMETER           ( SO = 6 ) */
/*        PARAMETER           ( ER = 7 ) */
/*        PARAMETER           ( AE = 8 ) */


/*        We set the lower bound for the years to be the beginning */
/*        of the space age. */

/*        FRSTYR = 1957 */

/*        Read in the next two lines from the text file that contains */
/*        the two-line elements.  We assume that file has been opened */
/*        properly and that we have set the ``file pointer'' to the */
/*        correct location for reading the next set of elements. */

/*        READ  (UNIT,FMT='(A)' ) LINE(1) */
/*        READ  (UNIT,FMT='(A)' ) LINE(2) */

/*        CALL GETELM ( FRSTYR, LINE, EPOCH, ELEMS ) */

/*        Set up the geophysical quantities.  At last check these */
/*        were the values used by Space Command. */

/*        GEOPHS( J2 ) =    1.082616D-3 */
/*        GEOPHS( J3 ) =   -2.53881D-6 */
/*        GEOPHS( J4 ) =   -1.65597D-6 */
/*        GEOPHS( KE ) =    7.43669161D-2 */
/*        GEOPHS( QO ) =  120.0D0 */
/*        GEOPHS( SO ) =   78.0D0 */
/*        GEOPHS( ER ) = 6378.135D0 */
/*        GEOPHS( AE ) =    1.0D0 */

/*        Now propagate the state using EV2LIN to the epoch of */
/*        interest. */

/*        CALL EV2LIN ( ET, GEOPHS, ELEMS, STATE ) */


/* $ Restrictions */

/*    Please refer to the header of ZZGETELM. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 3.0.0, 30-MAR-2004 (EDW) */

/*        Routine now passes inputs to ZZGETELM then reponds to */
/*        any error condition. */

/* -    SPICELIB Version 2.0.0, 03-MAR-2000 (WLT) */

/*        The routine was modified to check that all of the terms */
/*        in the two-line element set are parsed correctly. */

/* -    SPICELIB Version 1.0.0, 26-JUN-1997 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Parse two-line elements */

/* -& */

/*     Spicelib functions */


/*     Local. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("GETELM", (ftnlen)6);

/*     Pass the input to the parse routine... */

    zzgetelm_(frstyr, lines, epoch, elems, &ok, error, lines_len, (ftnlen)256)
	    ;

/*     ...check for an error parsing the TLE pair. Signal an */
/*     error if OK equals .FALSE. */

    if (! ok) {
	setmsg_("Error in TLE set. #", (ftnlen)19);
	errch_("#", error, (ftnlen)1, (ftnlen)256);
	sigerr_("SPICE(BADTLE)", (ftnlen)13);
	chkout_("GETELM", (ftnlen)6);
	return 0;
    }
    chkout_("GETELM", (ftnlen)6);
    return 0;
} /* getelm_ */

