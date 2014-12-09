/* curtim.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__11 = 11;

/* $Procedure CURTIM (Current Time) */
/* Subroutine */ int curtim_(char *time, ftnlen time_len)
{
    /* Initialized data */

    static char month[3*12] = "JAN" "FEB" "MAR" "APR" "MAY" "JUN" "JUL" "AUG" 
	    "SEP" "OCT" "NOV" "DEC";

    /* System generated locals */
    address a__1[11];
    integer i__1, i__2, i__3[11];

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    static integer ivec[6];
    static char year[4];
    static doublereal tvec[6];
    static char hour[2];
    extern /* Subroutine */ int zzcputim_(doublereal *);
    static integer i__;
    extern /* Subroutine */ int rjust_(char *, char *, ftnlen, ftnlen), 
	    replch_(char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, 
	    ftnlen), intstr_(integer *, char *, ftnlen);
    static char sec[2], day[2], min__[2];

/* $ Abstract */

/*     Return a string giving the current date and time */

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

/*       TIME */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      TIME       O   A string containing the current date and time. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     TIME        is a string that contains the current date and */
/*                 time in the format YEAR-MON-DY HR:MN:SC */

/* $ Parameters */

/*      None. */

/* $ Files */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This is a utility for creating time-stamps for inserting */
/*     into data products.  It is not intended to provide accurate */
/*     measurment of local time since local time is not necessarily */
/*     in step with the processor clock.  If you need the numeric */
/*     components, see the routine ZZCPUTIM. */


/* $ Examples */

/*     Suppose that you wish to insert into a data product the */
/*     system time at the time of creation of the product.  You */
/*     could call this routine to get the current time (in a string) */
/*     and then simply write that string into the data product. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -    SUPPORT Version 1.1.0, 11-SEP-2007 (EDW) */

/*        Replaced CPUTIM call with ZZCPUTIM call. Edited previous */
/*        Version entries to clarify CURTIM pedigree. */

/* -    SUPPORT Version 1.0.1, 03-MAY-1994 (WLT) */

/*        This is the configured version of the Command Loop */
/*        software as of May 4, 1994 */

/* -    SUPPORT Version 1.0.0, 20-APR-1994 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Get a string giving the current system time */

/* -& */
    zzcputim_(tvec);
    for (i__ = 1; i__ <= 6; ++i__) {
	ivec[(i__1 = i__ - 1) < 6 && 0 <= i__1 ? i__1 : s_rnge("ivec", i__1, 
		"curtim_", (ftnlen)146)] = (integer) tvec[(i__2 = i__ - 1) < 
		6 && 0 <= i__2 ? i__2 : s_rnge("tvec", i__2, "curtim_", (
		ftnlen)146)];
    }
    intstr_(ivec, year, (ftnlen)4);
    intstr_(&ivec[2], day, (ftnlen)2);
    intstr_(&ivec[3], hour, (ftnlen)2);
    intstr_(&ivec[4], min__, (ftnlen)2);
    intstr_(&ivec[5], sec, (ftnlen)2);
    rjust_(day, day, (ftnlen)2, (ftnlen)2);
    rjust_(hour, hour, (ftnlen)2, (ftnlen)2);
    rjust_(min__, min__, (ftnlen)2, (ftnlen)2);
    rjust_(sec, sec, (ftnlen)2, (ftnlen)2);
    replch_(day, " ", "0", day, (ftnlen)2, (ftnlen)1, (ftnlen)1, (ftnlen)2);
    replch_(hour, " ", "0", hour, (ftnlen)2, (ftnlen)1, (ftnlen)1, (ftnlen)2);
    replch_(min__, " ", "0", min__, (ftnlen)2, (ftnlen)1, (ftnlen)1, (ftnlen)
	    2);
    replch_(sec, " ", "0", sec, (ftnlen)2, (ftnlen)1, (ftnlen)1, (ftnlen)2);
/* Writing concatenation */
    i__3[0] = 4, a__1[0] = year;
    i__3[1] = 1, a__1[1] = "-";
    i__3[2] = 3, a__1[2] = month + ((i__1 = ivec[1] - 1) < 12 && 0 <= i__1 ? 
	    i__1 : s_rnge("month", i__1, "curtim_", (ftnlen)165)) * 3;
    i__3[3] = 1, a__1[3] = "-";
    i__3[4] = 2, a__1[4] = day;
    i__3[5] = 1, a__1[5] = " ";
    i__3[6] = 2, a__1[6] = hour;
    i__3[7] = 1, a__1[7] = ":";
    i__3[8] = 2, a__1[8] = min__;
    i__3[9] = 1, a__1[9] = ":";
    i__3[10] = 2, a__1[10] = sec;
    s_cat(time, a__1, i__3, &c__11, time_len);
    return 0;
} /* curtim_ */

