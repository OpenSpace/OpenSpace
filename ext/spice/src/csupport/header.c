/* header.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      HEADER (HEADER for a report) */
/* Subroutine */ int header_0_(int n__, integer *n, integer *comp, char *
	value, integer *wdth, ftnlen value_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer i__, j;
    extern integer rtrim_(char *, ftnlen);
    static char buffer[1600*40*10];

/* $ Abstract */

/*    This is an umbrella routine for setting up headers */
/*    on tabular reports. */

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

/*     REPORTS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     N         I/O  Column number */
/*     COMP      I/O  Component number */
/*     VALUE     I/O  String Value. */
/*     WDTH       O   Non-blank width of VALUE */

/* $ Detailed_Input */

/*     N         specifies which column is being defined. */
/*               Legitimate values are 1 to 40. */

/*     COMP      specifies which column component is being */
/*               specified.  Legitimate values are 1 to 10. */

/*     VALUE     specifies the column component value. It should */
/*               be 800 or fewer characters in length. */

/* $ Detailed_Output */

/*     N         specifies which column to fetch information from. */
/*               Legitimate values are 1 to 40. */

/*     COMP      specifies which column component is to obtain */
/*               information for. Legitimate values are 1 to 10. */

/*     VALUE     Value of requested column component. */

/*     WDTH      is the non-blank width of VALUE. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     If N or NCOMP is out of range: */

/*        SCOLMN simply returns.  No data is buffered. No warning */
/*        or error is issued. */

/*        GCOLMN returns a blank.  WDTH will be set to 1. */


/* $ Particulars */

/*     This is a routine designed to work with the routine */
/*     TABRPT when creating tabular outputs.  It is primarily */
/*     intended for creating the header portion of a report. */

/*     For example, the area marked with the arrow below. */

/*        Name       Phone       Address    <<============= */
/*        ================================= */
/*        Bill       555-1212    Pasadena, CA */
/*        Bob        555-2121    Flint, Michigan */
/*        Ian        555-1234    San Jose, CA */

/*     You could use this to fill out the contents of the report */
/*     if you don't have something already  that fetches */
/*     string values. */

/* $ Examples */

/*     Suppose you wanted to create the header above and have */
/*     it appear on your reports.  Here's all you need to do. */

/*        CALL SCOLMN ( 1, 1, 'Name'    ) */
/*        CALL SCOLMN ( 2, 1, 'Phone'   ) */
/*        CALL SCOLMN ( 3, 1, 'Address' ) */

/*     Then simply pass the entry point GCOLMN to TABRPT to construct */
/*     the header portion of the report. */

/*        CALL TABRPT ( 3, item,   size, */
/*    .                    width,  justr, */
/*    .                    presrv, spcial, */
/*    .                    lmarge, space, */
/*    .                    GCOLMN   ) */

/*     filling out the various items as is appropriate for the */
/*     table you plan to create. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*       W.L. Taber      (JPL) */

/* $ Literature_References */

/*       None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 3-AUG-1995 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Setting and getting values for reports */

/* -& */

/*     SPICELIB Functions */


/*     Buffer declarations */

    switch(n__) {
	case 1: goto L_scolmn;
	case 2: goto L_gcolmn;
	case 3: goto L_ccolmn;
	}

    return 0;

/*     Set a column component value. */


L_scolmn:
    if (first) {
	for (i__ = 1; i__ <= 40; ++i__) {
	    for (j = 1; j <= 10; ++j) {
		s_copy(buffer + ((i__1 = i__ + j * 40 - 41) < 400 && 0 <= 
			i__1 ? i__1 : s_rnge("buffer", i__1, "header_", (
			ftnlen)206)) * 1600, " ", (ftnlen)1600, (ftnlen)1);
	    }
	}
	first = FALSE_;
    }
    if (*n >= 1 && *n <= 40 && *comp >= 1 && *comp <= 10) {
	s_copy(buffer + ((i__1 = *n + *comp * 40 - 41) < 400 && 0 <= i__1 ? 
		i__1 : s_rnge("buffer", i__1, "header_", (ftnlen)219)) * 1600,
		 value, (ftnlen)1600, value_len);
    }
    return 0;

/*     Get a column component value. */


L_gcolmn:
    if (first) {
	for (i__ = 1; i__ <= 40; ++i__) {
	    for (j = 1; j <= 10; ++j) {
		s_copy(buffer + ((i__1 = i__ + j * 40 - 41) < 400 && 0 <= 
			i__1 ? i__1 : s_rnge("buffer", i__1, "header_", (
			ftnlen)234)) * 1600, " ", (ftnlen)1600, (ftnlen)1);
	    }
	}
	first = FALSE_;
    }
    if (*n >= 1 && *n <= 40 && *comp >= 1 && *comp <= 10) {
	s_copy(value, buffer + ((i__1 = *n + *comp * 40 - 41) < 400 && 0 <= 
		i__1 ? i__1 : s_rnge("buffer", i__1, "header_", (ftnlen)247)) 
		* 1600, value_len, (ftnlen)1600);
	*wdth = rtrim_(value, value_len);
    } else {
	s_copy(value, " ", value_len, (ftnlen)1);
	*wdth = 1;
    }
    return 0;

L_ccolmn:
    for (i__ = 1; i__ <= 40; ++i__) {
	for (j = 1; j <= 10; ++j) {
	    s_copy(buffer + ((i__1 = i__ + j * 40 - 41) < 400 && 0 <= i__1 ? 
		    i__1 : s_rnge("buffer", i__1, "header_", (ftnlen)263)) * 
		    1600, " ", (ftnlen)1600, (ftnlen)1);
	}
    }
    return 0;
} /* header_ */

/* Subroutine */ int header_(integer *n, integer *comp, char *value, integer *
	wdth, ftnlen value_len)
{
    return header_0_(0, n, comp, value, wdth, value_len);
    }

/* Subroutine */ int scolmn_(integer *n, integer *comp, char *value, ftnlen 
	value_len)
{
    return header_0_(1, n, comp, value, (integer *)0, value_len);
    }

/* Subroutine */ int gcolmn_(integer *n, integer *comp, char *value, integer *
	wdth, ftnlen value_len)
{
    return header_0_(2, n, comp, value, wdth, value_len);
    }

/* Subroutine */ int ccolmn_(void)
{
    return header_0_(3, (integer *)0, (integer *)0, (char *)0, (integer *)0, (
	    ftnint)0);
    }

