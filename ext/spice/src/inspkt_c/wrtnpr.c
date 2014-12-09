/* wrtnpr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int wrtnpr_(integer *idlist, integer *n, char *del, char *
	quote, ftnlen del_len, ftnlen quote_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char line[16000];
    integer myid;
    char temp[16000];
    logical null;
    integer i__, j, k;
    extern /* Subroutine */ int clgai_(integer *, char *, integer *, integer *
	    , ftnlen);
    integer limit, width, dummy;
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int clq2id_(integer *, integer *), clncmp_(
	    integer *, integer *), clpval_(integer *, integer *, char *, 
	    integer *, ftnlen);
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int setchr_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    logical doquot;
    extern /* Subroutine */ int nspwln_(char *, ftnlen);
    integer thstyp, num;
    char row[16000];
    integer put;


/* $ Abstract */

/*     Write out the current row of the current selection set preserving */
/*     components in separate rows. */

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

/*     INSPEKT */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     IDLIST     I   List of column ids for use in this report. */
/*     N          I   Number of columns to be output. */
/*     DEL        I   Character used to delimit colums. */
/*     QUOTE      I   Character used to quote strings. */

/* $ Detailed_Input */

/*     IDLIST     is the list of column idcodes that are used to */
/*                for the various subrows of this row of the matching */
/*                query. */

/*     N          The number of columns in IDLIST */

/*     DEL        Delimiter to use between columns.  You can use a space */
/*                but it kind of defeats the purpose of this routine. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This is simply a formatter.  Each component appears in an output */
/*     row by itself. */

/* $ Examples */

/*     Nope. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 27-MAR--2003 (WLT) */


/* -& */

/*     SPICELIB Functions */

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


/*     This file contains the parameter LNGSIZ which specifies the */
/*     longest character string that can be declared in all of the */
/*     FORTRAN environments supported by the SPICE system. */

    s_copy(row, " ", (ftnlen)16000, (ftnlen)1);
    put = 1;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	clq2id_(&idlist[i__], &myid);
	clncmp_(&idlist[i__], &num);
	clgai_(&myid, "TYPE", &dummy, &thstyp, (ftnlen)4);
	doquot = thstyp != 3 && thstyp != 2;
	s_copy(temp, " ", (ftnlen)16000, (ftnlen)1);

/*        Get the print value for each component of the current column of */
/*        this row.  Append all of the print values together */

	if (doquot) {
	    setchr_(quote, &put, row, (ftnlen)1, (ftnlen)16000);
	}
	null = FALSE_;
	i__2 = num;
	for (j = 1; j <= i__2; ++j) {
	    s_copy(line, " ", (ftnlen)16000, (ftnlen)1);
	    clpval_(&idlist[i__], &j, line, &width, (ftnlen)16000);

/*           If we get a null or absent value, we have to quote this */
/*           item.  If we haven't already done so, we still have time. */

	    if (s_cmp(line, "<null>", (ftnlen)16000, (ftnlen)6) == 0 || s_cmp(
		    line, "<absent>", (ftnlen)16000, (ftnlen)8) == 0) {
		if (! doquot) {
		    doquot = TRUE_;
		    setchr_(quote, &put, row, (ftnlen)1, (ftnlen)16000);
		}
		null = TRUE_;
	    }
	    if (j == 1) {
		limit = rtrim_(line, (ftnlen)16000);
	    } else {
		limit = lastnb_(line, (ftnlen)16000);
	    }
	    i__3 = limit;
	    for (k = 1; k <= i__3; ++k) {
		setchr_(line + (k - 1), &put, row, (ftnlen)1, (ftnlen)16000);
		if (doquot && *(unsigned char *)&line[k - 1] == *(unsigned 
			char *)quote) {
		    setchr_(quote, &put, row, (ftnlen)1, (ftnlen)16000);
		}
	    }
	    if (j != num && limit != 0 && ! null) {
		setchr_(" ", &put, row, (ftnlen)1, (ftnlen)16000);
	    }
	}

/*        Move the column value constructed above into the row we've */
/*        been constructing --- one character at a time, doubling the */
/*        quote character if necessary. */

	if (doquot) {
	    setchr_(quote, &put, row, (ftnlen)1, (ftnlen)16000);
	}

/*        If this is not the last column, we need to place a delimiter */
/*        onto the end of the row we've constructed so far. */

	if (i__ != *n) {
	    setchr_(del, &put, row, (ftnlen)1, (ftnlen)16000);
	}
    }
    nspwln_(row, (ftnlen)16000);
    return 0;
} /* wrtnpr_ */

