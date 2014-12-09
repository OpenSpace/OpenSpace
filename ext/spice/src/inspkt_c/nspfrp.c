/* nspfrp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

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


/*     This is the routine needed by NSPFLG to create the reports */
/*     that are produced for flagged preserved or flagged format. */
/*     It returns either a column alias, a separator (right now */
/*     a colon) or the current value of the column.  To get back */
/*     the column alias send the opposite of the column ID to */
/*     NSPFRP, to get the colon, send the ID of zero, to get the */
/*     current print value for the column send the ID for the */
/*     column. */

/* Subroutine */ int nspfrp_0_(int n__, integer *id, integer *compnt, char *
	string, integer *width, ftnlen string_len)
{
    /* Initialized data */

    static integer preset = 8;

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer myid;
    extern /* Subroutine */ int fetcha_(integer *, integer *, char *, integer 
	    *, ftnlen), clpval_(integer *, integer *, char *, integer *, 
	    ftnlen);

    switch(n__) {
	case 1: goto L_nspfrw;
	}

    myid = *id;
    if (myid < 0) {
	if (*compnt == 1) {
	    i__1 = -myid;
	    fetcha_(&i__1, compnt, string, width, string_len);
	    *width = preset;
	} else {
	    s_copy(string, " ", string_len, (ftnlen)1);
	    *width = preset;
	}
    } else if (*id == 0) {
	if (*compnt == 1) {
	    *width = 2;
	    s_copy(string, ":", string_len, (ftnlen)1);
	} else {
	    *width = 2;
	    s_copy(string, " ", string_len, (ftnlen)1);
	}
    } else {
	clpval_(id, compnt, string, width, string_len);
    }
    return 0;

/*     The entry point here allows NSPFLG to set the width */
/*     for all column aliases returned by NSPFRP. */


L_nspfrw:
    preset = *width;
    return 0;
} /* nspfrp_ */

/* Subroutine */ int nspfrp_(integer *id, integer *compnt, char *string, 
	integer *width, ftnlen string_len)
{
    return nspfrp_0_(0, id, compnt, string, width, string_len);
    }

/* Subroutine */ int nspfrw_(integer *width)
{
    return nspfrp_0_(1, (integer *)0, (integer *)0, (char *)0, width, (ftnint)
	    0);
    }

