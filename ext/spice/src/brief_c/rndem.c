/* rndem.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

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

/*     Round the windows for objects inward. */

/* Subroutine */ int rndem_(char *kertyp, logical *obnam, integer *objlis, 
	integer *objsiz, doublereal *intval, doublereal *filwin, char *winsym,
	 integer *winptr, doublereal *winval, ftnlen kertyp_len, ftnlen 
	winsym_len)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Local variables */
    logical keep;
    integer objn[2], i__;
    extern integer cardd_(doublereal *);
    integer n;
    doublereal q, r__;
    logical found;
    extern /* Subroutine */ int scardd_(integer *, doublereal *), maknam_(
	    integer *, integer *, logical *, char *, char *, ftnlen, ftnlen);
    integer object[3];
    char objnam[32];
    extern /* Subroutine */ int objget_(integer *, integer *, integer *), 
	    rmaind_(doublereal *, doublereal *, doublereal *, doublereal *), 
	    objcmp_(integer *), objrem_(integer *, integer *), wnfild_(
	    doublereal *, doublereal *), objnth_(integer *, integer *, 
	    integer *, logical *);
    doublereal offset;
    extern /* Subroutine */ int sygetd_(char *, char *, integer *, doublereal 
	    *, integer *, doublereal *, logical *, ftnlen, ftnlen), objnxt_(
	    integer *, integer *, integer *, logical *), syputd_(char *, 
	    doublereal *, integer *, char *, integer *, doublereal *, ftnlen, 
	    ftnlen);
    logical fnd;
    integer obj[2];


/*     Spicelib Functions */


/*     Local Variables. */

    objnth_(objlis, &c__1, obj, &found);
    while(found) {

/*        Look up the window associated with the current */
/*        object.  Round the window using the rounding */
/*        specified. */

	objget_(obj, objlis, object);
	maknam_(object, objsiz, obnam, kertyp, objnam, kertyp_len, (ftnlen)32)
		;
	sygetd_(objnam, winsym, winptr, winval, &n, &filwin[6], &fnd, (ftnlen)
		32, winsym_len);
	scardd_(&n, filwin);

/*        For each interval round it inward to the specified */
/*        level. */

	if (*intval == 86400.) {
	    offset = 43200.;
	} else {
	    offset = 0.;
	}
	i__1 = n;
	for (i__ = 1; i__ <= i__1; i__ += 2) {
	    filwin[i__ + 5] += offset;
	    filwin[i__ + 6] += offset;
	    rmaind_(&filwin[i__ + 5], intval, &q, &r__);
	    if (r__ != 0.) {
/* Computing MIN */
		d__1 = *intval * (q + 1), d__2 = filwin[i__ + 6];
		filwin[i__ + 5] = min(d__1,d__2);
	    }
	    rmaind_(&filwin[i__ + 6], intval, &q, &r__);
/* Computing MAX */
	    d__1 = *intval * q, d__2 = filwin[i__ + 5];
	    filwin[i__ + 6] = max(d__1,d__2);
	    filwin[i__ + 5] -= offset;
	    filwin[i__ + 6] -= offset;
	}

/*        Filter out any inteval that is less than the */
/*        specified rounding level. */

	d__1 = *intval - .5;
	wnfild_(&d__1, filwin);

/*        Put the window back into the table. */

	n = cardd_(filwin);
	if (n > 0) {
	    keep = TRUE_;
	    syputd_(objnam, &filwin[6], &n, winsym, winptr, winval, (ftnlen)
		    32, winsym_len);
	} else {
	    keep = FALSE_;
	}

/*        Get the next object. */

	objnxt_(obj, objlis, objn, &found);
	if (! keep) {

/*           If we rounded away all the coverage, remove */
/*           this object. */

	    objrem_(obj, objlis);
	}

/*        Move the next object into the current object. */

	obj[0] = objn[0];
	obj[1] = objn[1];
    }

/*     Now Compresss the object list. */

    objcmp_(objlis);
    return 0;
} /* rndem_ */

