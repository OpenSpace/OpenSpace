/* filtem.f -- translated by f2c (version 19980913).
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

/* Subroutine */ int filtem_(char *kertyp, logical *obnam, integer *objlis, 
	doublereal *from, doublereal *to, doublereal *filwin, doublereal *
	tmpwin, char *winsym, integer *winptr, doublereal *winval, ftnlen 
	kertyp_len, ftnlen winsym_len)
{
    logical keep;
    integer objn[2], sobj, n;
    logical found;
    extern /* Subroutine */ int scardd_(integer *, doublereal *), maknam_(
	    integer *, integer *, logical *, char *, char *, ftnlen, ftnlen);
    integer object[3];
    char objnam[32];
    extern /* Subroutine */ int objget_(integer *, integer *, integer *), 
	    objcmp_(integer *), objrem_(integer *, integer *);
    extern logical wnincd_(doublereal *, doublereal *, doublereal *);
    extern /* Subroutine */ int objnth_(integer *, integer *, integer *, 
	    logical *), sygetd_(char *, char *, integer *, doublereal *, 
	    integer *, doublereal *, logical *, ftnlen, ftnlen);
    extern integer objsiz_(integer *);
    extern /* Subroutine */ int objnxt_(integer *, integer *, integer *, 
	    logical *);
    logical fnd;
    integer obj[2];


/*     Spicelib Functions */


/*     Local Variables. */

    tmpwin[0] = 0.;
    sobj = objsiz_(objlis);
    objnth_(objlis, &c__1, obj, &found);
    while(found) {

/*        Look up the window associated with the current */
/*        object.  If it doesn't contain the FROM and TO */
/*        interval, remove the current object from the */
/*        list of objects to disply. */

	objget_(obj, objlis, object);
	maknam_(object, &sobj, obnam, kertyp, objnam, kertyp_len, (ftnlen)32);
	sygetd_(objnam, winsym, winptr, winval, &n, &filwin[6], &fnd, (ftnlen)
		32, winsym_len);
	scardd_(&n, filwin);
	keep = wnincd_(from, to, filwin);
	objnxt_(obj, objlis, objn, &found);
	if (! keep) {

/*           Remove the current object from this */
/*           list of objects to give summaries for. */

	    objrem_(obj, objlis);
	}
	obj[0] = objn[0];
	obj[1] = objn[1];
    }

/*     Compress the object list. */

    objcmp_(objlis);
    return 0;
} /* filtem_ */

