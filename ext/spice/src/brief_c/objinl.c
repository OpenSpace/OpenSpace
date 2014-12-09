/* objinl.f -- translated by f2c (version 19980913).
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


/*     Initialize an object list. */

/* Subroutine */ int objinl_(integer *objsiz, integer *nobj, integer *objlis)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer room, i__, j;
    extern /* Subroutine */ int ssizei_(integer *, integer *);
    integer mtasiz;

/* $ Abstract */

/*    Constants required by the family of "object" routines. */

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

/*     OBJECTS */

/* $ Parameters */

/*     LBCELL   is the lower bound for all cells used throughout */
/*              the SPICE library.. */

/*     NULL     is a constant used to indicate that a particular */
/*              object in a list is unused. */

/*     RMPOBJ   is the slot in the object list that tells how */
/*              many values are stored for each object.  I.E. */
/*              the number of values stored for each object */
/*              in an object list OBJLIS is OBJLIS(RMPOBJ). */

/*     NACTIV   is the slot in an object list that tells hows */
/*              many objects in the list are currently active. */
/*              In otherwords the number of active objects */
/*              in the object list OBJLIS is OBJLIS(NACTIV) */

/*     LSTID    is the slot in an object list that gives the */
/*              last object unique ID that was assigned. */
/*              In otherwords, the value of the last unique */
/*              object ID code in the object list OBJLIS */
/*              is OBJLIS(LSTID). */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Not Applicable */

/* $ Particulars */

/*     This include file contains the parameters used by the */
/*     family of object routines. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 23-FEB-1996 (WLT) */


/* -& */

/*     Each object has an extra space that specifies */
/*     the objects id-code and status.  The object */
/*     is "active" if the extra-space has a non-null */
/*     value. */


/*     Compute the number of slots required to store NOBJ objects */
/*     each of size OBJSIZ.  Recall that each object has an */
/*     extra "ID" slot. */

    room = (*objsiz + 1) * *nobj;
    mtasiz = *objsiz + 1;

/*     Set the size of the cell */

    ssizei_(&room, objlis);

/*     Fill in the extra slots of the cell used by the objects. */

    objlis[3] = mtasiz;
    objlis[2] = 0;
    objlis[1] = 0;

/*     Set each object to have a NULL ID code. */

    i__1 = *nobj;
    for (i__ = 1; i__ <= i__1; ++i__) {
	j = (i__ - 1) * mtasiz + 1;
	objlis[j + 5] = 0;
    }
    return 0;
} /* objinl_ */

