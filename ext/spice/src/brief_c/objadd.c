/* objadd.f -- translated by f2c (version 19980913).
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


/*     Add an object to an object list. */

/* Subroutine */ int objadd_(integer *object, integer *objlis, integer *obj)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer used, size, next, i__, j;
    extern integer cardi_(integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sizei_(integer *);
    integer count, allctd;
    extern /* Subroutine */ int scardi_(integer *, integer *), sigerr_(char *,
	     ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);
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

/*     SPICELIB Functions */


/*     Local Variables */

    size = sizei_(objlis);
    allctd = cardi_(objlis);
    count = objlis[2];
    mtasiz = objlis[3];
    used = count * mtasiz;

/*     Make sure there is room in the object list to hold */
/*     another object. */

    if (used >= size) {
	chkin_("OBJADD", (ftnlen)6);
	setmsg_("The object list already contains # objects. It is full. You"
		" will need to remove an object or increase the declared size"
		" of the object before another object can be added. ", (ftnlen)
		170);
	errint_("#", &count, (ftnlen)1);
	sigerr_("SPICE(OBJECTLISTFULL)", (ftnlen)21);
	chkout_("OBJADD", (ftnlen)6);
	return 0;
    }

/*     Ok. We've got room. Construct the idcode for the next */
/*     object.  And fill in the details in OBJLIS to indicate */
/*     this ID has been used. */

    next = objlis[1] + 1;
    objlis[1] = next;

/*     Do the easy step first. If the objects are already */
/*     packed together, we add this object to the end */
/*     of the list. */

    if (allctd == used) {
	i__ = used + 1;
	obj[0] = i__;
	obj[1] = next;
	objlis[i__ + 5] = next;
	i__1 = mtasiz - 1;
	for (j = 1; j <= i__1; ++j) {
	    ++i__;
	    objlis[i__ + 5] = object[j - 1];
	}

/*        Adjust the cardinality of the object list. */

	i__1 = used + mtasiz;
	scardi_(&i__1, objlis);
	++objlis[2];
	return 0;
    } else {

/*        There's room available in the object list.  Find */
/*        a NULL position and use that space for this object. */

	i__ = 1;
	while(i__ < allctd) {
	    if (objlis[i__ + 5] == 0) {
		objlis[i__ + 5] = next;
		obj[0] = i__;
		obj[1] = next;
		i__1 = mtasiz - 1;
		for (j = 1; j <= i__1; ++j) {
		    ++i__;
		    objlis[i__ + 5] = object[j - 1];
		}
		++objlis[2];
		return 0;
	    }
	    i__ += mtasiz;
	}
    }

/*     You are never supposed to be able to reach this */
/*     point in the code.  If you do, there's a bug somewhere. */

    chkin_("OBJADD", (ftnlen)6);
    setmsg_("A serious error has occurred.  The object list is supposed to h"
	    "ave room available in it, but no free areas were located.  The m"
	    "ost likely cause is that the object list has been inadvertantly "
	    "corrupted by some portion of your software.  The other possibili"
	    "ty is that there is a bug in the SPICE code. ", (ftnlen)300);
    sigerr_("SPICE(BUG)", (ftnlen)10);
    chkout_("OBJADD", (ftnlen)6);
    return 0;
} /* objadd_ */

