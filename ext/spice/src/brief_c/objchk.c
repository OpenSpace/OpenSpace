/* objchk.f -- translated by f2c (version 19980913).
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

/*     Check an object id for sanity. */

/* Subroutine */ int objchk_(char *name__, integer *obj, integer *objlis, 
	logical *ok, ftnlen name_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer size, room, i__, q;
    extern integer cardi_(integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sizei_(integer *);
    integer remain;
    extern /* Subroutine */ int rmaini_(integer *, integer *, integer *, 
	    integer *), sigerr_(char *, ftnlen), chkout_(char *, ftnlen);
    integer objsiz;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    integer mtasiz;


/*     This routine checks an object for sanity.  Moreover */
/*     as needed it refreshes the object in case the pointer */
/*     component no longer points to the object but the */
/*     object is still in the object list. */

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


/*     The null object is always present. */

    if (obj[0] == 0 && obj[1] == 0) {
	*ok = TRUE_;
	return 0;
    }

/*     Check for corrupted objects. */

    if (obj[0] < 0) {
	*ok = FALSE_;
	chkin_(name__, name_len);
	setmsg_("The object id supplied has an invalid pointer component. Th"
		"e pointer component must always be positive.  It had the val"
		"ue #.  This is probably the result of supplying an initializ"
		"ed object id. ", (ftnlen)193);
	sigerr_("SPICE(BADOBJECTID)", (ftnlen)18);
	chkout_(name__, name_len);
	return 0;
    }
    if (obj[1] < 0) {
	*ok = FALSE_;
	chkin_(name__, name_len);
	setmsg_("The object id supplied has an invalid identifier component."
		" The identifier component must always be positive.  It had t"
		"he value #.  This is probably the result of supplying an ini"
		"tialized object id. ", (ftnlen)199);
	sigerr_("SPICE(BADOBJECTID)", (ftnlen)18);
	chkout_(name__, name_len);
	return 0;
    }
    mtasiz = objlis[3];
    objsiz = mtasiz - 1;
    size = cardi_(objlis);
    room = sizei_(objlis);
    rmaini_(obj, &mtasiz, &q, &remain);
    if (remain != 1) {
	*ok = FALSE_;
	chkin_(name__, name_len);
	setmsg_("The pointer specified has an invalid value for the supplied"
		" object size.  The object size specified was #.  Given this "
		"object size the object pointer should be congruent to 1 MOD "
		"#.  The value of the object pointer was #. ", (ftnlen)222);
	errint_("#", &objsiz, (ftnlen)1);
	i__1 = objsiz + 1;
	errint_("#", &i__1, (ftnlen)1);
	errint_("#", obj, (ftnlen)1);
	sigerr_("SPICE(BADOBJECTID)", (ftnlen)18);
	chkout_(name__, name_len);
	return 0;
    }
    if (obj[0] > room && obj[1] != 0) {
	*ok = FALSE_;
	chkin_(name__, name_len);
	setmsg_("The pointer component of the object points outside of the o"
		"bject list.  The size of the object list is # and the value "
		"of the object pointer is #.  ", (ftnlen)148);
	errint_("#", &size, (ftnlen)1);
	errint_("#", obj, (ftnlen)1);
	sigerr_("SPICE(BADOBJECTID)", (ftnlen)18);
	chkout_(name__, name_len);
	return 0;
    }
    if (obj[0] > room && obj[1] == 0) {
	*ok = TRUE_;
	return 0;
    }
    if (objlis[obj[0] + 5] != obj[1]) {

/*        It is possible that the object list has been compressed. */
/*        If so the object id may be out of date.  See if we */
/*        can find this object elsewhere in the list. */

	i__1 = size;
	i__2 = mtasiz;
	for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
	    if (objlis[i__ + 5] == obj[1]) {

/*              Refressh the object pointer value. */

		obj[0] = i__;
		*ok = TRUE_;
		return 0;
	    }
	}

/*        If still, here there's a problem.  This cannot be */
/*        a legitimate object. */

	*ok = FALSE_;
	chkin_(name__, name_len);
	setmsg_("The pointer component and the identifier component of the o"
		"bject are not compatible. The pointer points to the identifi"
		"er value #.  The identifier of the object given is: #.  You "
		"may have a \"stale\" object.", (ftnlen)205);
	errint_("#", &objlis[obj[0] + 5], (ftnlen)1);
	errint_("#", &obj[1], (ftnlen)1);
	sigerr_("SPICE(BADOBJECTID)", (ftnlen)18);
	chkout_(name__, name_len);
	return 0;
    }

/*     If you get to this point, all obvious checks have passed. */
/*     This object is deemed to be a good one. */

    *ok = TRUE_;
    return 0;
} /* objchk_ */

