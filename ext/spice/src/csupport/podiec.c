/* podiec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      PODIEC ( Pod, insert elements, character ) */
/* Subroutine */ int podiec_(char *elems, integer *n, integer *loc, char *pod,
	 ftnlen elems_len, ftnlen pod_len)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern integer cardc_(char *, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sizec_(char *, ftnlen);
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen), inslac_(
	    char *, integer *, integer *, char *, integer *, ftnlen, ftnlen), 
	    podonc_(char *, integer *, integer *, ftnlen);
    integer offset, number;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    integer end;

/* $ Abstract */

/*     Insert elements at a specified location within the active group */
/*     of a pod. */

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

/*     PODS */

/* $ Keywords */

/*     ARRAY */
/*     CELLS */
/*     PODS */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ELEMS      I   New elements. */
/*     N          I   Number of new elements. */
/*     LOC        I   Location at which elements are to be inserted. */
/*     POD       I,O  Pod. */

/* $ Detailed_Input */

/*     ELEMS      contains elements to be inserted into the active */
/*                group of POD. */

/*     N          is the number of elements in ELEMS. */

/*     LOC        is the location (within the active group of the pod) */
/*                at which the new elements are to be inserted. The new */
/*                elements are inserted in front of the element currently */
/*                at this location. */

/*     POD        on input, is a pod. */

/* $ Detailed_Output */

/*     POD        on output, is the same pod, the active group of */
/*                which contains the new elements. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*      1) If N is not positive, the pod is not changed. */

/*     2) If there is insufficient room in the pod to insert all */
/*        of the new elements, the pod is not changed, and the error */
/*        SPICE(TOOMANYPEAS) is signalled. */

/*     3) If the location specified for location is not in the range */
/*        [1,NC+1], where NC is the number of elements in the active */
/*        group of the pod, the pod is not changed, and the error */
/*        SPICE(BADPODLOCATION) is signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine allows you to insert elements into the active */
/*     group of a pod without having to worry about checking for */
/*     overflow beforehand, or updating the cardinality afterwards. */

/* $ Examples */

/*     Elements can be inserted into the active group of a pod */
/*     by hand, */

/*        CALL PODONC ( POD, OFFSET, NUMBER ) */
/*        END = OFFSET + NUMBER */

/*        CALL INSLAC ( ELEMS, N, OFFSET + LOC, POD(1), CUREND ) */
/*        CALL SCARDC ( CUREND,                 POD            ) */

/*     However, this is tedious, and it gets worse when you have to */
/*     check for possible overflow. PODIE accomplishes the same thing, */

/*        CALL PODIEC ( ELEMS, N, LOC, POD ) */

/*     more simply, and with error-handling built in. */

/* $ Restrictions */

/*     1) In any pod, only the active group should be accessed, */
/*        and its location should always be determined by PODBE */
/*        or PODON. Never assume that the active group begins */
/*        at POD(1). */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 15-JUL-1989 (WLT) (IMU) */


/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("PODIEC", (ftnlen)6);
    }

/*     Three things can go `wrong': */

/*        1) No items to insert. */

/*        2) Too many items to insert. */

/*        3) No place to insert them. */

    podonc_(pod, &offset, &number, pod_len);
    if (*n < 1) {
	chkout_("PODIEC", (ftnlen)6);
	return 0;
    } else if (cardc_(pod, pod_len) + *n > sizec_(pod, pod_len)) {
	setmsg_("Cannot fit # elements into # spaces.", (ftnlen)36);
	errint_("#", n, (ftnlen)1);
	i__1 = sizec_(pod, pod_len) - cardc_(pod, pod_len);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(TOOMANYPEAS)", (ftnlen)18);
	chkout_("PODIEC", (ftnlen)6);
	return 0;
    } else if (*loc < 1 || *loc > number + 1) {
	setmsg_("Location (#) must be in the range [1,#].", (ftnlen)40);
	errint_("#", loc, (ftnlen)1);
	i__1 = number + 1;
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(BADPODLOCATION)", (ftnlen)21);
	chkout_("PODIEC", (ftnlen)6);
	return 0;
    }

/*     In theory, we are home free. The rest looks just like the */
/*     code in $Examples, above. */

    end = offset + number;
    i__1 = offset + *loc;
    inslac_(elems, n, &i__1, pod + pod_len * 6, &end, elems_len, pod_len);
    scardc_(&end, pod, pod_len);
    chkout_("PODIEC", (ftnlen)6);
    return 0;
} /* podiec_ */

