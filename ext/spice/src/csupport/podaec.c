/* podaec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      PODAEC ( Pod, append elements, character ) */
/* Subroutine */ int podaec_(char *elems, integer *n, char *pod, ftnlen 
	elems_len, ftnlen pod_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__;
    extern integer cardc_(char *, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sizec_(char *, ftnlen);
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen),
	     errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    integer end;

/* $ Abstract */

/*     Append elements to the active group of a pod. */

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
/*     POD       I,O  Pod. */

/* $ Detailed_Input */

/*     ELEMS      contains elements to be appended to the active group */
/*                of POD. */

/*     N          is the number of elements in ELEMS. */

/*     POD        on input, is a pod. */

/* $ Detailed_Output */

/*     POD        on output, is the same pod, the active group of */
/*                which ends with the new elements. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */
/* $ */
/*     1) If N is not positive, the pod is not changed. */

/*     2) If there is insufficient room in the pod to append all */
/*        ofthe new elements, the pod is not changed, and the error */
/*        SPICE(TOOMANYPEAS) is signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a slightly more general version of APPND, which appends */
/*     a single item to a cell or to the active group of a pod. PODAE */
/*     allows you to append several items with a single subroutine call. */

/* $ Examples */

/*     Elements can be appended to a POD by hand, */

/*        END = CARDC ( POD ) */

/*        DO I = 1, N */
/*           POD(END+I) = ELEMS(I) */
/*        END DO */

/*        CALL SCARDC ( END + N, POD ) */

/*     However, this is tedious, and it gets worse when you have to */
/*     check for possible overflow. PODAE accomplishes the same thing, */

/*        CALL PODAEC ( ELEMS, N, POD ) */

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


/*     Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("PODAEC", (ftnlen)6);
    }

/*     We can't append a non-positive number of items. */

    if (*n < 1) {
	chkout_("PODAEC", (ftnlen)6);
	return 0;
    }

/*     First see if there is room in the pod to append N elements. */
/*     If not, bail out. */

    if (sizec_(pod, pod_len) < cardc_(pod, pod_len) + *n) {
	setmsg_("Cannot fit # elements into # spaces.", (ftnlen)36);
	errint_("#", n, (ftnlen)1);
	i__1 = sizec_(pod, pod_len) - cardc_(pod, pod_len);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(TOOMANYPEAS)", (ftnlen)18);

/*     There is ample room, so we find out where the end of the */
/*     active group is and simply loop through the individual */
/*     copies of ELEMS, adjusting the cardinality afterwards. */
/*     (Just like in $Examples, above.) */

    } else {
	end = cardc_(pod, pod_len);
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_copy(pod + (end + i__ + 5) * pod_len, elems + (i__ - 1) * 
		    elems_len, pod_len, elems_len);
	}
	i__1 = end + *n;
	scardc_(&i__1, pod, pod_len);
    }
    chkout_("PODAEC", (ftnlen)6);
    return 0;
} /* podaec_ */

