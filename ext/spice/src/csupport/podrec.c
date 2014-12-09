/* podrec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      PODREC ( Pod, remove elements, character ) */
/* Subroutine */ int podrec_(integer *n, integer *loc, char *pod, ftnlen 
	pod_len)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), scardc_(integer *, 
	    char *, ftnlen), remlac_(integer *, integer *, char *, integer *, 
	    ftnlen), podonc_(char *, integer *, integer *, ftnlen);
    integer offset, number;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    integer end;

/* $ Abstract */

/*     Remove elements beginning at a specified location within the */
/*     active group of a pod. */

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
/*     N          I   Number of elements to remove. */
/*     LOC        I   Location of first element to be removed. */
/*     POD       I,O  Pod. */

/* $ Detailed_Input */

/*     N          is the number of elements to be removed from the */
/*                active group of POD. */

/*     LOC        is the location (within the active group of the pod) */
/*                of the first element to be removed. */

/*     POD        on input, is a pod. */

/* $ Detailed_Output */

/*     POD        on output, is the same pod, the active group of */
/*                which contains the elements preceding and following */
/*                the removed elements. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If N is not positive, the pod is not changed. */

/*     2) If the location of the last element to be removed (LOC+N-1) */
/*        is greater than the number of elements in the active group, */
/*        the pod is not changed, and the error SPICE(NOTENOUGHPEAS) */
/*        is signalled. */

/*     3) If the location specified for location is not in the range */
/*        [1,NC], where NC is the number of elements in the active */
/*        group of the pod, the pod is not changed, and the error */
/*        SPICE(BADPODLOCATION) is signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine allows you to remove elements from the active */
/*     group of a pod without having to worry about checking for */
/*     impossible requests beforehand, or updating the cardinality */
/*     afterwards. */

/* $ Examples */

/*     Elements can be removed from the active group of a pod */
/*     by hand, */

/*        CALL PODONC ( POD, OFFSET, NUMBER ) */
/*        END = OFFSET + NUMBER */

/*        CALL REMLAC ( N,   OFFSET + LOC, POD(1), END ) */
/*        CALL SCARDC ( END,               POD         ) */

/*     However, this is tedious, and it gets worse when you have to */
/*     check for impossible requests. PODRE accomplishes the same thing, */

/*        CALL PODIEC ( N, LOC, POD ) */

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
	chkin_("PODREC", (ftnlen)6);
    }

/*     Three things can go `wrong': */

/*        1) No items to remove. */

/*        2) Too many items to remove. */

/*        3) No place to remove them from. */

    podonc_(pod, &offset, &number, pod_len);
    if (*n < 1) {
	chkout_("PODREC", (ftnlen)6);
	return 0;
    } else if (*loc + *n - 1 > number) {
	setmsg_("LOC = #; N = #; there are only # elements.", (ftnlen)42);
	errint_("#", loc, (ftnlen)1);
	errint_("#", n, (ftnlen)1);
	errint_("#", &number, (ftnlen)1);
	sigerr_("SPICE(NOTENOUGHPEAS)", (ftnlen)20);
	chkout_("PODREC", (ftnlen)6);
	return 0;
    } else if (*loc < 1 || *loc > number) {
	setmsg_("Location (#) must be in the range [1,#].", (ftnlen)40);
	errint_("#", loc, (ftnlen)1);
	errint_("#", &number, (ftnlen)1);
	sigerr_("SPICE(BADPODLOCATION)", (ftnlen)21);
	chkout_("PODREC", (ftnlen)6);
	return 0;
    }

/*     No problem. This is just like $Examples, above. */

    end = offset + number;
    i__1 = offset + *loc;
    remlac_(n, &i__1, pod + pod_len * 6, &end, pod_len);
    scardc_(&end, pod, pod_len);
    chkout_("PODREC", (ftnlen)6);
    return 0;
} /* podrec_ */

