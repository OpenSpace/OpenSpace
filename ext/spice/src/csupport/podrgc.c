/* podrgc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      PODRGC ( Pod, replace group, character ) */
/* Subroutine */ int podrgc_(char *pod, ftnlen pod_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dcodec_(char *, 
	    integer *, ftnlen), scardc_(integer *, char *, ftnlen), podonc_(
	    char *, integer *, integer *, ftnlen);
    integer newoff, offset, number;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     End the active group of a pod, replacing the previous group. */

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
/* $ */
/*     ARRAY */
/*     CELLS */
/*     PODS */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     POD       I,O  Pod. */

/* $ Detailed_Input */

/*     POD       on input, is an arbitrary pod. */

/* $ Detailed_Output */

/*     POD       on output, is the same pod after the active group */
/*               has been closed. In other words, the number of groups */
/*               has been reduced by one, and the new active group */
/*               contains the ... oh, look at $Examples. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the active group is the only group in the pod, the */
/*        pod is not changed. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The active group of a pod may be removed by any of the */
/*     following routines: PODEG (end group), PODCG (close group), */
/*     or PODRG (replace group). */

/*     PODEG effectively returns the pod to its state before the */
/*     active group was created. The contents of the active group */
/*     are simply lost. */

/*     PODCG appends the contents of the active group to the previous */
/*     group to obtain the new active group, reducing the number of */
/*     groups in the pod by one. */

/*     PODRG also reduces the number of groups, but by replacing the */
/*     previous group with the active group, as though the previous */
/*     group had never existed. */

/* $ Examples */

/*     Let NAMES be a character POD containing the following groups: */

/*        Group 1:  NEWTON */
/*                  GALILEO */
/*                  KEPLER */

/*        Group 2:  EINSTEIN */
/*                  BOHR */
/*                  HEISENBERG */

/*        Group 3:  FEYNMAN */
/*                  BARDEEN */

/*     Following the call */

/*        CALL PODEGC ( NAMES ) */

/*     the active group (Group 2) contains EINSTEIN, BOHR, and */
/*     HEISENBERG. Following the call */

/*        CALL PODCGC ( NAMES ) */

/*     the active group (again, Group 2) contains EINSTEIN, BOHR, */
/*     HEISENBERG, FEYNMAN, and BARDEEN. Following the call */

/*        CALL PODRGC ( NAMES ) */

/*     the active group (also Group 2) contains FEYNMAN and BARDEEN. */

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


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("PODRGC", (ftnlen)6);
    }

/*     At any given time, the offset of the active group is stored */
/*     in location GRPOFF of the control area, so POD(GRPOFF) tells */
/*     us the location of the element preceding the active group. */

/*     This element is a backward pointer, containing the offset of */
/*     the previous group; and so on, with turtles all the way down. */
/*     For example, consider a pod with three groups */

/*         G.  <10> */
/*         1.  Bob */
/*         2.  Carol */
/*         3.  Ted */
/*         4.  Alice */
/*         5.  <0> */
/*         6.  Fred */
/*         7.  Wilma */
/*         8.  Barney */
/*         9.  Bettey */
/*        10.  <5> */
/*        11.  Ricky */
/*        12.  Lucy */
/*        13.  Fred */
/*        14.  Ethel */

/*     When the second group was created, the offset of the first */
/*     group (zero) was appended to the pod; the location of this */
/*     offset became the offset for the second group. When the */
/*     third group was created, the offset of the second group (5) */
/*     was appended; the location of this offset became the offset for */
/*     the third group. The offset for the third group is located */
/*     in element GRPOFF. */

/*     To remove a group then, all that is necessary is to look at */
/*     element GRPOFF to get the offset of the current group; go to */
/*     that location to get the offset of the previous group; and */
/*     move that offset into element GRPOFF. To replace the previous */
/*     group, just move all of the elements of the active group */
/*     toward the front of the pod. The new cardinality, of course, */
/*     should be the new offset plus the number of elements in the */
/*     original group. */

/*     If the pod contains only one group, we don't have to do */
/*     anything. */

    podonc_(pod, &offset, &number, pod_len);
    if (offset != 0) {
	s_copy(pod + pod_len * 3, pod + (offset + 5) * pod_len, pod_len, 
		pod_len);
	dcodec_(pod + (offset + 5) * pod_len, &newoff, pod_len);
	i__1 = number;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_copy(pod + (newoff + i__ + 5) * pod_len, pod + (offset + i__ + 
		    5) * pod_len, pod_len, pod_len);
	}
	i__1 = newoff + number;
	scardc_(&i__1, pod, pod_len);
    }
    chkout_("PODRGC", (ftnlen)6);
    return 0;
} /* podrgc_ */

