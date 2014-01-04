/* poddgi.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      PODDGI ( Pod, duplicate group, integer ) */
/* Subroutine */ int poddgi_(integer *pod)
{
    integer need, have;
    extern integer cardi_(integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sizei_(integer *);
    extern /* Subroutine */ int podaei_(integer *, integer *, integer *), 
	    podbgi_(integer *);
    integer offset, number;
    extern /* Subroutine */ int podoni_(integer *, integer *, integer *), 
	    sigerr_(char *, ftnlen), chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Begin a new group within a pod, containing the same elements */
/*     as the active group. */

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

/*     ARRAYS */
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

/*     POD       on output, is the same pod, in which the active */
/*               group has been sealed, and a new active group */
/*               (containing the same elements as the previous group) */
/*               begun. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If POD does not have sufficient free space to create */
/*        the new group, the pod is not changed, and the error */
/*        SPICE(TOOMANYPEAS) is signalled. (If the active group */
/*        contains no elements, there must be sufficient free */
/*        space for the new group to contain at least one element.) */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     There are two ways to create a new group within a pod. */
/*     PODBG (begin group) seals the current contents of the pod, */
/*     and creates a new active group containing no elements. */
/*     PODDG (duplicate group) also seals the current contents */
/*     of the pod, but places a copy of the previous active */
/*     group into the new active group. */

/*     In both cases, the active group and all previous groups are */
/*     unavailable so long as the new group exists. */

/*     The active group of a pod may be removed by any of the */
/*     following routines: PODEG (end group), PODCG (close group), */
/*     or PODRG (replace group). */

/* $ Examples */

/*     Let the active group of POD be located in elements 21 */
/*     through 40. Then following the call */

/*        CALL PODBGI ( POD ) */

/*     the active group is located in elements 42 through 41. */
/*     In other words, element 41 has been appropriated by the */
/*     pod itself, and the active group is empty. */

/*     However, following the call */

/*        CALL PODDG ( POD ) */

/*     the active group is located in elements 42 through 61, */
/*     and contains the same elements as the previous active */
/*     group. */

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
	chkin_("PODDGI", (ftnlen)6);
    }

/*     How many spaces are needed? One for bookkeeping, and one for */
/*     each of the elements in the active group. (If there are no */
/*     elements, then one for future use.) */

    podoni_(pod, &offset, &number);
    have = sizei_(pod);
    need = cardi_(pod) + 1 + max(1,number);
    if (have < need) {
	sigerr_("SPICE(TOOMANYPEAS)", (ftnlen)18);
	chkout_("PODDGI", (ftnlen)6);
	return 0;
    }

/*     Go ahead and create a new (empty) group. */

    podbgi_(pod);

/*     Append the old group (still in the same place) to the pod. */
/*     (Somewhat incestuous, but practical.) Kids, don't try this */
/*     at home: you aren't supposed to know that existing groups */
/*     arent't changed by the addition of new ones. */

    podaei_(&pod[offset + 6], &number, pod);
    chkout_("PODDGI", (ftnlen)6);
    return 0;
} /* poddgi_ */

