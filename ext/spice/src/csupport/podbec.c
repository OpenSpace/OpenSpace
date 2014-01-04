/* podbec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      PODBEC ( Pod, begin and end, character ) */
/* Subroutine */ int podbec_(char *pod, integer *begin, integer *end, ftnlen 
	pod_len)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen), podonc_(char *, 
	    integer *, integer *, ftnlen);
    integer offset, number;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return the indices of the initial and final elements of the */
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

/*     ARRAYS */
/*     CELLS */
/*     PODS */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     POD        I   Pod. */
/*     BEGIN      O   Index of initial item of active group of POD. */
/*     END        O   Index of final item of active group of POD. */

/* $ Detailed_Input */

/*     POD       is a pod. */

/* $ Detailed_Output */

/*     BEGIN, */
/*     END       are the indices of the initial and final elements of */
/*               the active group of POD. That is, the active group */
/*               is located in POD(BEGIN), POD(BEGIN+1), ..., POD(END). */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the active group of the pod contains no elements, */
/*        END is equal to (BEGIN - 1). */

/* $ Files */

/*     None. */

/* $ Particulars */

/*      PODBE (begin and end) and PODON (offset and number) provide */
/*      equivalent ways to access the elements of the active group */
/*      of a pod. Note that there is no way to access any group other */
/*      than the active group. */

/* $ Examples */

/*      PODBE is typically used to process the elements of the active */
/*      group of a pod one at a time, e.g., */

/*         CALL PODBEC ( POD, BEGIN, END ) */

/*         DO I = BEGIN, END */
/*            CALL PROCESS ( ..., POD(I), ... ) */
/*         END DO */

/*      Note that if the elements are to be correlated with the elements */
/*      of other arrays, PODON may be more convenient: */

/*         CALL PODONC ( POD, OFFSET, N ) */

/*         DO I = 1, N */
/*            CALL PROCESS ( ..., POD(OFFSET+I), ARRAY(I), ... ) */
/*         END DO */

/*      Either one may be used when the group is to be passed to a */
/*      subprogram as an array: */

/*         CALL SUBPROG ( ..., POD(BEGIN),    END-BEGIN+1, ... ) */
/*         CALL SUBPROG ( ..., POD(OFFSET+1),           N, ... ) */

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
	chkin_("PODBEC", (ftnlen)6);
    }

/*     We'll cheat: why write the same code twice? */

    podonc_(pod, &offset, &number, pod_len);
    *begin = offset + 1;
    *end = offset + number;
    chkout_("PODBEC", (ftnlen)6);
    return 0;
} /* podbec_ */

