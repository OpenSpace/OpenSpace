/* fixuni.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__4 = 4;

/* $Procedure      FIXUNI ( Temporary fix for UNITIM bug. ) */
/* Subroutine */ int fixuni_(void)
{
    /* Initialized data */

    static char vars__[32*4] = "DELTET/DELTA_T_A                " "DELTET/K "
	    "                       " "DELTET/EB                       " "DEL"
	    "TET/M                        ";

    extern /* Subroutine */ int swpool_(char *, integer *, char *, ftnlen, 
	    ftnlen);

/* $ Abstract */

/*     This routine sets POOL watch for LSK variables that is not */
/*     set by UNITIM when it fails the first time it's called. */
/*     This (or equivalent) fix will be made in UNITIM for N0049 */
/*     delivery. */

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

/* $ Author_and_Institution */

/*     B.V. Semenov     (NAIF) */

/* $ Version */

/* -    CKBRIEF Beta Version 1.0.0, 11-AUG-1998 (BVS) */

/* -& */

/*     All variables, parameters and actual code in this routine */
/*     were "borrowed" from SPICELIB's UNITIM. */


/*     NEEDED is the number of kernel pool variables needed by this */
/*     routine. */


/*     VARS holds names of these variables. VARS are to be saved. */


/*     Initial values of VARS correspond to those in UNITIM. */


/*     The only thing this routine has to do is to set watchers */
/*     to fix the problem in UNITIM. */

    swpool_("UNITIM", &c__4, vars__, (ftnlen)6, (ftnlen)32);

/*     All done. Bye-bye. */

    return 0;
} /* fixuni_ */

