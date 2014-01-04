/* elemi.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure            ELEMI ( Element of an integer set ) */
logical elemi_(integer *item, integer *a)
{
    /* System generated locals */
    integer i__1;
    logical ret_val;

    /* Local variables */
    extern integer cardi_(integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer bsrchi_(integer *, integer *, integer *);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*      Determine whether an item is an element of an integer set. */

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

/*      SETS */

/* $ Keywords */

/*      CELLS, SETS */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      ITEM       I   Item to be tested. */
/*      A          I   Set to be tested. */

/*      The function returns TRUE if ITEM is an element of set A. */

/* $ Detailed_Input */

/*      ITEM        is an item which may or may not be an element of */
/*                  the input set. */


/*      A           is a set. */


/* $ Detailed_Output */

/*      The function returns TRUE if ITEM is a member of the set A, */
/*      and returns FALSE otherwise. */

/* $ Parameters */

/*      None. */

/* $ Particulars */

/*      The LOGICAL functions ELEMC and ELEMI correspond to the */
/*      set operator IN in the Pascal language. */

/* $ Examples */

/*      Let the character sets PLANETS and ASTEROIDS contain the */
/*      following elements. */

/*            PLANETS            ASTEROIDS */
/*            --------           ---------- */
/*            'Earth'            'Apollo' */
/*            'Mars'             'Ceres' */
/*            'Pluto' */
/*            'Venus' */

/*      Then all of the following expressions are true. */

/*            ELEMC ( 'Earth',  PLANETS   ) */
/*            ELEMC ( 'Pluto',  PLANETS   ) */
/*            ELEMC ( 'Ceres',  ASTEROIDS ) */

/*      And all of the following expressions are false. */

/*            ELEMC ( 'Saturn', PLANETS   ) */
/*            ELEMC ( 'Pluto',  ASTEROIDS ) */
/*            ELEMC ( 'CERES',  ASTEROIDS ) */

/* $ Restrictions */

/*      None. */

/* $ Exceptions */

/*      None. */

/* $ Files */

/*      None. */

/* $ Literature_References */

/*      None. */

/* $ Author_and_Institution */

/*      N.J. Bachman    (JPL) */
/*      C.A. Curzon     (JPL) */
/*      H.A. Neilan     (JPL) */
/*      W.L. Taber      (JPL) */
/*      I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN) */

/*       If the value of the function RETURN is TRUE upon execution of */
/*       this module, this function is assigned a default value of */
/*       either 0, 0.0D0, .FALSE., or blank depending on the type of the */
/*       function. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     element of an integer set */

/* -& */
/* $ Revisions */

/* -    Beta Version 2.0.0, 13-MAR-1989 (NJB) */

/*        Now participates in error handling.  References to RETURN, */
/*        CHKIN, and CHKOUT added. */

/* -& */

/*     SPICELIB functions */


/*     Standard error handling: */

    if (return_()) {
	ret_val = FALSE_;
	return ret_val;
    } else {
	chkin_("ELEMI", (ftnlen)5);
    }

/*     Just a binary search. */

    i__1 = cardi_(a);
    ret_val = bsrchi_(item, &i__1, &a[6]) != 0;
    chkout_("ELEMI", (ftnlen)5);
    return ret_val;
} /* elemi_ */

