/* dvcrss.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      DVCRSS ( Derivative of Vector cross product ) */
/* Subroutine */ int dvcrss_(doublereal *s1, doublereal *s2, doublereal *sout)
{
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    doublereal vtemp[3];
    extern /* Subroutine */ int vcrss_(doublereal *, doublereal *, doublereal 
	    *);
    doublereal dvtmp1[3], dvtmp2[3];

/* $ Abstract */

/*     Compute the cross product of two 3-dimensional vectors */
/*     and the derivative of this cross product. */

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

/*     VECTOR */
/*     DERIVATIVE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     S1        I   Left hand state for cross product and derivative. */
/*     S2        I   Right hand state for cross product and derivative. */
/*     SOUT      O   State associated with cross product of positions. */

/* $ Detailed_Input */

/*     S1       This may be any state vector.  Typically, this */
/*              might represent the apparent state of a planet or the */
/*              Sun, which defines the orientation of axes of */
/*              some coordinate system. */

/*     S2       A state vector. */

/* $ Detailed_Output */

/*     SOUT     This variable represents the state associated with the */
/*              cross product of the position components of S1 and S2. */
/*              In other words, if S1 = (P1,V1) and S2 = (P2,V2) then */
/*              SOUT is ( P1xP2, d/dt{ P1xP2 } ). */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) If S1 and S2 are large in magnitude (taken together, */
/*        their magnitude surpasses the limit allowed by the */
/*        computer) then it may be possible to generate a */
/*        floating point overflow from an intermediate */
/*        computation even though the actual cross product and */
/*        derivative may be well within the range of double */
/*        precision numbers. */

/*        DVCRSS does NOT check the magnitude of S1 or S2 to */
/*        insure that overflow will not occur. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     DVCRSS calculates the three-dimensional cross product of two */
/*     vectors and the derivative of that cross product according to */
/*     the definition. */

/* $ Examples */

/*            S1                    S2                   SOUT */
/*     ----------------------------------------------------------------- */
/*     (0, 1, 0, 1, 0, 0)  ( 1,  0,  0, 1, 0, 0)  (0, 0, -1, 0,  0, -1 ) */
/*     (5, 5, 5, 1, 0, 0)  (-1, -1, -1, 2, 0, 0)  (0, 0,  0, 0, 11,-11 ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 22-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.0, 15-JUN-1995 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Compute the derivative of a cross product */

/* -& */


/*     Local Variables */


/*     Calculate the cross product of S1 and S2, store it in VTEMP. */

    vcrss_(s1, s2, vtemp);

/*     Calculate the two components of the derivative of S1 x S2. */

    vcrss_(&s1[3], s2, dvtmp1);
    vcrss_(s1, &s2[3], dvtmp2);

/*     Put all of the pieces into SOUT. */

    vequ_(vtemp, sout);
    vadd_(dvtmp1, dvtmp2, &sout[3]);
    return 0;
} /* dvcrss_ */

