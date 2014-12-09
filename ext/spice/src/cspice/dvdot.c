/* dvdot.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      DVDOT  ( Derivative of Vector Dot Product, 3-D ) */
doublereal dvdot_(doublereal *s1, doublereal *s2)
{
    /* System generated locals */
    doublereal ret_val;

/* $ Abstract */

/*     Compute the derivative of the dot product of two double */
/*     precision position vectors. */

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
/*     S1        I     First state vector in the dot product. */
/*     S2        I     Second state vector in the dot product. */

/*     The function returns the derivative of the dot product <S1,S2> */

/* $ Detailed_Input */

/*     S1      Any state vector.  The componets are in order */
/*             (x, y, z, dx/dt, dy/dt, dz/dt ) */

/*     S2      Any state vector. */

/* $ Detailed_Output */

/*     The function returns the derivative of the dot product of the */
/*     position portions of the two state vectors S1 and S2. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     Given two state vectors S1 and S2 made up of position and */
/*     velocity components (P1,V1) and (P2,V2) respectively, */
/*     DVDOT calculates the derivative of the dot product of P1 and P2, */
/*     i.e. the time derivative */

/*           d */
/*           -- < P1, P2 > = < V1, P2 > + < P1, V2 > */
/*           dt */

/*     where <,> denotes the dot product operation. */

/* $ Examples */

/*     Suppose that given two state vectors (S1 and S2)whose position */
/*     components are unit vectors, and that we need to compute the */
/*     rate of change of the angle between the two vectors. */

/*     We know that the Cosine of the angle THETA between them is given */
/*     by */

/*        COSINE(THETA) = VDOT(S1,S2) */

/*     Thus by the chain rule, the derivative of the angle is given */
/*     by: */

/*        SINE(THETA) dTHETA/dt = DVDOT(S1,S2) */

/*     Thus for values of THETA away from zero we can compute */

/*     dTHETA/dt as */

/*     DTHETA = DVDOT(S1,S2) / SQRT ( 1 - VDOT(S1,S2)**2 ) */

/*     Note that position components of S1 and S2 are parallel, the */
/*     derivative of the  angle between the positions does not */
/*     exist.  Any code that computes the derivative of the angle */
/*     between two position vectors should account for the case */
/*     when the position components are parallel. */

/* $ Restrictions */

/*     The user is responsible for determining that the states S1 and */
/*     S2 are not so large as to cause numeric overflow.  In most cases */
/*     this won't present a problem. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 18-MAY-1995 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Compute the derivative of a dot product */

/* -& */

    ret_val = s1[0] * s2[3] + s1[1] * s2[4] + s1[2] * s2[5] + s1[3] * s2[0] + 
	    s1[4] * s2[1] + s1[5] * s2[2];
    return ret_val;
} /* dvdot_ */

