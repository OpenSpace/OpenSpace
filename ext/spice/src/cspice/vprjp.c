/* vprjp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b3 = 1.;

/* $Procedure      VPRJP ( Vector projection onto plane ) */
/* Subroutine */ int vprjp_(doublereal *vin, doublereal *plane, doublereal *
	vout)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    extern doublereal vdot_(doublereal *, doublereal *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), vlcom_(doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    doublereal const__;
    extern /* Subroutine */ int pl2nvc_(doublereal *, doublereal *, 
	    doublereal *);
    doublereal normal[3];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Project a vector onto a specified plane, orthogonally. */

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

/*     PLANES */

/* $ Keywords */

/*     GEOMETRY */
/*     MATH */
/*     PLANE */
/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     VIN        I   Vector to be projected. */
/*     PLANE      I   A SPICELIB plane onto which VIN is projected. */
/*     VOUT       O   Vector resulting from projection. */

/* $ Detailed_Input */

/*     VIN            is a 3-vector that is to be orthogonally projected */
/*                    onto a specified plane. */

/*     PLANE          is a SPICELIB plane that represents the geometric */
/*                    plane onto which VIN is to be projected. */

/* $ Detailed_Output */

/*     VOUT           is the vector resulting from the orthogonal */
/*                    projection of VIN onto PLANE.  VOUT is the closest */
/*                    point in the specified plane to VIN. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  Invalid input planes are diagnosed by the routine PL2NVC, */
/*         which is called by this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Projecting a vector V orthogonally onto a plane can be thought of */
/*     as finding the closest vector in the plane to V.  This `closest */
/*     vector' always exists; it may be coincident with the original */
/*     vector. */

/*     Two related routines are VPRJPI, which inverts an orthogonal */
/*     projection of a vector onto a plane, and VPROJ, which projects */
/*     a vector orthogonally onto another vector. */

/* $ Examples */

/*     1)   Find the closest point in the ring plane of a planet to a */
/*          spacecraft located at POSITN (in body-fixed coordinates). */
/*          Suppose the vector NORMAL is normal to the ring plane, and */
/*          that ORIGIN, which represents the body center, is in the */
/*          ring plane.  Then we can make a `plane' with the code */

/*             CALL PNV2PL ( ORIGIN, NORMAL, PLANE ) */

/*          can find the projection by making the call */

/*             CALL VPRJP ( POSITN, PLANE, PROJ ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1] `Calculus and Analytic Geometry', Thomas and Finney. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     vector projection onto plane */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("VPRJP", (ftnlen)5);
    }

/*     Obtain a unit vector normal to the input plane, and a constant */
/*     for the plane. */

    pl2nvc_(plane, normal, &const__);

/*     Let the notation < a, b > indicate the inner product of vectors */
/*     a and b. */

/*     VIN differs from its projection onto PLANE by some multiple of */
/*     NORMAL.  That multiple is */


/*               < VIN - VOUT, NORMAL >                 *  NORMAL */

/*        =   (  < VIN, NORMAL > - < VOUT, NORMAL >  )  *  NORMAL */

/*        =   (  < VIN, NORMAL > - CONST             )  *  NORMAL */


/*     Subtracting this multiple of NORMAL from VIN yields VOUT. */

    d__1 = const__ - vdot_(vin, normal);
    vlcom_(&c_b3, vin, &d__1, normal, vout);
    chkout_("VPRJP", (ftnlen)5);
    return 0;
} /* vprjp_ */

