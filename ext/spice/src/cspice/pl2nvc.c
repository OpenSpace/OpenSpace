/* pl2nvc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      PL2NVC ( Plane to normal vector and constant ) */
/* Subroutine */ int pl2nvc_(doublereal *plane, doublereal *normal, 
	doublereal *const__)
{
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);

/* $ Abstract */

/*     Return a unit normal vector and constant that define a specified */
/*     plane. */

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

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     PLANE      I   A SPICELIB plane. */
/*     NORMAL, */
/*     CONST      O   A normal vector and constant defining the */
/*                    geometric plane represented by PLANE. */

/* $ Detailed_Input */

/*     PLANE          is a SPICELIB plane. */

/* $ Detailed_Output */

/*     NORMAL, */
/*     CONST          are, respectively, a unit normal vector and */
/*                    constant that define the geometric plane */
/*                    represented by PLANE.  Let the symbol < a, b > */
/*                    indicate the inner product of vectors a and b; */
/*                    then the geometric plane is the set of vectors X */
/*                    in three-dimensional space that satisfy */

/*                       < X,  NORMAL >  =  CONST. */

/*                    NORMAL is a unit vector.  CONST is the distance of */
/*                    the plane from the origin; */

/*                       CONST * NORMAL */

/*                    is the closest point in the plane to the origin. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  The input plane MUST have been created by one of the SPICELIB */
/*         routines */

/*            NVC2PL ( Normal vector and constant to plane ) */
/*            NVP2PL ( Normal vector and point to plane    ) */
/*            PSV2PL ( Point and spanning vectors to plane ) */

/*         Otherwise, the results of this routine are unpredictable. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     SPICELIB geometry routines that deal with planes use the `plane' */
/*     data type to represent input and output planes.  This data type */
/*     makes the subroutine interfaces simpler and more uniform. */

/*     The SPICELIB routines that produce SPICELIB planes from data that */
/*     define a plane are: */

/*        NVC2PL ( Normal vector and constant to plane ) */
/*        NVP2PL ( Normal vector and point to plane    ) */
/*        PSV2PL ( Point and spanning vectors to plane ) */

/*     The SPICELIB routines that convert SPICELIB planes to data that */
/*     define a plane are: */

/*        PL2NVC ( Plane to normal vector and constant ) */
/*        PL2NVP ( Plane to normal vector and point    ) */
/*        PL2PSV ( Plane to point and spanning vectors ) */

/* $ Examples */

/*     1)  Given a point in a plane and a normal vector, find the */
/*         distance of the plane from the origin.  We make a */
/*         `plane' from the point and normal, then convert the */
/*         plane to a unit normal and constant.  CONST is the distance */
/*         of the plane from the origin. */

/*            CALL NVP2PL ( NORMAL, POINT,  PLANE ) */
/*            CALL PL2NVC ( PLANE,  NORMAL, CONST ) */


/*     2)  Apply a linear transformation represented by the matrix M to */
/*         a plane represented by the normal vector N and the constant C. */
/*         Find a normal vector and constant for the transformed plane. */

/*            C */
/*            C     Make a SPICELIB plane from N and C, and then find a */
/*            C     point in the plane and spanning vectors for the */
/*            C     plane.  N need not be a unit vector. */
/*            C */
/*                  CALL NVC2PL ( N,      C,      PLANE         ) */
/*                  CALL PL2PSV ( PLANE,  POINT,  SPAN1,  SPAN2 ) */

/*            C */
/*            C     Apply the linear transformation to the point and */
/*            C     spanning vectors.  All we need to do is multiply */
/*            C     these vectors by M, since for any linear */
/*            C     transformation T, */
/*            C */
/*            C           T ( POINT  +  t1 * SPAN1     +  t2 * SPAN2 ) */
/*            C */
/*            C        =  T (POINT)  +  t1 * T(SPAN1)  +  t2 * T(SPAN2), */
/*            C */
/*            C     which means that T(POINT), T(SPAN1), and T(SPAN2) */
/*            C     are a point and spanning vectors for the transformed */
/*            C     plane. */
/*            C */
/*                  CALL MXV ( M, POINT, TPOINT ) */
/*                  CALL MXV ( M, SPAN1, TSPAN1 ) */
/*                  CALL MXV ( M, SPAN2, TSPAN2 ) */

/*            C */
/*            C     Make a new SPICELIB plane TPLANE from the */
/*            C     transformed point and spanning vectors, and find a */
/*            C     unit normal and constant for this new plane. */
/*            C */
/*                  CALL PSV2PL ( TPOINT,  TSPAN1,  TSPAN2,  TPLANE ) */
/*                  CALL PL2NVC ( TPLANE,  TN,      TC              ) */

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

/*     plane to normal vector and constant */

/* -& */

/*     The contents of SPICELIB planes are as follows: */

/*        Elements NMLPOS through NMLPOS + 2 contain a unit normal */
/*        vector for the plane. */

/*        Element CONPOS contains a constant for the plane;  every point */
/*        X in the plane satisifies */

/*           < X, PLANE(NMLPOS) >  =  PLANE(CONPOS). */

/*        The plane constant is the distance of the plane from the */
/*        origin; the normal vector, scaled by the constant, is the */
/*        closest point in the plane to the origin. */



/*     Unpack the plane. */

    vequ_(plane, normal);
    *const__ = plane[3];
    return 0;
} /* pl2nvc_ */

