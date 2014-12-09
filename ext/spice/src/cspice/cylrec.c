/* cylrec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      CYLREC ( Cylindrical to rectangular ) */
/* Subroutine */ int cylrec_(doublereal *r__, doublereal *long__, doublereal *
	z__, doublereal *rectan)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    doublereal x, y;

/* $ Abstract */

/*      Convert from cylindrical to rectangular coordinates. */

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

/*      CONVERSION,  COORDINATES */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  ------------------------------------------------- */
/*      R          I   Distance of a point from Z axis. */
/*      LONG       I   Angle (radians) of a point from XZ plane */
/*      Z          I   Height of a point above XY plane. */
/*      RECTAN     O   Rectangular coordinates of the point. */

/* $ Detailed_Input */

/*      R          Distance of the point of interest from Z axis. */

/*      LONG       Cylindrical angle (in radians) of the point of */
/*                 interest from XZ plane. */

/*      Z          Height of the point above XY plane. */

/* $ Detailed_Output */

/*      RECTAN     Rectangular coordinates of the point of interest. */

/* $ Parameters */

/*      None. */

/* $ Particulars */

/*      This routine transforms the coordinates of a point from */
/*      cylindrical to rectangular coordinates. */

/* $ Examples */

/*     Below are two tables. */

/*     Listed in the first table (under R, LONG and Z ) are */
/*     cylindrical coordinate triples that approximately represent */
/*     points whose rectangular coordinates are taken from the set */
/*     {-1, 0, 1}.  (Angular quantities are given in degrees.) */

/*     The result of the code fragment */

/*          Use the SPICELIB routine CONVRT to convert the angular */
/*          quantities to radians */

/*          CALL CONVRT ( LONG, 'DEGREES', 'RADIANS', LONG ) */

/*          CALL CYLREC ( R, LONG, Z, X ) */


/*     are listed in the second parallel table under X(1), X(2) and X(3). */


/*       R         LONG     Z            X(1)       X(2)     X(3) */
/*       -------------------------       -------------------------- */
/*       0.0000    0.0000   0.0000       0.0000     0.0000   0.0000 */
/*       1.0000    0.0000   0.0000       1.0000     0.0000   0.0000 */
/*       1.0000   90.0000   0.0000       0.0000     1.0000   0.0000 */
/*       0.0000    0.0000   1.0000       0.0000     0.0000   1.0000 */
/*       1.0000  180.0000   0.0000      -1.0000     0.0000   0.0000 */
/*       1.0000  270.0000   0.0000       0.0000    -1.0000   0.0000 */
/*       0.0000    0.0000  -1.0000       0.0000     0.0000  -1.0000 */
/*       1.4142   45.0000   0.0000       1.0000     1.0000   0.0000 */
/*       1.0000    0.0000   1.0000       1.0000     0.0000   1.0000 */
/*       1.0000   90.0000   1.0000       0.0000     1.0000   1.0000 */
/*       1.4142   45.0000   1.0000       1.0000     1.0000   1.0000 */


/* $ Restrictions */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*      None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     cylindrical to rectangular */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.0.1, 1-Feb-1989 (WLT) */

/*      Example section of header upgraded. */

/* -& */

/*     Local variables */


/*     Convert to rectangular coordinates, storing the results in */
/*     temporary variables. */

    x = *r__ * cos(*long__);
    y = *r__ * sin(*long__);

/*     Move the results to the output variables. */

    rectan[0] = x;
    rectan[1] = y;
    rectan[2] = *z__;
    return 0;
} /* cylrec_ */

