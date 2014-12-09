/* sphsd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b7 = -1.;
static doublereal c_b8 = 1.;

/* $Procedure  SPHSD ( Spherical surface distance ) */
doublereal sphsd_(doublereal *radius, doublereal *long1, doublereal *lat1, 
	doublereal *long2, doublereal *lat2)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double sin(doublereal), cos(doublereal), acos(doublereal);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    doublereal sl1sl2;
    extern doublereal brcktd_(doublereal *, doublereal *, doublereal *);
    doublereal cosang;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return the distance between two points on a sphere, measured */
/*     along the shortest great circle arc connecting them. */

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

/*     GEOMETRY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     RADIUS     I   Radius of sphere. */
/*     LONG1, */
/*     LAT1       I   Longitude and latitude of first point in radians. */
/*     LONG2, */
/*     LAT2       I   Longitude and latitude of second point in radians. */

/*     The function returns the distance between the two input points, */
/*     measured along the shortest great circle arc connecting them. */

/* $ Detailed_Input */

/*     RADIUS         Radius of the sphere on which the points are */
/*                    located. */

/*     LONG1, */
/*     LAT1           Longitude and latitude of the first point.  The */
/*                    units are radians. */

/*     LONG2, */
/*     LAT2           Longitude and latitude of the second point. The */
/*                    units are radians. */

/* $ Detailed_Output */

/*     The function returns the distance between the two input points, */
/*     measured along the shortest great circle arc connecting them. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If RADIUS is negative, the error SPICE(INPUTOUTOFRANGE) */
/*         is signalled.  SPHSD is set to zero.  RADIUS may be zero; */
/*         this case is not treated as an exception. */

/*     2)  Latitudes out of the range [-pi/2, pi/2] are NOT treated */
/*         as errors, although they are not valid in the latitudinal */
/*         coordinate system and so may be considered to be exceptional */
/*         inputs.  All latitude values are used in the same way in the */
/*         computation, regardless of whether or not they are in range. */
/*         See the code for the equation used. */

/*     3)  Longitudes out of the range (-pi, pi] are NOT treated */
/*         as errors, although they are not valid in the latitudinal */
/*         coordinate system and so may be considered to be exceptional */
/*         inputs.  All longitude values are used in the same way in the */
/*         computation, regardless of whether or not they are in range. */
/*         See the code for the equation used. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     You may need to consider whether a spherical model is adequate */
/*     for your application; some bodies may be more accurately modelled */
/*     by an oblate or prolate spheroid, or by a triaxial ellipsoid. */

/* $ Examples */

/*     1)  To find the distance along a sphere of radius 1000 km between */
/*         the points at */

/*            longitude = 1.570796326794897D0  (pi/2) radians, */
/*            latitude  = 7.853981633974483D-1 (pi/4) radians */

/*         and */

/*            longitude = 0.0D0 radians, */
/*            latitude  = 7.853981633974483D-1 (pi/4) radians, */

/*         we could make the function call: */

/*            DIST = SPHSD ( 1.0D3, */
/*         .                 1.570796326794897D0, 7.853981633974483D-1, */
/*         .                 0.D0,                7.853981633974483D-1  ) */

/*         The value of DIST should be */

/*            1.047197551196598D3, */

/*         which is (very, very close to) 1000 * pi/3. */

/*         The exact numbers used in this example were obtained using */
/*         VAX Fortran 77 on a VAX 11/780; different compilers and */
/*         systems may yield different results. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     H.A. Neilan     (JPL) */
/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN) */

/*       If the value of the function RETURN is TRUE upon execution of */
/*       this module, this function is assigned a default value of */
/*       either 0, 0.0D0, .FALSE., or blank depending on the type of */
/*       the function. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*       Comment section for permuted index source lines was added */
/*       following the header. */

/* -    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     spherical surface distance */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Check RETURN but do not check in unless an error is detected. */

    if (return_()) {
	ret_val = 0.;
	return ret_val;
    }

/*     Make sure that RADIUS is ok; check in only if it isn't. */

    if (*radius < 0.) {
	ret_val = 0.;
	chkin_("SPHSD", (ftnlen)5);
	setmsg_("Radius was #.", (ftnlen)13);
	errdp_("#", radius, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("SPHSD", (ftnlen)5);
	return ret_val;
    }

/*     The usual equation for the distance between points, measured */
/*     along a great circle, is: */

/*                  -1 */
/*       DIST  =  COS (   ( COS(LONG1-LONG2) * COS(LAT1) * COS(LAT2) ) */
/*                      + (                    SIN(LAT1) * SIN(LAT2) )  ) */

/*              * RADIUS */

/*     To arrive at this equation, we find the cartesian coordinates of */
/*     the input surface points and take the dot product of the two */
/*     points. */

/*     To save a trig function reference, however, we implement this */
/*     calculation slightly differently. */


/*     COSANG is the cosine of the angle between the two position */
/*     vectors.  We bracket COSANG 'tween -1 and 1 to make sure */
/*     round-off error doesn't take it out of the domain of arc */
/*     cosine... */

    sl1sl2 = sin(*lat1) * sin(*lat2);
    cosang = cos(*long1 - *long2) * (cos(*lat1 - *lat2) - sl1sl2) + sl1sl2;
    ret_val = *radius * acos(brcktd_(&cosang, &c_b7, &c_b8));
    return ret_val;
} /* sphsd_ */

