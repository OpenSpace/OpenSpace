/* tipbod.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;

/* $Procedure  TIPBOD ( Transformation, inertial position to bodyfixed ) */
/* Subroutine */ int tipbod_(char *ref, integer *body, doublereal *et, 
	doublereal *tipm, ftnlen ref_len)
{
    doublereal ref2j[9]	/* was [3][3] */;
    extern /* Subroutine */ int chkin_(char *, ftnlen), moved_(doublereal *, 
	    integer *, doublereal *);
    extern logical failed_(void);
    extern /* Subroutine */ int bodmat_(integer *, doublereal *, doublereal *)
	    , chkout_(char *, ftnlen);
    doublereal tmpmat[9]	/* was [3][3] */;
    extern /* Subroutine */ int irftrn_(char *, char *, doublereal *, ftnlen, 
	    ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int mxm_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*      Return a 3x3 matrix that transforms positions in inertial */
/*      coordinates to positions in body-equator-and-prime-meridian */
/*      coordinates. */

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

/*      PCK */
/*      NAIF_IDS */
/*     ROTATION */
/*      TIME */

/* $ Keywords */

/*      TRANSFORMATION */
/*      ROTATION */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      REF        I   ID of inertial reference frame to transform from. */
/*      BODY       I   ID code of body. */
/*      ET         I   Epoch of transformation. */
/*      TIPM       O   Transformation (position), inertial to prime */
/*                     meridian. */

/* $ Detailed_Input */

/*      REF         is the NAIF name for an inertial reference frame. */
/*                  Acceptable names include: */

/*                    Name       Description */
/*                    --------   -------------------------------- */
/*                    'J2000'    Earth mean equator, dynamical */
/*                               equinox of J2000 */

/*                    'B1950'    Earth mean equator, dynamical */
/*                               equinox of B1950 */

/*                    'FK4'      Fundamental Catalog (4) */

/*                    'DE-118'   JPL Developmental Ephemeris (118) */

/*                    'DE-96'    JPL Developmental Ephemeris ( 96) */

/*                    'DE-102'   JPL Developmental Ephemeris (102) */

/*                    'DE-108'   JPL Developmental Ephemeris (108) */

/*                    'DE-111'   JPL Developmental Ephemeris (111) */

/*                    'DE-114'   JPL Developmental Ephemeris (114) */

/*                    'DE-122'   JPL Developmental Ephemeris (122) */

/*                    'DE-125'   JPL Developmental Ephemeris (125) */

/*                    'DE-130'   JPL Developmental Ephemeris (130) */

/*                    'GALACTIC' Galactic System II */

/*                    'DE-200'   JPL Developmental Ephemeris (200) */

/*                    'DE-202'   JPL Developmental Ephemeris (202) */

/*                  (See the routine CHGIRF for a full list of names.) */

/*                  The output TIPM will give the transformation */
/*                  from this frame to the bodyfixed frame specified by */
/*                  BODY at the epoch specified by ET. */


/*      BODY        is the integer ID code of the body for which the */
/*                  position transformation matrix is requested. Bodies */
/*                  are numbered according to the standard NAIF */
/*                  numbering scheme.  The numbering scheme is */
/*                  explained in the NAIF_IDS required reading file. */

/*      ET          is the epoch at which the position transformation */
/*                  matrix is requested. (This is typically the */
/*                  epoch of observation minus the one-way light time */
/*                  from the observer to the body at the epoch of */
/*                  observation.) */

/* $ Detailed_Output */

/*      TIPM        is a 3x3 coordinate transformation matrix.  It is */
/*                  used to transform positions from inertial */
/*                  coordinates to body fixed (also called equator and */
/*                  prime meridian --- PM) coordinates. */

/*                  Given a position P in the inertial reference frame */
/*                  specified by REF, the corresponding bodyfixed */
/*                  position is given by the matrix vector product: */

/*                     TIPM * S */

/*                  The X axis of the PM system is directed to the */
/*                  intersection of the equator and prime meridian. */
/*                  The Z axis points along  the spin axis and points */
/*                  towards the same side of the invariable plane of */
/*                  the solar system as does earth's north pole. */

/* $ Parameters */

/*      None. */

/* $ Exceptions */

/*      1) If the kernel pool does not contain all of the data required */
/*         for computing the transformation matrix, TIPM, the error */
/*         SPICE(INSUFFICIENTANGLES) is signalled. */

/*      2) If the reference frame, REF,  is not recognized, a routine */
/*         called by TIPBOD will diagnose the condition and invoke the */
/*         SPICE error handling system. */

/*      3) If the specified body code, BODY, is not recognized, the */
/*         error is diagnosed by a routine called by TIPBOD. */

/* $ Files */

/*      None. */

/* $ Particulars */

/*     TIPBOD takes PCK information as input, either in the */
/*     form of a binary or text PCK file.  High precision */
/*     binary files are searched for first (the last loaded */
/*     file takes precedence); then it defaults to the text */
/*     PCK file.  If binary information is found for the */
/*     requested body and time, the Euler angles are */
/*     evaluated and the transformation matrix is calculated */
/*     from them.  Using the Euler angles PHI, DELTA and W */
/*     we compute */

/*            TIPM = [W] [DELTA] [PHI] */
/*                      3       1     3 */


/*      If no appropriate binary PCK files have been loaded, */
/*      the text PCK file is used.  Here information is found */
/*      as RA, DEC and W (with the possible addition of nutation */
/*      and libration terms for satellites).  Again, the Euler */
/*      angles are found, and the transformation matrix is */
/*      calculated from them.  The transformation from inertial to */
/*      bodyfixed coordinates is represented as: */

/*            TIPM = [W] [HALFPI-DEC] [RA+HALFPI] */
/*                      3            1           3 */

/*     These are basically the Euler angles, PHI, DELTA and W: */

/*       RA = PHI - HALFPI */
/*       DEC = HALFPI - DELTA */
/*       W = W */

/*      In the text file, RA, DEC, and W are defined as follows: */

/*                                         2      ____ */
/*                                    RA2*t       \ */
/*            RA  = RA0  + RA1*t/T  + ------   +  /     a  sin theta */
/*                                       2        ----   i          i */
/*                                      T           i */

/*                                          2     ____ */
/*                                    DEC2*t      \ */
/*            DEC = DEC0 + DEC1*t/T + -------  +  /    d  cos theta */
/*                                        2       ----  i          i */
/*                                       T          i */


/*                                        2      ____ */
/*                                    W2*t       \ */
/*            W   = W0   + W1*t/d   + -----   +  /     w  sin theta */
/*                                       2       ----   i          i */
/*                                      d          i */


/*      where: */

/*            d = seconds/day */

/*            T = seconds/Julian century */

/*            a , d , and w  arrays apply to satellites only. */
/*             i   i       i */

/*            theta  = THETA0(i) + THETA1(i)*t/T are specific to each */
/*                 i */

/*            planet. */


/*        These angles -- typically nodal rates -- vary in number and */
/*        definition from one planetary system to the next. */

/* $ Examples */

/*      Note that the items necessary to compute the Euler angles */
/*      must have been loaded into the kernel pool (by one or more */
/*      previous calls to FURNSH).  The Euler angles are typically */
/*      stored in the P_constants kernel file that comes with */
/*      SPICELIB. */

/*      1)  In the following code fragment, TIPBOD is used to transform */
/*          a position in J2000 inertial coordinates to a state in */
/*          bodyfixed coordinates. */

/*          The 3-vectors POSTN represents the inertial position */
/*          of an object with respect to the center of the */
/*          body at time ET. */

/*             C */
/*             C     First load the kernel pool. */
/*             C */
/*                   CALL FURNSH ( 'PLANETARY_CONSTANTS.KER' ) */

/*             C */
/*             C     Next get the transformation and its derivative. */
/*             C */
/*                   CALL TIPBOD ( 'J2000', BODY, ET, TIPM ) */

/*             C */
/*             C     Convert position, the first three elements of */
/*             C     STATE, to bodyfixed coordinates. */
/*             C */
/*                   CALL MXVG    ( TIPM, POSTN, BDPOS ) */

/* $ Restrictions */

/*      The kernel pool must be loaded with the appropriate */
/*      coefficients (from the P_constants kernel or binary PCK file) */
/*      prior to calling this routine. */

/* $ Literature_References */

/*      None. */

/* $ Author_and_Institution */

/*      N.J. Bachman   (JPL) */
/*      W.L. Taber     (JPL) */
/*      K.S. Zukor     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 23-OCT-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MXM call.  Replaced header references to LDPOOL with */
/*        references to FURNSH. */

/* -    SPICELIB Version 1.1.0, 05-JAN-2005 (NJB) */

/*        Tests of routine FAILED() were added. */

/* -     SPICELIB Version 1.0.3, 10-MAR-1994 (KSZ) */

/*         Underlying BODMAT code changed to look for binary PCK */
/*         data files, and use them to get orientation information if */
/*         they are available.  Only the comments to TIPBOD changed. */

/* -     SPICELIB Version 1.0.2, 06-JUL-1993 (HAN) */

/*         Example in header was corrected. Previous version had */
/*         incorrect matrix dimension specifications passed to MXVG. */

/* -     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -     SPICELIB Version 1.0.0, 05-AUG-1991 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     transformation from inertial position to bodyfixed */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0, 06-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MXM call.  Replaced header references to LDPOOL with */
/*        references to FURNSH. */


/* -    SPICELIB Version 1.1.0, 05-JAN-2005 (NJB) */

/*        Tests of routine FAILED() were added.  The new checks */
/*        are intended to prevent arithmetic operations from */
/*        being performed with uninitialized or invalid data. */
/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("TIPBOD", (ftnlen)6);
    }

/*     Get the transformation from the inertial from REF to J2000 */
/*     coordinates. */

    irftrn_(ref, "J2000", ref2j, ref_len, (ftnlen)5);

/*     Get the transformation from J2000 to body-fixed coordinates */
/*     for the requested epoch. */

    bodmat_(body, et, tipm);
    if (failed_()) {
	chkout_("TIPBOD", (ftnlen)6);
	return 0;
    }

/*     Compose the transformations to arrive at the REF-to-J2000 */
/*     transformation. */

    mxm_(tipm, ref2j, tmpmat);
    moved_(tmpmat, &c__9, tipm);

/*     That's all folks.  Check out and get out. */

    chkout_("TIPBOD", (ftnlen)6);
    return 0;
} /* tipbod_ */

