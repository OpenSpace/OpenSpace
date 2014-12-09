/* tkfram.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__200 = 200;
static integer c__1 = 1;
static integer c__2 = 2;
static integer c__9 = 9;
static doublereal c_b101 = -1.;
static integer c__3 = 3;
static integer c__4 = 4;
static integer c__14 = 14;

/* $Procedure      TKFRAM (Text kernel frame transformation ) */
/* Subroutine */ int tkfram_(integer *id, doublereal *rot, integer *frame, 
	logical *found)
{
    /* Initialized data */

    static integer at = 0;
    static logical first = TRUE_;

    /* System generated locals */
    address a__1[2];
    integer i__1, i__2[2], i__3;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    static char name__[32];
    static integer tail;
    static char spec[32], item[32*14];
    static integer idnt[1], axes[3];
    static logical full;
    static integer pool[412]	/* was [2][206] */;
    extern doublereal vdot_(doublereal *, doublereal *);
    static char type__[1];
    static doublereal qtmp[4];
    extern /* Subroutine */ int eul2m_(doublereal *, doublereal *, doublereal 
	    *, integer *, integer *, integer *, doublereal *);
    static integer i__, n, r__;
    static doublereal buffd[1800]	/* was [9][200] */;
    static integer buffi[200]	/* was [1][200] */, oldid;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char agent[32];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    ident_(doublereal *), errch_(char *, char *, ftnlen, ftnlen);
    static doublereal tempd;
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *),
	     repmi_(char *, char *, integer *, char *, ftnlen, ftnlen, ftnlen)
	    , vhatg_(doublereal *, integer *, doublereal *);
    extern integer lnktl_(integer *, integer *);
    static char idstr[32];
    extern integer rtrim_(char *, ftnlen);
    static char versn[8], units[32];
    static integer ar;
    extern logical failed_(void), badkpv_(char *, char *, char *, integer *, 
	    integer *, char *, ftnlen, ftnlen, ftnlen, ftnlen);
    static char frname[32];
    static doublereal angles[3];
    static char oldagt[32];
    static logical buffrd;
    extern /* Subroutine */ int locati_(integer *, integer *, integer *, 
	    integer *, integer *, logical *), frmnam_(integer *, char *, 
	    ftnlen), namfrm_(char *, integer *, ftnlen);
    static logical update;
    static char altnat[32];
    extern /* Subroutine */ int lnkini_(integer *, integer *);
    extern integer lnknfn_(integer *);
    static integer idents[200]	/* was [1][200] */;
    extern /* Subroutine */ int gcpool_(char *, integer *, integer *, integer 
	    *, char *, logical *, ftnlen, ftnlen), gdpool_(char *, integer *, 
	    integer *, integer *, doublereal *, logical *, ftnlen), sigerr_(
	    char *, ftnlen), gipool_(char *, integer *, integer *, integer *, 
	    integer *, logical *, ftnlen), chkout_(char *, ftnlen), sharpr_(
	    doublereal *), dtpool_(char *, logical *, integer *, char *, 
	    ftnlen, ftnlen), setmsg_(char *, ftnlen);
    static doublereal matrix[9]	/* was [3][3] */;
    extern /* Subroutine */ int cvpool_(char *, logical *, ftnlen), dwpool_(
	    char *, ftnlen), errint_(char *, integer *, ftnlen), vsclip_(
	    doublereal *, doublereal *);
    static doublereal quatrn[4];
    extern /* Subroutine */ int convrt_(doublereal *, char *, char *, 
	    doublereal *, ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int q2m_(doublereal *, doublereal *), intstr_(
	    integer *, char *, ftnlen), swpool_(char *, integer *, char *, 
	    ftnlen, ftnlen);
    static logical fnd;
    static char alt[32*14];

/* $ Abstract */

/*     This routine returns the rotation from the input frame */
/*     specified by ID to the associated frame given by FRAME. */

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

/*     FRAMES */

/* $ Keywords */

/*     POINTING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  ---------------------------------------------- */
/*     ID         I   Class identification code for the instrument */
/*     ROT        O   The rotation from ID to FRAME. */
/*     FRAME      O   The integer code of some reference frame. */
/*     FOUND      O   TRUE if the rotation could be determined. */

/* $ Detailed_Input */

/*     ID          The identification code used to specify an */
/*                 instrument in the SPICE system. */

/* $ Detailed_Output */

/*     ROT         is a rotation matrix that gives the transformation */
/*                 from the frame specified by ID to the frame */
/*                 specified by FRAME. */

/*     FRAME       is the id code of the frame used to define the */
/*                 orientation of the frame given by ID.  ROT gives */
/*                 the transformation from the IF frame to */
/*                 the frame specified by FRAME. */

/*     FOUND       is a logical indicating whether or not a frame */
/*                 definition for frame ID was constructed from */
/*                 kernel pool data.  If ROT and FRAME were constructed */
/*                 FOUND will be returned with the value TRUE. */
/*                 Otherwise it will be returned with the value FALSE. */

/* $ Parameters */

/*     BUFSIZ      is the number of rotation, frame id pairs that */
/*                 can have their instance data buffered for the */
/*                 sake of improving run-time performance.  This */
/*                 value MUST be positive and should probably be */
/*                 at least 10. */

/* $ Exceptions */

/*     1)  If some instance value associated with this frame */
/*         cannot be located, or does not have the proper type */
/*         or dimension, the error will be diagnosed by the */
/*         routine BADKPV. In such a case FOUND will be set to .FALSE. */

/*     2)  If the input ID has the value 0, the error */
/*         SPICE(ZEROFRAMEID) will be signaled. FOUND will be set */
/*         to FALSE. */

/*     3)  If the name of the frame corresponding to ID cannot be */
/*         determined, the error 'SPICE(INCOMPLETEFRAME)' is signaled. */

/*     4)  If the frame given by ID is defined relative to a frame */
/*         that is unrecognized, the error SPICE(BADFRAMESPEC) */
/*         will be signaled.  FOUND will be set to FALSE. */

/*     5)  If the kernel pool specification for ID is not one of */
/*         MATRIX, ANGLES, or QUATERNION, then the error */
/*         SPICE(UNKNOWNFRAMESPEC) will be signaled. FOUND will be */
/*         set to FALSE. */

/*     6)  If the frame ID is equal to the relative frame ID (i.e. the */
/*         frame is defined relative to itself), the error */
/*         SPICE(BADFRAMESPEC2) will be signaled.  FOUND will be set to */
/*         FALSE. */

/* $ Files */

/*     This routine makes use of the loaded text kernels to */
/*     determine the rotation from a constant offset frame */
/*     to its defining frame. */

/* $ Particulars */

/*     This routine is used to construct the rotation from some frame */
/*     that is a constant rotation offset from some other reference */
/*     frame. This rotation is derived from data stored in the kernel */
/*     pool. */

/*     It is considered to be an low level routine that */
/*     will need to be called directly only by persons performing */
/*     high volume processing. */

/* $ Examples */

/*     This is intended to be used as a low level routine by */
/*     the frame system software.  However, you could use this */
/*     routine to directly retrieve the rotation from an offset */
/*     frame to its relative frame.  One instance in which you */
/*     might do this is if you have a properly specified topocentric */
/*     frame for some site on earth and you wish to determine */
/*     the geodetic latitude and longitude of the site.  Here's how. */

/*        Suppose the name of the topocentric frame is: 'MYTOPO'. */
/*        First we get the id-code of the topocentric frame. */

/*        CALL NAMFRM ( 'MYTOPO', FRCODE ) */

/*        Next get the rotation from the topocentric frame to */
/*        the bodyfixed frame. */

/*        CALL TKFRAM ( FRCODE, ROT, FRAME, FOUND ) */

/*        Make sure the topoframe is relative to one of the earth */
/*        fixed frames. */

/*        CALL FRMNAM( FRAME, TEST ) */

/*        IF (       TEST .NE. 'IAU_EARTH' */
/*       .     .AND. TEST .NE. 'EARTH_FIXED' */
/*       .     .AND. TEST .NE. 'ITRF93'  ) THEN */

/*           WRITE (*,*) 'The frame MYTOPO does not appear to be ' */
/*           WRITE (*,*) 'defined relative to an earth fixed frame.' */
/*           STOP */

/*        END IF */

/*        Things look ok. Get the location of the Z-axis in the */
/*        topocentric frame. */

/*        Z(1) = ROT(1,3) */
/*        Z(2) = ROT(2,3) */
/*        Z(3) = ROT(3,3) */

/*        Convert the Z vector to latitude longitude and radius. */

/*        CALL RECLAT ( Z, LAT, LONG, RAD ) */

/*        WRITE (*,*) 'The geodetic coordinates of the center of' */
/*        WRITE (*,*) 'the topographic frame are: ' */
/*        WRITE (*,*) */
/*        WRITE (*,*) 'Latitude  (deg): ', LAT *DPR() */
/*        WRITE (*,*) 'Longitude (deg): ', LONG*DPR() */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     B.V. Semenov    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.2.0, 08-JAN-2014 (BVS) */

/*        Added an error check for frames defined relative to */
/*        themselves. */

/*        Increased BUFSIZ from 20 to 200. */

/* -    SPICELIB Version 2.1.0, 23-APR-2009 (NJB) */

/*        Bug fix: watch is deleted only for frames */
/*        that are deleted from the buffer. */

/* -    SPICELIB Version 2.0.0, 19-MAR-2009 (NJB) */

/*        Bug fix: this routine now deletes watches set on */
/*        kernel variables of frames that are discarded from */
/*        the local buffering system. */

/* -    SPICELIB Version 1.2.0, 09-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in CONVRT, UCRSS, VHATG and VSCL calls. */

/* -    SPICELIB Version 1.1.0, 21-NOV-2001 (FST) */

/*        Updated this routine to dump the buffer of frame ID codes */
/*        it saves when it or one of the modules in its call tree signals */
/*        an error.  This fixes a bug where a frame's ID code is */
/*        buffered, but the matrix and kernel pool watcher were not set */
/*        properly. */

/* -    SPICELIB Version 1.0.0, 18-NOV-1996 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Fetch the rotation and frame of a text kernel frame */
/*     Fetch the rotation and frame of a constant offset frame */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0, 09-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in CONVRT, UCRSS, VHATG and VSCL calls. */

/* -& */

/*     Spicelib Functions */


/*     Local Parameters */


/*     Local Variables */


/*     Saved variables */


/*     Initial values */


/*     Programmer's note: this routine makes use of the *implementation* */
/*     of LOCATI. If that routine is changed, the logic this routine */
/*     uses to locate buffered, old frame IDs may need to change as well. */


/*     Before we even check in, if N is less than 1 we can */
/*     just return. */


/*     Perform any initializations that might be needed for this */
/*     routine. */

    if (first) {
	first = FALSE_;
	s_copy(versn, "1.0.0", (ftnlen)8, (ftnlen)5);
	lnkini_(&c__200, pool);
    }

/*     Now do the standard SPICE error handling.  Sure this is */
/*     a bit unconventional, but nothing will be hurt by doing */
/*     the stuff above first. */

    if (return_()) {
	return 0;
    }
    chkin_("TKFRAM", (ftnlen)6);

/*     So far, we've not FOUND the rotation to the specified frame. */

    *found = FALSE_;

/*     Check the ID to make sure it is non-zero. */

    if (*id == 0) {
	lnkini_(&c__200, pool);
	setmsg_("Frame identification codes are required to be non-zero.  Yo"
		"u've specified a frame with ID value zero. ", (ftnlen)102);
	sigerr_("SPICE(ZEROFRAMEID)", (ftnlen)18);
	chkout_("TKFRAM", (ftnlen)6);
	return 0;
    }

/*     Find out whether our linked list pool is already full. */
/*     We'll use this information later to decide whether we're */
/*     going to have to delete a watcher. */

    full = lnknfn_(pool) == 0;
    if (full) {

/*        If the input frame ID is not buffered, we'll need to */
/*        overwrite an existing buffer entry. In this case */
/*        the call to LOCATI we're about to make will overwrite */
/*        the ID code in the slot we're about to use. We need */
/*        this ID code, so extract it now while we have the */
/*        opportunity. The old ID sits at the tail of the list */
/*        whose head node is AT. */

	tail = lnktl_(&at, pool);
	oldid = idents[(i__1 = tail - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		"idents", i__1, "tkfram_", (ftnlen)426)];

/*        Create the name of the agent associated with the old */
/*        frame. */

	s_copy(oldagt, "TKFRAME_#", (ftnlen)32, (ftnlen)9);
	repmi_(oldagt, "#", &oldid, oldagt, (ftnlen)32, (ftnlen)1, (ftnlen)32)
		;
    }

/*     Look up the address of the instance data. */

    idnt[0] = *id;
    locati_(idnt, &c__1, idents, pool, &at, &buffrd);
    if (full && ! buffrd) {

/*        Since the buffer is already full, we'll delete the watcher for */
/*        the kernel variables associated with OLDID, since there's no */
/*        longer a need for that watcher. */

/*        First clear the update status of the old agent; DWPOOL won't */
/*        delete an agent with a unchecked update. */

	cvpool_(oldagt, &update, (ftnlen)32);
	dwpool_(oldagt, (ftnlen)32);
    }

/*     Until we have better information we put the identity matrix */
/*     into the output rotation and set FRAME to zero. */

    ident_(rot);
    *frame = 0;

/*     If we have to look up the data for our frame, we do */
/*     it now and perform any conversions and computations that */
/*     will be needed when it's time to convert coordinates to */
/*     directions. */

/*     Construct the name of the agent associated with the */
/*     requested frame.  (Each frame has its own agent). */

    intstr_(id, idstr, (ftnlen)32);
    frmnam_(id, frname, (ftnlen)32);
    if (s_cmp(frname, " ", (ftnlen)32, (ftnlen)1) == 0) {
	lnkini_(&c__200, pool);
	setmsg_("The Text Kernel (TK) frame with id-code # does not have a r"
		"ecognized name. ", (ftnlen)75);
	errint_("#", id, (ftnlen)1);
	sigerr_("SPICE(INCOMPLETFRAME)", (ftnlen)21);
	chkout_("TKFRAM", (ftnlen)6);
	return 0;
    }
/* Writing concatenation */
    i__2[0] = 8, a__1[0] = "TKFRAME_";
    i__2[1] = 32, a__1[1] = idstr;
    s_cat(agent, a__1, i__2, &c__2, (ftnlen)32);
    r__ = rtrim_(agent, (ftnlen)32);
/* Writing concatenation */
    i__2[0] = 8, a__1[0] = "TKFRAME_";
    i__2[1] = 32, a__1[1] = frname;
    s_cat(altnat, a__1, i__2, &c__2, (ftnlen)32);
    ar = rtrim_(altnat, (ftnlen)32);

/*     If the frame is buffered, we check the kernel pool to */
/*     see if there has been an update to this frame. */

    if (buffrd) {
	cvpool_(agent, &update, r__);
    } else {

/*        If the frame is not buffered we definitely need to update */
/*        things. */
	update = TRUE_;
    }
    if (! update) {

/*        Just look up the rotation matrix and relative-to */
/*        information from the local buffer. */

	rot[0] = buffd[(i__1 = at * 9 - 9) < 1800 && 0 <= i__1 ? i__1 : 
		s_rnge("buffd", i__1, "tkfram_", (ftnlen)519)];
	rot[1] = buffd[(i__1 = at * 9 - 8) < 1800 && 0 <= i__1 ? i__1 : 
		s_rnge("buffd", i__1, "tkfram_", (ftnlen)520)];
	rot[2] = buffd[(i__1 = at * 9 - 7) < 1800 && 0 <= i__1 ? i__1 : 
		s_rnge("buffd", i__1, "tkfram_", (ftnlen)521)];
	rot[3] = buffd[(i__1 = at * 9 - 6) < 1800 && 0 <= i__1 ? i__1 : 
		s_rnge("buffd", i__1, "tkfram_", (ftnlen)522)];
	rot[4] = buffd[(i__1 = at * 9 - 5) < 1800 && 0 <= i__1 ? i__1 : 
		s_rnge("buffd", i__1, "tkfram_", (ftnlen)523)];
	rot[5] = buffd[(i__1 = at * 9 - 4) < 1800 && 0 <= i__1 ? i__1 : 
		s_rnge("buffd", i__1, "tkfram_", (ftnlen)524)];
	rot[6] = buffd[(i__1 = at * 9 - 3) < 1800 && 0 <= i__1 ? i__1 : 
		s_rnge("buffd", i__1, "tkfram_", (ftnlen)525)];
	rot[7] = buffd[(i__1 = at * 9 - 2) < 1800 && 0 <= i__1 ? i__1 : 
		s_rnge("buffd", i__1, "tkfram_", (ftnlen)526)];
	rot[8] = buffd[(i__1 = at * 9 - 1) < 1800 && 0 <= i__1 ? i__1 : 
		s_rnge("buffd", i__1, "tkfram_", (ftnlen)527)];
	*frame = buffi[(i__1 = at - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		"buffi", i__1, "tkfram_", (ftnlen)529)];
    } else {

/*        Determine how the frame is specified and what it */
/*        is relative to.  The variables that specify */
/*        how the frame is represented and what it is relative to */
/*        are TKFRAME_#_SPEC and TKFRAME_#_RELATIVE where # is */
/*        replaced by the text value of ID or the frame name. */

/* Writing concatenation */
	i__2[0] = r__, a__1[0] = agent;
	i__2[1] = 5, a__1[1] = "_SPEC";
	s_cat(item, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	i__2[0] = r__, a__1[0] = agent;
	i__2[1] = 9, a__1[1] = "_RELATIVE";
	s_cat(item + 32, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	i__2[0] = ar, a__1[0] = altnat;
	i__2[1] = 5, a__1[1] = "_SPEC";
	s_cat(alt, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	i__2[0] = ar, a__1[0] = altnat;
	i__2[1] = 9, a__1[1] = "_RELATIVE";
	s_cat(alt + 32, a__1, i__2, &c__2, (ftnlen)32);

/*        See if the friendlier version of the kernel pool variables */
/*        are available. */

	for (i__ = 1; i__ <= 2; ++i__) {
	    dtpool_(alt + (((i__1 = i__ - 1) < 14 && 0 <= i__1 ? i__1 : 
		    s_rnge("alt", i__1, "tkfram_", (ftnlen)550)) << 5), found,
		     &n, type__, (ftnlen)32, (ftnlen)1);
	    if (*found) {
		s_copy(item + (((i__1 = i__ - 1) < 14 && 0 <= i__1 ? i__1 : 
			s_rnge("item", i__1, "tkfram_", (ftnlen)553)) << 5), 
			alt + (((i__3 = i__ - 1) < 14 && 0 <= i__3 ? i__3 : 
			s_rnge("alt", i__3, "tkfram_", (ftnlen)553)) << 5), (
			ftnlen)32, (ftnlen)32);
	    }
	}

/*        If either the SPEC or RELATIVE frame are missing from */
/*        the kernel pool, we simply return. */

	if (badkpv_("TKFRAM", item, "=", &c__1, &c__1, "C", (ftnlen)6, (
		ftnlen)32, (ftnlen)1, (ftnlen)1) || badkpv_("TKFRAM", item + 
		32, "=", &c__1, &c__1, "C", (ftnlen)6, (ftnlen)32, (ftnlen)1, 
		(ftnlen)1)) {
	    lnkini_(&c__200, pool);
	    *frame = 0;
	    ident_(rot);
	    chkout_("TKFRAM", (ftnlen)6);
	    return 0;
	}

/*        If we make it this far, look up the SPEC and RELATIVE frame. */

	gcpool_(item, &c__1, &c__1, &n, spec, &fnd, (ftnlen)32, (ftnlen)32);
	gcpool_(item + 32, &c__1, &c__1, &n, name__, &fnd, (ftnlen)32, (
		ftnlen)32);

/*        Look up the id-code for this frame. */

	namfrm_(name__, frame, (ftnlen)32);
	if (*frame == 0) {
	    lnkini_(&c__200, pool);
	    setmsg_("The frame to which frame # is relatively defined is not"
		    " recognized. The kernel pool specification of the relati"
		    "ve frame is '#'.  This is not a recognized frame. ", (
		    ftnlen)161);
	    errint_("#", id, (ftnlen)1);
	    errch_("#", name__, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(BADFRAMESPEC)", (ftnlen)19);
	    chkout_("TKFRAM", (ftnlen)6);
	    return 0;
	}

/*        Make sure that the RELATIVE frame ID is distinct from the */
/*        frame ID. If they are the same, SPICE will go into an */
/*        indefinite loop. */

	if (*frame == *id) {
	    lnkini_(&c__200, pool);
	    setmsg_("Bad fixed offset frame specification: the frame '#' (fr"
		    "ame ID #) is defined relative to itself. SPICE cannot wo"
		    "rk with such frames. ", (ftnlen)132);
	    errch_("#", name__, (ftnlen)1, (ftnlen)32);
	    errint_("#", id, (ftnlen)1);
	    sigerr_("SPICE(BADFRAMESPEC2)", (ftnlen)20);
	    chkout_("TKFRAM", (ftnlen)6);
	    return 0;
	}

/*        Convert SPEC to upper case so that we can easily check */
/*        to see if this is one of the expected specification types. */

	ucase_(spec, spec, (ftnlen)32, (ftnlen)32);
	if (s_cmp(spec, "MATRIX", (ftnlen)32, (ftnlen)6) == 0) {

/*           This is the easiest case.  Just grab the matrix */
/*           from the kernel pool (and polish it up a bit just */
/*           to make sure we have a rotation matrix). */

/*           We give preference to the kernel pool variable */
/*           TKFRAME_<name>_MATRIX if it is available. */

/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 7, a__1[1] = "_MATRIX";
	    s_cat(item + 64, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 7, a__1[1] = "_MATRIX";
	    s_cat(alt + 64, a__1, i__2, &c__2, (ftnlen)32);
	    dtpool_(alt + 64, found, &n, type__, (ftnlen)32, (ftnlen)1);
	    if (*found) {
		s_copy(item + 64, alt + 64, (ftnlen)32, (ftnlen)32);
	    }
	    if (badkpv_("TKFRAM", item + 64, "=", &c__9, &c__1, "N", (ftnlen)
		    6, (ftnlen)32, (ftnlen)1, (ftnlen)1)) {
		lnkini_(&c__200, pool);
		*frame = 0;
		ident_(rot);
		chkout_("TKFRAM", (ftnlen)6);
		return 0;
	    }

/*           The variable meets current expectations, look it up */
/*           from the kernel pool. */

	    gdpool_(item + 64, &c__1, &c__9, &n, matrix, &fnd, (ftnlen)32);

/*           In this case the full transformation matrix has been */
/*           specified.  We simply polish it up a bit. */

	    moved_(matrix, &c__9, rot);
	    sharpr_(rot);

/*           The matrix might not be right-handed, so correct */
/*           the sense of the second and third columns if necessary. */

	    if (vdot_(&rot[3], &matrix[3]) < 0.) {
		vsclip_(&c_b101, &rot[3]);
	    }
	    if (vdot_(&rot[6], &matrix[6]) < 0.) {
		vsclip_(&c_b101, &rot[6]);
	    }
	} else if (s_cmp(spec, "ANGLES", (ftnlen)32, (ftnlen)6) == 0) {

/*           Look up the angles, their units and axes for the */
/*           frame specified by ID. (Note that UNITS are optional). */
/*           As in the previous case we give preference to the */
/*           form TKFRAME_<name>_<item> over TKFRAME_<id>_<item>. */

/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 7, a__1[1] = "_ANGLES";
	    s_cat(item + 64, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 5, a__1[1] = "_AXES";
	    s_cat(item + 96, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 6, a__1[1] = "_UNITS";
	    s_cat(item + 128, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 7, a__1[1] = "_ANGLES";
	    s_cat(alt + 64, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 5, a__1[1] = "_AXES";
	    s_cat(alt + 96, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 6, a__1[1] = "_UNITS";
	    s_cat(alt + 128, a__1, i__2, &c__2, (ftnlen)32);

/*           Again, we give preference to the more friendly form */
/*           of TKFRAME specification. */

	    for (i__ = 3; i__ <= 5; ++i__) {
		dtpool_(alt + (((i__1 = i__ - 1) < 14 && 0 <= i__1 ? i__1 : 
			s_rnge("alt", i__1, "tkfram_", (ftnlen)703)) << 5), 
			found, &n, type__, (ftnlen)32, (ftnlen)1);
		if (*found) {
		    s_copy(item + (((i__1 = i__ - 1) < 14 && 0 <= i__1 ? i__1 
			    : s_rnge("item", i__1, "tkfram_", (ftnlen)706)) <<
			     5), alt + (((i__3 = i__ - 1) < 14 && 0 <= i__3 ? 
			    i__3 : s_rnge("alt", i__3, "tkfram_", (ftnlen)706)
			    ) << 5), (ftnlen)32, (ftnlen)32);
		}
	    }
	    if (badkpv_("TKFRAM", item + 64, "=", &c__3, &c__1, "N", (ftnlen)
		    6, (ftnlen)32, (ftnlen)1, (ftnlen)1) || badkpv_("TKFRAM", 
		    item + 96, "=", &c__3, &c__1, "N", (ftnlen)6, (ftnlen)32, 
		    (ftnlen)1, (ftnlen)1)) {
		lnkini_(&c__200, pool);
		*frame = 0;
		ident_(rot);
		chkout_("TKFRAM", (ftnlen)6);
		return 0;
	    }
	    s_copy(units, "RADIANS", (ftnlen)32, (ftnlen)7);
	    gdpool_(item + 64, &c__1, &c__3, &n, angles, &fnd, (ftnlen)32);
	    gipool_(item + 96, &c__1, &c__3, &n, axes, &fnd, (ftnlen)32);
	    gcpool_(item + 128, &c__1, &c__1, &n, units, &fnd, (ftnlen)32, (
		    ftnlen)32);

/*           Convert angles to radians. */

	    for (i__ = 1; i__ <= 3; ++i__) {
		convrt_(&angles[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : 
			s_rnge("angles", i__1, "tkfram_", (ftnlen)735)], 
			units, "RADIANS", &tempd, (ftnlen)32, (ftnlen)7);
		angles[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
			"angles", i__1, "tkfram_", (ftnlen)736)] = tempd;
	    }
	    if (failed_()) {
		lnkini_(&c__200, pool);
		*frame = 0;
		ident_(rot);
		chkout_("TKFRAM", (ftnlen)6);
		return 0;
	    }

/*           Compute the rotation from instrument frame to CK frame. */

	    eul2m_(angles, &angles[1], &angles[2], axes, &axes[1], &axes[2], 
		    rot);
	} else if (s_cmp(spec, "QUATERNION", (ftnlen)32, (ftnlen)10) == 0) {

/*           Look up the quaternion and convert it to a rotation */
/*           matrix. Again there are two possible variables that */
/*           may point to the quaternion. We give preference to */
/*           the form TKFRAME_<name>_Q over the form TKFRAME_<id>_Q. */

/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 2, a__1[1] = "_Q";
	    s_cat(item + 64, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 2, a__1[1] = "_Q";
	    s_cat(alt + 64, a__1, i__2, &c__2, (ftnlen)32);
	    dtpool_(alt + 64, found, &n, type__, (ftnlen)32, (ftnlen)1);
	    if (*found) {
		s_copy(item + 64, alt + 64, (ftnlen)32, (ftnlen)32);
	    }
	    if (badkpv_("TKFRAM", item + 64, "=", &c__4, &c__1, "N", (ftnlen)
		    6, (ftnlen)32, (ftnlen)1, (ftnlen)1)) {
		lnkini_(&c__200, pool);
		*frame = 0;
		ident_(rot);
		chkout_("TKFRAM", (ftnlen)6);
		return 0;
	    }

/*           In this case we have the quaternion representation. */
/*           Again, we do a small amount of polishing of the input. */

	    gdpool_(item + 64, &c__1, &c__4, &n, quatrn, &fnd, (ftnlen)32);
	    vhatg_(quatrn, &c__4, qtmp);
	    q2m_(qtmp, rot);
	} else {

/*           We don't recognize the SPEC for this frame.  Say */
/*           so.  Also note that perhaps the user needs to upgrade */
/*           the toolkit. */

	    lnkini_(&c__200, pool);
	    setmsg_("The frame specification \"# = '#'\" is not one of the r"
		    "econized means of specifying a text-kernel constant offs"
		    "et frame (as of version # of the routine TKFRAM). This m"
		    "ay reflect a typographical error or may indicate that yo"
		    "u need to consider updating your version of the SPICE to"
		    "olkit. ", (ftnlen)284);
	    errch_("#", item, (ftnlen)1, (ftnlen)32);
	    errch_("#", spec, (ftnlen)1, (ftnlen)32);
	    errch_("#", versn, (ftnlen)1, (ftnlen)8);
	    sigerr_("SPICE(UNKNOWNFRAMESPEC)", (ftnlen)23);
	    chkout_("TKFRAM", (ftnlen)6);
	    return 0;
	}

/*        Buffer the identifier, relative frame and rotation matrix. */

	buffd[(i__1 = at * 9 - 9) < 1800 && 0 <= i__1 ? i__1 : s_rnge("buffd",
		 i__1, "tkfram_", (ftnlen)819)] = rot[0];
	buffd[(i__1 = at * 9 - 8) < 1800 && 0 <= i__1 ? i__1 : s_rnge("buffd",
		 i__1, "tkfram_", (ftnlen)820)] = rot[1];
	buffd[(i__1 = at * 9 - 7) < 1800 && 0 <= i__1 ? i__1 : s_rnge("buffd",
		 i__1, "tkfram_", (ftnlen)821)] = rot[2];
	buffd[(i__1 = at * 9 - 6) < 1800 && 0 <= i__1 ? i__1 : s_rnge("buffd",
		 i__1, "tkfram_", (ftnlen)822)] = rot[3];
	buffd[(i__1 = at * 9 - 5) < 1800 && 0 <= i__1 ? i__1 : s_rnge("buffd",
		 i__1, "tkfram_", (ftnlen)823)] = rot[4];
	buffd[(i__1 = at * 9 - 4) < 1800 && 0 <= i__1 ? i__1 : s_rnge("buffd",
		 i__1, "tkfram_", (ftnlen)824)] = rot[5];
	buffd[(i__1 = at * 9 - 3) < 1800 && 0 <= i__1 ? i__1 : s_rnge("buffd",
		 i__1, "tkfram_", (ftnlen)825)] = rot[6];
	buffd[(i__1 = at * 9 - 2) < 1800 && 0 <= i__1 ? i__1 : s_rnge("buffd",
		 i__1, "tkfram_", (ftnlen)826)] = rot[7];
	buffd[(i__1 = at * 9 - 1) < 1800 && 0 <= i__1 ? i__1 : s_rnge("buffd",
		 i__1, "tkfram_", (ftnlen)827)] = rot[8];
	buffi[(i__1 = at - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("buffi", 
		i__1, "tkfram_", (ftnlen)829)] = *frame;

/*        If these were not previously buffered, we need to set */
/*        a watch on the various items that might be used to define */
/*        this frame. */

	if (! buffrd) {

/*           Immediately check for an update so that we will */
/*           not redundantly look for this item the next time this */
/*           routine is called. */

/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 9, a__1[1] = "_RELATIVE";
	    s_cat(item, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 5, a__1[1] = "_SPEC";
	    s_cat(item + 32, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 5, a__1[1] = "_AXES";
	    s_cat(item + 64, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 7, a__1[1] = "_MATRIX";
	    s_cat(item + 96, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 2, a__1[1] = "_Q";
	    s_cat(item + 128, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 7, a__1[1] = "_ANGLES";
	    s_cat(item + 160, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = r__, a__1[0] = agent;
	    i__2[1] = 6, a__1[1] = "_UNITS";
	    s_cat(item + 192, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 9, a__1[1] = "_RELATIVE";
	    s_cat(item + 224, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 5, a__1[1] = "_SPEC";
	    s_cat(item + 256, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 5, a__1[1] = "_AXES";
	    s_cat(item + 288, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 7, a__1[1] = "_MATRIX";
	    s_cat(item + 320, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 2, a__1[1] = "_Q";
	    s_cat(item + 352, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 7, a__1[1] = "_ANGLES";
	    s_cat(item + 384, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = ar, a__1[0] = altnat;
	    i__2[1] = 6, a__1[1] = "_UNITS";
	    s_cat(item + 416, a__1, i__2, &c__2, (ftnlen)32);
	    swpool_(agent, &c__14, item, (ftnlen)32, (ftnlen)32);
	    cvpool_(agent, &update, (ftnlen)32);
	}
    }
    if (failed_()) {
	lnkini_(&c__200, pool);
	chkout_("TKFRAM", (ftnlen)6);
	return 0;
    }

/*     All errors cause the routine to exit before we get to this */
/*     point.  If we reach this point we didn't have an error and */
/*     hence did find the rotation from ID to FRAME. */

    *found = TRUE_;

/*     That's it */

    chkout_("TKFRAM", (ftnlen)6);
    return 0;
} /* tkfram_ */

