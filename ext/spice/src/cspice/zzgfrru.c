/* zzgfrru.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* $Procedure ZZGFRRU ( Private - GF, range rate utility routine ) */
/* Subroutine */ int zzgfrru_0_(int n__, char *target, char *abcorr, char *
	obsrvr, doublereal *refval, doublereal *et, doublereal *dt, logical *
	decres, logical *lssthn, doublereal *rvl, ftnlen target_len, ftnlen 
	abcorr_len, ftnlen obsrvr_len)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    doublereal dfdt[6];
    static doublereal s_dt__;
    doublereal rvel;
    extern doublereal vdot_(doublereal *, doublereal *);
    extern /* Subroutine */ int zzvalcor_(char *, logical *, ftnlen);
    integer n;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char s_ref__[32];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    errch_(char *, char *, ftnlen, ftnlen);
    static integer s_obs__;
    extern /* Subroutine */ int dvhat_(doublereal *, doublereal *);
    logical found;
    doublereal drvel, state[6], srhat[6];
    extern /* Subroutine */ int spkez_(integer *, doublereal *, char *, char *
	    , integer *, doublereal *, doublereal *, ftnlen, ftnlen), bods2c_(
	    char *, integer *, logical *, ftnlen);
    extern logical failed_(void);
    static char s_abco__[5];
    doublereal lt;
    logical attblk[15];
    static integer s_targ__;
    static doublereal s_vref__;
    extern /* Subroutine */ int qderiv_(integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *), sigerr_(char *, ftnlen), chkout_(
	    char *, ftnlen), setmsg_(char *, ftnlen);
    doublereal states[12]	/* was [6][2] */;
    extern /* Subroutine */ int cmprss_(char *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int zzgfrrq_(doublereal *, integer *, integer *, 
	    char *, doublereal *, ftnlen);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This is the umbrella routine for the entry points needed by */
/*     GFEVNT in order to find range rate events. */

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

/*     RANGE RATE */
/*     EPHEMERIS */
/*     GEOMETRY */
/*     SEARCH */

/* $ Declarations */
/* $ Abstract */

/*     Include file zzabcorr.inc */

/*     SPICE private file intended solely for the support of SPICE */
/*     routines.  Users should not include this file directly due */
/*     to the volatile nature of this file */

/*     The parameters below define the structure of an aberration */
/*     correction attribute block. */

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

/* $ Parameters */

/*     An aberration correction attribute block is an array of logical */
/*     flags indicating the attributes of the aberration correction */
/*     specified by an aberration correction string.  The attributes */
/*     are: */

/*        - Is the correction "geometric"? */

/*        - Is light time correction indicated? */

/*        - Is stellar aberration correction indicated? */

/*        - Is the light time correction of the "converged */
/*          Newtonian" variety? */

/*        - Is the correction for the transmission case? */

/*        - Is the correction relativistic? */

/*    The parameters defining the structure of the block are as */
/*    follows: */

/*       NABCOR    Number of aberration correction choices. */

/*       ABATSZ    Number of elements in the aberration correction */
/*                 block. */

/*       GEOIDX    Index in block of geometric correction flag. */

/*       LTIDX     Index of light time flag. */

/*       STLIDX    Index of stellar aberration flag. */

/*       CNVIDX    Index of converged Newtonian flag. */

/*       XMTIDX    Index of transmission flag. */

/*       RELIDX    Index of relativistic flag. */

/*    The following parameter is not required to define the block */
/*    structure, but it is convenient to include it here: */

/*       CORLEN    The maximum string length required by any aberration */
/*                 correction string */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) */

/* -& */
/*     Number of aberration correction choices: */


/*     Aberration correction attribute block size */
/*     (number of aberration correction attributes): */


/*     Indices of attributes within an aberration correction */
/*     attribute block: */


/*     Maximum length of an aberration correction string: */


/*     End of include file zzabcorr.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TARGET     I   Name of the target body */
/*     ABCORR     I   Aberration correction flag */
/*     OBSRVR     I   Name of the observing body */
/*     REFVAL     I   Reference value */
/*     ET         I   Ephemeris seconds past J2000 TDB. */
/*     DT         I   Interval from ET for derivative calculation. */
/*     DECRES     O   .TRUE. if range rate is decreasing, .FALSE. */
/*                    otherwise. */
/*     LSSTHN     O   .TRUE. if range rate is less than REFVAL, */
/*                    .FALSE. otherwise. */
/*     RVL        O   Range rate at time ET. */

/* $ Detailed_Input */

/*     TARGET   the string name of a target body.  Optionally, you may */
/*              supply the integer ID code for the object as an */
/*              integer string.  For example both 'MOON' and '301' */
/*              are legitimate strings that indicate the moon is the */
/*              target body. */

/*              The target and observer define a position vector */
/*              that points from the observer to the target. */

/*     ABCORR   the string description of the aberration corrections to */
/*              apply to the state evaluations to account for one-way */
/*              light time and stellar aberration. */

/*              Any aberration correction accepted by the SPICE */
/*              routine SPKEZR is accepted here. See the header */
/*              of SPKEZR for a detailed description of the */
/*              aberration correction options. For convenience, */
/*              the options are listed below: */

/*                 'NONE'     Apply no correction. Returns the "true" */
/*                            geometric state. */

/*                 'LT'       "Reception" case:  correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'LT+S'     "Reception" case:  correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. */

/*                 'CN'       "Reception" case:  converged */
/*                            Newtonian light time correction. */

/*                 'CN+S'     "Reception" case:  converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*                 'XLT'      "Transmission" case:  correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'XLT+S'    "Transmission" case:  correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. */

/*                 'XCN'      "Transmission" case:  converged */
/*                            Newtonian light time correction. */

/*                 'XCN+S'    "Transmission" case:  converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*              The ABCORR string lacks sensitivity to case, leading */
/*              and trailing blanks. */

/*     OBSRVR   the string name of an observing body.  Optionally, you */
/*              may supply the ID code of the object as an integer */
/*              string. For example, both 'EARTH' and '399' are */
/*              legitimate strings to indicate the observer as Earth. */

/*     REFVAL   the reference range rate (in km/sec) value against */
/*              which to compare the range rate of the oberrver-target */
/*              vector. */

/*     ET       time in TDB seconds past J2000 at which to calculate */
/*              the value of or characteristic of the range rate of */
/*              the observer-target vector. */

/*     DT       a scalar double precision value representing half the */
/*              interval in TDB seconds separating the evaluation */
/*              epochs; the evaluations occur at epochs */
/*              (ET + DT) and (ET - DT). */

/*              DT may be negative but must be non-zero. */

/*     For more information, see individual entry points. */

/* $ Detailed_Output */

/*     LSSTHN   is .TRUE. if the range rate between the two bodies is */
/*              less than the reference range rate value REFVAL at */
/*              time ET. Otherwise it is .FALSE.. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine serves as the umbrella routine for 4 entry points */
/*     needed by GFEVNT in solving for range rate conditions. */

/*     The 4 entry points are */

/*        ZZGFRRIN --- an initialization routine that must be called */
/*                     prior to attempting to solve for any range */
/*                     rate event. */

/*        ZZGFRRUR --- updates reference value, REFVAL. */

/*        ZZGFRRDC --- determines whether or not range rate is */
/*                     decreasing at some time. */

/*        ZZGFRRGQ --- returns the range rate of the two objects */
/*                     of concern as a function of ET. */

/*        ZZGFRRLT --- determines whether or not range rate is */
/*                     less than REFVAL */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     ZZGFRRIN must be called prior to use of any of the other */
/*     entry points (think constructor). */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */
/*     L.S. Elson     (JPL) */

/* $ Version */

/* -    SPICELIB version 1.0.0 09-OCT-2009 (LSE)(EDW) */

/* -& */
/* $ Index_Entries */

/*     find range rate events */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Saved Variables */

    switch(n__) {
	case 1: goto L_zzgfrrin;
	case 2: goto L_zzgfrrur;
	case 3: goto L_zzgfrrdc;
	case 4: goto L_zzgfrrgq;
	case 5: goto L_zzgfrrlt;
	}

    return 0;
/* $Procedure ZZGFRRIN ( Private - GF, range rate initialization routine ) */

L_zzgfrrin:
/* $ Abstract */

/*     This is the initialization entry point used for describing */
/*     the event that is to be solved for by ZZGFSOLV. */

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

/*     RANGE RATE */
/*     EPHEMERIS */
/*     GEOMETRY */
/*     SEARCH */

/* $ Declarations */

/*      CHARACTER*(*)         TARGET */
/*      CHARACTER*(*)         ABCORR */
/*      CHARACTER*(*)         OBSRVR */
/*      DOUBLE PRECISION      REFVAL */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TARGET     I   Name of the target body */
/*     ABCORR     I   Aberration correction flag */
/*     OBSRVR     I   Name of the observing body */
/*     REFVAL     I   Reference value */
/*     DT         I   Interval from ET for derivative calculation. */

/* $ Detailed_Input */

/*     TARGET   the string name of a target body.  Optionally, you may */
/*              supply the integer ID code for the object as an */
/*              integer string.  For example both 'MOON' and '301' */
/*              are legitimate strings that indicate the moon is the */
/*              target body. */

/*              The target and observer define a position vector */
/*              that points from the observer to the target. */

/*     ABCORR   the string description of the aberration corrections to */
/*              apply to the state evaluations to account for one-way */
/*              light time and stellar aberration. */

/*              Any aberration correction accepted by the SPICE */
/*              routine SPKEZR is accepted here. See the header */
/*              of SPKEZR for a detailed description of the */
/*              aberration correction options. For convenience, */
/*              the options are listed below: */

/*                 'NONE'     Apply no correction. Returns the "true" */
/*                            geometric state. */

/*                 'LT'       "Reception" case:  correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'LT+S'     "Reception" case:  correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. */

/*                 'CN'       "Reception" case:  converged */
/*                            Newtonian light time correction. */

/*                'CN+S'     "Reception" case:  converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*                 'XLT'      "Transmission" case:  correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'XLT+S'    "Transmission" case:  correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. */

/*                 'XCN'      "Transmission" case:  converged */
/*                            Newtonian light time correction. */

/*                 'XCN+S'    "Transmission" case:  converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*              The ABCORR string lacks sensitivity to case, leading */
/*              and trailing blanks. */

/*     OBSRVR   the string name of an observing body.  Optionally, you */
/*              may supply the ID code of the object as an integer */
/*              string. For example, both 'EARTH' and '399' are */
/*              legitimate strings to indicate the observer as Earth. */

/*     REFVAL   the reference range rate (in km/sec) value against */
/*              which to compare the range rate of the oberrver-target */
/*              vector. */

/*     DT         a scalar double precision value representing half the */
/*                interval in TDB seconds separating the evaluation */
/*                epochs; the evaluations occur at epochs */
/*                (ET + DT) and (ET - DT). */

/*                DT may be negative but must be non-zero. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */
/*     L.S. Elson     (JPL) */

/* $ Version */

/* -    SPICELIB version 1.0.0 09-OCT-2009 (LSE)(EDW) */

/* -& */
/* $ Index_Entries */

/*     range rate initialization routine. */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("ZZGFRRIN", (ftnlen)8);

/*     Find NAIF IDs for TARGET and OBSRVR. */

    bods2c_(target, &s_targ__, &found, target_len);
    if (! found) {
	setmsg_("The target object, '#', is not a recognized name for an eph"
		"emeris object. The cause of this problem may be that you nee"
		"d an updated version of the SPICE Toolkit. ", (ftnlen)162);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFRRIN", (ftnlen)8);
	return 0;
    }
    bods2c_(obsrvr, &s_obs__, &found, obsrvr_len);
    if (! found) {
	setmsg_("The observer, '#', is not a recognized name for an ephemeri"
		"s object. The cause of this problem may be that you need an "
		"updated version of the SPICE toolkit. ", (ftnlen)157);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFRRIN", (ftnlen)8);
	return 0;
    }

/*     Make sure the observer and target are distinct. */

    if (s_targ__ == s_obs__) {
	setmsg_("The observer and target must be distinct objects, but are n"
		"ot: OBSRVR = #; TARGET = #.", (ftnlen)86);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(BODIESNOTDISTINCT)", (ftnlen)24);
	chkout_("ZZGFRRIN", (ftnlen)8);
	return 0;
    }

/*     Squeeze all blanks out of the aberration correction */
/*     string; ensure the string is in upper case. */

    cmprss_(" ", &c__0, abcorr, s_abco__, (ftnlen)1, abcorr_len, (ftnlen)5);
    ucase_(s_abco__, s_abco__, (ftnlen)5, (ftnlen)5);

/*     Check the aberration correction. If SPKEZR can't handle it, */
/*     neither can we. */

    zzvalcor_(s_abco__, attblk, (ftnlen)5);
    if (failed_()) {
	chkout_("ZZGFRRIN", (ftnlen)8);
	return 0;
    }

/*     Save the reference value. */

    s_vref__ = *refval;
    s_copy(s_ref__, "J2000", (ftnlen)32, (ftnlen)5);
    s_dt__ = *dt;
    chkout_("ZZGFRRIN", (ftnlen)8);
    return 0;
/* $Procedure ZZGFRRUR ( Private - GF, range rate update reference value ) */

L_zzgfrrur:
/* $ Abstract */

/*     This is the entry point used for updating the reference */
/*     value. */

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

/*     RANGE RATE */
/*     EPHEMERIS */
/*     GEOMETRY */
/*     SEARCH */

/* $ Declarations */

/*      DOUBLE PRECISION      REFVAL */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     REFVAL     I   Reference value */

/* $ Detailed_Input */

/*     REFVAL     the reference range rate (in km/sec) value against */
/*                which to compare the range rate of the oberrver-target */
/*                vector. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */
/*     L.S. Elson     (JPL) */

/* $ Version */

/* -    SPICELIB version 1.0.0 09-JUN-2009 (LSE)(EDW) */

/* -& */
/* $ Index_Entries */

/*     range rate update reference value. */

/* -& */
    s_vref__ = *refval;
    return 0;
/* $Procedure ZZGFRRDC (  Private - GF, when range rate is decreasing ) */

L_zzgfrrdc:
/* $ Abstract */

/*     Computes whether or not the range rate between the observer */
/*     and the target is decreasing at time ET. */

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

/*     RANGE RATE */
/*     EPHEMERIS */
/*     GEOMETRY */
/*     SEARCH */

/* $ Declarations */

/*     DOUBLE PRECISION      ET */
/*     LOGICAL               DECRES */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Ephemeris seconds past J2000 TDB. */
/*     DECRES     O   .TRUE. if range rate is decreasing, .FALSE. */
/*                    otherwise. */

/* $ Detailed_Input */

/*     ET         time in seconds past J2000 at which to calculate */
/*                whether the range rate of the observer-target vector */
/*                is decreasing. */

/* $ Detailed_Output */

/*     DECRES     is .TRUE. if the range rate between the objects */
/*                is decreasing.  Otherwise it is .FALSE. */


/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */
/*     L.S. Elson     (JPL) */

/* $ Version */

/* -    SPICELIB version 1.0.0 09-OCT-2009 (LSE)(EDW) */

/* -& */
/* $ Index_Entries */

/*     when range rate is decreasing */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZGFRRDC", (ftnlen)8);
    }
    n = 6;

/*     The range rate of interest is of S_TARG relative to the S_OBS. */
/*     The function requires the acceleration of S_TARG relative */
/*     to S_OBS. */

    d__1 = *et - s_dt__;
    spkez_(&s_targ__, &d__1, s_ref__, s_abco__, &s_obs__, states, &lt, (
	    ftnlen)32, (ftnlen)5);
    d__1 = *et + s_dt__;
    spkez_(&s_targ__, &d__1, s_ref__, s_abco__, &s_obs__, &states[6], &lt, (
	    ftnlen)32, (ftnlen)5);

/*     Approximate the derivative of the position and valocity by */
/*     finding the derivative of a quadratic approximating function. */

/*        DFDT(1) = Vx */
/*        DFDT(2) = Vy */
/*        DFDT(3) = Vz */
/*        DFDT(4) = Ax */
/*        DFDT(5) = Ay */
/*        DFDT(6) = Az */

    qderiv_(&n, states, &states[6], &s_dt__, dfdt);
    spkez_(&s_targ__, et, s_ref__, s_abco__, &s_obs__, state, &lt, (ftnlen)32,
	     (ftnlen)5);
    if (failed_()) {
	chkout_("ZZGFRRDC", (ftnlen)8);
	return 0;
    }

/*        d ||r||     ^ */
/*        ------- = < r, v > */
/*        dt */

/*         2            ^          ^ */
/*        d ||r||   < d r, v > + < r, d v > */
/*        ------- =   ---             --- */
/*          2 */
/*        dt          dt              dt */

    dvhat_(state, srhat);
    drvel = vdot_(&dfdt[3], srhat) + vdot_(&state[3], &srhat[3]);
    *decres = drvel < 0.;
    chkout_("ZZGFRRDC", (ftnlen)8);
    return 0;
/* $Procedure ZZGFRRGQ ( Private - GF, get range rate between two bodies ) */

L_zzgfrrgq:
/* $ Abstract */

/*     Determine the range rate between the centers of the two */
/*     bodies. */

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

/*     RANGE RATE */
/*     EPHEMERIS */
/*     GEOMETRY */
/*     SEARCH */

/* $ Declarations */

/*      DOUBLE PRECISION      ET */
/*      DOUBLE PRECISION      RVL */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Ephemeris seconds past J2000 TDB. */
/*     RVL        O   Range rate at time ET. */

/* $ Detailed_Input */

/*     ET         time in ephemeris seconds past J2000 when the range */
/*                rate between the two bodies is to be computed. */

/* $ Detailed_Output */

/*     RVL        is the range rate of S_TARG as seen from S_OBS at */
/*                time ET. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */
/*     L.S. Elson     (JPL) */

/* $ Version */

/* -    SPICELIB version 1.0.0 09-JUN-2009 (LSE)(EDW) */

/* -& */
/* $ Index_Entries */

/*     get range rate between two bodies */

/* -& */
    zzgfrrq_(et, &s_targ__, &s_obs__, s_abco__, rvl, (ftnlen)5);
    return 0;
/* $Procedure ZZGFRRLT ( Private - GF, range rate < reference ) */

L_zzgfrrlt:
/* $ Abstract */

/*     Determine whether or not the range rate between the two */
/*     bodies is less than the reference value. */

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

/*     RANGE RATE */
/*     EPHEMERIS */
/*     GEOMETRY */
/*     SEARCH */

/* $ Declarations */

/*      DOUBLE PRECISION      ET */
/*      LOGICAL               LSSTHN */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Ephemeris seconds past J2000 TDB. */
/*     LSSTHN     O   .TRUE. if the range rate is less than */
/*                    REFVAL, .FALSE. otherwise. */

/* $ Detailed_Input */

/*     ET       is the time in second past J2000 at which one wants */
/*              to determine if the range rate between the */
/*              two bodies is less than the reference value. */

/* $ Detailed_Output */

/*     LSSTHN   is .TRUE. if the range rate between the two bodies is */
/*              less than the reference range rate value S_VREF at */
/*              time ET. Otherwise it is .FALSE.. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */
/*     L.S. Elson     (JPL) */

/* $ Version */

/* -    SPICELIB version 1.0.0 09-JUN-2009 (LSE)(EDW) */

/* -& */
/* $ Index_Entries */

/*     range rate less than a value */

/* -& */
    zzgfrrq_(et, &s_targ__, &s_obs__, s_abco__, &rvel, (ftnlen)5);
    if (rvel < s_vref__) {
	*lssthn = TRUE_;
    } else {
	*lssthn = FALSE_;
    }
    return 0;
} /* zzgfrru_ */

/* Subroutine */ int zzgfrru_(char *target, char *abcorr, char *obsrvr, 
	doublereal *refval, doublereal *et, doublereal *dt, logical *decres, 
	logical *lssthn, doublereal *rvl, ftnlen target_len, ftnlen 
	abcorr_len, ftnlen obsrvr_len)
{
    return zzgfrru_0_(0, target, abcorr, obsrvr, refval, et, dt, decres, 
	    lssthn, rvl, target_len, abcorr_len, obsrvr_len);
    }

/* Subroutine */ int zzgfrrin_(char *target, char *abcorr, char *obsrvr, 
	doublereal *refval, doublereal *dt, ftnlen target_len, ftnlen 
	abcorr_len, ftnlen obsrvr_len)
{
    return zzgfrru_0_(1, target, abcorr, obsrvr, refval, (doublereal *)0, dt, 
	    (logical *)0, (logical *)0, (doublereal *)0, target_len, 
	    abcorr_len, obsrvr_len);
    }

/* Subroutine */ int zzgfrrur_(doublereal *refval)
{
    return zzgfrru_0_(2, (char *)0, (char *)0, (char *)0, refval, (doublereal 
	    *)0, (doublereal *)0, (logical *)0, (logical *)0, (doublereal *)0,
	     (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzgfrrdc_(doublereal *et, logical *decres)
{
    return zzgfrru_0_(3, (char *)0, (char *)0, (char *)0, (doublereal *)0, et,
	     (doublereal *)0, decres, (logical *)0, (doublereal *)0, (ftnint)
	    0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzgfrrgq_(doublereal *et, doublereal *rvl)
{
    return zzgfrru_0_(4, (char *)0, (char *)0, (char *)0, (doublereal *)0, et,
	     (doublereal *)0, (logical *)0, (logical *)0, rvl, (ftnint)0, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzgfrrlt_(doublereal *et, logical *lssthn)
{
    return zzgfrru_0_(5, (char *)0, (char *)0, (char *)0, (doublereal *)0, et,
	     (doublereal *)0, (logical *)0, lssthn, (doublereal *)0, (ftnint)
	    0, (ftnint)0, (ftnint)0);
    }

