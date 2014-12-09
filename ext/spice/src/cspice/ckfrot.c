/* ckfrot.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure      CKFROT ( C-kernel, find rotation ) */
/* Subroutine */ int ckfrot_(integer *inst, doublereal *et, doublereal *
	rotate, integer *ref, logical *found)
{
    logical have, pfnd, sfnd;
    doublereal time;
    extern /* Subroutine */ int sce2c_(integer *, doublereal *, doublereal *);
    char segid[40];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal descr[5];
    extern /* Subroutine */ int dafus_(doublereal *, integer *, integer *, 
	    doublereal *, integer *), ckbss_(integer *, doublereal *, 
	    doublereal *, logical *), ckpfs_(integer *, doublereal *, 
	    doublereal *, doublereal *, logical *, doublereal *, doublereal *,
	     doublereal *, logical *), cksns_(integer *, doublereal *, char *,
	     logical *, ftnlen), xpose_(doublereal *, doublereal *);
    extern logical failed_(void);
    doublereal av[3];
    integer handle;
    extern /* Subroutine */ int ckhave_(logical *);
    logical needav;
    extern /* Subroutine */ int ckmeta_(integer *, char *, integer *, ftnlen);
    integer sclkid;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    doublereal clkout;
    extern logical return_(void), zzsclk_(integer *, integer *);
    doublereal dcd[2];
    integer icd[6];
    doublereal tol, rot[9]	/* was [3][3] */;

/* $ Abstract */

/*     Find the rotation from a C-kernel Id to the native */
/*     frame at the time requested. */

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

/*     CK */

/* $ Keywords */

/*     POINTING */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     INST       I   NAIF instrument ID. */
/*     ET         I   Epoch measured in seconds past J2000. */
/*     ROTATE     O   rotation from CK platform to frame REF. */
/*     REF        O   Reference frame. */
/*     FOUND      O   True when requested pointing is available. */

/* $ Detailed_Input */

/*     INST       is the unique NAIF integer ID for the spacecraft */
/*                instrument for which data is being requested. */

/*     ET         is the epoch for which the state rotation */
/*                is desired. ET should be given in seconds past the */
/*                epoch of J2000. */


/* $ Detailed_Output */

/*     ROTATE     is a rotation matrix that converts */
/*                positions relative to the input frame (given by INST) */
/*                to positions relative to the frame REF. */

/*                Thus, if a state S has components x,y,z,dx,dy,dz */
/*                in the frame of INST, frame, then S has components */
/*                x', y', z', dx', dy', dz' in frame REF. */

/*                     [  x' ]     [           ] [  x ] */
/*                     |  y' |  =  |   ROTATE  | |  y | */
/*                     [  z' ]     [           ] [  z ] */


/*     REF        is the id-code reference frame to which ROTATE will */
/*                transform states. */

/*     FOUND      is true if a record was found to satisfy the pointing */
/*                request.  FOUND will be false otherwise. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If a C-kernel file is not loaded using CKLPF prior to calling */
/*         this routine, an error is signalled by a routine that this */
/*         routine calls. */


/* $ Files */

/*     CKFROT searches through files loaded by CKLPF to locate a segment */
/*     that can satisfy the request for position rotation */
/*     for instrument INST at time ET.  You must load a C-kernel */
/*     file using CKLPF before calling this routine. */

/* $ Particulars */

/*     CKFROT searches through files loaded by CKLPF to satisfy a */
/*     pointing request. Last-loaded files are searched first, and */
/*     individual files are searched in backwards order, giving */
/*     priority to segments that were added to a file later than the */
/*     others. CKFROT considers only those segments that contain */
/*     angular velocity data. */

/*     The search ends when a segment is found that can give pointing */
/*     for the specified instrument at the request time. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     A C-kernel file should have been loaded by CKLPF. */

/*     In addition it is helpful to load a CK-info file into the */
/*     Kernel pool.  This file should have the following variables */
/*     defined. */

/*       CK_<INST>_SCLK = SCLK idcode that yields SCLK mapping for INST. */
/*       CK_<INST>_SPK  = SPK idcode  that yields ephemeris for INST. */

/*     where <INST> is the integer string corresponding to INST. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 17-FEB-2000 (WLT) */

/*        The routine now checks to make sure convert ET to TICKS */
/*        and that at least one C-kernel is loaded before trying */
/*        to look up the transformation.  Also the routine now calls */
/*        SCE2C instead of SCE2T. */

/* -    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT) */

/* -& */
/* $ Index_Entries */

/*     get instrument frame rotation and reference frame */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*        NDC        is the number of double precision components in an */
/*                   unpacked C-kernel segment descriptor. */

/*        NIC        is the number of integer components in an unpacked */
/*                   C-kernel segment descriptor. */

/*        NC         is the number of components in a packed C-kernel */
/*                   descriptor.  All DAF summaries have this formulaic */
/*                   relationship between the number of its integer and */
/*                   double precision components and the number of packed */
/*                   components. */

/*        IDLEN      is the length of the C-kernel segment identifier. */
/*                   All DAF names have this formulaic relationship */
/*                   between the number of summary components and */
/*                   the length of the name (You will notice that */
/*                   a name and a summary have the same length in bytes.) */


/*     Local variables */


/*     Set FOUND to FALSE right now in case we end up */
/*     returning before doing any work. */

    *found = FALSE_;
    *ref = 0;

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKFROT", (ftnlen)6);
    }

/*     We don't need angular velocity data. */
/*     Assume the segment won't be found until it really is. */

    needav = FALSE_;
    tol = 0.;

/*     Begin a search for this instrument and time, and get the first */
/*     applicable segment. */

    ckhave_(&have);
    ckmeta_(inst, "SCLK", &sclkid, (ftnlen)4);
    if (! have) {
	chkout_("CKFROT", (ftnlen)6);
	return 0;
    } else if (! zzsclk_(inst, &sclkid)) {
	chkout_("CKFROT", (ftnlen)6);
	return 0;
    }
    sce2c_(&sclkid, et, &time);
    ckbss_(inst, &time, &tol, &needav);
    cksns_(&handle, descr, segid, &sfnd, (ftnlen)40);

/*     Keep trying candidate segments until a segment can produce a */
/*     pointing instance within the specified time tolerance of the */
/*     input time. */

/*     Check FAILED to prevent an infinite loop if an error is detected */
/*     by a SPICELIB routine and the error handling is not set to abort. */

    while(sfnd && ! failed_()) {
	ckpfs_(&handle, descr, &time, &tol, &needav, rot, av, &clkout, &pfnd);
	if (pfnd) {

/*           Found one. Fetch the ID code of the reference frame */
/*           from the descriptor. */

	    dafus_(descr, &c__2, &c__6, dcd, icd);
	    *ref = icd[1];
	    *found = TRUE_;

/*           We now have the rotation matrix from */
/*           REF to INS. We invert ROT to get the rotation */
/*           from INST to REF. */

	    xpose_(rot, rotate);
	    chkout_("CKFROT", (ftnlen)6);
	    return 0;
	}
	cksns_(&handle, descr, segid, &sfnd, (ftnlen)40);
    }
    chkout_("CKFROT", (ftnlen)6);
    return 0;
} /* ckfrot_ */

