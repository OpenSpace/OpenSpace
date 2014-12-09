/* distim.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DISTIM ( Format Time for Displaying by BRIEF ) */
/* Subroutine */ int distim_(char *timtyp, doublereal *et, char *timlbl, char 
	*timstr, ftnlen timtyp_len, ftnlen timlbl_len, ftnlen timstr_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int etcal_(doublereal *, char *, ftnlen), chkin_(
	    char *, ftnlen), errch_(char *, char *, ftnlen, ftnlen), dpfmt_(
	    doublereal *, char *, char *, ftnlen, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int timout_(doublereal *, char *, char *, ftnlen, 
	    ftnlen);

/* $ Abstract */

/*     Format time for displaying by BRIEF. */

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

/*     KERNEL */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TIMTYP     I   Desired output format */
/*     ET         I   ET to be formatted */
/*     TIMLBL     O   Label for BRIEF table heading. */
/*     TIMSTR     O   Output time string. */

/* $ Detailed_Input */

/*     TIMTYP     is the desired output format type: ETCAL, UTCCAL, */
/*                UTCDOY, or ETSEC. */

/*     ET         is the input ET seconds past J2000 to be formatted. */

/* $ Detailed_Output */

/*     TIMLBL     is the label for BRIEF table heading. */

/*     TIMSTR     is the output time string. */

/* $ Parameters */

/*     The output format pictures for TIMOUT and DPFMT are provided */
/*     using parameters UCLPIC, UDYPIC, and ESCPIC. */

/* $ Exceptions */

/*     1) If the desired output time type is not recognized, then the */
/*        error SPICE(BADTIMEFORMAT) is signaled. */

/*     2) If required LSK data are not loaded an error will be signaled */
/*        by routines in the calling tree of this routine. */

/* $ Files */

/*     An LSK file must be loaded prior to calling this routine. */

/* $ Particulars */

/*     The following label and time string will be returned for each */
/*     of the allowed time formats: */

/*        ETCAL: */

/*           TIMLBL = 'ET' */
/*           TIMSTR returned by ETCAL */

/*        UTCCAL: */

/*           TIMLBL = 'UTC' */
/*           TIMSTR returned by TIMOUT in */
/*           'YYYY-MON-DD HR:MN:SC.###' format */

/*        UTCDOY: */

/*           TIMLBL = 'UTC' */
/*           TIMSTR returned by TIMOUT in */
/*           'YYYY-DOY // HR:MN:SC.###' format */

/*        ETSEC: */

/*           TIMLBL = 'ET' */
/*           TIMSTR returned by DPFMT in */
/*           'xxxxxxxxxxxxxxxxx.xxxxxx' format */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     This routine must not be called by any routines except BRIEF's */
/*     DISPLY routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    BRIEF Version 1.0.0, 22-OCT-2007 (BVS) */

/* -& */
/* $ Index_Entries */

/*     format time for display by BRIEF */

/* -& */

/*     SPICELIB functions */


/*     Local parameters. */


/*     Output format pictures. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DISTIM", (ftnlen)6);
    }

/*     Set outputs. */

    if (s_cmp(timtyp, "ETCAL", timtyp_len, (ftnlen)5) == 0) {
	s_copy(timlbl, "ET", timlbl_len, (ftnlen)2);
	etcal_(et, timstr, timstr_len);
    } else if (s_cmp(timtyp, "UTCCAL", timtyp_len, (ftnlen)6) == 0) {
	s_copy(timlbl, "UTC", timlbl_len, (ftnlen)3);
	timout_(et, "YYYY-MON-DD HR:MN:SC.###", timstr, (ftnlen)24, 
		timstr_len);
    } else if (s_cmp(timtyp, "UTCDOY", timtyp_len, (ftnlen)6) == 0) {
	s_copy(timlbl, "UTC", timlbl_len, (ftnlen)3);
	timout_(et, "YYYY-DOY // HR:MN:SC.###", timstr, (ftnlen)24, 
		timstr_len);
    } else if (s_cmp(timtyp, "ETSEC", timtyp_len, (ftnlen)5) == 0) {
	s_copy(timlbl, "ET", timlbl_len, (ftnlen)2);
	dpfmt_(et, "xxxxxxxxxxxxxxxxx.xxxxxx", timstr, (ftnlen)24, timstr_len)
		;
    } else {
	setmsg_("Time type '#' is not recognized.", (ftnlen)32);
	errch_("#", timtyp, (ftnlen)1, timtyp_len);
	sigerr_("SPICE(BADTIMEFORMAT)", (ftnlen)20);
	chkout_("DISTIM", (ftnlen)6);
	return 0;
    }

/*     All done. */

    chkout_("DISTIM", (ftnlen)6);
    return 0;
} /* distim_ */

