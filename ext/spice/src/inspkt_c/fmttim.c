/* fmttim.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

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


/*     This routine formats time strings. */

/* -    Version 2.0.0  29-FEB-2000 (WLT) */

/*        Added the call to SCTRAN to handle more SCLKs than just */
/*        MO and GLL. */

/* Subroutine */ int fmttim_(doublereal *et, char *format, char *string, 
	ftnlen format_len, ftnlen string_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen);

    /* Local variables */
    logical sclk;
    extern /* Subroutine */ int sce2s_(integer *, doublereal *, char *, 
	    ftnlen);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen), 
	    scn2id_(char *, integer *, logical *, ftnlen);
    integer id;
    char kervar[80];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    integer outlen;
    extern /* Subroutine */ int expool_(char *, logical *, ftnlen), timout_(
	    doublereal *, char *, char *, ftnlen, ftnlen);
    char tmpstr[132];

    chkin_("FMTTIM", (ftnlen)6);

/*     Is the desired output an SCLK string? If it is, which spacecraft */
/*     are we dealing with? */

    if (eqstr_(format, "MOSCLK", format_len, (ftnlen)6)) {
	id = -94;
	sclk = TRUE_;
    } else if (eqstr_(format, "GLLSCLK", format_len, (ftnlen)7)) {
	id = -77;
	sclk = TRUE_;
    } else {
	scn2id_(format, &id, &sclk, format_len);
    }

/*     If we want an SCLK string, check to see if an SCLK kernel has been */
/*     loaded. If not, signal an error, otherwise compute the SCLK */
/*     string. */

    if (sclk) {
	s_copy(kervar, "SCLK_DATA_TYPE_#", (ftnlen)80, (ftnlen)16);
	i__1 = -id;
	repmi_(kervar, "#", &i__1, kervar, (ftnlen)80, (ftnlen)1, (ftnlen)80);
	expool_(kervar, &found, (ftnlen)80);
	if (! found) {
	    setmsg_("The SCLK kernel file for body # was not loaded. Please "
		    "load it. ", (ftnlen)64);
	    errint_("#", &id, (ftnlen)1);
	    sigerr_("SPICE(KERNELNOTLOADED)", (ftnlen)22);
	    chkout_("FMTTIM", (ftnlen)6);
	    return 0;
	} else {
	    sce2s_(&id, et, tmpstr, (ftnlen)132);
	    ljust_(tmpstr, tmpstr, (ftnlen)132, (ftnlen)132);
	    outlen = i_len(string, string_len);
	    if (rtrim_(tmpstr, (ftnlen)132) > outlen) {
		i__1 = outlen;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    *(unsigned char *)&string[i__ - 1] = '*';
		}
	    } else {
		s_copy(string, tmpstr, string_len, (ftnlen)132);
	    }
	}
    } else {

/*        We have a "normal" time string conversion to compute. Check */
/*        to see if the leapseconds kernel file has been loaded. If not, */
/*        it's an error. */

	expool_("DELTET/DELTA_AT", &found, (ftnlen)15);
	if (! found) {
	    setmsg_("The leapseconds kernel file was not loaded.", (ftnlen)43)
		    ;
	    sigerr_("SPICE(NOKERNELLOADED)", (ftnlen)21);
	    chkout_("FMTTIM", (ftnlen)6);
	    return 0;
	}

/*        For some requested time formats we'll just use a format that */
/*        we know is correct. */

	if (eqstr_(format, "UTC", format_len, (ftnlen)3)) {
	    timout_(et, "YYYY-MON-DD HR:MN:SC ::RND", tmpstr, (ftnlen)26, (
		    ftnlen)132);
	} else if (eqstr_(format, "JED", format_len, (ftnlen)3)) {
	    timout_(et, "JD.##### ::TDB ::RND", tmpstr, (ftnlen)20, (ftnlen)
		    132);
	} else if (eqstr_(format, "ISO", format_len, (ftnlen)3)) {
	    timout_(et, "YYYY-MM-DDTHR:MN:SC ::RND", tmpstr, (ftnlen)25, (
		    ftnlen)132);
	} else if (eqstr_(format, "ISODOY", format_len, (ftnlen)6)) {
	    timout_(et, "YYYY-DOYTHR:MN:SC ::RND", tmpstr, (ftnlen)23, (
		    ftnlen)132);
	} else {
	    timout_(et, format, tmpstr, format_len, (ftnlen)132);
	}
	ljust_(tmpstr, tmpstr, (ftnlen)132, (ftnlen)132);
	outlen = i_len(string, string_len);
	if (rtrim_(tmpstr, (ftnlen)132) > outlen) {
	    i__1 = outlen;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		*(unsigned char *)&string[i__ - 1] = '*';
	    }
	} else {
	    s_copy(string, tmpstr, string_len, (ftnlen)132);
	}
    }
    chkout_("FMTTIM", (ftnlen)6);
    return 0;
} /* fmttim_ */

