/* nspcht.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;

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

/* Subroutine */ int nspcht_(char *format, integer *width, ftnlen format_len)
{
    /* System generated locals */
    address a__1[2];
    integer i__1, i__2[2], i__3;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    logical sclk;
    doublereal ncmp;
    integer i__, n;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), dpfmt_(doublereal *, char *, char *, ftnlen, 
	    ftnlen);
    logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    char mystr[128];
    extern /* Subroutine */ int scn2id_(char *, integer *, logical *, ftnlen);
    integer id;
    doublereal et;
    char getfld[32], getmod[32];
    doublereal moduli[10];
    char kervar[32];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), expool_(char *, logical *, 
	    ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int rtpool_(char *, integer *, doublereal *, 
	    logical *, ftnlen), timout_(doublereal *, char *, char *, ftnlen, 
	    ftnlen);


/* -    Version 2.2.0  29-FEB-2000 (WLT) */

/*        Added the call to SCTRAN to handle more SCLKs than just */
/*        MO and GLL. */

/* -    Version 2.1.0  22-MAY-1997 (WLT) */

/*        Balanced calls to CHKIN/CHKOUT. */

/* -    Version 2.0.0  22-MAY-1997 (WLT) */

/*        Needed to fix the damage caused by replacing DPFMT_1 by */
/*        DPFMT.  The format picture used in the creation of the */
/*        width for SCLKS had to be made large enough to hold */
/*        the modulus of a field. */

/* -    Version 1.1.0  09-JAN-1997 (WLT) */

/*       Replaced call to DPFMT_1 with call to DPFMT. */

/* -    Version 1.0.0  21-AUG-1995 (WLT) */

/*        Fixed the text in an error message. */

/*     This routine accepts a strint that is a potential time format. */
/*     Checks it and if it is passable, returns the width that */
/*     would be associated with that format. */


/*     Spicelib functions */


/*     Local Variables */


/*     Is the desired output an SCLK string? If it is, which spacecraft */
/*     are we dealing with? */

    if (return_()) {
	return 0;
    }
    chkin_("NSPCHT", (ftnlen)6);
    if (eqstr_(format, "MOSCLK", format_len, (ftnlen)6)) {
	id = -94;
	sclk = TRUE_;
    } else if (eqstr_(format, "GLLSCLK", format_len, (ftnlen)7)) {
	id = -77;
	sclk = TRUE_;
    } else {
	scn2id_(format, &id, &sclk, format_len);
    }

/*     Set up an empty string for use in determining the lenght */
/*     associated with the input format. */

    s_copy(mystr, " ", (ftnlen)128, (ftnlen)1);

/*     If we want an SCLK string, check to see if an SCLK kernel has been */
/*     loaded. If not, signal an error, otherwise compute the SCLK */
/*     string. */

    if (sclk) {
	s_copy(kervar, "SCLK_DATA_TYPE_#", (ftnlen)32, (ftnlen)16);
	i__1 = -id;
	repmi_(kervar, "#", &i__1, kervar, (ftnlen)32, (ftnlen)1, (ftnlen)32);
	expool_(kervar, &found, (ftnlen)32);
	if (! found) {
	    setmsg_("An SCLK kernel file for # was not loaded. You will need"
		    " to load one before this time format can be used.", (
		    ftnlen)104);
	    errch_("#", format, (ftnlen)1, rtrim_(format, format_len));
	    sigerr_("SPICE(KERNELNOTLOADED)", (ftnlen)22);
	    chkout_("NSPCHT", (ftnlen)6);
	    return 0;
	} else {
/* Writing concatenation */
	    i__2[0] = 16, a__1[0] = "SCLK01_N_FIELDS_";
	    i__2[1] = 17, a__1[1] = kervar + 15;
	    s_cat(getfld, a__1, i__2, &c__2, (ftnlen)32);
/* Writing concatenation */
	    i__2[0] = 14, a__1[0] = "SCLK01_MODULI_";
	    i__2[1] = 17, a__1[1] = kervar + 15;
	    s_cat(getmod, a__1, i__2, &c__2, (ftnlen)32);
	    rtpool_(getfld, &n, &ncmp, &found, (ftnlen)32);
	    rtpool_(getmod, &n, moduli, &found, (ftnlen)32);

/*           The format of an SCLK string has the form */

/*           pn/ xxxxxx#xxxxx# ... #xxxxx */

/*           where pn is the partition number and the x's are the */
/*           integer components of each field.  The '#' character */
/*           is used to separate fields and may be a period, colon */
/*           and so on but it is one character wide. Thus there */
/*           are 4 (for the partition and following space) plus NCMP - 1 */
/*           markers plus the widths of the individual fields. */

	    *width = (integer) ncmp + 3;
	    i__1 = n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		dpfmt_(&moduli[(i__3 = i__ - 1) < 10 && 0 <= i__3 ? i__3 : 
			s_rnge("moduli", i__3, "nspcht_", (ftnlen)169)], 
			"XXXXXXXXXXXX", mystr, (ftnlen)12, (ftnlen)128);
		ljust_(mystr, mystr, (ftnlen)128, (ftnlen)128);
		*width += rtrim_(mystr, (ftnlen)128);
		s_copy(mystr, " ", (ftnlen)128, (ftnlen)1);
	    }
	    chkout_("NSPCHT", (ftnlen)6);
	    return 0;
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
	    chkout_("NSPCHT", (ftnlen)6);
	    return 0;
	}
	et = 1e-8;

/*        For some requested time formats we'll just use a format that */
/*        we know is correct. */

	if (eqstr_(format, "UTC", format_len, (ftnlen)3)) {
	    timout_(&et, "YYYY-MON-DD HR:MN:SC ::RND", mystr, (ftnlen)26, (
		    ftnlen)128);
	} else if (eqstr_(format, "JED", format_len, (ftnlen)3)) {
	    timout_(&et, "JD.##### ::TDB ::RND", mystr, (ftnlen)20, (ftnlen)
		    128);
	} else if (eqstr_(format, "ISO", format_len, (ftnlen)3)) {
	    timout_(&et, "YYYY-MM-DDTHR:MN:SC ::RND", mystr, (ftnlen)25, (
		    ftnlen)128);
	} else if (eqstr_(format, "ISODOY", format_len, (ftnlen)6)) {
	    timout_(&et, "YYYY-DOYTHR:MN:SC ::RND", mystr, (ftnlen)23, (
		    ftnlen)128);
	} else {
	    timout_(&et, format, mystr, format_len, (ftnlen)128);
	}
	ljust_(mystr, mystr, (ftnlen)128, (ftnlen)128);
	*width = rtrim_(mystr, (ftnlen)128);
    }
    chkout_("NSPCHT", (ftnlen)6);
    return 0;
} /* nspcht_ */

