/* spab2t.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;

/* $Procedure      SPAB2T ( SPACIT, convert binary to transfer ) */
/* Subroutine */ int spab2t_(logical *logfil, integer *loglun)
{
    /* Initialized data */

    static char messag[255] = "Converting from binary file to transfer file."
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                  ";

    /* System generated locals */
    cllist cl__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), f_clos(cllist *);

    /* Local variables */
    static char arch[3], line[255], type__[4];
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), repmc_(char *, char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen, ftnlen), reset_(void);
    static char prmpt[80];
    extern /* Subroutine */ int fn2lun_(char *, integer *, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int daffnh_(char *, integer *, ftnlen);
    static integer handle;
    extern /* Subroutine */ int dafcls_(integer *), delfil_(char *, ftnlen), 
	    dasfnh_(char *, integer *, ftnlen);
    static char binfnm[128];
    static logical fileok;
    extern /* Subroutine */ int getfat_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen), dascls_(integer *);
    static char bfstat[3];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), convbt_(char *, char 
	    *, ftnlen, ftnlen);
    extern logical isopen_(char *, ftnlen);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    static char xfrfnm[128];
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    static char xfstat[3];
    static integer xfrlun;
    extern logical exists_(char *, ftnlen), return_(void);
    extern /* Subroutine */ int writln_(char *, integer *, ftnlen), 
	    getfnm_1__(char *, char *, char *, logical *, ftnlen, ftnlen, 
	    ftnlen);

/* $ Abstract */

/*     SPACIT utility subroutine used to manage the conversion of SPICE */
/*     binary format files into their equivalent transfer formats. This */
/*     subroutine is for use only be the SPACIT program. Use it at your */
/*     own risk. */

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

/*     None. */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*      LOGFIL    I    Logical flag indicating a log file is being kept. */
/*      LOGLUN    I    The logical unit of the log file. */

/* $ Detailed_Input */

/*      LOGFIL   Logical flag indicating a log file is being kept. This */
/*               Variable has the value of .TRUE. if a log file is being */
/*               written, and a value of .FALSE. otherwise. */

/*      LOGLUN   The logical unit of the log file. If LOGFIL has the */
/*               value .TRUE. then LOGLUN will be the Fortran logical */
/*               unit of the log file. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     xxx */

/* $ Examples */

/*     xxx */

/* $ Restrictions */

/*     xxx */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer     (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 10-JUL-1995 (KRG) */



/* -& */
/* $ Index_Entries */

/*     spacit convert binary to transfer */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     Set a value for the logical unit which represents the standard */
/*     output device, commonly a terminal. A value of 6 is widely used, */
/*     but the Fortran standard does not specify a value, so it may be */
/*     different for different Fortran implementations. */


/*     Set a value for the length of an input text line. */


/*     Set a value for the length of a filename. */


/*     Set a length for the prompt. */


/*     Set a length for the status of a file: 'OLD' or 'NEW'. */


/*     Set the length for the type of a file. */


/*     Set the length for the architecture of a file. */


/*     Local variables */


/*     Save everything to keep configuration control happy. */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPAB2T", (ftnlen)6);
    }



    s_copy(bfstat, "OLD", (ftnlen)3, (ftnlen)3);
    s_copy(xfstat, "NEW", (ftnlen)3, (ftnlen)3);
    fileok = FALSE_;
    s_copy(prmpt, "   Binary file  : ", (ftnlen)80, (ftnlen)18);
    getfnm_1__(prmpt, bfstat, binfnm, &fileok, (ftnlen)80, (ftnlen)3, (ftnlen)
	    128);
    if (failed_()) {
	chkout_("SPAB2T", (ftnlen)6);
	return 0;
    }
    getfat_(binfnm, arch, type__, (ftnlen)128, (ftnlen)3, (ftnlen)4);
    if (failed_()) {
	chkout_("SPAB2T", (ftnlen)6);
	return 0;
    }
    if (s_cmp(arch, "?", (ftnlen)3, (ftnlen)1) == 0 || s_cmp(type__, "?", (
	    ftnlen)4, (ftnlen)1) == 0) {
	setmsg_("The architecture and type of the file '#' could not be dete"
		"rmined.", (ftnlen)66);
	errch_("#", binfnm, (ftnlen)1, (ftnlen)128);
	sigerr_("SPICE(BADFILEFORMAT)", (ftnlen)20);
	chkout_("SPAB2T", (ftnlen)6);
	return 0;
    } else if (s_cmp(arch, "DAF", (ftnlen)3, (ftnlen)3) != 0 && s_cmp(arch, 
	    "DAS", (ftnlen)3, (ftnlen)3) != 0) {
	setmsg_("The file '#' was not a binary SPICE file. In order to conve"
		"rt a file it must be a binary CK, EK, PCK, or SPK file.", (
		ftnlen)114);
	errch_("#", binfnm, (ftnlen)1, (ftnlen)128);
	sigerr_("SPICE(IMPROPERFILE)", (ftnlen)19);
	chkout_("SPAB2T", (ftnlen)6);
	return 0;
    }
    fileok = FALSE_;
    s_copy(prmpt, "   Transfer file: ", (ftnlen)80, (ftnlen)18);
    getfnm_1__(prmpt, xfstat, xfrfnm, &fileok, (ftnlen)80, (ftnlen)3, (ftnlen)
	    128);
    if (! fileok) {
	chkout_("SPAB2T", (ftnlen)6);
	return 0;
    }
    s_copy(line, "   #", (ftnlen)255, (ftnlen)4);
    repmc_(line, "#", messag, line, (ftnlen)255, (ftnlen)1, (ftnlen)255, (
	    ftnlen)255);
    writln_(" ", &c__6, (ftnlen)1);
    writln_(line, &c__6, (ftnlen)255);
    if (*logfil) {
	writln_(" ", loglun, (ftnlen)1);
	writln_(line, loglun, (ftnlen)255);
    }
    s_copy(line, "   Converting Binary File: #", (ftnlen)255, (ftnlen)28);
    repmc_(line, "#", binfnm, line, (ftnlen)255, (ftnlen)1, (ftnlen)128, (
	    ftnlen)255);
    writln_(" ", &c__6, (ftnlen)1);
    writln_(line, &c__6, (ftnlen)255);
    if (*logfil) {
	writln_(" ", loglun, (ftnlen)1);
	writln_(line, loglun, (ftnlen)255);
    }
    s_copy(line, "   To Transfer File      : #", (ftnlen)255, (ftnlen)28);
    repmc_(line, "#", xfrfnm, line, (ftnlen)255, (ftnlen)1, (ftnlen)128, (
	    ftnlen)255);
    writln_(line, &c__6, (ftnlen)255);
    writln_(" ", &c__6, (ftnlen)1);
    if (*logfil) {
	writln_(line, loglun, (ftnlen)255);
	writln_(" ", loglun, (ftnlen)1);
    }
    writln_("   Please wait...", &c__6, (ftnlen)17);
    convbt_(binfnm, xfrfnm, (ftnlen)128, (ftnlen)128);
    if (failed_()) {
	reset_();
	if (s_cmp(arch, "DAF", (ftnlen)3, (ftnlen)3) == 0) {
	    daffnh_(binfnm, &handle, (ftnlen)128);
	    dafcls_(&handle);
	} else {
	    dasfnh_(binfnm, &handle, (ftnlen)128);
	    dascls_(&handle);
	}
	if (exists_(xfrfnm, (ftnlen)128)) {
	    if (isopen_(xfrfnm, (ftnlen)128)) {
		fn2lun_(xfrfnm, &xfrlun, (ftnlen)128);
		cl__1.cerr = 0;
		cl__1.cunit = xfrlun;
		cl__1.csta = 0;
		f_clos(&cl__1);
	    }
	    delfil_(xfrfnm, (ftnlen)128);
	}
	chkout_("SPAB2T", (ftnlen)6);
	return 0;
    }
    s_copy(line, "   Transfer file '#' created.", (ftnlen)255, (ftnlen)29);
    repmc_(line, "#", xfrfnm, line, (ftnlen)255, (ftnlen)1, (ftnlen)128, (
	    ftnlen)255);
    writln_(" ", &c__6, (ftnlen)1);
    writln_(line, &c__6, (ftnlen)255);
    writln_(" ", &c__6, (ftnlen)1);
    if (*logfil) {
	writln_(" ", loglun, (ftnlen)1);
	writln_(line, loglun, (ftnlen)255);
	writln_(" ", loglun, (ftnlen)1);
    }
    chkout_("SPAB2T", (ftnlen)6);
    return 0;
} /* spab2t_ */

