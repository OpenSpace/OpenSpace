/* spardc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;
static integer c__1 = 1;

/* $Procedure      SPARDC ( SPACIT, read comments from binary file ) */
/* Subroutine */ int spardc_(logical *logfil, integer *loglun)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char arch[3], line[255];
    integer ncom;
    char type__[4];
    extern /* Subroutine */ int dasec_(integer *, integer *, integer *, char *
	    , logical *, ftnlen), chkin_(char *, ftnlen), errch_(char *, char 
	    *, ftnlen, ftnlen), repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen);
    char separ[255];
    extern /* Subroutine */ int reset_(void);
    char prmpt[80];
    extern logical failed_(void);
    integer handle;
    extern /* Subroutine */ int dafcls_(integer *);
    char binfnm[128];
    logical fileok;
    extern /* Subroutine */ int getfat_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen), dafopr_(char *, integer *, ftnlen), dascls_(
	    integer *);
    char bfstat[3];
    extern /* Subroutine */ int dasopr_(char *, integer *, ftnlen), spcrfl_(
	    integer *, char *, logical *, ftnlen), sigerr_(char *, ftnlen);
    integer numcom;
    extern /* Subroutine */ int chkout_(char *, ftnlen), spcrnl_(char *, 
	    logical *, ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int writln_(char *, integer *, ftnlen);
    logical eoc;
    extern /* Subroutine */ int getfnm_1__(char *, char *, char *, logical *, 
	    ftnlen, ftnlen, ftnlen);

/* $ Abstract */

/*     SPACIT utility subroutine used to read the comments from a SPICE */
/*     binary kernel file. This subroutine is for use only be the SPACIT */
/*     program. Use it at your own risk. */

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

/* -    Beta Version 1.0.0, 11-JUL-1995 (KRG) */



/* -& */
/* $ Index_Entries */

/*     spacit convert binary to transfer */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     Set value for a separator */


/*     Set values for the NAIF SPICE file types */


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


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPARDC", (ftnlen)6);
    }

/*     Initialize the separator. */

    s_copy(separ, "*********************************************************"
	    "***********************", (ftnlen)255, (ftnlen)80);

/*     Get the filename. */

    s_copy(bfstat, "OLD", (ftnlen)3, (ftnlen)3);
    fileok = FALSE_;
    s_copy(prmpt, "   Binary file  : ", (ftnlen)80, (ftnlen)18);
    getfnm_1__(prmpt, bfstat, binfnm, &fileok, (ftnlen)80, (ftnlen)3, (ftnlen)
	    128);
    if (failed_()) {
	chkout_("SPARDC", (ftnlen)6);
	return 0;
    }
    getfat_(binfnm, arch, type__, (ftnlen)128, (ftnlen)3, (ftnlen)4);
    if (failed_()) {
	chkout_("SPARDC", (ftnlen)6);
	return 0;
    }
    if (s_cmp(arch, "?", (ftnlen)3, (ftnlen)1) == 0 || s_cmp(type__, "?", (
	    ftnlen)4, (ftnlen)1) == 0) {
	setmsg_("The architecture and type of the file '#' could not be dete"
		"rmined.", (ftnlen)66);
	errch_("#", binfnm, (ftnlen)1, (ftnlen)128);
	sigerr_("SPICE(BADFILEFORMAT)", (ftnlen)20);
	chkout_("SPARDC", (ftnlen)6);
	return 0;
    } else if (s_cmp(arch, "DAF", (ftnlen)3, (ftnlen)3) != 0 && s_cmp(arch, 
	    "DAS", (ftnlen)3, (ftnlen)3) != 0) {
	setmsg_("The file '#' was not a binary SPICE file. In order to read "
		"comments a file must be a binary CK, EK, PCK, or SPK file.", (
		ftnlen)117);
	errch_("#", binfnm, (ftnlen)1, (ftnlen)128);
	sigerr_("SPICE(IMPROPERFILE)", (ftnlen)19);
	chkout_("SPARDC", (ftnlen)6);
	return 0;
    }
    if (s_cmp(type__, "PRE", (ftnlen)4, (ftnlen)3) == 0) {
	s_copy(type__, "EK", (ftnlen)4, (ftnlen)2);
    }
    writln_(" ", &c__6, (ftnlen)1);
    writln_(separ, &c__6, (ftnlen)255);
    writln_(" ", &c__6, (ftnlen)1);
    if (*logfil) {
	writln_(" ", loglun, (ftnlen)1);
	writln_(separ, loglun, (ftnlen)255);
	writln_(" ", loglun, (ftnlen)1);
    }
    s_copy(line, "Comments From File: #", (ftnlen)255, (ftnlen)21);
    repmc_(line, "#", binfnm, line, (ftnlen)255, (ftnlen)1, (ftnlen)128, (
	    ftnlen)255);
    writln_(line, &c__6, (ftnlen)255);
    writln_(" ", &c__6, (ftnlen)1);
    if (*logfil) {
	writln_(line, loglun, (ftnlen)255);
	writln_(" ", loglun, (ftnlen)1);
    }
    if (s_cmp(type__, "CK", (ftnlen)4, (ftnlen)2) == 0 || s_cmp(type__, "SPK",
	     (ftnlen)4, (ftnlen)3) == 0 || s_cmp(type__, "PCK", (ftnlen)4, (
	    ftnlen)3) == 0) {

/*        Open the binary file, read the comments, and close */
/*        the binary file. */

	dafopr_(binfnm, &handle, (ftnlen)128);
	if (failed_()) {
	    reset_();
	    dafcls_(&handle);
	    chkout_("SPARDC", (ftnlen)6);
	    return 0;
	}

/*        The comments are read a line at a time and written to the */
/*        screen and the log file, if a log file is being used. */

	eoc = FALSE_;
	numcom = 0;
	while(! eoc) {
	    if (numcom == 0) {
		spcrfl_(&handle, line, &eoc, (ftnlen)255);
	    } else {
		spcrnl_(line, &eoc, (ftnlen)255);
	    }
	    if (failed_()) {
		reset_();
		dafcls_(&handle);
		chkout_("SPARDC", (ftnlen)6);
		return 0;
	    }
	    if (! eoc) {
		++numcom;
		writln_(line, &c__6, (ftnlen)255);
		if (*logfil) {
		    writln_(line, loglun, (ftnlen)255);
		}
	    }
	}
	if (numcom == 0) {
	    s_copy(line, "There were no comments in the file.", (ftnlen)255, (
		    ftnlen)35);
	    writln_(" ", &c__6, (ftnlen)1);
	    writln_(line, &c__6, (ftnlen)255);
	    if (*logfil) {
		writln_(" ", loglun, (ftnlen)1);
		writln_(line, loglun, (ftnlen)255);
	    }
	}
	dafcls_(&handle);
    } else if (s_cmp(type__, "EK", (ftnlen)4, (ftnlen)2) == 0) {

/*        Open the binary file, read the comments, and close */
/*        the binary file. */

	handle = 0;
	dasopr_(binfnm, &handle, (ftnlen)128);
	if (failed_()) {
	    reset_();
	    dascls_(&handle);
	    chkout_("SPARDC", (ftnlen)6);
	    return 0;
	}
	eoc = FALSE_;
	numcom = 0;
	while(! eoc) {
	    dasec_(&handle, &c__1, &ncom, line, &eoc, (ftnlen)255);
	    if (failed_()) {
		reset_();
		dascls_(&handle);
		chkout_("SPARDC", (ftnlen)6);
		return 0;
	    }
	    if (! eoc) {
		++numcom;
		writln_(line, &c__6, (ftnlen)255);
		if (*logfil) {
		    writln_(line, loglun, (ftnlen)255);
		}
	    }
	}
	if (numcom == 0) {
	    s_copy(line, "There were no comments in the file.", (ftnlen)255, (
		    ftnlen)35);
	    writln_(" ", &c__6, (ftnlen)1);
	    writln_(line, &c__6, (ftnlen)255);
	    if (*logfil) {
		writln_(" ", loglun, (ftnlen)1);
		writln_(line, loglun, (ftnlen)255);
	    }
	}
	dascls_(&handle);
    }
    writln_(" ", &c__6, (ftnlen)1);
    writln_(separ, &c__6, (ftnlen)255);
    if (*logfil) {
	writln_(" ", loglun, (ftnlen)1);
	writln_(separ, loglun, (ftnlen)255);
    }
    chkout_("SPARDC", (ftnlen)6);
    return 0;
} /* spardc_ */

