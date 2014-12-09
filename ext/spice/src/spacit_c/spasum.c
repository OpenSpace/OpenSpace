/* spasum.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;

/* $Procedure      SPASUM ( SPACIT, summarize binary file ) */
/* Subroutine */ int spasum_(logical *logfil, integer *loglun)
{
    /* Initialized data */

    static logical lpsldd = FALSE_;
    static logical sclldd = FALSE_;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char arch[3], line[255], type__[4];
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    logical ndlps;
    extern /* Subroutine */ int sumck_(integer *, char *, char *, char *, 
	    logical *, integer *, ftnlen, ftnlen, ftnlen), sumek_(integer *, 
	    char *, logical *, integer *, ftnlen);
    char prmpt[80];
    extern logical failed_(void);
    integer handle;
    extern /* Subroutine */ int dafcls_(integer *);
    char binfnm[128];
    logical fileok;
    extern /* Subroutine */ int getfat_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen), dafopr_(char *, integer *, ftnlen);
    logical ndsclk;
    extern /* Subroutine */ int dascls_(integer *);
    static char sclfnm[128];
    char bfstat[3];
    extern /* Subroutine */ int dasopr_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen);
    char lfstat[3];
    static char lpsfnm[128];
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), sumpck_(integer *, char *, char *, logical *, integer *, 
	    ftnlen, ftnlen);
    char sfstat[3];
    extern /* Subroutine */ int furnsh_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int writln_(char *, integer *, ftnlen), sumspk_(
	    integer *, char *, char *, logical *, integer *, ftnlen, ftnlen), 
	    getfnm_1__(char *, char *, char *, logical *, ftnlen, ftnlen, 
	    ftnlen);

/* $ Abstract */

/*     SPACIT utility subroutine used to summarize the segments in SPICE */
/*     data kernel files. This subroutine is for use only be the SPACIT */
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

/*     W.L. Taber         (JPL) */
/*     K.R. Gehringer     (JPL) */

/* $ Version */

/* -    Beta Version 2.1.0, 02-OCT-2006 (BVS) */

/*        Replaced LDPOOL with FURNSH. */

/* -    Beta Version 2.0.0, 14-MAR-1997 (WLT) */

/*        The routine was enhanced to provide a diagnostic in the */
/*        event that the type of the file does belong to EK, CK, SPK */
/*        or PCK */

/* -    Beta Version 1.0.0, 11-JUL-1995 (KRG) */



/* -& */
/* $ Index_Entries */

/*     spacit convert binary to transfer */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

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


/*     Saved values */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPASUM", (ftnlen)6);
    }



    s_copy(bfstat, "OLD", (ftnlen)3, (ftnlen)3);
    fileok = FALSE_;
    s_copy(prmpt, "   Binary file     : ", (ftnlen)80, (ftnlen)21);
    getfnm_1__(prmpt, bfstat, binfnm, &fileok, (ftnlen)80, (ftnlen)3, (ftnlen)
	    128);
    if (failed_()) {
	chkout_("SPASUM", (ftnlen)6);
	return 0;
    }
    getfat_(binfnm, arch, type__, (ftnlen)128, (ftnlen)3, (ftnlen)4);
    if (failed_()) {
	chkout_("SPASUM", (ftnlen)6);
	return 0;
    }
    if (s_cmp(arch, "?", (ftnlen)3, (ftnlen)1) == 0 || s_cmp(type__, "?", (
	    ftnlen)4, (ftnlen)1) == 0) {
	setmsg_("The architecture and type of the file '#' could not be dete"
		"rmined.", (ftnlen)66);
	errch_("#", binfnm, (ftnlen)1, (ftnlen)128);
	sigerr_("SPICE(BADFILEFORMAT)", (ftnlen)20);
	chkout_("SPASUM", (ftnlen)6);
	return 0;
    } else if (s_cmp(arch, "DAF", (ftnlen)3, (ftnlen)3) != 0 && s_cmp(arch, 
	    "DAS", (ftnlen)3, (ftnlen)3) != 0) {
	setmsg_("The file '#' was not a binary SPICE file. In order to summa"
		"rize a file it must be a binary CK, EK, PCK, or SPK file.", (
		ftnlen)116);
	errch_("#", binfnm, (ftnlen)1, (ftnlen)128);
	sigerr_("SPICE(IMPROPERFILE)", (ftnlen)19);
	chkout_("SPASUM", (ftnlen)6);
	return 0;
    }
    if (s_cmp(type__, "PRE", (ftnlen)4, (ftnlen)3) == 0) {
	s_copy(type__, "EK", (ftnlen)4, (ftnlen)2);
    }
    if (lpsldd || s_cmp(type__, "EK", (ftnlen)4, (ftnlen)2) == 0) {
	ndlps = FALSE_;
    } else {
	ndlps = TRUE_;
    }
    ndsclk = FALSE_;
    if (! sclldd && s_cmp(type__, "CK", (ftnlen)4, (ftnlen)2) == 0) {
	ndsclk = TRUE_;
    }
    if (ndlps) {
	s_copy(lfstat, "OLD", (ftnlen)3, (ftnlen)3);
	fileok = FALSE_;
	s_copy(prmpt, "   Leapseconds file: ", (ftnlen)80, (ftnlen)21);
	getfnm_1__(prmpt, lfstat, lpsfnm, &fileok, (ftnlen)80, (ftnlen)3, (
		ftnlen)128);
	if (failed_()) {
	    chkout_("SPASUM", (ftnlen)6);
	    return 0;
	}
    }
    if (ndsclk) {
	s_copy(sfstat, "OLD", (ftnlen)3, (ftnlen)3);
	fileok = FALSE_;
	s_copy(prmpt, "   SCLK file       : ", (ftnlen)80, (ftnlen)21);
	getfnm_1__(prmpt, sfstat, sclfnm, &fileok, (ftnlen)80, (ftnlen)3, (
		ftnlen)128);
	if (failed_()) {
	    chkout_("SPASUM", (ftnlen)6);
	    return 0;
	}
    }
    writln_(" ", &c__6, (ftnlen)1);
    if (ndlps) {
	s_copy(line, "   Loading the Leapseconds kernel file. Please wait ..."
		, (ftnlen)255, (ftnlen)55);
	writln_(line, &c__6, (ftnlen)255);
	furnsh_(lpsfnm, (ftnlen)128);
	if (failed_()) {
	    chkout_("SPASUM", (ftnlen)6);
	    return 0;
	}
	lpsldd = TRUE_;
    }
    if (ndsclk) {
	s_copy(line, "   Loading the SCLK kernel file. Please wait ...", (
		ftnlen)255, (ftnlen)48);
	writln_(line, &c__6, (ftnlen)255);
	furnsh_(sclfnm, (ftnlen)128);
	if (failed_()) {
	    chkout_("SPASUM", (ftnlen)6);
	    return 0;
	}
	sclldd = TRUE_;
    }
    writln_(" ", &c__6, (ftnlen)1);
    if (*logfil) {
	writln_(" ", loglun, (ftnlen)1);
    }
    if (s_cmp(type__, "CK", (ftnlen)4, (ftnlen)2) == 0) {

/*        Summarize a binary CK file. */

	dafopr_(binfnm, &handle, (ftnlen)128);
	if (failed_()) {
	    chkout_("SPASUM", (ftnlen)6);
	    return 0;
	}
	sumck_(&handle, binfnm, lpsfnm, sclfnm, logfil, loglun, (ftnlen)128, (
		ftnlen)128, (ftnlen)128);
	dafcls_(&handle);
    } else if (s_cmp(type__, "SPK", (ftnlen)4, (ftnlen)3) == 0) {

/*        Summarize a binary SPK file. */

	dafopr_(binfnm, &handle, (ftnlen)128);
	if (failed_()) {
	    chkout_("SPASUM", (ftnlen)6);
	    return 0;
	}
	sumspk_(&handle, binfnm, lpsfnm, logfil, loglun, (ftnlen)128, (ftnlen)
		128);
	dafcls_(&handle);
    } else if (s_cmp(type__, "PCK", (ftnlen)4, (ftnlen)3) == 0) {

/*        Summarize a binary PCK file. */

	dafopr_(binfnm, &handle, (ftnlen)128);
	if (failed_()) {
	    chkout_("SPASUM", (ftnlen)6);
	    return 0;
	}
	sumpck_(&handle, binfnm, lpsfnm, logfil, loglun, (ftnlen)128, (ftnlen)
		128);
	dafcls_(&handle);
    } else if (s_cmp(type__, "EK", (ftnlen)4, (ftnlen)2) == 0) {

/*        Summarize a binary EK file. */

	dasopr_(binfnm, &handle, (ftnlen)128);
	sumek_(&handle, binfnm, logfil, loglun, (ftnlen)128);
	dascls_(&handle);
    } else {
	setmsg_("The specified file is not of a \"type\" that can be summari"
		"zed. The types of files that can be summarized are: CK, EK, "
		"PCK, and SPK.  According to the type in the internal id-word"
		" of the file, this file has type: '#'.  You will need to get"
		" an upgrade of SPACIT to summarize this file. ", (ftnlen)283);
	errch_("#", type__, (ftnlen)1, (ftnlen)4);
	sigerr_("SPICE(UNKNOWNTYPE)", (ftnlen)18);
	chkout_("SPASUM", (ftnlen)6);
	return 0;
    }
    chkout_("SPASUM", (ftnlen)6);
    return 0;
} /* spasum_ */

