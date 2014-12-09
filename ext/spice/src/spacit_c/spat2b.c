/* spat2b.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;

/* $Procedure SPAT2B ( SPACIT, convert transfer to binary ) */
/* Subroutine */ int spat2b_(logical *logfil, integer *loglun)
{
    /* Initialized data */

    static char messag[255] = "Converting from transfer file to binary file."
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                  ";

    /* System generated locals */
    integer i__1, i__2;
    cllist cl__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer f_clos(cllist *), s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(
	    char *, integer, char *, integer);

    /* Local variables */
    static char arch[3], line[255], type__[4];
    extern /* Subroutine */ int zzgetfat_(char *, char *, char *, integer *, 
	    ftnlen, ftnlen, ftnlen), zzconvtb_(char *, char *, char *, char *,
	     integer *, ftnlen, ftnlen, ftnlen, ftnlen);
    static integer i__;
    extern integer cardi_(integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), repmc_(char *, char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen, ftnlen), reset_(void);
    static char prmpt[80];
    extern logical failed_(void);
    extern /* Subroutine */ int dafhof_(integer *), dafcls_(integer *), 
	    cleari_(integer *, integer *), delfil_(char *, ftnlen), scardi_(
	    integer *, integer *), dashof_(integer *);
    static char binfnm[128];
    static logical fileok;
    extern /* Subroutine */ int dascls_(integer *);
    static char bfstat[3];
    static integer number;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    static char xfrfnm[128];
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    static integer opnset[7];
    static char xfstat[3];
    static integer numopn;
    extern /* Subroutine */ int tostdo_(char *, ftnlen);
    extern logical exists_(char *, ftnlen), return_(void);
    extern /* Subroutine */ int writln_(char *, integer *, ftnlen), 
	    getfnm_1__(char *, char *, char *, logical *, ftnlen, ftnlen, 
	    ftnlen);

/* $ Abstract */

/*     SPACIT utility subroutine used to manage the conversion of SPICE */
/*     transfer format files into their equivalent binary formats. This */
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


/* -    SPICE Version 2.0.0, 13-MAR-1999 (EDW) */

/*        Replaced WRITLN's to standard output with TOSTDO, */
/*        calls to GETFAT with ZZGETFAT, calls to CONVTB to */
/*        ZZCONVTB. */

/*        Added a condition to the ARCH-TYPE test block following */
/*        the call to ZZGETFAT.  The block test for both ARCH and */
/*        TYPE as unknows, then for ARCH as XFR, then either ARCH */
/*        or TYPE as unknown. */

/* -    Beta Version 1.0.0, 13-NOV-1994 (KRG) */

/* -& */
/* $ Index_Entries */

/*     spacit convert transfer to binary */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     Lower bound for a SPICELIB CELL data structure. */


/*     Maximum number of open binary files allowed. */


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
	chkin_("SPAT2B", (ftnlen)6);
    }



    s_copy(bfstat, "NEW", (ftnlen)3, (ftnlen)3);
    s_copy(xfstat, "OLD", (ftnlen)3, (ftnlen)3);
    fileok = FALSE_;
    s_copy(prmpt, "   Transfer file: ", (ftnlen)80, (ftnlen)18);
    getfnm_1__(prmpt, xfstat, xfrfnm, &fileok, (ftnlen)80, (ftnlen)3, (ftnlen)
	    128);
    if (failed_()) {
	chkout_("SPAT2B", (ftnlen)6);
	return 0;
    }

/*     Get the architecture, file type and logical unit number for */
/*     the transfer file. */

    zzgetfat_(xfrfnm, arch, type__, &number, (ftnlen)128, (ftnlen)3, (ftnlen)
	    4);

/*     If something went wrong, close the transfer file and checkout. */

    if (failed_()) {
	cl__1.cerr = 0;
	cl__1.cunit = number;
	cl__1.csta = 0;
	f_clos(&cl__1);
	chkout_("SPAT2B", (ftnlen)6);
	return 0;
    }

/*     The file should be a transfer file.  If not, flag an error */
/*     then exit. */

    if (s_cmp(arch, "?", (ftnlen)3, (ftnlen)1) == 0 && s_cmp(type__, "?", (
	    ftnlen)4, (ftnlen)1) == 0) {
	setmsg_("The architecture and type of the file '#' could not be dete"
		"rmined.", (ftnlen)66);
	errch_("#", xfrfnm, (ftnlen)1, (ftnlen)128);
	sigerr_("SPICE(BADFILEFORMAT)", (ftnlen)20);
	chkout_("SPAT2B", (ftnlen)6);
	return 0;
    } else if (s_cmp(arch, "XFR", (ftnlen)3, (ftnlen)3) != 0 && s_cmp(arch, 
	    "DEC", (ftnlen)3, (ftnlen)3) != 0) {
	setmsg_("The file '#' was not a transfer format SPICE file. In order"
		" to convert a transfer file to binary format, it must be a C"
		"K, EK, PCK, or SPK file in one of the supported transfer fil"
		"e formats.", (ftnlen)189);
	errch_("#", xfrfnm, (ftnlen)1, (ftnlen)128);
	sigerr_("SPICE(IMPROPERFILE)", (ftnlen)19);
	chkout_("SPAT2B", (ftnlen)6);
	return 0;
    } else if (s_cmp(arch, "?", (ftnlen)3, (ftnlen)1) == 0 || s_cmp(type__, 
	    "?", (ftnlen)4, (ftnlen)1) == 0) {
	setmsg_("The architecture or type of the file '#' could not be deter"
		"mined.  The file did not read as a transfer file", (ftnlen)
		107);
	errch_("#", xfrfnm, (ftnlen)1, (ftnlen)128);
	sigerr_("SPICE(BADFILEFORMAT)", (ftnlen)20);
	chkout_("SPAT2B", (ftnlen)6);
	return 0;
    }

/*     No get the binary file name. */

    fileok = FALSE_;
    s_copy(prmpt, "   Binary file  : ", (ftnlen)80, (ftnlen)18);
    getfnm_1__(prmpt, bfstat, binfnm, &fileok, (ftnlen)80, (ftnlen)3, (ftnlen)
	    128);
    if (! fileok) {
	chkout_("SPAT2B", (ftnlen)6);
	return 0;
    }
    s_copy(line, "   #", (ftnlen)255, (ftnlen)4);
    repmc_(line, "#", messag, line, (ftnlen)255, (ftnlen)1, (ftnlen)255, (
	    ftnlen)255);
    tostdo_(" ", (ftnlen)1);
    tostdo_(line, (ftnlen)255);
    if (*logfil) {
	writln_(" ", loglun, (ftnlen)1);
	writln_(line, loglun, (ftnlen)255);
    }
    s_copy(line, "   Converting Transfer File: #", (ftnlen)255, (ftnlen)30);
    repmc_(line, "#", xfrfnm, line, (ftnlen)255, (ftnlen)1, (ftnlen)128, (
	    ftnlen)255);
    tostdo_(" ", (ftnlen)1);
    tostdo_(line, (ftnlen)255);
    if (*logfil) {
	writln_(" ", loglun, (ftnlen)1);
	writln_(line, loglun, (ftnlen)255);
    }
    s_copy(line, "   To Binary File          : #", (ftnlen)255, (ftnlen)30);
    repmc_(line, "#", binfnm, line, (ftnlen)255, (ftnlen)1, (ftnlen)128, (
	    ftnlen)255);
    tostdo_(line, (ftnlen)255);
    tostdo_(" ", (ftnlen)1);
    if (*logfil) {
	writln_(line, loglun, (ftnlen)255);
	writln_(" ", loglun, (ftnlen)1);
    }
    tostdo_("   Please wait...", (ftnlen)17);

/*     Now call the converted.  The real work starts here. */

    zzconvtb_(xfrfnm, arch, type__, binfnm, &number, (ftnlen)128, (ftnlen)3, (
	    ftnlen)4, (ftnlen)128);

/*      Important question, did the conversion work? */

    if (failed_()) {

/*        Something failed.  Reset the error system. */

	reset_();
	scardi_(&c__0, opnset);
	cleari_(&c__1, &opnset[6]);
	dafhof_(opnset);
	numopn = cardi_(opnset);

/*        Close the DAF files. */

	if (numopn > 0) {
	    i__1 = numopn;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		dafcls_(&opnset[(i__2 = i__ + 5) < 7 && 0 <= i__2 ? i__2 : 
			s_rnge("opnset", i__2, "spat2b_", (ftnlen)371)]);
	    }
	}
	scardi_(&c__0, opnset);
	cleari_(&c__1, &opnset[6]);
	dashof_(opnset);

/*        Close the DAS files. */

	numopn = cardi_(opnset);
	if (numopn > 0) {
	    i__1 = numopn;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		dascls_(&opnset[(i__2 = i__ + 5) < 7 && 0 <= i__2 ? i__2 : 
			s_rnge("opnset", i__2, "spat2b_", (ftnlen)386)]);
	    }
	}

/*        Erase any partially converted binary file, then checkout. */

	if (exists_(binfnm, (ftnlen)128)) {
	    delfil_(binfnm, (ftnlen)128);
	}
	chkout_("SPAT2B", (ftnlen)6);
	return 0;
    }

/*     If we got this far, the conversion worked (yippie!). */

    s_copy(line, "   Binary file '#' created.", (ftnlen)255, (ftnlen)27);
    repmc_(line, "#", binfnm, line, (ftnlen)255, (ftnlen)1, (ftnlen)128, (
	    ftnlen)255);
    tostdo_(" ", (ftnlen)1);
    tostdo_(line, (ftnlen)255);
    tostdo_(" ", (ftnlen)1);
    if (*logfil) {
	writln_(" ", loglun, (ftnlen)1);
	writln_(line, loglun, (ftnlen)255);
	writln_(" ", loglun, (ftnlen)1);
    }
    chkout_("SPAT2B", (ftnlen)6);
    return 0;
} /* spat2b_ */

