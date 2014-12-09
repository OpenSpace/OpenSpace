/* clcomm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__5 = 5;
static logical c_true = TRUE_;
static integer c__6 = 6;
static integer c__18 = 18;
static integer c__0 = 0;
static integer c__1 = 1;

/* $Procedure CLCOMM ( Command line COMMNT subroutine ) */
/* Subroutine */ int clcomm_(void)
{
    /* Initialized data */

    static char optflg[2*5] = "-A" "-D" "-E" "-H" "-R";
    static char optnam[16*5] = "ADD_COMMENTS    " "DELETE_COMMENTS " "EXTRAC"
	    "T_COMMENTS" "HELP            " "READ_COMMENTS   ";

    /* System generated locals */
    integer i__1, i__2;
    cllist cl__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer), f_clos(cllist *);

    /* Local variables */
    static char arch[3], type__[4];
    extern /* Subroutine */ int dafdc_(integer *);
    static integer i__;
    extern /* Subroutine */ int dasdc_(integer *);
    extern integer cardi_(integer *);
    extern /* Subroutine */ int spcac_(integer *, integer *, char *, char *, 
	    ftnlen, ftnlen), chkin_(char *, ftnlen), ucase_(char *, char *, 
	    ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    static char usage[80];
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen);
    extern integer wdcnt_(char *, ftnlen);
    extern /* Subroutine */ int reset_(void);
    extern logical failed_(void);
    extern /* Subroutine */ int dafecu_(integer *, integer *, logical *), 
	    dafhof_(integer *);
    static char cmdflg[7];
    extern /* Subroutine */ int dafcls_(integer *), cleari_(integer *, 
	    integer *), dasacu_(integer *, char *, char *, logical *, integer 
	    *, ftnlen, ftnlen), dasllc_(integer *), dasecu_(integer *, 
	    integer *, logical *), scardi_(integer *, integer *);
    static integer kerhan;
    extern /* Subroutine */ int dascls_(integer *);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int getcml_(char *, ftnlen), getfat_(char *, char 
	    *, char *, ftnlen, ftnlen, ftnlen), dafopr_(char *, integer *, 
	    ftnlen);
    static char comfnm[128], kerfnm[128];
    extern /* Subroutine */ int erract_(char *, char *, ftnlen, ftnlen), 
	    byebye_(char *, ftnlen), dafopw_(char *, integer *, ftnlen), 
	    dashof_(integer *), dasopr_(char *, integer *, ftnlen);
    static char inplin[512], hlpmsg[80*18];
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    static integer comlun;
    extern /* Subroutine */ int chkout_(char *, ftnlen), dasopw_(char *, 
	    integer *, ftnlen);
    static char tmplin[512];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), writla_(integer *, 
	    char *, integer *, ftnlen);
    static integer idxopt;
    static char option[16];
    static integer opnset[7], numopn, nwords;
    static logical gotsom;
    extern logical exists_(char *, ftnlen);
    extern /* Subroutine */ int errprt_(char *, char *, ftnlen, ftnlen), 
	    nextwd_(char *, char *, char *, ftnlen, ftnlen, ftnlen), tostdo_(
	    char *, ftnlen), writln_(char *, integer *, ftnlen), txtopn_(char 
	    *, integer *, ftnlen), txtopr_(char *, integer *, ftnlen);

/* $ Abstract */

/*     CLCOMM provides command line funcionality to the COMMNT */
/*     program. */

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

/* $ Brief_I/O */

/*     Command line options: */

/*        -a   indicates add comments to a binary kernel. */

/*        -e   indicates extract comments from a binary kernel. */

/*        -r   indicates read the comments in a binary kernel. */

/*        -d   indicates delete the comments from a binary kernel. */

/*        -h   displaye the help message */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     Performance of the action defined by the comand line options. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The command line usage of COMMNT is shown below: */

/*     Add comments to a kernel file from a comment file. */

/*        prompt> commnt -a kernel_file comment_file */

/*     Extract comments from a kernel file to a comment file. */

/*        prompt> commnt -e kernel_file comment_file */

/*     Read the comments in a kernel file. */

/*        prompt> commnt -r kernel_file */

/*     Delete all of the comments in a kernel file. */

/*        prompt> commnt -d kernel_file */

/* $ Examples */

/*      None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     For additional information, see the COMMNT User's Guide. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIC Version 1.1.0, 04-AUG-2006 (EDW) */

/*        Replaced the DAFACU call in the add comments block with */
/*        SPCAC. Use of DAFACU causes a system error under DOS from */
/*        CSPICE commnt for N52a and later packages. */

/*            CALL SPCAC  (  KERHAN, COMLUN, ' ', ' ' ) */

/*        replaces */

/*            CALL DAFACU ( COMLUN, ' ', ' ', .TRUE., KERHAN ) */

/*        Edited header. */

/* -    SPICELIB Version 1.0.1, 23-JAM-1997 (WLT) */

/*        Fixed a typo in the description of usage above. */

/* -    SPICELIB Version 1.0.0, 11-AUG-1995 (KRG) */

/* -& */

/*     Spicelib functions */


/*     Parameters */

/*     Set a value for the logical unit which represents the standard */
/*     output device, commonly a terminal. A value of 6 is widely used, */
/*     but the Fortran standard does not specify a value, so it may be */
/*     different for different fortran implementations. */


/*     Lower bound for a SPICELIB CELL data structure. */


/*     Maximum number of open binary files allowed. */


/*     Number of different options. */


/*     Number of different option namess. */


/*     Maximum length of an option flag, which may be the entire word for */
/*     the action: 'ADD', 'DELETE', 'EXTRACT', 'HELP', or 'READ'. */


/*     Number of characters that must match in the option flag. */


/*     Line length. */


/*     Output line length. */


/*     Length of a SPICE file architecture. */


/*     Length of a SPICE file type. */


/*     Filename length. */


/*     Number of help lines. */


/*     Variables */


/*     The option flags for the command line. */


/*     The options to perform. */


/*     Set up the help message. */

    s_copy(hlpmsg, "Usage:", (ftnlen)80, (ftnlen)6);
    s_copy(hlpmsg + 80, " ", (ftnlen)80, (ftnlen)1);
    s_copy(hlpmsg + 160, "   commnt [<-option> [<kernel file>]] [<comment fi"
	    "le>]", (ftnlen)80, (ftnlen)54);
    s_copy(hlpmsg + 240, " ", (ftnlen)80, (ftnlen)1);
    s_copy(hlpmsg + 320, "If no arguments are specified commnt runs in a men"
	    "u driven,", (ftnlen)80, (ftnlen)59);
    s_copy(hlpmsg + 400, "interactive mode.", (ftnlen)80, (ftnlen)17);
    s_copy(hlpmsg + 480, " ", (ftnlen)80, (ftnlen)1);
    s_copy(hlpmsg + 560, "Options:", (ftnlen)80, (ftnlen)8);
    s_copy(hlpmsg + 640, " ", (ftnlen)80, (ftnlen)1);
    s_copy(hlpmsg + 720, "   -a   is used to add comments to a binary kernel."
	    , (ftnlen)80, (ftnlen)51);
    s_copy(hlpmsg + 800, " ", (ftnlen)80, (ftnlen)1);
    s_copy(hlpmsg + 880, "   -e   is used to extract comments from a binary "
	    "kernel.", (ftnlen)80, (ftnlen)57);
    s_copy(hlpmsg + 960, " ", (ftnlen)80, (ftnlen)1);
    s_copy(hlpmsg + 1040, "   -r   is used to read the comments in a binary "
	    "kernel.", (ftnlen)80, (ftnlen)56);
    s_copy(hlpmsg + 1120, " ", (ftnlen)80, (ftnlen)1);
    s_copy(hlpmsg + 1200, "   -d   is used to delete the comments from a bin"
	    "ary kernel.", (ftnlen)80, (ftnlen)60);
    s_copy(hlpmsg + 1280, " ", (ftnlen)80, (ftnlen)1);
    s_copy(hlpmsg + 1360, "   -h   is used to display this message.", (ftnlen)
	    80, (ftnlen)40);
    s_copy(usage, hlpmsg + 160, (ftnlen)80, (ftnlen)80);

/*     Check into the error handling. */

    chkin_("CLCOMMNT", (ftnlen)8);

/*     Set the error action to ABORT mode. */

    erract_("SET", "ABORT", (ftnlen)3, (ftnlen)5);

/*     Set the error messages that are printed. */

    errprt_("SET", "NONE, SHORT, LONG, TRACEBACK", (ftnlen)3, (ftnlen)28);

/*     Get the command line arguments. */

    s_copy(inplin, " ", (ftnlen)512, (ftnlen)1);
    getcml_(inplin, (ftnlen)512);

/*     Store the input command line into a temporary line that we may */
/*     mangle. */

    s_copy(tmplin, inplin, (ftnlen)512, (ftnlen)512);

/*     If the input line is blank return. */

    if (s_cmp(tmplin, " ", (ftnlen)512, (ftnlen)1) == 0) {
	chkout_("CLCOMMNT", (ftnlen)8);
	return 0;
    }

/*     Get the option. */

    nextwd_(tmplin, cmdflg, tmplin, (ftnlen)512, (ftnlen)7, (ftnlen)512);
    ucase_(cmdflg, cmdflg, (ftnlen)7, (ftnlen)7);

/*     Check to see if the option is one that we recognize. We only want */
/*     to match FLGMCH characters, including the dash, '-'. */

    idxopt = isrchc_(cmdflg, &c__5, optflg, (ftnlen)2, (ftnlen)2);

/*     If we didn't find the option, then signal an error. */

    if (idxopt == 0) {
	for (i__ = 1; i__ <= 18; ++i__) {
	    tostdo_(hlpmsg + ((i__1 = i__ - 1) < 18 && 0 <= i__1 ? i__1 : 
		    s_rnge("hlpmsg", i__1, "clcomm_", (ftnlen)328)) * 80, (
		    ftnlen)80);
	}
	byebye_("FAILURE", (ftnlen)7);
    }

/*     Get the number of words remaining on the command line. This */
/*     number should be zero, for '-h', one for '-r' and '-d', or two */
/*     for '-a' and '-e'. */

    nwords = wdcnt_(tmplin, (ftnlen)512);
    if (nwords > 2) {
	for (i__ = 1; i__ <= 18; ++i__) {
	    tostdo_(hlpmsg + ((i__1 = i__ - 1) < 18 && 0 <= i__1 ? i__1 : 
		    s_rnge("hlpmsg", i__1, "clcomm_", (ftnlen)342)) * 80, (
		    ftnlen)80);
	}
	byebye_("FAILURE", (ftnlen)7);
    }

/*     If we have one word, it should be the name of a kernel file; if */
/*     two words, the first one should be the name of a kernel file. */

    if (nwords > 0) {

/*        We should have at least the kernel file. */

	nextwd_(tmplin, kerfnm, tmplin, (ftnlen)512, (ftnlen)128, (ftnlen)512)
		;

/*        Check to see if it exists. */

	if (! exists_(kerfnm, (ftnlen)128)) {
	    setmsg_("The SPICE kernel file '#' does not exist.", (ftnlen)41);
	    errch_("#", kerfnm, (ftnlen)1, (ftnlen)128);
	    sigerr_("SPICE(FILEDOESNOTEXIST)", (ftnlen)23);
	}

/*        Get the file architecture and type, which we do not use, for */
/*        the kernel file. */

	getfat_(kerfnm, arch, type__, (ftnlen)128, (ftnlen)3, (ftnlen)4);
	if (s_cmp(arch, "?", (ftnlen)3, (ftnlen)1) == 0) {
	    setmsg_("The architecture of the file '#' could not be determine"
		    "d.", (ftnlen)57);
	    errch_("#", kerfnm, (ftnlen)1, (ftnlen)128);
	    sigerr_("SPICE(BADFILEFORMAT)", (ftnlen)20);
	} else if (s_cmp(arch, "DAF", (ftnlen)3, (ftnlen)3) != 0 && s_cmp(
		arch, "DAS", (ftnlen)3, (ftnlen)3) != 0) {
	    setmsg_("The file '#' was not a SPICE kernel file. ", (ftnlen)42);
	    errch_("#", kerfnm, (ftnlen)1, (ftnlen)128);
	    sigerr_("SPICE(IMPROPERFILE)", (ftnlen)19);
	}
    }

/*     Set the option. */

    s_copy(option, optnam + (((i__1 = idxopt - 1) < 5 && 0 <= i__1 ? i__1 : 
	    s_rnge("optnam", i__1, "clcomm_", (ftnlen)395)) << 4), (ftnlen)16,
	     (ftnlen)16);

/*     Perform the action based on the option selected. */

    if (s_cmp(option, "ADD_COMMENTS", (ftnlen)16, (ftnlen)12) == 0) {

/*        The number of words should be 2; the first is the name of the */
/*        SPICE kernel file, which we got from the command line above, */
/*        and the second is the name of the file containing the comments */
/*        to be added to the kernel file. */

	if (nwords != 2) {
	    setmsg_("Usage is: clcommnt -a <kernel file> <comment file>", (
		    ftnlen)50);
	    errch_("#", usage, (ftnlen)1, (ftnlen)80);
	    sigerr_("SPICE(USAGEERROR)", (ftnlen)17);
	}

/*        We have one word remaining, so lets assume that it is the */
/*        filename of the comment file. */

	nextwd_(tmplin, comfnm, tmplin, (ftnlen)512, (ftnlen)128, (ftnlen)512)
		;

/*        The comment file should exist. */

	if (! exists_(comfnm, (ftnlen)128)) {
	    setmsg_("The file of comments '#' to be added to the SPICE kerne"
		    "l file '#' does not exist.", (ftnlen)81);
	    errch_("#", comfnm, (ftnlen)1, (ftnlen)128);
	    errch_("#", kerfnm, (ftnlen)1, (ftnlen)128);
	    sigerr_("SPICE(FILEDOESNOTEXIST)", (ftnlen)23);
	}

/*        Now we get to work. */

/*        We open the comment file. */

	txtopr_(comfnm, &comlun, (ftnlen)128);

/*        Based on the architecture, we open the kernel file, add the */
/*        comments, and close the kernel file. */

	if (s_cmp(arch, "DAF", (ftnlen)3, (ftnlen)3) == 0) {
	    dafopw_(kerfnm, &kerhan, (ftnlen)128);
	    spcac_(&kerhan, &comlun, " ", " ", (ftnlen)1, (ftnlen)1);
	    dafcls_(&kerhan);
	} else if (s_cmp(arch, "DAS", (ftnlen)3, (ftnlen)3) == 0) {
	    dasopw_(kerfnm, &kerhan, (ftnlen)128);
	    dasacu_(&comlun, " ", " ", &c_true, &kerhan, (ftnlen)1, (ftnlen)1)
		    ;
	    dasllc_(&kerhan);
/*            CALL DASCLS ( KERHAN ) */
	}

/*        Close the comment file. */

	cl__1.cerr = 0;
	cl__1.cunit = comlun;
	cl__1.csta = 0;
	f_clos(&cl__1);
    } else if (s_cmp(option, "DELETE_COMMENTS ", (ftnlen)16, (ftnlen)16) == 0)
	     {

/*        The number of words should be 1. It is the name of the SPICE */
/*        kernel file, which we got from the command line above. */

	if (nwords != 1) {
	    setmsg_("Usage is: clcommnt -d <kernel file>", (ftnlen)35);
	    errch_("#", usage, (ftnlen)1, (ftnlen)80);
	    sigerr_("SPICE(USAGEERROR)", (ftnlen)17);
	}

/*        We simply get to work. */

/*        Based on the architecture, we open the kernel file, delete */
/*        all of the comments, and close the kernel file. */

	if (s_cmp(arch, "DAF", (ftnlen)3, (ftnlen)3) == 0) {
	    dafopw_(kerfnm, &kerhan, (ftnlen)128);
	    dafdc_(&kerhan);
	    dafcls_(&kerhan);
	} else if (s_cmp(arch, "DAS", (ftnlen)3, (ftnlen)3) == 0) {
	    dasopw_(kerfnm, &kerhan, (ftnlen)128);
	    dasdc_(&kerhan);
	    dasllc_(&kerhan);
/*            CALL DASCLS ( KERHAN ) */
	}
    } else if (s_cmp(option, "EXTRACT_COMMENTS", (ftnlen)16, (ftnlen)16) == 0)
	     {

/*        The number of words should be 2; the first is the name of the */
/*        SPICE kernel file, which we got from the command line above, */
/*        and the second is the name of the file to which comments from */
/*        the kernel file are to be extracted.. */

	if (nwords != 2) {
	    setmsg_("Usage is: clcommnt -e <kernel file> <comment file>", (
		    ftnlen)50);
	    errch_("#", usage, (ftnlen)1, (ftnlen)80);
	    sigerr_("SPICE(USAGEERROR)", (ftnlen)17);
	}

/*        We have one word remaining, so lets assume that it is the */
/*        filename of the comment file. */

	nextwd_(tmplin, comfnm, tmplin, (ftnlen)512, (ftnlen)128, (ftnlen)512)
		;

/*        The comment file should exist. */

	if (exists_(comfnm, (ftnlen)128)) {
	    setmsg_("The file '#' already exists. Please use a new filename "
		    "for the file which will contain the comments extracted f"
		    "rom the '#'.", (ftnlen)123);
	    errch_("#", comfnm, (ftnlen)1, (ftnlen)128);
	    errch_("#", kerfnm, (ftnlen)1, (ftnlen)128);
	    sigerr_("SPICE(FILEALREADYEXISTS)", (ftnlen)24);
	}

/*        Now we get to work. */

/*        We open the comment file. */

	txtopn_(comfnm, &comlun, (ftnlen)128);

/*        Based on the architecture, we open the kernel file, extract the */
/*        comments, and close the kernel file. */

	gotsom = FALSE_;
	if (s_cmp(arch, "DAF", (ftnlen)3, (ftnlen)3) == 0) {
	    dafopr_(kerfnm, &kerhan, (ftnlen)128);
	    dafecu_(&kerhan, &comlun, &gotsom);
	    dafcls_(&kerhan);
	} else if (s_cmp(arch, "DAS", (ftnlen)3, (ftnlen)3) == 0) {
	    dasopr_(kerfnm, &kerhan, (ftnlen)128);
	    dasecu_(&kerhan, &comlun, &gotsom);
	    dascls_(&kerhan);
	}

/*        If there were no comments in the file, write a message saying */
/*        that to the comment file and the screen. */

	if (! gotsom) {
	    s_copy(tmplin, "There were no comments in the file '#'.", (ftnlen)
		    512, (ftnlen)39);
	    repmc_(tmplin, "#", kerfnm, tmplin, (ftnlen)512, (ftnlen)1, (
		    ftnlen)128, (ftnlen)512);
	    writln_(" ", &c__6, (ftnlen)1);
	    writln_(tmplin, &c__6, (ftnlen)512);
	    writln_(" ", &c__6, (ftnlen)1);
	    writln_(" ", &comlun, (ftnlen)1);
	    writln_(tmplin, &comlun, (ftnlen)512);
	    writln_(" ", &comlun, (ftnlen)1);
	}

/*        Close the comment file. */

	cl__1.cerr = 0;
	cl__1.cunit = comlun;
	cl__1.csta = 0;
	f_clos(&cl__1);
    } else if (s_cmp(option, "HELP ", (ftnlen)16, (ftnlen)5) == 0) {
	writln_(" ", &c__6, (ftnlen)1);
	writla_(&c__18, hlpmsg, &c__6, (ftnlen)80);
	writln_(" ", &c__6, (ftnlen)1);
    } else if (s_cmp(option, "READ_COMMENTS ", (ftnlen)16, (ftnlen)14) == 0) {

/*        The number of words should be 1. It is the name of the SPICE */
/*        kernel file, which we got from the command line above. */

	if (nwords != 1) {
	    setmsg_("Usage is: clcommnt -r <kernel file>", (ftnlen)35);
	    errch_("#", usage, (ftnlen)1, (ftnlen)80);
	    sigerr_("SPICE(USAGEERROR)", (ftnlen)17);
	}

/*        We simply get to work. */

/*        Based on the architecture, we open the kernel file, read */
/*        (extract to standard output) all of the comments, and close the */
/*        kernel file. */

	gotsom = FALSE_;
	writln_(" ", &c__6, (ftnlen)1);
	if (s_cmp(arch, "DAF", (ftnlen)3, (ftnlen)3) == 0) {
	    dafopr_(kerfnm, &kerhan, (ftnlen)128);
	    dafecu_(&kerhan, &c__6, &gotsom);
	    dafcls_(&kerhan);
	} else if (s_cmp(arch, "DAS", (ftnlen)3, (ftnlen)3) == 0) {
	    dasopr_(kerfnm, &kerhan, (ftnlen)128);
	    dasecu_(&kerhan, &c__6, &gotsom);
	    dascls_(&kerhan);
	}

/*        If there were no comments in the file, write a message saying */
/*        that to the comment file and the screen. */

	if (! gotsom) {
	    s_copy(tmplin, "There were no comments in the file '#'.", (ftnlen)
		    512, (ftnlen)39);
	    repmc_(tmplin, "#", kerfnm, tmplin, (ftnlen)512, (ftnlen)1, (
		    ftnlen)128, (ftnlen)512);
	    writln_(tmplin, &c__6, (ftnlen)512);
	}
	writln_(" ", &c__6, (ftnlen)1);
    }
    if (failed_()) {

/*     If we failed, reset the error handling and delete the binary file */
/*     that we were creating if it exists. */

	reset_();
	scardi_(&c__0, opnset);
	cleari_(&c__1, &opnset[6]);

/*        Get the handles for any DAF files which may still be */
/*        open and close them. */

	dafhof_(opnset);
	numopn = cardi_(opnset);
	if (numopn > 0) {
	    i__1 = numopn;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		dafcls_(&opnset[(i__2 = i__ + 5) < 7 && 0 <= i__2 ? i__2 : 
			s_rnge("opnset", i__2, "clcomm_", (ftnlen)637)]);
	    }
	}

/*        Clear out any binary file handles in the open set, */
/*        OPNSET. */

	scardi_(&c__0, opnset);
	cleari_(&c__1, &opnset[6]);

/*        Get the handles for any DAS files which may still be */
/*        open and close them. */

	dashof_(opnset);
	numopn = cardi_(opnset);
	if (numopn > 0) {
	    i__1 = numopn;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		dascls_(&opnset[(i__2 = i__ + 5) < 7 && 0 <= i__2 ? i__2 : 
			s_rnge("opnset", i__2, "clcomm_", (ftnlen)658)]);
	    }
	}

/*        Call RESET one more time just in case there was an */
/*        error closing of deleting the file. */

	reset_();
    }

/*     If we make it this far we are done.... */

    chkout_("CLCOMMNT", (ftnlen)8);
    byebye_("SUCCESS", (ftnlen)7);
    return 0;
} /* clcomm_ */

