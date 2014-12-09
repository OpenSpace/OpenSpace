/* rdcmd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__20 = 20;
static integer c__2 = 2;
static integer c__5 = 5;
static integer c__3 = 3;
static integer c__1 = 1;
static integer c__0 = 0;

/* $Procedure      RDCMD (Read command file) */
/* Subroutine */ int rdcmd_(char *cmdfil, char *cmdsym, integer *cmdptr, char 
	*cmdval, ftnlen cmdfil_len, ftnlen cmdsym_len, ftnlen cmdval_len)
{
    /* Initialized data */

    static char kwds1[32*2] = "LEAPSECONDS_KERNEL  1  1        " "SPK_KERNEL"
	    "          1  1000     ";
    static char kwds2[32*5] = "SOURCE_SPK_KERNEL   1  1000     " "LOG_FILE  "
	    "          0  1        " "BODIES              0  1        " "BEGI"
	    "N_TIME          0  1000     " "INCLUDE_TEXT_FILE   0  1000     ";
    static char kwds3[32*3] = "BODIES              0  1        " "BEGIN_TIME"
	    "          0  1000     " "INCLUDE_COMMENTS    0  1        ";
    static char kwds4[32*1] = "END_TIME            1  1        ";

    /* System generated locals */
    cilist ci__1;

    /* Builtin functions */
    integer s_rsfe(cilist *), do_fio(integer *, char *, ftnlen), e_rsfe(void);

    /* Local variables */
    static char line[350];
    static integer unit;
    extern /* Subroutine */ int chkin_(char *, ftnlen), cperr_(char *, 
	    integer *, ftnlen), repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    static char tabval[32*26];
    extern /* Subroutine */ int evalcp_(char *, logical *, char *, integer *, 
	    char *, logical *, ftnlen, ftnlen, ftnlen), initcp_(char *, 
	    integer *, char *, char *, ftnlen, ftnlen, ftnlen);
    static char reason[160];
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    static integer tabptr[26];
    extern /* Subroutine */ int prefix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    static char tabsym[32*26];
    extern /* Subroutine */ int ssizec_(integer *, char *, ftnlen), chkout_(
	    char *, ftnlen);
    static integer linnum, iostat;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), ssizei_(integer *, 
	    integer *);
    extern logical return_(void);
    extern /* Subroutine */ int syputc_(char *, char *, integer *, char *, 
	    integer *, char *, ftnlen, ftnlen, ftnlen, ftnlen), txtopr_(char *
	    , integer *, ftnlen);
    static logical eof, err;

/* $ Abstract */

/*     Parse the command file. */

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
/*     CMDFIL     I   Name of command file. */
/*     CMDSYM, */
/*     CMDPTR, */
/*     CMDVAL     O   Command symbol table. */

/* $ Detailed_Input */

/*     CMDFIL     is the name of the command file. */

/* $ Detailed_Output */

/*     CMDSYM, */
/*     CMDPTR, */
/*     CMDVAL     is the command symbol table. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) An error is signaled if the file cannot be parsed */
/*        successfully. */

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

/*     M.J. Spencer   (JPL) */

/* $ Version */

/* -    Beta Version 1.1.0, 17-JAN-2014 (BVS) */

/*        Increased LINLEN from 120 to 350 (350 = 300 characters for */
/*        value consistent with VALLEN in CPARSE_2 and the main program */
/*        + 50 more characters for the keyword name, =, and blanks.) */

/*        Increased maximum counts of child values in KWDS* from 300 to */
/*        1000 for all values. */

/*        Saved all variables. */

/* -    Beta Version 1.0.0, 26-JAN-1994 (MJS) */

/* -& */

/*     SPICELIB functions */


/*     Other functions */


/*     Local parameters */


/*     Local variables */


/*     Save all. */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("RDCMD", (ftnlen)5);
    }

/*     Initialize the parser. */

    ssizec_(&c__20, tabsym, (ftnlen)32);
    ssizei_(&c__20, tabptr);
    ssizec_(&c__20, tabval, (ftnlen)32);
    syputc_("HEAD", kwds1, &c__2, tabsym, tabptr, tabval, (ftnlen)4, (ftnlen)
	    32, (ftnlen)32, (ftnlen)32);
    syputc_("SPK_KERNEL", kwds2, &c__5, tabsym, tabptr, tabval, (ftnlen)10, (
	    ftnlen)32, (ftnlen)32, (ftnlen)32);
    syputc_("SOURCE_SPK_KERNEL", kwds3, &c__3, tabsym, tabptr, tabval, (
	    ftnlen)17, (ftnlen)32, (ftnlen)32, (ftnlen)32);
    syputc_("BEGIN_TIME", kwds4, &c__1, tabsym, tabptr, tabval, (ftnlen)10, (
	    ftnlen)32, (ftnlen)32, (ftnlen)32);
    initcp_(tabsym, tabptr, tabval, "HEAD", (ftnlen)32, (ftnlen)32, (ftnlen)4)
	    ;

/*     Open the command file, and parse its contents */

    txtopr_(cmdfil, &unit, cmdfil_len);
    eof = FALSE_;
    err = FALSE_;
    while(! eof && ! err) {
	ci__1.cierr = 1;
	ci__1.ciend = 1;
	ci__1.ciunit = unit;
	ci__1.cifmt = "(A)";
	iostat = s_rsfe(&ci__1);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_fio(&c__1, line, (ftnlen)350);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = e_rsfe();
L100001:
	eof = iostat != 0;
	evalcp_(line, &eof, cmdsym, cmdptr, cmdval, &err, (ftnlen)350, 
		cmdsym_len, cmdval_len);
    }
    if (err) {
	cperr_(reason, &linnum, (ftnlen)160);
	repmi_(reason, "#", &linnum, reason, (ftnlen)160, (ftnlen)1, (ftnlen)
		160);
	prefix_(":", &c__1, reason, (ftnlen)1, (ftnlen)160);
	prefix_(cmdfil, &c__0, reason, cmdfil_len, (ftnlen)160);
	setmsg_(reason, (ftnlen)160);
	sigerr_("SPICE(CMDPARSEERROR)", (ftnlen)20);
	chkout_("RDCMD", (ftnlen)5);
	return 0;
    }
    chkout_("RDCMD", (ftnlen)5);
    return 0;
} /* rdcmd_ */

