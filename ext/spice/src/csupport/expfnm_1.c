/* expfnm_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure      EXPFNM_1 ( Expand a filename ) */
/* Subroutine */ int expfnm_1__(char *infil, char *outfil, ftnlen infil_len, 
	ftnlen outfil_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen);

    /* Local variables */
    integer need, keep;
    char word[255];
    integer blank;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    integer inlen, slash;
    extern integer rtrim_(char *, ftnlen);
    integer dirlen;
    extern /* Subroutine */ int getenv_(char *, char *, ftnlen, ftnlen);
    integer wrdlen;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), repsub_(char *, integer *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen), setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    integer outlen;
    extern logical return_(void);
    char dir[255];
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);

/* $ Abstract */

/*     Given a filename, expand it to be a full filename. */

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

/*     FILES */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     INFIL      I   The filename to be expanded. */
/*     OUTFIL     O   The expanded filename. */

/* $ Detailed_Input */

/*     INFIL      is the filename to be expanded. */

/* $ Detailed_Output */

/*     OUTFIL     is the expanded filename. If no expansion could be */
/*                done, the value of OUTFIL is equal to the value of */
/*                INFIL. OUTFIL may not overwrite INFIL. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the input filename is blank, begins with blank characters, */
/*        or has embedded blanks in it, the error SPICE(BADFILENAME) */
/*        is signalled. */

/*     2) If the expanded filename is too long to fit into the */
/*        output string, the error SPICE(STRINGTOOSMALL) is signalled. */

/*     3) The output string may not overwrite the input string. */

/*     4) If no expansion of the input filename can be done, the */
/*        output filename is assigned the value of the input filename. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The input filename may not be blank, begin with blank characters, */
/*     nor may it it contain embedded blanks. As a general rule, */
/*     SPICELIB routines do not allow blank characters as part of a */
/*     filename. */

/*     Unix platforms: */

/*     On the Unix platforms, a filename containing an environment */
/*     variable must be expanded completely before FORTRAN can do */
/*     anything with it. FORTRAN interacts directly with the kernel, and */
/*     as a result does not pass input filenames through the shell */
/*     for expansion of environment variables. */

/*     VAX/VMS, Alpha/OpenVMS platforms: */

/*     The operating system does filname expansion itself, so this */
/*     routine currently does not expand the name. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     Unix platforms: */

/*     This routine cannot be used to expand a file name whose form */
/*     is '~xxx', where xxx is an account name. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     H.A. Neilan    (JPL) */

/* $ Version */

/* -    Beta Version 3.20.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    Beta Version 3.19.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-CC_C. */

/* -    Beta Version 3.18.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C. */

/* -    Beta Version 3.17.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    Beta Version 3.16.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-WINDOWS-64BIT-IFORT. */

/* -    Beta Version 3.15.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    Beta Version 3.14.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-64BIT-MS_C. */

/* -    Beta Version 3.13.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-INTEL_C. */

/* -    Beta Version 3.12.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    Beta Version 3.11.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    Beta Version 3.10.0, 18-MAR-2009 (BVS) */

/*        Updated for PC-LINUX-GFORTRAN. */

/* -    Beta Version 3.9.0, 18-MAR-2009 (BVS) */

/*        Updated for MAC-OSX-GFORTRAN. */

/* -    Beta Version 3.8.0, 19-FEB-2008 (BVS) */

/*        Updated for PC-LINUX-IFORT. */

/* -    Beta Version 3.7.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-LINUX-64BIT-GCC_C. */

/* -    Beta Version 3.6.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-INTEL_C. */

/* -    Beta Version 3.5.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-IFORT. */

/* -    Beta Version 3.4.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-WINDOWS-IFORT. */

/* -    Beta Version 3.3.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    Beta Version 3.2.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN_C. */

/* -    Beta Version 3.1.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    Beta Version 3.0.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    Beta Version 3.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are WIN-NT */

/* -    Beta Version 3.0.3, 21-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    Beta Version 3.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    Beta Version 3.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    Beta Version 3.0.0, 05-APR-1998 (NJB) */

/*        Added references to the PC-LINUX environment. */

/* -    Beta Version 2.1.0, 5-JAN-1995 (HAN) */

/*        Removed Sun Solaris environment since it is now the same */
/*        as the Sun OS 4.1.x environment. */
/*        Removed DEC Alpha/OpenVMS environment since it is now the */
/*        same as the VAX environment. */

/* -    Beta Version 2.0.0, 08-JUL-1994 (HAN) */

/*        The capability of resolving a Unix filename that contains */
/*        an environment variable directory specificiation plus a */
/*        filename has been added. */

/* -    Beta Version 1.0.0, 06-APR-1992 (HAN) */

/* -& */
/* $ Index_Entries */

/*     expand a filename */

/* -& */

/*     SPICELIB functions */


/*     Parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EXPFNM_1", (ftnlen)8);
    }

/*     If the input filename is blank, that's an error. */

    if (s_cmp(infil, " ", infil_len, (ftnlen)1) == 0) {
	setmsg_("The input filename '#' was blank.", (ftnlen)33);
	errch_("#", infil, (ftnlen)1, infil_len);
	sigerr_("SPICE(BADFILENAME)", (ftnlen)18);
	chkout_("EXPFNM_1", (ftnlen)8);
	return 0;
    }

/*     If there are blanks anywhere in the filename, SPICELIB */
/*     considers the filename to be invalid. */

    blank = pos_(infil, " ", &c__1, rtrim_(infil, infil_len), (ftnlen)1);
    if (blank != 0) {
	setmsg_("The input filename '#' had blank characters in it.", (ftnlen)
		50);
	errch_("#", infil, (ftnlen)1, infil_len);
	sigerr_("SPICE(BADFILENAME)", (ftnlen)18);
	chkout_("EXPFNM_1", (ftnlen)8);
	return 0;
    }

/*     Look for a slash in the filename. */

    slash = pos_(infil, "/", &c__1, infil_len, (ftnlen)1);

/*     If we found a slash in a position other than the first */
/*     character position, we want to examine the word that */
/*     comes before it just in case it is an environment */
/*     variable. */

    if (slash > 1) {
	s_copy(word, infil, (ftnlen)255, slash - 1);
	getenv_(word, dir, (ftnlen)255, (ftnlen)255);

/*        If the word was an environment variable, then construct */
/*        the expanded filename. If it wasn't, just return the original */
/*        input filename. */

	if (s_cmp(dir, " ", (ftnlen)255, (ftnlen)1) != 0) {
	    s_copy(outfil, infil, outfil_len, infil_len);
	    inlen = rtrim_(infil, infil_len);
	    wrdlen = rtrim_(word, (ftnlen)255);
	    dirlen = rtrim_(dir, (ftnlen)255);
	    outlen = i_len(outfil, outfil_len);
	    keep = inlen - wrdlen;
	    need = keep + dirlen;

/*           If the output filename length is not long enough for */
/*           the substitution, signal an error. Otherwise, substitute */
/*           in the new value. */

	    if (need > outlen) {
		setmsg_("The expanded filename for the input filename '#' ex"
			"ceeded the length of the output filename. The expand"
			"ed name was # characters too long.", (ftnlen)137);
		errch_("#", infil, (ftnlen)1, infil_len);
		i__1 = need - outlen;
		errint_("#", &i__1, (ftnlen)1);
		sigerr_("SPICE(STRINGTOOSMALL)", (ftnlen)21);
		chkout_("EXPFNM_1", (ftnlen)8);
		return 0;
	    } else {
		i__1 = slash - 1;
		repsub_(infil, &c__1, &i__1, dir, outfil, infil_len, rtrim_(
			dir, (ftnlen)255), outfil_len);
	    }
	} else {
	    s_copy(outfil, infil, outfil_len, infil_len);
	}
    } else {

/*        No slashes are in the filename, so it's just an easy case. */

/*        It's possible that the entire filename is an environment */
/*        variable. If it's not, then just return the input filename. */

	getenv_(infil, outfil, infil_len, outfil_len);
	if (s_cmp(outfil, " ", outfil_len, (ftnlen)1) == 0) {
	    s_copy(outfil, infil, outfil_len, infil_len);
	}
    }
    chkout_("EXPFNM_1", (ftnlen)8);
    return 0;
} /* expfnm_1__ */

