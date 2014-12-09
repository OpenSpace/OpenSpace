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
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer blank;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);
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

/* -    Beta Version 3.25.0, 10-MAR-2014 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-INTEL. */

/* -    Beta Version 3.24.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-LINUX-64BIT-IFORT. */

/* -    Beta Version 3.23.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-GFORTRAN. */

/* -    Beta Version 3.22.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GFORTRAN. */

/* -    Beta Version 3.21.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GCC_C. */

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

/*     Because the operating system takes care of any necessary filename */
/*     translations, just return the input filename. */

    s_copy(outfil, infil, outfil_len, infil_len);
    chkout_("EXPFNM_1", (ftnlen)8);
    return 0;
} /* expfnm_1__ */

