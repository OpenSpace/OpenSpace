/* inspekt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Main program */ MAIN__(void)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char versn[80], logfil[127];
    extern /* Subroutine */ int byebye_(char *, ftnlen), setlan_(char *, 
	    ftnlen), setedt_(char *, ftnlen), cmloop_(char *, char *, char *, 
	    char *, U_fp, U_fp, U_fp, ftnlen, ftnlen, ftnlen, ftnlen);
    extern /* Subroutine */ int preprc_(), inspkn_(), nspint_();

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

/* $ Abstract */

/*     This is the main module for the program Inspekt.  Only very */
/*     high level actions are taken here.  Among these are */

/*        1)  The choice of language that will be used during */
/*            the program (ENGLISH, FRENCH, GERMAN, etc.) */

/*        2)  The name of the log file for the program */

/*        3)  Setting the choice of command line editor. */

/*        4)  Program name and version number. */

/*        5)  A choice of greeting, command preprocessor and */
/*            the actual work-horse of the program INSPKN which */
/*            handles the parsing and actions associated with */
/*            all user commands. */

/*     This version of the program was adapted from the software */
/*     written by Bill Taber in February of 1993 */

/*     The primary author is Bill Taber (NAIF - JPL) */

/*     Additional support was given by Hester Neilan and Nat Bachman */


/* $ Version */

/* -    Inspekt Version 7.48.0, 10-MAR-2014 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-INTEL. */

/* -    Inspekt Version 7.47.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-LINUX-64BIT-IFORT. */

/* -    Inspekt Version 7.46.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-GFORTRAN. */

/* -    Inspekt Version 7.45.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GFORTRAN. */

/* -    Inspekt Version 7.44.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GCC_C. */

/* -    Inspekt Version 7.43.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    Inspekt Version 7.42.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-CC_C. */

/* -    Inspekt Version 7.41.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C. */

/* -    Inspekt Version 7.40.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    Inspekt Version 7.39.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-WINDOWS-64BIT-IFORT. */

/* -    Inspekt Version 7.38.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    Inspekt Version 7.37.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-64BIT-MS_C. */

/* -    Inspekt Version 7.36.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-INTEL_C. */

/* -    Inspekt Version 7.35.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    Inspekt Version 7.34.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    Inspekt Version 7.33.0, 18-MAR-2009 (BVS) */

/*        Updated for PC-LINUX-GFORTRAN. */

/* -    Inspekt Version 7.32.0, 18-MAR-2009 (BVS) */

/*        Updated for MAC-OSX-GFORTRAN. */

/* -    Inspekt Version 7.31.0, 19-FEB-2008 (BVS) */

/*        Updated for PC-LINUX-IFORT. */

/* -    Inspekt Version 7.30.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-LINUX-64BIT-GCC_C. */

/* -    Inspekt Version 7.29.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-INTEL_C. */

/* -    Inspekt Version 7.28.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-IFORT. */

/* -    Inspekt Version 7.27.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-WINDOWS-IFORT. */

/* -    Inspekt Version 7.26.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    Inspekt Version 7.25.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN_C. */

/* -    Inspekt Version 7.24.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    Inspekt Version 7.23, 31-MAR-2003 (WLT) */

/*        Added the DELIMITED formats. */

/* -    Inspekt Version 7.22, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    Inspekt Version 7.21, 03-NOV-2000 (EDW) */

/*        Added a BYEBYE( 'SUCCESS' ) call at program's end. */
/*        Included the PROGRAM INSPEKT declaration and IMPLICIT NONE. */
/*        Code now meets SPICE delivery specifications. */

/* -    Inspekt Version 7.20, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    Inspekt Version 7.19, 20-SEP-1999 (NJB) */

/*        CSPICE environment lines were added.  Some typos were */
/*        corrected. */

/* -    Inspekt Version 7.18  28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    Inspekt Version 7.17  18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    Inspekt Version 7.16  09-APR-1998 (NJB) */

/*        April 9, 1998 (NJB) */
/*        Added reference to the PC-LINUX environment. */

/* -    Inspekt Version 7.15 */

/*        October 9, 1997 */
/*        Reorganized the initialization portion of online help */
/*        to overcome compiler problems with Windows 95 and Windows NT. */


/* -    Inspekt Version 7.14 */

/*        May 23, 1997 */
/*        Fixed a number of small bugs (many pedantic) that */
/*        were present in the previous version. */

/* -    Inspekt Version 7.13 */

/*        October 30, 1996 */
/*        Upgraded to fully support the MAC environment. (WLT) */

/* -    Inspekt Version 7.12 */

/*        Fixed a bug in TABRPT (see Beta Version 3.0.0 of that routine */
/*        for details.  Also corrected a diagnostic message that */
/*        is issued when the symbol name is too long. (WLT) */

/* -    Inspekt Version 7.11.1 */

/*        Added DEC-OSF1 to the list of supported environments */
/*        12-AUG-1996 (WLT) */

/* -    Inspekt Version 7.11 */

/*        Set the default editor for SGI to emacs.  Added "support" */
/*        for Macintosh.  Note editing commands is disabled for the */
/*        MAC.  Also NSPIO is now portable.  No change in functionality */
/*        just a slight re-implementation of that routine. */

/*        A number of errors in Help text have been corrected. */

/* -    Inspekt Version 7.10 */

/*        Changed the default editor for the PC to 'edit' for both */
/*        PC-LAHEY and PC-MS */

/* -    Inspekt Version 7.10   5-Dec-1995 */

/*        Fixed a bug in the routine EDTCOM */

/* -    Inspekt Version 7.08   21-Nov-1995 */

/*        Upgraded Inspekt to use the new command loop capabilities. */

/* -    Inspekt Version 7.07   21-Nov-1995 */

/*        Fixed problem with column/table name expansion in queries. */

/* -    Inspekt Version 7.06,  21-NOV-1995 */

/*        Modified the structure of the help text file hlptxt.f */

/* -    Inspekt Version 1.1.0, 1-JUN-1995 (HAN) */

/*        Created master source file. */

/* -    Inspekt Version 1.0.0, 14-APR-1994 (BLT) */

/* -& */
    s_copy(logfil, "nsp{0-9}{0-9}{0-9}{0-9}{0-9}.log", (ftnlen)127, (ftnlen)
	    32);
    s_copy(versn, "                  Inspekt --- Version 7.23", (ftnlen)80, (
	    ftnlen)42);
    setlan_("ENGLISH", (ftnlen)7);
    setedt_("edit", (ftnlen)4);
    cmloop_(";", "Inspekt>", logfil, versn, (U_fp)nspint_, (U_fp)preprc_, (
	    U_fp)inspkn_, (ftnlen)1, (ftnlen)8, (ftnlen)127, (ftnlen)80);
    byebye_("SUCCESS", (ftnlen)7);
    return 0;
} /* MAIN__ */

/* Main program alias */ int inspekt_ () { MAIN__ (); return 0; }
