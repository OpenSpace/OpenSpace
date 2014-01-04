/* zzgetenv.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZGETENV ( Get environment variable value. ) */
/* Subroutine */ int zzgetenv_(char *envvar, char *value, ftnlen envvar_len, 
	ftnlen value_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int getenv_(char *, char *, ftnlen, ftnlen), 
	    chkout_(char *, ftnlen);
    char myvalu[255];
    extern logical return_(void);

/* $ Abstract */

/*     Get the value of a specified environment variable or VAX DCL */
/*     symbol, if it exists. */

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

/*     Length of an environment variable or DCL symbol name. */


/*     Length of an environment variable or DCL symbol value. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ENVVAR     I   The name of the environment variable or symbol. */
/*     VALUE      O   The value of the environment variable or symbol. */
/*     ENVLEN     P   Maximum length of an environemt variable or symbol. */
/*     VALLEN     P   Maximum length of a value. */

/* $ Detailed_Input */

/*     ENVVAR   This is the name of the environment variable, or DCL */
/*              symbol, whose value is desired. The significant, i.e., */
/*              nonblank, portion of the environment variable, or DCL */
/*              symbol, name may be at most ENVLEN characters in length */
/*              and may not contain embedded blanks. */

/*              A standard convention used for naming environment */
/*              variables is to use only the upper case characters */
/*              'A' - 'Z', the digits '0' - '9', and the underscore */
/*              character '_', in the names. We do not enforce this */
/*              convention but we strongly recommend its use for */
/*              interface consistency across heterogeneous computing */
/*              environments. */

/*              For a particular operating system and compiler the */
/*              maximum allowed length of an environment variable name */
/*              may be less than ENVLEN. Consult the appropriate */
/*              operating system and/or compiler manuals for details. */

/* $ Detailed_Output */

/*     VALUE    This is the value obtained for the environment variable */
/*              ENVVAR if it is defined. The result will be left */
/*              justified on output. */

/*              If any of the following are true: */

/*                 1) a value for the environment variable cannot be */
/*                    obtained, */

/*                 2) the significant portion of ENVVAR contains */
/*                    embedded blanks, */

/*                 3) the input ENVVAR is blank, */

/*                 4) The input ENVVAR contains characters other than */
/*                    the upper case characters 'A' - 'Z', the digits */
/*                    '0' - '9', and the underscore '_', */

/*                 5) The value for the environment variable is too long */
/*                    to fit in the available space, */

/*              then VALUE will be blank. */

/* $ Parameters */

/*     ENVLEN   The maximum allowed length of an environment variable */
/*              or DCL symbol name. */

/*     VALLEN   The maximum allowed length of an environment variable */
/*              or DCL symbol value. */

/* $ Exceptions */

/*     None. */

/*     1) If a value for the environment variable cannot be obtained, */
/*        a blank string will be returned. */

/*     2) If the significant portion of ENVVAR contains embedded blanks, */
/*        a blank string will be returned. */

/*     3) If the input ENVVAR is blank, a blank string will be returned. */

/*     4) If the value for the environment variable is too long to fit */
/*        in the available space, a blank string will be returned. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Call the subroutine 'GETENV( ENVVAR, VALUE )', provided for */
/*     UNIX compatibility. Given the name of an environment variable, */
/*     this subroutine storing in VALUE the value of the specified */
/*     environment variable or a blank string if an error occurs. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer     (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.20.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    SPICELIB Version 2.19.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-CC_C. */

/* -    SPICELIB Version 2.18.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C. */

/* -    SPICELIB Version 2.17.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    SPICELIB Version 2.16.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-WINDOWS-64BIT-IFORT. */

/* -    SPICELIB Version 2.15.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 2.14.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-64BIT-MS_C. */

/* -    SPICELIB Version 2.13.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-INTEL_C. */

/* -    SPICELIB Version 2.12.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    SPICELIB Version 2.11.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 2.10.0, 18-MAR-2009 (BVS) */

/*        Updated for PC-LINUX-GFORTRAN. */

/* -    SPICELIB Version 2.9.0, 18-MAR-2009 (BVS) */

/*        Updated for MAC-OSX-GFORTRAN. */

/* -    SPICELIB Version 2.8.0, 19-FEB-2008 (BVS) */

/*        Updated for PC-LINUX-IFORT. */

/* -    SPICELIB Version 2.7.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-LINUX-64BIT-GCC_C. */

/* -    SPICELIB Version 2.6.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-INTEL_C. */

/* -    SPICELIB Version 2.5.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-IFORT. */

/* -    SPICELIB Version 2.4.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-WINDOWS-IFORT. */

/* -    SPICELIB Version 2.3.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    SPICELIB Version 2.2.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN_C. */

/* -    SPICELIB Version 2.1.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    SPICELIB Version 2.0.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 2.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 2.0.3, 21-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    Beta Version 2.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    Beta Version 2.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    Beta Version 2.0.0, 05-APR-1998 (NJB) */

/*        Added the PC-LINUX environment. */

/* -    Beta Version 1.0.0, 31-MAY-1996 (KRG) */

/* -& */
/* $ Index_Entries */

/*      get environment variable value */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZGETENV", (ftnlen)8);
    }

/*     We do three things: */

/*        1) Check to see if the input is blank. */
/*        2) Attempt to get the value. */
/*        3) If we got a nonblank value, see if it will fit in the */
/*           space provided. */

    if (s_cmp(envvar, " ", envvar_len, (ftnlen)1) == 0) {
	s_copy(myvalu, " ", (ftnlen)255, (ftnlen)1);
    } else {
	getenv_(envvar, myvalu, envvar_len, (ftnlen)255);
	if (s_cmp(myvalu, " ", (ftnlen)255, (ftnlen)1) != 0) {
	    if (rtrim_(myvalu, (ftnlen)255) > i_len(value, value_len)) {
		s_copy(myvalu, " ", (ftnlen)255, (ftnlen)1);
	    }
	}
    }
    s_copy(value, myvalu, value_len, (ftnlen)255);
    chkout_("ZZGETENV", (ftnlen)8);
    return 0;
} /* zzgetenv_ */

