/* exesys.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure   EXESYS  ( Execute system command ) */
/* Subroutine */ int exesys_(char *cmd, ftnlen cmd_len)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    integer status;
    extern integer system_(char *, ftnlen);

/* $ Abstract */

/*     Execute an operating system command. */

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

/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     CMD        I   Command to be executed. */

/* $ Detailed_Input */

/*     CMD            is a character string containing a command */
/*                    recognized by the command line interpreter of */
/*                    the operating system.  The significance of case */
/*                    in CMD is system-dependent.  Trailing white space */
/*                    is not significant. */

/* $ Detailed_Output */

/*     None.   See $Particulars for a description of the action of this */
/*     routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input command is not executed successfully, and if */
/*         this routine is able to detect the failure, the error */
/*         SPICE(SYSTEMCALLFAILED) is signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Most popular operating systems provide a Fortran-callable */
/*     interface that allows a program to execute an operating system */
/*     command by passing the command, in the form of a string, to the */
/*     operating system's command interpreter. This routine encapulates */
/*     most of the system-dependent code required to execute operating */
/*     system commands in this manner.  The input commands are of course */
/*     system-dependent. */

/*     Side effects of this routine vary from system to system. */
/*     See $Restrictions for more information. */

/*     Error checking capabilities also vary from system to system; this */
/*     routine does the best it can to diagnose errors resulting from */
/*     the attempt to execute the input command. */

/* $ Examples */

/*     1)  Unix:  copy the file spud.dat to the file spam.dat.  Test */
/*         whether the copy command was executed successfully. */

/*         For safety, we recommend appending a null character to the */
/*         command. */

/*            CALL EXESYS (  'cp spud.dat spam.dat'//CHAR(O)  ) */

/*            IF ( FAILED() ) THEN */

/*               [process error condition] */

/*            END IF */


/*     2)  VMS:  same action as in example (1): */

/*            CALL EXESYS ( 'COPY  SPUD.DAT;  SPAM.DAT;' ) */

/*            IF ( FAILED() ) THEN */

/*               [process error condition] */

/*            END IF */

/* $ Restrictions */

/*     1)  This routine should be used with caution; executing a system */
/*         command from within your program may have surprising side */
/*         effects.  For example, the Sun Fortran Reference Manual [1] */
/*         gives this warning: */

/*               *System* flushes all open files.  For output files, */
/*               the buffer is flushed to the actual file.  For input */
/*               files, the position of the pointer is unpredictable. */

/*     2)  Under Sun Fortran */

/*            -- The shell used to execute the command is determined by */
/*               the environment variable SHELL. */

/*            -- The command string cannot exceed 1024 characters in */
/*               length. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    Beta Version 2.26.0, 02-APR-2014 (BVS) */

/*        Changed PC-CYGWIN-GFORTRAN and PC-CYGWIN-64BIT-GFORTRAN */
/*        to be like other GFORTRAN environments. */

/* -    Beta Version 2.25.0, 10-MAR-2014 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-INTEL. */

/* -    Beta Version 2.24.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-LINUX-64BIT-IFORT. */

/* -    Beta Version 2.23.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-GFORTRAN. */

/* -    Beta Version 2.22.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GFORTRAN. */

/* -    Beta Version 2.21.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GCC_C. */

/* -    Beta Version 2.20.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    Beta Version 2.19.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-CC_C. */

/* -    Beta Version 2.18.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C. */

/* -    Beta Version 2.17.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    Beta Version 2.16.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-WINDOWS-64BIT-IFORT. */

/* -    Beta Version 2.15.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    Beta Version 2.14.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-64BIT-MS_C. */

/* -    Beta Version 2.13.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-INTEL_C. */

/* -    Beta Version 2.12.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    Beta Version 2.11.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    Beta Version 2.10.0, 06-APR-2009 (EDW) */

/*        Updated for PC-LINUX-GFORTRAN MAC-OSX-GFORTRAN. Eliminated */
/*        environment descriptions. Most were out-of-date or wrong. */
/*        IMPLICIT NONE now included in all environments. */

/* -    Beta Version 2.9.0, 19-FEB-2008 (BVS) */

/*        Updated for PC-LINUX-IFORT. */

/* -    Beta Version 2.8.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-LINUX-64BIT-GCC_C. */

/* -    Beta Version 2.7.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-INTEL_C. */

/* -    Beta Version 2.6.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-IFORT. */

/* -    Beta Version 2.5.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-WINDOWS-IFORT. */

/* -    Beta Version 2.4.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    Beta Version 2.3.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN_C. */

/* -    Beta Version 2.2.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    Beta Version 2.1.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    Beta Version 2.1.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    Beta Version 2.1.3, 22-SEP-1999 (NJB) */

/*        CSPICE and PC-LINUX environment lines were added.  Some */
/*        typos were corrected. */

/* -    Beta Version 2.1.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    Beta Version 2.1.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    Beta Version 2.1.0, 12-AUG-1996 (WLT) */

/*        Added the DEC-OSF1 environment. */

/* -    Beta Version 2.0.0, 16-JUN-1995 (WLT)(HAN) */

/*        Master version of machine dependent collections. */
/*        Copyright notice added. */

/* -    Beta Version 1.0.0, 16-AUG-1994 (NJB) */

/* -& */
/* $ Index_Entries */

/*     execute an operating system command */

/* -& */

/*     SPICELIB functions */


/*     System functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EXESYS", (ftnlen)6);
    }


    status = system_(cmd, rtrim_(cmd, cmd_len));
    if (status != 0) {

/*        Uh, we've got a problem. */

	setmsg_("The \"system\" call returned code # in response to command "
		"#.", (ftnlen)59);
	errint_("#", &status, (ftnlen)1);
	errch_("#", cmd, (ftnlen)1, cmd_len);
	sigerr_("SPICE(SYSTEMCALLFAILED)", (ftnlen)23);
	chkout_("EXESYS", (ftnlen)6);
	return 0;
    }
    chkout_("EXESYS", (ftnlen)6);
    return 0;
} /* exesys_ */

