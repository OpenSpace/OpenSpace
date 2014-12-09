/* edtcmd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure     EDTCMD ( Edit a file using a specified text editor ) */
/* Subroutine */ int edtcmd_(char *cmd, char *file, ftnlen cmd_len, ftnlen 
	file_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    char loccmd[255];
    extern /* Subroutine */ int chkout_(char *, ftnlen), suffix_(char *, 
	    integer *, char *, ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int exesys_(char *, ftnlen);

/* $ Abstract */

/*     Edit a file using a specified editor. */

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

/*     SYSTEM */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     CMD        I   Command string used to invoke editor. */
/*     FILE       I   Name of file to edit. */

/* $ Detailed_Input */

/*     CMD            is a character string containing the command */
/*                    used to invoke a text editor available on the */
/*                    system under which the calling program is running. */
/*                    This routine will invoke the specified editor */
/*                    using FILE as the target file to edit.  The name */
/*                    of the file to be edited is not included in the */
/*                    command; this name is input as a separate argument. */

/*                    Case sensitivity of CMD varies with the system on */
/*                    which the calling program is run. */

/*                    Trailing white space in CMD is not significant. */


/*     FILE           is the name of a file that is to be edited.  FILE */
/*                    need not exist at the time this routine is called. */

/*                    Case sensitivity of FILE varies with the system on */
/*                    which the calling program is run. */

/*                    Trailing white space in FILE is not significant. */

/* $ Detailed_Output */

/*     None.  See $Particulars for further information on the action of */
/*     this routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the specified edit command fails, the error will be */
/*         diagnosed by routines called by this routine. */

/*     2)  If the editing session started by this routine is terminated */
/*         abnormally, the effect on the operation of the calling program */
/*         is unspecified. */

/* $ Files */

/*     See $Particulars. */

/* $ Particulars */

/*     This routine should be used with caution; calls to this routine */
/*     may have unintended side effects on the operation of the calling */
/*     program.  A solid understanding of the global operation of the */
/*     calling program is a prerequisite for programmers wishing to */
/*     use this routine. */

/*     The input argument FILE should unambiguously designate a file */
/*     that can be edited by the specified editor on the system under */
/*     which the calling program is being run.  The calling program */
/*     should have read or write privileges consistent with the intended */
/*     mode of access to FILE. */

/*     This routine may fail to recover in a predictable fashion from */
/*     abnormal termination of an editing session. */

/* $ Examples */

/*     1)   On a VAX/VMS system, the EDT editor could be invoked by */
/*          the calls */

/*             CALL EDTCMD ( 'EDIT/EDT',  FILE  ) */

/*          or */

/*             CALL EDTCMD ( 'EDIT/EDT/COMMAND = <command file>',  FILE ) */


/*     2)   On a Unix system, the emacs editor could be invoked */
/*          (normally) by the calls */

/*              CALL EDTCMD ( 'emacs', FILE ) */

/*          or */

/*              CALL EDTCMD ( '/usr/bin/emacs', FILE ) */


/* $ Restrictions */

/*     1)   The means by which this routine invokes an editor are system- */
/*          dependent; invoking the editor may have side effects that */
/*          affect the operation of the calling program.  For example, */
/*          on Unix systems, this routine may start a new shell in which */
/*          to run the editor; starting a new shell may interfere with */
/*          any sequential file I/O in progress at the time the shell is */
/*          started. */

/*          See the code for implementation details. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    Beta Version 2.27.0, 10-MAR-2014 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-INTEL. */

/* -    Beta Version 2.26.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-LINUX-64BIT-IFORT. */

/* -    Beta Version 2.25.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-GFORTRAN. */

/* -    Beta Version 2.24.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GFORTRAN. */

/* -    Beta Version 2.23.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GCC_C. */

/* -    Beta Version 2.22.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    Beta Version 2.21.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-CC_C. */

/* -    Beta Version 2.20.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C. */

/* -    Beta Version 2.19.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    Beta Version 2.18.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-WINDOWS-64BIT-IFORT. */

/* -    Beta Version 2.17.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    Beta Version 2.16.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-64BIT-MS_C. */

/* -    Beta Version 2.15.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-INTEL_C. */

/* -    Beta Version 2.14.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    Beta Version 2.13.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    Beta Version 2.12.0, 18-MAR-2009 (BVS) */

/*        Updated for PC-LINUX-GFORTRAN. */

/* -    Beta Version 2.11.0, 18-MAR-2009 (BVS) */

/*        Updated for MAC-OSX-GFORTRAN. */

/* -    Beta Version 2.10.0, 19-FEB-2008 (BVS) */

/*        Updated for PC-LINUX-IFORT. */

/* -    Beta Version 2.9.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-LINUX-64BIT-GCC_C. */

/* -    Beta Version 2.8.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-INTEL_C. */

/* -    Beta Version 2.7.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-IFORT. */

/* -    Beta Version 2.6.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-WINDOWS-IFORT. */

/* -    Beta Version 2.5.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    Beta Version 2.4.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN_C. */

/* -    Beta Version 2.3.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    Beta Version 2.2.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    Beta Version 2.2.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    Beta Version 2.2.3, 20-SEP-1999 (NJB) */

/*        CSPICE and PC-LINUX environment lines were added.  Some */
/*        typos were corrected. */

/* -    Beta Version 2.2.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    Beta Version 2.2.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    Beta Version 2.2.0, 12-AUG-1996 (WLT) */

/*        Added DEC-OSF1 to the list of supported environments */

/* -    Beta Version 2.1.0, 10-JAN-1996 (WLT) */

/*        Added PC-LAHEY to the list of supported environments. */

/* -    Beta Version 2.0.0, 16-JUN-1995 (WLT)(HAN) */

/*        Created master file from collection of machine dependent */
/*        routines.  Copyright notice added. */

/* -    Beta Version 1.0.0, 16-AUG-1994 (NJB) */

/* -& */
/* $ Index_Entries */

/*     invoke a text editor within a program */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EDTCMD", (ftnlen)6);
    }
/*     VAX: */

/*        Computer:         VAX 11/780 */
/*        Operating System: VAX VMS 5.3 */
/*        Fortran:          VAX FORTRAN 5.5 */


/*     PC-MS: */

/*        Computer:         PC */
/*        Operating System: Microsoft DOS 5.00 */
/*        Fortran:          Microsoft Powerstation Fortran V1.0 */


/*     Build the edit command to be passed to the system. */

    s_copy(loccmd, cmd, (ftnlen)255, cmd_len);
    suffix_(file, &c__1, loccmd, file_len, (ftnlen)255);

/*     Invoke the editor. */

    exesys_(loccmd, rtrim_(loccmd, (ftnlen)255));
    chkout_("EDTCMD", (ftnlen)6);
    return 0;
} /* edtcmd_ */

