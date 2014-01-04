/* txtops.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      TXTOPS ( Text file, open scratch ) */
/* Subroutine */ int txtops_(integer *unit)
{
    /* System generated locals */
    olist o__1;

    /* Builtin functions */
    integer f_open(olist *);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), getlun_(integer *), setmsg_(
	    char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Open a scratch text file for subsequent write access. */

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
/*     TEXT */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     UNIT       O   Logical unit. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     UNIT           is the logical unit connected to the opened */
/*                    scratch file. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the file cannot be opened, the error SPICE(FILEOPENFAILED) */
/*        is signalled. */

/* $ Files */

/*     See UNIT above. */

/* $ Particulars */

/*     In SPICELIB, a text file is formatted and sequential and may */
/*     contain only printable ASCII characters and blanks (ASCII 32-127). */
/*     When printing a text file, records are single spaced; the first */
/*     character will not be interpreted as a carriage control character. */

/*     TXTOPS opens a scratch text file and makes use of the SPICELIB */
/*     mechanism for coordinating the use of logical units. */

/*     System Dependencies */
/*     =================== */

/*     The open statement will include the following keyword = value */
/*     pairs: */

/*            UNIT   =  UNIT */
/*            FILE   =  FNAME */
/*            FORM   = 'FORMATTED' */
/*            ACCESS = 'SEQUENTIAL' */
/*            STATUS = 'SCRATCH' */
/*            IOSTAT =  IOSTAT */

/*     In addition, the statement will include */

/*            CARRIAGECONTROL = 'LIST' */

/*     for the Vax and Macintosh. */

/* $ Examples */

/*     The following example reads a line from an input file, */
/*     'INPUT.TXT', and writes it to an output file, 'OUTPUT.TXT'. */

/*        CALL TXTOPR ( 'INPUT.TXT',  IN  ) */
/*        CALL TXTOPS ( OUT ) */

/*        READ  ( IN,  FMT='(A)' ) LINE */
/*        WRITE ( OUT, FMT='(A)' ) LINE */

/*        CLOSE ( IN  ) */
/*        CLOSE ( OUT ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     1. "Absoft FORTRAN 77 Language Reference Manual", page 7-12 for */
/*        the NeXT. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    SPICELIB Version 1.11.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-CC_C. */

/* -    SPICELIB Version 1.10.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C. */

/* -    SPICELIB Version 1.9.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    SPICELIB Version 1.8.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-WINDOWS-64BIT-IFORT. */

/* -    SPICELIB Version 1.7.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 1.6.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-64BIT-MS_C. */

/* -    SPICELIB Version 1.5.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-INTEL_C. */

/* -    SPICELIB Version 1.4.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    SPICELIB Version 1.3.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 1.2.0, 18-MAR-2009 (BVS) */

/*        Updated for PC-LINUX-GFORTRAN. */

/* -    SPICELIB Version 1.1.0, 18-MAR-2009 (BVS) */

/*        Updated for MAC-OSX-GFORTRAN. */

/* -    SPICELIB Version 1.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 1.0.3, 21-SEP-1999 (NJB) */

/*        CSPICE and PC-LINUX environment lines were added.  Some */
/*        typos were corrected. */

/* -    SPICELIB Version 1.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 1.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 1.0.0, 20-FEB-1996 (WLT) */


/* -& */
/* $ Index_Entries */

/*     text file open scratch */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.0.0, 20-FEB-1996 (WLT) */

/*        This routine is basically a simple tweak of TXTOPN. */
/*        It replaces txtopn that Mike Spencer wrote because */
/*        the master file could not be located. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("TXTOPS", (ftnlen)6);
    }
    getlun_(unit);
    o__1.oerr = 1;
    o__1.ounit = *unit;
    o__1.ofnm = 0;
    o__1.orl = 0;
    o__1.osta = "SCRATCH";
    o__1.oacc = "SEQUENTIAL";
    o__1.ofm = "FORMATTED";
    o__1.oblnk = 0;
    iostat = f_open(&o__1);
    if (iostat != 0) {
	setmsg_("Could not scratch file. IOSTAT was #. ", (ftnlen)38);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(FILEOPENFAILED)", (ftnlen)21);
	chkout_("TXTOPS", (ftnlen)6);
	return 0;
    }
    chkout_("TXTOPS", (ftnlen)6);
    return 0;
} /* txtops_ */

