/* pltfrm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      PLTFRM ( Get platform attributes ) */
/* Subroutine */ int pltfrm_(integer *room, integer *n, char *attr, ftnlen 
	attr_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    char item[32*3];
    extern /* Subroutine */ int zzplatfm_(char *, char *, ftnlen, ftnlen);
    integer i__, limit;

/* ~  NEXT */
/* *    IMPLICIT NONE */
/* ~~ */
/* $ Abstract */

/*     Return platform id and various attributes of the platform */
/*     environment */

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

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ROOM       I   amount of room available for returning attributes */
/*     N          O   number of attributes returned */
/*     ATTR       O   string values of various attributes */

/* $ Detailed_Input */

/*     ROOM        is the amount of space available in the character */
/*                 string array ATTR for returning platform attributes. */

/* $ Detailed_Output */

/*     N           is the actual number of attributes returned.  N will */
/*                 always be less than or equal to ROOM. */

/*     ATTR        is an array of attributes about the platform */
/*                 and environment on which this routine is running. */

/*                 ATTR will contain in the following order */

/*                 1) machine name    :   HP, NEXT, PC, SGI, etc. */
/*                 2) fortran compiler:   HP , ABSOFT, etc. */
/*                 3) Operating System */

/*                 Other items may be added later.  Check your local */
/*                 listing for details. */

/*                 If a value is not available it will be returned */
/*                 with the value '<unavailable>' */

/*                 The routine that calls this should declare */
/*                 ATTR to be at least CHARACTER*(32). */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If ROOM is less than or equal to zero, the N will be */
/*         returned with a value of zero and ATTR will not be */
/*         changed from it's input state. */

/* $ Particulars */

/*     This routine serves to identify the platforma and compiler */
/*     used in creating SPICELIB.  It is provided so that routines */
/*     and programs can make run-time decisions based upon the */
/*     ambient fortran environment. */

/* $ Examples */

/*     This routine could be used so that a single routine */
/*     can be written that translates the meaning of IOSTAT values */
/*     that depend upon the machine and compiler.  At run time */
/*     the routine can look up the appropriate message to associate */
/*     with an IOSTAT value. */
/* C */
/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    Support Version 1.4.0, 28-JUL-1999 (WLT) */

/*        Changed routine to call new SPICE private routine ZZPLATFM */
/*        The routine is no longer environment specific. */

/* -    Inspekt Version 1.3.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    Inspekt Version 1.3.0, 05-APR-1998 (NJB) */

/*        Added the PC-LINUX environment. */

/* -    Inspekt Version 1.2.0, 12-AUG-1996 (WLT) */

/*        Added the DEC-OSF1 environment. */

/* -    Inspekt Version 1.1.0, 16-JUN-1995 (WLT) */

/*        Master version of original machine dependent collection. */
/*        Copyright notice added. */

/* -    Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*        This is the configured version of the Command Loop */
/*        software as of May 4, 1994 */


/* -    SPICELIB Version 1.0.0, 21-APR-1994 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Determine the machine, OS and fortran version. */

/* -& */
    s_copy(item, "SYSTEM", (ftnlen)32, (ftnlen)6);
    s_copy(item + 32, "COMPILER", (ftnlen)32, (ftnlen)8);
    s_copy(item + 64, "O/S", (ftnlen)32, (ftnlen)3);
/* Computing MAX */
    i__1 = 0, i__2 = min(3,*room);
    limit = max(i__1,i__2);
    i__1 = limit;
    for (i__ = 1; i__ <= i__1; ++i__) {
	zzplatfm_(item + (((i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge(
		"item", i__2, "pltfrm_", (ftnlen)194)) << 5), attr + (i__ - 1)
		 * attr_len, (ftnlen)32, attr_len);
    }
    *n = limit;
    return 0;
} /* pltfrm_ */

