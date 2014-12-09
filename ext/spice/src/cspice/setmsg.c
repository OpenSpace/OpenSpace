/* setmsg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure     SETMSG  ( Set Long Error Message ) */
/* Subroutine */ int setmsg_(char *msg, ftnlen msg_len)
{
    extern logical allowd_(void);
    extern /* Subroutine */ int putlms_(char *, ftnlen);

/* $ Abstract */

/*     Set the value of the current long error message. */

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

/*     ERROR */

/* $ Keywords */

/*     ERROR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MSG        I   A long error message. */

/* $ Detailed_Input */

/*     MSG     A ``long'' error message. */
/*             MSG is a detailed description of the error. */
/*             MSG is supposed to start with the name of the */
/*             module which detected the error, followed by a */
/*             colon.  Example: */

/*                'RDTEXT:  There are no more free logical units' */

/*             Only the first LMSGLN characters of MSG are stored; */
/*             any further characters are truncated. */

/*             Generally, MSG will be stored internally by the SPICELIB */
/*             error handling mechanism.  The only exception */
/*             is the case in which the user has commanded the */
/*             toolkit to ``ignore'' the error indicated by MSG. */

/*             As a default, MSG will be output to the screen. */
/*             See the required reading file for a discussion of how */
/*             to customize toolkit error handling behavior, and */
/*             in particular, the disposition of MSG. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     LMSGLN  is the maximum length of the long error message.  See */
/*             the include file errhnd.inc for the value of LMSGLN. */

/* $ Exceptions */

/*     This routine does not detect any errors. */

/*     However, this routine is part of the interface to the */
/*     SPICELIB error handling mechanism.  For this reason, */
/*     this routine does not participate in the trace scheme, */
/*     even though it has external references. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The SPICELIB routine SIGERR should always be called */
/*     AFTER this routine is called, when an error is detected. */

/*     The effects of this routine are: */

/*        1.  If acceptance of a new long error message is */
/*            allowed: */

/*            MSG will be stored internally.  As a result, */
/*            The SPICELIB routine, GETMSG, will be able to */
/*            retrieve MSG, until MSG has been ``erased'' */
/*            by a call to RESET, or overwritten by another */
/*            call to SETMSG. */


/*        2.  If acceptance of a new long error message is not allowed, */
/*            a call to this routine has no effect. */

/* $ Examples */


/*      In the following example, N is supposed to be less than */
/*      MAXLUN.  If it isn't, an error condition exists. */

/*      C */
/*      C      We will need a free logical unit.  But only if we don't */
/*      C      have too many files open already. */
/*      C */

/*             IF ( N .EQ. MAXLUN ) THEN */

/*                CALL SETMSG ( 'RDTEXT: Too many files open already' ) */
/*                CALL SIGERR ( 'SPICE(TOOMANYFILESOPEN)' ) */

/*                RETURN */

/*             END IF */


/* $ Restrictions */

/*     SIGERR must be called once after each call to this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 29-JUL-1997 (NJB) */

/*        Maximum length of the long error message is now represented */
/*        by the parameter LMSGLN.  Miscellaneous header fixes were */
/*        made.  Some indentation and vertical white space abnormalities */
/*        in the code were fixed.  Some dubious comments were deleted */
/*        from the code. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     set long error message */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.0.2, 29-JUL-1997 (NJB) */

/*        Maximum length of the long error message is now represented */
/*        by the parameter LMSGLN.  Miscellaneous header fixes were */
/*        made.  Some indentation and vertical white space abnormalities */
/*        in the code were fixed.  Some dubious comments were deleted */
/*        from the code. */

/* -     Beta Version 1.1.0, 17-FEB-1989 (NJB) */

/*         Declarations of the unused variable STAT and unused function */
/*         ACCEPT removed. */

/* -& */

/*     SPICELIB functions */


/*     We store the long error message only when updates */
/*     of the long message are allowed: */

    if (allowd_()) {
	putlms_(msg, msg_len);
    }
    return 0;
} /* setmsg_ */

