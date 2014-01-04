/* lbdes_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure LBDES ( Line buffer, describe ) */
/* Subroutine */ int lbdes_1__(integer *ptrs, integer *maxln, integer *nline, 
	integer *ncom, integer *pcard)
{
    extern integer cardi_(integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sizei_(integer *);
    integer psize;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Describe the current internal status of a line buffer. */

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

/*     CB, LB */

/* $ Keywords */

/*     ASCII */
/*     CHARACTER */
/*     STRING */
/*     TEXT */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     PTRS       I   Pointer component of the buffer. */
/*     MAXLN      O   Maximum number of lines. */
/*     NLINE      O   Current number of lines. */
/*     NCOM       O   Current number of complement intervals. */
/*     PCARD      O   Current cardinality of PTRS. */

/* $ Detailed_Input */

/*     PTRS        is the pointer component of a line buffer. */

/* $ Detailed_Output */

/*     MAXLN       is the maximum number of lines that can be stored in */
/*                 the buffer at any one time. */

/*     NLINE       is the number of lines currently stored in the buffer. */

/*     NCOM        is the number of complement intervals (contiguous */
/*                 spaces in which new lines can be stored) currently */
/*                 available in the buffer. */

/*     PCARD       is the current cardinality of PTRS. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Particulars */

/*     This routine is intended primarily for internal use by the */
/*     line buffer routines. However, the information that it returns */
/*     can be useful for error checking and debugging purposes. */

/* $ Examples */

/*     In the following code fragment, a check is performed before */
/*     attempting to use the routine LBAPP. */

/*        CALL LBDES ( PTRS, MAXLN, NLINE, NCOM, PCARD ) */

/*        IF ( NLINE .LT. MAXLN ) THEN */
/*           CALL LBAPP ( LINE, PTRS, BUFFER ) */

/*        ELSE */
/*           WRITE (6,*) 'Sorry, there isn't room for another line.' */
/*           WRITE (6,*) 'Please delete something and try again.' */
/*        END IF */

/*     For more examples, see the source code of the other LB routines. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart, (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 19-JAN-1989 (DT) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("LBDES_1", (ftnlen)7);
    }

/*     Recover some information directly. */

    psize = sizei_(ptrs);
    *pcard = cardi_(ptrs);
    *nline = ptrs[3];

/*     Infer the rest. */

    *maxln = psize / 4 - 1;
    *ncom = *pcard / 2 - *nline;
    chkout_("LBDES_1", (ftnlen)7);
    return 0;
} /* lbdes_1__ */

