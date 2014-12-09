/* tempb.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      TEMPB ( Manage SUBTeX temporary buffer ) */
/* Subroutine */ int tempb_(char *action, char *line, ftnlen action_len, 
	ftnlen line_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char what[4];
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, ftnlen)
	    , pagput_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Add lines to the SUBTeX temporary line buffer. */

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

/*     SUBTeX */

/* $ Keywords */

/*     SUBTeX */

/* $ Declarations */
/* $ Detailed_Input */

/*     ACTION      determines whether the buffer is to be cleared */
/*                 ('NEW') before adding the line, or whether the */
/*                 line is to be appended ('ADD') to the existing */
/*                 buffer. */

/*     LINE        is a line of text to be added to the SUBTeX */
/*                 temporary line buffer. */

/* $ Detailed_Output */

/*     None. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ACTION     I   'NEW' or 'ADD'. */
/*     LINE       I   New (processed) line. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If ACTION is not recognized, the error 'SUBTeX(BADTEMPACTION)' */
/*        is signalled. */

/* $ Particulars */

/*     As each chunk of the input source is processed, the resulting */
/*     lines are placed in the temporary line buffer. The lines from */
/*     this buffer ultimately replace the input source. */

/* $ Examples */


/* $ Restrictions */

/*     The nominal buffer contains sufficient storage for up to 1000 */
/*     lines averaging 60 characters per line. */

/* $ Literature_References */

/* $Include SUBTeX.REFS */

/* $ Author_and_Institution */

/*     I.M. Underwood (JPL) */

/* $ Version */

/*     Beta Version 1.0.0, 11-JUN-1988 (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

/*     INTEGER               PTRS          ( LBCELL:MAXPTR ) */
/*     LOGICAL               INIT */

/*     Saved variables */

/*     SAVE                  PTRS */
/*     SAVE                  BUFFER */
/*     SAVE                  INIT */

/*     Initial values */

/*     DATA                  INIT          / .FALSE. / */

/*     Standard SPICE error handling */

    if (return_()) {
	return 0;
    } else {
	chkin_("TEMPB", (ftnlen)5);
    }

/*     Initialize the line buffer if necessary. */

/*     IF ( .NOT. INIT ) THEN */

/*        CALL SSIZEI ( MAXPTR, PTRS   ) */
/*        CALL SDIMCB ( MAXLN,  BUFFER ) */
/*        CALL LBINIT ( PTRS,   BUFFER ) */

/*     END IF */

/*     Shake or bake? */

    ucase_(action, what, action_len, (ftnlen)4);
    if (s_cmp(what, "NEW", (ftnlen)4, (ftnlen)3) == 0) {
/*        CALL LBINIT ( PTRS, BUFFER ) */
/*        CALL LBAPP  ( LINE, PTRS, BUFFER ) */
	pagput_(line, line_len);
    } else if (s_cmp(what, "ADD", (ftnlen)4, (ftnlen)3) == 0) {
/*        CALL LBAPP ( LINE, PTRS, BUFFER ) */
	pagput_(line, line_len);
    } else {
	sigerr_("SUBTeX(BADTEMPACTION)", (ftnlen)21);
    }
    chkout_("TEMPB", (ftnlen)5);
    return 0;
} /* tempb_ */

