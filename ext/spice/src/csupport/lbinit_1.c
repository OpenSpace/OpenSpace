/* lbinit_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;

/* $Procedure LBINIT ( Line buffer, initialize ) */
/* Subroutine */ int lbinit_1__(integer *psize, integer *vdim, integer *ptrs, 
	char *buffer, ftnlen buffer_len)
{
    /* System generated locals */
    integer ptrs_dim1, i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer maxln;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), ssizei_(integer *, integer *);
    extern logical return_(void);
    extern /* Subroutine */ int lbupd_1__(integer *, integer *, integer *), 
	    cbinit_1__(integer *, char *, ftnlen);
    extern integer sizecb_1__(char *, ftnlen);

/* $ Abstract */

/*     Initialize a line buffer. */

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
/*     PSIZE      I   Pointer size. */
/*     VDIM       I   Value dimension. */
/*     PTRS, */
/*     BUFFER    I,O  Line buffer. */

/* $ Detailed_Input */

/*     PTRS        is an integer cell array to be used as the pointer */
/*                 component of a line buffer. */

/*     PSIZE       is the declared dimension of PTRS. */

/*     BUFFER      is a character buffer array to be used as the */
/*                 character compnent of a line buffer. */

/*     VDIM        is the declared dimension of BUFFER. */

/* $ Detailed_Output */

/*     PTRS, */
/*     BUFFER      together are an initialized line buffer. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If the line buffer cannot hold even a single line, the error */
/*        'SPICE(LBINSUFPTRSIZE)' is signalled. */

/* $ Particulars */

/*     A line buffer must be initialized to allow subsequent */
/*     operations on the buffer to detect possible overflows. */
/*     Both components of the buffer are initialized by a single */
/*     call to LBINIT. */

/*     In order to store N lines, PSIZE should be at least 4N+4. */

/* $ Examples */

/*     The following code fragment illustrates the initialization */
/*     of a typical line buffer. */

/*        INTEGER               LBCELL */
/*        PARAMETER           ( LBCELL =    -5 ) */

/*        INTEGER               LBCBUF */
/*        PARAMETER           ( LBCBUF =     0 ) */

/*        INTEGER               MAXLN */
/*        PARAMETER           ( MAXLN  =  1000 ) */

/*        INTEGER               PSIZE */
/*        PARAMETER           ( PSIZE  =  4 * MAXLN + 4 ) */

/*        INTEGER               BUFDIM */
/*        PARAMETER           ( BUFDIM =    25 ) */

/*        INTEGER               PTRS     ( LBCELL:PSIZE   ) */
/*        CHARACTER*(MAXLN)     BUFFER   ( LBCBUF:BUFDIM  ) */
/*         . */
/*         . */

/*        CALL LBINIT ( PSIZE, BUFDIM, PTRS, BUFFER ) */

/*     In this example, the buffer may be used to store up to 1000 lines */
/*     averaging 25 characters per line, or 25,000 total characters. The */
/*     length of any particular line may range from a single character */
/*     to the entire 25,000 characters. */

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

    /* Parameter adjustments */
    ptrs_dim1 = *psize + 6;

    /* Function Body */
    if (return_()) {
	return 0;
    } else {
	chkin_("LBINIT_1", (ftnlen)8);
    }

/*     Initialize the character buffer first. */

    cbinit_1__(vdim, buffer, buffer_len);

/*     The size must be 4(n+1), where n is the maximum number of */
/*     lines that can be stored. (The line buffer must be able to */
/*     store at least one line!) */

/*     Every line buffer starts out with zero lines and one complement */
/*     interval, which covers the entire CB. */

    maxln = *psize / 4 - 1;
    if (maxln < 1) {
	sigerr_("SPICE(INSUFPTRSIZE)", (ftnlen)19);
    } else {
	i__1 = maxln + 1 << 2;
	ssizei_(&i__1, ptrs);
	ptrs[(i__1 = 6) < ptrs_dim1 ? i__1 : s_rnge("ptrs", i__1, "lbinit_1__"
		, (ftnlen)197)] = 1;
	ptrs[(i__1 = 7) < ptrs_dim1 ? i__1 : s_rnge("ptrs", i__1, "lbinit_1__"
		, (ftnlen)198)] = sizecb_1__(buffer, buffer_len);
	lbupd_1__(&c__0, &c__1, ptrs);
    }
    chkout_("LBINIT_1", (ftnlen)8);
    return 0;
} /* lbinit_1__ */

