/* sbinit_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SBINIT ( String buffer, initialize ) */
/* Subroutine */ int sbinit_1__(integer *nsize, integer *psize, integer *vdim,
	 char *names, integer *ptrs, char *buffer, ftnlen names_len, ftnlen 
	buffer_len)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), ssizec_(integer *, char *, 
	    ftnlen);
    integer maxptr;
    extern logical return_(void);
    extern /* Subroutine */ int lbinit_1__(integer *, integer *, integer *, 
	    char *, ftnlen);

/* $ Abstract */


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

/*     CB, LB, SB */

/* $ Keywords */

/*     ASCII */
/*     CHARACTER */
/*     STRING */
/*     TEXT */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     NSIZE      I   Name size. */
/*     PSIZE      I   Pointer size. */
/*     VDIM       I   Value dimension */
/*     NAMES, */
/*     PTRS, */
/*     BUFFER    I,O  String buffer. */

/* $ Detailed_Input */

/*     NAMES       is a character cell array to be used as the name */
/*                 component of a string buffer. */

/*     NSIZE       is the declared dimension of NAMES. */

/*     PTRS        is an integer cell array to be used as the pointer */
/*                 component of a string buffer. */

/*     PSIZE       is the declared dimension of PTRS. */

/*     BUFFER      is a character buffer array to be used as the */
/*                 character component of a string buffer. */

/*     VDIM        is the declared dimension of BUFFER. */

/* $ Detailed_Output */

/*     NAMES, */
/*     PTRS, */
/*     BUFFER      together are an initialized string buffer. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If the size of the pointer array is not sufficient to */
/*        hold pointers for the maximum number of strings, the */
/*        error 'SPICE(SBINSUFPTRSIZE)' is signalled. */

/* $ Particulars */

/*     A string buffer must be initialized to allow subsequent */
/*     operations on the buffer to detect possible overflows. */
/*     All three components of the buffer are initialized by a */
/*     single call to SBINIT. */

/*     In order to make full use of the name cell of the string buffer, */
/*     the arrays and name buffers should be declared as shown below. */

/*       INTEGER              LBCELL */
/*       PARAMETER          ( LBCELL = -5 ) */

/*       INTEGER              MAXNL */
/*       PARAMETER          ( MAXNL  = maximum name length ) */

/*       INTEGER              MAXN */
/*       PARAMETER          ( MAXN   = maximum number of names ) */

/*       CHARACTER*(MAXNL)    NAMES ( MAXN         ) */
/*       INTEGER              PTRS  ( MAXN * 4 + 4 ) */

/*     The character buffer portion of the string buffer should be */
/*     declared as shown below. */

/*       INTEGER              MAXL */
/*       PARAMETER          ( MAXL = maximum expected string length ) */

/*       INTEGER              AVGL */
/*       PARAMETER          ( AVGL = average expected string length ) */

/*       INTEGER              LBCBUF */
/*       PARAMETER          ( LBCBUF = 0  ) */

/*       CHARACTER*(MAXL)     BUFFER ( LBCBUF:(MAXN * AVGL) / MAXL + 1 ) */

/* $ Examples */

/*     The following code fragment illustrates the initialization */
/*     of a typical string buffer. */

/*        INTEGER               LBCELL */
/*        PARAMETER           ( LBCELL =    -5 ) */

/*        INTEGER               LBCBUF */
/*        PARAMETER           ( LBCBUF =     0 ) */

/*        CHARACTER*(32)        NAMES    ( LBCELL:1000    ) */
/*        INTEGER               PTRS     ( LBCELL:4004    ) */
/*        CHARACTER*(250)       BUFFER   ( LBCBUF:100     ) */
/*         . */
/*         . */

/*        CALL SBINIT ( MAXN, PSIZE, BUFDIM, NAMES, PTRS, BUFFER ) */

/*     In this example, the buffer may be used to store up to 1000 */
/*     strings averaging 25 characters per string, or 25,000 total */
/*     characters. The length of any particular string may range from */
/*     a single character to the entire 25,000 characters. The names */
/*     used to identify the strings may contain up to 32 characters. */

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
	chkin_("SBINIT_1", (ftnlen)8);
    }

/*     Make sure that the line buffer is large enough (but ONLY large */
/*     enough) to hold the maximum number of strings. The name list */
/*     should be empty. The LB should be initialized as a unit. */

    maxptr = *nsize + 1 << 2;
    if (*psize < maxptr) {
	sigerr_("SPICE(SBINSUFPTRSIZE)", (ftnlen)21);
    } else {
	ssizec_(nsize, names, names_len);
	lbinit_1__(&maxptr, vdim, ptrs, buffer, buffer_len);
    }
    chkout_("SBINIT_1", (ftnlen)8);
    return 0;
} /* sbinit_1__ */

