/* lbins_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;

/* $Procedure LBINS ( Line buffer, insert ) */
/* Subroutine */ int lbins_1__(integer *pos, char *line, integer *ptrs, char *
	buffer, ftnlen line_len, ftnlen buffer_len)
{
    /* System generated locals */
    integer i__1, i__2;
    static integer equiv_1[2];

    /* Local variables */
    integer ncom, f, l;
#define begin (equiv_1)
    integer pcard, avail;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer nline, lnlen, maxln;
    extern /* Subroutine */ int inslai_(integer *, integer *, integer *, 
	    integer *, integer *);
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen), lbdes_1__(integer *, integer *, integer *, integer *, 
	    integer *);
    extern logical return_(void);
    extern /* Subroutine */ int lbupd_1__(integer *, integer *, integer *);
    integer posptr;
    extern /* Subroutine */ int cbput_1__(integer *, integer *, char *, char *
	    , ftnlen, ftnlen);
#define end (equiv_1 + 1)
#define ptr (equiv_1)
    extern /* Subroutine */ int lbpack_1__(integer *, char *, ftnlen);

/* $ Abstract */

/*     Insert a line into a line buffer. */

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
/*     POS        I   Position of new line. */
/*     LINE       I   Line to be inserted. */
/*     PTRS, */
/*     BUFFER    I,O  Line buffer. */

/* $ Detailed_Input */

/*     POS         is the position (line number) at which the new line is */
/*                 to be inserted. */

/*     LINE        is the line to be inserted. */

/*     PTRS, */
/*     BUFFER      are the pointer and character components of a line */
/*                 buffer. */

/* $ Detailed_Output */

/*     PTRS, */
/*     BUFFER      are the pointer and character components of the */
/*                 same line buffer, after the new line has been */
/*                 inserted. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If POS exceeds by exactly one the number of lines currently */
/*        stored in the buffer, then LINE is appended to the end of */
/*        the buffer, and no other lines are affected. */

/*     2) If POS is less than one, or if POS exceeds by more than one */
/*        the number of lines currently stored in the buffer, the error */
/*        'SPICE(LBNOSUCHLINE)' is signalled. */

/*     3) If the maximum number of lines is currently stored, the */
/*        error 'SPICE(LBTOOMANYLINES)' is signalled. */

/*     4) If the line buffer contains insufficient free space to store */
/*        the new line, the error 'SPICE(LBLINETOOLONG)' is signalled. */

/* $ Particulars */

/*     New lines may be inserted at any position within a line buffer. */
/*     The line currently at the specified position and all subsequent */
/*     lines are moved back to make room for the new line. */

/* $ Examples */

/*     Let the line buffer (P,B) contain the following lines */

/*       If neither foes nor loving friends can hurt you, */
/*       If all men count with you, but none too much: */

/*     Following the calls */

/*       CALL LBINS ( 1, */
/*      .            'If you can talk with crowds and keep your virtue,', */
/*      .             P, B ) */

/*       CALL LBINS ( 2, */
/*      .            'Or walk with Kings---nor lose the common touch,', */
/*      .             P, B ) */

/*     it contains the lines */

/*       If you can talk with crowds and keep your virtue, */
/*       Or walk with Kings---nor lose the common touch, */
/*       If neither foes nor loving friends can hurt you, */
/*       If all men count with you, but none too much: */

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


/*     Equivalences */


/*     Standard error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("LBINS_1", (ftnlen)7);
    }

/*     Recover all the essential control information. */

    lbdes_1__(ptrs, &maxln, &nline, &ncom, &pcard);

/*     Where should this line be inserted, if at all? */

    if (nline == maxln) {
	setmsg_("Current line limit is #.", (ftnlen)24);
	errint_("#", &maxln, (ftnlen)1);
	sigerr_("SPICE(LBTOOMANYLINES)", (ftnlen)21);
    } else if (*pos < 1 || *pos - nline > 1) {
	setmsg_("Tried to access line # of #.", (ftnlen)28);
	errint_("#", pos, (ftnlen)1);
	errint_("#", &nline, (ftnlen)1);
	sigerr_("SPICE(LBNOSUCHLINE)", (ftnlen)19);
    } else {
	posptr = (*pos << 1) - 1;

/*        Leading blanks are significant; trailing blanks are history. */
/*        (Store a blank string as a single blank character.) */

	f = 1;
/* Computing MAX */
	i__1 = 1, i__2 = lastnb_(line, line_len);
	l = max(i__1,i__2);
	lnlen = l - f + 1;

/*        Store each new string at the end of the end of the CB. */
/*        If the final interval in the complement isn't large enough */
/*        to hold the new string, pack the CB and try again. */

	avail = ptrs[pcard + 5] - ptrs[pcard + 4] + 1;
	if (avail < lnlen) {
	    lbpack_1__(ptrs, buffer, buffer_len);
	    lbdes_1__(ptrs, &maxln, &nline, &ncom, &pcard);
	    avail = ptrs[pcard + 5] - ptrs[pcard + 4] + 1;
	}

/*        If there still isn't enough room? Well, those are the breaks. */

	if (avail < lnlen) {
	    sigerr_("SPICE(LBLINETOOLONG)", (ftnlen)20);

/*        If there is room, allocate just enough of the final interval */
/*        in the complement to contain the new string; store the string; */
/*        and insert the name and pointers at their proper locations. */

	} else {
	    *begin = ptrs[pcard + 4];
	    *end = *begin + lnlen - 1;
	    ptrs[pcard + 4] = *end + 1;
	    cbput_1__(begin, end, line + (f - 1), buffer, l - (f - 1), 
		    buffer_len);
	    inslai_(ptr, &c__2, &posptr, &ptrs[6], &pcard);
	    i__1 = nline + 1;
	    lbupd_1__(&i__1, &ncom, ptrs);
	}
    }
    chkout_("LBINS_1", (ftnlen)7);
    return 0;
} /* lbins_1__ */

#undef ptr
#undef end
#undef begin


