/* lbrem_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;

/* $Procedure LBREM ( Line buffer, remove ) */
/* Subroutine */ int lbrem_1__(integer *pos, integer *ptrs, char *buffer, 
	ftnlen buffer_len)
{
    /* System generated locals */
    integer i__1;
    char ch__1[1];
    static integer equiv_1[2];

    /* Local variables */
    integer ncom, i__;
#define begin (equiv_1)
    integer pcard;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer nline, maxln;
    extern /* Subroutine */ int remlai_(integer *, integer *, integer *, 
	    integer *), inslai_(integer *, integer *, integer *, integer *, 
	    integer *);
    extern /* Character */ VOID touchc_(char *, ftnlen, char *, ftnlen);
    integer offset;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    integer poscom;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), lbdes_1__(integer *, integer *, integer *, 
	    integer *, integer *);
    extern logical return_(void);
    extern /* Subroutine */ int lbupd_1__(integer *, integer *, integer *);
    integer posptr;
#define end (equiv_1 + 1)
#define ptr (equiv_1)

/* $ Abstract */

/*     Remove a line from a line buffer. */

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
/*     POS        I   Position of line to be removed. */
/*     PTRS, */
/*     BUFFER    I,O  Line buffer. */

/* $ Detailed_Input */

/*     POS         is the position of an existing line within a line */
/*                 buffer. */

/*     PTRS, */
/*     BUFFER      are the pointer and character components of a line */
/*                 buffer. */

/* $ Detailed_Output */

/*     PTRS, */
/*     BUFFER      are the pointer and character components of the */
/*                 same line buffer, after the specified line has been */
/*                 removed. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If POS is less than one, or if POS is greater than the */
/*        number of lines currently stored in the buffer, the error */
/*        'SPICE(LBNOSUCHLINE)' is signalled. */

/* $ Particulars */

/*     Existing lines may be removed from at any position within a line */
/*     buffer. All subsequent lines are moved forward to take up the */
/*     slack. */

/* $ Examples */

/*     Let the line buffer (P,B) contain the following lines */

/*       If you can make one heap of all your winnings */
/*       And risk it on one turn of pitch-and-toss, */
/*       And lose, and start again at your beginnings, */
/*       And never breathe a word about your loss: */
/*       If you can force your heart and nerve and sinew */
/*       To serve your turn long after they are gone, */

/*     Following the calls */

/*       CALL LBREM ( 3, P, B ) */
/*       CALL LBREP ( 3, P, B ) */

/*     it contains the lines */

/*       If you can make one heap of all your winnings */
/*       And risk it on one turn of pitch-and-toss, */
/*       If you can force your heart and nerve and sinew */
/*       To serve your turn long after they are gone, */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart, (JPL) */

/* $ Version */

/* -    Inspekt Version 3.0.0 9-May-1994 (WLT) */

/*        Added a "TOUCHC" to the input buffer so that compilers */
/*        won't complain about input arguments not being used. */

/*        And fixed the addition of "TOUCHC" to refere to LBCBUF */
/*        instead of LBCELL */

/* -    Beta Version 1.0.0, 19-JAN-1989 (DT) */

/* -& */

/*     SPICELIB functions */


/*     Other Functions */


/*     Local variables */


/*     Equivalences */


/*     Standard error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("LBREM_1", (ftnlen)7);
    }

/*     We touch the input buffer so that compilers will not complain */
/*     that BUFFER is an unused argument.  It really is unused, but */
/*     it's in the calling sequence for the sake of uniformity of */
/*     the calling sequences for the line buffer routines. */

    touchc_(ch__1, (ftnlen)1, buffer, buffer_len);
    *(unsigned char *)&buffer[0] = *(unsigned char *)&ch__1[0];

/*     Recover the essential control information. */

    lbdes_1__(ptrs, &maxln, &nline, &ncom, &pcard);

/*     No way to remove a line that's not in the table. */

    if (*pos < 1 || *pos > nline) {
	setmsg_("Tried to access line # of #.", (ftnlen)28);
	errint_("#", pos, (ftnlen)1);
	errint_("#", &nline, (ftnlen)1);
	sigerr_("SPICE(LBNOSUCHLINE)", (ftnlen)19);
	chkout_("LBREM_1", (ftnlen)7);
	return 0;
    }

/*     Save the bounds of the stored line before removing the name */
/*     and pointers from their respective tables. */

    posptr = (*pos << 1) - 1;
    *begin = ptrs[posptr + 5];
    *end = ptrs[posptr + 6];
    --nline;
    remlai_(&c__2, &posptr, &ptrs[6], &pcard);

/*     Add the interval to the complement. Insert it directly, then */
/*     do any merges required. */

    offset = nline << 1;
    poscom = offset + 1;
    i__1 = pcard;
    for (i__ = offset + 2; i__ <= i__1; i__ += 2) {
	if (*begin > ptrs[i__ + 5]) {
	    poscom = i__ + 1;
	}
    }
    inslai_(ptr, &c__2, &poscom, &ptrs[6], &pcard);
    i__1 = offset + 2;
    for (i__ = pcard - 2; i__ >= i__1; i__ += -2) {
	if (ptrs[i__ + 6] == ptrs[i__ + 5] + 1) {
	    remlai_(&c__2, &i__, &ptrs[6], &pcard);
	}
    }
    ncom = pcard / 2 - nline;
    lbupd_1__(&nline, &ncom, ptrs);
    chkout_("LBREM_1", (ftnlen)7);
    return 0;
} /* lbrem_1__ */

#undef ptr
#undef end
#undef begin


