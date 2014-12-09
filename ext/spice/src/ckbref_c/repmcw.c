/* repmcw.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure  REPMCW  ( Replace marker with character string, width ) */
/* Subroutine */ int repmcw_(char *in, char *marker, char *value, integer *
	width, char *out, ftnlen in_len, ftnlen marker_len, ftnlen value_len, 
	ftnlen out_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_indx(char *, char *, ftnlen, ftnlen), i_len(char *, ftnlen);

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer lastnb_(char *, ftnlen), frstnb_(char *, ftnlen);
    extern /* Subroutine */ int chkout_(char *, ftnlen), remsub_(char *, 
	    integer *, integer *, char *, ftnlen, ftnlen), repsub_(char *, 
	    integer *, integer *, char *, char *, ftnlen, ftnlen, ftnlen);
    integer vlngth, mrkpos;
    extern logical return_(void);

/* $ Abstract */

/*     Replace a marker with a character string of specified width. */

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

/*     CHARACTER */
/*     CONVERSION */
/*     STRING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     IN         I   Input string. */
/*     MARKER     I   Marker to be replaced. */
/*     VALUE      I   Replacement string. */
/*     WIDTH      I   Width of replacement string to appear in OUT. */
/*     OUT        O   Output string. */

/* $ Detailed_Input */

/*     IN             is an arbitrary character string. */

/*     MARKER         is an arbitrary character string. The first */
/*                    occurrence of MARKER in the input string is */
/*                    to be replaced by VALUE. */

/*                    Leading and trailing blanks in MARKER are NOT */
/*                    significant. In particular, no substitution is */
/*                    performed if MARKER is blank. */

/*     VALUE          is an arbitrary character string. Leading and */
/*                    trailing blanks in VALUE are significant. */

/*     WIDTH          Number of characters from VALUE that must be */
/*                    inserted in place of MARKER. If WIDTH is greater */
/*                    than actual length of the VALUE, blanks are */
/*                    appended to the end of the VALUE when it is */
/*                    inserted. Zero or negative widths are allowed, */
/*                    if such specified, marker is simply deleted */
/*                    from input string. */

/* $ Detailed_Output */

/*     OUT            is the string obtained by substituting VALUE */
/*                    of specified width for the first occurrence */
/*                    of MARKER in the input string. */

/*                    OUT and IN must be identical or disjoint. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If OUT does not have sufficient length to accommodate the */
/*        result of the substitution, the result will be truncated on */
/*        the right. */

/*     2) If MARKER is blank, or if MARKER is not a substring of IN, */
/*        no substitution is performed. (OUT and IN are identical.) */

/*     3) If VALUE is blank, a WIDTH number of blanks is substituted */
/*        for the first occurrence of MARKER. */

/* $ Particulars */

/*     This is routine was created by modifying SPICELIB's REPMC */
/*     behavior a little bit to make it more suitable for */
/*     generation fixed width tables. */

/* $ Examples */

/*     This program using REPMCW: */

/*              INTEGER               WORDLN */
/*              PARAMETER           ( WORDLN = 10 ) */

/*              INTEGER               ACWDTH */
/*              PARAMETER           ( ACWDTH = 13 ) */

/*              INTEGER               NMWDTH */
/*              PARAMETER           ( NMWDTH = 22 ) */

/*              INTEGER               IDWDTH */
/*              PARAMETER           ( IDWDTH = 13 ) */

/*              CHARACTER*(WORDLN  )  ACRNYM */
/*              CHARACTER*(WORDLN*2)  SCNAME */
/*              CHARACTER*(WORDLN*4)  SCID */
/*              CHARACTER*(WORDLN*8)  TRECRD */
/*              CHARACTER*(WORDLN*8)  HLINE */
/*              CHARACTER*(WORDLN*8)  HTITLE */
/*              CHARACTER*(WORDLN*8)  DASHLN */
/*        C */
/*        C     Table line templates. */
/*        C */
/*              HLINE  = '      +#+#+#+' */
/*              HTITLE = '      |#|#|#|' */
/*              TRECRD = '      |#|#|#|' */
/*              DASHLN = '--------------------------------------' */
/*        C */
/*        C     Create table separator line. */
/*        C */
/*              CALL REPMCW ( HLINE, '#', DASHLN,  ACWDTH, HLINE  ) */
/*              CALL REPMCW ( HLINE, '#', DASHLN,  NMWDTH, HLINE  ) */
/*              CALL REPMCW ( HLINE, '#', DASHLN,  IDWDTH, HLINE  ) */
/*        C */
/*        C     Create table title line. */
/*        C */
/*              CALL REPMCW ( HTITLE, '#', ' S/C ACRONYM', */
/*             .                                   ACWDTH, HTITLE ) */
/*              CALL REPMCW ( HTITLE, '#', '   SPACECRAFT NAME  ', */
/*             .                                   NMWDTH, HTITLE ) */
/*              CALL REPMCW ( HTITLE, '#', ' S/C NAIF ID ', */
/*             .                                   IDWDTH, HTITLE ) */
/*        C */
/*        C     Create table record for MPF. */
/*        C */
/*              ACRNYM = '  MPF' */
/*              SCNAME = ' Mars Pathfinder' */
/*              SCID   = '     -53' */
/*              CALL REPMCW ( TRECRD, '#', ACRNYM, ACWDTH, TRECRD ) */
/*              CALL REPMCW ( TRECRD, '#', SCNAME, NMWDTH, TRECRD ) */
/*              CALL REPMCW ( TRECRD, '#', SCID,   IDWDTH, TRECRD ) */
/*        C */
/*        C     Print table. */
/*        C */
/*              WRITE (*,*) HLINE */
/*              WRITE (*,*) HTITLE */
/*              WRITE (*,*) HLINE */
/*              WRITE (*,*) TRECRD */
/*              WRITE (*,*) HLINE */

/*              END */

/*        print the following output: */

/*              +-------------+----------------------+-------------+ */
/*              | S/C ACRONYM |   SPACECRAFT NAME    | S/C NAIF ID | */
/*              +-------------+----------------------+-------------+ */
/*              |  MPF        | Mars Pathfinder      |     -53     | */
/*              +-------------+----------------------+-------------+ */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V.Semenov   (JPL) */

/* $ Version */

/* -    Alpha Version 1.0.0, 29-JUL-1998 (BVS) */

/* -& */
/* $ Index_Entries */

/*     replace marker with character_string of a specified width */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("REPMCW", (ftnlen)6);
    }

/*     If MARKER is blank, no substitution is possible. */

    if (s_cmp(marker, " ", marker_len, (ftnlen)1) == 0) {
	s_copy(out, in, out_len, in_len);
	chkout_("REPMCW", (ftnlen)6);
	return 0;
    }

/*     Locate the leftmost occurrence of MARKER, if there is one */
/*     (ignoring leading and trailing blanks). If MARKER is not */
/*     a substring of IN, no substitution can be performed. */

    i__1 = frstnb_(marker, marker_len) - 1;
    mrkpos = i_indx(in, marker + i__1, in_len, lastnb_(marker, marker_len) - 
	    i__1);
    if (mrkpos == 0) {
	s_copy(out, in, out_len, in_len);
	chkout_("REPMCW", (ftnlen)6);
	return 0;
    }

/*     Okay, MARKER is non-blank and has been found. Substitute string */
/*     of required width. */

    vlngth = i_len(value, value_len);
    if (*width <= 0) {

/*        In this case we just delete marker from input string. */

	i__1 = mrkpos + lastnb_(marker, marker_len) - frstnb_(marker, 
		marker_len);
	remsub_(in, &mrkpos, &i__1, out, in_len, out_len);
    } else if (*width <= vlngth) {

/*        Here we simply insert as many characters as needed into */
/*        our output string. */

	i__1 = mrkpos + lastnb_(marker, marker_len) - frstnb_(marker, 
		marker_len);
	repsub_(in, &mrkpos, &i__1, value, out, in_len, (*width), out_len);
    } else {

/*        Here we insert all characters from VALUE and as many spaces */
/*        as needed to make in our output string WIDTH wide. */

	i__1 = mrkpos + lastnb_(marker, marker_len) - frstnb_(marker, 
		marker_len);
	repsub_(in, &mrkpos, &i__1, value, out, in_len, vlngth, out_len);
	if (mrkpos + vlngth - 1 < i_len(out, out_len)) {
	    i__1 = *width - vlngth;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (mrkpos + vlngth - 1 < i_len(out, out_len)) {
		    i__2 = mrkpos + vlngth;
		    i__3 = mrkpos + vlngth - 1;
		    repsub_(out, &i__2, &i__3, " ", out, out_len, (ftnlen)1, 
			    out_len);
		}
	    }
	}
    }
    chkout_("REPMCW", (ftnlen)6);
    return 0;
} /* repmcw_ */

