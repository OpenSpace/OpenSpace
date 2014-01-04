/* errint.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__2 = 2;

/* $Procedure      ERRINT ( Insert Integer into Error Message Text ) */
/* Subroutine */ int errint_(char *marker, integer *integr, ftnlen marker_len)
{
    /* System generated locals */
    address a__1[3], a__2[2];
    integer i__1, i__2[3], i__3[2];

    /* Builtin functions */
    integer i_indx(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen),
	     s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern logical allowd_(void);
    extern integer lastnb_(char *, ftnlen);
    char lngmsg[1840];
    extern /* Subroutine */ int getlms_(char *, ftnlen);
    extern integer frstnb_(char *, ftnlen);
    char istrng[11], tmpmsg[1840];
    extern /* Subroutine */ int intstr_(integer *, char *, ftnlen), putlms_(
	    char *, ftnlen);
    integer strpos;

/* $ Abstract */

/*      Substitute an integer for the first occurrence of a marker found */
/*      in the current long error message. */

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

/*     ERROR, CONVERSION */

/* $ Declarations */
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


/*     Include File:  SPICELIB Error Handling Parameters */

/*        errhnd.inc  Version 2    18-JUN-1997 (WLT) */

/*           The size of the long error message was */
/*           reduced from 25*80 to 23*80 so that it */
/*           will be accepted by the Microsoft Power Station */
/*           FORTRAN compiler which has an upper bound */
/*           of 1900 for the length of a character string. */

/*        errhnd.inc  Version 1    29-JUL-1997 (NJB) */



/*     Maximum length of the long error message: */


/*     Maximum length of the short error message: */


/*     End Include File:  SPICELIB Error Handling Parameters */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MARKER     I   A substring of the error message to be replaced. */
/*     INTEGR     I   The integer to substitute for MARKER. */

/* $ Detailed_Input */

/*     MARKER     is a character string which marks a position in */
/*                the long error message where a character string */
/*                representing an integer is to be substituted. */
/*                Leading and trailing blanks in MARKER are not */
/*                significant. */

/*                Case IS significant;  'XX' is considered to be */
/*                a different marker from 'xx'. */

/*     INTEGR     is an integer whose character representation will */
/*                be substituted for the first occurrence of MARKER */
/*                in the long error message.  This occurrence of the */
/*                substring indicated by MARKER will be removed, and */
/*                replaced by a character string, with no leading or */
/*                trailing blanks, representing INTEGR. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     LMSGLN  is the maximum length of the long error message.  See */
/*             the include file errhnd.inc for the value of LMSGLN. */

/* $ Exceptions */

/*     This routine does not detect any errors. */

/*     However, this routine is part of the SPICELIB error */
/*     handling mechanism. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine updates the current long error message.  If no marker */
/*     is found, (e.g., in the case that the long error message is */
/*     blank), the routine has no effect.  If multiple instances of the */
/*     marker designated by MARKER are found, only the first one is */
/*     replaced. */

/*     If the character string resulting from the substitution */
/*     exceeds the maximum length of the long error message, the */
/*     characters on the right are lost.  No error is signalled. */

/*     This routine has no effect if changes to the long message */
/*     are not allowed. */

/* $ Examples */


/*      1.   In this example, the marker is:   # */


/*           The current long error message is: */

/*              'Invalid operation value.  The value was #'. */


/*           After the call, */


/*              CALL ERRINT ( '#',  5  ) */

/*           The long error message becomes: */

/*              'Invalid operation value.  The value was 5'. */



/*      2.   In this example, the marker is:   XX */


/*           The current long error message is: */

/*              'Left endpoint exceeded right endpoint.  The left'// */
/*              'endpoint was:  XX.  The right endpoint was:  XX.' */


/*           After the call, */

/*              CALL ERRINT ( 'XX',  5  ) */

/*           The long error message becomes: */

/*              'Left endpoint exceeded right endpoint.  The left'// */
/*              'endpoint was:  5.  The right endpoint was:  XX.' */


/* $ Restrictions */

/*     The caller must ensure that the message length, after sub- */
/*     stitution is performed, doesn't exceed LMSGLN characters. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*      N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 29-JUL-1997 (NJB) */

/*        Maximum length of the long error message is now represented */
/*        by the parameter LMSGLN.  Miscellaneous format changes to the */
/*        header, code and in-line comments were made. */

/* -      SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*          Comment section for permuted index source lines was added */
/*          following the header. */

/* -      SPICELIB Version 1.0.0, 31-JAN-1990  (NJB) */

/* -& */
/* $ Index_Entries */

/*     insert integer into error message text */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.1.0, 29-JUL-1997 (NJB) */

/*        Maximum length of the long error message is now represented */
/*        by the parameter LMSGLN.  Miscellaneous format changes to the */
/*        header, code and in-line comments were made. */

/* -& */

/*     SPICELIB functions */


/*     Local Variables: */


/*     Changes to the long error message have to be allowed, or we */
/*     do nothing. */

    if (! allowd_()) {
	return 0;
    }

/*     MARKER has to have some non-blank characters, or we do nothing. */

    if (lastnb_(marker, marker_len) == 0) {
	return 0;
    }

/*     Get a copy of the current long error message.  Convert INTEGR */
/*     to a character string. */

    getlms_(lngmsg, (ftnlen)1840);
    intstr_(integr, istrng, (ftnlen)11);

/*     Locate the leftmost occurrence of MARKER, if there is one */
/*     (ignoring leading and trailing blanks): */

    i__1 = frstnb_(marker, marker_len) - 1;
    strpos = i_indx(lngmsg, marker + i__1, (ftnlen)1840, lastnb_(marker, 
	    marker_len) - i__1);
    if (strpos == 0) {
	return 0;
    } else {

/*        We put together TMPMSG, a copy of LNGMSG with MARKER */
/*        replaced by the character representation of INTEGR: */

	if (strpos > 1) {
	    if (strpos + lastnb_(marker, marker_len) - frstnb_(marker, 
		    marker_len) < lastnb_(lngmsg, (ftnlen)1840)) {

/*              There's more of the long message after the marker... */

		i__1 = strpos + lastnb_(marker, marker_len) - frstnb_(marker, 
			marker_len);
/* Writing concatenation */
		i__2[0] = strpos - 1, a__1[0] = lngmsg;
		i__2[1] = lastnb_(istrng, (ftnlen)11), a__1[1] = istrng;
		i__2[2] = 1840 - i__1, a__1[2] = lngmsg + i__1;
		s_cat(tmpmsg, a__1, i__2, &c__3, (ftnlen)1840);
	    } else {
/* Writing concatenation */
		i__3[0] = strpos - 1, a__2[0] = lngmsg;
		i__3[1] = lastnb_(istrng, (ftnlen)11), a__2[1] = istrng;
		s_cat(tmpmsg, a__2, i__3, &c__2, (ftnlen)1840);
	    }
	} else {

/*           We're starting with the integer, so we know it fits... */

	    if (lastnb_(marker, marker_len) - frstnb_(marker, marker_len) < 
		    lastnb_(lngmsg, (ftnlen)1840)) {

/*              There's more of the long message after the marker... */

		i__1 = strpos + lastnb_(marker, marker_len) - frstnb_(marker, 
			marker_len);
/* Writing concatenation */
		i__3[0] = lastnb_(istrng, (ftnlen)11), a__2[0] = istrng;
		i__3[1] = 1840 - i__1, a__2[1] = lngmsg + i__1;
		s_cat(tmpmsg, a__2, i__3, &c__2, (ftnlen)1840);
	    } else {

/*              The marker's the whole string: */

		s_copy(tmpmsg, istrng, (ftnlen)1840, (ftnlen)11);
	    }
	}

/*        Update the long message: */

	putlms_(tmpmsg, (ftnlen)1840);
    }
    return 0;
} /* errint_ */

