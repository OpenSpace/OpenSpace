/* cbget_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure CBGET ( Character buffer, get ) */
/* Subroutine */ int cbget_1__(integer *begin, integer *end, char *buffer, 
	char *string, ftnlen buffer_len, ftnlen string_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer last, next, b, i__, l;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer buflen;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    extern integer sizecb_1__(char *, ftnlen);

/* $ Abstract */

/*     Get (return) a substring of a character buffer. */

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

/*     CB */

/* $ Keywords */

/*     ASCII */
/*     CHARACTER */
/*     STRING */
/*     TEXT */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     BEGIN, */
/*     END        I   Initial, final buffer locations. */
/*     BUFFER     I   Character buffer. */
/*     STRING     O   String. */

/* $ Detailed_Input */

/*     BEGIN, */
/*     END         are the initial and final buffer locations of */
/*                 the string to be returned. */

/*     BUFFER      is a character buffer. */

/* $ Detailed_Output */

/*     STRING      is the string contained between locations BEGIN and */
/*                 END of BUFFER. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) The error 'SPICE(CBNOSUCHSTR)' is signalled whenever any of */
/*        the following conditions is detected: */

/*           -- BEGIN is less than one. */

/*           -- END is greater than the size of BUFFER. */

/*           -- BEGIN is greater than END. */

/* $ Particulars */

/*     If you think of the character buffer as a single character string, */
/*     this is exactly equivalent to the operation */

/*        STRING = BUFFER(BEGIN:END) */

/*     If shorter than the substring, STRING is truncated. If longer, */
/*     it is padded with blanks. */

/* $ Examples */

/*     The code fragment */

/*        STR = '..........................' */

/*        CALL CBPUT (  1, 13, 'ABCDEFGHIJKLM', BUFFER             ) */
/*        CALL CBPUT ( 14, 26, 'NOPQRSTUVWXYZ', BUFFER             ) */
/*        CALL CBGET (  1,  3,                  BUFFER, STR( 1:10) ) */
/*        CALL CBGET (  1, 26,                  BUFFER, STR(11:13) ) */

/*        WRITE (*,*) '+--------------------------+' */
/*        WRITE (*,*) '|'    // STR(1:26) //     '|' */
/*        WRITE (*,*) '+--------------------------+' */

/*     produces the following output. */

/*        +--------------------------+ */
/*        |ABC       ABC.............| */
/*        +--------------------------+ */

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
	chkin_("CBGET_1", (ftnlen)7);
	if (*begin < 1 || *end > sizecb_1__(buffer, buffer_len) || *begin > *
		end) {
	    setmsg_("Tried to access locations #:#.", (ftnlen)30);
	    errint_("#", begin, (ftnlen)1);
	    errint_("#", end, (ftnlen)1);
	    sigerr_("SPICE(CBNOSUCHSTR)", (ftnlen)18);
	    chkout_("CBGET_1", (ftnlen)7);
	    return 0;
	}
    }

/*     Storage begins at location B in line L. */

    buflen = i_len(buffer + buffer_len, buffer_len);
    l = (*begin - 1) / buflen + 1;
    b = (*begin - 1) % buflen + 1;

/*     Assign one character at a time, changing input lines when */
/*     necessary. Do not assign any characters beyond the end of */
/*     the output string. */

    next = 1;
    last = i_len(string, string_len);
    i__1 = *end;
    for (i__ = *begin; i__ <= i__1; ++i__) {
	if (next <= last) {
	    *(unsigned char *)&string[next - 1] = *(unsigned char *)&buffer[l 
		    * buffer_len + (b - 1)];
	    ++next;
	}
	if (b < buflen) {
	    ++b;
	} else {
	    ++l;
	    b = 1;
	}
    }

/*     Pad the output string with blanks, if necessary. */

    if (next <= last) {
	s_copy(string + (next - 1), " ", string_len - (next - 1), (ftnlen)1);
    }
    chkout_("CBGET_1", (ftnlen)7);
    return 0;
} /* cbget_1__ */

