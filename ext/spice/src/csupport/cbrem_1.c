/* cbrem_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure CBREM ( Character buffer, remove ) */
/* Subroutine */ int cbrem_1__(integer *begin, integer *end, char *buffer, 
	ftnlen buffer_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen);

    /* Local variables */
    integer b, i__, l;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer nb, nl, endbuf, buflen;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int cbput_1__(integer *, integer *, char *, char *
	    , ftnlen, ftnlen);
    extern integer sizecb_1__(char *, ftnlen);

/* $ Abstract */

/*     Remove a string from a character buffer. */

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
/*     BUFFER    I,O  Character buffer. */

/* $ Detailed_Input */

/*     BEGIN, */
/*     END         are the initial and final locations within the */
/*                 character buffer bounding the part of the buffer */
/*                 to be removed. */

/*     BUFFER      is a character buffer. */

/* $ Detailed_Output */

/*     BUFFER      is the same character buffer, with the original */
/*                 contents of locations BEGIN through END removed. */

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
/*     this is exactly equivalent to the sequence */

/*        TEMP            = BUFFER(END+1: ) */
/*        BUFFER(BEGIN: ) = TEMP */

/*     where TEMP is a string of infinite length. */

/* $ Examples */

/*     The code fragment */

/*       CALL CBPUT (  1, 26, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', BUFFER      ) */
/*       CALL CBPUT ( 27, 52, '..........................', BUFFER      ) */
/*       CALL CBREM (  2, 25,                               BUFFER      ) */
/*       CALL CBGET (  1, 26,                               BUFFER, STR ) */

/*       WRITE (*,*) '+--------------------------+' */
/*       WRITE (*,*) '|'    // STR(1:26) //     '|' */
/*       WRITE (*,*) '+--------------------------+' */

/*     produces the following output. */

/*       +--------------------------+ */
/*       |AZ........................| */
/*       +--------------------------+ */

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
	chkin_("CBREM_1", (ftnlen)7);
	if (*begin < 1 || *end > sizecb_1__(buffer, buffer_len) || *begin > *
		end) {
	    setmsg_("Tried to access locations #:#.", (ftnlen)30);
	    errint_("#", begin, (ftnlen)1);
	    errint_("#", end, (ftnlen)1);
	    sigerr_("SPICE(CBNOSUCHSTR)", (ftnlen)18);
	    chkout_("CBREM_1", (ftnlen)7);
	    return 0;
	}
    }

/*     Essential limits. */

    buflen = i_len(buffer + buffer_len, buffer_len);
    endbuf = sizecb_1__(buffer, buffer_len);

/*     Each guy gets moved from location B in line L to location NB */
/*     in line NL. (N stands for New.) */

    l = *end / buflen + 1;
    b = *end % buflen + 1;
    nl = (*begin - 1) / buflen + 1;
    nb = (*begin - 1) % buflen + 1;
    i__1 = endbuf;
    for (i__ = *end + 1; i__ <= i__1; ++i__) {
	*(unsigned char *)&buffer[nl * buffer_len + (nb - 1)] = *(unsigned 
		char *)&buffer[l * buffer_len + (b - 1)];
	if (b < buflen) {
	    ++b;
	} else {
	    ++l;
	    b = 1;
	}
	if (nb < buflen) {
	    ++nb;
	} else {
	    ++nl;
	    nb = 1;
	}
    }

/*     Now we can just overwrite the vacated space at the end. */

    i__1 = endbuf - (*end - *begin);
    cbput_1__(&i__1, &endbuf, " ", buffer, (ftnlen)1, buffer_len);
    chkout_("CBREM_1", (ftnlen)7);
    return 0;
} /* cbrem_1__ */

