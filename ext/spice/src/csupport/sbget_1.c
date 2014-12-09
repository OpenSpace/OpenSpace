/* sbget_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SBGET ( String buffer, get ) */
/* Subroutine */ int sbget_1__(char *name__, char *names, integer *ptrs, char 
	*buffer, char *str, integer *pos, ftnlen name_len, ftnlen names_len, 
	ftnlen buffer_len, ftnlen str_len)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern integer cardc_(char *, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical found;
    extern integer bsrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int chkout_(char *, ftnlen), lbget_1__(integer *, 
	    integer *, char *, char *, logical *, ftnlen, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Get (return) a string from a string buffer. */

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
/*     NAME       I   Name of the string to be returned. */
/*     NAMES, */
/*     PTRS, */
/*     BUFFER     I   String buffer. */
/*     STR        O   The string. */
/*     POS        O   Position of the string within the buffer. */

/* $ Detailed_Input */

/*     NAME        is the name of a string contained within a string */
/*                 buffer. */

/*     NAMES, */
/*     PTRS, */
/*     BUFFER      are the name, pointer, and character components */
/*                 of the string buffer. */

/* $ Detailed_Output */

/*     STR         is the string associated with the specified name. */
/*                 If STRING is shorter than the stored string, it is */
/*                 truncated. If longer, STRING is padded with spaces. */

/*     POS         is the position of the specified string within the */
/*                 string buffer, as determined by the ASCII collating */
/*                 sequence. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If the specified string is not in the list, POS is zero */
/*        and STR is not changed. */

/* $ Particulars */

/*     There are two routines that you can use to retrieve a string */
/*     from a string buffer: */

/*        SBGET      which takes the name of the string, and returns */
/*                   the string and its position within the buffer. */

/*        SBGETP     which takes the position of the string within */
/*                   the buffer, and returns the string and its address */
/*                   within the name table. */

/* $ Examples */

/*     The following code fragment stores three strings, associated */
/*     with the names WHO, WHAT, and WHERE. */

/*        CALL SBSET  ( 'WHO',   'Feynman',                 N, P, B ) */
/*        CALL SBSET  ( 'WHAT',  'Quantum electrodynamics', N, P, B ) */
/*        CALL SBSET  ( 'WHERE', 'Caltech',                 N, P, B ) */

/*     The strings can be retrieved using either SBGET, */

/*        CALL SBGET  ( 'WHO',   S(1), N, P, B, POS ) */
/*        CALL SBGET  ( 'WHAT',  S(2), N, P, B, POS ) */
/*        CALL SBGET  ( 'WHERE', S(3), N, P, B, POS ) */

/*      or SBGETP, */

/*        CALL SBGETP ( 3, S(1), N, P, B, ADDR ) */
/*        CALL SBGETP ( 1, S(2), N, P, B, ADDR ) */
/*        CALL SBGETP ( 2, S(3), N, P, B, ADDR ) */

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
	chkin_("SBGET_1", (ftnlen)7);
    }

/*     Is this string even in the list? */

    i__1 = cardc_(names, names_len);
    *pos = bsrchc_(name__, &i__1, names + names_len * 6, name_len, names_len);

/*     If so, get it. */

    if (*pos > 0) {
	lbget_1__(pos, ptrs, buffer, str, &found, buffer_len, str_len);
    }
    chkout_("SBGET_1", (ftnlen)7);
    return 0;
} /* sbget_1__ */

