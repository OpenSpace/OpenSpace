/* sbrem_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure SBREM ( String buffer, remove ) */
/* Subroutine */ int sbrem_1__(char *name__, char *names, integer *ptrs, char 
	*buffer, ftnlen name_len, ftnlen names_len, ftnlen buffer_len)
{
    integer nstr;
    extern integer cardc_(char *, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen), scardc_(integer *, 
	    char *, ftnlen), remlac_(integer *, integer *, char *, integer *, 
	    ftnlen);
    extern integer bsrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int lbrem_1__(integer *, integer *, char *, 
	    ftnlen);
    integer pos;

/* $ Abstract */

/*     Remove a string from a string buffer. */

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
/*     NAME       I   Name of the string to be removed. */
/*     NAMES, */
/*     PTRS, */
/*     BUFFER    I,O  String buffer. */

/* $ Detailed_Input */

/*     NAME        is the name of a string currently stored within a */
/*                 string buffer. */

/*     NAMES, */
/*     PTRS, */
/*     BUFFER      are the name, pointer, and character components */
/*                 of a string buffer. */

/* $ Detailed_Output */

/*     NAMES, */
/*     PTRS, */
/*     BUFFER      are the name, pointer, and character components */
/*                 of the same string buffer, from which the specified */
/*                 string has been removed. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If NAME is longer than the maximum length of the names in */
/*        the buffer, it is truncated. Thus, long names may conflict, */
/*        removing each other's associated strings. */

/*     2) If a string with the specified name is not already contained */
/*        in the string buffer, nothing happens. */

/* $ Particulars */

/*     SBREM is the only way to get a string out of a string buffer. */

/* $ Examples */

/*     The code fragment */

/*        CALL SBSET ( 'EINSTEIN', 'Brownian motion',  N, P, B      ) */
/*        CALL SBSET ( 'BOHR',     'Atomic structure', N, P, B      ) */
/*        CALL SBGET ( 'EINSTEIN',                     N, P, B, POS ) */

/*        WRITE (*,*) 'Found at position ', POS */

/*        CALL SBREM ( 'EINSTEIN', N, P, B      ) */
/*        CALL SBGET ( 'EINSTEIN', N, P, B, POS ) */

/*        WRITE (*,*) 'Found at position ', POS */

/*     Produces the following output. */

/*        Found at position 2 */
/*        Found at position 0 */

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
	chkin_("SBREM_1", (ftnlen)7);
    }

/*     Recover the essential control information. */

    nstr = cardc_(names, names_len);

/*     Which string is to be removed? */

    pos = bsrchc_(name__, &nstr, names + names_len * 6, name_len, names_len);

/*     If the string is not in the buffer, do nothing. */

    if (pos > 0) {

/*        Remove the name from the name list, and the string from the */
/*        line buffer. */

	remlac_(&c__1, &pos, names + names_len * 6, &nstr, names_len);
	scardc_(&nstr, names, names_len);
	lbrem_1__(&pos, ptrs, buffer, buffer_len);
    }
    chkout_("SBREM_1", (ftnlen)7);
    return 0;
} /* sbrem_1__ */

