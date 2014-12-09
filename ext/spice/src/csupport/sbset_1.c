/* sbset_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure SBSET ( String buffer, set value ) */
/* Subroutine */ int sbset_1__(char *name__, char *str, char *names, integer *
	ptrs, char *buffer, ftnlen name_len, ftnlen str_len, ftnlen names_len,
	 ftnlen buffer_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer nstr, f;
    extern integer cardc_(char *, ftnlen);
    integer l;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sizec_(char *, ftnlen);
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen), inslac_(
	    char *, integer *, integer *, char *, integer *, ftnlen, ftnlen);
    extern integer lastnb_(char *, ftnlen), lstlec_(char *, integer *, char *,
	     ftnlen, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern integer frstnb_(char *, ftnlen);
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    integer maxstr;
    extern logical return_(void);
    extern /* Subroutine */ int lbins_1__(integer *, char *, integer *, char *
	    , ftnlen, ftnlen), sbrem_1__(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    integer pos;

/* $ Abstract */

/*     Set the value of a string within a string buffer. */

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
/*     NAME       I   Name of the string to be stored. */
/*     STR        I   The string. */
/*     NAMES, */
/*     PTRS, */
/*     BUFFER    I,O  String buffer. */

/* $ Detailed_Input */

/*     NAME        is the name of a string to be stored within a string */
/*                 buffer. This name may be used to retrieve the string */
/*                 at some later time. */

/*     STR         is the string to be stored. */

/*     NAMES, */
/*     PTRS, */
/*     BUFFER      are the name, pointer, and character components of */
/*                 a string buffer. */

/* $ Detailed_Output */

/*     NAMES, */
/*     PTRS, */
/*     BUFFER      are the name, pointer, and character components of */
/*                 the same string buffer, now containing the specified */
/*                 string. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If NAME is longer than the maximum length of the names in */
/*        the buffer, it is truncated. Thus, long names may conflict, */
/*        overwriting each other's associated strings. */

/*     2) If a string with the specified name is not already contained */
/*        in the string buffer, and if the maximum number of strings */
/*        is currently stored, the error 'SPICE(SBTOOMANYSTRS)' is */
/*        signalled. */

/* $ Particulars */

/*     SBSET is the only way to get a string into a string buffer. */

/* $ Examples */

/*     The following code fragment stores three strings, associated */
/*     with the names WHO, WHAT, and WHERE. */

/*        CALL SBSET ( 'WHO',   'Feynman',                 N, P, B ) */
/*        CALL SBSET ( 'WHAT',  'Quantum electrodynamics', N, P, B ) */
/*        CALL SBSET ( 'WHERE', 'Caltech',                 N, P, B ) */

/*     The strings can be retrieved using either SBGET, */

/*        CALL SBGET ( 'WHO',   S(1), N, P, B, POS ) */
/*        CALL SBGET ( 'WHAT',  S(2), N, P, B, POS ) */
/*        CALL SBGET ( 'WHERE', S(3), N, P, B, POS ) */

/*      or SBGETP, */

/*        CALL SBGET ( 3, S(1), N, P, B, ADDR ) */
/*        CALL SBGET ( 1, S(2), N, P, B, ADDR ) */
/*        CALL SBGET ( 2, S(3), N, P, B, ADDR ) */

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
	chkin_("SBSET_1", (ftnlen)7);
    }

/*     If the buffer already contains a string with this name, remove it. */

    sbrem_1__(name__, names, ptrs, buffer, name_len, names_len, buffer_len);

/*     Recover the (new) essential control information. */

    maxstr = sizec_(names, names_len);
    nstr = cardc_(names, names_len);

/*     Where should the name be inserted? */

    if (nstr == maxstr) {
	setmsg_("Current limit is #.", (ftnlen)19);
	errint_("#", &maxstr, (ftnlen)1);
	sigerr_("SPICE(SBTOOMANYSTRS)", (ftnlen)20);
    } else {
	pos = lstlec_(name__, &nstr, names + names_len * 6, name_len, 
		names_len) + 1;

/*        Store only the non-blank part of the string. (Store a blank */
/*        string as a single blank character.) */

/* Computing MAX */
	i__1 = 1, i__2 = frstnb_(str, str_len);
	f = max(i__1,i__2);
/* Computing MAX */
	i__1 = 1, i__2 = lastnb_(str, str_len);
	l = max(i__1,i__2);

/*        Add the name of the string to the name list, and the string */
/*        itself to the LB. */

	inslac_(name__, &c__1, &pos, names + names_len * 6, &nstr, name_len, 
		names_len);
	scardc_(&nstr, names, names_len);
	lbins_1__(&pos, str + (f - 1), ptrs, buffer, l - (f - 1), buffer_len);
    }
    chkout_("SBSET_1", (ftnlen)7);
    return 0;
} /* sbset_1__ */

