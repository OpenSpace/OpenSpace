/* size.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure      SIZE ( Process a SUBTeX size change request ) */
/* Subroutine */ int size_(char *source, ftnlen source_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer endc, endn, endt;
    extern integer cpos_(char *, char *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer nblen_(char *, ftnlen), ncpos_(char *, char *, integer *, 
	    ftnlen, ftnlen);
    integer beginc, beginn, begint;
    extern /* Subroutine */ int params_(char *, char *, integer *, ftnlen, 
	    ftnlen), chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Process a @setvarsize or @setparamsize control sequence. */

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

/*     SUBTeX */

/* $ Keywords */

/*     SUBTeX */

/* $ Declarations */
/* $ Detailed_Input */

/*     SOURCE      is a source line containing a @setvarsize or */
/*                 @setparamsize control sequence. */

/* $ Detailed_Output */

/*     No lines of output are produced by this routine. The values of */
/*     VARNAMESIZE, VARTYPESIZE, and PARAMNAMESIZE may be changed. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SOURCE     I   Source line. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Particulars */


/* $ Examples */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/* $Include SUBTeX.REFS */

/* $ Author_and_Institution */

/*     I.M. Underwood (JPL) */

/* $ Version */

/*     Beta Version 1.0.0, 11-JUN-1988 (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling */

    if (return_()) {
	return 0;
    } else {
	chkin_("SIZE", (ftnlen)4);
    }

/*     We should see something like */

/*        @setvarsize{xxx}{yyy} */

/*     or */

/*        @setparamsize{zzz} */

/*     In either case, the control sequence is delimited by '{', */
/*     and the variable or parameter name is delimited by '}'. */

    beginc = ncpos_(source, " ", &c__1, source_len, (ftnlen)1);
    endc = cpos_(source, "{", &beginc, source_len, (ftnlen)1) - 1;
    beginn = cpos_(source, "{", &endc, source_len, (ftnlen)1) + 1;
    endn = cpos_(source, "}", &beginn, source_len, (ftnlen)1) - 1;

/*     Set the name size. Go back for the type template if this */
/*     is a variable. */

    if (s_cmp(source + (beginc - 1), "@setvarsize", endc - (beginc - 1), (
	    ftnlen)11) == 0) {
	i__1 = nblen_(source + (beginn - 1), endn - (beginn - 1));
	params_("SET", "VARNAMESIZE", &i__1, (ftnlen)3, (ftnlen)11);
	begint = cpos_(source, "{", &endn, source_len, (ftnlen)1) + 1;
	endt = cpos_(source, "}", &begint, source_len, (ftnlen)1) - 1;
	i__1 = nblen_(source + (begint - 1), endt - (begint - 1));
	params_("SET", "VARTYPESIZE", &i__1, (ftnlen)3, (ftnlen)11);
    } else if (s_cmp(source + (beginc - 1), "@setparamsize", endc - (beginc - 
	    1), (ftnlen)13) == 0) {
	i__1 = nblen_(source + (beginn - 1), endn - (beginn - 1));
	params_("SET", "PARAMNAMESIZE", &i__1, (ftnlen)3, (ftnlen)13);
    }
    chkout_("SIZE", (ftnlen)4);
    return 0;
} /* size_ */

