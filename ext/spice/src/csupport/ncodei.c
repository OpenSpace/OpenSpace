/* ncodei.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure NCODEI ( Encode integer value into integer item ) */
/* Subroutine */ int ncodei_0_(int n__, integer *value, integer *item)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Encode an integer value into an integer item. */

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

/*     CELLS */

/* $ Keywords */

/*     CELLS */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     VALUE      I   Non-negative integer value. */
/*     ITEM       O   Item into which VALUE has been encoded. */

/* $ Detailed_Input */

/*     VALUE       is an arbitrary non-negative integer value. */

/* $ Detailed_Output */

/*     ITEM        is an integer item, into which the value has been */
/*                 been encoded. The value can be recovered by calling */
/*                 subroutine DCODE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the number to be encoded is negative, the error */
/*        'SPICE(OUTOFRANGE)' is signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     NCODE (and its inverse, DCODE) provide a uniform way to store */
/*     and retrieve values from the control areas of cells and cell- */
/*     based data types. This makes it possible to write templates */
/*     for a generic data type, suitable for instantiation by a */
/*     pre-compiler. */

/* $ Examples */

/*     The following code fragment illustrates how NCODE and DCODE */
/*     can be used to create instantiable subroutine templates. */

/*        C */
/*        C     Check the bolzat counter, to see if the elements */
/*        C     have been freebished; if not, do it now, and set */
/*        C     the counter to zero. */
/*        C */
/*              CALL DCODE@ ( STRUCT(-4), BCOUNT ) */

/*              IF ( BCOUNT .GT. 0 ) THEN */
/*                 CALL FREEB@ ( CARD@ ( STRUCT ), STRUCT(1)  ) */
/*                 CALL NCODE@ (                0, STRUCT(-4) ) */
/*              END IF */

/*     By replacing all occurrences of `@' with the appropriate */
/*     type ending (C, D, or I), this single template can give */
/*     rise to three separate pieces of type-dependent code. */

/*     The alternative to using NCODE and DCODE is to use simple */
/*     assignments for numeric cells, and calls to ENCHAR and */
/*     DECHAR for character cells, destroying the symmetry inherent */
/*     in the rest of the code. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 15-JUL-1989 (WLT) (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Standard SPICE error handling. */

    switch(n__) {
	case 1: goto L_dcodei;
	}

    if (return_()) {
	return 0;
    } else {
	chkin_("NCODEI", (ftnlen)6);
    }
    if (*value >= 0) {
	*item = *value;
    } else {
	setmsg_("Cannot encode #; must be non-negative.", (ftnlen)38);
	errint_("#", value, (ftnlen)1);
	sigerr_("SPICE(OUTOFRANGE)", (ftnlen)17);
    }
    chkout_("NCODEI", (ftnlen)6);
    return 0;
/* $Procedure DCODEI ( Decode integer value from integer item ) */

L_dcodei:
/* $ Abstract */

/*     Decode the integer value stored in an integer item by a */
/*     previous call to NCODEI. */

/* $ Required_Reading */

/*     CELLS */

/* $ Keywords */

/*     CELLS */

/* $ Declarations */

/*     INTEGER               ITEM */
/*     INTEGER               VALUE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ITEM       I   Item into which an integer value has been encoded. */
/*     VALUE      O   Encoded value. */

/* $ Detailed_Input */

/*     ITEM        is an integer item, into which an integer value */
/*                 has been encoded by a previous call to NCODEI. */

/* $ Detailed_Output */

/*     VALUE       is the encoded value. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     NCODE (and its inverse, DCODE) provide a uniform way to store */
/*     and retrieve values from the control areas of cells and cell- */
/*     based data types. This makes it possible to write templates */
/*     for a generic data type, suitable for instantiation by a */
/*     pre-compiler. */

/* $ Examples */

/*     The following code fragment illustrates how NCODE and DCODE */
/*     can be used to create instantiable subroutine templates. */

/*        C */
/*        C     Check the bolzat counter, to see if the elements */
/*        C     have been freebished; if not, do it now, and set */
/*        C     the counter to zero. */
/*        C */
/*              CALL DCODE@ ( STRUCT(-4), BCOUNT ) */

/*              IF ( BCOUNT .GT. 0 ) THEN */
/*                 CALL FREEB@ ( CARD@ ( STRUCT ), STRUCT(1)  ) */
/*                 CALL NCODE@ (                0, STRUCT(-4) ) */
/*              END IF */

/*     By replacing all occurrences of `@' with the appropriate */
/*     type ending (C, D, or I), this single template can give */
/*     rise to three separate pieces of type-dependent code. */

/*     The alternative to using NCODE and DCODE is to use simple */
/*     assignments for numeric cells, and calls to ENCHAR and */
/*     DECHAR for character cells, destroying the symmetry inherent */
/*     in the rest of the code. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 15-JUL-1989 (WLT) (IMU) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DCODEI", (ftnlen)6);
    }
    *value = *item;
    chkout_("DCODEI", (ftnlen)6);
    return 0;
} /* ncodei_ */

/* Subroutine */ int ncodei_(integer *value, integer *item)
{
    return ncodei_0_(0, value, item);
    }

/* Subroutine */ int dcodei_(integer *item, integer *value)
{
    return ncodei_0_(1, value, item);
    }

