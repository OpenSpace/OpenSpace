/* crtptr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1024 = 1024;
static integer c__5 = 5;

/* $Procedure                  CRTPTR (Create pointer) */
/* Character */ VOID crtptr_(char *ret_val, ftnlen ret_val_len, char *base, 
	integer *index, char *pnter, ftnlen base_len, ftnlen pnter_len)
{
    /* System generated locals */
    address a__1[5];
    integer i__1[5];

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);

    /* Local variables */
    integer blen, clen, plen;
    char cnum[10];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer total;
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int intstr_(integer *, char *, ftnlen);
    char sym[1024];

/* $ Abstract */

/*     Returns the symbol 'BASE~INDEX~PNTER'. */

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

/*     None. */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*      BASE, */
/*      INDEX, */
/*      PNTER     I    Components of the symbol 'BASE~INDEX~PNTER'. */

/*      SYMLEN    P    Maximum length of the symbol. */

/* $ Detailed_Input */

/*     BASE, */
/*     INDEX, */
/*     PNTER      are components of the symbol 'BASE~INDEX~PNTER'. */

/* $ Detailed_Output */

/*     CRTPTR     is the symbol 'BASE~INDEX~PNTER'. */

/* $ Parameters */

/*     SYMLEN     is the maximum length of the symbol 'BASE~INDEX~PNTER'. */

/* $ Exceptions */

/*     1) If the length of the symbol 'BASE~INDEX~PNTER' exceeds SYMLEN, */
/*        the error SPICE(BUFFERTOOSMALL) is signalled. */

/*     2) If the length of the symbol 'BASE~INDEX~PNTER' exceeds */
/*        LEN(CRTPTR), the error SPICE(DIMENSIONTOOSMALL) is signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine creates a symbol that may be used to look up */
/*     nodes in the symbol table created by CPARSE. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     M.J. Spencer   (JPL) */

/* $ Version */

/* -    Version 1.1.1, 13-JAN-2007, (EDW) */

/*        Corrected typo in the previous version string; */
/*        from: */

/*           09-DEC-203 */

/*        to */

/*           09-DEC-2003 */

/* -    Version 1.1.0, 09-DEC-2003, (EDW) */

/*       Set the SYMLEN value to 1024 to match the same */
/*       value in niospk. */

/* -    Beta Version 1.0.0, 11-AUG-1992 (MJS) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     This routine will use discovery check-in. */

    if (return_()) {
	return ;
    }

/*     Compute the lengths of the strings involved. */

    intstr_(index, cnum, (ftnlen)10);
    clen = rtrim_(cnum, (ftnlen)10);
    blen = rtrim_(base, base_len);
    plen = rtrim_(pnter, pnter_len);
    total = clen + blen + plen + 2;

/*     TOTAL must be SYMLEN characters, or fewer. */

    if (total > 1024) {
	chkin_("CRTPTR", (ftnlen)6);
	setmsg_("Symbol exceeds # characters. Increase the value of SYMLEN.", 
		(ftnlen)58);
	errint_("#", &c__1024, (ftnlen)1);
	sigerr_("SPICE(BUFFERTOOSMALL)", (ftnlen)21);
	chkout_("CRTPTR", (ftnlen)6);
	return ;
    }

/*     And TOTAL must be LEN(CRTPTR) characters, or fewer. */

    if (total > i_len(ret_val, ret_val_len)) {
	chkin_("CRTPTR", (ftnlen)6);
	setmsg_("Symbol exceeds the dimension of CRTPTR.", (ftnlen)39);
	sigerr_("SPICE(DIMENSIONTOOSMALL)", (ftnlen)24);
	chkout_("CRTPTR", (ftnlen)6);
	return ;
    }

/*     Form the symbol 'BASE~INDEX~PNTER'. */

    s_copy(sym, " ", (ftnlen)1024, (ftnlen)1);
/* Writing concatenation */
    i__1[0] = blen, a__1[0] = base;
    i__1[1] = 1, a__1[1] = "~";
    i__1[2] = clen, a__1[2] = cnum;
    i__1[3] = 1, a__1[3] = "~";
    i__1[4] = plen, a__1[4] = pnter;
    s_cat(sym, a__1, i__1, &c__5, (ftnlen)1024);
    s_copy(ret_val, sym, ret_val_len, (ftnlen)1024);
    return ;
} /* crtptr_ */

