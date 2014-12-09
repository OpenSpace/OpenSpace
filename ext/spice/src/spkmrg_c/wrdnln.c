/* wrdnln.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      WRDNLN (Write a definition line) */
/* Subroutine */ int wrdnln_(char *k, char *v, integer *i__, integer *unit, 
	ftnlen k_len, ftnlen v_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern integer posr_(char *, char *, integer *, ftnlen, ftnlen);
    static integer b, e;
    static char l[350];
    extern integer ltrim_(char *, ftnlen);
    static integer vlmax, vpmax;
    extern integer rtrim_(char *, ftnlen);
    static integer ep, kl, kp, vp;
    extern logical return_(void);
    extern /* Subroutine */ int writln_(char *, integer *, ftnlen);
    static integer vp1;
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);

/* $ Abstract */

/*     Write a line, keyword and value, to a NIOSPK command file. */

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
/*      K         I    Name of keyword. */
/*      V         I    Keyword's value. */
/*      I         I    Indentation of keyword. */
/*      UNIT      I    Unit of command file. */

/*      EQPOS     P    Default equal sign position. */
/*      LL        P    Preferred length of line. */
/*      LINLEN     P    Maximum length of line. */

/* $ Detailed_Input */

/*      K         is the name of the keyword in the assignment. */

/*      V         is the value of the assignment. */

/*      I         is the position the keyword should be indented to, */
/*                if possible. */

/*      UNIT      is the unit of the file to which the assignment will */
/*                be written. */

/* $ Detailed_Output */

/*      None. */

/* $ Parameters */

/*     EQPOS      is the default position of the equal sign. */

/*     LL         is the preferred length of the line. Lines will */
/*                be broken at this position, or before, if possible. */

/*     LINLEN      is the maximum length of the line. This value should */
/*                be set high enough so that the keyword and value (if */
/*                non-blank) can fit on one line. */

/* $ Exceptions */

/*     This routine does not signal any errors. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine will try to form the best looking assignment, taking */
/*     into account indentation, and equal sign position. The value may */
/*     be extended over multiple lines, if needed. */

/*     K and V are not changed. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     M.J. Spencer   (JPL) */

/* $ Version */

/* -    SPKMERGE Version 1.2.0, 30-APR-2014 (BVS) */

/*        Renamed LLMAX to LINLEN and increased it from 128 to 350 */
/*        (consistent with other modules) */

/* -    SPKMERGE Version 1.1.0, 30-MAY-1996 (WLT) */

/*        Put the DO WHILE construct back in and replace WRITE */
/*        statement with a call to WRITLN. */

/* -    Beta Version 1.0.0, 10-AUG-1992 (MJS) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Save all. */


/*     Statement Functions */

/*     C just returns the minimum of X and VLMAX. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }

/*     K   = keyword */
/*     V   = value */
/*     KP  = keyword position */
/*     VP  = value position */
/*     KL  = keyword length */
/*     VL  = value length */
/*     B:E = substring in value */
/*     L   = line to screen */

/*     If we assumed the keyword and value would fit on one line, then */
/*     this routine would be trivial. Unfortunately, values may */
/*     be long sometimes, longer than LL. In such cases, */
/*     we'll lose the indentation and any spaces between the */
/*     keyword and equal sign (all but one); if the value still */
/*     exceeds LL, we'll try breaking the value at word boundaries. */
/*     If the value is really ugly---long and no spaces---we'll just */
/*     have to let it extend past LL, but not past LINLEN. */

    vlmax = i_len(v, v_len);
    kl = rtrim_(k, k_len);
/* Computing MAX */
    i__1 = *i__ + kl + 1;
    vpmax = max(i__1,22) + 2;

/*     First find B and E, since this will determine where we place the */
/*     keyword and equal sign. */

    e = 0;
    i__2 = e + 1;
    i__1 = min(i__2,vlmax) - 1;
    b = e + ltrim_(v + i__1, v_len - i__1);
    i__1 = b + 79 - vpmax + 1;
    e = min(i__1,vlmax);
    if (e != vlmax) {
	i__1 = b + 79 - vpmax + 1;
	i__2 = min(i__1,vlmax);
	e = posr_(v, " ", &i__2, v_len, (ftnlen)1) - 1;
	if (e < b) {
	    i__1 = b + 79 - vpmax + 1;
	    i__2 = min(i__1,vlmax);
	    e = pos_(v, " ", &i__2, v_len, (ftnlen)1) - 1;
	    if (e < b) {
		e = vlmax;
	    }
	}
    }

/*     Now we can figure out the keyword position (KP) and the value */
/*     position (VP). EP will always be two less than VP. We'll always */
/*     have a minimum, thus, of one space surrounding the equal sign. */

/* Computing MAX */
/* Computing MIN */
    i__3 = *i__, i__4 = 79 - e + b - kl - 3;
    i__1 = 1, i__2 = min(i__3,i__4);
    kp = max(i__1,i__2);
/* Computing MIN */
/* Computing MAX */
    i__3 = 79 - e + b, i__4 = kp + kl + 3;
    i__1 = vpmax, i__2 = max(i__3,i__4);
    vp = min(i__1,i__2);
    vp1 = vp;
    ep = vp - 2;
    s_copy(l, " ", (ftnlen)350, (ftnlen)1);
    s_copy(l + (kp - 1), k, 350 - (kp - 1), k_len);
    s_copy(l + (ep - 1), "=", 350 - (ep - 1), (ftnlen)1);

/*     Same thing over again, except we have no keyword, or equal sign. */

    while(b <= vlmax && s_cmp(v + (b - 1), " ", v_len - (b - 1), (ftnlen)1) !=
	     0) {

/*        If E extends past VLMAX, we'd better do something about it. */

	if (vp + e - b > vlmax) {
	    e = vlmax - vp + b;
	}
	s_copy(l + (vp - 1), v + (b - 1), 350 - (vp - 1), e - (b - 1));
	writln_(l, unit, (ftnlen)350);
	i__2 = e + 1;
	i__1 = min(i__2,vlmax) - 1;
	b = e + ltrim_(v + i__1, v_len - i__1);
	i__1 = b + 79 - vp1 + 1;
	e = min(i__1,vlmax);
	if (e != vlmax) {
	    i__1 = b + 79 - vp1 + 1;
	    i__2 = min(i__1,vlmax);
	    e = posr_(v, " ", &i__2, v_len, (ftnlen)1) - 1;
	    if (e < b) {
		i__1 = b + 79 - vp1 + 1;
		i__2 = min(i__1,vlmax);
		e = pos_(v, " ", &i__2, v_len, (ftnlen)1) - 1;
		if (e < b) {
		    e = vlmax;
		}
	    }
	}

/*        Since we only have a value on this line, we can shift it to */
/*        the left if we have to to get it visible. */

/* Computing MIN */
/* Computing MAX */
	i__3 = 79 - e + b;
	i__1 = vp1, i__2 = max(i__3,1);
	vp = min(i__1,i__2);
	s_copy(l, " ", (ftnlen)350, (ftnlen)1);
    }

/*     That was fun. */

    return 0;
} /* wrdnln_ */

