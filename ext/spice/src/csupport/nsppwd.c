/* nsppwd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure      NSPPWD ( NSP --- Page width) */
/* Subroutine */ int nsppwd_0_(int n__, char *margin, integer *left, integer *
	right, ftnlen margin_len)
{
    /* Initialized data */

    static integer myleft = 1;
    static integer myrght = 80;

    /* System generated locals */
    integer i__1, i__2, i__3, i__4;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    static char strlft[16];
    extern /* Subroutine */ int intstr_(integer *, char *, ftnlen);
    static char strrht[16];

/* $ Abstract */

/*    This routine is an umbrella routine used to cover the */
/*    three entry points used for setting and retrieving */
/*    page width settings. */

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

/*     PAGE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MARGIN     O   A "NICEIO" style string for left and right margins */
/*     LEFT      I/O  The column to be used for the left margin. */
/*     RIGHT     I/O  The column to be used for the right margin. */
/*     MXPGWD     P   Maximum allowed page width. */

/*     The function returns */

/* $ Detailed_Input */

/*     LEFT      is an integer that sets the left margin. */

/*     RIGHT     is an integer that sets the right margin. */


/* $ Detailed_Output */

/*     LEFT      is the current left margin. */

/*     RIGHT     is the current right margin. */

/* $ Parameters */

/*     MXPGWD    is the maximum allowed page width.  This is here */
/*               so that this routine can be error free.  It is */
/*               possible that on some systems this could be made */
/*               substantially larger. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This routine is an umbrella for the three entry points */

/*     NSPMRG   ---  returns a NICEIO/NICEBT/NICEPR style string */
/*                   of the form 'LEFT number1 RIGHT number2 ' */
/*                   where number1 and number2 give the left and */
/*                   right margins to use when creating NICEIO */
/*                   style output. */

/*                   Other style items may be added to this string */
/*                   for use in creating output. */

/*     NSPSLR   ---  sets the left and right margins to be used */
/*                   when creating a style string.  Note there are */
/*                   no erroneous inputs.  Values are forced into */
/*                   a "reasonable" range. */

/*     NSPGLR   ---  get the current left and right margins. */

/* $ Examples */

/*     To set the margins to 1 to 72 make the following call: */

/*        CALL NSPSLR ( 1, 72 ) */

/*     To get back a NICEPR string that will be used for setting */
/*     the style of page output. */

/*        CALL NSPMRG ( MARGIN ) */

/*     To get the numeric values (so you don't have to parse MARGIN) */
/*     make the following call: */

/*        CALL NSPGLR ( LEFT, RIGHT ) */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*       W.L. Taber      (JPL) */

/* $ Literature_References */

/*       None. */

/* $ Version */

/* -    Command Loop Version 1.0.0, 1-AUG-1995 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Set or get command loop page margins. */

/* -& */
    switch(n__) {
	case 1: goto L_nspmrg;
	case 2: goto L_nspslr;
	case 3: goto L_nspglr;
	}

    return 0;

L_nspmrg:

/*        Return the current margins to be used by the NICEIO and NICEPR */
/*        routines. */

    intstr_(&myleft, strlft, (ftnlen)16);
    intstr_(&myrght, strrht, (ftnlen)16);
    s_copy(margin, "LEFT", margin_len, (ftnlen)4);
    suffix_(strlft, &c__1, margin, (ftnlen)16, margin_len);
    suffix_("RIGHT", &c__1, margin, (ftnlen)5, margin_len);
    suffix_(strrht, &c__1, margin, (ftnlen)16, margin_len);
    return 0;

L_nspslr:

/*        Set the left and right margins to be used when creating */
/*        margin style strings in the entry point above.  Note */
/*        we force these to be reasonable.  No error checking is */
/*        done. */

/* Computing MAX */
/* Computing MIN */
    i__3 = min(*left,*right);
    i__1 = 1, i__2 = min(i__3,129);
    myleft = max(i__1,i__2);
/* Computing MIN */
/* Computing MAX */
    i__3 = max(*left,*right), i__4 = myleft + 2;
    i__1 = 131, i__2 = max(i__3,i__4);
    myrght = min(i__1,i__2);
    return 0;

L_nspglr:

/*        Get the left and right margins that are currently */
/*        being used. */

    *left = myleft;
    *right = myrght;
    return 0;
} /* nsppwd_ */

/* Subroutine */ int nsppwd_(char *margin, integer *left, integer *right, 
	ftnlen margin_len)
{
    return nsppwd_0_(0, margin, left, right, margin_len);
    }

/* Subroutine */ int nspmrg_(char *margin, ftnlen margin_len)
{
    return nsppwd_0_(1, margin, (integer *)0, (integer *)0, margin_len);
    }

/* Subroutine */ int nspslr_(integer *left, integer *right)
{
    return nsppwd_0_(2, (char *)0, left, right, (ftnint)0);
    }

/* Subroutine */ int nspglr_(integer *left, integer *right)
{
    return nsppwd_0_(3, (char *)0, left, right, (ftnint)0);
    }

