/* dspvrs.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;

/* $Procedure      DSPVRS ( Display Version ) */
/* Subroutine */ int dspvrs_(char *pname, char *vrsn, ftnlen pname_len, 
	ftnlen vrsn_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char line[80];
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen), tostdo_(char *, ftnlen), tkvrsn_(char *, char *, ftnlen, 
	    ftnlen);
    char tkv[8];

/* $ Abstract */

/*    This routine displays the name of a program as well as its */
/*    version and the version of SPICELIB that the calling */
/*    program has been linked with. */

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

/*     UTITITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     PNAME      I   The name of the calling program */
/*     VRSN       I   The version number of the calling program */

/* $ Detailed_Input */

/*     PNAME      is the name of the calling program */

/*     VRSN       is the version number of the calling program */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This is a utility routine for printing the name and */
/*     version number of a program as well as the identifier */
/*     of the SPICELIB library that was used in linking */
/*     the program. */

/*     The following template is filled out and then displayed */
/*     at standard output. */

/*       <pname> --- Version <vrsn>,  SPICE Toolkit <tkvrsn> */

/* $ Examples */

/*     Suppose you are creating an program called "DoIt" */
/*     and you would like to have the name and current version */
/*     of the program displayed along with the linked version */
/*     of SPICELIB at some point in the execution of the program, */
/*     Here is how you can use this routine to perform the */
/*     version display function. */

/*        CALL DSPVRS ( 'DoIt', '1.0.0' ) */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 26-SEP-1997 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Print a version line */

/* -& */
    tkvrsn_("toolkit", tkv, (ftnlen)7, (ftnlen)8);
    s_copy(line, pname, (ftnlen)80, pname_len);
    suffix_("Version", &c__1, line, (ftnlen)7, (ftnlen)80);
    suffix_(vrsn, &c__1, line, vrsn_len, (ftnlen)80);
    suffix_(", SPICE Toolkit", &c__0, line, (ftnlen)15, (ftnlen)80);
    suffix_(tkv, &c__1, line, (ftnlen)8, (ftnlen)80);
    tostdo_(line, (ftnlen)80);
    return 0;
} /* dspvrs_ */

