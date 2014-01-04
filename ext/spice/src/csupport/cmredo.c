/* cmredo.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      CMREDO ( COMMND loop trap ) */
/* Subroutine */ int cmredo_(char *commnd, integer *from, logical *trap, 
	ftnlen commnd_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char exit[32], rest[300], stop[32];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    static integer b1, b2, e1, e2;
    extern integer rtrim_(char *, ftnlen);
    static char start[32];
    extern logical m2wmch_(char *, integer *, integer *, char *, ftnlen, 
	    ftnlen);
    static char scndwd[32];
    extern /* Subroutine */ int trnlat_(char *, char *, ftnlen, ftnlen), 
	    putcom_(char *, integer *, ftnlen);
    static char frstwd[32];
    extern /* Subroutine */ int nextwd_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen);

/* $ Abstract */

/*    This routine examines COMMND and checks to see if it */
/*    should be sent to the COMMND loop stuff so that it */
/*    can be re-evaluated. */

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

/*     COMMAND LOOP */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     COMMND    I   A COMMND string to be checked for special syntax */
/*     TRAP       O   Indicates whether the string has special form */

/* $ Detailed_Input */

/*     COMMND    is a string that represents some COMMND. */


/* $ Detailed_Output */

/*     TRAP       is a logical idicating whether the string was special */
/*                and was put on the COMMND buffer. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This routine examines the input COMMND to see if it is one */
/*     of the following. */

/*     EDIT number */
/*     RECALL ALL */
/*     RECALL number */
/*     START  word */
/*     STOP */
/*     EXIT */


/* $ Examples */

/*     Later, */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*       W.L. Taber      (JPL) */

/* $ Literature_References */

/*       None. */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 23-AUG-1995 (WLT) */

/*        Updated the routine so that EDIT *, DO * and RECALL * */
/*        are trapped. */


/* -& */
/* $ Index_Entries */

/*     «We need a permuted index entry */

/* -& */

    if (first) {
	trnlat_("STOP", stop, (ftnlen)4, (ftnlen)32);
	trnlat_("EXIT", exit, (ftnlen)4, (ftnlen)32);
	trnlat_("START", start, (ftnlen)5, (ftnlen)32);
	first = FALSE_;
    }
    nextwd_(commnd, frstwd, rest, commnd_len, (ftnlen)32, (ftnlen)300);
    nextwd_(rest, scndwd, rest, (ftnlen)300, (ftnlen)32, (ftnlen)300);
    ucase_(frstwd, frstwd, (ftnlen)32, (ftnlen)32);
    ucase_(scndwd, scndwd, (ftnlen)32, (ftnlen)32);
    b1 = 1;
    b2 = 1;
    e1 = rtrim_(frstwd, (ftnlen)32);
    e2 = rtrim_(scndwd, (ftnlen)32);
    if (s_cmp(rest, " ", (ftnlen)300, (ftnlen)1) != 0) {
	*trap = FALSE_;
	return 0;
    }
    if (s_cmp(frstwd, " ", (ftnlen)32, (ftnlen)1) == 0) {
	*trap = FALSE_;
	return 0;
    }
    if (s_cmp(frstwd, start, (ftnlen)32, (ftnlen)32) == 0) {
	*trap = TRUE_;
	putcom_(commnd, from, commnd_len);
	return 0;
    } else if (s_cmp(frstwd, exit, (ftnlen)32, (ftnlen)32) == 0 && s_cmp(
	    scndwd, " ", (ftnlen)32, (ftnlen)1) == 0 && *from != 2) {
	*trap = TRUE_;
	putcom_(commnd, from, commnd_len);
	return 0;
    } else if (s_cmp(frstwd, stop, (ftnlen)32, (ftnlen)32) == 0 && s_cmp(
	    scndwd, " ", (ftnlen)32, (ftnlen)1) == 0 && *from != 2) {
	*trap = TRUE_;
	putcom_(commnd, from, commnd_len);
	return 0;
    } else if (*from != 2) {
	*trap = FALSE_;
	return 0;
    } else if (s_cmp(scndwd, " ", (ftnlen)32, (ftnlen)1) == 0 && ! m2wmch_(
	    frstwd, &b1, &e1, "RECALL", (ftnlen)32, (ftnlen)6)) {
	*trap = FALSE_;
	return 0;
    } else if (m2wmch_(frstwd, &b1, &e1, "RECALL", (ftnlen)32, (ftnlen)6) && 
	    m2wmch_(scndwd, &b2, &e2, "@int(1:20)", (ftnlen)32, (ftnlen)10)) {
	*trap = TRUE_;
	putcom_(commnd, from, commnd_len);
    } else if (m2wmch_(frstwd, &b1, &e1, "RECALL", (ftnlen)32, (ftnlen)6) && 
	    m2wmch_(scndwd, &b2, &e2, "ALL", (ftnlen)32, (ftnlen)3)) {
	*trap = TRUE_;
	putcom_(commnd, from, commnd_len);
    } else if (m2wmch_(frstwd, &b1, &e1, "EDIT", (ftnlen)32, (ftnlen)4) && 
	    m2wmch_(scndwd, &b2, &e2, "@int(1:20)", (ftnlen)32, (ftnlen)10)) {
	*trap = TRUE_;
	putcom_(commnd, from, commnd_len);
    } else if (m2wmch_(frstwd, &b1, &e1, "DO", (ftnlen)32, (ftnlen)2) && 
	    m2wmch_(scndwd, &b2, &e2, "@int(1:20)", (ftnlen)32, (ftnlen)10)) {
	*trap = TRUE_;
	putcom_(commnd, from, commnd_len);
    } else if (m2wmch_(frstwd, &b1, &e1, "RECALL", (ftnlen)32, (ftnlen)6) && 
	    s_cmp(scndwd, " ", (ftnlen)32, (ftnlen)1) != 0 && s_cmp(rest, 
	    " ", (ftnlen)300, (ftnlen)1) == 0) {
	*trap = TRUE_;
	putcom_(commnd, from, commnd_len);
    } else if (m2wmch_(frstwd, &b1, &e1, "EDIT", (ftnlen)32, (ftnlen)4) && 
	    s_cmp(scndwd, " ", (ftnlen)32, (ftnlen)1) != 0 && s_cmp(rest, 
	    " ", (ftnlen)300, (ftnlen)1) == 0) {
	*trap = TRUE_;
	putcom_(commnd, from, commnd_len);
    } else if (m2wmch_(frstwd, &b1, &e1, "DO", (ftnlen)32, (ftnlen)2) && 
	    s_cmp(scndwd, " ", (ftnlen)32, (ftnlen)1) != 0 && s_cmp(rest, 
	    " ", (ftnlen)300, (ftnlen)1) == 0) {
	*trap = TRUE_;
	putcom_(commnd, from, commnd_len);
    } else {
	*trap = FALSE_;
    }
    return 0;
} /* cmredo_ */

