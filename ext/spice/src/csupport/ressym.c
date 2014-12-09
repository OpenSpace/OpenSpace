/* ressym.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

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

/* Subroutine */ int ressym_(char *input, char *output, ftnlen input_len, 
	ftnlen output_len)
{
    logical tran1, tran2;
    integer e, i__, r__;
    char space[1];
    extern /* Subroutine */ int chkin_(char *, ftnlen), geteq_(char *, ftnlen)
	    , qtran_(char *, char *, logical *, ftnlen, ftnlen), stran_(char *
	    , char *, logical *, ftnlen, ftnlen), ljust_(char *, char *, 
	    ftnlen, ftnlen);
    extern logical failed_(void);
    logical change;
    extern /* Subroutine */ int replch_(char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen);
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    char equote[1];
    extern /* Subroutine */ int prtrap_(char *, logical *, ftnlen);
    char substr[255];
    extern /* Subroutine */ int nthuqw_(char *, integer *, char *, char *, 
	    integer *, ftnlen, ftnlen, ftnlen);
    char tab[1];
    integer loc;

    chkin_("RESSYM", (ftnlen)6);
    *(unsigned char *)tab = '\t';
    *(unsigned char *)space = ' ';
    geteq_(equote, (ftnlen)1);
    replch_(input, tab, space, output, input_len, (ftnlen)1, (ftnlen)1, 
	    output_len);
    prtrap_(output, &change, output_len);

/*     Now we just loop until all translations have */
/*     been performed.  We do: */

/*        1) symbol resolution */
/*        2) query resolution */
/*        3) tab removal */

    while(change) {
	change = FALSE_;
	tran1 = TRUE_;
	tran2 = TRUE_;

/*        First we resolve all symbols.  After each pass we check */
/*        that we have not created a command that must be trapped. */

	while(tran1 && tran2) {
	    stran_(output, output, &tran1, output_len, output_len);
	    prtrap_(output, &tran2, output_len);

/*           Determine whether or not more changes are possible */
/*           at this point. */

	    change = (change || tran1) && tran2 && ! failed_();
	}

/*        If we don't have any errors we take a stab at replacing */
/*        all queries.  Note that queries can not result in changing */
/*        anything that isn't a query so we don't have to trap */
/*        inside the loop.  Note that this means you can't have */
/*        a command like DEFINE? SYMBOL? VALUE? and just replace */
/*        the first two queries.  You've got to do them all.  If */
/*        you want a symbol to have a query you must do it this */
/*        way:  DEFINE SYMBOL  QUERY?  That way the queries won't */
/*        get resolve too soon. */

/*        Note:  This can easily be changed so that if a query */
/*        introduces a symbol, we immediately loop back to the */
/*        symbol resolution branch.  Simply change the DO WHILE */
/*        loop below to an IF.  The "loop" will then terminate */
/*        after one execution leaving any remaining queries */
/*        untouched until the next pass through the loop. */

	if (failed_()) {
	    chkout_("RESSYM", (ftnlen)6);
	    return 0;
	}
	tran1 = ! failed_();
	while(tran1) {
	    qtran_(output, output, &tran1, output_len, output_len);
	    replch_(output, tab, space, output, output_len, (ftnlen)1, (
		    ftnlen)1, output_len);
	    change = change || tran1;
	}
	prtrap_(output, &tran2, output_len);
	change = change && tran2;
	if (failed_()) {
	    chkout_("RESSYM", (ftnlen)6);
	    return 0;
	}
    }
    if (tran2) {

/*        We remove the special markers that may have been present to */
/*        protect symbol or query resolution. */

	i__ = 1;
	nthuqw_(output, &i__, " ", substr, &loc, output_len, (ftnlen)1, (
		ftnlen)255);
	while(loc > 0) {
	    r__ = lastnb_(substr, (ftnlen)255) - 1;
	    e = loc + r__;
	    replch_(output + (loc - 1), equote, space, output + (loc - 1), e 
		    - (loc - 1), (ftnlen)1, (ftnlen)1, e - (loc - 1));
	    ++i__;
	    nthuqw_(output, &i__, " ", substr, &loc, output_len, (ftnlen)1, (
		    ftnlen)255);
	}
    }

/*     Finally, left justify the commmand. */

    ljust_(output, output, output_len, output_len);
    chkout_("RESSYM", (ftnlen)6);
    return 0;
} /* ressym_ */

