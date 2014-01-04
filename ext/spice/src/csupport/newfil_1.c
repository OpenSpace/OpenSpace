/* newfil_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      NEWFIL_1 ( Generate a filename that does not exist ) */
/* Subroutine */ int newfil_1__(char *pattrn, char *file, ftnlen pattrn_len, 
	ftnlen file_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char name__[255];
    logical done;
    char this__[255], fname[255];
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, ftnlen)
	    ;
    logical nomore;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    extern logical exists_(char *, ftnlen), return_(void);
    extern /* Subroutine */ int fststr_(char *, char *, ftnlen, ftnlen), 
	    nxtstr_(char *, char *, char *, ftnlen, ftnlen, ftnlen);

/* $ Abstract */

/*     This routine generates a filename that is derived from */
/*     the input PATTRN and returns the name that was generated */
/*     in FILE. */

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

/*      None. */

/* $ Keywords */

/*       FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      PATTRN     I   is a name pattern following the rules of MAKSTR */
/*      FILE       O   the name of the file generated. */

/* $ Detailed_Input */

/*     PATTRN      The description below is lifted without change */
/*                 from the routine MAKSTR. */

/*                 PATTRN is a string that specifies a pattern that */
/*                 all strings in a sequence must match. There are */
/*                 several special substrings in PATTRN that must */
/*                 be recognized. */

/*                 1) A substring of the form '<*>' (where * is used */
/*                    as a variable length wildcard character) is called */
/*                    an expansion. The substring that occurs between */
/*                    the angle brackets < > is called the invisible */
/*                    portion of the expansion.  When the tokens of */
/*                    PATTRN are counted the invisible portion of the */
/*                    expansion is not counted.  Thus an expansion has */
/*                    exactly two tokens '<' and '>'  The invisible */
/*                    portion of the expansion must not contain */
/*                    any of the characters '<', '>', '{', or '}'. */

/*                 2) A substring of the form '{#-$}' where # and $ */
/*                    stand for any chacter from the set */
/*                    '0', ... , '9', 'a', ... , 'z' is called a */
/*                    restriction. */

/*                 A pattern may consist of any collection of */
/*                 characters.  However, the characters '<' and */
/*                 '>' must always occur in balanced pairs with '<' */
/*                 on the left and '>' on the right. Moreover, they */
/*                 cannot be nested even if they are balanced. Similary */
/*                 '{' and '}' must always appear as a balanced pair */
/*                 and have exactly 3 characters between them.  The */
/*                 first is a lower case letter or a digit.  The second */
/*                 letter may be anything (usually a hyphen, colon or */
/*                 comma).  The third character must */
/*                 also be a letter between 0, ... ,9, a, b, ... , z */
/*                 and must occur later in the collating sequence than */
/*                 the first letter in the triple that occurs between */
/*                 '{' and '}'. */

/*                 For example the following are valid patterns */

/*                 PAT_<Value: >_{0-9}{a-z}{a-d} */
/*                 COUNTER{0-9}{0-9}{0-9}{0-9} */
/*                 COUNTER{0:9}{0,9}{a;b} */

/*                 but the following are not */

/*                 PAT_<<>>_{0-9}{a-z}{a-d}    --- Nested < > */
/*                 COUNTER{9-0}                --- 9 before 0 */
/*                 PAT_{0to0}                  --- 4 characters between{} */
/*                 PAT_{A-Z}                   --- uppercase letters in{} */
/*                 PAT_{+-$}                   --- bad characters in {} */

/*                 Pattern should be viewed as consisting of a sequence */
/*                 of tokens.  The tokens consist of characters that */
/*                 are not part of an expansion or restriction */
/*                 restrictions and the '<' and '>' characters of */
/*                 any expansion. */

/* $ Detailed_Output */

/*     FILE        is a string that is the name of the file that was */
/*                 generated.  The name of the file will match the */
/*                 input PATTRN and will be the first name generated */
/*                 from PATTRN that does not exist.  See the routine */
/*                 MAKSTR for a more detailed explanation of the names */
/*                 that are generated using FSTSTR and NXTSTR. */

/* $ Parameters */

/*      None. */

/* $ Files */

/*      None. */

/* $ Exceptions */

/*     None. */

/* $ Particulars */

/*     This is a utility routine for creating a file name that */
/*     can be opened without fear of name collisions, i.e., it */
/*     creates tha name of a file that does not exist, thus */
/*     guaranteeing that you can open the file. */

/* $ Examples */

/*     Suppose that you need a utility file for holding some */
/*     temporary data structure in a program that makes use */
/*     of NSPIO for its IO.  Then you could make the following */
/*     call */

/*        PATTRN = 'util{0-9}{0-9}{0-9}{0-9}.tmp' */

/*        CALL NEWFIL ( PATTRN, FILE ) */

/*     If successful, FILE will hold the name of the new file. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*      K.R. Gehringer  (JPL) */
/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -     Beta Version 1.0.0, 30-MAY-1996 (KRG) (WLT) */

/* -& */
/* $ Index_Entries */

/*     Create a new file name from a pattern */

/* -& */

/*     Spicelib routines. */


/*     Local Parameters */

/*     Length of a filename. */


/*     Local Variables */

    if (return_()) {
	return 0;
    } else {
	chkin_("NEWFIL_1", (ftnlen)8);
    }
    s_copy(fname, " ", (ftnlen)255, (ftnlen)1);

/*     Get the first filename in the pattern space. */

    fststr_(pattrn, fname, pattrn_len, (ftnlen)255);
    s_copy(name__, fname, (ftnlen)255, (ftnlen)255);
    nomore = FALSE_;
    done = FALSE_;

/*     Look for a file name that does not already exist. */

    while(! done) {
	s_copy(this__, name__, (ftnlen)255, (ftnlen)255);
	s_copy(name__, " ", (ftnlen)255, (ftnlen)1);
	nxtstr_(pattrn, this__, name__, pattrn_len, (ftnlen)255, (ftnlen)255);
	done = s_cmp(name__, fname, (ftnlen)255, (ftnlen)255) == 0;
	if (! done) {
	    if (! exists_(name__, (ftnlen)255)) {
		done = TRUE_;
	    }
	} else {
	    nomore = TRUE_;
	}
    }
    if (nomore) {
	s_copy(file, " ", file_len, (ftnlen)1);
	setmsg_("It was not possible to create a file name using '#' as the "
		"pattern. All of the file names that can be generated from th"
		"is pattern already exist.", (ftnlen)144);
	errch_("#", pattrn, (ftnlen)1, pattrn_len);
	sigerr_("SPICE(CANNOTMAKEFILE)", (ftnlen)21);
	chkout_("NEWFIL_1", (ftnlen)8);
	return 0;
    }
    s_copy(file, name__, file_len, (ftnlen)255);
    chkout_("NEWFIL_1", (ftnlen)8);
    return 0;
} /* newfil_1__ */

