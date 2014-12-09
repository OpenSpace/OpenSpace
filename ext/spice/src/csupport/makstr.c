/* makstr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      MAKSTR (Make String ) */
/* Subroutine */ int makstr_0_(int n__, char *pattrn, char *this__, char *
	next, ftnlen pattrn_len, ftnlen this_len, ftnlen next_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen);

    /* Local variables */
    logical keep;
    integer i__, j, k;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    integer min__, max__;

/* $ Abstract */

/*     Make a string matching a pattern.  This routine serves as an */
/*     umbrella routine for the two entry points FSTSTR and NXTSTR. */

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

/*       Utility */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      PATTRN     I   FSTSTR, NXTSTR */
/*      THIS       I   NXTSTR */
/*      NEXT       O   FSTSTR, NXSTR */

/* $ Detailed_Input */

/*     PATTRN      is a string that specifies a pattern that all strings */
/*                 in a sequence must match. There are several special */
/*                 substrings in PATTRN that must be recognized. */

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

/*     THIS        is a string that should be incremented to get the */
/*                 NEXT string that matches the pattern. */

/*                 Note THIS must match the input pattern. */

/*                 The tokens of THIS are the characters upto and */
/*                 including the last non-blank character of THIS. */

/*                 This should have the same number of tokens as does */
/*                 PATTRN. */

/*                 Suppose that TOKTHS (I) is the I'th token of THIS */
/*                 and that TOKPAT(I) is the I'th token of PATTRN. */

/*                 If TOKPAT(I) is a restriction then TOKTHS(I) must */
/*                 be one of the characters belonging to the range */
/*                 of the restriction. */

/*                 Otherwise TOKPAT(I) and TOKTHS(I) match. */

/*                 Thus the pattern */

/*                   'XXX<value: >{0-9}{0-z}' */

/*                 Matches */

/*                   'THIS_5a' */

/*                 This kind of matching is of course a bit */
/*                 confusing.  It is probably more useful to */
/*                 have THIS take all of its tokens to be identical */
/*                 to the character tokens of of PATTRN and match */
/*                 the restriction tokens in all other cases. */

/*                 In particular, the routine FSTSTR, will take */
/*                 PATTRN as an input and produce the a first */
/*                 string in the sequence of strings that matches */
/*                 PATTRN by simply copying the character tokens */
/*                 of PATTRN to the output string and taking the */
/*                 lower bound of the restrictions of PATTRN */
/*                 to get the matching tokens for each restriction. */

/*                 See FSTSTR for a more complete discussion. */

/* $ Detailed_Output */

/*     NEXT        See the entry points FSTSTR and NXTSTR */


/* $ Parameters */

/*      None. */

/* $ Files */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This is a rather immature routine that can assist with the */
/*     problem of constructing a sequence of character strings. */

/*     This routine was written as a support routine for the */
/*     SPICE program CHRISTEN and the routine NOMEN.  As such */
/*     it was possible to ensure that all of the detailed conditions */
/*     of PATTRN and THIS were met by the routines that call this. */

/*     However, this routine can prove useful in other contexts and */
/*     is provided so that you can easily produce a large sequence of */
/*     non-repeating character strings.  But  YOU MUST BE CAREFUL */
/*     WITH YOUR USE OF THIS ROUTINE.  Unlike most SPICE routines */
/*     there is no exception handling done.  If you pass in a bad PATTRN */
/*     or value for THIS that does not match PATTRN the result are */
/*     of this routine are unpredictable.  The routine will certainly */
/*     not diagnose the problem and can possibly cause your program */
/*     to crash with no diagnostics to help with finding the problem. */

/*     If you simply need to produce a sequence of strings, you */
/*     should probably avoid putting expansions ( substrings like */
/*     <something> ) in your input pattern.  These are special */
/*     strings that support the tasks needed by NOMEN and CHRISTEN. */

/*     Stick to simple patterns such as the one shown here: */

/*        PATTRN = 'base{0-z}{0-z}{0-z}{0-z}.tmp' */

/*        THIS   = 'base0000.tmp' */

/*     For creating file names or unique non-frequently repeating */
/*     strings, this will probably do the job. */

/*     Note that upper case letters are not supported in PATTRNs, this */
/*     is a UNIX-ish restriction (most file names are written in */
/*     lower case in UNIX).  This routine could be easily modified */
/*     to support a wider range of characters.  Or if you want all */
/*     uppercase characters, apply the SPICE routine UPPER to NEXT */
/*     when you get back from your call to NXTSTR. */

/*     Still even with all the restrictions and lack of exception */
/*     handling this does solve a basic problem of creating an */
/*     increasing sequence of character strings and saves you */
/*     from the task of figuring out the details (in particular */
/*     how to cascade up the string when you have many letters */
/*     to change to get to the next string). */

/*     The most common useage is to use FSTSTR to get a first string */
/*     in a sequence that matches PATTRN and then to call NXTSTR */
/*     to produce subsequent matching strings. */

/* $ Examples */

/*     See the inividiual entry points. */

/* $ Restrictions */

/*     There are lots of restrictions.  See the detailed input */
/*     and particulars for all the warnings. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -     Support Version 1.1.0, 18-JUN-1999 (WLT) */

/*         Placed a RETURN statement before the first entry point */
/*         to protect against the coding error of calling the */
/*         subroutine MAKSTR directly. */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/* -    Prototype Version 1.0.0, 16-APR-1994 (WLT) */


/* -& */

/*     Spicelib functions */


/*     Local Varialbes */

    switch(n__) {
	case 1: goto L_fststr;
	case 2: goto L_nxtstr;
	}

    return 0;
/* $Procedure      FSTSTR ( First string matching a pattern ) */

L_fststr:
/* $ Abstract */

/*     Given a naming pattern, this routine produces the first */
/*     legal name implied by the pattern. */

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

/*       UTILITY */

/* $ Declarations */

/*     IMPLICIT NONE */
/*     CHARACTER*(*)         PATTRN */
/*     CHARACTER*(*)         NEXT */

/* $ Brief_I/O */
/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      PATTRN     I   A pattern to use when constructing strings */
/*      NEXT       O   The first pattern that fits the input pattern */

/* $ Detailed_Input */

/*     PATTRN     is a pattern from which NEXT will be constructed. */
/*                See the discussion of PATTRN in the umbrella routine */
/*                for more details. */

/* $ Detailed_Output */

/*     NEXT       is the first string in the ASCII collating sequence */
/*                 that matches pattern.  The tokens of NEXT are the */
/*                 characters up to the last non-blank character.  The */
/*                 number of tokens in NEXT and PATTRN are the same. */
/*                 Moreover, the tokens of NEXT are constructed from */
/*                 PATTRN from the following rule: */

/*                    If TOKEN(I) is the I'th token of PATTRN and */
/*                    it is not a restriction (i.e. it's a single letter) */
/*                    then the I'th token of NEXT is TOKEN(I). */

/*                    If TOKEN(I) is the I'th token of PATTRN and it */
/*                    is a restriction then the I'th token of NEXT is */
/*                    the character of the restriction that follows */
/*                    the left brace '{' of the restriction. */

/*                 In particular this means that expansions are copied */
/*                 into NEXT as simply '<>'. */

/* $ Parameters */

/*      None. */

/* $ Files */

/*      None. */

/* $ Exceptions */

/*     1) If the output string is not long enough to hold the */
/*        first string that matches PATTRN the error */
/*        SPICE(OUTPUTTOOLONG) will be signalled. */

/* $ Particulars */

/*     This is a rather immature routine that is used by Christen for */
/*     generating the first string in a sequence of strings specified */
/*     by a naming convention.  There are plenty of things that could */
/*     go wrong if the input PATTRN is not well formed or if there */
/*     is not room in NEXT to hold the string that should be */
/*     constructed by this routine.  However, none of these problems */
/*     are checked for or diagnosed. */

/*     Nevertheless, this routine may prove useful in many contexts */
/*     where you need to create a sequence of names and simply want */
/*     to start with a pattern and let software handle the rest for */
/*     you. */

/*     Normal usage would be to use FSTSTR to get the first string */
/*     of a set specified by a string pattern and to then generate */
/*     the rest using the routine NXTSTR.  This can be useful in those */
/*     situations where you need to create a new file and don't want */
/*     to overwrite any existing file. */

/*     If you plan to make use of this routine in conjuction with */
/*     NXTSTR you should be sure to read the discussion of NXTSTR */
/*     that appears in the header to that routine. */

/* $ Examples */

/*     Suppose that you want to be able to create a file name */
/*     that can be used as a scratch area for some aspect of your */
/*     program.  You can use this routine in conjuction with NXTSTR */
/*     to generate a name of a NEW file for this purpose. */

/*        PATTRN = 'file{0-z}{0-z}{0-z}{0-z}.tmp' */

/*        CALL FSTSTR ( PATTRN, NAME ) */

/*        DO WHILE ( EXISTS(NAME) ) */

/*           THIS = NAME */
/*           CALL NXTSTR ( PATTRN, THIS, NAME ) */

/*        END DO */

/*        CALL TXTOPN ( NAME, UNIT ) */


/* $ Restrictions */

/*     There are lots of restrictions associated with PATTRN and */
/*     NEXT that are discussed above.  This routine doesn't perform */
/*     any error checking so you need to be sure that the inputs */
/*     are properly specified before you call this routine. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -     Support Version 1.1.0, 18-JUN-1999 (WLT) */

/*         Placed a RETURN statement before the first entry point */
/*         to protect against the coding error of calling the */
/*         subroutine MAKSTR directly. */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/* -    Prototype Version 1.0.0, 17-APR-1994 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Get the first name in a sequence that matches a pattern */

/* -& */

/*     There are two things to handle: */

/*     balanced brackets: <> */
/*     balanced braces:   {} */

/*     We do this in one pass. */

    s_copy(next, " ", next_len, (ftnlen)1);
    keep = TRUE_;
    j = 1;
    i__1 = rtrim_(pattrn, pattrn_len);
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (*(unsigned char *)&pattrn[i__ - 1] == '>') {
	    keep = TRUE_;
	}
	if (*(unsigned char *)&pattrn[i__ - 1] == '{') {
	    i__2 = i__;
	    s_copy(next + (j - 1), pattrn + i__2, (ftnlen)1, i__ + 1 - i__2);
	    ++j;
	    keep = FALSE_;
	}
	if (keep) {
	    *(unsigned char *)&next[j - 1] = *(unsigned char *)&pattrn[i__ - 
		    1];
	    ++j;
	}
	if (*(unsigned char *)&pattrn[i__ - 1] == '<') {
	    keep = FALSE_;
	}
	if (*(unsigned char *)&pattrn[i__ - 1] == '}') {
	    keep = TRUE_;
	}
	if (j > i_len(next, next_len)) {
	    chkin_("FSTSTR", (ftnlen)6);
	    setmsg_("The string provided for the first name is too short for"
		    " the input pattern. ", (ftnlen)75);
	    sigerr_("SPICE(OUTPUTTOOLONG)", (ftnlen)20);
	    chkout_("FSTSTR", (ftnlen)6);
	}
    }
    return 0;
/* $Procedure      NXTSTR (Next String) */

L_nxtstr:
/* $ Abstract */

/*     Given a pattern for incrementing a string and a current */
/*     string value (that fits the pattern) produce the next */
/*     string in the sequence. */

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

/*       UTILITY */

/* $ Declarations */

/*     IMPLICIT NONE */

/*     CHARACTER*(*)         PATTRN */
/*     CHARACTER*(*)         THIS */
/*     CHARACTER*(*)         NEXT */

/* $ Brief_I/O */
/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      PATTRN     I   a pattern to use to get NEXT from THIS */
/*      THIS       I   is a string that matches PATTRN */
/*      NEXT       O   is the first string after THIS to match PATTRN */

/* $ Detailed_Input */

/*     PATTRN      is a string that specifies a pattern that all strings */
/*                 in a sequence must match. See the discussion of */
/*                 PATTRN in the umbrella routine for more details. */

/*     THIS        is a string that should be incremented to get the */
/*                 NEXT string that matches the pattern. */

/*                 Note THIS must match the input pattern. */

/*                 The tokens of THIS are the characters upto and */
/*                 including the last non-blank character of THIS. */

/*                 This should have the same number of tokens as does */
/*                 PATTRN. */

/*                 Suppose that TOKTHS (I) is the I'th token of THIS */
/*                 and that TOKPAT(I) is the I'th token of PATTRN. */

/*                 If TOKPAT(I) is a restriction then TOKTHS(I) must */
/*                 be one of the characters belonging to the range */
/*                 of the restriction. */

/*                 Otherwise TOKPAT(I) and TOKTHS(I) match. */

/*                 Thus the pattern */

/*                   'XXX<value: >{0-9}{0-z}' */

/*                 Matches */

/*                   'THIS_5a' */

/*                 This kind of matching is of course a bit */
/*                 confusing.  It is probably more useful to */
/*                 have THIS take all of its tokens to be identical */
/*                 to the character tokens of of PATTRN and match */
/*                 the restriction tokens in all other cases. */

/*                 In particular, the routine FSTSTR, will take */
/*                 PATTRN as an input and produce the a first */
/*                 string in the sequence of strings that matches */
/*                 PATTRN by simply copying the character tokens */
/*                 of PATTRN to the output string and taking the */
/*                 lower bound of the restrictions of PATTRN */
/*                 to get the matching tokens for each restriction. */

/*                 See FSTSTR for a more complete discussion. */

/* $ Detailed_Output */

/*     NEXT        is the next string in the ascii collating */
/*                 sequence that matches PATTRN and is equal to */
/*                 THIS on the non-restriction matching letters */
/*                 of THIS.  There is one exception to this rule. */
/*                 If there is no such string, (i.e. THIS is the */
/*                 last string that can be produced that matches */
/*                 PATTRN) then NEXT will be the first string */
/*                 that matches PATTRN and is equal to THIS on the */
/*                 non-restriction matching letters of THIS. */

/*                 If PATTRN contains no restrictions, then NEXT */
/*                 will equal THIS. */

/* $ Parameters */

/*      None. */

/* $ Files */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This is a rather immature routine that can assist with the */
/*     problem of constructing a sequence of character strings. */

/*     This routine was written as a support routine for the */
/*     SPICE program CHRISTEN and the routine NOMEN.  As such */
/*     it was possible to ensure that all of the detailed conditions */
/*     of PATTRN and THIS were met by the routines that call this. */

/*     However, this routine can prove useful in other contexts and */
/*     is provided so that you can easily produce a large sequence of */
/*     non-repeating character strings.  But  YOU MUST BE CAREFUL */
/*     WITH YOUR USE OF THIS ROUTINE.  Unlike most SPICE routines */
/*     there is no exception handling done.  If you pass in a bad PATTRN */
/*     or value for THIS that does not match PATTRN the result are */
/*     of this routine are unpredictable.  The routine will certainly */
/*     not diagnose the problem and can possibly cause your program */
/*     to crash with no diagnostics to help with finding the problem. */

/*     If you simply need to produce a sequence of strings, you */
/*     should probably avoid putting expansions ( substrings like */
/*     <something> ) in your input pattern.  These are special */
/*     strings that support the tasks needed by NOMEN and CHRISTEN. */

/*     Stick to simple patterns such as the one shown here: */

/*        PATTRN = 'base{0-z}{0-z}{0-z}{0-z}.tmp' */

/*        THIS   = 'base0000.tmp' */

/*     For creating file names or unique non-frequently repeating */
/*     strings, this will probably do the job. */

/*     Note that upper case letters are not supported in PATTRNs, this */
/*     is a UNIX-ish restriction (most file names are written in */
/*     lower case in UNIX).  This routine could be easily modified */
/*     to support a wider range of characters.  Or if you want all */
/*     uppercase characters, apply the SPICE routine UPPER to NEXT */
/*     when you get back from your call to NXTSTR. */

/*     Still even with all the restrictions and lack of exception */
/*     handling this does solve a basic problem of creating an */
/*     increasing sequence of character strings and saves you */
/*     from the task of figuring out the details (in particular */
/*     how to cascade up the string when you have many letters */
/*     to change to get to the next string). */

/* $ Examples */

/*     Suppose you wanted to create the sequence of strings that */
/*     give the times on a 24 hour clock.  I.e 00:00:00, 00:00:01, ... */
/*     23:59:59.  This routine is ideally suited to this task. */

/*        PATTRN = {0-2}{0-9}:{0-5}{0-9}:{0-5}{0-9} */
/*        START  = '29:59:59' */
/*        LAST   = '23:59:59' */

/*        THIS = START */

/*        DO WHILE ( NEXT .NE. LAST ) */

/*           CALL NXTSTR ( PATTRN, THIS, NEXT ) */
/*           WRITE (*,*) NEXT */

/*           THIS = NEXT */

/*        END DO */


/*     The output of the routine would be: */

/*        00:00:00 */
/*        00:00:01 */
/*        00:00:02 */

/*           . */
/*           . */
/*           . */

/*        23:59:57 */
/*        23:59:58 */
/*        23:59:59 */


/* $ Restrictions */

/*     There are lots of restrictions.  See the detailed input */
/*     and particulars for all the warnings. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -     Support Version 1.1.0, 18-JUN-1999 (WLT) */

/*         Placed a RETURN statement before the first entry point */
/*         to protect against the coding error of calling the */
/*         subroutine MAKSTR directly. */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/* -    Prototype Version 1.0.0, 16-APR-1994 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Construct a non-repeating increasing sequence of strings */

/* -& */

/*     First copy THIS into NEXT and find the ends of PATTRN and NEXT. */

    s_copy(next, this__, next_len, this_len);
    j = rtrim_(pattrn, pattrn_len);
    i__ = rtrim_(next, next_len);

/*     We work backwards from the right end of the string. */

    while(j > 0) {

/*        If the current character is a right brace we are going */
/*        to assume we are at the end of a restriction token.  Use */
/*        the range of the restriction and the current character */
/*        of NEXT to determine the "next" character and whether or */
/*        not we can quit now. */

	if (*(unsigned char *)&pattrn[j - 1] == '}') {
	    i__1 = j - 2;
	    max__ = *(unsigned char *)&pattrn[i__1];
	    i__1 = j - 4;
	    min__ = *(unsigned char *)&pattrn[i__1];
	    k = *(unsigned char *)&next[i__ - 1] + 1;
	    if (k > max__) {

/*              Roll over the characters, We aren't done we */
/*              need to keep stepping back through the string */

		*(unsigned char *)&next[i__ - 1] = (char) min__;
	    } else if (k > '9' && k < 'a') {

/*              By convention, the first character following '9' is 'a'. */
/*              Since we don't need to "roll over" this character we */
/*              are done at this point. */

		*(unsigned char *)&next[i__ - 1] = 'a';
		return 0;
	    } else {

/*              We didn't need to roll over the character so we just */
/*              put in the new one and we can quit now. */

		*(unsigned char *)&next[i__ - 1] = (char) k;
		return 0;
	    }

/*           perform the arithmetic needed if we had to roll over the */
/*           character. */

	    j += -5;
	    --i__;

/*        If the character is '>' we assume we are at the right end */
/*        of an expansion. */

	} else if (*(unsigned char *)&pattrn[j - 1] == '>') {

/*           Skip over the invisible portion of the expansion. */

	    while(*(unsigned char *)&pattrn[j - 1] != '<') {
		--j;
	    }
	    --i__;
	} else {

/*           Nothing to do, just back up to the character to the */
/*           left of the current character. */

	    --j;
	    --i__;
	}
    }
    return 0;
} /* makstr_ */

/* Subroutine */ int makstr_(char *pattrn, char *this__, char *next, ftnlen 
	pattrn_len, ftnlen this_len, ftnlen next_len)
{
    return makstr_0_(0, pattrn, this__, next, pattrn_len, this_len, next_len);
    }

/* Subroutine */ int fststr_(char *pattrn, char *next, ftnlen pattrn_len, 
	ftnlen next_len)
{
    return makstr_0_(1, pattrn, (char *)0, next, pattrn_len, (ftnint)0, 
	    next_len);
    }

/* Subroutine */ int nxtstr_(char *pattrn, char *this__, char *next, ftnlen 
	pattrn_len, ftnlen this_len, ftnlen next_len)
{
    return makstr_0_(2, pattrn, this__, next, pattrn_len, this_len, next_len);
    }

