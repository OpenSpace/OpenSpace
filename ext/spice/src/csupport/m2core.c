/* m2core.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__10 = 10;
static integer c__1 = 1;
static integer c__0 = 0;
static integer c__2 = 2;

/* $Procedure      M2CORE ( META/2 core syntax checking routines. ) */
/* Subroutine */ int m2core_0_(int n__, char *temp, integer *tbeg, char *
	keywds, char *string, integer *sbeg, logical *reason, integer *cutoff,
	 integer *m2code, integer *score, char *cause, integer *send, ftnlen 
	temp_len, ftnlen keywds_len, ftnlen string_len, ftnlen cause_len)
{
    /* Initialized data */

    static logical pass1 = TRUE_;

    /* System generated locals */
    integer i__1, i__2, i__3;
    char ch__1[2];

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);

    /* Local variables */
    static integer pbeg, pend, best[16], slen, nkey;
    static char mssg[420], root[32];
    extern /* Subroutine */ int m2cal_(char *, char *, integer *, ftnlen, 
	    ftnlen);
    static integer i__;
    extern integer cardc_(char *, ftnlen), cardi_(integer *);
    extern /* Subroutine */ int lcase_(char *, char *, ftnlen, ftnlen), 
	    chkin_(char *, ftnlen);
    static integer tcode;
    static logical endok;
    static integer timeb;
    static logical endit;
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen), copyc_(char *, char *, ftnlen, ftnlen);
    static integer lower;
    static logical error;
    static integer count, upper;
    static char known[32*16];
    extern /* Subroutine */ int m2begr_(char *, integer *, integer *, integer 
	    *, integer *, ftnlen), m2mark_(char *, integer *, integer *, char 
	    *, ftnlen, ftnlen);
    extern logical m2wmch_(char *, integer *, integer *, char *, ftnlen, 
	    ftnlen);
    extern /* Subroutine */ int m2clss_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    static integer db, de;
    extern /* Subroutine */ int m2trim_(char *, char *, ftnlen, ftnlen);
    static integer kb;
    extern logical m2keyw_(char *, ftnlen);
    static integer ke, sb, tb, tc, se, te, endchk;
    static logical cmatch;
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen);
    extern integer esrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static integer tbegin, sbegin;
    static char artcle[2];
    static logical calwrd;
    static integer bscore;
    extern /* Subroutine */ int fndnwd_(char *, integer *, integer *, integer 
	    *, ftnlen);
    static char phrase[120];
    static logical keytbe, useend;
    static integer begout, lastsb, suffsb, orignl, lastse, mspell, dcount, 
	    suffse;
    static char lowerc[64];
    static integer oversb;
    static char upperc[64], countc[64];
    extern integer qlstnb_(char *, ftnlen);
    static logical usekey;
    static integer mcount, overse;
    static logical keywrd;
    static integer scores[16];
    extern /* Subroutine */ int ssizei_(integer *, integer *);
    extern integer qrtrim_(char *, ftnlen);
    static logical uselst;
    extern /* Subroutine */ int ssizec_(integer *, char *, ftnlen), setmsg_(
	    char *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, ftnlen),
	     suffix_(char *, integer *, char *, ftnlen, ftnlen), bestwd_(char 
	    *, char *, integer *, integer *, integer *, char *, ftnlen, 
	    ftnlen, ftnlen), inttxt_(integer *, char *, ftnlen);
    extern /* Character */ VOID ana_(char *, ftnlen, char *, char *, ftnlen, 
	    ftnlen);
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);

/* $ Abstract */

/*     This routine is the header routine for use by M2MTCH and its */
/*     associated entry point M2RCVR.  As it takes no action, it should */
/*     not be called directly. */

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

/*      The META/2 book. */

/* $ Keywords */

/*     PARSING */
/*     UTILITY */
/*     WORD */

/* $ Declarations */
/* $ Brief_I/O */

/*     See the entry point headers for description of each of the */
/*     input/output arguements. */

/* $ Detailed_Input */

/*     See individual entry points. */

/* $ Detailed_Output */

/*     See individual entry points. */

/* $ Error_Handling */

/*     See individual entry points. */

/* $ Input_Files */

/*     None. */

/* $ Output_Files */

/*     None. */

/* $ Particulars */

/*     This routine serves as the header routine for entry point M2MTCH */
/*     and its associated entry M2RCVR.  M2MTCH is the essential syntax */
/*     checking portion of the META/2 syntax comparison routine. */

/* $ Examples */

/*     To compare two templates call M2MTCH */

/*     To find the position of a mispelled keyword in the input string */
/*     and the possible spelling corrections call M2RCVR */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/*     Version B1.0.0, 7-APR-1988 (WLT) (IMU) */

/* -& */


/*     SPICELIB functions */


/*     Local variables */


/*     KNOWN, PBEG, and PEND are storage for the recovery entry point. */

/*     Should a spelling error be detected, the best matching words will */
/*     be stored in KNOWN and the index of the beginning and ending */
/*     of the problem word in STRING will be stored in PBEG and PEND */
/*     respectively. */


/*     Initial values */

    /* Parameter adjustments */
    if (cause) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_m2mtch;
	case 2: goto L_m2rcvr;
	}

    return 0;
/* $Procedure  M2MTCH ( Match a string with a simple META/2 template ) */

L_m2mtch:
/* $ Abstract */

/*     This entry points compares simple templates with strings and */
/*     produces scores reflecting the extent of agreement between */
/*     the template and string.  If requested diagnostics are produced. */

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

/*     The META/2 book. */

/* $ Keywords */

/*     PARSING */
/*     UTILITY */
/*     WORD */

/* $ Declarations */

/*     INTEGER               LBCELL */
/*     PARAMETER           ( LBCELL = -5 ) */

/*     CHARACTER*(*)         TEMP */
/*     INTEGER               TBEG */
/*     CHARACTER*(*)         KEYWDS ( LBCELL: * ) */
/*     CHARACTER*(*)         STRING */
/*     INTEGER               SBEG */
/*     LOGICAL               REASON */
/*     INTEGER               CUTOFF */
/*     INTEGER               M2CODE */
/*     INTEGER               SCORE */
/*     CHARACTER*(*)         CAUSE  ( 2 ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TEMP       I   A simple language specification sentence . */
/*     TBEG       I   Position in the template to start match attempt. */
/*     KEYWDS     I   A cell of possible terminators of a META-KEY string */
/*     STRING     I   A potential language sentence */
/*     SBEG      I/O  Marker of the current start of the string */
/*     REASON     I   Set to .TRUE. to request production of diagnostics */
/*     CUTOFF     I   Spelling error threshold */
/*     M2CODE     O   Indicates type of mismatch between TEMP and  STRING */
/*     SCORE      O   Number giving a measure of how closeness of match. */
/*     CAUSE      O   If requested, a diagnostic of mismatch. */

/* $ Detailed_Input */

/*     TEMP       A simple language specification sentence.  Such a */
/*                statement consists of only keyword and META/2 class */
/*                specifiers.  No groups or @then directives are allowed. */

/*     TBEG       Position in the template to start match attempt. */

/*     KEYWDS     A cell of possible terminators of a META-KEY string. */
/*                This is primarily usefull for higher level routines */
/*                that pick simple templates out of more complex META/2 */
/*                specification sentences. */

/*     STRING     A is a collection of words that might make up a valid */
/*                sentence in a META/2 language.  A substring of STRING */
/*                beginning with SBEG will be matched against TEMP to */
/*                see if we have a valid phrase in a META/2 language. */

/*     SBEG       Marker of the current start of the string */

/*     REASON     Set to .TRUE. to request production of diagnostics */

/*     CUTOFF     is a parameter used to determine how close words */
/*                of STRING must match up with keywords in TEMP */
/*                in order to be diagnosed as spelling errors. */
/*                Ranges from 0 to 100 are acceptable.  A "good" range */
/*                of values is from from 65 to 75. */

/* $ Detailed_Output */

/*     SBEG       if the match is successful, SBEG will be set to the */
/*                first word of the input string that follows the */
/*                matched substring. ( Note that words in KEYWDS do */
/*                not qualify as part of the template, but merely */
/*                serve to delimit the ends of variable length */
/*                templates. Thus if one of these words was actually */
/*                used to delimit the end of TEMP, SBEG will point to */
/*                the beginning of that word in STRING.) */


/*     M2CODE     Indicates type of mismatch between TEMP and  STRING */

/*     M2CODE = 0     Indicates that the template supplied matched the */
/*                    input string as far as it went. */

/*     M2CODE = 10    Indicates that the keyword that was supposed to */
/*                    terminate a variable length template was probably */
/*                    mispelled. */

/*     M2CODE = 11    We were expecting a specific keyword and failed */
/*                    in our match attempt.  It is likely that the */
/*                    keyword was simply misspelled. */

/*     M2CODE = 101   Indicates that a variable length template had too */
/*                    few entries before the keyword was encountered. */

/*     M2CODE = 102   Indicates that a variable length template had too */
/*                    many entries before the keyword was encountered. */

/*     M2CODE = 103   Indicates that the correct number of entries */
/*                    for a variable length template were encountered */
/*                    but the input string terminated without finding */
/*                    the correct keyword. */

/*     M2CODE = 104   Indicates that the string should have terminated */
/*                    but instead contained extra characters. */

/*     M2CODE = 105   Indicates that correct number if items were */
/*                    present in the variable length template but that */
/*                    it did not end with an expected keyword. */
/*                    Moreover, it is not thought that the problem is */
/*                    likely to be a simple spelling error. */

/*     M2CODE = 106   The number of items found in a variable length */
/*                    template was too small and we did not get */
/*                    an expected keyword.  A possible explanation */
/*                    is a mistyping one or more of the letters in */
/*                    one of the META class words. */

/*     M2CODE = 107   The number of items found in a variable length */
/*                    template was too large and we did not get */
/*                    an expected keyword.  A possible explanation */
/*                    is a a forgotten keyword. */

/*     M2CODE = 108   We ran out of string while in a fixed length */
/*                    template. */

/*     M2CODE = 109   We failed to match a META class word while within */
/*                    a fixed length template. */

/*     M2CODE = 110   We were expecting to see a specific keyword and */
/*                    got something else.  This is not thought to be */
/*                    the result of a spelling error. */

/*     M2CODE = 111   We were expecting to see a META class word and */
/*                    failed in our matching attempt. */

/*     M2CODE values from 1001 to 1014 indicate problems that can occur */
/*     when attempting to match a substring with the @calendar specifier. */

/*     M2CODE = 1001  Too many tokens in a @calendar string. */

/*     M2CODE = 1002  Time indicated is JD but no numeric portion */
/*                    supplied. */

/*     M2CODE = 1003  The date portion of the Julian date didn't make */
/*                    it through the number parsing. */

/*     M2CODE = 1004  No date was supplied */

/*     M2CODE = 1005  A year was not supplied in a calendar date */

/*     M2CODE = 1006  Ambiguous date specification. */

/*     M2CODE = 1007  Ambiguous month specification */

/*     M2CODE = 1008  Invalid day specification in a calendar date. */

/*     M2CODE = 1009  Year appears as the second item without a */
/*                    spelled month. */

/*     M2CODE = 1010  Month is not spelled and is not an integer */
/*                    between 1 and 12. */

/*     M2CODE = 1011  Month not spelled and day is not between 1 and */
/*                    366. */

/*     M2CODE = 1012  Hour portion of time is not an integer from 0 */
/*                    to 23. */

/*     M2CODE = 1013  Minutes portio of time is not an integer from */
/*                    0 to 59. */

/*     M2CODE = 1014  Seconds must be a positive number less than 61 */

/*     SCORE      Number giving a measure of how closeness of match, 100 */
/*                points are awarded for matched keywords, 15 points */
/*                for matched classes, 100 points for matched calendar */
/*                strings.  Fractions of 100 awarded for words that */
/*                look like they might be misspelled keyword.  The */
/*                score is used primarily in thos cases when a substring */
/*                does not match any of a collection of templates */
/*                exactly.  In this case the one that has the highest */
/*                score is regarded as being what the user probably */
/*                meant. */

/*     CAUSE      If requested, a diagnostic of mismatch. */

/* $ Exceptions */

/*     The following errors are detected by this routine. */

/*           'SPICE(KEYWORDNOTFOUND)' */

/*     Additional errors may be detected by SPICELIB routines called */
/*     by this routine. */


/* $ Input_Files */

/*     None. */

/* $ Output_Files */

/*     None. */

/* $ Particulars */

/*     This routine is the central utility used in META/2 when attempting */
/*     to match potential sentences with language templates.  It compares */
/*     simple templates with substrings of a command and produces a score */
/*     indicating the degree of match.  Moreover, if requested, */
/*     diagnostics are available that indicate why a string did not */
/*     match a given template. */

/* $ Examples */

/*      None. */

/* $ Restrictions */

/*      It is assumed that all templates are simple META/2 templates. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/*     Version B1.0.0, 7-APR-1988 (WLT) (IMU) */

/* -& */


/*     Take care of the SPICE error handling first. */

    if (pass1) {
	pass1 = FALSE_;
	ssizei_(&c__10, best);
	ssizei_(&c__10, scores);
	ssizec_(&c__10, known, (ftnlen)32);
	scardc_(&c__1, known, (ftnlen)32);
    }
    slen = i_len(string, string_len);
    s_copy(cause, " ", cause_len, (ftnlen)1);
    *score = 0;
    tbegin = *tbeg;
    sbegin = *sbeg;
    error = FALSE_;
    *m2code = 0;
    mspell = 0;
    kb = 0;
    ke = 0;
    nkey = cardc_(keywds, keywds_len);

/*     Locate the next word of the template. */

    fndnwd_(temp, &tbegin, &tb, &te, temp_len);
    while(tb != 0 && ! error) {

/*        Zero out the keyword pointers. */

	kb = 0;
	ke = 0;
	endok = FALSE_;

/*        Examine the current template word.  Is there a range template */
/*        attatched? */

	orignl = tb;
	m2begr_(temp, &tb, &te, &lower, &upper, temp_len);

/*        Locate the boundaries of the root of this template word. */

	m2trim_(temp + (tb - 1), root, te - (tb - 1), (ftnlen)32);
	tc = qrtrim_(root, (ftnlen)32) - 1 + tb;

/*        If TB changed from its original value there is a range template */
/*        attached to the word TEMP(TB:TE).  The associated values are in */
/*        LOWER and UPPER. */

	keytbe = m2keyw_(temp + (tb - 1), te - (tb - 1));
	calwrd = s_cmp(temp + (tb - 1), "@calendar", tc - (tb - 1), (ftnlen)9)
		 == 0 && ! keytbe;
	if (orignl < tb || calwrd) {

/*           Yes.   There is a range template attatched.  Is it of */
/*           variable length? */

	    if (calwrd) {
		lower = 1;
		upper = 40;
		timeb = sbegin;
	    }
	    if (lower != upper) {

/*              Yes.  The template has a variable length. Determine */
/*              what delimiters might signal the end of a matching */
/*              substring of word from string. */

/*              Possibilities are:  The end of the string     (USEEND) */
/*                                  One of the listed KEYWDS  (USELST) */
/*                                  A keyword listed in TEMP. (USEKEY) */

/*              Right now we don't know which of the three cases to use. */

		useend = FALSE_;
		uselst = FALSE_;
		usekey = FALSE_;
		endok = FALSE_;
		endchk = te + 1;

/*              If the end of the current template word, was not */
/*              at the end of the template, then there might be */
/*              a keyword next.  Look for the next word to find out. */

		fndnwd_(temp, &endchk, &kb, &ke, temp_len);
		if (ke > 0) {

/*                 There is a word in the template that follows */
/*                 our current template word.  See if it is a keyword. */

		    if (m2keyw_(temp + (kb - 1), ke - (kb - 1))) {

/*                    If it is a keyword, it will be used as the */
/*                    delimiter for a sequence of words in STRING. */
/*                    ( Note we only want to work with the root of this */
/*                      template word. ) */

			usekey = TRUE_;
			m2trim_(temp + (kb - 1), root, ke - (kb - 1), (ftnlen)
				32);
			ke = qrtrim_(root, (ftnlen)32) + kb - 1;
		    } else {

/*                    Its not a keyword.  Bad, Bad. The user was not */
/*                    using META/2 properly. */

			chkin_("M2MTCH", (ftnlen)6);
			setmsg_("M2MTCH: Any META-KEY that is preceded by a "
				"variable length range template in a specific"
				"ation statement must be followed by a keywor"
				"d. ", (ftnlen)134);
			sigerr_("SPICE(KEYWORDNOTFOUND)", (ftnlen)22);
			chkout_("M2MTCH", (ftnlen)6);
			return 0;
		    }
		} else if (ke <= 0) {

/*                 We got to this point because there was nothing */
/*                 to look at beyond where we were in TEMP.  So we */
/*                 either use one of the listed keywords or the end */
/*                 of the string will be our delimiter. */

		    if (nkey > 0) {
			uselst = TRUE_;
			endok = esrchc_("@end", &nkey, keywds + keywds_len * 
				6, (ftnlen)4, keywds_len) != 0;
		    } else {
			useend = TRUE_;
		    }
		}

/*              Until we have detected one of the keywords */
/*                 or we have not matched the current class */
/*                 or we run out of words in the sentence */

/*                   Grab the next word of the sentence */
/*                   Check it for keyword . */
/*                   Check it for class . */

		endit = FALSE_;
		keywrd = FALSE_;
		mcount = 0;
		suffsb = 0;
		oversb = 0;
		overse = 0;
		lastsb = sbegin;
		lastse = pos_(string, " ", &sbegin, string_len, (ftnlen)1) - 
			1;
		while(! endit) {

/*                 Fetch the next word of the sentence. */

		    fndnwd_(string, &sbegin, &sb, &se, string_len);

/*                 If there WAS a next word SE will not be zero. */

		    if (se == 0) {
			keywrd = useend || endok;
			endit = TRUE_;

/*                    BEGOUT will point past the matched portion of the */
/*                    string.  If no errors occur, it will be used to */
/*                    set SBEG on output. */

			begout = slen + 1;
		    } else {

/*                    is this a delimiting word for a variable length */
/*                    list? */

			if (uselst) {
			    keywrd = esrchc_(string + (sb - 1), &nkey, keywds 
				    + keywds_len * 6, se - (sb - 1), 
				    keywds_len) != 0;
			    endit = keywrd;
			    if (keywrd) {

/*                          Mark the position just before the beginning */
/*                          of this word in STRING  so that SBEG will */
/*                          point to the first word past the end of */
/*                          the matched portion of STRING. */

				begout = sb - 1;
			    }
			} else if (usekey) {
			    keywrd = m2wmch_(string, &sb, &se, temp + (kb - 1)
				    , string_len, ke - (kb - 1)) && m2keyw_(
				    temp + (kb - 1), ke - (kb - 1));
			    endit = keywrd;

/*                       Mark the position of the "next" character */
/*                       in the string beyond the end of the current */
/*                       STRING word. */

			    begout = se + 1;
			}

/*                    If we didn't bump into a keyword this must */
/*                    be (or should be) another of the words specified */
/*                    by the META-KEY TEMP(TB:TE) */

			if (! keywrd) {
			    cmatch = m2wmch_(string, &sb, &se, temp + (tb - 1)
				    , string_len, te - (tb - 1));
			    if (cmatch) {
				++mcount;

/*                          Mark the position of the first character */
/*                          beyond the end of the current STRING */
/*                          word. */

				begout = se + 1;

/*                          If MCOUNT has gotten too big, record the */
/*                          begin and end of the "bad" portion of the */
/*                          substring. */

				if (mcount == lower + 1) {

/*                             Mark the location of the beginning */
/*                             and end of this word in case we need to */
/*                             backtrack to here. */

				    suffsb = sb;
				    suffse = se;
				} else if (mcount <= upper) {

/*                             Mark the end of this word in case */
/*                             we need it later. */

				    suffse = se;
				} else if (mcount == upper + 1) {
				    oversb = sb;
				    overse = se;
				} else if (mcount > upper) {
				    overse = se;
				}
			    } else {
				endit = TRUE_;
			    }
			}
			lastsb = sb;
			lastse = se;
		    }

/*                 Set the pointer to the input string to the first */
/*                 character past the end of the current word. */

		    sbegin = se + 1;
		}

/*              We're now at the end of the loop matching words of STRING */
/*              with the class of object that had a variable length */
/*              template. */

/*              The question now is: 'Did we get out of the loop in */
/*              a healthy or unhealthy way?' */


/*              Did we have the required range of items in the class? */
/*              Did we hit  the keyword? */

/*              If both questions were answered YES, */

		if (keywrd && mcount >= lower && mcount <= upper) {

/*                 Increment the score by METASC times the number of */
/*                 words found in the variable length template plus */
/*                 KEYSC for getting the keyword right. */

		    if (! calwrd) {
			*score = *score + mcount * 15 + 100;
			if (usekey) {

/*                       set the end of the last template word used to */
/*                       be the end of the keyword that we just hit. */

			    te = ke;
			}
		    } else {
			s_copy(mssg, " ", (ftnlen)420, (ftnlen)1);
			m2cal_(string + (timeb - 1), mssg, &tcode, suffse - (
				timeb - 1), (ftnlen)420);
			if (tcode == 0) {
			    *score += 200;
			} else {
			    *score += 100;
			    error = TRUE_;
			    if (*m2code == 0) {
				*m2code = tcode + 1000;
			    }
			    if (*reason) {
				s_copy(cause, "I was not able to parse the c"
					"alendar string \"", cause_len, (
					ftnlen)45);
				suffix_(string + (timeb - 1), &c__0, cause, 
					suffse - (timeb - 1), cause_len);
				suffix_("\". ", &c__0, cause, (ftnlen)3, 
					cause_len);
				suffix_(mssg, &c__1, cause, (ftnlen)420, 
					cause_len);
				m2mark_(string, &timeb, &suffse, cause, 
					string_len, cause_len);
				s_copy(cause + cause_len, cause, cause_len, 
					cause_len);
			    }
			}
		    }

/*              If less than the required range but a keyword was found */
/*              the error was: " not enough values loaded. " */

		} else if (keywrd && mcount < lower) {
		    inttxt_(&lower, lowerc, (ftnlen)64);
		    inttxt_(&mcount, countc, (ftnlen)64);
		    lcase_(lowerc, lowerc, (ftnlen)64, (ftnlen)64);
		    lcase_(countc, countc, (ftnlen)64, (ftnlen)64);
		    error = TRUE_;
		    if (*m2code == 0) {
			*m2code = 101;
		    }

/*                 We grant METASC points for every word of the current */
/*                 class that was found, but we subtract METASC points */
/*                 for each item we were short.  That is: */

/*                 MCOUNT + ( LOWER - MCOUNT ) = 2*MCOUNT - LOWER */

/* Computing MAX */
		    i__1 = 0, i__2 = (mcount << 1) - lower;
		    *score += max(i__1,i__2) * 15;

/*                 Add on KEYSC points for getting the correct keyword. */

		    *score += 100;
		    if (*reason) {
			s_copy(cause, "I was expecting to see at least # # a"
				"t this point in the command string. I counte"
				"d #. ", cause_len, (ftnlen)86);
			m2clss_(temp + (tb - 1), &lower, phrase, tc - (tb - 1)
				, (ftnlen)120);
			repmc_(cause, "#", lowerc, cause, cause_len, (ftnlen)
				1, (ftnlen)64, cause_len);
			repmc_(cause, "#", phrase, cause, cause_len, (ftnlen)
				1, (ftnlen)120, cause_len);
			repmc_(cause, "#", countc, cause, cause_len, (ftnlen)
				1, (ftnlen)64, cause_len);

/*                    OK. now we want to tack on the string and keep */
/*                    track of where the current word STRING(SB:SE) */
/*                    will get put. */

			m2mark_(string, &lastsb, &lastse, cause, string_len, 
				cause_len);
			s_copy(cause + cause_len, cause, cause_len, cause_len)
				;
		    }

/*              If more than the required range but a keyword was found */
/*              the error was too many values loaded. */

		} else if (keywrd && mcount > upper) {
		    inttxt_(&upper, upperc, (ftnlen)64);
		    inttxt_(&mcount, countc, (ftnlen)64);
		    lcase_(upperc, upperc, (ftnlen)64, (ftnlen)64);
		    lcase_(countc, countc, (ftnlen)64, (ftnlen)64);
		    error = TRUE_;
		    if (*m2code == 0) {
			*m2code = 102;
		    }

/*                 We grant METASC points for every word of the current */
/*                 class that was found prior to the cutoff limit. */
/*                 But we subtract METASC points for each extra item. */
/*                 That is: */

/*                 UPPER + ( MCOUNT - UPPER ) = 2*UPPER - MCOUNT */

/* Computing MAX */
		    i__1 = 0, i__2 = (upper << 1) - mcount;
		    *score += max(i__1,i__2) * 15;

/*                 Add on KEYSC points for getting the correct keyword. */

		    *score += 100;
		    if (*reason) {
			s_copy(cause, "I was expecting to see at most # #. I"
				" counted #. I've marked the location of the "
				"problem for you. ", cause_len, (ftnlen)98);
			m2clss_(temp + (tb - 1), &upper, phrase, tc - (tb - 1)
				, (ftnlen)120);
			repmc_(cause, "#", upperc, cause, cause_len, (ftnlen)
				1, (ftnlen)64, cause_len);
			repmc_(cause, "#", phrase, cause, cause_len, (ftnlen)
				1, (ftnlen)120, cause_len);
			repmc_(cause, "#", countc, cause, cause_len, (ftnlen)
				1, (ftnlen)64, cause_len);
			m2mark_(string, &oversb, &overse, cause, string_len, 
				cause_len);
			s_copy(cause + cause_len, cause, cause_len, cause_len)
				;
		    }

/*              If required range but no keyword, error could be */
/*              misspelled keyword ( we estimate this ) or keyword */
/*              was missing. */

		} else if (mcount >= lower && mcount <= upper) {

/*                 Add METASC points to the score for each of the */
/*                 words encountered. */

		    if (se == 0) {

/*                    We are going to try to see if we had a spelling */
/*                    error that caused us to run out of string */

			fndnwd_(string, &suffsb, &db, &de, string_len);
			orignl = suffsb;
			count = lower + 1;
			bscore = 0;
			dcount = 0;
			while(count <= mcount) {
			    if (usekey) {
				s_copy(known + 192, temp + (kb - 1), (ftnlen)
					32, ke - (kb - 1));

/*                          Compare the last word encountered in the */
/*                          string with the KEYWORD we were expecting. */

				bestwd_(string + (db - 1), known, cutoff, 
					best, scores, mssg, de - (db - 1), (
					ftnlen)32, (ftnlen)420);
			    } else if (uselst) {

/*                          Compare the last word that we hit with one */
/*                          of the keywords from the list of possible */
/*                          closing keywords. */

				bestwd_(string + (db - 1), keywds, cutoff, 
					best, scores, mssg, de - (db - 1), 
					keywds_len, (ftnlen)420);
			    }
			    if (cardi_(scores) > 0 && scores[6] >= *cutoff) {

/*                          We are going to treat this as a spelling */
/*                          error. */

				if (*m2code == 0) {
				    *m2code = 13;
				}

/*                          Save the beginning and ending of the */
/*                          problem word for use in the recovery */
/*                          entry point. */

				if (scores[6] > bscore) {
				    bscore = scores[6];
				    pbeg = db;
				    pend = de;

/*                             Everything up to this is now regarded */
/*                             as simply matching the META-KEY.  Store */
/*                             this number of META-KEYs for use by */
/*                             diagnostics generation. */

				    dcount = count - 1;
				}
			    }
			    suffsb = de + 1;

/*                       Look at the next word until we have gone */
/*                       past UPPER even if we already have a */
/*                       candidate for misspelling, there might be */
/*                       a better one. */

			    fndnwd_(string, &suffsb, &db, &de, string_len);
			    ++count;
			}

/*                    Save the misspelling information associated */
/*                    with the best match (if there was one). */

			if (bscore > 0) {
			    if (usekey) {
				s_copy(known + 192, temp + (kb - 1), (ftnlen)
					32, ke - (kb - 1));

/*                          Compare the last word encountered in the */
/*                          string with the KEYWORD we were expecting. */

				bestwd_(string + (pbeg - 1), known, cutoff, 
					best, scores, mssg, pend - (pbeg - 1),
					 (ftnlen)32, (ftnlen)420);
			    } else if (uselst) {

/*                          Compare the last word that we hit with one */
/*                          of the keywords from the list of possible */
/*                          closing keywords. */

				bestwd_(string + (pbeg - 1), keywds, cutoff, 
					best, scores, mssg, pend - (pbeg - 1),
					 keywds_len, (ftnlen)420);

/*                          Save the best matches for use in the */
/*                          recovery entry point. */

				i__1 = cardi_(best);
				for (i__ = 1; i__ <= i__1; ++i__) {
				    s_copy(known + (((i__2 = i__ + 5) < 16 && 
					    0 <= i__2 ? i__2 : s_rnge("known",
					     i__2, "m2core_", (ftnlen)1202)) 
					    << 5), keywds + (best[(i__3 = i__ 
					    + 5) < 16 && 0 <= i__3 ? i__3 : 
					    s_rnge("best", i__3, "m2core_", (
					    ftnlen)1202)] + 5) * keywds_len, (
					    ftnlen)32, keywds_len);
				}
				i__1 = cardi_(best);
				scardc_(&i__1, known, (ftnlen)32);
			    }

/*                       This is not regarded as an error worth */
/*                       stopping for unless our */
/*                       misspelling total has runs over 100. */

			    mspell += 100 - bscore;
			    if (mspell < 100) {
				*score = *score + dcount * 15 + bscore;

/*                          Back the value of SBEGIN back up to the */
/*                          point of failure, so that we can continue */
/*                          processing as if nothing had gone wrong. */

				sbegin = pend + 1;
			    } else {
				*score += dcount * 15;
				error = TRUE_;
			    }
			} else {

/*                       Restore the initial value of SUFFSB */

			    suffsb = orignl;
			    *score += mcount * 15;
			    if (*m2code == 0) {
				*m2code = 103;
			    }

/*                       This occurs if we ran out of stuff in STRING */
/*                       and we were looking to find a keyword instead. */

			    error = TRUE_;
			}
			if (usekey && *reason) {
			    s_copy(cause, "I was looking for the keyword \"", 
				    cause_len, (ftnlen)31);
			    suffix_(temp + (kb - 1), &c__1, cause, ke - (kb - 
				    1), cause_len);
			    suffix_("\" when I reached the", &c__1, cause, (
				    ftnlen)20, cause_len);
			    suffix_("end of the input ", &c__1, cause, (
				    ftnlen)17, cause_len);
			    suffix_("command. ", &c__1, cause, (ftnlen)9, 
				    cause_len);
			} else if (uselst && *reason) {
			    s_copy(cause, "I was looking for one of the keyw"
				    "ords that follow when I reached the end "
				    "of the input command.  Keywords: {", 
				    cause_len, (ftnlen)107);
			    i__1 = nkey;
			    for (i__ = 1; i__ <= i__1; ++i__) {
				suffix_(keywds + (i__ + 5) * keywds_len, &
					c__2, cause, keywds_len, cause_len);
				suffix_(",", &c__0, cause, (ftnlen)1, 
					cause_len);
			    }
			    i__1 = qlstnb_(cause, cause_len) - 1;
			    s_copy(cause + i__1, " }.", cause_len - i__1, (
				    ftnlen)3);
			}
			if (*reason && bscore != 0) {
			    s_copy(cause + cause_len, cause, cause_len, 
				    cause_len);
			    s_copy(cause, " ", cause_len, (ftnlen)1);
			    suffix_(mssg, &c__1, cause, (ftnlen)420, 
				    cause_len);
			    m2mark_(string, &pbeg, &pend, cause, string_len, 
				    cause_len);
			    suffix_(mssg, &c__1, cause + cause_len, (ftnlen)
				    420, cause_len);
			    m2mark_(string, &pbeg, &pend, cause + cause_len, 
				    string_len, cause_len);
			} else if (*reason) {
			    m2mark_(string, &lastsb, &lastse, cause, 
				    string_len, cause_len);
			    s_copy(cause + cause_len, cause, cause_len, 
				    cause_len);
			}

/*                 Recall that we are examining the case when the number */
/*                 of word matches is within the expected range, but */
/*                 no keyword was present.  We have already looked at */
/*                 what to do if we ran out of string prematurely. */

		    } else if (se > 0) {
			*score += mcount * 15;

/*                    We ran into something unexepected.  Possibly */
/*                    a misspelled keyword.  See if any of the */
/*                    expected keywords are close to what we got. */

			if (useend) {
			    error = TRUE_;
			    if (*m2code == 0) {
				*m2code = 104;
			    }
			    if (*reason) {
				s_copy(cause, "The input command contains ex"
					"tra characters that are not part of "
					"a valid command.  ", cause_len, (
					ftnlen)83);
				m2mark_(string, &sb, &se, cause, string_len, 
					cause_len);
				s_copy(cause + cause_len, cause, cause_len, 
					cause_len);
			    }
			} else if (usekey || uselst) {
			    if (usekey) {
				s_copy(known + 192, temp + (kb - 1), (ftnlen)
					32, ke - (kb - 1));

/*                          Compare the last word encountered in the */
/*                          string with the KEYWORD we were expecting. */

				bestwd_(string + (sb - 1), known, cutoff, 
					best, scores, mssg, se - (sb - 1), (
					ftnlen)32, (ftnlen)420);
			    } else if (uselst) {

/*                          Compare the last word that we hit with one */
/*                          of the keywords from the list of possible */
/*                          closing keywords. */

				bestwd_(string + (sb - 1), keywds, cutoff, 
					best, scores, mssg, se - (sb - 1), 
					keywds_len, (ftnlen)420);

/*                          Save the best matches for use in the recovery */
/*                          entry point. */

				i__1 = cardi_(best);
				for (i__ = 1; i__ <= i__1; ++i__) {
				    s_copy(known + (((i__2 = i__ + 5) < 16 && 
					    0 <= i__2 ? i__2 : s_rnge("known",
					     i__2, "m2core_", (ftnlen)1362)) 
					    << 5), keywds + (best[(i__3 = i__ 
					    + 5) < 16 && 0 <= i__3 ? i__3 : 
					    s_rnge("best", i__3, "m2core_", (
					    ftnlen)1362)] + 5) * keywds_len, (
					    ftnlen)32, keywds_len);
				}
				i__1 = cardi_(best);
				scardc_(&i__1, known, (ftnlen)32);
			    }

/*                       We are still checking out the case in which we */
/*                       had a correct range of words for a variable */
/*                       length template, but ran into */
/*                       something that was not a terminating keyword */
/*                       that we were expecting.  Possibly we hit a */
/*                       mispelled keyword. */

/*                       Well? Was there anything to the rumor of a */
/*                       spelling error? */

			    if (cardi_(scores) > 0 && scores[6] >= *cutoff) {
				if (*m2code == 0) {
				    *m2code = 10;

/*                             Save the beginning and ending of the */
/*                             problem word for use in the recovery */
/*                             entry point. */

				    pbeg = sb;
				    pend = se;
				}

/*                          This is probably a spelling error. */
/*                          Point out the error. */

				mspell += 100 - scores[6];
				if (mspell < 100) {
				    *score += scores[6];
				} else {
				    error = TRUE_;
				}
				if (*reason) {

/*                             Construct an error message indicating */
/*                             the spelling diagnostic. */

				    error = TRUE_;
				}
			    } else if (cardi_(scores) == 0 || scores[6] < *
				    cutoff) {

/*                          This is not a misspelling. */
/*                          Set the error flag */

				error = TRUE_;
				if (*m2code == 0) {
				    *m2code = 105;
				}
				s_copy(mssg, " ", (ftnlen)420, (ftnlen)1);
			    }
			    if (*reason && usekey) {
				s_copy(cause, "I was looking for the ", 
					cause_len, (ftnlen)22);
				suffix_("keyword \"", &c__1, cause, (ftnlen)9,
					 cause_len);
				suffix_(temp + (kb - 1), &c__0, cause, ke - (
					kb - 1), cause_len);
				suffix_("\" when I ", &c__0, cause, (ftnlen)9,
					 cause_len);
				suffix_("encountered ", &c__1, cause, (ftnlen)
					12, cause_len);
				suffix_("the word \"", &c__1, cause, (ftnlen)
					10, cause_len);
				suffix_(string + (sb - 1), &c__0, cause, se - 
					(sb - 1), cause_len);
				suffix_("\".   ", &c__0, cause, (ftnlen)5, 
					cause_len);
				suffix_(mssg, &c__1, cause, (ftnlen)420, 
					cause_len);
				s_copy(cause + cause_len, cause, cause_len, 
					cause_len);
				s_copy(cause, mssg, cause_len, (ftnlen)420);
				m2mark_(string, &sb, &se, cause, string_len, 
					cause_len);
				m2mark_(string, &sb, &se, cause + cause_len, 
					string_len, cause_len);
			    } else if (*reason && uselst) {
				s_copy(cause, "I was looking for one of the "
					"keywords in the list: { ", cause_len, 
					(ftnlen)53);
				i__1 = nkey;
				for (i__ = 1; i__ <= i__1; ++i__) {
				    suffix_(keywds + (i__ + 5) * keywds_len, &
					    c__1, cause, keywds_len, 
					    cause_len);
				    if (i__ != nkey) {
					suffix_(",", &c__0, cause, (ftnlen)1, 
						cause_len);
				    }
				}
				suffix_("}  when I ", &c__1, cause, (ftnlen)
					10, cause_len);
				suffix_("encountered ", &c__1, cause, (ftnlen)
					12, cause_len);
				suffix_("the word \"", &c__1, cause, (ftnlen)
					10, cause_len);
				suffix_(string + (sb - 1), &c__0, cause, se - 
					(sb - 1), cause_len);
				suffix_("\".   ", &c__0, cause, (ftnlen)5, 
					cause_len);
				suffix_(mssg, &c__1, cause, (ftnlen)420, 
					cause_len);
				m2mark_(string, &sb, &se, cause, string_len, 
					cause_len);
				s_copy(cause + cause_len, cause, cause_len, 
					cause_len);
			    }
			}
		    }

/*              If out of range and no keyword then we don't have */
/*              a good guess as to what went wrong. */

		} else if (! keywrd && (mcount < lower || mcount > upper)) {
		    if (mcount < lower) {
			if (*m2code == 0) {
			    *m2code = 106;
			}
/* Computing MAX */
			i__1 = 0, i__2 = (mcount << 1) - lower;
			*score += max(i__1,i__2) * 15;
			error = TRUE_;
		    } else if (mcount > upper) {
			if (usekey || uselst) {

/*                       We are going to try to see if we had a spelling */
/*                       error that caused us to get too many words. */

			    fndnwd_(string, &suffsb, &db, &de, string_len);
			    count = lower + 1;
			    bscore = 0;
			    dcount = 0;
			    while(count <= upper + 1) {
				if (usekey) {
				    s_copy(known + 192, temp + (kb - 1), (
					    ftnlen)32, ke - (kb - 1));

/*                             Compare the last word encountered in the */
/*                             string with the KEYWORD we were expecting. */

				    bestwd_(string + (db - 1), known, cutoff, 
					    best, scores, mssg, de - (db - 1),
					     (ftnlen)32, (ftnlen)420);
				} else if (uselst) {

/*                             Compare the last word that we hit with one */
/*                             of the keywords from the list of possible */
/*                             closing keywords. */

				    bestwd_(string + (db - 1), keywds, cutoff,
					     best, scores, mssg, de - (db - 1)
					    , keywds_len, (ftnlen)420);
				}
				if (cardi_(scores) > 0 && scores[6] >= *
					cutoff) {

/*                             We are going to treat this as a spelling */
/*                             error. */

				    if (*m2code == 0) {
					*m2code = 12;
				    }

/*                             Save the beginning and ending of the */
/*                             problem word for use in the recovery */
/*                             entry point. */

				    if (scores[6] > bscore) {
					bscore = scores[6];
					pbeg = db;
					pend = de;

/*                                Everything up to this is now regarded */
/*                                as simply matching the META-KEY.  Store */
/*                                this number of META-KEYs for use by */
/*                                diagnostics generation. */

					dcount = count - 1;
				    }
				}
				suffsb = de + 1;

/*                          Look at the next word until we have gone */
/*                          past UPPER even if we already have a */
/*                          candidate for misspelling, there might be */
/*                          a better one. */

				fndnwd_(string, &suffsb, &db, &de, string_len)
					;
				++count;
			    }

/*                       Save the misspelling information associated */
/*                       with the best match (if there was one). */

			    if (bscore > 0) {
				if (usekey) {
				    s_copy(known + 192, temp + (kb - 1), (
					    ftnlen)32, ke - (kb - 1));

/*                             Compare the last word encountered in the */
/*                             string with the KEYWORD we were expecting. */

				    bestwd_(string + (pbeg - 1), known, 
					    cutoff, best, scores, mssg, pend 
					    - (pbeg - 1), (ftnlen)32, (ftnlen)
					    420);
				} else if (uselst) {

/*                             Compare the last word that we hit with one */
/*                             of the keywords from the list of possible */
/*                             closing keywords. */

				    bestwd_(string + (pbeg - 1), keywds, 
					    cutoff, best, scores, mssg, pend 
					    - (pbeg - 1), keywds_len, (ftnlen)
					    420);

/*                             Save the best matches for use in the */
/*                             recovery entry point. */

				    i__1 = cardi_(best);
				    for (i__ = 1; i__ <= i__1; ++i__) {
					s_copy(known + (((i__2 = i__ + 5) < 
						16 && 0 <= i__2 ? i__2 : 
						s_rnge("known", i__2, "m2cor"
						"e_", (ftnlen)1625)) << 5), 
						keywds + (best[(i__3 = i__ + 
						5) < 16 && 0 <= i__3 ? i__3 : 
						s_rnge("best", i__3, "m2core_"
						, (ftnlen)1625)] + 5) * 
						keywds_len, (ftnlen)32, 
						keywds_len);
				    }
				    i__1 = cardi_(best);
				    scardc_(&i__1, known, (ftnlen)32);
				}

/*                          This is not regarded as an error worth */
/*                          stopping for unless our */
/*                          misspelling total has runs over 100. */

				mspell += 100 - bscore;
				if (mspell < 100) {
				    *score = *score + dcount * 15 + bscore;

/*                             Back the value of SBEGIN back up to the */
/*                             point of failure, so that we can continue */
/*                             processing as if nothing had gone wrong. */

				    sbegin = pend + 1;
				} else {
				    *score += dcount * 15;
				    error = TRUE_;
				}
			    }
			}

/*                    We might not have had a good candidate for a */
/*                    misspelling, if not we don't have a good clue */
/*                    as to what went wrong. */

			if (*m2code == 0) {
			    *m2code = 107;
/* Computing MAX */
			    i__1 = 0, i__2 = (upper << 1) - mcount;
			    *score += max(i__1,i__2) * 15;
			    error = TRUE_;
			}
		    }

/*                 If there is to be a diagnostic generated, set up */
/*                 the beginning of it so that everyone else can */
/*                 share in the same work. */

		    if (*reason) {
			error = TRUE_;
			s_copy(cause, "I was expecting to see between # and "
				"# # ", cause_len, (ftnlen)41);
			m2clss_(temp + (tb - 1), &upper, phrase, tc - (tb - 1)
				, (ftnlen)120);
			repmc_(cause, "#", lowerc, cause, cause_len, (ftnlen)
				1, (ftnlen)64, cause_len);
			repmc_(cause, "#", upperc, cause, cause_len, (ftnlen)
				1, (ftnlen)64, cause_len);
			repmc_(cause, "#", phrase, cause, cause_len, (ftnlen)
				1, (ftnlen)120, cause_len);
			if (usekey) {
			    suffix_("followed by ", &c__1, cause, (ftnlen)12, 
				    cause_len);
			    suffix_("the keyword, ", &c__1, cause, (ftnlen)13,
				     cause_len);
			    suffix_(temp + (kb - 1), &c__1, cause, ke - (kb - 
				    1), cause_len);
			    suffix_(".", &c__0, cause, (ftnlen)1, cause_len);
			} else if (uselst) {
			    suffix_("followed by ", &c__1, cause, (ftnlen)12, 
				    cause_len);
			    suffix_("one of the ", &c__1, cause, (ftnlen)11, 
				    cause_len);
			    suffix_("keywords from the", &c__1, cause, (
				    ftnlen)17, cause_len);
			    suffix_("list {", &c__1, cause, (ftnlen)6, 
				    cause_len);
			    i__1 = nkey;
			    for (i__ = 1; i__ <= i__1; ++i__) {
				suffix_(keywds + (i__ + 5) * keywds_len, &
					c__1, cause, keywds_len, cause_len);
				if (i__ != nkey) {
				    suffix_(",", &c__1, cause, (ftnlen)1, 
					    cause_len);
				}
			    }
			    suffix_("}.", &c__1, cause, (ftnlen)2, cause_len);
			} else if (useend) {
			    suffix_("filling out the   ", &c__1, cause, (
				    ftnlen)18, cause_len);
			    suffix_("end of the string.", &c__1, cause, (
				    ftnlen)18, cause_len);
			}

/*                    Use the information stored in M2CODE to determine */
/*                    how many words we encountered before we figured */
/*                    out we had an error. */

			if (*m2code >= 100) {
			    inttxt_(&mcount, countc, (ftnlen)64);
			    lcase_(countc, countc, (ftnlen)64, (ftnlen)64);
			} else {
			    inttxt_(&dcount, countc, (ftnlen)64);
			    lcase_(countc, countc, (ftnlen)64, (ftnlen)64);
			}
			suffix_("I had counted ", &c__1, cause, (ftnlen)14, 
				cause_len);
			suffix_(countc, &c__1, cause, (ftnlen)64, cause_len);
			if (mcount == 1) {
			    suffix_("such word", &c__1, cause, (ftnlen)9, 
				    cause_len);
			} else {
			    suffix_("such words", &c__1, cause, (ftnlen)10, 
				    cause_len);
			}
			suffix_("when I encountered", &c__1, cause, (ftnlen)
				18, cause_len);
		    }

/*                 We are still in the case of a variable length template */
/*                 for which we did not hit a keyword and did not have */
/*                 the expected range of items for the current META-KEY. */

/*                 OK. Now tailor the end of the message to reflect */
/*                 what went wrong in particular. */

		    if (*reason && *m2code < 100) {
			suffix_("the word \"", &c__1, cause, (ftnlen)10, 
				cause_len);
			suffix_(string + (pbeg - 1), &c__0, cause, pend - (
				pbeg - 1), cause_len);
			suffix_("\" .", &c__0, cause, (ftnlen)3, cause_len);
			suffix_(mssg, &c__1, cause, (ftnlen)420, cause_len);
			m2mark_(string, &pbeg, &pend, cause, string_len, 
				cause_len);
			s_copy(cause + cause_len, cause, cause_len, cause_len)
				;
		    } else if (*reason && se == 0) {
			suffix_("the end of the input", &c__1, cause, (ftnlen)
				20, cause_len);
			suffix_("string.    ", &c__1, cause, (ftnlen)11, 
				cause_len);
			i__1 = qlstnb_(string, string_len) + 1;
			i__2 = qlstnb_(string, string_len) + 1;
			m2mark_(string, &i__1, &i__2, cause, string_len, 
				cause_len);
			s_copy(cause + cause_len, cause, cause_len, cause_len)
				;

/*                  check for a misspell. */

		    } else if (*reason && se != 0) {
			suffix_("the word \"", &c__1, cause, (ftnlen)10, 
				cause_len);
			suffix_(string + (sb - 1), &c__0, cause, se - (sb - 1)
				, cause_len);
			suffix_("\" .", &c__0, cause, (ftnlen)3, cause_len);

/*                    If misspell likely mention that too. */

			if (usekey) {
			    s_copy(known + 192, temp + (kb - 1), (ftnlen)32, 
				    ke - (kb - 1));
			    bestwd_(string + (sb - 1), known, cutoff, best, 
				    scores, mssg, se - (sb - 1), (ftnlen)32, (
				    ftnlen)420);
			} else if (uselst) {
			    bestwd_(string + (sb - 1), keywds, cutoff, best, 
				    scores, mssg, se - (sb - 1), keywds_len, (
				    ftnlen)420);
			}
			if (cardi_(scores) > 0 && scores[6] > *cutoff) {
			    suffix_(mssg, &c__1, cause, (ftnlen)420, 
				    cause_len);
			}
			m2mark_(string, &sb, &se, cause, string_len, 
				cause_len);
			s_copy(cause + cause_len, cause, cause_len, cause_len)
				;
		    }
		}
	    } else {

/*              This "ELSE" is the "NO" response to the question:  "Ok. */
/*              we have a range template. Is it of variable length?" */

		endit = lower == 0;
		mcount = 0;
		while(! endit) {
		    fndnwd_(string, &sbegin, &sb, &se, string_len);
		    if (se == 0) {
			endit = TRUE_;
			error = TRUE_;
			if (*m2code == 0) {
			    *m2code = 108;
			}
			if (*reason) {
			    s_copy(cause, "I was expecting to see # # when I"
				    " ran out of words in the command string. "
				    , cause_len, (ftnlen)74);
			    m2clss_(temp + (tb - 1), &c__1, phrase, tc - (tb 
				    - 1), (ftnlen)120);
			    ana_(ch__1, (ftnlen)2, phrase, "L", (ftnlen)120, (
				    ftnlen)1);
			    s_copy(artcle, ch__1, (ftnlen)2, (ftnlen)2);
			    repmc_(cause, "#", artcle, cause, cause_len, (
				    ftnlen)1, (ftnlen)2, cause_len);
			    repmc_(cause, "#", phrase, cause, cause_len, (
				    ftnlen)1, (ftnlen)120, cause_len);
			    i__1 = qlstnb_(string, string_len) + 1;
			    i__2 = qlstnb_(string, string_len) + 1;
			    m2mark_(string, &i__1, &i__2, cause, string_len, 
				    cause_len);
			    s_copy(cause + cause_len, cause, cause_len, 
				    cause_len);
			}
		    } else if (m2wmch_(string, &sb, &se, temp + (tb - 1), 
			    string_len, te - (tb - 1))) {
			++mcount;
			*score += 15;
			sbegin = se + 1;

/*                    Mark the position of the first character beyond the */
/*                    current STRING word. */

			begout = sbegin;
			endit = mcount >= lower;
		    } else {
			if (*m2code == 0) {
			    *m2code = 109;
			}
			error = TRUE_;
			endit = TRUE_;
			if (*reason) {
			    s_copy(cause, "I was expecting to see # # when I"
				    " encounterd the word \"#\" in the comman"
				    "d. ", cause_len, (ftnlen)74);
			    m2clss_(temp + (tb - 1), &c__1, phrase, tc - (tb 
				    - 1), (ftnlen)120);
			    ana_(ch__1, (ftnlen)2, phrase, "L", (ftnlen)120, (
				    ftnlen)1);
			    s_copy(artcle, ch__1, (ftnlen)2, (ftnlen)2);
			    repmc_(cause, "#", artcle, cause, cause_len, (
				    ftnlen)1, (ftnlen)2, cause_len);
			    repmc_(cause, "#", phrase, cause, cause_len, (
				    ftnlen)1, (ftnlen)120, cause_len);
			    repmc_(cause, "#", string + (sb - 1), cause, 
				    cause_len, (ftnlen)1, se - (sb - 1), 
				    cause_len);
			    m2mark_(string, &sb, &se, cause, string_len, 
				    cause_len);
			    s_copy(cause + cause_len, cause, cause_len, 
				    cause_len);
			}
		    }
		}
	    }
	} else {
	    fndnwd_(string, &sbegin, &sb, &se, string_len);

/*           This "ELSE" is the "NO" response to the question: "Is a */
/*           range template present?" that was asked a very long, long */
/*           time ago. */

	    cmatch = m2wmch_(string, &sb, &se, temp + (tb - 1), string_len, 
		    te - (tb - 1));

/*           Set the string pointer to the first character following */
/*           the current string word. */

	    sbegin = se + 1;

/*           Record SBEGIN in case we have run out of teplate and */
/*           haven't produced any errors. */

	    begout = sbegin;
	    if (cmatch) {
		keywrd = m2keyw_(temp + (tb - 1), te - (tb - 1));
		if (keywrd) {
		    *score += 100;
		} else {
		    *score += 15;
		}
	    } else if (! cmatch) {
		keywrd = m2keyw_(temp + (tb - 1), te - (tb - 1));

/*              See if we were supposed to get a keyword and if */
/*              so see if this is just some simple spelling error. */

		if (keywrd) {
		    s_copy(known + 192, temp + (tb - 1), (ftnlen)32, tc - (tb 
			    - 1));
		    scardc_(&c__1, known, (ftnlen)32);
		    if (se > 0) {
			bestwd_(string + (sb - 1), known, cutoff, best, 
				scores, mssg, se - (sb - 1), (ftnlen)32, (
				ftnlen)420);
		    }
		    if (cardi_(scores) > 0 && scores[6] >= *cutoff) {
			if (*m2code == 0) {
			    *m2code = 11;

/*                       Save the beginning and ending of the */
/*                       problem word for use in the recovery */
/*                       entry point. */

			    pbeg = sb;
			    pend = se;
			}

/*                    We regard this to be a spelling error  of the */
/*                    keyword. This will be a signal to stop looking at */
/*                    this keyword if we are asking for diagnostics. */

			if (mspell > 100) {
			    error = TRUE_;
			} else {
			    *score += scores[6];
			    mspell += 100 - scores[6];
			}
			if (*reason) {
			    error = TRUE_;
			    s_copy(cause, "I was expecting to see the keywor"
				    "d \"", cause_len, (ftnlen)36);
			    suffix_(temp + (tb - 1), &c__0, cause, tc - (tb - 
				    1), cause_len);
			    suffix_("\" when I encountered", &c__0, cause, (
				    ftnlen)20, cause_len);
			    suffix_("the word \"", &c__1, cause, (ftnlen)10, 
				    cause_len);
			    suffix_(string + (sb - 1), &c__0, cause, se - (sb 
				    - 1), cause_len);
			    suffix_("\" in the input ", &c__0, cause, (ftnlen)
				    15, cause_len);
			    suffix_("string.     ", &c__1, cause, (ftnlen)12, 
				    cause_len);
			    suffix_(mssg, &c__1, cause, (ftnlen)420, 
				    cause_len);
			    s_copy(cause + cause_len, cause, cause_len, 
				    cause_len);
			    s_copy(cause, mssg, cause_len, (ftnlen)420);
			    m2mark_(string, &sb, &se, cause, string_len, 
				    cause_len);
			    m2mark_(string, &sb, &se, cause + cause_len, 
				    string_len, cause_len);
			}
		    } else if (cardi_(scores) == 0 || scores[6] < *cutoff) {
			error = TRUE_;
			if (*m2code == 0) {
			    *m2code = 110;
			    if (se > 0) {
				bestwd_(string + (sb - 1), known, &c__1, best,
					 scores, mssg, se - (sb - 1), (ftnlen)
					32, (ftnlen)420);
			    }
			    if (sb != 0 && cardi_(scores) > 0) {
				*score += scores[6];
			    }
			}
			if (*reason) {
			    s_copy(cause, "I was expecting to see the keywor"
				    "d \"", cause_len, (ftnlen)36);
			    suffix_(temp + (tb - 1), &c__0, cause, tc - (tb - 
				    1), cause_len);
			    suffix_("\" when I ", &c__0, cause, (ftnlen)9, 
				    cause_len);
			    if (sb == 0) {
				suffix_("ran out of ", &c__1, cause, (ftnlen)
					11, cause_len);
				suffix_("characters in the", &c__1, cause, (
					ftnlen)17, cause_len);
				suffix_("input string. ", &c__1, cause, (
					ftnlen)14, cause_len);
				sb = qlstnb_(string, string_len) + 1;
				se = sb;
			    } else {
				suffix_("encountered", &c__1, cause, (ftnlen)
					11, cause_len);
				suffix_("the word \"", &c__1, cause, (ftnlen)
					10, cause_len);
				suffix_(string + (sb - 1), &c__0, cause, se - 
					(sb - 1), cause_len);
				suffix_("\" in the input ", &c__0, cause, (
					ftnlen)15, cause_len);
				suffix_("string.     ", &c__1, cause, (ftnlen)
					12, cause_len);
			    }
			    m2mark_(string, &sb, &se, cause, string_len, 
				    cause_len);
			    s_copy(cause + cause_len, cause, cause_len, 
				    cause_len);
			}
		    }
		} else if (! m2keyw_(temp + (tb - 1), te - (tb - 1))) {
		    error = TRUE_;
		    if (*m2code == 0) {
			*m2code = 111;
		    }
		    if (*reason) {
			s_copy(cause, "I was expecting to see # # when I ", 
				cause_len, (ftnlen)34);
			m2clss_(temp + (tb - 1), &c__1, phrase, tc - (tb - 1),
				 (ftnlen)120);
			ana_(ch__1, (ftnlen)2, phrase, "L", (ftnlen)120, (
				ftnlen)1);
			s_copy(artcle, ch__1, (ftnlen)2, (ftnlen)2);
			repmc_(cause, "#", artcle, cause, cause_len, (ftnlen)
				1, (ftnlen)2, cause_len);
			repmc_(cause, "#", phrase, cause, cause_len, (ftnlen)
				1, (ftnlen)120, cause_len);
			if (sb == 0) {
			    suffix_("ran out of characters", &c__1, cause, (
				    ftnlen)21, cause_len);
			    suffix_("in the input string. ", &c__1, cause, (
				    ftnlen)21, cause_len);
			    sb = qlstnb_(string, string_len) + 1;
			    se = sb;
			} else {
			    suffix_("encountered the word \"", &c__1, cause, (
				    ftnlen)22, cause_len);
			    suffix_(string + (sb - 1), &c__0, cause, se - (sb 
				    - 1), cause_len);
			    suffix_("\" in the input string.", &c__0, cause, (
				    ftnlen)22, cause_len);
			}
			m2mark_(string, &sb, &se, cause, string_len, 
				cause_len);
			s_copy(cause + cause_len, cause, cause_len, cause_len)
				;
		    }
		}
	    }
	}
	tbegin = max(ke,te) + 1;

/*        Locate the next word of the template and continue unless */
/*        we get a second error detected. */

	fndnwd_(temp, &tbegin, &tb, &te, temp_len);
    }

/*     If we got out of the template without an error, set SBEG to */
/*     BEGOUT---the first character after the matched portion of the */
/*     STRING and before the first word of whatever is left. */

    if (*m2code == 0) {
	*sbeg = begout;
    }
    return 0;

/* $Prodedure M2RCVR ( Recover from a spelling error ) */


L_m2rcvr:

/* $ Abstract */

/*     Fetch the indices of the beginning and end of a "misspelled" */
/*     keyword along with the list of corrections. */

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

/*     The META/2 book. */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     INTEGER               SBEG */
/*     INTEGER               SEND */
/*     CHARACTER*(*)         KEYWDS ( LBCELL: * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SBEG       O   Beginning of "misspelled" word in STRING */
/*     SEND       O   Ending of "misspelled" word in STRING */
/*     KEYWDS     O   Cell of possible correct spellings of keyword. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     SBEG       Beginning of "misspelled" word in STRING */

/*     SEND       Ending of "misspelled" word in STRING */

/*     KEYWDS     Cell of possible correct spellings of keyword. */

/* $ Error_Handling */

/*     No errors are detected by this entry point. */

/* $ Input_Files */

/*     None. */

/* $ Output_Files */

/*     None. */

/* $ Particulars */


/* $ Examples */


/* $ Restrictions */

/*     One must call M2MTCH before calling this routine if correct */
/*     results are desired. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/*     Version B1.0.0, 7-APR-1988 (WLT) (IMU) */

/* -& */

    *sbeg = pbeg;
    *send = pend;
    copyc_(known, keywds, (ftnlen)32, keywds_len);
    return 0;
} /* m2core_ */

/* Subroutine */ int m2core_(char *temp, integer *tbeg, char *keywds, char *
	string, integer *sbeg, logical *reason, integer *cutoff, integer *
	m2code, integer *score, char *cause, integer *send, ftnlen temp_len, 
	ftnlen keywds_len, ftnlen string_len, ftnlen cause_len)
{
    return m2core_0_(0, temp, tbeg, keywds, string, sbeg, reason, cutoff, 
	    m2code, score, cause, send, temp_len, keywds_len, string_len, 
	    cause_len);
    }

/* Subroutine */ int m2mtch_(char *temp, integer *tbeg, char *keywds, char *
	string, integer *sbeg, logical *reason, integer *cutoff, integer *
	m2code, integer *score, char *cause, ftnlen temp_len, ftnlen 
	keywds_len, ftnlen string_len, ftnlen cause_len)
{
    return m2core_0_(1, temp, tbeg, keywds, string, sbeg, reason, cutoff, 
	    m2code, score, cause, (integer *)0, temp_len, keywds_len, 
	    string_len, cause_len);
    }

/* Subroutine */ int m2rcvr_(integer *sbeg, integer *send, char *keywds, 
	ftnlen keywds_len)
{
    return m2core_0_(2, (char *)0, (integer *)0, keywds, (char *)0, sbeg, (
	    logical *)0, (integer *)0, (integer *)0, (integer *)0, (char *)0, 
	    send, (ftnint)0, keywds_len, (ftnint)0, (ftnint)0);
    }

