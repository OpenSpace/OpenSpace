/* bestwd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;
static integer c__2 = 2;

/* $Procedure      BESTWD ( Perform a spell match against a set of words ) */
/* Subroutine */ int bestwd_(char *word, char *known, integer *cutoff, 
	integer *best, integer *scores, char *mssg, ftnlen word_len, ftnlen 
	known_len, ftnlen mssg_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen), i_len(char *, ftnlen);

    /* Local variables */
    char case__[32];
    integer help[10], item[10], hits;
    char mywd[32];
    integer i__, j, k;
    extern integer cardc_(char *, ftnlen);
    integer l;
    extern integer cardi_(integer *);
    extern /* Subroutine */ int lcase_(char *, char *, ftnlen, ftnlen);
    integer nbest, maxsc;
    extern integer sizei_(integer *);
    integer tries;
    char trans[16];
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    integer usize;
    extern integer matchc_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int matche_(char *, char *, char *, integer *, 
	    ftnlen, ftnlen, ftnlen), scardi_(integer *, integer *);
    extern integer matcho_(char *, char *, ftnlen, ftnlen);
    integer bscore[10], cscore, length;
    extern /* Subroutine */ int mspeld_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen);
    integer oscore;
    extern /* Subroutine */ int intord_(integer *, char *, ftnlen), suffix_(
	    char *, integer *, char *, ftnlen, ftnlen);
    integer nknown;
    extern integer lstlti_(integer *, integer *, integer *), qrtrim_(char *, 
	    ftnlen);
    integer loc;
    logical hit;
    char nth[80];

/* $ Abstract */

/*     Given a word and a list of known words, return those of the list */
/*     closest to the word along with a diagnostic message. */

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

/*     COMPARE */
/*     PARSING */
/*     UTILITY */
/*     WORD */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     WORD       I   Word to compare against a list of known words. */
/*     KNOWN      I   List of known words. */
/*     CUTOFF     I   Fine tuning value.  A "good" value is 70. */
/*     BEST       O   Indices of the best matches. */
/*     SCORES     O   Scores of the best matches. */
/*     MSSG       O   Explanatory message. */

/* $ Detailed_Input */

/*     WORD       is any word.  Typically it will be a word that was not */
/*                equal to some "known" word and for which one wants to */
/*                find the "closest" known word. ONLY the first 32 */
/*                characters of WORD are regarded as being significant. */

/*     KNOWN      is a cell containing "known" words.  These might be */
/*                keywords from a command, filenames, directories, etc. */
/*                From these a collection are found that most closely */
/*                match WORD. */

/*     CUTOFF     is an integer used to "fine tune" the matching */
/*                between WORD and the words in KNOWN. */

/*                CUTOFF should be between 0 and 100.  Values of */
/*                CUTOFF greater than 100 will effectively disable */
/*                the more detailed error diagnostics.  Values */
/*                less than 0 will simply cause the routine to work */
/*                harder with no gain in information. */

/*                A "good" value for CUTOFF is 70.  You will probably */
/*                want your input value to be close to this. */

/* $ Detailed_Output */

/*      BEST      BEST is a cell. On output BEST contains the indices of */
/*                the items in KNOWN that yielded the maximum comparison */
/*                score when compared to word. BEST will never contain */
/*                more than 10 indices. (It will rarely contain more */
/*                than two.) */

/*      SCORE     SCORE is a cell. SCORE is assumed to be declared the */
/*                same size as BEST.  On output SCORE(I) contains the */
/*                score that measures the similarity between between */
/*                KNOWN(BEST(I)) and WORD. */

/*                If WORD should happen to equal one of the KNOWN words */
/*                SCORE will be returned with a value of 1000.  Otherwise */
/*                it will be returned with a value between 0 and 100. */
/*                The higher the value of SCORE(I) the greater the */
/*                similarity between WORD and KNOWN(BEST(I)). */

/*                By comparing the values in SCORE with CUTOFF you can */
/*                determine how good a particular match is. */
/*                If SCORE is at least as big as CUTOFF the match is */
/*                regarded as a good one.  An attempt will have been */
/*                made at giving detailed diagnostics on the difference */
/*                between WORD and the best matching KNOWNs. */

/*      MSSG      is a message that identifies those KNOWN words that */
/*                best match WORD.  Moreover, if detailed diagnostics */
/*                are available, they will be reported in MSSG. */

/* $ Error_Handling */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*      This routine can be used to help a program recover from common */
/*      typing and spelling mistakes.  When a word is not recognized, it */
/*      is possible (perhaps likely) that a keystroke or two went awry in */
/*      the typing of the word.  If the list of legitimate words is */
/*      available, the unrecognized word can be compared with the */
/*      recognized words.  In this way, the one or ones that most nearly */
/*      resemble the unrecognized word can be identified.  From there the */
/*      program may point out the likely problem, offer to fix it, fix it */
/*      and continue (or any subset of these).  Thus to some extent the */
/*      program can do what you meant, not what you typed. */

/*      To measure the similarity between two words several techniques */
/*      are employed.  The first of these just compares the letter */
/*      sets of the two words.  For example the letter sets for the */
/*      words 'SIMILARITY' and 'SIMILITUDE' are */

/*         {A1, I1, I2, I3, L1, M1, R1, S1, T1, Y1 } */

/*      and */

/*         {E1, I1, I2, I3, L1, M1, S1, T1, U1 } */

/*      (Note that repeated letters are regardeds as distinct.) */

/*      By examining the symmetric difference between these two sets */
/*      one can get a measure of the how close the two words are. */
/*      The method used to compute the score will yield a value of */
/*      75 or higher for pairs of words whose letter sets have */
/*      a symmetric difference of 2 or fewer letters. */

/*      This does a good job of separating words such as */
/*      'CANADA' and 'ILLINOIS'.  However, it fails completely to */
/*      separate the words 'TRIANGLE', 'INTEGRAL', 'RELATING' and */
/*      'ALTERING'.  These four words all have the same letter sets. */

/*      Thus for words that compare well on the basis of letter sets */
/*      a second (more time consuming) comparison is made to see if */
/*      the words preserve the relative letter order.  In this step */
/*      each word is used to construct a sequence of templates */
/*      that are then matched against the other.  A tally of the */
/*      hits to misses is kept.  The roles of the two words are then */
/*      reversed and another tally computed.  The average of these */
/*      two scores is given to the word pair. */
/*      This is best illustrated with a simple example. */

/*      Consider the words ANGER and RANGE. */

/*      ANGER will be used to construct the 10 templates: */

/*         *A*N*, *A*G*, *A*E*, *A*R*, *N*G*, */
/*         *N*E*, *N*R*, *G*E*, *G*R*, *E*R* */

/*      Six of these match RANGE, namely */

/*         *A*N*, *A*G*, *A*E*, *N*G*, *N*E*, *G*E*, *E*R* */

/*      Next the 4 templates */

/*         *AN*,  *NG*, *GE*, *ER* */

/*      will be compared with RANGE,  The first three match.  Each */
/*      of these matches are "extra matches" that are added on to */
/*      the first 6 matches.  The score for ANGER to RANGE is */

/*        100 * MIN{1,(total matches / numer of templates of form *x*y*)} */
/*      = 100 * MIN{1, 9/10 } */
/*      = 90 */

/*      The method extends in the obvious way to longer and shorter */
/*      words than ANGER and RANGE. As can be seen, this method of */
/*      comparing one word against another, requires not only the */
/*      correct letters to be present but they must also be in the */
/*      correct relative order.  Note that a perfect score of 100 */
/*      does not mean the words are the same.  For example */

/*         AEAE and EAEA */

/*      yield an identical set of templates and hence have a matching */
/*      score of 100.  However, if both words have no letters repeated, */
/*      a score of 100 implies that the words are in fact the same. */

/*      If both methods of scoring exceed the value of CUTOFF, an */
/*      attempt is made to determine the exact difference between the */
/*      two words. The recognizable differences are: transposition of */
/*      two letters, a letter missing, a letter mistyped, an extra */
/*      letter. Thus CUTOFF allows the user to tune the circumstances */
/*      underwhich a attempts will be made to perform detailed */
/*      diagnosis of the the difference between a pair of words. */

/*      Empirically, it has been found that two words match up well if */
/*      both methods of scoring yield values of at least 70. This */
/*      is the recommended value for CUTOFF. */

/*      If both methods of scoring above yield values that exceed CUTOFF, */
/*      the two scores are averaged to give the score reported in SCORE. */
/*      If they do not both exceed CUTOFF but the average does, then */
/*      the score returned is CUTOFF-1. */

/*      CUTOFF can also be used as your means of determining how good */
/*      a match was.  Words with matching scores at least CUTOFF are */
/*      regarded as "good" matches, otherwise the match is viewed as */
/*      "poor." */

/* $ Examples */

/*      Suppose that */

/*      CUTOFF = 70 */
/*      KNOWN  = 'ALTITUDE',      'CONTRACT', */
/*               'APPLE',         'INTEGRATE', */
/*               'LONGITUDE',     'EXTRACT', */
/*               'JUPITER',       'LATITUDE', */
/*               'EXPAND',        'SATURN', */
/*               'MERIDIAN',      'SHIFT', */
/*               'URANUS',        'ELEVATION', */
/*               'EPOCH',         'NEPTUNE', */
/*               'ASCENSION',     'DELTA', */
/*               'PLUTO',         'DECLINATION', */
/*               'COMPLEMENT' */

/*      If    WORD    = 'APPLY'   then     BEST(0)        = 1 */
/*                                         KNOWN(BEST(1)) = 'APPLE' */
/*                                         SCORE(     1 ) = 89 */


/*      If    WORD    = 'X'       then     BEST(0)        = 2 */
/*                                         KNOWN(BEST(1)) = 'EXTRACT' */
/*                                         SCORE(     1 ) = 7 */

/*                                         KNOWN(BEST(2)) = 'EXPAND' */
/*                                         SCORE(     2 ) = 8 */


/*      If    WORD    = 'NEMPTUNE'  then   BEST(0)        = 1 */
/*                                         KNOWN(BEST(1)) = 'NEPTUNE' */
/*                                         SCORE(     1 ) = 95 */


/*      If    WORD    = 'ELATION'   then   BEST(0)        = 1 */
/*                                         KNOWN(BEST(1)) = 'ELEVATION' */
/*                                         SCORE(     1 ) = 94 */


/*      If    WORD    = 'QQQ'       then   BEST(0)        = 0 */


/*      If    WORD    = 'COEMPLMENT' then  BEST(1)        = 1 */
/*                                         KNOWN(BEST(1)) = 'COMPLEMENT' */
/*                                         SCORE(     1 ) = 100 */


/* $ Restrictions */

/*     SCORES must be declared to be at least as large as BEST. */

/*     At most 10 best indices will be returned in BEST. */

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


/*     Version B1.0.0, 12-APR-1988 (WLT) (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     First determine how many words we have to compare with */
/*     and the amount of room for reporting indices of "good" */
/*     matches. */

    nknown = cardc_(known, known_len);
    nbest = sizei_(best);

/*     This routine only works on words of 32 or fewer characters */

    s_copy(mywd, " ", (ftnlen)32, (ftnlen)1);
    s_copy(mywd, word, (ftnlen)32, word_len);

/*     USIZE refers to the amount of space we will actually */
/*     use in the buffers that store the best MATCHC scores and */
/*     the associated KNOWN word. */

/* Computing MIN */
    i__1 = min(10,nknown);
    usize = min(i__1,nbest);
    i__1 = usize;
    for (i__ = 1; i__ <= i__1; ++i__) {
	bscore[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("bscore", 
		i__2, "bestwd_", (ftnlen)394)] = 0;
	item[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("item", i__2, 
		"bestwd_", (ftnlen)395)] = 0;
	help[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("help", i__2, 
		"bestwd_", (ftnlen)396)] = 0;
	scores[i__ + 5] = 0;
    }

/*     First apply MATCHC against each of the KNOWNs and keep the */
/*     top USIZE words that match. */

    i__1 = nknown;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Just in case, see if we have an exact match. */

	if (eqstr_(mywd, known + (i__ + 5) * known_len, (ftnlen)32, known_len)
		) {
	    scardi_(&c__1, best);
	    scardi_(&c__1, scores);
	    best[6] = i__;
	    scores[6] = 1000;
	    intord_(&i__, nth, (ftnlen)80);
	    lcase_(nth, nth, (ftnlen)80, (ftnlen)80);
	    s_copy(mssg, mywd, mssg_len, (ftnlen)32);
	    suffix_("is equal to the ", &c__1, mssg, (ftnlen)16, mssg_len);
	    suffix_(nth, &c__1, mssg, (ftnlen)80, mssg_len);
	    suffix_(" known word.", &c__1, mssg, (ftnlen)12, mssg_len);
	    return 0;
	}
	cscore = matchc_(mywd, known + (i__ + 5) * known_len, (ftnlen)32, 
		known_len);
	j = lstlti_(&cscore, &usize, bscore);
	i__2 = j - 1;
	for (k = 1; k <= i__2; ++k) {
	    bscore[(i__3 = k - 1) < 10 && 0 <= i__3 ? i__3 : s_rnge("bscore", 
		    i__3, "bestwd_", (ftnlen)437)] = bscore[(i__4 = k) < 10 &&
		     0 <= i__4 ? i__4 : s_rnge("bscore", i__4, "bestwd_", (
		    ftnlen)437)];
	    item[(i__3 = k - 1) < 10 && 0 <= i__3 ? i__3 : s_rnge("item", 
		    i__3, "bestwd_", (ftnlen)438)] = item[(i__4 = k) < 10 && 
		    0 <= i__4 ? i__4 : s_rnge("item", i__4, "bestwd_", (
		    ftnlen)438)];
	}
	if (j > 0) {
	    bscore[(i__2 = j - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("bscore", 
		    i__2, "bestwd_", (ftnlen)442)] = cscore;
	    item[(i__2 = j - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("item", 
		    i__2, "bestwd_", (ftnlen)443)] = i__;
	}
    }

/*     Now for the top USIZE matches, perform a MATCHO comparison. */
/*     If we get a match of CUTOFF or higher.  Run MATCHE against it */
/*     to see if we can guess at what went wrong. */

/*     So far our best score is 0 and we haven't HIT any good matches. */

    maxsc = 0;
    hits = 0;
    i__1 = usize;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Only examine items that have legitimate indices. */

	if (item[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("item", 
		i__2, "bestwd_", (ftnlen)463)] != 0) {
	    bscore[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("bscore"
		    , i__2, "bestwd_", (ftnlen)465)] = matcho_(mywd, known + (
		    item[(i__3 = i__ - 1) < 10 && 0 <= i__3 ? i__3 : s_rnge(
		    "item", i__3, "bestwd_", (ftnlen)465)] + 5) * known_len, (
		    ftnlen)32, known_len);
	    cscore = matchc_(mywd, known + (item[(i__2 = i__ - 1) < 10 && 0 <=
		     i__2 ? i__2 : s_rnge("item", i__2, "bestwd_", (ftnlen)
		    466)] + 5) * known_len, (ftnlen)32, known_len);
/* Computing MAX */
	    i__3 = bscore[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		    "bscore", i__2, "bestwd_", (ftnlen)467)];
	    maxsc = max(i__3,maxsc);
	    if (bscore[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		    "bscore", i__2, "bestwd_", (ftnlen)469)] >= *cutoff && 
		    cscore >= *cutoff) {

/*              We've HIT a good match. */

		++hits;

/*              See if the problem with this word can be diagnosed */
/*              with MATCHE. */

		matche_(mywd, known + (item[(i__2 = i__ - 1) < 10 && 0 <= 
			i__2 ? i__2 : s_rnge("item", i__2, "bestwd_", (ftnlen)
			481)] + 5) * known_len, trans, &loc, (ftnlen)32, 
			known_len, (ftnlen)16);

/*              If a diagnosis can be performed on this item, we */
/*              say that HELP is available at level 2.  Otherwise */
/*              since we have a good match anyway we say HELP is */
/*              available at level 1. */

		if (s_cmp(trans, "NONE", (ftnlen)16, (ftnlen)4) != 0) {
		    help[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
			    "help", i__2, "bestwd_", (ftnlen)491)] = 2;
		} else {
		    help[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
			    "help", i__2, "bestwd_", (ftnlen)493)] = 1;
		}
	    }
	}
    }

/*     If none of the words had a sufficiently high score, just */
/*     report those that had the maximum score. */

    if (hits == 0) {

/*        Just report the item(s) that had the biggest score. */

/*        First see how many had the maximum score. */

	i__1 = usize;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (item[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("item"
		    , i__2, "bestwd_", (ftnlen)513)] != 0) {
		if (bscore[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
			"bscore", i__2, "bestwd_", (ftnlen)515)] == maxsc) {
		    ++hits;
		}
	    }
	}

/*        If there were no KNOWN words that had letters in common */
/*        with MYWD, all of the elements of the array ITEM will be */
/*        zero and we will not have made any HITS against MAXSC. */
/*        There is nothing at all we can do in this case. */

	if (hits == 0) {
	    s_copy(mssg, "The word", mssg_len, (ftnlen)8);
	    suffix_(mywd, &c__1, mssg, (ftnlen)32, mssg_len);
	    suffix_("has nothing in common with any of", &c__1, mssg, (ftnlen)
		    33, mssg_len);
	    suffix_("the words I can recognize.  If ", &c__1, mssg, (ftnlen)
		    31, mssg_len);
	    suffix_("this word was typed interactively,", &c__1, mssg, (
		    ftnlen)34, mssg_len);
	    suffix_("you may want to see if your ", &c__1, mssg, (ftnlen)28, 
		    mssg_len);
	    suffix_("fingers are over the correct keys.", &c__1, mssg, (
		    ftnlen)34, mssg_len);
	    scardi_(&c__0, best);
	    scardi_(&c__0, scores);
	    return 0;
	}

/*        Still here.  Then we have at least some item that has */
/*        something in common with MYWD.  Set up a closing string so */
/*        that grammar will be correct. */

	if (hits > 1) {
	    s_copy(case__, "my closest matches are: ", (ftnlen)32, (ftnlen)24)
		    ;
	} else {
	    s_copy(case__, "my closest match is: ", (ftnlen)32, (ftnlen)21);
	}
	s_copy(mssg, "The word '", mssg_len, (ftnlen)10);
	suffix_(mywd, &c__1, mssg, (ftnlen)32, mssg_len);
	suffix_("' did not match up well with any of", &c__1, mssg, (ftnlen)
		35, mssg_len);
	suffix_("the words I was comparing against.", &c__1, mssg, (ftnlen)34,
		 mssg_len);
	suffix_("However,", &c__1, mssg, (ftnlen)8, mssg_len);
	suffix_(case__, &c__1, mssg, (ftnlen)32, mssg_len);

/*        Now append the list of KNOWN words that matched MYWD with */
/*        the highest score. */

	hit = FALSE_;
	j = 0;
	i__1 = usize;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (item[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("item"
		    , i__2, "bestwd_", (ftnlen)576)] == 0) {

/*              don't do anything */

	    } else if (bscore[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : 
		    s_rnge("bscore", i__2, "bestwd_", (ftnlen)582)] == maxsc) 
		    {
		++j;
		best[j + 5] = item[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 :
			 s_rnge("item", i__2, "bestwd_", (ftnlen)586)];
		l = qrtrim_(known + (item[(i__2 = i__ - 1) < 10 && 0 <= i__2 ?
			 i__2 : s_rnge("item", i__2, "bestwd_", (ftnlen)587)] 
			+ 5) * known_len, known_len);
		if (hit) {
		    suffix_(",  '", &c__0, mssg, (ftnlen)4, mssg_len);
		    suffix_(known + (item[(i__2 = i__ - 1) < 10 && 0 <= i__2 ?
			     i__2 : s_rnge("item", i__2, "bestwd_", (ftnlen)
			    592)] + 5) * known_len, &c__0, mssg, l, mssg_len);
		    suffix_("'", &c__0, mssg, (ftnlen)1, mssg_len);
		} else {
		    hit = TRUE_;
		    suffix_("'", &c__1, mssg, (ftnlen)1, mssg_len);
		    suffix_(known + (item[(i__2 = i__ - 1) < 10 && 0 <= i__2 ?
			     i__2 : s_rnge("item", i__2, "bestwd_", (ftnlen)
			    600)] + 5) * known_len, &c__0, mssg, l, mssg_len);
		    suffix_("'", &c__0, mssg, (ftnlen)1, mssg_len);
		}
		suffix_(".", &c__0, mssg, (ftnlen)1, mssg_len);
	    }
	}

/*        Set the cardinality of the window of BEST indices. */

	scardi_(&j, best);
    } else if (hits == 1) {

/*        There was just one KNOWN word for which there was a good */
/*        match.  Call MSPELD to produce a diagnosis of the problem */
/*        and record the index of the item. */

	i__ = 1;
	while(help[(i__1 = i__ - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("help", 
		i__1, "bestwd_", (ftnlen)625)] == 0) {
	    ++i__;
	}
	mspeld_(mywd, known + (item[(i__1 = i__ - 1) < 10 && 0 <= i__1 ? i__1 
		: s_rnge("item", i__1, "bestwd_", (ftnlen)629)] + 5) * 
		known_len, mssg, (ftnlen)32, known_len, mssg_len);
	best[6] = item[(i__1 = i__ - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
		"item", i__1, "bestwd_", (ftnlen)631)];
	scardi_(&c__1, best);
    } else {

/*        There were at least two "good" words.  If any of them */
/*        could be diagnosed, then report them.  Otherwise */
/*        report only those that had a maximum MATCHO score. */

	tries = 0;
	for (i__ = 1; i__ <= 5; ++i__) {
	    if (help[(i__1 = i__ - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("help"
		    , i__1, "bestwd_", (ftnlen)645)] == 2) {
		++tries;
	    }
	}
	if (tries == 0) {

/*           None of the KNOWN words had diagnostics available. */

	    s_copy(mssg, "Although a the spelling error can't be described i"
		    "n a simple way,  I have found the following words that m"
		    "ay be what you were trying to say.  ", mssg_len, (ftnlen)
		    142);
	    j = 0;
	    i__1 = usize;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (help[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
			"help", i__2, "bestwd_", (ftnlen)665)] != 0) {
		    suffix_("'", &c__2, mssg, (ftnlen)1, mssg_len);
		    suffix_(known + (item[(i__2 = i__ - 1) < 10 && 0 <= i__2 ?
			     i__2 : s_rnge("item", i__2, "bestwd_", (ftnlen)
			    668)] + 5) * known_len, &c__0, mssg, known_len, 
			    mssg_len);
		    suffix_("',", &c__0, mssg, (ftnlen)2, mssg_len);
		    ++j;
		    best[j + 5] = item[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? 
			    i__2 : s_rnge("item", i__2, "bestwd_", (ftnlen)
			    672)];
		}
	    }
	    scardi_(&j, best);
	    i__1 = qrtrim_(mssg, mssg_len) - 1;
	    s_copy(mssg + i__1, " ", qrtrim_(mssg, mssg_len) - i__1, (ftnlen)
		    1);
	} else if (tries == 1) {

/*            Only one of the KNOWN words had diagnostics available. */

	    for (i__ = 1; i__ <= 5; ++i__) {
		if (help[(i__1 = i__ - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
			"help", i__1, "bestwd_", (ftnlen)689)] == 2) {
		    mspeld_(mywd, known + (item[(i__1 = i__ - 1) < 10 && 0 <= 
			    i__1 ? i__1 : s_rnge("item", i__1, "bestwd_", (
			    ftnlen)690)] + 5) * known_len, mssg, (ftnlen)32, 
			    known_len, mssg_len);
		    best[6] = item[(i__1 = i__ - 1) < 10 && 0 <= i__1 ? i__1 :
			     s_rnge("item", i__1, "bestwd_", (ftnlen)691)];
		}
	    }
	    scardi_(&c__1, best);
	} else {

/*           At least two of the KNOWN words had diagnostics available. */
/*           Report all of them. */

	    s_copy(mssg, "The following common spelling mistakes may be the "
		    "reason I did not recognize ", mssg_len, (ftnlen)77);
	    suffix_(mywd, &c__1, mssg, (ftnlen)32, mssg_len);
	    suffix_(".", &c__1, mssg, (ftnlen)1, mssg_len);
	    length = i_len(mssg, mssg_len);
	    j = 0;
	    i__1 = usize;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (help[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
			"help", i__2, "bestwd_", (ftnlen)716)] == 2) {
		    if (qrtrim_(mssg, mssg_len) < length - 3) {
			i__3 = qrtrim_(mssg, mssg_len) + 2;
			mspeld_(mywd, known + (item[(i__2 = i__ - 1) < 10 && 
				0 <= i__2 ? i__2 : s_rnge("item", i__2, "bes"
				"twd_", (ftnlen)719)] + 5) * known_len, mssg + 
				i__3, (ftnlen)32, known_len, mssg_len - i__3);
			++j;
			best[j + 5] = item[(i__2 = i__ - 1) < 10 && 0 <= i__2 
				? i__2 : s_rnge("item", i__2, "bestwd_", (
				ftnlen)723)];
		    }
		}
	    }
	    scardi_(&j, best);
	}
    }

/*     As for the scores, we will report the average of the MATCHO and */
/*     MATCHC scores for the best matches. */

    i__1 = cardi_(best);
    for (i__ = 1; i__ <= i__1; ++i__) {
	oscore = matcho_(mywd, known + (best[i__ + 5] + 5) * known_len, (
		ftnlen)32, known_len);
	cscore = matchc_(mywd, known + (best[i__ + 5] + 5) * known_len, (
		ftnlen)32, known_len);
	scores[i__ + 5] = (oscore + cscore) / 2;
	if (oscore < *cutoff || cscore < *cutoff) {
/* Computing MIN */
	    i__2 = scores[i__ + 5], i__3 = *cutoff - 1;
	    scores[i__ + 5] = min(i__2,i__3);
	}
    }
    i__1 = cardi_(best);
    scardi_(&i__1, scores);
    return 0;
} /* bestwd_ */

