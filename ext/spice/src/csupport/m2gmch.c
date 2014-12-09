/* m2gmch.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__64 = 64;
static integer c__1 = 1;
static integer c__0 = 0;
static logical c_false = FALSE_;
static integer c__5 = 5;

/* $Procedure      M2GMCH ( Match a META/2 template including groups ) */
/* Subroutine */ int m2gmch_(char *temp, char *thnwds, char *string, integer *
	sbeg, logical *reason, integer *cutoff, logical *pssthn, integer *
	m2code, integer *score, char *cause, ftnlen temp_len, ftnlen 
	thnwds_len, ftnlen string_len, ftnlen cause_len)
{
    /* Initialized data */

    static logical pass1 = TRUE_;

    /* System generated locals */
    address a__1[5];
    integer i__1, i__2, i__3[5];
    char ch__1[1184], ch__2[1265];

    /* Builtin functions */
    integer i_len(char *, ftnlen), s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_indx(char *, char *, 
	    ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    static integer tbeg, tend, tlen;
    static logical more;
    static char last[8];
    static integer size, tmpj, most;
    extern integer upto_(char *, char *, integer *, ftnlen, ftnlen);
    static integer a, b, e, i__, j, k;
    extern integer cardc_(char *, ftnlen);
    static integer bcode;
    static char label[32];
    extern integer cardi_(integer *);
    extern logical match_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sizec_(char *, ftnlen);
    extern /* Subroutine */ int copyc_(char *, char *, ftnlen, ftnlen);
    extern integer ncpos_(char *, char *, integer *, ftnlen, ftnlen);
    static char terms[32*70];
    static logical group;
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int m2begr_(char *, integer *, integer *, integer 
	    *, integer *, ftnlen);
    static integer t1code;
    extern /* Subroutine */ int m2keep_(void), m2mark_(char *, integer *, 
	    integer *, char *, ftnlen, ftnlen), m2mtch_(char *, integer *, 
	    char *, char *, integer *, logical *, integer *, integer *, 
	    integer *, char *, ftnlen, ftnlen, ftnlen, ftnlen), m2pclr_(void),
	     m2tclr_(void), m2term_(char *, char *, integer *, ftnlen, ftnlen)
	    , m2thnq_(char *, integer *, char *, ftnlen, ftnlen), m2trim_(
	    char *, char *, ftnlen, ftnlen);
    extern logical m2keyw_(char *, ftnlen);
    static integer bo, eo, bs, es, bw, begofg, bdiags, ew, ediags;
    static logical rediag;
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen);
    static integer endofg, gmatch;
    extern /* Subroutine */ int remlac_(integer *, integer *, char *, integer 
	    *, ftnlen), scardi_(integer *, integer *);
    static integer afterg;
    extern /* Subroutine */ int remlai_(integer *, integer *, integer *, 
	    integer *);
    extern integer bsrchi_(integer *, integer *, integer *);
    static integer bscore;
    extern /* Subroutine */ int fndnwd_(char *, integer *, integer *, integer 
	    *, ftnlen), fndptk_(char *, char *, integer *, integer *, integer 
	    *, ftnlen, ftnlen);
    static logical simple;
    static integer indxes[70];
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    static integer fewest;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    static integer blstwd, clstwd;
    static logical optdir;
    static integer elstwd;
    extern integer qlstnb_(char *, ftnlen);
    static integer tscore;
    extern /* Subroutine */ int ssizec_(integer *, char *, ftnlen);
    static char keywds[32*70];
    static logical vtempl;
    static char subtmp[1024];
    extern integer lstlti_(integer *, integer *, integer *);
    static integer positn;
    extern /* Subroutine */ int ssizei_(integer *, integer *), cmprss_(char *,
	     integer *, char *, char *, ftnlen, ftnlen, ftnlen), suffix_(char 
	    *, integer *, char *, ftnlen, ftnlen);
    static char swords[32*70];
    extern /* Subroutine */ int prefix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    static integer loc;
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);

/* $ Abstract */

/*      This routine will match a META/2 template that contains no */
/*      qualified @then directives. */

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
/*     STRING */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TEMP       I   A META/2 template */
/*     THNWDS     I   A cell of initial keywords for a following @then */
/*     STRING     I   A candidate META/2 sentence. */
/*     SBEG       I   Where to start processing this sentence */
/*     REASON     I   Flag indicating diagnostics should be produced. */
/*     CUTOFF     I   Threshold used for spelling error diagnostics. */
/*     PSSTHN     O   Flag to indicate we made it past a @then */
/*     M2CODE     O   META/2 code indicating how a match failed. */
/*     SCORE      O   a measure of how well STRING matched TEMP. */
/*     CAUSE      O   Diagnostic message if requested for non-matches. */

/* $ Detailed_Input */

/*     TEMP       is a META/2 template to be compared with a portion */
/*                of the candidate input sentence. */

/*     THNWDS     is a cell containing KEYWORDS that may be used as */
/*                terminators for the entire template.  Typically */
/*                this cell will contain the initial keywords of */
/*                a class of templates that can be branched to from */
/*                this template. */

/*     STRING     A string, a substring of which ( STRING(SBEG:) ) */
/*                should be compared with the input META/2 template. */

/*     SBEG       is the beginning of the substring that should be */
/*                compared with TEMP. */

/*     REASON     Is a logical flag, that should be set to .TRUE. */
/*                if the user wishes to have error mismatch diagnostics */
/*                to be returned by this routine. */

/*     CUTOFF     is a parameter used to determine how close words */
/*                of STRING must match up with keywords in TEMP */
/*                in order to be diagnosed as spelling errors. */
/*                Ranges from 0 to 100 are acceptable.  A "good" range */
/*                of values is from from 65 to 75. */

/* $ Detailed_Output */

/*     SBEG       if the match is successful, SBEG will be set to the */
/*                first word of the input string that follows the */
/*                matched substring. ( Note that words in THNWDS do */
/*                not qualify as part of the template, but merely */
/*                serve to delimit the ends of variable length */
/*                templates. Thus if one of these words was actually */
/*                used to delimit the end of TEMP, SBEG will point to */
/*                the beginning of that word in STRING.) */

/*     PSSTHN     is set only if the template matches up to an */
/*                an unqualified @then directive.  In such a case */
/*                PSSTHN will be set to .TRUE.  Otherwise it will not */
/*                be changed from its input value. */

/*     M2CODE     is an integer META/2 code that indicates how an attempt */
/*                to match the input failed.  If the match was successful */
/*                M2CODE will be returned as zero.  Otherwise it will */
/*                be returned with a positive value.  Possible values */
/*                and meanings are: */



/*     SCORE      is a measure of how well STRING matched TEMP.  This */
/*                is useful primarily when looking through several */
/*                templates, none of which yield an M2CODE of zero.  In */
/*                this case, the template with the highest SCORE is */
/*                likely to be the template the input string was */
/*                "intended" to match. */

/*     CAUSE      If REASON is set to .TRUE. and the match fails */
/*                (M2CODE .NE. 0 ), this string will contain a */
/*                description of the suspected cause of the match */
/*                failure.  Moreover, the input string will be "marked" */
/*                at the location of the match failure. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1)  If the number of delimiting keywords is greater than 64 a */
/*         SPICE TOOLKIT error will be SIGERRled. */

/*         SPICE(TOOMANYKEYWORDS) */

/*         Delimiting keywords are: */

/*         a) Keywords that immediately follow group templates. */
/*         b) Keywords that are the initial keywords of a simple template */
/*            contained within a group template. */
/*         c) The keywords passed to the routine in the cell THNWDS. */

/* $ Particulars */

/*     This routine allows one to compare strings with those META/2 */
/*     language templates that do not end with a qualified-'@then'. */
/*     Moreover, it serves as the principle tool for matching the */
/*     various pieces of full META/2 templates.  If a match occurs */
/*     the remainder of the string can be compared with the templates */
/*     pointed to by the @then  directive. */


/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     The maximum number of delimiting keywords is 64. */

/*     No checks are made to see if the template supplied is in fact */
/*     a valid META/2 template. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -     META/2 Configured Version 2.1.0, 08-NOV-2005 (BVS) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in FNDNWD calls. */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         Added an extra blank after a carriage return "/cr" */
/*         substring in */

/*           DIAGNS = 'I was trying to match part of the input '  ... */
/*           //       'string with one of the expresions listed ' ... */
/*           //       'here: /cr/cr '                             ... */
/*           //        SUBTMP(1:RTRIM(SUBTMP))                    ... */
/*           //       './cr/cr The expression that came closest ' ... */
/*           //       'was: /cr/cr, ' */

/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/*     Beta Version 1.0.0, 19-MAY-1988 (WLT) (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     If this is the first pass through this routine, set the size of */
/*     the cells TERMS and INDXES */

    if (pass1) {
	pass1 = FALSE_;
	ssizec_(&c__64, terms, (ftnlen)32);
	ssizei_(&c__64, indxes);
	ssizec_(&c__64, keywds, (ftnlen)32);
	ssizec_(&c__64, swords, (ftnlen)32);
    }

/*     Clear out the parse table. */

    m2pclr_();

/*     Collect the list of potential terminating keywords. */

    m2term_(temp, terms, indxes, temp_len, (ftnlen)32);

/*     Append all of the '@then(*)'-keywords to the list of terminators. */

    if (cardc_(terms, (ftnlen)32) + cardc_(thnwds, thnwds_len) >= sizec_(
	    terms, (ftnlen)32) - 2) {
	chkin_("M2GMCH", (ftnlen)6);
	sigerr_("SPICE(TOOMANYKEYWORDS)", (ftnlen)22);
	chkout_("M2GMCH", (ftnlen)6);
	return 0;
    }
    tlen = i_len(temp, temp_len) + 1;
    j = cardc_(terms, (ftnlen)32) + 1;
    i__1 = cardc_(thnwds, thnwds_len);
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_copy(terms + (((i__2 = j + 5) < 70 && 0 <= i__2 ? i__2 : s_rnge(
		"terms", i__2, "m2gmch_", (ftnlen)364)) << 5), thnwds + (i__ 
		+ 5) * thnwds_len, (ftnlen)32, thnwds_len);
	indxes[(i__2 = j + 5) < 70 && 0 <= i__2 ? i__2 : s_rnge("indxes", 
		i__2, "m2gmch_", (ftnlen)365)] = tlen;
	++j;
    }

/*     Append a '@end' and a '}' to the end of the terminators, and */
/*     adjust the cardinality of the TERMS cell */

    s_copy(terms + (((i__1 = j + 5) < 70 && 0 <= i__1 ? i__1 : s_rnge("terms",
	     i__1, "m2gmch_", (ftnlen)373)) << 5), "@end", (ftnlen)32, (
	    ftnlen)4);
    indxes[(i__1 = j + 5) < 70 && 0 <= i__1 ? i__1 : s_rnge("indxes", i__1, 
	    "m2gmch_", (ftnlen)374)] = tlen;
    ++j;
    s_copy(terms + (((i__1 = j + 5) < 70 && 0 <= i__1 ? i__1 : s_rnge("terms",
	     i__1, "m2gmch_", (ftnlen)377)) << 5), "}", (ftnlen)32, (ftnlen)1)
	    ;
    indxes[(i__1 = j + 5) < 70 && 0 <= i__1 ? i__1 : s_rnge("indxes", i__1, 
	    "m2gmch_", (ftnlen)378)] = tlen;
    size = cardc_(terms, (ftnlen)32) + cardc_(thnwds, thnwds_len) + 2;
    scardc_(&size, terms, (ftnlen)32);
    scardi_(&size, indxes);

/*     This routine will only use the portion of the template up */
/*     to a qualified @then. */

    m2thnq_(temp, &positn, label, temp_len, (ftnlen)32);
    if (positn <= i_len(temp, temp_len)) {
	s_copy(temp + (positn - 1), " ", temp_len - (positn - 1), (ftnlen)1);
    }

/*     Now initialize pointers and the loop control variable MORE so */
/*     that we can start the loop. */

    tbeg = 1;
    tend = 1;
    more = TRUE_;
    while(more) {

/*     As long as we are not told to exit */

/*        Look at the next word, */

	fndnwd_(temp, &tbeg, &bw, &ew, temp_len);
	if (bw == 0) {

/*           There wasn't a next word.  There is nothing left to do. */
/*           We set MORE to .FALSE. so that we can exit the loop. */

	    more = FALSE_;
	    group = FALSE_;
	    simple = FALSE_;
	} else if (s_cmp(temp + (bw - 1), "@then", ew - (bw - 1), (ftnlen)5) 
		== 0) {

/*           We have an unqualified @then directive.  This means that */
/*           we are on the right track as far as determining what */
/*           command we are working on.  Set the PASSED-A-@then flag */
/*           (PSSTHN) to .TRUE. and the other candidates to .FALSE. */

	    *pssthn = TRUE_;
	    group = FALSE_;
	    simple = FALSE_;
	    tbeg = ew + 1;
	    tend = tbeg;
	} else if (match_(temp + (bw - 1), "(%*:%*){", ew - (bw - 1), (ftnlen)
		8)) {

/*           We are about to enter a group template.  Determine */
/*           the FEWEST number of simple templates in the group */
/*           that must match and the MOST that we will check. */

	    m2begr_(temp, &bw, &ew, &fewest, &most, temp_len);
	    group = TRUE_;
	    simple = FALSE_;
	    s_copy(last, "GROUP", (ftnlen)8, (ftnlen)5);

/*           Set up the pointers for looking for the simple */
/*           templates within this group. */

	    tbeg = ew + 1;
	    tend = tbeg;
	} else {

/*           The only possible candidate is a simple template. */

	    group = FALSE_;
	    simple = TRUE_;
	    tbeg = bw;
	    s_copy(last, "SIMPLE", (ftnlen)8, (ftnlen)6);
	}
	if (group) {

/*           Set up the initial values for this group.  We need */

/*           1)  The number of simple template matches so far for */
/*               this group. */

	    gmatch = 0;

/*           2)  A best score of the simple templates checked so far. */

	    bscore = -1;

/*           3)  A temporary place to store the M2CODE returned for */
/*               a simple template of this group. */

	    t1code = 0;

/*           4)  The position in the full template to jump to when we are */
/*               done with this template, the beginning and the end of */
/*               the group */

	    begofg = tbeg;
	    endofg = upto_(temp, " }", &tbeg, temp_len, (ftnlen)2);
	    afterg = endofg + 3;

/*           Make sure there is a viable simple template within this */
/*           group. */

	    bs = ncpos_(temp, " ", &begofg, temp_len, (ftnlen)1);
/* Computing MIN */
	    i__1 = upto_(temp, " | ", &begofg, temp_len, (ftnlen)3), i__2 = 
		    upto_(temp, " } ", &begofg, temp_len, (ftnlen)3);
	    es = min(i__1,i__2);
	    if (bs == 0 || bs >= es || *(unsigned char *)&temp[bs - 1] == '}' 
		    || es > endofg) {
		group = FALSE_;
	    }

/*           Finally, if FEWEST is 1 or 0, */
/*           remove the '}' that has index equal to the index */
/*           of the '}' that is the terminator of this group. */

	    if (fewest <= 1) {
		i__1 = endofg + 2;
		i__2 = cardi_(indxes);
		loc = bsrchi_(&i__1, &i__2, &indxes[6]);
		if (loc != 0) {
		    i__1 = cardi_(indxes);
		    remlai_(&c__1, &loc, &indxes[6], &i__1);
		    i__1 = cardc_(terms, (ftnlen)32);
		    remlac_(&c__1, &loc, terms + 192, &i__1, (ftnlen)32);
		    i__1 = cardc_(terms, (ftnlen)32) - 1;
		    scardc_(&i__1, terms, (ftnlen)32);
		    i__1 = cardi_(indxes) - 1;
		    scardi_(&i__1, indxes);
		}
	    }
	    while(group) {

/*              We've got a viable simple template for this group. */

/*              If it ends with a variable template find out what the */
/*              possible terminating words are. */

		a = 0;
		b = 0;
		i__1 = es + 1;
		fndptk_(temp, " ", &i__1, &blstwd, &elstwd, temp_len, (ftnlen)
			1);
		m2begr_(temp, &blstwd, &elstwd, &a, &b, temp_len);
/* Computing MIN */
		i__1 = elstwd, i__2 = blstwd + 8;
		clstwd = min(i__1,i__2);
		vtempl = a != b || ! m2keyw_(temp + (blstwd - 1), elstwd - (
			blstwd - 1)) && s_cmp("@calendar", temp + (blstwd - 1)
			, (ftnlen)9, clstwd - (blstwd - 1)) == 0;
		if (vtempl) {

/*                 There is a variable length template, the keywords */
/*                 that might terminate this template are given */
/*                 in TERMS up to the first occurance of a '}'. */

		    if (gmatch < most - 1) {
			i__1 = cardi_(indxes);
			i__ = lstlti_(&begofg, &i__1, &indxes[6]) + 1;
		    } else {
			i__1 = cardi_(indxes);
			i__ = lstlti_(&afterg, &i__1, &indxes[6]) + 1;
		    }
		    j = 0;
		    while(s_cmp(terms + (((i__1 = i__ + 5) < 70 && 0 <= i__1 ?
			     i__1 : s_rnge("terms", i__1, "m2gmch_", (ftnlen)
			    566)) << 5), "}", (ftnlen)32, (ftnlen)1) != 0) {

/*                    Keep only those keywords that are not the initial */
/*                    keyword of this template. */

			if (indxes[(i__1 = i__ + 5) < 70 && 0 <= i__1 ? i__1 :
				 s_rnge("indxes", i__1, "m2gmch_", (ftnlen)
				573)] != bs) {
			    ++j;
			    m2trim_(terms + (((i__1 = i__ + 5) < 70 && 0 <= 
				    i__1 ? i__1 : s_rnge("terms", i__1, "m2g"
				    "mch_", (ftnlen)575)) << 5), keywds + (((
				    i__2 = j + 5) < 70 && 0 <= i__2 ? i__2 : 
				    s_rnge("keywds", i__2, "m2gmch_", (ftnlen)
				    575)) << 5), (ftnlen)32, (ftnlen)32);
			}
			++i__;
		    }
		    scardc_(&j, keywds, (ftnlen)32);
		} else {
		    scardc_(&c__0, keywds, (ftnlen)32);
		}

/*              Check the current template with M2MTCH. */

		if (s_cmp(temp + (bs - 1), "@options", es - (bs - 1), (ftnlen)
			8) == 0) {
		    t1code = -1;
		    tscore = -1;
		} else {

/*                 Dump the temporary parse table. */

		    m2tclr_();
		    m2mtch_(temp + (bs - 1), &c__1, keywds, string, sbeg, &
			    c_false, cutoff, &t1code, &tscore, cause, es - (
			    bs - 1), (ftnlen)32, string_len, cause_len);
		}

/*              If the attempt at a match succeeded ... */

		if (t1code == 0) {

/*                 Increment the number of group matches by 1. */
/*                 Increment the score for this template. */
/*                 Set the best score obtained thus far to zero */
/*                 in preparation for the next pass through the */
/*                 group. */

		    ++gmatch;
		    *score += tscore;
		    bscore = -1;

/*                 Move the temporary parse table to the keepers */
/*                 parse table. */

		    m2keep_();

/*                 The current template should be taken off the viable */
/*                 list. */

		    if (es < endofg) {
			s_copy(temp + (bs - 1), " ", es + 2 - (bs - 1), (
				ftnlen)1);
		    } else {
			fndptk_(temp, " ", &bs, &a, &b, temp_len, (ftnlen)1);
			if (s_cmp(temp + (a - 1), "|", b - (a - 1), (ftnlen)1)
				 == 0) {
			    s_copy(temp + (a - 1), " ", es - (a - 1), (ftnlen)
				    1);
			} else {
			    s_copy(temp + (bs - 1), " ", es - (bs - 1), (
				    ftnlen)1);
			}
		    }

/*                 Reset ES to be the one before the beginning of */
/*                 the group template (BS will be set to ES + 1 */
/*                 at the end of the group loop). */

		    es = begofg - 1;

/*                 Adjust the possible terminating keyword set. */
/*                 (remove the initial keyword of the simple template */
/*                 just matched from the collection). */

		    i__1 = cardi_(indxes);
		    loc = bsrchi_(&bs, &i__1, &indxes[6]);
		    i__1 = cardi_(indxes);
		    remlai_(&c__1, &loc, &indxes[6], &i__1);
		    i__1 = cardc_(terms, (ftnlen)32);
		    remlac_(&c__1, &loc, terms + 192, &i__1, (ftnlen)32);
		    i__1 = cardc_(terms, (ftnlen)32) - 1;
		    scardc_(&i__1, terms, (ftnlen)32);
		    i__1 = cardi_(indxes) - 1;
		    scardi_(&i__1, indxes);

/*                 Finally, if we have now exactly matched FEWEST-1, */
/*                 remove the '}' that has index equal to the index */
/*                 of the '}' that is the terminator of this group. */

		    if (gmatch == fewest - 1) {
			i__1 = endofg + 2;
			i__2 = cardi_(indxes);
			loc = bsrchi_(&i__1, &i__2, &indxes[6]);
			if (loc != 0) {
			    i__1 = cardi_(indxes);
			    remlai_(&c__1, &loc, &indxes[6], &i__1);
			    i__1 = cardc_(terms, (ftnlen)32);
			    remlac_(&c__1, &loc, terms + 192, &i__1, (ftnlen)
				    32);
			    i__1 = cardc_(terms, (ftnlen)32) - 1;
			    scardc_(&i__1, terms, (ftnlen)32);
			    i__1 = cardi_(indxes) - 1;
			    scardi_(&i__1, indxes);
			}
		    }
		} else {

/*                 Record the score if this is higher than a previous */
/*                 value. */

		    if (tscore > bscore) {
			bscore = tscore;
			bdiags = bs;
			ediags = es;
			bcode = t1code;
			copyc_(keywds, swords, (ftnlen)32, (ftnlen)32);
		    }
		}

/*              Remove all introductory '@options' directives. */

		optdir = TRUE_;
		while(optdir) {
		    bo = ncpos_(temp, " ", &begofg, temp_len, (ftnlen)1);
/* Computing MIN */
		    i__1 = upto_(temp, " | ", &begofg, temp_len, (ftnlen)3), 
			    i__2 = upto_(temp, " } ", &begofg, temp_len, (
			    ftnlen)3);
		    eo = min(i__1,i__2);
		    if (bo < eo) {
			optdir = s_cmp(temp + (bo - 1), "@options", eo - (bo 
				- 1), (ftnlen)8) == 0;
			if (optdir) {
			    s_copy(temp + (bo - 1), " ", eo - (bo - 1), (
				    ftnlen)1);
			    eo += 2;
			    if (*(unsigned char *)&temp[eo - 1] == '|') {
				*(unsigned char *)&temp[eo - 1] = ' ';
			    }
			}
		    } else {
			optdir = FALSE_;
		    }
		}

/*              Should we stay in this group? Only if you can answer yes */
/*              to  all of the following: */

/*                  1.) Are more matches allowed for this group. */

/*                  2.) Is there another template in this group that */
/*                      hasn't been checked. */

		if (gmatch >= most) {
		    group = FALSE_;
		} else {

/*                 Make sure there is a viable simple template within */
/*                 this group. */

		    i__1 = es + 1;
		    bs = ncpos_(temp, " |", &i__1, temp_len, (ftnlen)2);
/* Computing MIN */
		    i__1 = upto_(temp, " | ", &bs, temp_len, (ftnlen)3), i__2 
			    = upto_(temp, " } ", &bs, temp_len, (ftnlen)3);
		    es = min(i__1,i__2);
		    if (bs == 0 || bs >= es || *(unsigned char *)&temp[bs - 1]
			     == '}' || es > endofg) {
			group = FALSE_;
		    }
		}
	    }

/*           When we leave the group, see if we had a sufficient number */
/*           of matches.  If we did, jump past the end of the group. */
/*           If we didn't, this is an error---head for home. */

	    optdir = i_indx(temp + (begofg - 1), " @options ", endofg - (
		    begofg - 1), (ftnlen)10) != 0;
	    if (! optdir && gmatch >= fewest) {
		tbeg = afterg;
	    } else if (optdir && gmatch >= most) {
		if (*reason) {
		    cmprss_(" ", &c__1, temp + (begofg - 1), temp + (begofg - 
			    1), (ftnlen)1, endofg - (begofg - 1), endofg - (
			    begofg - 1));
		    b = begofg - 1;
		    e = i_indx(temp + (begofg - 1), " @options ", endofg - (
			    begofg - 1), (ftnlen)10) + 1;
		    s_copy(cause, "I had already matched the maximum number "
			    "of allowed simple templates in a group without m"
			    "atching the  following REQUIRED templates./cr/cr"
			    "(3:3)", cause_len, (ftnlen)142);
		    suffix_(temp + (b - 1), &c__1, cause, e - (b - 1), 
			    cause_len);
		    suffix_("} /cr/cr(-3:-3)", &c__1, cause, (ftnlen)15, 
			    cause_len);
		    *m2code = 11000;
		    more = FALSE_;
		}
	    } else if (optdir && gmatch >= fewest) {
		*score += bscore;

/*              If diagnostics are requested then see what went wrong */
/*              with the best fitting simple template. */

		if (*reason) {
		    bs = bdiags;
		    es = ediags;
		    m2mtch_(temp + (bs - 1), &c__1, swords, string, sbeg, 
			    reason, cutoff, &t1code, &tscore, cause, es - (bs 
			    - 1), (ftnlen)32, string_len, cause_len);
		    s_copy(cause + cause_len, cause, cause_len, cause_len);
		    b = begofg - 1;
		    e = endofg + 2;
		    cmprss_(" ", &c__1, temp + (b - 1), subtmp, (ftnlen)1, e 
			    - (b - 1), (ftnlen)1024);
		    if (i_indx(subtmp, " | ", (ftnlen)1024, (ftnlen)3) == 
			    i_indx(subtmp, " @options ", (ftnlen)1024, (
			    ftnlen)10) - 2) {
			prefix_("/cr/cr(-3:)", &c__1, cause + cause_len, (
				ftnlen)11, cause_len);
			prefix_(subtmp, &c__1, cause + cause_len, (ftnlen)
				1024, cause_len);
			prefix_("Although I had matched a required number of"
				" expressions in the group below, I had not y"
				"et matched the explicitely required expressi"
				"on that appears prior to the META/2 \"@optio"
				"ns\" directive in the group shown here./cr(3"
				":) ", &c__1, cause + cause_len, (ftnlen)220, 
				cause_len);
			i__1 = i_indx(subtmp, " @options ", (ftnlen)1024, (
				ftnlen)10);
			k = pos_(subtmp, " | ", &i__1, (ftnlen)1024, (ftnlen)
				3);
			if (k > 0 && ncpos_(string, " ", sbeg, string_len, (
				ftnlen)1) != 0) {
			    suffix_("/cr/cr Of the remaining simple template"
				    "s (including the optional ones) the one "
				    "that comes closest to matching is: /cr/c"
				    "r(3:) ", &c__1, cause + cause_len, (
				    ftnlen)125, cause_len);
			    suffix_(temp + (bdiags - 1), &c__1, cause + 
				    cause_len, ediags - (bdiags - 1), 
				    cause_len);
			    suffix_("/cr/cr(-3:)", &c__0, cause + cause_len, (
				    ftnlen)11, cause_len);
			}
		    } else {
			prefix_("/cr/cr(-3:)", &c__1, cause + cause_len, (
				ftnlen)11, cause_len);
			prefix_(subtmp, &c__0, cause + cause_len, (ftnlen)
				1024, cause_len);
			prefix_("Although I had matched a required number of"
				" expressions in the group below, I had not y"
				"et matched the explicitely required expressi"
				"ons that appear prior to the META/2 \"@optio"
				"ns\" directive in the group shown here. ./cr"
				"/cr(3:) ", &c__1, cause + cause_len, (ftnlen)
				225, cause_len);
			if (ncpos_(string, " ", sbeg, string_len, (ftnlen)1) 
				!= 0) {
			    suffix_("/cr/crOf the remaining simple templates"
				    ", the one that comes closest to matching"
				    " is: /cr/cr(3:) ", &c__1, cause + 
				    cause_len, (ftnlen)95, cause_len);
			    suffix_(temp + (bdiags - 1), &c__1, cause + 
				    cause_len, ediags - (bdiags - 1), 
				    cause_len);
			    suffix_("/cr/cr(-3:)", &c__0, cause + cause_len, (
				    ftnlen)11, cause_len);
			}
		    }
		}
		*m2code = bcode;
		more = FALSE_;
	    } else if (gmatch < fewest) {
		*score += bscore;

/*              If diagnostics are requested then see what went wrong */
/*              with the best fitting simple template. */

		if (*reason) {
		    bs = bdiags;
		    es = ediags;
		    m2mtch_(temp + (bs - 1), &c__1, swords, string, sbeg, 
			    reason, cutoff, &t1code, &tscore, cause, es - (bs 
			    - 1), (ftnlen)32, string_len, cause_len);
		    s_copy(cause + cause_len, cause, cause_len, cause_len);
		    b = begofg - 1;
		    e = endofg + 2;
		    cmprss_(" ", &c__1, temp + (b - 1), subtmp, (ftnlen)1, e 
			    - (b - 1), (ftnlen)1024);
		    if (i_indx(subtmp, " | ", (ftnlen)1024, (ftnlen)3) != 0) {
			prefix_("'./cr/cr(-3:)", &c__1, cause + cause_len, (
				ftnlen)13, cause_len);
			prefix_(temp + (bdiags - 1), &c__0, cause + cause_len,
				 ediags - (bdiags - 1), cause_len);
/* Writing concatenation */
			i__3[0] = 98, a__1[0] = "I was trying to match part "
				"of the input string with one of the expressi"
				"ons listed here:/cr/cr(3:) ";
			i__3[1] = rtrim_(subtmp, (ftnlen)1024), a__1[1] = 
				subtmp;
			i__3[2] = 28, a__1[2] = "./cr/cr(-3:) The expression "
				;
			i__3[3] = 10, a__1[3] = "that came ";
			i__3[4] = 24, a__1[4] = "closest was: /cr/cr(3:)'";
			s_cat(ch__1, a__1, i__3, &c__5, (ftnlen)1184);
			prefix_(ch__1, &c__0, cause + cause_len, rtrim_(
				subtmp, (ftnlen)1024) + 160, cause_len);
		    } else {
			prefix_("'./cr/cr(-3:)", &c__1, cause + cause_len, (
				ftnlen)13, cause_len);
			prefix_(temp + (bdiags - 1), &c__0, cause + cause_len,
				 ediags - (bdiags - 1), cause_len);
			prefix_("I was trying to match part of the input str"
				"ing with the expression: /cr/cr(3:) '", &c__0,
				 cause + cause_len, (ftnlen)80, cause_len);
		    }
		}
		*m2code = bcode;
		more = FALSE_;
	    }

	} else if (simple) {
/* Computing MIN */
	    i__1 = upto_(temp, " @then", &tbeg, temp_len, (ftnlen)6), i__2 = 
		    upto_(temp, "){ ", &tbeg, temp_len, (ftnlen)3);
	    tend = min(i__1,i__2) + 1;
	    fndptk_(temp, " ", &tend, &blstwd, &elstwd, temp_len, (ftnlen)1);
	    tend = elstwd;

/*           See if the simple template ends with a variable template. */
/*           If it does, find out what the possible terminating words */
/*           are. */

	    a = 0;
	    b = 0;
	    m2begr_(temp, &blstwd, &elstwd, &a, &b, temp_len);
/* Computing MIN */
	    i__1 = elstwd, i__2 = blstwd + 8;
	    clstwd = min(i__1,i__2);
	    vtempl = a != b || ! m2keyw_(temp + (blstwd - 1), elstwd - (
		    blstwd - 1)) && s_cmp("@calendar", temp + (blstwd - 1), (
		    ftnlen)9, clstwd - (blstwd - 1)) == 0;
	    if (vtempl) {

/*              There is a variable length template, the keywords */
/*              that might terminate this template are given */
/*              in TERMS up to the first occurance of a '}'. */

		i__1 = cardi_(indxes);
		i__ = lstlti_(&elstwd, &i__1, &indxes[6]) + 1;
		j = 0;

/*              Just load keywords onto the list until we hit a '}' */
/*              (We are guarenteed that this will happen, because */
/*              we put a '}' on the end of the list at the beginning */
/*              of this routine.) */

		while(s_cmp(terms + (((i__1 = i__ + 5) < 70 && 0 <= i__1 ? 
			i__1 : s_rnge("terms", i__1, "m2gmch_", (ftnlen)993)) 
			<< 5), "}", (ftnlen)32, (ftnlen)1) != 0) {
		    ++j;
		    m2trim_(terms + (((i__1 = i__ + 5) < 70 && 0 <= i__1 ? 
			    i__1 : s_rnge("terms", i__1, "m2gmch_", (ftnlen)
			    996)) << 5), keywds + (((i__2 = j + 5) < 70 && 0 
			    <= i__2 ? i__2 : s_rnge("keywds", i__2, "m2gmch_",
			     (ftnlen)996)) << 5), (ftnlen)32, (ftnlen)32);
		    ++i__;
		}
		scardc_(&j, keywds, (ftnlen)32);
	    } else {
		scardc_(&c__0, keywds, (ftnlen)32);
	    }

/*           Check the current template with M2MTCH. */

	    m2tclr_();
	    m2mtch_(temp + (tbeg - 1), &c__1, keywds, string, sbeg, &c_false, 
		    cutoff, &t1code, &tscore, cause, tend - (tbeg - 1), (
		    ftnlen)32, string_len, cause_len);

/*           If the attempt at a match succeeded ... */

	    if (t1code == 0) {
		*score += tscore;
		tbeg = tend + 1;
		m2keep_();
	    } else {
		m2mtch_(temp + (tbeg - 1), &c__1, keywds, string, sbeg, 
			reason, cutoff, &t1code, &tscore, cause, tend - (tbeg 
			- 1), (ftnlen)32, string_len, cause_len);
		*score += tscore;
		*m2code = t1code;
		more = FALSE_;
	    }
	}
    }

/*     If there were no THNWDS and there is stuff left in the string and */
/*     we haven't already noticed, we've got an error dude. */

    if (cardc_(thnwds, thnwds_len) == 0 && *sbeg < qlstnb_(string, string_len)
	     && *m2code == 0) {

/*        Until we have evidence to justify looking for probable causes */
/*        of the current overage of input string, we assume that we */
/*        are not interested in offering conjectures about what the */
/*        problem is.  We'll just say there is extra stuff. */

	rediag = FALSE_;

/*        Now look for justification of fancier diagnostics. */

/*        Was the last thing we attempted to match part of a group */
/*        template? */

	if (s_cmp(last, "GROUP", (ftnlen)8, (ftnlen)5) == 0 && gmatch < most) 
		{

/*           We are going to see if one of the options of an ending group */
/*           template looks like it was the intention of the user. */

	    if (bcode < 100) {

/*              We had a probable spelling error, set the flag to */
/*              diagnose the problem. */

		rediag = TRUE_;
	    } else {

/*              Look at what the score could have been for the */
/*              simple template that was the closest match. */

		i__ = 1;
		j = bdiags;
		tscore = 0;
		fndnwd_(temp, &j, &i__, &tmpj, temp_len);
		j = tmpj;
		while(i__ != 0 && i__ < ediags) {
		    a = 1;
		    b = 1;
		    m2begr_(temp, &i__, &j, &a, &b, temp_len);
		    if (m2keyw_(temp + (i__ - 1), j - (i__ - 1))) {
			tscore += 100;
		    } else {
			tscore += a * 15;
		    }
		    fndnwd_(temp, &j, &i__, &tmpj, temp_len);
		    j = tmpj;
		}

/*              If the score actually recorded made it at least a quarter */
/*              of the way, we will guess that this may have been the */
/*              root of the problem. */

/* Computing MAX */
		i__1 = *cutoff, i__2 = tscore / 4;
		rediag = bscore > max(i__1,i__2);
	    }
	}

/*        If there was sufficient grounds to warrant second guessing, */
/*        run the best guess template through M2MTCH to get a diagnostic. */

	if (rediag) {
	    if (*reason) {
		bs = bdiags;
		es = ediags;
		m2mtch_(temp + (bs - 1), &c__1, keywds, string, sbeg, reason, 
			cutoff, &t1code, &tscore, cause, es - (bs - 1), (
			ftnlen)32, string_len, cause_len);
		s_copy(cause + cause_len, cause, cause_len, cause_len);
		b = begofg - 1;
		e = endofg + 2;
		cmprss_(" ", &c__1, temp + (b - 1), subtmp, (ftnlen)1, e - (b 
			- 1), (ftnlen)1024);
		if (i_indx(subtmp, " | ", (ftnlen)1024, (ftnlen)3) != 0) {
		    prefix_("'./cr/cr(-3:)", &c__1, cause + cause_len, (
			    ftnlen)13, cause_len);
		    prefix_(temp + (bdiags - 1), &c__0, cause + cause_len, 
			    ediags - (bdiags - 1), cause_len);
/* Writing concatenation */
		    i__3[0] = 178, a__1[0] = "Extra words appear in the inpu"
			    "t string that are not part of a valid expression"
			    ". I think you may have been trying to supply one"
			    " of the optional expressions listed here:/cr/cr("
			    "3:) ";
		    i__3[1] = rtrim_(subtmp, (ftnlen)1024), a__1[1] = subtmp;
		    i__3[2] = 13, a__1[2] = "/cr/cr(-3:). ";
		    i__3[3] = 25, a__1[3] = "The expression that came ";
		    i__3[4] = 25, a__1[4] = "closest was: /cr/cr(3:) '";
		    s_cat(ch__2, a__1, i__3, &c__5, (ftnlen)1265);
		    prefix_(ch__2, &c__0, cause + cause_len, rtrim_(subtmp, (
			    ftnlen)1024) + 241, cause_len);
		} else {
		    prefix_("'./cr/cr(-3:)", &c__1, cause + cause_len, (
			    ftnlen)13, cause_len);
		    prefix_(temp + (bdiags - 1), &c__0, cause + cause_len, 
			    ediags - (bdiags - 1), cause_len);
		    prefix_("Extra words appear in the input string that are"
			    " not part of a valid expression. I think you may"
			    " have been trying to supply the optional express"
			    "ion:/cr/cr(3:)'", &c__0, cause + cause_len, (
			    ftnlen)158, cause_len);
		}
	    }

/*           Whatever error code we got back, add 10000 so that this */
/*           routine will have its stamp on it to indicate we are second */
/*           level guessing at what went wrong. */

	    *m2code = t1code + 10000;
	} else {

/*           Sorry, we couldn't guess why there was extra stuff in the */
/*           command.  Maybe just happy fingers.  Anyway, just say there */
/*           was extra stuff and hit the road. */

	    if (*reason) {
		s_copy(cause, "The input string contains extra words that ar"
			"e not recognized as part of a valid command.", 
			cause_len, (ftnlen)89);
		i__1 = qlstnb_(string, string_len);
		m2mark_(string, sbeg, &i__1, cause, string_len, cause_len);
	    }
	    *m2code = 10200;
	}
    }
    return 0;
} /* m2gmch_ */

