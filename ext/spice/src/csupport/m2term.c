/* m2term.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* $Procedure      M2TERM (Find possible terminators of variable template) */
/* Subroutine */ int m2term_(char *temp, char *terms, integer *indxes, ftnlen 
	temp_len, ftnlen terms_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer mark;
    static logical more;
    static integer room, next;
    extern integer posr_(char *, char *, integer *, ftnlen, ftnlen), upto_(
	    char *, char *, integer *, ftnlen, ftnlen);
    static integer b, e;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sizec_(char *, ftnlen), ncpos_(char *, char *, integer *, 
	    ftnlen, ftnlen), sizei_(integer *);
    static integer nextg, d1, d2, count;
    static logical group;
    static integer nextt;
    extern /* Subroutine */ int m2begr_(char *, integer *, integer *, integer 
	    *, integer *, ftnlen);
    extern logical m2keyw_(char *, ftnlen);
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen), scardi_(
	    integer *, integer *);
    static logical mrkend, dothen;
    extern /* Subroutine */ int fndnwd_(char *, integer *, integer *, integer 
	    *, ftnlen);
    extern logical matchw_(char *, char *, char *, char *, ftnlen, ftnlen, 
	    ftnlen, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    static integer beg;
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);

/* $ Abstract */

/*     Find those keywords that are initial keywords of group templates */
/*     or immediately follow such a template. */

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

/*     The META/2 Book. */

/* $ Keywords */

/*     PARSING */
/*     SEARCH */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TEMP       I   A META/2 template. */
/*     TERMS      O   Possible terminating keywords. */
/*     INDXES     O   Indices  of the beginnings of TERMS within TEMP. */

/* $ Detailed_Input */

/*     TEMP       A META/2 template. */

/* $ Detailed_Output */

/*     TERMS      These are those keywords that begin the simple */
/*                templates of the groups templates of TEMP, as well */
/*                as the keywords that immediately follow group */
/*                templates. */

/*     INDXES     Contains the indexes of the first characters of */
/*                each of the words in TERMS within TEMP.  Specifically, */
/*                if we let L = LASTNB(TERMS(I)) then */
/*                TERMS(I)( 1 : L ) = TEMP( INDXES(I) : INDXES(I) + L ) */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Particulars */

/*      This is a utility routine to aid the general M2 pattern matching */
/*      routine ( M2CHCK ).  It determines */

/*         1.) initial keywords of simple templates of group templates; */
/*         2.) the keywords that immediately follow groups; */
/*         3.) the keywords that immediately follow unqualified @then */
/*             directives. */

/*      These keywords together with their indexes are loaded in the */
/*      order they appear in the template into the cells TERMS and */
/*      INDXES.  Additionally, the marker '}' is inserted in the */
/*      cell of keywords immediately following */

/*         1.) the last initial keyword of a group, provided the */
/*             group has a range template that is NOT of the form */
/*             (0:n) (where n is any integer).  The index associated */
/*             with such a marker is the index in the template of the */
/*             '}' that ends the group associated with the marker. */

/*         2.) any keyword that immediately follows a group.  It */
/*             is assigned the index of the first blank that follows */
/*             the keyword. */

/*         3.) any keyword that immediatly follows an unqualified @then */
/*             directive.  It is given the index of the first blank */
/*             following the keyword. */

/*         4.) after all keywords provided that the template does not */
/*             end with a qualified @then directive.  The marker is */
/*             assigned an index equal to the length of the template */
/*             plus 1. */


/*      The marker can be used to determine what keywords might end */
/*      a variable length template. */


/* $ Examples */

/*      Suppose that the template was */

/*      (0:1){ PLEASE } */
/*             SEND (1:7)@english (0:1){ AND @english } */

/*                  (1:1){ A         @english(MESSAGE|CHECK|LETTER) */
/*                       | MEMO (0:1)@english(NUMBER)      @int(1:) */
/*                       | THE       @english(SCHEDULE|PROPOSAL) */
/*                       | HOME */
/*                       | FLOWERS } */

/*      Then the cells TERMS and INDXES (assuming that spaces have been */
/*      compressed down to 1 between words) would be returned as: */

/*      TERMS        INDXES */
/*      -------      ------- */
/*      PLEASE          8 */
/*      SEND           17 */
/*      }              21 */
/*      AND            43 */
/*      A              65 */
/*      MEMO          101 */
/*      THE           139 */
/*      HOME          173 */
/*      FLOWERS       180 */
/*      }             188 */
/*      }             189 */

/* $ Restrictions */

/*     It is expected that any template input to this routine satisfies */
/*     the rules required of META/2 templates. */

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


/*     Beta Version 1.0.0, 10-MAY-1987 (WLT) (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

/* Computing MIN */
    i__1 = sizec_(terms, terms_len), i__2 = sizei_(indxes);
    room = min(i__1,i__2);
    scardc_(&c__0, terms, terms_len);
    scardi_(&c__0, indxes);

/*     Just look through the string and located the appropriate keywords. */
/*     First see if there are any group templates. */

    beg = 1;
    count = 0;
    more = TRUE_;

/*     NEXT will point to the next group template so long as there are */
/*     more to find. */

    while(more) {
	nextg = upto_(temp, "){ ", &beg, temp_len, (ftnlen)3);
	nextt = upto_(temp, " @then ", &beg, temp_len, (ftnlen)7);
	if (nextg < nextt) {
	    group = TRUE_;
	    dothen = FALSE_;
	    next = nextg;
	} else if (nextt < nextg) {
	    dothen = TRUE_;
	    group = FALSE_;
	    next = nextt;
	} else {
	    dothen = FALSE_;
	    group = FALSE_;
	    more = FALSE_;
	}
	if (group) {

/*           Find the beginning of the range template and see if */
/*           it has the form (0:*).  If it has that form we will */
/*           not want to mark the end of the group when we finish */
/*           with it. */

	    b = posr_(temp, "(", &nextg, temp_len, (ftnlen)1) + 1;
	    mrkend = ncpos_(temp, "0", &b, temp_len, (ftnlen)1) != pos_(temp, 
		    ":", &b, temp_len, (ftnlen)1);

/*           Find the end of the next group template and set BEG */

	    beg = pos_(temp, "}", &nextg, temp_len, (ftnlen)1) + 1;
	    mark = beg - 1;
	    if (beg == 1) {
		chkin_("M2TERM", (ftnlen)6);
		setmsg_("A switch was begun, but never ended.", (ftnlen)36);
		sigerr_("SPICE(META2DEFERR)", (ftnlen)18);
		chkout_("M2TERM", (ftnlen)6);
		return 0;
	    }

/*           Locate the first keyword of the group template. */

	    fndnwd_(temp, &nextg, &b, &e, temp_len);
	    if (count > room) {
		chkin_("M2TERM", (ftnlen)6);
		setmsg_("There are too many possible terminating keywords. ", 
			(ftnlen)50);
		sigerr_("SPICE(META2TOOMANYKEYS)", (ftnlen)23);
		chkout_("M2TERM", (ftnlen)6);
		return 0;
	    }
	    ++count;
	    s_copy(terms + (count + 5) * terms_len, temp + (b - 1), terms_len,
		     e - (b - 1));
	    indxes[count + 5] = b;

/*           See if there are anymore simple templates in the this */
/*           group template ( they will all be preceeded by ' | '. */

	    nextg = e;
	    nextg = pos_(temp, " | ", &next, beg, (ftnlen)3) + 2;
	    while(nextg >= 3) {

/*              Locate the next keyword. */

		fndnwd_(temp, &nextg, &b, &e, temp_len);

/*              Take care of any errors that might occur. */

		if (b == 0) {
		    chkin_("M2TERM", (ftnlen)6);
		    setmsg_("An improperly composed META/2 switch was encoun"
			    "tered.", (ftnlen)53);
		    sigerr_("SPICE(META2DEFERR)", (ftnlen)18);
		    chkout_("M2TERM", (ftnlen)6);
		    return 0;
		}
		if (count >= room) {
		    chkin_("M2TERM", (ftnlen)6);
		    setmsg_("There are too many possible terminating keyword"
			    "s. ", (ftnlen)50);
		    sigerr_("SPICE(META2TOOMANYKEYS)", (ftnlen)23);
		    chkout_("M2TERM", (ftnlen)6);
		    return 0;
		}

/*              Put the keyword on the list and note its string position. */

		++count;
		s_copy(terms + (count + 5) * terms_len, temp + (b - 1), 
			terms_len, e - (b - 1));
		indxes[count + 5] = b;
		nextg = e;
		nextg = pos_(temp, " | ", &nextg, beg, (ftnlen)3) + 2;
	    }

/*           If the group template just processed DID NOT have a range */
/*           template of the form (0:*%), put the marker '}' into the */
/*           list of keywords. */

	    if (mrkend) {
		++count;
		s_copy(terms + (count + 5) * terms_len, "}", terms_len, (
			ftnlen)1);
		indxes[count + 5] = mark;
	    }

/*           We are out of initial keywords in the group. Get the next */
/*           word and see if it is a keyword or the beginning of */
/*           another group template. */

	    fndnwd_(temp, &beg, &b, &e, temp_len);
	} else if (dothen) {
	    beg = next + 6;
	    fndnwd_(temp, &beg, &b, &e, temp_len);
	}
	if (! more) {

/*           Don't do anything, just get ready to drop through the loop. */

	} else if (b == 0) {

/*           We are out of template */

	    more = FALSE_;
	    scardc_(&count, terms, terms_len);
	    scardi_(&count, indxes);
	} else if (matchw_(temp + (b - 1), "(%*:%*){", "*", "%", e - (b - 1), 
		(ftnlen)8, (ftnlen)1, (ftnlen)1)) {

/*           Do nothing, this will all be taken care of later. */

	} else if (s_cmp(temp + (b - 1), "@then", e - (b - 1), (ftnlen)5) == 
		0) {

/*           Don't do anything, we'll get back to this in a moment. */

	} else if (matchw_(temp + (b - 1), "@then(%*)", "*", "%", e - (b - 1),
		 (ftnlen)9, (ftnlen)1, (ftnlen)1)) {

/*           That's it.  I quit. */

	    scardc_(&count, terms, terms_len);
	    scardi_(&count, indxes);
	    more = FALSE_;
	} else {

/*           Get rid of any beginning range template. (If there is a */
/*           range template we just dump the values into D1 and D2 */
/*           and never use them.) */

	    m2begr_(temp, &b, &e, &d1, &d2, temp_len);
	    if (b > e) {

/*              do nothing */

	    } else if (m2keyw_(temp + (b - 1), e - (b - 1))) {
		++count;
		s_copy(terms + (count + 5) * terms_len, temp + (b - 1), 
			terms_len, e - (b - 1));
		indxes[count + 5] = b;
		beg = e + 1;
		++count;
		s_copy(terms + (count + 5) * terms_len, "}", terms_len, (
			ftnlen)1);
		indxes[count + 5] = beg;
	    }
	}
	group = FALSE_;
	dothen = FALSE_;
    }

/*     Set the cardinality and return */

    scardc_(&count, terms, terms_len);
    scardi_(&count, indxes);
    return 0;
} /* m2term_ */

