/* niceio_3.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure     NICEIO_3 ( Nicely formatted output -- test version ) */
/* Subroutine */ int niceio_3__(char *string, integer *unit, char *style, 
	ftnlen string_len, ftnlen style_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer vbeg;
    char line[512];
    integer left, last;
    extern integer upto_(char *, char *, integer *, ftnlen, ftnlen);
    integer b, e, flagb, k, flage, w;
    extern logical match_(char *, char *, ftnlen, ftnlen);
    integer leftb, vtabb, lefte, flagw, vtabe, nlinb;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    integer nline, nleft, rmarg, origl, right, width;
    extern integer ncpos_(char *, char *, integer *, ftnlen, ftnlen);
    integer vtabw, origr, start;
    logical trltk;
    extern logical failed_(void);
    integer vb, leadrb, ve, leadre;
    logical leadtk;
    char breaks[1];
    logical flagtk, mrgchg;
    integer trailb, leadrw, rightb, traile;
    logical hardsp;
    integer righte, vtabat, indent;
    logical looped;
    integer pstamb;
    char hspchr[1];
    integer prambw, lright;
    extern /* Subroutine */ int fndnwd_(char *, integer *, integer *, integer 
	    *, ftnlen);
    logical vtabtk;
    integer nlinew, nright;
    extern integer frstnb_(char *, ftnlen);
    logical nlintk, newlin;
    extern /* Subroutine */ int nparsi_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen);
    integer trailw;
    extern integer qlstnb_(char *, ftnlen);
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    char errorl[160];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), suffix_(char *, integer *, char *, ftnlen, ftnlen), 
	    fndntk_(char *, char *, integer *, integer *, integer *, ftnlen, 
	    ftnlen);
    char errorr[160];
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen), replch_(
	    char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int writln_(char *, integer *, ftnlen), cutstr_(
	    char *, integer *, integer *, char *, integer *, integer *, 
	    ftnlen, ftnlen);
    integer beg, end;
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);

/* $ Abstract */

/*     Output a string to a unit using one of a set of available styles. */
/*     Format and output a string so that it has a pleasing appearance */
/*     (breaks for newlines occurring at natural places, margins set at */
/*     desired levels, etc.) */

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

/* $ Keywords */

/*     STRING */
/*     TEXT */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  --------------------------------------------------- */
/*     STRING     I   Message to be printed. */
/*     UNIT       I   Logical unit connected to output. */
/*     STYLE      I   Format specification string. */

/* $ Detailed_Input */

/*     STRING    A long string to be broken into columns and output. */
/*               If desired, the user can force various changes to the */
/*               format of the output by inserting control substrings */
/*               into the desired text, and specifying these control */
/*               strings in the character string STYLE. */

/*               Three control functions are possible.  They are: */

/*               1) Force a newline. */
/*               2) Force a newline and alter the margins for output. */
/*               3) Insert a vertical tab in the output. */

/*               To force a new line at some location of the string */
/*               during output you must put the KEYWORD 'NEWLINE' */
/*               into the string STYLE followed by a word that will */
/*               be used to signal a linebreak.  For example you */
/*               might use ' NEWLINE /cr '.  At any point of the */
/*               string that a newline is desired, insert the string */
/*               associated with the NEWLINE keyword ( in this case */
/*               '/cr' ).  Spaces are not required around the NEWLINE */
/*               control string (or any other control string). */

/*               To modify the margins after a line break, you insert */
/*               the line break control string into STRING and insert */
/*               immediately after it a string of the form (x:y) where */
/*               x and y are numeric strings.  The number x indicates */
/*               that the left margin should be moved x to the right. */
/*               The number y indicates the right margin should be */
/*               moved y to the left.  Both negative and positive */
/*               values are allowed.  Spaces are allowed within the */
/*               spaces between parentheses. In keeping with our */
/*               previous example both */

/*                      '/cr(5:-2)' and '/cr( 5 : -2 )' */

/*               directs the routine to force an line break; move the */
/*               left margin 5 to the right; move the right margin */
/*               -2 to the left (.i.e. 2 to the right). */

/*                      /cr (5: -2) */

/*               would be treated as simply a new line directive, the */
/*               remainder (5: -2) is treated as part of the string */
/*               to output. */

/*               To force a vertical tab at any point of the string */
/*               you must specify a vertical tab control string in */
/*               the style string.  Then at the point in string */
/*               where you want a vertical tab to appear, simply insert */
/*               the vertical tab string, spaces are NOT required */
/*               around the vertical tab string. */

/*               All control substrings in STRING are treated as having */
/*               zero width and invisible to output. */

/*     UNIT      is the logical unit to which the output will be */
/*               directed. */

/*     STYLE  is a character string that controls how the string */
/*            should be formatted on output and what substrings */
/*            of STRING will be treated as control characters */
/*            for display of STRING.  STYLE should consist of */
/*            a sequence of keyword/value word pairs.  That is, */
/*            it should consist of a sequence of words ( according */
/*            to the SPICE definition of a word ) in a pattern */
/*            as illustrated here: */

/*            Keyword_1 Value_1 Keyword_2 Value_2 ... Keyword_n Value_n */

/*            Acceptable keywords, their meanings, and expectations */
/*            regarding their associate values are spelled at below. */

/*          'FLAG'       is a keyword used to indicate that a string */
/*                       will prefix the output of STRING.  Moreover */
/*                       STRING will be printed in a block that is */
/*                       indented by one more than the nonblank length */
/*                       of the FLAG. (The appearance will parallel what */
/*                       you see here in this description, where 'FLAG' */
/*                       is the flag associated with this block of text.) */

/*                       If a flag is specified, the resulting output */
/*                       will consist of a flag, 1 space and formatted */
/*                       output text. */

/*                       Unless the FLAG keyword appears, no flag is */
/*                       used with the output. */

/*          'LEADER'     is the keyword used to indicate that the left */
/*                       margin of the output will always begin with */
/*                       the word that follows LEADER.  The leader */
/*                       string will not appear on the FLAG line */
/*                       if a FLAG is specified. The leader can */
/*                       be placed on the FLAG line by simply making */
/*                       it part of the flag. */

/*                       Unless the LEADER keyword appears, no leader is */
/*                       used with the output. */

/*          'TRAILER'    is the keyword used to indicate that the right */
/*                       margin of the output will always end with */
/*                       the word that follows TRAILER.  The trailer */
/*                       will appear in every line. */

/*           The effect of using the keywords LEADER, TRAILER and FLAG */
/*           is to change the margins specified (or implied) through */
/*           the use of LEFT and RIGHT.  The effective value of LEFT */
/*           will become LEFT + MAX ( LEN(LEADER), LEN(FLAG)+1 ). */
/*           The right margin becomes RIGHT - LEN(TRAILER). */


/*          'LEFT'       is the keyword used to indicate where the */
/*                       left margin of the output text should appear */
/*                       (either on the output screen or in a file). */
/*                       Note if a FLAG is present, when displayed the */
/*                       flag will start in this column of the output. */
/*                       The remaining text will be indented one */
/*                       more than the width of the nonblank portion of */
/*                       the flag.  If no flag is present, output will */
/*                       begin in the specified LEFT column. */

/*                       The word that immediately follows LEFT must */
/*                       successfully parse as an integer. */

/*                       If the LEFT keyword does not appear the left */
/*                       margin is set to 1. */

/*          'RIGHT'      is the keyword used to indicate where the */
/*                       right margin of the output text should appear */
/*                       (either on the output screen or in a file). */

/*                       The word that immediately follows RIGHT must */
/*                       successfully parse as an integer. */

/*                       If the RIGHT keyword does not appear the right */
/*                       margin is set to 80. */

/*          'NEWLINE'    is the keyword used to indicate what substring */
/*                       if any within the text string will be */
/*                       intrepreted as meaning "start a new line" and */
/*                       optionally "reset the margins."  (See STRING */
/*                       for details concerning the use of the newline */
/*                       substring.) */

/*                       If the keyword NEWLINE is not present, no */
/*                       substring of STRING will be interpreted as */
/*                       directing a newline to be started. */

/*          'VTAB'       is the keyword used to indicate what substring */
/*                       within the text string will be interpreted */
/*                       as meaning "start a new line, but indent it */
/*                       to the current position within this line." */
/*                       This is refered to as a vertical tab. */

/*                       If the keyword VTAB is not present no substring */
/*                       of STRING will be interpreted as a vertical */
/*                       tab. */

/*          'HARDSPACE'  is the keyword used to indicate what character */
/*                       within the text string will be processed as a */
/*                       normal text character, but will be written out */
/*                       as a space upon output. Note HARDSPACES in both */
/*                       the FLAG and LEADER will converted into spaces */
/*                       upon output. */

/*                       If the keyword HARDSPACE is not present, no */
/*                       character will be interpreted as a hard space. */

/* $ Detailed_Output */

/*     None. */

/* $ Exceptions */

/*     1)  If a keyword/value pair is entered more than once in */
/*         the style string, the last pair takes precedence. */

/*     2)  If a keyword appears without a following value in the */
/*         style string the SPICE error 'SPICE(UNBALACEDPAIR)' is */
/*         signaled. */

/*     3)  If a keyword is not recognized, the error 'SPICE(UNKNOWNKEY)' */
/*         is signaled */

/*     4)  If one of the margin keywords (LEFT RIGHT) is not followed */
/*         by a numeric string, the error 'SPICE(NONNUMERICSTRING)' */
/*         is signaled. */

/*     5)  If the left column becomes less than zero, or the right column */
/*         becomes less than the left column the error */
/*         'SPICE(INVALIDCOLUMN)' is signaled. */

/*     6)  If the number of columns from the left to the right margin */
/*         becomes less than or equal to the number of characters in the */
/*         flag (assuming one is specified) the error */
/*         'SPICE(SPACETOONARROW)' is signaled. */

/*      7) If output cannot be performed, the error 'SPICE(OUTPUTERROR)' */
/*         will be signaled and a descriptive long message set to */
/*         aid in determining the cause of the output failure. */

/*      8) If the right margin exceeds 512, the output will be truncated */
/*         at column 512. */

/* $ Particulars */

/*     This routine is designed to aid in the problem of creating */
/*     nice looking messages that must extend over 1 line.  It */
/*     allows the user to construct messages by simply appending, */
/*     prefixing or inserting text into an existing string until */
/*     the message is finished.  The user need not be concerned */
/*     about breaking up the message in good spots for output. */
/*     This routine searches the message in STRING for "good" places */
/*     at which to break STRING. */

/*     The user may specify a "flag" that will be used to prefix the */
/*     first output line, left and right margins for the output, */
/*     and special strings for forcing creation of newlines, changing */
/*     margins, and inserting vertical tabs. */

/*     This routine always sends to output a blank line prior to */
/*     the start of the output formatted string. */

/*     Since strings are often built by concatenation, the user may */
/*     want to compress out extra spaces in string before calling */
/*     this routine.  This routine breaks the input string at gaps */
/*     in the string, but does not get rid of large gaps within */
/*     a successfully broken output line. (See the examples below.) */

/*     For a discussion of the string breaking algorithm see the */
/*     particulars section of the SPICE routine CUTSTR. */

/* $ Files */

/*     The output is sent to the file or device connected to the logical */
/*     unit UNIT that has been appropriately prepared by the calling */
/*     program. */

/* $ Examples */

/*     Suppose */

/*           STYLE  = 'LEFT 10 RIGHT 50 ' */
/*     and */

/*           STRING = 'Now is the time for all good men to come to ' // */
/*          .         'the aid of their party. Out with the bad ' // */
/*          .         'air and in with the good.  Health and purity '// */
/*          .         'preserve our essence. ' */

/*     The the output would look like: */

/*     Column */
/*     1........10........20........30........40........50........60 */

/*              Now is the time for all good men to come */
/*              the aid of their party.  Out with the */
/*              bad air and in with the good.  Health and */
/*              purity preserve our essence. */



/*     Suppose */

/*           STYLE  = 'FLAG Example: LEFT 10 RIGHT 50 ' */
/*     and */

/*           STRING = 'Now is the time for all good men to come to ' // */
/*          .         'the aid of their party. Out with the bad ' // */
/*          .         'air and in with the good.  Health and purity '// */
/*          .         'preserve our essence. ' */

/*     The the output would look like: */

/*     Column */
/*     1........10........20........30........40........50........60 */

/*               Example: Now is the time for all good men */
/*                        to come to the aid of their */
/*                        party. Out with the bad air and */
/*                        in with the good.  Health and */
/*                        purity preserve our essence. */





/*     Suppose */

/*           STYLE  = 'FLAG Example: LEFT 10 RIGHT 50 NEWLINE .' */
/*     and */

/*           STRING = 'Now is the time for all good men to come to ' // */
/*          .         'the aid of their party. Out with the bad ' // */
/*          .         'air and in with the good.  Health and purity '// */
/*          .         'preserve our essence. ' */

/*     The the output would look like: */

/*     Column */
/*     1........10........20........30........40........50........60 */

/*              Example: Now is the time for all good men */
/*                       to come to the aid of their */
/*                       party */
/*                       Out with the bad air and in with */
/*                       the good */
/*                       Health and purity preserve our */
/*                       essence */









/*     Suppose */

/*           STYLE  = 'FLAG Example: LEFT 10 RIGHT 50 VTAB . HARDSPACE _' */
/*     and */

/*           STRING = '___ is the time for all good men to come to ' // */
/*          .         'the aid of their party. Out with the bad ' // */
/*          .         'air and in with the good.  Health and purity '// */
/*          .         'preserve our essence. ' */

/*     The the output would look like: */

/*     Column */
/*     1........10........20........30........40........50........60 */

/*              Example:     is the time for all good men */
/*                       to come to the aid of their */
/*                       party */
/*                             Out with the bad air and */
/*                       in with the good */
/*                                         Health and */
/*                       purity preserve our essence */





/*     Suppose */

/*        STYLE  = 'FLAG Error: LEFT 1 RIGHT 60 NEWLINE /cr VTAB /vt' */

/*     and */

/*        STRING = 'I believe the command you were attempting to enter'// */
/*       .         'was /cr/cr(5:5)FIND TIMES OF GREATEST ELONGATION ' // */
/*       .         'FOR VENUS /cr/cr(-5:-5) I was expecting to the '   // */
/*       .         'word GREATEST when I encountered the word GRETEST '// */
/*       .         'in your input command. /cr/cr(5:5) FIND TIMES OF ' // */
/*       .         '/vt/vt GRETEST /vt/vt ELONGATION FOR VENUS '       // */
/*       .         '/cr/cr(-5:-5) I think you left out the fourth '    // */
/*       .         'letter --- "A" . */

/*     The the output would look like: */

/*     Column */
/*     1........10........20........30........40........50........60 */

/*     Error: I believe the command you were attempting to enter */
/*            was */

/*                 FIND TIMES OF GREATEST ELONGATION FOR VENUS */

/*            I was expecting to see the word GREATEST when I */
/*            encountered the word GRETEST in your input command. */

/*                 FIND TIMES OF */

/*                               GRETEST */

/*                                       ELONGATION FOR VENUS */

/*            I think you left out the fourth letter --- "A" . */


/*   Some care should be taken when choosing substrings to indidicate */
/*   newline and vertical tab control.  For example, suppose */

/*      STYLE  = ' FLAG NAIF: LEFT 6 RIGHT 56 NEWLINE xx VTAB AA ' */

/*   and */

/*      STRING = 'Officials at Exxon today reported a deal with the '  // */
/*     .         'Automobile Association of America (AAA) that would ' // */
/*     .         'provide club memebers with discount prices on '      // */
/*     .         'gasoline. xxxx( 3:3) Spokesmen said AA "Get your '   // */
/*     .         'AAA membership cards now." AA xx(-3:-3)xx Texeco '   // */
/*     .         'officials had no comment.' */


/*     The the output would look like: */

/*     Column */
/*     1........10........20........30........40........50........60 */

/*          NAIF: Officials at E */
/*                on today reported a deal with the Automobile */
/*                Assosiation of America ( */
/*                                        A) that would provide */
/*                club members with discount prices on */
/*                gasoline. */

/*                   Spokesmen said */
/*                                   "Get your */
/*                                             A membership */
/*                   cards now." */

/*                Texeco officials had no comment. */


/* $ Restrictions */

/*     It is the responsibility of the calling program to properly */
/*     prepare the device/file associated with the logical unit UNIT */
/*     to receive the output from this routine. */

/*     The RIGHT margin must be less than or equal to 512. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -     Support Version 1.1.0, 22-APR-1997 (WLT) */

/*        Modified calls to SETMSG to use a marker and then replace */
/*        marker using ERRCH. */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/*     Beta Version 1.0.0, 12-AUG-1988 (WLT) (IMU) */

/* -& */

/*     SPICELIB Functions. */


/*   Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("NICEIO_3", (ftnlen)8);
    }

/*     Set the defaults and initial values. */


/*     Set the defaults and initial values. */

    left = 1;
    right = 80;
    flagtk = FALSE_;
    leadtk = FALSE_;
    hardsp = FALSE_;
    nlintk = FALSE_;
    trltk = FALSE_;
    vtabtk = FALSE_;
    *(unsigned char *)hspchr = ' ';
    flagw = 0;
    leadrw = 0;
    trailw = 0;
    prambw = 0;
    vtabw = 0;
    beg = 1;
    s_copy(errorl, " ", (ftnlen)160, (ftnlen)1);
    s_copy(errorr, " ", (ftnlen)160, (ftnlen)1);
    *(unsigned char *)breaks = ' ';

/*     Parse the style string. */

    fndnwd_(style, &beg, &b, &e, style_len);
    while(b != 0) {
	vbeg = e + 1;
	fndnwd_(style, &vbeg, &vb, &ve, style_len);
	if (vb != 0) {
	    if (s_cmp(style + (b - 1), "FLAG", e - (b - 1), (ftnlen)4) == 0) {
		flagb = vb;
		flage = ve;
		flagw = ve - vb + 2;
		flagtk = TRUE_;
	    } else if (s_cmp(style + (b - 1), "LEADER", e - (b - 1), (ftnlen)
		    6) == 0) {
		leadrb = vb;
		leadre = ve;
		leadrw = ve - vb + 1;
		leadtk = TRUE_;
	    } else if (s_cmp(style + (b - 1), "TRAILER", e - (b - 1), (ftnlen)
		    7) == 0) {
		trailb = vb;
		traile = ve;
		trailw = ve - vb + 1;
		trltk = TRUE_;
	    } else if (s_cmp(style + (b - 1), "VTAB", e - (b - 1), (ftnlen)4) 
		    == 0) {
		vtabb = vb;
		vtabe = ve;
		vtabw = ve - vb + 1;
		vtabtk = TRUE_;
	    } else if (s_cmp(style + (b - 1), "NEWLINE", e - (b - 1), (ftnlen)
		    7) == 0) {
		nlinb = vb;
		nline = ve;
		nlinew = ve - vb + 1;
		nlintk = TRUE_;
	    } else if (s_cmp(style + (b - 1), "LEFT", e - (b - 1), (ftnlen)4) 
		    == 0) {
		nparsi_(style + (vb - 1), &left, errorl, &k, ve - (vb - 1), (
			ftnlen)160);
		if (s_cmp(errorl, " ", (ftnlen)160, (ftnlen)1) != 0) {
		    setmsg_("The word following the keyword 'LEFT' must pars"
			    "e as an integer. # ", (ftnlen)66);
		    errch_("#", errorl, (ftnlen)1, (ftnlen)160);
		    sigerr_("SPICE(NONNUMERICSTRING)", (ftnlen)23);
		    chkout_("NICEIO_3", (ftnlen)8);
		    return 0;
		}
	    } else if (s_cmp(style + (b - 1), "RIGHT", e - (b - 1), (ftnlen)5)
		     == 0) {
		nparsi_(style + (vb - 1), &right, errorr, &k, ve - (vb - 1), (
			ftnlen)160);
		if (s_cmp(errorr, " ", (ftnlen)160, (ftnlen)1) != 0) {
		    setmsg_("The word following the keyword 'RIGHT' must par"
			    "se as an integer. #", (ftnlen)66);
		    errch_("#", errorl, (ftnlen)1, (ftnlen)160);
		    sigerr_("SPICE(NONNUMERICSTRING)", (ftnlen)23);
		    chkout_("NICEIO_3", (ftnlen)8);
		    return 0;
		}
	    } else if (s_cmp(style + (b - 1), "HARDSPACE", e - (b - 1), (
		    ftnlen)9) == 0) {
		hardsp = TRUE_;
		if (vb != ve) {
		    setmsg_("Hardspaces must be a single character.  You hav"
			    "e \"#\".", (ftnlen)53);
		    errch_("#", style + (vb - 1), (ftnlen)1, ve - (vb - 1));
		    sigerr_("SPICE(BADHARDSPACE)", (ftnlen)19);
		    chkout_("NICEIO_3", (ftnlen)8);
		    return 0;
		} else {
		    *(unsigned char *)hspchr = *(unsigned char *)&style[vb - 
			    1];
		}
	    } else {
		s_copy(line, style + (b - 1), (ftnlen)512, e - (b - 1));
		suffix_("is not a recognized keyword for the SPICELIB routin"
			"e NICEIO. ", &c__1, line, (ftnlen)61, (ftnlen)512);
		setmsg_(line, (ftnlen)512);
		sigerr_("SPICE(UNKNOWNKEY)", (ftnlen)17);
		chkout_("NICEIO_3", (ftnlen)8);
		return 0;
	    }
	    beg = ve + 1;
	    fndnwd_(style, &beg, &b, &e, style_len);
	} else {
	    setmsg_("# did not have an associated value", (ftnlen)34);
	    errch_("#", style + (b - 1), (ftnlen)1, e - (b - 1));
	    sigerr_("SPICE(UNBALANCEDPAIR)", (ftnlen)21);
	    chkout_("NICEIO_3", (ftnlen)8);
	    return 0;
	}
    }

/*     So ends the parsing of the style string.  Now do the actual work. */

    s_copy(line, " ", (ftnlen)512, (ftnlen)1);

/*     Determine how much space needs to be allocated for the */
/*     flag and leaders. */

    origr = right;
    origl = left;
    rmarg = right;
    prambw = max(flagw,leadrw);
    pstamb = right - trailw + 1;
    right -= trailw;
    if (flagw > 0) {
	s_copy(line + (origl - 1), style + (flagb - 1), 512 - (origl - 1), 
		flage - (flagb - 1));
    } else if (leadrw > 0) {
	s_copy(line + (origl - 1), style + (leadrb - 1), 512 - (origl - 1), 
		leadre - (leadrb - 1));
    }
    if (trailw > 0) {
	s_copy(line + (pstamb - 1), style + (trailb - 1), origr - (pstamb - 1)
		, traile - (trailb - 1));
    }
/* Computing MAX */
    i__1 = 1, i__2 = frstnb_(string, string_len);
    b = max(i__1,i__2);
    last = qlstnb_(string, string_len);

/*     If there is a newline token, we have to write out empty lines */
/*     and modify the margins as we encounter newline tokens and */
/*     newline tokens with margin modifiers.  Typically the loop */
/*     in the if block below will never be exercised. */

    if (nlintk) {
	e = b + nlinew - 1;
	if (e < last) {
	    newlin = s_cmp(string + (b - 1), style + (nlinb - 1), e - (b - 1),
		     nline - (nlinb - 1)) == 0;
	} else {
	    newlin = FALSE_;
	}
	while(newlin) {

/*           See if the new line token is qualified so as to change */
/*           the margins of the output. */

	    if (e + 1 < last) {
		i__1 = e;
		mrgchg = match_(string + i__1, "(*:*)*", string_len - i__1, (
			ftnlen)6);
	    } else {
		mrgchg = FALSE_;
	    }
	    if (mrgchg) {

/*              Looks like we should change the columns. Locate the */
/*              tokens of the newline marker. */

		i__1 = e + 1;
		fndntk_(string, "(:", &i__1, &leftb, &lefte, string_len, (
			ftnlen)2);
		fndntk_(string, ":)", &lefte, &rightb, &righte, string_len, (
			ftnlen)2);

/*              Parse the strings representing the increments to left */
/*              and right column positions. */

		s_copy(errorl, " ", (ftnlen)160, (ftnlen)1);
		s_copy(errorr, " ", (ftnlen)160, (ftnlen)1);
		if (leftb <= lefte) {
		    nparsi_(string + (leftb - 1), &nleft, errorl, &k, lefte - 
			    (leftb - 1), (ftnlen)160);
		} else {
		    nleft = 0;
		}
		if (rightb <= righte) {
		    nparsi_(string + (rightb - 1), &nright, errorr, &k, 
			    righte - (rightb - 1), (ftnlen)160);
		} else {
		    nright = 0;
		}

/*              Only if no errors were encountered during parsing do we */
/*              change the columns. */

		if (s_cmp(errorl, " ", (ftnlen)160, (ftnlen)1) == 0 && s_cmp(
			errorr, " ", (ftnlen)160, (ftnlen)1) == 0) {
		    b = righte + 2;
		    left += nleft;
		    right -= nright;
		    rmarg = max(origr,right);
		} else {
		    b += nlinew;
		}
	    } else {
		b += nlinew;
	    }

/*           Check for goofy margins. */

	    if (left < 1) {
		setmsg_("The current value for the left column is #. This is"
			" less than 1 and thus not a valid value. ", (ftnlen)
			92);
		errint_("#", &left, (ftnlen)1);
		sigerr_("SPICE(INVALIDCOLUMN)", (ftnlen)20);
		chkout_("NICEIO_3", (ftnlen)8);
		return 0;
	    } else if (left > right) {
		setmsg_("The current value for the left column is greater th"
			"an the value for the right column. The value for the"
			" left column is #.  The value for the right column i"
			"s #. ", (ftnlen)160);
		errint_("#", &left, (ftnlen)1);
		errint_("#", &right, (ftnlen)1);
		sigerr_("SPICE(INVALIDCOLUMN)", (ftnlen)20);
		chkout_("NICEIO_3", (ftnlen)8);
		return 0;
	    }

/*           Output something, but first replace hard spaces by spaces */

	    if (hardsp) {
		replch_(line, hspchr, " ", line, rmarg, (ftnlen)1, (ftnlen)1, 
			rmarg);
	    }
	    writln_(line, unit, rmarg);
	    if (failed_()) {
		chkout_("NICEIO_3", (ftnlen)8);
		return 0;
	    }
	    s_copy(line, " ", (ftnlen)512, (ftnlen)1);
	    if (leadtk) {
		s_copy(line + (origl - 1), style + (leadrb - 1), 512 - (origl 
			- 1), leadre - (leadrb - 1));
	    }
	    if (trltk) {
		s_copy(line + (pstamb - 1), style + (trailb - 1), 512 - (
			pstamb - 1), traile - (trailb - 1));
	    }

/*           Adjust the beginning and ending of the next portion */
/*           of the string to examine. */

/* Computing MAX */
	    i__1 = b, i__2 = ncpos_(string, " ", &b, string_len, (ftnlen)1);
	    b = max(i__1,i__2);
	    e = b + nlinew - 1;
	    if (e < last) {
		newlin = s_cmp(string + (b - 1), style + (nlinb - 1), e - (b 
			- 1), nline - (nlinb - 1)) == 0;
	    } else {
		newlin = FALSE_;
	    }
	}

/*        Find the next portion of the string to examine (it's up to */
/*        the next new line token or end of string whichever */
/*        comes first. */

	e = upto_(string, style + (nlinb - 1), &b, string_len, nline - (nlinb 
		- 1));
    } else {
	e = last;
    }

/*     Now we have are to the point of processing legitimate text. */
/*     Process the current substring  STRING(B:E).  It contains */
/*     no newline tokens. */

    while(e != 0) {
	width = right - left + 1 - prambw;
	if (width < 1) {
	    sigerr_("SPICE(SPACETOONARROW)", (ftnlen)21);
	    chkout_("NICEIO_3", (ftnlen)8);
	    return 0;
	}
	w = width;
	start = b;
	indent = 0;

/*        Grab the biggest piece of the substring that can be output */
/*        within the allowed space. */

	cutstr_(string, &start, &w, breaks, &beg, &end, e, (ftnlen)1);
	while(beg != 0) {

/*           See if there are any vertical tab marks */

	    if (! vtabtk) {
		i__1 = left + prambw - 1;
		s_copy(line + i__1, string + (beg - 1), right - i__1, end - (
			beg - 1));
	    } else {
		vtabat = pos_(string, style + (vtabb - 1), &start, e, vtabe - 
			(vtabb - 1));
		if (vtabat > 0 && vtabat <= end) {

/*                 If there is a vertical tab at the beginning of the */
/*                 string, we don't need to modify LINE. */

		    if (vtabat > beg) {
			end = vtabat - 1;
			i__1 = left + prambw + indent - 1;
			s_copy(line + i__1, string + (beg - 1), right - i__1, 
				end - (beg - 1));
			indent = indent + end - beg + 1;
			end = end + vtabe - vtabb + 1;
		    } else if (vtabat == beg) {
			i__1 = left + prambw + indent - 1;
			s_copy(line + i__1, " ", right - i__1, (ftnlen)1);
			end = beg + vtabe - vtabb;
		    }
		} else {

/*                 We just fill out the rest of this line.  There will */
/*                 be no need to indent the next one. */

		    i__1 = left + prambw + indent - 1;
		    s_copy(line + i__1, string + (beg - 1), right - i__1, end 
			    - (beg - 1));
		    indent = 0;
		}
	    }

/*           Handle any hard spaces */

	    if (hardsp) {
		replch_(line, hspchr, " ", line, rmarg, (ftnlen)1, (ftnlen)1, 
			rmarg);
	    }
	    writln_(line, unit, rmarg);
	    if (failed_()) {
		chkout_("NICEIO_3", (ftnlen)8);
		return 0;
	    }
	    s_copy(line, " ", (ftnlen)512, (ftnlen)1);
	    if (leadtk) {
		s_copy(line + (origl - 1), style + (leadrb - 1), 512 - (origl 
			- 1), leadre - (leadrb - 1));
	    }
	    if (trltk) {
		s_copy(line + (pstamb - 1), style + (trailb - 1), 512 - (
			pstamb - 1), traile - (trailb - 1));
	    }
	    start = end + 1;
	    w = width - indent;
	    if (w < 3) {
		w = width;
		indent = 0;
	    }
	    cutstr_(string, &start, &w, breaks, &beg, &end, e, (ftnlen)1);
	}

/*        Check to see if we should be looking for a newline token. */

	if (nlintk) {

/*           Ok.  Get ready to jump through hoops again.  We have to */
/*           look for newline tokens, for all those in excess of one */
/*           in a row, we have to output a blank line. */

	    b = e + 1;
	    e += nlinew;
	    looped = FALSE_;
	    if (e <= last) {
		newlin = s_cmp(string + (b - 1), style + (nlinb - 1), e - (b 
			- 1), nline - (nlinb - 1)) == 0;
	    } else {
		newlin = FALSE_;
	    }
	    while(newlin) {
		lright = right;

/*              See if the new line token is qualified so as to change */
/*              the margins of the output. */

		if (e >= last) {

/*                 In this case we can't possibly match as in the case */
/*                 below */

		    b += nlinew;
		} else /* if(complicated condition) */ {
		    i__1 = e;
		    if (match_(string + i__1, "(*:*)*", string_len - i__1, (
			    ftnlen)6)) {

/*                 Looks like we should change the columns. Locate the */
/*                 tokens of the newline marker. */

			fndntk_(string, "(:", &e, &leftb, &lefte, string_len, 
				(ftnlen)2);
			fndntk_(string, ":)", &lefte, &rightb, &righte, 
				string_len, (ftnlen)2);

/*                 Parse the strings representing the increments to left */
/*                 and right column positions. */

			s_copy(errorl, " ", (ftnlen)160, (ftnlen)1);
			s_copy(errorr, " ", (ftnlen)160, (ftnlen)1);
			if (leftb <= lefte) {
			    nparsi_(string + (leftb - 1), &nleft, errorl, &k, 
				    lefte - (leftb - 1), (ftnlen)160);
			} else {
			    nleft = 0;
			}
			if (rightb <= righte) {
			    nparsi_(string + (rightb - 1), &nright, errorr, &
				    k, righte - (rightb - 1), (ftnlen)160);
			} else {
			    nright = 0;
			}

/*                 Only if no errors were encountered during parsing */
/*                 do we change the columns. */

			if (s_cmp(errorl, " ", (ftnlen)160, (ftnlen)1) == 0 &&
				 s_cmp(errorr, " ", (ftnlen)160, (ftnlen)1) ==
				 0) {
			    b = righte + 2;
			    left += nleft;
			    right -= nright;
			    rmarg = max(origr,right);
			} else {
			    b += nlinew;
			}
		    } else {
			b += nlinew;
		    }
		}

/*              Take care of the case when outdenting or indenting has */
/*              forced us into absurd margins. */

		if (left < 1) {
		    setmsg_("The current value for the left column is #. Thi"
			    "s is less than 1 and thus not a valid value. ", (
			    ftnlen)92);
		    errint_("#", &left, (ftnlen)1);
		    sigerr_("SPICE(INVALIDCOLUMN)", (ftnlen)20);
		    chkout_("NICEIO_3", (ftnlen)8);
		    return 0;
		} else if (left > right) {
		    setmsg_("The current value for the left column is greate"
			    "r than the value for the right column. The value"
			    " for the left column is #.  The value for the ri"
			    "ght column is #. ", (ftnlen)160);
		    errint_("#", &left, (ftnlen)1);
		    errint_("#", &right, (ftnlen)1);
		    sigerr_("SPICE(INVALIDCOLUMN)", (ftnlen)20);
		    chkout_("NICEIO_3", (ftnlen)8);
		    return 0;
		}

/*              Output something if this is not the first pass through */
/*              the loop. */

		if (! looped) {
		    looped = TRUE_;
		    s_copy(line, " ", (ftnlen)512, (ftnlen)1);
		    if (leadtk) {
			s_copy(line + (origl - 1), style + (leadrb - 1), 512 
				- (origl - 1), leadre - (leadrb - 1));
		    }
		    if (trltk) {
			s_copy(line + (pstamb - 1), style + (trailb - 1), 512 
				- (pstamb - 1), traile - (trailb - 1));
		    }
		} else {

/*                 Handle any hard spaces */

		    if (hardsp) {
			replch_(line, hspchr, " ", line, rmarg, (ftnlen)1, (
				ftnlen)1, rmarg);
		    }
		    writln_(line, unit, rmarg);
		    if (failed_()) {
			chkout_("NICEIO_3", (ftnlen)8);
			return 0;
		    }
		}
/* Computing MAX */
		i__1 = b, i__2 = ncpos_(string, " ", &b, string_len, (ftnlen)
			1);
		b = max(i__1,i__2);
		e = b + nlinew - 1;
		if (e <= last) {
		    newlin = s_cmp(string + (b - 1), style + (nlinb - 1), e - 
			    (b - 1), nline - (nlinb - 1)) == 0;
		} else {
		    newlin = FALSE_;
		}
	    }
	    e = upto_(string, style + (nlinb - 1), &b, string_len, nline - (
		    nlinb - 1));

/*           Just in case we went through the loop, and didn't */
/*           output a line, and we've reached the end of the */
/*           string.  We check and write a blank line if necessary */

	    if (looped && e == 0) {

/*              Handle any hard spaces */

		if (hardsp) {
		    replch_(line, hspchr, " ", line, rmarg, (ftnlen)1, (
			    ftnlen)1, rmarg);
		}
		writln_(line, unit, rmarg);
		if (failed_()) {
		    chkout_("NICEIO_3", (ftnlen)8);
		    return 0;
		}
	    }
	} else {
	    e = 0;
	}
    }
    chkout_("NICEIO_3", (ftnlen)8);
    return 0;
} /* niceio_3__ */

