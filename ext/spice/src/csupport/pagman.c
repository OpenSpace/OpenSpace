/* pagman.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      PAGMAN (Page Manager) */
/* Subroutine */ int pagman_0_(int n__, char *which, char *line, integer *
	value, ftnlen which_len, ftnlen line_len)
{
    /* Initialized data */

    static integer pagesz = 24;
    static logical doprmt = FALSE_;
    static logical didpmt = FALSE_;
    static integer wfactr = 0;
    static char sectn[32] = "BODY                            ";
    static char respns[255] = "                                             "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                  ";
    static integer pagewd = 80;
    static integer freq[5] = { -1,-1,-1,-1,-1 };
    static integer need[5] = { 0,0,0,0,0 };
    static integer size[5] = { 0,0,0,0,0 };
    static integer row = 0;
    static integer pageno = 0;
    static logical body = TRUE_;
    static logical domark = FALSE_;

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    static integer i__;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    static char title[255*10];
    extern integer rtrim_(char *, ftnlen);
    static char header[255*15];
    static integer pagmln;
    static char pagmrk[32];
    static logical keepsp[5], visibl[5];
    static integer qlenth;
    static char myline[255], footer[255*10], questn[255];
    extern /* Subroutine */ int nspwln_(char *, ftnlen), prompt_(char *, char 
	    *, ftnlen, ftnlen);

/* $ Abstract */

/*     This routine serves as an umbrella for a collection of entry */
/*     points that manage the layout and printing of a series of */
/*     pages of text that may include fixed titles, headers, and */
/*     footers. */

/* $ Required_Reading */

/*     REPORTS */

/* $ Keywords */

/*     OUTPUT */
/*     TEXT */
/*     FORMATTING */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     WHICH      I   indicates which section to send lines of text to */
/*     LINE       I   a line of text */
/*     ATTR       I   the name of a global page attribute to be set */
/*     VALUE      I   the value of some global page attribute */

/* $ Detailed_Input */

/*     See the individual entry points for details */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     MXWDTH     is the width of the page in characters. */

/*     ROOMH      is the number of lines allowed for use in the */
/*                header section of the page. */

/*     ROOMT      is the amount of room allowed for the title section */
/*                of each page. */

/*     ROOMF      is the amount of room allowed for the footer of each */
/*                page. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     This routine sends lines of output to the routine NSPWLN. */

/* $ Particulars */

/*     By combining the function of the entry points in this routine you */
/*     may create a page having fixed titles, headers, and footers. */
/*     together with a variable body for each page.  In addition you may */
/*     insert the current page number in either the title or footer */
/*     portion of each page.  In addition you may adjust the size of */
/*     each page and the width of the page. */

/*     In addition since the IO path has not been selected (you supply */
/*     your own routine for receiving lines for output.  You may be able */
/*     to use this routine to build up pages that you may then process */
/*     further adding touches to the output that have not been provided */
/*     here. */

/*     The capabilities provided are: */

/*     PAGRST   ---  sets the page number to zero, and empties all */
/*                   sections of the page format. It does not affect the */
/*                   global page properties such as the frequency of */
/*                   titles, headers and footers, page width and height. */

/*     PAGSCN   ---  allows you to set the section to which the next */
/*                   lines should be written. */

/*     PAGSET   ---  allows you to set page geometry and frequency */
/*                   parameters. */

/*     PAGSMK   ---  allows you to set the marker that indicates "put the */
/*                   current page number here." */

/*     PAGPUT   ---  allows you put a line of text on the page.  Note */
/*                   that the "printing" of title, header and footer */
/*                   text is deferred until the first line of text in */
/*                   the body is sent to PAGPUT. */

/*     PAGSFT   ---  is a soft page reset,  the page number is not */
/*                   altered but the current page is ended (causing a */
/*                   footer to be written if one is to be printed on the */
/*                   current page) and then empties the sections */
/*                   indicated so that they can be updated with new */
/*                   text. */

/*     PAGPMT   ---  allows you to determine if the last call to PAGPUT */
/*                   caused a prompt to be issued and if so to see what */
/*                   the user's response to that prompt was. */

/*                   Note that for a prompt to be issued you must take */
/*                   several steps. */

/*                   1) You must enable prompts through the PAGSET entry */
/*                      point. */

/*                      CALL PAGSET ( 'PROMPT', 0 ) */

/*                   2) You next need to set the prompt that will be used */
/*                      This is done with two calls. */

/*                      CALL PAGSCN ( 'PROMPT' ) */
/*                      CALL PAGPUT ( 'your prompt value:' ) */

/*                   Having made these preparations, the page manager is */
/*                   now ready to issue your prompt and retain the user's */
/*                   response when a page is finished. */

/*                   Note that prompts are not issued as a result of */
/*                   calling a page reset for (either soft or hard) */

/*                   Also note that once a reset is issued, the prompt */
/*                   status is set back to the default value --- No */
/*                   Prompts. */

/*     You might use this routine in conjunction with NICEPR, or TABRPT. */

/*     A typical useage might go as shown here. */

/*     First set the basic global attributes of the page and report. */

/*     CALL PAGSET ( 'PAGEHEIGHT',        60 ) */
/*     CALL PAGSET ( 'PAGEWIDTH',         80 ) */
/*     CALL PAGSET ( 'HEADERFREQUENCY',   -1 ) */
/*     CALL PAGSET ( 'TITLEFREQUENCY',     1 ) */
/*     CALL PAGSET ( 'NOSPACEHEADER',      0 ) */
/*     CALL PAGSET ( 'SPACETITLE',         0 ) */
/*     CALL PAGSET ( 'FOOTERFREQUENCY',    1 ) */
/*     CALL PAGSMK ( '#'                     ) */
/*     CALL PAGRST */

/*     Create the title that will appear on every page. */

/*     CALL PAGSCN ( 'TITLE'                 ) */
/*     CALL PAGPUT ( ' '                     ) */
/*     CALL PAGPUT ( 'Results of Test'       ) */
/*     CALL PAGPUT ( ' '                     ) */
/*     CALL PAGPUT ( ' '                     ) */

/*     Create the footer that will appear on every page. */

/*     CALL PAGSCN ( 'FOOTER'              ) */
/*     CALL PAGPUT ( ' '                   ) */
/*     CALL PAGPUT ( ' '                   ) */
/*     CALL PAGPUT ( '           Page # '  ) */
/*     CALL PAGPUT ( ' '                   ) */
/*     CALL PAGPUT ( ' '                   ) */
/*     CALL PAGSCN ( 'BODY'                ) */
/*     DO I = 1, NLINES */
/*        CALL PAGPUT ( TEXT(I) ) */
/*     END DO */
/*     CALL PAGSFT */

/* $ Examples */

/*     See above. */

/* $ Restrictions */

/*    Since these routines interact by side effect, you should */
/*    read carefully the required reading documentation. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 9-JAN-1992 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Control the format of output pages */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

/*     The arrays TITLE, HEADER and FOOTER are used to store the */
/*     text that will be written to the TITLE, HEADER and FOOTER */
/*     sections of a page. */


/*     The variable RESPNS is used to keep track of any response */
/*     that the user may supply to a prompt that can be triggered */
/*     at the completion of a page. */


/*     The variable SECTN contains the name of the section to which */
/*     lines of text should be sent. */


/*     The array FREQ is used to store the */
/*     frequency with which footers, headers and titles should */
/*     be displayed PAGESZ and PAGEWD give the size of the page */
/*     in height and width. */

/*     The array SIZE is  used to maintain the */
/*     size of the TITLE, HEADER, BODY, and FOOTER sections. */

/*     The array NEED is used to determine how many lines */
/*     need to be devoted to the TITLE, HEADER and FOOTER section */
/*     on a page (the value will be a function of FREQ, the page */
/*     number and the array KEEPSP) */

/*     The array KEEPSP is used to store whether or not sections */
/*     should be kept but presented as white space when the */
/*     page number and frequency imply that the section should */
/*     not be printed on a given page. */

/*     The array INVIS is used to keep track of whether or not */
/*     a section should be visible on the current page. */


/*     The variable ROW points to the position of the last */
/*     row in the body portion of the page where text was last */
/*     written.  PAGENO is the page number of the page that is */
/*     currently being filled. */


/*     The logical BODY is used to indicate whether the section */
/*     has been set to BODY since the last call to PAGRST to reset */
/*     the dynamic page attributes. */


/*     The logical DOPRMT is used to indicate whether or not a prompt */
/*     should be issued when the production of a page is finished. */


/*     Loop counter */


/*     Saved variables */


/*     Initial values */

    switch(n__) {
	case 1: goto L_pagrst;
	case 2: goto L_pagsft;
	case 3: goto L_pagset;
	case 4: goto L_pagsmk;
	case 5: goto L_pagscn;
	case 6: goto L_pagput;
	case 7: goto L_pagpmt;
	}

    return 0;
/* $Procedure      PAGRST (Page Reset) */

L_pagrst:
/* $ Abstract */

/*     Reset the page to page zero and empty all sections of */
/*     the page. */

/* $ Required_Reading */

/*     REPORTS */

/* $ Keywords */

/*     OUTPUT */
/*     TEXT */
/*     FORMATTING */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See the subroutine header */

/* $ Exceptions */

/*     None */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This entry point is used to reset the page manager */
/*     so that it may receive new section information and */
/*     so that the lines of text when output will start on the */
/*     first page of the sequence of pages. */

/*     This entry point should be called only prior to the beginning */
/*     of a sequence of page productions. */

/*     A call to this routine always halts production of the current */
/*     page.  No cleanup is performed.  In particular any footer */
/*     that was waiting to be output, will be elliminated and */
/*     not produced.  For this reason it is better to call the */
/*     soft reset PAGSFT (which will output any footers) prior to */
/*     calling this entry point if you have already begun production */
/*     of a document and want the last page of the document */
/*     to be finished prior to beginning a new document. */

/* $ Examples */

/*     See above. */

/* $ Restrictions */

/*     See particulars. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 9-JAN-1992 (WLT) */

/* -& */
    row = 0;
    pageno = 1;
    size[1] = 0;
    size[2] = 0;
    size[0] = 0;
    size[4] = 0;
    doprmt = FALSE_;
    didpmt = FALSE_;
    s_copy(respns, " ", (ftnlen)255, (ftnlen)1);
    wfactr = 0;
    body = FALSE_;
    return 0;
/* $Procedure      PAGSFT (Page Soft Reset) */

L_pagsft:
/* $ Abstract */

/*     Finish production of the current page, and empty all section */
/*     of the page. */

/* $ Required_Reading */

/*     REPORTS */

/* $ Keywords */

/*     OUTPUT */
/*     TEXT */
/*     FORMATTING */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See the subroutine header */

/* $ Exceptions */

/*     None */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This entry point is used to reset the page manager */
/*     so that a new style (header, title and footer, etc). */

/*     The page number is not altered. */

/*     This entry point differs from PAGRST in that it cleanly */
/*     finished the current page.  This routine should typically */
/*     be called after the last body text line has been sent to */
/*     the PAGE MANAGER. */

/*     To perform a complet reset, call the entry point PAGRST. */

/* $ Examples */

/*     See above. */

/* $ Restrictions */

/*     See particulars. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 9-JAN-1992 (WLT) */

/* -& */
    if (row > 0) {
	s_copy(myline, " ", (ftnlen)255, (ftnlen)1);
	while(row < size[3]) {
	    nspwln_(myline, pagewd);
	    ++row;
	}

/*        The user may want to have the page number appear */
/*        in the footer.  So we replace the PAGMRK by the */
/*        number if this is the case. */

	i__1 = need[2];
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (visibl[2]) {
		repmi_(footer + ((i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : 
			s_rnge("footer", i__2, "pagman_", (ftnlen)582)) * 255,
			 pagmrk, &pageno, myline, (ftnlen)255, (ftnlen)32, (
			ftnlen)255);
		nspwln_(myline, pagewd);
	    } else {
		nspwln_(myline, pagewd);
	    }
	}
	++pageno;
    }
    row = 0;
    size[1] = 0;
    size[2] = 0;
    size[0] = 0;
    size[4] = 0;
    doprmt = FALSE_;
    didpmt = FALSE_;
    s_copy(respns, " ", (ftnlen)255, (ftnlen)1);
    wfactr = 0;
    body = FALSE_;
    return 0;
/* $Procedure      PAGSET (Page Set attributes ) */

L_pagset:
/* $ Abstract */

/*     Set one of the global page attributes */

/* $ Required_Reading */

/*     REPORTS */

/* $ Keywords */

/*     OUTPUT */
/*     TEXT */
/*     FORMATTING */

/* $ Declarations */

/*     CHARACTER*(*)         WHICH */
/*     INTEGER               VALUE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     WHICH      I   indicates which attribute to set */
/*     VALUE      I   the value of the attribute */

/* $ Detailed_Input */

/*     WHICH      is the name of some attribute to set.  The acceptable */
/*                values are:  PAGEWIDTH, PAGEHEIGHT, HEADERFREQUENCY, */
/*                TITLEFREQUENCY,  SPACETITLE, NOSPACETITLE, SPACEHEADER */
/*                NOSPACEHEADER, SPACEFOOTER, NOSPACEFOOTER. */

/*     VALUE      is the value to assign to one of the page attributes. */
/*                In the case of any of the frequency attributes the */
/*                values carry the following implication: If the */
/*                frequency is less than zero, that section never */
/*                appears in the page. If the frequency is 0, that */
/*                section appears on the first page.  However it does */
/*                not appear on any other pages.  If the frequency is N */
/*                > 0 then the section appears on the first page and */
/*                every page of the form  1 + K*N where K is a positive */
/*                integer. */

/*                The values supplied for the SPACE/NOSPACE WAIT/NOWAIT */
/*                attributes are ignored.  The text of WHICH is used to */
/*                determine if blank lines should be used in place of */
/*                the text of the section it is not supposed to appear */
/*                in output. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See the subroutine header */

/* $ Exceptions */

/*     If one of the recognized values for WHICH is not entered the state */
/*     of the page manager will not change. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     See above. */

/* $ Restrictions */

/*    It is intended that this routine be called to set up the page */
/*    manager prior to the productio of pages.  However, user's may */
/*    call this routine to change page attributes at any time. */
/*    Nevertheless, due to the method by which pages are produced, the */
/*    affects of a call to this routine may be delayed. Once the body */
/*    of a new page has begun, all attributes but PAGEWIDTH are ignored */
/*    until the page has been completed according to the attributes */
/*    that were in effect when the body section of the page was begun. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 9-JAN-1992 (WLT) */

/* -& */
    if (s_cmp(which, "PAGEHEIGHT", which_len, (ftnlen)10) == 0) {
	pagesz = *value;
    } else if (s_cmp(which, "PAGEWIDTH", which_len, (ftnlen)9) == 0) {
	pagewd = *value;
    } else if (s_cmp(which, "HEADERFREQUENCY", which_len, (ftnlen)15) == 0) {
	freq[1] = *value;
    } else if (s_cmp(which, "TITLEFREQUENCY", which_len, (ftnlen)14) == 0) {
	freq[0] = *value;
    } else if (s_cmp(which, "FOOTERFREQUENCY", which_len, (ftnlen)15) == 0) {
	freq[2] = *value;
    } else if (s_cmp(which, "SPACETITLE", which_len, (ftnlen)10) == 0) {
	keepsp[0] = TRUE_;
    } else if (s_cmp(which, "NOSPACETITLE", which_len, (ftnlen)12) == 0) {
	keepsp[0] = FALSE_;
    } else if (s_cmp(which, "SPACEHEADER", which_len, (ftnlen)11) == 0) {
	keepsp[1] = TRUE_;
    } else if (s_cmp(which, "NOSPACEHEADER", which_len, (ftnlen)13) == 0) {
	keepsp[1] = FALSE_;
    } else if (s_cmp(which, "SPACEFOOTER", which_len, (ftnlen)11) == 0) {
	keepsp[2] = TRUE_;
    } else if (s_cmp(which, "NOSPACEFOOTER", which_len, (ftnlen)13) == 0) {
	keepsp[2] = FALSE_;
    } else if (s_cmp(which, "NOPAGEMARK", which_len, (ftnlen)10) == 0) {
	domark = FALSE_;
    } else if (s_cmp(which, "DOPAGEMARK", which_len, (ftnlen)10) == 0) {
	domark = TRUE_;
    } else if (s_cmp(which, "PROMPT", which_len, (ftnlen)6) == 0) {
	doprmt = TRUE_;
	wfactr = 1;
	size[4] = 1;
    } else if (s_cmp(which, "NOPROMPT", which_len, (ftnlen)8) == 0) {
	doprmt = FALSE_;
	didpmt = FALSE_;
	s_copy(respns, " ", (ftnlen)255, (ftnlen)1);
	wfactr = 0;
	size[4] = 0;
    }
    return 0;
/* $Procedure      PAGSMK (Page set page number marker ) */

L_pagsmk:
/* $ Abstract */

/*     Set the mark that will be replaced by the current page number */
/*     within the title and footer sections of a page. */

/* $ Required_Reading */

/*     REPORTS */

/* $ Keywords */

/*     OUTPUT */
/*     TEXT */
/*     FORMATTING */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     WHICH      I   mark to be replaced by current page number */

/* $ Detailed_Input */

/*     WHICH      is a string which when encountered as a substring */
/*                of a line of text in either the title or footer */
/*                section will be replaced by the current page number. */


/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See the subroutine header */

/* $ Exceptions */

/*     None */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This entry point is used to set the "mark" that */
/*     the page manager will recognize as the position to */
/*     fill in the current page number in either the title */
/*     or footer section of a page.   It has no effect in the */
/*     HEADER or BODY section of the document. */

/*     Usually you will want to set the page number mark at the */
/*     beginning of a document and leave this unchanged throughout */
/*     the production of the document. */

/*     The effect of a call to PAGSMK will begin on the next call */
/*     to PAGPUT. */


/* $ Examples */

/*     See above. */

/* $ Restrictions */

/*     See particulars. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 9-JAN-1992 (WLT) */

/* -& */
    s_copy(pagmrk, which, (ftnlen)32, which_len);
    pagmln = rtrim_(pagmrk, (ftnlen)32);
    domark = TRUE_;
    return 0;
/* $Procedure      PAGSCN (Page Section) */

L_pagscn:
/* $ Abstract */

/*     Set the section to which lines should be sent. */

/* $ Required_Reading */

/*     REPORTS */

/* $ Keywords */

/*     OUTPUT */
/*     TEXT */
/*     FORMATTING */

/* $ Declarations */

/*     CHARACTER*(*)         WHICH */
/*     CHARACTER*(*)         LINE */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     WHICH      I   indicates which section to send lines of text to */

/* $ Detailed_Input */

/*     WHICH      the section to which lines will be sent by */
/*                the entry point PAGPUT.  Valid choices for */
/*                WHICH are 'TITLE', 'HEADER', 'FOOTER' and 'BODY'. */
/*                The routine is case sensitive. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See the subroutine header */

/* $ Exceptions */

/*     If one of the recognized values is not entered, calls */
/*     to PAGPUT will have no effect. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     See above. */

/* $ Restrictions */

/*    None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 9-JAN-1992 (WLT) */

/* -& */
    s_copy(sectn, which, (ftnlen)32, which_len);
    body = s_cmp(sectn, "BODY", (ftnlen)32, (ftnlen)4) == 0;
    return 0;
/* $Procedure      PAGPUT (Page put a line of text ) */

L_pagput:
/* $ Abstract */

/*     Put a line of text in the current section of the current */
/*     page. */

/* $ Required_Reading */

/*     REPORTS */

/* $ Keywords */

/*     OUTPUT */
/*     TEXT */
/*     FORMATTING */

/* $ Declarations */

/*     CHARACTER*(*)         LINE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     LINE       I   a line of text to put on the current page */

/* $ Detailed_Input */

/*     LINE       is a line of text that should be output (eventually) */
/*                via the routine NSPWLN. */


/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See the subroutine header */

/* $ Exceptions */

/*     None */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This entry point is used prepare a line of text for output. */
/*     Whether the text is sent immediately to NSPWLN or is defered */
/*     depends upon which section is currently active. */

/*     If the current section is the TITLE, HEADER or FOOTER section */
/*     the line of text is simply buffered and output is defered */
/*     until the appropriate line of the body of the page is */
/*     output. */

/*     If the  current section if BODY, the line will be output */
/*     in the appropriate order along with any of the TITLE, */
/*     HEADER and FOOTER sections that should be output along with */
/*     it. */

/*     The calling program should ensure that if sections other than */
/*     the BODY section are to be written, that their text be */
/*     established prior to calling this entry point when the body */
/*     section is active. */

/* $ Examples */

/*     See above. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 9-JAN-1992 (WLT) */

/* -& */

/*     We handle the TITLE, HEADER and FOOTER sections first. */

    didpmt = FALSE_;
    if (! body) {
	if (s_cmp(sectn, "TITLE", (ftnlen)32, (ftnlen)5) == 0) {
/* Computing MIN */
	    i__1 = 10, i__2 = size[0] + 1;
	    size[0] = min(i__1,i__2);
	    s_copy(title + ((i__1 = size[0] - 1) < 10 && 0 <= i__1 ? i__1 : 
		    s_rnge("title", i__1, "pagman_", (ftnlen)1067)) * 255, 
		    line, (ftnlen)255, line_len);
	} else if (s_cmp(sectn, "HEADER", (ftnlen)32, (ftnlen)6) == 0) {
/* Computing MIN */
	    i__1 = 15, i__2 = size[1] + 1;
	    size[1] = min(i__1,i__2);
	    s_copy(header + ((i__1 = size[1] - 1) < 15 && 0 <= i__1 ? i__1 : 
		    s_rnge("header", i__1, "pagman_", (ftnlen)1072)) * 255, 
		    line, (ftnlen)255, line_len);
	} else if (s_cmp(sectn, "FOOTER", (ftnlen)32, (ftnlen)6) == 0) {
/* Computing MIN */
	    i__1 = 10, i__2 = size[2] + 1;
	    size[2] = min(i__1,i__2);
	    s_copy(footer + ((i__1 = size[2] - 1) < 10 && 0 <= i__1 ? i__1 : 
		    s_rnge("footer", i__1, "pagman_", (ftnlen)1077)) * 255, 
		    line, (ftnlen)255, line_len);
	} else if (s_cmp(sectn, "PROMPT", (ftnlen)32, (ftnlen)6) == 0) {
	    size[4] = 1;
	    s_copy(questn, line, (ftnlen)255, line_len);
	    qlenth = rtrim_(line, line_len) + 1;
	}
	return 0;
    }

/*     The only way to get to this point is if we are working on */
/*     the body section of a page.  If the row number is zero, then */
/*     we need to see how much room is available on this page for */
/*     the body.  And, if appropriate output the TITLE and */
/*     HEADER sections of this page. */

    if (row == 0) {

/*        We need to compute how much room is available */
/*        for the body of this page. */

	for (i__ = 1; i__ <= 3; ++i__) {

/*           First determine how much room is needed for */
/*           this section and whether or not it will be */
/*           visible on this page if we simply fill it with */
/*           blanks. */

	    if (freq[(i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge("freq",
		     i__1, "pagman_", (ftnlen)1109)] < 0) {
		need[(i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge("need",
			 i__1, "pagman_", (ftnlen)1110)] = 0;
		visibl[(i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge(
			"visibl", i__1, "pagman_", (ftnlen)1111)] = FALSE_;
	    } else if (pageno == 1) {
		need[(i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge("need",
			 i__1, "pagman_", (ftnlen)1113)] = size[(i__2 = i__ - 
			1) < 5 && 0 <= i__2 ? i__2 : s_rnge("size", i__2, 
			"pagman_", (ftnlen)1113)];
		visibl[(i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge(
			"visibl", i__1, "pagman_", (ftnlen)1114)] = TRUE_;
	    } else if (freq[(i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge(
		    "freq", i__1, "pagman_", (ftnlen)1115)] == 0) {
		need[(i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge("need",
			 i__1, "pagman_", (ftnlen)1116)] = 0;
		visibl[(i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge(
			"visibl", i__1, "pagman_", (ftnlen)1117)] = TRUE_;
	    } else if (freq[(i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge(
		    "freq", i__1, "pagman_", (ftnlen)1118)] == 1) {
		need[(i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge("need",
			 i__1, "pagman_", (ftnlen)1119)] = size[(i__2 = i__ - 
			1) < 5 && 0 <= i__2 ? i__2 : s_rnge("size", i__2, 
			"pagman_", (ftnlen)1119)];
		visibl[(i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge(
			"visibl", i__1, "pagman_", (ftnlen)1120)] = TRUE_;
	    } else if (pageno % freq[(i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 
		    : s_rnge("freq", i__1, "pagman_", (ftnlen)1121)] == 1) {
		need[(i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge("need",
			 i__1, "pagman_", (ftnlen)1122)] = size[(i__2 = i__ - 
			1) < 5 && 0 <= i__2 ? i__2 : s_rnge("size", i__2, 
			"pagman_", (ftnlen)1122)];
		visibl[(i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge(
			"visibl", i__1, "pagman_", (ftnlen)1123)] = TRUE_;
	    } else {
		need[(i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge("need",
			 i__1, "pagman_", (ftnlen)1125)] = 0;
		visibl[(i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge(
			"visibl", i__1, "pagman_", (ftnlen)1126)] = TRUE_;
	    }
	    if (keepsp[(i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge(
		    "keepsp", i__1, "pagman_", (ftnlen)1129)]) {
		need[(i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge("need",
			 i__1, "pagman_", (ftnlen)1130)] = size[(i__2 = i__ - 
			1) < 5 && 0 <= i__2 ? i__2 : s_rnge("size", i__2, 
			"pagman_", (ftnlen)1130)];
	    }
	}
	size[3] = pagesz - need[0] - need[1] - need[2] - wfactr * size[4];

/*        We haven't yet written a line in the body of the */
/*        page, we will write out the title and header sections */
/*        (provided we are on the right page number) */

/*        We allow for the possibility that the user might */
/*        place the page number in the title section. */

	s_copy(myline, " ", (ftnlen)255, (ftnlen)1);
	i__1 = need[0];
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (visibl[0]) {
		if (domark) {
		    repmi_(title + ((i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 
			    : s_rnge("title", i__2, "pagman_", (ftnlen)1150)) 
			    * 255, pagmrk, &pageno, myline, (ftnlen)255, 
			    pagmln, (ftnlen)255);
		    nspwln_(myline, pagewd);
		} else {
		    nspwln_(title + ((i__2 = i__ - 1) < 10 && 0 <= i__2 ? 
			    i__2 : s_rnge("title", i__2, "pagman_", (ftnlen)
			    1154)) * 255, pagewd);
		}
	    } else {
		nspwln_(myline, pagewd);
	    }
	}

/*        Next output whatever portion of the header section is */
/*        appropriate. */

	s_copy(myline, " ", (ftnlen)255, (ftnlen)1);
	i__1 = need[1];
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (visibl[1]) {
		nspwln_(header + ((i__2 = i__ - 1) < 15 && 0 <= i__2 ? i__2 : 
			s_rnge("header", i__2, "pagman_", (ftnlen)1169)) * 
			255, pagewd);
	    } else {
		nspwln_(myline, pagewd);
	    }
	}
    }

/*     Write the line and update the number of lines we */
/*     have written so far. */

    ++row;
    s_copy(myline, line, (ftnlen)255, line_len);
    nspwln_(myline, pagewd);

/*     If we reached the end of the body section, write out */
/*     the footer (provided we are on the right page). And */
/*     update the page number. */

    if (row == size[3]) {

/*        The user may want to have the page number appear */
/*        in the footer.  So we replace the PAGMRK by the */
/*        number if this is the case. */

	s_copy(myline, " ", (ftnlen)255, (ftnlen)1);
	i__1 = need[2];
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (visibl[2]) {
		if (domark) {
		    repmi_(footer + ((i__2 = i__ - 1) < 10 && 0 <= i__2 ? 
			    i__2 : s_rnge("footer", i__2, "pagman_", (ftnlen)
			    1201)) * 255, pagmrk, &pageno, myline, (ftnlen)
			    255, pagmln, (ftnlen)255);
		    nspwln_(myline, pagewd);
		} else {
		    nspwln_(footer + ((i__2 = i__ - 1) < 10 && 0 <= i__2 ? 
			    i__2 : s_rnge("footer", i__2, "pagman_", (ftnlen)
			    1205)) * 255, pagewd);
		}
	    } else {
		nspwln_(myline, pagewd);
	    }
	}

/*        Advance the page number and reset the row to zero. */
/*        (we won't have written anything in the body of the */
/*        next page until later.) */

	++pageno;
	row = 0;
	if (doprmt) {
	    prompt_(questn, respns, qlenth, (ftnlen)255);
	    didpmt = TRUE_;
	}
    }
    return 0;
/* $Procedure      PAGPMT ( Page prompt returned ) */

L_pagpmt:
/* $ Abstract */

/*    Determine if a prompt issued and a value returned on the last */
/*    call to PAGPUT. */

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

/*     PAGE MANAGER */

/* $ Declarations */

/*     INTEGER               VALUE */
/*     CHARACTER*(*)         LINE */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     VALUE      O   1 if a prompt was entered, 0 otherwise. */
/*     LINE       O   The value of the prompt supplied */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     VALUE      is an integer indicating whether or not a prompt was */
/*                displayed and a value returned.  If no prompt was */
/*                issued on the last call to PAGPUT, VALUE will have the */
/*                value zero.  Otherwise VALUE will have some non-zero */
/*                value. */

/*     LINE       is the value of the prompt returned if there was one. */
/*                Otherwise a blank is returned. */


/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This entry point returns information about what happened in the */
/*     last call to PAGPUT.  If a page was finished and a prompt was */
/*     displayed and the user responded, this routine will return two */
/*     a non-zero integer for VALUE and will place the results of the */
/*     prompt in the string LINE. */

/*     Note that this routine will return the same results until some */
/*     call to PAGPUT is made again. */

/* $ Examples */

/*     Suppose that you are using the page manager to send output */
/*     to some device.  But that you want to allow the user to */
/*     pause in the course of sending the output. The following */
/*     illustrates how you would do this. */

/*     CALL getnext  ( LINE, MORE ) */

/*     DO WHILE ( MORE ) */

/*        CALL PAGPUT ( LINE ) */
/*        CALL PAGPMT ( VALUE, RESPNS ) */

/*        IF ( VALUE .NE. 0 ) */

/*           take some action concerning RESPNS */

/*        END IF */

/*        CALL getnext  ( LINE, MORE ) */

/*     END DO */



/*     Alternatively you might like to just have the page manager */
/*     wait for the user after a page has been finished.  To do this */
/*     you could set things up as follows. */

/*     CALL PAGSET ( 'PROMPT', 0 ) */
/*     CALL PAGSCN ( 'PROMPT'    ) */
/*     CALL PAGPUT ( '(Hit Return to Continue) >' */

/*     CALL getnext  ( LINE, MORE ) */

/*     DO WHILE ( MORE ) */

/*        CALL PAGPUT  ( LINE ) */
/*        CALL getnext ( LINE, MORE ) */

/*     END DO */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*       W.L. Taber      (JPL) */

/* $ Literature_References */

/*       None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 16-AUG-1995 (WLT) */


/* -& */
/* $ Index_Entries */

/*     See if the last line sent resulted in a prompt */

/* -& */
    if (didpmt) {
	*value = 1;
	s_copy(line, respns, line_len, (ftnlen)255);
    } else {
	*value = 0;
	s_copy(line, " ", line_len, (ftnlen)1);
    }
    return 0;
} /* pagman_ */

/* Subroutine */ int pagman_(char *which, char *line, integer *value, ftnlen 
	which_len, ftnlen line_len)
{
    return pagman_0_(0, which, line, value, which_len, line_len);
    }

/* Subroutine */ int pagrst_(void)
{
    return pagman_0_(1, (char *)0, (char *)0, (integer *)0, (ftnint)0, (
	    ftnint)0);
    }

/* Subroutine */ int pagsft_(void)
{
    return pagman_0_(2, (char *)0, (char *)0, (integer *)0, (ftnint)0, (
	    ftnint)0);
    }

/* Subroutine */ int pagset_(char *which, integer *value, ftnlen which_len)
{
    return pagman_0_(3, which, (char *)0, value, which_len, (ftnint)0);
    }

/* Subroutine */ int pagsmk_(char *which, ftnlen which_len)
{
    return pagman_0_(4, which, (char *)0, (integer *)0, which_len, (ftnint)0);
    }

/* Subroutine */ int pagscn_(char *which, ftnlen which_len)
{
    return pagman_0_(5, which, (char *)0, (integer *)0, which_len, (ftnint)0);
    }

/* Subroutine */ int pagput_(char *line, ftnlen line_len)
{
    return pagman_0_(6, (char *)0, line, (integer *)0, (ftnint)0, line_len);
    }

/* Subroutine */ int pagpmt_(integer *value, char *line, ftnlen line_len)
{
    return pagman_0_(7, (char *)0, line, value, (ftnint)0, line_len);
    }

