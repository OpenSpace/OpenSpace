/* hlpmen.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__100 = 100;
static integer c__300 = 300;
static integer c__500 = 500;
static integer c__1 = 1;
static integer c__3 = 3;
static integer c__0 = 0;
static integer c__2 = 2;
static integer c__4 = 4;

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

/* Subroutine */ int hlpmen_0_(int n__, char *topic, ftnlen topic_len)
{
    /* Initialized data */

    static char optnam[2*19] = "P " "Q " "1 " "2 " "3 " "4 " "5 " "6 " "7 " 
	    "8 " "9 " "A " "B " "C " "D " "E " "F " "G " "H ";

    /* System generated locals */
    address a__1[2];
    integer i__1, i__2, i__3, i__4[2];

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int getopt_2__(char *, integer *, integer *, char 
	    *, char *, integer *, integer *, ftnlen, ftnlen, ftnlen);
    static char name__[32], help[80*506];
    static integer item, indx, last, size, nopt, ntxt, b, i__, j;
    extern integer cardc_(char *, ftnlen);
    static integer n;
    extern logical batch_(void), elemc_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int lcase_(char *, char *, ftnlen, ftnlen);
    extern logical match_(char *, char *, ftnlen, ftnlen);
    static char lname[32];
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen), repmc_(
	    char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, ftnlen);
    static logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    static char ordnl[32];
    static logical dopmt;
    extern integer ltrim_(char *, ftnlen);
    static logical lstat[3], sstat[3];
    static char style[80];
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    static integer menbeg;
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen);
    static integer pagewd, pageht;
    extern /* Subroutine */ int pagscn_(char *, ftnlen);
    static integer dothem;
    static logical didpmt;
    static integer opagwd;
    static char messge[200];
    extern /* Subroutine */ int params_(char *, char *, integer *, ftnlen, 
	    ftnlen);
    static logical ftitle;
    extern /* Subroutine */ int pagset_(char *, integer *, ftnlen), pagpmt_(
	    logical *, char *, ftnlen);
    static integer oindnt;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    extern integer frstnb_(char *, ftnlen);
    static integer olndnt;
    static char visitd[32*20];
    static integer olskip, oiskip, txinfo[3], option, txtdat[306];
    static char respns[32], txtnam[32*106];
    static integer ostart;
    extern logical return_(void);
    extern /* Subroutine */ int nspwln_();
    extern /* Subroutine */ int ssizec_(integer *, char *, ftnlen), ssizei_(
	    integer *, integer *), hlpsiz_(integer *), hlptxt_(integer *, 
	    char *, ftnlen), cmprss_(char *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen), intord_(integer *, char *, ftnlen), 
	    setmsg_(char *, ftnlen), chkout_(char *, ftnlen), sygeti_(char *, 
	    char *, integer *, integer *, integer *, integer *, logical *, 
	    ftnlen, ftnlen), syputi_(char *, integer *, integer *, char *, 
	    integer *, integer *, ftnlen, ftnlen), nspmrg_(char *, ftnlen), 
	    suffix_(char *, integer *, char *, ftnlen, ftnlen), prefix_(char *
	    , integer *, char *, ftnlen, ftnlen), pagrst_(void);
    static char opttxt[32*19];
    extern /* Subroutine */ int pagput_(char *, ftnlen), subtex_(char *, 
	    ftnlen);
    static integer txtptr[106];
    extern /* Subroutine */ int nspgst_(char *, logical *, ftnlen), nspioh_(
	    char *, ftnlen), prompt_(char *, char *, ftnlen, ftnlen), nsppst_(
	    char *, logical *, ftnlen), bbgeti_1__(char *, char *, integer *, 
	    integer *, ftnlen, ftnlen), nicepr_1__(char *, char *, U_fp, 
	    ftnlen, ftnlen);


/*     Version 2.0 September 8, 1995 */

/*        Increased MAXLIN from 200 to 500. */


/*     Spicelib Functions */


/*     We are going to keep track of the text via pointers in a */
/*     symbol table. We shall use parameters to point to the various */
/*     components */

    switch(n__) {
	case 1: goto L_hlpint;
	case 2: goto L_dohelp;
	}

    return 0;

L_hlpint:
    if (return_()) {
	return 0;
    }
    chkin_("HLPINT", (ftnlen)6);
    ssizec_(&c__100, txtnam, (ftnlen)32);
    ssizei_(&c__100, txtptr);
    ssizei_(&c__300, txtdat);
    hlpsiz_(&ntxt);
    i__1 = ntxt;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        For each help screen determine it's menu name, the location */
/*        of the @@ characters, and the location of the associated menu. */

/*        Fetch the I'th block of help text. */

	ssizec_(&c__500, help, (ftnlen)80);
	hlptxt_(&i__, help, (ftnlen)80);

/*        We have not located the title yet and we don't know */
/*        the number of lines of text present so far.  Note that */
/*        if we don't find a title, this item will become */
/*        invisible from the point of view of the help system. */

	ftitle = FALSE_;
	j = 1;
	while(j <= cardc_(help, (ftnlen)80)) {
	    b = ltrim_(help + ((i__2 = j + 5) < 506 && 0 <= i__2 ? i__2 : 
		    s_rnge("help", i__2, "hlpmen_", (ftnlen)202)) * 80, (
		    ftnlen)80);
	    if (s_cmp(help + (((i__2 = j + 5) < 506 && 0 <= i__2 ? i__2 : 
		    s_rnge("help", i__2, "hlpmen_", (ftnlen)204)) * 80 + (b - 
		    1)), "@@", (ftnlen)2, (ftnlen)2) == 0) {
		ftitle = TRUE_;
		i__3 = b + 1;
		cmprss_(" ", &c__1, help + (((i__2 = j + 5) < 506 && 0 <= 
			i__2 ? i__2 : s_rnge("help", i__2, "hlpmen_", (ftnlen)
			207)) * 80 + i__3), name__, (ftnlen)1, 80 - i__3, (
			ftnlen)32);
		ucase_(name__, name__, (ftnlen)32, (ftnlen)32);
		ljust_(name__, name__, (ftnlen)32, (ftnlen)32);
		txinfo[1] = i__;
		txinfo[2] = j - 1;
		txinfo[0] = j + 1;

/*              Store the index and the length of this text in */
/*              the integer text data table. */

		syputi_(name__, txinfo, &c__3, txtnam, txtptr, txtdat, (
			ftnlen)32, (ftnlen)32);

/*              Set J to point past the end of the text so that */
/*              we can finish the loop now. */

		j = cardc_(help, (ftnlen)80) + 1;
	    }
	    ++j;
	}

/*        The following block is simply a courtesy to developers */
/*        so they can determine easily if at least every text */
/*        block has a title.  Once a text system has been "debugged" */
/*        this block should never get exercised. */

	if (! ftitle) {
	    intord_(&i__, ordnl, (ftnlen)32);
	    lcase_(ordnl, ordnl, (ftnlen)32, (ftnlen)32);
	    setmsg_("The # block of help text does not have a title. ", (
		    ftnlen)48);
	    errch_("#", ordnl, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(UNTITLEDHELP)", (ftnlen)19);
	    chkout_("HLPINT", (ftnlen)6);
	    return 0;
	}
    }

/*     Now we perform another courtesy check for the developer */
/*     to make sure every menu item points to something in */
/*     the help list. */

    i__1 = cardc_(txtnam, (ftnlen)32);
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_copy(name__, txtnam + (((i__2 = i__ + 5) < 106 && 0 <= i__2 ? i__2 :
		 s_rnge("txtnam", i__2, "hlpmen_", (ftnlen)262)) << 5), (
		ftnlen)32, (ftnlen)32);
	sygeti_(name__, txtnam, txtptr, txtdat, &n, txinfo, &found, (ftnlen)
		32, (ftnlen)32);
	item = txinfo[1];
	menbeg = txinfo[0];
	scardc_(&c__500, help, (ftnlen)80);
	hlptxt_(&item, help, (ftnlen)80);
	i__2 = cardc_(help, (ftnlen)80);
	for (j = menbeg; j <= i__2; ++j) {
	    ucase_(help + ((i__3 = j + 5) < 506 && 0 <= i__3 ? i__3 : s_rnge(
		    "help", i__3, "hlpmen_", (ftnlen)275)) * 80, lname, (
		    ftnlen)80, (ftnlen)32);
	    ljust_(lname, lname, (ftnlen)32, (ftnlen)32);
	    cmprss_(" ", &c__1, lname, lname, (ftnlen)1, (ftnlen)32, (ftnlen)
		    32);
	    if (! elemc_(lname, txtnam, (ftnlen)32, (ftnlen)32) && s_cmp(
		    lname, " ", (ftnlen)32, (ftnlen)1) != 0 && s_cmp(lname, 
		    "QUIT HELP", (ftnlen)32, (ftnlen)9) != 0) {
		s_copy(messge, "Under the topic '#', (the #'th text block) t"
			"he link menu item # does not point to a recognized h"
			"elp section. ", (ftnlen)200, (ftnlen)109);
		repmc_(messge, "#", name__, messge, (ftnlen)200, (ftnlen)1, (
			ftnlen)32, (ftnlen)200);
		repmi_(messge, "#", &item, messge, (ftnlen)200, (ftnlen)1, (
			ftnlen)200);
		repmc_(messge, "#", lname, messge, (ftnlen)200, (ftnlen)1, (
			ftnlen)32, (ftnlen)200);
		nspmrg_(style, (ftnlen)80);
		nicepr_1__(messge, style, (U_fp)nspwln_, (ftnlen)200, (ftnlen)
			80);
	    }
	}
    }
    chkout_("HLPINT", (ftnlen)6);
    return 0;

/*     The routine DOHELP does the real work of displaying menus and */
/*     help information. */


L_dohelp:

/*     Fetch the margins to be used for displaying text */

    last = 1;
    s_copy(visitd + (((i__1 = last - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge(
	    "visitd", i__1, "hlpmen_", (ftnlen)309)) << 5), "HELP", (ftnlen)
	    32, (ftnlen)4);
    if (s_cmp(topic, " ", topic_len, (ftnlen)1) == 0) {
	s_copy(name__, "HELP", (ftnlen)32, (ftnlen)4);
    } else {
	cmprss_(" ", &c__1, topic, name__, (ftnlen)1, topic_len, (ftnlen)32);
	ljust_(name__, name__, (ftnlen)32, (ftnlen)32);
	ucase_(name__, name__, (ftnlen)32, (ftnlen)32);

/*        If we get an exact match, we don't spend any more time */
/*        on this item. */

	if (elemc_(name__, txtnam, (ftnlen)32, (ftnlen)32)) {
	    i__ = cardc_(txtnam, (ftnlen)32) + 1;
	    found = TRUE_;
	} else {

/*           Hmmm. No exact match.  Append a wild card to the end */
/*           of name and look for a pattern match. */

	    i__ = 1;
	    suffix_("*", &c__0, name__, (ftnlen)1, (ftnlen)32);
	}
	while(i__ <= cardc_(txtnam, (ftnlen)32)) {
	    if (match_(txtnam + (((i__1 = i__ + 5) < 106 && 0 <= i__1 ? i__1 :
		     s_rnge("txtnam", i__1, "hlpmen_", (ftnlen)335)) << 5), 
		    name__, (ftnlen)32, (ftnlen)32)) {
		s_copy(name__, txtnam + (((i__1 = i__ + 5) < 106 && 0 <= i__1 
			? i__1 : s_rnge("txtnam", i__1, "hlpmen_", (ftnlen)
			336)) << 5), (ftnlen)32, (ftnlen)32);
		i__ = cardc_(txtnam, (ftnlen)32);
		found = TRUE_;
	    }
	    ++i__;
	}
	if (! found) {

/*           Still no match.  Put a wild card on the front of name */
/*           and look for a match. */

	    prefix_("*", &c__0, name__, (ftnlen)1, (ftnlen)32);
	    i__ = 1;
	    while(i__ <= cardc_(txtnam, (ftnlen)32)) {
		if (match_(txtnam + (((i__1 = i__ + 5) < 106 && 0 <= i__1 ? 
			i__1 : s_rnge("txtnam", i__1, "hlpmen_", (ftnlen)352))
			 << 5), name__, (ftnlen)32, (ftnlen)32)) {
		    s_copy(name__, txtnam + (((i__1 = i__ + 5) < 106 && 0 <= 
			    i__1 ? i__1 : s_rnge("txtnam", i__1, "hlpmen_", (
			    ftnlen)353)) << 5), (ftnlen)32, (ftnlen)32);
		    i__ = cardc_(txtnam, (ftnlen)32);
		    found = TRUE_;
		}
		++i__;
	    }
	}
    }
    if (s_cmp(name__, "HELP", (ftnlen)32, (ftnlen)4) != 0) {
	++last;
	s_copy(visitd + (((i__1 = last - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge(
		"visitd", i__1, "hlpmen_", (ftnlen)366)) << 5), name__, (
		ftnlen)32, (ftnlen)32);
    }

/*     We begin help by asking for the top level menu HELP. */

    sygeti_(name__, txtnam, txtptr, txtdat, &n, txinfo, &found, (ftnlen)32, (
	    ftnlen)32);

/*     Get the current page settings. */

    dothem = 0;
    bbgeti_1__("COPY", "PAGEWIDTH", &n, &pagewd, (ftnlen)4, (ftnlen)9);
    bbgeti_1__("COPY", "PAGEHEIGHT", &n, &pageht, (ftnlen)4, (ftnlen)10);
    bbgeti_1__("COPY", "HELPPROMPT", &n, &dothem, (ftnlen)4, (ftnlen)10);
    dopmt = dothem != 0;
    params_("GET", "PAGEWIDTH", &opagwd, (ftnlen)3, (ftnlen)9);
    params_("GET", "LEFTSKIP", &olskip, (ftnlen)3, (ftnlen)8);
    params_("GET", "LITERALINDENT", &olndnt, (ftnlen)3, (ftnlen)13);
    params_("GET", "ITEMINDENT", &oindnt, (ftnlen)3, (ftnlen)10);
    params_("GET", "ITEMSKIP", &oiskip, (ftnlen)3, (ftnlen)8);
    params_("SET", "PAGEWIDTH", &pagewd, (ftnlen)3, (ftnlen)9);
    params_("SET", "LEFTSKIP", &c__3, (ftnlen)3, (ftnlen)8);
    params_("SET", "LITERALINDENT", &c__2, (ftnlen)3, (ftnlen)13);
    params_("SET", "ITEMINDENT", &c__4, (ftnlen)3, (ftnlen)10);
    params_("SET", "ITEMSKIP", &c__2, (ftnlen)3, (ftnlen)8);
    while(found) {

/*        Get the various attributes associated with this help */
/*        item. */

	indx = txinfo[1];
	size = txinfo[2];
	menbeg = txinfo[0];

/*        Fetch the text associated with this item. */

	ssizec_(&c__500, help, (ftnlen)80);
	hlptxt_(&indx, help, (ftnlen)80);

/*        Extract the links menu from the end of the help text. */

/*        Note that if we have previouly buffered topics we */
/*        add a n option to the menu to return to the previous */
/*        topic.  We also need to adjust where the OPTNAMs begin */
/*        when we create our menu.  We use OSTART to do this. */

	if (last > 1) {
	    nopt = 1;
	    ostart = 1;
	    s_copy(opttxt, "Previous Topic", (ftnlen)32, (ftnlen)14);
	} else {
	    ostart = 2;
	    nopt = 0;
	}
	i__1 = cardc_(help, (ftnlen)80);
	for (j = menbeg; j <= i__1; ++j) {
	    if (s_cmp(help + ((i__2 = j + 5) < 506 && 0 <= i__2 ? i__2 : 
		    s_rnge("help", i__2, "hlpmen_", (ftnlen)434)) * 80, " ", (
		    ftnlen)80, (ftnlen)1) != 0) {
		++nopt;
		s_copy(opttxt + (((i__2 = nopt - 1) < 19 && 0 <= i__2 ? i__2 :
			 s_rnge("opttxt", i__2, "hlpmen_", (ftnlen)436)) << 5)
			, help + ((i__3 = j + 5) < 506 && 0 <= i__3 ? i__3 : 
			s_rnge("help", i__3, "hlpmen_", (ftnlen)436)) * 80, (
			ftnlen)32, (ftnlen)80);
	    }
	}

/*        If there is some text associated with this item, now */
/*        is the time to display it. */

	if (size > 0) {

/*           Set up the page manager for receiving output. */

	    pagrst_();
	    pagset_("TITLEFREQUENCY", &c__0, (ftnlen)14);
	    pagset_("PAGEWIDTH", &pagewd, (ftnlen)9);
	    pagset_("PAGEHEIGHT", &pageht, (ftnlen)10);
	    pagset_("NOSPACETITLE", &c__0, (ftnlen)12);
	    pagset_("NOSPACEHEADER", &c__0, (ftnlen)13);
	    pagset_("NOSPACEFOOTER", &c__0, (ftnlen)13);
	    if (dopmt) {
		pagset_("PROMPT", &c__1, (ftnlen)6);
		pagscn_("PROMPT", (ftnlen)6);
		pagput_("---", (ftnlen)3);
	    }

/*           The title for this menu is the topic listing */
/*           that appears in the text.  This starts with */
/*           the characters '@@' which we don't want to display */

	    b = frstnb_(help + ((i__1 = menbeg + 4) < 506 && 0 <= i__1 ? i__1 
		    : s_rnge("help", i__1, "hlpmen_", (ftnlen)467)) * 80, (
		    ftnlen)80);
	    pagscn_("TITLE", (ftnlen)5);
	    pagput_(" ", (ftnlen)1);
	    i__2 = b + 1;
	    pagput_(help + (((i__1 = menbeg + 4) < 506 && 0 <= i__1 ? i__1 : 
		    s_rnge("help", i__1, "hlpmen_", (ftnlen)470)) * 80 + i__2)
		    , 80 - i__2);
	    pagput_(" ", (ftnlen)1);

/*           That's it.  The page settings are all in order */
/*           Set the page section to BODY so that the stuff */
/*           created by SUBTEX will be displayed appropriately. */

	    pagscn_("BODY", (ftnlen)4);

/*           Set the cardinality of the HELP buffer so that */
/*           stuff starting with the title is ignored. */

	    scardc_(&size, help, (ftnlen)80);
	    subtex_(help, (ftnlen)80);
	}

/*        See if the last line sent caused a prompt to be issued. */
/*        If so, we won't have to issue one.  Also we reset the */
/*        page so that others won't have to deal with any debris */
/*        left around by this routine. */

	pagpmt_(&didpmt, respns, (ftnlen)32);
	pagrst_();
	if (size > 0 && dopmt && ! didpmt) {

/*           Issue our own prompt since we didn't do one last */
/*           time. (This gives the user a chance to read the */
/*           output before going on to the next menu.) */

	    nspgst_("LOG", lstat, (ftnlen)3);
	    nspgst_("SAVE", sstat, (ftnlen)4);
	    nspioh_("LOG", (ftnlen)3);
	    nspioh_("SAVE", (ftnlen)4);
	    prompt_("---", respns, (ftnlen)3, (ftnlen)32);
	    nsppst_("LOG", lstat, (ftnlen)3);
	    nsppst_("SAVE", sstat, (ftnlen)4);
	}

/*        If there is any menu to display showing related options, */
/*        now is the time to display it. */

	if (nopt > 1) {
	    if (batch_()) {
		return 0;
	    }
	    getopt_2__(" ", &c__1, &nopt, optnam + (((i__1 = ostart - 1) < 19 
		    && 0 <= i__1 ? i__1 : s_rnge("optnam", i__1, "hlpmen_", (
		    ftnlen)526)) << 1), opttxt, &c__3, &option, (ftnlen)1, (
		    ftnlen)2, (ftnlen)32);

/*           Fetch the help data for the user's selection. */
/*           First normalize the topic name. */

	    ucase_(opttxt + (((i__1 = option - 1) < 19 && 0 <= i__1 ? i__1 : 
		    s_rnge("opttxt", i__1, "hlpmen_", (ftnlen)532)) << 5), 
		    name__, (ftnlen)32, (ftnlen)32);
	    ljust_(name__, name__, (ftnlen)32, (ftnlen)32);
	    cmprss_(" ", &c__1, name__, name__, (ftnlen)1, (ftnlen)32, (
		    ftnlen)32);

/*           We keep a buffer of previouly visited topics.  If */
/*           the user requests the previous topic we need to */
/*           get it and look up the text information for it. */

/*           Also the number of previous topics is now smaller. */

	    if (s_cmp(name__, "PREVIOUS TOPIC", (ftnlen)32, (ftnlen)14) == 0) 
		    {
/* Computing MAX */
		i__1 = 1, i__2 = last - 1;
		last = max(i__1,i__2);
		s_copy(name__, visitd + (((i__1 = last - 1) < 20 && 0 <= i__1 
			? i__1 : s_rnge("visitd", i__1, "hlpmen_", (ftnlen)
			545)) << 5), (ftnlen)32, (ftnlen)32);
		sygeti_(name__, txtnam, txtptr, txtdat, &n, txinfo, &found, (
			ftnlen)32, (ftnlen)32);
	    } else {

/*              This is some other topic, look it up and put it */
/*              on the buffer of visited topics. */

		sygeti_(name__, txtnam, txtptr, txtdat, &n, txinfo, &found, (
			ftnlen)32, (ftnlen)32);
		++last;
		if (last > 20) {
		    for (i__ = 3; i__ <= 20; ++i__) {
			s_copy(visitd + (((i__1 = i__ - 2) < 20 && 0 <= i__1 ?
				 i__1 : s_rnge("visitd", i__1, "hlpmen_", (
				ftnlen)564)) << 5), visitd + (((i__2 = i__ - 
				1) < 20 && 0 <= i__2 ? i__2 : s_rnge("visitd",
				 i__2, "hlpmen_", (ftnlen)564)) << 5), (
				ftnlen)32, (ftnlen)32);
		    }
		    last = 20;
		}
		s_copy(visitd + (((i__1 = last - 1) < 20 && 0 <= i__1 ? i__1 :
			 s_rnge("visitd", i__1, "hlpmen_", (ftnlen)571)) << 5)
			, name__, (ftnlen)32, (ftnlen)32);
	    }
	} else {
	    s_copy(name__, "QUIT HELP", (ftnlen)32, (ftnlen)9);
	    found = FALSE_;
	}
    }
    if (s_cmp(name__, "QUIT HELP", (ftnlen)32, (ftnlen)9) != 0) {
/* Writing concatenation */
	i__4[0] = 36, a__1[0] = "Sorry, but no help is available for ";
	i__4[1] = 32, a__1[1] = name__;
	s_cat(messge, a__1, i__4, &c__2, (ftnlen)200);
	nspmrg_(style, (ftnlen)80);
	nicepr_1__(messge, style, (U_fp)nspwln_, (ftnlen)200, (ftnlen)80);
    }
    params_("SET", "PAGEWIDTH", &opagwd, (ftnlen)3, (ftnlen)9);
    params_("SET", "LEFTSKIP", &olskip, (ftnlen)3, (ftnlen)8);
    params_("SET", "LITERALINDENT", &olndnt, (ftnlen)3, (ftnlen)13);
    params_("SET", "ITEMINDENT", &oindnt, (ftnlen)3, (ftnlen)10);
    params_("SET", "ITEMSKIP", &oiskip, (ftnlen)3, (ftnlen)8);
    return 0;
} /* hlpmen_ */

/* Subroutine */ int hlpmen_(char *topic, ftnlen topic_len)
{
    return hlpmen_0_(0, topic, topic_len);
    }

/* Subroutine */ int hlpint_(void)
{
    return hlpmen_0_(1, (char *)0, (ftnint)0);
    }

/* Subroutine */ int dohelp_(char *topic, ftnlen topic_len)
{
    return hlpmen_0_(2, topic, topic_len);
    }

