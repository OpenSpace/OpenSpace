/* cparse_2.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__300 = 300;
static integer c__1 = 1;
static integer c__0 = 0;

/* $Procedure      CPARSE (Command Parse) */
/* Subroutine */ int cparse_0_(int n__, char *kwdsym, integer *kwdptr, char *
	kwdval, char *head, char *line, logical *eoc, char *cmdsym, integer *
	cmdptr, char *cmdval, logical *err, char *reason, integer *linnum, 
	ftnlen kwdsym_len, ftnlen kwdval_len, ftnlen head_len, ftnlen 
	line_len, ftnlen cmdsym_len, ftnlen cmdval_len, ftnlen reason_len)
{
    /* Initialized data */

    static integer nkwd = 0;

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8;
    char ch__1[32], ch__2[100];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen), i_indx(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer nchl, vlen, i__, j, k;
    extern integer cardc_(char *, ftnlen);
    static integer p;
    extern /* Subroutine */ int lcase_(char *, char *, ftnlen, ftnlen), 
	    chkin_(char *, ftnlen);
    static char minch[20], maxch[20];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    repmc_(char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, 
	    ftnlen);
    static integer lines, hdnum;
    static char value[300], kwnam[32*100];
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    static integer pnter;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    static integer kwstk[6];
    static char hd[32];
    static integer cp;
    extern logical failed_(void);
    static integer ep;
    static char ln[350];
    extern /* Subroutine */ int cleari_(integer *, integer *);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static char action[10];
    extern /* Subroutine */ int replch_(char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen);
    extern integer isrchi_(integer *, integer *, integer *), lastnb_(char *, 
	    ftnlen), frstnb_(char *, ftnlen);
    static char values[350*40];
    static integer newlev, kwdmin, kwdmax, chlnum[100];
    static char symbol[100];
    static integer kwcntr[240]	/* was [40][6] */, stklev, kwdnum, nummin[100]
	    ;
    extern logical return_(void);
    extern /* Character */ VOID crtptr_(char *, ftnlen, char *, integer *, 
	    char *, ftnlen, ftnlen);
    static integer nummax[100], chlptr[101], lstcmd, linstk[5];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), sygetc_(char *, char *, integer 
	    *, char *, integer *, char *, logical *, ftnlen, ftnlen, ftnlen, 
	    ftnlen), nextwd_(char *, char *, char *, ftnlen, ftnlen, ftnlen), 
	    nparsi_(char *, integer *, char *, integer *, ftnlen, ftnlen), 
	    suffix_(char *, integer *, char *, ftnlen, ftnlen), prefix_(char *
	    , integer *, char *, ftnlen, ftnlen), inttxt_(integer *, char *, 
	    ftnlen), syenqc_(char *, char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen);
    static logical fnd;
    static char kwd[32], msg[320];

/* $ Abstract */

/*     Command parser. */

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

/*     PARSING */
/*     STRING */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Entry */
/*     --------  ---  -------------------------------------------------- */
/*     KWDSYM, */
/*     KWDPTR, */
/*     KWDVAL     I   INITCP */
/*     HEAD       I   INITCP */
/*     LINE       I   EVALCP */
/*     EOC        I   EVALCP */
/*     CMDSYM, */
/*     CMDPTR, */
/*     CMDVAL     O   EVALCP */
/*     REASON     O   CPERR */
/*     LINNUM     O   CPERR */

/* $ Detailed_Input */

/*     See the ENTRY points for a discussion of their arguments. */

/* $ Detailed_Output */

/*     See the ENTRY points for a discussion of their arguments. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If CPARSE is called directly, the error SPICE(BOGUSENTRY) is */
/*        signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     CPARSE was built to parse NIOSPK command files. However, it */
/*     can be used to parse similar types of files. Below is an example */
/*     NIOSPK command file. The indentation is not necessary. */

/*        leapseconds_file   = leapseconds.ker */

/*        spk_file           = gll.bsp */
/*           note            = this is an example */
/*           source_nio_file = gll.nio */
/*              bodies       = -77 */
/*           source_nio_file = de200.nio */
/*              bodies       = 10 399 3 */

/*     Values may span any number of lines; no explicit continuation */
/*     characters are necessary. If a value does span multiple lines, */
/*     it must be broken at word boundaries. For example, the */
/*     following two commands are equivalent: */

/*        note = this is my note */

/*        note = this */
/*                    is */
/*                       my */
/*                          note */

/*     CPARSE must be told the relationships between keywords; this */
/*     is done in the initialization procedure, INITCP; it must be */
/*     told which keywords are parent keywords, and which keywords */
/*     are child keywords. These two terms are defined below. */

/*        parent keyword    is a keyword that has one or more */
/*                          child keywords. In the example above, */
/*                          spk_file and source_nio_file are parent */
/*                          keywords. The child keywords of spk_file */
/*                          are source_nio_file and note; the child */
/*                          keyword of source_nio_file is bodies. */

/*        child keyword     is a keyword that follows a parent keyword. */
/*                          In the example above, source_nio_file is a */
/*                          child keyword of spk_file. Because */
/*                          source_nio_file is a child keyword of */
/*                          spk_file, it must follow spk_file, as is */
/*                          shown. A child keyword may also be a */
/*                          parent keyword. A keyword may be */
/*                          a child keyword of two different parent */
/*                          keywords. For example, NIOSPK defines the */
/*                          bodies keyword to be a child keyword of */
/*                          spk_file as well as source_nio_file. If a */
/*                          keyword is a child keyword of multiple */
/*                          parent keywords, CPARSE assumes the */
/*                          closest parent keyword above it is its */
/*                          parent keyword (that's why source_nio_file, */
/*                          not spk_file, is the parent keyword of bodies */
/*                          in example). */

/*     In addition to telling INITCP which keywords are parent keywords */
/*     and which keywords are child keywords, it must be told how many */
/*     times the child keywords may appear under each parent */
/*     keyword---a minimum and a maximum number of times. For NIOSPK, */
/*     the source_nio_file keyword must appear at least once, and may */
/*     appear up to 100 (or so) number of times. The bodies keyword may */
/*     appear 0, or up to a 100 (or so) number of times. source_nio_file, */
/*     then, is a required keyword, while bodies is not. */

/*     The associations between the parent and child keywords and the */
/*     number of times the child keywords may appear per parent keyword */
/*     are passed to INITCP in the form of a symbol table. Each symbol */
/*     in the symbol table is a parent keyword; the values associated */
/*     with a symbol are the child keywords of the symbol, or parent */
/*     keyword. Two numbers must follow each child keyword; these numbers */
/*     are the minimum and maximum number of times the associated keyword */
/*     may appear under its parent keyword. For example, the bodies */
/*     keyword with the parent keyword source_nio_file would be given */
/*     as: */

/*        source_nio_file   -->   bodies 0 100 */

/*     and the source_nio_file keyword with the parent keyword spk_file */
/*     would be given as */

/*        spk_file          -->   source_nio_file 1 100 */

/*     bodies is also a child keyword of spk_file, so it can be given */
/*     in the same association: */

/*        spk_file          -->   source_nio_file 1 100 */
/*                                bodies          0 100 */


/*     In order to specify the minimum and maximum occurrence of keywords */
/*     that don't have a parent keyword, such as spk_file and */
/*     leapseconds_file in the example, a fictional keyword is used to */
/*     act as the parent keyword of those keywords. This keyword may */
/*     be named anything (the name is passed to INITCP); here, it is */
/*     named head. So, if spk_file and leapseconds_file were the only */
/*     two keywords without a parent, NIOSPK would create the following */
/*     association: */

/*        head              -->   spk_file         1 100 */
/*                                leapseconds_file 1 1 */

/*     Here, spk_file must appear at least once and no more than */
/*     100 times, and leapseconds_file must appear exactly */
/*     once. */

/*     Once CPARSE is initialized, it can process lines from a */
/*     command file, or from some other device, with the procedure */
/*     EVALCP. EVALCP does not read from a unit; rather, it is passed */
/*     a line at a time. EVALCP stores the data it processes in a */
/*     symbol table called the command symbol table; this data */
/*     structure is passed to EVALCP. When the last line is passed to */
/*     EVALCP, it must be called one last time with one of its arguments */
/*     (EOC--end of commands) set to true. */

/*     The calling procedure may access the data stored in the command */
/*     symbol table by first forming the appropriate symbol, then calling */
/*     SYGETC. In most cases, the procedure CRTPTR (create pointer) must */
/*     be called to form the appropriate symbol. The calling sequence */
/*     of CRTPTR is */

/*        CRTPTR (PARENT, INDEX, CHILD) */

/*     For example, if NIOSPK wanted the nio files associated with */
/*     the first spk file, it would need to perform the following: */

/*        SYMBOL = CRTPTR ('SPK_FILE', 1, 'SOURCE_NIO_FILE') */
/*        CALL SYGETC (SYMBOL, CMDSYM, CMDPTR, CMDVAL, N, VALUES, FND) */

/*     Where CMDxxx is the command symbol table. Or, if NIOSPK wanted */
/*     the bodies associated with the second source nio file in the first */
/*     spk file, it would need to call CRTPTR twice: */

/*        SYMBOL = CRTPTR ('SPK_FILE', 1, 'SOURCE_NIO_FILE') */
/*        SYMBOL = CRTPTR (SYMBOL,     2, 'BODIES'         ) */
/*        CALL SYGETC (SYMBOL, CMDSYM, CMDPTR, CMDVAL, N, VALUES, FND) */

/*     If FND is returned as false, then no bodies were given for the */
/*     source nio file. To retrieve values associated with keywords that */
/*     do not have parent keywords (if you ignore head), no calls to */
/*     CRTPTR are necessary. For example, if NIOSPK wished to get all the */
/*     spk files, it would just call SYGETC: */

/*        SYMBOL = 'SPK_FILE' */
/*        CALL SYGETC (SYMBOL, CMDSYM, CMDPTR, CMDVAL, N, VALUES, FND) */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     M.J. Spencer   (JPL) */

/* $ Version */

/* -    Beta Version 2.2.0, 17-JAN-2014 (BVS) */

/*       Increased LINLEN from 127 to 350. */

/* -    Beta Version 2.1.0, 14-JAN-1994 (MJS) */

/*       Removed VALSTK, as it no longer used. Increased VALLEN from */
/*       255 to 300. */

/* -    Beta Version 2.0.0, 12-DEC-1993 (MJS) */

/*       Parse errors no longer signal SPICE errors. Rather, applications */
/*       get error messages via the new entry point CPERR --- EVALCP */
/*       has a new argument, ERR, which indicates when an error has */
/*       occurred. */

/*       Modified internal data structure. This version uses one less */
/*       array (NUMCHL removed). CHLBEG changed to CHLPTR. CHLPTR */
/*       contains pointers to entries in CHLNUM, NUMMIN, and NUMMAX. */
/*       CHLPTR(I)-CHLPTR(I-1) is the number of child keywords of */
/*       parent keyword I. Child keywords of parent keyword I start at */
/*       CHLPTR(I-1)+1 (CHLPTR starts at 0). */

/* -    Beta Version 1.0.0, 8-OCT-1992 (MJS) */

/* -& */

/*     SPICELIB functions */


/*     Statement functions */


/*     Other functions */


/*     Local parameters */


/*     MAXKWD    is the maximum number of keywords */

/*     MAXSTK    is the maximum number of keywords one must follow to */
/*               find any keyword. */

/*     CHLSIZ    is the maximum number of child keywords. If a keyword */
/*               is a child of more than one parent keyword, it must */
/*               be counted as many times. */

/*     KWDLEN    is the maximum length of a keyword. */

/*     LINLEN    is the maximum length of a line. */

/*     VALLEN    is the maximum length of a value. A value may span */
/*               over multiple lines, but leading and trailing spaces */
/*               are ignored (except for one). */

/*     MAXCHL    is the maximum number of child keywords a parent keyword */
/*               may have. */

/*     TAB       is the ASCII value for the tab character. */

/*     ERRLEN    is the maximum length of an error message. */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    /* Parameter adjustments */
    if (kwdsym) {
	}
    if (kwdptr) {
	}
    if (kwdval) {
	}
    if (cmdsym) {
	}
    if (cmdptr) {
	}
    if (cmdval) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_initcp;
	case 2: goto L_evalcp;
	case 3: goto L_cperr;
	}


/*     Statement function definitions. */

/*        NUMCHL (KWN)       returns the number of child keywords of */
/*                           parent keyword index KWN in KWNAM. */
/*        KWLOOK (KW)        returns the index of KW in KWNAM. */
/*        CHLOOK (KWN,CHN)   returns the offset position of CHN in */
/*                           CHLNUM using the parent index KWN. */

/*     Where, */

/*        KW   is the name of a keyword. */
/*        KWN  is the index of a keyword in KWNAM. */
/*        CHN  is the index of a keyword in KWNAM. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CPARSE", (ftnlen)6);
    }

/*     This routine should never be called. If this routine is called, */
/*     an error is signaled. */

    setmsg_("CPARSE: You have called an entry which performs no run-time fun"
	    "ction. This may indicate a bug.", (ftnlen)94);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("CPARSE", (ftnlen)6);
    return 0;
/* $Procedure INITCP (Initialize, command parser) */

L_initcp:
/* $ Abstract */

/*     Initialize command parser. */

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

/*     PARSING */
/*     STRING */

/* $ Declarations */

/*     INTEGER               LBCELL */
/*     PARAMETER           ( LBCELL = -5 ) */

/*     CHARACTER*(*)         KWDSYM (LBCELL: *) */
/*     INTEGER               KWDPTR (LBCELL: *) */
/*     CHARACTER*(*)         KWDVAL (LBCELL: *) */
/*     CHARACTER*(*)         HEAD */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*      KWDSYM, */
/*      KWDPTR, */
/*      KWDVAL    I    Symbol table containing the parent-child */
/*                     associations. */

/*      HEAD      I    Head symbol. */

/* $ Detailed_Input */

/*      KWDSYM, */
/*      KWDPTR, */
/*      KWDVAL    is a symbol table containing parent-child keyword */
/*                associations, including the number of times the */
/*                child keyword map appear under its parent. ALL */
/*                associations in this symbol table are assumed to */
/*                be parent-keyword associations. */

/*                See the CPARSE particulars section for more details. */

/*      HEAD      is the name of the HEAD node. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If HEAD is not a symbol in the symbol table, the error */
/*        SPICE(NOHEADNODE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     See the CPARSE particulars section. */

/*     ALL the symbols in CMDSYM are treated as parent keywords (that is, */
/*     make sure your symbol table has only parent-child associations). */

/*     INITCP translates the parent-child associations in the symbol */
/*     table to a data structure that will make parsing the commands */
/*     easier. The data structure is made up of five, one dimensional */
/*     arrays. The arrays are named KWNAM, CHLPTR, CHLNUM, */
/*     NUMMIN, and NUMMAX. These arrays are shown graphically below. */

/*         KWNAM   CHLPTR      CHLNUM  NUMMIN  NUMMAX */
/*          +---+  +---+       +---+   +---+   +---+ */
/*        1 |   |  |   |-------|   |   |   |   |   | 1 */
/*          +---+  +---+       +---+   +---+   +---+ */
/*          | . |  | . |       |   |   |   |   |   | */
/*            .      .         +---+   +---+   +---+ */
/*        i | . |  | . |       | . |   | . |   | . | */
/*          +---+  +---+         .       .       . */
/*          |   |  |   |\      | . |   | . |   | . | */
/*          +---+  +---+ \     +---+   +---+   +---+ */
/*   MAXKWD |   |  |   |  \    |   |   |   |   |   | */
/*          +---+  +---+   \   +---+   +---+   +---+ */
/*                          \  |   |   |   |   |   | */
/*                           \ +---+   +---+   +---+ */
/*                            \|   |   |   |   |   | */
/*                             +---+   +---+   +---+ */
/*                             |   |   |   |   |   | MAXCHL */
/*                             +---+   +---+   +---+ */

/*     The first three arrays are of dimension MAXKWD, and the last */
/*     three are of dimension MAXCHL. These arrays are discussed */
/*     separately below. */

/*        Name    Type   Contents */

/*        CHLNAM  CHAR   The names of all the keywords in no particular */
/*                       order. */

/*        CHLPTR  INT    Pointer to CHLNUM, NUMMIN, and NUMMAX. */

/*        CHLNUM  INT    Child keywords associated with the keywords in */
/*                       in KWNAM. Keywords are referenced by their */
/*                       position in KWNAM. */

/*        NUMMIN  INT   Minimum occurrence of child keyword in CHLNUM. */

/*        NUMMAX  INT   Maximum occurrence of child keyword in CHLNUM. */

/* $ Examples */

/*     See the NIOSPK main module. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     M.J. Spencer   (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 8-OCT-1992 (MJS) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("INITCP", (ftnlen)6);
    }

/*     We want all the keywords in KWNAM and ordered. */

    nkwd = 0;
    i__1 = cardc_(kwdsym, kwdsym_len);
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_copy(kwd, kwdsym + (i__ + 5) * kwdsym_len, (ftnlen)32, kwdsym_len);
	ljust_(kwd, kwd, (ftnlen)32, (ftnlen)32);
	ucase_(kwd, kwd, (ftnlen)32, (ftnlen)32);

/*        Have we already come across this keyword? */

	p = isrchc_(kwd, &nkwd, kwnam, (ftnlen)32, (ftnlen)32);
	if (p == 0) {
	    ++nkwd;
	    s_copy(kwnam + (((i__2 = nkwd - 1) < 100 && 0 <= i__2 ? i__2 : 
		    s_rnge("kwnam", i__2, "cparse_", (ftnlen)681)) << 5), kwd,
		     (ftnlen)32, (ftnlen)32);
	}

/*        Check its children. */

	sygetc_(kwd, kwdsym, kwdptr, kwdval, &nchl, values, &fnd, (ftnlen)32, 
		kwdsym_len, kwdval_len, (ftnlen)350);
	i__2 = nchl;
	for (j = 1; j <= i__2; ++j) {
	    nextwd_(values + ((i__3 = j - 1) < 40 && 0 <= i__3 ? i__3 : 
		    s_rnge("values", i__3, "cparse_", (ftnlen)691)) * 350, 
		    kwd, values + ((i__4 = j - 1) < 40 && 0 <= i__4 ? i__4 : 
		    s_rnge("values", i__4, "cparse_", (ftnlen)691)) * 350, (
		    ftnlen)350, (ftnlen)32, (ftnlen)350);
	    ucase_(kwd, kwd, (ftnlen)32, (ftnlen)32);

/*           If this is a new keyword, add it to our array. */

	    p = isrchc_(kwd, &nkwd, kwnam, (ftnlen)32, (ftnlen)32);
	    if (p == 0) {
		++nkwd;
		s_copy(kwnam + (((i__3 = nkwd - 1) < 100 && 0 <= i__3 ? i__3 :
			 s_rnge("kwnam", i__3, "cparse_", (ftnlen)702)) << 5),
			 kwd, (ftnlen)32, (ftnlen)32);
	    }
	}
    }

/*     Here we could sort KWNAM and use a binary search, for maybe */
/*     a bit better performance. */


/*     Now, for each keyword in KWNAM find the child keywords, if any. */

    chlptr[0] = 0;
    i__1 = nkwd;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_copy(kwd, kwnam + (((i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : 
		s_rnge("kwnam", i__2, "cparse_", (ftnlen)721)) << 5), (ftnlen)
		32, (ftnlen)32);
	sygetc_(kwd, kwdsym, kwdptr, kwdval, &nchl, values, &fnd, (ftnlen)32, 
		kwdsym_len, kwdval_len, (ftnlen)350);
	p = chlptr[(i__2 = i__ - 1) < 101 && 0 <= i__2 ? i__2 : s_rnge("chlp"
		"tr", i__2, "cparse_", (ftnlen)725)];
	if (fnd) {
	    i__2 = nchl;
	    for (j = 1; j <= i__2; ++j) {
		nextwd_(values + ((i__3 = j - 1) < 40 && 0 <= i__3 ? i__3 : 
			s_rnge("values", i__3, "cparse_", (ftnlen)730)) * 350,
			 kwd, values + ((i__4 = j - 1) < 40 && 0 <= i__4 ? 
			i__4 : s_rnge("values", i__4, "cparse_", (ftnlen)730))
			 * 350, (ftnlen)350, (ftnlen)32, (ftnlen)350);
		nextwd_(values + ((i__3 = j - 1) < 40 && 0 <= i__3 ? i__3 : 
			s_rnge("values", i__3, "cparse_", (ftnlen)731)) * 350,
			 minch, values + ((i__4 = j - 1) < 40 && 0 <= i__4 ? 
			i__4 : s_rnge("values", i__4, "cparse_", (ftnlen)731))
			 * 350, (ftnlen)350, (ftnlen)20, (ftnlen)350);
		nextwd_(values + ((i__3 = j - 1) < 40 && 0 <= i__3 ? i__3 : 
			s_rnge("values", i__3, "cparse_", (ftnlen)732)) * 350,
			 maxch, values + ((i__4 = j - 1) < 40 && 0 <= i__4 ? 
			i__4 : s_rnge("values", i__4, "cparse_", (ftnlen)732))
			 * 350, (ftnlen)350, (ftnlen)20, (ftnlen)350);
		ucase_(kwd, kwd, (ftnlen)32, (ftnlen)32);
		nparsi_(minch, &kwdmin, msg, &pnter, (ftnlen)20, (ftnlen)320);
		nparsi_(maxch, &kwdmax, msg, &pnter, (ftnlen)20, (ftnlen)320);
		s_copy(ch__1, kwd, (ftnlen)32, (ftnlen)32);
		chlnum[(i__3 = p + j - 1) < 100 && 0 <= i__3 ? i__3 : s_rnge(
			"chlnum", i__3, "cparse_", (ftnlen)739)] = isrchc_(
			ch__1, &nkwd, kwnam, (ftnlen)32, (ftnlen)32);
		nummin[(i__3 = p + j - 1) < 100 && 0 <= i__3 ? i__3 : s_rnge(
			"nummin", i__3, "cparse_", (ftnlen)740)] = kwdmin;
		nummax[(i__3 = p + j - 1) < 100 && 0 <= i__3 ? i__3 : s_rnge(
			"nummax", i__3, "cparse_", (ftnlen)741)] = kwdmax;
	    }
	} else {
	    nchl = 0;
	}
	chlptr[(i__2 = i__) < 101 && 0 <= i__2 ? i__2 : s_rnge("chlptr", i__2,
		 "cparse_", (ftnlen)748)] = p + nchl;
    }

/*     HEAD must be in the data base. Its index is stored in HDNUM. */

    s_copy(hd, head, (ftnlen)32, head_len);
    ljust_(hd, hd, (ftnlen)32, (ftnlen)32);
    ucase_(hd, hd, (ftnlen)32, (ftnlen)32);
    s_copy(ch__1, hd, (ftnlen)32, (ftnlen)32);
    hdnum = isrchc_(ch__1, &nkwd, kwnam, (ftnlen)32, (ftnlen)32);
    if (hdnum == 0) {
	sigerr_("SPICE(NOHEADNODE)", (ftnlen)17);
	chkout_("INITCP", (ftnlen)6);
	return 0;
    }

/*     Zero out the number of child nodes under HEAD. This is stack */
/*     level zero. HDNUM is the first element in KWSTK. No keywords */
/*     have been read, so KWD is set to ' '. */

    stklev = 0;
    kwstk[(i__1 = stklev) < 6 && 0 <= i__1 ? i__1 : s_rnge("kwstk", i__1, 
	    "cparse_", (ftnlen)774)] = hdnum;
    s_copy(kwd, " ", (ftnlen)32, (ftnlen)1);
    i__4 = chlptr[(i__1 = hdnum) < 101 && 0 <= i__1 ? i__1 : s_rnge("chlptr", 
	    i__1, "cparse_", (ftnlen)777)] - chlptr[(i__2 = hdnum - 1) < 101 
	    && 0 <= i__2 ? i__2 : s_rnge("chlptr", i__2, "cparse_", (ftnlen)
	    777)];
    cleari_(&i__4, &kwcntr[(i__3 = stklev * 40) < 240 && 0 <= i__3 ? i__3 : 
	    s_rnge("kwcntr", i__3, "cparse_", (ftnlen)777)]);

/*     The first line we'll read is 1. */

    lines = 0;

/*     That's it. We're set up to process commands. */

    chkout_("INITCP", (ftnlen)6);
    return 0;
/* $Procedure EVALCP (Evaluate, command parser) */

L_evalcp:
/* $ Abstract */

/*     Evaluate commands. */

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

/*     PARSING */
/*     STRING */

/* $ Declarations */

/*     INTEGER               LBCELL */
/*     PARAMETER           ( LBCELL = -5 ) */

/*     CHARACTER*(*)         LINE */
/*     LOGICAL               EOC */
/*     CHARACTER*(*)         CMDSYM (LBCELL: *) */
/*     INTEGER               CMDPTR (LBCELL: *) */
/*     CHARACTER*(*)         CMDVAL (LBCELL: *) */
/*     LOGICAL               ERR */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*      LINE      I    Command. */
/*      EOC       I    End of commands. Must be set to true after last */
/*                     line has been processed. */
/*      CMDSYM, */
/*      CMDPTR, */
/*      CMDVAL    O    Command symbol table. */
/*      ERR       O    True if a parse error occurs. If true, application */
/*                     should call CPERR for reason. */

/* $ Detailed_Input */

/*     See the CPARSE particulars section. */

/* $ Detailed_Output */

/*     See the CPARSE particulars section. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     If the parser is not initialized an error will be signaled. Other */
/*     may be obtained via the entry point CPERR. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     See the CPARSE particulars section. */

/* $ Examples */

/*     Suppose you have a command file named INPUT.CMD, and you wish */
/*     to process commands in this file after the delimiter */
/*     '\begin_commands' through the end of the file. The following */
/*     code fragment illustrates how one perform this task. */

/*        CALL RDTEXT ('INPUT.CMD', LINE, EOF) */

/*        DO WHILE (LINE .NE. '\begin_commands') */

/*           CALL RDTEXT ('INPUT.CMD', LINE, EOF) */

/*        END DO */

/*        DO WHILE (.NOT. EOF) */

/*           CALL RDTEXT ('INPUT.CMD', LINE, EOF) */
/*           CALL EVALCP (LINE, EOF, CMDSYM, CMDPTR, CMDVAL) */

/*        END DO */

/*     To see how CPARSE is used in an application, see the */
/*     NIOSPK main module. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     M.J. Spencer   (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 8-OCT-1992 (MJS) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EVALCP", (ftnlen)6);
    }

/*     If NKWD is zero, the parser hasn't been initialized. */

    if (nkwd == 0) {
	sigerr_("SPICE(PARSERNOTREADY)", (ftnlen)21);
	chkout_("EVALCP", (ftnlen)6);
	return 0;
    }

/*     No error so far. */

    *err = FALSE_;

/*     If we reached the end of commands, we just need to evaluate */
/*     what's in the buffer. */

    if (*eoc) {
	if (s_cmp(kwd, " ", (ftnlen)32, (ftnlen)1) == 0) {
	    s_copy(action, "DONE", (ftnlen)10, (ftnlen)4);
	} else {
	    s_copy(action, "EVAL", (ftnlen)10, (ftnlen)4);
	}
    } else {
	s_copy(action, "MARK", (ftnlen)10, (ftnlen)4);
	++lines;
    }
    while(s_cmp(action, "DONE", (ftnlen)10, (ftnlen)4) != 0 && ! failed_()) {

/*        RETURN exits from the routine (gets the next line). */

	if (s_cmp(action, "RETURN", (ftnlen)10, (ftnlen)6) == 0) {
	    chkout_("EVALCP", (ftnlen)6);
	    return 0;

/*        MARK marks the position of the equal sign and comment */
/*        character. */

	} else if (s_cmp(action, "MARK", (ftnlen)10, (ftnlen)4) == 0) {

/*           Replace all tabs with spaces in the line, and left justify */
/*           it. */

	    s_copy(ln, line, (ftnlen)350, line_len);
	    replch_(ln, "\t", " ", ln, (ftnlen)350, (ftnlen)1, (ftnlen)1, (
		    ftnlen)350);
	    ljust_(ln, ln, (ftnlen)350, (ftnlen)350);

/*           A blank line or a comment line is of no use to EVALCP. */

	    if (s_cmp(ln, " ", (ftnlen)350, (ftnlen)1) == 0 || *(unsigned 
		    char *)ln == ';') {
		s_copy(action, "RETURN", (ftnlen)10, (ftnlen)6);
	    } else {

/*              Mark the positions of the equal sign and comment */
/*              character (EP and CP, respectively). If the comment */
/*              character is non-existent, CP is set to the length of */
/*              the line plus one (makes for easier parsing). */

		ep = i_indx(ln, "=", (ftnlen)350, (ftnlen)1);
		cp = i_indx(ln, ";", (ftnlen)350, (ftnlen)1);
		if (cp == 0) {
		    cp = lastnb_(ln, (ftnlen)350) + 1;
		}
		s_copy(action, "PARSE", (ftnlen)10, (ftnlen)5);
	    }

/*        PARSE separates the keyword from the value, or if the line is */
/*        a continuation of the previous command, it appends the value. */

	} else if (s_cmp(action, "PARSE", (ftnlen)10, (ftnlen)5) == 0) {

/*           If KWD is blank, this should be the first line of a */
/*           command. */

	    if (s_cmp(kwd, " ", (ftnlen)32, (ftnlen)1) == 0) {
		lstcmd = lines;
		if (ep > 1 && ep < cp) {
		    s_copy(kwd, ln, (ftnlen)32, ep - 1);
		    i__1 = ep;
		    s_copy(value, ln + i__1, (ftnlen)300, cp - 1 - i__1);
		    ljust_(kwd, kwd, (ftnlen)32, (ftnlen)32);
		    ucase_(kwd, kwd, (ftnlen)32, (ftnlen)32);
		    ljust_(value, value, (ftnlen)300, (ftnlen)300);
		    vlen = lastnb_(value, (ftnlen)300);
		} else {
		    s_copy(msg, "Line #: Invalid command. Commands must be o"
			    "f the form 'keyword = value.'", (ftnlen)320, (
			    ftnlen)72);
		    *err = TRUE_;
		    chkout_("EVALCP", (ftnlen)6);
		    return 0;
		}
		s_copy(action, "RETURN", (ftnlen)10, (ftnlen)6);
	    } else {

/*              Is it a continuation of the previous command, or is it */
/*              a new command? If EP is equal to 1, we still have new */
/*              command, but an error will be signaled when we return */
/*              to PARSE later. */

		if (ep >= 1 && ep < cp) {
		    s_copy(action, "EVAL", (ftnlen)10, (ftnlen)4);
		} else {
		    i__ = frstnb_(ln, (ftnlen)350);
		    vlen = vlen + cp - i__ + 1;
		    if (vlen > 300) {
			s_copy(msg, "Line #: Value of % command exceeds % ch"
				"aracters.", (ftnlen)320, (ftnlen)48);
			repmc_(msg, "%", kwd, msg, (ftnlen)320, (ftnlen)1, (
				ftnlen)32, (ftnlen)320);
			repmi_(msg, "%", &c__300, msg, (ftnlen)320, (ftnlen)1,
				 (ftnlen)320);
			*err = TRUE_;
			chkout_("EVALCP", (ftnlen)6);
			return 0;
		    }
		    suffix_(ln, &c__1, value, cp - 1, (ftnlen)300);
		    s_copy(action, "RETURN", (ftnlen)10, (ftnlen)6);
		}
	    }

/*        EVAL evaluates the command, and stores it in the data */
/*        structure. */

	} else if (s_cmp(action, "EVAL", (ftnlen)10, (ftnlen)4) == 0) {

/*           Do we have an acceptable keyword? */

	    s_copy(ch__1, kwd, (ftnlen)32, (ftnlen)32);
	    kwdnum = isrchc_(ch__1, &nkwd, kwnam, (ftnlen)32, (ftnlen)32);
	    if (kwdnum == 0 || kwdnum == hdnum) {
		s_copy(msg, "Line #: % is not an acceptable keyword.", (
			ftnlen)320, (ftnlen)39);
		repmc_(msg, "%", kwd, msg, (ftnlen)320, (ftnlen)1, (ftnlen)32,
			 (ftnlen)320);
		*err = TRUE_;
		chkout_("EVALCP", (ftnlen)6);
		return 0;
	    }

/*           How about a value? */

	    if (s_cmp(value, " ", (ftnlen)300, (ftnlen)1) == 0) {
		s_copy(msg, "Line #: % command has no value.", (ftnlen)320, (
			ftnlen)31);
		repmc_(msg, "%", kwd, msg, (ftnlen)320, (ftnlen)1, (ftnlen)32,
			 (ftnlen)320);
		*err = TRUE_;
		chkout_("EVALCP", (ftnlen)6);
		return 0;
	    }

/*           Check if this keyword is a child node of the current level. */
/*           STKLEV is the current level (KWSTK(0) is HDNUM). If not, */
/*           decrease STKLEV until we find a place for this keyword. */

	    newlev = stklev;
	    i__6 = chlptr[(i__2 = kwstk[(i__1 = newlev) < 6 && 0 <= i__1 ? 
		    i__1 : s_rnge("kwstk", i__1, "cparse_", (ftnlen)1131)]) < 
		    101 && 0 <= i__2 ? i__2 : s_rnge("chlptr", i__2, "cparse_"
		    , (ftnlen)1131)] - chlptr[(i__3 = kwstk[(i__1 = newlev) < 
		    6 && 0 <= i__1 ? i__1 : s_rnge("kwstk", i__1, "cparse_", (
		    ftnlen)1131)] - 1) < 101 && 0 <= i__3 ? i__3 : s_rnge(
		    "chlptr", i__3, "cparse_", (ftnlen)1131)];
	    p = isrchi_(&kwdnum, &i__6, &chlnum[(i__5 = chlptr[(i__4 = kwstk[(
		    i__1 = newlev) < 6 && 0 <= i__1 ? i__1 : s_rnge("kwstk", 
		    i__1, "cparse_", (ftnlen)1131)] - 1) < 101 && 0 <= i__4 ? 
		    i__4 : s_rnge("chlptr", i__4, "cparse_", (ftnlen)1131)]) <
		     100 && 0 <= i__5 ? i__5 : s_rnge("chlnum", i__5, "cpars"
		    "e_", (ftnlen)1131)]);
	    while(newlev != 0 && p == 0) {
		--newlev;
		i__6 = chlptr[(i__2 = kwstk[(i__1 = newlev) < 6 && 0 <= i__1 ?
			 i__1 : s_rnge("kwstk", i__1, "cparse_", (ftnlen)1136)
			]) < 101 && 0 <= i__2 ? i__2 : s_rnge("chlptr", i__2, 
			"cparse_", (ftnlen)1136)] - chlptr[(i__3 = kwstk[(
			i__1 = newlev) < 6 && 0 <= i__1 ? i__1 : s_rnge("kws"
			"tk", i__1, "cparse_", (ftnlen)1136)] - 1) < 101 && 0 
			<= i__3 ? i__3 : s_rnge("chlptr", i__3, "cparse_", (
			ftnlen)1136)];
		p = isrchi_(&kwdnum, &i__6, &chlnum[(i__5 = chlptr[(i__4 = 
			kwstk[(i__1 = newlev) < 6 && 0 <= i__1 ? i__1 : 
			s_rnge("kwstk", i__1, "cparse_", (ftnlen)1136)] - 1) <
			 101 && 0 <= i__4 ? i__4 : s_rnge("chlptr", i__4, 
			"cparse_", (ftnlen)1136)]) < 100 && 0 <= i__5 ? i__5 :
			 s_rnge("chlnum", i__5, "cparse_", (ftnlen)1136)]);
	    }

/*           If P is still zero, KWD does not belong here. */

	    if (p == 0) {
		s_copy(msg, " ", (ftnlen)320, (ftnlen)1);
		k = 0;

/*              We'll try to be helpful by letting the user know */
/*              where this keyword may appear. */

		i__1 = nkwd;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    i__6 = chlptr[(i__2 = i__) < 101 && 0 <= i__2 ? i__2 : 
			    s_rnge("chlptr", i__2, "cparse_", (ftnlen)1154)] 
			    - chlptr[(i__3 = i__ - 1) < 101 && 0 <= i__3 ? 
			    i__3 : s_rnge("chlptr", i__3, "cparse_", (ftnlen)
			    1154)];
		    p = isrchi_(&kwdnum, &i__6, &chlnum[(i__5 = chlptr[(i__4 =
			     i__ - 1) < 101 && 0 <= i__4 ? i__4 : s_rnge(
			    "chlptr", i__4, "cparse_", (ftnlen)1154)]) < 100 
			    && 0 <= i__5 ? i__5 : s_rnge("chlnum", i__5, 
			    "cparse_", (ftnlen)1154)]);
		    if (p != 0) {
			++k;
			suffix_(kwnam + (((i__2 = i__ - 1) < 100 && 0 <= i__2 
				? i__2 : s_rnge("kwnam", i__2, "cparse_", (
				ftnlen)1158)) << 5), &c__1, msg, (ftnlen)32, (
				ftnlen)320);
		    }
		}
		if (k == 1) {
		    prefix_("Line #: Placement of the % command is incorrect"
			    ". The % command may only appear after the", &c__0,
			     msg, (ftnlen)88, (ftnlen)320);
		    suffix_("command.", &c__1, msg, (ftnlen)8, (ftnlen)320);
		} else {
		    prefix_("Line #: Placement of the % command is incorrect"
			    ". The % command may only appear after the follow"
			    "ing commands:", &c__0, msg, (ftnlen)108, (ftnlen)
			    320);
		    suffix_(".", &c__0, msg, (ftnlen)1, (ftnlen)320);
		}
		repmc_(msg, "%", kwd, msg, (ftnlen)320, (ftnlen)1, (ftnlen)32,
			 (ftnlen)320);
		repmc_(msg, "%", kwd, msg, (ftnlen)320, (ftnlen)1, (ftnlen)32,
			 (ftnlen)320);
		*err = TRUE_;
		chkout_("EVALCP", (ftnlen)6);
		return 0;
	    }

/*           Check if we have the necessary commands between STKLEV */
/*           and NEWLEV (have all the nodes been given a sufficient */
/*           number of times)? */

	    i__1 = newlev + 1;
	    for (i__ = stklev; i__ >= i__1; --i__) {

/*              J is the index in CHLNUM where the child keywords are */
/*              given for parent keyword index I. */

		j = chlptr[(i__3 = kwstk[(i__2 = i__) < 6 && 0 <= i__2 ? i__2 
			: s_rnge("kwstk", i__2, "cparse_", (ftnlen)1195)] - 1)
			 < 101 && 0 <= i__3 ? i__3 : s_rnge("chlptr", i__3, 
			"cparse_", (ftnlen)1195)];
		i__5 = chlptr[(i__3 = kwstk[(i__2 = i__) < 6 && 0 <= i__2 ? 
			i__2 : s_rnge("kwstk", i__2, "cparse_", (ftnlen)1197)]
			) < 101 && 0 <= i__3 ? i__3 : s_rnge("chlptr", i__3, 
			"cparse_", (ftnlen)1197)] - chlptr[(i__4 = kwstk[(
			i__2 = i__) < 6 && 0 <= i__2 ? i__2 : s_rnge("kwstk", 
			i__2, "cparse_", (ftnlen)1197)] - 1) < 101 && 0 <= 
			i__4 ? i__4 : s_rnge("chlptr", i__4, "cparse_", (
			ftnlen)1197)];
		for (k = 1; k <= i__5; ++k) {
		    if (kwcntr[(i__2 = k + i__ * 40 - 1) < 240 && 0 <= i__2 ? 
			    i__2 : s_rnge("kwcntr", i__2, "cparse_", (ftnlen)
			    1199)] < nummin[(i__3 = k + j - 1) < 100 && 0 <= 
			    i__3 ? i__3 : s_rnge("nummin", i__3, "cparse_", (
			    ftnlen)1199)]) {
			inttxt_(&nummin[(i__2 = k + j - 1) < 100 && 0 <= i__2 
				? i__2 : s_rnge("nummin", i__2, "cparse_", (
				ftnlen)1201)], minch, (ftnlen)20);
			if (nummax[(i__2 = k + j - 1) < 100 && 0 <= i__2 ? 
				i__2 : s_rnge("nummax", i__2, "cparse_", (
				ftnlen)1203)] == nummin[(i__3 = k + j - 1) < 
				100 && 0 <= i__3 ? i__3 : s_rnge("nummin", 
				i__3, "cparse_", (ftnlen)1203)]) {
			    s_copy(msg, "Line #: % %", (ftnlen)320, (ftnlen)
				    11);
			    lcase_(minch + 1, minch + 1, (ftnlen)19, (ftnlen)
				    19);
			} else {
			    s_copy(msg, "Line #: At least % %", (ftnlen)320, (
				    ftnlen)20);
			    lcase_(minch, minch, (ftnlen)20, (ftnlen)20);
			}
			if (nummin[(i__2 = k + j - 1) < 100 && 0 <= i__2 ? 
				i__2 : s_rnge("nummin", i__2, "cparse_", (
				ftnlen)1211)] == 1) {
			    suffix_("command must appear after the % command."
				    , &c__1, msg, (ftnlen)40, (ftnlen)320);
			} else {
			    suffix_("commands must appear after the % comman"
				    "d.", &c__1, msg, (ftnlen)41, (ftnlen)320);
			}
			repmc_(msg, "%", minch, msg, (ftnlen)320, (ftnlen)1, (
				ftnlen)20, (ftnlen)320);
			repmc_(msg, "%", kwnam + (((i__3 = chlnum[(i__2 = k + 
				j - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge(
				"chlnum", i__2, "cparse_", (ftnlen)1220)] - 1)
				 < 100 && 0 <= i__3 ? i__3 : s_rnge("kwnam", 
				i__3, "cparse_", (ftnlen)1220)) << 5), msg, (
				ftnlen)320, (ftnlen)1, (ftnlen)32, (ftnlen)
				320);
			repmc_(msg, "%", kwnam + (((i__3 = kwstk[(i__2 = i__) 
				< 6 && 0 <= i__2 ? i__2 : s_rnge("kwstk", 
				i__2, "cparse_", (ftnlen)1221)] - 1) < 100 && 
				0 <= i__3 ? i__3 : s_rnge("kwnam", i__3, 
				"cparse_", (ftnlen)1221)) << 5), msg, (ftnlen)
				320, (ftnlen)1, (ftnlen)32, (ftnlen)320);
			lstcmd = linstk[(i__2 = i__ - 1) < 5 && 0 <= i__2 ? 
				i__2 : s_rnge("linstk", i__2, "cparse_", (
				ftnlen)1223)];
			*err = TRUE_;
			chkout_("EVALCP", (ftnlen)6);
			return 0;
		    }
		}
	    }

/*           If we add the command to NEWLEV, will we exceed its */
/*           maximum occurrence? */

	    j = chlptr[(i__5 = kwstk[(i__1 = i__) < 6 && 0 <= i__1 ? i__1 : 
		    s_rnge("kwstk", i__1, "cparse_", (ftnlen)1238)] - 1) < 
		    101 && 0 <= i__5 ? i__5 : s_rnge("chlptr", i__5, "cparse_"
		    , (ftnlen)1238)];
	    if (kwcntr[(i__1 = p + newlev * 40 - 1) < 240 && 0 <= i__1 ? i__1 
		    : s_rnge("kwcntr", i__1, "cparse_", (ftnlen)1240)] == 
		    nummax[(i__5 = p + j - 1) < 100 && 0 <= i__5 ? i__5 : 
		    s_rnge("nummax", i__5, "cparse_", (ftnlen)1240)]) {
		if (nummax[(i__1 = p + j - 1) < 100 && 0 <= i__1 ? i__1 : 
			s_rnge("nummax", i__1, "cparse_", (ftnlen)1242)] > 1) 
			{
		    s_copy(msg, "Line #: Only % % commands may appear", (
			    ftnlen)320, (ftnlen)36);
		} else {
		    s_copy(msg, "Line #: Only % % command may appear", (
			    ftnlen)320, (ftnlen)35);
		}
		inttxt_(&nummax[(i__1 = p + j - 1) < 100 && 0 <= i__1 ? i__1 :
			 s_rnge("nummax", i__1, "cparse_", (ftnlen)1248)], 
			maxch, (ftnlen)20);
		lcase_(maxch, maxch, (ftnlen)20, (ftnlen)20);
		repmc_(msg, "%", maxch, msg, (ftnlen)320, (ftnlen)1, (ftnlen)
			20, (ftnlen)320);
		repmc_(msg, "%", kwd, msg, (ftnlen)320, (ftnlen)1, (ftnlen)32,
			 (ftnlen)320);
		if (newlev > 0) {
		    suffix_("following the % command.", &c__1, msg, (ftnlen)
			    24, (ftnlen)320);
		    repmc_(msg, "%", kwnam + (((i__5 = kwstk[(i__1 = newlev) <
			     6 && 0 <= i__1 ? i__1 : s_rnge("kwstk", i__1, 
			    "cparse_", (ftnlen)1256)] - 1) < 100 && 0 <= i__5 
			    ? i__5 : s_rnge("kwnam", i__5, "cparse_", (ftnlen)
			    1256)) << 5), msg, (ftnlen)320, (ftnlen)1, (
			    ftnlen)32, (ftnlen)320);
		} else {
		    suffix_(".", &c__0, msg, (ftnlen)1, (ftnlen)320);
		}
		*err = TRUE_;
		chkout_("EVALCP", (ftnlen)6);
		return 0;
	    }

/*           Everything checks out. Update our records. */

	    kwcntr[(i__1 = p + newlev * 40 - 1) < 240 && 0 <= i__1 ? i__1 : 
		    s_rnge("kwcntr", i__1, "cparse_", (ftnlen)1270)] = kwcntr[
		    (i__5 = p + newlev * 40 - 1) < 240 && 0 <= i__5 ? i__5 : 
		    s_rnge("kwcntr", i__5, "cparse_", (ftnlen)1270)] + 1;
	    stklev = newlev + 1;
	    kwstk[(i__1 = stklev) < 6 && 0 <= i__1 ? i__1 : s_rnge("kwstk", 
		    i__1, "cparse_", (ftnlen)1272)] = kwdnum;
	    linstk[(i__1 = stklev - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge("lin"
		    "stk", i__1, "cparse_", (ftnlen)1273)] = lstcmd;

/*           Zero out the number of child nodes for KWDNUM. */

	    i__3 = chlptr[(i__1 = kwdnum) < 101 && 0 <= i__1 ? i__1 : s_rnge(
		    "chlptr", i__1, "cparse_", (ftnlen)1278)] - chlptr[(i__5 =
		     kwdnum - 1) < 101 && 0 <= i__5 ? i__5 : s_rnge("chlptr", 
		    i__5, "cparse_", (ftnlen)1278)];
	    cleari_(&i__3, &kwcntr[(i__2 = stklev * 40) < 240 && 0 <= i__2 ? 
		    i__2 : s_rnge("kwcntr", i__2, "cparse_", (ftnlen)1278)]);

/*           Form the symbol. */

	    s_copy(symbol, kwnam + (((i__1 = kwstk[1] - 1) < 100 && 0 <= i__1 
		    ? i__1 : s_rnge("kwnam", i__1, "cparse_", (ftnlen)1283)) 
		    << 5), (ftnlen)100, (ftnlen)32);
	    i__1 = stklev;
	    for (i__ = 2; i__ <= i__1; ++i__) {
		i__8 = chlptr[(i__3 = kwstk[(i__5 = i__ - 2) < 6 && 0 <= i__5 
			? i__5 : s_rnge("kwstk", i__5, "cparse_", (ftnlen)
			1286)]) < 101 && 0 <= i__3 ? i__3 : s_rnge("chlptr", 
			i__3, "cparse_", (ftnlen)1286)] - chlptr[(i__4 = 
			kwstk[(i__5 = i__ - 2) < 6 && 0 <= i__5 ? i__5 : 
			s_rnge("kwstk", i__5, "cparse_", (ftnlen)1286)] - 1) <
			 101 && 0 <= i__4 ? i__4 : s_rnge("chlptr", i__4, 
			"cparse_", (ftnlen)1286)];
		p = isrchi_(&kwstk[(i__2 = i__ - 1) < 6 && 0 <= i__2 ? i__2 : 
			s_rnge("kwstk", i__2, "cparse_", (ftnlen)1286)], &
			i__8, &chlnum[(i__7 = chlptr[(i__6 = kwstk[(i__5 = 
			i__ - 2) < 6 && 0 <= i__5 ? i__5 : s_rnge("kwstk", 
			i__5, "cparse_", (ftnlen)1286)] - 1) < 101 && 0 <= 
			i__6 ? i__6 : s_rnge("chlptr", i__6, "cparse_", (
			ftnlen)1286)]) < 100 && 0 <= i__7 ? i__7 : s_rnge(
			"chlnum", i__7, "cparse_", (ftnlen)1286)]);
		crtptr_(ch__2, (ftnlen)100, symbol, &kwcntr[(i__5 = p + (i__ 
			- 2) * 40 - 1) < 240 && 0 <= i__5 ? i__5 : s_rnge(
			"kwcntr", i__5, "cparse_", (ftnlen)1287)], kwnam + (((
			i__3 = kwstk[(i__2 = i__) < 6 && 0 <= i__2 ? i__2 : 
			s_rnge("kwstk", i__2, "cparse_", (ftnlen)1287)] - 1) <
			 100 && 0 <= i__3 ? i__3 : s_rnge("kwnam", i__3, 
			"cparse_", (ftnlen)1287)) << 5), (ftnlen)100, (ftnlen)
			32);
		s_copy(symbol, ch__2, (ftnlen)100, (ftnlen)100);
	    }
	    syenqc_(symbol, value, cmdsym, cmdptr, cmdval, (ftnlen)100, (
		    ftnlen)300, cmdsym_len, cmdval_len);

/*           What next? If we're at the end of commands, we need to */
/*           check for closure. If not, we'll parse LINE (this time */
/*           with KWD set to ' '). */

	    if (*eoc) {
		s_copy(action, "DONE", (ftnlen)10, (ftnlen)4);
	    } else {
		s_copy(action, "PARSE", (ftnlen)10, (ftnlen)5);
		s_copy(kwd, " ", (ftnlen)32, (ftnlen)1);
		s_copy(value, " ", (ftnlen)300, (ftnlen)1);
	    }
	}
    }

/*     We've evaluated the last of the commands. Here, we check for */
/*     closure (we'll decrease STKLEV to zero, checking for minimum */
/*     child nodes). */

    newlev = 0;
    i__1 = newlev;
    for (i__ = stklev; i__ >= i__1; --i__) {

/*        J is the index in CHLNUM where the child keywords are */
/*        given for parent keyword index I. */

	j = chlptr[(i__2 = kwstk[(i__5 = i__) < 6 && 0 <= i__5 ? i__5 : 
		s_rnge("kwstk", i__5, "cparse_", (ftnlen)1322)] - 1) < 101 && 
		0 <= i__2 ? i__2 : s_rnge("chlptr", i__2, "cparse_", (ftnlen)
		1322)];
	i__4 = chlptr[(i__2 = kwstk[(i__5 = i__) < 6 && 0 <= i__5 ? i__5 : 
		s_rnge("kwstk", i__5, "cparse_", (ftnlen)1324)]) < 101 && 0 <=
		 i__2 ? i__2 : s_rnge("chlptr", i__2, "cparse_", (ftnlen)1324)
		] - chlptr[(i__3 = kwstk[(i__5 = i__) < 6 && 0 <= i__5 ? i__5 
		: s_rnge("kwstk", i__5, "cparse_", (ftnlen)1324)] - 1) < 101 
		&& 0 <= i__3 ? i__3 : s_rnge("chlptr", i__3, "cparse_", (
		ftnlen)1324)];
	for (k = 1; k <= i__4; ++k) {
	    if (kwcntr[(i__5 = k + i__ * 40 - 1) < 240 && 0 <= i__5 ? i__5 : 
		    s_rnge("kwcntr", i__5, "cparse_", (ftnlen)1326)] < nummin[
		    (i__2 = k + j - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge(
		    "nummin", i__2, "cparse_", (ftnlen)1326)]) {
		inttxt_(&nummin[(i__5 = k + j - 1) < 100 && 0 <= i__5 ? i__5 :
			 s_rnge("nummin", i__5, "cparse_", (ftnlen)1328)], 
			minch, (ftnlen)20);
		if (i__ > 0) {
		    s_copy(msg, "Line #:", (ftnlen)320, (ftnlen)7);
		} else {
		    s_copy(msg, " ", (ftnlen)320, (ftnlen)1);
		}
		if (nummax[(i__5 = k + j - 1) < 100 && 0 <= i__5 ? i__5 : 
			s_rnge("nummax", i__5, "cparse_", (ftnlen)1336)] == 
			nummin[(i__2 = k + j - 1) < 100 && 0 <= i__2 ? i__2 : 
			s_rnge("nummin", i__2, "cparse_", (ftnlen)1336)]) {
		    suffix_("% %", &c__1, msg, (ftnlen)3, (ftnlen)320);
		    lcase_(minch + 1, minch + 1, (ftnlen)19, (ftnlen)19);
		} else {
		    suffix_("At least % %", &c__1, msg, (ftnlen)12, (ftnlen)
			    320);
		    lcase_(minch, minch, (ftnlen)20, (ftnlen)20);
		}
		repmc_(msg, "%", minch, msg, (ftnlen)320, (ftnlen)1, (ftnlen)
			20, (ftnlen)320);
		repmc_(msg, "%", kwnam + (((i__2 = chlnum[(i__5 = k + j - 1) <
			 100 && 0 <= i__5 ? i__5 : s_rnge("chlnum", i__5, 
			"cparse_", (ftnlen)1345)] - 1) < 100 && 0 <= i__2 ? 
			i__2 : s_rnge("kwnam", i__2, "cparse_", (ftnlen)1345))
			 << 5), msg, (ftnlen)320, (ftnlen)1, (ftnlen)32, (
			ftnlen)320);
		if (nummin[(i__5 = k + j - 1) < 100 && 0 <= i__5 ? i__5 : 
			s_rnge("nummin", i__5, "cparse_", (ftnlen)1347)] == 1)
			 {
		    suffix_("command must appear", &c__1, msg, (ftnlen)19, (
			    ftnlen)320);
		} else {
		    suffix_("commands must appear", &c__1, msg, (ftnlen)20, (
			    ftnlen)320);
		}
		if (i__ > 0) {
		    suffix_("after the % command.", &c__1, msg, (ftnlen)20, (
			    ftnlen)320);
		    repmc_(msg, "%", kwnam + (((i__2 = kwstk[(i__5 = i__) < 6 
			    && 0 <= i__5 ? i__5 : s_rnge("kwstk", i__5, "cpa"
			    "rse_", (ftnlen)1355)] - 1) < 100 && 0 <= i__2 ? 
			    i__2 : s_rnge("kwnam", i__2, "cparse_", (ftnlen)
			    1355)) << 5), msg, (ftnlen)320, (ftnlen)1, (
			    ftnlen)32, (ftnlen)320);
		    lstcmd = linstk[(i__5 = i__ - 1) < 5 && 0 <= i__5 ? i__5 :
			     s_rnge("linstk", i__5, "cparse_", (ftnlen)1357)];
		} else {
		    suffix_(".", &c__0, msg, (ftnlen)1, (ftnlen)320);
		}
		ljust_(msg, msg, (ftnlen)320, (ftnlen)320);
		*err = TRUE_;
		chkout_("EVALCP", (ftnlen)6);
		return 0;
	    }
	}
    }

/*     That's it. */

    chkout_("EVALCP", (ftnlen)6);
    return 0;
/* $Procedure CPERR (Command parser error) */

L_cperr:
/* $ Abstract */

/*     Return the reason and line number of a parser error. */

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

/*     PARSING */
/*     STRING */

/* $ Declarations */

/*     CHARACTER*(*)         REASON */
/*     INTEGER               LINNUM */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*      REASON    O    Reason for error. */
/*      LINNUM    O    Line number offset of the error. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     REASON     is the reason a parser error occurred. REASON may */
/*                contain a #, indicating the line number offset of the */
/*                error. The first line sent to EVALCP is line 1. The */
/*                marker # should be substituted the corrected line */
/*                number before REASON is displayed. */

/*     LINNUM     is the line offset number of the command that caused */
/*                the error. This number may not be needed if REASON */
/*                does not contain the substitution marker, #. For */
/*                example, REASON may be 'One LEAPSECONDS_FILE command */
/*                must appear.'. No line number is necessary in this */
/*                case. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine should only be called if EVALCP indicates an error. */
/*     EVALCP should not be called after an error is found. Doing so */
/*     will produce undesired results, that is you can only find the */
/*     first error. */
/*     (Maybe in the next version this can be fixed.) */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     M.J. Spencer   (JPL) */

/* $ Version */

/* -    Beta Version 1.1.0, 18-JUN-1999 (WLT) */

/*        Balanced CHKIN/CHKOUT calls. */

/* -    Beta Version 1.0.0, 11-DEC-1993 (MJS) */

/* -& */
    s_copy(reason, msg, reason_len, (ftnlen)320);
    *linnum = lstcmd;
    return 0;
} /* cparse_ */

/* Subroutine */ int cparse_(char *kwdsym, integer *kwdptr, char *kwdval, 
	char *head, char *line, logical *eoc, char *cmdsym, integer *cmdptr, 
	char *cmdval, logical *err, char *reason, integer *linnum, ftnlen 
	kwdsym_len, ftnlen kwdval_len, ftnlen head_len, ftnlen line_len, 
	ftnlen cmdsym_len, ftnlen cmdval_len, ftnlen reason_len)
{
    return cparse_0_(0, kwdsym, kwdptr, kwdval, head, line, eoc, cmdsym, 
	    cmdptr, cmdval, err, reason, linnum, kwdsym_len, kwdval_len, 
	    head_len, line_len, cmdsym_len, cmdval_len, reason_len);
    }

/* Subroutine */ int initcp_(char *kwdsym, integer *kwdptr, char *kwdval, 
	char *head, ftnlen kwdsym_len, ftnlen kwdval_len, ftnlen head_len)
{
    return cparse_0_(1, kwdsym, kwdptr, kwdval, head, (char *)0, (logical *)0,
	     (char *)0, (integer *)0, (char *)0, (logical *)0, (char *)0, (
	    integer *)0, kwdsym_len, kwdval_len, head_len, (ftnint)0, (ftnint)
	    0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int evalcp_(char *line, logical *eoc, char *cmdsym, integer *
	cmdptr, char *cmdval, logical *err, ftnlen line_len, ftnlen 
	cmdsym_len, ftnlen cmdval_len)
{
    return cparse_0_(2, (char *)0, (integer *)0, (char *)0, (char *)0, line, 
	    eoc, cmdsym, cmdptr, cmdval, err, (char *)0, (integer *)0, (
	    ftnint)0, (ftnint)0, (ftnint)0, line_len, cmdsym_len, cmdval_len, 
	    (ftnint)0);
    }

/* Subroutine */ int cperr_(char *reason, integer *linnum, ftnlen reason_len)
{
    return cparse_0_(3, (char *)0, (integer *)0, (char *)0, (char *)0, (char *
	    )0, (logical *)0, (char *)0, (integer *)0, (char *)0, (logical *)
	    0, reason, linnum, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0, reason_len);
    }

