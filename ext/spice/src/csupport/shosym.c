/* shosym.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

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

/* Subroutine */ int shosym_(char *templt, ftnlen templt_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    char name__[32], line[132];
    integer ncol, item[3];
    logical tran;
    integer size[3];
    char rest[132];
    integer i__, n, r__, space[3];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    repmc_(char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, 
	    ftnlen);
    char value[2000];
    integer width[3];
    extern /* Subroutine */ int stran_(char *, char *, logical *, ftnlen, 
	    ftnlen);
    extern integer rtrim_(char *, ftnlen);
    logical justr[3];
    integer lmarge, pagewd;
    char spcial[1*3];
    extern /* Subroutine */ int pagscn_(char *, ftnlen);
    char margin[32], messge[132];
    extern /* Subroutine */ int pagset_(char *, integer *, ftnlen), tabrpt_(
	    integer *, integer *, integer *, integer *, logical *, logical *, 
	    char *, integer *, integer *, U_fp, ftnlen);
    char myline[132];
    extern /* Subroutine */ int pagrst_(void), nspmrg_(char *, ftnlen), 
	    symget_(char *, char *, ftnlen, ftnlen);
    char frstwd[32];
    extern /* Subroutine */ int nspglr_(integer *, integer *), nextwd_(char *,
	     char *, char *, ftnlen, ftnlen, ftnlen), sympat_(char *, ftnlen),
	     nspwln_(char *, ftnlen);
    extern /* Subroutine */ int retsym_();
    logical presrv[3];
    extern /* Subroutine */ int setsym_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen);
    char def[2000];
    extern /* Subroutine */ int nicepr_1__(char *, char *, S_fp, ftnlen, 
	    ftnlen);

    r__ = rtrim_(templt, templt_len);
    sympat_(templt, r__);
    symget_(name__, def, (ftnlen)32, (ftnlen)2000);
    nspmrg_(margin, (ftnlen)32);
    if (s_cmp(name__, " ", (ftnlen)32, (ftnlen)1) == 0) {
	s_copy(messge, "There are no symbols that match the template \"#\".", 
		(ftnlen)132, (ftnlen)49);
	repmc_(messge, "#", templt, messge, (ftnlen)132, (ftnlen)1, r__, (
		ftnlen)132);
	nicepr_1__(messge, margin, (S_fp)nspwln_, (ftnlen)132, (ftnlen)32);
	return 0;
    }

/*     If still here there are some matching symbols.  Set up the */
/*     standard defaults. */

    s_copy(line, "=========================================================="
	    "================================================================"
	    "==============================================", (ftnlen)132, (
	    ftnlen)168);
    presrv[0] = TRUE_;
    presrv[1] = TRUE_;
    presrv[2] = TRUE_;
    lmarge = 1;
    space[0] = 2;
    space[1] = 2;
    space[2] = 2;
    *(unsigned char *)&spcial[0] = ' ';
    *(unsigned char *)&spcial[1] = ' ';
    *(unsigned char *)&spcial[2] = ' ';
    justr[0] = FALSE_;
    justr[1] = FALSE_;
    justr[2] = FALSE_;

/*     Get the width of the page and based upon that determine */
/*     the basic table style that will be used to display the */
/*     symbol definition. */

    nspglr_(&n, &pagewd);
    width[0] = 14;
    width[1] = 30;
    width[2] = 30;
    size[0] = 1;
    size[1] = 1;
    size[2] = 1;
    item[0] = 1;
    item[1] = 2;
    item[2] = 3;
    ncol = 3;

/*     Adjust all of the columns */

    i__1 = ncol;
    for (i__ = 1; i__ <= i__1; ++i__) {
	width[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("width", i__2,
		 "shosym_", (ftnlen)156)] = width[(i__3 = i__ - 1) < 3 && 0 <=
		 i__3 ? i__3 : s_rnge("width", i__3, "shosym_", (ftnlen)156)] 
		* pagewd / 80;
    }
    pagewd = 0;
    i__1 = ncol;
    for (i__ = 1; i__ <= i__1; ++i__) {
	pagewd = width[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge(
		"width", i__2, "shosym_", (ftnlen)162)] + space[(i__3 = i__ - 
		1) < 3 && 0 <= i__3 ? i__3 : s_rnge("space", i__3, "shosym_", 
		(ftnlen)162)] + pagewd;
    }
    pagewd -= space[(i__1 = ncol - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("space"
	    , i__1, "shosym_", (ftnlen)165)];
    nspwln_(" ", (ftnlen)1);
    nspwln_("Symbols Matching Request: ", (ftnlen)26);
    nspwln_(" ", (ftnlen)1);
    pagrst_();
    pagset_("PAGEWIDTH", &pagewd, (ftnlen)9);
    pagscn_("BODY", (ftnlen)4);
    setsym_("Symbol Name", "Definition", "Expanded Value", (ftnlen)11, (
	    ftnlen)10, (ftnlen)14);
    tabrpt_(&ncol, item, size, width, justr, presrv, spcial, &lmarge, space, (
	    U_fp)retsym_, (ftnlen)1);
    s_copy(myline, line, (ftnlen)132, pagewd);
    nspwln_(myline, (ftnlen)132);
    while(s_cmp(name__, " ", (ftnlen)32, (ftnlen)1) != 0) {

/*        Expand this symbol until there's nothing left to do. */

	s_copy(value, def, (ftnlen)2000, (ftnlen)2000);
	tran = TRUE_;
	while(tran) {
	    nextwd_(def, frstwd, rest, (ftnlen)2000, (ftnlen)32, (ftnlen)132);
	    ucase_(frstwd, frstwd, (ftnlen)32, (ftnlen)32);
	    if (s_cmp(frstwd, "DEFINE", (ftnlen)32, (ftnlen)6) != 0 && s_cmp(
		    frstwd, "UNDEFINE", (ftnlen)32, (ftnlen)8) != 0) {
		stran_(value, value, &tran, (ftnlen)2000, (ftnlen)2000);
	    } else {
		tran = FALSE_;
	    }
	}
	setsym_(name__, def, value, (ftnlen)32, (ftnlen)2000, (ftnlen)2000);
	tabrpt_(&ncol, item, size, width, justr, presrv, spcial, &lmarge, 
		space, (U_fp)retsym_, (ftnlen)1);
	symget_(name__, def, (ftnlen)32, (ftnlen)2000);
    }
    nspwln_(" ", (ftnlen)1);
    return 0;
} /* shosym_ */

