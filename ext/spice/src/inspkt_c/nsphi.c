/* nsphi.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;

/* Subroutine */ int nsphi_(char *versn, ftnlen versn_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern integer ltrim_(char *, ftnlen);
    char tkver[80];
    extern integer rtrim_(char *, ftnlen);
    integer sizet, lt, lv, mv, rv;
    extern /* Subroutine */ int prefix_(char *, integer *, char *, ftnlen, 
	    ftnlen), nspioh_(char *, ftnlen), suffix_(char *, integer *, char 
	    *, ftnlen, ftnlen), nspgst_(char *, logical *, ftnlen), nspwln_(
	    char *, ftnlen);
    logical status[3];
    extern /* Subroutine */ int tkvrsn_(char *, char *, ftnlen, ftnlen), 
	    nsppst_(char *, logical *, ftnlen);
    char tkv[80];


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

/* $ Version */

/* -    Inspekt Version 2.1.0, 11-NOV-2005 (BVS) */

/*        Removed copyright statement from the banner. */

/* -    Inspekt Version 2.0.0, 21-JUL-1997 (WLT) */

/*        Added the line that gives the version of SPICELIB this */
/*        program was linked against. */

/* -    Inspekt Version 1.1.0, 15-JUN-1995 (WLT) */

/*        Added Copyright notice. */

/* -& */
    lv = ltrim_(versn, versn_len);
    rv = rtrim_(versn, versn_len);
    mv = (rv + lv) / 2;
    tkvrsn_("TOOLKIT", tkv, (ftnlen)7, (ftnlen)80);
    prefix_("(SPICE Toolkit", &c__1, tkv, (ftnlen)14, (ftnlen)80);
    suffix_(")", &c__0, tkv, (ftnlen)1, (ftnlen)80);
    sizet = rtrim_(tkv, (ftnlen)80);
    lt = mv - sizet / 2;
    s_copy(tkver, " ", (ftnlen)80, (ftnlen)1);
    s_copy(tkver + (lt - 1), tkv, 80 - (lt - 1), (ftnlen)80);
    nspgst_("LOG", status, (ftnlen)3);
    nspioh_("LOG", (ftnlen)3);
    nspwln_(" ", (ftnlen)1);
    nspwln_(versn, versn_len);
    nspwln_(tkver, (ftnlen)80);
    nspwln_(" ", (ftnlen)1);
    nspwln_("       A NAIF program for inspecting the contents of E-kernels.",
	     (ftnlen)63);
    nspwln_(" ", (ftnlen)1);
    nspwln_("                         by Bill Taber ", (ftnlen)39);
    nspwln_("                    with assistance from", (ftnlen)40);
    nspwln_("                  Hester Neilan and Nat Bachman", (ftnlen)47);
    nspwln_(" ", (ftnlen)1);
    nspwln_("   Please note: ", (ftnlen)16);
    nspwln_(" ", (ftnlen)1);
    nspwln_("   1) Commands may extend over multiple lines but", (ftnlen)49);
    nspwln_("      ALL commands must end with a semi-colon.", (ftnlen)46);
    nspwln_(" ", (ftnlen)1);
    nspwln_("   2) To leave the program type \"EXIT;\". ", (ftnlen)41);
    nspwln_(" ", (ftnlen)1);
    nspwln_("   3) To get a summary of commands type \"HELP;\" or \"help;\". "
	    , (ftnlen)60);
    nspwln_(" ", (ftnlen)1);
    nsppst_("LOG", status, (ftnlen)3);
    return 0;
} /* nsphi_ */

