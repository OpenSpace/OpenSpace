/* disply.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b3 = 0.;
static integer c__1 = 1;
static integer c__3 = 3;
static integer c_b177 = 100000;

/* $Procedure DISPLY ( BRIEF Display Summary ) */
/* Subroutine */ int disply_(char *fmtpic, logical *tdsp, logical *gdsp, 
	logical *sdsp, logical *obnam, integer *objlis, char *winsym, integer 
	*winptr, doublereal *winval, char *timtyp, char *kertyp, ftnlen 
	fmtpic_len, ftnlen winsym_len, ftnlen timtyp_len, ftnlen kertyp_len)
{
    /* System generated locals */
    address a__1[3];
    integer i__1, i__2, i__3[3], i__4, i__5;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static char name__[64];
    static logical same;
    static char line[132];
    static integer nobj, objn[2], sobj, size;
    static char rest[132];
    static integer b, e, i__, j;
    extern integer cardc_(char *, ftnlen);
    static integer s;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char names[64*100006], sname[64];
    static logical found;
    extern integer rtrim_(char *, ftnlen);
    static integer n1, n2;
    static char p1[8], p2[8];
    static integer start;
    static logical group;
    static doublereal sstop[1000000];
    static integer objct2[3];
    static char header[132*2], wd[8];
    extern integer objact_(integer *);
    extern /* Subroutine */ int maknam_(integer *, integer *, logical *, char 
	    *, char *, ftnlen, ftnlen), appndc_(char *, char *, ftnlen, 
	    ftnlen);
    static integer object[3], remain;
    extern /* Subroutine */ int objget_(integer *, integer *, integer *), 
	    objrem_(integer *, integer *), orderd_(doublereal *, integer *, 
	    integer *), reordd_(integer *, integer *, doublereal *), rmaini_(
	    integer *, integer *, integer *, integer *);
    static char timlbl[8];
    static integer iorder[1000000], npline;
    extern /* Subroutine */ int objnth_(integer *, integer *, integer *, 
	    logical *);
    static doublereal filwin[100006];
    static integer nlines, objtmp[2];
    extern integer touchi_(integer *);
    extern /* Subroutine */ int distim_(char *, doublereal *, char *, char *, 
	    ftnlen, ftnlen, ftnlen);
    static integer widest;
    extern integer objsiz_(integer *);
    static integer stotal;
    extern /* Subroutine */ int chkout_(char *, ftnlen), nextwd_(char *, char 
	    *, char *, ftnlen, ftnlen, ftnlen);
    static integer ngroup;
    extern /* Subroutine */ int objnxt_(integer *, integer *, integer *, 
	    logical *), prname_(integer *, integer *, char *, char *, char *, 
	    integer *, char *, char *, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen)
	    , sygetd_(char *, char *, integer *, doublereal *, integer *, 
	    doublereal *, logical *, ftnlen, ftnlen);
    extern logical return_(void);
    static doublereal sstart[1000000], lstwin[100006];
    static char timstr[64];
    extern /* Subroutine */ int writit_(char *, ftnlen), reordi_(integer *, 
	    integer *, integer *), ssizec_(integer *, char *, ftnlen);
    static logical fnd;
    static integer obj[2], obj1[1000000], obj2[1000000];

/* $ Abstract */

/*     Display BRIEF summary. */

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

/*     None. */

/* $ Declarations */
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

/* $ Author_and_Institution */

/*     W.L. Taber     (NAIF) */
/*     B.V. Semenov   (NAIF) */

/* $ Version */

/* -    BRIEF Version 4.0.0, 08-SEP-2010 (BVS) */

/*        Moved WINRM from main program to this include file. */

/* -    BRIEF Version 3.0.0, 14-JAN-2008 (BVS) */

/*        Increased MAXBOD to 100,000 (from 20,000). */

/*        Increased CMDSIZ to 25,000 (from 4,000). */

/*        Updated version string and changed its format to */
/*        '#.#.#, Month DD, YYYY' (from '#.#.#'). */

/* -    BRIEF Version 1.0.0, 14-MAR-1996 (WLT) */

/*        Initial release. */

/* -& */

/*     The Version is stored as a string. */


/*     MAXUSE is the maximum number of bodies that can be explicitly */
/*     specified on the command line for brief summaries. */


/*     The longest command line that can be accommodated is */
/*     given by CMDSIZ */


/*     The maximum number of bodies that can be summarized is stored */
/*     in the parameter MAXBOD */


/*     The average number of intervals per body */


/*     The largest expected window */


/*     Room in the DP symbol table that holds all windows for all */
/*     objects. */


/*     End of inlcude file. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FMTPIC     I   Body name/ID format picture (see BRIEF.PGM) */
/*     TDSP       I   Tabular display flag */
/*     GDSP       I   Grouping display flag */
/*     SDSP       I   Time-sorted tabular display flag */
/*     OBNAM      I   Name ordering flag */
/*     OBJLIS     I   List of object (?) */
/*     WINSYM     I   Symbol table with object attributes (?) */
/*     WINPTR     I   Symbol table with object attributes (?) */
/*     WINVAL     I   Symbol table with object attributes (?) */
/*     TIMTYP     I   Output time type (see DISTIM.FOR) */
/*     KERTYP     I   Kernel type (SPK, PCK) */

/* $ Detailed_Input */

/*     See Brief_I/O. */

/* $ Detailed_Output */

/*     This routine return no outputs. Instead it prints summary of */
/*     provided input information to STDOUT. */

/* $ Parameters */

/*     LBCELL. */

/* $ Exceptions */

/*     1) Errors may be signaled by routines in the calling tree of */
/*        this routine. */

/* $ Files */

/*     TBD. */

/* $ Particulars */

/*     TBD. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     This routine must not be called by any routines except BRIEF's */
/*     main program. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov   (JPL) */
/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    BRIEF Version 3.0.0, 08-SEP-2010 (BVS) */

/*        Added sorted-by-time tabular output (-s). Changed calling */
/*        sequence: add sorted-by-time flag (SDSP). */

/* -    BRIEF Version 2.0.0, 22-OCT-2007 (BVS) */

/*        Added output time type to the argument list. Changed to */
/*        call DISTIM to format output time and provide time system */
/*        label for the summary table header. */

/* -    BRIEF Version 1.0.0, 14-MAR-1996 (WLT) */

/*        Bill's initial version. */

/* -& */
/* $ Index_Entries */

/*     display summary by BRIEF */

/* -& */

/*     SPICELIB functions */


/*     Parameters */


/*     Local Variables. */


/*     SPICELIB Calls */


/*     Saved variables */

/*     The SAVE statement that appears here causes f2c to create */
/*     local variables with static duration.  This enables the CSPICE */
/*     version of brief to run under cygwin. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DISPLY", (ftnlen)6);
    }

/*     Get time system label for the table header. */

    distim_(timtyp, &c_b3, timlbl, timstr, timtyp_len, (ftnlen)8, (ftnlen)64);

/*     Set local grouping flag. */

    group = ! (*tdsp) || *gdsp;

/*     First take apart the format picture to see what */
/*     the various components are. */

    nextwd_(fmtpic, p1, rest, fmtpic_len, (ftnlen)8, (ftnlen)132);
    nextwd_(rest, wd, rest, (ftnlen)132, (ftnlen)8, (ftnlen)132);
    nextwd_(rest, p2, rest, (ftnlen)132, (ftnlen)8, (ftnlen)132);
    size = 1;
    if (s_cmp(p2, " ", (ftnlen)8, (ftnlen)1) != 0) {
	size = 3;
    }

/*     Find out the width of the widest name. */

    nobj = objact_(objlis);
    sobj = objsiz_(objlis);

/*     If we don't have any objects to display then */
/*     we just return. */

    if (nobj == 0) {
	chkout_("DISPLY", (ftnlen)6);
	return 0;
    }
    objnth_(objlis, &c__1, obj, &found);
    widest = 0;
    while(found) {
	objget_(obj, objlis, object);
	objnxt_(obj, objlis, objn, &found);
	prname_(object, &sobj, p1, wd, p2, &size, kertyp, name__, (ftnlen)8, (
		ftnlen)8, (ftnlen)8, kertyp_len, (ftnlen)64);
/* Computing MAX */
	i__1 = widest, i__2 = rtrim_(name__, (ftnlen)64);
	widest = max(i__1,i__2);
	obj[0] = objn[0];
	obj[1] = objn[1];
    }

/*     Are we going to group by or sort by time window? If not, this is */
/*     pretty easy. Just display tabular output. */

    if (*tdsp && ! (*gdsp) && ! (*sdsp)) {
	s = widest + 3;
	e = s + 32;
	if (s_cmp(kertyp, "SPK", kertyp_len, (ftnlen)3) == 0) {
	    s_copy(line, "Bodies", (ftnlen)132, (ftnlen)6);
	} else if (s_cmp(kertyp, "PCK", kertyp_len, (ftnlen)3) == 0) {
	    s_copy(line, "Frames", (ftnlen)132, (ftnlen)6);
	} else {
	    s_copy(line, "IDs", (ftnlen)132, (ftnlen)3);
	}
/* Writing concatenation */
	i__3[0] = 19, a__1[0] = "Start of Interval (";
	i__3[1] = rtrim_(timlbl, (ftnlen)8), a__1[1] = timlbl;
	i__3[2] = 1, a__1[2] = ")";
	s_cat(line + (s - 1), a__1, i__3, &c__3, 132 - (s - 1));
/* Writing concatenation */
	i__3[0] = 17, a__1[0] = "End of Interval (";
	i__3[1] = rtrim_(timlbl, (ftnlen)8), a__1[1] = timlbl;
	i__3[2] = 1, a__1[2] = ")";
	s_cat(line + (e - 1), a__1, i__3, &c__3, 132 - (e - 1));
	writit_(line, (ftnlen)132);
	s_copy(line, "-------", (ftnlen)132, (ftnlen)7);
	s_copy(line + (s - 1), "-----------------------------", 132 - (s - 1),
		 (ftnlen)29);
	s_copy(line + (e - 1), "-----------------------------", 132 - (e - 1),
		 (ftnlen)29);
	writit_(line, (ftnlen)132);
	objnth_(objlis, &c__1, obj, &found);
	n1 = 0;
	while(found) {
	    s_copy(line, " ", (ftnlen)132, (ftnlen)1);
	    objget_(obj, objlis, object);
	    prname_(object, &sobj, p1, wd, p2, &size, kertyp, line, (ftnlen)8,
		     (ftnlen)8, (ftnlen)8, kertyp_len, (ftnlen)132);
	    maknam_(object, &sobj, obnam, kertyp, name__, kertyp_len, (ftnlen)
		    64);
	    sygetd_(name__, winsym, winptr, winval, &n2, &filwin[6], &found, (
		    ftnlen)64, winsym_len);
	    if (n2 == n1) {
		same = TRUE_;
		i__ = 1;
		while(same && i__ <= n1) {
		    same = filwin[(i__1 = i__ + 5) < 100006 && 0 <= i__1 ? 
			    i__1 : s_rnge("filwin", i__1, "disply_", (ftnlen)
			    359)] == lstwin[(i__2 = i__ + 5) < 100006 && 0 <= 
			    i__2 ? i__2 : s_rnge("lstwin", i__2, "disply_", (
			    ftnlen)359)];
		    ++i__;
		}
	    } else {
		same = FALSE_;
	    }
	    if (! same) {
		i__1 = n2;
		for (i__ = 1; i__ <= i__1; i__ += 2) {
		    distim_(timtyp, &filwin[(i__2 = i__ + 5) < 100006 && 0 <= 
			    i__2 ? i__2 : s_rnge("filwin", i__2, "disply_", (
			    ftnlen)371)], timlbl, line + (s - 1), timtyp_len, 
			    (ftnlen)8, 132 - (s - 1));
		    distim_(timtyp, &filwin[(i__2 = i__ + 6) < 100006 && 0 <= 
			    i__2 ? i__2 : s_rnge("filwin", i__2, "disply_", (
			    ftnlen)372)], timlbl, line + (e - 1), timtyp_len, 
			    (ftnlen)8, 132 - (e - 1));
		    writit_(line, (ftnlen)132);
		    s_copy(line, " ", (ftnlen)132, (ftnlen)1);
		    lstwin[(i__2 = i__ + 5) < 100006 && 0 <= i__2 ? i__2 : 
			    s_rnge("lstwin", i__2, "disply_", (ftnlen)375)] = 
			    filwin[(i__4 = i__ + 5) < 100006 && 0 <= i__4 ? 
			    i__4 : s_rnge("filwin", i__4, "disply_", (ftnlen)
			    375)];
		    lstwin[(i__2 = i__ + 6) < 100006 && 0 <= i__2 ? i__2 : 
			    s_rnge("lstwin", i__2, "disply_", (ftnlen)376)] = 
			    filwin[(i__4 = i__ + 6) < 100006 && 0 <= i__4 ? 
			    i__4 : s_rnge("filwin", i__4, "disply_", (ftnlen)
			    376)];
		    n1 = n2;
		}
	    } else {
		i__1 = s + 11;
		s_copy(line + i__1, "Same coverage as previous object ", 132 
			- i__1, (ftnlen)33);
		writit_(line, (ftnlen)132);
	    }
	    objnxt_(obj, objlis, objtmp, &found);
	    obj[0] = touchi_(objtmp);
	    obj[1] = touchi_(&objtmp[1]);
	}

/*     Were we asked to do tabular output sorted by start time for each */
/*     SPK body or PCK frame? */

    } else if (*tdsp && *sdsp) {
	s = widest + 3;
	e = s + 32;
	if (s_cmp(kertyp, "SPK", kertyp_len, (ftnlen)3) == 0) {
	    s_copy(line, "Bodies", (ftnlen)132, (ftnlen)6);
	} else if (s_cmp(kertyp, "PCK", kertyp_len, (ftnlen)3) == 0) {
	    s_copy(line, "Frames", (ftnlen)132, (ftnlen)6);
	} else {
	    s_copy(line, "IDs", (ftnlen)132, (ftnlen)3);
	}
/* Writing concatenation */
	i__3[0] = 19, a__1[0] = "Start of Interval (";
	i__3[1] = rtrim_(timlbl, (ftnlen)8), a__1[1] = timlbl;
	i__3[2] = 1, a__1[2] = ")";
	s_cat(line + (s - 1), a__1, i__3, &c__3, 132 - (s - 1));
/* Writing concatenation */
	i__3[0] = 17, a__1[0] = "End of Interval (";
	i__3[1] = rtrim_(timlbl, (ftnlen)8), a__1[1] = timlbl;
	i__3[2] = 1, a__1[2] = ")";
	s_cat(line + (e - 1), a__1, i__3, &c__3, 132 - (e - 1));
	writit_(line, (ftnlen)132);
	s_copy(line, "-------", (ftnlen)132, (ftnlen)7);
	s_copy(line + (s - 1), "-----------------------------", 132 - (s - 1),
		 (ftnlen)29);
	s_copy(line + (e - 1), "-----------------------------", 132 - (e - 1),
		 (ftnlen)29);
	writit_(line, (ftnlen)132);
	objnth_(objlis, &c__1, obj, &found);
	n1 = 0;
	while(found) {

/*           Get and buffer individual coverage intervals for this */
/*           object. */

	    objget_(obj, objlis, object);
	    maknam_(object, &sobj, obnam, kertyp, name__, kertyp_len, (ftnlen)
		    64);
	    sygetd_(name__, winsym, winptr, winval, &n2, &filwin[6], &found, (
		    ftnlen)64, winsym_len);
	    stotal = 0;
	    i__1 = n2;
	    for (i__ = 1; i__ <= i__1; i__ += 2) {
		++stotal;
		obj1[(i__2 = stotal - 1) < 1000000 && 0 <= i__2 ? i__2 : 
			s_rnge("obj1", i__2, "disply_", (ftnlen)443)] = obj[0]
			;
		obj2[(i__2 = stotal - 1) < 1000000 && 0 <= i__2 ? i__2 : 
			s_rnge("obj2", i__2, "disply_", (ftnlen)444)] = obj[1]
			;
		sstart[(i__2 = stotal - 1) < 1000000 && 0 <= i__2 ? i__2 : 
			s_rnge("sstart", i__2, "disply_", (ftnlen)445)] = 
			filwin[(i__4 = i__ + 5) < 100006 && 0 <= i__4 ? i__4 :
			 s_rnge("filwin", i__4, "disply_", (ftnlen)445)];
		sstop[(i__2 = stotal - 1) < 1000000 && 0 <= i__2 ? i__2 : 
			s_rnge("sstop", i__2, "disply_", (ftnlen)446)] = 
			filwin[(i__4 = i__ + 6) < 100006 && 0 <= i__4 ? i__4 :
			 s_rnge("filwin", i__4, "disply_", (ftnlen)446)];
	    }

/*           Buffer coverage intervals for subsequent objects as long as */
/*           these objects have the same ID or we run out of objects. */

	    objnxt_(obj, objlis, objtmp, &found);
	    while(found) {
		objget_(objtmp, objlis, objct2);
		if (object[0] == objct2[0]) {
		    obj[0] = objtmp[0];
		    obj[1] = objtmp[1];
		    objget_(obj, objlis, object);
		    maknam_(object, &sobj, obnam, kertyp, name__, kertyp_len, 
			    (ftnlen)64);
		    sygetd_(name__, winsym, winptr, winval, &n2, &filwin[6], &
			    found, (ftnlen)64, winsym_len);
		    i__1 = n2;
		    for (i__ = 1; i__ <= i__1; i__ += 2) {
			++stotal;
			obj1[(i__2 = stotal - 1) < 1000000 && 0 <= i__2 ? 
				i__2 : s_rnge("obj1", i__2, "disply_", (
				ftnlen)472)] = obj[0];
			obj2[(i__2 = stotal - 1) < 1000000 && 0 <= i__2 ? 
				i__2 : s_rnge("obj2", i__2, "disply_", (
				ftnlen)473)] = obj[1];
			sstart[(i__2 = stotal - 1) < 1000000 && 0 <= i__2 ? 
				i__2 : s_rnge("sstart", i__2, "disply_", (
				ftnlen)474)] = filwin[(i__4 = i__ + 5) < 
				100006 && 0 <= i__4 ? i__4 : s_rnge("filwin", 
				i__4, "disply_", (ftnlen)474)];
			sstop[(i__2 = stotal - 1) < 1000000 && 0 <= i__2 ? 
				i__2 : s_rnge("sstop", i__2, "disply_", (
				ftnlen)475)] = filwin[(i__4 = i__ + 6) < 
				100006 && 0 <= i__4 ? i__4 : s_rnge("filwin", 
				i__4, "disply_", (ftnlen)475)];
		    }
		    objnxt_(obj, objlis, objtmp, &found);
		} else {
		    found = FALSE_;
		}
	    }

/*           Re-order buffered information by start time. */

	    orderd_(sstart, &stotal, iorder);
	    reordi_(iorder, &stotal, obj1);
	    reordi_(iorder, &stotal, obj2);
	    reordd_(iorder, &stotal, sstart);
	    reordd_(iorder, &stotal, sstop);

/*           Loop through the buffer and print its contents. */

	    j = 1;
	    objtmp[0] = obj1[(i__1 = j - 1) < 1000000 && 0 <= i__1 ? i__1 : 
		    s_rnge("obj1", i__1, "disply_", (ftnlen)500)];
	    objtmp[1] = obj2[(i__1 = j - 1) < 1000000 && 0 <= i__1 ? i__1 : 
		    s_rnge("obj2", i__1, "disply_", (ftnlen)501)];
	    objget_(objtmp, objlis, object);
	    prname_(object, &sobj, p1, wd, p2, &size, kertyp, line, (ftnlen)8,
		     (ftnlen)8, (ftnlen)8, kertyp_len, (ftnlen)132);
	    filwin[6] = sstart[(i__1 = j - 1) < 1000000 && 0 <= i__1 ? i__1 : 
		    s_rnge("sstart", i__1, "disply_", (ftnlen)504)];
	    filwin[7] = sstop[(i__1 = j - 1) < 1000000 && 0 <= i__1 ? i__1 : 
		    s_rnge("sstop", i__1, "disply_", (ftnlen)505)];
	    n2 = 2;
	    j = 2;
	    while(j <= stotal) {
		objtmp[0] = obj1[(i__1 = j - 1) < 1000000 && 0 <= i__1 ? i__1 
			: s_rnge("obj1", i__1, "disply_", (ftnlen)511)];
		objtmp[1] = obj2[(i__1 = j - 1) < 1000000 && 0 <= i__1 ? i__1 
			: s_rnge("obj2", i__1, "disply_", (ftnlen)512)];
		objget_(objtmp, objlis, object);
		prname_(object, &sobj, p1, wd, p2, &size, kertyp, sname, (
			ftnlen)8, (ftnlen)8, (ftnlen)8, kertyp_len, (ftnlen)
			64);
		if (s_cmp(line, sname, (ftnlen)132, (ftnlen)64) != 0) {
		    if (n2 == n1) {
			same = TRUE_;
			i__ = 1;
			while(same && i__ <= n1) {
			    same = filwin[(i__1 = i__ + 5) < 100006 && 0 <= 
				    i__1 ? i__1 : s_rnge("filwin", i__1, 
				    "disply_", (ftnlen)524)] == lstwin[(i__2 =
				     i__ + 5) < 100006 && 0 <= i__2 ? i__2 : 
				    s_rnge("lstwin", i__2, "disply_", (ftnlen)
				    524)];
			    ++i__;
			}
		    } else {
			same = FALSE_;
		    }
		    if (! same) {
			i__1 = n2;
			for (i__ = 1; i__ <= i__1; i__ += 2) {
			    distim_(timtyp, &filwin[(i__2 = i__ + 5) < 100006 
				    && 0 <= i__2 ? i__2 : s_rnge("filwin", 
				    i__2, "disply_", (ftnlen)535)], timlbl, 
				    line + (s - 1), timtyp_len, (ftnlen)8, 
				    132 - (s - 1));
			    distim_(timtyp, &filwin[(i__2 = i__ + 6) < 100006 
				    && 0 <= i__2 ? i__2 : s_rnge("filwin", 
				    i__2, "disply_", (ftnlen)537)], timlbl, 
				    line + (e - 1), timtyp_len, (ftnlen)8, 
				    132 - (e - 1));
			    writit_(line, (ftnlen)132);
			    s_copy(line, " ", (ftnlen)132, (ftnlen)1);
			    lstwin[(i__2 = i__ + 5) < 100006 && 0 <= i__2 ? 
				    i__2 : s_rnge("lstwin", i__2, "disply_", (
				    ftnlen)541)] = filwin[(i__4 = i__ + 5) < 
				    100006 && 0 <= i__4 ? i__4 : s_rnge("fil"
				    "win", i__4, "disply_", (ftnlen)541)];
			    lstwin[(i__2 = i__ + 6) < 100006 && 0 <= i__2 ? 
				    i__2 : s_rnge("lstwin", i__2, "disply_", (
				    ftnlen)542)] = filwin[(i__4 = i__ + 6) < 
				    100006 && 0 <= i__4 ? i__4 : s_rnge("fil"
				    "win", i__4, "disply_", (ftnlen)542)];
			    n1 = n2;
			}
		    } else {
			i__1 = s + 11;
			s_copy(line + i__1, "Same coverage as previous objec"
				"t ", 132 - i__1, (ftnlen)33);
			writit_(line, (ftnlen)132);
		    }
		    s_copy(line, sname, (ftnlen)132, (ftnlen)64);
		    filwin[6] = sstart[(i__1 = j - 1) < 1000000 && 0 <= i__1 ?
			     i__1 : s_rnge("sstart", i__1, "disply_", (ftnlen)
			    554)];
		    filwin[7] = sstop[(i__1 = j - 1) < 1000000 && 0 <= i__1 ? 
			    i__1 : s_rnge("sstop", i__1, "disply_", (ftnlen)
			    555)];
		    n2 = 2;
		} else {
		    filwin[(i__1 = n2 + 6) < 100006 && 0 <= i__1 ? i__1 : 
			    s_rnge("filwin", i__1, "disply_", (ftnlen)560)] = 
			    sstart[(i__2 = j - 1) < 1000000 && 0 <= i__2 ? 
			    i__2 : s_rnge("sstart", i__2, "disply_", (ftnlen)
			    560)];
		    filwin[(i__1 = n2 + 7) < 100006 && 0 <= i__1 ? i__1 : 
			    s_rnge("filwin", i__1, "disply_", (ftnlen)561)] = 
			    sstop[(i__2 = j - 1) < 1000000 && 0 <= i__2 ? 
			    i__2 : s_rnge("sstop", i__2, "disply_", (ftnlen)
			    561)];
		    n2 += 2;
		}
		++j;
	    }

/*           Print information for the last object from the buffered */
/*           set. */

	    if (n2 == n1) {
		same = TRUE_;
		i__ = 1;
		while(same && i__ <= n1) {
		    same = filwin[(i__1 = i__ + 5) < 100006 && 0 <= i__1 ? 
			    i__1 : s_rnge("filwin", i__1, "disply_", (ftnlen)
			    579)] == lstwin[(i__2 = i__ + 5) < 100006 && 0 <= 
			    i__2 ? i__2 : s_rnge("lstwin", i__2, "disply_", (
			    ftnlen)579)];
		    ++i__;
		}
	    } else {
		same = FALSE_;
	    }
	    if (! same) {
		i__1 = n2;
		for (i__ = 1; i__ <= i__1; i__ += 2) {
		    distim_(timtyp, &filwin[(i__2 = i__ + 5) < 100006 && 0 <= 
			    i__2 ? i__2 : s_rnge("filwin", i__2, "disply_", (
			    ftnlen)590)], timlbl, line + (s - 1), timtyp_len, 
			    (ftnlen)8, 132 - (s - 1));
		    distim_(timtyp, &filwin[(i__2 = i__ + 6) < 100006 && 0 <= 
			    i__2 ? i__2 : s_rnge("filwin", i__2, "disply_", (
			    ftnlen)591)], timlbl, line + (e - 1), timtyp_len, 
			    (ftnlen)8, 132 - (e - 1));
		    writit_(line, (ftnlen)132);
		    s_copy(line, " ", (ftnlen)132, (ftnlen)1);
		    lstwin[(i__2 = i__ + 5) < 100006 && 0 <= i__2 ? i__2 : 
			    s_rnge("lstwin", i__2, "disply_", (ftnlen)594)] = 
			    filwin[(i__4 = i__ + 5) < 100006 && 0 <= i__4 ? 
			    i__4 : s_rnge("filwin", i__4, "disply_", (ftnlen)
			    594)];
		    lstwin[(i__2 = i__ + 6) < 100006 && 0 <= i__2 ? i__2 : 
			    s_rnge("lstwin", i__2, "disply_", (ftnlen)595)] = 
			    filwin[(i__4 = i__ + 6) < 100006 && 0 <= i__4 ? 
			    i__4 : s_rnge("filwin", i__4, "disply_", (ftnlen)
			    595)];
		    n1 = n2;
		}
	    } else {
		i__1 = s + 11;
		s_copy(line + i__1, "Same coverage as previous object ", 132 
			- i__1, (ftnlen)33);
		writit_(line, (ftnlen)132);
	    }

/*           Move onto the next object. */

	    objnxt_(obj, objlis, objtmp, &found);
	    obj[0] = touchi_(objtmp);
	    obj[1] = touchi_(&objtmp[1]);
	}

/*     Were we asked to do tabular output grouped by similar coverages? */

    } else if (*tdsp && *gdsp) {
	s = widest + 3;
	e = s + 32;
	if (s_cmp(kertyp, "SPK", kertyp_len, (ftnlen)3) == 0) {
	    s_copy(line, "Bodies", (ftnlen)132, (ftnlen)6);
	} else if (s_cmp(kertyp, "PCK", kertyp_len, (ftnlen)3) == 0) {
	    s_copy(line, "Frames", (ftnlen)132, (ftnlen)6);
	} else {
	    s_copy(line, "IDs", (ftnlen)132, (ftnlen)3);
	}
/* Writing concatenation */
	i__3[0] = 19, a__1[0] = "Start of Interval (";
	i__3[1] = rtrim_(timlbl, (ftnlen)8), a__1[1] = timlbl;
	i__3[2] = 1, a__1[2] = ")";
	s_cat(line + (s - 1), a__1, i__3, &c__3, 132 - (s - 1));
/* Writing concatenation */
	i__3[0] = 17, a__1[0] = "End of Interval (";
	i__3[1] = rtrim_(timlbl, (ftnlen)8), a__1[1] = timlbl;
	i__3[2] = 1, a__1[2] = ")";
	s_cat(line + (e - 1), a__1, i__3, &c__3, 132 - (e - 1));
	writit_(line, (ftnlen)132);
	s_copy(line, "-------", (ftnlen)132, (ftnlen)7);
	s_copy(line + (s - 1), "-----------------------------", 132 - (s - 1),
		 (ftnlen)29);
	s_copy(line + (e - 1), "-----------------------------", 132 - (e - 1),
		 (ftnlen)29);
	writit_(line, (ftnlen)132);
	objnth_(objlis, &c__1, obj, &found);
	while(found) {
	    s_copy(line, " ", (ftnlen)132, (ftnlen)1);
	    objget_(obj, objlis, object);
	    prname_(object, &sobj, p1, wd, p2, &size, kertyp, line, (ftnlen)8,
		     (ftnlen)8, (ftnlen)8, kertyp_len, (ftnlen)132);
	    maknam_(object, &sobj, obnam, kertyp, name__, kertyp_len, (ftnlen)
		    64);
	    sygetd_(name__, winsym, winptr, winval, &n1, &filwin[6], &found, (
		    ftnlen)64, winsym_len);
	    i__1 = n1;
	    for (i__ = 1; i__ <= i__1; i__ += 2) {
		distim_(timtyp, &filwin[(i__2 = i__ + 5) < 100006 && 0 <= 
			i__2 ? i__2 : s_rnge("filwin", i__2, "disply_", (
			ftnlen)659)], timlbl, line + (s - 1), timtyp_len, (
			ftnlen)8, 132 - (s - 1));
		distim_(timtyp, &filwin[(i__2 = i__ + 6) < 100006 && 0 <= 
			i__2 ? i__2 : s_rnge("filwin", i__2, "disply_", (
			ftnlen)660)], timlbl, line + (e - 1), timtyp_len, (
			ftnlen)8, 132 - (e - 1));
		writit_(line, (ftnlen)132);
		s_copy(line, " ", (ftnlen)132, (ftnlen)1);
		lstwin[(i__2 = i__ + 5) < 100006 && 0 <= i__2 ? i__2 : s_rnge(
			"lstwin", i__2, "disply_", (ftnlen)663)] = filwin[(
			i__4 = i__ + 5) < 100006 && 0 <= i__4 ? i__4 : s_rnge(
			"filwin", i__4, "disply_", (ftnlen)663)];
		lstwin[(i__2 = i__ + 6) < 100006 && 0 <= i__2 ? i__2 : s_rnge(
			"lstwin", i__2, "disply_", (ftnlen)664)] = filwin[(
			i__4 = i__ + 6) < 100006 && 0 <= i__4 ? i__4 : s_rnge(
			"filwin", i__4, "disply_", (ftnlen)664)];
	    }
	    objnxt_(obj, objlis, objn, &fnd);
	    objrem_(obj, objlis);
	    obj[0] = objn[0];
	    obj[1] = objn[1];
	    while(fnd) {
		s_copy(line, " ", (ftnlen)132, (ftnlen)1);
		objget_(obj, objlis, object);
		prname_(object, &sobj, p1, wd, p2, &size, kertyp, line, (
			ftnlen)8, (ftnlen)8, (ftnlen)8, kertyp_len, (ftnlen)
			132);
		maknam_(object, &sobj, obnam, kertyp, name__, kertyp_len, (
			ftnlen)64);
		sygetd_(name__, winsym, winptr, winval, &n2, &filwin[6], &
			found, (ftnlen)64, winsym_len);
		if (n2 == n1) {
		    same = TRUE_;
		    i__ = 1;
		    while(same && i__ <= n1) {
			same = filwin[(i__1 = i__ + 5) < 100006 && 0 <= i__1 ?
				 i__1 : s_rnge("filwin", i__1, "disply_", (
				ftnlen)688)] == lstwin[(i__2 = i__ + 5) < 
				100006 && 0 <= i__2 ? i__2 : s_rnge("lstwin", 
				i__2, "disply_", (ftnlen)688)];
			++i__;
		    }
		} else {
		    same = FALSE_;
		}
		if (same) {
		    i__1 = s + 11;
		    s_copy(line + i__1, "Same coverage as previous object ", 
			    132 - i__1, (ftnlen)33);
		    writit_(line, (ftnlen)132);
		}
		objnxt_(obj, objlis, objn, &fnd);
		if (same) {
		    objrem_(obj, objlis);
		}
		obj[0] = objn[0];
		obj[1] = objn[1];
	    }
	    objnth_(objlis, &c__1, obj, &found);
	}

/*     We were not asked to do tabular output. So do a regular one. */

    } else {
	objnth_(objlis, &c__1, obj, &found);
	while(found) {
	    ssizec_(&c_b177, names, (ftnlen)64);
	    objget_(obj, objlis, object);
	    prname_(object, &sobj, p1, wd, p2, &size, kertyp, name__, (ftnlen)
		    8, (ftnlen)8, (ftnlen)8, kertyp_len, (ftnlen)64);
	    appndc_(name__, names, (ftnlen)64, (ftnlen)64);

/*           Look up the window associated with this object. */

	    maknam_(object, &sobj, obnam, kertyp, name__, kertyp_len, (ftnlen)
		    64);
	    sygetd_(name__, winsym, winptr, winval, &n1, &lstwin[6], &fnd, (
		    ftnlen)64, winsym_len);

/*           Fetch the next object. */

	    objnxt_(obj, objlis, objn, &fnd);
	    objrem_(obj, objlis);
	    obj[0] = objn[0];
	    obj[1] = objn[1];
	    while(fnd) {
		objget_(obj, objlis, object);
		maknam_(object, &sobj, obnam, kertyp, name__, kertyp_len, (
			ftnlen)64);
		sygetd_(name__, winsym, winptr, winval, &n2, &filwin[6], &fnd,
			 (ftnlen)64, winsym_len);

/*              See if this window is the same as the current */
/*              window under considerations. */

		if (n1 == n2) {
		    same = TRUE_;
		    i__ = 1;
		    while(same && i__ <= n1) {
			same = filwin[(i__1 = i__ + 5) < 100006 && 0 <= i__1 ?
				 i__1 : s_rnge("filwin", i__1, "disply_", (
				ftnlen)766)] == lstwin[(i__2 = i__ + 5) < 
				100006 && 0 <= i__2 ? i__2 : s_rnge("lstwin", 
				i__2, "disply_", (ftnlen)766)];
			++i__;
		    }
		} else {
		    same = FALSE_;
		}
		objnxt_(obj, objlis, objn, &fnd);
		if (same) {
		    objrem_(obj, objlis);
		    prname_(object, &sobj, p1, wd, p2, &size, kertyp, name__, 
			    (ftnlen)8, (ftnlen)8, (ftnlen)8, kertyp_len, (
			    ftnlen)64);
		    appndc_(name__, names, (ftnlen)64, (ftnlen)64);
		}
		obj[0] = objn[0];
		obj[1] = objn[1];
	    }
	    ngroup = cardc_(names, (ftnlen)64);
	    if (ngroup == 1) {
		if (s_cmp(kertyp, "SPK", kertyp_len, (ftnlen)3) == 0) {
		    s_copy(line, "Body: ", (ftnlen)132, (ftnlen)6);
		    start = 7;
		} else if (s_cmp(kertyp, "PCK", kertyp_len, (ftnlen)3) == 0) {
		    s_copy(line, "Frame: ", (ftnlen)132, (ftnlen)7);
		    start = 8;
		} else {
		    s_copy(line, "ID: ", (ftnlen)132, (ftnlen)4);
		    start = 5;
		}
	    } else {
		if (s_cmp(kertyp, "SPK", kertyp_len, (ftnlen)3) == 0) {
		    s_copy(line, "Bodies: ", (ftnlen)132, (ftnlen)8);
		    start = 9;
		} else if (s_cmp(kertyp, "PCK", kertyp_len, (ftnlen)3) == 0) {
		    s_copy(line, "Frames: ", (ftnlen)132, (ftnlen)8);
		    start = 9;
		} else {
		    s_copy(line, "IDs: ", (ftnlen)132, (ftnlen)5);
		    start = 6;
		}
	    }
	    npline = (80 - widest - start) / (widest + 2) + 1;
	    rmaini_(&ngroup, &npline, &nlines, &remain);
	    if (remain != 0) {
		++nlines;
	    }
	    i__1 = nlines;
	    for (j = 1; j <= i__1; ++j) {
		b = start;
		i__2 = ngroup;
		i__4 = nlines;
		for (i__ = j; i__4 < 0 ? i__ >= i__2 : i__ <= i__2; i__ += 
			i__4) {
		    s_copy(line + (b - 1), names + (((i__5 = i__ + 5) < 
			    100006 && 0 <= i__5 ? i__5 : s_rnge("names", i__5,
			     "disply_", (ftnlen)826)) << 6), 132 - (b - 1), (
			    ftnlen)64);
		    b = b + widest + 2;
		}
		writit_(line, (ftnlen)132);
		s_copy(line, " ", (ftnlen)132, (ftnlen)1);
	    }
	    s = start;
	    e = start + 36;
	    s_copy(header, " ", (ftnlen)132, (ftnlen)1);
	    s_copy(header + 132, " ", (ftnlen)132, (ftnlen)1);
/* Writing concatenation */
	    i__3[0] = 19, a__1[0] = "Start of Interval (";
	    i__3[1] = rtrim_(timlbl, (ftnlen)8), a__1[1] = timlbl;
	    i__3[2] = 1, a__1[2] = ")";
	    s_cat(header + (s - 1), a__1, i__3, &c__3, 132 - (s - 1));
/* Writing concatenation */
	    i__3[0] = 17, a__1[0] = "End of Interval (";
	    i__3[1] = rtrim_(timlbl, (ftnlen)8), a__1[1] = timlbl;
	    i__3[2] = 1, a__1[2] = ")";
	    s_cat(header + (e - 1), a__1, i__3, &c__3, 132 - (e - 1));
	    s_copy(header + (s + 131), "-----------------------------", 132 - 
		    (s - 1), (ftnlen)29);
	    s_copy(header + (e + 131), "-----------------------------", 132 - 
		    (e - 1), (ftnlen)29);
	    writit_(header, (ftnlen)132);
	    writit_(header + 132, (ftnlen)132);
	    i__1 = n1;
	    for (i__ = 1; i__ <= i__1; i__ += 2) {
		s_copy(line, " ", (ftnlen)132, (ftnlen)1);
		distim_(timtyp, &lstwin[(i__4 = i__ + 5) < 100006 && 0 <= 
			i__4 ? i__4 : s_rnge("lstwin", i__4, "disply_", (
			ftnlen)854)], timlbl, line + (s - 1), timtyp_len, (
			ftnlen)8, 132 - (s - 1));
		distim_(timtyp, &lstwin[(i__4 = i__ + 6) < 100006 && 0 <= 
			i__4 ? i__4 : s_rnge("lstwin", i__4, "disply_", (
			ftnlen)855)], timlbl, line + (e - 1), timtyp_len, (
			ftnlen)8, 132 - (e - 1));
		writit_(line, (ftnlen)132);
	    }
	    writit_(" ", (ftnlen)1);
	    objnth_(objlis, &c__1, obj, &found);
	}
    }

/*     All done. */

    chkout_("DISPLY", (ftnlen)6);
    return 0;
} /* disply_ */

