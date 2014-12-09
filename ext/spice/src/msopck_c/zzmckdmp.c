/* zzmckdmp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;
static integer c__1 = 1;

/* $Procedure      ZZMCKDMP ( Dump CK to test MSOPCK ) */
/* Subroutine */ int zzmckdmp_(char *ckfile, ftnlen ckfile_len)
{
    /* System generated locals */
    address a__1[2];
    integer i__1[2], i__2, i__3;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    static integer addr__, i__, j;
    extern /* Subroutine */ int dafgs_(doublereal *), ckgr01_(integer *, 
	    doublereal *, integer *, doublereal *), ckgr02_(integer *, 
	    doublereal *, integer *, doublereal *), ckgr03_(integer *, 
	    doublereal *, integer *, doublereal *), chkin_(char *, ftnlen), 
	    cknr01_(integer *, doublereal *, integer *), cklpf_(char *, 
	    integer *, ftnlen), cknr02_(integer *, doublereal *, integer *), 
	    cknr03_(integer *, doublereal *, integer *), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *);
    static integer nidir;
    extern /* Subroutine */ int dpfmt_(doublereal *, char *, char *, ftnlen, 
	    ftnlen);
    static logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), dafgda_(integer *, integer *, integer *, 
	    doublereal *);
    static doublereal dc[2];
    static integer ic[6];
    extern /* Subroutine */ int daffna_(logical *), dafbfs_(integer *);
    static integer handle;
    static doublereal intbeg, record[10];
    static char number[80];
    static integer numrec, segmnt;
    extern /* Subroutine */ int chkout_(char *, ftnlen), suffix_(char *, 
	    integer *, char *, ftnlen, ftnlen);
    static char outlin[80];
    static integer numint;
    extern /* Subroutine */ int tostdo_(char *, ftnlen);
    static doublereal sum[5];

/* $ Abstract */

/*     Private module. Dumps contents of a CK file to the screen to */
/*     support testing of MSOPCK. */

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

/*     CK.REQ */
/*     MSOPCK.UG */

/* $ Keywords */

/*     CK */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O              DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     CKFILE     I   Name of the CK file to be dumped */

/* $ Detailed_Input */

/*     TBD */

/* $ Detailed_Output */

/*     TBD */

/* $ Parameters */

/*     TBD */

/* $ Particulars */

/*     TBD */

/* $ Examples */

/*     TBD */

/* $ Restrictions */

/*     TBD */

/* $ Exceptions */

/*     TBD */

/* $ Files */

/*     TBD */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    Version 1.0.0, 01-SEP-2006 (BVS) */

/* -& */

/*     Local Parameters */


/*     Local Variables */


/*     Save everything to prevent potential memory problems in f2c'ed */
/*     version. */


/*     Check in. */

    chkin_("ZZMCKDMP", (ftnlen)8);

/*     Load CK file. */

    cklpf_(ckfile, &handle, ckfile_len);

/*     Start scanning through the segments from the front of the file. */

    dafbfs_(&handle);
    daffna_(&found);
    segmnt = 1;
    while(found) {

/*        Get and unpack the segment summary. */

	dafgs_(sum);
	dafus_(sum, &c__2, &c__6, dc, ic);

/*        Dump the segment summary. */

	tostdo_(" ", (ftnlen)1);
	s_copy(outlin, "SEGMENT #", (ftnlen)80, (ftnlen)9);
	repmi_(outlin, "#", &segmnt, outlin, (ftnlen)80, (ftnlen)1, (ftnlen)
		80);
	tostdo_(outlin, (ftnlen)80);
	s_copy(outlin, "   CKID #", (ftnlen)80, (ftnlen)9);
	repmi_(outlin, "#", ic, outlin, (ftnlen)80, (ftnlen)1, (ftnlen)80);
	tostdo_(outlin, (ftnlen)80);
	s_copy(outlin, "  REFID #", (ftnlen)80, (ftnlen)9);
	repmi_(outlin, "#", &ic[1], outlin, (ftnlen)80, (ftnlen)1, (ftnlen)80)
		;
	tostdo_(outlin, (ftnlen)80);
	s_copy(outlin, "   TYPE #", (ftnlen)80, (ftnlen)9);
	repmi_(outlin, "#", &ic[2], outlin, (ftnlen)80, (ftnlen)1, (ftnlen)80)
		;
	tostdo_(outlin, (ftnlen)80);
	s_copy(outlin, " AVFLAG #", (ftnlen)80, (ftnlen)9);
	repmi_(outlin, "#", &ic[3], outlin, (ftnlen)80, (ftnlen)1, (ftnlen)80)
		;
	tostdo_(outlin, (ftnlen)80);
	dpfmt_(dc, "xxxxxxxxxxxxxx.xxx", number, (ftnlen)18, (ftnlen)80);
/* Writing concatenation */
	i__1[0] = 8, a__1[0] = "  BTIME ";
	i__1[1] = 80, a__1[1] = number;
	s_cat(outlin, a__1, i__1, &c__2, (ftnlen)80);
	tostdo_(outlin, (ftnlen)80);
	dpfmt_(&dc[1], "xxxxxxxxxxxxxx.xxx", number, (ftnlen)18, (ftnlen)80);
/* Writing concatenation */
	i__1[0] = 8, a__1[0] = "  ETIME ";
	i__1[1] = 80, a__1[1] = number;
	s_cat(outlin, a__1, i__1, &c__2, (ftnlen)80);
	tostdo_(outlin, (ftnlen)80);

/*        Branch depending on segment type. */

	if (ic[2] == 1) {

/*           Obtain the number of pointing records. */

	    cknr01_(&handle, sum, &numrec);

/*           Dump data records one by one. */

	    i__2 = numrec;
	    for (j = 1; j <= i__2; ++j) {
		ckgr01_(&handle, sum, &j, record);
		s_copy(outlin, "T(#)", (ftnlen)80, (ftnlen)4);
		repmi_(outlin, "#", &j, outlin, (ftnlen)80, (ftnlen)1, (
			ftnlen)80);
		dpfmt_(record, "xxxxxxxxxxxxxx.xxx", number, (ftnlen)18, (
			ftnlen)80);
		suffix_(number, &c__1, outlin, (ftnlen)80, (ftnlen)80);
		tostdo_(outlin, (ftnlen)80);
		s_copy(outlin, "Q(#)", (ftnlen)80, (ftnlen)4);
		repmi_(outlin, "#", &j, outlin, (ftnlen)80, (ftnlen)1, (
			ftnlen)80);
		for (i__ = 1; i__ <= 4; ++i__) {
		    dpfmt_(&record[(i__3 = i__) < 10 && 0 <= i__3 ? i__3 : 
			    s_rnge("record", i__3, "zzmckdmp_", (ftnlen)233)],
			     "+0.xxxxxxxx", number, (ftnlen)11, (ftnlen)80);
		    if (s_cmp(number, "+0.00000000", (ftnlen)11, (ftnlen)11) 
			    == 0 || s_cmp(number, "-0.00000000", (ftnlen)11, (
			    ftnlen)11) == 0) {
			s_copy(number, " 0.00000000", (ftnlen)11, (ftnlen)11);
		    }
		    suffix_(number, &c__1, outlin, (ftnlen)80, (ftnlen)80);
		}
		tostdo_(outlin, (ftnlen)80);
		if (ic[3] != 0) {
		    s_copy(outlin, "R(#)", (ftnlen)80, (ftnlen)4);
		    repmi_(outlin, "#", &j, outlin, (ftnlen)80, (ftnlen)1, (
			    ftnlen)80);
		    for (i__ = 1; i__ <= 3; ++i__) {
			dpfmt_(&record[(i__3 = i__ + 4) < 10 && 0 <= i__3 ? 
				i__3 : s_rnge("record", i__3, "zzmckdmp_", (
				ftnlen)246)], "+0.xxxxxxxx", number, (ftnlen)
				11, (ftnlen)80);
			if (s_cmp(number, "+0.00000000", (ftnlen)11, (ftnlen)
				11) == 0 || s_cmp(number, "-0.00000000", (
				ftnlen)11, (ftnlen)11) == 0) {
			    s_copy(number, " 0.00000000", (ftnlen)11, (ftnlen)
				    11);
			}
			suffix_(number, &c__1, outlin, (ftnlen)80, (ftnlen)80)
				;
		    }
		    tostdo_(outlin, (ftnlen)80);
		}
	    }
	} else if (ic[2] == 2) {

/*           Obtain the number of pointing records. */

	    cknr02_(&handle, sum, &numrec);

/*           Dump data records one by one. */

	    i__2 = numrec;
	    for (j = 1; j <= i__2; ++j) {
		ckgr02_(&handle, sum, &j, record);
		s_copy(outlin, "B(#)", (ftnlen)80, (ftnlen)4);
		repmi_(outlin, "#", &j, outlin, (ftnlen)80, (ftnlen)1, (
			ftnlen)80);
		dpfmt_(record, "xxxxxxxxxxxxxx.xxx", number, (ftnlen)18, (
			ftnlen)80);
		suffix_(number, &c__1, outlin, (ftnlen)80, (ftnlen)80);
		tostdo_(outlin, (ftnlen)80);
		s_copy(outlin, "E(#)", (ftnlen)80, (ftnlen)4);
		repmi_(outlin, "#", &j, outlin, (ftnlen)80, (ftnlen)1, (
			ftnlen)80);
		dpfmt_(&record[1], "xxxxxxxxxxxxxx.xxx", number, (ftnlen)18, (
			ftnlen)80);
		suffix_(number, &c__1, outlin, (ftnlen)80, (ftnlen)80);
		tostdo_(outlin, (ftnlen)80);
		s_copy(outlin, "S(#)", (ftnlen)80, (ftnlen)4);
		repmi_(outlin, "#", &j, outlin, (ftnlen)80, (ftnlen)1, (
			ftnlen)80);
		dpfmt_(&record[2], "xxxxxxxxxxxxxx.xxx", number, (ftnlen)18, (
			ftnlen)80);
		suffix_(number, &c__1, outlin, (ftnlen)80, (ftnlen)80);
		tostdo_(outlin, (ftnlen)80);
		s_copy(outlin, "Q(#)", (ftnlen)80, (ftnlen)4);
		repmi_(outlin, "#", &j, outlin, (ftnlen)80, (ftnlen)1, (
			ftnlen)80);
		for (i__ = 1; i__ <= 4; ++i__) {
		    dpfmt_(&record[(i__3 = i__ + 2) < 10 && 0 <= i__3 ? i__3 :
			     s_rnge("record", i__3, "zzmckdmp_", (ftnlen)293)]
			    , "+0.xxxxxxxx", number, (ftnlen)11, (ftnlen)80);
		    if (s_cmp(number, "+0.00000000", (ftnlen)11, (ftnlen)11) 
			    == 0 || s_cmp(number, "-0.00000000", (ftnlen)11, (
			    ftnlen)11) == 0) {
			s_copy(number, " 0.00000000", (ftnlen)11, (ftnlen)11);
		    }
		    suffix_(number, &c__1, outlin, (ftnlen)80, (ftnlen)80);
		}
		tostdo_(outlin, (ftnlen)80);
		s_copy(outlin, "R(#)", (ftnlen)80, (ftnlen)4);
		repmi_(outlin, "#", &j, outlin, (ftnlen)80, (ftnlen)1, (
			ftnlen)80);
		for (i__ = 1; i__ <= 3; ++i__) {
		    dpfmt_(&record[(i__3 = i__ + 6) < 10 && 0 <= i__3 ? i__3 :
			     s_rnge("record", i__3, "zzmckdmp_", (ftnlen)305)]
			    , "+0.xxxxxxxx", number, (ftnlen)11, (ftnlen)80);
		    if (s_cmp(number, "+0.00000000", (ftnlen)11, (ftnlen)11) 
			    == 0 || s_cmp(number, "-0.00000000", (ftnlen)11, (
			    ftnlen)11) == 0) {
			s_copy(number, " 0.00000000", (ftnlen)11, (ftnlen)11);
		    }
		    suffix_(number, &c__1, outlin, (ftnlen)80, (ftnlen)80);
		}
		tostdo_(outlin, (ftnlen)80);
	    }
	} else if (ic[2] == 3) {

/*           Obtain the number of pointing records. */

	    cknr03_(&handle, sum, &numrec);

/*           Dump data records one by one. */

	    i__2 = numrec;
	    for (j = 1; j <= i__2; ++j) {
		ckgr03_(&handle, sum, &j, record);
		s_copy(outlin, "T(#)", (ftnlen)80, (ftnlen)4);
		repmi_(outlin, "#", &j, outlin, (ftnlen)80, (ftnlen)1, (
			ftnlen)80);
		dpfmt_(record, "xxxxxxxxxxxxxx.xxx", number, (ftnlen)18, (
			ftnlen)80);
		suffix_(number, &c__1, outlin, (ftnlen)80, (ftnlen)80);
		tostdo_(outlin, (ftnlen)80);
		s_copy(outlin, "Q(#)", (ftnlen)80, (ftnlen)4);
		repmi_(outlin, "#", &j, outlin, (ftnlen)80, (ftnlen)1, (
			ftnlen)80);
		for (i__ = 1; i__ <= 4; ++i__) {
		    dpfmt_(&record[(i__3 = i__) < 10 && 0 <= i__3 ? i__3 : 
			    s_rnge("record", i__3, "zzmckdmp_", (ftnlen)339)],
			     "+0.xxxxxxxx", number, (ftnlen)11, (ftnlen)80);
		    if (s_cmp(number, "+0.00000000", (ftnlen)11, (ftnlen)11) 
			    == 0 || s_cmp(number, "-0.00000000", (ftnlen)11, (
			    ftnlen)11) == 0) {
			s_copy(number, " 0.00000000", (ftnlen)11, (ftnlen)11);
		    }
		    suffix_(number, &c__1, outlin, (ftnlen)80, (ftnlen)80);
		}
		tostdo_(outlin, (ftnlen)80);
		if (ic[3] != 0) {
		    s_copy(outlin, "R(#)", (ftnlen)80, (ftnlen)4);
		    repmi_(outlin, "#", &j, outlin, (ftnlen)80, (ftnlen)1, (
			    ftnlen)80);
		    for (i__ = 1; i__ <= 3; ++i__) {
			dpfmt_(&record[(i__3 = i__ + 4) < 10 && 0 <= i__3 ? 
				i__3 : s_rnge("record", i__3, "zzmckdmp_", (
				ftnlen)352)], "+0.xxxxxxxx", number, (ftnlen)
				11, (ftnlen)80);
			if (s_cmp(number, "+0.00000000", (ftnlen)11, (ftnlen)
				11) == 0 || s_cmp(number, "-0.00000000", (
				ftnlen)11, (ftnlen)11) == 0) {
			    s_copy(number, " 0.00000000", (ftnlen)11, (ftnlen)
				    11);
			}
			suffix_(number, &c__1, outlin, (ftnlen)80, (ftnlen)80)
				;
		    }
		    tostdo_(outlin, (ftnlen)80);
		}
	    }

/*           Dump interval start times. */

	    i__2 = ic[5] - 1;
	    i__3 = ic[5] - 1;
	    dafgda_(&handle, &i__2, &i__3, &intbeg);
	    numint = (integer) intbeg;
	    nidir = (numint - 1) / 100;
	    i__2 = numint;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		addr__ = ic[5] - 2 - nidir - numint + i__;
		dafgda_(&handle, &addr__, &addr__, &intbeg);
		s_copy(outlin, "I(#)", (ftnlen)80, (ftnlen)4);
		repmi_(outlin, "#", &i__, outlin, (ftnlen)80, (ftnlen)1, (
			ftnlen)80);
		dpfmt_(&intbeg, "xxxxxxxxxxxxxx.xxx", number, (ftnlen)18, (
			ftnlen)80);
		suffix_(number, &c__1, outlin, (ftnlen)80, (ftnlen)80);
		tostdo_(outlin, (ftnlen)80);
	    }
	} else {
	    s_copy(outlin, "Dump for this segment type is not supported.", (
		    ftnlen)80, (ftnlen)44);
	    tostdo_(outlin, (ftnlen)80);
	}
	tostdo_(" ", (ftnlen)1);

/*        Fetch the next segment. */

	++segmnt;
	daffna_(&found);
    }

/*     Check in. */

    chkout_("ZZMCKDMP", (ftnlen)8);
    return 0;
} /* zzmckdmp_ */

