/* ckcovr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      CKCOVR ( CK coverage as ETs adjusted for round off ) */
/* Subroutine */ int ckcovr_(char *ck, integer *idcode, logical *needav, char 
	*level, doublereal *tol, doublereal *cover, ftnlen ck_len, ftnlen 
	level_len)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2, d__3;

    /* Local variables */
    extern /* Subroutine */ int sce2c_(integer *, doublereal *, doublereal *),
	     sct2e_(integer *, doublereal *, doublereal *);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ckcov_(char *, 
	    integer *, logical *, char *, doublereal *, char *, doublereal *, 
	    ftnlen, ftnlen, ftnlen), ckmeta_(integer *, char *, integer *, 
	    ftnlen);
    doublereal maxdif;
    integer sclkid;
    extern integer wncard_(doublereal *);
    extern /* Subroutine */ int wncond_(doublereal *, doublereal *, 
	    doublereal *), chkout_(char *, ftnlen);
    extern logical return_(void);
    doublereal hdp1, hdp2;

/* $ Abstract */

/*     Find the ET coverage window adjusted for SCLK -> ET -> SCLK */
/*     conversion round off for a specified object in a specified CK */
/*     file. */

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

/*     CELLS */
/*     DAF */
/*     CK */
/*     TIME */
/*     WINDOWS */

/* $ Keywords */

/*     POINTING */
/*     TIME */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     CK         I   Name of CK file. */
/*     IDCODE     I   ID code of object. */
/*     NEEDAV     I   Flag indicating whether angular velocity is needed. */
/*     LEVEL      I   Coverage level:  'SEGMENT' OR 'INTERVAL'. */
/*     TOL        I   Tolerance in ticks. */
/*     COVER      O   Window giving coverage for IDCODE. */

/* $ Detailed_Input */

/*     TBD. */

/* $ Detailed_Output */

/*     TBD. */

/* $ Parameters */

/*     FACTOR         is the factor to multiply the maximum round off */
/*                    value for contracting the output window. */

/* $ Exceptions */

/*     1) See exceptions signaled by CKCOV and SCT2E/SCE2C. */

/* $ Files */

/*     TBD. */

/* $ Particulars */

/*     This routine passed all inputs directly to CKCOV, gets SCLK */
/*     coverage window out of it, does SCLK -> ET -> SCLK conversion for */
/*     each interval endpoint, computes the difference between each */
/*     source and resulting SCLK, saves the maximum difference, */
/*     contracts SCLK window by this difference multiplied by a factor, */
/*     and then convert the resulting SCLK to ET for output. */

/*     It is possible that a value very near an endpoint would have a */
/*     greater roundoff than the roundoff at the endpoint. This routine */
/*     does not attempt to adjust coverage for such cases. */

/* $ Examples */

/*     TBD. */

/* $ Restrictions */

/*     TBD. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 13-MAR-2012 (BVS) */

/*        Increased FACTOR from 2.D0 to 3.D0. */

/* -    SPICELIB Version 1.0.0, 09-JUL-2008 (BVS) */

/* -& */
/* $ Index_Entries */

/*     get coverage window adjusted for roundoff for ck object */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("CKCOVR", (ftnlen)6);

/*     Pass all inputs directly to CKCOV to get SCLK coverage window. */

    ckcov_(ck, idcode, needav, level, tol, "SCLK", cover, ck_len, level_len, (
	    ftnlen)4);

/*     Get spacecraft ID that will be used for time conversions. */

    ckmeta_(idcode, "SCLK", &sclkid, (ftnlen)4);

/*     Convert each SCLK to ET, then back to SCLK and compute */
/*     roundoff. Save maximum round off. */

    maxdif = 0.;
    i__1 = wncard_(cover) << 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sct2e_(&sclkid, &cover[i__ + 5], &hdp1);
	sce2c_(&sclkid, &hdp1, &hdp2);
/* Computing MAX */
	d__2 = maxdif, d__3 = (d__1 = cover[i__ + 5] - hdp2, abs(d__1));
	maxdif = max(d__2,d__3);
    }

/*     If there is a roundoff, contract window on each side by factor * */
/*     roundoff value. */

    if (maxdif != 0.) {
	d__1 = maxdif * 3.;
	d__2 = maxdif * 3.;
	wncond_(&d__1, &d__2, cover);
    }

/*     Convert SCLK window to ET. */

    i__1 = wncard_(cover) << 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sct2e_(&sclkid, &cover[i__ + 5], &hdp1);
	cover[i__ + 5] = hdp1;
    }

/*     All done. */

    chkout_("CKCOVR", (ftnlen)6);
    return 0;
} /* ckcovr_ */

