/* dr2str.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b4 = 3600.;
static doublereal c_b6 = 60.;
static doublereal c_b8 = 1.;

/* $Procedure      DR2STR ( Converts Duration to DDDAYS:HR:MN:SC.###### ) */
/* Subroutine */ int dr2str_(doublereal *duratn, char *string, ftnlen 
	string_len)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int dpfmt_(doublereal *, char *, char *, ftnlen, 
	    ftnlen), rmaind_(doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    extern doublereal spd_(void);
    doublereal hdp1, hdp2, hdp3;

/* $ Abstract */

/*     Converts input duration in seconds to DDDAYS:HR:MN:SC.###### */
/*     string. */

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
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     DURATN     I   Interval length, seconds */
/*     STRING     O   Output string, DAYS:HR:MN:SC.###### */

/* $ Detailed_Input */

/*     TBD. */

/* $ Detailed_Output */

/*     TBD. */

/* $ Parameters */

/*     None */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     TBD. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    Version 1.0.0, 30-AUG-2008 (BVS) */

/*        Initial version. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Make output string. */

    s_copy(string, "0ddddd:0h:0m:0s.0msecs", string_len, (ftnlen)22);
    d__1 = spd_();
    rmaind_(duratn, &d__1, &hdp1, &hdp2);
    dpfmt_(&hdp1, "dddddd", string, (ftnlen)6, (ftnlen)6);
    rmaind_(&hdp2, &c_b4, &hdp1, &hdp3);
    dpfmt_(&hdp1, "0h", string + 7, (ftnlen)2, (ftnlen)2);
    rmaind_(&hdp3, &c_b6, &hdp1, &hdp2);
    dpfmt_(&hdp1, "0m", string + 10, (ftnlen)2, (ftnlen)2);
    rmaind_(&hdp2, &c_b8, &hdp1, &hdp3);
    dpfmt_(&hdp1, "0s", string + 13, (ftnlen)2, (ftnlen)2);
    hdp1 = hdp3 * 1e6;
    if (hdp1 > 999999.) {
	hdp1 = 999999.;
    }
    dpfmt_(&hdp1, "0msecs", string + 16, (ftnlen)6, (ftnlen)6);

/*     All done. */

    return 0;
} /* dr2str_ */

