/* zzholdd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZHOLDD ( Private --- hold a scalar DP ) */
/* Subroutine */ int zzholdd_(char *op, doublereal *value, ftnlen op_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    static doublereal s_value__;

/* $ Abstract */

/*     SPICE private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Persistently store a double precision value or retrieve a */
/*     stored double precision value. */

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

/*     STORE_VALUE */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     OP         I   String name of operation to execute */
/*     VALUE     I-O  Double precision value returned or to store */

/* $ Detailed_Input */

/*     OP          The scalar string name of the operation to execute. */
/*                 Proper values of OP: */

/*                    'PUT'   store a double precision value for later */
/*                            use */

/*                    'GET'   retrieve a stored double precision value */

/*                    'RESET' reset function to require a PUT prior */
/*                            to a subsequent GET. */

/*     VALUE       The scalar double precision value to store; */
/*                 corresponding to a 'PUT' OP. */

/* $ Detailed_Output */

/*     VALUE       The scalar double precision value returned; */
/*                 corresponding to a 'GET' OP. The value is that stored */
/*                 by the previous 'PUT' operation. */

/* $ Parameters */

/*    None. */

/* $ Exceptions */

/*     1)  The error SPICE(ZZHOLDNOPUT) signals if a 'GET' operation */
/*         precedes any 'PUT' operation. */

/*     2)  The error SPICE(UNKNOWNOP) signals if the value of OP is */
/*         neither 'GET', 'PUT', or 'RESET'. */

/* $ Files */

/*    None. */

/* $ Particulars */

/*     This routine simply stores a double precision value for */
/*     later retrieval. The value stored persists in memory until */
/*     overwritten by a subsequent 'PUT' operation. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     Store values using ZZHOLDD then attempt to retrieve the values. */

/*           PROGRAM ZZHOLDD_T */

/*           IMPLICIT NONE */

/*           DOUBLE PRECISION      VAL */

/*     C */
/*     C     Set a default value for VAL. */
/*     C */
/*           VAL = 0.D0 */

/*     C */
/*     C     Store 941.0 in ZZHOLDD. */
/*     C */
/*           CALL ZZHOLDD ( 'PUT', 941.D0 ) */

/*     C */
/*     C     Retrieve 941.0 to VAL. */
/*     C */
/*           CALL ZZHOLDD ( 'GET', VAL ) */

/*     C */
/*     C     Output VAL. It should have value 941.0. */
/*     C */
/*           WRITE (*,*) VAL */


/*     C */
/*     C     Another 'PUT' 'GET' cycle. */
/*     C */
/*           CALL ZZHOLDD ( 'PUT', 830.D0 ) */

/*     C */
/*     C     Output VAL. It should have value 830.0. */
/*     C */
/*           CALL ZZHOLDD ( 'GET', VAL ) */

/*           WRITE (*,*) VAL */


/*           END */

/*   The program outputs (OS X Intel run): */

/*       941. */
/*       830. */

/*    As expected. */

/* $ Restrictions */

/*     Code logic enforces the requirement at least one 'PUT' operation */
/*     occurs before a 'GET'. You can't 'GET' until at least one 'PUT'. */
/*     'RESET' returns the routine to the state requiring a 'PUT'. */

/* $ Literature_References */

/*    None. */

/* $ Author_and_Institution */

/*    E.D. Wright    (JPL) */

/* $ Version */

/* -   SPICELIB Version 1.0.0  16-FEB-2010 (EDW) */

/* -& */
/* $ Index_Entries */

/*    store a double precision value */
/*    retrieve a stored double precision value */

/* -& */
    if (eqstr_(op, "GET", op_len, (ftnlen)3)) {

/*        Retrieve a stored double precision value. Signal */
/*        an error if a "GET" call occurs prior to a "PUT." */

	if (first) {
	    chkin_("ZZHOLDD", (ftnlen)7);
	    setmsg_("ZZHOLDD GET called without PUT initialization. Either t"
		    "he first GET call of program run or first GET call after"
		    " RESET.", (ftnlen)118);
	    sigerr_("SPICE(ZZHOLDNOPUT) ", (ftnlen)19);
	    chkout_("ZZHOLDD", (ftnlen)7);
	    return 0;
	}
	*value = s_value__;
    } else if (eqstr_(op, "PUT", op_len, (ftnlen)3)) {

/*        Store a value for later use. Set FIRST to false */
/*        so subsequent "GET" calls will work. */

	if (first) {
	    first = FALSE_;
	}
	s_value__ = *value;
    } else if (eqstr_(op, "RESET", op_len, (ftnlen)5)) {

/*        Reset FIRST forcing a PUT before an further GET. */

	first = TRUE_;
    } else {

/*        'OP' not "PUT," "RESET" or "GET." Signal an error. */

	chkin_("ZZHOLDD", (ftnlen)7);
	setmsg_("Unknown operation '#'. Routine supports only GET, PUT and R"
		"ESET.", (ftnlen)64);
	errch_("#", op, (ftnlen)1, op_len);
	sigerr_("SPICE(UNKNOWNOP)", (ftnlen)16);
	chkout_("ZZHOLDD", (ftnlen)7);
	return 0;
    }
    return 0;
} /* zzholdd_ */

