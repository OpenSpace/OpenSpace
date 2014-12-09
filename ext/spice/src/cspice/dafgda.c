/* dafgda.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DAFGDA ( DAF, read data from address ) */
/* Subroutine */ int dafgda_(integer *handle, integer *begin, integer *end, 
	doublereal *data)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer begr, begw, endr, endw, last, next;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer recno;
    logical found;
    integer first;
    extern /* Subroutine */ int dafgdr_(integer *, integer *, integer *, 
	    integer *, doublereal *, logical *), cleard_(integer *, 
	    doublereal *), dafarw_(integer *, integer *, integer *), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen),
	     errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Read the double precision data bounded by two addresses within */
/*     a DAF. */

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

/*     DAF */

/* $ Keywords */

/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of a DAF. */
/*     BEGIN, */
/*     END        I   Initial, final address within file. */
/*     DATA       O   Data contained between BEGIN and END. */

/* $ Detailed_Input */

/*     HANDLE      is the handle of a DAF. */

/*     BEGIN, */
/*     END         are the initial and final addresses of a contiguous */
/*                 set of double precision numbers within a DAF. */
/*                 Presumably, these make up all or part of a particular */
/*                 array. */

/* $ Detailed_Output */

/*     DATA        are the double precision data contained between */
/*                 the specified addresses within the specified file. */

/* $ Parameters */

/*      None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If BEGIN is zero or negative, the error SPICE(DAFNEGADDR) */
/*        is signalled. */

/*     2) If the BEGIN > END, the error SPICE(DAFBEGGTEND) */
/*        is signalled. */

/*     3) If HANDLE is invalid, routines in the call tree of DAFGDA */
/*        signal an appropriate error. */

/*     4) If the range of addresses covered between BEGIN and END */
/*        includes records that do not contain strictly double */
/*        precision data, then the values returned in DATA are */
/*        undefined.  See the Restrictions section below for details. */

/* $ Particulars */

/*     The principal reason that DAFs are so easy to use is that */
/*     the data in each DAF are considered to be one long contiguous */
/*     set of double precision numbers. You can grab data from anywhere */
/*     within a DAF without knowing (or caring) about the physical */
/*     records in which they are stored. */

/*     This routine replaces DAFRDA as the principle mechanism for */
/*     reading the contents of DAF arrays. */

/* $ Examples */

/*     The following code fragment illustrates the use of DAFGDA */
/*     to read data from an imaginary array. The array begins with a */
/*     directory containing 11 epochs. Each pair of epochs bounds */
/*     an interval, and each interval is covered by a set of eight */
/*     osculating elements. */

/*        CALL DAFUS ( SUM, ND, NI, DC, IC ) */
/*        BEGIN = IC(5) */
/*        END   = IC(6) */

/*        CALL DAFGDA ( HANDLE, BEGIN, BEGIN+10, EPOCHS ) */

/*        DO I = 1, 10 */
/*           IF ( ET .GE. EPOCHS(I)  .AND.  ET .LE. EPOCHS(I+1) ) THEN */
/*              OFFSET = 11 + (I - 1) * 8 */

/*              CALL DAFGDA ( HANDLE, OFFSET+1, OFFSET+8, ELEMENTS ) */
/*              RETURN */
/*           END IF */
/*        END DO */


/* $ Restrictions */

/*     1) There are several types of records in a DAF.  This routine */
/*        is only to be used to read double precision data bounded */
/*        between two DAF addresses.  The range of addresses input */
/*        may not cross data and summary record boundaries. */

/* $ Literature_References */

/*     NAIF Document 167.0, "Double Precision Array Files (DAF) */
/*     Specification and User's Guide" */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 16-NOV-2001 (FST) */

/* -& */
/* $ Index_Entries */

/*     read data from daf address */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }

/*     Bad addresses? */

    if (*begin <= 0) {
	chkin_("DAFGDA", (ftnlen)6);
	setmsg_("Negative value for BEGIN address: #", (ftnlen)35);
	errint_("#", begin, (ftnlen)1);
	sigerr_("SPICE(DAFNEGADDR)", (ftnlen)17);
	chkout_("DAFGDA", (ftnlen)6);
	return 0;
    } else if (*begin > *end) {
	chkin_("DAFGDA", (ftnlen)6);
	setmsg_("Beginning address (#) greater than ending address (#).", (
		ftnlen)54);
	errint_("#", begin, (ftnlen)1);
	errint_("#", end, (ftnlen)1);
	sigerr_("SPICE(DAFBEGGTEND)", (ftnlen)18);
	chkout_("DAFGDA", (ftnlen)6);
	return 0;
    }

/*     Convert raw addresses to record/word representations. */

    dafarw_(begin, &begr, &begw);
    dafarw_(end, &endr, &endw);

/*     Get as many records as needed. Return the last part of the */
/*     first record, the first part of the last record, and all of */
/*     every record in between. Any record not found is assumed to */
/*     be filled with zeros. */

    next = 1;
    i__1 = endr;
    for (recno = begr; recno <= i__1; ++recno) {
	if (begr == endr) {
	    first = begw;
	    last = endw;
	} else if (recno == begr) {
	    first = begw;
	    last = 128;
	} else if (recno == endr) {
	    first = 1;
	    last = endw;
	} else {
	    first = 1;
	    last = 128;
	}
	dafgdr_(handle, &recno, &first, &last, &data[next - 1], &found);
	if (! found) {
	    i__2 = last - first + 1;
	    cleard_(&i__2, &data[next - 1]);
	}
	next += last - first + 1;
    }
    return 0;
} /* dafgda_ */

