/* eqstr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure EQSTR ( Equivalent strings ) */
logical eqstr_(char *a, char *b, ftnlen a_len, ftnlen b_len)
{
    /* System generated locals */
    logical ret_val;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_len(char *, ftnlen);

    /* Local variables */
    integer lena, lenb;
    logical done;
    integer delta, ca, cb, pa, pb, lbound, ubound;

/* $ Abstract */

/*     Determine whether two strings are equivalent. */

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

/*     ALPHANUMERIC */
/*     ASCII */
/*     CHARACTER */
/*     COMPARE */
/*     PARSING */
/*     SEARCH */
/*     STRING */
/*     TEXT */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     A, */
/*     B          I   Arbitrary character strings. */

/*     The function returns TRUE if A and B are equivalent. */

/* $ Detailed_Input */

/*     A, */
/*     B           are arbitrary character strings. */

/* $ Detailed_Output */

/*     The function returns TRUE if A and B are equivalent: that is, */
/*     if A and B contain  the same characters in the same order, */
/*     when blanks are ignored and uppercase and lowercase characters */
/*     are considered equal. */

/* $ Parameters */

/*      None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Particulars */

/*     This routine is provided for those cases in which two strings */
/*     must be compared, and in which allowances are to be made for */
/*     extra (leading, trailing, and embedded) blanks and differences */
/*     in case. For the most part, */

/*        IF ( EQSTR ( A, B ) ) THEN */
/*           . */
/*           . */

/*     is true whenever */

/*        CALL CMPRSS ( ' ', 0, A, TEMPA ) */
/*        CALL UCASE  (            TEMPA, TEMPA ) */

/*        CALL CMPRSS ( ' ', 0, B, TEMPB ) */
/*        CALL UCASE  (            TEMPB, TEMPB ) */

/*        IF ( TEMPA .EQ. TEMPB ) THEN */
/*           . */
/*           . */

/*     is true. There are two important differences, however. */

/*        1) The single reference to EQSTR is much simpler to */
/*           write, and simpler to understand. */

/*        2) The reference to EQSTR does not require any temporary */
/*           storage, nor does it require that the strings A and B */
/*           be changed. This feature is especially useful when */
/*           comparing strings recieved as subprogram arguments */
/*           against strings stored internally within the subprogram. */

/* $ Examples */

/*      Usage */
/*      -------------------------------------------- */

/*         All of the following are TRUE. */

/*            EQSTR ( 'A short string   ', */
/*           .        'ashortstring'        ) */

/*            EQSTR ( 'Embedded        blanks', */
/*           .        'Em be dd ed bl an ks'    ) */

/*            EQSTR ( 'Embedded        blanks', */
/*           .        '   Embeddedblanks'    ) */

/*            EQSTR ( ' ', */
/*           .        '          ' ) */

/*         All of the following are FALSE. */

/*            EQSTR ( 'One word left out', */
/*           .        'WORD LEFT OUT'      ) */

/*            EQSTR ( 'Extra [] delimiters', */
/*           .        'extradelimiters'      ) */

/*            EQSTR ( 'Testing 1, 2, 3', */
/*           .        'TESTING123'       ) */


/*      Use */
/*      -------------------------------------------- */

/*         The following illustrates a typical use for EQSTR. */

/*            SUBROUTINE GREETING ( WHO, WHAT ) */

/*            CHARACTER*(*)         WHO */
/*            CHARACTER*(*)         WHAT */

/*            IF ( EQSTR ( WHO, 'Steve' ) ) THEN */
/*               WHAT = 'Yes, sir?' */

/*            ELSE IF ( EQSTR ( WHO, 'Chuck' ) ) THEN */
/*               WHAT = 'What can I do for you?' */

/*            ELSE */
/*               WHAT = 'Whaddya want?' */
/*            END IF */

/*            RETURN */
/*            END */

/*         Note that all of the following calls will elicit the */
/*         greeting 'Yes, sir?': */

/*            CALL GREETING ( 'STEVE',       WHAT ) */
/*            CALL GREETING ( 'steve',       WHAT ) */
/*            CALL GREETING ( 'Steve',       WHAT ) */
/*            CALL GREETING ( 'sTEVE',       WHAT ) */
/*            CALL GREETING ( ' S T E V E ', WHAT ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 03-AUG-1994 (NJB) */

/*        Code changed to eliminate DO WHILE ( .TRUE. ) construct. */
/*        The purpose of the change was to eliminate compilation */
/*        diagnostics relating to unreachable statements.  The code */
/*        ran just fine before this change. */

/* -    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.1.0, 10-MAY-1990 (NJB) */

/*        Loop termination condition fixed. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     equivalent strings */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0, 07-JUL-1994 (NJB) */

/*        Code changed to eliminate DO WHILE ( .TRUE. ) construct. */
/*        The purpose of the change was to eliminate compilation */
/*        diagnostics relating to unreachable statements. */

/*        Changed some statements of form */

/*           IF <condition> <statement> */

/*        to */

/*           IF <condition> THEN */

/*             <statement> */

/*           END IF */


/* -    SPICELIB Version 1.1.0, 10-MAY-1990 (NJB) */

/*        Loop termination condition fixed.  The routine now checks */
/*        the termination case where both string pointers are pointing */
/*        to blanks, and at least one pointer has a value greater than */
/*        the length of the string it corresponds to.  Internal comments */
/*        were updated accordingly. */

/* -& */

/*     Local variables */


/*     The general plan is to move a pair of pointers (PA, PB) */
/*     through strings A and B, skipping blank characters and */
/*     comparing others one-for-one. */

/*        Repeat: */

/*           If (A is blank) then */
/*              Increment A */

/*           Else if (B is blank) then */
/*              Increment B */

/*           Else */
/*              If (A and B are equivalent) then */
/*                 Increment A and B */
/*              Else */
/*                 Return FALSE */

/*           If (A and B are past end) then */
/*              Return TRUE */

/*           Else if (A or B is past end and other is non-blank) then */
/*              Return FALSE */

/*           Else if (A or B is past end and other is blank) then */
/*              Return TRUE */

/*     Note that no pointer gets incremented more than once on each */
/*     pass through the loop. */

/*     On the other hand, in many cases the strings will be exactly */
/*     equal. If so, why knock ourselves out? */

    if (s_cmp(a, b, a_len, b_len) == 0) {
	ret_val = TRUE_;
	return ret_val;
    } else {
	pa = 1;
	pb = 1;
	lena = i_len(a, a_len);
	lenb = i_len(b, b_len);
	lbound = 'a';
	ubound = 'z';
	delta = 'A' - 'a';
	done = FALSE_;
	while(! done) {

/*           At this point, we're guaranteed that */

/*             ( PA .LE. LENA )   and   ( PB .LE. LENB ) */

	    if (*(unsigned char *)&a[pa - 1] == ' ') {
		++pa;
	    } else if (*(unsigned char *)&b[pb - 1] == ' ') {
		++pb;
	    } else {
		ca = *(unsigned char *)&a[pa - 1];
		cb = *(unsigned char *)&b[pb - 1];
		if (ca >= lbound && ca <= ubound) {
		    ca += delta;
		}
		if (cb >= lbound && cb <= ubound) {
		    cb += delta;
		}
		if (ca == cb) {
		    ++pa;
		    ++pb;
		} else {
		    ret_val = FALSE_;
		    done = TRUE_;

/*                 We'll return from this point, having taken no further */
/*                 action. */

		}
	    }
	    if (! done) {
		if (pa > lena) {

/*                 Whichever of the following tests passes, we're going */
/*                 to have a verdict at the end of the IF block below. */

		    if (pb > lenb) {
			ret_val = TRUE_;
		    } else if (s_cmp(b + (pb - 1), " ", b_len - (pb - 1), (
			    ftnlen)1) != 0) {
			ret_val = FALSE_;
		    } else {
			ret_val = TRUE_;
		    }
		    done = TRUE_;

/*                 We'll return from this point, having taken no further */
/*                 action. */

		} else if (pb > lenb) {

/*                 Whichever of the following tests passes, we're going */
/*                 to have a verdict at the end of the IF block below. */

		    if (s_cmp(a + (pa - 1), " ", a_len - (pa - 1), (ftnlen)1) 
			    != 0) {
			ret_val = FALSE_;
		    } else {
			ret_val = TRUE_;
		    }
		    done = TRUE_;

/*                 We'll return from this point, having taken no further */
/*                 action. */

		}
	    }
	}
    }
    return ret_val;
} /* eqstr_ */

