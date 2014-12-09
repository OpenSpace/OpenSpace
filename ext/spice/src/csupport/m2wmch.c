/* m2wmch.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure            M2WMCH ( Match a word against a META/2 class ) */
logical m2wmch_(char *string, integer *wordb, integer *worde, char *class__, 
	ftnlen string_len, ftnlen class_len)
{
    /* System generated locals */
    integer i__1, i__2;
    logical ret_val;

    /* Builtin functions */
    integer i_len(char *, ftnlen), s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern logical zztokns_(char *, char *, ftnlen, ftnlen);
    static char base[32];
    extern /* Subroutine */ int convrt_3__(doublereal *, char *, char *, 
	    doublereal *, integer *, ftnlen, ftnlen);
    static logical temp;
    static integer pntr;
    static doublereal xout;
    extern logical m2day_(char *, ftnlen), m2mon_(char *, ftnlen), m2int_(
	    char *, ftnlen);
    static integer i__, l;
    static doublereal v, x, y;
    static char error[160];
    extern logical eqstr_(char *, char *, ftnlen, ftnlen), m2name_(char *, 
	    ftnlen), m2alph_(char *, ftnlen), m2engl_(char *, ftnlen), 
	    m2epoc_(char *, ftnlen), m2body_(char *, ftnlen), m2time_(char *, 
	    ftnlen), m2year_(char *, ftnlen), m2numb_(char *, ftnlen);
    extern /* Subroutine */ int m2save_(char *, integer *, integer *, ftnlen),
	     m2ntem_(char *, char *, integer *, integer *, doublereal *, 
	    doublereal *, ftnlen, ftnlen), m2tran_(char *, integer *, integer 
	    *, char *, logical *, logical *, ftnlen, ftnlen);
    extern logical m2unit_(char *, ftnlen);
    static integer nb, ne, lbrace, wb, we, rbrace;
    static logical namfnd;
    extern logical matchm_(char *, char *, char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen);
    extern /* Subroutine */ int nparsd_(char *, doublereal *, char *, integer 
	    *, ftnlen, ftnlen);
    static logical tmplog;
    static integer status, beg, end;
    static logical key;
    static doublereal xin;

/* $ Abstract */

/*     Determine whether or not the WORD is a member of a META/2 */
/*     class. */

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

/*     META/2 a language specification language. */

/* $ Keywords */

/*     PARSING */
/*     UTILITY */
/*     WORD */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   A string containing words. */
/*     WORDB      I   The beginning of a word. */
/*     WORDE      I   The ending of the same word. */
/*     CLASS      I   A META/2 specification keyword or META-KEY */

/*     The function is returned as .TRUE. if WORD is a member of CLASS. */

/* $ Detailed_Input */

/*      STRING    is any character string.  It is expected to be composed */
/*                of words. */

/*      WORDB     is the beginning of some word in STRING. */

/*      WORDE     is the ending of the same word of STRING. */

/*                The word of interest is STRING(WORDB:WORDE). */

/*      CLASS     is one of the recognized classes of words in META/2 or */
/*                a META-KEY.  CLASS is expected to be right justified. */
/*                This class may be modified by a restriction template. */
/*                The possible classes are: */

/*                @word            @number */
/*                @alpha           @int */
/*                @name            @body */
/*                @english         @unit */
/*                @epoc */
/*                @day */
/*                @time */
/*                @month */
/*                @year */
/*                @calendar */

/*                Of these, the following can be modified by a */
/*                restriction template. */

/*                @word            @number */
/*                @alpha           @int */
/*                @name            @unit */
/*                @english */
/*                @month */

/*                If CLASS is not one of these words ( possibly qualified */
/*                by a restriction template ) it is assumed to be a */
/*                specification keyword. */

/* $ Detailed_Output */

/*      M2WMCH is returned as .TRUE. if */

/*         1.)  CLASS is a META-KEY and STRINB(WORDB:WORDE) falls into */
/*              the category specified by this META-KEY */

/*         or */

/*         2.) CLASS is determined to be a specification keyword and */
/*             STRING(WORDB:WORDE) is equal to this keyword. */

/*         Otherwise, it is returned as .FALSE. */

/* $ Error_Handling */

/*     None. */

/* $ Input_Files */

/*     None. */

/* $ Output_Files */

/*     None. */

/* $ Particulars */

/*      This is a utility routine for use by META/2.  It determines */
/*      whether or not a word from a candidate sentence matches */
/*      a desired class. */

/* $ Examples */

/*      The following table gives a sample of the results that */
/*      are returned by this function. */

/*      WORD          CLASS               M2WMCH */
/*      ---------     ---------           ------ */
/*      SEPARATION    OBJECT                F */
/*      SEPARATION    @english              T */
/*      SEPARATION    @english(T*)          F */
/*      SEPARATION    @english(T*|S*)       T */
/*      12:15:15      @number               F */
/*      12:15:15      @time                 T */
/*      44:12:18      @time                 F */
/*      PIG           @english              T */
/*      PIG           @int                  T */
/*      12.182        NUMBER                F */
/*      12.182        @number               T */
/*      12.182        @int                  F */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -     META/2 Configured Version 3.1.0, 07-NOV-2005 (BVS) */

/*         Fixed the way ZZTOKNS is called. */

/* -     META/2 Configured Version 3.0.0, 23-MAR-2000 (WLT) */

/*         Extended the routine so that it can now check the keyword */
/*         @unit and @unit(unitspec). */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/*     Version B1.0.0, 31-MAR-1988 (WLT) (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     There are some obvious things we can handle right now. */
/*     Note that if we input a substring that is completely outside */
/*     the range (1, LEN(STRING)), then WB will be greater than WE. */
/*     Otherwize we will have trimmed the substring to lie within */
/*     the bounds of the string. */

    wb = max(*wordb,1);
/* Computing MIN */
    i__1 = *worde, i__2 = i_len(string, string_len);
    we = min(i__1,i__2);
    if (wb > we) {
	ret_val = FALSE_;
	return ret_val;
    }

/*     Get the first and last characters of CLASS */
/*     These are EXPECTED to be the first and last characters of */
/*     CLASS. */

    beg = 1;
    l = i_len(class__, class_len);
    lbrace = '[';
    rbrace = ']';

/*     Next see if there is a name attached to which we will write the */
/*     results of successful matches. */

    namfnd = FALSE_;
    end = l;

/*     If the length is not at least 4 or the last character is not */
/*     a right brace, there is no name associated with this word. */

    if (*(unsigned char *)&class__[l - 1] == rbrace && l >= 4) {

/*        Ok. We have a chance at getting a name.  Look for */
/*        a left brace and if found set the name and class end. */

	i__ = 2;
	while(i__ < l - 1) {
	    if (*(unsigned char *)&class__[i__ - 1] == lbrace) {
		nb = i__ + 1;
		ne = l - 1;
		end = i__ - 1;
		i__ = l;
		namfnd = TRUE_;
	    }
	    ++i__;
	}
    }

/*     See if CLASS represents a specification keyword or a META-KEY. */

    m2tran_(class__, &beg, &end, base, &key, &temp, class_len, (ftnlen)32);

/*     If we have a specification keyword, the input WORD must match */
/*     exactly. */

    if (key) {
	ret_val = eqstr_(class__, string + (wb - 1), end, we - (wb - 1));

/*     See if we are trying to match a numeric string. */

    } else if (s_cmp(base, "@int", (ftnlen)32, (ftnlen)4) == 0 || s_cmp(base, 
	    "@number", (ftnlen)32, (ftnlen)7) == 0) {
	if (s_cmp(base, "@int", (ftnlen)32, (ftnlen)4) == 0) {
	    ret_val = m2int_(string + (wb - 1), we - (wb - 1));
	} else if (s_cmp(base, "@number", (ftnlen)32, (ftnlen)7) == 0) {
	    ret_val = m2numb_(string + (wb - 1), we - (wb - 1));
	}
	if (ret_val && temp) {

/*           Parse the number and see if it is in bounds. */

	    m2ntem_(class__, base, &beg, &end, &x, &y, class_len, (ftnlen)32);
	    nparsd_(string + (wb - 1), &v, error, &pntr, we - (wb - 1), (
		    ftnlen)160);
	    ret_val = v <= y && v >= x;
	}
	if (ret_val && namfnd) {
	    m2save_(class__ + (nb - 1), &wb, &we, ne - (nb - 1));
	}
	return ret_val;
    } else if (s_cmp(base, "@unit", (ftnlen)32, (ftnlen)5) == 0) {
	ret_val = m2unit_(string + (wb - 1), we - (wb - 1));
	if (ret_val && temp) {
	    xin = 1.;
	    i__1 = beg;
	    convrt_3__(&xin, string + (wb - 1), class__ + i__1, &xout, &
		    status, we - (wb - 1), end - 1 - i__1);
	    ret_val = status == 0;
	}
	if (ret_val && namfnd) {
	    m2save_(class__ + (nb - 1), &wb, &we, ne - (nb - 1));
	}
	return ret_val;
    } else if (s_cmp(base, "@name", (ftnlen)32, (ftnlen)5) == 0) {
	ret_val = m2name_(string + (wb - 1), we - (wb - 1));
    } else if (s_cmp(base, "@body", (ftnlen)32, (ftnlen)5) == 0) {
	ret_val = m2body_(string + (wb - 1), we - (wb - 1));
    } else if (s_cmp(base, "@english", (ftnlen)32, (ftnlen)8) == 0) {
	ret_val = m2engl_(string + (wb - 1), we - (wb - 1));
    } else if (s_cmp(base, "@alpha", (ftnlen)32, (ftnlen)6) == 0) {
	ret_val = m2alph_(string + (wb - 1), we - (wb - 1));
    } else if (s_cmp(base, "@time", (ftnlen)32, (ftnlen)5) == 0) {
	ret_val = m2time_(string + (wb - 1), we - (wb - 1));
    } else if (s_cmp(base, "@epoch", (ftnlen)32, (ftnlen)6) == 0) {
	ret_val = m2epoc_(string + (wb - 1), we - (wb - 1));
    } else if (s_cmp(base, "@day", (ftnlen)32, (ftnlen)4) == 0) {
	ret_val = m2day_(string + (wb - 1), we - (wb - 1));
    } else if (s_cmp(base, "@year", (ftnlen)32, (ftnlen)5) == 0) {
	ret_val = m2year_(string + (wb - 1), we - (wb - 1));
    } else if (s_cmp(base, "@month", (ftnlen)32, (ftnlen)6) == 0) {
	ret_val = m2mon_(string + (wb - 1), we - (wb - 1));
    } else if (s_cmp(base, "@calendar", (ftnlen)32, (ftnlen)9) == 0) {
	tmplog = zztokns_(string + (wb - 1), error, we - (wb - 1), (ftnlen)
		160);
	ret_val = s_cmp(error, " ", (ftnlen)160, (ftnlen)1) == 0;
    } else if (s_cmp(base, "@word", (ftnlen)32, (ftnlen)5) == 0) {
	ret_val = TRUE_;
    }
    if (ret_val && temp) {
	i__1 = beg;
	ret_val = matchm_(string + (wb - 1), class__ + i__1, "*", "%", "~", 
		"|", we - (wb - 1), end - 1 - i__1, (ftnlen)1, (ftnlen)1, (
		ftnlen)1, (ftnlen)1);
    }
    if (ret_val && namfnd) {
	m2save_(class__ + (nb - 1), &wb, &we, ne - (nb - 1));
    }
    return ret_val;
} /* m2wmch_ */

