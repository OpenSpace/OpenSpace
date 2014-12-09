/* chunk.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

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

/* Subroutine */ int chunk_(char *buffer, integer *first, integer *last, 
	ftnlen buffer_len)
{
    /* Initialized data */

    static char terms[32*24] = "|endliteral                     " "!endliter"
	    "al                     " "@chapter                        " "@se"
	    "ction                        " "@setvarsize                     " 
	    "@var                            " "@setparamsize               "
	    "    " "@param                          " "@literal              "
	    "          " "@literalitem                    " "@literalparam   "
	    "                " "@literalvar                     " "@exliteral"
	    "                      " "@exliteralitem                  " "@exl"
	    "iteralparam                 " "@exliteralvar                   " 
	    "@newlist                        " "@newpage                    "
	    "    " "@numitem                        " "@paritem              "
	    "          " "@symitem                        " "@moreparam      "
	    "                " "@morevar                        " "          "
	    "                      ";

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    char cseq[32];
    extern integer cpos_(char *, char *, integer *, ftnlen, ftnlen);
    integer term, i__, j;
    extern integer cardc_(char *, ftnlen);
    integer begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer index;
    extern integer ncpos_(char *, char *, integer *, ftnlen, ftnlen);
    integer nterm;
    extern integer ltrim_(char *, ftnlen);
    integer endbuf;
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen), 
	    touchi_(integer *);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    integer end;


/*     Find the next `chunk' of a FORTeX source buffer. The chunk begins */
/*     sometime after BUFFER(FIRST), and ends at BUFFER(LAST). */


/* $ Revisions */

/* -    Faketex version 1.3.0 5-DEC-1995  WLT */

/*        Set I = TOUCHI( I ) in the IF ( RETURN() ) block so that buggy */
/*        compilers won't complain that it isn't used. */

/* -    Faketex version 1.2.0 17-NOV-1995 NJB */

/*        Data statement for TERMS broken up into multiple statements */
/*        to avoid violation of continuation limit on Sun. */

/* -    Faketex version 1.1.0 16-MAY-1994 NJB */

/*        Substring bounds on line 106 safeguarded to stay in range. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling */

    if (return_()) {
	i__ = 0;
	i__ = touchi_(&i__);
	return 0;
    } else {
	chkin_("CHUNK", (ftnlen)5);
    }

/*     Because we can safely assume that the first line of the chunk */
/*     is not inside a literal section, we can skip blank lines and */
/*     @newpage directives with impunity to find the beginning of the */
/*     chunk. */

    endbuf = cardc_(buffer, buffer_len);
    j = ltrim_(buffer + (*first + 5) * buffer_len, buffer_len);
    while(*first < endbuf && (s_cmp(buffer + (*first + 5) * buffer_len, " ", 
	    buffer_len, (ftnlen)1) == 0 || s_cmp(buffer + ((*first + 5) * 
	    buffer_len + (j - 1)), "@newpage", buffer_len - (j - 1), (ftnlen)
	    8) == 0)) {
	++(*first);
    }
    *last = *first;

/*     A literal chunk may be terminated only by an explicit end marker */
/*     (|endliteral or !endliteral) or the end of the buffer. A normal */
/*     chunk is terminated by the beginning of another chunk, a */
/*     blank line, or a @newpage. */

/* Computing MAX */
    i__1 = 1, i__2 = ncpos_(buffer + (*first + 5) * buffer_len, "  ", &c__1, 
	    buffer_len, (ftnlen)2);
    begin = max(i__1,i__2);
/* Computing MAX */
    i__1 = begin, i__2 = cpos_(buffer + (*first + 5) * buffer_len, " {", &
	    begin, buffer_len, (ftnlen)2) - 1;
    end = max(i__1,i__2);
    s_copy(cseq, buffer + ((*first + 5) * buffer_len + (begin - 1)), (ftnlen)
	    32, end - (begin - 1));
    if (s_cmp(cseq, "@literal", (ftnlen)8, (ftnlen)8) == 0) {
	term = 1;
	nterm = 1;
    } else if (s_cmp(cseq, "@exliteral", (ftnlen)10, (ftnlen)10) == 0) {
	term = 2;
	nterm = 1;
    } else {
	term = 3;
	nterm = 22;
    }

/*     Check subsequent lines until the proper terminator or the end */
/*     of the buffer is reached. */

    index = 0;
    while(index == 0 && *last < endbuf) {
	++(*last);
	if (s_cmp(buffer + (*last + 5) * buffer_len, " ", buffer_len, (ftnlen)
		1) == 0) {
	    s_copy(cseq, " ", (ftnlen)32, (ftnlen)1);
	} else {
	    begin = ncpos_(buffer + (*last + 5) * buffer_len, "  ", &c__1, 
		    buffer_len, (ftnlen)2);
/* Computing MAX */
	    i__1 = begin, i__2 = cpos_(buffer + (*last + 5) * buffer_len, 
		    " {", &begin, buffer_len, (ftnlen)2) - 1;
	    end = max(i__1,i__2);
	    s_copy(cseq, buffer + ((*last + 5) * buffer_len + (begin - 1)), (
		    ftnlen)32, end - (begin - 1));
	}
	index = isrchc_(cseq, &nterm, terms + (((i__1 = term - 1) < 24 && 0 <=
		 i__1 ? i__1 : s_rnge("terms", i__1, "chunk_", (ftnlen)193)) 
		<< 5), (ftnlen)32, (ftnlen)32);
    }

/*     Only a literal section retains the line that terminates it. */

    if (term > 2 && *last != endbuf) {
	--(*last);
    }
    chkout_("CHUNK", (ftnlen)5);
    return 0;
} /* chunk_ */

