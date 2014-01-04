/* cbinit_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure CBINIT ( Character buffer, initialize  ) */
/* Subroutine */ int cbinit_1__(integer *dim, char *buffer, ftnlen buffer_len)
{
    /* System generated locals */
    integer buffer_dim1, i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), i_len(char *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), enchar_(integer *, 
	    char *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, ftnlen),
	     setmsg_(char *, ftnlen), errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Initialize a character buffer. */

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

/*     CB */

/* $ Keywords */

/*     ASCII */
/*     CHARACTER */
/*     STRING */
/*     TEXT */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     DIM        I   Dimension of the character buffer array. */
/*     BUFFER    I,O  Character buffer. */

/* $ Detailed_Input */

/*     DIM         is the dimension of the array containing the */
/*                 character buffer to be initialized. */

/*     BUFFER      is the array. */

/* $ Detailed_Output */

/*     BUFFER      is an initialized character buffer. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) The error 'SPICE(NOTLEGALCB)' is signalled whenever any of */
/*        the following conditions is detected. */

/*           -- The length of the individual array elements is less */
/*              than eight. */

/*           -- DIM is less than one. */

/* $ Particulars */

/*     A character buffer must be initialized to allow subsequent */
/*     operations on the buffer to detect possible overflows. */

/* $ Examples */

/*     The following code fragment illustrates the initialization */
/*     of a character buffer. */

/*        INTEGER               LBCBUF */
/*        PARAMETER           ( LBCBUF =    0 ) */

/*        INTEGER               BUFDIM */
/*        PARAMETER           ( BUFDIM =  256 ) */

/*        INTEGER               BUFLEN */
/*        PARAMETER           ( BUFLEN = 1024 ) */

/*        CHARACTER*(BUFLEN)    BUFFER   ( LBCBUF:BUFDIM ) */
/*         . */
/*         . */

/*        CALL CBINIT ( BUFDIM, BUFFER ) */

/*     In this example, the buffer contains 256K characters of available */
/*     storage (256 array elements of 1024 characters each). Note that */
/*     it is only necessary to supply the dimension of the array (256), */
/*     and not the length of the individual elements (1024). */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart, (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 19-JAN-1989 (DT) */

/* -& */

/*     SPICELIB functions */


/*     Standard error handling. */

    /* Parameter adjustments */
    buffer_dim1 = *dim + 1;

    /* Function Body */
    if (return_()) {
	return 0;
    } else {
	chkin_("CBINIT_1", (ftnlen)8);
	if (i_len(buffer + ((i__1 = 0) < buffer_dim1 ? i__1 : s_rnge("buffer",
		 i__1, "cbinit_1__", (ftnlen)149)) * buffer_len, buffer_len) <
		 8) {
	    setmsg_("Length is #.", (ftnlen)12);
	    i__2 = i_len(buffer + ((i__1 = 0) < buffer_dim1 ? i__1 : s_rnge(
		    "buffer", i__1, "cbinit_1__", (ftnlen)151)) * buffer_len, 
		    buffer_len);
	    errint_("#", &i__2, (ftnlen)1);
	    sigerr_("SPICE(NOTLEGALCB)", (ftnlen)17);
	    chkout_("CBINIT_1", (ftnlen)8);
	    return 0;
	} else if (*dim < 1) {
	    setmsg_("Dimension is #.", (ftnlen)15);
	    errint_("#", dim, (ftnlen)1);
	    sigerr_("SPICE(NOTLEGALCB)", (ftnlen)17);
	    chkout_("CBINIT_1", (ftnlen)8);
	    return 0;
	}
    }

/*     Store only the dimension. */

    enchar_(dim, buffer + ((i__1 = 0) < buffer_dim1 ? i__1 : s_rnge("buffer", 
	    i__1, "cbinit_1__", (ftnlen)170)) * buffer_len, (ftnlen)8);
    chkout_("CBINIT_1", (ftnlen)8);
    return 0;
} /* cbinit_1__ */

