/* redbuf.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;

/* $Procedure      REDBUF ( Read input data into the buffer line ) */
/* Subroutine */ int redbuf_(integer *inpunt, char *endlin, integer *nlnrec, 
	integer *epocfl, char *buffer, integer *lstbuf, char *bufaux, logical 
	*eof, ftnlen endlin_len, ftnlen buffer_len, ftnlen bufaux_len)
{
    /* Initialized data */

    static integer nl = 0;

    /* System generated locals */
    address a__1[2];
    integer i__1, i__2[2];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    char line[513];
    integer last;
    logical gtln;
    extern integer posr_(char *, char *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer first, n1, n2;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    extern integer lastnb_(char *, ftnlen);
    extern logical return_(void);
    char wrkchr[513];
    extern /* Subroutine */ int readln_(integer *, char *, logical *, ftnlen),
	     chkout_(char *, ftnlen);
    logical eob;

/* $ Abstract */

/*     This routine is a module of the MKSPK program. It load text */
/*     from the input data file into the line buffer. */

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

/*     MKSPK User's Guide */

/* $ Keywords */

/*     None. */

/* $ Declarations */
/* $ Abstract */

/*     MKSPK Include File. */

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

/*     N.G. Khavenson (IKI RAS, Russia) */
/*     B.V. Semenov   (NAIF, JPL) */

/* $ Version */

/* -    Version 1.3.0, 08-FEB-2012 (BVS). */

/*        Added TLE coverage and ID keywords. Added default TLE pad */
/*        parameter. */

/* -    Version 1.2.0, 16-JAN-2008 (BVS). */

/*        Added ETTMWR parameter */

/* -    Version 1.1.0, 05-JUN-2001 (BVS). */

/*        Added MAXDEG parameter. */

/* -    Version 1.0.4, 21-MAR-2001 (BVS). */

/*        Added parameter for command line flag '-append' indicating */
/*        that appending to an existing output file was requested. */
/*        Added corresponding setup file keyword ('APPEND_TO_OUTPUT'.) */
/*        Added parameters for yes and no values of this keyword. */

/* -    Version 1.0.3, 28-JAN-2000 (BVS). */

/*        Added parameter specifying number of supported input data */
/*        types and parameter specifying number of supported output SPK */
/*        types */

/* -    Version 1.0.2, 22-NOV-1999 (NGK). */

/*        Added parameters for two-line elements processing. */

/* -    Version 1.0.1, 18-MAR-1999 (BVS). */

/*        Added usage, help and template displays. Corrected comments. */

/* -    Version 1.0.0,  8-SEP-1998 (NGK). */

/* -& */

/*     Begin Include Section:  MKSPK generic parameters. */


/*     Maximum number of states allowed per one segment. */


/*     String size allocation parameters */


/*     Length of buffer for input text processing */


/*     Length of a input text line */


/*     Length of file name and comment line */


/*     Length of string for keyword value processing */


/*     Length of string for word processing */


/*     Length of data order parameters string */


/*     Length of string reserved as delimiter */


/*     Numbers of different parameters */



/*     Maximum number of allowed comment lines. */


/*     Reserved number of input parameters */


/*     Full number of delimiters */


/*     Number of delimiters that may appear in time string */


/*     Command line flags */


/*     Setup file keywords reserved values */


/*     Standard YES and NO values for setup file keywords. */


/*     Number of supported input data types and input DATA TYPE */
/*     reserved values. */


/*     Number of supported output SPK data types -- this version */
/*     supports SPK types 5, 8, 9, 10, 12, 13, 15 and 17. */


/*     End of input record marker */


/*     Maximum allowed polynomial degree. The value of this parameter */
/*     is consistent with the ones in SPKW* routines. */


/*     Special time wrapper tag for input times given as ET seconds past */
/*     J2000 */


/*     Default TLE pad, 1/2 day in seconds. */


/*     End Include Section:  MKSPK generic parameters. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     INPUNT     I   Input data file unit */
/*     ENDLIN     I   Data delimiter character */
/*     NLNREC     I   Number of lines per one record of input file */
/*     EPOCFL     I   Flag of epoch string value processing */
/*     BUFFER    I/O  Main line buffer */
/*     LSTBUF    I/O  Length of string stored in main (on input) or */
/*                    auxiliary (on output) buffer */
/*     BUFAUX     O   Auxiliary line buffer */
/*     EOF        O   End of file flag */

/* $ Detailed_Input */

/*     INPUNT      is the Fortran logical unit connected to the input */
/*                 data file. */

/*     ENDLIN      is a character that should be substituted in place */
/*                 on the end-of-lines in the input data file. Normally */
/*                 this is delimiter specified by a user in the setup */
/*                 file. */

/*     NLNREC      is number of lines containing in single record of */
/*                 the input file. */

/*     EPOCFL      is the flag that defined a way of epoch string value */
/*                 processing: */

/*                    EPOCFL = 1, if the length of epoch string */
/*                                is defined. */

/*                    EPOCFL = 2, if data delimiter is not a character */
/*                                allowed in SPICE time strings (comma */
/*                                or white space). */

/*                    EPOCFL = 3, if data delimiter is a character */
/*                                allowed in SPICE time strings. */

/*     BUFFER      is the line buffer into which the text from the */
/*                 input data file is loaded for future parsing. On */
/*                 the input this buffer contains value of the BUFAUX */
/*                 which was returned on the previous iteration. */

/*     LSTBUF      is the length of the string stored in the main */
/*                 buffer on the input. */

/* $ Detailed_Output */

/*     BUFFER      is the line buffer into which the text from the */
/*                 input data file is loaded for future parsing. On the */
/*                 output the BUFFER contains text of one or more input */
/*                 data records. If the last record in the BUFFER was */
/*                 not loaded completely, the remaining part of it is */
/*                 stored in BUFAUX. */

/*     LSTBUF      is the length of the string stored in auxillary */
/*                 buffer on the output. */

/*     BUFAUX      is the auxillary line buffer into which contains the */
/*                 remainder of the text of the last record loaded into */
/*                 BUFFER if that record was not loaded completely. */

/*     EOF         is end-of-file flag. Set to .TRUE. when the */
/*                 end of input data file is reached. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     This routine read data from the input data file connected to the */
/*     INPUNT logical unit. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.G. Khavenson (IKI RAS, Russia) */
/*     B.V. Semenov   (NAIF, JPL) */

/* $ Version */

/* -    Version 1.0.1, 18-MAR-1999 (BVS). */

/*        Corrected EORMRK substitution logic to handle zero length */
/*        empty files at the end of the input data lines. Removed NL */
/*        from argument list and made it SAVEd variable with initial */
/*        value 0. Re-ordered argument list to place input parameters */
/*        before output parameters. Corrected comments. */

/* -    Version 1.0.1, 15-NOV-1998 (NGK). */


/* -    Version 1.0.0, 8-SEP-1998 (NGK). */

/* -& */
/* $ Index_Entries */

/*     Load part input file text into MKSPK buffer line. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Size LINLEN is defined in include file */


/*     Counter of the non-blank data lines in the input. We need to */
/*     keep track of the non-blank lines if go with LINES PER RECORD */
/*     parsing method. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("REDBUF", (ftnlen)6);
    }

/*     Loop until line buffer is not full. Reset end-of-buffer flag */
/*     and counters before entering the loop. */

    eob = FALSE_;
    first = *lstbuf + 1;
    gtln = TRUE_;
    while(! eob) {

/*        Check whether get-next-line flag is set. If it is, read */
/*        a non-blank line from the file. */

	if (gtln) {
	    readln_(inpunt, line, eof, (ftnlen)513);
	    if (*eof) {
		s_copy(line, " ", (ftnlen)513, (ftnlen)1);
	    } else if (s_cmp(line, " ", (ftnlen)513, (ftnlen)1) == 0) {
		s_copy(wrkchr, " ", (ftnlen)513, (ftnlen)1);
		while(s_cmp(wrkchr, " ", (ftnlen)513, (ftnlen)1) == 0 && ! (*
			eof)) {
		    readln_(inpunt, wrkchr, eof, (ftnlen)513);
		    if (*eof) {
			s_copy(line, " ", (ftnlen)513, (ftnlen)1);
		    } else {
			s_copy(line, wrkchr, (ftnlen)513, (ftnlen)513);
		    }
		}
	    }
	    ++nl;
	}

/*        Set get-next-line flag to TRUE. This value will be */
/*        overwritten by FALSE if buffer is full. */

	gtln = TRUE_;

/*        If we actually got a new line (but not encountered */
/*        end-of-file), we add this line to a buffer. */

	if (! (*eof)) {
	    ljust_(line, line, (ftnlen)513, (ftnlen)513);
	    last = lastnb_(line, (ftnlen)513) + 1;

/*           If delimiter is omitted at the end of data line, */
/*           we add it at the and of buffer data before get new line. */

	    i__1 = last - 2;
	    if (s_cmp(line + i__1, endlin, last - 1 - i__1, endlin_len) == 0) 
		    {
		--last;
	    } else {
		s_copy(line + (last - 1), endlin, (ftnlen)1, endlin_len);
	    }
	    if (*epocfl == 3) {

/*              If delimiter is allowed for time string */
/*              we put end of record marker instead of delimiter */
/*              at the end of last line of current record. */
/*              End of record marker is declared in include file. */

		if (nl % *nlnrec == 0) {
		    *(unsigned char *)&line[last - 1] = '$';
		}
	    }

/*           Check if we have space in the current buffer line. */

	    if (first + last - 1 <= 1024) {
/*              We append input line at the end of the */
/*              current buffer line. */

		s_copy(buffer + (first - 1), line, first + last - 1 - (first 
			- 1), last);
		first += last;
		s_copy(bufaux, " ", bufaux_len, (ftnlen)1);
	    } else {

/*              There was not space in the  buffer line. */
/*              Set corresponding flags and proceed. */

		eob = TRUE_;
		gtln = FALSE_;
	    }
	} else {

/*           We reached end-of-file. Set flag to exit the loop and */
/*           proceed. */

	    eob = TRUE_;
	    last = 1;
	}

/*     End of the "filling line buffer" loop. */

    }
    if (*epocfl == 3) {
	n2 = first - 1;
	n1 = posr_(buffer, "$", &n2, buffer_len, (ftnlen)1);
	*lstbuf = last + n2 - n1;
	if (n1 != n2) {
	    i__1 = n1;
/* Writing concatenation */
	    i__2[0] = n2 - i__1, a__1[0] = buffer + i__1;
	    i__2[1] = last, a__1[1] = line;
	    s_cat(bufaux, a__1, i__2, &c__2, (*lstbuf));
	    i__1 = n1;
	    s_copy(buffer + i__1, " ", buffer_len - i__1, (ftnlen)1);
	} else {
	    s_copy(bufaux, line, (*lstbuf), last);
	}
    } else {
	*lstbuf = last;
	s_copy(bufaux, line, (*lstbuf), last);
    }
    chkout_("REDBUF", (ftnlen)6);
    return 0;
} /* redbuf_ */

