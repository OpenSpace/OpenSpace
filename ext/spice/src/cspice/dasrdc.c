/* dasrdc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure      DASRDC ( DAS, read data, character ) */
/* Subroutine */ int dasrdc_(integer *handle, integer *first, integer *last, 
	integer *bpos, integer *epos, char *data, ftnlen data_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_len(char *, ftnlen);

    /* Local variables */
    integer l, n, nread;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer recno, nmove, rcpos;
    extern /* Subroutine */ int dasa2l_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    extern logical failed_(void);
    integer clbase;
    extern /* Subroutine */ int dasrrc_(integer *, integer *, integer *, 
	    integer *, char *, ftnlen);
    integer nmoved, clsize;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer numchr;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen);
    integer wordno, chr, elt;

/* $ Abstract */

/*     Read character data from a range of DAS logical addresses. */

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

/*     DAS */

/* $ Keywords */

/*     ARRAY */
/*     ASSIGNMENT */
/*     DAS */
/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   DAS file handle. */
/*     FIRST, */
/*     LAST       I   Range of DAS character logical addresses. */
/*     BPOS, */
/*     EPOS       I   Begin and end positions of substrings. */
/*     DATA       O   Data having addresses FIRST through LAST. */

/* $ Detailed_Input */

/*     HANDLE         is a file handle for an open DAS file. */

/*     FIRST, */
/*     LAST           are a range of DAS character logical addresses. */
/*                    FIRST and LAST must be greater than or equal to */
/*                    1 and less than or equal to the highest character */
/*                    logical address in the DAS file designated by */
/*                    HANDLE. */

/*     BPOS, */
/*     EPOS           are begin and end character positions that define */
/*                    the substrings of the elements of the output array */
/*                    DATA into which character data is to be read. */

/* $ Detailed_Output */

/*     DATA           is an array of character strings.  On output, the */
/*                    character words in the logical address range */
/*                    FIRST through LAST are copied into the characters */

/*                       DATA(1)(BPOS:BPOS), */
/*                       DATA(1)(BPOS+1:BPOS+1), */
/*                                   . */
/*                                   . */
/*                                   . */
/*                       DATA(1)(EPOS:EPOS), */
/*                       DATA(2)(BPOS:BPOS), */
/*                       DATA(2)(BPOS+1:BPOS+1), */
/*                                   . */
/*                                   . */
/*                                   . */

/*                     in that order. */

/*                    DATA must have dimension at least */

/*                       ( LAST - FIRST + L ) / L */

/*                    where */

/*                       L  =  EPOS - BPOS + 1 */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input file handle is invalid, the error will be */
/*         diagnosed by routines called by this routine.  DATA will */
/*         not be modified. */

/*     2)  If EPOS or BPOS are outside of the range */
/*         [  1,  LEN( DATA(1) )  ],  or if EPOS < BPOS, the error */
/*         SPICE(BADSUBSTRINGBOUNDS) will be signalled. */

/*     3)  If FIRST or LAST are out of range, the error will be diagnosed */
/*         by routines called by this routine.  DATA will not be */
/*         modified. */

/*     4)  If FIRST is greater than LAST, DATA is left unchanged. */

/*     5)  If DATA is declared with length less than */

/*            ( LAST - FIRST + ( EPOS-BPOS+1 )  ) / ( EPOS-BPOS+1 ) */

/*         the error cannot be diagnosed by this routine. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     This routine provides random read access to the character data in */
/*     a DAS file.  This data is logically structured as a */
/*     one-dimensional array of characters. */

/*     However, since Fortran programs usually use strings rather */
/*     than arrays of individual characters, the interface of this */
/*     routine provides for extraction of data from a DAS file into */
/*     an array of strings. */

/*     DASRDC allows the caller to control the amount of character data */
/*     read into each array element.  This feature allows a program to */
/*     read character data into an array that has a different string */
/*     length from the one used to write the character data, without */
/*     losing the correspondence between input and output array elements. */
/*     For example, an array of strings of 32 characters can be written */
/*     to a DAS file and read back by DASRDC into a buffer of strings */
/*     having length 80 characters, mapping each 32-character string to */
/*     characters 1--32 of the output buffer. */


/* $ Examples */

/*     1)  Create the new DAS file TEST.DAS and add 240 characters to it. */
/*         Close the file, then re-open it and read the data back out. */


/*                  PROGRAM TEST_ADD */

/*                  CHARACTER*(40)        LINES  ( 3 ) */
/*                  CHARACTER*(80)        BUFFER ( 3 ) */
/*                  CHARACTER*(4)         TYPE */

/*                  INTEGER               FIRST */
/*                  INTEGER               HANDLE */
/*                  INTEGER               I */
/*                  INTEGER               LAST */

/*                  DATA LINES  / 'Here is the first line.', */
/*                 .              'Here is the second line.', */
/*                 .              'Here is the third line.'    / */

/*            C */
/*            C     Open a new DAS file.  Use the file name as */
/*            C     the internal file name. */
/*            C */
/*                  TYPE = 'TEST' */
/*                  CALL DASONW ( 'TEST.DAS', TYPE, 'TEST.DAS', HANDLE ) */

/*            C */
/*            C     Add the contents of the array LINES to the file. */
/*            C */
/*                  CALL DASADC ( HANDLE, 120, 1, 40, LINES ) */

/*            C */
/*            C     Close the file. */
/*            C */
/*                  CALL DASCLS ( HANDLE ) */

/*            C */
/*            C     Now verify the addition of data by opening the */
/*            C     file for read access and retrieving the data.  This */
/*            C     time, use a buffer of 80-character strings to read */
/*            C     the data.  Use only the first 40 characters of each */
/*            C     buffer element. */
/*            C */
/*                  DO I = 1, 3 */
/*                     BUFFER(I) = ' ' */
/*                  END DO */

/*                  CALL DASRDC ( HANDLE, 1, 120, 1, 40, BUFFER ) */

/*            C */
/*            C     Dump the data to the screen.  We should see the */
/*            C     sequence */
/*            C */
/*            C        Here is the first line. */
/*            C        Here is the second line. */
/*            C        Here is the third line. */
/*            C */
/*                  WRITE (*,*) ' ' */
/*                  WRITE (*,*) 'Data from TEST.DAS: ' */
/*                  WRITE (*,*) ' ' */
/*                  WRITE (*,*) BUFFER */

/*                  END */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer (JPL) */
/*     N.J. Bachman   (JPL) */
/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.2 03-JUL-1996 (NJB) */

/*        Various errors in the header comments were fixed. */

/* -    SPICELIB Version 1.2.1 19-DEC-1995 (NJB) */

/*        Corrected title of permuted index entry section. */

/* -    SPICELIB Version 1.2.0, 03-NOV-1995 (NJB) */

/*        Routine now uses discovery check-in.  FAILED test moved inside */
/*        loops. */

/* -    SPICELIB Version 1.2.0, 14-SEP-1995 (NJB) */

/*        Bug fix:  reference to DASADS in CHKOUT calls corrected. */

/* -    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB) */

/*        Test of FAILED() added to loop termination conditions. */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/*        Modified the $ Examples section to demonstrate the new ID word */
/*        format which includes a file type and to include a call to the */
/*        new routine DASONW, open new for write, which makes use of the */
/*        file type. Also,  a variable for the type of the file to be */
/*        created was added. */

/* -    SPICELIB Version 1.0.0, 12-NOV-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     read character data from a DAS file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0, 03-NOV-1995 (NJB) */

/*        Routine now uses discovery check-in.  FAILED test moved inside */
/*        loops. */

/* -    SPICELIB Version 1.2.0, 14-SEP-1995 (NJB) */

/*        Bug fix:  reference to DASADS in CHKOUT calls corrected. */
/*        These references have been changed to 'DASRDC'. */


/* -    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB) */

/*        Test of FAILED() added to loop termination conditions.  Without */
/*        this test, an infinite loop could result if DASA2L or DASRRC */
/*        signaled an error inside the loops. */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/*        Modified the $ Examples section to demonstrate the new ID word */
/*        format which includes a file type and to include a call to the */
/*        new routine DASONW, open new for write, which makes use of the */
/*        file type. Also,  a variable for the type of the file to be */
/*        created was added. */

/* -    SPICELIB Version 1.0.0, 12-NOV-1992 (NJB) (WLT) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Make sure BPOS and EPOS are ok; stop here if not. */

    if (*bpos < 1 || *epos < 1 || *bpos > i_len(data, data_len) || *epos > 
	    i_len(data, data_len)) {
	chkin_("DASRDC", (ftnlen)6);
	setmsg_("Substring bounds must be in range [1,#]. Actual range [BPOS"
		",EPOS] was [#,#].", (ftnlen)76);
	i__1 = i_len(data, data_len);
	errint_("#", &i__1, (ftnlen)1);
	errint_("#", bpos, (ftnlen)1);
	errint_("#", epos, (ftnlen)1);
	sigerr_("SPICE(BADSUBSTRINGBOUNDS)", (ftnlen)25);
	chkout_("DASRDC", (ftnlen)6);
	return 0;
    } else if (*epos < *bpos) {
	chkin_("DASRDC", (ftnlen)6);
	setmsg_("Substring upper bound must not be less than lower bound.  A"
		"ctual range [BPOS,EPOS] was [#,#].", (ftnlen)93);
	errint_("#", bpos, (ftnlen)1);
	errint_("#", epos, (ftnlen)1);
	sigerr_("SPICE(BADSUBSTRINGBOUNDS)", (ftnlen)25);
	chkout_("DASRDC", (ftnlen)6);
	return 0;
    }

/*     Find out the physical location of the first character to read.  If */
/*     FIRST is out of range, DASA2L will cause an error to be signalled. */

    dasa2l_(handle, &c__1, first, &clbase, &clsize, &recno, &wordno);

/*     Get the length of the elements of DATA.  Count the total number */
/*     of characters to read. */

    l = *epos - *bpos + 1;
    n = *last - *first + 1;
    nread = 0;

/*     Read as much data from record RECNO as is necessary and possible. */

/* Computing MIN */
    i__1 = n, i__2 = 1024 - wordno + 1;
    numchr = min(i__1,i__2);
    elt = 1;
    chr = *bpos;
    nmoved = 0;
    rcpos = wordno;
    while(nmoved < numchr) {
	if (failed_()) {
	    return 0;
	}
	if (chr > *epos) {
	    ++elt;
	    chr = *bpos;
	}

/*        Find out how many characters to move from the current record */
/*        to the current array element. */

/* Computing MIN */
	i__1 = numchr - nmoved, i__2 = *epos - chr + 1;
	nmove = min(i__1,i__2);
	i__1 = rcpos + nmove - 1;
	dasrrc_(handle, &recno, &rcpos, &i__1, data + ((elt - 1) * data_len + 
		(chr - 1)), chr + nmove - 1 - (chr - 1));
	nmoved += nmove;
	rcpos += nmove;
	chr += nmove;
    }
    nread = numchr;
    ++recno;

/*     Read from as many additional records as necessary. */

    while(nread < n) {
	if (failed_()) {
	    return 0;
	}

/*        At this point, RECNO is the correct number of the */
/*        record to read from next.  CLBASE is the number */
/*        of the first record of the cluster we're about */
/*        to read from. */


	if (recno < clbase + clsize) {

/*           We can continue reading from the current cluster.  Find */
/*           out how many elements to read from the current record, */
/*           and read them. */

/* Computing MIN */
	    i__1 = n - nread;
	    numchr = min(i__1,1024);
	    nmoved = 0;
	    rcpos = 1;
	    while(nmoved < numchr && ! failed_()) {
		if (chr > *epos) {
		    ++elt;
		    chr = *bpos;
		}

/*              Find out how many characters to move from the current */
/*              record to the current array element. */

/* Computing MIN */
		i__1 = numchr - nmoved, i__2 = *epos - chr + 1;
		nmove = min(i__1,i__2);
		i__1 = rcpos + nmove - 1;
		dasrrc_(handle, &recno, &rcpos, &i__1, data + ((elt - 1) * 
			data_len + (chr - 1)), chr + nmove - 1 - (chr - 1));
		nmoved += nmove;
		rcpos += nmove;
		chr += nmove;
	    }
	    nread += numchr;
	    ++recno;
	} else {

/*           We must find the next character cluster to */
/*           read from.  The first character in this */
/*           cluster has address FIRST + NREAD. */

	    i__1 = *first + nread;
	    dasa2l_(handle, &c__1, &i__1, &clbase, &clsize, &recno, &wordno);
	}
    }
    return 0;
} /* dasrdc_ */

