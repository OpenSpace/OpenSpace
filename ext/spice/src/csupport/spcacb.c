/* spcacb.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $ Procedure  SPCACB  ( SPK and CK add comments from a buffer  ) */
/* Subroutine */ int spcacb_(integer *dafhdl, char *buffer, ftnlen buffer_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer free;
    char line[255];
    integer last;
    extern integer cpos_(char *, char *, integer *, ftnlen, ftnlen);
    integer i__, j;
    extern integer cardc_(char *, ftnlen);
    integer space;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer recno, first, nd, ni;
    char ifname[255];
    extern /* Subroutine */ int dafarr_(integer *, integer *), dafrcr_(
	    integer *, integer *, char *, ftnlen);
    char crecrd[1000];
    extern /* Subroutine */ int dafrfr_(integer *, integer *, integer *, char 
	    *, integer *, integer *, integer *, ftnlen), dafwcr_(integer *, 
	    integer *, char *, ftnlen);
    integer ncrecs, nchars;
    char eocmrk[1];
    extern integer lastnb_(char *, ftnlen);
    integer length, eocpos;
    char eolmrk[1];
    integer nnrecs, nrrecs, nlines;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    integer curpos;
    extern logical return_(void);

/* $ Abstract */

/*     Store text from a line buffer in the comment area of a binary SPK */
/*     or CK file, appending it to whatever text may already have */
/*     been stored there. */

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

/*     SPC */

/* $ Keywords */

/*     FILES */
/*     UTILITY */

/* $ Declarations */


/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*      LBCELL    P    Lower bound for the CELL 'data type' */
/*      MXCREC    P    Maximum length of a character record in a DAF */
/*      LINLEN    P    The maximum length of an input line */
/*      DAFHDL    I    DAF file handle for output */
/*      BUFFER    I    Buffer of comment lines to be written */

/* $ Detailed_Input */

/*     DAFHDL   The NAIF DAF file handle for accessing a DAF file. */

/*     BUFFER   A list of comment lines which are to be added to the */
/*              comment area of the binary DAF file attached to the */
/*              DAF file handle DAFHDL. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     MXCREC   This is the maximum length of a character record in a */
/*              DAF file. */

/*     LBCELL   This is the lower bound for the CELL data type which */
/*              is supported by SPICELIB. */

/*     LINLEN   This is the maximum length of a single text record in */
/*              a text file. */

/* $ Exceptions */

/*     1)   If the length of the cell buffer is not positive, the error */
/*          SPICE(NONPOSBUFLENGTH) will be signalled. */

/*     2)   If the end of of comment marker is not found, then the error */
/*          SPICE(MISSINGEOT) will be signalled. ( NOTE: the end comment */
/*          marker is also referred to as the end of transmission */
/*          character. ) */

/*     3)   If the comment area of the file exists, i.e., the number of */
/*          comment records is greater than zero, and the last comment */
/*          record is not the last reserved record, then the error */
/*          SPICE(BADCOMMENTAREA) will be signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine will take a character CELL buffer of text lines and */
/*     append them to the comment area of a binary SPK or CK DAF file. */
/*     The lines of text in the buffer will be 'packed' into a DAF */
/*     character record, and when the character record is full it will be */
/*     written to the comment area of the file. This is repeated until */
/*     all of the lines in the buffer have been processed. */

/*     If there are no comments in the comment area, then space will */
/*     be allocated in the file and the text lines in BUFFER will be */
/*     written into the file. Blank text lines are allowed. If there */
/*     are already comments in the comment area, then the text lines */
/*     in BUFFER will be appended to these comments, with a single */
/*     blank line separating the two comment blocks. */

/* $ Examples */

/*     Let */
/*           DAFHDL = The DAF handle for an SPK or CK file */

/*           BUFFER = A list of text lines to be added to the comment */
/*                    area of the SPK or CK file. */

/*     The call */

/*           CALL SPCACB( DAFHDL, BUFFER ) */

/*     will append the text line(s) in BUFFER to the comment area of */
/*     the SPK or CK file. */

/* $ Restrictions */

/*     The conventions for the comment area specified by the SPC family */
/*     of routines is used. Any SPK or CK files which do not conform */
/*     to these conventions may not have 'readable' comment areas. Only */
/*     comments are to be placed into the comment area, where a comment */
/*     consists of only ASCII printable characters. */

/*     NOTE: The SPC family of routines should be the only routines used */
/*           to write to and read from the comment area of SPK or CK */
/*           files. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer (JPL) */
/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    Beta Version 1.1.0, 18-MAY-2004 (BVS) */

/*        Removed check requiring the number of comment records to be */
/*        one less than the number of reserved records. Fixed logic */
/*        adding the end-of-comment marker to handle cases when it */
/*        "rolls" over to the next reserved record. */

/* -    Beta Version 1.0.1, 30-MAR-1999 (BVS) */

/*        Changed LINLEN to 255 (was 80). */

/* -    Beta Version 1.0.0, 23-APR-1992 (KRG) */

/* -& */
/* $ Index_Entries */

/*      WRITE A LINE BUFFER TO AN SPK OR CK COMMENT AREA */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     This is needed for the call to DAFRFR to get some of the */
/*     information needed. It is not used anywhere else. */


/*     These are needed to call DAFRFR to get some of the information */
/*     needed. Only FIRST will be used, and this is to determine the */
/*     number of reserved records which exist. */


/*     Initial values */

    *(unsigned char *)eocmrk = '\4';
    *(unsigned char *)eolmrk = '\0';

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPCACB", (ftnlen)6);
    }

/*     Give some of the variables a value so that they have one. */

    ncrecs = 0;
    nnrecs = 0;
    nrrecs = 0;

/*     First, extract the number of lines in the buffer */

    nlines = cardc_(buffer, buffer_len);

/*     Check for a nonpositive number of lines. */

    if (nlines <= 0) {
	setmsg_("An invalid buffer length was found: #", (ftnlen)37);
	errint_("#", &nlines, (ftnlen)1);
	sigerr_("SPICE(NONPOSBUFLENGTH)", (ftnlen)22);
	chkout_("SPCACB", (ftnlen)6);
	return 0;
    }

/*     Count the number of characters in the buffer, ignoring leading */
/*     and trailing blanks on nonblank lines. Blank lines will not count */
/*     here, their contribution to the size of the comment area will be */
/*     incorporated later. This is for determining the number of */
/*     character records to add to the file attached to handle DAFHDL. */

    nchars = 0;
    i__ = 0;
    while(i__ < nlines) {
	++i__;
	s_copy(line, buffer + (i__ + 5) * buffer_len, (ftnlen)255, buffer_len)
		;
	length = lastnb_(line, (ftnlen)255);
	nchars += length;
    }

/*     Add NLINES + 1 to NCHARS to allow for the end of line markers */
/*     ( EOLMRK ) and the end of comments marker ( EOCMRK ). */

    nchars = nchars + nlines + 1;

/*     Get the number of reserved records from the file. */

    dafrfr_(dafhdl, &nd, &ni, ifname, &first, &last, &free, (ftnlen)255);

/*     Subtract 1 from FIRST to obtain the number of reserved records. */

/*     Note that this should be one more than the number of comment */
/*     records in the comment area for the SPK or CK file comment area */
/*     to conform to the SPC comment area conventions. That is, the */
/*     number of reserved records = the number of comment records + 1. */

    nrrecs = first - 1;

/*     If the number of reserved records, NRRECS, is greater then 1, */
/*     determine the number of comment records in the comment area. */
/*     The comments begin on record CASTRT and should continue to record */
/*     NRRECS - 1. The comments are terminated by and end of comment */
/*     marker EOCMRK = CHAR(4). */

    eocpos = 0;
    i__ = 0;
    while(i__ < nrrecs - 1 && eocpos == 0) {
	recno = i__ + 2;
	dafrcr_(dafhdl, &recno, crecrd, (ftnlen)1000);
	eocpos = cpos_(crecrd, eocmrk, &c__1, (ftnlen)1000, (ftnlen)1);
	++i__;
    }
    if (eocpos == 0 && nrrecs > 1) {
	setmsg_("End-of-transmission character missing in comment area of bi"
		"nary file.", (ftnlen)69);
	sigerr_("SPICE(MISSINGEOT)", (ftnlen)17);
	chkout_("SPCACB", (ftnlen)6);
	return 0;
    }
    ncrecs = i__;

/*     Check to see if the number of comment records is one less than */
/*     the number of reserved records. If not, signal an error. */

/*      IF ( NCRECS .NE. NRRECS - 1 ) THEN */
/*         CALL SETMSG ( 'The number of comment records and the'// */
/*     .                 ' number of reserved records do not agree.'// */
/*     .                 ' The comment area could be bad.' ) */
/*         CALL SIGERR ( 'SPICE(BADCOMMENTAREA)' ) */
/*         CALL CHKOUT ( 'SPCACB' ) */
/*         RETURN */
/*      END IF */

/*     Determine the amount of free space in the comment area. This */
/*     will be the space remaining on the last comment record, i.e., */
/*     the maximum length of a DAF character record - the position */
/*     of the end of comments marker - 1. */

    if (ncrecs > 0) {
	space = 1000 - eocpos;
    } else {
	space = 0;
    }

/*     Determine the number of extra reserved records which are */
/*     necessary to store the comments in the buffer. */

    if (nchars > space) {
	nnrecs = (nchars - space) / 1000 + 1;
    } else {
	nnrecs = 0;
    }

/*     Now call the DAF routine to add reserved records to the file, */
/*     if we need to. */

    if (nnrecs > 0) {
	dafarr_(dafhdl, &nnrecs);
    }

/*     At this point, we know that we have enough space to write the */
/*     comments in the buffer to the comment area. Either there was */
/*     enough space already there, or we figured out how many new */
/*     character records were needed, and we added them to the file. */
/*     So, now we begin 'packing' the comments into the character record. */

/*     We begin by reading the last comment record if there is one, */
/*     otherwise we just initialize the appropriate variables. */

    if (ncrecs == 0) {
	recno = 2;
	curpos = 0;
	s_copy(crecrd, " ", (ftnlen)1000, (ftnlen)1);
    } else {
	recno = ncrecs + 1;
	dafrcr_(dafhdl, &recno, crecrd, (ftnlen)1000);

/*        Find the end of comment marker again. This is really not */
/*        necessary, but it is here to localize all the info needed. */

	eocpos = cpos_(crecrd, eocmrk, &c__1, (ftnlen)1000, (ftnlen)1);

/*        Set the current record position */

	curpos = eocpos;

/*        Put an end of line marker here to separate the new comments */
/*        from the old ones, and increment the current record position. */

	*(unsigned char *)&crecrd[curpos - 1] = *(unsigned char *)eolmrk;
    }
    i__ = 0;
    while(i__ < nlines) {
	++i__;
	s_copy(line, buffer + (i__ + 5) * buffer_len, (ftnlen)255, buffer_len)
		;
	length = lastnb_(line, (ftnlen)255);
	j = 0;
	while(j < length) {
	    if (curpos < 1000) {
		++j;
		++curpos;
		*(unsigned char *)&crecrd[curpos - 1] = *(unsigned char *)&
			line[j - 1];
	    } else {
		dafwcr_(dafhdl, &recno, crecrd, (ftnlen)1000);
		++recno;
		curpos = 0;
		s_copy(crecrd, " ", (ftnlen)1000, (ftnlen)1);
	    }
	}

/*        Check to see if we happened to get exactly MXCREC characters */
/*        when we stopped moving characters from LINE. If we did, then */
/*        we need to write out the current record and appropriately */
/*        adjust the necessary variables. */

	if (curpos == 1000) {
	    dafwcr_(dafhdl, &recno, crecrd, (ftnlen)1000);
	    ++recno;
	    curpos = 0;
	    s_copy(crecrd, " ", (ftnlen)1000, (ftnlen)1);
	}
	++curpos;
	*(unsigned char *)&crecrd[curpos - 1] = *(unsigned char *)eolmrk;
    }

/*     We have now finished processing all of the lines, so we */
/*     need to append the end of comment marker to the current */
/*     record and write it to the file. */

    if (curpos == 1000) {
	dafwcr_(dafhdl, &recno, crecrd, (ftnlen)1000);
	++recno;
	curpos = 0;
	s_copy(crecrd, " ", (ftnlen)1000, (ftnlen)1);
    }
    ++curpos;
    *(unsigned char *)&crecrd[curpos - 1] = *(unsigned char *)eocmrk;
    dafwcr_(dafhdl, &recno, crecrd, (ftnlen)1000);
    chkout_("SPCACB", (ftnlen)6);
    return 0;
} /* spcacb_ */

