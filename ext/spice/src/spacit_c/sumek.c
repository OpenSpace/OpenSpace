/* sumek.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;

/* $Procedure      SUMEK ( Summarize the tables in a binary EK file ) */
/* Subroutine */ int sumek_(integer *handle, char *binfnm, logical *logfil, 
	integer *loglun, ftnlen binfnm_len)
{
    /* Initialized data */

    static char strtyp[4*4] = "CHR " "DP  " "INT " "TIME";

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    char line[255];
    integer mark;
    extern /* Subroutine */ int zzeksinf_(integer *, integer *, char *, 
	    integer *, char *, integer *, ftnlen, ftnlen);
    integer i__, j;
    extern /* Subroutine */ int chkin_(char *, ftnlen), repmc_(char *, char *,
	     char *, char *, ftnlen, ftnlen, ftnlen, ftnlen);
    char separ[80];
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    integer ncols, nsegs, nrows;
    extern logical failed_(void);
    char tabnam[64], cnames[32*100];
    integer segdsc[24];
    extern /* Subroutine */ int dassih_(integer *, char *, ftnlen);
    extern integer eknseg_(integer *);
    extern logical return_(void);
    char sumsep[80], fnmout[255];
    integer cdscrs[1100]	/* was [11][100] */, lincnt;
    extern /* Subroutine */ int chkout_(char *, ftnlen), writln_(char *, 
	    integer *, ftnlen), setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen);

/* $ Abstract */

/*     Summarize the tables in a binary E-Kernel file sequentially. */

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

/*     EK Required Reading */

/* $ Keywords */

/*     None. */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*      HANDLE    I   File handle for the E-kernal to be summarized. */
/*      LOGFIL    I   Write the summary to a log file and to screen? */
/*      LOGLUN    I   Logical unit connected to the log file. */

/* $ Detailed_Input */

/*     HANDLE   The file handle of a previously opened binary E-Kernel */
/*              file whose summary is desired. */

/*     LOGFIL     if TRUE means that the summary will be written to */
/*                a log file as well as displayed on the terminal */
/*                screen.  Otherwise, the summary will not be written */
/*                to a file. */

/*     LOGLUN     is the logical unit connected to a log file to which */
/*                the summary is to be written if LOGFIL is TRUE. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)   If an unrecognized data type is encountered in the file, */
/*          the error SPICE(UNKNOWNDATATYPE) will be signalled. */

/*     2)   If the binary file attached to HANDLE is not open for */
/*          reading, an error will be signalled by a routine called by */
/*          this routine. */

/*     3)   If an error occurs while writing the summary to UNIT, an */
/*          error will be signalled by a routine called by this routine. */

/*     4)   If a leapseconds file has not been loaded before this routine */
/*          is called, an error will be signalled by a routine called */
/*          by this routine. */

/* $ Files */

/*     See parameters HANDLE and STDOUT in the $ Detailed_Inputs section. */

/* $ Particulars */

/*     This routine provides a mechanism for summarizing a binary */
/*     E-Kernel file. For each table in the file a brief summary */
/*     of its characteristics will be written to UNIT. */

/*     This routine is for use with the SPACIT utility program. */

/* $ Examples */

/*     Assume that a SPICE leapseconds file has been loaded into the */
/*     SPICELIB kernel pool, and let */

/*           HANDLE   be the file handle of a previously opened binary */
/*                    E-Kernel file. */

/*           STDOUT   be the logical unit to which the summary is to be */
/*                    written. */

/*     Then the subroutine call: */

/*        CALL SUMEK ( HANDLE, STDOUT ) */

/*     will collect summary information for each table in a binary */
/*     E-Kernel file and write it to the logical unit STDOUT. */

/* $ Restrictions */

/*     1) Beware having more columns in an E-Kernel table than you have */
/*        array storage allocated. The E-Kernel sugment summary routine */
/*        called by this routine does not know about the amount of space */
/*        the caller has allocated for storing the columns that it gets. */
/*        Currently, there is room for MAXCOL columns. See the $ Local */
/*        Parameters section below. */

/*     2) This routine assumes that a SPICE leapseconds file has */
/*        already been loaded into the SPICELIB kernel pool. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer (JPL) */

/* $ Version */

/* -    Beta Version 1.1.0, 25-APR-1994 (KRG) */

/*        Added two now calling arguments and modified the dispaly */
/*        algorithm to display a table to the logfile immediately after */
/*        it displays one to the screen. */

/* -    Beta Version 1.1.0, 25-APR-1994 (KRG) */

/*        Removed the unused variable EKNAME from the declarations. */

/* -    Beta Version 1.0.0, 06-JAN-1993 (KRG) */

/* -& */
/* $ Index_Entries */

/*      summarize the tables in a binary ek file */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.1.0, 25-APR-1994 (KRG) */

/*        Removed the unused variable EKNAME from the declarations. */

/* -    Beta Version 1.0.0, 06-JAN-1993 (KRG) */

/* -& */

/*     SPICELIB functions */


/*     Other functions */


/*     Local parameters */

/*     Parameters from the following files are used. See these files for */
/*     descriptions of their parameters. */

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


/*     Include Section:  EK Boolean Enumerated Type */


/*        ekbool.inc Version 1   21-DEC-1994 (NJB) */


/*     Within the EK system, boolean values sometimes must be */
/*     represented by integer or character codes.  The codes and their */
/*     meanings are listed below. */

/*     Integer code indicating `true': */


/*     Integer code indicating `false': */


/*     Character code indicating `true': */


/*     Character code indicating `false': */


/*     End Include Section:  EK Boolean Enumerated Type */

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


/*     Include Section:  EK Column Name Size */

/*        ekcnamsz.inc Version 1    17-JAN-1995 (NJB) */


/*     Size of column name, in characters. */


/*     End Include Section:  EK Column Name Size */

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


/*     Include Section:  EK Column Descriptor Parameters */

/*        ekcoldsc.inc Version 6    23-AUG-1995 (NJB) */


/*     Note:  The column descriptor size parameter CDSCSZ  is */
/*     declared separately in the include section CDSIZE$INC.FOR. */

/*     Offset of column descriptors, relative to start of segment */
/*     integer address range.  This number, when added to the last */
/*     integer address preceding the segment, yields the DAS integer */
/*     base address of the first column descriptor.  Currently, this */
/*     offset is exactly the size of a segment descriptor.  The */
/*     parameter SDSCSZ, which defines the size of a segment descriptor, */
/*     is declared in the include file eksegdsc.inc. */


/*     Size of column descriptor */


/*     Indices of various pieces of column descriptors: */


/*     CLSIDX is the index of the column's class code.  (We use the */
/*     word `class' to distinguish this item from the column's data */
/*     type.) */


/*     TYPIDX is the index of the column's data type code (CHR, INT, DP, */
/*     or TIME).  The type is actually implied by the class, but it */
/*     will frequently be convenient to look up the type directly. */



/*     LENIDX is the index of the column's string length value, if the */
/*     column has character type.  A value of IFALSE in this element of */
/*     the descriptor indicates that the strings have variable length. */


/*     SIZIDX is the index of the column's element size value.  This */
/*     descriptor element is meaningful for columns with fixed-size */
/*     entries.  For variable-sized columns, this value is IFALSE. */


/*     NAMIDX is the index of the base address of the column's name. */


/*     IXTIDX is the data type of the column's index.  IXTIDX */
/*     contains a type value only if the column is indexed. For columns */
/*     that are not indexed, the location IXTIDX contains the boolean */
/*     value IFALSE. */


/*     IXPIDX is a pointer to the column's index.  IXTPDX contains a */
/*     meaningful value only if the column is indexed.  The */
/*     interpretation of the pointer depends on the data type of the */
/*     index. */


/*     NFLIDX is the index of a flag indicating whether nulls are */
/*     permitted in the column.  The value at location NFLIDX is */
/*     ITRUE if nulls are permitted and IFALSE otherwise. */


/*     ORDIDX is the index of the column's ordinal position in the */
/*     list of columns belonging to the column's parent segment. */


/*     METIDX is the index of the column's integer metadata pointer. */
/*     This pointer is a DAS integer address. */


/*     The last position in the column descriptor is reserved.  No */
/*     parameter is defined to point to this location. */


/*     End Include Section:  EK Column Descriptor Parameters */

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


/*     Include Section:  EK Segment Descriptor Parameters */

/*        eksegdsc.inc  Version 8  06-NOV-1995 (NJB) */


/*     All `base addresses' referred to below are the addresses */
/*     *preceding* the item the base applies to.  This convention */
/*     enables simplied address calculations in many cases. */

/*     Size of segment descriptor.  Note:  the include file ekcoldsc.inc */
/*     must be updated if this parameter is changed.  The parameter */
/*     CDOFF in that file should be kept equal to SDSCSZ. */


/*     Index of the segment type code: */


/*     Index of the segment's number.  This number is the segment's */
/*     index in the list of segments contained in the EK to which */
/*     the segment belongs. */


/*     Index of the DAS integer base address of the segment's integer */
/*     meta-data: */


/*     Index of the DAS character base address of the table name: */


/*     Index of the segment's column count: */


/*     Index of the segment's record count: */


/*     Index of the root page number of the record tree: */


/*     Index of the root page number of the character data page tree: */


/*     Index of the root page number of the double precision data page */
/*     tree: */


/*     Index of the root page number of the integer data page tree: */


/*     Index of the `modified' flag: */


/*     Index of the `initialized' flag: */


/*     Index of the shadowing flag: */


/*     Index of the companion file handle: */


/*     Index of the companion segment number: */


/*     The next three items are, respectively, the page numbers of the */
/*     last character, d.p., and integer data pages allocated by the */
/*     segment: */


/*     The next three items are, respectively, the page-relative */
/*     indices of the last DAS word in use in the segment's */
/*     last character, d.p., and integer data pages: */


/*     Index of the DAS character base address of the column name list: */


/*     The last descriptor element is reserved for future use.  No */
/*     parameter is defined to point to this location. */


/*     End Include Section:  EK Segment Descriptor Parameters */


/*     Set the maximum number of columns we allow in a single EK table. */

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


/*     Include Section:  EK Table Name Size */

/*        ektnamsz.inc Version 1    17-JAN-1995 (NJB) */


/*     Size of table name, in characters. */


/*     End Include Section:  EK Table Name Size */


/*     Set the length of a text line. */


/*     Set the size for the character type equivalents. */


/*     Set up the page size for displaying headings for the EK column */
/*     description table. */


/*     Set up a mnemonic for displaying the table description data. */


/*     Set up some mnemonics for the display of the table's columnar */
/*     data */


/*     Set up some mnemonics for the data types used in an EK file. */


/*     Set the maximum number of data types. */


/*     Parameter for the standard output unit. */


/*     Set the length for an output line. */


/*     Set value for a separator */


/*     Set up some character string equivalents for some things. */


/*     Set up labels for various output things. */


/*     Local variables */


/*     Initial Values */

/*     Set up some character string equivalents for the data types. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SUMEK", (ftnlen)5);
    }

/*     Initialize the separator. */

    s_copy(separ, "*********************************************************"
	    "***********************", (ftnlen)80, (ftnlen)80);

/*     Initialize the table separator. */

    s_copy(sumsep, "--------------------------------------------------------"
	    "------------------------", (ftnlen)80, (ftnlen)80);

/*     Verify that the file attached to HANDLE is opened for reading */
/*     by calling the routine to signal an invalid access mode on a */
/*     handle. */

    dassih_(handle, "READ", (ftnlen)4);
    if (failed_()) {
	chkout_("SUMEK", (ftnlen)5);
	return 0;
    }
    repmc_("Summary for EK file: #", "#", binfnm, fnmout, (ftnlen)22, (ftnlen)
	    1, binfnm_len, (ftnlen)255);
    writln_(" ", &c__6, (ftnlen)1);
    writln_(separ, &c__6, (ftnlen)80);
    writln_(" ", &c__6, (ftnlen)1);
    writln_(fnmout, &c__6, (ftnlen)255);
    writln_(" ", &c__6, (ftnlen)1);
    if (*logfil) {
	writln_(" ", loglun, (ftnlen)1);
	writln_(separ, loglun, (ftnlen)80);
	writln_(" ", loglun, (ftnlen)1);
	writln_(fnmout, loglun, (ftnlen)255);
	writln_(" ", loglun, (ftnlen)1);
    }

/*     Get the number of tables in the EK file, and write it. */

    nsegs = eknseg_(handle);
    s_copy(line, "Number of Tables: #", (ftnlen)255, (ftnlen)19);
    repmi_(line, "#", &nsegs, line, (ftnlen)255, (ftnlen)1, (ftnlen)255);
    writln_(line, &c__6, (ftnlen)255);
    writln_(" ", &c__6, (ftnlen)1);
    if (*logfil) {
	writln_(line, loglun, (ftnlen)255);
	writln_(" ", loglun, (ftnlen)1);
    }
    if (failed_()) {
	chkout_("SUMEK", (ftnlen)5);
	return 0;
    }

/*     Loop through all of the tables in the EK file, writing out the */
/*     summary information for each of them. */

    i__1 = nsegs;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Get the summary information for the EK table with index I. */

	zzeksinf_(handle, &i__, tabnam, segdsc, cnames, cdscrs, (ftnlen)64, (
		ftnlen)32);
	if (failed_()) {
	    chkout_("SUMEK", (ftnlen)5);
	    return 0;
	}

/*        Write out the current table number. */

	s_copy(line, "Table number: #", (ftnlen)255, (ftnlen)15);
	repmi_(line, "#", &i__, line, (ftnlen)255, (ftnlen)1, (ftnlen)255);
	writln_(line, &c__6, (ftnlen)255);
	if (*logfil) {
	    writln_(line, loglun, (ftnlen)255);
	}
	if (failed_()) {
	    chkout_("SUMEK", (ftnlen)5);
	    return 0;
	}

/*        Write out a line that marks the beginning of the table. */

	writln_(sumsep, &c__6, (ftnlen)80);
	if (*logfil) {
	    writln_(sumsep, loglun, (ftnlen)80);
	}
	if (failed_()) {
	    chkout_("SUMEK", (ftnlen)5);
	    return 0;
	}

/*        Write out the table name. */

	mark = 16;
	s_copy(line, "   Table Name:", (ftnlen)255, (ftnlen)14);
	s_copy(line + (mark - 1), "#", 255 - (mark - 1), (ftnlen)1);
	repmc_(line, "#", tabnam, line, (ftnlen)255, (ftnlen)1, (ftnlen)64, (
		ftnlen)255);
	writln_(line, &c__6, (ftnlen)255);
	if (*logfil) {
	    writln_(line, loglun, (ftnlen)255);
	}
	if (failed_()) {
	    chkout_("SUMEK", (ftnlen)5);
	    return 0;
	}

/*        Write out the number of rows in the table. */

	mark = 16;
	nrows = segdsc[5];
	s_copy(line, "   Rows      :", (ftnlen)255, (ftnlen)14);
	s_copy(line + (mark - 1), "#", 255 - (mark - 1), (ftnlen)1);
	repmi_(line, "#", &nrows, line, (ftnlen)255, (ftnlen)1, (ftnlen)255);
	writln_(line, &c__6, (ftnlen)255);
	if (*logfil) {
	    writln_(line, loglun, (ftnlen)255);
	}
	if (failed_()) {
	    chkout_("SUMEK", (ftnlen)5);
	    return 0;
	}

/*        Write out the number of columns in the table. */

	mark = 16;
	ncols = segdsc[4];
	s_copy(line, "   Columns   :", (ftnlen)255, (ftnlen)14);
	s_copy(line + (mark - 1), "#", 255 - (mark - 1), (ftnlen)1);
	repmi_(line, "#", &ncols, line, (ftnlen)255, (ftnlen)1, (ftnlen)255);
	writln_(line, &c__6, (ftnlen)255);
	if (*logfil) {
	    writln_(line, loglun, (ftnlen)255);
	}
	if (failed_()) {
	    chkout_("SUMEK", (ftnlen)5);
	    return 0;
	}

/*        Begin writing out the columns for this table. */

	lincnt = 0;
	i__2 = ncols;
	for (j = 1; j <= i__2; ++j) {

/*           If we need to, write out some headings for the EK column */
/*           description table. */

	    if (lincnt == 0) {
		writln_(" ", &c__6, (ftnlen)1);
		s_copy(line, " ", (ftnlen)255, (ftnlen)1);
		s_copy(line + 3, "#", (ftnlen)252, (ftnlen)1);
		repmc_(line, "#", "Column Name", line, (ftnlen)255, (ftnlen)1,
			 (ftnlen)11, (ftnlen)255);
		s_copy(line + 36, "#", (ftnlen)219, (ftnlen)1);
		repmc_(line, "#", "Type", line, (ftnlen)255, (ftnlen)1, (
			ftnlen)4, (ftnlen)255);
		s_copy(line + 41, "#", (ftnlen)214, (ftnlen)1);
		repmc_(line, "#", "Length", line, (ftnlen)255, (ftnlen)1, (
			ftnlen)6, (ftnlen)255);
		s_copy(line + 48, "#", (ftnlen)207, (ftnlen)1);
		repmc_(line, "#", "Fixed", line, (ftnlen)255, (ftnlen)1, (
			ftnlen)5, (ftnlen)255);
		s_copy(line + 54, "#", (ftnlen)201, (ftnlen)1);
		repmc_(line, "#", "Size", line, (ftnlen)255, (ftnlen)1, (
			ftnlen)4, (ftnlen)255);
		s_copy(line + 65, "#", (ftnlen)190, (ftnlen)1);
		repmc_(line, "#", "Index", line, (ftnlen)255, (ftnlen)1, (
			ftnlen)5, (ftnlen)255);
		s_copy(line + 71, "#", (ftnlen)184, (ftnlen)1);
		repmc_(line, "#", "Null", line, (ftnlen)255, (ftnlen)1, (
			ftnlen)4, (ftnlen)255);
		writln_(line, &c__6, (ftnlen)255);
		if (*logfil) {
		    writln_(line, loglun, (ftnlen)255);
		}

/*              Write out a seperator line for the table headings. */

		s_copy(line, "   -------------------------------- ---- -----"
			"- ----- ---------- ----- ----", (ftnlen)255, (ftnlen)
			75);
		writln_(line, &c__6, (ftnlen)255);
		if (*logfil) {
		    writln_(line, loglun, (ftnlen)255);
		}
		if (failed_()) {
		    chkout_("SUMEK", (ftnlen)5);
		    return 0;
		}
	    }
	    ++lincnt;

/*           Place the column name into the output line. */

	    s_copy(line + 3, "#", (ftnlen)252, (ftnlen)1);
	    repmc_(line, "#", cnames + (((i__3 = j - 1) < 100 && 0 <= i__3 ? 
		    i__3 : s_rnge("cnames", i__3, "sumek_", (ftnlen)535)) << 
		    5), line, (ftnlen)255, (ftnlen)1, (ftnlen)32, (ftnlen)255)
		    ;

/*           Place the column data type and length, if applicable, into */
/*           the output line. */

	    s_copy(line + 36, "#", (ftnlen)219, (ftnlen)1);
	    if (cdscrs[(i__3 = j * 11 - 10) < 1100 && 0 <= i__3 ? i__3 : 
		    s_rnge("cdscrs", i__3, "sumek_", (ftnlen)541)] == 1) {
		repmc_(line, "#", strtyp, line, (ftnlen)255, (ftnlen)1, (
			ftnlen)4, (ftnlen)255);
		s_copy(line + 41, "#", (ftnlen)214, (ftnlen)1);
		if (cdscrs[(i__3 = j * 11 - 9) < 1100 && 0 <= i__3 ? i__3 : 
			s_rnge("cdscrs", i__3, "sumek_", (ftnlen)545)] == -1) 
			{
		    repmc_(line, "#", "Var", line, (ftnlen)255, (ftnlen)1, (
			    ftnlen)3, (ftnlen)255);
		} else {
		    repmi_(line, "#", &cdscrs[(i__3 = j * 11 - 9) < 1100 && 0 
			    <= i__3 ? i__3 : s_rnge("cdscrs", i__3, "sumek_", 
			    (ftnlen)548)], line, (ftnlen)255, (ftnlen)1, (
			    ftnlen)255);
		}
	    } else if (cdscrs[(i__3 = j * 11 - 10) < 1100 && 0 <= i__3 ? i__3 
		    : s_rnge("cdscrs", i__3, "sumek_", (ftnlen)551)] == 2) {
		repmc_(line, "#", strtyp + 4, line, (ftnlen)255, (ftnlen)1, (
			ftnlen)4, (ftnlen)255);
		s_copy(line + 41, "#", (ftnlen)214, (ftnlen)1);
		repmc_(line, "#", "n/a", line, (ftnlen)255, (ftnlen)1, (
			ftnlen)3, (ftnlen)255);
	    } else if (cdscrs[(i__3 = j * 11 - 10) < 1100 && 0 <= i__3 ? i__3 
		    : s_rnge("cdscrs", i__3, "sumek_", (ftnlen)557)] == 3) {
		repmc_(line, "#", strtyp + 8, line, (ftnlen)255, (ftnlen)1, (
			ftnlen)4, (ftnlen)255);
		s_copy(line + 41, "#", (ftnlen)214, (ftnlen)1);
		repmc_(line, "#", "n/a", line, (ftnlen)255, (ftnlen)1, (
			ftnlen)3, (ftnlen)255);
	    } else if (cdscrs[(i__3 = j * 11 - 10) < 1100 && 0 <= i__3 ? i__3 
		    : s_rnge("cdscrs", i__3, "sumek_", (ftnlen)563)] == 4) {
		repmc_(line, "#", strtyp + 12, line, (ftnlen)255, (ftnlen)1, (
			ftnlen)4, (ftnlen)255);
		s_copy(line + 41, "#", (ftnlen)214, (ftnlen)1);
		repmc_(line, "#", "n/a", line, (ftnlen)255, (ftnlen)1, (
			ftnlen)3, (ftnlen)255);
	    } else {
		setmsg_("Unknown data type encountered, value: #", (ftnlen)39)
			;
		errint_("#", &cdscrs[(i__3 = j * 11 - 10) < 1100 && 0 <= i__3 
			? i__3 : s_rnge("cdscrs", i__3, "sumek_", (ftnlen)573)
			], (ftnlen)1);
		sigerr_("SPICE(UNKNOWNDATATYPE)", (ftnlen)22);
		chkout_("SUMEK", (ftnlen)5);
		return 0;
	    }

/*           Place the fixed length column indicator and the length, when */
/*           applicable, into the output line. */

	    s_copy(line + 48, "#", (ftnlen)207, (ftnlen)1);
	    if (cdscrs[(i__3 = j * 11 - 8) < 1100 && 0 <= i__3 ? i__3 : 
		    s_rnge("cdscrs", i__3, "sumek_", (ftnlen)584)] == -1) {
		repmc_(line, "#", "N", line, (ftnlen)255, (ftnlen)1, (ftnlen)
			1, (ftnlen)255);
		s_copy(line + 54, "#", (ftnlen)201, (ftnlen)1);
		repmc_(line, "#", "n/a", line, (ftnlen)255, (ftnlen)1, (
			ftnlen)3, (ftnlen)255);
	    } else {
		repmc_(line, "#", "Y", line, (ftnlen)255, (ftnlen)1, (ftnlen)
			1, (ftnlen)255);
		s_copy(line + 54, "#", (ftnlen)201, (ftnlen)1);
		repmi_(line, "#", &cdscrs[(i__3 = j * 11 - 8) < 1100 && 0 <= 
			i__3 ? i__3 : s_rnge("cdscrs", i__3, "sumek_", (
			ftnlen)594)], line, (ftnlen)255, (ftnlen)1, (ftnlen)
			255);
	    }

/*           Place the indexed column indicator into the output line. */

	    s_copy(line + 65, "#", (ftnlen)190, (ftnlen)1);
	    if (cdscrs[(i__3 = j * 11 - 6) < 1100 && 0 <= i__3 ? i__3 : 
		    s_rnge("cdscrs", i__3, "sumek_", (ftnlen)601)] != -1) {
		repmc_(line, "#", "Y", line, (ftnlen)255, (ftnlen)1, (ftnlen)
			1, (ftnlen)255);
	    } else {
		repmc_(line, "#", "N", line, (ftnlen)255, (ftnlen)1, (ftnlen)
			1, (ftnlen)255);
	    }

/*           Place the null values column indicator into the output line. */

	    s_copy(line + 71, "#", (ftnlen)184, (ftnlen)1);
	    if (cdscrs[(i__3 = j * 11 - 4) < 1100 && 0 <= i__3 ? i__3 : 
		    s_rnge("cdscrs", i__3, "sumek_", (ftnlen)610)] != -1) {
		repmc_(line, "#", "Y", line, (ftnlen)255, (ftnlen)1, (ftnlen)
			1, (ftnlen)255);
	    } else {
		repmc_(line, "#", "N", line, (ftnlen)255, (ftnlen)1, (ftnlen)
			1, (ftnlen)255);
	    }

/*           Finally, write out the column description. */

	    writln_(line, &c__6, (ftnlen)255);
	    if (*logfil) {
		writln_(line, loglun, (ftnlen)255);
	    }
	    if (failed_()) {
		chkout_("SUMEK", (ftnlen)5);
		return 0;
	    }

/*           Reset the line counter if we need to so that we will use the */
/*           table headings again. */

	    if (lincnt == 22) {
		lincnt = 0;
	    }

/*        If there was some sort of errer, then check out and return. */

	    if (failed_()) {
		chkout_("SUMEK", (ftnlen)5);
		return 0;
	    }
	}

/*        Write out a line that marks the end of the table, and then */
/*        skip a line. */

	writln_(sumsep, &c__6, (ftnlen)80);
	writln_(" ", &c__6, (ftnlen)1);
	if (*logfil) {
	    writln_(sumsep, loglun, (ftnlen)80);
	    writln_(" ", loglun, (ftnlen)1);
	}

/*        If there was some sort of error, then check out and return. */

	if (failed_()) {
	    chkout_("SUMEK", (ftnlen)5);
	    return 0;
	}
    }
    writln_(separ, &c__6, (ftnlen)80);
    writln_(" ", &c__6, (ftnlen)1);
    if (*logfil) {
	writln_(separ, loglun, (ftnlen)80);
	writln_(" ", loglun, (ftnlen)1);
    }
    chkout_("SUMEK", (ftnlen)5);
    return 0;
} /* sumek_ */

