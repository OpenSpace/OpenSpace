/* convbt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;

/* $ Procedure CONVBT ( Convert Kernel file from binary to text ) */
/* Subroutine */ int convbt_(char *binfil, char *txtfil, ftnlen binfil_len, 
	ftnlen txtfil_len)
{
    /* System generated locals */
    cllist cl__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), f_clos(cllist *), s_wsle(
	    cilist *), do_lio(integer *, integer *, char *, ftnlen), e_wsle(
	    void);

    /* Local variables */
    extern /* Subroutine */ int dafbt_(char *, integer *, ftnlen);
    char farch[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen), spcec_(integer *, 
	    integer *), dasbt_(char *, integer *, ftnlen), errch_(char *, 
	    char *, ftnlen, ftnlen);
    char ftype[4];
    extern logical failed_(void);
    integer handle;
    extern /* Subroutine */ int dafcls_(integer *), getfat_(char *, char *, 
	    char *, ftnlen, ftnlen, ftnlen), dafopr_(char *, integer *, 
	    ftnlen), sigerr_(char *, ftnlen), chkout_(char *, ftnlen), 
	    setmsg_(char *, ftnlen);
    integer iostat;
    logical comnts;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int txtopn_(char *, integer *, ftnlen);
    integer txtlun;

    /* Fortran I/O blocks */
    static cilist io___7 = { 1, 0, 0, 0, 0 };
    static cilist io___8 = { 1, 0, 0, 0, 0 };


/* $ Abstract */

/*     Convert a SPICE binary file to an equivalent text file format. */

/*     NOTE: This routine is currently for use ONLY with the SPACIT */
/*           utility program. Use it at your own risk. */

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

/*     CONVERSION */
/*     FILES */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*      BINFIL    I   Name of an existing SPICE binary file. */
/*      TXTFIL    I   Name of the text file to be created. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) */

/* $ Particulars */

/*     This routine accepts as inputs the name of a binary file to be */
/*     converted to text and the name of the text file to be created. */
/*     The binary file must already exist and the text file must not */
/*     exist for this routine to work correctly. The architecture and the */
/*     file type are determined and then an appropriate file conversion */
/*     is performed. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     1) */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer (JPL) */

/* $ Version */

/* -    Beta Version 3.2.0, 30-AUG-1994 (KRG) */

/*        Improved the error diagnostics when incorrect inputs are */
/*        provided, e.g., a transfer filename instead of a binary kernel */
/*        filename. */

/* -    Beta Version 3.1.0, 12-AUG-1994 (KRG) */

/*        Fixed a minor bug that would occur when formatting a long error */
/*        message. ERRFNM was called with a logical unit that had already */
/*        been closed. */

/* -    Beta Version 3.0.0, 22-APR-1994 (KRG) */

/*        Made updates to the routine to make use of the new SPICE */
/*        capability of determining binary kernel file types at run time. */

/*        Removed the arguments for the file architecture and file type */
/*        from the calling list. This information was no longer */
/*        necessary. */

/*        Rearranged some of the code to make it easier to understand. */

/*        Added a new error: if the architecture or type are not */
/*        recognized, we can't process the file. */

/* -    Beta Version 2.0.0, 28-JAN-1994 (KRG) */

/* -& */
/* $ Index_Entries */

/*     convert binary SPICE files to text */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     Begin and end markers in the file for the comment area. */


/*     File types that are recognized. */


/*     Length of a file architecture. */


/*     Maximum length for a file type. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CONVBT", (ftnlen)6);
    }

/*     Initialize the file architecture and the file type. */

    s_copy(farch, " ", (ftnlen)3, (ftnlen)1);
    s_copy(ftype, " ", (ftnlen)4, (ftnlen)1);

/*     Get the architecture and type of the binary file. */

    getfat_(binfil, farch, ftype, binfil_len, (ftnlen)3, (ftnlen)4);
    if (failed_()) {

/*        If there was an error getting the file architecture, just */
/*        return. An appropriate error message should have been set. */
/*        So, all we need to do here is return to the caller. */

	chkout_("CONVBT", (ftnlen)6);
	return 0;
    }

/*     Check to see that we got back a valid architecture and type. */


/*     Open the text file for output, obtaining a Fortran logical */
/*     unit. */

    txtopn_(txtfil, &txtlun, txtfil_len);
    if (failed_()) {

/*        If there was an error opening the text file, just return. */
/*        An appropriate error message should have been set by TXTOPN. */
/*        So, all we need to do here is return to the caller. */

	chkout_("CONVBT", (ftnlen)6);
	return 0;
    }

/*     Process the files based on their binary architectures */

    if (s_cmp(farch, "DAF", (ftnlen)3, (ftnlen)3) == 0) {

/*        If the file is a NAIF SPK, CK, or PCK binary file, it may have */
/*        a comment area. So set the COMNTS flag appropriately. */

	comnts = s_cmp(ftype, "SPK", (ftnlen)4, (ftnlen)3) == 0;
	comnts = comnts || s_cmp(ftype, "CK", (ftnlen)4, (ftnlen)2) == 0;
	comnts = comnts || s_cmp(ftype, "PCK", (ftnlen)4, (ftnlen)3) == 0;

/*        First, convert the data portion of the binary file to text. */
/*        We only support the latest and greatest text file format for */
/*        conversion of the binary files to text. */

	dafbt_(binfil, &txtlun, binfil_len);
	if (failed_()) {

/*           If an error occurred while attempting to convert the */
/*           data portion of the DAF file to text, we need to close */
/*           the text file and return to the caller. We will delete */
/*           the text file when we close it. */

	    cl__1.cerr = 0;
	    cl__1.cunit = txtlun;
	    cl__1.csta = "DELETE";
	    f_clos(&cl__1);
	    chkout_("CONVBT", (ftnlen)6);
	    return 0;
	}

/*        The DAF file may or may not have a comment area. If it is a */
/*        NAIF SPICE kernel file, then it does and we need to deal with */
/*        it. Otherwise we do nothing. */

	if (comnts) {

/*           We need to open the binary DAF file so that we can extract */
/*           the comments from its comment area and place them in the */
/*           text file. */

	    dafopr_(binfil, &handle, binfil_len);
	    if (failed_()) {

/*              If an error occurred, we need to close the text file and */
/*              return to the caller. We will delete the text file when */
/*              we close it. */

		cl__1.cerr = 0;
		cl__1.cunit = txtlun;
		cl__1.csta = "DELETE";
		f_clos(&cl__1);
		chkout_("CONVBT", (ftnlen)6);
		return 0;
	    }

/*           Write the begin comments marker to the text file. */

	    io___7.ciunit = txtlun;
	    iostat = s_wsle(&io___7);
	    if (iostat != 0) {
		goto L100001;
	    }
	    iostat = do_lio(&c__9, &c__1, "~NAIF/SPC BEGIN COMMENTS~", (
		    ftnlen)25);
	    if (iostat != 0) {
		goto L100001;
	    }
	    iostat = e_wsle();
L100001:
	    if (iostat != 0) {

/*              An error occurred, so close both the text and binary */
/*              files, set an appropriate error message, and return to */
/*              the caller. The text file is deleted when it is closed. */

		cl__1.cerr = 0;
		cl__1.cunit = txtlun;
		cl__1.csta = "DELETE";
		f_clos(&cl__1);
		dafcls_(&handle);
		setmsg_("Error writing the begin comments marker to the text"
			" file: #. IOSTAT = #.", (ftnlen)72);
		errch_("#", txtfil, (ftnlen)1, txtfil_len);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
		chkout_("CONVBT", (ftnlen)6);
		return 0;
	    }

/*           Extract the comment area of the binary file to the text */
/*           file. */

	    spcec_(&handle, &txtlun);
	    if (failed_()) {

/*              If the comment extraction failed, then an appropriate */
/*              error message should have already been set, so close */
/*              the text and binary files and return to the caller. The */
/*              text file is deleted when it is closed. */

		cl__1.cerr = 0;
		cl__1.cunit = txtlun;
		cl__1.csta = "DELETE";
		f_clos(&cl__1);
		chkout_("CONVBT", (ftnlen)6);
		return 0;
	    }

/*           Write the end comments marker. */

	    io___8.ciunit = txtlun;
	    iostat = s_wsle(&io___8);
	    if (iostat != 0) {
		goto L100002;
	    }
	    iostat = do_lio(&c__9, &c__1, "~NAIF/SPC END COMMENTS~", (ftnlen)
		    23);
	    if (iostat != 0) {
		goto L100002;
	    }
	    iostat = e_wsle();
L100002:
	    if (iostat != 0) {

/*              An error occurred, so close both the text and binary */
/*              files, set an appropriate error message, and return to */
/*              the caller. The text file is deleted when it is closed. */

		cl__1.cerr = 0;
		cl__1.cunit = txtlun;
		cl__1.csta = "DELETE";
		f_clos(&cl__1);
		dafcls_(&handle);
		setmsg_("Error writing the end comments marker to the text f"
			"ile: #. IOSTAT = #.", (ftnlen)70);
		errch_("#", txtfil, (ftnlen)1, txtfil_len);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
		chkout_("CONVBT", (ftnlen)6);
		return 0;
	    }

/*           Close the binary DAF file that we opened to extract the */
/*           comments. */

	    dafcls_(&handle);
	}
    } else if (s_cmp(farch, "DAS", (ftnlen)3, (ftnlen)3) == 0) {

/*        DAS files are easy. Everything is integrated into the files */
/*        so we do not need to worry about comments or reserved records */
/*        or anything. We just convert it. */

/*        Convert the data portion of the binary file to text. We */
/*        only support the latest and greatest text file format for */
/*        conversion of the binary files to text. */

	dasbt_(binfil, &txtlun, binfil_len);
	if (failed_()) {

/*           If an error occurred while attempting to convert the */
/*           DAS file to text, we need to close the text file and */
/*           return to the caller. We will delete the text file */
/*           when we close it. */

	    cl__1.cerr = 0;
	    cl__1.cunit = txtlun;
	    cl__1.csta = "DELETE";
	    f_clos(&cl__1);
	    chkout_("CONVBT", (ftnlen)6);
	    return 0;
	}
    } else if (s_cmp(farch, "XFR", (ftnlen)3, (ftnlen)3) == 0) {

/*        This is an error case, most likely caused by reading a transfer */
/*        file by accident. So signal an appropriate error. */

	cl__1.cerr = 0;
	cl__1.cunit = txtlun;
	cl__1.csta = "DELETE";
	f_clos(&cl__1);
	setmsg_("The file '#' appears to be a transfer file and not a binary"
		" kernel file.", (ftnlen)72);
	errch_("#", binfil, (ftnlen)1, binfil_len);
	sigerr_("SPICE(NOTABINARYKERNEL)", (ftnlen)23);
	chkout_("CONVBT", (ftnlen)6);
	return 0;
    } else if (s_cmp(farch, "DEC", (ftnlen)3, (ftnlen)3) == 0) {

/*        This is an error case, most likely caused by reading a transfer */
/*        file by accident. So signal an appropriate error. */

	cl__1.cerr = 0;
	cl__1.cunit = txtlun;
	cl__1.csta = "DELETE";
	f_clos(&cl__1);
	setmsg_("The file '#' appears to be a decimal transfer file and not "
		"a binary kernel file.", (ftnlen)80);
	errch_("#", binfil, (ftnlen)1, binfil_len);
	sigerr_("SPICE(NOTABINARYKERNEL)", (ftnlen)23);
	chkout_("CONVBT", (ftnlen)6);
	return 0;
    } else {

/*        This is the catch all error case. At this point, we didn't */
/*        match any of the files whose architecture and types are */
/*        recognized. So, we toss our hands in the air and signal an */
/*        error. */

	cl__1.cerr = 0;
	cl__1.cunit = txtlun;
	cl__1.csta = "DELETE";
	f_clos(&cl__1);
	setmsg_("The architecture and type of the file '#' were not recogniz"
		"ed.", (ftnlen)62);
	errch_("#", binfil, (ftnlen)1, binfil_len);
	sigerr_("SPICE(BADFILEFORMAT)", (ftnlen)20);
	chkout_("CONVBT", (ftnlen)6);
	return 0;
    }

/*     Close the text file that was created. */

    cl__1.cerr = 0;
    cl__1.cunit = txtlun;
    cl__1.csta = 0;
    f_clos(&cl__1);
    chkout_("CONVBT", (ftnlen)6);
    return 0;
} /* convbt_ */

