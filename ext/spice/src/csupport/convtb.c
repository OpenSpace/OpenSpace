/* convtb.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;

/* $ Procedure CONVTB ( Convert kernel file from text to binary ) */
/* Subroutine */ int convtb_(char *txtfil, char *binfil, ftnlen txtfil_len, 
	ftnlen binfil_len)
{
    /* System generated locals */
    integer i__1;
    cilist ci__1;
    olist o__1;
    cllist cl__1;
    alist al__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rsfe(cilist *), do_fio(
	    integer *, char *, ftnlen), e_rsfe(void), f_clos(cllist *), 
	    f_back(alist *), f_open(olist *), s_wsfe(cilist *), e_wsfe(void);

    /* Local variables */
    char arch[3], line[255], type__[4];
    extern /* Subroutine */ int daftb_(integer *, char *, ftnlen), spcac_(
	    integer *, integer *, char *, char *, ftnlen, ftnlen), chkin_(
	    char *, ftnlen), dastb_(integer *, char *, ftnlen), errch_(char *,
	     char *, ftnlen, ftnlen);
    extern integer ltrim_(char *, ftnlen), rtrim_(char *, ftnlen);
    extern /* Subroutine */ int daft2b_(integer *, char *, integer *, ftnlen);
    extern logical failed_(void);
    integer handle;
    extern /* Subroutine */ int dafcls_(integer *), getfat_(char *, char *, 
	    char *, ftnlen, ftnlen, ftnlen);
    logical havcom;
    extern /* Subroutine */ int dafopw_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), getlun_(integer *), 
	    setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    integer scrlun;
    extern logical return_(void);
    integer txtlun;
    extern /* Subroutine */ int txtopr_(char *, integer *, ftnlen);
    logical eoc;

/* $ Abstract */

/*     Convert a SPICE text file into its equivalent binary format. */

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

/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     TXTFIL     I   Name of text file to be converted. */
/*     BINARY     I   Name of a binary file to be created. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     1)  This routine uses a Fortran scratch file to temporarily */
/*         store the lines of comments if there are any. */

/* $ Exceptions */

/*     1) If there is a problem opening or writing to the binary */
/*        file, a routine that CONVTB calls diagnoses and signals */
/*        an error. */

/*     2) If there is a problem reading from the text file, the */
/*        error SPICE(FILEREADFAILED) is signalled. */

/*     3) If there is a problem opening the scratch file, the error */
/*        SPICE(FILEOPENERROR) is signalled. */

/*     4) If there is a problem writing to the scratch file, the */
/*        error SPICE(FILEWRITEFAILED) is signalled. */

/*     5) If the binary file archictecture is not recognized, the error */
/*        SPICE(UNSUPPBINARYARCH) will be signalled. */

/*     7) If the transfer file format is not recognized, the error */
/*        SPICE(NOTATRANSFERFILE) will be signalled. */

/*     8) If the input file format cannot be identified, the error */
/*        SPICE(UNRECOGNIZABLEFILE) will be signalled.. */

/* $ Particulars */

/*     This routine is currently only for use with the SPACIT program. */

/* $ Examples */



/* $ Restrictions */

/*     1)  This routine assumes that the data and comments in the */
/*         text format SPK, PCK or CK file come from a binary file */
/*         and were written by one of the SPICELIB binary to text */
/*         conversion routines. Data and/or comments written any */
/*         other way may not be in the correct format and, therefore, */
/*         may not be handled properly. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer  (JPL) */

/* $ Version */

/* -    Beta Version 3.2.0, 12-AUG-1994 (KRG) */

/*        Added a new exception SPICE(UNRECOGNIZABLEFILE). This occurs */
/*        when the file that is to be converted cannot be adequately */
/*        identified to give a meaningful error message or proceed with */
/*        the conversion. */

/*        Fixed (I hope) a bug when dealing with LARGE binary files that */
/*        were accidentally passed to this reoutine as the text/transfer */
/*        file to be converted. The routine would hang in a formatted */
/*        READ while searching for the next "record" inthe binary file. */
/*        Not Good. */

/* -    Beta Version 3.1.0, 12-AUG-1994 (KRG) */

/*        Added a few more tests for incorrect files on input. */

/*        Fixed a minor bug that would occur when formatting a long error */
/*        message. ERRFNM was called with a logical unit that had already */
/*        been closed. */

/*        ERRFNM was also called with the logical unit of a SCRATCH file. */
/*        Fortran 77 does not allow names to be associated with SCRATCH */
/*        files, so this was removed. */

/*        Replaced the exception SPICE(UNSUPPTEXTFORMAT) with the */
/*        exception: SPICE(NOTATRANSFERFILE). */

/* -    Beta Version 3.0.0, 22-APR-1994 (KRG) */

/*        Made updates to the routine to make use of the new SPICE */
/*        capability of determining binary kernel file types at run time. */

/*        Removed the arguments for the file architecture and file type */
/*        from the calling list. This information was no longer */
/*        necessary. */

/*        Rearranged some of the code to make it easier to understand. */

/*        When checking for comments in a DAF text file the program now */
/*        backspaces if the forst record read after the data portion of */
/*        the file has been converted does not indicate the existence */
/*        of comments in the text file. This repositions the input */
/*        pointer so that the next read will return the record that we */
/*        read to test for the existence of comments. THIS ACTION IS */
/*        DIFFERENT FROM THE BEHAVIOR OF PREVIOUS VERSIONS OF THIS */
/*        ROUTINE WHICH DID NOT PERFORM THE BACKSPACE. See the comments */
/*        in the code near the end of this file for the details. */

/* -    Beta Version 2.0.0, 28-JAN-1994 (KRG) */

/* -& */
/* $ Index_Entries */

/*     convert text SPICE files to binary */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     Begin and end markers in the file for the comment area. */


/*     Maximum length of an input text line. */


/*     Maximum length of a file architecture. */


/*     Maximum length of a file type. */


/*     Number of reserved records to use when creating a binar DAF file. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CONVTB", (ftnlen)6);
    }

/*     Get the architecture and type of the file to be converted. */

    getfat_(txtfil, arch, type__, txtfil_len, (ftnlen)3, (ftnlen)4);
    if (failed_()) {
	chkout_("CONVTB", (ftnlen)6);
	return 0;
    }

/*     Verify the architecture and type of the file, and perform any */
/*     processing necessary.. */

    if (s_cmp(arch, "XFR", (ftnlen)3, (ftnlen)3) == 0 || s_cmp(arch, "DEC", (
	    ftnlen)3, (ftnlen)3) == 0) {

/*        Open the text file that is to be converted to binary. */

	txtopr_(txtfil, &txtlun, txtfil_len);
	if (failed_()) {
	    chkout_("CONVTB", (ftnlen)6);
	    return 0;
	}

/*        Read the information line to skip it. We already know the */
/*        architecture and type of the file. */

	ci__1.cierr = 1;
	ci__1.ciend = 1;
	ci__1.ciunit = txtlun;
	ci__1.cifmt = "(A)";
	iostat = s_rsfe(&ci__1);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_fio(&c__1, line, (ftnlen)255);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = e_rsfe();
L100001:
	if (iostat != 0) {

/*           If there was an error then we need to close the text */
/*           file, and then check out and return to the caller. */

	    cl__1.cerr = 0;
	    cl__1.cunit = txtlun;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    setmsg_("Error reading the text file: #. IOSTAT =  #.", (ftnlen)
		    44);
	    errch_("#", txtfil, (ftnlen)1, txtfil_len);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	    chkout_("CONVTB", (ftnlen)6);
	    return 0;
	}
    }

/*     Process the file based on the derived architecture and type. */

    if (s_cmp(arch, "XFR", (ftnlen)3, (ftnlen)3) == 0 && s_cmp(type__, "DAF", 
	    (ftnlen)4, (ftnlen)3) == 0) {

/*        We got a DAF file. */

/*        Convert the data portion of the text file to binary. At this */
/*        point, we know that we have a current DAF text file format. */

/*        We expect to have comments. */

	havcom = TRUE_;

/*        Convert it. */

	daftb_(&txtlun, binfil, binfil_len);
	if (failed_()) {

/*           If there was an error then we need to close the */
/*           text file, and then check out and return to the */
/*           caller. */

	    cl__1.cerr = 0;
	    cl__1.cunit = txtlun;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    chkout_("CONVTB", (ftnlen)6);
	    return 0;
	}
    } else if (s_cmp(arch, "XFR", (ftnlen)3, (ftnlen)3) == 0 && s_cmp(type__, 
	    "DAS", (ftnlen)4, (ftnlen)3) == 0) {

/*        We got a DAS file. So we should begin converting it to binary. */
/*        DAS files are easier: all we do is call one routine. */

/*        We do not have comments. Actually, we might but they are */
/*        included as part of the DAS file conversion process. */

	havcom = FALSE_;

/*        Convert it. */

	dastb_(&txtlun, binfil, binfil_len);
	if (failed_()) {

/*           If there was an error then we need to close the */
/*           text file, and then check out and return to the */
/*           caller. */

	    cl__1.cerr = 0;
	    cl__1.cunit = txtlun;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    chkout_("CONVTB", (ftnlen)6);
	    return 0;
	}
    } else if (s_cmp(arch, "DAS", (ftnlen)3, (ftnlen)3) == 0) {

/*        This is an error case, most likely caused by reading a binary */
/*        DAS file by accident. So signal an appropriate error. */

	setmsg_("The file '#' appears to be a binary DAS file and not a tran"
		"sfer file.", (ftnlen)69);
	errch_("#", txtfil, (ftnlen)1, txtfil_len);
	sigerr_("SPICE(NOTATRANSFERFILE)", (ftnlen)23);
	chkout_("CONVTB", (ftnlen)6);
	return 0;
    } else if (s_cmp(arch, "DAS", (ftnlen)3, (ftnlen)3) == 0 && s_cmp(type__, 
	    "PRE", (ftnlen)4, (ftnlen)3) == 0) {

/*        This is an error case, most likely caused by reading a binary */
/*        DAS file by accident. So signal an appropriate error. */

	cl__1.cerr = 0;
	cl__1.cunit = txtlun;
	cl__1.csta = 0;
	f_clos(&cl__1);
	setmsg_("The file '#' appears to be a pre-release binary DAS file an"
		"d not a transfer file.", (ftnlen)81);
	errch_("#", txtfil, (ftnlen)1, txtfil_len);
	sigerr_("SPICE(NOTATRANSFERFILE)", (ftnlen)23);
	chkout_("CONVTB", (ftnlen)6);
	return 0;
    } else if (s_cmp(arch, "DAF", (ftnlen)3, (ftnlen)3) == 0) {

/*        This is an error case, most likely caused by reading a binary */
/*        DAF file by accident. So signal an appropriate error. */

	setmsg_("The file '#' appears to be a binary DAF file and not a tran"
		"sfer file.", (ftnlen)69);
	errch_("#", txtfil, (ftnlen)1, txtfil_len);
	sigerr_("SPICE(NOTATRANSFERFILE)", (ftnlen)23);
	chkout_("CONVTB", (ftnlen)6);
	return 0;
    } else if (s_cmp(arch, "DEC", (ftnlen)3, (ftnlen)3) == 0 && s_cmp(type__, 
	    "DAF", (ftnlen)4, (ftnlen)3) == 0) {

/*        This is the case for the old text file format. It has no */
/*        identifying marks whatsoever, so we simply have to try and */
/*        convert it. */

/*        We expect to have comments. */

	havcom = TRUE_;

/*        Back up one record so that we are positioned in the file where */
/*        we were when this routine was entered. */

	al__1.aerr = 0;
	al__1.aunit = txtlun;
	f_back(&al__1);

/*        Convert it. */

	daft2b_(&txtlun, binfil, &c__0, binfil_len);
	if (failed_()) {

/*           If there was an error then we need to close the text */
/*           file, and then check out and return to the caller. */

	    cl__1.cerr = 0;
	    cl__1.cunit = txtlun;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    chkout_("CONVTB", (ftnlen)6);
	    return 0;
	}
    } else {

/*        This is the catch all error case. At this point, we didn't */
/*        match any of the files whose architecture and types are */
/*        recognized. So, we toss our hands in the air and signal an */
/*        error. */

	setmsg_("The architecture and type of the file '#'could not be deter"
		"mined.", (ftnlen)65);
	errch_("#", txtfil, (ftnlen)1, txtfil_len);
	sigerr_("SPICE(UNRECOGNIZABLEFILE)", (ftnlen)25);
	chkout_("CONVTB", (ftnlen)6);
	return 0;
    }

/*     If we have comments to process, then process them. */

    if (havcom) {

/*        There are three situations that we need to consider here: */

/*           1) We have a SPICE text file with comments. This implies */
/*              that we have a bunch of comments to be put into the */
/*              comment area that are surrounded by the begin comments */
/*              marker, BCMARK, and the end comemnts marker, ECMARK. */

/*           2) We are at the end of the file. This means that we have */
/*              an old SPICE kernel file, from the good old days before */
/*              the comment area was implemented, or we ahve a plain old */
/*              ordinary DAF file. */

/*           3) We are not at the end of the file, but there are no */
/*              comments. This means that a text DAF file may be embedded */
/*              in a larger text file or something. PDS does things like */
/*              this: SFDUs and such. */

/*        So, we need to look out for and deal with each of these */
/*        possibilities. */

	ci__1.cierr = 1;
	ci__1.ciend = 1;
	ci__1.ciunit = txtlun;
	ci__1.cifmt = "(A)";
	iostat = s_rsfe(&ci__1);
	if (iostat != 0) {
	    goto L100002;
	}
	iostat = do_fio(&c__1, line, (ftnlen)255);
	if (iostat != 0) {
	    goto L100002;
	}
	iostat = e_rsfe();
L100002:
	if (iostat > 0) {

/*           If there was an error then we need to close the text */
/*           file, and then check out and return to the caller. */

	    cl__1.cerr = 0;
	    cl__1.cunit = txtlun;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    setmsg_("Error reading the text file: #. IOSTAT = #.", (ftnlen)43)
		    ;
	    errch_("#", txtfil, (ftnlen)1, txtfil_len);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	    chkout_("CONVTB", (ftnlen)6);
	    return 0;
	}

/*        If we encountered the end of the file, just check out and */
/*        return. This is not an error. */

	if (iostat < 0) {
	    chkout_("CONVTB", (ftnlen)6);
	    return 0;
	}

/*        We got a line, so left justify it and see if it matches the */
/*        begin comments marker. If not, then use the Fortran BACKSPACE */
/*        command to reposition the file pointer to be ready to read the */
/*        line we just read. */

	i__1 = ltrim_(line, (ftnlen)255) - 1;
	if (s_cmp(line + i__1, "~NAIF/SPC BEGIN COMMENTS~", 255 - i__1, (
		ftnlen)25) != 0) {
	    al__1.aerr = 0;
	    al__1.aunit = txtlun;
	    f_back(&al__1);
	    chkout_("CONVTB", (ftnlen)6);
	    return 0;
	}

/*        We're not at the end of the file, and the line we read */
/*        is BCMARK, so we write the comments to a scratch file. */
/*        We do this because we have to use SPCAC to add the comments */
/*        to the comment area of the binary file, and SPCAC rewinds */
/*        the file. It's okay for SPCAC to rewind a scratch file, because */
/*        it will probably not be very big, but it's not okay to rewind */
/*        the file connected to TXTLUN -- we don't know the initial */
/*        location of the file pointer or how big the file is. */

	getlun_(&scrlun);
	o__1.oerr = 1;
	o__1.ounit = scrlun;
	o__1.ofnm = 0;
	o__1.orl = 0;
	o__1.osta = "SCRATCH";
	o__1.oacc = "SEQUENTIAL";
	o__1.ofm = "FORMATTED";
	o__1.oblnk = 0;
	iostat = f_open(&o__1);
	if (iostat != 0) {

/*           If there was an error then we need to close the text */
/*           file, and then check out and return to the caller. */

	    cl__1.cerr = 0;
	    cl__1.cunit = scrlun;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    cl__1.cerr = 0;
	    cl__1.cunit = txtlun;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    setmsg_("Error opening temporary file. IOSTAT = #.", (ftnlen)41);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(FILEOPENERROR)", (ftnlen)20);
	    chkout_("CONVTB", (ftnlen)6);
	    return 0;
	}

/*        Continue reading lines from the text file and storing them */
/*        in the scratch file until we get to the end marker. We do not */
/*        write the begin and end markers to the scratch file. We do not */
/*        need them. */

	eoc = FALSE_;
	while(! eoc) {
	    ci__1.cierr = 1;
	    ci__1.ciend = 1;
	    ci__1.ciunit = txtlun;
	    ci__1.cifmt = "(A)";
	    iostat = s_rsfe(&ci__1);
	    if (iostat != 0) {
		goto L100003;
	    }
	    iostat = do_fio(&c__1, line, (ftnlen)255);
	    if (iostat != 0) {
		goto L100003;
	    }
	    iostat = e_rsfe();
L100003:
	    if (iostat != 0) {

/*              If there was an error then we need to close the */
/*              scratch file, the text file, and then check out */
/*              and return to the caller. */

		cl__1.cerr = 0;
		cl__1.cunit = scrlun;
		cl__1.csta = 0;
		f_clos(&cl__1);
		cl__1.cerr = 0;
		cl__1.cunit = txtlun;
		cl__1.csta = 0;
		f_clos(&cl__1);
		setmsg_("Error reading the text file: #. IOSTAT = #.", (
			ftnlen)43);
		errch_("#", txtfil, (ftnlen)1, txtfil_len);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
		chkout_("CONVTB", (ftnlen)6);
		return 0;
	    }

/*           If we are not at the end of the comments, then write the */
/*           line ot the scratch file. Otherwise set the end of comments */
/*           flag to .TRUE.. */

	    i__1 = ltrim_(line, (ftnlen)255) - 1;
	    if (s_cmp(line + i__1, "~NAIF/SPC END COMMENTS~", 255 - i__1, (
		    ftnlen)23) != 0) {
		ci__1.cierr = 1;
		ci__1.ciunit = scrlun;
		ci__1.cifmt = "(A)";
		iostat = s_wsfe(&ci__1);
		if (iostat != 0) {
		    goto L100004;
		}
		iostat = do_fio(&c__1, line, rtrim_(line, (ftnlen)255));
		if (iostat != 0) {
		    goto L100004;
		}
		iostat = e_wsfe();
L100004:
		if (iostat != 0) {

/*                 If there was an error then we need to close the */
/*                 scratch file, the text file, and then check out */
/*                 and return to the caller. */

		    cl__1.cerr = 0;
		    cl__1.cunit = scrlun;
		    cl__1.csta = 0;
		    f_clos(&cl__1);
		    cl__1.cerr = 0;
		    cl__1.cunit = txtlun;
		    cl__1.csta = 0;
		    f_clos(&cl__1);
		    setmsg_("Error writing to temporary file. IOSTAT = #.", (
			    ftnlen)44);
		    errint_("#", &iostat, (ftnlen)1);
		    sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
		    chkout_("CONVTB", (ftnlen)6);
		    return 0;
		}
	    } else {
		eoc = TRUE_;
	    }
	}

/*        Open the new binary file and add the comments that have been */
/*        stored temporarily in a scratch file. */

	dafopw_(binfil, &handle, binfil_len);
	if (failed_()) {

/*           If there was an error then we need to close the scratch */
/*           file and the text file, and then check out and return to */
/*           the caller. */

	    cl__1.cerr = 0;
	    cl__1.cunit = scrlun;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    cl__1.cerr = 0;
	    cl__1.cunit = txtlun;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    chkout_("CONVTB", (ftnlen)6);
	    return 0;
	}
	spcac_(&handle, &scrlun, " ", " ", (ftnlen)1, (ftnlen)1);
	if (failed_()) {

/*           If there was an error then we need to close the scratch */
/*           file and the text file, and then check out and return to */
/*           the caller. */

	    cl__1.cerr = 0;
	    cl__1.cunit = scrlun;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    cl__1.cerr = 0;
	    cl__1.cunit = txtlun;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    dafcls_(&handle);
	    chkout_("CONVTB", (ftnlen)6);
	    return 0;
	}

/*        We succeeded, so close the files we opened to deal with the */
/*        comments. The scratch file is automatically deleted. */

	cl__1.cerr = 0;
	cl__1.cunit = scrlun;
	cl__1.csta = 0;
	f_clos(&cl__1);
	dafcls_(&handle);
    }

/*     Close the transfer file. We know it is open, because we got here. */

    cl__1.cerr = 0;
    cl__1.cunit = txtlun;
    cl__1.csta = 0;
    f_clos(&cl__1);
    chkout_("CONVTB", (ftnlen)6);
    return 0;
} /* convtb_ */

