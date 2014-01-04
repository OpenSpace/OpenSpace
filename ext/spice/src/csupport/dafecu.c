/* dafecu.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__22 = 22;

/* $Procedure      DAFECU( DAF extract comments to a logical unit ) */
/* Subroutine */ int dafecu_(integer *handle, integer *comlun, logical *
	comnts)
{
    /* System generated locals */
    inlist ioin__1;

    /* Builtin functions */
    integer f_inqu(inlist *);

    /* Local variables */
    extern /* Subroutine */ int dafec_(integer *, integer *, integer *, char *
	    , logical *, ftnlen), chkin_(char *, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int dafsih_(integer *, char *, ftnlen);
    logical opened;
    char combuf[1000*22];
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer numcom;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), writla_(integer *, char *, integer *, ftnlen);
    logical gotsom;
    extern logical return_(void);
    logical eoc;

/* $ Abstract */

/*     Extract comments from a previously opened binary DAF file to a */
/*     previously opened text file attached to a Fortran logical unit. */

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

/*     None. */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*      HANDLE    I   Handle of a DAF file opened with read access. */
/*      COMLUN    I   Logical unit of an opened text file. */
/*      COMNTS    O   Logical flag, indicating comments were found. */

/* $ Detailed_Input */

/*     HANDLE   The file handle for a binary DAF file that has been */
/*              opened with read access. */

/*     COMLUN   The Fortran logical unit of a previously opened text */
/*              file to which the comments from a binary DAF file are */
/*              to be written. */

/*              The comments will be placed into the text file beginning */
/*              at the current location in the file and continuing */
/*              until all of the comments from the comment area of the */
/*              DAF file have been written. */

/* $ Detailed_Output */

/*     COMNTS   A logical flag indicating whether or not any comments */
/*              were found in the comment area of a DAF file. COMNTS will */
/*              have the value .TRUE. if there were some comments, and */
/*              the value .FALSE. otherwise. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)   If the input logical unit COMLUN is not positive or there */
/*          is not an opened file attached to it, the error */
/*          SPICE(INVALIDARGUMENT) will be signalled. */

/*     2)   If the INQUIRE on the logical unit to see if there is a */
/*          file attached fails, the error SPICE(INQUIREFAILED) will */
/*          be signalled. */

/*     3)   If an error occurs while reading from the binary DAF file */
/*          attached to HANDLE, a routine called by this routine will */
/*          signal an error. */

/*     4)   If an error occurs while writing to the text file attached */
/*          to COMLUN, a routine called by this routine will signal an */
/*          error. */

/* $ Files */

/*     See parameters COMLUN and HANDLE in the $ Detailed_Inputs section. */

/* $ Particulars */

/*     This routine will extract all of the comments from the comment */
/*     area of a binary DAF file, placing them into a text file */
/*     attached to COMLUN beginning at the current position in the */
/*     text file. If there are no comments in the DAF file, nothing is */
/*     written to the text file attached to COMLUN. */

/* $ Examples */

/*      Let */

/*         HANDLE   be the DAF file handle of a previously opened binary */
/*                  DAF file. */

/*         COMLUN   be the Fortran logical unit of a previously opened */
/*                  text file that is to accept the comments from the */
/*                  DAF comment area. */

/*      The subroutine call */

/*         CALL DAFECU ( HANDLE, COMLUN, COMNTS ) */

/*      will extract the comments from the comment area of the binary */
/*      DAF file attached to HANDLE, if there are any, and write them */
/*      to the logical unit COMLUN. Upon successful completion, the */
/*      value of COMNTS will be .TRUE. if there were some comments */
/*      in the comment area of the DAF file and .FALSE. otherwise. */

/* $ Restrictions */

/*     The maximum length of a single comment line in the comment area is */
/*     specified by the parameter LINLEN defined below. Currently this */
/*     value is 1000 characters. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer (JPL) */

/* $ Version */

/* -    Beta Version 1.1.1, 08-MAY-2001 (BVS) */

/*        Buffer line size (LINLEN) was increased from 255 to 1000 */
/*        characters to make it consistent the line size in SPC */
/*        routines. */

/* -    Beta Version 1.1.0, 18-JAN-1996 (KRG) */

/*        Added a test and errors for checking to see whether COMLUN */
/*        was actually attached to an ASCII text file when this routine */
/*        was called. */

/* -    Beta Version 1.0.0, 23-SEP-1994 (KRG) */

/* -& */
/* $ Index_Entries */

/*      extract comments from a DAF to a logical unit */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     Set the value for the maximum length of a text line. */


/*     Set the size of the comment buffer. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DAFECU", (ftnlen)6);
    }

/*     Verify that the DAF file attached to HANDLE is opened for reading. */

    dafsih_(handle, "READ", (ftnlen)4);
    if (failed_()) {
	chkout_("DAFECU", (ftnlen)6);
	return 0;
    }

/*     Logical units must be positive. If it is not, signal an error. */

    if (*comlun <= 0) {
	setmsg_("# is not a valid logical unit. Logical units must be positi"
		"ve.", (ftnlen)62);
	errint_("#", comlun, (ftnlen)1);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("DAFECU", (ftnlen)6);
	return 0;
    }

/*     Verify that there is an open ASCII text file attached to COMLUN. */

    ioin__1.inerr = 1;
    ioin__1.inunit = *comlun;
    ioin__1.infile = 0;
    ioin__1.inex = 0;
    ioin__1.inopen = &opened;
    ioin__1.innum = 0;
    ioin__1.innamed = 0;
    ioin__1.inname = 0;
    ioin__1.inacc = 0;
    ioin__1.inseq = 0;
    ioin__1.indir = 0;
    ioin__1.infmt = 0;
    ioin__1.inform = 0;
    ioin__1.inunf = 0;
    ioin__1.inrecl = 0;
    ioin__1.innrec = 0;
    ioin__1.inblank = 0;
    iostat = f_inqu(&ioin__1);
    if (iostat != 0) {
	setmsg_("The INQUIRE on logical unit # failed. The value of IOSTAT w"
		"as #.", (ftnlen)64);
	errint_("#", comlun, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(INQUIREFAILED)", (ftnlen)20);
	chkout_("DAFECU", (ftnlen)6);
	return 0;
    }
    if (! opened) {
	setmsg_("There is no open file attached to logical unit #, so no com"
		"ments could be written.", (ftnlen)82);
	errint_("#", comlun, (ftnlen)1);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("DAFECU", (ftnlen)6);
	return 0;
    }

/*     Initialize some things before the loop. */

    numcom = 0;
    eoc = FALSE_;
    gotsom = FALSE_;
    while(! eoc) {

/*        While we have not reached the end of the comments, get some */
/*        more. */

	dafec_(handle, &c__22, &numcom, combuf, &eoc, (ftnlen)1000);
	if (failed_()) {
	    chkout_("DAFECU", (ftnlen)6);
	    return 0;
	}
	if (numcom > 0) {

/*           If NUMCOM .GT. 0 then we did get some comments, and we need */
/*           to write them out, but first, set the flag indicating that */
/*           we got some comments. */

	    if (! gotsom) {
		gotsom = TRUE_;
	    }
	    writla_(&numcom, combuf, comlun, (ftnlen)1000);
	    if (failed_()) {
		chkout_("DAFECU", (ftnlen)6);
		return 0;
	    }
	}
    }

/*     Set the output flag indicating whether or not we got any comments. */

    *comnts = gotsom;
    chkout_("DAFECU", (ftnlen)6);
    return 0;
} /* dafecu_ */

