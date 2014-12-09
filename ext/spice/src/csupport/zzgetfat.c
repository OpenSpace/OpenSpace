/* zzgetfat.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure ZZGETFAT ( Get file architecture, type, and unit ) */
/* Subroutine */ int zzgetfat_(char *file, char *arch, char *type__, integer *
	number, ftnlen file_len, ftnlen arch_len, ftnlen type_len)
{
    /* System generated locals */
    cilist ci__1;
    olist o__1;
    cllist cl__1;
    inlist ioin__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), f_inqu(inlist *), f_open(
	    olist *), s_rdue(cilist *), do_uio(integer *, char *, ftnlen), 
	    e_rdue(void), f_clos(cllist *), s_rsfe(cilist *), do_fio(integer *
	    , char *, ftnlen), e_rsfe(void);

    /* Local variables */
    integer i__;
    logical check;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    logical exist;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen), 
	    idw2at_(char *, char *, char *, ftnlen, ftnlen, ftnlen);
    logical opened;
    char idword[12];
    logical diropn;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), getlun_(integer *);
    integer iostat;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    logical seqopn;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen), nextwd_(
	    char *, char *, char *, ftnlen, ftnlen, ftnlen);
    char tmpwrd[12];
    extern logical return_(void);

    /* Fortran I/O blocks */
    static cilist io___8 = { 1, 0, 1, 0, 1 };
    static cilist io___11 = { 1, 0, 1, 0, 1 };


/* $ Abstract */

/*     Determine the file architecture and file type of most SPICE kernel */
/*     files. */

/*     NOTE: This routine is currently for use ONLY with the SPACIT */
/*           and TOBIN utility programs. Use it at your own risk. */

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

/*     KERNEL */
/*     UTILITY */

/* $ Declarations */

/*     The record length should be big enough to hold 128 double */
/*     precision numbers. */

/*     For some environments, record length is measured in longwords, */
/*     since our records are unformatted, with two longwords per double */
/*     precision number. The value of RECL is 256. */

/*     Environment: VAX/VMS, VAX FORTRAN */
/*     Source:      Programming in VAX Fortran */

/*     Environment: Silicon Graphics IRIX OS, SGI FORTRAN 77 */
/*     Source:      NAIF Program */

/*     Environment: DEC Alpha 3000/4000, OSF/1, DEC FORTRAN-77 */
/*     Source:      NAIF Program */

/*     For the following environments, record length is measured in */
/*     characters (bytes) with eight characters per double precision */
/*     number. The value of RECL is 1024. */

/*     Environment: Sun, Sun FORTRAN */
/*     Source:      Sun Fortran Programmer's Guide */

/*     Environment: PC, MS FORTRAN */
/*     Source:      Microsoft Fortran Optimizing Compiler User's Guide */

/*     Environment: Macintosh, Language Systems FORTRAN */
/*     Source:      Language Systems FORTRAN Reference Manual, */
/*                  Version 1.2, page 12-7 */

/*     Environment: PC, Lahey F77 EM/32 Version 4.0 */
/*     Source:      Lahey F77 EM/32 Language Reference Manual, */
/*                  page 144 */

/*     Environment: HP-UX 9000/750, FORTRAN/9000 Series 700 computers */
/*     Source:      FORTRAN/9000 Reference-Series 700 Computers, */
/*                  page 5-110 */

/*     Environment: NeXT/Mach OS, Absoft Fortran */
/*     Source:      NAIF Program */

/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      FILE       I   The name of a file to be examined. */
/*      ARCH       O   The architecture of the kernel file. */
/*      TYPE       O   The type of the kernel file. */
/*      NUMBER     O   The logical unit number for the open file FILE. */

/* $ Detailed_Input */

/*     FILE        is the name of a SPICE kernel file whose architecture */
/*                 and type are desired. This file must be closed when */
/*                 this routine is called. */

/* $ Detailed_Output */

/*     ARCH        is the file architecture of the SPICE kernel file */
/*                 specified be FILE. If the architecture cannot be */
/*                 determined or is not recognized the value '?' is */
/*                 returned. */

/*                 Architectures currently recognized are: */

/*                    DAF - The file is based on the DAF architecture. */
/*                    DAS - The file is based on the DAS architecture. */
/*                    XFR - The file is in a SPICE transfer file format. */
/*                    DEC - The file is an old SPICE decimal text file. */
/*                    ASC -- An ASCII text file. */
/*                    KPL -- Kernel Pool File (i.e., a text kernel) */
/*                    TXT -- An ASCII text file. */
/*                    TE1 -- Text E-Kernel type 1. */
/*                     ?  - The architecture could not be determined. */

/*                 This variable must be at least 3 characters long. */

/*     TYPE        is the type of the SPICE kernel file. If the type */
/*                 can not be determined the value '?' is returned. */

/*                 Kernel file types may be any sequence of at most four */
/*                 printing characters. NAIF has reserved for its use */
/*                 types which contain all upper case letters. */

/*                 A file type of 'PRE' means that the file is a */
/*                 pre-release file. */

/*                 This variable may be at most 4 characters long. */

/*     NUMBER      The logical unit number assigned to the file FILE */
/*                 when opened.  An inyteger, returned to the calling */
/*                 routine. */

/* $ Parameters */

/*     RECL        is the record length of a binary kernel file. Each */
/*                 record must be large enough to hold 128 double */
/*                 precision numbers. The units in which the record */
/*                 length must be specified vary from environment to */
/*                 environment. For example, VAX Fortran requires */
/*                 record lengths to be specified in longwords, */
/*                 where two longwords equal one double precision */
/*                 number. */

/* $ Exceptions */

/*      1)  If the inquire on the filename specified by FILE fails for */
/*          some reason, the error SPICE(INQUIREERROR) will be signalled. */

/*      2)  If the file specified by FILE is already open, the error */
/*          SPICE(FILECURRENTLYOPEN) will be signalled. */

/*      3)  If the file specified by FILE does not exist, the error */
/*          SPICE(NOSUCHFILE) will be signalled. */

/*      4)  If the attempt to open the file specified by FILE fails, the */
/*          error SPICE(FILEOPENFAILED) will be signalled. */

/*      5)  If all attempts to open the file specified by FILE fail, the */
/*          error SPICE(FILEOPENFAILED) will be signalled. */

/*      6)  If all attempts to read from the file specified be FILE */
/*          fail, the error SPICE(FILEREADFAILED) will be signalled. */

/* $ Files */

/*     The SPICE kernel file specified by FILE is opened and then */
/*     closed by this routine to determine its file architecture and */
/*     type. Names of open files should not be passed to this routine. */

/* $ Particulars */

/*     This subroutine is a support utility routine that determines the */
/*     architecture and type of a SPICE kernel file. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     This routine should only be called as part of spacit or tobin */
/*     by spat2b. */

/*     The file to be examined must be closed when this routine is */
/*     invoked. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer  (JPL) */
/*     H.A. Neilan     (JPL) */
/*     W.L. Taber      (JPL) */
/*     E.D. Wright     (JPL) */

/* $ Version */

/* -    Beta Version 1.25.0, 10-MAR-2014 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-INTEL. */

/* -    Beta Version 1.24.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-LINUX-64BIT-IFORT. */

/* -    Beta Version 1.23.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-GFORTRAN. */

/* -    Beta Version 1.22.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GFORTRAN. */

/* -    Beta Version 1.21.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GCC_C. */

/* -    Beta Version 1.20.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    Beta Version 1.19.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-CC_C. */

/* -    Beta Version 1.18.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C. */

/* -    Beta Version 1.17.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    Beta Version 1.16.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-WINDOWS-64BIT-IFORT. */

/* -    Beta Version 1.15.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    Beta Version 1.14.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-64BIT-MS_C. */

/* -    Beta Version 1.13.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-INTEL_C. */

/* -    Beta Version 1.12.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    Beta Version 1.11.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    Beta Version 1.10.0, 18-MAR-2009 (BVS) */

/*        Updated for PC-LINUX-GFORTRAN. */

/* -    Beta Version 1.9.0, 18-MAR-2009 (BVS) */

/*        Updated for MAC-OSX-GFORTRAN. */

/* -    Beta Version 1.8.0, 19-FEB-2008 (BVS) */

/*        Updated for PC-LINUX-IFORT. */

/* -    Beta Version 1.7.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-LINUX-64BIT-GCC_C. */

/* -    Beta Version 1.6.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-INTEL_C. */

/* -    Beta Version 1.5.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-IFORT. */

/* -    Beta Version 1.4.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-WINDOWS-IFORT. */

/* -    Beta Version 1.3.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    Beta Version 1.2.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN_C. */

/* -    Beta Version 1.1.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    Beta Version 1.0.3, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    Beta Version 1.0.2, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    Beta Version 1.0.1, 21-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    Beta Version 1.0.0, 19-MAR-1999 (EDW) */

/*        This routine is a modification of the GETFAT routine. */
/*        Both have the same basic functionality, but this routine */
/*        will ignore all data until a known NAIF file identifier */
/*        is found.  The derivation of file type and architecture */
/*        proceeds as in GETFAT.  Note:  the file is not closed */
/*        on exit. */

/*        The logic for the case architecture = DAF, type = unknown, '?', */
/*        has been removed. */

/* -& */

/* $ Index_Entries */

/*     determine the architecture and type of a kernel file */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     The following parameters point to the various slots in the */
/*     integer portion of the DAF descriptor where the values are */
/*     located. */


/*     These parameters give the number of integer and double precision */
/*     components of the descriptor for SPK and CK files. */


/*     The size of a summary. */


/*     Set the length of a SPICE kernel file ID word. */


/*     Set minimum and maximum values for the range of ASCII printing */
/*     characters. */


/*     Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZGETFAT", (ftnlen)8);
    }

/*     Initialize the temporary storage variables that we use. */

    s_copy(idword, " ", (ftnlen)12, (ftnlen)1);
    seqopn = FALSE_;
    check = TRUE_;

/*     If the filename we got is blank, signal an error and return. */

    if (s_cmp(file, " ", file_len, (ftnlen)1) == 0) {
	setmsg_("The file name is blank.", (ftnlen)23);
	sigerr_("SPICE(BLANKFILENAME)", (ftnlen)20);
	chkout_("ZZGETFAT", (ftnlen)8);
	return 0;
    }

/*     We'll do a bit of inquiring before we try opening anything. */

    ioin__1.inerr = 1;
    ioin__1.infilen = file_len;
    ioin__1.infile = file;
    ioin__1.inex = &exist;
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

/*     Not too likely, but if the INQUIRE statement fails... */

    if (iostat != 0) {
	setmsg_("IOSTAT error in INQUIRE statement. IOSTAT = #.", (ftnlen)46);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(INQUIREERROR)", (ftnlen)19);
	chkout_("ZZGETFAT", (ftnlen)8);
	return 0;
    }

/*     Note: the following two tests MUST be performed in the order in */
/*           which they appear, since in some environments files that do */
/*           not exist are considered to be open. */

/*     By calling this routine, the user implies that the file exists. */

    if (! exist) {
	setmsg_("The kernel file '#' does not exist.", (ftnlen)35);
	errch_("#", file, (ftnlen)1, file_len);
	sigerr_("SPICE(NOSUCHFILE)", (ftnlen)17);
	chkout_("ZZGETFAT", (ftnlen)8);
	return 0;
    }

/*     This routine should not be called if the file is already open. */

    if (opened) {
	setmsg_("The kernel file '#' is already open.", (ftnlen)36);
	errch_("#", file, (ftnlen)1, file_len);
	sigerr_("SPICE(FILECURRENTLYOPEN)", (ftnlen)24);
	chkout_("ZZGETFAT", (ftnlen)8);
	return 0;
    }

/*     Open the file with a record length of RECL (the length of the */
/*     DAF and DAS records). We assume, for now, that opening the file as */
/*     a direct access file will work. */

    diropn = TRUE_;
    getlun_(number);
    o__1.oerr = 1;
    o__1.ounit = *number;
    o__1.ofnmlen = file_len;
    o__1.ofnm = file;
    o__1.orl = 1024;
    o__1.osta = "OLD";
    o__1.oacc = "DIRECT";
    o__1.ofm = 0;
    o__1.oblnk = 0;
    iostat = f_open(&o__1);

/*     If we had trouble opening the file, try opening it as a sequential */
/*     file. */

    if (iostat != 0) {
	diropn = FALSE_;
	o__1.oerr = 1;
	o__1.ounit = *number;
	o__1.ofnmlen = file_len;
	o__1.ofnm = file;
	o__1.orl = 0;
	o__1.osta = "OLD";
	o__1.oacc = "SEQUENTIAL";
	o__1.ofm = 0;
	o__1.oblnk = 0;
	iostat = f_open(&o__1);

/*        If we still have problems opening the file, we don't have a */
/*        clue about the file architecture and type. */

	if (iostat != 0) {
	    s_copy(arch, "?", arch_len, (ftnlen)1);
	    s_copy(type__, "?", type_len, (ftnlen)1);
	    setmsg_("Attempt to open the file '#' failed. IOSTAT = #.", (
		    ftnlen)48);
	    errch_("#", file, (ftnlen)1, file_len);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(FILEOPENFAILED)", (ftnlen)21);
	    chkout_("ZZGETFAT", (ftnlen)8);
	    return 0;
	}
    }

/*     We opened the file successfully, so let's try to read from the */
/*     file. We need to be sure to use the correct form of the read */
/*     statement, depending on whether the file was opened with direct */
/*     acces or sequential access. */

    if (diropn) {
	io___8.ciunit = *number;
	iostat = s_rdue(&io___8);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_uio(&c__1, tmpwrd, (ftnlen)12);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = e_rdue();
L100001:

/*        If we couldn't read from the file as a direct access file with */
/*        a fixed record length, then try to open the file as a */
/*        sequential file and read from it. */

	if (iostat == 0) {
	    seqopn = TRUE_;
	    diropn = FALSE_;
	    cl__1.cerr = 0;
	    cl__1.cunit = *number;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    o__1.oerr = 1;
	    o__1.ounit = *number;
	    o__1.ofnmlen = file_len;
	    o__1.ofnm = file;
	    o__1.orl = 0;
	    o__1.osta = "OLD";
	    o__1.oacc = "SEQUENTIAL";
	    o__1.ofm = 0;
	    o__1.oblnk = 0;
	    iostat = f_open(&o__1);

/*           If we could not open the file, we don't have a clue about */
/*           the file architecture and type. */

	    if (iostat != 0) {
		s_copy(arch, "?", arch_len, (ftnlen)1);
		s_copy(type__, "?", type_len, (ftnlen)1);
		setmsg_("Attempt to open the file '#' failed. IOSTAT = #.", (
			ftnlen)48);
		errch_("#", file, (ftnlen)1, file_len);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEOPENFAILED)", (ftnlen)21);
		chkout_("ZZGETFAT", (ftnlen)8);
		return 0;
	    }

/*           Try to read from the file. */

	    ci__1.cierr = 1;
	    ci__1.ciend = 1;
	    ci__1.ciunit = *number;
	    ci__1.cifmt = "(A)";
	    iostat = s_rsfe(&ci__1);
	    if (iostat != 0) {
		goto L100002;
	    }
	    iostat = do_fio(&c__1, tmpwrd, (ftnlen)12);
	    if (iostat != 0) {
		goto L100002;
	    }
	    iostat = e_rsfe();
L100002:
	    ;
	}
    } else {
	seqopn = TRUE_;
	ci__1.cierr = 1;
	ci__1.ciend = 1;
	ci__1.ciunit = *number;
	ci__1.cifmt = "(A)";
	iostat = s_rsfe(&ci__1);
	if (iostat != 0) {
	    goto L100003;
	}
	iostat = do_fio(&c__1, tmpwrd, (ftnlen)12);
	if (iostat != 0) {
	    goto L100003;
	}
	iostat = e_rsfe();
L100003:
	;
    }

/*     If we had an error while reading, we don't recognize this file. */

    if (iostat != 0) {
	s_copy(arch, "?", arch_len, (ftnlen)1);
	s_copy(type__, "?", type_len, (ftnlen)1);
	cl__1.cerr = 0;
	cl__1.cunit = *number;
	cl__1.csta = 0;
	f_clos(&cl__1);
	setmsg_("Attempt to read from file '#' failed. IOSTAT = #.", (ftnlen)
		49);
	errch_("#", file, (ftnlen)1, file_len);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	chkout_("ZZGETFAT", (ftnlen)8);
	return 0;
    }

/*     Loop until a known NAIF file ID word is found. */

    while(check) {

/*        At this point, we have a candidate for an ID word. To avoid */
/*        difficulties with Fortran I/O and other things, we will now */
/*        replace any non printing ASCII characters with blanks. */

	for (i__ = 1; i__ <= 12; ++i__) {
	    if (*(unsigned char *)&tmpwrd[i__ - 1] < 32 || *(unsigned char *)&
		    tmpwrd[i__ - 1] > 126) {
		*(unsigned char *)&tmpwrd[i__ - 1] = ' ';
	    }
	}

/*        Identify the architecture and type, if we can. */

	ljust_(tmpwrd, tmpwrd, (ftnlen)12, (ftnlen)12);
	ucase_(tmpwrd, tmpwrd, (ftnlen)12, (ftnlen)12);
	nextwd_(tmpwrd, idword, tmpwrd, (ftnlen)12, (ftnlen)12, (ftnlen)12);
	if (s_cmp(idword, "DAFETF", (ftnlen)12, (ftnlen)6) == 0) {

/*           We have a DAF encoded transfer file. */

	    s_copy(arch, "XFR", arch_len, (ftnlen)3);
	    s_copy(type__, "DAF", type_len, (ftnlen)3);
	    check = FALSE_;
	} else if (s_cmp(idword, "DASETF", (ftnlen)12, (ftnlen)6) == 0) {

/*           We have a DAS encoded transfer file. */

	    s_copy(arch, "XFR", arch_len, (ftnlen)3);
	    s_copy(type__, "DAS", type_len, (ftnlen)3);
	    check = FALSE_;
	} else if (s_cmp(idword, "'NAIF/DAF'", (ftnlen)10, (ftnlen)10) == 0) {

/*           We have an old DAF decimal text file. */

	    s_copy(arch, "DEC", arch_len, (ftnlen)3);
	    s_copy(type__, "DAF", type_len, (ftnlen)3);
	    check = FALSE_;
	} else if (s_cmp(idword, "NAIF/DAS", (ftnlen)8, (ftnlen)8) == 0) {

/*           We have a pre release DAS binary file. */

	    s_copy(arch, "DAS", arch_len, (ftnlen)3);
	    s_copy(type__, "PRE", type_len, (ftnlen)3);
	    check = FALSE_;
	} else {

/*           Get the architecture and type from the ID word, if we can. */

	    idw2at_(idword, arch, type__, (ftnlen)8, arch_len, type_len);
	    if (s_cmp(arch, "DAF", arch_len, (ftnlen)3) == 0 && s_cmp(type__, 
		    "?", type_len, (ftnlen)1) == 0) {
		check = FALSE_;
	    } else {

/*              No identification on line.  Read another line. */

		if (seqopn) {
		    ci__1.cierr = 1;
		    ci__1.ciend = 1;
		    ci__1.ciunit = *number;
		    ci__1.cifmt = "(A)";
		    iostat = s_rsfe(&ci__1);
		    if (iostat != 0) {
			goto L100004;
		    }
		    iostat = do_fio(&c__1, tmpwrd, (ftnlen)12);
		    if (iostat != 0) {
			goto L100004;
		    }
		    iostat = e_rsfe();
L100004:
		    ;
		} else {
		    io___11.ciunit = *number;
		    iostat = s_rdue(&io___11);
		    if (iostat != 0) {
			goto L100005;
		    }
		    iostat = do_uio(&c__1, tmpwrd, (ftnlen)12);
		    if (iostat != 0) {
			goto L100005;
		    }
		    iostat = e_rdue();
L100005:
		    ;
		}

/*              If IOSTAT is a negative value, we probably hit an */
/*              end-of-file.  Error out. */

		if (iostat < 0) {
		    s_copy(arch, "?", arch_len, (ftnlen)1);
		    s_copy(type__, "?", type_len, (ftnlen)1);
		    cl__1.cerr = 0;
		    cl__1.cunit = *number;
		    cl__1.csta = 0;
		    f_clos(&cl__1);
		    setmsg_("Encountered end-of-file of # before  finding kn"
			    "own SPICE ID word.", (ftnlen)65);
		    errch_("#", file, (ftnlen)1, file_len);
		    sigerr_("SPICE(ENDOFFILE)", (ftnlen)16);
		    chkout_("ZZGETFAT", (ftnlen)8);
		    return 0;
		}
	    }
	}
    }
    chkout_("ZZGETFAT", (ftnlen)8);
    return 0;
} /* zzgetfat_ */

