/* dasfm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__21 = 21;
static integer c__2 = 2;
static integer c__4 = 4;
static integer c__1 = 1;
static integer c__14 = 14;
static integer c__3 = 3;
static integer c__0 = 0;
static integer c__256 = 256;

/* $Procedure DASFM ( DAS, file manager ) */
/* Subroutine */ int dasfm_0_(int n__, char *fname, char *ftype, char *ifname,
	 integer *handle, integer *unit, integer *free, integer *lastla, 
	integer *lastrc, integer *lastwd, integer *nresvr, integer *nresvc, 
	integer *ncomr, integer *ncomc, integer *fhset, char *access, ftnlen 
	fname_len, ftnlen ftype_len, ftnlen ifname_len, ftnlen access_len)
{
    /* Initialized data */

    static logical pass1 = TRUE_;
    static integer fthead = 0;
    static integer nxthan = 0;
    static integer next[3] = { 2,3,1 };
    static integer prev[3] = { 3,1,2 };
    static integer nw[3] = { 1024,128,256 };
    static char bfflst[8*4] = "BIG-IEEE" "LTL-IEEE" "VAX-GFLT" "VAX-DFLT";

    /* System generated locals */
    address a__1[2];
    integer i__1, i__2, i__3, i__4[2], i__5;
    olist o__1;
    cllist cl__1;
    inlist ioin__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), f_inqu(inlist *), s_rnge(
	    char *, integer, char *, integer), f_open(olist *), f_clos(cllist 
	    *);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rdue(cilist *), do_uio(integer *, char *, ftnlen), e_rdue(void);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer s_wdue(cilist *), e_wdue(void);

    /* Local variables */
    static integer nrec;
    static char tail[932];
    static integer last, pool[54]	/* was [2][27] */, type__;
    extern /* Subroutine */ int zzddhppf_(integer *, integer *, integer *), 
	    zzdasnfr_(integer *, char *, char *, integer *, integer *, 
	    integer *, integer *, char *, ftnlen, ftnlen, ftnlen), zzplatfm_(
	    char *, char *, ftnlen, ftnlen);
    static integer i__, ftacc[21], ldrec[3];
    extern logical elemi_(integer *, integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static integer fthan[21];
    static char tarch[3];
    extern /* Subroutine */ int maxai_(integer *, integer *, integer *, 
	    integer *), errch_(char *, char *, ftnlen, ftnlen), lnkan_(
	    integer *, integer *), ucase_(char *, char *, ftnlen, ftnlen);
    static logical found;
    static integer ftlnk[21];
    extern /* Subroutine */ int copyi_(integer *, integer *);
    extern integer ltrim_(char *, ftnlen);
    static integer ftlun[21];
    extern integer rtrim_(char *, ftnlen);
    static integer ftsum[294]	/* was [14][21] */;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    static char ttype[4];
    extern /* Subroutine */ int idw2at_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen);
    extern logical failed_(void);
    static char dasfil[255];
    static integer endrec, loccch, dirrec[256], loccrc;
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen), 
	    lnknfn_(integer *);
    static char format[8], idword[8], lngmsg[1840], locifn[60], locfmt[8];
    static integer dsctyp, fhlist[27], findex, iostat, ldrmax, locrrc;
    extern integer lnknxt_(integer *, integer *);
    extern logical exists_(char *, ftnlen), return_(void);
    static integer locrch, maxadr, number, curtyp, nxtdir, nxtrec;
    static logical opened;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), lnkini_(integer *, integer *), ssizei_(integer *, 
	    integer *), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen), getlun_(integer *);
    static integer prvtyp;
    extern /* Subroutine */ int lnkilb_(integer *, integer *, integer *), 
	    cleari_(integer *, integer *);
    static char acc[10];
    extern /* Subroutine */ int dasioi_(char *, integer *, integer *, integer 
	    *, ftnlen), insrti_(integer *, integer *);
    static integer bff;
    extern /* Subroutine */ int lnkfsl_(integer *, integer *, integer *), 
	    removi_(integer *, integer *), errfnm_(char *, integer *, ftnlen);
    static integer fnb, loc, new__, pos;

    /* Fortran I/O blocks */
    static cilist io___22 = { 1, 0, 1, 0, 1 };
    static cilist io___48 = { 1, 0, 1, 0, 1 };
    static cilist io___52 = { 1, 0, 0, 0, 1 };
    static cilist io___53 = { 1, 0, 1, 0, 1 };
    static cilist io___55 = { 1, 0, 0, 0, 1 };


/* $ Abstract */

/*     Manage open DAS files. */

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

/*     DAS */
/*     FILES */
/*     UTILITY */

/* $ Declarations */
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


/*     Include File:  SPICELIB Error Handling Parameters */

/*        errhnd.inc  Version 2    18-JUN-1997 (WLT) */

/*           The size of the long error message was */
/*           reduced from 25*80 to 23*80 so that it */
/*           will be accepted by the Microsoft Power Station */
/*           FORTRAN compiler which has an upper bound */
/*           of 1900 for the length of a character string. */

/*        errhnd.inc  Version 1    29-JUL-1997 (NJB) */



/*     Maximum length of the long error message: */


/*     Maximum length of the short error message: */


/*     End Include File:  SPICELIB Error Handling Parameters */

/*     The record length should be big enough to hold the greatest of the */
/*     following: */
/*        -- NWD double precision numbers. */
/*        -- NWI integers. */
/*        -- NWC characters. */
/*     These parameters are named to enhance ease of maintenance of */
/*     the code; the values should not be changed. */
/*     For the following environments, record length is measured in */
/*     characters (bytes) with eight characters per double precision */
/*     number. */
/*     Environment: Sun, Sun FORTRAN */
/*     Source:      Sun Fortran Programmer's Guide */
/*     Environment: PC, MS FORTRAN */
/*     Source:      Microsoft Fortran Optimizing Compiler User's Guide */
/*     Environment: Macintosh, Language Systems FORTRAN */
/*     Source:      Language Systems FORTRAN Reference Manual, */
/*                  Version 1.2, page 12-7 */
/*     Environment: PC/Linux, Fort77 */
/*     Source:      Determined by experiment. */
/*     Environment: PC, Lahey F77 EM/32 Version 4.0 */
/*     Source:      Lahey F77 EM/32 Language Reference Manual, */
/*                  page 144 */
/*     Environment: HP-UX 9000/750, FORTRAN/9000 Series 700 computers */
/*     Source:      FORTRAN/9000 Reference-Series 700 Computers, */
/*                  page 5-110 */
/*     Environment: NeXT Mach OS (Black Hardware), */
/*                  Absoft Fortran Version 3.2 */
/*     Source:      NAIF Program */
/*     FTSIZE is the maximum number of DAS files that a user can have */
/*     open simultaneously. See the description in the $ Parameters */
/*     section for details. */
/* $ Brief_I/O */

/*     Variable  I/O  Entry points */
/*     --------  ---  -------------------------------------------------- */
/*     FNAME     I,O  OPR, OPW, ONW, OPN (Obsolete), HFN, FNH */
/*     FTYPE      I   ONW */
/*     IFNAME     I   ONW, OPN (Obsolete) */
/*     SUM       I,O  UFS, HFS */
/*     HANDLE    I,O  OPR, OPW, ONW, OPN (Obsolete), OPS, LLC, HLU, LUH, */
/*                    HFN, FNH, HAM, SIH */
/*     UNIT      I,O  HLU, LUH */
/*     FREE      I,O  HFS, UFS */
/*     LASTLA    I,O  HFS, UFS */
/*     LASTRC    I,O  HFS, UFS */
/*     LASTWD    I,O  HFS, UFS */
/*     NRESVR     O   HFS */
/*     NRESVC     O   HFS */
/*     NCOMR      O   HFS */
/*     NCOMC      O   HFS */
/*     FHSET      O   HOF */
/*     ACCESS    I,O  SIH, HAM */
/*     RECL       P   OPR, OPW, ONW, OPN (Obsolete) */
/*     FTSIZE     P   OPR, OPW, ONW, OPN (Obsolete), LLC, HLU, LUH, HFN, */
/*                    FNH */

/* $ Detailed_Input */

/*     FNAME       on input is the name of a DAS file to be opened, or */
/*                 the name of a DAS file about which some information */
/*                 (handle, logical unit) is requested. */

/*     FTYPE       on input is a code for the type of data that is */
/*                 contained in the DAS file. This code has no meaning or */
/*                 interpretation at the level of the DAS file */
/*                 architecture, but is provided as a convenience for */
/*                 higher level software. The maximum length for the file */
/*                 type is four (4) characters. If the input string is */
/*                 longer than four characters, the first nonblank */
/*                 character and its three, at most, immediate successors */
/*                 will be used as the file type. The file type may not */
/*                 contain nonprinting characters, and it IS case */
/*                 sensitive. */

/*     IFNAME      is the internal file name for a DAS file to be */
/*                 created. */

/*     HANDLE      on input is the handle of a DAS file about which some */
/*                 information (file name, logical unit) is requested, */
/*                 or the handle of a DAS file to be closed. */

/*     UNIT        on input is the logical unit connected to a DAS file */
/*                 about which some information (file name, handle) is */
/*                 requested. */

/*     FREE        is the Fortran record number of the first free record */
/*                 in a specified DAS file. */

/*     LASTLA      is an array containing the highest current logical */
/*                 addresses, in the specified DAS file, of data of */
/*                 character, double precision, and integer types, in */
/*                 that order. */

/*     LASTRC      is an array containing the Fortran record numbers, in */
/*                 the specified DAS file, of the directory records */
/*                 containing the current last descriptors of clusters */
/*                 of character, double precision, and integer data */
/*                 records, in that order. */

/*     LASTWD      is an array containing the word positions, in the */
/*                 specified DAS file, of the current last descriptors */
/*                 of clusters of character, double precision, and */
/*                 integer data records, in that order. */

/*     ACCESS      is the type of access for which a DAS file is open. */
/*                 The values of ACCESS may be */

/*                    'READ' */
/*                    'WRITE' */

/*                 Leading and trailing blanks are ignored, and case */
/*                 is not significant. */

/*                 DAS files that are open for writing may also be read. */

/* $ Detailed_Output */

/*     FNAME       on output is the name of a DAS file for which */
/*                 the corresponding handle or logical unit has been */
/*                 supplied. */


/*     HANDLE      on output is the handle of a DAS file for which */
/*                 the corresponding file name or logical unit has been */
/*                 supplied. */

/*     UNIT        on output is the logical unit connected to a DAS file */
/*                 for which the corresponding file name or handle has */
/*                 been supplied. */

/*     FREE        is the Fortran record number of the first free record */
/*                 in a specified DAS file. */

/*     LASTLA      is an array containing the highest current logical */
/*                 addresses, in the specified DAS file, of data of */
/*                 character, double precision, and integer types, in */
/*                 that order. */

/*     LASTRC      is an array containing the Fortran record numbers, in */
/*                 the specified DAS file, of the directory records */
/*                 containing the current last descriptors of clusters */
/*                 of character, double precision, and integer data */
/*                 records, in that order. */

/*     LASTWD      is an array containing the word positions, in the */
/*                 specified DAS file, of the current last descriptors */
/*                 of clusters of character, double precision, and */
/*                 integer data records, in that order. */

/*     NRESVR      is the number of reserved records in a specified DAS */
/*                 file. */

/*     NRESVC      is the number of characters in use in the reserved */
/*                 record area of a specified DAS file. */

/*     NCOMR       is the number of comment records in a specified DAS */
/*                 file. */

/*     NCOMC       is the number of characters in use in the comment area */
/*                 of a specified DAS file. */

/*     FHSET       is a SPICELIB set containing the handles of the */
/*                 currently open DAS files. */

/* $ Parameters */

/*     RECL        is the record length of a DAS file. Each record */
/*                 must be large enough to hold the greatest of NWI */
/*                 integers, NWD double precision numbers, or NWC */
/*                 characters, whichever is greater.  The units in which */
/*                 the record length must be specified vary from */
/*                 environment to environment. For example, VAX Fortran */
/*                 requires record lengths to be specified in longwords, */
/*                 where two longwords equal one double precision */
/*                 number. */

/*     FTSIZE      is the maximum number of DAS files that a user can */
/*                 have open simultaneously. This includes any files used */
/*                 by the DAS system when closing files opened with write */
/*                 access. Currently, DASCLS (via DASSDR) opens a scratch */
/*                 DAS file using DASOPS to segregate (sort by data */
/*                 type) the records in the DAS file being closed. */
/*                 Segregating the data by type improves the speed of */
/*                 access to the data. */

/*                 In order to avoid the possibility of overflowing the */
/*                 DAS file table we recommend, when at least one DAS */
/*                 file is open with write access, that users of this */
/*                 software limit themselves to at most FTSIZE - 2  other */
/*                 open DAS files. If no files are to be open with write */
/*                 access, then users may open FTSIZE files with no */
/*                 possibility of overflowing the DAS file table. */

/* $ Exceptions */

/*     1) If DASFM is called directly, the error SPICE(BOGUSENTRY) */
/*        is signaled. */

/*     2) See entry points DASOPR, DASOPW, DASONW, DASOPN, DASOPS, */
/*        DASLLC, DASHFS, DASUFS, DASHLU, DASLUH, DASHFN, DASFNH, DASHOF, */
/*        and DASSIH for exceptions specific to those entry points. */

/* $ Files */

/*     This set of routines is intended to support the creation, */
/*     updating, and reading of Fortran direct access files that */
/*     conform to the DAS file format.  This format is described in */
/*     detail in the DAS Required Reading. */

/*     See FTSIZE in the $ Parameters section for a description of a */
/*     potential problem with overflowing the DAS file table when at */
/*     least one DAS file is opened with write access. */

/* $ Particulars */

/*     DASFM serves as an umbrella, allowing data to be shared by its */
/*     entry points: */

/*        DASOPR         Open for read. */
/*        DASOPW         Open for write. */
/*        DASONW         Open new. */
/*        DASOPN         Open new. (Obsolete: Use DASONW instead.) */
/*        DASOPS         Open as scratch file. */

/*        DASLLC         Low-level close. */

/*        DASHFS         Handle to file summary. */
/*        DASUFS         Update file summary. */

/*        DASHLU         Handle to logical unit. */
/*        DASLUH         Logical to handle. */

/*        DASHFN         Handle to name. */
/*        DASFNH         File name to handle. */

/*        DASHAM         Handle to access method. */

/*        DASHOF         Handles of open files. */
/*        DASSIH         Signal invalid handles. */


/*     Before a DAS file  can be used, it must be opened. Entry points */
/*     DASOPR and DASOPW provide the only means for opening an */
/*     existing DAS file. */

/*     Several files may be opened for use simultaneously. (This makes */
/*     it convenient to combine data from several files to produce a */
/*     single result, or to route subsets of data from a single source */
/*     to multiple DAS files.)  As each DAS file is opened, it is */
/*     assigned a file handle, which is used to keep track of the file */
/*     internally, and which is used by the calling program to refer to */
/*     the file in all subsequent calls to DAS routines. */

/*     DAS files may be opened for either read or write access.  Files */
/*     open for read access may not be changed in any way. Files opened */
/*     for write access may be both read from and written to. */

/*     DASONW is used to open a new DAS file. This routine extends the */
/*     functionality of DASOPN by providing a mechanism for associating a */
/*     type with the data in the DAS file. The use of this entry over */
/*     DASOPN is highly recommended. */

/*     Since the only reason for creating a new file is to write */
/*     something in it, all new files are opened for write access. */

/*     Entry point DASOPN, for opening a new DAS file, has been rendered */
/*     obsolete by the new entry point DASONW. The entry point DASOPN */
/*     will continue to be supported for purposes of backward */
/*     compatibility, but its use in new software development is strongly */
/*     discouraged. */

/*     Entry point DASOPS creates a new scratch DAS file.  As with new */
/*     permanent files, these files are opened for write access.  DAS */
/*     files opened by DASOPS are automatically deleted when they are */
/*     closed. */

/*     Entry point DASLLC is used by DASCLS ( DAS, close file ) to close */
/*     an open DAS file and update DASFM's bookkeeping information */
/*     accordingly.  DASCLS provides the only official means of closing */
/*     a DAS file that is currently open. Closing a DAS file any other */
/*     way (for example, by determining its logical unit and using the */
/*     Fortran CLOSE statement directly) may affect your calling program */
/*     in mysterious ways.  Normally, DASLLC should not be called by */
/*     non-SPICELIB routines; these should call DASCLS instead. */

/*     Entry point DASHFS allows you to obtain a file summary for any */
/*     DAS file that is currently open, without calling DASRFR to */
/*     re-read the file record.  Entry point DASUFS can be used to */
/*     update a file summary at run-time.  Normally, there is no */
/*     need for routines outside of SPICELIB to modify a DAS file's */
/*     summary. */

/*     Entry point DASHAM allows you to determine which access method */
/*     a DAS file has been opened for. */

/*     Entry point DASHOF allows you to determine which DAS files are */
/*     open at any time.  In particular, you can use DASHOF to determine */
/*     whether any file handle points to an open DAS file. */

/*     Entry point DASSIH signals errors when it is supplied with invalid */
/*     handles, so it serves to centralize error handling associated */
/*     with invalid handles. */

/*     The remaining entry points exist mainly to translate between */
/*     alternative representations of DAS files. There are three ways to */
/*     identify any open DAS file: by name, by handle, and by logical */
/*     unit. Given any one of these, you may use these entry points to */
/*     find the other two. */

/* $ Examples */

/*     See entry points DASOPR, DASOPW, DASONW, DASOPN (Obsolete), */
/*     DASLLC, DASHFS, DASUFS, DASHLU, DASLUH, DASHFN, DASFNH, DASHAM, */
/*     DASHOF, and DASSIH for examples specific to those entry points. */

/* $ Restrictions */

/*     1) The value of parameter RECL may need to be changed when DASFM */
/*        and its entry points are ported to a new environment (CPU and */
/*        compiler). */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     K.R. Gehringer  (JPL) */
/*     H.A. Neilan     (JPL) */
/*     W.L. Taber      (JPL) */
/*     F.S. Turner     (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 7.18.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    SPICELIB Version 7.17.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-CC_C. */

/* -    SPICELIB Version 7.16.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C. */

/* -    SPICELIB Version 7.15.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    SPICELIB Version 7.14.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-WINDOWS-64BIT-IFORT. */

/* -    SPICELIB Version 7.13.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 7.12.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-64BIT-MS_C. */

/* -    SPICELIB Version 7.11.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-INTEL_C. */

/* -    SPICELIB Version 7.10.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    SPICELIB Version 7.9.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 7.8.0, 18-MAR-2009 (BVS) */

/*        Updated for PC-LINUX-GFORTRAN. */

/* -    SPICELIB Version 7.7.0, 18-MAR-2009 (BVS) */

/*        Updated for MAC-OSX-GFORTRAN. */

/* -    SPICELIB Version 7.6.0, 19-FEB-2008 (BVS) */

/*        Updated for PC-LINUX-IFORT. */

/* -    SPICELIB Version 7.5.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-LINUX-64BIT-GCC_C. */

/* -    SPICELIB Version 7.4.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-INTEL_C. */

/* -    SPICELIB Version 7.3.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-IFORT. */

/* -    SPICELIB Version 7.2.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-WINDOWS-IFORT. */

/* -    SPICELIB Version 7.1.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    SPICELIB Version 7.0.0, 28-SEP-2005 (NJB) */

/*        Error handling for non-native files was added to */
/*        entry points DASOPR and DASOPW. */

/*        Bug in code for constructing long error message in entry */
/*        point DASUFS was corrected. */

/* -    SPICELIB Version 6.2.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN_C. */

/* -    SPICELIB Version 6.1.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    SPICELIB Version 6.0.3, 24-APR-2003 (EDW) */

/*        Added MAC-OSX-F77 to the list of platforms */
/*        that require READONLY to read write protected */
/*        kernels. */

/* -    SPICELIB Version 6.0.2, 21-FEB-2003 (NJB) */

/*        Corrected inline comment in DASLLC:  determination of */
/*        whether file is open is done by searching the handle column of */
/*        the file table, not the unit column. */

/* -    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 6.0.0, 11-DEC-2001 (NJB) (FST) */

/*        To accomodate future updates to the DAS system, including */
/*        integration with the handle manager and FTP validation */
/*        checks, the following entry points were modified: */

/*           DASONW, DASOPN */

/*        See their headers and code for the details of the changes. */

/*        Bug fix:  removed local buffering of the DAS file ID word */
/*        and the internal file name, as this was causing DASWFR */
/*        to exhibit improper behavior. */

/*        Bug fix:  missing call to CHKIN was added to an error */
/*        handling branch in entry point DASUFS.  This call is */
/*        required because DASUFS uses discovery check-in. */

/* -    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 5.0.0, 05-APR-1998 (NJB) */

/*        Added references to the PC-LINUX environment.  Repaired some */
/*        format errors involving placement of comment markers in */
/*        column 1. */

/* -    SPICELIB Version 4.0.1, 19-DEC-1995 (NJB) */

/*        Added permuted index entry section. */

/* -    SPICELIB Version 4.0.0, 31-AUG-1995 (NJB) */

/*        Changed argument list of the entry point DASONW.  The input */
/*        argument NCOMR, which indicates the number of comment records */
/*        to reserve, was added to the argument list. */

/* -    SPICELIB Version 3.1.0, 5-JAN-1995 (HAN) */

/*        Removed Sun Solaris environment since it is now the same */
/*        as the Sun OS 4.1.x environment. */
/*        Removed DEC Alpha/OpenVMS environment since it is now the same */
/*        as the VAX environment. */
/*        Entry points affected are: DASFM, DASOPR. */

/* -    SPICELIB Version 3.0.0, 15-JUN-1994 (KRG) */

/*        Modified the umbrella routine DASFM to allow the inclusion of */
/*        a file type in the creation and manipulation of DAS files. */

/* -    SPICELIB Version 2.0.0, 11-APR-1994 (HAN) */

/*        Updated module to include values for the Silicon Graphics/IRIX, */
/*        DEC Alpha-OSF/1, and Next/Absoft Fortran platforms. Entry */
/*        points affected are: DASFM, DASOPR. */

/* -    SPICELIB Version 1.0.0, 15-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     manage open DAS files */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 7.0.0, 28-SEP-2005 (NJB) */

/*        Error handling for non-native files was added to */
/*        entry points DASOPR and DASOPW. */

/*        Bug in code for constructing long error message in entry */
/*        point DASUFS was corrected. */

/*        Local variable DAS was renamed to DASFIL in DASSIH. */

/* -    SPICELIB Version 6.0.0, 11-DEC-2001 (NJB) (FST) */

/*        Binary File Format Identification: */

/*        The file record now contains an 8 character string that */
/*        identifies the binary file format utilized by DAS files. */
/*        The purpose of this string's inclusion in the file record */
/*        is preparatory in nature, to accelerate the migration to */
/*        files that support the runtime translation update that */
/*        is scheduled. */

/*        FTP Validation: */

/*        The file record now contains a sequence of characters */
/*        commonly corrupted by improper FTP transfers.  These */
/*        characters will be examined by the handle manager when */
/*        existing files are opened. */

/*        FTIDW and FTIFN have been removed from the elements of */
/*        the DAS file table.  Their presence and use in DASUFS */
/*        was causing DASWFR difficulties in updating the internal */
/*        filename under situations where changes to the comment and */
/*        reserved record parameters in the file record were updated. */
/*        This change effects DASOPR, DASOPN, DASONW, DASOPW, and */
/*        DASUFS. */

/* -    SPICELIB Version 3.0.0, 15-JUN-1994 (KRG) */

/*        Modified the umbrella routine DASFM to allow the inclusion of */
/*        a file type in the creation and manipulation of DAS files. In */
/*        particular, the following changes were made: */

/*           1) Added variable FTYPE to the SUBROUTINE declaration, and */
/*              added appropriate entries for this variable in the */
/*              $Brief_I/O and $ Detailed_Input sections of the header. */

/*           2) Removed erroneous references to OPC from the $ Brief_I/O */
/*              section. */

/*           3) Added a new entry point, DASONW, which will support the */
/*              ability to associate a data type with a new DAS file */
/*              when it is created. The addition of this new entry point */
/*              makes the entry point DASOPN obsolete. */

/*           4) Added a description of the new entry point DASONW to the */
/*              $ Particulars section. Also added a statement that the */
/*              entry point DASOPN has been made obsolete by this new */
/*              entry point, and its use in new code development is */
/*              discouraged. */

/*           5) Added a new variable to the file table, FTIDW, which */
/*              will be used to store the ID words from successfully */
/*              opened DAS files. We need to maintain this information */
/*              when writing the file record, as we do not want to */
/*              modify the ID word in the file. */

/*           6) Removed the parameter DASID as it is no longer needed. */

/*           7) Added new variables TARCH and TTYPE for temporary */
/*              storage of the file architecture and type. Also added a */
/*              new variable FNB for storing the position of the first */
/*              nonblank in a string. */

/*           8) Added new parameters: */

/*                 ARCLEN The maximum length of a file architecture */
/*                 TYPLEN The maximum length of a file type */
/*                 MAXPC  Decimal value for the upper limit of printable */
/*                        ASCII characters. */
/*                 MINPC  Decimal value for the lower limit of printable */
/*                        ASCII characters. */

/*           9) Modified entry points which open DAS files: OPR, OPW, */
/*              OPS, OPN, ONW to support the new file ID word format. */

/*          10) Made all occurrences of error message formatting of */
/*              filenames consistent. All filenames will be single */
/*              quoted in output error messages. */

/*          11) Added a test for a blank filename before the inquire */
/*              to obtain information about a file in the entry points: */
/*              DASOPR, DASOPW, DASONW, and DASOPN. */

/*          12) Modified the description of FTSIZE in the $ Parameters */
/*              section to reflect the possibility of overflowing the */
/*              DAS file table when at least one DAS file had been */
/*              opened with write access. */

/*              The problem occurs when the file table is full, the */
/*              number of open DAS files equals FTSIZE, and at least one */
/*              of the open files was opened with write access. If an */
/*              attempt to close a file opened with write access is made */
/*              under these conditions, by calling DASCLS, it will fail. */
/*              DASCLS (via DASSDR) calls DASOPS to open a scratch DAS */
/*              file, but the scratch file CANNOT be opened because the */
/*              file table is full. If this occurs, close a file open */
/*              for read access, or restrict the number of open files */
/*              in use to be at most FTSIZE - 1 when there will be at */
/*              least one file opened with write access. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Access method parameters: */


/*     File summary parameters: */

/*        A DAS file summary has the following structure: */

/*           +----------------------------------------+ */
/*           | <number of reserved records>           | */
/*           +----------------------------------------+ */
/*           | <number of characters in r.r. area>    | */
/*           +----------------------------------------+ */
/*           | <number of comment records>            | */
/*           +----------------------------------------+ */
/*           | <number of characters in comment area> | */
/*           +----------------------------------------+ */
/*           | <first free record number>             | */
/*           +----------------------------------------+ */
/*           | <last character logical address>       | */
/*           +----------------------------------------+ */
/*           | <last d.p. logical address>            | */
/*           +----------------------------------------+ */
/*           | <last integer logical address>         | */
/*           +----------------------------------------+ */
/*           | <last character descriptor record>     | */
/*           +----------------------------------------+ */
/*           | <last d.p. descriptor record>          | */
/*           +----------------------------------------+ */
/*           | <last integer descriptor record>       | */
/*           +----------------------------------------+ */
/*           | <last character descriptor word>       | */
/*           +----------------------------------------+ */
/*           | <last d.p. descriptor word>            | */
/*           +----------------------------------------+ */
/*           | <last integer descriptor word>         | */
/*           +----------------------------------------+ */


/*     Base indices for: */

/*        -- last logical addresses */
/*        -- records containing last descriptor for a given type */
/*        -- word containing last descriptor for a given type */

/*     The offset into the file summary for any of these items */
/*     is obtained by adding the appropriate data type parameter */
/*     (DP, INT, or CHAR) to the base index for the item. */


/*     Descriptor record pointer locations (within descriptor records): */


/*     Directory address range location parameters: */


/*     First descriptor position in descriptor record: */


/*     Length of the Binary File Format string: */


/*     The parameter TAILEN determines the tail length of a DAS file */
/*     record.  This is the number of bytes (characters) that */
/*     occupy the portion of the file record that follows the */
/*     integer holding the first free address.  For environments */
/*     with a 32 bit word length, 1 byte characters, and DAS */
/*     record sizes of 1024 bytes, we have: */

/*           8 bytes - IDWORD */
/*          60 bytes - IFNAME */
/*           4 bytes - NRESVR (32 bit integer) */
/*           4 bytes - NRESVC (32 bit integer) */
/*           4 bytes - NCOMR  (32 bit integer) */
/*         + 4 bytes - NCOMC  (32 bit integer) */
/*          --------- */
/*          84 bytes - (All file records utilize this space.) */

/*     So the size of the remaining portion (or tail) of the DAS */
/*     file record for computing enviroments as described above */
/*     would be: */

/*        1024 bytes - DAS record size */
/*      -    8 bytes - DAS Binary File Format Word */
/*      -   84 bytes - (from above) */
/*       ------------ */
/*         932 bytes - DAS file record tail length */

/*     Note: environments that do not have a 32 bit word length, */
/*     1 byte characters, and a DAS record size of 1024 bytes, will */
/*     require the adjustment of this parameter. */


/*     Local variables */


/*     The file table consists of a set of arrays which serve as */
/*     `columns' of the table.  The sets of elements having the same */
/*     index in the arrays form the `rows' of the table.  Each column */
/*     contains a particular type of information; each row contains */
/*     all of the information pertaining to a particular DAS file. */

/*     All column names in the file table begin with `FT'.  The */
/*     columns are: */

/*        HAN      Handle */
/*        LUN      Logical unit */
/*        ACC      Access method */
/*        LNK      Number of links */
/*        SUM      File summary */

/*     The rows of the file table are indexed by a doubly linked */
/*     list pool.  The pool contains an active list and a free list. */
/*     when a file is opened, a pointer to the file (the pointer */
/*     is called a `node').  it is placed at the head of the active */
/*     list; when a file is closed, the node in the active list that */
/*     pointed to the file is placed on the free list. */

/*     NEXT is incremented each time a file is opened to become the */
/*     next file handle assigned. */


/*     FTHEAD is a pointer to the head of the active file list. */


/*     NEXT and PREV map the DAS data type codes to their */
/*     successors and predecessors, respectively. */


/*     Length of binary file format name. */


/*     Number of binary file formats. */


/*     Other local variables */


/*     Save everything between calls. */


/*     Initial values */

    /* Parameter adjustments */
    if (lastla) {
	}
    if (lastrc) {
	}
    if (lastwd) {
	}
    if (fhset) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_dasopr;
	case 2: goto L_dasopw;
	case 3: goto L_dasonw;
	case 4: goto L_dasopn;
	case 5: goto L_dasops;
	case 6: goto L_dasllc;
	case 7: goto L_dashfs;
	case 8: goto L_dasufs;
	case 9: goto L_dashlu;
	case 10: goto L_dasluh;
	case 11: goto L_dashfn;
	case 12: goto L_dasfnh;
	case 13: goto L_dashof;
	case 14: goto L_dassih;
	case 15: goto L_dasham;
	}


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASFM", (ftnlen)5);
	sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
	chkout_("DASFM", (ftnlen)5);
    }
    return 0;
/* $Procedure DASOPR ( DAS, open for read ) */

L_dasopr:
/* $ Abstract */

/*     Open a DAS file for reading. */

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

/*     DAS */
/*     FILES */

/* $ Declarations */

/*     CHARACTER*(*)         FNAME */
/*     INTEGER               HANDLE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     FNAME      I   Name of a DAS file to be opened. */
/*     HANDLE     O   Handle assigned to the opened DAS file. */

/* $ Detailed_Input */

/*     FNAME       is the name of a DAS file to be opened with read */
/*                 access. */

/* $ Detailed_Output */

/*     HANDLE      is the handle that is  associated with the file. This */
/*                 handle is used to identify the file in subsequent */
/*                 calls to other DAS routines. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the input filename is blank, the error SPICE(BLANKFILENAME) */
/*        will be signaled. */

/*     2) If the specified file does not exist, the error */
/*        SPICE(FILENOTFOUND) will be signaled. */

/*     3) If the specified file has already been opened for read */
/*        access, the handle already associated with the file is */
/*        returned. */

/*     4) If the specified file has already been opened for write */
/*        access, the error SPICE(DASRWCONFLICT) is signaled. */

/*     5) If the specified file has already been opened by a non-DAS */
/*        routine, the error SPICE(DASIMPROPOPEN) is signaled. */

/*     6) If the specified file cannot be opened without exceeding */
/*        the maximum allowed number of open DAS files, the error */
/*        SPICE(DASFTFULL) is signaled. */

/*     7) If the named file cannot be opened properly, the error */
/*        SPICE(DASOPENFAIL) is signaled. */

/*     8) If the file record cannot be read, the error */
/*        SPICE(FILEREADFAILED) will be signaled. */

/*     9) If the specified file is not a DAS file, as indicated by the */
/*        file's ID word, the error SPICE(NOTADASFILE) is signaled. */

/*    10) If no logical units are available, the error will be */
/*        signaled by routines called by this routine. */

/* $ Files */

/*     See argument FNAME. */

/* $ Particulars */

/*     Most DAS files require only read access. If you do not need to */
/*     change the contents of a file, you should open it using DASOPR. */

/* $ Examples */

/*     1)  Open the existing DAS file TEST.DAS for reading. */

/*            CALL DASOPR ( 'TEST.DAS', HANDLE ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     K.R. Gehringer  (JPL) */
/*     W.L. Taber      (JPL) */
/*     F.S. Turner     (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 7.0.0, 28-SEP-2005 (NJB) */

/*        Error handling for non-native files was added. */

/* -    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 6.0.0, 14-DEC-2001 (FST) */

/*        The DAS file ID word and internal file name are no longer */
/*        buffered by this routine.  See DASFM's Revisions section */
/*        for details. */

/* -    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 3.0.0, 15-JUN-1994 (KRG) */

/*        Modified the entry point to use the new file ID format which */
/*        contains a mnemonic code for the data type.  Added error */
/*        checks on file names.  Fixed bug involving use of sign of */
/*        file handles.  Improved some error messages.  (delete rest) */

/* -    SPICELIB Version 2.0.0, 11-APR-1994 (HAN) */

/*        Updated module to include values for the Silicon Graphics/IRIX, */
/*        DEC Alpha-OSF/1, and Next/Absoft Fortran platforms. Entry */
/*        points affected are: DASFM, DASOPR. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     open a DAS file for reading */
/*     open a DAS file for read access */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 7.0.0, 28-SEP-2005 (NJB) */

/*        Error handling for non-native files was added. */

/* -    SPICELIB Version 3.0.1, 24-APR-2003 (EDW) */

/*        Added MAC-OSX-F77 to the list of platforms */
/*        that require READONLY to read write protected */
/*        kernels. */

/* -    SPICELIB Version 3.0.0, 15-JUN-1994 (KRG) */

/*        Modified the entry point to use the new file ID format which */
/*        contains a mnemonic code for the data type. */

/*        Split an IF ... ELSE IF ... statement into 2 IF statements of */
/*        equivalent behavior to allow testing of the file architecture. */

/*        Added code to test the file architecture and to verify that the */
/*        file is a DAS file. */

/*        Removed the error SPICE(DASNOIDWORD) as it was no longer */
/*        relevant. */

/*        Added the error SPICE(NOTADASFILE) if this routine is called */
/*        with a file that does not contain an ID word identifying the */
/*        file as a DAS file. */

/*        Added a test for a blank filename before attempting to use the */
/*        filename in the routine. If the filename is blank, the error */
/*        SPICE(BLANKFILENAME) will be signaled. */

/*        Fixed a bug when dealing with a read/write open conflict for */
/*        DAS files: the code used the DAF positive/negative handle */
/*        method to determine read/write access rather than the DAS file */
/*        table column FTACC. Replaced the code: */

/*           IF ( FTHAN(FINDEX) .LT. 0 ) THEN */

/*        with */

/*           IF ( FTACC(FINDEX) .EQ. WRITE ) THEN */

/*        Changed the long error message when the error */
/*        SPICE(NOTADASFILE) is signaled to suggest that a common error */
/*        is attempting to use a text version of the desired file rather */
/*        than the binary version. */

/* -    SPICELIB Version 2.0.0, 11-APR-1994 (HAN) */

/*        Updated module to include values for the Silicon Graphics/IRIX, */
/*        DEC Alpha-OSF/1, and Next/Absoft Fortran platforms. Entry */
/*        points affected are: DASFM, DASOPR. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASOPR", (ftnlen)6);
    }

/*     Initialize the file table pool and handle list, if necessary. */

    if (pass1) {
	lnkini_(&c__21, pool);
	ssizei_(&c__21, fhlist);
	pass1 = FALSE_;
    }

/*     Check to see whether the filename is blank. If it is, signal an */
/*     error, check out, and return. */

    if (s_cmp(fname, " ", fname_len, (ftnlen)1) == 0) {
	setmsg_("The file name is blank. ", (ftnlen)24);
	sigerr_("SPICE(BLANKFILENAME)", (ftnlen)20);
	chkout_("DASOPR", (ftnlen)6);
	return 0;
    }

/*     If the file doesn't exist, we can't continue. */

    if (! exists_(fname, rtrim_(fname, fname_len))) {
	setmsg_("The file '#' was not found.", (ftnlen)27);
	errch_("#", fname, (ftnlen)1, fname_len);
	sigerr_("SPICE(FILENOTFOUND)", (ftnlen)19);
	chkout_("DASOPR", (ftnlen)6);
	return 0;
    }

/*     The file may or may not already be open. If so, it should have */
/*     not been opened for writing FTACC .EQ. WRITE. If opened for */
/*     reading, just increment the number of links and return the handle. */
/*     If opened elsewhere, panic. */

    ioin__1.inerr = 0;
    ioin__1.infilen = rtrim_(fname, fname_len);
    ioin__1.infile = fname;
    ioin__1.inex = 0;
    ioin__1.inopen = &opened;
    ioin__1.innum = &number;
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
    f_inqu(&ioin__1);
    if (opened) {

/*        Peruse the `unit' column of the file table; see whether this */
/*        unit is present. */

	findex = fthead;
	found = FALSE_;
	while(! found && findex > 0) {
	    if (ftlun[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
		    "ftlun", i__1, "dasfm_", (ftnlen)1412)] == number) {
		found = TRUE_;
	    } else {
		findex = lnknxt_(&findex, pool);
	    }
	}
	if (found) {
	    if (ftacc[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
		    "ftacc", i__1, "dasfm_", (ftnlen)1422)] == 2) {
		setmsg_("'#' already opened for write access.", (ftnlen)36);
		errch_("#", fname, (ftnlen)1, fname_len);
		sigerr_("SPICE(DASRWCONFLICT)", (ftnlen)20);
		chkout_("DASOPR", (ftnlen)6);
		return 0;
	    } else {

/*              The file is open for read access.  Increment the number */
/*              of links to this file. */

		ftlnk[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
			"ftlnk", i__1, "dasfm_", (ftnlen)1435)] = ftlnk[(i__2 
			= findex - 1) < 21 && 0 <= i__2 ? i__2 : s_rnge("ftl"
			"nk", i__2, "dasfm_", (ftnlen)1435)] + 1;
		*handle = fthan[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 :
			 s_rnge("fthan", i__1, "dasfm_", (ftnlen)1436)];
	    }
	} else {

/*           The file is open, but it wasn't opened by DAS routines. */

	    setmsg_("'#' is already connected to unit #.", (ftnlen)35);
	    errch_("#", fname, (ftnlen)1, fname_len);
	    errint_("#", &number, (ftnlen)1);
	    sigerr_("SPICE(DASIMPROPOPEN)", (ftnlen)20);
	    chkout_("DASOPR", (ftnlen)6);
	    return 0;
	}

/*        If it hasn't been opened, it needs to be, but only if there */
/*        is room for another file. */

    } else if (lnknfn_(pool) == 0) {
	setmsg_("The file table is full, with # entries. Could not open '#'.",
		 (ftnlen)59);
	errint_("#", &c__21, (ftnlen)1);
	errch_("#", fname, (ftnlen)1, fname_len);
	sigerr_("SPICE(DASFTFULL)", (ftnlen)16);
	chkout_("DASOPR", (ftnlen)6);
	return 0;

/*     To open for reading: get a free unit, open the file, get the */
/*     internal file name, and increment the number of links. */

/*     Look out for: */

/*        -- No free logical units. */

/*        -- Error opening the file. */

/*        -- No ID word in the first record. */

    } else {
	getlun_(&number);
	if (failed_()) {
	    chkout_("DASOPR", (ftnlen)6);
	    return 0;
	}
	o__1.oerr = 1;
	o__1.ounit = number;
	o__1.ofnmlen = rtrim_(fname, fname_len);
	o__1.ofnm = fname;
	o__1.orl = 1024;
	o__1.osta = "OLD";
	o__1.oacc = "DIRECT";
	o__1.ofm = 0;
	o__1.oblnk = 0;
	iostat = f_open(&o__1);
	if (iostat != 0) {
	    cl__1.cerr = 0;
	    cl__1.cunit = number;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    setmsg_("Attempt to open file '#' failed. Value of IOSTAT was #.",
		     (ftnlen)55);
	    errch_("#", fname, (ftnlen)1, fname_len);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(DASOPENFAIL)", (ftnlen)18);
	    chkout_("DASOPR", (ftnlen)6);
	    return 0;
	} else {

/*           Try to determine the binary file format of this file. */

	    zzddhppf_(&number, &c__2, &bff);
	    if (failed_()) {
		cl__1.cerr = 0;
		cl__1.cunit = number;
		cl__1.csta = 0;
		f_clos(&cl__1);
		chkout_("DASOPR", (ftnlen)6);
		return 0;
	    }

/*           Find the local binary file format. */

	    zzplatfm_("FILE_FORMAT", locfmt, (ftnlen)11, (ftnlen)8);

/*           Compare binary format to local format.  These must match. */

	    if (bff != isrchc_(locfmt, &c__4, bfflst, (ftnlen)8, (ftnlen)8)) {
		cl__1.cerr = 0;
		cl__1.cunit = number;
		cl__1.csta = 0;
		f_clos(&cl__1);
		s_copy(lngmsg, "File '#' has the non-native binary format #."
			" The SPICE Toolkit does not support reading non-nati"
			"ve files, such as E-kernels, that are based on SPICE"
			"'s DAS architecture. To port a DAS file between plat"
			"forms having incompatible binary formats, for exampl"
			"e big-endian (Sun) vs little-endian (PC), use the SP"
			"ICE utility toxfr to create a transfer format versio"
			"n of the file, then move (ftp) the transfer file in "
			"ASCII mode. You will need to perform line terminator"
			" conversion when moving files between Windows and Un"
			"ix systems if the ASCII mode of ftp is unavailable; "
			"the freeware utilities dos2unix and unix2dos are mea"
			"ns for doing this. Then transform the file to binary"
			" format on the target system using the SPICE utility"
			" tobin. See the SPICE document convert.ug for detail"
			"s on using the SPICE utility programs.", (ftnlen)1840,
			 (ftnlen)810);
		setmsg_(lngmsg, (ftnlen)1840);
		errch_("#", fname, (ftnlen)1, fname_len);
		errch_("#", bfflst + (((i__1 = bff - 1) < 4 && 0 <= i__1 ? 
			i__1 : s_rnge("bfflst", i__1, "dasfm_", (ftnlen)1556))
			 << 3), (ftnlen)1, (ftnlen)8);
		sigerr_("SPICE(NONNATIVEFILE)", (ftnlen)20);
		chkout_("DASOPR", (ftnlen)6);
		return 0;
	    }
	    io___22.ciunit = number;
	    iostat = s_rdue(&io___22);
	    if (iostat != 0) {
		goto L100001;
	    }
	    iostat = do_uio(&c__1, idword, (ftnlen)8);
	    if (iostat != 0) {
		goto L100001;
	    }
	    iostat = do_uio(&c__1, locifn, (ftnlen)60);
	    if (iostat != 0) {
		goto L100001;
	    }
	    iostat = do_uio(&c__1, (char *)&locrrc, (ftnlen)sizeof(integer));
	    if (iostat != 0) {
		goto L100001;
	    }
	    iostat = do_uio(&c__1, (char *)&locrch, (ftnlen)sizeof(integer));
	    if (iostat != 0) {
		goto L100001;
	    }
	    iostat = do_uio(&c__1, (char *)&loccrc, (ftnlen)sizeof(integer));
	    if (iostat != 0) {
		goto L100001;
	    }
	    iostat = do_uio(&c__1, (char *)&loccch, (ftnlen)sizeof(integer));
	    if (iostat != 0) {
		goto L100001;
	    }
	    iostat = e_rdue();
L100001:
	    if (iostat != 0) {
		cl__1.cerr = 0;
		cl__1.cunit = number;
		cl__1.csta = 0;
		f_clos(&cl__1);
		setmsg_("Could not read file record.  File was '#'.  IOSTAT "
			"was #.", (ftnlen)57);
		errch_("#", fname, (ftnlen)1, fname_len);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
		chkout_("DASOPR", (ftnlen)6);
		return 0;
	    }

/*           Check the ID word to see if we have opened a DAS file. First */
/*           separate the ID word into its components and verify that we */
/*           are looking at a DAS file. If we're not, then this routine */
/*           should not be used. */

	    idw2at_(idword, tarch, ttype, (ftnlen)8, (ftnlen)3, (ftnlen)4);
	    if (s_cmp(tarch, "DAS", (ftnlen)3, (ftnlen)3) != 0) {
		cl__1.cerr = 0;
		cl__1.cunit = number;
		cl__1.csta = 0;
		f_clos(&cl__1);
		setmsg_("File '#' is not a DAS file. A common error is attem"
			"pting to open a text version of the file rather than"
			" the binary version of the file.", (ftnlen)135);
		errch_("#", fname, (ftnlen)1, fname_len);
		sigerr_("SPICE(NOTADASFILE)", (ftnlen)18);
		chkout_("DASOPR", (ftnlen)6);
		return 0;
	    }

/*           At this point, we know that we have a valid DAS file, and */
/*           we're set up to read from it, so ... */

/*           Update the file table to include information about */
/*           our newly opened DAS file.  Link the information */
/*           for this file at the head of the file table list. */

/*           Set the output argument HANDLE as well. */

	    lnkan_(pool, &new__);
	    lnkilb_(&new__, &fthead, pool);
	    fthead = new__;
	    ++nxthan;
	    fthan[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("fth"
		    "an", i__1, "dasfm_", (ftnlen)1622)] = nxthan;
	    ftlun[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("ftl"
		    "un", i__1, "dasfm_", (ftnlen)1623)] = number;
	    ftacc[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("fta"
		    "cc", i__1, "dasfm_", (ftnlen)1624)] = 1;
	    ftlnk[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("ftl"
		    "nk", i__1, "dasfm_", (ftnlen)1625)] = 1;

/*           Fill in the file summary.  We already know how many */
/*           reserved records and comment records there are.  To find */
/*           the number of the first free record, the last logical */
/*           address of each type, and the locations of the last */
/*           descriptors of each type, we must examine the directory */
/*           records.  Note that we do not assume that the data records */
/*           in the DAS file have been segregated:  we could be */
/*           restoring a DAS file whose creation was interrupted. */

	    cleari_(&c__14, &ftsum[(i__1 = fthead * 14 - 14) < 294 && 0 <= 
		    i__1 ? i__1 : s_rnge("ftsum", i__1, "dasfm_", (ftnlen)
		    1637)]);
	    ftsum[(i__1 = fthead * 14 - 14) < 294 && 0 <= i__1 ? i__1 : 
		    s_rnge("ftsum", i__1, "dasfm_", (ftnlen)1639)] = locrrc;
	    ftsum[(i__1 = fthead * 14 - 13) < 294 && 0 <= i__1 ? i__1 : 
		    s_rnge("ftsum", i__1, "dasfm_", (ftnlen)1640)] = locrch;
	    ftsum[(i__1 = fthead * 14 - 12) < 294 && 0 <= i__1 ? i__1 : 
		    s_rnge("ftsum", i__1, "dasfm_", (ftnlen)1641)] = loccrc;
	    ftsum[(i__1 = fthead * 14 - 11) < 294 && 0 <= i__1 ? i__1 : 
		    s_rnge("ftsum", i__1, "dasfm_", (ftnlen)1642)] = loccch;

/*           We'll find the values for each data type separately. */

	    for (type__ = 1; type__ <= 3; ++type__) {

/*              The first directory record is located right after the */
/*              last comment record. */

		nrec = locrrc + loccrc + 2;

/*              Keep track of the record number of the last data */
/*              record of the current type. */

		ldrec[(i__1 = type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
			"ldrec", i__1, "dasfm_", (ftnlen)1658)] = 0;

/*              Find the last directory containing a descriptor of a */
/*              record cluster of the current type. */

		dasioi_("READ", &number, &nrec, dirrec, (ftnlen)4);
		maxadr = dirrec[(i__1 = (type__ << 1) + 1) < 256 && 0 <= i__1 
			? i__1 : s_rnge("dirrec", i__1, "dasfm_", (ftnlen)
			1666)];
		nxtdir = dirrec[1];
		while(nxtdir > 0) {

/*                 Read the directory record.  If this record contains */
/*                 descriptors for clusters we're interested in, update */
/*                 the directory record number. */

		    dasioi_("READ", &number, &nxtdir, dirrec, (ftnlen)4);
		    if (dirrec[(i__1 = (type__ << 1) + 1) < 256 && 0 <= i__1 ?
			     i__1 : s_rnge("dirrec", i__1, "dasfm_", (ftnlen)
			    1678)] > 0) {
			maxadr = dirrec[(i__1 = (type__ << 1) + 1) < 256 && 0 
				<= i__1 ? i__1 : s_rnge("dirrec", i__1, "das"
				"fm_", (ftnlen)1679)];
			nrec = nxtdir;
		    }
		    nxtdir = dirrec[1];
		}

/*              At this point, NREC is the record number of the directory */
/*              containing the last descriptor for clusters of TYPE, if */
/*              there are any such descriptors. */

/*              MAXADR is the maximum logical address of TYPE. */

		ftsum[(i__1 = type__ + 5 + fthead * 14 - 15) < 294 && 0 <= 
			i__1 ? i__1 : s_rnge("ftsum", i__1, "dasfm_", (ftnlen)
			1694)] = maxadr;
		if (maxadr > 0) {
		    ftsum[(i__1 = type__ + 8 + fthead * 14 - 15) < 294 && 0 <=
			     i__1 ? i__1 : s_rnge("ftsum", i__1, "dasfm_", (
			    ftnlen)1697)] = nrec;
		} else {
		    ftsum[(i__1 = type__ + 8 + fthead * 14 - 15) < 294 && 0 <=
			     i__1 ? i__1 : s_rnge("ftsum", i__1, "dasfm_", (
			    ftnlen)1699)] = 0;
		}

/*              We still need to set the word location of the final */
/*              descriptor of TYPE, if there are any descriptors of TYPE. */

		if (maxadr > 0) {

/*                 Re-read the directory record containing the last */
/*                 descriptor of the current type. */

		    dasioi_("READ", &number, &nrec, dirrec, (ftnlen)4);

/*                 Traverse the directory record, looking for the last */
/*                 descriptor of TYPE.  We'll keep track of the maximum */
/*                 logical address of TYPE for each cluster of TYPE */
/*                 whose descriptor we examine.  When this value is */
/*                 the maximum logical address of TYPE, we've found */
/*                 the last descriptor of TYPE. */

/*                 Also keep track of the end record numbers for each */
/*                 cluster. */

		    last = dirrec[(i__1 = type__ << 1) < 256 && 0 <= i__1 ? 
			    i__1 : s_rnge("dirrec", i__1, "dasfm_", (ftnlen)
			    1722)] - 1;
		    dsctyp = dirrec[8];
		    prvtyp = prev[(i__1 = dsctyp - 1) < 3 && 0 <= i__1 ? i__1 
			    : s_rnge("prev", i__1, "dasfm_", (ftnlen)1724)];
		    endrec = nrec;
		    pos = 9;
		    while(last < maxadr) {
			++pos;
			if (dirrec[(i__1 = pos - 1) < 256 && 0 <= i__1 ? i__1 
				: s_rnge("dirrec", i__1, "dasfm_", (ftnlen)
				1732)] > 0) {
			    curtyp = next[(i__1 = prvtyp - 1) < 3 && 0 <= 
				    i__1 ? i__1 : s_rnge("next", i__1, "dasf"
				    "m_", (ftnlen)1733)];
			} else {
			    curtyp = prev[(i__1 = prvtyp - 1) < 3 && 0 <= 
				    i__1 ? i__1 : s_rnge("prev", i__1, "dasf"
				    "m_", (ftnlen)1735)];
			}
			if (curtyp == type__) {
			    last += nw[(i__1 = type__ - 1) < 3 && 0 <= i__1 ? 
				    i__1 : s_rnge("nw", i__1, "dasfm_", (
				    ftnlen)1739)] * (i__3 = dirrec[(i__2 = 
				    pos - 1) < 256 && 0 <= i__2 ? i__2 : 
				    s_rnge("dirrec", i__2, "dasfm_", (ftnlen)
				    1739)], abs(i__3));
			}
			endrec += (i__2 = dirrec[(i__1 = pos - 1) < 256 && 0 
				<= i__1 ? i__1 : s_rnge("dirrec", i__1, "das"
				"fm_", (ftnlen)1742)], abs(i__2));
			prvtyp = curtyp;
		    }

/*                 At this point, POS is the word position of the last */
/*                 descriptor of TYPE, and ENDREC is the record number */
/*                 of the last data record of TYPE. */

		    ftsum[(i__1 = type__ + 11 + fthead * 14 - 15) < 294 && 0 
			    <= i__1 ? i__1 : s_rnge("ftsum", i__1, "dasfm_", (
			    ftnlen)1751)] = pos;
		    ldrec[(i__1 = type__ - 1) < 3 && 0 <= i__1 ? i__1 : 
			    s_rnge("ldrec", i__1, "dasfm_", (ftnlen)1752)] = 
			    endrec;
		} else {

/*                 There's no data of TYPE in the file. */

		    ftsum[(i__1 = type__ + 11 + fthead * 14 - 15) < 294 && 0 
			    <= i__1 ? i__1 : s_rnge("ftsum", i__1, "dasfm_", (
			    ftnlen)1759)] = 0;
		    ldrec[(i__1 = type__ - 1) < 3 && 0 <= i__1 ? i__1 : 
			    s_rnge("ldrec", i__1, "dasfm_", (ftnlen)1760)] = 
			    0;
		}
	    }

/*           We're almost done; we need to find the number of the first */
/*           free record.  This record follows all of the data records */
/*           and all of the directory records.  It may happen that the */
/*           last record in use is an empty directory. */

	    maxai_(ldrec, &c__3, &ldrmax, &loc);
	    nrec = locrrc + loccrc + 2;
	    dasioi_("READ", &number, &nrec, dirrec, (ftnlen)4);
	    nxtrec = dirrec[1];
	    while(nxtrec != 0) {
		nrec = nxtrec;
		dasioi_("READ", &number, &nrec, dirrec, (ftnlen)4);
		nxtrec = dirrec[1];
	    }

/*           Now NREC is the last directory record. */

	    ftsum[(i__1 = fthead * 14 - 10) < 294 && 0 <= i__1 ? i__1 : 
		    s_rnge("ftsum", i__1, "dasfm_", (ftnlen)1795)] = max(
		    ldrmax,nrec) + 1;

/*           Insert the new handle into our handle set. */

	    *handle = fthan[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : 
		    s_rnge("fthan", i__1, "dasfm_", (ftnlen)1800)];
	    insrti_(handle, fhlist);
	}
    }
    chkout_("DASOPR", (ftnlen)6);
    return 0;
/* $Procedure DASOPW ( DAS, open for write ) */

L_dasopw:
/* $ Abstract */

/*     Open a DAS file for writing. */

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

/*     DAS */
/*     FILES */

/* $ Declarations */

/*     CHARACTER*(*)         FNAME */
/*     INTEGER               HANDLE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     FNAME      I   Name of a DAS file to be opened. */
/*     HANDLE     O   Handle assigned to the opened DAS file. */

/* $ Detailed_Input */

/*     FNAME       is the name of a DAS file to be opened with write */
/*                 access. */

/* $ Detailed_Output */

/*     HANDLE      is the handle that is associated with the file. This */
/*                 handle is used to identify the file in subsequent */
/*                 calls to other DAS routines. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the input filename is blank, the error SPICE(BLANKFILENAME) */
/*        will be signaled. */

/*     2) If the specified file does not exist, the error */
/*        SPICE(FILENOTFOUND) will be signaled. */

/*     3) If the specified file has already been opened, either by */
/*        the DAS file routines or by other code, the error */
/*        SPICE(DASOPENCONFLICT) is signaled.  Note that this */
/*        response is not paralleled by DASOPR, which allows you */
/*        to open a DAS file for reading even if it is already open for */
/*        reading. */

/*     4) If the specified file cannot be opened without exceeding */
/*        the maximum allowed number of open DAS files, the error */
/*        SPICE(DASFTFULL) is signaled. */

/*     5) If the specified file cannot be opened properly, the error */
/*        SPICE(DASOPENFAIL) is signaled. */

/*     6) If the file record cannot be read, the error */
/*        SPICE(FILEREADFAILED) will be signaled. */

/*     7) If the specified file is not a DAS file, as indicated by the */
/*        file's ID word, the error SPICE(NOTADASFILE) is signaled. */

/*     8) If no logical units are available, the error will be */
/*        signaled by routines called by this routine. */

/* $ Files */

/*     See argument FNAME. */

/* $ Particulars */

/*     Most DAS files require only read access. If you do not need to */
/*     change the contents of a file, you should open it with DASOPR. */

/* $ Examples */

/*     1)  Open the existing DAS file TEST.DAS in order to add data */
/*         to it. */

/*            CALL DASOPW ( 'TEST.DAS', HANDLE ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     K.R. Gehringer  (JPL) */
/*     W.L. Taber      (JPL) */
/*     F.S. Turner     (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 7.0.0, 28-SEP-2005 (NJB) */

/*        Error handling for non-native files was added. */

/* -    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 6.0.0, 14-DEC-2001 (FST) */

/*        The DAS file ID word and internal file name are no longer */
/*        buffered by this routine.  See DASFM's Revisions section */
/*        for details. */

/* -    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 2.0.0, 29-OCT-1993 (KRG) */

/*        Modified the entry point to use the new file ID format which */
/*        contains a mnemonic code for the data type. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     open a DAS file for writing */
/*     open a DAS file for write access */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 7.0.0, 28-SEP-2005 (NJB) */

/*        Error handling for non-native files was added. */

/* -    SPICELIB Version 2.0.0, 29-OCT-1993 (KRG) */

/*        Modified the entry point to use the new file ID format which */
/*        contains a mnemonic code for the data type. */

/*        Split an IF ... ELSE IF ... statement into 2 IF statements of */
/*        equivalent behavior to allow testing of the file architecture. */

/*        Added code to test the file architecture and to verify that the */
/*        file is a DAS file. */

/*        Removed the error SPICE(DASNOIDWORD) as it was no longer */
/*        relevant. */

/*        Added the error SPICE(NOTADASFILE) if this routine is called */
/*        with a file that does not contain an ID word identifying the */
/*        file as a DAF file. */

/*        Added a test for a blank filename before attempting to use the */
/*        filename in the routine. If the filename is blank, the error */
/*        SPICE(BLANKFILENAME) will be signaled. */

/*        Changed the long error message when the error */
/*        SPICE(NOTADASFILE) is signaled to suggest that a common error */
/*        is attempting to load a text version of the desired file rather */
/*        than the binary version. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASOPW", (ftnlen)6);
    }

/*     Initialize the file table pool and handle list, if necessary. */

    if (pass1) {
	lnkini_(&c__21, pool);
	ssizei_(&c__21, fhlist);
	pass1 = FALSE_;
    }

/*     Check to see whether the filename is blank. If it is, signal an */
/*     error, check out, and return. */

    if (s_cmp(fname, " ", fname_len, (ftnlen)1) == 0) {
	setmsg_("The file name is blank. ", (ftnlen)24);
	sigerr_("SPICE(BLANKFILENAME)", (ftnlen)20);
	chkout_("DASOPW", (ftnlen)6);
	return 0;
    }

/*     If the file doesn't exist, we can't continue. */

    if (! exists_(fname, rtrim_(fname, fname_len))) {
	setmsg_("The file '#' was not found.", (ftnlen)27);
	errch_("#", fname, (ftnlen)1, fname_len);
	sigerr_("SPICE(FILENOTFOUND)", (ftnlen)19);
	chkout_("DASOPW", (ftnlen)6);
	return 0;
    }

/*     A file may not be opened for writing if it is already open. */

    ioin__1.inerr = 0;
    ioin__1.infilen = rtrim_(fname, fname_len);
    ioin__1.infile = fname;
    ioin__1.inex = 0;
    ioin__1.inopen = &opened;
    ioin__1.innum = &number;
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
    f_inqu(&ioin__1);
    if (opened) {
	setmsg_("File '#' already opened.", (ftnlen)24);
	errch_("#", fname, (ftnlen)1, fname_len);
	sigerr_("SPICE(DASOPENCONFLICT)", (ftnlen)22);
	chkout_("DASOPW", (ftnlen)6);
	return 0;

/*        If it hasn't been opened, it needs to be, but only if there */
/*        is room for another file. */

    } else if (lnknfn_(pool) == 0) {
	setmsg_("The file table is full, with # entries. Could not open '#'.",
		 (ftnlen)59);
	errint_("#", &c__21, (ftnlen)1);
	errch_("#", fname, (ftnlen)1, fname_len);
	sigerr_("SPICE(DASFTFULL)", (ftnlen)16);
	chkout_("DASOPW", (ftnlen)6);
	return 0;

/*     To open for writing: get a free unit, open the file, get the */
/*     internal file name, and set the number of links to one. */

/*     Look out for: */

/*        -- No free logical units. */

/*        -- Error opening the file. */

    } else {
	getlun_(&number);
	if (failed_()) {
	    chkout_("DASOPW", (ftnlen)6);
	    return 0;
	}
	o__1.oerr = 1;
	o__1.ounit = number;
	o__1.ofnmlen = rtrim_(fname, fname_len);
	o__1.ofnm = fname;
	o__1.orl = 1024;
	o__1.osta = "OLD";
	o__1.oacc = "DIRECT";
	o__1.ofm = 0;
	o__1.oblnk = 0;
	iostat = f_open(&o__1);
	if (iostat != 0) {
	    cl__1.cerr = 0;
	    cl__1.cunit = number;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    setmsg_("Attempt to open file '#' failed. Value of IOSTAT was #.",
		     (ftnlen)55);
	    errch_("#", fname, (ftnlen)1, fname_len);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(DASOPENFAIL)", (ftnlen)18);
	    chkout_("DASOPW", (ftnlen)6);
	    return 0;
	} else {

/*           Try to determine the binary file format of this file. */

	    zzddhppf_(&number, &c__2, &bff);
	    if (failed_()) {
		cl__1.cerr = 0;
		cl__1.cunit = number;
		cl__1.csta = 0;
		f_clos(&cl__1);
		chkout_("DASOPW", (ftnlen)6);
		return 0;
	    }

/*           Find the local binary file format. */

	    zzplatfm_("FILE_FORMAT", locfmt, (ftnlen)11, (ftnlen)8);

/*           Compare binary format to local format.  These must match. */

	    if (bff != isrchc_(locfmt, &c__4, bfflst, (ftnlen)8, (ftnlen)8)) {
		cl__1.cerr = 0;
		cl__1.cunit = number;
		cl__1.csta = 0;
		f_clos(&cl__1);
		s_copy(lngmsg, "File '#' has the non-native binary format #."
			" The SPICE Toolkit does not support writing to non-n"
			"ative files, such as E-kernels, that are based on SP"
			"ICE's DAS architecture. To port a DAS file between p"
			"latforms having incompatible binary formats, for exa"
			"mple big-endian (Sun) vs little-endian (PC), use the"
			" SPICE utility toxfr to create a transfer format ver"
			"sion of the file, then move (ftp) the transfer file "
			"in ASCII mode. You will need to perform line termina"
			"tor conversion when moving files between Windows and"
			" Unix systems if the ASCII mode of ftp is unavailabl"
			"e; the freeware utilities dos2unix and unix2dos are "
			"means for doing this. Then transform the file to bin"
			"ary format on the target system using the SPICE util"
			"ity tobin. See the SPICE document convert.ug for det"
			"ails on using the SPICE utility programs.", (ftnlen)
			1840, (ftnlen)813);
		setmsg_(lngmsg, (ftnlen)1840);
		errch_("#", fname, (ftnlen)1, fname_len);
		errch_("#", bfflst + (((i__1 = bff - 1) < 4 && 0 <= i__1 ? 
			i__1 : s_rnge("bfflst", i__1, "dasfm_", (ftnlen)2199))
			 << 3), (ftnlen)1, (ftnlen)8);
		sigerr_("SPICE(NONNATIVEFILE)", (ftnlen)20);
		chkout_("DASOPW", (ftnlen)6);
		return 0;
	    }

/*           Read the file record. */

	    io___48.ciunit = number;
	    iostat = s_rdue(&io___48);
	    if (iostat != 0) {
		goto L100002;
	    }
	    iostat = do_uio(&c__1, idword, (ftnlen)8);
	    if (iostat != 0) {
		goto L100002;
	    }
	    iostat = do_uio(&c__1, locifn, (ftnlen)60);
	    if (iostat != 0) {
		goto L100002;
	    }
	    iostat = do_uio(&c__1, (char *)&locrrc, (ftnlen)sizeof(integer));
	    if (iostat != 0) {
		goto L100002;
	    }
	    iostat = do_uio(&c__1, (char *)&locrch, (ftnlen)sizeof(integer));
	    if (iostat != 0) {
		goto L100002;
	    }
	    iostat = do_uio(&c__1, (char *)&loccrc, (ftnlen)sizeof(integer));
	    if (iostat != 0) {
		goto L100002;
	    }
	    iostat = do_uio(&c__1, (char *)&loccch, (ftnlen)sizeof(integer));
	    if (iostat != 0) {
		goto L100002;
	    }
	    iostat = e_rdue();
L100002:
	    if (iostat != 0) {
		cl__1.cerr = 0;
		cl__1.cunit = number;
		cl__1.csta = 0;
		f_clos(&cl__1);
		setmsg_("Could not read file record.  File was '#'.  IOSTAT "
			"was #.", (ftnlen)57);
		errch_("#", fname, (ftnlen)1, fname_len);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
		chkout_("DASOPW", (ftnlen)6);
		return 0;
	    }

/*           Check the ID word to see if we have opened a DAS file. First */
/*           separate the ID word into its components and verify that we */
/*           are looking at a DAS file. If we're not, then this routine */
/*           should not be used. */

	    idw2at_(idword, tarch, ttype, (ftnlen)8, (ftnlen)3, (ftnlen)4);
	    if (s_cmp(tarch, "DAS", (ftnlen)3, (ftnlen)3) != 0) {
		cl__1.cerr = 0;
		cl__1.cunit = number;
		cl__1.csta = 0;
		f_clos(&cl__1);
		setmsg_("File '#' is not a DAS file. A common error is attem"
			"pting to open a text version of the file rather than"
			" the binary version of the file.", (ftnlen)135);
		errch_("#", fname, (ftnlen)1, fname_len);
		sigerr_("SPICE(NOTADASFILE)", (ftnlen)18);
		chkout_("DASOPW", (ftnlen)6);
		return 0;
	    }

/*           At this point, we know that we have a valid DAS file, and */
/*           we're set up to read from it, so ... */

/*           Update the file table to include information about */
/*           our newly opened DAS file.  Link the information */
/*           for this file at the head of the file table list. */

/*           Set the output argument HANDLE as well. */

	    lnkan_(pool, &new__);
	    lnkilb_(&new__, &fthead, pool);
	    fthead = new__;
	    ++nxthan;
	    fthan[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("fth"
		    "an", i__1, "dasfm_", (ftnlen)2270)] = nxthan;
	    ftlun[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("ftl"
		    "un", i__1, "dasfm_", (ftnlen)2271)] = number;
	    ftacc[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("fta"
		    "cc", i__1, "dasfm_", (ftnlen)2272)] = 2;
	    ftlnk[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("ftl"
		    "nk", i__1, "dasfm_", (ftnlen)2273)] = 1;

/*           Fill in the file summary.  We already know how many */
/*           reserved records and comment records there are.  To find */
/*           the number of the first free record, the last logical */
/*           address of each type, and the locations of the last */
/*           descriptors of each type, we must examine the directory */
/*           records.  Note that we do not assume that the data records */
/*           in the DAS file have been segregated:  we could be */
/*           restoring a DAS file whose creation was interrupted. */

	    cleari_(&c__14, &ftsum[(i__1 = fthead * 14 - 14) < 294 && 0 <= 
		    i__1 ? i__1 : s_rnge("ftsum", i__1, "dasfm_", (ftnlen)
		    2285)]);
	    ftsum[(i__1 = fthead * 14 - 14) < 294 && 0 <= i__1 ? i__1 : 
		    s_rnge("ftsum", i__1, "dasfm_", (ftnlen)2287)] = locrrc;
	    ftsum[(i__1 = fthead * 14 - 13) < 294 && 0 <= i__1 ? i__1 : 
		    s_rnge("ftsum", i__1, "dasfm_", (ftnlen)2288)] = locrch;
	    ftsum[(i__1 = fthead * 14 - 12) < 294 && 0 <= i__1 ? i__1 : 
		    s_rnge("ftsum", i__1, "dasfm_", (ftnlen)2289)] = loccrc;
	    ftsum[(i__1 = fthead * 14 - 11) < 294 && 0 <= i__1 ? i__1 : 
		    s_rnge("ftsum", i__1, "dasfm_", (ftnlen)2290)] = loccch;

/*           We'll find the values for each data type separately. */

	    for (type__ = 1; type__ <= 3; ++type__) {

/*              The first directory record is located right after the */
/*              last comment record.  The directory may be empty. */

		nrec = locrrc + loccrc + 2;

/*              Keep track of the record number of the last data */
/*              record of the current type. */

		ldrec[(i__1 = type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
			"ldrec", i__1, "dasfm_", (ftnlen)2306)] = 0;

/*              Find the last directory containing a descriptor of a */
/*              record cluster of the current type. */

		dasioi_("READ", &number, &nrec, dirrec, (ftnlen)4);
		maxadr = dirrec[(i__1 = (type__ << 1) + 1) < 256 && 0 <= i__1 
			? i__1 : s_rnge("dirrec", i__1, "dasfm_", (ftnlen)
			2314)];
		nxtdir = dirrec[1];
		while(nxtdir > 0) {

/*                 Read the directory record.  If this record contains */
/*                 descriptors for clusters we're interested in, update */
/*                 the directory record number. */

		    dasioi_("READ", &number, &nxtdir, dirrec, (ftnlen)4);
		    if (dirrec[(i__1 = (type__ << 1) + 1) < 256 && 0 <= i__1 ?
			     i__1 : s_rnge("dirrec", i__1, "dasfm_", (ftnlen)
			    2326)] > 0) {
			maxadr = dirrec[(i__1 = (type__ << 1) + 1) < 256 && 0 
				<= i__1 ? i__1 : s_rnge("dirrec", i__1, "das"
				"fm_", (ftnlen)2327)];
			nrec = nxtdir;
		    }
		    nxtdir = dirrec[1];
		}

/*              At this point, NREC is the record number of the directory */
/*              containing the last descriptor for clusters of TYPE, if */
/*              there are any such descriptors. */

/*              MAXADR is the maximum logical address of TYPE. */

		ftsum[(i__1 = type__ + 5 + fthead * 14 - 15) < 294 && 0 <= 
			i__1 ? i__1 : s_rnge("ftsum", i__1, "dasfm_", (ftnlen)
			2342)] = maxadr;
		if (maxadr > 0) {
		    ftsum[(i__1 = type__ + 8 + fthead * 14 - 15) < 294 && 0 <=
			     i__1 ? i__1 : s_rnge("ftsum", i__1, "dasfm_", (
			    ftnlen)2345)] = nrec;
		} else {
		    ftsum[(i__1 = type__ + 8 + fthead * 14 - 15) < 294 && 0 <=
			     i__1 ? i__1 : s_rnge("ftsum", i__1, "dasfm_", (
			    ftnlen)2347)] = 0;
		}

/*              We still need to set the word location of the final */
/*              descriptor of TYPE, if there are any descriptors of TYPE. */

		if (maxadr > 0) {

/*                 Re-read the directory record containing the last */
/*                 descriptor of the current type. */

		    dasioi_("READ", &number, &nrec, dirrec, (ftnlen)4);

/*                 Traverse the directory record, looking for the last */
/*                 descriptor of TYPE.  We'll keep track of the maximum */
/*                 logical address of TYPE for each cluster of TYPE */
/*                 whose descriptor we examine.  When this value is */
/*                 the maximum logical address of TYPE, we've found */
/*                 the last descriptor of TYPE. */

/*                 Also keep track of the end record numbers for each */
/*                 cluster. */

		    last = dirrec[(i__1 = type__ << 1) < 256 && 0 <= i__1 ? 
			    i__1 : s_rnge("dirrec", i__1, "dasfm_", (ftnlen)
			    2371)] - 1;
		    dsctyp = dirrec[8];
		    prvtyp = prev[(i__1 = dsctyp - 1) < 3 && 0 <= i__1 ? i__1 
			    : s_rnge("prev", i__1, "dasfm_", (ftnlen)2373)];
		    endrec = nrec;
		    pos = 9;
		    while(last < maxadr) {
			++pos;
			if (dirrec[(i__1 = pos - 1) < 256 && 0 <= i__1 ? i__1 
				: s_rnge("dirrec", i__1, "dasfm_", (ftnlen)
				2381)] > 0) {
			    curtyp = next[(i__1 = prvtyp - 1) < 3 && 0 <= 
				    i__1 ? i__1 : s_rnge("next", i__1, "dasf"
				    "m_", (ftnlen)2382)];
			} else {
			    curtyp = prev[(i__1 = prvtyp - 1) < 3 && 0 <= 
				    i__1 ? i__1 : s_rnge("prev", i__1, "dasf"
				    "m_", (ftnlen)2384)];
			}
			if (curtyp == type__) {
			    last += nw[(i__1 = type__ - 1) < 3 && 0 <= i__1 ? 
				    i__1 : s_rnge("nw", i__1, "dasfm_", (
				    ftnlen)2388)] * (i__3 = dirrec[(i__2 = 
				    pos - 1) < 256 && 0 <= i__2 ? i__2 : 
				    s_rnge("dirrec", i__2, "dasfm_", (ftnlen)
				    2388)], abs(i__3));
			}
			endrec += (i__2 = dirrec[(i__1 = pos - 1) < 256 && 0 
				<= i__1 ? i__1 : s_rnge("dirrec", i__1, "das"
				"fm_", (ftnlen)2391)], abs(i__2));
			prvtyp = curtyp;
		    }

/*                 At this point, POS is the word position of the last */
/*                 descriptor of TYPE, and ENDREC is the record number */
/*                 of the last data record of TYPE. */

		    ftsum[(i__1 = type__ + 11 + fthead * 14 - 15) < 294 && 0 
			    <= i__1 ? i__1 : s_rnge("ftsum", i__1, "dasfm_", (
			    ftnlen)2400)] = pos;
		    ldrec[(i__1 = type__ - 1) < 3 && 0 <= i__1 ? i__1 : 
			    s_rnge("ldrec", i__1, "dasfm_", (ftnlen)2401)] = 
			    endrec;
		} else {

/*                 There's no data of TYPE in the file. */

		    ftsum[(i__1 = type__ + 11 + fthead * 14 - 15) < 294 && 0 
			    <= i__1 ? i__1 : s_rnge("ftsum", i__1, "dasfm_", (
			    ftnlen)2407)] = 0;
		    ldrec[(i__1 = type__ - 1) < 3 && 0 <= i__1 ? i__1 : 
			    s_rnge("ldrec", i__1, "dasfm_", (ftnlen)2408)] = 
			    0;
		}
	    }

/*           We're almost done; we need to find the number of the first */
/*           free record.  This record follows all of the data records */
/*           and all of the directory records.  It may happen that the */
/*           last record in use is an empty directory. */

	    maxai_(ldrec, &c__3, &ldrmax, &loc);
	    nrec = locrrc + loccrc + 2;
	    dasioi_("READ", &number, &nrec, dirrec, (ftnlen)4);
	    nxtrec = dirrec[1];
	    while(nxtrec != 0) {
		nrec = nxtrec;
		dasioi_("READ", &number, &nrec, dirrec, (ftnlen)4);
		nxtrec = dirrec[1];
	    }

/*           Now NREC is the last directory record. */

	    ftsum[(i__1 = fthead * 14 - 10) < 294 && 0 <= i__1 ? i__1 : 
		    s_rnge("ftsum", i__1, "dasfm_", (ftnlen)2443)] = max(
		    ldrmax,nrec) + 1;

/*           Insert the new handle into our handle set. */

	    *handle = fthan[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : 
		    s_rnge("fthan", i__1, "dasfm_", (ftnlen)2448)];
	    insrti_(handle, fhlist);
	}
    }
    chkout_("DASOPW", (ftnlen)6);
    return 0;
/* $Procedure DASONW ( DAS, open new file ) */

L_dasonw:
/* $ Abstract */

/*     Open a new DAS file and set the file type. */

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

/*     DAS */
/*     FILES */

/* $ Declarations */

/*     CHARACTER*(*)         FNAME */
/*     CHARACTER*(*)         FTYPE */
/*     CHARACTER*(*)         IFNAME */
/*     INTEGER               NCOMR */
/*     INTEGER               HANDLE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     FNAME      I   Name of a DAS file to be opened. */
/*     FTYPE      I   Mnemonic code for type of data in the DAF file. */
/*     IFNAME     I   Internal file name. */
/*     NCOMR      I   Number of comment records to allocate. */
/*     HANDLE     O   Handle assigned to the opened DAS file. */

/* $ Detailed_Input */

/*     FNAME       is the name of a new DAS file to be created (and */
/*                 consequently opened for write access). */

/*     FTYPE       is a code for type of data placed into a DAS file. */
/*                 The first nonblank character and the three (3), or */
/*                 fewer, characters immediately following it, giving */
/*                 four (4) characters, are used to represent the type of */
/*                 the data placed in the DAF file. This is provided as a */
/*                 convenience for higher level software. It is an error */
/*                 if this string is blank. Also, the file type may not */
/*                 contain any nonprinting characters. When written to */
/*                 the DAS file, the value for the type IS case */
/*                 sensitive. */

/*                 NAIF has reserved for its own use file types */
/*                 consisting of the upper case letters (A-Z) and the */
/*                 digits 0-9. NAIF recommends lower case or mixed case */
/*                 file types be used by all others in order to avoid any */
/*                 conflicts with NAIF file types. */

/*     IFNAME      is the internal file name for the new file.  The name */
/*                 may contain as many as 60 characters.  This should */
/*                 uniquely identify the file. */


/*     NCOMR       is the number of comment records to allocate. */
/*                 Allocating comment records at file creation time may */
/*                 reduce the likelihood of having to expand the */
/*                 comment area later. */

/* $ Detailed_Output */

/*     HANDLE      is the file handle associated with the file. This */
/*                 handle is used to identify the file in subsequent */
/*                 calls to other DAS routines. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the input filename is blank, the error SPICE(BLANKFILENAME) */
/*        is signaled. */

/*     2) If the specified file cannot be opened without exceeding */
/*        the maximum allowed number of open DAS files, the error */
/*        SPICE(DASFTFULL) is signaled.  No file will be created. */

/*     3) If the file cannot be opened properly, the error */
/*        SPICE(DASOPENFAIL) is signaled.  No file will be created. */

/*     4) If the initial records in the file cannot be written, the */
/*        error is diagnosed by routines called by this routine.  No */
/*        file will be created. */

/*     5) If no logical units are available, the error will be */
/*        signaled by routines called by this routine.  No file will be */
/*        created. */

/*     6) If the file type is blank, the error SPICE(BLANKFILETYPE) will */
/*        be signaled. */

/*     7) If the file type contains nonprinitng characters, decimal */
/*        0-31 and 127-255, the error SPICE(ILLEGALCHARACTER) is */
/*        signaled. */

/*     8) If the number of comment records allocated NCOMR is negative, */
/*        the error SPICE(INVALIDCOUNT) is signaled. */

/* $ Files */

/*     See argument FNAME. */

/* $ Particulars */

/*     The DAS files created by this routine have initialized file */
/*     records. */

/*     This entry point creates a new DAS file and sets the type of the */
/*     file to the mnemonic code passed to it. */

/* $ Examples */

/*     1)  Create a new DAS file, using an internal file name that */
/*         attempts to serve as an unique identifier, and give the file a */
/*         type of 'TEST'. */

/*            FNAME  =  'TEST.DAS' */
/*            FTYPE  =  'TEST' */
/*            IFNAME =  'TEST.DAS/NAIF/NJB/11-NOV-1992-20:12:20' */

/*            CALL DASONW ( FNAME, FTYPE, IFNAME, HANDLE ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     K.R. Gehringer  (JPL) */
/*     W.L. Taber      (JPL) */
/*     F.S. Turner     (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 6.0.0, 11-DEC-2001 (FST) */

/*        The DAS file ID word and internal file name are no longer */
/*        buffered by this routine.  See DASFM's Revisions section */
/*        for details. */

/*        The entry point was modified to insert the FTP validation */
/*        string, as well as the binary file format into the file record. */

/* -    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 2.0.0, 31-AUG-1995 (NJB) */

/*        Changed argument list of the entry point DASONW.  The input */
/*        argument NCOMR, which indicates the number of comment records */
/*        to reserve, was added to the argument list. */

/* -    SPICELIB Version 1.0.0, 29-OCT-1993 (KRG) */

/* -& */
/* $ Index_Entries */

/*     open a new DAS file */
/*     open a new DAS file with write access */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 6.0.0, 11-DEC-2001 (NJB) (FST) */

/*        See the Revisions section under DASFM for a discussion of */
/*        the various changes made for this version. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASONW", (ftnlen)6);
    }

/*     Initialize the file table pool and handle list, if necessary. */

    if (pass1) {
	lnkini_(&c__21, pool);
	ssizei_(&c__21, fhlist);
	pass1 = FALSE_;
    }

/*     Check to see whether the filename is blank. If it is, signal an */
/*     error, check out, and return. */

    if (s_cmp(fname, " ", fname_len, (ftnlen)1) == 0) {
	setmsg_("The file name is blank. ", (ftnlen)24);
	sigerr_("SPICE(BLANKFILENAME)", (ftnlen)20);
	chkout_("DASONW", (ftnlen)6);
	return 0;
    }

/*     Check if the file type is blank. */

    if (s_cmp(ftype, " ", ftype_len, (ftnlen)1) == 0) {
	setmsg_("The file type is blank. ", (ftnlen)24);
	sigerr_("SPICE(BLANKFILETYPE)", (ftnlen)20);
	chkout_("DASONW", (ftnlen)6);
	return 0;
    }

/*     Check for nonprinting characters in the file type. */

    fnb = ltrim_(ftype, ftype_len);
    i__1 = rtrim_(ftype, ftype_len);
    for (i__ = fnb; i__ <= i__1; ++i__) {
	if (*(unsigned char *)&ftype[i__ - 1] > 126 || *(unsigned char *)&
		ftype[i__ - 1] < 32) {
	    setmsg_("The file type contains nonprinting characters. ", (
		    ftnlen)47);
	    sigerr_("SPICE(ILLEGALCHARACTER)", (ftnlen)23);
	    chkout_("DASONW", (ftnlen)6);
	    return 0;
	}
    }

/*     Validate the comment record count. */

    if (*ncomr < 0) {
	setmsg_("The number of comment records allocated must be non-negativ"
		"e but was #.", (ftnlen)71);
	errint_("#", ncomr, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("DASONW", (ftnlen)6);
	return 0;
    }

/*     Set the value the file type in a temporary variable to be sure of */
/*     its length and then set the value of the ID word. Only 4 */
/*     characters are allowed for the file type, and they are the first */
/*     nonblank character and its three (3) immediate successors in the */
/*     input string FTYPE. */

    s_copy(ttype, ftype + (fnb - 1), (ftnlen)4, ftype_len - (fnb - 1));
/* Writing concatenation */
    i__4[0] = 4, a__1[0] = "DAS/";
    i__4[1] = 4, a__1[1] = ttype;
    s_cat(idword, a__1, i__4, &c__2, (ftnlen)8);

/*     The file can be opened only if there is room for another file. */

    if (lnknfn_(pool) == 0) {
	setmsg_("The file table is full, with # entries. Could not open '#'.",
		 (ftnlen)59);
	errint_("#", &c__21, (ftnlen)1);
	errch_("#", fname, (ftnlen)1, fname_len);
	sigerr_("SPICE(DASFTFULL)", (ftnlen)16);
	chkout_("DASONW", (ftnlen)6);
	return 0;
    } else {

/*        To open a new file: get a free unit, open the file, write */
/*        the file record, and set the number of links to one. */

/*        Look out for: */

/*           -- No free logical units. */

/*           -- Error opening the file. */

/*           -- Error writing to the file. */

/*        If anything goes wrong after the file has been opened, delete */
/*        the file. */


	getlun_(&number);
	if (failed_()) {
	    chkout_("DASONW", (ftnlen)6);
	    return 0;
	}
	o__1.oerr = 1;
	o__1.ounit = number;
	o__1.ofnmlen = rtrim_(fname, fname_len);
	o__1.ofnm = fname;
	o__1.orl = 1024;
	o__1.osta = "NEW";
	o__1.oacc = "DIRECT";
	o__1.ofm = 0;
	o__1.oblnk = 0;
	iostat = f_open(&o__1);
	if (iostat != 0) {
	    cl__1.cerr = 0;
	    cl__1.cunit = number;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    setmsg_("Attempt to open file '#' failed. Value of IOSTAT was #.",
		     (ftnlen)55);
	    errch_("#", fname, (ftnlen)1, fname_len);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(DASOPENFAIL)", (ftnlen)18);
	    chkout_("DASONW", (ftnlen)6);
	    return 0;
	} else {

/*           Fetch the system file format. */

	    zzplatfm_("FILE_FORMAT", format, (ftnlen)11, (ftnlen)8);

/*           Prepare to write the file record.  Clear out the file */
/*           summary, except for the number of reserved records and */
/*           the free record pointer.  The free record pointer should */
/*           point to the first record AFTER the first directory. */

/*           Use a local variable for the internal file name to ensure */
/*           that IFNLEN characters are written.  The remaining */
/*           elements of the file record are: */

/*              -- the number of reserved records */

/*              -- the number of characters in use in the reserved */
/*                 record area */

/*              -- the number of comment records */

/*              -- the number of characters in use in the comment */
/*                 area */

/*           Initially, all of these counts are zero, except for the */
/*           comment record count, which is set by the caller. */


	    s_copy(locifn, ifname, (ftnlen)60, ifname_len);
	    zzdasnfr_(&number, idword, locifn, &c__0, &c__0, ncomr, &c__0, 
		    format, (ftnlen)8, (ftnlen)60, (ftnlen)8);

/*           Check to see whether or not ZZDASNFR generated an error */
/*           writing the file record to the logical unit.  In the event */
/*           an error occurs, checkout and return. */

	    if (failed_()) {
		chkout_("DASONW", (ftnlen)6);
		return 0;
	    }

/*           Zero out the first directory record in the file.  If this */
/*           write fails, close the file with delete status and return */
/*           immediately.  The first directory record follows the */
/*           comment records and reserved records.  Currently there */
/*           are no reserved records, so the directory occupies record */
/*           NCOMR+2. */

	    cleari_(&c__256, dirrec);
	    i__1 = *ncomr + 2;
	    dasioi_("WRITE", &number, &i__1, dirrec, (ftnlen)5);
	    if (failed_()) {
		cl__1.cerr = 0;
		cl__1.cunit = number;
		cl__1.csta = "DELETE";
		f_clos(&cl__1);
		chkout_("DASONW", (ftnlen)6);
		return 0;
	    }

/*           Update the file table to include information about */
/*           our newly opened DAS file.  Link the information */
/*           for this file at the head of the file table list. */

/*           Set the output argument HANDLE as well. */

	    lnkan_(pool, &new__);
	    lnkilb_(&new__, &fthead, pool);
	    ++nxthan;
	    fthead = new__;
	    cleari_(&c__14, &ftsum[(i__1 = fthead * 14 - 14) < 294 && 0 <= 
		    i__1 ? i__1 : s_rnge("ftsum", i__1, "dasfm_", (ftnlen)
		    2926)]);
	    fthan[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("fth"
		    "an", i__1, "dasfm_", (ftnlen)2928)] = nxthan;
	    ftlun[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("ftl"
		    "un", i__1, "dasfm_", (ftnlen)2929)] = number;
	    ftacc[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("fta"
		    "cc", i__1, "dasfm_", (ftnlen)2930)] = 2;
	    ftlnk[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("ftl"
		    "nk", i__1, "dasfm_", (ftnlen)2931)] = 1;
	    ftsum[(i__1 = fthead * 14 - 10) < 294 && 0 <= i__1 ? i__1 : 
		    s_rnge("ftsum", i__1, "dasfm_", (ftnlen)2932)] = *ncomr + 
		    3;
	    ftsum[(i__1 = fthead * 14 - 12) < 294 && 0 <= i__1 ? i__1 : 
		    s_rnge("ftsum", i__1, "dasfm_", (ftnlen)2933)] = *ncomr;
	    *handle = fthan[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : 
		    s_rnge("fthan", i__1, "dasfm_", (ftnlen)2935)];

/*           Insert the new handle into our handle set. */

	    insrti_(handle, fhlist);
	}
    }
    chkout_("DASONW", (ftnlen)6);
    return 0;
/* $Procedure DASOPN ( DAS, open new ) */

L_dasopn:
/* $ Abstract */

/*     Open a new DAS file for writing. */
/*     Obsolete: This routine has been superceded by DASONW, and it is */
/*     supported for purposes of backward compatibility only. */

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

/*     DAS */
/*     FILES */

/* $ Declarations */

/*     CHARACTER*(*)         FNAME */
/*     CHARACTER*(*)         IFNAME */
/*     INTEGER               HANDLE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     FNAME      I   Name of a DAS file to be opened. */
/*     IFNAME     I   Internal file name. */
/*     HANDLE     O   Handle assigned to the opened DAS file. */

/* $ Detailed_Input */

/*     FNAME       is the name of a new DAS file to be created (and */
/*                 consequently opened for write access). */

/*     IFNAME      is the internal file name for the new file.  The name */
/*                 may contain as many as 60 characters.  This should */
/*                 uniquely identify the file. */

/* $ Detailed_Output */

/*     HANDLE      is the file handle associated with the file. This */
/*                 handle is used to identify the file in subsequent */
/*                 calls to other DAS routines. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the input filename is blank, the error SPICE(BLANKFILENAME) */
/*        will be signaled. */

/*     2) If the specified file cannot be opened without exceeding */
/*        the maximum allowed number of open DAS files, the error */
/*        SPICE(DASFTFULL) is signaled.  No file will be created. */

/*     3) If the file cannot be opened properly, the error */
/*        SPICE(DASOPENFAIL) is signaled.  No file will be created. */

/*     4) If the initial records in the file cannot be written, the */
/*        error is diagnosed by routines called by this routine.  No */
/*        file will be created. */

/*     5) If no logical units are available, the error will be */
/*        signaled by routines called by this routine.  No file will be */
/*        created. */

/* $ Files */

/*     See argument FNAME. */

/* $ Particulars */

/*     The DAS files created by this routine have initialized file */
/*     records. */

/*     This entry point has been made obsolete by the entry point DASONW, */
/*     and it is supported for reasons of backward compatibility only. */
/*     New software development should use the entry point DASONW. */

/* $ Examples */

/*     1)  Create a new DAS file, using an internal file name that */
/*         attempts to serve as an unique identifier. */

/*            FNAME    =  'TEST.DAS' */
/*            IFNAME   =  'TEST.DAS/NAIF/NJB/11-NOV-1992-20:12:20' */

/*            CALL DASOPN ( FNAME, IFNAME, HANDLE ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     K.R. Gehringer  (JPL) */
/*     W.L. Taber      (JPL) */
/*     F.S. Turner     (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 6.0.0, 11-DEC-2001 (FST) */

/*        The DAS file ID word and internal file name are no longer */
/*        buffered by this routine.  See DASFM's Revisions section */
/*        for details. */

/*        This entry point was modified to insert the FTP validation */
/*        string, as well as the binary file format into the file record. */

/* -    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 2.0.0, 29-OCT-1993 (KRG) */

/*        The effect of this routine is unchanged. It still uses the ID */
/*        word 'NAIF/DAS'. This is for backward compatibility only. */

/*        Added statements to the $ Abstract and $ Particulars sections */
/*        to document that this entry is now considered to be obsolete, */
/*        and that it has been superceded by the entry point DASONW. */

/*        Added a test for a blank filename before attempting to use the */
/*        filename in the routine. If the filename is blank, the error */
/*        SPICE(BLANKFILENAME) will be signaled. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     open a new DAS file for writing */
/*     open a new DAS file for write access */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 6.0.0, 11-DEC-2001 (FST) */

/*        See the Revisions section under DASFM for a discussion */
/*        of the changes made for this version. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASOPN", (ftnlen)6);
    }

/*     Initialize the file table pool and handle list, if necessary. */

    if (pass1) {
	lnkini_(&c__21, pool);
	ssizei_(&c__21, fhlist);
	pass1 = FALSE_;
    }

/*     Check to see whether the filename is blank. If it is, signal an */
/*     error, check out, and return. */

    if (s_cmp(fname, " ", fname_len, (ftnlen)1) == 0) {
	setmsg_("The file name is blank. ", (ftnlen)24);
	sigerr_("SPICE(BLANKFILENAME)", (ftnlen)20);
	chkout_("DASOPN", (ftnlen)6);
	return 0;
    }

/*     The file can be opened only if there is room for another file. */

    if (lnknfn_(pool) == 0) {
	setmsg_("The file table is full, with # entries. Could not open '#'.",
		 (ftnlen)59);
	errint_("#", &c__21, (ftnlen)1);
	errch_("#", fname, (ftnlen)1, fname_len);
	sigerr_("SPICE(DASFTFULL)", (ftnlen)16);
	chkout_("DASOPN", (ftnlen)6);
	return 0;
    } else {

/*        To open a new file: get a free unit, open the file, write */
/*        the file record, and set the number of links to one. */

/*        Look out for: */

/*           -- No free logical units. */

/*           -- Error opening the file. */

/*           -- Error writing to the file. */

/*        If anything goes wrong after the file has been opened, delete */
/*        the file. */


	getlun_(&number);
	if (failed_()) {
	    chkout_("DASOPN", (ftnlen)6);
	    return 0;
	}
	o__1.oerr = 1;
	o__1.ounit = number;
	o__1.ofnmlen = rtrim_(fname, fname_len);
	o__1.ofnm = fname;
	o__1.orl = 1024;
	o__1.osta = "NEW";
	o__1.oacc = "DIRECT";
	o__1.ofm = 0;
	o__1.oblnk = 0;
	iostat = f_open(&o__1);
	if (iostat != 0) {
	    cl__1.cerr = 0;
	    cl__1.cunit = number;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    setmsg_("Attempt to open file '#' failed. Value of IOSTAT was #.",
		     (ftnlen)55);
	    errch_("#", fname, (ftnlen)1, fname_len);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(DASOPENFAIL)", (ftnlen)18);
	    chkout_("DASOPN", (ftnlen)6);
	    return 0;
	} else {

/*           Fetch the system file format. */

	    zzplatfm_("FILE_FORMAT", format, (ftnlen)11, (ftnlen)8);

/*           Prepare to write the file record.  Clear out the file */
/*           summary, except for the number of reserved records and */
/*           the free record pointer.  The free record pointer should */
/*           point to the first record AFTER the first directory. */

/*           Use a local variable for the internal file name to ensure */
/*           that IFNLEN characters are written.  The remaining */
/*           elements of the file record are: */

/*              -- the number of reserved records */

/*              -- the number of characters in use in the reserved */
/*                 record area */

/*              -- the number of comment records */

/*              -- the number of characters in use in the comment */
/*                 area */

/*           Initially, all of these counts are zero. */


	    s_copy(locifn, ifname, (ftnlen)60, ifname_len);
	    s_copy(idword, "NAIF/DAS", (ftnlen)8, (ftnlen)8);
	    zzdasnfr_(&number, idword, locifn, &c__0, &c__0, &c__0, &c__0, 
		    format, (ftnlen)8, (ftnlen)60, (ftnlen)8);
	    if (failed_()) {
		chkout_("DASOPN", (ftnlen)6);
		return 0;
	    }

/*           Zero out the first directory record (record #2) in the */
/*           file.  If this write fails, close the file with delete */
/*           status and return immediately. */

	    cleari_(&c__256, dirrec);
	    dasioi_("WRITE", &number, &c__2, dirrec, (ftnlen)5);
	    if (failed_()) {
		cl__1.cerr = 0;
		cl__1.cunit = number;
		cl__1.csta = "DELETE";
		f_clos(&cl__1);
		chkout_("DASOPN", (ftnlen)6);
		return 0;
	    }

/*           Update the file table to include information about */
/*           our newly opened DAS file.  Link the information */
/*           for this file at the head of the file table list. */

/*           Set the output argument HANDLE as well. */

	    lnkan_(pool, &new__);
	    lnkilb_(&new__, &fthead, pool);
	    ++nxthan;
	    fthead = new__;
	    cleari_(&c__14, &ftsum[(i__1 = fthead * 14 - 14) < 294 && 0 <= 
		    i__1 ? i__1 : s_rnge("ftsum", i__1, "dasfm_", (ftnlen)
		    3324)]);
	    fthan[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("fth"
		    "an", i__1, "dasfm_", (ftnlen)3326)] = nxthan;
	    ftlun[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("ftl"
		    "un", i__1, "dasfm_", (ftnlen)3327)] = number;
	    ftacc[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("fta"
		    "cc", i__1, "dasfm_", (ftnlen)3328)] = 2;
	    ftlnk[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("ftl"
		    "nk", i__1, "dasfm_", (ftnlen)3329)] = 1;
	    ftsum[(i__1 = fthead * 14 - 10) < 294 && 0 <= i__1 ? i__1 : 
		    s_rnge("ftsum", i__1, "dasfm_", (ftnlen)3330)] = 3;
	    *handle = fthan[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : 
		    s_rnge("fthan", i__1, "dasfm_", (ftnlen)3332)];

/*           Insert the new handle into our handle set. */

	    insrti_(handle, fhlist);
	}
    }
    chkout_("DASOPN", (ftnlen)6);
    return 0;
/* $Procedure DASOPS ( DAS, open scratch ) */

L_dasops:
/* $ Abstract */

/*     Open a scratch DAS file for writing. */

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

/*     DAS */
/*     FILES */
/*     UTILITY */

/* $ Declarations */

/*     INTEGER               HANDLE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     O   Handle assigned to a scratch DAS file. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     HANDLE      is the file handle associated with the scratch file */
/*                 opened by this routine.  This handle is used to */
/*                 identify the file in subsequent calls to other DAS */
/*                 routines. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the specified file cannot be opened without exceeding */
/*        the maximum allowed number of open DAS files, the error */
/*        SPICE(DASFTFULL) is signaled.  No file will be created. */

/*     2) If file cannot be opened properly, the error */
/*        SPICE(DASOPENFAIL) is signaled.  No file will be created. */

/*     3) If the initial records in the file cannot be written, the */
/*        error SPICE(DASWRITEFAIL) is signaled.  No file will be */
/*        created. */

/*     4) If no logical units are available, the error will be */
/*        signaled by routines called by this routine.  No file will be */
/*        created. */

/* $ Files */

/*     See output argument HANDLE. */

/*     See FTSIZE in the $ Parameters section for a description of a */
/*     potential problem with overflowing the DAS file table when at */
/*     least one DAS file is opened with write access. */

/* $ Particulars */

/*     This routine is a utility used by the DAS system to provide */
/*     work space needed when creating new DAS files. */

/*     The DAS files created by this routine have initialized file */
/*     records. The file type for a DAS scratch file is 'SCR ', so the */
/*     file type 'SCR ' is not available for general use. */

/* $ Examples */

/*     1)  Create a scratch DAS file to use as a temporary storage */
/*         area. */

/*            CALL DASOPS ( HANDLE ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     K.R. Gehringer  (JPL) */
/*     W.L. Taber      (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 2.0.0, 29-OCT-1993 (KRG) */

/*        Modified the entry point to use the new file ID format which */
/*        contains a mnemonic code for the data type. */

/*        Put meaningful values into the type and internal filename */
/*        for a DAS scratch file, rather than leaving them blank. */

/*        Documented the potential problem of overflowing the DAS file */
/*        table when attempting to close a DAS file opened with write */
/*        access when the file table is full. Modified the long error */
/*        message to indicate this as a cause of the problem. */

/* -    SPICELIB Version 1.1.0, 04-MAY-1993 (NJB) */

/*        Bug fix:  removed file name variable from error message. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     open a scratch DAS file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 29-OCT-1993 (KRG) */

/*        Modified the entry point to use the new file ID format which */
/*        contains a mnemonic code for the data type. */

/*        DAS scratch files use the type 'SCR ', so the ID word for a DAS */
/*        scratch file would be: 'DAS/SCR ' */

/*        Changed the internal fielname from blank to the string: */

/*           'DAS SCRATCH FILE' */

/*        It's probably better to have something written there than */
/*        nothing. */

/*        Documented the potential problem of overflowing the DAS file */
/*        table when attempting to close a DAS file opened with write */
/*        access when the file table is full. Modified the long error */
/*        message to indicate this as a cause of the problem. */

/*        The problem occurs when the file table is full, the number of */
/*        open DAS files equals FTSIZE, and at least one of the open */
/*        files was opened with write access. If an attempt to close a */
/*        file opened with write access is made under these conditions, */
/*        by calling DASCLS, it will fail. DASCLS (via DASSDR) calls */
/*        DASOPS to open a scratch DAS file, but the scratch file CANNOT */
/*        be opened because the file table is full. If this occurs, close */
/*        a file open for read access, or restrict the number of open */
/*        files in use to be at most FTSIZE - 1 when there will be at */
/*        least one file opened with write access. */

/* -    SPICELIB Version 1.1.0, 04-MAY-1993 (NJB) */

/*        Bug fix:  removed unneeded file name variable FNAME from */
/*        error message. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASOPS", (ftnlen)6);
    }

/*     Initialize the file table pool and handle list, if necessary. */

    if (pass1) {
	lnkini_(&c__21, pool);
	ssizei_(&c__21, fhlist);
	pass1 = FALSE_;
    }

/*     The file can be opened only if there is room for another file. */

    if (lnknfn_(pool) == 0) {
	setmsg_("The file table is full, with # entries. Could not open a sc"
		"ratch file. If a call to DASOPS was not made and this error "
		"occurred, it is likely that the DAS file table was full and "
		"an attempt to close a file opened with write access was made"
		". See the DAS required reading and DASFM for details.", (
		ftnlen)292);
	errint_("#", &c__21, (ftnlen)1);
	sigerr_("SPICE(DASFTFULL)", (ftnlen)16);
	chkout_("DASOPS", (ftnlen)6);
	return 0;
    } else {

/*        To open a new file: get a free unit, open the file, write */
/*        the file record, and set the number of links to one. */

/*        Look out for: */

/*           -- No free logical units. */

/*           -- Error opening the file. */

/*           -- Error writing to the file. */

/*        If anything goes wrong after the file has been opened, delete */
/*        the file. */


	getlun_(&number);
	if (failed_()) {
	    chkout_("DASOPS", (ftnlen)6);
	    return 0;
	}
	o__1.oerr = 1;
	o__1.ounit = number;
	o__1.ofnm = 0;
	o__1.orl = 1024;
	o__1.osta = "SCRATCH";
	o__1.oacc = "DIRECT";
	o__1.ofm = 0;
	o__1.oblnk = 0;
	iostat = f_open(&o__1);
	if (iostat != 0) {
	    cl__1.cerr = 0;
	    cl__1.cunit = number;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    setmsg_("Attempt to open scratch file failed.  IOSTAT was #.", (
		    ftnlen)51);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(DASOPENFAIL)", (ftnlen)18);
	    chkout_("DASOPS", (ftnlen)6);
	    return 0;
	} else {

/*           Prepare to write the file record.  Clear out the file */
/*           summary, the free record pointer.  The free record pointer */
/*           should point to the first record AFTER the first directory. */

	    s_copy(locifn, "DAS SCRATCH FILE", (ftnlen)60, (ftnlen)16);
	    s_copy(idword, "DAS/SCR ", (ftnlen)8, (ftnlen)8);
	    io___52.ciunit = number;
	    iostat = s_wdue(&io___52);
	    if (iostat != 0) {
		goto L100003;
	    }
	    iostat = do_uio(&c__1, idword, (ftnlen)8);
	    if (iostat != 0) {
		goto L100003;
	    }
	    iostat = do_uio(&c__1, locifn, (ftnlen)60);
	    if (iostat != 0) {
		goto L100003;
	    }
	    iostat = do_uio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
	    if (iostat != 0) {
		goto L100003;
	    }
	    iostat = do_uio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
	    if (iostat != 0) {
		goto L100003;
	    }
	    iostat = do_uio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
	    if (iostat != 0) {
		goto L100003;
	    }
	    iostat = do_uio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
	    if (iostat != 0) {
		goto L100003;
	    }
	    iostat = e_wdue();
L100003:
	    if (iostat != 0) {
		cl__1.cerr = 0;
		cl__1.cunit = number;
		cl__1.csta = "DELETE";
		f_clos(&cl__1);
		setmsg_("Attempt to write scratch file failed. Value of IOST"
			"AT was #.", (ftnlen)60);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(DASWRITEFAIL)", (ftnlen)19);
		chkout_("DASOPS", (ftnlen)6);
		return 0;
	    } else {

/*              Update the file table to include information about */
/*              our newly opened DAS file.  Link the information */
/*              for this file at the head of the file table list. */

/*              Set the output argument HANDLE as well. */

		lnkan_(pool, &new__);
		lnkilb_(&new__, &fthead, pool);
		++nxthan;
		fthead = new__;
		cleari_(&c__14, &ftsum[(i__1 = fthead * 14 - 14) < 294 && 0 <=
			 i__1 ? i__1 : s_rnge("ftsum", i__1, "dasfm_", (
			ftnlen)3690)]);
		fthan[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
			"fthan", i__1, "dasfm_", (ftnlen)3692)] = nxthan;
		ftlun[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
			"ftlun", i__1, "dasfm_", (ftnlen)3693)] = number;
		ftacc[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
			"ftacc", i__1, "dasfm_", (ftnlen)3694)] = 2;
		ftlnk[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
			"ftlnk", i__1, "dasfm_", (ftnlen)3695)] = 1;
		ftsum[(i__1 = fthead * 14 - 10) < 294 && 0 <= i__1 ? i__1 : 
			s_rnge("ftsum", i__1, "dasfm_", (ftnlen)3696)] = 3;
		*handle = fthan[(i__1 = fthead - 1) < 21 && 0 <= i__1 ? i__1 :
			 s_rnge("fthan", i__1, "dasfm_", (ftnlen)3698)];

/*              Insert the new handle into our handle set. */

		insrti_(handle, fhlist);
	    }
	}
    }
    chkout_("DASOPS", (ftnlen)6);
    return 0;
/* $Procedure DASLLC ( DAS, low-level close ) */

L_dasllc:
/* $ Abstract */

/*     Close the DAS file associated with a given handle. */

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

/*     DAS */
/*     FILES */

/* $ Declarations */

/*     INTEGER               HANDLE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of a DAS file to be closed. */

/* $ Detailed_Input */

/*     HANDLE      is the handle of a previously opened DAS file. */

/* $ Detailed_Output */

/*     None.  See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the specified handle does not belong to a DAS file */
/*        that is currently open, nothing happens. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     Normally, routines outside of SPICELIB will not need to call this */
/*     routine.  Application programs should close DAS files by calling */
/*     the SPICELIB routine DASCLS.  This routine is a lower-level */
/*     routine that is called by DASCLS, but (obviously) does not have */
/*     the full functionality of DASCLS. */

/*     This routine closes a DAS file and updates DASFM's bookkeeping */
/*     information on open DAS files.  Because DASFM and its entry */
/*     points must keep track of what files are open at any given time, */
/*     it is important that DAS files be closed only with DASCLS or */
/*     DASLLC, to prevent the remaining DAS routines from failing, */
/*     sometimes mysteriously. */

/*     Note that when a file is opened more than once for read or write */
/*     access, DASOPR returns the same handle each time it is re-opened. */
/*     Each time the file is closed, DASLLC checks to see if any other */
/*     claims on the file are still active before physically closing */
/*     the file. */

/*     Unlike DASCLS, this routine does not force a write of updated, */
/*     buffered records to the indicated file, nor does it segregate the */
/*     data records in the file. */

/* $ Examples */

/*     1)  Here's how DASCLS uses this routine: */


/*            C */
/*            C     If the file is open for writing, flush any buffered */
/*            C     records that belong to it. */
/*            C */
/*                  CALL DASHAM ( HANDLE, METHOD ) */

/*                  IF ( METHOD .EQ. WRITE ) THEN */

/*                     Make sure that all updated, buffered records are */
/*                     written out to the indicated file. */

/*                     CALL DASWUR ( HANDLE ) */

/*                     Segregate the data records in the file according */
/*                     to data type. */

/*                     CALL DASSDR ( HANDLE ) */

/*                  END IF */

/*            C */
/*            C     Close the file. */
/*            C */
/*                  CALL DASLLC ( HANDLE ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     K.R. Gehringer  (JPL) */
/*     W.L. Taber      (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 6.0.2, 21-FEB-2003 (NJB) */

/*        Corrected inline comment:  determination of whether file */
/*        is open is done by searching the handle column of the file */
/*        table, not the unit column. */

/* -    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if these open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     close a DAS file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if these open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASLLC", (ftnlen)6);
    }

/*     Initialize the file table pool and handle list, if necessary. */

    if (pass1) {
	lnkini_(&c__21, pool);
	ssizei_(&c__21, fhlist);
	pass1 = FALSE_;
    }

/*     Is this file even open?  Peruse the `handle' column of the file */
/*     table; see whether this handle is present. */

    findex = fthead;
    found = FALSE_;
    while(! found && findex > 0) {
	if (fthan[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("fth"
		"an", i__1, "dasfm_", (ftnlen)3956)] == *handle) {
	    found = TRUE_;
	} else {
	    findex = lnknxt_(&findex, pool);
	}
    }

/*     If the file is not open: no harm, no foul.  Otherwise, decrement */
/*     the number of links to the file.  If the number of links drops to */
/*     zero, physically close the file and remove it from the file */
/*     buffer. */

    if (found) {
	ftlnk[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("ftlnk", 
		i__1, "dasfm_", (ftnlen)3972)] = ftlnk[(i__2 = findex - 1) < 
		21 && 0 <= i__2 ? i__2 : s_rnge("ftlnk", i__2, "dasfm_", (
		ftnlen)3972)] - 1;
	if (ftlnk[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("ftl"
		"nk", i__1, "dasfm_", (ftnlen)3974)] == 0) {

/*           Close this file and delete it from the active list. */
/*           If this was the head node of the list, the head node */
/*           becomes the successor of this node (which may be NIL). */
/*           Delete the handle from our handle set. */

	    cl__1.cerr = 0;
	    cl__1.cunit = ftlun[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 :
		     s_rnge("ftlun", i__1, "dasfm_", (ftnlen)3981)];
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    if (findex == fthead) {
		fthead = lnknxt_(&findex, pool);
	    }
	    lnkfsl_(&findex, &findex, pool);
	    removi_(handle, fhlist);
	}
    }
    chkout_("DASLLC", (ftnlen)6);
    return 0;
/* $Procedure DASHFS ( DAS, handle to file summary ) */

L_dashfs:
/* $ Abstract */

/*     Return a file summary for a specified DAS file. */

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

/*     CONVERSION */
/*     DAS */
/*     FILES */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               NRESVR */
/*     INTEGER               NRESVC */
/*     INTEGER               NCOMR */
/*     INTEGER               NCOMC */
/*     INTEGER               FREE */
/*     INTEGER               LASTLA ( 3 ) */
/*     INTEGER               LASTRC ( 3 ) */
/*     INTEGER               LASTWD ( 3 ) */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of a DAS file. */
/*     NRESVR     O   Number of reserved records in file. */
/*     NRESVC     O   Number of characters in use in reserved rec. area. */
/*     NCOMR      O   Number of comment records in file. */
/*     NCOMC      O   Number of characters in use in comment area. */
/*     FREE       O   Number of first free record. */
/*     LASTLA     O   Array of last logical addresses for each data type. */
/*     LASTRC     O   Record number of last descriptor of each data type. */
/*     LASTWD     O   Word number of last descriptor of each data type. */

/* $ Detailed_Input */

/*     HANDLE      is the handle of a previously opened DAS file. */

/* $ Detailed_Output */

/*     NRESVR      is the number of reserved records in a specified DAS */
/*                 file. */

/*     NRESVC      is the number of characters in use in the reserved */
/*                 record area of a specified DAS file. */

/*     NCOMR       is the number of comment records in a specified DAS */
/*                 file. */

/*     NCOMC       is the number of characters in use in the comment area */
/*                 of a specified DAS file. */

/*     FREE        is the Fortran record number of the first free record */
/*                 in a specified DAS file. */

/*     LASTLA      is an array containing the highest current logical */
/*                 addresses, in the specified DAS file, of data of */
/*                 character, double precision, and integer types, in */
/*                 that order. */

/*     LASTRC      is an array containing the Fortran record numbers, in */
/*                 the specified DAS file, of the directory records */
/*                 containing the current last descriptors of clusters */
/*                 of character, double precision, and integer data */
/*                 records, in that order. */

/*     LASTWD      is an array containing the word positions, in the */
/*                 specified DAS file, of the current last descriptors */
/*                 of clusters of character, double precision, and */
/*                 integer data records, in that order. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the specified handle does not belong to any file that is */
/*        currently known to be open, the error SPICE(DASNOSUCHHANDLE) */
/*        is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The quantities NRESVR, NRESRC, NCOMR, NCOMC, FREE, LASTLA, */
/*     LASTRC, and LASTWD define the `state' of a DAS file, and in */
/*     particular the state of the directory structure of the file. */
/*     This information is needed by other DAS routines, but application */
/*     programs will usually have no need for it.  The one exception is */
/*     the array of `last' logical addresses LASTLA:  these addresses */
/*     indicate how many words of data of each type are contained in the */
/*     specified DAS file.  The elements of LASTLA can be conveniently */
/*     retrieved by calling DASLLA. */

/* $ Examples */

/*     1)  Dump the data from a DAS file. */

/*            C */
/*            C     Open the DAS file for reading. */
/*            C */
/*                  CALL DASOPR ( FILE, HANDLE ) */

/*            C */
/*            C     Obtain the file summary. */
/*            C */
/*                  CALL DASHFS ( HANDLE, */
/*                 .              NRESVR, */
/*                 .              RRESVC, */
/*                 .              NCOMR, */
/*                 .              NCOMC, */
/*                 .              FREE, */
/*                 .              LASTLA, */
/*                 .              LASTRC, */
/*                 .              LASTWD ) */

/*            C */
/*            C     Read the integers and dump them. */
/*            C */
/*                  DO I = 1, LASTLA(INT) */
/*                     CALL DASRDI ( HANDLE, I, I, N ) */
/*                     WRITE (*,*) N */
/*                  END DO */

/*            C */
/*            C     Now the d.p. numbers: */
/*            C */
/*                  DO I = 1, LASTLA(DP) */
/*                     CALL DASRDD ( HANDLE, I, I, X ) */
/*                     WRITE (*,*) X */
/*                  END DO */

/*            C */
/*            C     Now the characters.  In this case, we read the */
/*            C     data a line at a time. */
/*            C */
/*                  FIRST   =  0 */
/*                  LAST    =  0 */
/*                  REMAIN  =  LASTLA(CHAR) */

/*                  DO WHILE ( REMAIN .GT. 0 ) */

/*                     NREAD = MIN ( LINLEN, REMAIN ) */
/*                     FIRST = LAST + 1 */
/*                     LAST  = LAST + NREAD */

/*                     CALL DASRDC ( HANDLE, FIRST, LAST, LINE ) */

/*                     WRITE (*,*) LINE(:NREAD) */

/*                     REMAIN = REMAIN - NREAD */

/*                  END DO */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     K.R. Gehringer  (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if these open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUL-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     return the file summary of a DAS file */
/*     find the amount of data in a DAS file */
/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if these open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUL-1992 (NJB) (WLT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASHFS", (ftnlen)6);
    }

/*     Initialize the file table pool and handle list, if necessary. */

    if (pass1) {
	lnkini_(&c__21, pool);
	ssizei_(&c__21, fhlist);
	pass1 = FALSE_;
    }
    findex = fthead;
    found = FALSE_;
    while(! found && findex > 0) {
	if (fthan[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("fth"
		"an", i__1, "dasfm_", (ftnlen)4299)] == *handle) {
	    found = TRUE_;
	} else {
	    findex = lnknxt_(&findex, pool);
	}
    }
    if (found) {

/*        Give the caller the current summary from the file table. */

	*nresvr = ftsum[(i__1 = findex * 14 - 14) < 294 && 0 <= i__1 ? i__1 : 
		s_rnge("ftsum", i__1, "dasfm_", (ftnlen)4312)];
	*nresvc = ftsum[(i__1 = findex * 14 - 13) < 294 && 0 <= i__1 ? i__1 : 
		s_rnge("ftsum", i__1, "dasfm_", (ftnlen)4313)];
	*ncomr = ftsum[(i__1 = findex * 14 - 12) < 294 && 0 <= i__1 ? i__1 : 
		s_rnge("ftsum", i__1, "dasfm_", (ftnlen)4314)];
	*ncomc = ftsum[(i__1 = findex * 14 - 11) < 294 && 0 <= i__1 ? i__1 : 
		s_rnge("ftsum", i__1, "dasfm_", (ftnlen)4315)];
	*free = ftsum[(i__1 = findex * 14 - 10) < 294 && 0 <= i__1 ? i__1 : 
		s_rnge("ftsum", i__1, "dasfm_", (ftnlen)4316)];
	for (i__ = 1; i__ <= 3; ++i__) {
	    lastla[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("lastla",
		     i__1, "dasfm_", (ftnlen)4319)] = ftsum[(i__2 = i__ + 5 + 
		    findex * 14 - 15) < 294 && 0 <= i__2 ? i__2 : s_rnge(
		    "ftsum", i__2, "dasfm_", (ftnlen)4319)];
	    lastrc[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("lastrc",
		     i__1, "dasfm_", (ftnlen)4320)] = ftsum[(i__2 = i__ + 8 + 
		    findex * 14 - 15) < 294 && 0 <= i__2 ? i__2 : s_rnge(
		    "ftsum", i__2, "dasfm_", (ftnlen)4320)];
	    lastwd[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("lastwd",
		     i__1, "dasfm_", (ftnlen)4321)] = ftsum[(i__2 = i__ + 11 
		    + findex * 14 - 15) < 294 && 0 <= i__2 ? i__2 : s_rnge(
		    "ftsum", i__2, "dasfm_", (ftnlen)4321)];
	}
    } else {
	setmsg_("There is no DAS file open with handle = #", (ftnlen)41);
	errint_("#", handle, (ftnlen)1);
	sigerr_("SPICE(DASNOSUCHHANDLE)", (ftnlen)22);
    }
    chkout_("DASHFS", (ftnlen)6);
    return 0;
/* $Procedure DASUFS ( DAS, update file summary ) */

L_dasufs:
/* $ Abstract */

/*     Update the file summary in a specified DAS file. */

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

/*     CONVERSION */
/*     DAS */
/*     FILES */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               NRESVR */
/*     INTEGER               NRESVC */
/*     INTEGER               NCOMR */
/*     INTEGER               NCOMC */
/*     INTEGER               FREE */
/*     INTEGER               LASTLA ( 3 ) */
/*     INTEGER               LASTRC ( 3 ) */
/*     INTEGER               LASTWD ( 3 ) */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of an open DAS file. */
/*     NRESVR     I   Number of reserved records in file. */
/*     NRESVC     I   Number of characters in use in reserved rec. area. */
/*     NCOMR      I   Number of comment records in file. */
/*     NCOMC      I   Number of characters in use in comment area. */
/*     FREE       I   Number of first free record. */
/*     LASTLA     I   Array of last logical addresses for each data type. */
/*     LASTRC     I   Record number of last descriptor of each data type. */
/*     LASTWD     I   Word number of last descriptor of each data type. */

/* $ Detailed_Input */

/*     HANDLE      is the handle of a previously opened DAS file. */

/*     NRESVR      is the number of reserved records in a specified DAS */
/*                 file. */

/*     NRESVC      is the number of characters in use in the reserved */
/*                 record area of a specified DAS file. */

/*     NCOMR       is the number of comment records in a specified DAS */
/*                 file. */

/*     NCOMC       is the number of characters in use in the comment area */
/*                 of a specified DAS file. */

/*     FREE        is the Fortran record number of the first free record */
/*                 in a specified DAS file. */

/*     LASTLA      is an array containing the highest current logical */
/*                 addresses, in the specified DAS file, of data of */
/*                 character, double precision, and integer types, in */
/*                 that order. */

/*     LASTRC      is an array containing the Fortran record numbers, in */
/*                 the specified DAS file, of the directory records */
/*                 containing the current last descriptors of clusters */
/*                 of character, double precision, and integer data */
/*                 records, in that order. */

/*     LASTWD      is an array containing the word positions, in the */
/*                 specified DAS file, of the current last descriptors */
/*                 of clusters of character, double precision, and */
/*                 integer data records, in that order. */

/* $ Detailed_Output */

/*     None.  See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the specified handle does not belong to any file that is */
/*        currently known to be open, the error SPICE(DASNOSUCHHANDLE) */
/*        is signaled. */

/*     2) If the specified handle is not open for WRITE access, the */
/*        error SPICE(DASINVALIDACCESS) is signaled. */

/*     3) If this routine's attempts to read the DAS file record */
/*        fail before an update, the error SPICE(DASREADFAIL) is */
/*        signaled. */

/*     4) If the attempt to write to the DAS file record fails, the */
/*        error SPICE(DASWRITEFAIL) is signaled. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     The quantities NRESVR, NRESRC, NCOMR, NCOMC, FREE, LASTLA, */
/*     LASTRC, and LASTWD define the `state' of a DAS file, and in */
/*     particular the state of the directory structure of the file. */
/*     These quantities should normally be updated only by DAS routines. */

/*     The higher-level DAS routines that affect a DAS file's summary, */
/*     such as */

/*        DASADx */
/*        DASUDx */
/*        DASARR */

/*     automatically update the file summary, so there is no need for */
/*     the calling program to perform the update explicitly. */

/* $ Examples */

/*     1)  Update the last d.p. logical address for a DAS file, leaving */
/*         the rest of the file summary intact. */

/*            C */
/*            C     Read the file summary. */
/*            C */
/*                  CALL DASHFS ( HANDLE, */
/*                 .              NRESVR, */
/*                 .              RRESVC, */
/*                 .              NCOMR, */
/*                 .              NCOMC, */
/*                 .              FREE, */
/*                 .              LASTLA, */
/*                 .              LASTRC, */
/*                 .              LASTWD ) */

/*            C */
/*            C     Update the d.p. component of the `last logical */
/*            C     address' array. */
/*            C */
/*                  LASTLA(DP) = NEWVAL */

/*                  CALL DASUFS ( HANDLE, */
/*                 .              NRESVR, */
/*                 .              RRESVC, */
/*                 .              NCOMR, */
/*                 .              NCOMC, */
/*                 .              FREE, */
/*                 .              LASTLA, */
/*                 .              LASTRC, */
/*                 .              LASTWD ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     K.R. Gehringer  (JPL) */
/*     W.L. Taber      (JPL) */
/*     F.S. Turner     (JPL) */

/* $ Version */

/* -    SPICELIB Version 6.1.0, 26-SEP-2005 (NJB) */

/*        Bug fix:  file name is now correctly inserted into long */
/*        error message generated when target file is not open for */
/*        write access. */

/* -    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 6.0.0, 15-OCT-2001 (FST) (NJB) */

/*        Bug fix:  this routine now reads the file record */
/*        before attempting to update it.  The buffered values */
/*        of IDWORD and IFN are no longer present. */

/*        Bug fix:  missing call to CHKIN was added to an error */
/*        handling branch in entry point DASUFS.  This call is */
/*        required because DASUFS uses discovery check-in. */

/* -    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if these open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUL-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     update the file summary of a DAS file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 6.1.0, 26-SEP-2005 (NJB) */

/*        Bug fix:  file name is now correctly inserted into long */
/*        error message generated when target file is not open for */
/*        write access. */

/* -    SPICELIB Version 5.1.0, 15-OCT-2001 (NJB) */

/*        Bug fix:  missing call to CHKIN was added to an error */
/*        handling branch in entry point DASUFS.  This call is */
/*        required because DASUFS uses discovery check-in. */

/* -    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if these open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUL-1992 (NJB) (WLT) */

/* -& */

/*     We use discovery check-ins in this routine. */

    if (return_()) {
	return 0;
    }

/*     Initialize the file table pool and handle list, if necessary. */

    if (pass1) {
	chkin_("DASUFS", (ftnlen)6);
	lnkini_(&c__21, pool);
	ssizei_(&c__21, fhlist);
	chkout_("DASUFS", (ftnlen)6);
	pass1 = FALSE_;
    }

/*     Find the file table entries for this file. */

    findex = fthead;
    found = FALSE_;
    while(! found && findex > 0) {
	if (fthan[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("fth"
		"an", i__1, "dasfm_", (ftnlen)4660)] == *handle) {
	    found = TRUE_;
	} else {
	    findex = lnknxt_(&findex, pool);
	}
    }
    if (found) {

/*        Now check to see that HANDLE is open for write, as one has */
/*        no business updating a file summary for files that are */
/*        open for read access only. */

	if (ftacc[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("fta"
		"cc", i__1, "dasfm_", (ftnlen)4675)] != 2) {
	    chkin_("DASUFS", (ftnlen)6);
	    setmsg_("DAS file not open for writing. Handle = #, file = '#'.", 
		    (ftnlen)54);
	    errint_("#", handle, (ftnlen)1);
	    errfnm_("#", &ftlun[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 :
		     s_rnge("ftlun", i__1, "dasfm_", (ftnlen)4681)], (ftnlen)
		    1);
	    sigerr_("SPICE(DASINVALIDACCESS)", (ftnlen)23);
	    chkout_("DASUFS", (ftnlen)6);
	    return 0;
	}

/*        If any of the counts pertaining to the reserved record are or */
/*        the comment area were changed, we need to record the new */
/*        counts in the file record.  Otherwise, leave the file alone. */

	if (*nresvr != ftsum[(i__1 = findex * 14 - 14) < 294 && 0 <= i__1 ? 
		i__1 : s_rnge("ftsum", i__1, "dasfm_", (ftnlen)4693)] || *
		nresvc != ftsum[(i__2 = findex * 14 - 13) < 294 && 0 <= i__2 ?
		 i__2 : s_rnge("ftsum", i__2, "dasfm_", (ftnlen)4693)] || *
		ncomr != ftsum[(i__3 = findex * 14 - 12) < 294 && 0 <= i__3 ? 
		i__3 : s_rnge("ftsum", i__3, "dasfm_", (ftnlen)4693)] || *
		ncomc != ftsum[(i__5 = findex * 14 - 11) < 294 && 0 <= i__5 ? 
		i__5 : s_rnge("ftsum", i__5, "dasfm_", (ftnlen)4693)]) {

/*           Read the file record. */

	    io___53.ciunit = ftlun[(i__1 = findex - 1) < 21 && 0 <= i__1 ? 
		    i__1 : s_rnge("ftlun", i__1, "dasfm_", (ftnlen)4701)];
	    iostat = s_rdue(&io___53);
	    if (iostat != 0) {
		goto L100004;
	    }
	    iostat = do_uio(&c__1, idword, (ftnlen)8);
	    if (iostat != 0) {
		goto L100004;
	    }
	    iostat = do_uio(&c__1, locifn, (ftnlen)60);
	    if (iostat != 0) {
		goto L100004;
	    }
	    iostat = do_uio(&c__1, (char *)&locrrc, (ftnlen)sizeof(integer));
	    if (iostat != 0) {
		goto L100004;
	    }
	    iostat = do_uio(&c__1, (char *)&locrch, (ftnlen)sizeof(integer));
	    if (iostat != 0) {
		goto L100004;
	    }
	    iostat = do_uio(&c__1, (char *)&loccrc, (ftnlen)sizeof(integer));
	    if (iostat != 0) {
		goto L100004;
	    }
	    iostat = do_uio(&c__1, (char *)&loccch, (ftnlen)sizeof(integer));
	    if (iostat != 0) {
		goto L100004;
	    }
	    iostat = do_uio(&c__1, locfmt, (ftnlen)8);
	    if (iostat != 0) {
		goto L100004;
	    }
	    iostat = do_uio(&c__1, tail, (ftnlen)932);
	    if (iostat != 0) {
		goto L100004;
	    }
	    iostat = e_rdue();
L100004:
	    if (iostat != 0) {
		chkin_("DASUFS", (ftnlen)6);
		setmsg_("Attempt to read file record failed. File was '#'.  "
			"Value of IOSTAT was '#'.", (ftnlen)75);
		errfnm_("#", &ftlun[(i__1 = findex - 1) < 21 && 0 <= i__1 ? 
			i__1 : s_rnge("ftlun", i__1, "dasfm_", (ftnlen)4718)],
			 (ftnlen)1);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(DASREADFAIL)", (ftnlen)18);
		chkout_("DASUFS", (ftnlen)6);
		return 0;
	    }
	    io___55.ciunit = ftlun[(i__1 = findex - 1) < 21 && 0 <= i__1 ? 
		    i__1 : s_rnge("ftlun", i__1, "dasfm_", (ftnlen)4726)];
	    iostat = s_wdue(&io___55);
	    if (iostat != 0) {
		goto L100005;
	    }
	    iostat = do_uio(&c__1, idword, (ftnlen)8);
	    if (iostat != 0) {
		goto L100005;
	    }
	    iostat = do_uio(&c__1, locifn, (ftnlen)60);
	    if (iostat != 0) {
		goto L100005;
	    }
	    iostat = do_uio(&c__1, (char *)&(*nresvr), (ftnlen)sizeof(integer)
		    );
	    if (iostat != 0) {
		goto L100005;
	    }
	    iostat = do_uio(&c__1, (char *)&(*nresvc), (ftnlen)sizeof(integer)
		    );
	    if (iostat != 0) {
		goto L100005;
	    }
	    iostat = do_uio(&c__1, (char *)&(*ncomr), (ftnlen)sizeof(integer))
		    ;
	    if (iostat != 0) {
		goto L100005;
	    }
	    iostat = do_uio(&c__1, (char *)&(*ncomc), (ftnlen)sizeof(integer))
		    ;
	    if (iostat != 0) {
		goto L100005;
	    }
	    iostat = do_uio(&c__1, locfmt, (ftnlen)8);
	    if (iostat != 0) {
		goto L100005;
	    }
	    iostat = do_uio(&c__1, tail, (ftnlen)932);
	    if (iostat != 0) {
		goto L100005;
	    }
	    iostat = e_wdue();
L100005:
	    if (iostat != 0) {
		chkin_("DASUFS", (ftnlen)6);
		cl__1.cerr = 0;
		cl__1.cunit = ftlun[(i__1 = findex - 1) < 21 && 0 <= i__1 ? 
			i__1 : s_rnge("ftlun", i__1, "dasfm_", (ftnlen)4741)];
		cl__1.csta = 0;
		f_clos(&cl__1);
		setmsg_("Attempt to update file record failed. File was '#'."
			"  Value of IOSTAT was '#'.", (ftnlen)77);
		errfnm_("#", &ftlun[(i__1 = findex - 1) < 21 && 0 <= i__1 ? 
			i__1 : s_rnge("ftlun", i__1, "dasfm_", (ftnlen)4746)],
			 (ftnlen)1);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(DASWRITEFAIL)", (ftnlen)19);
		chkout_("DASUFS", (ftnlen)6);
		return 0;
	    }
	}

/*        Update the file table. */

	ftsum[(i__1 = findex * 14 - 14) < 294 && 0 <= i__1 ? i__1 : s_rnge(
		"ftsum", i__1, "dasfm_", (ftnlen)4759)] = *nresvr;
	ftsum[(i__1 = findex * 14 - 13) < 294 && 0 <= i__1 ? i__1 : s_rnge(
		"ftsum", i__1, "dasfm_", (ftnlen)4760)] = *nresvc;
	ftsum[(i__1 = findex * 14 - 12) < 294 && 0 <= i__1 ? i__1 : s_rnge(
		"ftsum", i__1, "dasfm_", (ftnlen)4761)] = *ncomr;
	ftsum[(i__1 = findex * 14 - 11) < 294 && 0 <= i__1 ? i__1 : s_rnge(
		"ftsum", i__1, "dasfm_", (ftnlen)4762)] = *ncomc;
	ftsum[(i__1 = findex * 14 - 10) < 294 && 0 <= i__1 ? i__1 : s_rnge(
		"ftsum", i__1, "dasfm_", (ftnlen)4763)] = *free;
	for (i__ = 1; i__ <= 3; ++i__) {
	    ftsum[(i__1 = i__ + 5 + findex * 14 - 15) < 294 && 0 <= i__1 ? 
		    i__1 : s_rnge("ftsum", i__1, "dasfm_", (ftnlen)4766)] = 
		    lastla[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge(
		    "lastla", i__2, "dasfm_", (ftnlen)4766)];
	    ftsum[(i__1 = i__ + 8 + findex * 14 - 15) < 294 && 0 <= i__1 ? 
		    i__1 : s_rnge("ftsum", i__1, "dasfm_", (ftnlen)4767)] = 
		    lastrc[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge(
		    "lastrc", i__2, "dasfm_", (ftnlen)4767)];
	    ftsum[(i__1 = i__ + 11 + findex * 14 - 15) < 294 && 0 <= i__1 ? 
		    i__1 : s_rnge("ftsum", i__1, "dasfm_", (ftnlen)4768)] = 
		    lastwd[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge(
		    "lastwd", i__2, "dasfm_", (ftnlen)4768)];
	}
    } else {
	chkin_("DASUFS", (ftnlen)6);
	setmsg_("There is no file open with handle = #", (ftnlen)37);
	errint_("#", handle, (ftnlen)1);
	sigerr_("SPICE(DASNOSUCHHANDLE)", (ftnlen)22);
	chkout_("DASUFS", (ftnlen)6);
    }
    return 0;
/* $Procedure DASHLU ( DAS, handle to logical unit ) */

L_dashlu:
/* $ Abstract */

/*     Return the logical unit associated with a handle. */

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

/*     CONVERSION */
/*     DAS */
/*     FILES */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               UNIT */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of a DAS file. */
/*     UNIT       O   Corresponding logical unit. */

/* $ Detailed_Input */

/*     HANDLE      is the handle of a previously opened DAS file. */

/* $ Detailed_Output */

/*     UNIT        is the Fortran logical unit to which the file is */
/*                 connected. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the specified handle does not belong to any file that is */
/*        currently known to be open, the error SPICE(DASNOSUCHHANDLE) */
/*        is signaled. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     This routine is a utility used by the DAS system to support */
/*     file I/O.  DASHLU may also prove useful to general SPICELIB */
/*     users for constructing error messages. */

/* $ Examples */

/*     1)  Obtain the logical unit associated with a DAS file having */
/*         a known handle. */

/*            CALL DASHLU ( HANDLE, UNIT ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     W.L. Taber      (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if these open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     map DAS file handle to logical unit */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if these open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */

/*     We use discovery check-ins in this routine. */

    if (return_()) {
	return 0;
    }

/*     Initialize the file table pool and handle list, if necessary. */

    if (pass1) {
	chkin_("DASHLU", (ftnlen)6);
	lnkini_(&c__21, pool);
	ssizei_(&c__21, fhlist);
	chkout_("DASHLU", (ftnlen)6);
	pass1 = FALSE_;
    }

/*     Find the file table entries for this file. */

    findex = fthead;
    found = FALSE_;
    while(! found && findex > 0) {
	if (fthan[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("fth"
		"an", i__1, "dasfm_", (ftnlen)4980)] == *handle) {
	    found = TRUE_;
	} else {
	    findex = lnknxt_(&findex, pool);
	}
    }
    if (found) {
	*unit = ftlun[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
		"ftlun", i__1, "dasfm_", (ftnlen)4991)];
    } else {
	chkin_("DASHLU", (ftnlen)6);
	setmsg_("There is no file open with handle = #", (ftnlen)37);
	errint_("#", handle, (ftnlen)1);
	sigerr_("SPICE(DASNOSUCHHANDLE)", (ftnlen)22);
	chkout_("DASHLU", (ftnlen)6);
    }
    return 0;
/* $Procedure DASLUH ( DAS, logical unit to handle ) */

L_dasluh:
/* $ Abstract */

/*     Return the handle associated with a logical unit. */

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

/*     CONVERSION */
/*     DAS */
/*     FILES */

/* $ Declarations */

/*     INTEGER               UNIT */
/*     INTEGER               HANDLE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     UNIT       I   Logical unit connected to a DAS file. */
/*     HANDLE     O   Corresponding handle. */

/* $ Detailed_Input */

/*     UNIT        is the logical unit to which a DAS file has been */
/*                 connected when it was opened. */

/* $ Detailed_Output */

/*     HANDLE      is the handle associated with the file. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the specified unit is not connected to any DAS file that is */
/*        currently known to be open, the error SPICE(DASNOSUCHUNIT) */
/*        is signaled. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     It is unlikely, but possible, that a calling program would know */
/*     the logical unit to which a file is connected without knowing the */
/*     handle associated with the file.  DASLUH is provided mostly for */
/*     completeness. */

/* $ Examples */

/*     In the following code fragment, the handle associated with */
/*     a DAS file is retrieved using the logical unit to which the */
/*     file is connected. The handle is then used to determine the */
/*     name of the file. */

/*        CALL DASLUH ( UNIT,   HANDLE ) */
/*        CALL DASHFN ( HANDLE, FNAME  ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     K.R. Gehringer  (JPL) */
/*     W.L. Taber      (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if these open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     map logical unit to DAS file handle */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if these open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASLUH", (ftnlen)6);
    }

/*     Initialize the file table pool and handle list, if necessary. */

    if (pass1) {
	lnkini_(&c__21, pool);
	ssizei_(&c__21, fhlist);
	pass1 = FALSE_;
    }

/*     Find the file table entries for this file. */

    findex = fthead;
    found = FALSE_;
    while(! found && findex > 0) {
	if (ftlun[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("ftl"
		"un", i__1, "dasfm_", (ftnlen)5205)] == *unit) {
	    found = TRUE_;
	} else {
	    findex = lnknxt_(&findex, pool);
	}
    }
    if (found) {
	*handle = fthan[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
		"fthan", i__1, "dasfm_", (ftnlen)5215)];
    } else {
	setmsg_("There is no DAS file open with unit = #", (ftnlen)39);
	errint_("#", unit, (ftnlen)1);
	sigerr_("SPICE(DASNOSUCHUNIT)", (ftnlen)20);
    }
    chkout_("DASLUH", (ftnlen)6);
    return 0;
/* $Procedure DASHFN ( DAS, handle to file name ) */

L_dashfn:
/* $ Abstract */

/*     Return the name of the DAS file associated with a handle. */

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

/*     CONVERSION */
/*     DAS */
/*     FILES */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     CHARACTER*(*)         FNAME */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of a DAS file. */
/*     FNAME      O   Corresponding file name. */

/* $ Detailed_Input */

/*     HANDLE      is the handle of a previously opened DAS file. */

/* $ Detailed_Output */

/*     FNAME       is the name of the DAS file associated with the input */
/*                 file handle. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the specified handle does not belong to any file that is */
/*        currently known to be open, the error SPICE(DASNOSUCHHANDLE) */
/*        is signaled. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     It may be desirable to recover the names of one or more DAS */
/*     files in a different part of the program from the one in which */
/*     they were opened. Note that the names returned by DASHFN may */
/*     not be identical to the names used to open the files. Under */
/*     most operating systems, a particular file can be accessed using */
/*     many different names. DASHFN returns one of them. */

/* $ Examples */

/*     In the following code fragment, the name of a DAS file is */
/*     recovered using the handle associated with the file. */

/*        CALL DASOPR ( 'sample.DAS', HANDLE ) */
/*         . */
/*         . */

/*        CALL DASHFN ( HANDLE, FNAME ) */

/*     Depending on the circumstances (operating system, compiler, */
/*     default directory) the value of FNAME might resemble any of */
/*     the following: */

/*        'USER$DISK:[WYATT.IMAGES]SAMPLE.DAS;4' */

/*        '/wyatt/images/sample.DAS' */

/*        'A:\IMAGES\SAMPLE.DAS' */

/*     On the other hand, it might not. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     K.R. Gehringer  (JPL) */
/*     W.L. Taber      (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if these open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     map DAS handle to file name */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if these open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASHFN", (ftnlen)6);
    }

/*     Initialize the file table pool and handle list, if necessary. */

    if (pass1) {
	lnkini_(&c__21, pool);
	ssizei_(&c__21, fhlist);
	pass1 = FALSE_;
    }

/*     Find the file table entries for this file. */

    findex = fthead;
    found = FALSE_;
    while(! found && findex > 0) {
	if (fthan[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("fth"
		"an", i__1, "dasfm_", (ftnlen)5443)] == *handle) {
	    found = TRUE_;
	} else {
	    findex = lnknxt_(&findex, pool);
	}
    }
    if (found) {
	ioin__1.inerr = 0;
	ioin__1.inunit = ftlun[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : 
		s_rnge("ftlun", i__1, "dasfm_", (ftnlen)5453)];
	ioin__1.infile = 0;
	ioin__1.inex = 0;
	ioin__1.inopen = 0;
	ioin__1.innum = 0;
	ioin__1.innamed = 0;
	ioin__1.innamlen = fname_len;
	ioin__1.inname = fname;
	ioin__1.inacc = 0;
	ioin__1.inseq = 0;
	ioin__1.indir = 0;
	ioin__1.infmt = 0;
	ioin__1.inform = 0;
	ioin__1.inunf = 0;
	ioin__1.inrecl = 0;
	ioin__1.innrec = 0;
	ioin__1.inblank = 0;
	f_inqu(&ioin__1);
    } else {
	setmsg_("There is no DAS file open with handle = #", (ftnlen)41);
	errint_("#", handle, (ftnlen)1);
	sigerr_("SPICE(DASNOSUCHHANDLE)", (ftnlen)22);
    }
    chkout_("DASHFN", (ftnlen)6);
    return 0;
/* $Procedure DASFNH ( DAS, file name to handle ) */

L_dasfnh:
/* $ Abstract */

/*     Return handle associated with a file name. */

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

/*     CONVERSION */
/*     DAS */
/*     FILES */

/* $ Declarations */

/*     CHARACTER*(*)         FNAME */
/*     INTEGER               HANDLE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     FNAME      I   Name of a DAS file. */
/*     HANDLE     O   Corresponding handle. */

/* $ Detailed_Input */

/*     FNAME       is the name of a previously opened DAS file. */

/* $ Detailed_Output */

/*     HANDLE      is the handle associated with the file. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the specified name does not specify any DAS file currently */
/*        known to be open, the error SPICE(DASNOSUCHFILE) is signaled. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     It is sometimes easier to work with file names (which are */
/*     meaningful, and often predictable) than with file handles */
/*     (which are neither), especially in interactive situations. */
/*     However, nearly every DAS routine requires that you use file */
/*     handles to refer to files. DASFNH is provided to bridge the gap */
/*     between the two representations. */

/* $ Examples */

/*     In the following code fragment, the handle associated with a */
/*     DAS file is recovered using the name of the file. */

/*        CALL DASOPR ( 'sample.DAS', HANDLE ) */
/*         . */
/*         . */

/*        CALL DASFNH ( 'sample.DAS', HANDLE ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     W.L. Taber      (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if these open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     map file name to DAS handle */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if these open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASFNH", (ftnlen)6);
    }

/*     Initialize the file table pool and handle list, if necessary. */

    if (pass1) {
	lnkini_(&c__21, pool);
	ssizei_(&c__21, fhlist);
	pass1 = FALSE_;
    }
    ioin__1.inerr = 0;
    ioin__1.infilen = rtrim_(fname, fname_len);
    ioin__1.infile = fname;
    ioin__1.inex = 0;
    ioin__1.inopen = &opened;
    ioin__1.innum = &number;
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
    f_inqu(&ioin__1);

/*     Find the file table entries for this file. */

    findex = fthead;
    found = FALSE_;
    while(! found && findex > 0) {
	if (ftlun[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("ftl"
		"un", i__1, "dasfm_", (ftnlen)5671)] == number) {
	    found = TRUE_;
	} else {
	    findex = lnknxt_(&findex, pool);
	}
    }
    if (found) {
	*handle = fthan[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
		"fthan", i__1, "dasfm_", (ftnlen)5681)];
    } else {
	setmsg_("There is no DAS file in the table with file name = '#'", (
		ftnlen)54);
	errch_("#", fname, (ftnlen)1, fname_len);
	sigerr_("SPICE(DASNOSUCHFILE)", (ftnlen)20);
    }
    chkout_("DASFNH", (ftnlen)6);
    return 0;
/* $Procedure      DASHOF ( DAS, handles of open files ) */

L_dashof:
/* $ Abstract */

/*     Return a SPICELIB set containing the handles of all currently */
/*     open DAS files. */

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
/*     SETS */

/* $ Keywords */

/*     DAS */
/*     FILES */

/* $ Declarations */

/*     INTEGER               LBCELL */
/*     PARAMETER           ( LBCELL = -5 ) */

/*     INTEGER               FHSET ( LBCELL : * ) */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     FHSET      O   A set containing handles of currently open DAS */
/*                    files. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     FHSET          is a SPICELIB set containing the file handles of */
/*                    all currently open DAS files. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the set FHSET is not initialized, the error will be */
/*         diagnosed by routines called by this routine. */

/*     2)  If the set FHSET is too small to accommodate the set of */
/*         handles to be returned, the error will be diagnosed by */
/*         routines called by this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine allows subroutines to test DAS file handles for */
/*     validity before using them.  Many DAS operations that rely on */
/*     handles to identify DAS files cause errors to be signaled if */
/*     the handles are invalid. */

/* $ Examples */

/*     1)  Find out how may DAS files are open for writing. */

/*            C */
/*            C    Find out which DAS files are open. */
/*            C */
/*                 CALL DASHOF  ( FHSET ) */

/*            C */
/*            C    Count the ones open for writing. */
/*            C */
/*                 COUNT = 0 */

/*                 DO I = 1, CARDC(FHSET) */

/*                    CALL DASHAM ( FHSET(I), METHOD ) */

/*                    IF ( METHOD .EQ. WRITE ) THEN */
/*                       COUNT = COUNT + 1 */
/*                    END IF */

/*                 END DO */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     K.R. Gehringer  (JPL) */
/*     W.L. Taber      (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     return set of handles of open DAS files */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASHOF", (ftnlen)6);
    }

/*     Initialize the file table pool and handle list, if necessary. */

    if (pass1) {
	lnkini_(&c__21, pool);
	ssizei_(&c__21, fhlist);
	pass1 = FALSE_;
    }

/*     Just stuff our local list into the set. */

    copyi_(fhlist, fhset);
    chkout_("DASHOF", (ftnlen)6);
    return 0;
/* $Procedure      DASSIH ( DAS, signal invalid handles ) */

L_dassih:
/* $ Abstract */

/*     Signal an error if a DAS file file handle does not designate a */
/*     DAS file that is open for a specified type of access. */

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
/*     ERROR */
/*     SETS */

/* $ Keywords */

/*     DAS */
/*     FILES */
/*     UTILITY */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     CHARACTER*(*)         ACCESS */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   HANDLE to be validated. */
/*     ACCESS     I   String indicating access type. */

/* $ Detailed_Input */

/*     HANDLE         is a DAS file handle to validate.  For HANDLE to be */
/*                    considered valid, it must specify a DAS file that */
/*                    is open for the type of access specified by the */
/*                    input argument ACCESS. */


/*     ACCESS         is a string indicating the type of access that */
/*                    the DAS file specified by the input argument HANDLE */
/*                    must be open for.  The values of ACCESS may be */

/*                       'READ'      File must be open for read access */
/*                                   by DAS routines.  DAS files opened */
/*                                   for read or write access may be */
/*                                   read. */

/*                       'WRITE'     File must be open for write access */
/*                                   by DAS routines.  Note that files */
/*                                   open for write access may be read as */
/*                                   well as written. */

/*                    Leading and trailing blanks in ACCESS are ignored, */
/*                    and case is not significant. */

/* $ Detailed_Output */

/*     None.  See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input argument ACCESS has an unrecognized value, */
/*         the error SPICE(INVALIDOPTION) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine signals the error SPICE(DASINVALIDACCESS) if the */
/*     DAS designated by the input argument HANDLE is not open */
/*     for the specified type of access.  If HANDLE does not designate */
/*     an open DAS file, the error SPICE(DASNOSUCHHANDLE) is signaled. */

/*     This routine allows subroutines to test file handles for */
/*     validity before attempting to access the files they designate, */
/*     or before performing operations on the handles themselves, such */
/*     as finding the name of the file designated by a handle.  This */
/*     routine should be used in situations where the appropriate action */
/*     to take upon determining that a handle is invalid is to signal */
/*     an error.  DASSIH centralizes the error response for this type of */
/*     error in a single routine. */

/*     In cases where it is necessary to determine the validity of a */
/*     file handle, but it is not an error for the handle to refer */
/*     to a closed file, the entry point DASHOF should be used instead */
/*     of DASSIH. */

/* $ Examples */

/*     1)  Make sure that a file handle designates a DAS file that can */
/*         be read.  Signal an error if not. */

/*         Note that if a DAS file is open for reading or writing, read */
/*         access is allowed. */

/*                  CALL DASSIH ( HANDLE, 'READ' ) */

/*                  IF ( FAILED() ) THEN */
/*                     RETURN */
/*                  END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     K.R. Gehringer  (JPL) */
/*     W.L. Taber      (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 6.1.0, 26-SEP-2005 (NJB) */

/*        Local variable DAS was renamed to DASFIL.  This */
/*        was done to avoid future conflict with parameters */
/*        in zzddhman.inc. */

/* -    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if these open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     detect invalid DAS handles */
/*     validate DAS handles */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 6.1.0, 26-SEP-2005 (NJB) */

/*        Local variable DAS was renamed to DASFIL.  This */
/*        was done to avoid future conflict with parameters */
/*        in zzddhman.inc. */

/* -    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if these open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASSIH", (ftnlen)6);
    }

/*     Initialize the file table pool and handle list, if necessary. */

    if (pass1) {
	lnkini_(&c__21, pool);
	ssizei_(&c__21, fhlist);
	pass1 = FALSE_;
    }

/*     Get an upper case, left-justified copy of ACCESS. */

    ljust_(access, acc, access_len, (ftnlen)10);
    ucase_(acc, acc, (ftnlen)10, (ftnlen)10);

/*     Make sure we recognize the access type specified by the caller. */

    if (s_cmp(acc, "READ", (ftnlen)10, (ftnlen)4) != 0 && s_cmp(acc, "WRITE", 
	    (ftnlen)10, (ftnlen)5) != 0) {
	setmsg_("Unrecognized access type.  Type was #. ", (ftnlen)39);
	errch_("#", access, (ftnlen)1, access_len);
	sigerr_("SPICE(INVALIDOPTION)", (ftnlen)20);
	chkout_("DASSIH", (ftnlen)6);
	return 0;
    }

/*     See whether the input handle is in our list at all.  It's */
/*     unlawful for the handle to be absent. */

    if (! elemi_(handle, fhlist)) {
	setmsg_("Handle # is not attached to an open DAS file.", (ftnlen)45);
	errint_("#", handle, (ftnlen)1);
	sigerr_("SPICE(DASNOSUCHHANDLE)", (ftnlen)22);
	chkout_("DASSIH", (ftnlen)6);
	return 0;
    } else {

/*        Find the file table entries for this file.  We know they */
/*        must exist. */

	findex = fthead;
	found = FALSE_;
	while(! found && findex > 0) {
	    if (fthan[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
		    "fthan", i__1, "dasfm_", (ftnlen)6184)] == *handle) {
		found = TRUE_;
	    } else {
		findex = lnknxt_(&findex, pool);
	    }
	}

/*        At this point, FINDEX points to the file table entries */
/*        for this file. */

	if (s_cmp(acc, "WRITE", (ftnlen)10, (ftnlen)5) == 0 && ftacc[(i__1 = 
		findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("ftacc", i__1, 
		"dasfm_", (ftnlen)6196)] != 2) {

/*           If the access type is 'WRITE', the DAS file must be open */
/*           for writing. */

	    ioin__1.inerr = 0;
	    ioin__1.inunit = ftlun[(i__1 = findex - 1) < 21 && 0 <= i__1 ? 
		    i__1 : s_rnge("ftlun", i__1, "dasfm_", (ftnlen)6202)];
	    ioin__1.infile = 0;
	    ioin__1.inex = 0;
	    ioin__1.inopen = 0;
	    ioin__1.innum = 0;
	    ioin__1.innamed = 0;
	    ioin__1.innamlen = 255;
	    ioin__1.inname = dasfil;
	    ioin__1.inacc = 0;
	    ioin__1.inseq = 0;
	    ioin__1.indir = 0;
	    ioin__1.infmt = 0;
	    ioin__1.inform = 0;
	    ioin__1.inunf = 0;
	    ioin__1.inrecl = 0;
	    ioin__1.innrec = 0;
	    ioin__1.inblank = 0;
	    f_inqu(&ioin__1);
	    setmsg_("DAS file not open for writing. Handle = #, file = '#'.", 
		    (ftnlen)54);
	    errint_("#", handle, (ftnlen)1);
	    errch_("#", dasfil, (ftnlen)1, (ftnlen)255);
	    sigerr_("SPICE(DASINVALIDACCESS)", (ftnlen)23);
	    chkout_("DASSIH", (ftnlen)6);
	    return 0;
	}
    }

/*     The DAS file's handle is o.k. */

    chkout_("DASSIH", (ftnlen)6);
    return 0;
/* $Procedure      DASHAM ( DAS, handle to access method ) */

L_dasham:
/* $ Abstract */

/*     Return the allowed access method for a specified DAS file. */

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

/*     DAS */
/*     FILES */
/*     UTILITY */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     CHARACTER*(*)         ACCESS */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   HANDLE of a DAS file. */
/*     ACCESS     O   String indicating allowed access method. */

/* $ Detailed_Input */

/*     HANDLE      is the handle of a previously opened DAS file. */

/* $ Detailed_Output */

/*     ACCESS         is a string indicating the type of access that */
/*                    the DAS file specified by the input argument HANDLE */
/*                    is open for.  The values of ACCESS may be */

/*                       'READ'      File is open for read access by DAS */
/*                                   routines.  Both the data area and */
/*                                   the comment area may be read.  The */
/*                                   file may not be modified. */

/*                       'WRITE'     File is open for write access by */
/*                                   DAS routines.  Files open for */
/*                                   write access may be read as well as */
/*                                   written. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input handle is invalid, the error SPICE(INVALIDHANDLE) */
/*         is signaled.  ACCESS is not modified. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine allows subroutines to determine the access methods */
/*     allowed for a given DAS file. */

/* $ Examples */

/*     1)  Make sure that a file handle designates a DAS file that can */
/*         be read.  Signal an error if not. */

/*         Note that if a DAS file is open for reading or writing, read */
/*         access is allowed. */

/*                  CALL DASHAM ( HANDLE, 'READ' ) */

/*                  IF ( FAILED() ) THEN */
/*                     RETURN */
/*                  END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     K.R. Gehringer  (JPL) */
/*     W.L. Taber      (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input and $ Output sections of the header. This was */
/*        done in order to minimize documentation changes if these open */
/*        routines ever change. */

/* -    SPICELIB Version 1.0.0, 01-FEB-1993 (NJB) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     return allowed access methods for DAS files */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input and $ Output sections of the header. This was */
/*        done in order to minimize documentation changes if these open */
/*        routines ever change. */

/* -    SPICELIB Version 1.0.0, 01-FEB-1993 (NJB) (WLT) (IMU) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASHAM", (ftnlen)6);
    }

/*     Initialize the file table pool and handle list, if necessary. */

    if (pass1) {
	lnkini_(&c__21, pool);
	ssizei_(&c__21, fhlist);
	pass1 = FALSE_;
    }

/*     See whether the input handle is in our list at all.  It's */
/*     unlawful for the handle to be absent. */

    findex = fthead;
    found = FALSE_;
    while(! found && findex > 0) {
	if (fthan[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("fth"
		"an", i__1, "dasfm_", (ftnlen)6435)] == *handle) {
	    found = TRUE_;
	} else {
	    findex = lnknxt_(&findex, pool);
	}
    }
    if (! found) {
	setmsg_("The handle # does not designate a known DAS file ", (ftnlen)
		49);
	errint_("#", handle, (ftnlen)1);
	sigerr_("SPICE(INVALIDHANDLE)", (ftnlen)20);
	chkout_("DASHAM", (ftnlen)6);
	return 0;
    }

/*     We know about the file if we got this far.  Set the output */
/*     argument accordingly. */

    if (ftacc[(i__1 = findex - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("ftacc", 
	    i__1, "dasfm_", (ftnlen)6458)] == 1) {
	s_copy(access, "READ", access_len, (ftnlen)4);
    } else {
	s_copy(access, "WRITE", access_len, (ftnlen)5);
    }
    chkout_("DASHAM", (ftnlen)6);
    return 0;
} /* dasfm_ */

/* Subroutine */ int dasfm_(char *fname, char *ftype, char *ifname, integer *
	handle, integer *unit, integer *free, integer *lastla, integer *
	lastrc, integer *lastwd, integer *nresvr, integer *nresvc, integer *
	ncomr, integer *ncomc, integer *fhset, char *access, ftnlen fname_len,
	 ftnlen ftype_len, ftnlen ifname_len, ftnlen access_len)
{
    return dasfm_0_(0, fname, ftype, ifname, handle, unit, free, lastla, 
	    lastrc, lastwd, nresvr, nresvc, ncomr, ncomc, fhset, access, 
	    fname_len, ftype_len, ifname_len, access_len);
    }

/* Subroutine */ int dasopr_(char *fname, integer *handle, ftnlen fname_len)
{
    return dasfm_0_(1, fname, (char *)0, (char *)0, handle, (integer *)0, (
	    integer *)0, (integer *)0, (integer *)0, (integer *)0, (integer *)
	    0, (integer *)0, (integer *)0, (integer *)0, (integer *)0, (char *
	    )0, fname_len, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int dasopw_(char *fname, integer *handle, ftnlen fname_len)
{
    return dasfm_0_(2, fname, (char *)0, (char *)0, handle, (integer *)0, (
	    integer *)0, (integer *)0, (integer *)0, (integer *)0, (integer *)
	    0, (integer *)0, (integer *)0, (integer *)0, (integer *)0, (char *
	    )0, fname_len, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int dasonw_(char *fname, char *ftype, char *ifname, integer *
	ncomr, integer *handle, ftnlen fname_len, ftnlen ftype_len, ftnlen 
	ifname_len)
{
    return dasfm_0_(3, fname, ftype, ifname, handle, (integer *)0, (integer *)
	    0, (integer *)0, (integer *)0, (integer *)0, (integer *)0, (
	    integer *)0, ncomr, (integer *)0, (integer *)0, (char *)0, 
	    fname_len, ftype_len, ifname_len, (ftnint)0);
    }

/* Subroutine */ int dasopn_(char *fname, char *ifname, integer *handle, 
	ftnlen fname_len, ftnlen ifname_len)
{
    return dasfm_0_(4, fname, (char *)0, ifname, handle, (integer *)0, (
	    integer *)0, (integer *)0, (integer *)0, (integer *)0, (integer *)
	    0, (integer *)0, (integer *)0, (integer *)0, (integer *)0, (char *
	    )0, fname_len, (ftnint)0, ifname_len, (ftnint)0);
    }

/* Subroutine */ int dasops_(integer *handle)
{
    return dasfm_0_(5, (char *)0, (char *)0, (char *)0, handle, (integer *)0, 
	    (integer *)0, (integer *)0, (integer *)0, (integer *)0, (integer *
	    )0, (integer *)0, (integer *)0, (integer *)0, (integer *)0, (char 
	    *)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int dasllc_(integer *handle)
{
    return dasfm_0_(6, (char *)0, (char *)0, (char *)0, handle, (integer *)0, 
	    (integer *)0, (integer *)0, (integer *)0, (integer *)0, (integer *
	    )0, (integer *)0, (integer *)0, (integer *)0, (integer *)0, (char 
	    *)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int dashfs_(integer *handle, integer *nresvr, integer *
	nresvc, integer *ncomr, integer *ncomc, integer *free, integer *
	lastla, integer *lastrc, integer *lastwd)
{
    return dasfm_0_(7, (char *)0, (char *)0, (char *)0, handle, (integer *)0, 
	    free, lastla, lastrc, lastwd, nresvr, nresvc, ncomr, ncomc, (
	    integer *)0, (char *)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)
	    0);
    }

/* Subroutine */ int dasufs_(integer *handle, integer *nresvr, integer *
	nresvc, integer *ncomr, integer *ncomc, integer *free, integer *
	lastla, integer *lastrc, integer *lastwd)
{
    return dasfm_0_(8, (char *)0, (char *)0, (char *)0, handle, (integer *)0, 
	    free, lastla, lastrc, lastwd, nresvr, nresvc, ncomr, ncomc, (
	    integer *)0, (char *)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)
	    0);
    }

/* Subroutine */ int dashlu_(integer *handle, integer *unit)
{
    return dasfm_0_(9, (char *)0, (char *)0, (char *)0, handle, unit, (
	    integer *)0, (integer *)0, (integer *)0, (integer *)0, (integer *)
	    0, (integer *)0, (integer *)0, (integer *)0, (integer *)0, (char *
	    )0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int dasluh_(integer *unit, integer *handle)
{
    return dasfm_0_(10, (char *)0, (char *)0, (char *)0, handle, unit, (
	    integer *)0, (integer *)0, (integer *)0, (integer *)0, (integer *)
	    0, (integer *)0, (integer *)0, (integer *)0, (integer *)0, (char *
	    )0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int dashfn_(integer *handle, char *fname, ftnlen fname_len)
{
    return dasfm_0_(11, fname, (char *)0, (char *)0, handle, (integer *)0, (
	    integer *)0, (integer *)0, (integer *)0, (integer *)0, (integer *)
	    0, (integer *)0, (integer *)0, (integer *)0, (integer *)0, (char *
	    )0, fname_len, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int dasfnh_(char *fname, integer *handle, ftnlen fname_len)
{
    return dasfm_0_(12, fname, (char *)0, (char *)0, handle, (integer *)0, (
	    integer *)0, (integer *)0, (integer *)0, (integer *)0, (integer *)
	    0, (integer *)0, (integer *)0, (integer *)0, (integer *)0, (char *
	    )0, fname_len, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int dashof_(integer *fhset)
{
    return dasfm_0_(13, (char *)0, (char *)0, (char *)0, (integer *)0, (
	    integer *)0, (integer *)0, (integer *)0, (integer *)0, (integer *)
	    0, (integer *)0, (integer *)0, (integer *)0, (integer *)0, fhset, 
	    (char *)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int dassih_(integer *handle, char *access, ftnlen access_len)
{
    return dasfm_0_(14, (char *)0, (char *)0, (char *)0, handle, (integer *)0,
	     (integer *)0, (integer *)0, (integer *)0, (integer *)0, (integer 
	    *)0, (integer *)0, (integer *)0, (integer *)0, (integer *)0, 
	    access, (ftnint)0, (ftnint)0, (ftnint)0, access_len);
    }

/* Subroutine */ int dasham_(integer *handle, char *access, ftnlen access_len)
{
    return dasfm_0_(15, (char *)0, (char *)0, (char *)0, handle, (integer *)0,
	     (integer *)0, (integer *)0, (integer *)0, (integer *)0, (integer 
	    *)0, (integer *)0, (integer *)0, (integer *)0, (integer *)0, 
	    access, (ftnint)0, (ftnint)0, (ftnint)0, access_len);
    }

