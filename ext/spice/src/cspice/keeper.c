/* keeper.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__255 = 255;
static integer c__5300 = 5300;
static integer c__1 = 1;

/* $Procedure KEEPER ( Keeps track of SPICE kernels ) */
/* Subroutine */ int keeper_0_(int n__, integer *which, char *kind, char *
	file, integer *count, char *filtyp, integer *handle, char *source, 
	logical *found, ftnlen kind_len, ftnlen file_len, ftnlen filtyp_len, 
	ftnlen source_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer loaded = 0;

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    logical dock, doek;
    integer fidx, lidx;
    char norc[1];
    integer hits, size, b, d__, e, i__, j, k, n;
    logical didck, didek;
    integer r__;
    extern /* Subroutine */ int eklef_(char *, integer *, ftnlen), chkin_(
	    char *, ftnlen), ekuef_(integer *);
    logical dopck;
    extern /* Subroutine */ int cklpf_(char *, integer *, ftnlen);
    static char files[255*5300];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen), 
	    repmc_(char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, 
	    ftnlen), ckupf_(integer *);
    static integer srces[5300];
    logical dospk, paths, gotit;
    static char known[32*3];
    extern integer rtrim_(char *, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    integer n1, n2, n3, start;
    static char types[8*5300];
    char fil2ld[255];
    extern logical failed_(void);
    logical ok, didpck;
    extern /* Subroutine */ int remlac_(integer *, integer *, char *, integer 
	    *, ftnlen);
    static integer handls[5300];
    logical dometa;
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    char nofile[500];
    integer dollar, fnmlen, myhand;
    logical didspk, update;
    extern /* Subroutine */ int gcpool_(char *, integer *, integer *, integer 
	    *, char *, logical *, ftnlen, ftnlen), fndnwd_(char *, integer *, 
	    integer *, integer *, ftnlen), pckuof_(integer *), clpool_(void), 
	    remlai_(integer *, integer *, integer *, integer *);
    extern logical samsub_(char *, integer *, integer *, char *, integer *, 
	    integer *, ftnlen, ftnlen);
    integer filnum;
    char pvalue[255];
    integer npaths;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    integer cursrc, npvals;
    char symbol[80];
    logical didtxt, dotext;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int cvpool_(char *, logical *, ftnlen), errint_(
	    char *, integer *, ftnlen), swpool_(char *, integer *, char *, 
	    ftnlen, ftnlen), dtpool_(char *, logical *, integer *, char *, 
	    ftnlen, ftnlen), stpool_(char *, integer *, char *, char *, 
	    integer *, logical *, ftnlen, ftnlen, ftnlen), sepool_(char *, 
	    integer *, char *, char *, integer *, integer *, logical *, 
	    ftnlen, ftnlen, ftnlen), repsub_(char *, integer *, integer *, 
	    char *, char *, ftnlen, ftnlen, ftnlen), repmot_(char *, char *, 
	    integer *, char *, char *, ftnlen, ftnlen, ftnlen, ftnlen), 
	    dvpool_(char *, ftnlen);
    char thstyp[8];
    extern /* Subroutine */ int spkuef_(integer *), ldpool_(char *, ftnlen), 
	    spklef_(char *, integer *, ftnlen), pcklof_(char *, integer *, 
	    ftnlen);
    logical add, fnd;
    integer src, use;
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int zzldker_(char *, char *, char *, integer *, 
	    ftnlen, ftnlen, ftnlen);

/* $ Abstract */

/*     This routine is an umbrella for a collection of entry points */
/*     that manage the loading and unloading of SPICE kernels from */
/*     an application program. */

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

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  ENTRY POINT */
/*     --------  ---  -------------------------------------------------- */
/*     KIND       I   KTOTAL, KDATA */
/*     FILE      I/O  FURNSH, KDATA,  UNLOAD, KINFO */
/*     FILTYP    I/O  KTOTAL, KDATA,  KINFO */
/*     COUNT      O   KTOTAL */
/*     HANDLE     O   KDATA,  KINFO */
/*     SOURCE     O   KDATA.  KINFO */
/*     FOUND      O   KDATA.  KINFO */
/*     FILSIZ     P   Maximum file name length. */
/*     MAXFIL     P   Is the maximum number of files that can be loaded. */


/* $ Detailed_Input */

/*     See Individual Entry points. */

/* $ Detailed_Output */

/*     See Individual Entry points. */

/* $ Parameters */

/*     FILSIZ    is the maximum file name length that can be */
/*               accommodated by this set of routines. */


/*     MAXFIL    is the number of entries that can be stored in KEEPER's */
/*               kernel database.  Each time a kernel is loaded via */
/*               FURNSH, a database entry is created for that kernel. */
/*               If a meta-kernel is loaded, a database entry is created */
/*               for the meta-kernel itself and for all files referenced */
/*               in the meta-kernel's KERNELS_TO_LOAD specification. */
/*               Unloading a kernel or meta-kernel deletes database */
/*               entries created when the file was loaded. */

/*               The parameter MAXFIL is an upper bound on number of */
/*               SPICE kernels that can be loaded at any time via the */
/*               KEEPER interface, but the number of kernels that can be */
/*               loaded may be smaller, since re-loading a loaded kernel */
/*               or meta-kernel results in creation of additional */
/*               database entries. */

/*               Kernels loaded into the KEEPER system are subject to */
/*               constraints imposed by lower-level subsystems. The */
/*               binary kernel systems (SPK, CK, binary PCK, and EK) */
/*               have their own limits on the maximum number of kernels */
/*               that may be loaded. */

/*               The total number of DAF-based files (this set includes */
/*               SPKs, CKs, and binary PCKs) that may be loaded at any */
/*               time may not exceed 1000.  This limit applies whether */
/*               the files are loaded via FURNSH or lower-level loaders */
/*               such as SPKLEF or DAFOPR.  File access performance */
/*               normally will degrade as the number of loaded kernels */
/*               increases. */

/*               The total number of DAS-based files that may be loaded */
/*               at any time is currently limited to 20 files. */

/* $ Exceptions */

/*     1) If the main routine KEEPER is called, the error */
/*       'SPICE(BOGUSENTRY)' will be signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine serves as an umbrella for a collection of */
/*     entry points that unify the task of loading, tracking, */
/*     and unloading SPICE kernels.  A description of each entry */
/*     point is given below: */

/*     FURNSH    Furnish a kernel to a program.  This entry point */
/*               provides a single interface for loading kernels into */
/*               your application program.  All SPICE kernels (Text */
/*               kernels, SPK, CK, Binary PCK, and EK) can be loaded */
/*               through this entry point.  In addition, special text */
/*               kernels, called meta-Text kernels, that contain a list */
/*               of other kernels to load can be processed by FURNSH. */

/*               Meta-text kernels allow you to easily control which */
/*               kernels will be loaded by your program without having */
/*               to write your own kernel managing routines. */

/*     KTOTAL    returns the number of kernels that are currently */
/*               available to your program as a result of previous calls */
/*               to FURNSH and UNLOAD. */

/*     KDATA     provides an interface for retrieving (in order of their */
/*               specification through FURNSH) kernels that are active in */
/*               your application. */

/*     KINFO     allows you to retrieve information about a loaded */
/*               kernel using the name of that kernel. */

/*     KCLEAR    Unloads all kernels that were loaded via the KEEPER */
/*               system, clears the kernel pool, and re-initializes the */
/*               KEEPER system. */

/*     UNLOAD    provides an interface for unloading kernels that have */
/*               been loaded via the routine FURNSH. */

/*     For more details concerning any particular entry point, see the */
/*     header for that entry point. */

/* $ Examples */

/*     The code fragment below illustrates the use of the various entry */
/*     points of KEEPER.  The details of creating meta-text kernels are */
/*     not discussed here, but are spelled out in the entry point */
/*     FURNSH. */


/*     Load several kernels into the program. */


/*     CALL FURNSH ( 'myspk.bsp'    ) */
/*     CALL FURNSH ( 'myck.bc'      ) */
/*     CALL FURNSH ( 'leapsecs.ker' ) */
/*     CALL FURNSH ( 'sclk.tsc'     ) */
/*     CALL FURNSH ( 'metatext.ker' ) */

/*     See how many kernels have been loaded. */

/*     CALL KTOTAL ( 'ALL', COUNT ) */

/*     WRITE (*,*) 'The total number of kernels is: ', COUNT */

/*     Summarize the kernels and types. */

/*     DO WHICH = 1, COUNT */

/*        CALL KDATA( WHICH, 'ALL', FILE, FILTYP, SOURCE, HANDLE, FOUND ) */

/*        IF ( .NOT. FOUND ) THEN */

/*           WRITE (*,*) 'This is NOT supposed to happen.  Call NAIF' */
/*           WRITE (*,*) 'and let them know of this problem.' */

/*        ELSE */

/*           WRITE (*,*) */
/*           WRITE (*,*) 'File  : ', FILE */
/*           WRITE (*,*) 'Type  : ', FILTYP */
/*           WRITE (*,*) 'Handle: ', HANDLE */

/*           IF ( SOURCE .NE. ' ' ) THEN */
/*              WRITE (*,*) 'This file was loaded via meta-text kernel:' */
/*              WRITE (*,*) SOURCE */
/*           END IF */

/*        END IF */

/*     END DO */


/*     Unload the first kernel we loaded. */

/*     CALL UNLOAD ( 'myspk.bsp' ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     C.H. Acton      (JPL) */
/*     N.J. Bachman    (JPL) */
/*     W.L. Taber      (JPL) */
/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.1.0, 01-JUL-2014 (NJB) (BVS) */

/*        Updated the discussion of kernel variable watches in entry */
/*        points KCLEAR and UNLOAD. Added to the FURNSH header mention */
/*        of the effects of failure during text kernel or meta-kernel */
/*        loading. */

/*     Last update was 12-APR-2012 (BVS) */

/*        Increased FTSIZE (from 1000 to 5000). */

/*        Changed to use SEPOOL instead of STPOOL to reduce loading time */
/*        for large meta-kernels due to n^2 delay in STPOOL. */

/* -    SPICELIB Version 4.0.2, 13-APR-2011 (EDW) */

/*        Trivial edit to KCLEAR Restrictions, replaced P*POOL with */
/*        PXPOOL. The "*" character causes the HTML documentation */
/*        script to create a link for the "POOL" substring. */

/* -    SPICELIB Version 4.0.1, 10-FEB-2010 (EDW) */

/*        Added mention of the restriction on kernel pool variable */
/*        names to MAXLEN (defined in pool.f) characters or less. */

/* -    SPICELIB Version 4.0.0, 02-APR-2009 (NJB) */

/*        Continued path values are now supported. FURNSH now rejects */
/*        file names longer than FILSIZ characters. */

/*        Deleted references to unneeded variable DOALL. Made */
/*        THSTYP declaration compatible with TYPES array. */

/* -    SPICELIB Version 3.0.1, 27-APR-2007 (NJB) */

/*        Fixed header typo: added quotes to literal string */
/*        input arguments in example FURNSH calls. */

/* -    SPICELIB Version 3.0.0, 15-NOV-2006 (NJB) */

/*        Added entry point KCLEAR. Bug fix:  meta-kernel unloading bug */
/*        in UNLOAD was corrected. Some header updates were made. */

/* -    SPICELIB Version 2.0.2, 29-JUL-2003 (NJB) (CHA) */

/*        Only the header of the entry point FURNSH was modified. */
/*        Numerous updates were made to improve clarity.  Some */
/*        corrections were made. */

/* -    SPICELIB VERSION 2.0.1, 06-DEC-2002 (NJB) */

/*        Typo in header example was corrected. */

/* -    SPICELIB VERSION 2.0.0, 07-JAN-2002 (WLT) */

/*        Added a call to CVPOOL in FURNSH so that watches that are */
/*        triggered are triggered by loading Meta-kernels and not by */
/*        some external interaction with the kernel pool. */

/*        Added code to make sure that UNLOAD has the effect of */
/*        loading all remaining kernels in the order they were first */
/*        introduced. */

/* -    SPICELIB Version 1.1.0, 19-SEP-2000 (WLT) */

/*        Corrected the error message template used */
/*        by ZZLDKER */

/* -    SPICELIB Version 1.0.1, 16-DEC-1999 (NJB) */

/*        Documentation fix:  corrected second code example in the */
/*        header of the entry point FURNSH.  The example previously used */
/*        the kernel variable PATH_NAMES; that name has been replaced */
/*        with the correct name PATH_VALUES. */

/* -    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Generic loading and unloading of SPICE kernels */

/* -& */

/*     SPICELIB Functions */


/*     Here we set up the database of loaded kernels */

/*     The name of every file loaded through this interface will */
/*     be stored in the array FILES. */


/*     The handle of every loaded file will be stored in the array */
/*     HANDLS.  If the file is a text kernel it will be assigned the */
/*     handle 0. */


/*     The source of each file specified will be stored in the integer */
/*     array SOURCE.  If the file is loaded directly, its source */
/*     will be zero.  If it is loaded as the result of meta-information */
/*     in a text kernel, the index of the source file in FILES will */
/*     be stored in SRCES. */


/*     The file type of every loaded kernel will be stored in the array */
/*     TYPES. */


/*     The number of files loaded through this interfaces is kept in the */
/*     integer LOADED. */

    switch(n__) {
	case 1: goto L_furnsh;
	case 2: goto L_ktotal;
	case 3: goto L_kdata;
	case 4: goto L_kinfo;
	case 5: goto L_kclear;
	case 6: goto L_unload;
	}

    chkin_("KEEPER", (ftnlen)6);
    setmsg_("The routine KEEPER is an umbrella for a collection of entry poi"
	    "nts that manage the loading, tracking and unloading of SPICE ker"
	    "nels.  KEEPER should not be called directly. It is likely that a"
	    " programming error has been made. ", (ftnlen)225);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("KEEPER", (ftnlen)6);
    return 0;
/* $Procedure      FURNSH ( Furnish a program with SPICE kernels ) */

L_furnsh:
/* $ Abstract */

/*     Load one or more SPICE kernels into a program. */

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

/*      None. */

/* $ Keywords */

/*      UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         FILE */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FILE       I   SPICE kernel file (text or binary). */
/*     FILSIZ     P   Maximum file name length. */

/* $ Detailed_Input */

/*     FILE       is a SPICE kernel file.  The file may be either binary */
/*                or text. If the file is a binary SPICE kernel it will */
/*                be loaded into the appropriate SPICE subsystem.  If */
/*                FILE is a SPICE text kernel it will be loaded into the */
/*                kernel pool. If FILE is a SPICE meta-kernel containing */
/*                initialization instructions (through use of the */
/*                correct kernel pool variables), the files specified in */
/*                those variables will be loaded into the appropriate */
/*                SPICE subsystem. */

/*                The SPICE text kernel format supports association of */
/*                names and data values using a "keyword = value" */
/*                format. The keyword-value pairs thus defined are */
/*                called "kernel variables." */

/*                While any information can be placed in a text kernel */
/*                file, the following string valued kernel variables are */
/*                recognized by SPICE as meta-kernel keywords: */

/*                     KERNELS_TO_LOAD */
/*                     PATH_SYMBOLS */
/*                     PATH_VALUES */

/*                Each kernel variable is discussed below. */

/*                KERNELS_TO_LOAD   is a list of SPICE kernels to be */
/*                                  loaded into a program.  If file */
/*                                  names do not fit within the kernel */
/*                                  pool 80 character limit, they may be */
/*                                  continued to subsequent array */
/*                                  elements by placing the continuation */
/*                                  character ('+') at the end of an */
/*                                  element and then placing the */
/*                                  remainder of the file name in the */
/*                                  next array element.  (See the */
/*                                  examples below for an illustration */
/*                                  of this technique or consult the */
/*                                  routine STPOOL for further details.) */

/*                                  You may use one or more PATH_SYMBOL */
/*                                  assignments (see below) to specify */
/*                                  strings to be substituted for some */
/*                                  part of a file name. */

/*                PATH_SYMBOLS      is a list of strings (without */
/*                                  embedded blanks) which if */
/*                                  encountered following the '$' */
/*                                  character will be replaced with the */
/*                                  corresponding PATH_VALUES string. */
/*                                  Note that PATH_SYMBOLS are */
/*                                  interpreted only in values */
/*                                  associated with the KERNELS_TO_LOAD */
/*                                  variable. There must be a one-to-one */
/*                                  correspondence between the values */
/*                                  supplied for PATH_SYMBOLS and */
/*                                  PATH_VALUES. For the purpose of */
/*                                  determining this correspondence, any */
/*                                  path value that is continued over */
/*                                  multiple array elements counts as a */
/*                                  single value. */

/*                PATH_VALUES       is a list of expansions to use when */
/*                                  PATH_SYMBOLS are encountered. If */
/*                                  path values do not fit within the */
/*                                  kernel pool 80 character limit, they */
/*                                  may be continued in the same way as */
/*                                  file names (see the KERNELS_TO_LOAD */
/*                                  description above). */

/*               These kernel pool variables persist within the kernel */
/*               pool only until all kernels associated with the */
/*               variable KERNELS_TO_LOAD have been loaded.  Once all */
/*               specified kernels have been loaded, the variables */
/*               KERNELS_TO_LOAD, PATH_SYMBOLS and PATH_VALUES are */
/*               removed from the kernel pool. */

/* $ Detailed_Output */

/*     None. The routine loads various SPICE kernels for use by your */
/*     application. */

/* $ Parameters */

/*     FILSIZ    is the maximum file name length that can be */
/*               accommodated by this routine. */

/*     MAXFIL    is the number of entries that can be stored in KEEPER's */
/*               kernel database.  Each time a kernel is loaded via */
/*               FURNSH, a database entry is created for that kernel. */
/*               If a meta-kernel is loaded, a database entry is created */
/*               for the meta-kernel itself and for all files referenced */
/*               in the meta-kernel's KERNELS_TO_LOAD specification. */
/*               Unloading a kernel or meta-kernel deletes database */
/*               entries created when the file was loaded. */

/*               The parameter MAXFIL is an upper bound on number of */
/*               SPICE kernels that can be loaded at any time via the */
/*               KEEPER interface, but the number of kernels that can be */
/*               loaded may be smaller, since re-loading a loaded kernel */
/*               or meta-kernel results in creation of additional */
/*               database entries. */

/*               Kernels loaded into the KEEPER system are subject to */
/*               constraints imposed by lower-level subsystems. The */
/*               binary kernel systems (SPK, CK, binary PCK, and EK) */
/*               have their own limits on the maximum number of kernels */
/*               that may be loaded. */

/*               The total number of DAF-based files (this set includes */
/*               SPKs, CKs, and binary PCKs) that may be loaded at any */
/*               time may not exceed 1000.  This limit applies whether */
/*               the files are loaded via FURNSH or lower-level loaders */
/*               such as SPKLEF or DAFOPR.  File access performance */
/*               normally will degrade as the number of loaded kernels */
/*               increases. */

/*               The total number of DAS-based files that may be loaded */
/*               at any time is currently limited to 20 files. */

/* $ Exceptions */

/*     1) If a problem is encountered while trying to load FILE, */
/*        it will be diagnosed by a routine in the call tree of this */
/*        routine. */

/*     2) If the input FILE is a meta-kernel and some file in the */
/*        KERNELS_TO_LOAD assignment cannot be found, or if an error */
/*        occurs while trying to load a file specified by this */
/*        assignment, the error will be diagnosed by a routine in the */
/*        call tree of this routine, and this routine will return. Any */
/*        files loaded prior to encountering the missing file will */
/*        remain loaded. */

/*     3) If an attempt to load a text kernel fails while the kernel is */
/*        being parsed, any kernel variable assignments made before */
/*        the failure occurred will be retained in the kernel pool. */

/*     4) If a PATH_SYMBOLS assignment is specified without a */
/*        corresponding PATH_VALUES assignment, the error */
/*        SPICE(NOPATHVALUE) will be signaled. */

/*     5) If a meta-text kernel is supplied to FURNSH that contains */
/*        instructions specifying that another meta-text kernel be */
/*        loaded, the error SPICE(RECURSIVELOADING) will be signaled. */

/*     6) If the input file name has non-blank length exceeding FILSIZ */
/*        characters, the error SPICE(FILENAMETOOLONG) is signaled. */

/*     7) If the input file is a meta-kernel and some file in the */
/*        KERNELS_TO_LOAD assignment has name length exceeding FILSIZ */
/*        characters, the error SPICE(FILENAMETOOLONG) is signaled. */

/*     8) If the input file is a meta-kernel and some value in the */
/*        PATH_VALUES assignment has length exceeding FILSIZ */
/*        characters, the error SPICE(PATHTOOLONG) is signaled. */

/*     9) If the input file is a meta-kernel and some file in the */
/*        KERNELS_TO_LOAD assignment has, after symbol substitution, */
/*        combined name and path length exceeding FILSIZ characters, the */
/*        error SPICE(FILENAMETOOLONG) is signaled. */

/*    10) The error 'SPICE(BADVARNAME)' signals from a routine in the */
/*        call tree of FURNSH if a kernel pool variable name length */
/*        exceeds MAXLEN characters (defined in pool.f). */


/* $ Files */

/*     The input FILE is examined and loaded into the appropriate SPICE */
/*     subsystem.  If the file is a meta-kernel, any kernels specified */
/*     by the KERNELS_TO_LOAD keyword (and if present, the PATH_SYMBOLS */
/*     and PATH_VALUES keywords) are loaded as well. */

/* $ Particulars */

/*     This routine provides a uniform interface to the SPICE kernel */
/*     loading systems.  It allows you to easily assemble a list of */
/*     SPICE kernels required by your application and to modify that set */
/*     without modifying the source code of programs that make use of */
/*     these kernels. */

/* $ Examples */

/*     Example 1 */
/*     --------- */

/*     Load the leapseconds kernel naif0007.tls and the planetary */
/*     ephemeris SPK file de405s.bsp. */

/*        CALL FURNSH ( 'naif0007.tls' ) */
/*        CALL FURNSH ( 'de405s.bsp'   ) */


/*     Example 2 */
/*     --------- */

/*     This example illustrates how you could create a meta-kernel file */
/*     for a program that requires several text and binary kernels. */

/*     First create a list of the kernels you need in a text file as */
/*     shown below. */

/*        \begintext */

/*           Here are the SPICE kernels required for my application */
/*           program. */

/*           Note that kernels are loaded in the order listed. Thus we */
/*           need to list the highest priority kernel last. */


/*        \begindata */

/*        KERNELS_TO_LOAD = ( */

/*              '/home/mydir/kernels/spk/lowest_priority.bsp', */
/*              '/home/mydir/kernels/spk/next_priority.bsp', */
/*              '/home/mydir/kernels/spk/highest_priority.bsp', */
/*              '/home/mydir/kernels/text/leapsecond.ker', */
/*              '/home/mydir/kernels+', */
/*              '/custom+', */
/*              '/kernel_data/constants.ker', */
/*              '/home/mydir/kernels/text/sclk.tsc', */
/*              '/home/mydir/kernels/ck/c-kernel.bc' ) */


/*     Note that the file name */

/*        /home/mydir/kernels/custom/kernel_data/constants.ker */

/*     is continued across several lines in the right hand side of the */
/*     assignment of the kernel variable KERNELS_TO_LOAD. */

/*     Once you've created your list of kernels, call FURNSH near the */
/*     beginning of your application program to load the meta-kernel */
/*     automatically at program start up. */

/*        CALL FURNSH ( 'myfile.txt' ) */

/*     This will cause each of the kernels listed in your meta-kernel */
/*     to be loaded. */


/*     Example 3 */
/*     --------- */

/*     This example illustrates how you can simplify the previous */
/*     kernel list by using PATH_SYMBOLS. */


/*        \begintext */

/*           Here are the SPICE kernels required for my application */
/*           program. */

/*           We are going to let A substitute for the directory that */
/*           contains SPK files; B substitute for the directory that */
/*           contains C-kernels; and C substitute for the directory that */
/*           contains text kernels.  And we'll let D substitute for */
/*           a "custom" directory that contains a special planetary */
/*           constants kernel made just for our mission. */

/*           Note that our PATH_VALUES and the corresponding */
/*           PATH_SYMBOLS must be listed in the same order. */


/*        \begindata */

/*        PATH_VALUES  = ( '/home/mydir/kernels/spk', */
/*                         '/home/mydir/kernels/ck', */
/*                         '/home/mydir/kernels/text', */
/*                         '/home/mydir/kernels/custom/kernel_data' ) */

/*        PATH_SYMBOLS = ( 'A', */
/*                         'B', */
/*                         'C', */
/*                         'D'  ) */

/*        KERNELS_TO_LOAD = (  '$A/lowest_priority.bsp', */
/*                             '$A/next_priority.bsp', */
/*                             '$A/highest_priority.bsp', */
/*                             '$C/leapsecond.ker', */
/*                             '$D/constants.ker', */
/*                             '$C/sclk.tsc', */
/*                             '$B/c-kernel.bc'         ) */


/*     Example 4 */
/*     --------- */

/*     This example illustrates continuation of path values. The */
/*     meta-kernel shown here is a modified version of that from */
/*     example 3. */

/*        \begintext */

/*           Here are the SPICE kernels required for my application */
/*           program. */

/*           We are going to let A substitute for the directory that */
/*           contains SPK files; B substitute for the directory that */
/*           contains C-kernels; and C substitute for the directory that */
/*           contains text kernels.  And we'll let D substitute for */
/*           a "custom" directory that contains a special planetary */
/*           constants kernel made just for our mission. */

/*           Note that our PATH_VALUES and the corresponding */
/*           PATH_SYMBOLS must be listed in the same order. */

/*           The values for path symbols A and D are continued over */
/*           multiple lines. */

/*        \begindata */

/*        PATH_VALUES  = ( '/very_long_top_level_path_name/mydir/+', */
/*                         'kernels/spk', */
/*                         '/home/mydir/kernels/ck', */
/*                         '/home/mydir/kernels/text', */
/*                         '/very_long_top_level_path_name+', */
/*                         '/mydir/kernels/custom+', */
/*                         '/kernel_data'                ) */

/*        PATH_SYMBOLS = ( 'A', */
/*                         'B', */
/*                         'C', */
/*                         'D'  ) */

/*        KERNELS_TO_LOAD = (  '$A/lowest_priority.bsp', */
/*                             '$A/next_priority.bsp', */
/*                             '$A/highest_priority.bsp', */
/*                             '$C/leapsecond.ker', */
/*                             '$D/constants.ker', */
/*                             '$C/sclk.tsc', */
/*                             '$B/c-kernel.bc'         ) */

/* $ Restrictions */

/*     1) A meta-kernel cannot reference another meta-kernel. */

/*     2) Failure during an attempt to load a text kernel or a */
/*        meta-kernel can result in a subset of the intended kernel */
/*        variables being set or a subset of the intended files */
/*        being loaded. FURNSH does not "clean up" so as to undo the */
/*        effects of a failed load operation. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     C.H. Acton      (JPL) */
/*     N.J. Bachman    (JPL) */
/*     W.L. Taber      (JPL) */
/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.1.0, 01-JUL-2014 (NJB) (BVS) */

/*        Updated discussion of partially completed kernel loading. */

/*     Last update was 12-APR-2012 (BVS) */

/*        Changed to use SEPOOL instead of STPOOL to reduce loading time */
/*        for large meta-kernels due to n^2 delay in STPOOL. */

/* -    SPICELIB Version 4.0.1, 10-FEB-2010 (EDW) */

/*        Added mention of the restriction on kernel pool variable */
/*        names to MAXLEN (defined in pool.f) characters or less. */

/* -    SPICELIB Version 4.0.0, 02-APR-2009 (NJB) */

/*        Continued path values are now supported. FURNSH now rejects */
/*        file names longer than FILSIZ characters. */

/* -    SPICELIB Version 2.0.3, 27-APR-2007 (NJB) */

/*        Fixed header typo: added quotes to literal string */
/*        input arguments in example FURNSH calls. */

/* -    SPICELIB Version 2.0.2, 15-NOV-2006 (NJB) */

/*        Added description of parameter MAXFIL to header. */

/* -    SPICELIB Version 2.0.1, 29-JUL-2003 (NJB) (CHA) */

/*        Numerous updates to improve clarity.  Some corrections were */
/*        made. */

/* -    SPICELIB VERSION 2.0.0, 23-AUG-2001 (WLT) */

/*        Added a call to CVPOOL in FURNSH so that watches that are */
/*        triggered are triggered by loading Meta-kernels and not by */
/*        some external interaction with the kernel pool. */

/* -    SPICELIB Version 1.1.0, 19-SEP-2000 (WLT) */

/*        Corrected the error message template used */
/*        by ZZLDKER */

/* -    SPICELIB Version 1.0.1, 16-DEC-1999 (NJB) */

/*        Documentation fix:  corrected second code example in the */
/*        header of this entry point.  The example previously used the */
/*        kernel variable PATH_NAMES; that name has been replaced with */
/*        the correct name PATH_VALUES. */

/* -    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Load SPICE kernels from a list of kernels */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("FURNSH", (ftnlen)6);
    if (first) {
	first = FALSE_;
	s_copy(known, "KERNELS_TO_LOAD", (ftnlen)32, (ftnlen)15);
	s_copy(known + 32, "PATH_SYMBOLS", (ftnlen)32, (ftnlen)12);
	s_copy(known + 64, "PATH_VALUES", (ftnlen)32, (ftnlen)11);
	loaded = 0;
	swpool_("FURNSH", &c__3, known, (ftnlen)6, (ftnlen)32);
	cvpool_("FURNSH", &update, (ftnlen)6);
    }

/*     Reject excessively long file names. */

    if (rtrim_(file, file_len) > 255) {
	setmsg_("Input file name <#> has length @ characters. The limit on t"
		"he length of file names stored by FURNSH is @ characters.", (
		ftnlen)116);
	errch_("#", file, (ftnlen)1, file_len);
	i__1 = rtrim_(file, file_len);
	errint_("@", &i__1, (ftnlen)1);
	errint_("@", &c__255, (ftnlen)1);
	sigerr_("SPICE(FILENAMETOOLONG)", (ftnlen)22);
	chkout_("FURNSH", (ftnlen)6);
	return 0;
    }

/*     Make sure we have room to load at least one more file. */

    if (loaded == 5300) {
	setmsg_("There is no room left in KEEPER to load another SPICE kerne"
		"l.  The current limit on the number of files that can be loa"
		"ded is #.  If you really need more than this many files, you"
		" should increase the parameter MAXFIL in the subroutine KEEP"
		"ER. ", (ftnlen)243);
	errint_("#", &c__5300, (ftnlen)1);
	sigerr_("SPICE(NOMOREROOM)", (ftnlen)17);
	chkout_("FURNSH", (ftnlen)6);
	return 0;
    }

/*     We don't want external interactions with the kernel pool to */
/*     have any affect on FURNSH's watch so we check the watcher */
/*     here prior to the call to ZZLDKER. */

    cvpool_("FURNSH", &update, (ftnlen)6);

/*     Set a preliminary value for the error message in case the */
/*     call to ZZLDKER doesn't succeed. */

    s_copy(nofile, "The attempt to load \"#\" by the routine FURNSH failed. "
	    "It #", (ftnlen)500, (ftnlen)58);
    zzldker_(file, nofile, thstyp, &myhand, file_len, (ftnlen)500, (ftnlen)8);
    if (failed_()) {
	chkout_("FURNSH", (ftnlen)6);
	return 0;
    }
    ++loaded;
    cursrc = loaded;
    s_copy(files + ((i__1 = loaded - 1) < 5300 && 0 <= i__1 ? i__1 : s_rnge(
	    "files", i__1, "keeper_", (ftnlen)1045)) * 255, file, (ftnlen)255,
	     file_len);
    s_copy(types + (((i__1 = loaded - 1) < 5300 && 0 <= i__1 ? i__1 : s_rnge(
	    "types", i__1, "keeper_", (ftnlen)1046)) << 3), thstyp, (ftnlen)8,
	     (ftnlen)8);
    handls[(i__1 = loaded - 1) < 5300 && 0 <= i__1 ? i__1 : s_rnge("handls", 
	    i__1, "keeper_", (ftnlen)1047)] = myhand;
    srces[(i__1 = loaded - 1) < 5300 && 0 <= i__1 ? i__1 : s_rnge("srces", 
	    i__1, "keeper_", (ftnlen)1048)] = 0;
    cvpool_("FURNSH", &update, (ftnlen)6);
    if (! update) {

/*        Nothing to do.  None of the control variables */
/*        were set in FILE. */

	chkout_("FURNSH", (ftnlen)6);
	return 0;
    }

/*     See what is present in the kernel pool: Are any path symbols */
/*     defined? */

    dtpool_("PATH_SYMBOLS", &paths, &npaths, norc, (ftnlen)12, (ftnlen)1);
    if (paths && *(unsigned char *)norc == 'C') {

/*        Make sure that the values are equal in number. We need to */
/*        use STPOOL to count the path values, since some of them */
/*        might span multiple array elements. */

	i__ = 1;
	stpool_("PATH_VALUES", &i__, "+", pvalue, &size, &ok, (ftnlen)11, (
		ftnlen)1, (ftnlen)255);
	while(ok && ! failed_()) {

/*           Reject excessively long path names. */

	    if (size > 255) {
		setmsg_("In meta-kernel <#>, the path at index # in the PATH"
			"_VALUES list has length # characters; the limit is #"
			" characters.", (ftnlen)115);
		errch_("#", file, (ftnlen)1, file_len);
		errint_("#", &i__, (ftnlen)1);
		errint_("#", &size, (ftnlen)1);
		errint_("#", &c__255, (ftnlen)1);
		sigerr_("SPICE(PATHTOOLONG)", (ftnlen)18);
		chkout_("FURNSH", (ftnlen)6);
		return 0;
	    }
	    ++i__;
	    stpool_("PATH_VALUES", &i__, "+", pvalue, &size, &ok, (ftnlen)11, 
		    (ftnlen)1, (ftnlen)255);
	}
	if (failed_()) {
	    chkout_("FURNSH", (ftnlen)6);
	    return 0;
	}
	npvals = i__ - 1;
	if (npvals != npaths) {
	    setmsg_("Number of path symbols is #; number of path values is #"
		    "; counts must match.", (ftnlen)75);
	    errint_("#", &npaths, (ftnlen)1);
	    errint_("#", &npvals, (ftnlen)1);
	    sigerr_("SPICE(PATHMISMATCH)", (ftnlen)19);
	    chkout_("FURNSH", (ftnlen)6);
	    return 0;
	}
    } else {
	paths = FALSE_;
    }

/*     This kernel appears to be a legitimate meta-text kernel. Mark */
/*     it as such and then process its contents. */

    s_copy(types + (((i__1 = loaded - 1) < 5300 && 0 <= i__1 ? i__1 : s_rnge(
	    "types", i__1, "keeper_", (ftnlen)1131)) << 3), "META", (ftnlen)8,
	     (ftnlen)4);

/*     Now load all kernels specified in the KERNELS_TO_LOAD variable. */

    filnum = 1;
    fidx = 1;
    sepool_("KERNELS_TO_LOAD", &fidx, "+", fil2ld, &fnmlen, &lidx, &ok, (
	    ftnlen)15, (ftnlen)1, (ftnlen)255);
    while(ok && ! failed_()) {

/*        Reject excessively long file names. */

	if (fnmlen > 255) {
	    setmsg_("In meta-kernel <#>, the file name at index # in the KER"
		    "NELS_TO_LOAD list has length # characters; the limit is "
		    "# characters.", (ftnlen)124);
	    errch_("#", file, (ftnlen)1, file_len);
	    errint_("#", &filnum, (ftnlen)1);
	    errint_("#", &fnmlen, (ftnlen)1);
	    errint_("#", &c__255, (ftnlen)1);
	    sigerr_("SPICE(FILENAMETOOLONG)", (ftnlen)22);
	    chkout_("FURNSH", (ftnlen)6);
	    return 0;
	}

/*        Make sure we have room to load at least one more file. */

	if (loaded == 5300) {
	    setmsg_("There is no room left in KEEPER to load another SPICE k"
		    "ernel. The current limit on the number of files that can"
		    " be loaded is #.", (ftnlen)127);
	    errint_("#", &c__5300, (ftnlen)1);
	    sigerr_("SPICE(NOMOREROOM)", (ftnlen)17);
	    chkout_("FURNSH", (ftnlen)6);
	    return 0;
	}

/*        Resolve any path symbols that may be present. */
/*        Make sure we have room to load at least one more file. */

	if (paths) {
	    start = 1;
	    dollar = pos_(fil2ld, "$", &start, (ftnlen)255, (ftnlen)1);
	    while(dollar > 0) {

/*              Determine the longest path symbol that fits into the */
/*              current file name.  We fetch path symbols one at a */
/*              time and see if they match the portion of the */
/*              string that follows the '$'.  The longest match */
/*              is the one we use as a symbol. */

		size = 0;
		use = 0;
		d__ = dollar;
		i__1 = npaths;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    gcpool_("PATH_SYMBOLS", &i__, &c__1, &n, symbol, &fnd, (
			    ftnlen)12, (ftnlen)80);
		    r__ = rtrim_(symbol, (ftnlen)80);
		    i__2 = d__ + 1;
		    i__3 = d__ + r__;
		    if (r__ > size && samsub_(symbol, &c__1, &r__, fil2ld, &
			    i__2, &i__3, (ftnlen)80, (ftnlen)255)) {
			use = i__;
			size = r__;
		    }
		}

/*              If we found a matching path symbol, get the corresponding */
/*              value and put it into the file name. */

		if (use > 0) {

/*                 Get the path value having index USE in the set of */
/*                 path values. Note that we've already checked that */
/*                 the path value will fit in PVALUE. */

		    stpool_("PATH_VALUES", &use, "+", pvalue, &n, &fnd, (
			    ftnlen)11, (ftnlen)1, (ftnlen)255);

/*                 When the path is substituted for the symbol, the */
/*                 total length of the path and file name must fit in */
/*                 the name buffer. */

		    if (fnmlen + n - size - 1 > 255) {
			setmsg_("In meta-kernel <#>, the path at index # in "
				"the PATH_SYMBOLS list has # characters and t"
				"he file name at index # has # characters. Th"
				"e combined path and file name has # characte"
				"rs; the limit is # characters.", (ftnlen)205);
			errch_("#", file, (ftnlen)1, file_len);
			errint_("#", &use, (ftnlen)1);
			errint_("#", &n, (ftnlen)1);
			errint_("#", &filnum, (ftnlen)1);
			errint_("#", &fnmlen, (ftnlen)1);
			i__1 = fnmlen + n;
			errint_("#", &i__1, (ftnlen)1);
			errint_("#", &c__255, (ftnlen)1);
			sigerr_("SPICE(FILENAMETOOLONG)", (ftnlen)22);
			chkout_("FURNSH", (ftnlen)6);
			return 0;
		    }
		    i__1 = d__ + size;
		    repsub_(fil2ld, &d__, &i__1, pvalue, fil2ld, (ftnlen)255, 
			    n, (ftnlen)255);
		}

/*              Look for the next occurrence of a '$' after the last */
/*              place we found one. */

		start = dollar + 1;
		dollar = pos_(fil2ld, "$", &start, (ftnlen)255, (ftnlen)1);
	    }
	}

/*        If any path symbols were present, they have now been */
/*        resolved.  Let ZZLDKER handle the task of loading this */
/*        kernel.  Make up a message template for use if ZZLDKER */
/*        runs into a problem. */

	s_copy(nofile, "The @ file '#' specified by KERNELS_TO_LOAD in the f"
		"ile @ #", (ftnlen)500, (ftnlen)59);
	repmot_(nofile, "@", &filnum, "L", nofile, (ftnlen)500, (ftnlen)1, (
		ftnlen)1, (ftnlen)500);
	repmc_(nofile, "@", file, nofile, (ftnlen)500, (ftnlen)1, file_len, (
		ftnlen)500);
	zzldker_(fil2ld, nofile, thstyp, &myhand, (ftnlen)255, (ftnlen)500, (
		ftnlen)8);
	if (failed_()) {
	    chkout_("FURNSH", (ftnlen)6);
	    return 0;
	}
	if (s_cmp(thstyp, "TEXT", (ftnlen)8, (ftnlen)4) == 0) {

/*           See if we stepped on any of the recognized variables.  If */
/*           we did, there's no point in trying to continue. */

	    cvpool_("FURNSH", &update, (ftnlen)6);
	    if (update) {

/*              First clean up the debris created by this attempt */
/*              at recursion. */

		for (i__ = 1; i__ <= 3; ++i__) {
		    dvpool_(known + (((i__1 = i__ - 1) < 3 && 0 <= i__1 ? 
			    i__1 : s_rnge("known", i__1, "keeper_", (ftnlen)
			    1298)) << 5), (ftnlen)32);
		}

/*              Take care of any watcher activation caused by the */
/*              mop-up of the preceding loop. */

		cvpool_("FURNSH", &update, (ftnlen)6);
		setmsg_("Hmmm.  This is interesting. In the meta-text kernel"
			" '#' you've requested that the text kernel '#' be lo"
			"aded. This second file is also a \"meta-text\" kerne"
			"l and specifies new kernel loading instructions. Alt"
			"hough you receive high marks for creativity, this pa"
			"th is fraught with peril and can not be supported by"
			" FURNSH. ", (ftnlen)318);
		errch_("#", file, (ftnlen)1, file_len);
		errch_("#", fil2ld, (ftnlen)1, (ftnlen)255);
		sigerr_("SPICE(RECURSIVELOADING)", (ftnlen)23);
		chkout_("FURNSH", (ftnlen)6);
		return 0;
	    }
	}

/*        Add the latest file loaded to our database of loaded */
/*        files. */

	++loaded;
	s_copy(files + ((i__1 = loaded - 1) < 5300 && 0 <= i__1 ? i__1 : 
		s_rnge("files", i__1, "keeper_", (ftnlen)1330)) * 255, fil2ld,
		 (ftnlen)255, (ftnlen)255);
	s_copy(types + (((i__1 = loaded - 1) < 5300 && 0 <= i__1 ? i__1 : 
		s_rnge("types", i__1, "keeper_", (ftnlen)1331)) << 3), thstyp,
		 (ftnlen)8, (ftnlen)8);
	handls[(i__1 = loaded - 1) < 5300 && 0 <= i__1 ? i__1 : s_rnge("hand"
		"ls", i__1, "keeper_", (ftnlen)1332)] = myhand;
	srces[(i__1 = loaded - 1) < 5300 && 0 <= i__1 ? i__1 : s_rnge("srces",
		 i__1, "keeper_", (ftnlen)1333)] = cursrc;

/*        Get the name of the next file to load. */

	++filnum;
	fidx = lidx + 1;
	sepool_("KERNELS_TO_LOAD", &fidx, "+", fil2ld, &fnmlen, &lidx, &ok, (
		ftnlen)15, (ftnlen)1, (ftnlen)255);
    }

/*     Last Step.  Remove the special variables from the kernel pool. */

    for (i__ = 1; i__ <= 3; ++i__) {
	dvpool_(known + (((i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		"known", i__1, "keeper_", (ftnlen)1350)) << 5), (ftnlen)32);
    }
    cvpool_("FURNSH", &update, (ftnlen)6);
    chkout_("FURNSH", (ftnlen)6);
    return 0;
/* $Procedure      KTOTAL ( Kernel Totals ) */

L_ktotal:
/* $ Abstract */

/*     Return the number of kernels that are currently loaded */
/*     via the KEEPER interface and that are of a specified type. */

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

/* $ Declarations */

/*     CHARACTER*(*)         KIND */
/*     INTEGER               COUNT */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     KIND       I   A list of kinds of kernels to count. */
/*     COUNT      O   The number of kernels of type KIND. */

/* $ Detailed_Input */

/*     KIND       is a list of types of kernels to count when */
/*                computing loaded kernels.  KIND should consist */
/*                of a list of words of kernels to examine.  Recognized */
/*                types are */

/*                   SPK  --- all SPK files are counted in the total. */
/*                   CK   --- all CK files are counted in the total. */
/*                   PCK  --- all binary PCK files are counted in the */
/*                            total. */
/*                   EK   --- all EK files are counted in the total. */
/*                   TEXT --- all text kernels that are not meta-text */
/*                            kernels are included in the total. */
/*                   META --- all meta-text kernels are counted in the */
/*                            total. */
/*                   ALL  --- every type of kernel is counted in the */
/*                            total. */

/*                 KIND is case insensitive.  If a word appears in KIND */
/*                 that is not one of those listed above it is ignored. */

/*                 See the Examples section for illustrations of the */
/*                 use of KIND. */

/* $ Detailed_Output */

/*     COUNT       is the number of kernels loaded through FURNSH that */
/*                 belong to the list specified by KIND. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If a word on the list specified by KIND is not recognized */
/*        it is ignored. */

/*     2) If KIND is blank, or none of the words in KIND is on the */
/*        list specified above, COUNT will be returned as zero. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     KTOTAL allows you to easily determine the number of kernels */
/*     loaded via the interface FURNSH that are of a type of interest. */

/* $ Examples */

/*     Suppose you wish to determine the number of SPK kernels that */
/*     have been loaded via the interface FURNSH.  Assign KIND */
/*     the value 'SPK' and call KTOTAL as shown: */


/*        KIND = 'SPK' */
/*        CALL KTOTAL ( KIND, COUNT ) */

/*        WRITE (*,*) 'The number of loaded SPK files is: ', COUNT */

/*     To determine the number of text kernels that are loaded that */
/*     are not meta-kernels: */

/*        KIND = 'TEXT' */
/*        CALL KTOTAL ( KIND, NTEXT ) */

/*        WRITE (*,*) 'The number of non-meta-text kernels loaded is: ' */
/*       .             NTEXT */

/*     To determine the number of SPK, CK and PCK kernels loaded */
/*     make the following call: */

/*        KIND = 'SPK PCK CK' */
/*        CALL KTOTAL ( KIND, COUNT ) */


/*     To get a count of all loaded kernels */

/*        KIND = 'ALL' */
/*        CALL KTOTAL ( KIND, COUNT ) */

/*        WRITE (*,*) 'There are ', COUNT, ' SPICE kernels loaded.' */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 02-APR-2009 (NJB) */

/*        Deleted reference to unneeded variable DOALL. */

/* -    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Number of loaded kernels of a given type */

/* -& */
    if (loaded == 0) {
	*count = 0;
	return 0;
    }
    chkin_("KTOTAL", (ftnlen)6);

/*     Parse KIND to see which kernels are of interest. */

    dospk = FALSE_;
    dock = FALSE_;
    dotext = FALSE_;
    dometa = FALSE_;
    doek = FALSE_;
    dopck = FALSE_;
    start = 1;
    fndnwd_(kind, &start, &b, &e, kind_len);
    while(b > 0) {
	if (eqstr_(kind + (b - 1), "ALL", e - (b - 1), (ftnlen)3)) {
	    *count = loaded;
	    chkout_("KTOTAL", (ftnlen)6);
	    return 0;
	} else {
	    dock = dock || eqstr_(kind + (b - 1), "CK", e - (b - 1), (ftnlen)
		    2);
	    doek = doek || eqstr_(kind + (b - 1), "EK", e - (b - 1), (ftnlen)
		    2);
	    dometa = dometa || eqstr_(kind + (b - 1), "META", e - (b - 1), (
		    ftnlen)4);
	    dopck = dopck || eqstr_(kind + (b - 1), "PCK", e - (b - 1), (
		    ftnlen)3);
	    dospk = dospk || eqstr_(kind + (b - 1), "SPK", e - (b - 1), (
		    ftnlen)3);
	    dotext = dotext || eqstr_(kind + (b - 1), "TEXT", e - (b - 1), (
		    ftnlen)4);
	}
	start = e + 1;
	fndnwd_(kind, &start, &b, &e, kind_len);
    }
    *count = 0;
    i__1 = loaded;
    for (i__ = 1; i__ <= i__1; ++i__) {
	add = s_cmp(types + (((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 : 
		s_rnge("types", i__2, "keeper_", (ftnlen)1576)) << 3), "CK", (
		ftnlen)8, (ftnlen)2) == 0 && dock || s_cmp(types + (((i__3 = 
		i__ - 1) < 5300 && 0 <= i__3 ? i__3 : s_rnge("types", i__3, 
		"keeper_", (ftnlen)1576)) << 3), "EK", (ftnlen)8, (ftnlen)2) 
		== 0 && doek || s_cmp(types + (((i__4 = i__ - 1) < 5300 && 0 
		<= i__4 ? i__4 : s_rnge("types", i__4, "keeper_", (ftnlen)
		1576)) << 3), "META", (ftnlen)8, (ftnlen)4) == 0 && dometa || 
		s_cmp(types + (((i__5 = i__ - 1) < 5300 && 0 <= i__5 ? i__5 : 
		s_rnge("types", i__5, "keeper_", (ftnlen)1576)) << 3), "PCK", 
		(ftnlen)8, (ftnlen)3) == 0 && dopck || s_cmp(types + (((i__6 =
		 i__ - 1) < 5300 && 0 <= i__6 ? i__6 : s_rnge("types", i__6, 
		"keeper_", (ftnlen)1576)) << 3), "SPK", (ftnlen)8, (ftnlen)3) 
		== 0 && dospk || s_cmp(types + (((i__7 = i__ - 1) < 5300 && 0 
		<= i__7 ? i__7 : s_rnge("types", i__7, "keeper_", (ftnlen)
		1576)) << 3), "TEXT", (ftnlen)8, (ftnlen)4) == 0 && dotext;
	if (add) {
	    ++(*count);
	}
    }
    chkout_("KTOTAL", (ftnlen)6);
    return 0;
/* $Procedure      KDATA ( Kernel Data ) */

L_kdata:
/* $ Abstract */

/*     Return data for the nth kernel that is among a list of specified */
/*     kernel types. */

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

/* $ Declarations */

/*     INTEGER               WHICH */
/*     CHARACTER*(*)         KIND */
/*     CHARACTER*(*)         FILE */
/*     CHARACTER*(*)         FILTYP */
/*     CHARACTER*(*)         SOURCE */
/*     INTEGER               HANDLE */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     WHICH      I   Index of kernel to fetch from the list of kernels. */
/*     KIND       I   The kind of kernel to which fetches are limited. */
/*     FILE       O   The name of the kernel file. */
/*     FILTYP     O   The type of the kernel. */
/*     SOURCE     O   Name of the source file used to load FILE. */
/*     HANDLE     O   The handle attached to FILE. */
/*     FOUND      O   TRUE if the specified file could be located. */

/* $ Detailed_Input */

/*     WHICH      is the number of the kernel to fetch (matching the */
/*                type specified by KIND) from the list of kernels that */
/*                have been loaded through the entry point FURNSH but */
/*                that have not been unloaded through the entry point */
/*                UNLOAD. */

/*     KIND       is a list of types of kernels to be considered when */
/*                fetching kernels from the list of loaded kernels. KIND */
/*                should consist of words from list of kernel types */
/*                given below. */

/*                   SPK  --- All SPK files are counted in the total. */
/*                   CK   --- All CK files are counted in the total. */
/*                   PCK  --- All binary PCK files are counted in the */
/*                            total. */
/*                   EK   --- All EK files are counted in the total. */
/*                   TEXT --- All text kernels that are not meta-text */
/*                            kernels are included in the total. */
/*                   META --- All meta-text kernels are counted in the */
/*                            total. */
/*                   ALL  --- Every type of kernel is counted in the */
/*                            total. */

/*                 KIND is case insensitive.  If a word appears in KIND */
/*                 that is not one of those listed above it is ignored. */

/*                 See the entry point KTOTAL for examples of the use */
/*                 of KIND. */

/* $ Detailed_Output */

/*     FILE        is the name of the WHICH'th file of a type matching */
/*                 KIND that is currently loaded via FURNSH.  FILE */
/*                 will be blank if there is not a WHICH'th kernel. */

/*     FILTYP      is the type of the kernel specified by FILE.  FILE */
/*                 will be blank if there is no file matching the */
/*                 specification of WHICH and KIND. */

/*     SOURCE      is the name of the source file that was used to */
/*                 specify FILE as one to load.  If FILE was loaded */
/*                 directly via a call to FURNSH, SOURCE will be blank. */
/*                 If there is no file matching the specification of */
/*                 WHICH and KIND, SOURCE will be blank. */

/*     HANDLE      is the handle attached to FILE if it is a binary */
/*                 kernel.  If FILE is a text kernel or meta-text kernel */
/*                 HANDLE will be zero.  If there is no file matching */
/*                 the specification of WHICH and KIND, HANDLE will be */
/*                 set to zero. */

/*     FOUND       is returned TRUE if a FILE matching the specification */
/*                 of WHICH and KIND exists.  If there is no such file, */
/*                 FOUND will be set to FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) If a file is not loaded matching the specification of WHICH */
/*        and KIND, FOUND will be FALSE, FILE, FILTYP, and SOURCE */
/*        will be blank and HANDLE will be set to zero. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This entry point allows you to determine which kernels have */
/*     been loaded via FURNSH and to obtain information sufficient */
/*     to directly query those files. */

/* $ Examples */

/*     The following example shows how you could print a summary */
/*     of SPK files that have been loaded through the interface */
/*     FURNSH. */


/*     CALL KTOTAL ( 'SPK', COUNT ) */

/*     IF ( COUNT .EQ. 0 ) THEN */
/*        WRITE (*,*) 'There are no SPK files loaded at this time.' */
/*     ELSE */
/*        WRITE (*,*) 'The loaded SPK files are: ' */
/*        WRITE (*,*) */
/*     END IF */

/*     DO WHICH = 1, COUNT */

/*        CALL KDATA( WHICH, 'SPK', FILE, FILTYP, SOURCE, HANDLE, FOUND ) */
/*        WRITE (*,*) FILE */

/*     END DO */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 02-APR-2009 (NJB) */

/*        Deleted reference to unneeded variable DOALL. */

/* -    SPICELIB Version 1.0.1, 06-DEC-2002 (NJB) */

/*        Typo in header example was corrected. */

/* -    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Retrieve information on loaded SPICE kernels */

/* -& */
    s_copy(file, " ", file_len, (ftnlen)1);
    s_copy(filtyp, " ", filtyp_len, (ftnlen)1);
    s_copy(source, " ", source_len, (ftnlen)1);
    *handle = 0;
    *found = FALSE_;
    if (*which < 1 || *which > loaded) {
	return 0;
    }

/*     Parse KIND to see which kernels are of interest. */

    dospk = FALSE_;
    dock = FALSE_;
    dotext = FALSE_;
    dometa = FALSE_;
    doek = FALSE_;
    dopck = FALSE_;
    start = 1;
    fndnwd_(kind, &start, &b, &e, kind_len);
    while(b > 0) {
	if (eqstr_(kind + (b - 1), "ALL", e - (b - 1), (ftnlen)3)) {

/*           There's no point in going on, we can fill in the output */
/*           variables right now. */

	    *found = TRUE_;
	    s_copy(file, files + ((i__1 = *which - 1) < 5300 && 0 <= i__1 ? 
		    i__1 : s_rnge("files", i__1, "keeper_", (ftnlen)1821)) * 
		    255, file_len, (ftnlen)255);
	    s_copy(filtyp, types + (((i__1 = *which - 1) < 5300 && 0 <= i__1 ?
		     i__1 : s_rnge("types", i__1, "keeper_", (ftnlen)1822)) <<
		     3), filtyp_len, (ftnlen)8);
	    *handle = handls[(i__1 = *which - 1) < 5300 && 0 <= i__1 ? i__1 : 
		    s_rnge("handls", i__1, "keeper_", (ftnlen)1823)];
	    if (srces[(i__1 = *which - 1) < 5300 && 0 <= i__1 ? i__1 : s_rnge(
		    "srces", i__1, "keeper_", (ftnlen)1825)] != 0) {
		s_copy(source, files + ((i__2 = srces[(i__1 = *which - 1) < 
			5300 && 0 <= i__1 ? i__1 : s_rnge("srces", i__1, 
			"keeper_", (ftnlen)1826)] - 1) < 5300 && 0 <= i__2 ? 
			i__2 : s_rnge("files", i__2, "keeper_", (ftnlen)1826))
			 * 255, source_len, (ftnlen)255);
	    }
	    return 0;
	} else {
	    dock = dock || eqstr_(kind + (b - 1), "CK", e - (b - 1), (ftnlen)
		    2);
	    doek = doek || eqstr_(kind + (b - 1), "EK", e - (b - 1), (ftnlen)
		    2);
	    dometa = dometa || eqstr_(kind + (b - 1), "META", e - (b - 1), (
		    ftnlen)4);
	    dopck = dopck || eqstr_(kind + (b - 1), "PCK", e - (b - 1), (
		    ftnlen)3);
	    dospk = dospk || eqstr_(kind + (b - 1), "SPK", e - (b - 1), (
		    ftnlen)3);
	    dotext = dotext || eqstr_(kind + (b - 1), "TEXT", e - (b - 1), (
		    ftnlen)4);
	}
	start = e + 1;
	fndnwd_(kind, &start, &b, &e, kind_len);
    }

/*     Examine the loaded kernels one at a time until we match */
/*     WHICH files of the specified KIND. */

    hits = 0;
    i__1 = loaded;
    for (i__ = 1; i__ <= i__1; ++i__) {
	add = s_cmp(types + (((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 : 
		s_rnge("types", i__2, "keeper_", (ftnlen)1852)) << 3), "CK", (
		ftnlen)8, (ftnlen)2) == 0 && dock || s_cmp(types + (((i__3 = 
		i__ - 1) < 5300 && 0 <= i__3 ? i__3 : s_rnge("types", i__3, 
		"keeper_", (ftnlen)1852)) << 3), "EK", (ftnlen)8, (ftnlen)2) 
		== 0 && doek || s_cmp(types + (((i__4 = i__ - 1) < 5300 && 0 
		<= i__4 ? i__4 : s_rnge("types", i__4, "keeper_", (ftnlen)
		1852)) << 3), "META", (ftnlen)8, (ftnlen)4) == 0 && dometa || 
		s_cmp(types + (((i__5 = i__ - 1) < 5300 && 0 <= i__5 ? i__5 : 
		s_rnge("types", i__5, "keeper_", (ftnlen)1852)) << 3), "PCK", 
		(ftnlen)8, (ftnlen)3) == 0 && dopck || s_cmp(types + (((i__6 =
		 i__ - 1) < 5300 && 0 <= i__6 ? i__6 : s_rnge("types", i__6, 
		"keeper_", (ftnlen)1852)) << 3), "SPK", (ftnlen)8, (ftnlen)3) 
		== 0 && dospk || s_cmp(types + (((i__7 = i__ - 1) < 5300 && 0 
		<= i__7 ? i__7 : s_rnge("types", i__7, "keeper_", (ftnlen)
		1852)) << 3), "TEXT", (ftnlen)8, (ftnlen)4) == 0 && dotext;
	if (add) {
	    ++hits;

/*           If we've reached the specified number, fill in the */
/*           requested information and return. */

	    if (hits == *which) {
		*found = TRUE_;
		s_copy(file, files + ((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? 
			i__2 : s_rnge("files", i__2, "keeper_", (ftnlen)1869))
			 * 255, file_len, (ftnlen)255);
		s_copy(filtyp, types + (((i__2 = i__ - 1) < 5300 && 0 <= i__2 
			? i__2 : s_rnge("types", i__2, "keeper_", (ftnlen)
			1870)) << 3), filtyp_len, (ftnlen)8);
		*handle = handls[(i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 :
			 s_rnge("handls", i__2, "keeper_", (ftnlen)1871)];
		if (srces[(i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 : 
			s_rnge("srces", i__2, "keeper_", (ftnlen)1873)] != 0) 
			{
		    s_copy(source, files + ((i__3 = srces[(i__2 = i__ - 1) < 
			    5300 && 0 <= i__2 ? i__2 : s_rnge("srces", i__2, 
			    "keeper_", (ftnlen)1874)] - 1) < 5300 && 0 <= 
			    i__3 ? i__3 : s_rnge("files", i__3, "keeper_", (
			    ftnlen)1874)) * 255, source_len, (ftnlen)255);
		}
		return 0;
	    }
	}
    }
    return 0;
/* $Procedure      KINFO ( Kernel Information ) */

L_kinfo:
/* $ Abstract */

/*     Return information about a specific kernel */

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

/* $ Declarations */

/*     CHARACTER*(*)         FILE */
/*     CHARACTER*(*)         FILTYP */
/*     CHARACTER*(*)         SOURCE */
/*     INTEGER               HANDLE */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FILE       I   Name of a kernel to fetch information for */
/*     FILTYP     O   The type of the kernel */
/*     SOURCE     O   Name of the source file used to load FILE. */
/*     HANDLE     O   The handle attached to FILE. */
/*     FOUND      O   TRUE if the specified file could be located. */

/* $ Detailed_Input */

/*     FILE       is the name of a kernel file for which KEEPER */
/*                information is desired. */

/* $ Detailed_Output */

/*     FILTYP      is the type of the kernel specified by FILE.  FILE */
/*                 will be blank if FILE is not on the list of loaded */
/*                 kernels. */

/*     SOURCE      is the name of the source file that was used to */
/*                 specify FILE as one to load.  If FILE was loaded */
/*                 directly via a call to FURNSH, SOURCE will be blank. */
/*                 If FILE is not on the list of loaded kernels, SOURCE */
/*                 will be blank */

/*     HANDLE      is the handle attached to FILE if it is a binary */
/*                 kernel.  If FILE is a text kernel or meta-text kernel */
/*                 HANDLE will be zero.  If FILE is not on the list of */
/*                 loaded kernels, HANDLE will be set to zero. */

/*     FOUND       is returned TRUE if FILE is on the KEEPER list of */
/*                 loaded kernels.  If there is no such file, FOUND will */
/*                 be set to FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) If the specified file is not on the list of files that */
/*        are currently loaded via the interface FURNSH, FOUND */
/*        will be FALSE, HANDLE will be set to zero and FILTYP */
/*        and SOURCE will be set to blanks. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This entry point allows you to request information directly */
/*     for a specific SPICE kernel. */

/* $ Examples */

/*     Suppose you wish to determine the type of a loaded kernel */
/*     so that you can call the correct summarizing routines */
/*     for the kernel.  The following bit of pseudo code shows */
/*     how you might use this entry point together with summarizing */
/*     code to produce a report on the file.  (Note that the */
/*     routines SPK_SUMMRY, CK_SUMMRY, PCK_SUMMRY and EK_SUMMRY */
/*     are simply names to indicate what you might do with the */
/*     information returned by KINFO.  They are not routines that */
/*     are part of the SPICE toolkit.) */

/*     FILE = '<name of the file of interest>' */

/*     CALL KINFO ( FILE, FILTYP, SOURCE, HANDLE, FOUND ) */

/*     IF ( .NOT. FOUND ) THEN */
/*        WRITE (*,*) FILE */
/*        WRITE (*,*) 'is not loaded at this time.' */
/*     ELSE */

/*        IF      ( FILTYP .EQ. 'SPK' ) THEN */

/*           WRITE (*,*) FILE */
/*           WRITE (*,*) 'is an SPK file.' */

/*           CALL SPK_SUMMRY ( HANDLE ) */

/*        ELSE IF ( FILTYP .EQ. 'CK'  ) THEN */

/*           WRITE (*,*) FILE */
/*           WRITE (*,*) 'is a CK file.' */

/*           CALL CK_SUMMRY ( HANDLE ) */

/*        ELSE IF ( FILTYP .EQ. 'PCK' ) THEN */

/*           WRITE (*,*) FILE */
/*           WRITE (*,*) 'is a  PCK file.' */

/*           CALL PCK_SUMMRY ( HANDLE ) */

/*        ELSE IF ( FILTYP .EQ. 'EK'  ) THEN */

/*           WRITE (*,*) FILE */
/*           WRITE (*,*) 'is an EK file.' */

/*           CALL EK_SUMMRY ( HANDLE ) */

/*        ELSE IF ( FILTYP .EQ. 'META') THEN */
/*           WRITE (*,*) FILE */
/*           WRITE (*,*) 'is a meta-text kernel.' */
/*        ELSE */
/*           WRITE (*,*) FILE */
/*           WRITE (*,*) 'is a text kernel.' */
/*        END IF */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Fetch information about a loaded SPICE kernel */

/* -& */
    s_copy(filtyp, " ", filtyp_len, (ftnlen)1);
    s_copy(source, " ", source_len, (ftnlen)1);
    *handle = 0;
    *found = FALSE_;
    i__ = isrchc_(file, &loaded, files, file_len, (ftnlen)255);
    if (i__ > 0) {
	*found = TRUE_;
	s_copy(filtyp, types + (((i__1 = i__ - 1) < 5300 && 0 <= i__1 ? i__1 :
		 s_rnge("types", i__1, "keeper_", (ftnlen)2088)) << 3), 
		filtyp_len, (ftnlen)8);
	*handle = handls[(i__1 = i__ - 1) < 5300 && 0 <= i__1 ? i__1 : s_rnge(
		"handls", i__1, "keeper_", (ftnlen)2089)];
	if (srces[(i__1 = i__ - 1) < 5300 && 0 <= i__1 ? i__1 : s_rnge("srces"
		, i__1, "keeper_", (ftnlen)2091)] != 0) {
	    s_copy(source, files + ((i__2 = srces[(i__1 = i__ - 1) < 5300 && 
		    0 <= i__1 ? i__1 : s_rnge("srces", i__1, "keeper_", (
		    ftnlen)2092)] - 1) < 5300 && 0 <= i__2 ? i__2 : s_rnge(
		    "files", i__2, "keeper_", (ftnlen)2092)) * 255, 
		    source_len, (ftnlen)255);
	}
    }
    return 0;
/* $Procedure      KCLEAR ( Keeper clear ) */

L_kclear:
/* $ Abstract */

/*     Clear the KEEPER subsystem: unload all kernels, clear the kernel */
/*     pool, and re-initialize the subsystem. Existing watches on kernel */
/*     variables are retained. */

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

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     None. */

/* $ Detailed_Input */

/*     None.  This routine operates by side effects.  See Particulars */
/*     below. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) Any errors that occur when setting a kernel pool watch */
/*        or checking watched variables will be diagnosed by */
/*        routines in the call tree of this routine. */

/* $ Files */

/*     See Particulars. */

/* $ Particulars */

/*     This entry point allows you re-initialize the KEEPER system with */
/*     a single call. */

/*     This routine unloads all kernels from their kernel-type-specific */
/*     kernel management subsystems (SPKBSR, CKBSR, etc.), clears the */
/*     kernel pool, clears KEEPER's internal file database, and re-sets */
/*     the watch status for the kernel variables used to load kernels */
/*     via meta-kernels. As a side effect of clearing the kernel pool, */
/*     all watched variables are marked as updated. Note that clearing */
/*     the kernel pool does not delete watches (aka "watchers"). Watches */
/*     can be deleted by calling the POOL entry point DWPOOL. */

/*     This capability, though implemented in Fortran, is particularly */
/*     relevant to SPICE implementations such as Icy, for which the */
/*     state of the KEEPER system persists after any Icy-based IDL */
/*     script is run. Successive runs of Icy-based scripts may perform */
/*     in unexpected ways when scripts access data loaded during runs of */
/*     previous scripts. */

/*     Cleaning up after such programs using explicit UNLOAD commands is */
/*     tedious and error-prone.  One call to this routine sets the */
/*     KEEPER system to its initial state, preventing unintentional */
/*     interaction between scripts via KEEPER's state. */

/* $ Examples */

/*     Clear the KEEPER system; check for residual loaded files. */
/*     We shouldn't find any. */

/*         CALL KCLEAR */
/*         CALL KTOTAL ( 'ALL', N ) */
/*         WRITE (*,*) 'Count of loaded kernels after KCLEAR call: ', N */

/* $ Restrictions */

/*     Calling this routine will wipe out any kernel pool data */
/*     inserted via the PXPOOL API routines. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 01-JUL-2014 (NJB) (EDW) */

/*        Updated the discussion of kernel variable watchers. */

/*     Last update was 13-APR-2011 (EDW) */

/*        Trivial edit to Restrictions, replaced P*POOL with */
/*        PXPOOL. The "*" character causes the HTML documentation */
/*        script to create a link for the "POOL" substring. */

/* -    SPICELIB Version 1.0.0, 15-NOV-2006 (NJB) */

/* -& */
/* $ Index_Entries */

/*     Re-initialize the keeper system */
/*     Clear the keeper system */
/*     Unload all kernels */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("KCLEAR", (ftnlen)6);

/*     Unloading all kernels is actually much less work than */
/*     unloading just a few of them.  We unload all of the */
/*     binary kernels via the "unload" routines for their */
/*     respective subsystems, then clear the kernel pool. */

    i__1 = loaded;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (s_cmp(types + (((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 : 
		s_rnge("types", i__2, "keeper_", (ftnlen)2264)) << 3), "SPK", 
		(ftnlen)8, (ftnlen)3) == 0) {
	    spkuef_(&handls[(i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 : 
		    s_rnge("handls", i__2, "keeper_", (ftnlen)2266)]);
	} else if (s_cmp(types + (((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? 
		i__2 : s_rnge("types", i__2, "keeper_", (ftnlen)2268)) << 3), 
		"CK", (ftnlen)8, (ftnlen)2) == 0) {
	    ckupf_(&handls[(i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 : 
		    s_rnge("handls", i__2, "keeper_", (ftnlen)2270)]);
	} else if (s_cmp(types + (((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? 
		i__2 : s_rnge("types", i__2, "keeper_", (ftnlen)2272)) << 3), 
		"PCK", (ftnlen)8, (ftnlen)3) == 0) {
	    pckuof_(&handls[(i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 : 
		    s_rnge("handls", i__2, "keeper_", (ftnlen)2274)]);
	} else if (s_cmp(types + (((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? 
		i__2 : s_rnge("types", i__2, "keeper_", (ftnlen)2276)) << 3), 
		"EK", (ftnlen)8, (ftnlen)2) == 0) {
	    ekuef_(&handls[(i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 : 
		    s_rnge("handls", i__2, "keeper_", (ftnlen)2278)]);
	}
    }
    clpool_();

/*     Although it's not strictly necessary, we initialize */
/*     KEEPER's database arrays.  This step may occasionally */
/*     be helpful for debugging. */

    i__1 = loaded;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_copy(files + ((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 : s_rnge(
		"files", i__2, "keeper_", (ftnlen)2293)) * 255, " ", (ftnlen)
		255, (ftnlen)1);
	handls[(i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 : s_rnge("handls", 
		i__2, "keeper_", (ftnlen)2294)] = 0;
	srces[(i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 : s_rnge("srces", 
		i__2, "keeper_", (ftnlen)2295)] = 0;
	s_copy(types + (((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 : s_rnge(
		"types", i__2, "keeper_", (ftnlen)2296)) << 3), " ", (ftnlen)
		8, (ftnlen)1);
    }

/*     There's just one counter that indicates the number of */
/*     database entries:  LOADED.  Set this counter to */
/*     its initial state. */

    loaded = 0;

/*     Calling CLPOOL doesn't remove watches, but it does send a message */
/*     to each agent indicating that its variables have been touched. */
/*     Clear this indication by calling CVPOOL.  (This is done for */
/*     safety; the current implementation of FURNSH doesn't require it.) */

    cvpool_("FURNSH", &update, (ftnlen)6);
    chkout_("KCLEAR", (ftnlen)6);
    return 0;
/* $Procedure      UNLOAD ( Unload a kernel ) */

L_unload:
/* $ Abstract */

/*     Unload a SPICE kernel. */

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

/* $ Declarations */

/*     CHARACTER*(*)         FILE */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FILE       I   The name of a kernel to unload. */

/* $ Detailed_Input */

/*     FILE       is the name of a file to unload.  This file */
/*                should be one loaded through the interface FURNSH. */
/*                If the file is not on the list of loaded kernels */
/*                no action is taken. */

/*                Note that if FILE is a meta-text kernel, all of */
/*                the files loaded as a result of loading the meta-text */
/*                kernel will be unloaded. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) If the specified kernel is not on the list of loaded kernels */
/*        no action is taken. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The call */

/*        CALL UNLOAD ( FILE ) */

/*     has the effect of "erasing" the last previous call: */

/*        CALL FURNSH ( FILE ) */

/*     This interface allows you to unload binary and text kernels. */
/*     Moreover, if you used a meta-text kernel to set up your */
/*     working environment, you can unload all of the kernels loaded */
/*     through the meta-kernel by unloading the meta-kernel. */

/*     The usual usage of FURNSH is to load each file needed by your */
/*     program exactly one time.  However, it is possible to load a */
/*     kernel more than one time.  (Usually, this is a result of loading */
/*     meta-kernels without taking the care needed to ensure that the */
/*     meta-kernels do not specify the same file more than once.)  The */
/*     effect of unloading a kernel that has been loaded more than once */
/*     is to "undo" the last loading of the kernel.  Depending upon the */
/*     kernel and its relationship to other loaded kernels, this may */
/*     have no visible effect on the working of your program.  To */
/*     illustrate this behavior suppose that you have a collection of */
/*     files FILE1, FILE2, FILE3, FILE4, FILE5, FILE6, FILE7, FILE8, */
/*     META1, META2  where FILE1 ... FILE8 are SPICE kernels and META1 */
/*     and META2 are meta-kernels with the specified kernels to load as */
/*     shown below. */


/*         META1: */
/*            KERNELS_TO_LOAD = ( FILE2, */
/*                                FILE3, */
/*                                FILE4, */
/*                                FILE5 ) */

/*         META2: */
/*            KERNELS_TO_LOAD = ( FILE2, */
/*                                FILE3, */
/*                                FILE7, */
/*                                FILE8 ) */


/*      The following sequence of calls */

/*          CALL FURNSH ( FILE1 ) */
/*          CALL FURNSH ( FILE2 ) */
/*          CALL FURNSH ( FILE3 ) */
/*          CALL FURNSH ( META1 ) */
/*          CALL FURNSH ( FILE6 ) */
/*          CALL FURNSH ( META2 ) */

/*      has the effect: */

/*          "Load" FILE1 */
/*          "Load" FILE2 */
/*          "Load" FILE3 */
/*          "Load" META1 as a text kernel and then... */
/*                "Load" FILE2 (note that it was loaded from META1) */
/*                "Load" FILE3 (note that it was loaded from META1) */
/*                "Load" FILE4 (note that it was loaded from META1) */
/*                "Load" FILE5 (note that it was loaded from META1) */
/*          "Load" FILE6 */
/*          "Load" META2 as a text kernel and then... */
/*                "Load" FILE2 (note that it was loaded from META2) */
/*                "Load" FILE3 (note that it was loaded from META2) * */
/*                "Load" FILE7 (note that it was loaded from META2) */
/*                "Load" FILE8 (note that it was loaded from META2) */

/*      If we  UNLOAD FILE3 */

/*         CALL UNLOAD ( FILE3 ) */

/*      we locate the last time FILE3 was loaded (* above) and modify the */
/*      state of loaded kernels so that it looks as if we had made the */
/*      following sequence of "load" operations. */

/*          "Load" FILE1 */
/*          "Load" FILE2 */
/*          "Load" FILE3 */
/*          "Load" META1 as a text kernel and then... */
/*                "Load" FILE2 (note that it was loaded from META1) */
/*                "Load" FILE3 (note that it was loaded from META1) */
/*                "Load" FILE4 (note that it was loaded from META1) */
/*                "Load" FILE5 (note that it was loaded from META1) */
/*          "Load" FILE6 */
/*          "Load" META2 as a text kernel and then... */
/*                "Load" FILE2 (note that it was loaded from META2) */
/*                "Load" FILE7 (note that it was loaded from META2) */
/*                "Load" FILE8 (note that it was loaded from META2) */

/*      As you can see, the data from FILE3 is still available to the */
/*      program.  All that may have changed is the usage priority */
/*      associated with that data. */

/*      If we unload META2 (or META1) we remove all remaining files that */
/*      are noted as being loaded from META2 (or META1) */

/*          CALL UNLOAD ( META2 ) */

/*      produces the following load state for the program: */

/*          "Load" FILE1 */
/*          "Load" FILE2 */
/*          "Load" FILE3 */
/*          "Load" META1 as a text kernel and then... */
/*                "Load" FILE2 (note that it was loaded from META1) */
/*                "Load" FILE3 (note that it was loaded from META1) */
/*                "Load" FILE4 (note that it was loaded from META1) */
/*                "Load" FILE5 (note that it was loaded from META1) */
/*          "Load" FILE6 */

/*      If we had unloaded META1 instead, we would have this load state. */

/*          "Load" FILE1 */
/*          "Load" FILE2 */
/*          "Load" FILE3 */
/*          "Load" FILE6 */
/*          "Load" META2 as a text kernel and then... */
/*                "Load" FILE2 (note that it was loaded from META2) */
/*                "Load" FILE7 (note that it was loaded from META2) */
/*                "Load" FILE8 (note that it was loaded from META2) */

/*      So we see that unloading a file does not necessarily make its */
/*      data unavailable to your program.  Unloading modifies the */
/*      precedence of the files loaded in your program. The data */
/*      associated with an unloaded file becomes unavailable only when */
/*      the file has been unloaded as many times as it was loaded. */

/*      When would you encounter such a scenario? The situation of */
/*      loading a file more than once might appear if you were trying to */
/*      contrast the results of computations performed with two */
/*      different meta-kernels.  In such a scenario you might load a */
/*      "baseline" set of kernels early in your program and then load */
/*      and unload meta-kernels to compare results between the two */
/*      different sets of data. */

/*     Unloading Text Kernels or Meta-Kernels */
/*     -------------------------------------- */

/*     Part of the action of unloading text (or meta-kernels) is */
/*     the clearing of the kernel pool and re-loading any kernels that */
/*     were not in the specified set of kernels to unload.  Since */
/*     loading of text kernels is not a very fast process, unloading */
/*     text kernels takes considerably longer than unloading binary */
/*     kernels.  Moreover, since the kernel pool is cleared, any kernel */
/*     pool variables you have set from your program by using one of the */
/*     interfaces PCPOOL, PDPOOL, PIPOOL, or LMPOOL will be removed from */
/*     the kernel pool.  For this reason, if you plan to use this */
/*     feature in your program, together with one of the routines */
/*     specified above, you will need to take special precautions to */
/*     make sure kernel pool variables required by your program do not */
/*     inadvertently disappear. */

/*     As a side effect of unloading a text kernel, all watched kernel */
/*     variables are marked as updated. Note that unloading a text */
/*     kernel does not delete watchers. Watchers can be deleted by */
/*     calling the POOL entry point DWPOOL. */

/* $ Examples */

/*     Suppose that you wish to compare two different sets of kernels */
/*     used to describe the geometry of a mission (for example a predict */
/*     model and a reconstructed model). You can place all of the */
/*     kernels for one model in one meta-text kernel, and the other set */
/*     in a second meta-text kernel.  Let's call these PREDICT.MTA and */
/*     ACTUAL.MTA. */

/*        CALL FURNSH ( 'PREDCT.MTA' ) */

/*        compute quantities of interest and store them */
/*        for comparison with results of reconstructed */
/*        (actual) kernels. */

/*        Now unload the predict model and load the reconstructed */
/*        model. */

/*        CALL UNLOAD ( 'PREDCT.MTA' ) */
/*        CALL FURNSH ( 'ACTUAL.MTA' ) */

/*        re-compute quantities of interest and compare them */
/*        with the stored quantities. */

/* $ Restrictions */

/*     See the note regarding the unloading of Text and meta-text */
/*     Kernels. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.0.1 01-JUL-2014(NJB) */

/*        Updated discussion of kernel variable watchers. */

/* -    SPICELIB Version 3.0.0 15-NOV-2006 (NJB) */

/*        Bug fix:  corrected update of source pointers when a */
/*        meta-kernel is unloaded.  Previously source pointers */
/*        having higher indices than those of the files referenced */
/*        by the meta kernel were not adjusted when the database */
/*        was compressed. */

/* -    SPICELIB VERSION 2.0.0, 23-AUG-2001 (WLT) */

/*        Added code to make sure that UNLOAD has the effect of */
/*        loading all remaining kernels in the order they were first */
/*        introduced. */

/* -    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Unload a SPICE kernel */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("UNLOAD", (ftnlen)6);
    didspk = FALSE_;
    didpck = FALSE_;
    didck = FALSE_;
    didek = FALSE_;
    didtxt = FALSE_;

/*     First locate the file we need to unload, we search backward */
/*     through the list of loaded files so that we unload in the right */
/*     order. */

    gotit = FALSE_;
    i__ = loaded;
    while(! gotit && i__ > 0) {
	if (s_cmp(files + ((i__1 = i__ - 1) < 5300 && 0 <= i__1 ? i__1 : 
		s_rnge("files", i__1, "keeper_", (ftnlen)2657)) * 255, file, (
		ftnlen)255, file_len) == 0) {
	    gotit = TRUE_;
	} else {
	    --i__;
	}
    }

/*     If we didn't locate the requested file, there is nothing to do. */

    if (! gotit) {
	chkout_("UNLOAD", (ftnlen)6);
	return 0;
    }

/*     We need to know what type of file we've got so that we */
/*     can take the correct "unload" action. */

    if (s_cmp(types + (((i__1 = i__ - 1) < 5300 && 0 <= i__1 ? i__1 : s_rnge(
	    "types", i__1, "keeper_", (ftnlen)2677)) << 3), "SPK", (ftnlen)8, 
	    (ftnlen)3) == 0) {
	spkuef_(&handls[(i__1 = i__ - 1) < 5300 && 0 <= i__1 ? i__1 : s_rnge(
		"handls", i__1, "keeper_", (ftnlen)2678)]);
	didspk = TRUE_;
    } else if (s_cmp(types + (((i__1 = i__ - 1) < 5300 && 0 <= i__1 ? i__1 : 
	    s_rnge("types", i__1, "keeper_", (ftnlen)2680)) << 3), "CK", (
	    ftnlen)8, (ftnlen)2) == 0) {
	ckupf_(&handls[(i__1 = i__ - 1) < 5300 && 0 <= i__1 ? i__1 : s_rnge(
		"handls", i__1, "keeper_", (ftnlen)2681)]);
	didck = TRUE_;
    } else if (s_cmp(types + (((i__1 = i__ - 1) < 5300 && 0 <= i__1 ? i__1 : 
	    s_rnge("types", i__1, "keeper_", (ftnlen)2683)) << 3), "PCK", (
	    ftnlen)8, (ftnlen)3) == 0) {
	pckuof_(&handls[(i__1 = i__ - 1) < 5300 && 0 <= i__1 ? i__1 : s_rnge(
		"handls", i__1, "keeper_", (ftnlen)2684)]);
	didpck = TRUE_;
    } else if (s_cmp(types + (((i__1 = i__ - 1) < 5300 && 0 <= i__1 ? i__1 : 
	    s_rnge("types", i__1, "keeper_", (ftnlen)2686)) << 3), "EK", (
	    ftnlen)8, (ftnlen)2) == 0) {
	ekuef_(&handls[(i__1 = i__ - 1) < 5300 && 0 <= i__1 ? i__1 : s_rnge(
		"handls", i__1, "keeper_", (ftnlen)2687)]);
	didek = TRUE_;
    } else if (s_cmp(types + (((i__1 = i__ - 1) < 5300 && 0 <= i__1 ? i__1 : 
	    s_rnge("types", i__1, "keeper_", (ftnlen)2689)) << 3), "TEXT", (
	    ftnlen)8, (ftnlen)4) == 0) {
	clpool_();
	didtxt = TRUE_;
    } else if (s_cmp(types + (((i__1 = i__ - 1) < 5300 && 0 <= i__1 ? i__1 : 
	    s_rnge("types", i__1, "keeper_", (ftnlen)2692)) << 3), "META", (
	    ftnlen)8, (ftnlen)4) == 0) {

/*        This is a special case, we need to undo the effect of loading */
/*        the meta-kernel.  This means we need to unload all kernels */
/*        that were loaded using this meta-kernel. */

	didtxt = TRUE_;
	src = i__;
	i__1 = src + 1;
	for (j = loaded; j >= i__1; --j) {
	    if (srces[(i__2 = j - 1) < 5300 && 0 <= i__2 ? i__2 : s_rnge(
		    "srces", i__2, "keeper_", (ftnlen)2704)] == src) {

/*              This file was loaded by the meta-kernel of interest. */
/*              We only need to unload the binary kernels as we */
/*              will get rid of all text kernels by clearing the */
/*              kernel pool. */

		if (s_cmp(types + (((i__2 = j - 1) < 5300 && 0 <= i__2 ? i__2 
			: s_rnge("types", i__2, "keeper_", (ftnlen)2711)) << 
			3), "SPK", (ftnlen)8, (ftnlen)3) == 0) {
		    spkuef_(&handls[(i__2 = j - 1) < 5300 && 0 <= i__2 ? i__2 
			    : s_rnge("handls", i__2, "keeper_", (ftnlen)2712)]
			    );
		    didspk = TRUE_;
		} else if (s_cmp(types + (((i__2 = j - 1) < 5300 && 0 <= i__2 
			? i__2 : s_rnge("types", i__2, "keeper_", (ftnlen)
			2714)) << 3), "CK", (ftnlen)8, (ftnlen)2) == 0) {
		    ckupf_(&handls[(i__2 = j - 1) < 5300 && 0 <= i__2 ? i__2 :
			     s_rnge("handls", i__2, "keeper_", (ftnlen)2715)])
			    ;
		    didck = TRUE_;
		} else if (s_cmp(types + (((i__2 = j - 1) < 5300 && 0 <= i__2 
			? i__2 : s_rnge("types", i__2, "keeper_", (ftnlen)
			2717)) << 3), "PCK", (ftnlen)8, (ftnlen)3) == 0) {
		    pckuof_(&handls[(i__2 = j - 1) < 5300 && 0 <= i__2 ? i__2 
			    : s_rnge("handls", i__2, "keeper_", (ftnlen)2718)]
			    );
		    didpck = TRUE_;
		} else if (s_cmp(types + (((i__2 = j - 1) < 5300 && 0 <= i__2 
			? i__2 : s_rnge("types", i__2, "keeper_", (ftnlen)
			2720)) << 3), "EK", (ftnlen)8, (ftnlen)2) == 0) {
		    ekuef_(&handls[(i__2 = j - 1) < 5300 && 0 <= i__2 ? i__2 :
			     s_rnge("handls", i__2, "keeper_", (ftnlen)2721)])
			    ;
		    didek = TRUE_;
		}
		n1 = loaded;
		n2 = loaded;
		n3 = loaded;
		remlac_(&c__1, &j, files, &n1, (ftnlen)255);
		remlac_(&c__1, &j, types, &n2, (ftnlen)8);
		remlai_(&c__1, &j, srces, &n3);
		remlai_(&c__1, &j, handls, &loaded);

/*              Each time we delete an item from the database, any */
/*              pointer to a location past the deletion point must be */
/*              updated to reflect the compression of the database. */
/*              Files loaded from meta kernels are always recorded */
/*              in the database *after* their sources, so each pointer */
/*              value is less than the index at which it occurs. */
/*              So, we need examine only those entries from index J */
/*              upwards. */

		i__2 = loaded;
		for (k = j; k <= i__2; ++k) {
		    if (srces[(i__3 = k - 1) < 5300 && 0 <= i__3 ? i__3 : 
			    s_rnge("srces", i__3, "keeper_", (ftnlen)2745)] > 
			    j) {

/*                    This pointer is affected by the deletion of */
/*                    the Jth database entry. */

			srces[(i__3 = k - 1) < 5300 && 0 <= i__3 ? i__3 : 
				s_rnge("srces", i__3, "keeper_", (ftnlen)2750)
				] = srces[(i__4 = k - 1) < 5300 && 0 <= i__4 ?
				 i__4 : s_rnge("srces", i__4, "keeper_", (
				ftnlen)2750)] - 1;
		    }
		}
	    }
	}

/*        Now clear the kernel pool. */

	clpool_();
    }

/*     Remove the I'th kernel from our local database. */

    n1 = loaded;
    n2 = loaded;
    n3 = loaded;
    remlac_(&c__1, &i__, files, &n1, (ftnlen)255);
    remlac_(&c__1, &i__, types, &n2, (ftnlen)8);
    remlai_(&c__1, &i__, srces, &n3);
    remlai_(&c__1, &i__, handls, &loaded);

/*     Update any source pointers affected by the deletion of the Ith */
/*     database entry. */

    i__1 = loaded;
    for (j = i__; j <= i__1; ++j) {
	if (srces[(i__2 = j - 1) < 5300 && 0 <= i__2 ? i__2 : s_rnge("srces", 
		i__2, "keeper_", (ftnlen)2782)] > i__) {

/*           This pointer is affected by the deletion of the Ith */
/*           database entry. */

	    srces[(i__2 = j - 1) < 5300 && 0 <= i__2 ? i__2 : s_rnge("srces", 
		    i__2, "keeper_", (ftnlen)2787)] = srces[(i__3 = j - 1) < 
		    5300 && 0 <= i__3 ? i__3 : s_rnge("srces", i__3, "keeper_"
		    , (ftnlen)2787)] - 1;
	}
    }

/*     If we unloaded a text kernel, we now need to reload all */
/*     of the text kernels that were not unloaded. */

    if (didtxt) {
	i__1 = loaded;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (s_cmp(types + (((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 : 
		    s_rnge("types", i__2, "keeper_", (ftnlen)2801)) << 3), 
		    "TEXT", (ftnlen)8, (ftnlen)4) == 0 || s_cmp(types + (((
		    i__3 = i__ - 1) < 5300 && 0 <= i__3 ? i__3 : s_rnge("typ"
		    "es", i__3, "keeper_", (ftnlen)2801)) << 3), "META", (
		    ftnlen)8, (ftnlen)4) == 0) {
		ldpool_(files + ((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 :
			 s_rnge("files", i__2, "keeper_", (ftnlen)2804)) * 
			255, (ftnlen)255);
		if (s_cmp(types + (((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? 
			i__2 : s_rnge("types", i__2, "keeper_", (ftnlen)2806))
			 << 3), "META", (ftnlen)8, (ftnlen)4) == 0) {

/*                 Clean up any debris that may have been left lying */
/*                 around because we reloaded a meta-text kernel. */

		    for (j = 1; j <= 3; ++j) {
			dvpool_(known + (((i__2 = j - 1) < 3 && 0 <= i__2 ? 
				i__2 : s_rnge("known", i__2, "keeper_", (
				ftnlen)2812)) << 5), (ftnlen)32);
		    }
		    cvpool_("FURNSH", &update, (ftnlen)6);
		}
	    }
	}
    }

/*     If any SPK files were unloaded, we need to reload everything */
/*     to establish the right priority sequence for segments. */

    if (didspk) {
	i__1 = loaded;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (s_cmp(types + (((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 : 
		    s_rnge("types", i__2, "keeper_", (ftnlen)2832)) << 3), 
		    "SPK", (ftnlen)8, (ftnlen)3) == 0) {
		spklef_(files + ((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 :
			 s_rnge("files", i__2, "keeper_", (ftnlen)2833)) * 
			255, &handls[(i__3 = i__ - 1) < 5300 && 0 <= i__3 ? 
			i__3 : s_rnge("handls", i__3, "keeper_", (ftnlen)2833)
			], (ftnlen)255);
	    }
	}
    }

/*     If any CK files were unloaded, we need to reload all of the */
/*     C-kernels to make sure that we have the correct priorities */
/*     for the remaining C-kernels. */

    if (didck) {
	i__1 = loaded;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (s_cmp(types + (((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 : 
		    s_rnge("types", i__2, "keeper_", (ftnlen)2846)) << 3), 
		    "CK", (ftnlen)8, (ftnlen)2) == 0) {
		cklpf_(files + ((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 : 
			s_rnge("files", i__2, "keeper_", (ftnlen)2847)) * 255,
			 &handls[(i__3 = i__ - 1) < 5300 && 0 <= i__3 ? i__3 :
			 s_rnge("handls", i__3, "keeper_", (ftnlen)2847)], (
			ftnlen)255);
	    }
	}
    }

/*     If any binary PCK files were unloaded, we need to reload any */
/*     remaining ones to re-establish the correct priorities for */
/*     kernels. */

    if (didpck) {
	i__1 = loaded;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (s_cmp(types + (((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 : 
		    s_rnge("types", i__2, "keeper_", (ftnlen)2861)) << 3), 
		    "PCK", (ftnlen)8, (ftnlen)3) == 0) {
		pcklof_(files + ((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 :
			 s_rnge("files", i__2, "keeper_", (ftnlen)2862)) * 
			255, &handls[(i__3 = i__ - 1) < 5300 && 0 <= i__3 ? 
			i__3 : s_rnge("handls", i__3, "keeper_", (ftnlen)2862)
			], (ftnlen)255);
	    }
	}
    }

/*     Finally, if any E-kernels were unloaded, we reload the remaining */
/*     kernels to make sure the state is restored to the correct set */
/*     of loaded kernels. */

    if (didek) {
	i__1 = loaded;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (s_cmp(types + (((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 : 
		    s_rnge("types", i__2, "keeper_", (ftnlen)2875)) << 3), 
		    "EK", (ftnlen)8, (ftnlen)2) == 0) {
		eklef_(files + ((i__2 = i__ - 1) < 5300 && 0 <= i__2 ? i__2 : 
			s_rnge("files", i__2, "keeper_", (ftnlen)2876)) * 255,
			 &handls[(i__3 = i__ - 1) < 5300 && 0 <= i__3 ? i__3 :
			 s_rnge("handls", i__3, "keeper_", (ftnlen)2876)], (
			ftnlen)255);
	    }
	}
    }
    chkout_("UNLOAD", (ftnlen)6);
    return 0;
} /* keeper_ */

/* Subroutine */ int keeper_(integer *which, char *kind, char *file, integer *
	count, char *filtyp, integer *handle, char *source, logical *found, 
	ftnlen kind_len, ftnlen file_len, ftnlen filtyp_len, ftnlen 
	source_len)
{
    return keeper_0_(0, which, kind, file, count, filtyp, handle, source, 
	    found, kind_len, file_len, filtyp_len, source_len);
    }

/* Subroutine */ int furnsh_(char *file, ftnlen file_len)
{
    return keeper_0_(1, (integer *)0, (char *)0, file, (integer *)0, (char *)
	    0, (integer *)0, (char *)0, (logical *)0, (ftnint)0, file_len, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int ktotal_(char *kind, integer *count, ftnlen kind_len)
{
    return keeper_0_(2, (integer *)0, kind, (char *)0, count, (char *)0, (
	    integer *)0, (char *)0, (logical *)0, kind_len, (ftnint)0, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int kdata_(integer *which, char *kind, char *file, char *
	filtyp, char *source, integer *handle, logical *found, ftnlen 
	kind_len, ftnlen file_len, ftnlen filtyp_len, ftnlen source_len)
{
    return keeper_0_(3, which, kind, file, (integer *)0, filtyp, handle, 
	    source, found, kind_len, file_len, filtyp_len, source_len);
    }

/* Subroutine */ int kinfo_(char *file, char *filtyp, char *source, integer *
	handle, logical *found, ftnlen file_len, ftnlen filtyp_len, ftnlen 
	source_len)
{
    return keeper_0_(4, (integer *)0, (char *)0, file, (integer *)0, filtyp, 
	    handle, source, found, (ftnint)0, file_len, filtyp_len, 
	    source_len);
    }

/* Subroutine */ int kclear_(void)
{
    return keeper_0_(5, (integer *)0, (char *)0, (char *)0, (integer *)0, (
	    char *)0, (integer *)0, (char *)0, (logical *)0, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int unload_(char *file, ftnlen file_len)
{
    return keeper_0_(6, (integer *)0, (char *)0, file, (integer *)0, (char *)
	    0, (integer *)0, (char *)0, (logical *)0, (ftnint)0, file_len, (
	    ftnint)0, (ftnint)0);
    }

