/* spkbsr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c_b8 = 100000;
static integer c__5000 = 5000;
static integer c__5 = 5;
static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure      SPKBSR ( S/P Kernel, Buffer segments for readers ) */
/* Subroutine */ int spkbsr_0_(int n__, char *fname, integer *handle, integer 
	*body, doublereal *et, doublereal *descr, char *ident, logical *found,
	 ftnlen fname_len, ftnlen ident_len)
{
    /* Initialized data */

    static integer nft = 0;
    static integer nbt = 0;
    static integer next = 0;

    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer head;
    static doublereal btlb[200];
    integer tail;
    static doublereal btub[200];
    integer cost, i__, j;
    extern /* Subroutine */ int dafgn_(char *, ftnlen);
    integer cheap, p;
    static integer btbeg[200];
    extern /* Subroutine */ int dafgs_(doublereal *);
    static integer btbod[200];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static integer fthan[5000];
    char doing[15];
    extern /* Subroutine */ int dafus_(doublereal *, integer *, integer *, 
	    doublereal *, integer *);
    char stack[15*2];
    static integer bthfs[200];
    extern doublereal dpmin_(void);
    extern /* Subroutine */ int lnkan_(integer *, integer *);
    extern doublereal dpmax_(void);
    static integer btlfs[200];
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *);
    static integer sthan[100000], btexp[200];
    static doublereal stdes[500000]	/* was [5][100000] */;
    extern integer lnktl_(integer *, integer *);
    static integer ftnum[5000];
    extern /* Subroutine */ int daffna_(logical *), dafbbs_(integer *), 
	    daffpa_(logical *);
    extern logical failed_(void);
    extern /* Subroutine */ int dafbfs_(integer *), cleard_(integer *, 
	    doublereal *), dafcls_(integer *);
    logical fndhan;
    integer crflbg, bindex;
    extern /* Subroutine */ int lnkila_(integer *, integer *, integer *);
    static logical btchkp[200];
    integer findex;
    extern /* Subroutine */ int dafopr_(char *, integer *, ftnlen), lnkilb_(
	    integer *, integer *, integer *);
    extern integer isrchi_(integer *, integer *, integer *);
    extern /* Subroutine */ int lnkini_(integer *, integer *);
    extern integer lnknfn_(integer *);
    extern /* Subroutine */ int lnkfsl_(integer *, integer *, integer *), 
	    sigerr_(char *, ftnlen), chkout_(char *, ftnlen);
    extern integer intmax_(void);
    static doublereal btprvd[1000]	/* was [5][200] */;
    static char btprvi[40*200];
    static integer btprvh[200];
    static char stidnt[40*100000];
    static integer btruex[200];
    char urgent[15];
    integer minexp;
    extern integer lnkprv_(integer *, integer *);
    integer nxtseg;
    extern integer lnknxt_(integer *, integer *);
    extern logical return_(void);
    static integer stpool[200012]	/* was [2][100006] */;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    char status[15];
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    doublereal dcd[2];
    integer icd[6];
    logical fnd;
    integer new__, top;

/* $ Abstract */

/*     Load and unload files for use by the readers.  Buffer segments */
/*     for readers. */

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

/*     SPK */

/* $ Keywords */

/*     EPHEMERIS */
/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Entry points */
/*     --------  ---  -------------------------------------------------- */
/*     FNAME      I   SPKLEF */
/*     HANDLE    I/O  SPKLEF, SPKUEF, SPKSFS */
/*     BODY       I   SPKSFS */
/*     ET         I   SPKSFS */
/*     DESCR      O   SPKSFS */
/*     IDENT      O   SPKSFS */

/* $ Detailed_Input */

/*     FNAME      is the name of an SPK file to be loaded. */

/*     HANDLE     on input is the handle of an SPK file to be */
/*                unloaded. */

/*     BODY       is the NAIF integer code of an ephemeris object, */
/*                typically a solar system body. */

/*     ET         is a time, in seconds past the epoch J2000 TDB. */

/* $ Detailed_Output */

/*     HANDLE     on output is the handle of the S/P-kernel file */
/*                containing a located segment. */

/*     DESCR      is the descriptor of a located segment. */

/*     IDENT      is the identifier of a located segment. */

/*     FOUND      indicates whether a requested segment was found or not. */

/* $ Parameters */

/*     FTSIZE     is the maximum number of ephemeris files that can be */
/*                loaded by SPKLEF at any given time for use by the */
/*                readers. */

/*     BTSIZE     is the maximum number of bodies whose segments can be */
/*                buffered by SPKSFS. */

/*     STSIZE     Maximum number of segments that can be buffered at any */
/*                given time by SPKSFS. */


/* $ Exceptions */

/*     1) If SPKBSR is called directly, the error 'SPICE(BOGUSENTRY)' */
/*        is signaled. */

/*     2) See entry points SPKLEF, SPKUEF, and SPKSFS for exceptions */
/*        specific to them. */

/* $ Files */

/*     S/P-kernel ephemeris files are indicated by filename before */
/*     loading (see SPKLEF) and handle after loading (all other places). */

/* $ Particulars */

/*     SPKBSR serves as an umbrella, allowing data to be shared by its */
/*     entry points: */

/*        SPKLEF       Load ephemeris file. */
/*        SPKUEF       Unload ephemeris file. */
/*        SPKSFS       Select file and segment. */

/*     Before a file can be read by the S/P-kernel readers, it must be */
/*     loaded by SPKLEF, which among other things, loads the file into */
/*     the DAF system. */

/*     Up to FTSIZE files may be loaded for use simultaneously, and a */
/*     file only has to be loaded once to become a potential search */
/*     target for any number of subsequent reads. */

/*     Once an SPK file has been loaded, it is assigned a file */
/*     handle, which is used to keep track of the file internally, */
/*     and which is used by the calling program to refer to the file */
/*     in all subsequent calls to SPK routines. */

/*     A file may be removed from the list of files for potential */
/*     searching by unloading it via a call to SPKUEF. */

/*     SPKSFS performs the search for segments within a file for the */
/*     S/P-kernel readers.  It searches through last-loaded files first. */
/*     Within a single file, it searches through last-inserted segments */
/*     first, thus assuming that "newest data is best". */

/*     Information on loaded files is used by SPKSFS to manage a buffer */
/*     of saved segment descriptors and identifiers to speed up access */
/*     time without having to necessarily perform file reads. */

/* $ Examples */

/*     Suppose that ephemeris data for the Mars Global Surveyor */
/*     spacecraft relative to Mars are contained in three separate files: */
/*     PREDICT.SPK contains complete predict ephemeris data for several */
/*     successive orbits, and UPDATE_1.SPK and UPDATE_2.SPK contain two */
/*     separate updates to selected intervals within those orbits, based */
/*     on altimeter fits. */

/*     In the following example, states of the spacecraft are computed */
/*     in two different ways: */

/*     First, the predict file and one of the update files are both */
/*     loaded and states are requested for regular intervals within */
/*     the orbits. The update file is searched through first, and if no */
/*     data for the requested time is available, the predict file is */
/*     used. */

/*     Then, the first update file is unloaded, the second update file */
/*     is loaded, and the same requests are made as above. */

/*     Throughout the two searches, a table is written which contains */
/*     the state (position and velocity) of the spacecraft, and the */
/*     file from which the data came, if such data was found, and an */
/*     error message otherwise. */

/*     It is assumed that the beginning and ending ephemeris times */
/*     (BEG_ET, END_ET) for the entire span have already been */
/*     initialized, along with the step-size for each measurement */
/*     (DELTA). The two routines WRITE_TABLE and WRITE_ERROR do not */
/*     exist in SPICELIB. */


/*           INTEGER               PRED_HNDL */
/*           INTEGER               UPD1_HNDL */
/*           INTEGER               UPD2_HNDL */
/*           INTEGER               HANDLE */
/*           INTEGER               BODY */
/*           INTEGER               CENTER */

/*           DOUBLE PRECISION      BEG_ET */
/*           DOUBLE PRECISION      END_ET */
/*           DOUBLE PRECISION      DELTA */
/*           DOUBLE PRECISION      ET */
/*           DOUBLE PRECISION      DESCR ( 5 ) */
/*           DOUBLE PRECISION      STATE ( 6 ) */

/*           CHARACTER*40          IDENT */
/*           CHARACTER*25          FNAME */

/*           LOGICAL               FOUND */

/*     C */
/*     C     Load the predict file and the first update file.  Since */
/*     C     last-loaded files get searched first, we want to load the */
/*     C     update file second. */
/*     C */
/*           CALL SPKLEF ( 'PREDICT.SPK',  PRED_HNDL ) */
/*           CALL SPKLEF ( 'UPDATE_1.SPK', UPD1_HNDL ) */

/*     C */
/*     C     NAIF code for the Mars Global Surveyor spacecraft is -94. */
/*     C */
/*           BODY = -94 */

/*     C */
/*     C     Compute states for regular intervals between BEG_ET and */
/*     C     END_ET. */
/*     C */
/*           ET = BEG_ET */

/*           DO WHILE ( ET .LE. END_ET ) */

/*     C */
/*     C        Locate the applicable segment (handle and descriptor). */
/*     C */
/*              CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND ) */

/*              IF ( FOUND ) THEN */
/*     C */
/*     C           Evaluate the state, get the name of the file from */
/*     C           whence the data came, and write the results to the */
/*     C           table. */
/*     C */
/*                 CALL SPKPV ( HANDLE, DESCR, ET, 'J2000', STATE, */
/*             .                CENTER ) */

/*                 CALL DAFHFN ( HANDLE, FNAME ) */

/*                 CALL WRITE_TABLE ( ET, STATE, FNAME ) */

/*              ELSE */

/*                 CALL WRITE_ERROR ( ET ) */

/*              END IF */

/*     C */
/*     C        The next time. */
/*     C */
/*              ET = ET + DELTA */

/*           END DO */

/*     C */
/*     C     Unload the first update file, load the second, and do */
/*     C     everything over again.  Since the original file stays */
/*     C     loaded, the update file once again gets searched first. */
/*     C */
/*           CALL SPKUEF (  UPD1_HNDL ) */
/*           CALL SPKLEF ( 'UPDATE_2.SPK', UPD2_HNDL ) */

/*           ET = BEG_ET */

/*           DO WHILE ( ET .LE. END_ET ) */

/*     C */
/*     C        Locate the applicable segment. */
/*     C */
/*              CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND ) */

/*              IF ( FOUND ) THEN */
/*     C */
/*     C           Evaluate the state, get the name of the file from */
/*     C           whence the data came, and write the results to the */
/*     C           table. */
/*     C */
/*                 CALL SPKPV ( HANDLE, DESCR, ET, 'J2000', STATE, */
/*             .                CENTER ) */

/*                 CALL DAFHFN ( HANDLE, FNAME ) */

/*                 CALL WRITE_TABLE ( ET, STATE, FNAME ) */

/*              ELSE */

/*                 CALL WRITE_ERROR ( ET ) */

/*              END IF */

/*     C */
/*     C        The next time. */
/*     C */
/*              ET = ET + DELTA */

/*           END DO */

/* $ Restrictions */

/*     1) If Fortran I/O errors occur while searching a loaded SPK */
/*        file, the internal state of this suite of routines may */
/*        be corrupted.  It may be possible to correct the state */
/*        by unloading the pertinent SPK files and then re-loading */
/*        them. */

/* $ Literature_References */

/*     SPK Required Reading. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     J.M. Lynch      (JPL) */
/*     R.E. Thurman    (JPL) */
/*     W.L. Taber      (JPL) */
/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 6.0.0, 17-MAR-2014 (NJB) */

/*        Updated segment pool initialization condition in entry */
/*        point SPKLEF so that the pool is initialized only if the file */
/*        table is empty. */

/* -    SPICELIB Version 5.4.0, 13-JUN-2013 (BVS) */

/*        Increased FTSIZE (from 1000 to 5000). */

/*        Increased STSIZE (from 50000 to 100000). */

/* -    SPICELIB Version 5.3.0, 01-MAR-2011 (NJB) */

/*        Bug fix: */

/*          In the SPKSFS 'MAKE ROOM' state, when the suspended activity */
/*          is 'ADD TO FRONT' and no segment table room is available, */
/*          the body table's pointer to the current segment list */
/*          is now set to null. Previously the pointer was allowed to go */
/*          stale. */

/* -    SPICELIB Version 5.2.0, 07-APR-2010 (NJB) */

/*        Increased segment table buffer size to 50000 entries. */

/* -    SPICELIB Version 5.1.0, 08-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MOVED calls in entry points SPKUEF and SPKSFS. */

/*        Increased segment table buffer size to 30000 entries. */

/* -    SPICELIB Version 5.0.0, 21-FEB-2003 (NJB) */

/*        Increased segment table buffer size to 10000 entries. */

/* -    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB) */

/*        Bug fixes: */

/*           1) When a segment list is freed because the entire list */
/*              is contributed by a single SPK file, and the list is */
/*              too large to be buffered, the corresponding body table */
/*              pointer is now set to null. */

/*           2) An algorithm change has eliminated a bug caused by not */
/*              updating the current body index when body table entries */
/*              having empty segment lists were compressed out of the */
/*              body table.  Previously the body table pointer BINDEX */
/*              could go stale after the compression. */

/*           3) When a already loaded kernel is re-opened with DAFOPR, */
/*              it now has its link count reset to 1 via a call to */
/*              DAFCLS. */

/*           4) The load routine SPKLEF now resets all file numbers when */
/*              the next file number reaches INTMAX()-1, thereby */
/*              avoiding arithmetic overflow. */

/*           5) The unload routine SPKUEF now calls RETURN() on entry and */
/*              returns if so directed. */

/*           6) In SPKSFS, DAF calls are followed by tests of FAILED() */
/*              in order to ensure that the main state loop terminates. */

/*           7) In SPKSFS, a subscript bound violation in a loop */
/*              termination test was corrected. */

/*        The "re-use interval" feature was introduced to improve speed */
/*        in the case where repeated, consecutive requests are satisified */
/*        by the same segment. */

/*        The segment list cost algorithm was modified slightly: */
/*        the contribution of a file search to the cost of a list */
/*        is included only when the file search is completed.  The */
/*        cost of finding the re-use interval is accounted for when */
/*        unbuffered searches are required. */

/*        The file table size has been increased to 1000, in order */
/*        to take advantage of the DAF system's new ability to load */
/*        1000 files. */

/*        The body table size has been increased to 200 in order to */
/*        decrease the chance of thrashing due to swapping segment */
/*        lists for different bodies. */

/*        Various small updates and corrections were made to the */
/*        comments throughout the file. */

/* -    SPICELIB Version 3.0.0, 14-AUG-1995 (WLT) */

/*        An interim fix to a bug in SPKBSR was made.  The parameters */
/*        STSIZE and BTSIZE were increased to be much larger than before */
/*        (from 100 and 20 to 2000 and 40 respectively).  This should */
/*        keep the boundary errors experienced by Cassini users from */
/*        occurring again.  Version 4.0.0 with a real fix to the */
/*        boundary problem should be installed in SPICELIB by */
/*        October 1995 */

/* -    SPICELIB Version 2.0.0, 25-NOV-1992 (JML) */

/*     1) When loading a file, SPKLEF now checks if the file table is */
/*        full only after determining that the file is not currently */
/*        loaded. Previously, if the file table was full and an attempt */
/*        was made to reload a file, an error was signaled. A new */
/*        exception was added as a result of this change. */

/*     2) A bug in the way that SPKLEF and SPKUEF clean up the body */
/*        tables after a file is unloaded was fixed. */

/*     3) Variable declarations were added to the example program */
/*        so that it can now be compiled. */

/*     4) A cut and paste error in the description of the segment */
/*        table was corrected. */

/* -    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.2, 9-SEP-1991 (HAN) */

/*        The declaraion of the variable STATE in the Examples section */
/*        was changed from a 3 dimensional vector to a 6 dimensional */
/*        vector, and the term state was specified to be the position */
/*        and velocity of a body relative to another body. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (RET) */

/* -& */
/* $ Index_Entries */

/*     buffer spk segments for readers */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 5.3.0, 01-MAR-2011 (NJB) */

/*        Bug fix: */

/*          In the SPKSFS 'MAKE ROOM' state, when the suspended activity */
/*          is 'ADD TO FRONT' and no segment table room is available, */
/*          the body table's pointer to the current segment list */
/*          is now set to null. Previously the pointer was allowed to go */
/*          stale. */

/* -    SPICELIB Version 5.1.0, 08-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MOVED calls in entry points SPKUEF and SPKSFS. */

/*        Increased segment table buffer size to 30000 entries. */

/* -    SPICELIB Version 5.0.0, 21-FEB-2003 (NJB) */

/*        Increased segment table buffer size to 10000 entries. */

/* -    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB) */


/*        Bug fixes: */

/*           1) When a segment list is freed because the entire list */
/*              is contributed by a single SPK file, and the list is */
/*              too large to be buffered, the corresponding body table */
/*              pointer is now set to null. */

/*           2) An algorithm change has eliminated a bug caused by not */
/*              updating the current body index when body table entries */
/*              having empty segment lists were compressed out of the */
/*              body table.  Previously the body table pointer BINDEX */
/*              could go stale after the compression. */

/*           3) When a already loaded kernel is re-opened with DAFOPR, */
/*              it now has its link count reset to 1 via a call to */
/*              DAFCLS. */

/*           4) The load routine SPKLEF now resets all file numbers when */
/*              the next file number reaches INTMAX()-1, thereby */
/*              avoiding arithmetic overflow. */

/*           5) The unload routine SPKUEF now calls RETURN() on entry and */
/*              returns if so directed. */

/*           6) In SPKSFS, DAF calls are followed by tests of FAILED() */
/*              in order to ensure that the main state loop terminates. */

/*           7) In SPKSFS, a subscript bound violation in a loop */
/*              termination test was corrected. */

/*        The "re-use interval" feature was introduced to improve speed */
/*        in the case where repeated, consecutive requests are satisified */
/*        by the same segment.  For each body, the associated re-use */
/*        interval marks the time interval containing the previous */
/*        request time for which the previously returned segment provides */
/*        the  highest-priority data available. */

/*        The segment list cost algorithm was modified slightly: */
/*        the contribution of a file search to the cost of a list */
/*        is included only when the file search is completed.  The */
/*        cost of finding the re-use interval is accounted for when */
/*        unbuffered searches are required. */

/*        The file table size has been increased to 1000, in order */
/*        to take advantage of the DAF system's new ability to load */
/*        1000 files. */

/*        The body table size has been increased to 200 in order to */
/*        decrease the chance of thrashing due to swapping segment */
/*        lists for different bodies. */

/*        Various small updates and corrections were made to the */
/*        comments throughout the file. */

/*        In order to simplify the source code, the in-line singly */
/*        linked list implementation of the segment table has been */
/*        replaced by an implementation relying on the SPICELIB */
/*        doubly linked list routines. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Constants used in the doubly linked list structure: */


/*     Local variables */


/*     The file table contains the handle and file number of each file */
/*     that has been loaded for use with the SPK readers. File */
/*     numbers begin at one, and are incremented until they reach a */
/*     value of INTMAX() - 1, at which point they are mapped to the */
/*     range 1:NFT, where NFT is the number of loaded SPK files. */

/*     (A file number is similar to a file handle, but it is assigned */
/*     and used exclusively by this module. The purpose of file numbers */
/*     is to keep track of the order in which files are loaded and the */
/*     order in which they are searched.) */

/*     All names begin with FT. */

/*        HAN      Handle */
/*        NUM      File number */

/*     NFT is the number of files that have been loaded. NEXT is */
/*     incremented whenever a new file is loaded to give the file */
/*     number of the file. FINDEX is the index of whatever file is */
/*     of current interest at any given time. */

/*     New files are added at the end of the table. As files are */
/*     removed, succeeding files are moved forward to take up the */
/*     slack. This keeps the table ordered by file number. */


/*     The body table contains the beginning of the list of the stored */
/*     segments for each body, and the expense at which that list */
/*     was constructed. (The expense of a body list is the number of */
/*     segment descriptors examined during the construction of the list.) */
/*     It also contains the highest and lowest file numbers searched */
/*     during the construction of the list. */

/*     For each body, the time bounds of the "re-use interval" of the */
/*     last segment found are stored.  This interval is the maximal */
/*     interval containing the epoch of the last request for data for */
/*     this body, such that the interval is not masked by higher-priority */
/*     segments.  The handle, segment descriptor, and segment identifier */
/*     returned on the last request are also stored. */

/*     All names begin with BT. */

/*        BOD      Body */
/*        EXP      Expense */
/*        HFS      Highest file (number) searched */
/*        LFS      Lowest  file (number) searched */
/*        BEG      Beginning of segment list */
/*        LB       Lower bound of the re-use interval of */
/*                 previous segment returned. */
/*        UB       Upper bound of the re-use interval of */
/*                 previous segment returned. */
/*        PRVD     Previous descriptor returned. */
/*        PRVI     Previous segment identifier returned. */
/*        PRVH     Previous handle returned. */
/*        CHKP     Logical indicating that previous segment should */
/*                 be checked to see whether it satisfies a request. */
/*        RUEX     Expense of the re-use interval. */

/*     NBT is the number of bodies for which segments are currently */
/*     being stored in the table. BINDEX is the index of whatever */
/*     body is of current interest at any given time. */

/*     New bodies are added at the end of the table. As bodies are */
/*     removed, the last body is moved forward to take up the slack. */
/*     This keeps the entries in the table contiguous. */


/*     The segment table contains the handle, descriptor, and identifier */
/*     for each segment that has been found so far. */

/*     The segment table is implemented as a set of arrays indexed by */
/*     a SPICE doubly linked list structure.  For each body in the */
/*     body table, there is a segment table list; each node of a list */
/*     points to data associated with a segment.  In each list, the head */
/*     node corresponds to the highest-priority segment in that list, */
/*     and segment priority decreases in the forward direction. */

/*     All names begin with ST. */

/*        POOL     Doubly linked list pool. */
/*        HAN      Handle */
/*        DES      Descriptor */
/*        IDNT     Identifier */

/*     New segments are added to the front or end of a body list */
/*     as appropriate, according to the rules spelled out under */
/*     entry point SPKSFS. */


/*     Other stuff */


/*     Saved variables */


/*     Initial values */

    /* Parameter adjustments */
    if (descr) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_spklef;
	case 2: goto L_spkuef;
	case 3: goto L_spksfs;
	}


/*     Nobody has any business calling SPKBSR directly. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKBSR", (ftnlen)6);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("SPKBSR", (ftnlen)6);
    return 0;
/* $Procedure SPKLEF ( S/P Kernel, Load ephemeris file ) */

L_spklef:
/* $ Abstract */

/*     Load an ephemeris file for use by the readers.  Return that */
/*     file's handle, to be used by other SPK routines to refer to the */
/*     file. */

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

/*     SPK */

/* $ Keywords */

/*     EPHEMERIS */
/*     FILES */

/* $ Declarations */

/*     CHARACTER*(*)         FNAME */
/*     INTEGER               HANDLE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     FNAME      I   Name of the file to be loaded. */
/*     HANDLE     O   Loaded file's handle. */
/*     FTSIZE     P   Maximum number of loaded SPK files. */

/* $ Detailed_Input */

/*     FNAME      Character name of the file to be loaded. */

/* $ Detailed_Output */

/*     HANDLE     Integer handle assigned to the file upon loading. */
/*                Almost every other SPK routine will subsequently use */
/*                this number to refer to the file. */

/* $ Parameters */

/*     FTSIZE     is the maximum number of SPK files that may */
/*                be loaded simultaneously under any circumstances. */
/*                FTSIZE is currently set to match the maximum number */
/*                of DAF files that may be loaded simultaneously. */

/* $ Exceptions */

/*     1) If an attempt is made to open more DAF files than is specified */
/*        by the parameter FTSIZE in DAFAH, an error is signaled by a */
/*        routine in the call tree of this routine. */

/*     2) If an attempt is made to load more files than is specified */
/*        by the local paramater FTSIZE, and if the DAF system has */
/*        room to load another file, the error SPICE(SPKFILETABLEFULL) */
/*        signaled.  The current setting of FTSIZE does not allow this */
/*        situation to arise:  the DAF system will trap the error */
/*        before this routine has the chance. */

/* $ Files */

/*     A file specified by FNAME, to be loaded.  The file is assigned a */
/*     handle by SPKLEF, which will be used by most other routines to */
/*     refer to it. */

/* $ Particulars */

/*     If there is room for a new file in the file table, SPKLEF creates */
/*     an entry for it and loads the file for reading using DAFOPR. */

/* $ Examples */

/*     See the Example above, in SPKBSR. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     SPK Required Reading. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     J.M. Lynch      (JPL) */
/*     R.E. Thurman    (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 5.0.0, 17-MAR-2014 (NJB) */

/*        Updated segment pool initialization condition in entry */
/*        point SPKLEF so that the pool is initialized only if the file */
/*        table is empty. */

/* -    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB) */

/*        Bug fixes: */

/*        1) When an already loaded kernel is opened with DAFOPR, */
/*           it now has its link count reset to 1 via a call to */
/*           DAFCLS. */

/*        2) This routine now resets all file numbers when */
/*           the next file number reaches INTMAX()-1, thereby avoiding */
/*           arithmetic overflow.  The numbers in the file table */
/*           are replaced with consecutive integers in the range */
/*           1 : NFT, such that the ordering of the numbers is not */
/*           changed.  The HFS and LFS arrays are updated accordingly. */

/*        Also, the flags indicating validity of the re-use intervals */
/*        are set to .FALSE. here. */


/* -    SPICELIB Version 2.0.0, 25-NOV-1992 (JML) */

/*     1) When loading a file, SPKLEF now checks if the file table is */
/*        full only after determining that the file is not currently */
/*        loaded. Previously, if the file table was full and an attempt */
/*        was made to reload a file, an error was signaled.  A new */
/*        exception was added as a result of this change. */

/*     2) A bug in the way that SPKLEF and SPKUEF clean up the body */
/*        tables after a file is unloaded was fixed. */

/* -    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (RET) */

/* -& */
/* $ Index_Entries */

/*     load spk ephemeris file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB) */

/*        Bug fixes: */

/*        1) When a loaded kernel is opened with DAFOPR, */
/*           it now has its link count reset to 1 via a call to */
/*           DAFCLS. */

/*        2) This routine now resets all file numbers when */
/*           the next file number reaches INTMAX()-1, thereby avoiding */
/*           arithmetic overflow.  The numbers in the file table */
/*           are replaced with consecutive integers in the range */
/*           1 : NFT, such that the ordering of the numbers is not */
/*           changed.  The HFS and LFS arrays are updated accordingly. */
/*           HFS and LFS entries that have gone stale are set to zero. */

/*        Also, the flags indicating validity of the re-use intervals */
/*        are set to .FALSE. here. */


/* -    SPICELIB Version 3.0.0, 14-AUG-1995 (WLT) */

/*        An interim fix to a bug in SPKBSR was made.  The parameters */
/*        STSIZE and BTSIZE were increase to be much larger than before */
/*        (from 100 and 20 to 2000 and 40 respectively).  This should */
/*        keep the boundary errors experienced by Cassini users from */
/*        occurring again.  Version 4.0.0 with a real fix to the */
/*        boundary problem should be installed in SPICELIB by */
/*        October 1995 */

/* -    SPICELIB Version 2.0.0, 25-NOV-1992 (JML) */

/*     1) When loading a file, SPKLEF now checks if the file table is */
/*        full only after determining that the file is not currently */
/*        loaded. Previously, if the file table was full and an attempt */
/*        was made to reload a file, an error was signaled.  A new */
/*        exception was added as a result of this change. */

/*     2) A bug in the way that SPKLEF and SPKUEF clean up the body */
/*        tables after a file is unloaded was fixed. */

/*        If as the result of loading a file that was previously loaded, */
/*        there are no more segments buffered for a particular body, */
/*        the counter variable for the bodies is no longer incremented. */

/*        The following code fragment changed: */

/*           IF ( BTBEG( I ) .EQ. 0 ) THEN */

/*              . */
/*              . */
/*              . */
/*              NBT = NBT - 1 */

/*           END IF */

/*           I = I + 1 */

/*        This is the fix: */

/*           IF ( BTBEG( I ) .EQ. 0 ) THEN */

/*              . */
/*              . */
/*              . */
/*              NBT = NBT - 1 */

/*           ELSE */

/*              I = I + 1 */

/*           END IF */

/* -    Beta Version 1.1.0, 25-JAN-1990 (IMU) */

/*        If a file that has already been loaded is loaded a second */
/*        (or third or fourth) time, it should be removed from the */
/*        file table, and any segments from the file must be removed */
/*        from the segment lists, just as if the user had unloaded */
/*        the file before loading it again. This means that a single */
/*        file cannot occur more than once in the file table. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPKLEF", (ftnlen)6);
    }

/*     Any time we load a file, there is a possibility that the */
/*     re-use intervals are invalid because they're been superseded */
/*     by higher-priority data.  Since we're not going to examine */
/*     the loaded file, simply indicate that all of the re-use */
/*     intervals are invalid. */

    i__1 = nbt;
    for (i__ = 1; i__ <= i__1; ++i__) {
	btchkp[(i__2 = i__ - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge("btchkp", 
		i__2, "spkbsr_", (ftnlen)1089)] = FALSE_;
    }

/*     Nothing works unless at least one file has been loaded, so this */
/*     is as good a place as any to initialize the segment table pool. */
/*     We want to avoid unnecessary initializations, so we only */
/*     initialize the list when no files are loaded. It's quite possible */
/*     to have files loaded and an empty body table, so we don't */
/*     want to re-initialize just because there are no body table */
/*     entries. */

    if (nft == 0) {
	lnkini_(&c_b8, stpool);
    }

/*     To load a new file, first try to open it for reading. */

    dafopr_(fname, handle, fname_len);
    if (failed_()) {
	chkout_("SPKLEF", (ftnlen)6);
	return 0;
    }

/*     Determine if the file is already in the table. */

    findex = isrchi_(handle, &nft, fthan);
    if (findex > 0) {

/*        The last call we made to DAFOPR added another DAF link to */
/*        the SPK file.  Remove this link. */

	dafcls_(handle);

/*        Remove the file from the file table and remove its segments */
/*        from the segment table.  If the segment list for a body */
/*        becomes empty, remove that body from the body table. */

	--nft;
	i__1 = nft;
	for (i__ = findex; i__ <= i__1; ++i__) {
	    fthan[(i__2 = i__ - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge("fthan"
		    , i__2, "spkbsr_", (ftnlen)1135)] = fthan[(i__3 = i__) < 
		    5000 && 0 <= i__3 ? i__3 : s_rnge("fthan", i__3, "spkbsr_"
		    , (ftnlen)1135)];
	    ftnum[(i__2 = i__ - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge("ftnum"
		    , i__2, "spkbsr_", (ftnlen)1136)] = ftnum[(i__3 = i__) < 
		    5000 && 0 <= i__3 ? i__3 : s_rnge("ftnum", i__3, "spkbsr_"
		    , (ftnlen)1136)];
	}
	i__ = 1;
	while(i__ <= nbt) {
	    p = btbeg[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		    "btbeg", i__1, "spkbsr_", (ftnlen)1143)];
	    while(p > 0) {

/*              Find the successor of P, if any. */

		nxtseg = lnknxt_(&p, stpool);
		if (sthan[(i__1 = p - 1) < 100000 && 0 <= i__1 ? i__1 : 
			s_rnge("sthan", i__1, "spkbsr_", (ftnlen)1151)] == *
			handle) {

/*                 The segment corresponding to node P came from */
/*                 the file we're unloading.  Delete the node for */
/*                 P from the segment list for body I; if P happens */
/*                 to be the head node for body I's segment list, */
/*                 make the successor of P the head of the list. */

		    lnkfsl_(&p, &p, stpool);
		    if (p == btbeg[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 
			    : s_rnge("btbeg", i__1, "spkbsr_", (ftnlen)1161)])
			     {
			btbeg[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : 
				s_rnge("btbeg", i__1, "spkbsr_", (ftnlen)1162)
				] = nxtseg;
		    }
		}

/*              Update P. */

		p = nxtseg;
	    }

/*           If the list for this body is now empty, shorten the current */
/*           table by one: put all the entries for the last body in the */
/*           table into the space occupied by the one we've deleted. */

	    if (btbeg[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		    "btbeg", i__1, "spkbsr_", (ftnlen)1178)] <= 0) {

/*              Because all of the re-use intervals are invalid, we need */
/*              not copy the saved items associated with them.  The */
/*              items not copied are */

/*                 BTCHKP */
/*                 BTLB */
/*                 BTPRVD */
/*                 BTPRVH */
/*                 BTPRVI */
/*                 BTRUEX */
/*                 BTUB */

		btbod[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"btbod", i__1, "spkbsr_", (ftnlen)1192)] = btbod[(
			i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"btbod", i__2, "spkbsr_", (ftnlen)1192)];
		btexp[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"btexp", i__1, "spkbsr_", (ftnlen)1193)] = btexp[(
			i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"btexp", i__2, "spkbsr_", (ftnlen)1193)];
		bthfs[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"bthfs", i__1, "spkbsr_", (ftnlen)1194)] = bthfs[(
			i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"bthfs", i__2, "spkbsr_", (ftnlen)1194)];
		btlfs[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"btlfs", i__1, "spkbsr_", (ftnlen)1195)] = btlfs[(
			i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"btlfs", i__2, "spkbsr_", (ftnlen)1195)];
		btbeg[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"btbeg", i__1, "spkbsr_", (ftnlen)1196)] = btbeg[(
			i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"btbeg", i__2, "spkbsr_", (ftnlen)1196)];
		--nbt;
	    } else {
		++i__;
	    }
	}
    } else {

/*        This is a new file.  Make sure that there are unused slots */
/*        in the file table. */

	if (nft == 5000) {

/*           This error case can occur only if FTSIZE is larger than */
/*           the maximum number of open DAF files.  Currently FTSIZE */
/*           is equal to this limit. */

	    dafcls_(handle);
	    setmsg_("The internal file table is already full, with # entries."
		    , (ftnlen)56);
	    errint_("#", &c__5000, (ftnlen)1);
	    sigerr_("SPICE(SPKFILETABLEFULL)", (ftnlen)23);
	    chkout_("SPKLEF", (ftnlen)6);
	    return 0;
	}
    }

/*     Determine the next file number.  Note that later code assumes */
/*     that the file number can be incremented by 1, so we can't allow */
/*     the file number to reach INTMAX(). */

    if (next < intmax_() - 1) {
	++next;
    } else {

/*        The user is to be congratulated:  we've run out of file */
/*        numbers. */

/*        Re-set the valid file numbers so they lie in the range 1:NFT, */
/*        with the Ith file in the file table having file number I. */
/*        First update the LFS and HFS components of the body table */
/*        according to this mapping. */

/*        Set any body table entries that are lower than FTNUM(1) to */
/*        zero. */

	i__1 = nbt;
	for (i__ = 1; i__ <= i__1; ++i__) {

/*           Re-map the HFS table for the Ith body. */

	    j = isrchi_(&bthfs[(i__2 = i__ - 1) < 200 && 0 <= i__2 ? i__2 : 
		    s_rnge("bthfs", i__2, "spkbsr_", (ftnlen)1258)], &nft, 
		    ftnum);
	    if (j > 0) {

/*              The highest file searched for body I is the Jth file */
/*              in the file table. */

		bthfs[(i__2 = i__ - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"bthfs", i__2, "spkbsr_", (ftnlen)1265)] = j;
	    } else {

/*              The highest file searched for body I is not in the file */
/*              table.  This occurs when the highest file searched has */
/*              been unloaded.  Note that this assigment makes all files */
/*              appear to be "new" when a lookup for body I is performed. */

		bthfs[(i__2 = i__ - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"bthfs", i__2, "spkbsr_", (ftnlen)1274)] = 0;
	    }

/*           Re-map the LFS table for the Ith body. */

	    j = isrchi_(&btlfs[(i__2 = i__ - 1) < 200 && 0 <= i__2 ? i__2 : 
		    s_rnge("btlfs", i__2, "spkbsr_", (ftnlen)1281)], &nft, 
		    ftnum);
	    if (j > 0) {

/*              The lowest file searched for body I is the Jth file */
/*              in the file table. */

		btlfs[(i__2 = i__ - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"btlfs", i__2, "spkbsr_", (ftnlen)1288)] = j;
	    } else {

/*              The lowest file searched for body I is not in the file */
/*              table.  This occurs when the lowest file searched has */
/*              been unloaded.  Force reconstruction of the list by */
/*              making all files "new." */

		btlfs[(i__2 = i__ - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"btlfs", i__2, "spkbsr_", (ftnlen)1297)] = 0;
		bthfs[(i__2 = i__ - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"bthfs", i__2, "spkbsr_", (ftnlen)1298)] = 0;
	    }
	}

/*        Re-map the file number table itself. */

	i__1 = nft;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ftnum[(i__2 = i__ - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge("ftnum"
		    , i__2, "spkbsr_", (ftnlen)1309)] = i__;
	}

/*        Assign a new file number. */

	next = nft + 1;
    }
    ++nft;
    fthan[(i__1 = nft - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge("fthan", i__1, 
	    "spkbsr_", (ftnlen)1322)] = *handle;
    ftnum[(i__1 = nft - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge("ftnum", i__1, 
	    "spkbsr_", (ftnlen)1323)] = next;
    chkout_("SPKLEF", (ftnlen)6);
    return 0;
/* $Procedure SPKUEF ( S/P Kernel, Unload ephemeris file ) */

L_spkuef:
/* $ Abstract */

/*     Unload an ephemeris file so that it will no longer be searched by */
/*     the readers. */

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

/*     SPK */

/* $ Keywords */

/*     EPHEMERIS */
/*     FILES */

/* $ Declarations */

/*     INTEGER               HANDLE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of file to be unloaded */

/* $ Detailed_Input */

/*     HANDLE     Integer handle assigned to the file upon loading. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) Unloading a file that has not been loaded is a no-op. */
/*        No error is signaled. */

/* $ Files */

/*     The file referred to by HANDLE is unloaded. */

/* $ Particulars */

/*     A file is removed from consideration by the readers by a call to */
/*     SPKUEF. */

/*     If the file specified by HANDLE is not currently loaded in the */
/*     SPK system, no action is taken. */

/* $ Examples */

/*     See the Example above, in SPKBSR. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     SPK Required Reading. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     J.M. Lynch      (JPL) */
/*     R.E. Thurman    (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.1.0, 08-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MOVED call. */

/* -    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB) */

/*        Bug fixes: */

/*        1) This routine now calls RETURN() on entry and */
/*           returns if so directed. */

/*        Also, the flags indicating validity of those re-use intervals */
/*        whose data comes from the unloaded file are set to .FALSE. */


/* -    SPICELIB Version 3.0.0, 14-AUG-1995 (WLT) */

/*        An interim fix to a bug in SPKBSR was made.  The parameters */
/*        STSIZE and BTSIZE were increase to be much larger than before */
/*        (from 100 and 20 to 2000 and 40 respectively).  This should */
/*        keep the boundary errors experienced by Cassini users from */
/*        occurring again.  Version 4.0.0 with a real fix to the */
/*        boundary problem should be installed in SPICELIB by */
/*        October 1995 */

/* -    SPICELIB Version 2.0.0, 25-NOV-1992 (JML) */

/*     1) A bug in the way that SPKLEF and SPKUEF clean up the body */
/*        tables after a file is unloaded was fixed. */

/* -    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.1.0, 2-MAY-1990 (RET) */

/*        If unloading a file causes all segments in the list for a */
/*        body to go away, delete that body from the body list. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (RET) */

/* -& */
/* $ Index_Entries */

/*     unload spk ephemeris file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.1.0, 08-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MOVED call. */

/* -    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB) */

/*        Bug fixes: */

/*        1) This routine now calls RETURN() on entry and */
/*           returns if so directed. */

/*        Also, the flags indicating validity of those re-use intervals */
/*        whose data comes from the unloaded file are set to .FALSE. */

/* -    SPICELIB Version 2.0.0, 25-NOV-1992 (JML) */

/*     1) A bug in the way that SPKLEF and SPKUEF clean up the body */
/*        tables after a file is unloaded was fixed. */

/*        If as the result of unloading a file there are no more */
/*        segments buffered for a particular body, the counter variable */
/*        for the bodies is no longer incremented. */

/*        The following code fragment changed: */

/*           IF ( BTBEG( I ) .EQ. 0 ) THEN */

/*              . */
/*              . */
/*              . */
/*              NBT = NBT - 1 */

/*           END IF */

/*           I = I + 1 */

/*        This is the fix: */

/*           IF ( BTBEG( I ) .EQ. 0 ) THEN */

/*              . */
/*              . */
/*              . */
/*              NBT = NBT - 1 */

/*           ELSE */

/*              I = I + 1 */

/*           END IF */

/* -    SPICELIB Version 1.1.0, 2-MAY-1990 (RET) */

/*        If unloading a file causes all segments in the list for a */
/*        body to go away, delete that body from the body list. */

/* -    Beta Version 1.1.0, 25-JAN-1990 (IMU) */

/*        When unloading a file, close it. */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("SPKUEF", (ftnlen)6);

/*     All of the stored segments from the file must be removed */
/*     from the segment table (by returning the corresponding nodes */
/*     to the segment table pool.) */

/*     Don't do anything if the given handle is not in the file table. */

    findex = isrchi_(handle, &nft, fthan);
    if (findex == 0) {
	chkout_("SPKUEF", (ftnlen)6);
	return 0;
    }

/*     First get rid of the entry in the file table. Close the file */
/*     before wiping out the handle. */

    dafcls_(&fthan[(i__1 = findex - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
	    "fthan", i__1, "spkbsr_", (ftnlen)1578)]);
    --nft;
    i__1 = nft;
    for (i__ = findex; i__ <= i__1; ++i__) {
	fthan[(i__2 = i__ - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge("fthan", 
		i__2, "spkbsr_", (ftnlen)1583)] = fthan[(i__3 = i__) < 5000 &&
		 0 <= i__3 ? i__3 : s_rnge("fthan", i__3, "spkbsr_", (ftnlen)
		1583)];
	ftnum[(i__2 = i__ - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge("ftnum", 
		i__2, "spkbsr_", (ftnlen)1584)] = ftnum[(i__3 = i__) < 5000 &&
		 0 <= i__3 ? i__3 : s_rnge("ftnum", i__3, "spkbsr_", (ftnlen)
		1584)];
    }

/*     Check each body list individually. Note that the first node */
/*     on each list, having no predecessor, must be handled specially. */

    i__ = 1;
    while(i__ <= nbt) {
	p = btbeg[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("btbeg",
		 i__1, "spkbsr_", (ftnlen)1595)];
	while(p > 0) {
	    nxtseg = lnknxt_(&p, stpool);
	    if (sthan[(i__1 = p - 1) < 100000 && 0 <= i__1 ? i__1 : s_rnge(
		    "sthan", i__1, "spkbsr_", (ftnlen)1601)] == *handle) {
		if (p == btbeg[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : 
			s_rnge("btbeg", i__1, "spkbsr_", (ftnlen)1603)]) {
		    btbeg[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			    "btbeg", i__1, "spkbsr_", (ftnlen)1604)] = nxtseg;
		}
		lnkfsl_(&p, &p, stpool);
	    }
	    p = nxtseg;
	}

/*        If we happened to get rid of all of the segments for this */
/*        body, then the body should be deleted from the table: shift */
/*        all entries for the body at the end of the table into the */
/*        space occupied by the deleted body. */

	if (btbeg[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("btbeg",
		 i__1, "spkbsr_", (ftnlen)1621)] <= 0) {
	    if (i__ != nbt) {
		btbod[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"btbod", i__1, "spkbsr_", (ftnlen)1625)] = btbod[(
			i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"btbod", i__2, "spkbsr_", (ftnlen)1625)];
		btexp[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"btexp", i__1, "spkbsr_", (ftnlen)1626)] = btexp[(
			i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"btexp", i__2, "spkbsr_", (ftnlen)1626)];
		bthfs[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"bthfs", i__1, "spkbsr_", (ftnlen)1627)] = bthfs[(
			i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"bthfs", i__2, "spkbsr_", (ftnlen)1627)];
		btlfs[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"btlfs", i__1, "spkbsr_", (ftnlen)1628)] = btlfs[(
			i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"btlfs", i__2, "spkbsr_", (ftnlen)1628)];
		btbeg[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"btbeg", i__1, "spkbsr_", (ftnlen)1629)] = btbeg[(
			i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"btbeg", i__2, "spkbsr_", (ftnlen)1629)];
		btlb[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"btlb", i__1, "spkbsr_", (ftnlen)1630)] = btlb[(i__2 =
			 nbt - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge("btlb", 
			i__2, "spkbsr_", (ftnlen)1630)];
		btub[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"btub", i__1, "spkbsr_", (ftnlen)1631)] = btub[(i__2 =
			 nbt - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge("btub", 
			i__2, "spkbsr_", (ftnlen)1631)];
		btprvh[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"btprvh", i__1, "spkbsr_", (ftnlen)1632)] = btprvh[(
			i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"btprvh", i__2, "spkbsr_", (ftnlen)1632)];
		s_copy(btprvi + ((i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : 
			s_rnge("btprvi", i__1, "spkbsr_", (ftnlen)1633)) * 40,
			 btprvi + ((i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 
			: s_rnge("btprvi", i__2, "spkbsr_", (ftnlen)1633)) * 
			40, (ftnlen)40, (ftnlen)40);
		btchkp[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"btchkp", i__1, "spkbsr_", (ftnlen)1634)] = btchkp[(
			i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"btchkp", i__2, "spkbsr_", (ftnlen)1634)];
		btruex[(i__1 = i__ - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"btruex", i__1, "spkbsr_", (ftnlen)1635)] = btruex[(
			i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"btruex", i__2, "spkbsr_", (ftnlen)1635)];
		moved_(&btprvd[(i__1 = nbt * 5 - 5) < 1000 && 0 <= i__1 ? 
			i__1 : s_rnge("btprvd", i__1, "spkbsr_", (ftnlen)1637)
			], &c__5, &btprvd[(i__2 = i__ * 5 - 5) < 1000 && 0 <= 
			i__2 ? i__2 : s_rnge("btprvd", i__2, "spkbsr_", (
			ftnlen)1637)]);
	    }
	    --nbt;
	} else {
	    ++i__;
	}
    }

/*     Any time we unload a file, we may be removing the file */
/*     providing data for the re-use interval for one or more bodies. */
/*     For each body, if the handle associated with the re-use interval */
/*     happens to be that of the file we're unloading, indicate */
/*     that the re-use interval is invalid. */

    i__1 = nbt;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (btchkp[(i__2 = i__ - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge("btch"
		"kp", i__2, "spkbsr_", (ftnlen)1660)]) {
	    if (btprvh[(i__2 = i__ - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
		    "btprvh", i__2, "spkbsr_", (ftnlen)1662)] == *handle) {
		btchkp[(i__2 = i__ - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"btchkp", i__2, "spkbsr_", (ftnlen)1663)] = FALSE_;
	    }
	}
    }
    chkout_("SPKUEF", (ftnlen)6);
    return 0;
/* $Procedure SPKSFS ( S/P Kernel, Select file and segment ) */

L_spksfs:
/* $ Abstract */

/*     Search through loaded files to find the first segment applicable */
/*     to the body and time specified.  Buffer searched segments in the */
/*     process, to attempt to avoid re-reading files. */

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

/*     SPK */

/* $ Keywords */

/*     EPHEMERIS */
/*     FILES */

/* $ Declarations */

/*     INTEGER               BODY */
/*     DOUBLE PRECISION      ET */
/*     INTEGER               HANDLE */
/*     DOUBLE PRECISION      DESCR  ( * ) */
/*     CHARACTER*(*)         IDENT */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     BODY       I   Body ID. */
/*     ET         I   Ephemeris time. */
/*     HANDLE     O   Handle of file containing the applicable segment. */
/*     DESCR      O   Descriptor of the applicable segment. */
/*     IDENT      O   Identifier of the applicable segment. */
/*     FOUND      O   Indicates whether or not a segment was found. */

/* $ Detailed_Input */

/*     BODY       is the NAIF integer code of an ephemeris object, */
/*                typically a solar system body. */

/*     ET         is a time, in seconds past the epoch J2000 TDB. */

/* $ Detailed_Output */

/*     HANDLE     on output is the handle of the S/P-kernel file */
/*                containing a located segment. */

/*     DESCR      is the descriptor of a located segment. */

/*     IDENT      is the identifier of a located segment. */

/*     FOUND      indicates whether a requested segment was found or not. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If an attempt is made to call SPKSFS when there aren't any */
/*        files loaded, the error SPICE(NOLOADEDFILES) is signaled. */

/* $ Files */

/*     All files loaded by SPKLEF are potential search targets for */
/*     SPKSFS. */

/* $ Particulars */

/*     This routine finds the highest-priority segment, in any loaded */
/*     SPK file, such that the segment provides data for the specified */
/*     body and epoch. */

/* $ Examples */

/*     See the Example above, in SPKBSR. */

/* $ Restrictions */

/*     1) If Fortran I/O errors occur while searching a loaded SPK */
/*        file, the internal state of this suite of routines may */
/*        be corrupted.  It may be possible to correct the state */
/*        by unloading the pertinent SPK files and then re-loading */
/*        them. */

/* $ Literature_References */

/*     SPK Required Reading. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     R.E. Thurman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.2.0, 01-MAR-2011 (NJB) */

/*        Bug fix: */

/*          In the SPKSFS 'MAKE ROOM' state, when the suspended activity */
/*          is 'ADD TO FRONT' and no segment table room is available, */
/*          the body table's pointer to the current segment list */
/*          is now set to null. Previously the pointer was allowed to go */
/*          stale. */

/* -    SPICELIB Version 4.1.0, 08-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MOVED call. */

/* -    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB) */

/*        Bug fixes: */

/*           1) When a segment list is freed because the entire list */
/*              is contributed by a single SPK file, and the list is */
/*              too large to be buffered, the corresponding body table */
/*              pointer is now set to null. */

/*           2) An algorithm change has eliminated a bug caused by not */
/*              updating the current body index when body table entries */
/*              having empty segment lists were compressed out of the */
/*              body table.  Previously the body table pointer BINDEX */
/*              could go stale after the compression. */

/*           3) DAF calls are now followed by tests of FAILED() */
/*              in order to ensure that the main state loop terminates. */

/*           4) A subscript bound violation in a loop termination test */
/*              was corrected. */

/*        The "re-use interval" feature was introduced to improve speed */
/*        in the case where repeated, consecutive requests are satisified */
/*        by the same segment. */

/*        The segment list cost algorithm was modified slightly: */
/*        the contribution of a file search to the cost of a list */
/*        is included only when the file search is completed.  The */
/*        cost of finding the re-use interval is accounted for when */
/*        unbuffered searches are required. */

/*        The file table size has been increased to 1000, in order */
/*        to take advantage of the DAF system's new ability to load */
/*        1000 files. */

/*        The body table size has been increased to 200 in order to */
/*        decrease the chance of thrashing due to swapping segment */
/*        lists for different bodies. */

/*        Various small updates and corrections were made to the */
/*        comments throughout the file. */


/* -    SPICELIB Version 3.0.0, 14-AUG-1995 (WLT) */

/*        An interim fix to a bug in SPKBSR was made.  The parameters */
/*        STSIZE and BTSIZE were increase to be much larger than before */
/*        (from 100 and 20 to 2000 and 40 respectively).  This should */
/*        keep the boundary errors experienced by Cassini users from */
/*        occurring again.  Version 4.0.0 with a real fix to the */
/*        boundary problem should be installed in SPICELIB by */
/*        October 1995 */

/* -    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.1.0, 2-MAY-1990 (RET) */

/*        New error detected. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (RET) */

/* -& */
/* $ Index_Entries */

/*     select spk file and segment */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.2.0, 01-MAR-2011 (NJB) */

/*        Bug fix: */

/*          In the SPKSFS 'MAKE ROOM' state, when the suspended activity */
/*          is 'ADD TO FRONT' and no segment table room is available, */
/*          the body table's pointer to the current segment list */
/*          is now set to null. Previously the pointer was allowed to go */
/*          stale. */

/* -    SPICELIB Version 4.1.0, 08-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MOVED call. */

/* -    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB) */

/*        Bug fixes: */

/*           1) When a segment list is freed because the entire list */
/*              is contributed by a single SPK file, and the list is */
/*              too large to be buffered, the corresponding body table */
/*              pointer is now set to null. */

/*           2) An algorithm change has eliminated a bug caused by not */
/*              updating the current body index when body table entries */
/*              having empty segment lists were compressed out of the */
/*              body table.  Previously the body table pointer BINDEX */
/*              could go stale after the compression. */

/*           3) DAF calls are now followed by tests of FAILED() */
/*              in order to ensure that the main state loop terminates. */

/*           4) A subscript bound violation in a loop termination test */
/*              was corrected.  The loop is located in the */
/*              'SEARCH W/O BUFFERING' block; it finds the start of a */
/*              partial list that is to be freed. */

/*        The "re-use interval" feature was introduced to improve speed */
/*        in the case where repeated, consecutive requests are satisified */
/*        by the same segment. */

/*        The segment list cost algorithm was modified slightly: */
/*        the contribution of a file search to the cost of a list */
/*        is included only when the file search is completed.  The */
/*        cost of finding the re-use interval is accounted for when */
/*        unbuffered searches are required. */

/*        The file table size has been increased to 1000, in order */
/*        to take advantage of the DAF system's new ability to load */
/*        1000 files. */

/*        The body table size has been increased to 200 in order to */
/*        decrease the chance of thrashing due to swapping segment */
/*        lists for different bodies. */

/*        Various small updates and corrections were made to the */
/*        comments throughout the file. */

/*        In order to simplify the source code, the in-line singly */
/*        linked list implementation of the segment table has been */
/*        replaced by an implementation relying on the SPICELIB */
/*        doubly linked list routines. */

/* -    SPICELIB Version 1.1.0, 2-MAY-1990 (RET) */

/*        If an attempt is made to call SPKSFS when there are no files */
/*        loaded, an error is now signaled. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPKSFS", (ftnlen)6);
    }

/*     Assume the segment is not found, until it actually is. */

    *found = FALSE_;

/*     Buffering segments involves maintaining three tables:  the */
/*     file table, the body table, and the segment table.  The routine */
/*     is broken down into various tasks, described below, which */
/*     perform these manipulations.  A description of the components */
/*     of each table is provided in the declarations section of SPKBSR. */

/*     There must be at least ONE file loaded. */

    if (nft == 0) {
	setmsg_("At least one SPK file needs to be loaded by SPKLEF before b"
		"eginning a search.", (ftnlen)77);
	sigerr_("SPICE(NOLOADEDFILES)", (ftnlen)20);
	chkout_("SPKSFS", (ftnlen)6);
	return 0;
    }

/*     The stack of suspended tasks is empty. */

    top = 0;

/*     In the following loop, we will try to simplify things by */
/*     doing exactly one thing on each pass through the loop. */
/*     After each pass, the status of the loop (STATUS) will be */
/*     adjusted to reflect the next thing that needs to be done. */
/*     Occasionally, the current task will have to be interrupted */
/*     until another task can be carried out. (For example, when */
/*     collecting new segments, an interrupt might place a segment */
/*     at the front or end of the current body list; when placing */
/*     the segment on the list, a second interrupt might free up */
/*     room in the segment table in order to allow the addition */
/*     to proceed.) In this case, the current task will be saved and */
/*     restored after the more urgent task has been completed. */

/*     The loop can terminate in only one of two ways (unless an */
/*     error occurs). First, if an applicable segment is found in */
/*     the segment table, the  handle, descriptor, and identifier for */
/*     the segment are returned immediately.  Second, if the table */
/*     does not contain an applicable segment, and if no files remain */
/*     to be searched, the loop terminates normally, and no data are */
/*     returned. */

/*     The individual tasks are described below. */

/*     'NEW BODY' */


/*        This indicates that the specified body has no segments stored */
/*        for it at all. It must be added to the body table.  (This is */
/*        followed immediately by an OLD FILES search, in which every */
/*        file loaded is considered an old file.) */

/*     'NEW FILES' */

/*        This indicates that at least one new file has been added */
/*        since the last time the segment list for the specified */
/*        body was searched. Find the oldest of these new files, */
/*        and begin a NEW SEGMENTS search in forward order for */
/*        segments to add to the front of the list. */

/*     'NEW SEGMENTS' */

/*        Continue a NEW FILES search, adding segments for the specified */
/*        body to the front of the list. */

/*     'OLD FILES' */

/*        This indicates that although the list has been searched */
/*        and found to contain no applicable segment, some of the */
/*        older files remain to be searched. Find the newest of these */
/*        old files, and begin an OLD SEGMENTS search in backward order. */

/*     'OLD SEGMENTS' */

/*        Continue an OLD FILES search, adding segments for the specified */
/*        body to the end of the list. */

/*     'CHECK LIST' */

/*        This indicates that the list is ready to be searched, */
/*        either because no new files have been added, or because */
/*        segments from a new file or an old file have recently */
/*        been added. */

/*        The list is never checked until all new files have been */
/*        searched. */

/*        If an applicable segment is found, it is returned. */

/*     'MAKE ROOM' (Interrupt) */

/*        This indicates that one of the bodies must be removed, */
/*        along with its stored segments, to make room for another */
/*        body or segment.  The body (other than the one being searched */
/*        for) with the smallest expense is selected for this honor. */

/*     'ADD TO FRONT' (Interrupt) */

/*        This indicates that a segment has been found (during the */
/*        course of a NEW FILES search) and must be added to the front */
/*        of the list. */

/*     'ADD TO END' (Interrupt) */

/*        This indicates that a segment has been found (during the */
/*        course of an OLD FILES search) and must be added to the end */
/*        of the list. */

/*     'SUSPEND' */

/*        This indicates that the current task (DOING) should be */
/*        interrupted until a more urgent task (URGENT) can be */
/*        carried out. The current task is placed on a stack for */
/*        safekeeping. */

/*     'RESUME' */

/*        This indicates that the most recently interrupted task */
/*        should be resumed immediately. */

/*     '?' */

/*        This indicates that the next task is not immediately */
/*        apparent: if new files exist, they should be searched; */
/*        otherwise the list should be checked. */


/*     Is the body already in the body table?  This determines what the */
/*     first task should be. */

    bindex = isrchi_(body, &nbt, btbod);
    if (bindex == 0) {
	s_copy(status, "NEW BODY", (ftnlen)15, (ftnlen)8);
    } else {

/*        Much of the time, the segment used to satisfy the previous */
/*        request for a given body will also satisfy the current request */
/*        for data for that body.  Check whether this is the case. */

	if (btchkp[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		"btchkp", i__1, "spkbsr_", (ftnlen)2128)]) {

/*           The previous segment found for the current body is a */
/*           viable candidate for the current request.  See whether */
/*           the input ET value falls into the re-use interval for this */
/*           body:  the time interval for which the previously returned */
/*           segment for this body provides the highest-priority */
/*           coverage. */

/*           We treat the re-use interval as topologically open because */
/*           one or both endpoints may belong to higher-priority */
/*           segments. */

	    if (*et > btlb[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
		    s_rnge("btlb", i__1, "spkbsr_", (ftnlen)2141)] && *et < 
		    btub[(i__2 = bindex - 1) < 200 && 0 <= i__2 ? i__2 : 
		    s_rnge("btub", i__2, "spkbsr_", (ftnlen)2141)]) {

/*              The request time is covered by the segment found on */
/*              the previous request for data for the current body, */
/*              and this interval is not masked by any higher-priority */
/*              segments.  The previous segment for this body satisfies */
/*              the request. */

		*handle = btprvh[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? 
			i__1 : s_rnge("btprvh", i__1, "spkbsr_", (ftnlen)2150)
			];
		s_copy(ident, btprvi + ((i__1 = bindex - 1) < 200 && 0 <= 
			i__1 ? i__1 : s_rnge("btprvi", i__1, "spkbsr_", (
			ftnlen)2151)) * 40, ident_len, (ftnlen)40);
		moved_(&btprvd[(i__1 = bindex * 5 - 5) < 1000 && 0 <= i__1 ? 
			i__1 : s_rnge("btprvd", i__1, "spkbsr_", (ftnlen)2153)
			], &c__5, descr);
		*found = TRUE_;
		chkout_("SPKSFS", (ftnlen)6);
		return 0;
	    }

/*           Adjust the expense here. If the expense of the list */
/*           contains a component due to the cost of finding the */
/*           unbuffered segment providing data for re-use, subtract */
/*           that component from the expense. */

	    btexp[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		    "btexp", i__1, "spkbsr_", (ftnlen)2168)] = btexp[(i__2 = 
		    bindex - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge("btexp", 
		    i__2, "spkbsr_", (ftnlen)2168)] - btruex[(i__3 = bindex - 
		    1) < 200 && 0 <= i__3 ? i__3 : s_rnge("btruex", i__3, 
		    "spkbsr_", (ftnlen)2168)];
	    btruex[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		    "btruex", i__1, "spkbsr_", (ftnlen)2169)] = 0;

/*           The re-use interval becomes invalid if it didn't satisfy */
/*           the request.  The validity flag gets re-set below. */

/*           At this point, the previous segment is not a candidate */
/*           to satisfy the request---at least not until we've verified */
/*           that */

/*              - The previous segment is still available. */

/*              - The previous segment hasn't been superseded by a more */
/*                recently loaded segment. */

	    btchkp[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		    "btchkp", i__1, "spkbsr_", (ftnlen)2184)] = FALSE_;
	}

/*        If the segment list for this body is empty, make sure the */
/*        expense is reset to 0. */

	if (btbeg[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		"btbeg", i__1, "spkbsr_", (ftnlen)2193)] == 0) {
	    btexp[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		    "btexp", i__1, "spkbsr_", (ftnlen)2195)] = 0;
	}
	s_copy(status, "?", (ftnlen)15, (ftnlen)1);
    }
    while(s_cmp(status, "HOPELESS", (ftnlen)15, (ftnlen)8) != 0) {

/*        If new files have been added, they have to be searched. */
/*        Otherwise, we can go right to the list of stored segments. */

	if (s_cmp(status, "?", (ftnlen)15, (ftnlen)1) == 0) {

/*           There are two ways to get to this point. */

/*           1)  Status may have been set to '?' prior to the */
/*               loop DO WHILE ( STATUS .NE. HOPELESS ). */

/*           2)  Status was set to '?' by the NEW SEGMENTS block */
/*               of code as the result of finishing the read of */
/*               a new file. */

	    if (bthfs[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		    "bthfs", i__1, "spkbsr_", (ftnlen)2222)] < ftnum[(i__2 = 
		    nft - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge("ftnum", 
		    i__2, "spkbsr_", (ftnlen)2222)]) {
		s_copy(status, "NEW FILES", (ftnlen)15, (ftnlen)9);
	    } else {
		s_copy(status, "CHECK LIST", (ftnlen)15, (ftnlen)10);
	    }
	} else if (s_cmp(status, "NEW BODY", (ftnlen)15, (ftnlen)8) == 0) {

/*           New bodies are added to the end of the body table. If the */
/*           table is full, one of the current occupants must be */
/*           removed to make room for the new one. */

/*           Setting LFS to one more than the highest current */
/*           file number means the OLD FILES SEARCH that follows will */
/*           begin with the last-loaded file. */

/*           There is one way to get here: */

/*           1)  The variable STATUS was set to NEW BODY prior to the */
/*               loop DO WHILE ( STATUS .NE. HOPELESS ). */

/*           Find the cheapest slot in the body table to store */
/*           the initial information about this body. */

/*           NOTE:  This used to be handled by the MAKE ROOM section. */
/*           However, trying to handle this special case there was */
/*           just more trouble than it was worth. */

	    if (nbt < 200) {

/*              If the body table isn't full, the cheapest place is */
/*              just the next unused row of the table. */

		++nbt;
		cheap = nbt;
	    } else {

/*              The body table is full.  Find the least */
/*              expensive body in the table and remove it. */

		cheap = 1;
		minexp = btexp[0];
		i__1 = nbt;
		for (i__ = 2; i__ <= i__1; ++i__) {
		    if (btexp[(i__2 = i__ - 1) < 200 && 0 <= i__2 ? i__2 : 
			    s_rnge("btexp", i__2, "spkbsr_", (ftnlen)2271)] < 
			    minexp) {
			cheap = i__;
			minexp = btexp[(i__2 = i__ - 1) < 200 && 0 <= i__2 ? 
				i__2 : s_rnge("btexp", i__2, "spkbsr_", (
				ftnlen)2273)];
		    }
		}

/*              If there are any segments associated with the */
/*              least expensive body, we put them back on the free */
/*              list. */

		head = btbeg[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : 
			s_rnge("btbeg", i__1, "spkbsr_", (ftnlen)2283)];
		if (head > 0) {
		    tail = -lnkprv_(&head, stpool);
		    lnkfsl_(&head, &tail, stpool);
		}
	    }

/*           Set up a body table entry for the new body. */

	    btbod[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("btb"
		    "od", i__1, "spkbsr_", (ftnlen)2297)] = *body;
	    btexp[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("bte"
		    "xp", i__1, "spkbsr_", (ftnlen)2298)] = 0;
	    bthfs[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("bth"
		    "fs", i__1, "spkbsr_", (ftnlen)2299)] = ftnum[(i__2 = nft 
		    - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge("ftnum", i__2, 
		    "spkbsr_", (ftnlen)2299)];
	    btlfs[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("btl"
		    "fs", i__1, "spkbsr_", (ftnlen)2300)] = ftnum[(i__2 = nft 
		    - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge("ftnum", i__2, 
		    "spkbsr_", (ftnlen)2300)] + 1;
	    btbeg[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("btb"
		    "eg", i__1, "spkbsr_", (ftnlen)2301)] = 0;
	    btchkp[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		    "btchkp", i__1, "spkbsr_", (ftnlen)2302)] = FALSE_;

/*           The following items associated with the re-use interval */
/*           need not be initialized at this point: */

/*              BTRUEX */
/*              BTLB */
/*              BTUB */
/*              BTPRVH */
/*              BTPRVI */
/*              BTPRVD */

/*           However, we'll give these items initial values to */
/*           help prevent compilation warnings from zealous */
/*           compilers. */

	    btruex[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		    "btruex", i__1, "spkbsr_", (ftnlen)2319)] = 0;
	    btlb[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("btlb",
		     i__1, "spkbsr_", (ftnlen)2320)] = dpmin_();
	    btub[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("btub",
		     i__1, "spkbsr_", (ftnlen)2321)] = dpmax_();
	    btprvh[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		    "btprvh", i__1, "spkbsr_", (ftnlen)2322)] = 0;
	    s_copy(btprvi + ((i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : 
		    s_rnge("btprvi", i__1, "spkbsr_", (ftnlen)2323)) * 40, 
		    " ", (ftnlen)40, (ftnlen)1);
	    cleard_(&c__5, &btprvd[(i__1 = cheap * 5 - 5) < 1000 && 0 <= i__1 
		    ? i__1 : s_rnge("btprvd", i__1, "spkbsr_", (ftnlen)2324)])
		    ;

/*           BINDEX is the body table index of the new entry. */

	    bindex = cheap;

/*           Now search the loaded SPK files for segments relating to */
/*           this body.  We start with the last-loaded files and */
/*           work backwards. */

	    s_copy(status, "OLD FILES", (ftnlen)15, (ftnlen)9);
	} else if (s_cmp(status, "NEW FILES", (ftnlen)15, (ftnlen)9) == 0) {

/*           When new files exist, they should be searched in forward */
/*           order, beginning with the oldest new file not yet searched. */
/*           All new files must be searched before the list can be */
/*           checked, to ensure that the best (newest) segments are */
/*           being used. */

/*           Begin a forward search, and prepare to look for individual */
/*           segments from the file. */

/*           The only way to get here is to have STATUS set to */
/*           the value NEW FILES in the STATUS .EQ. '?' block */
/*           of the IF structure. */

/*           Find the next file to search; set FINDEX to the */
/*           corresponding file table entry. */

	    findex = 1;
	    while(bthfs[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
		    s_rnge("bthfs", i__1, "spkbsr_", (ftnlen)2360)] >= ftnum[(
		    i__2 = findex - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge(
		    "ftnum", i__2, "spkbsr_", (ftnlen)2360)]) {
		++findex;
	    }
	    bthfs[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		    "bthfs", i__1, "spkbsr_", (ftnlen)2364)] = ftnum[(i__2 = 
		    findex - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge("ftnum", 
		    i__2, "spkbsr_", (ftnlen)2364)];
	    dafbfs_(&fthan[(i__1 = findex - 1) < 5000 && 0 <= i__1 ? i__1 : 
		    s_rnge("fthan", i__1, "spkbsr_", (ftnlen)2366)]);
	    if (failed_()) {
		chkout_("SPKSFS", (ftnlen)6);
		return 0;
	    }
	    s_copy(status, "NEW SEGMENTS", (ftnlen)15, (ftnlen)12);

/*           The cost of the list contributed by the new file is */
/*           zero so far. */

	    cost = 0;
	} else if (s_cmp(status, "NEW SEGMENTS", (ftnlen)15, (ftnlen)12) == 0)
		 {

/*           New files are searched in forward order. Segments, when */
/*           found, are inserted at the front of the list. Invisible */
/*           segments (alpha > omega) are ignored. */

/*           Each segment examined, whether applicable or not, adds to */
/*           the expense of the list. */

/*           The only way to get here is from the NEW FILES block */
/*           of the IF structure. */
	    daffna_(&fnd);
	    if (failed_()) {
		chkout_("SPKSFS", (ftnlen)6);
		return 0;
	    }
	    if (! fnd) {

/*              We're out of segments in the current file.  Decide */
/*              whether we need to examine another new file, or */
/*              whether we're ready to check the list. */

		s_copy(status, "?", (ftnlen)15, (ftnlen)1);
		btexp[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"btexp", i__1, "spkbsr_", (ftnlen)2408)] = btexp[(
			i__2 = bindex - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"btexp", i__2, "spkbsr_", (ftnlen)2408)] + cost;
	    } else {
		dafgs_(descr);
		dafus_(descr, &c__2, &c__6, dcd, icd);
		if (failed_()) {
		    chkout_("SPKSFS", (ftnlen)6);
		    return 0;
		}
		if (icd[0] == *body && dcd[0] <= dcd[1]) {
		    s_copy(doing, "NEW SEGMENTS", (ftnlen)15, (ftnlen)12);
		    s_copy(urgent, "ADD TO FRONT", (ftnlen)15, (ftnlen)12);
		    s_copy(status, "SUSPEND", (ftnlen)15, (ftnlen)7);
		}
		++cost;
	    }

/*           If we haven't reset the status, we'll return for another */
/*           'NEW SEGMENTS' pass. */

	} else if (s_cmp(status, "OLD FILES", (ftnlen)15, (ftnlen)9) == 0) {

/*           When old files must be searched (because the segments */
/*           in the list are inadequate), they should be searched */
/*           in backward order, beginning with the newest old file */
/*           not yet searched. The segment list will be re-checked */
/*           after each file is searched.  If a match is found, */
/*           the search terminates, so some old files may not be */
/*           searched. */

/*           Search from the end, and prepare to look for individual */
/*           segments from the file. */

/*           You can get to this block in two ways. */

/*           1) We can have a NEW BODY */

/*           2) We have checked the current list (CHECK LIST) for */
/*              this body, didn't find an applicable segment and */
/*              have some files left that have not been seached. */
	    findex = nft;
	    while(btlfs[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
		    s_rnge("btlfs", i__1, "spkbsr_", (ftnlen)2460)] <= ftnum[(
		    i__2 = findex - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge(
		    "ftnum", i__2, "spkbsr_", (ftnlen)2460)]) {
		--findex;
	    }
	    dafbbs_(&fthan[(i__1 = findex - 1) < 5000 && 0 <= i__1 ? i__1 : 
		    s_rnge("fthan", i__1, "spkbsr_", (ftnlen)2464)]);
	    if (failed_()) {
		chkout_("SPKSFS", (ftnlen)6);
		return 0;
	    }
	    s_copy(status, "OLD SEGMENTS", (ftnlen)15, (ftnlen)12);

/*           The next thing we'll do is search through all the segments */
/*           of this file for those that applicable to this body. */
/*           The cost of the list contributed by the current file is */
/*           zero so far. */

	    cost = 0;
	} else if (s_cmp(status, "OLD SEGMENTS", (ftnlen)15, (ftnlen)12) == 0)
		 {

/*           Old files are searched in backward order. Segments, when */
/*           found, are inserted at the end of the list. Invisible */
/*           segments (alpha > omega) are ignored. */

/*           Each segment examined, whether applicable or not, adds to */
/*           the expense of the list. */

/*           There is only one way to get here---from the */
/*           block 'OLD FILES'.  Note we do not add to the */
/*           expense of the list for this body until we've */
/*           completely searched this file. */

	    daffpa_(&fnd);
	    if (failed_()) {
		chkout_("SPKSFS", (ftnlen)6);
		return 0;
	    }
	    if (! fnd) {

/*              We've been through all of the segments in this file. */
/*              Change the lowest file searched indicator for this body */
/*              to be the current file, and go check the current list. */

		btlfs[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"btlfs", i__1, "spkbsr_", (ftnlen)2509)] = ftnum[(
			i__2 = findex - 1) < 5000 && 0 <= i__2 ? i__2 : 
			s_rnge("ftnum", i__2, "spkbsr_", (ftnlen)2509)];
		btexp[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"btexp", i__1, "spkbsr_", (ftnlen)2510)] = btexp[(
			i__2 = bindex - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge(
			"btexp", i__2, "spkbsr_", (ftnlen)2510)] + cost;
		s_copy(status, "CHECK LIST", (ftnlen)15, (ftnlen)10);
	    } else {
		dafgs_(descr);
		dafus_(descr, &c__2, &c__6, dcd, icd);
		if (failed_()) {
		    chkout_("SPKSFS", (ftnlen)6);
		    return 0;
		}
		if (icd[0] == *body && dcd[0] <= dcd[1]) {
		    s_copy(doing, "OLD SEGMENTS", (ftnlen)15, (ftnlen)12);
		    s_copy(urgent, "ADD TO END", (ftnlen)15, (ftnlen)10);
		    s_copy(status, "SUSPEND", (ftnlen)15, (ftnlen)7);
		}
		++cost;
	    }

/*           If we haven't reset the status, we'll return for another */
/*           'OLD SEGMENTS' pass. */

	} else if (s_cmp(status, "CHECK LIST", (ftnlen)15, (ftnlen)10) == 0) {

/*           Okay, all the new files (and maybe an old file or two) have */
/*           been searched. Time to look at the list of segments stored */
/*           for the body to see if one applicable to the specified */
/*           epoch is hiding in there. If so, return it.  If not, */
/*           try another old file.  If there are no more old files, */
/*           give up the ghost. */

/*           There are two ways to get to this point. */

/*           1) From the '?' block. */
/*           2) From the 'OLD SEGMENTS' block. */

/*           For every segment examined, initialize the re-use interval */
/*           associated with the current body. */

	    btlb[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("btlb"
		    , i__1, "spkbsr_", (ftnlen)2557)] = dpmin_();
	    btub[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("btub"
		    , i__1, "spkbsr_", (ftnlen)2558)] = dpmax_();
	    p = btbeg[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		    "btbeg", i__1, "spkbsr_", (ftnlen)2559)];
	    while(p > 0) {
		if (*et > stdes[(i__1 = p * 5 - 4) < 500000 && 0 <= i__1 ? 
			i__1 : s_rnge("stdes", i__1, "spkbsr_", (ftnlen)2563)]
			) {

/*                 ET is to the right of the coverage interval of this */
/*                 segment. */

/* Computing MAX */
		    d__1 = btlb[(i__2 = bindex - 1) < 200 && 0 <= i__2 ? i__2 
			    : s_rnge("btlb", i__2, "spkbsr_", (ftnlen)2568)], 
			    d__2 = stdes[(i__3 = p * 5 - 4) < 500000 && 0 <= 
			    i__3 ? i__3 : s_rnge("stdes", i__3, "spkbsr_", (
			    ftnlen)2568)];
		    btlb[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btlb", i__1, "spkbsr_", (ftnlen)2568)] = 
			    max(d__1,d__2);
		} else if (*et < stdes[(i__1 = p * 5 - 5) < 500000 && 0 <= 
			i__1 ? i__1 : s_rnge("stdes", i__1, "spkbsr_", (
			ftnlen)2571)]) {

/*                 ET is to the left of the coverage interval of this */
/*                 segment. */

/* Computing MIN */
		    d__1 = btub[(i__2 = bindex - 1) < 200 && 0 <= i__2 ? i__2 
			    : s_rnge("btub", i__2, "spkbsr_", (ftnlen)2576)], 
			    d__2 = stdes[(i__3 = p * 5 - 5) < 500000 && 0 <= 
			    i__3 ? i__3 : s_rnge("stdes", i__3, "spkbsr_", (
			    ftnlen)2576)];
		    btub[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btub", i__1, "spkbsr_", (ftnlen)2576)] = 
			    min(d__1,d__2);
		} else {

/*                 The segment coverage interval includes ET. */

		    moved_(&stdes[(i__1 = p * 5 - 5) < 500000 && 0 <= i__1 ? 
			    i__1 : s_rnge("stdes", i__1, "spkbsr_", (ftnlen)
			    2582)], &c__5, descr);
		    s_copy(ident, stidnt + ((i__1 = p - 1) < 100000 && 0 <= 
			    i__1 ? i__1 : s_rnge("stidnt", i__1, "spkbsr_", (
			    ftnlen)2583)) * 40, ident_len, (ftnlen)40);
		    *handle = sthan[(i__1 = p - 1) < 100000 && 0 <= i__1 ? 
			    i__1 : s_rnge("sthan", i__1, "spkbsr_", (ftnlen)
			    2584)];
		    *found = TRUE_;

/*                 Set the re-use interval for the current body. */

/* Computing MAX */
		    d__1 = btlb[(i__2 = bindex - 1) < 200 && 0 <= i__2 ? i__2 
			    : s_rnge("btlb", i__2, "spkbsr_", (ftnlen)2590)], 
			    d__2 = stdes[(i__3 = p * 5 - 5) < 500000 && 0 <= 
			    i__3 ? i__3 : s_rnge("stdes", i__3, "spkbsr_", (
			    ftnlen)2590)];
		    btlb[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btlb", i__1, "spkbsr_", (ftnlen)2590)] = 
			    max(d__1,d__2);
/* Computing MIN */
		    d__1 = btub[(i__2 = bindex - 1) < 200 && 0 <= i__2 ? i__2 
			    : s_rnge("btub", i__2, "spkbsr_", (ftnlen)2591)], 
			    d__2 = stdes[(i__3 = p * 5 - 4) < 500000 && 0 <= 
			    i__3 ? i__3 : s_rnge("stdes", i__3, "spkbsr_", (
			    ftnlen)2591)];
		    btub[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btub", i__1, "spkbsr_", (ftnlen)2591)] = 
			    min(d__1,d__2);

/*                 Save the returned output items, in case this segment */
/*                 may satisfy the next request. */

		    btprvh[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btprvh", i__1, "spkbsr_", (ftnlen)2597)] =
			     *handle;
		    s_copy(btprvi + ((i__1 = bindex - 1) < 200 && 0 <= i__1 ? 
			    i__1 : s_rnge("btprvi", i__1, "spkbsr_", (ftnlen)
			    2598)) * 40, ident, (ftnlen)40, ident_len);
		    moved_(descr, &c__5, &btprvd[(i__1 = bindex * 5 - 5) < 
			    1000 && 0 <= i__1 ? i__1 : s_rnge("btprvd", i__1, 
			    "spkbsr_", (ftnlen)2599)]);
		    btchkp[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btchkp", i__1, "spkbsr_", (ftnlen)2600)] =
			     TRUE_;
		    chkout_("SPKSFS", (ftnlen)6);
		    return 0;
		}

/*              Get the next node.  We avoid LNKNXT here in order */
/*              to speed up the operation. */

		p = stpool[(i__1 = (p << 1) + 10) < 200012 && 0 <= i__1 ? 
			i__1 : s_rnge("stpool", i__1, "spkbsr_", (ftnlen)2611)
			];
	    }

/*           If we're still here we didn't have information for this */
/*           body in the segment list. */

/*           If there are more files, search them. */
/*           Otherwise, things are hopeless, set the status that way. */

	    if (btlfs[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		    "btlfs", i__1, "spkbsr_", (ftnlen)2622)] > ftnum[0]) {
		s_copy(status, "OLD FILES", (ftnlen)15, (ftnlen)9);
	    } else {
		s_copy(status, "HOPELESS", (ftnlen)15, (ftnlen)8);
	    }
	} else if (s_cmp(status, "MAKE ROOM", (ftnlen)15, (ftnlen)9) == 0) {

/*           When adding a segment to a full segment table, one of */
/*           the current bodies must be dropped. The ideal candidate */
/*           is the one whose list was constructed at the lowest expense. */
/*           The candidate should be removed from the body table, and */
/*           its list transferred to the segment table pool. */

/*           There is ``room'' if the segment table pool contains at */
/*           least one free node. */

/*           It is possible that a single body requires more than the */
/*           entire segment table for its own segments. Two things might */
/*           happen in such a case: */

/*              1) If the list under consideration was being added to at */
/*                 the end, then a search is continued without buffering */
/*                 any segments. */

/*              2) If the list was being added to at the beginning, then */
/*                 that means there was a NEW FILES search going on, and */
/*                 so a brand new list is constructed for the body, much */
/*                 as in a 'NEW BODY' task. */

/*           There are two different ways to get to this point. */

/*              1) From 'ADD TO FRONT' if the segment table pool is full. */
/*              2) From 'ADD TO END' if the segment table pool is full. */

/*           Try to make room by deleting a segment list.  CHEAP will */
/*           be the index of the "cheapest" segment list in the body */
/*           table. */

	    minexp = intmax_();
	    cheap = 0;
	    i__1 = nbt;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (i__ != bindex) {

/*                 This list is for a body other than the current */
/*                 one. */

		    if (btexp[(i__2 = i__ - 1) < 200 && 0 <= i__2 ? i__2 : 
			    s_rnge("btexp", i__2, "spkbsr_", (ftnlen)2673)] < 
			    minexp || cheap == 0) {

/*                    This list is the cheapest seen so far, */
/*                    possibly because it's the first one */
/*                    considered.  At the moment, it's as good */
/*                    a candidate for removal as any. */

			cheap = i__;
			minexp = btexp[(i__2 = i__ - 1) < 200 && 0 <= i__2 ? 
				i__2 : s_rnge("btexp", i__2, "spkbsr_", (
				ftnlen)2682)];
		    }
		}
	    }
	    if (cheap == 0) {

/*              What we do if there are no delete-able segments */
/*              depends on the task that was suspended before entering */
/*              'MAKE ROOM'. */

		if (s_cmp(stack + ((i__1 = top - 1) < 2 && 0 <= i__1 ? i__1 : 
			s_rnge("stack", i__1, "spkbsr_", (ftnlen)2697)) * 15, 
			"ADD TO END", (ftnlen)15, (ftnlen)10) == 0) {

/*                 There's nothing left to do but search the remaining */
/*                 files and segments without buffering them. */

		    s_copy(status, "SEARCH W/O BUFF", (ftnlen)15, (ftnlen)15);
		} else {

/*                 STACK(TOP) is set to 'ADD TO FRONT'. */

/*                 If there is no room left in the table in the middle */
/*                 of an attempt to add to the front of the list, just */
/*                 start from scratch by treating all files as */
/*                 unsearched and doing an OLD FILES search, as would */
/*                 be done for a new body. */

/*                 Return the current list to the segment table pool. */

/*                 Note that, according to the specification of the */
/*                 SPICELIB doubly linked list routines, the backward */
/*                 pointer of a list head is the negative of the tail */
/*                 node. */

		    p = btbeg[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btbeg", i__1, "spkbsr_", (ftnlen)2722)];
		    tail = -lnkprv_(&p, stpool);
		    lnkfsl_(&p, &tail, stpool);

/*                 Re-initialize the table for this body, and initiate */
/*                 an 'OLD FILES' search, just as in 'NEW BODY'. */
/*                 Also, reset the suspended task stack to be empty. */

		    btbeg[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btbeg", i__1, "spkbsr_", (ftnlen)2732)] = 
			    0;
		    btexp[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btexp", i__1, "spkbsr_", (ftnlen)2733)] = 
			    0;
		    bthfs[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("bthfs", i__1, "spkbsr_", (ftnlen)2734)] = 
			    ftnum[(i__2 = nft - 1) < 5000 && 0 <= i__2 ? i__2 
			    : s_rnge("ftnum", i__2, "spkbsr_", (ftnlen)2734)];
		    btlfs[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btlfs", i__1, "spkbsr_", (ftnlen)2735)] = 
			    ftnum[(i__2 = nft - 1) < 5000 && 0 <= i__2 ? i__2 
			    : s_rnge("ftnum", i__2, "spkbsr_", (ftnlen)2735)] 
			    + 1;
		    s_copy(status, "OLD FILES", (ftnlen)15, (ftnlen)9);
		    top = 0;
		}
	    } else {

/*              Return this cheapest list to the segment pool. */

		p = btbeg[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : 
			s_rnge("btbeg", i__1, "spkbsr_", (ftnlen)2745)];
		if (p > 0) {
		    tail = -lnkprv_(&p, stpool);
		    lnkfsl_(&p, &tail, stpool);
		}

/*              Fill the deleted body's space in the table with */
/*              the final entry in the table. */

		if (cheap != nbt) {
		    btbod[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btbod", i__1, "spkbsr_", (ftnlen)2760)] = 
			    btbod[(i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 :
			     s_rnge("btbod", i__2, "spkbsr_", (ftnlen)2760)];
		    btexp[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btexp", i__1, "spkbsr_", (ftnlen)2761)] = 
			    btexp[(i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 :
			     s_rnge("btexp", i__2, "spkbsr_", (ftnlen)2761)];
		    bthfs[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("bthfs", i__1, "spkbsr_", (ftnlen)2762)] = 
			    bthfs[(i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 :
			     s_rnge("bthfs", i__2, "spkbsr_", (ftnlen)2762)];
		    btlfs[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btlfs", i__1, "spkbsr_", (ftnlen)2763)] = 
			    btlfs[(i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 :
			     s_rnge("btlfs", i__2, "spkbsr_", (ftnlen)2763)];
		    btbeg[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btbeg", i__1, "spkbsr_", (ftnlen)2764)] = 
			    btbeg[(i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 :
			     s_rnge("btbeg", i__2, "spkbsr_", (ftnlen)2764)];
		    btlb[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btlb", i__1, "spkbsr_", (ftnlen)2765)] = 
			    btlb[(i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 : 
			    s_rnge("btlb", i__2, "spkbsr_", (ftnlen)2765)];
		    btub[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btub", i__1, "spkbsr_", (ftnlen)2766)] = 
			    btub[(i__2 = nbt - 1) < 200 && 0 <= i__2 ? i__2 : 
			    s_rnge("btub", i__2, "spkbsr_", (ftnlen)2766)];
		    btprvh[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btprvh", i__1, "spkbsr_", (ftnlen)2767)] =
			     btprvh[(i__2 = nbt - 1) < 200 && 0 <= i__2 ? 
			    i__2 : s_rnge("btprvh", i__2, "spkbsr_", (ftnlen)
			    2767)];
		    s_copy(btprvi + ((i__1 = cheap - 1) < 200 && 0 <= i__1 ? 
			    i__1 : s_rnge("btprvi", i__1, "spkbsr_", (ftnlen)
			    2768)) * 40, btprvi + ((i__2 = nbt - 1) < 200 && 
			    0 <= i__2 ? i__2 : s_rnge("btprvi", i__2, "spkbs"
			    "r_", (ftnlen)2768)) * 40, (ftnlen)40, (ftnlen)40);
		    btruex[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btruex", i__1, "spkbsr_", (ftnlen)2769)] =
			     btruex[(i__2 = nbt - 1) < 200 && 0 <= i__2 ? 
			    i__2 : s_rnge("btruex", i__2, "spkbsr_", (ftnlen)
			    2769)];
		    btchkp[(i__1 = cheap - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btchkp", i__1, "spkbsr_", (ftnlen)2770)] =
			     btchkp[(i__2 = nbt - 1) < 200 && 0 <= i__2 ? 
			    i__2 : s_rnge("btchkp", i__2, "spkbsr_", (ftnlen)
			    2770)];
		    moved_(&btprvd[(i__1 = nbt * 5 - 5) < 1000 && 0 <= i__1 ? 
			    i__1 : s_rnge("btprvd", i__1, "spkbsr_", (ftnlen)
			    2773)], &c__5, &btprvd[(i__2 = cheap * 5 - 5) < 
			    1000 && 0 <= i__2 ? i__2 : s_rnge("btprvd", i__2, 
			    "spkbsr_", (ftnlen)2773)]);
		}

/*              If the final entry in the table happened to be the */
/*              current body of interest, then we also have to change */
/*              the current body index. */

		if (bindex == nbt) {
		    bindex = cheap;
		}

/*              One less body now. */

		--nbt;
		s_copy(status, "RESUME", (ftnlen)15, (ftnlen)6);
	    }

/*           Either we made room by freeing a non-empty segment list, */
/*           or we're going to work without additional space.  In the */
/*           latter case, the state is now 'OLD FILES' or */
/*           'SEARCH W/O BUFF'. */

	} else if (s_cmp(status, "ADD TO FRONT", (ftnlen)15, (ftnlen)12) == 0)
		 {

/*           The current segment information should be linked in at */
/*           the head of the segment list for the current body, and */
/*           the pertinent body table entry should point to the new */
/*           head of the list. */

/*           The only way to get here is from the block NEW SEGMENTS */
/*           after suspending that task. */

	    if (lnknfn_(stpool) == 0) {

/*              There's no room left in the segment pool.  We must make */
/*              room before continuing. */

		s_copy(doing, "ADD TO FRONT", (ftnlen)15, (ftnlen)12);
		s_copy(urgent, "MAKE ROOM", (ftnlen)15, (ftnlen)9);
		s_copy(status, "SUSPEND", (ftnlen)15, (ftnlen)7);
	    } else {

/*              Allocate a node and link it to the front of the list */
/*              for the current body. */

		lnkan_(stpool, &new__);
		sthan[(i__1 = new__ - 1) < 100000 && 0 <= i__1 ? i__1 : 
			s_rnge("sthan", i__1, "spkbsr_", (ftnlen)2827)] = 
			fthan[(i__2 = findex - 1) < 5000 && 0 <= i__2 ? i__2 :
			 s_rnge("fthan", i__2, "spkbsr_", (ftnlen)2827)];
		moved_(descr, &c__5, &stdes[(i__1 = new__ * 5 - 5) < 500000 &&
			 0 <= i__1 ? i__1 : s_rnge("stdes", i__1, "spkbsr_", (
			ftnlen)2828)]);
		dafgn_(stidnt + ((i__1 = new__ - 1) < 100000 && 0 <= i__1 ? 
			i__1 : s_rnge("stidnt", i__1, "spkbsr_", (ftnlen)2829)
			) * 40, (ftnlen)40);
		if (failed_()) {
		    chkout_("SPKSFS", (ftnlen)6);
		    return 0;
		}

/*              If the current list is empty, this append operation */
/*              is a no-op. */

		lnkilb_(&new__, &btbeg[(i__1 = bindex - 1) < 200 && 0 <= i__1 
			? i__1 : s_rnge("btbeg", i__1, "spkbsr_", (ftnlen)
			2840)], stpool);
		btbeg[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"btbeg", i__1, "spkbsr_", (ftnlen)2841)] = new__;
		s_copy(status, "RESUME", (ftnlen)15, (ftnlen)6);
	    }
	} else if (s_cmp(status, "ADD TO END", (ftnlen)15, (ftnlen)10) == 0) {

/*           The current segment information should be linked in at */
/*           the tail of the segment list for the current body. */

/*           The only way to get to this task is from the OLD SEGMENTS */
/*           block after suspending that task. */

	    if (lnknfn_(stpool) == 0) {

/*              There's no room left in the segment pool.  We must make */
/*              room before continuing. */

		s_copy(doing, "ADD TO END", (ftnlen)15, (ftnlen)10);
		s_copy(urgent, "MAKE ROOM", (ftnlen)15, (ftnlen)9);
		s_copy(status, "SUSPEND", (ftnlen)15, (ftnlen)7);
	    } else {

/*              Allocate a new node in the segment table pool. */

		lnkan_(stpool, &new__);
		sthan[(i__1 = new__ - 1) < 100000 && 0 <= i__1 ? i__1 : 
			s_rnge("sthan", i__1, "spkbsr_", (ftnlen)2872)] = 
			fthan[(i__2 = findex - 1) < 5000 && 0 <= i__2 ? i__2 :
			 s_rnge("fthan", i__2, "spkbsr_", (ftnlen)2872)];
		moved_(descr, &c__5, &stdes[(i__1 = new__ * 5 - 5) < 500000 &&
			 0 <= i__1 ? i__1 : s_rnge("stdes", i__1, "spkbsr_", (
			ftnlen)2873)]);
		dafgn_(stidnt + ((i__1 = new__ - 1) < 100000 && 0 <= i__1 ? 
			i__1 : s_rnge("stidnt", i__1, "spkbsr_", (ftnlen)2874)
			) * 40, (ftnlen)40);
		if (failed_()) {
		    chkout_("SPKSFS", (ftnlen)6);
		    return 0;
		}
		if (btbeg[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
			s_rnge("btbeg", i__1, "spkbsr_", (ftnlen)2881)] <= 0) 
			{

/*                 This is the first node in the list for this body. */

		    btbeg[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btbeg", i__1, "spkbsr_", (ftnlen)2885)] = 
			    new__;
		} else {

/*                 Link the new node to the tail of the list. */

		    tail = -lnkprv_(&btbeg[(i__1 = bindex - 1) < 200 && 0 <= 
			    i__1 ? i__1 : s_rnge("btbeg", i__1, "spkbsr_", (
			    ftnlen)2891)], stpool);
		    lnkila_(&tail, &new__, stpool);
		}
		s_copy(status, "RESUME", (ftnlen)15, (ftnlen)6);
	    }
	} else if (s_cmp(status, "SEARCH W/O BUFF", (ftnlen)15, (ftnlen)15) ==
		 0) {

/*           When the segment table is completely full, continue */
/*           the search by looking through the unchecked portion */
/*           of the segment list for the current body, and */
/*           then searching old, unchecked files without buffering */
/*           their segments. */

/*           The only way to get here is from the MAKE ROOM state */
/*           via the block ADD TO END.  If you get here there is no */
/*           free space in the segment table pool. */

/*           At this point, we need to initialize the cost of */
/*           the re-use interval. */

	    btruex[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		    "btruex", i__1, "spkbsr_", (ftnlen)2917)] = 0;

/*           Need to find the portion of the current body's segment */
/*           list which comes from the current file of interest.  It */
/*           will be returned to the segment table pool, since the */
/*           remainder of the file's segments can't be added to the list. */

	    crflbg = btbeg[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
		    s_rnge("btbeg", i__1, "spkbsr_", (ftnlen)2925)];
	    fndhan = FALSE_;
	    while(! fndhan && crflbg > 0) {
		fndhan = sthan[(i__1 = crflbg - 1) < 100000 && 0 <= i__1 ? 
			i__1 : s_rnge("sthan", i__1, "spkbsr_", (ftnlen)2930)]
			 == fthan[(i__2 = findex - 1) < 5000 && 0 <= i__2 ? 
			i__2 : s_rnge("fthan", i__2, "spkbsr_", (ftnlen)2930)]
			;
		if (! fndhan) {

/*                 Get the next node.  We avoid LNKNXT here in order */
/*                 to speed up the operation. */

		    crflbg = stpool[(i__1 = (crflbg << 1) + 10) < 200012 && 0 
			    <= i__1 ? i__1 : s_rnge("stpool", i__1, "spkbsr_",
			     (ftnlen)2937)];
		}
	    }
	    if (crflbg > 0) {

/*              The sub-list from the current node onwards is to be */
/*              returned to the segment table pool.  Save this node, */
/*              since we'll finish searching the list before freeing */
/*              the sub-list. */

		p = crflbg;

/*              It may be that the sub-list we're deleting is the */
/*              entire segment list for this body.  If so, the */
/*              corresponding body table entry should be set to */
/*              a non-positive value to indicate an empty segment list. */

		if (p == btbeg[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 :
			 s_rnge("btbeg", i__1, "spkbsr_", (ftnlen)2958)]) {
		    btbeg[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btbeg", i__1, "spkbsr_", (ftnlen)2960)] = 
			    0;

/*                 Also in this case, we must initialize the re-use */
/*                 interval for this body. */

		    btlb[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btlb", i__1, "spkbsr_", (ftnlen)2965)] = 
			    dpmin_();
		    btub[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btub", i__1, "spkbsr_", (ftnlen)2966)] = 
			    dpmax_();
		}

/*              Finish searching through the incomplete list for the */
/*              desired segment. */

		while(crflbg > 0) {

/*                 Every segment seen from the current file contributes */
/*                 to the expense of the re-use interval. */

		    btruex[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btruex", i__1, "spkbsr_", (ftnlen)2979)] =
			     btruex[(i__2 = bindex - 1) < 200 && 0 <= i__2 ? 
			    i__2 : s_rnge("btruex", i__2, "spkbsr_", (ftnlen)
			    2979)] + 1;
		    if (*et > stdes[(i__1 = crflbg * 5 - 4) < 500000 && 0 <= 
			    i__1 ? i__1 : s_rnge("stdes", i__1, "spkbsr_", (
			    ftnlen)2982)]) {

/*                    ET is to the right of the coverage interval of this */
/*                    segment. */

/* Computing MAX */
			d__1 = btlb[(i__2 = bindex - 1) < 200 && 0 <= i__2 ? 
				i__2 : s_rnge("btlb", i__2, "spkbsr_", (
				ftnlen)2987)], d__2 = stdes[(i__3 = crflbg * 
				5 - 4) < 500000 && 0 <= i__3 ? i__3 : s_rnge(
				"stdes", i__3, "spkbsr_", (ftnlen)2987)];
			btlb[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
				s_rnge("btlb", i__1, "spkbsr_", (ftnlen)2987)]
				 = max(d__1,d__2);
		    } else if (*et < stdes[(i__1 = crflbg * 5 - 5) < 500000 &&
			     0 <= i__1 ? i__1 : s_rnge("stdes", i__1, "spkbs"
			    "r_", (ftnlen)2990)]) {

/*                    ET is to the left of the coverage interval of this */
/*                    segment. */

/* Computing MIN */
			d__1 = btub[(i__2 = bindex - 1) < 200 && 0 <= i__2 ? 
				i__2 : s_rnge("btub", i__2, "spkbsr_", (
				ftnlen)2995)], d__2 = stdes[(i__3 = crflbg * 
				5 - 5) < 500000 && 0 <= i__3 ? i__3 : s_rnge(
				"stdes", i__3, "spkbsr_", (ftnlen)2995)];
			btub[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
				s_rnge("btub", i__1, "spkbsr_", (ftnlen)2995)]
				 = min(d__1,d__2);
		    } else {

/*                    The segment coverage interval includes ET. */

			moved_(&stdes[(i__1 = crflbg * 5 - 5) < 500000 && 0 <=
				 i__1 ? i__1 : s_rnge("stdes", i__1, "spkbsr_"
				, (ftnlen)3001)], &c__5, descr);
			s_copy(ident, stidnt + ((i__1 = crflbg - 1) < 100000 
				&& 0 <= i__1 ? i__1 : s_rnge("stidnt", i__1, 
				"spkbsr_", (ftnlen)3003)) * 40, ident_len, (
				ftnlen)40);
			*handle = sthan[(i__1 = crflbg - 1) < 100000 && 0 <= 
				i__1 ? i__1 : s_rnge("sthan", i__1, "spkbsr_",
				 (ftnlen)3004)];
			*found = TRUE_;

/*                    Set the re-use interval for the current body. */

/* Computing MAX */
			d__1 = btlb[(i__2 = bindex - 1) < 200 && 0 <= i__2 ? 
				i__2 : s_rnge("btlb", i__2, "spkbsr_", (
				ftnlen)3010)], d__2 = stdes[(i__3 = crflbg * 
				5 - 5) < 500000 && 0 <= i__3 ? i__3 : s_rnge(
				"stdes", i__3, "spkbsr_", (ftnlen)3010)];
			btlb[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
				s_rnge("btlb", i__1, "spkbsr_", (ftnlen)3010)]
				 = max(d__1,d__2);
/* Computing MIN */
			d__1 = btub[(i__2 = bindex - 1) < 200 && 0 <= i__2 ? 
				i__2 : s_rnge("btub", i__2, "spkbsr_", (
				ftnlen)3011)], d__2 = stdes[(i__3 = crflbg * 
				5 - 4) < 500000 && 0 <= i__3 ? i__3 : s_rnge(
				"stdes", i__3, "spkbsr_", (ftnlen)3011)];
			btub[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
				s_rnge("btub", i__1, "spkbsr_", (ftnlen)3011)]
				 = min(d__1,d__2);

/*                    Save the output items, in case this */
/*                    segment may be satisfy the next request. */

			btprvh[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 :
				 s_rnge("btprvh", i__1, "spkbsr_", (ftnlen)
				3017)] = *handle;
			s_copy(btprvi + ((i__1 = bindex - 1) < 200 && 0 <= 
				i__1 ? i__1 : s_rnge("btprvi", i__1, "spkbsr_"
				, (ftnlen)3018)) * 40, ident, (ftnlen)40, 
				ident_len);
			moved_(descr, &c__5, &btprvd[(i__1 = bindex * 5 - 5) <
				 1000 && 0 <= i__1 ? i__1 : s_rnge("btprvd", 
				i__1, "spkbsr_", (ftnlen)3019)]);
			btchkp[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 :
				 s_rnge("btchkp", i__1, "spkbsr_", (ftnlen)
				3020)] = TRUE_;

/*                    Update the expense of the list to reflect */
/*                    the cost of locating this segment. */

			btexp[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
				s_rnge("btexp", i__1, "spkbsr_", (ftnlen)3026)
				] = btexp[(i__2 = bindex - 1) < 200 && 0 <= 
				i__2 ? i__2 : s_rnge("btexp", i__2, "spkbsr_",
				 (ftnlen)3026)] + btruex[(i__3 = bindex - 1) <
				 200 && 0 <= i__3 ? i__3 : s_rnge("btruex", 
				i__3, "spkbsr_", (ftnlen)3026)];

/*                    Free the sub-list we were searching. */

			tail = lnktl_(&crflbg, stpool);
			lnkfsl_(&p, &tail, stpool);
			chkout_("SPKSFS", (ftnlen)6);
			return 0;
		    }
/*                 Get the next node.  We avoid LNKNXT here in order */
/*                 to speed up the operation. */

		    crflbg = stpool[(i__1 = (crflbg << 1) + 10) < 200012 && 0 
			    <= i__1 ? i__1 : s_rnge("stpool", i__1, "spkbsr_",
			     (ftnlen)3042)];
		}

/*              Return the sub-list to the segment table pool. */
/*              CRFLBG at this point is the negative of the list head. */
/*              The list tail is (by the spec of the SPICELIB doubly */
/*              linked list routines) the negative of the predecessor */
/*              of the head. */

/*              Note the list is always non-empty. */

		i__1 = -crflbg;
		tail = -lnkprv_(&i__1, stpool);
		lnkfsl_(&p, &tail, stpool);
	    }

/*           Search through the remaining files without buffering. */
/*           Recall that a search is already in progress and that a */
/*           segment is currently under consideration (FND = .TRUE.). */

	    while(findex > 0) {
		while(fnd) {

/*                 Each segment found contributes to the expense of the */
/*                 re-use interval. */

		    btruex[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("btruex", i__1, "spkbsr_", (ftnlen)3073)] =
			     btruex[(i__2 = bindex - 1) < 200 && 0 <= i__2 ? 
			    i__2 : s_rnge("btruex", i__2, "spkbsr_", (ftnlen)
			    3073)] + 1;
		    dafgs_(descr);
		    dafus_(descr, &c__2, &c__6, dcd, icd);
		    if (failed_()) {
			chkout_("SPKSFS", (ftnlen)6);
			return 0;
		    }
		    if (*body == icd[0]) {

/*                    This is a segment for the body of interest. */
/*                    Update the re-use interval for this body. */

			if (*et > dcd[1]) {

/*                       ET is to the right of the coverage interval */
/*                       of this segment. */

/* Computing MAX */
			    d__1 = btlb[(i__2 = bindex - 1) < 200 && 0 <= 
				    i__2 ? i__2 : s_rnge("btlb", i__2, "spkb"
				    "sr_", (ftnlen)3093)];
			    btlb[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? 
				    i__1 : s_rnge("btlb", i__1, "spkbsr_", (
				    ftnlen)3093)] = max(d__1,dcd[1]);
			} else if (*et < dcd[0]) {

/*                       ET is to the left of the coverage interval */
/*                       of this segment. */

/* Computing MIN */
			    d__1 = btub[(i__2 = bindex - 1) < 200 && 0 <= 
				    i__2 ? i__2 : s_rnge("btub", i__2, "spkb"
				    "sr_", (ftnlen)3101)];
			    btub[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? 
				    i__1 : s_rnge("btub", i__1, "spkbsr_", (
				    ftnlen)3101)] = min(d__1,dcd[0]);
			} else {

/*                       The segment coverage interval includes ET. */

			    dafgn_(ident, ident_len);
			    if (failed_()) {
				chkout_("SPKSFS", (ftnlen)6);
				return 0;
			    }
			    *handle = fthan[(i__1 = findex - 1) < 5000 && 0 <=
				     i__1 ? i__1 : s_rnge("fthan", i__1, 
				    "spkbsr_", (ftnlen)3114)];
			    *found = TRUE_;

/*                       Set the re-use interval for the current body. */

/* Computing MAX */
			    d__1 = btlb[(i__2 = bindex - 1) < 200 && 0 <= 
				    i__2 ? i__2 : s_rnge("btlb", i__2, "spkb"
				    "sr_", (ftnlen)3120)];
			    btlb[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? 
				    i__1 : s_rnge("btlb", i__1, "spkbsr_", (
				    ftnlen)3120)] = max(d__1,dcd[0]);
/* Computing MIN */
			    d__1 = btub[(i__2 = bindex - 1) < 200 && 0 <= 
				    i__2 ? i__2 : s_rnge("btub", i__2, "spkb"
				    "sr_", (ftnlen)3121)];
			    btub[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? 
				    i__1 : s_rnge("btub", i__1, "spkbsr_", (
				    ftnlen)3121)] = min(d__1,dcd[1]);

/*                       Save the output items, in case this */
/*                       segment may satisfy the next request. */

			    btprvh[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? 
				    i__1 : s_rnge("btprvh", i__1, "spkbsr_", (
				    ftnlen)3127)] = *handle;
			    s_copy(btprvi + ((i__1 = bindex - 1) < 200 && 0 <=
				     i__1 ? i__1 : s_rnge("btprvi", i__1, 
				    "spkbsr_", (ftnlen)3128)) * 40, ident, (
				    ftnlen)40, ident_len);
			    moved_(descr, &c__5, &btprvd[(i__1 = bindex * 5 - 
				    5) < 1000 && 0 <= i__1 ? i__1 : s_rnge(
				    "btprvd", i__1, "spkbsr_", (ftnlen)3129)])
				    ;
			    btchkp[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? 
				    i__1 : s_rnge("btchkp", i__1, "spkbsr_", (
				    ftnlen)3130)] = TRUE_;

/*                       Update the expense of the list to reflect */
/*                       the cost of locating this segment. */

			    btexp[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? 
				    i__1 : s_rnge("btexp", i__1, "spkbsr_", (
				    ftnlen)3136)] = btexp[(i__2 = bindex - 1) 
				    < 200 && 0 <= i__2 ? i__2 : s_rnge("btexp"
				    , i__2, "spkbsr_", (ftnlen)3136)] + 
				    btruex[(i__3 = bindex - 1) < 200 && 0 <= 
				    i__3 ? i__3 : s_rnge("btruex", i__3, 
				    "spkbsr_", (ftnlen)3136)];
			    chkout_("SPKSFS", (ftnlen)6);
			    return 0;
			}
		    }
		    daffpa_(&fnd);
		    if (failed_()) {
			chkout_("SPKSFS", (ftnlen)6);
			return 0;
		    }
		}

/*              Try the next oldest file. */

		--findex;
		if (findex > 0) {
		    dafbbs_(&fthan[(i__1 = findex - 1) < 5000 && 0 <= i__1 ? 
			    i__1 : s_rnge("fthan", i__1, "spkbsr_", (ftnlen)
			    3161)]);
		    daffpa_(&fnd);
		    if (failed_()) {
			chkout_("SPKSFS", (ftnlen)6);
			return 0;
		    }
		}
	    }

/*           If you get to here, sorry. */

	    btruex[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		    "btruex", i__1, "spkbsr_", (ftnlen)3176)] = 0;
	    s_copy(status, "HOPELESS", (ftnlen)15, (ftnlen)8);

/*        When a task is suspended, the current activity is placed on */
/*        a stack, to be restored later. Two levels are provided, since */
/*        some interrupts can be interrupted by others. */

	} else if (s_cmp(status, "SUSPEND", (ftnlen)15, (ftnlen)7) == 0) {
	    ++top;
	    s_copy(stack + ((i__1 = top - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge(
		    "stack", i__1, "spkbsr_", (ftnlen)3187)) * 15, doing, (
		    ftnlen)15, (ftnlen)15);
	    s_copy(status, urgent, (ftnlen)15, (ftnlen)15);
	} else if (s_cmp(status, "RESUME", (ftnlen)15, (ftnlen)6) == 0) {

/*           Pop the status stack. */

	    s_copy(status, stack + ((i__1 = top - 1) < 2 && 0 <= i__1 ? i__1 :
		     s_rnge("stack", i__1, "spkbsr_", (ftnlen)3194)) * 15, (
		    ftnlen)15, (ftnlen)15);
	    --top;
	}
    }

/*     If we didn't find a segment, don't attempt to use saved */
/*     outputs from a previous call.  BINDEX will always be set */
/*     at this point.  Also clear the re-use interval's expense. */

    if (bindex > 0) {
	btchkp[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("btchkp"
		, i__1, "spkbsr_", (ftnlen)3208)] = FALSE_;
	btruex[(i__1 = bindex - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("btruex"
		, i__1, "spkbsr_", (ftnlen)3209)] = 0;
    }
    chkout_("SPKSFS", (ftnlen)6);
    return 0;
} /* spkbsr_ */

/* Subroutine */ int spkbsr_(char *fname, integer *handle, integer *body, 
	doublereal *et, doublereal *descr, char *ident, logical *found, 
	ftnlen fname_len, ftnlen ident_len)
{
    return spkbsr_0_(0, fname, handle, body, et, descr, ident, found, 
	    fname_len, ident_len);
    }

/* Subroutine */ int spklef_(char *fname, integer *handle, ftnlen fname_len)
{
    return spkbsr_0_(1, fname, handle, (integer *)0, (doublereal *)0, (
	    doublereal *)0, (char *)0, (logical *)0, fname_len, (ftnint)0);
    }

/* Subroutine */ int spkuef_(integer *handle)
{
    return spkbsr_0_(2, (char *)0, handle, (integer *)0, (doublereal *)0, (
	    doublereal *)0, (char *)0, (logical *)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int spksfs_(integer *body, doublereal *et, integer *handle, 
	doublereal *descr, char *ident, logical *found, ftnlen ident_len)
{
    return spkbsr_0_(3, (char *)0, handle, body, et, descr, ident, found, (
	    ftnint)0, ident_len);
    }

