/* ckbsr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c_b9 = 100000;
static integer c__5000 = 5000;
static integer c__5 = 5;
static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure      CKBSR ( C-kernel, buffer segments for readers ) */
/* Subroutine */ int ckbsr_0_(int n__, char *fname, integer *handle, integer *
	inst, doublereal *sclkdp, doublereal *tol, logical *needav, 
	doublereal *descr, char *segid, logical *found, ftnlen fname_len, 
	ftnlen segid_len)
{
    /* Initialized data */

    static logical fresub = FALSE_;
    static integer nft = 0;
    static integer nit = 0;
    static integer next = 0;
    static integer savep = 0;
    static doublereal savtol = 0.;
    static char status[40] = "BOGUS ENTRY                             ";

    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    integer head, tail;
    static doublereal itlb[100], itub[100];
    integer cost;
    static doublereal reqt;
    integer i__, j;
    extern /* Subroutine */ int dafgn_(char *, ftnlen);
    integer cheap, p;
    extern /* Subroutine */ int dafgs_(doublereal *);
    static doublereal alpha, omega;
    static integer itbeg[100], slbeg;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafps_(integer *, 
	    integer *, doublereal *, integer *, doublereal *);
    static integer fthan[5000];
    static doublereal stdcd[200000]	/* was [2][100000] */;
    char doing[40], stack[40*2];
    static integer sticd[600000]	/* was [6][100000] */;
    extern doublereal dpmin_(void), dpmax_(void);
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *);
    static integer ithfs[100], sthan[100000];
    extern /* Subroutine */ int dafus_(doublereal *, integer *, integer *, 
	    doublereal *, integer *);
    static integer itlfs[100];
    extern /* Subroutine */ int lnkan_(integer *, integer *);
    extern integer lnktl_(integer *, integer *);
    static integer itins[100], ftnum[5000], itexp[100];
    extern /* Subroutine */ int daffna_(logical *), dafbbs_(integer *), 
	    daffpa_(logical *);
    extern logical failed_(void);
    extern /* Subroutine */ int dafbfs_(integer *), cleard_(integer *, 
	    doublereal *), dafcls_(integer *);
    logical fndhan;
    static logical avneed;
    extern /* Subroutine */ int lnkila_(integer *, integer *, integer *), 
	    dafopr_(char *, integer *, ftnlen);
    static integer findex;
    extern /* Subroutine */ int lnkilb_(integer *, integer *, integer *);
    extern integer isrchi_(integer *, integer *, integer *);
    static integer iindex;
    static logical itchkp[100];
    extern /* Subroutine */ int lnkini_(integer *, integer *);
    extern integer lnknfn_(integer *);
    static logical newsch;
    extern /* Subroutine */ int lnkfsl_(integer *, integer *, integer *), 
	    sigerr_(char *, ftnlen), chkout_(char *, ftnlen);
    extern integer intmax_(void);
    integer minexp;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    static char stidnt[40*100000];
    char urgent[40];
    static doublereal itprvd[500]	/* was [5][100] */;
    static integer itprvf[100];
    integer nxtseg;
    extern integer lnkprv_(integer *, integer *);
    static char itprvi[40*100];
    extern integer lnknxt_(integer *, integer *);
    extern logical return_(void);
    static integer itprvh[100], itruex[100], stpool[200012]	/* was [2][
	    100006] */, scinst;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    doublereal dcd[2];
    integer icd[6];
    static logical fnd;
    integer new__;
    static integer top;

/* $ Abstract */

/*     Load and unload files for use by the readers. Buffer segments */
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

/*     CK */
/*     DAF */

/* $ Keywords */

/*     POINTING */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Entry points */
/*     --------  ---  -------------------------------------------------- */
/*     FNAME      I   CKLPF */
/*     HANDLE    I,O  CKLPF, CKUPF, CKSNS */
/*     INST       I   CKBSS */
/*     SCLKDP     I   CKBSS */
/*     TOL        I   CKBSS */
/*     NEEDAV     I   CKBSS */
/*     DESCR      O   CKSNS */
/*     SEGID      O   CKSNS */
/*     FOUND      O   CKSNS */

/* $ Detailed_Input */

/*     FNAME      is the name of a binary C-kernel file to be loaded. */

/*     HANDLE     on input is the handle of a binary C-kernel file to be */
/*                unloaded. */


/*     The purpose of entry points CKBSS and CKSNS is to search for */
/*     segments in CK files matching certain criteria. The four */
/*     quantities below establish these search criteria. */


/*     INST       is the NAIF ID of an instrument. */

/*     SCLKDP     is an encoded spacecraft clock time. */

/*     TOL        is a time tolerance, measured in the same units as */
/*                encoded spacecraft clock. */

/*     NEEDAV     indicates whether or not angular velocity data are */
/*                required. */

/*                If true, only segments containing pointing and angular */
/*                velocity data will be checked. If false, segments */
/*                containing just pointing data will also be considered. */


/*     A segment matches the CKBSS/CKSNS search criteria when the */
/*     following statements are true. */

/*        1) INST matches the instrument number for the segment. */

/*        2) The time interval [SCLKDP - TOL, SCLKDP + TOL] intersects */
/*           the time interval of the segment. */

/*        3) If angular velocity data are required, as indicated by */
/*           NEEDAV, the segment contains angular velocity data. */


/* $ Detailed_Output */

/*     HANDLE     on output is the handle of the C-kernel file */
/*                containing a located segment. */

/*     DESCR      is the packed descriptor of a located segment. */

/*     SEGID      is the identifier of a located segment. */

/*     FOUND      indicates whether a requested segment was found or not. */

/* $ Parameters */

/*     FTSIZE     is the maximum number of pointing files that can */
/*                be loaded by CKLPF at any given time for use by the */
/*                readers. */

/*     ITSIZE     is the maximum number of instruments whose segments */
/*                are buffered by CKSNS. */

/*     STSIZE     is the maximum number of segments that can be buffered */
/*                at any given time by CKSNS. */

/* $ Exceptions */

/*     1) If CKBSR is called directly, the error SPICE(CKBOGUSENTRY) */
/*        is signaled. */

/*     2) See entry points CKLPF, CKUPF, CKBSS, and CKSNS for exceptions */
/*        specific to them. */

/* $ Files */

/*     C-kernel pointing files are indicated by filename before loading */
/*     (see CKLPF) and handle after loading (all other places). */

/* $ Particulars */

/*     CKBSR serves as an umbrella, allowing data to be shared by its */
/*     entry points: */

/*        CKLPF       Load pointing file. */
/*        CKUPF       Unload pointing file. */
/*        CKBSS       Begin search for segment. */
/*        CKSNS       Select next segment. */

/*     Before a file can be read by the C-kernel readers, it must be */
/*     loaded by CKLPF, which among other things load the file into */
/*     the DAF subsystem. */

/*     Up to FTSIZE files may be loaded for use simultaneously, and a */
/*     file only has to be loaded once to become a potential search */
/*     target for any number of subsequent reads. */

/*     Once a C-kernel has been loaded, it is assigned a file */
/*     handle, which is used to keep track of the file internally, and */
/*     which is used by the calling program to refer to the file in all */
/*     subsequent calls to CK routines. */

/*     A file may be removed from the list of files for potential */
/*     searching by unloading it via a call to CKUPF. */

/*     CKBSS and CKSNS are used together to search through loaded files */
/*     for segments. */

/*     CKBSS sets up the search. You tell it the instrument and time */
/*     that you are interested in, and whether you require segments */
/*     containing angular velocity data. */

/*     CKSNS finds segments matching the search criteria set up by */
/*     CKBSS. Last-loaded files get searched first, and individual files */
/*     are searched backwards. */

/*     When an applicable segment is found, CKSNS returns that segment's */
/*     descriptor and identifier, along with the handle of the file */
/*     containing the segment. */

/*     Subsequent calls to CKSNS continue the search, picking up where */
/*     the previous call to this routine left off. */

/*     CKSNS uses information on loaded files to manage a buffer */
/*     of saved segment descriptors and identifiers. The buffer is used */
/*     to speed up access time by minimizing file reads. */

/* $ Examples */

/*     Suppose that pointing data for the Voyager 2 narrow angle camera */
/*     for a certain interval of time are contained in three separate */
/*     files:  ORIGINAL.CK contains an original complete set of pointing */
/*     data and UPDATE_1.CK and UPDATE_2.CK contain two separate pointing */
/*     updates for certain pictures in the same time period. */

/*     In the following example, pointing from the C-kernel is extracted */
/*     in two different ways for the purpose of comparing the two */
/*     updates: */

/*     First, the original pointing file and one of the update files are */
/*     both loaded and pointing is retrieved for all of the pictures. */
/*     The update file is searched through first, and if no data for the */
/*     desired picture is located, then the original file provides the */
/*     requested pointing. */

/*     Then, the first update file is unloaded, the second update file */
/*     is loaded, and the same search is performed, as above. */

/*     Throughout the two searches, a ficticious non-SPICELIB routine */
/*     named WRTABL writes an entry into a table that contains */
/*     the pointing of the camera and the file from which the pointing */
/*     came, if such pointing was found.  WRERR, another ficticious, */
/*     non-SPICELIB routine writes an error message if no such pointing */
/*     was found. */

/*     It is assumed that an array (FDS) exists that contains character */
/*     representations of the spacecraft clock time for each picture, */
/*     and that there are NPICS pictures. */

/*           INTEGER               NPICS */
/*           PARAMETER           ( NPICS = 100 ) */

/*           INTEGER               HANDLE */
/*           INTEGER               HNORIG */
/*           INTEGER               HUPDT */
/*           INTEGER               UPDATE */
/*           INTEGER               INST */
/*           INTEGER               SC */
/*           INTEGER               I */

/*           DOUBLE PRECISION      DESCR    (    5 ) */
/*           DOUBLE PRECISION      SCLKDP */
/*           DOUBLE PRECISION      TOL */
/*           DOUBLE PRECISION      CLKOUT */
/*           DOUBLE PRECISION      CMAT     ( 3, 3 ) */
/*           DOUBLE PRECISION      AV       (    3 ) */

/*           CHARACTER*(12)        FDS      ( NPICS ) */
/*           CHARACTER*(25)        FNAME */
/*           CHARACTER*(40)        SEGID */
/*           CHARACTER*(12)        OUTFDS */
/*           CHARACTER*(12)        TOLSTR */
/*           CHARACTER*(25)        UDFILE   (    2 ) */

/*           LOGICAL               PFOUND */
/*           LOGICAL               SFOUND */
/*           LOGICAL               NEEDAV */


/*           UDFILE ( 1 ) = 'UPDATE_1.CK' */
/*           UDFILE ( 2 ) = 'UPDATE_2.CK' */

/*     C */
/*     C     The NAIF integer ID codes for the Voyager 2 spacecraft */
/*     C     and the narrow angle camera on Voyager 2 are -32 and */
/*     C     -32001, respectively. */
/*     C */
/*           SC           = -32 */
/*           INST         = -32001 */
/*     C */
/*     C     Load the Voyager SCLK file. */
/*     C */
/*           CALL FURNSH ( 'VG2_SCLK.TSC' ) */

/*     C */
/*     C     Allow a time tolerance of 400 line counts.  Convert */
/*     C     the tolerance to 'ticks', the units of encoded spacecraft */
/*     C     clock time. */
/*     C */
/*           TOLSTR  = '0:00:400' */
/*           CALL SCTIKS ( SC, TOLSTR, TOL ) */

/*     C */
/*     C     Don't care about angular velocity data. */
/*     C */
/*           NEEDAV = .FALSE. */

/*     C */
/*     C     Load the original CK file first. */
/*     C */
/*           CALL CKLPF ( 'ORIGINAL.CK', HNORIG ) */


/*           DO UPDATE = 1, 2 */
/*     C */
/*     C        Load the update file.  Last-loaded files get searched */
/*     C        first, so the update file will be searched before */
/*     C        the original file. */
/*     C */
/*              CALL CKLPF ( UDFILE ( UPDATE ), HUPDT ) */

/*              DO I = 1, NPICS */

/*     C */
/*     C           Encode the character string representation of */
/*     C           spacecraft clock time in FDS. */
/*     C */
/*                 CALL SCENCD ( SC, FDS( I ), SCLKDP ) */

/*     C */
/*     C           Begin a search for this instrument and time, and */
/*     C           get the first applicable segment. */
/*     C */
/*                 CALL CKBSS ( INST,   SCLKDP, TOL,   NEEDAV  ) */
/*                 CALL CKSNS ( HANDLE, DESCR,  SEGID, SFOUND  ) */

/*     C */
/*     C           Keep trying candidate segments until a segment can */
/*     C           produce a pointing instance within the specified */
/*     C           time tolerance of SCLKDP, the encoded spacecraft */
/*     C           clock time. */
/*     C */
/*                 PFOUND = .FALSE. */

/*                 DO WHILE (  SFOUND .AND. ( .NOT. PFOUND )  ) */

/*                    CALL CKPFS ( HANDLE, DESCR, SCLKDP, TOL,  NEEDAV, */
/*          .                      CMAT,   AV,    CLKOUT, PFOUND       ) */

/*                    IF ( PFOUND ) THEN */

/*     C                 Get the name of the file from whence the */
/*     C                 pointing instance came, decode the spacecraft */
/*     C                 clock time associated with the instance, and */
/*     C                 write the results to the table. */
/*     C */
/*                       CALL DAFHFN ( HANDLE, FNAME          ) */
/*                       CALL SCDECD ( SC,     CLKOUT, OUTFDS ) */

/*                       CALL WRTABL ( FDS( I ), OUTFDS, CMAT, FNAME ) */

/*                    ELSE */
/*     C */
/*     C                 Look for another candidate segment. */
/*     C */
/*                       CALL CKSNS ( HANDLE, DESCR, SEGID, SFOUND ) */

/*                    END IF */

/*                 END DO */

/*                 IF ( .NOT. PFOUND ) THEN */

/*                    CALL WRERR ( FDS( I ) ) */

/*                 END IF */

/*              END DO */

/*     C */
/*     C        Unload the update file.  The original file stays loaded. */
/*     C */
/*              CALL CKUPF  ( HUPDT ) */

/*           END DO */

/* $ Restrictions */

/*     1) If Fortran I/O errors occur while searching a loaded CK */
/*        file, the internal state of this suite of routines may */
/*        be corrupted.  It may be possible to correct the state */
/*        by unloading the pertinent CK files and then re-loading */
/*        them. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     J.M. Lynch     (JPL) */
/*     J.E. McLean    (JPL) */
/*     B.V. Semenov   (JPL) */
/*     M.J. Spencer   (JPL) */
/*     R.E. Thurman   (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -    SPICELIB Version 5.0.0, 17-MAR-2014 (NJB) */

/*        Updated segment pool initialization condition in entry */
/*        point CKLPF so that the pool is initialized only if the file */
/*        table is empty. */

/* -    SPICELIB Version 4.6.0, 13-JUN-2013 (BVS) */

/*        Increased FTSIZE (from 1000 to 5000). */

/*        Increased STSIZE (from 50000 to 100000). */

/* -    SPICELIB Version 4.5.0, 24-FEB-2011 (NJB) */

/*        Bug fixes: */

/*          1) In the CKSNS 'MAKE ROOM' state, when the */
/*             suspended activity is 'ADD TO FRONT' and no segment table */
/*             room is available, the instrument table's pointer to the */
/*             current segment list is now set to null. Previously the */
/*             pointer was allowed to go stale. */

/*          2) In CKUPF, the null pointer test used to determine */
/*             eligibility for segment list deletion now uses the .LE. */
/*             operator instead of the .EQ. operator. */


/* -    SPICELIB Version 4.4.0, 07-APR-2010 (NJB) */

/*        Increased STSIZE to 50000. */

/* -    SPICELIB Version 4.3.1, 28-FEB-2008 (BVS) */

/*        Corrected the contents of the Required_Reading section */
/*        of the CKHAVE entry point header. */

/* -    SPICELIB Version 4.3.0, 23-OCT-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments in */
/*        MOVED calls in entry points CKUPF and CKSNS.  Replaced header */
/*        reference to LDPOOL with reference to FURNSH. */

/* -    SPICELIB Version 4.2.0, 30-DEC-2004 (NJB) */

/*        Increased STSIZE to 20000. */

/* -    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB) */

/*        Bug fixes: */

/*           1) When a segment list is freed because the entire list */
/*              is contributed by a single CK file, and the list is */
/*              too large to be buffered, the corresponding instrument */
/*              table pointer is now set to null. */

/*           2) An algorithm change has eliminated a bug caused by not */
/*              updating the current instrument index when instrument */
/*              table entries having empty segment lists were compressed */
/*              out of the instrument table.  Previously the instrument */
/*              table pointer IINDEX could go stale after the */
/*              compression. */

/*           3) When a already loaded kernel is re-opened with DAFOPR, */
/*              it now has its link count reset to 1 via a call to */
/*              DAFCLS. */

/*           4) The load routine CKLPF now resets all file numbers when */
/*              the next file number reaches INTMAX()-1, thereby */
/*              avoiding arithmetic overflow. */

/*           5) The unload routine CKUPF now calls RETURN() on entry and */
/*              returns if so directed. */

/*           6) In CKSNS, DAF calls are followed by tests of FAILED() */
/*              in order to ensure that the main state loop terminates. */

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

/*        The instrument table size has been increased to 100 in order */
/*        to decrease the chance of thrashing due to swapping segment */
/*        lists for different bodies. */

/*        Various small updates and corrections were made to the */
/*        comments throughout the file. */

/* -    SPICELIB Version 4.0.0, 17-FEB-2000 (WLT) */

/*        Added the Entry point CKHAVE */

/* -    SPICELIB Version 3.0.0, 03-MAR-1999 (WLT) */

/*        The parameter STSIZE was increased from 1000 to 4000 to */
/*        avoid the buffering error that exists in the CKBSR. */

/* -    SPICELIB Version 2.0.0, 25-NOV-1992 (JML) */

/*     1) When loading a file, CKLPF now checks if the file table is */
/*        full only after determining that the file is not currently */
/*        loaded. Previously, if the file table was full and an attempt */
/*        was made to reload a file, an error was signaled. A new */
/*        exception was added as a result of this change. */

/*     2) A bug in the way that CKLPF and CKUPF clean up the instrument */
/*        tables after a file is unloaded was fixed. */

/*     3) Variable declarations were added to the example program */
/*        so that it can now be compiled. */

/*     4) The length of the elements in the array of segment */
/*        indentifiers ( STIDNT ) was changed from 56 to 40. */

/* -    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.1.0, 01-NOV-1990 (JML) */

/*        An intial value was assigned to the variable STATUS so */
/*        that an error will be signaled if CKSNS is called */
/*        without CKBSS ever having been called to initiate the */
/*        search. */

/* -    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU) */

/* -& */
/* $ Index_Entries */

/*     buffer ck segments for readers */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.5.0, 24-FEB-2011 (NJB) */

/*        Bug fixes: */

/*          1) In the CKSNS 'MAKE ROOM' state, when the */
/*             suspended activity is 'ADD TO FRONT' and no segment table */
/*             room is available, the instrument table's pointer to the */
/*             current segment list is now set to null. Previously the */
/*             pointer was allowed to go stale. */

/*          2) In CKUPF, the null pointer test used to determine */
/*             eligibility for segment list deletion now uses the .LE. */
/*             operator instead of the .EQ. operator. */


/* -    SPICELIB Version 4.3.0, 23-OCT-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments in */
/*        MOVED calls in entry points CKUPF and CKSNS.  Replaced header */
/*        reference to LDPOOL with reference to FURNSH. */

/* -    SPICELIB Version 4.2.0, 30-DEC-2004 (NJB) */

/*        Increased STSIZE to 20000. */

/* -    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB) */

/*        Bug fixes: */

/*           1) When a segment list is freed because the entire list */
/*              is contributed by a single CK file, and the list is */
/*              too large to be buffered, the corresponding instrument */
/*              table pointer is now set to null. */

/*           2) An algorithm change has eliminated a bug caused by not */
/*              updating the current instrument index when instrument */
/*              table entries having empty segment lists were compressed */
/*              out of the instrument.  Previously the instrument table */
/*              pointer IINDEX could go stale after the compression. */

/*           3) When a already loaded kernel is re-opened with DAFOPR, */
/*              it now has its link count reset to 1 via a call to */
/*              DAFCLS. */

/*           4) The load routine CKLPF now resets all file numbers when */
/*              the next file number reaches INTMAX()-1, thereby */
/*              avoiding arithmetic overflow. */

/*           5) The unload routine CKUPF now calls RETURN() on entry and */
/*              returns if so directed. */

/*           6) In CKSNS, DAF calls are followed by tests of FAILED() */
/*              in order to ensure that the main state loop terminates. */

/*        The "re-use interval" feature was introduced to improve speed */
/*        in the case where repeated, consecutive requests are satisified */
/*        by the same segment.  For each instrument, the associated */
/*        re-use interval marks the time interval containing the previous */
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

/*        The instrument table size has been increased to 100 in order */
/*        to decrease the chance of thrashing due to swapping segment */
/*        lists for different instruments. */

/*        Various small updates and corrections were made to the */
/*        comments throughout the file. */

/*        In order to simplify the source code, the in-line singly */
/*        linked list implementation of the segment table has been */
/*        replaced by an implementation relying on the SPICELIB */
/*        doubly linked list routines. */


/* -    SPICELIB Version 2.0.0, 25-NOV-1992 (JML) */

/*     1) When loading a file, CKLPF now checks if the file table is */
/*        full only after determining that the file is not currently */
/*        loaded. Previously, if the file table was full and an attempt */
/*        was made to reload a file, an error was signaled. A new */
/*        exception was added as a result of this change. */

/*     2) A bug in the way that CKLPF and CKUPF clean up the instrument */
/*        tables after a file is unloaded was fixed. */

/*     3) Variable declarations were added to the example program */
/*        so that it can now be compiled. */

/*     4) The length of the elements in the array of segment */
/*        indentifiers ( STIDNT ) was changed from 56 to 40. */

/* -    SPICELIB Version 1.1.0, 01-NOV-1990 (JML) */

/*        An intial value was assigned to the variable STATUS so */
/*        that an error will be signaled if CKSNS is called */
/*        without CKBSS ever having been called to initiate the */
/*        search. */


/* -    Beta Version 1.1.0, 28-AUG-1990 (MJS) (JEM) */

/*        The following changes were made as a result of the */
/*        NAIF CK Code and Documentation Review: */

/*           1) The variable SCLK  was changed to SCLKDP. */
/*           2) The variable IDENT was changed to SEGID. */
/*           3) The parameterized values for FTSIZE and ITSIZE were */
/*               increased from 5 to 20. */
/*           4) The paramterized value for STSIZE was increased from 100 */
/*              to 1000. */
/*           5) The local variables INTDES and DPDES were changed to */
/*              ICD and DCD. */
/*           6) The extended SAVE statement was broken in to single */
/*              SAVE statements. */
/*           7) Header and internal documentation was corrected and */
/*              updated. */

/* -    Beta Version 1.0.0, 14-MAR-1990 (RET) (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*        ND         is the number of double precision components in an */
/*                   unpacked C-kernel descriptor. */

/*        NI         is the number of integer components in an unpacked */
/*                   C-kernel descriptor. */

/*        DSCSIZ     is the number of components in a packed C-kernel */
/*                   descriptor.  All DAF summaries have this formulaic */
/*                   relationship between the number of its integer and */
/*                   double precision components and the number of packed */
/*                   components. */


/*     Constants used in the doubly linked list structure: */


/*     Local variables */


/*     The file table contains the handle and file number of each file */
/*     that has been loaded for use with the CK readers. File */
/*     numbers begin at one, and are incremented until they reach a */
/*     value of INTMAX() - 1, at which point they are mapped to the */
/*     range 1:NFT, where NFT is the number of loaded CK files. */

/*     A file number is similar to a file handle, but it is assigned */
/*     and used exclusively by this module. The purpose of file numbers */
/*     is to keep track of the order in which files are loaded and the */
/*     order in which they are searched. */

/*     All names begin with FT. */

/*        HAN      Handle */
/*        NUM      File number */

/*     NFT is the number of currently loaded CK files. NEXT is */
/*     incremented whenever a new file is loaded to give the file */
/*     number for that file. FINDEX is the index of whatever file is */
/*     of current interest. */

/*     New files are added at the end of the table. As files are */
/*     removed, succeeding files are moved forward to take up the */
/*     slack. This keeps the table ordered by file number. */


/*     The instrument table contains the beginning of the list of the */
/*     stored segments for each spacecraft/instrument pair, and the */
/*     expense at which that list was constructed. (The expense of an */
/*     instrument list is the number of segment descriptors examined */
/*     during the construction of the list.) It also contains the */
/*     highest and lowest file numbers searched during the construction */
/*     of the list. */

/*     For each instrument, the time bounds of the "re-use interval" */
/*     of the last segment found are stored.  This interval is the */
/*     maximal interval containing the epoch of the last request for */
/*     data for this instrument, such that the interval is not masked */
/*     by higher-priority segments.  The handle, segment descriptor, */
/*     and segment identifier returned on the last request are also */
/*     stored. */

/*     The reuse-interval is computed without regard to presence of */
/*     angular velocity:  all segments seen while searching for */
/*     a segment satisfying a request are used to define the bounds */
/*     of the re-use interval. */

/*     Re-use intervals are defined on the *first* search following */
/*     a setup call to CKBSS.  If a search is resumed (multiple calls */
/*     to CKSNS are made consecutively), the re-use interval becomes */
/*     invalid after the first CKSNS call. */

/*     All names begin with IT. */

/*        INS      Spacecraft/instrument number */
/*        EXP      Expense */
/*        HFS      Highest file (number) searched */
/*        LFS      Lowest  file (number) searched */
/*        BEG      Beginning of segment list */
/*        LB       Lower bound of effective coverage interval of */
/*                 previous segment returned. */
/*        UB       Upper bound of effective coverage interval of */
/*                 previous segment returned. */
/*        PRVD     Previous descriptor. */
/*        PRVF     Previous descriptor angular velocity flag.  Angular */
/*                 velocity is present when ITPRVF is non-zero. */
/*        PRVI     Previous segment identifier returned. */
/*        PRVH     Previous handle returned. */
/*        CHKP     Logical indicating that previous segment should */
/*                 be checked to see whether it satisfies a request. */
/*        RUEX     Expense of the re-use interval. */

/*     NIT is the number of instruments for which segments are currently */
/*     being stored in the table. IINDEX is the index of whatever */
/*     instrument is of current interest at any given time. */

/*     New instruments are added at the end of the table. As instruments */
/*     are removed, the last instrument is moved forward to take up the */
/*     slack. This keeps the entries in the table contiguous. */


/*     The segment table contains the handle, descriptor, and identifier */
/*     for each segment that has been found so far. */

/*     The segment table is implemented as a set of arrays indexed by */
/*     a SPICE doubly linked list structure.  For each instrument */
/*     in the instrument table, there is a segment table list; each */
/*     node of a list points to data associated with a segment.  In */
/*     each list, the head node corresponds to the highest-priority */
/*     segment in that list, and segment priority decreases in the */
/*     forward direction. */

/*     All names begin with ST. */

/*        IDNT     Identifier */
/*        DCD      Double Precision component of descriptor */
/*        HAN      Handle */
/*        ICD      Integer component of descriptor */
/*        POOL     Doubly linked list pool. */

/*     New segments are added to the front or end of an instrument list */
/*     as appropriate, according to the rules spelled out under */
/*     entry point CKSNS. */


/*     Other local variables */


/*     Saved variables */


/*     Initial values */

    /* Parameter adjustments */
    if (descr) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_cklpf;
	case 2: goto L_ckupf;
	case 3: goto L_ckbss;
	case 4: goto L_cksns;
	case 5: goto L_ckhave;
	}


/*     Nobody has any business calling CKBSR directly. */

    if (return_()) {
	return 0;
    }
    chkin_("CKBSR", (ftnlen)5);
    sigerr_("SPICE(CKBOGUSENTRY)", (ftnlen)19);
    chkout_("CKBSR", (ftnlen)5);
    return 0;
/* $Procedure CKLPF ( C-kernel, load pointing file ) */

L_cklpf:
/* $ Abstract */

/*     Load a CK pointing file for use by the CK readers.  Return that */
/*     file's handle, to be used by other CK routines to refer to the */
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

/*     CK */
/*     DAF */

/* $ Keywords */

/*     POINTING */

/* $ Declarations */

/*     CHARACTER*(*)         FNAME */
/*     INTEGER               HANDLE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     FNAME      I   Name of the CK file to be loaded. */
/*     HANDLE     O   Loaded file's handle. */
/*     FTSIZE     P   Maximum number of loaded CK files. */

/* $ Detailed_Input */

/*     FNAME      is the name of a C-kernel file to be loaded. */

/* $ Detailed_Output */

/*     HANDLE     is an integer handle assigned to the file upon loading. */
/*                Almost every other CK routine will subsequently use */
/*                this number to refer to the file. */

/* $ Parameters */

/*     FTSIZE     is the maximum number of CK files that may */
/*                be loaded simultaneously under any circumstances. */
/*                FTSIZE is currently set to match the maximum number */
/*                of DAF files that may be loaded simultaneously. */

/* $ Exceptions */

/*     1) If an attempt is made to open more DAF files than is specified */
/*        by the parameter FTSIZE in DAFAH, an error is signaled by a */
/*        routine in the call tree of this routine. */

/*     2) If an attempt is made to load more files than is specified */
/*        by the local paramater FTSIZE, and if the DAF system has */
/*        room to load another file, the error SPICE(CKTOOMANYFILES) */
/*        signaled.  The current setting of FTSIZE does not allow this */
/*        situation to arise:  the DAF system will trap the error */
/*        before this routine has the chance. */

/*     3) If the file specified by FNAME can not be opened, an error */
/*        is signaled by a routine that this routine calls. */

/*     4) If the file specified by FNAME has already been loaded, */
/*        it will become the "last-loaded" file.  The readers */
/*        search the last-loaded file first. */

/* $ Files */

/*     The C-kernel file specified by FNAME is loaded.  The file is */
/*     assigned an integer handle by CKLPF.  Other CK routines will refer */
/*     to this file by its handle. */

/* $ Particulars */

/*     See Particulars above, in CKBSR. */

/*     If there is room for a new file, CKLPF opens the file for */
/*     reading.  This routine must be called prior to a call to CKGP or */
/*     CKGPAV. */

/*     CK readers search files loaded with CKLPF in the reverse order */
/*     in which they were loaded.  That is, last-loaded files are */
/*     searched first. */

/* $ Examples */

/*     See the Example above, in CKBSR. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     J.M. Lynch     (JPL) */
/*     J.E. McLean    (JPL) */
/*     M.J. Spencer   (JPL) */
/*     R.E. Thurman   (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -    SPICELIB Version 5.0.0, 17-MAR-2014 (NJB) */

/*        Updated segment pool initialization condition in entry */
/*        point CKLPF so that the pool is initialized only if the file */
/*        table is empty. */

/* -    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB) */

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

/* -    SPICELIB Version 4.0.0, 17-FEB-2000 (WLT) */

/*        Added the Entry point CKHAVE */

/* -    SPICELIB Version 3.0.0, 03-MAR-1999 (WLT) */

/*        The parameter STSIZE was increased from 1000 to 4000 to */
/*        avoid the buffering error that exists in the CKBSR. */

/* -    SPICELIB Version 2.0.0, 25-NOV-1992 (JML) */

/*     1) When loading a file, CKLPF now checks if the file table is */
/*        full only after determining that the file is not currently */
/*        loaded. Previously, if the file table was full and an attempt */
/*        was made to reload a file, an error was signaled.  A new */
/*        exception was added as a result of this change. */

/*     2) A bug in the way that CKLPF and CKUPF clean up the instrument */
/*        tables after a file is unloaded was fixed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU) */

/* -& */
/* $ Index_Entries */

/*     load ck pointing file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB) */

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

/* -    SPICELIB Version 2.0.0, 25-NOV-1992 (JML) */

/*        Temp version for testing purposes. */

/*     1) When loading a file, CKLPF now checks if the file table is */
/*        full only after determining that the file is not currently */
/*        loaded. Previously, if the file table was full and an attempt */
/*        was made to reload a file, an error was signaled.  A new */
/*        exception was added as a result of this change. */

/*     2) A bug in the way that CKLPF and CKUPF clean up the instrument */
/*        tables after a file is unloaded was fixed. */

/*        If as the result of loading a file that was previously loaded, */
/*        there are no more segments buffered for a particular */
/*        instrument, the counter variable for the instruments is no */
/*        longer incremented. */

/*        The following code fragment changed: */

/*           IF ( ITBEG( I ) .EQ. 0 ) THEN */

/*              . */
/*              . */
/*              . */
/*              NIT = NIT - 1 */

/*           END IF */

/*           I = I + 1 */

/*        This is the fix: */

/*           IF ( ITBEG( I ) .EQ. 0 ) THEN */

/*              . */
/*              . */
/*              . */
/*              NIT = NIT - 1 */

/*           ELSE */

/*              I = I + 1 */

/*           END IF */

/* -    Beta Version 1.1.0, 28-AUG-1990 (MJS) (JEM) */

/*        Header documentation was updated, and error handling was */
/*        modified. */

/* -    Beta Version 1.0.0, 14-MAR-1990 (RET) (IMU) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKLPF", (ftnlen)5);
    }

/*     Don't allow a search to continue after loading a file; a new */
/*     search should be re-started. */

    s_copy(status, "BOGUS ENTRY", (ftnlen)40, (ftnlen)11);

/*     Since a current search cannot be continued at this point, */
/*     free the left-over partial list searched in the */
/*     'CHECK PARTIAL LIST' state, if the list is present. */

    if (fresub) {

/*        Return the partial list to the free list. */

	tail = lnktl_(&slbeg, stpool);
	lnkfsl_(&slbeg, &tail, stpool);
	fresub = FALSE_;
    }

/*     Any time we load a file, there is a possibility that the */
/*     re-use intervals are invalid because they're been superseded */
/*     by higher-priority data.  Since we're not going to examine */
/*     the loaded file, simply indicate that all of the re-use */
/*     intervals are invalid. */

    i__1 = nit;
    for (i__ = 1; i__ <= i__1; ++i__) {
	itchkp[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("itchkp", 
		i__2, "ckbsr_", (ftnlen)1308)] = FALSE_;
    }

/*     Nothing works unless at least one file has been loaded, so this */
/*     is as good a place as any to initialize the segment table pool. */
/*     We want to avoid unnecessary initializations, so we only */
/*     initialize the list when no files are loaded. It's quite possible */
/*     to have files loaded and an empty instrument table, so we don't */
/*     want to re-initialize just because there are no instrument table */
/*     entries. */

    if (nft == 0) {
	lnkini_(&c_b9, stpool);
    }

/*     To load a new file, first try to open it for reading. */

    dafopr_(fname, handle, fname_len);
    if (failed_()) {
	chkout_("CKLPF", (ftnlen)5);
	return 0;
    }

/*     Determine if the file is already in the table. */

    findex = isrchi_(handle, &nft, fthan);
    if (findex > 0) {

/*        The last call we made to DAFOPR added another DAF link to */
/*        the CK file.  Remove this link. */

	dafcls_(handle);

/*        Handle is already in the table.  Remove it. */

	--nft;
	i__1 = nft;
	for (i__ = findex; i__ <= i__1; ++i__) {
	    fthan[(i__2 = i__ - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge("fthan"
		    , i__2, "ckbsr_", (ftnlen)1352)] = fthan[(i__3 = i__) < 
		    5000 && 0 <= i__3 ? i__3 : s_rnge("fthan", i__3, "ckbsr_",
		     (ftnlen)1352)];
	    ftnum[(i__2 = i__ - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge("ftnum"
		    , i__2, "ckbsr_", (ftnlen)1353)] = ftnum[(i__3 = i__) < 
		    5000 && 0 <= i__3 ? i__3 : s_rnge("ftnum", i__3, "ckbsr_",
		     (ftnlen)1353)];
	}

/*        Unlink any segments that came from this file. */

	i__ = 1;
	while(i__ <= nit) {
	    p = itbeg[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
		    "itbeg", i__1, "ckbsr_", (ftnlen)1363)];
	    while(p > 0) {

/*              Find the successor of P, if any. */

		nxtseg = lnknxt_(&p, stpool);
		if (sthan[(i__1 = p - 1) < 100000 && 0 <= i__1 ? i__1 : 
			s_rnge("sthan", i__1, "ckbsr_", (ftnlen)1371)] == *
			handle) {

/*                 The segment corresponding to node P came from */
/*                 the file we're unloading.  Delete the node for */
/*                 P from the segment list for instrument I; if P happens */
/*                 to be the head node for instrument I's segment list, */
/*                 make the successor of P the head of the list. */

		    lnkfsl_(&p, &p, stpool);
		    if (p == itbeg[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 
			    : s_rnge("itbeg", i__1, "ckbsr_", (ftnlen)1381)]) 
			    {
			itbeg[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : 
				s_rnge("itbeg", i__1, "ckbsr_", (ftnlen)1382)]
				 = nxtseg;
		    }
		}

/*              Update P. */

		p = nxtseg;
	    }

/*           If the list for this instrument is now empty, shorten the */
/*           current table by one: put all the entries for the last */
/*           instrument in the table into the space occupied by the */
/*           one we've deleted. */

	    if (itbeg[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
		    "itbeg", i__1, "ckbsr_", (ftnlen)1399)] <= 0) {

/*              Because all of the re-use intervals are invalid, we need */
/*              not copy the saved items associated with them.  The */
/*              items not copied are */

/*                 ITCHKP */
/*                 ITLB */
/*                 ITPRVD */
/*                 ITPRVF */
/*                 ITPRVH */
/*                 ITPRVI */
/*                 ITRUEX */
/*                 ITUB */

		itins[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itins", i__1, "ckbsr_", (ftnlen)1414)] = itins[(i__2 
			= nit - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("itins",
			 i__2, "ckbsr_", (ftnlen)1414)];
		itexp[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itexp", i__1, "ckbsr_", (ftnlen)1415)] = itexp[(i__2 
			= nit - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("itexp",
			 i__2, "ckbsr_", (ftnlen)1415)];
		ithfs[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"ithfs", i__1, "ckbsr_", (ftnlen)1416)] = ithfs[(i__2 
			= nit - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("ithfs",
			 i__2, "ckbsr_", (ftnlen)1416)];
		itlfs[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itlfs", i__1, "ckbsr_", (ftnlen)1417)] = itlfs[(i__2 
			= nit - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("itlfs",
			 i__2, "ckbsr_", (ftnlen)1417)];
		itbeg[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itbeg", i__1, "ckbsr_", (ftnlen)1418)] = itbeg[(i__2 
			= nit - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("itbeg",
			 i__2, "ckbsr_", (ftnlen)1418)];
		--nit;
	    } else {
		++i__;
	    }
	}
    } else {

/*        This is a new file.  Make sure that there are unused slots */
/*        in the file table. */

	if (nft == 5000) {
	    dafcls_(handle);
	    setmsg_("Number of files loaded is at a maximum, as specified by"
		    " the parameter FTSIZE, the value of which is #. You will"
		    " need to either load fewer files, or change the paramete"
		    "r FTSIZE.", (ftnlen)176);
	    errint_("#", &c__5000, (ftnlen)1);
	    sigerr_("SPICE(CKTOOMANYFILES)", (ftnlen)21);
	    chkout_("CKLPF", (ftnlen)5);
	    return 0;
	}
    }

/*     Determine the next file number. */

    if (next < intmax_() - 1) {
	++next;
    } else {

/*        The user is to be congratulated:  we've run out of file */
/*        numbers. */

/*        Re-set the valid file numbers so they lie in the range 1:NFT, */
/*        with the Ith file in the file table having file number I. */
/*        First update the LFS and HFS components of the instrument table */
/*        according to this mapping. */

/*        Set any instrument table entries that are lower than FTNUM(1) */
/*        to zero. */

	i__1 = nit;
	for (i__ = 1; i__ <= i__1; ++i__) {

/*           Re-map the HFS table for the Ith instrument. */

	    j = isrchi_(&ithfs[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : 
		    s_rnge("ithfs", i__2, "ckbsr_", (ftnlen)1477)], &nft, 
		    ftnum);
	    if (j > 0) {

/*              The highest file searched for instrument I is the Jth */
/*              file in the file table. */

		ithfs[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge(
			"ithfs", i__2, "ckbsr_", (ftnlen)1484)] = j;
	    } else {

/*              The highest file searched for instrument I is not in the */
/*              file table.  This occurs when the highest file searched */
/*              has been unloaded.  Note that this assigment makes all */
/*              files appear to be "new" when a lookup for instrument */
/*              I is performed. */

		ithfs[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge(
			"ithfs", i__2, "ckbsr_", (ftnlen)1494)] = 0;
	    }

/*           Re-map the LFS table for the Ith instrument. */

	    j = isrchi_(&itlfs[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : 
		    s_rnge("itlfs", i__2, "ckbsr_", (ftnlen)1501)], &nft, 
		    ftnum);
	    if (j > 0) {

/*              The lowest file searched for instrument I is the Jth file */
/*              in the file table. */

		itlfs[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge(
			"itlfs", i__2, "ckbsr_", (ftnlen)1508)] = j;
	    } else {

/*              The lowest file searched for instrument I is not in the */
/*              file table.  This occurs when the lowest file searched */
/*              has been unloaded.  Zero out both the lowest and */
/*              highest file searched to force reconstruction of the */
/*              list. */

		itlfs[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge(
			"itlfs", i__2, "ckbsr_", (ftnlen)1518)] = 0;
		ithfs[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge(
			"ithfs", i__2, "ckbsr_", (ftnlen)1519)] = 0;
	    }
	}

/*        Re-map the file number table itself. */

	i__1 = nft;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ftnum[(i__2 = i__ - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge("ftnum"
		    , i__2, "ckbsr_", (ftnlen)1530)] = i__;
	}

/*        Assign a new file number. */

	next = nft + 1;
    }

/*     Now add this file to file table. */

    ++nft;
    fthan[(i__1 = nft - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge("fthan", i__1, 
	    "ckbsr_", (ftnlen)1545)] = *handle;
    ftnum[(i__1 = nft - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge("ftnum", i__1, 
	    "ckbsr_", (ftnlen)1546)] = next;
    chkout_("CKLPF", (ftnlen)5);
    return 0;
/* $Procedure CKUPF ( C-kernel, Unload pointing file ) */

L_ckupf:
/* $ Abstract */

/*     Unload a CK pointing file so that it will no longer be searched */
/*     by the readers. */

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

/*     CK */
/*     DAF */

/* $ Keywords */

/*     POINTING */

/* $ Declarations */

/*     INTEGER               HANDLE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of CK file to be unloaded */

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

/*     See Particulars section above, in CKBSR. */

/*     Unloading a file with CKUPF removes that file from consideration */
/*     by the CK readers.  In doing so, it frees up space for another */
/*     file to be loaded. */

/* $ Examples */

/*     See the Example above, in CKBSR. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     J.M. Lynch     (JPL) */
/*     R.E. Thurman   (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.3.0, 24-FEB-2011 (NJB) */

/*        Bug fix: the null pointer test used to determine eligibility */
/*        for segment list deletion now uses the .LE. operator instead */
/*        of the .EQ. operator. */

/* -    SPICELIB Version 4.2.0, 08-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MOVED call. */

/* -    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB) */

/*        Bug fixes: */

/*        1) This routine now calls RETURN() on entry and */
/*           returns if so directed. */

/*        Also, the flags indicating validity of those re-use intervals */
/*        whose data comes from the unloaded file are set to .FALSE. */

/* -    SPICELIB Version 4.0.0, 17-FEB-2000 (WLT) */

/*        Added the Entry point CKHAVE */

/* -    SPICELIB Version 3.0.0, 03-MAR-1999 (WLT) */

/*        The parameter STSIZE was increased from 1000 to 4000 to */
/*        avoid the buffering error that exists in the CKBSR. */

/* -    SPICELIB Version 2.0.0, 25-NOV-1992 (JML) */

/*     1) A bug in the way that CKLPF and CKUPF clean up the instrument */
/*        tables after a file is unloaded was fixed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU) */

/* -& */
/* $ Index_Entries */

/*     unload ck pointing file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.3.0, 24-FEB-2011 (NJB) */

/*        Bug fix: the null pointer test used to determine eligibility */
/*        for segment list deletion now uses the .LE. operator instead */
/*        of the .EQ. operator. */

/* -    SPICELIB Version 4.2.0, 08-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MOVED call. */

/* -    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB) */

/*        Bug fixes: */

/*        1) This routine now calls RETURN() on entry and */
/*           returns if so directed. */

/*        Also, the flags indicating validity of those re-use intervals */
/*        whose data comes from the unloaded file are set to .FALSE. */

/* -    SPICELIB Version 2.0.0, 25-NOV-1992 (JML) */

/*     1) A bug in the way that CKLPF and CKUPF clean up the instrument */
/*        tables after a file is unloaded was fixed. */

/*        If as the result of unloading a file there are no more */
/*        segments buffered for a particular instrument, the counter */
/*        variable for the instruments in the instrument table is no */
/*        longer incremented. */

/*        The following code fragment changed: */

/*           IF ( ITBEG( I ) .EQ. 0 ) THEN */

/*              . */
/*              . */
/*              . */
/*              NIT = NIT - 1 */

/*           END IF */

/*           I = I + 1 */

/*        This is the fix: */

/*           IF ( ITBEG( I ) .EQ. 0 ) THEN */

/*              . */
/*              . */
/*              . */
/*              NIT = NIT - 1 */

/*           ELSE */

/*              I = I + 1 */

/*           END IF */

/* -    Beta Version 1.0.1, 29-AUG-1990 (MJS) (JEM) */

/*        Comments were updated. */

/* -    Beta Version 1.0.0, 07-SEP-1990 (RET) (IMU) */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("CKUPF", (ftnlen)5);

/*     Don't allow a search to continue after unloading a file; a new */
/*     search should be re-started. */

    s_copy(status, "BOGUS ENTRY", (ftnlen)40, (ftnlen)11);

/*     Since a current search cannot be continued at this point, */
/*     free the left-over partial list searched in the */
/*     'CHECK PARTIAL LIST' state, if the list is present. */

    if (fresub) {

/*        Return the partial list to the free list. */

	tail = lnktl_(&slbeg, stpool);
	lnkfsl_(&slbeg, &tail, stpool);
	fresub = FALSE_;
    }

/*     All of the stored segments from the file must be removed */
/*     from the segment table (by returning the corresponding nodes */
/*     to the segment table pool.) */

/*     Don't do anything if the given handle is not in the file table. */

    findex = isrchi_(handle, &nft, fthan);
    if (findex == 0) {
	chkout_("CKUPF", (ftnlen)5);
	return 0;
    }


/*     First get rid of the entry in the file table. Close the file */
/*     before wiping out the handle. */

    dafcls_(&fthan[(i__1 = findex - 1) < 5000 && 0 <= i__1 ? i__1 : s_rnge(
	    "fthan", i__1, "ckbsr_", (ftnlen)1823)]);
    --nft;
    i__1 = nft;
    for (i__ = findex; i__ <= i__1; ++i__) {
	fthan[(i__2 = i__ - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge("fthan", 
		i__2, "ckbsr_", (ftnlen)1829)] = fthan[(i__3 = i__) < 5000 && 
		0 <= i__3 ? i__3 : s_rnge("fthan", i__3, "ckbsr_", (ftnlen)
		1829)];
	ftnum[(i__2 = i__ - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge("ftnum", 
		i__2, "ckbsr_", (ftnlen)1830)] = ftnum[(i__3 = i__) < 5000 && 
		0 <= i__3 ? i__3 : s_rnge("ftnum", i__3, "ckbsr_", (ftnlen)
		1830)];
    }

/*     Check each instrument list individually. Note that the first */
/*     node on each list, having no predecessor, must be handled */
/*     specially. */

    i__ = 1;
    while(i__ <= nit) {
	p = itbeg[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge("itbeg",
		 i__1, "ckbsr_", (ftnlen)1842)];
	while(p > 0) {
	    nxtseg = lnknxt_(&p, stpool);
	    if (sthan[(i__1 = p - 1) < 100000 && 0 <= i__1 ? i__1 : s_rnge(
		    "sthan", i__1, "ckbsr_", (ftnlen)1848)] == *handle) {
		if (p == itbeg[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : 
			s_rnge("itbeg", i__1, "ckbsr_", (ftnlen)1850)]) {
		    itbeg[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			    "itbeg", i__1, "ckbsr_", (ftnlen)1851)] = nxtseg;
		}

/*              Free this segment table entry. */

		lnkfsl_(&p, &p, stpool);
	    }
	    p = nxtseg;
	}

/*        If the list for this instrument is now empty, shorten the */
/*        current table by one: put all the entries for the last */
/*        instrument in the table into the space occupied by the */
/*        one we've deleted. */

	if (itbeg[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge("itbeg",
		 i__1, "ckbsr_", (ftnlen)1870)] <= 0) {
	    if (i__ != nit) {
		itins[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itins", i__1, "ckbsr_", (ftnlen)1874)] = itins[(i__2 
			= nit - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("itins",
			 i__2, "ckbsr_", (ftnlen)1874)];
		itexp[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itexp", i__1, "ckbsr_", (ftnlen)1875)] = itexp[(i__2 
			= nit - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("itexp",
			 i__2, "ckbsr_", (ftnlen)1875)];
		ithfs[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"ithfs", i__1, "ckbsr_", (ftnlen)1876)] = ithfs[(i__2 
			= nit - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("ithfs",
			 i__2, "ckbsr_", (ftnlen)1876)];
		itlfs[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itlfs", i__1, "ckbsr_", (ftnlen)1877)] = itlfs[(i__2 
			= nit - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("itlfs",
			 i__2, "ckbsr_", (ftnlen)1877)];
		itbeg[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itbeg", i__1, "ckbsr_", (ftnlen)1878)] = itbeg[(i__2 
			= nit - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("itbeg",
			 i__2, "ckbsr_", (ftnlen)1878)];
		itlb[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itlb", i__1, "ckbsr_", (ftnlen)1879)] = itlb[(i__2 = 
			nit - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("itlb", 
			i__2, "ckbsr_", (ftnlen)1879)];
		itub[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itub", i__1, "ckbsr_", (ftnlen)1880)] = itub[(i__2 = 
			nit - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("itub", 
			i__2, "ckbsr_", (ftnlen)1880)];
		itprvf[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itprvf", i__1, "ckbsr_", (ftnlen)1881)] = itprvf[(
			i__2 = nit - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge(
			"itprvf", i__2, "ckbsr_", (ftnlen)1881)];
		itprvh[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itprvh", i__1, "ckbsr_", (ftnlen)1882)] = itprvh[(
			i__2 = nit - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge(
			"itprvh", i__2, "ckbsr_", (ftnlen)1882)];
		s_copy(itprvi + ((i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : 
			s_rnge("itprvi", i__1, "ckbsr_", (ftnlen)1883)) * 40, 
			itprvi + ((i__2 = nit - 1) < 100 && 0 <= i__2 ? i__2 :
			 s_rnge("itprvi", i__2, "ckbsr_", (ftnlen)1883)) * 40,
			 (ftnlen)40, (ftnlen)40);
		itchkp[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itchkp", i__1, "ckbsr_", (ftnlen)1884)] = itchkp[(
			i__2 = nit - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge(
			"itchkp", i__2, "ckbsr_", (ftnlen)1884)];
		itruex[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itruex", i__1, "ckbsr_", (ftnlen)1885)] = itruex[(
			i__2 = nit - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge(
			"itruex", i__2, "ckbsr_", (ftnlen)1885)];
		moved_(&itprvd[(i__1 = nit * 5 - 5) < 500 && 0 <= i__1 ? i__1 
			: s_rnge("itprvd", i__1, "ckbsr_", (ftnlen)1887)], &
			c__5, &itprvd[(i__2 = i__ * 5 - 5) < 500 && 0 <= i__2 
			? i__2 : s_rnge("itprvd", i__2, "ckbsr_", (ftnlen)
			1887)]);
	    }
	    --nit;
	} else {
	    ++i__;
	}
    }

/*     Any time we unload a file, we may be removing the file */
/*     providing data for the re-use interval for one or more */
/*     instruments.  For each instrument, if the handle associated */
/*     with the re-use interval happens to be that of the file */
/*     we're unloading, indicate that the re-use interval is invalid. */

    i__1 = nit;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (itchkp[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("itch"
		"kp", i__2, "ckbsr_", (ftnlen)1910)]) {
	    if (itprvh[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge(
		    "itprvh", i__2, "ckbsr_", (ftnlen)1912)] == *handle) {
		itchkp[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge(
			"itchkp", i__2, "ckbsr_", (ftnlen)1913)] = FALSE_;
	    }
	}
    }
    chkout_("CKUPF", (ftnlen)5);
    return 0;
/* $Procedure CKBSS ( C-kernel, begin search for segment ) */

L_ckbss:
/* $ Abstract */

/*     Initiate search through loaded files to find segments applicable */
/*     to the spacecraft instrument and time specified. */

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

/*     CK */
/*     DAF */

/* $ Keywords */

/*     POINTING */

/* $ Declarations */

/*     INTEGER               INST */
/*     DOUBLE PRECISION      SCLKDP */
/*     DOUBLE PRECISION      TOL */
/*     LOGICAL               NEEDAV */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     INST       I   Spacecraft and instrument ID. */
/*     SCLKDP     I   Encoded spacecraft clock time. */
/*     TOL        I   Time tolerance. */
/*     NEEDAV     I   Is there a need for angular velocity? */

/* $ Detailed_Input */

/*     CKBSS sets up a search for segments. The four quantities below */
/*     establish the search criteria. */


/*     INST       is the NAIF ID of an instrument. */

/*     SCLKDP     is an encoded spacecraft clock time. */

/*     TOL        is a time tolerance, measured in the same units as */
/*                encoded spacecraft clock. */

/*     NEEDAV     indicates whether or not angular velocity data is */
/*                required. */

/*                If true, only segments containing pointing and angular */
/*                velocity data will be checked. If false, segments */
/*                containing just pointing data will also be considered. */


/*     A segment matches the CKBSS/CKSNS search criteria when the */
/*     following statements are true. */

/*        1) INST matches the instrument number for the segment. */

/*        2) The time interval [SCLKDP - TOL, SCLKDP + TOL] intersects */
/*           the time interval of the segment. */

/*        3) If angular velocity data is required, as indicated by */
/*           NEEDAV, the segment contains angular velocity data. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If no files have been loaded, the error SPICE(NOLOADEDFILES) */
/*        is signaled. */

/* $ Files */

/*     All files loaded by CKLPF are potential search targets for */
/*     CKSNS. */

/* $ Particulars */

/*     CKBSS sets up a search for segments by CKSNS. It records the */
/*     instrument and time to be searched for, and whether to require */
/*     segments containing angular velocity data. If angular velocity */
/*     data are required, only segments containing angular velocity */
/*     data will be returned by CKSNS. If angular velocity data are */
/*     not required, segments returned by CKSNS may or may not contain */
/*     angular velocity data. */

/*     CKBSS determines the first task that CKSNS will have to perform */
/*     if it is called to get an applicable segment. */

/* $ Examples */

/*     See Examples in CKBSR. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     M.J. Spencer   (JPL) */
/*     J.E. McLean    (JPL) */
/*     R.E. Thurman   (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB) */

/*        Updated to support new doubly-linked list implementation: */
/*        partial segment list that cannot be buffered is now */
/*        deallocated here rather than in CKSNS.  Minor changes to */
/*        comments were made as well. */

/* -    SPICELIB Version 4.0.0, 17-FEB-2000 (WLT) */

/*        Added the Entry point CKHAVE */

/* -    SPICELIB Version 3.0.0, 03-MAR-1999 (WLT) */

/*        The parameter STSIZE was increased from 1000 to 4000 to */
/*        avoid the buffering error that exists in the CKBSR. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU) */

/* -& */
/* $ Index_Entries */

/*     begin search for ck segment */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB) */

/*        Updated to support new doubly-linked list implementation: */
/*        partial segment list that cannot be buffered is now */
/*        deallocated here rather than in CKSNS.  Minor changes to */
/*        comments were made as well. */

/* -    Beta Version 1.1.0, 28-AUG-1990 (MJS) (JEM) */

/*        The following changes were made as a result of the */
/*        NAIF CK Code and Documentation Review: */

/*          1) The variable SCLK was changed to SCLKDP. */
/*          2) Header documentation was updated. */

/* -    Beta Version 1.0.0, 20-APR-1990 (RET) (IMU) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKBSS", (ftnlen)5);
    }

/*     If we're starting a new search after passing through the */
/*     'CHECK PARTIAL LIST' state, free the left-over partial list */
/*     that was searched in that state, if necessary. */

    if (fresub) {

/*        Return the partial list to the free list. */

	tail = lnktl_(&slbeg, stpool);
	lnkfsl_(&slbeg, &tail, stpool);
	fresub = FALSE_;
    }

/*     Make copies of the instrument ID code and angular velocity flag. */
/*     Save the request time itself. */

/*     And form the endpoints of the acceptable time interval using the */
/*     input time and time tolerance. */

    scinst = *inst;
    alpha = *sclkdp - *tol;
    omega = *sclkdp + *tol;
    avneed = *needav;
    reqt = *sclkdp;
    savtol = *tol;

/*     There must be at least one file loaded. */

    if (nft == 0) {
	setmsg_("At least one CK file needs must be loaded by CKLPF before b"
		"eginning a search.", (ftnlen)77);
	sigerr_("SPICE(NOLOADEDFILES)", (ftnlen)20);
	chkout_("CKBSS", (ftnlen)5);
	return 0;
    }

/*     The stack of suspended tasks is empty. */

    top = 0;

/*     Is the instrument already in the instrument table?  The answer */
/*     determines what the first task for CKSNS will be. */

    iindex = isrchi_(&scinst, &nit, itins);
    if (iindex == 0) {
	s_copy(status, "NEW INSTRUMENT", (ftnlen)40, (ftnlen)14);
    } else {

/*        Set the status so that CKSNS will determine whether to check */
/*        the segment list, search new files, or return data from the */
/*        re-use interval. */

	s_copy(status, "?", (ftnlen)40, (ftnlen)1);
    }

/*     Indicate a new search has started. */

    newsch = TRUE_;
    chkout_("CKBSS", (ftnlen)5);
    return 0;
/* $Procedure CKSNS ( C-kernel, Select next segment ) */

L_cksns:
/* $ Abstract */

/*     Search through loaded files to find a segment matching the */
/*     requested instrument, time, and need for angular velocity. */
/*     Buffer segment descriptors, identifiers, and handles in the */
/*     process to minimize file reads. */

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

/*     CK */
/*     DAF */

/* $ Keywords */

/*     POINTING */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     DOUBLE PRECISION      DESCR  ( * ) */
/*     CHARACTER*(*)         SEGID */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     O   Handle of file containing the applicable segment. */
/*     DESCR      O   Descriptor of the applicable segment. */
/*     SEGID      O   Identifier of the applicable segment. */
/*     FOUND      O   True if a segment was found. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     HANDLE     is an integer handle of the file containing the */
/*                segment matching the instrument and time */
/*                specifications made in the last call to CKBSS. */

/*     DESCR, */
/*     SEGID      are the descriptor and identifier of the segment found */
/*                which matches the instrument and time specifications */
/*                made in the last call to CKBSS. */

/*     FOUND      is true if an applicable segment was found.  False */
/*                otherwise.  If FOUND is false, the values of the */
/*                other arguments are meaningless. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If CKSNS is called without CKBSS ever having been called, */
/*        the error 'SPICE(CALLCKBSSFIRST)' is signaled. */

/*     2) If no segment is found that matches the search criteria, */
/*        FOUND is set to false, but the values of HANDLE, DESCR, */
/*        and SEGID will be meaningless. */

/* $ Files */

/*     All files loaded by CKLPF are potential search targets for */
/*     CKSNS. The files are all referred to by their integer handles. */

/* $ Particulars */

/*     CKSNS is used to locate segments based on the search criteria */
/*     established by the most recent call to CKBSS.  When a segment */
/*     is found it will have the following characteristics: */

/*        1) Its instrument will match the instrument specified in the */
/*           call to CKBSS. */

/*        2) Its time interval will intersect the time interval */

/*              [SCLKDP - TOL, SCLKDP + TOL], */

/*           where SCLKDP and TOL were specified in the call to CKBSS. */

/*        3) If there is a need for angular velocity data, as specified */
/*           by NEEDAV in the call to CKBSS, a returned segment */
/*           will contain angular velocity data. If there is no need */
/*           for such data, the returned segment may or may not contain */
/*           angular velocity data. */

/*     The first call to CKSNS following a call to CKBSS starts a search */
/*     through loaded files and either returns the first applicable */
/*     segment, or indicates that no segment was found. */

/*     CKSNS searches through last-loaded files first. Individual */
/*     files are searched backwards, so that segments that were inserted */
/*     last into the file get checked first. */

/*     Subsequent calls to CKSNS pick up the search exactly where the */
/*     previous calls left off. If a segment is not found, future calls */
/*     will also indicate that no segment could be found, until a new */
/*     search is begun. */

/*     CKSNS also buffers segment descriptors and identifiers, to */
/*     attempt to minimize file reads. */

/* $ Examples */

/*     See Examples in CKBSR. */

/* $ Restrictions */

/*     1) This subroutine assumes that a search has been initiated by */
/*        a call to CKBSS. */

/*     2) When a CK file is loaded or unloaded, a new search must */
/*        be started via a call to CKBSS before this routine may */
/*        be called. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     J.E. McLean    (JPL) */
/*     M.J. Spencer   (JPL) */
/*     R.E. Thurman   (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.5.0, 24-FEB-2011 (NJB) */

/*        Bug fix: in the 'MAKE ROOM' state, when the suspended activity */
/*        is 'ADD TO FRONT' and no segment table room is available, the */
/*        instrument table's pointer to the current segment list is now */
/*        set to null. Previously the pointer was allowed to go stale. */

/* -    SPICELIB Version 4.2.0, 08-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MOVED call. */

/* -    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB) */

/*        Bug fixes: */

/*           1) When a segment list is freed because the entire list */
/*              is contributed by a single CK file, and the list is */
/*              too large to be buffered, the corresponding intrument */
/*              table pointer is now set to null. */

/*           2) An algorithm change has eliminated a bug caused by not */
/*              updating the current instrument index when instrument */
/*              table entries  having empty segment lists were compressed */
/*              out of the instrument table.  Previously the instrument */
/*              table pointer IINDEX could go stale after the */
/*              compression. */

/*           3) DAF calls are now followed by tests of FAILED() */
/*              in order to ensure that the main state loop terminates. */

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

/*        The instrument table size has been increased to 100 in order to */
/*        decrease the chance of thrashing due to swapping segment */
/*        lists for different bodies. */

/*        Various small updates and corrections were made to the */
/*        comments throughout the file. */

/* -    SPICELIB Version 4.0.0, 17-FEB-2000 (WLT) */

/*        Added the Entry point CKHAVE */

/* -    SPICELIB Version 3.0.0, 03-MAR-1999 (WLT) */

/*        The parameter STSIZE was increased from 1000 to 4000 to */
/*        avoid the buffering error that exists in the CKBSR. */

/* -    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.1.0, 01-NOV-1990 (JML) */

/*        A check on the initial value of the variable STATUS */
/*        was added in order to detect the situation in which */
/*        CKBSS was never called to initiate a search. */


/* -    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU) */

/* -& */
/* $ Index_Entries */

/*     select next ck segment */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.5.0, 24-FEB-2011 (NJB) */

/*        Bug fix: in the 'MAKE ROOM' state, when the suspended activity */
/*        is 'ADD TO FRONT' and no segment table room is available, the */
/*        instrument table's pointer to the current segment list is now */
/*        set to null. Previously the pointer was allowed to go stale. */

/* -    SPICELIB Version 4.2.0, 08-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MOVED call. */

/* -    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB) */

/*        Bug fixes: */

/*           1) When a segment list is freed because the entire list */
/*              is contributed by a single CK file, and the list is */
/*              too large to be buffered, the corresponding instrument */
/*              table pointer is now set to null. */

/*           2) An algorithm change has eliminated a bug caused by not */
/*              updating the current instrument index when instrument */
/*              table entries  having empty segment lists were compressed */
/*              out of the instrument table.  Previously the instrument */
/*              table pointer IINDEX could go stale after the */
/*              compression. */

/*           3) DAF calls are now followed by tests of FAILED() */
/*              in order to ensure that the main state loop terminates. */

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

/*        The instrument table size has been increased to 100 in order to */
/*        decrease the chance of thrashing due to swapping segment */
/*        lists for different instruments. */

/*        Various small updates and corrections were made to the */
/*        comments throughout the file. */


/* -    SPICELIB Version 1.1.0, 01-NOV-1990 (JML) */

/*        A check on the initial value of the variable STATUS */
/*        was added in order to detect the situation in which */
/*        CKBSS was never called to initiate a search. */


/* -    Beta Version 1.1.0, 28-AUG-1990 (MJS) (JEM) */

/*        The following changes were made as a result of the */
/*        NAIF CK Code and Documentation Review: */

/*           1) The variable IDENT was changed to SEGID. */
/*           2) The local variables INTDES and DPDES were changed to */
/*              ICD and DCD. */
/*           3) Header and internal documentation was corrected and */
/*              updated. */

/* -    Beta Version 1.0.0, 20-APR-1990 (RET) (IMU) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKSNS", (ftnlen)5);
    }

/*     Nothing's been found yet. */

    *found = FALSE_;

/*     Initialize the segment list pointer to the saved value from */
/*     the previous pass through this routine, if any. */

    p = savep;

/*     CKSNS buffers segment descriptors and identifiers, to */
/*     attempt to minimize file reads. Buffering segments involves */
/*     maintaining three tables:  the file table, the instrument table, */
/*     and the segment table. CKSNS is broken down into various tasks, */
/*     described in the code below, which perform these manipulations. */

/*     A description of the components of each table is provided in */
/*     the declarations section of CKBSR. */

/*     Basically, the buffering is performed as follows: once a request */
/*     for a segment for a particular instrument is made, if there are */
/*     no adequate entries in the buffer already, a search is made */
/*     through loaded files for applicable segments.  Every segment */
/*     pertaining to that instrument in a searched file is buffered, */
/*     before a check of the current buffer is made.  If the search */
/*     doesn't turn up a segment matching the specified search criteria */
/*     the next file is searched and new segments are added to the list, */
/*     and so on. */

/*     The information in the segment table (ST) is stored in a */
/*     doubly-linked list. Each node in the list contains several */
/*     individual pieces of data, which are stored in parallel */
/*     arrays. */

/*     In the following loop, we will try to simplify things by */
/*     doing exactly one thing on each pass through the loop. */
/*     After each pass, the status of the loop (STATUS) will be */
/*     adjusted to reflect the next thing that needs to be done. */
/*     The first task is set by CKBSS. */

/*     Occasionally, the current task will have to be interrupted */
/*     until another task can be carried out. (For example, when */
/*     collecting new segments, an interrupt might place a segment */
/*     at the front or end of the current instrument list; when placing */
/*     the segment on the list, a second interrupt might free */
/*     room in the segment table in order to allow the addition */
/*     to proceed.) In this case, the current task will be saved and */
/*     restored after the more urgent task has been completed. */

/*     The loop can terminate in only one of two ways (unless an error */
/*     occurs). First, if an applicable segment is found in the segment */
/*     table, the handle, descriptor, and identifier for the segment */
/*     are returned immediately.  Second, if the table does not contain */
/*     an applicable segment, and if no files remain to be searched, */
/*     the loop terminates normally, and no data are returned. */

/*     The status is saved on exit, however, so that subsequent calls */
/*     will resume a search exactly where previous calls left off. */

/*     Each status is described below. */

/*     'NEW INSTRUMENT' */

/*        This indicates that the specified spacecraft/instrument has */
/*        no segments stored for it at all. It must be added to the */
/*        instrument table.  (This is followed immediately by an */
/*        OLD FILES search, in which every file loaded is considered an */
/*        old file.) */

/*     'NEW FILES' */

/*        This indicates that at least one new file has been added */
/*        since the last time the segment list for the specified */
/*        instrument was searched. Find the oldest of these new files, */
/*        and begin a NEW SEGMENTS search in forward order for */
/*        segments to add to the front of the list. */

/*     'NEW SEGMENTS' */

/*        Continue a NEW FILES search, adding segments for the specified */
/*        instrument to the front of the list. */

/*     'OLD FILES' */

/*        This indicates that although the list has been searched */
/*        and found to contain no applicable segment, some of the */
/*        older files remain to be searched. Find the newest of these */
/*        old files, and begin an OLD SEGMENTS search in backward order. */

/*     'OLD SEGMENTS' */

/*        Continue an OLD FILES search, adding segments for the specified */
/*        instrument to the end of the list. */

/*     'CHECK LIST' */

/*        This indicates that the list is ready to be searched, */
/*        either because no new files have been added, or because */
/*        segments from a new file or an old file have recently */
/*        been added. */

/*        The list is never checked until all new files have been */
/*        searched. */

/*        If an applicable segment is found, it is returned. */

/*     'MAKE ROOM' (Interrupt) */

/*        This indicates that one of the instruments must be removed, */
/*        along with its stored segments, to make room for another */
/*        instrument or segment.  The instrument (other than the */
/*        specified instrument) with the smallest expense is selected */
/*        for this honor. */

/*     'ADD TO FRONT' (Interrupt) */

/*        This indicates that a segment has been found (during the */
/*        course of a NEW FILES search) and must be added to the front */
/*        of the list. */

/*     'ADD TO END' (Interrupt) */

/*        This indicates that a segment has been found (during the */
/*        course of an OLD FILES search) and must be added to the end */
/*        of the list. */

/*     'PREPARE PARTIAL LIST' */

/*        This indicates that an attempt to 'MAKE ROOM' failed when */
/*        trying to 'ADD TO END' because all of the segments in the */
/*        table were for the instrument being searched on.  The partial */
/*        list is found that contains all of the segments that were in */
/*        the process of being added to the table for the current old */
/*        file.  Next a 'CHECK PARTIAL LIST' is performed. Following */
/*        that, a 'SEARCH W/O BUFF' is performed on all unsearched */
/*        files. */

/*     'CHECK PARTIAL LIST' */

/*        This indicates that a portion of the list can't be buffered. */
/*        Before this portion is freed, it is to be checked for */
/*        applicable segments. */

/*     'SEARCH W/O BUFF' */

/*        This indicates that the segment table was too small to handle */
/*        all of the segments for the current instrument, and that the */
/*        remaining unchecked old files should be searched for applicable */
/*        segments, without buffering the segments. */

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

/*     'HOPELESS' */

/*        This indicates that the table does not contain an applicable */
/*        segment, and no files remain to be searched. */

/*      'BOGUS ENTRY' */

/*        This is the initial value of STATUS and indicates that no */
/*        call to CKBSS was ever made. If this is the case then an */
/*        error will be signaled. */

    if (s_cmp(status, "BOGUS ENTRY", (ftnlen)40, (ftnlen)11) == 0) {
	setmsg_("Must begin a search by calling CKBSS first.", (ftnlen)43);
	sigerr_("SPICE(CALLCKBSSFIRST)", (ftnlen)21);
	chkout_("CKSNS", (ftnlen)5);
	return 0;
    }
    while(s_cmp(status, "HOPELESS", (ftnlen)40, (ftnlen)8) != 0) {

/*        If new files have been added, they have to be searched. */
/*        Otherwise, go right to the list of stored segments. */

	if (s_cmp(status, "?", (ftnlen)40, (ftnlen)1) == 0) {

/*           There are two ways to get to this point. */

/*           1)  Status may have been set to '?' by CKBSS. */

/*           2)  Status was set to '?' by the NEW SEGMENTS block */
/*               of code as the result of finishing the read of */
/*               a new file. */

	    if (ithfs[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
		    "ithfs", i__1, "ckbsr_", (ftnlen)2755)] < ftnum[(i__2 = 
		    nft - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge("ftnum", 
		    i__2, "ckbsr_", (ftnlen)2755)]) {
		s_copy(status, "NEW FILES", (ftnlen)40, (ftnlen)9);
	    } else {

/*              Much of the time, the segment used to satisfy the */
/*              previous request will also satisfy the current */
/*              request.  Check whether this is the case. */

		if (itchkp[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
			s_rnge("itchkp", i__1, "ckbsr_", (ftnlen)2765)]) {

/*                 The previous segment found for the current instrument */
/*                 is a viable candidate for the current request.  See */
/*                 whether the request time REQT falls into the time */
/*                 interval for which this segment provides the */
/*                 highest-priority coverage. */

/*                 We treat the re-use interval as topologically open */
/*                 because one or both endpoints may belong to */
/*                 higher-priority segments. */

		    if (reqt > itlb[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? 
			    i__1 : s_rnge("itlb", i__1, "ckbsr_", (ftnlen)
			    2777)] + savtol && reqt < itub[(i__2 = iindex - 1)
			     < 100 && 0 <= i__2 ? i__2 : s_rnge("itub", i__2, 
			    "ckbsr_", (ftnlen)2777)] - savtol) {

/*                    The request time falls into the portion of */
/*                    the re-use interval that isn't blocked by */
/*                    higher-priority segments, when the coverage of */
/*                    those segments is extended in either direction */
/*                    by TOL. */

			if (! avneed || itprvf[(i__1 = iindex - 1) < 100 && 0 
				<= i__1 ? i__1 : s_rnge("itprvf", i__1, "ckb"
				"sr_", (ftnlen)2786)] != 0) {

/*                       This segment has angular velocity if we */
/*                       need it.  The segment satisfies the */
/*                       request. */

			    *handle = itprvh[(i__1 = iindex - 1) < 100 && 0 <=
				     i__1 ? i__1 : s_rnge("itprvh", i__1, 
				    "ckbsr_", (ftnlen)2793)];
			    s_copy(segid, itprvi + ((i__1 = iindex - 1) < 100 
				    && 0 <= i__1 ? i__1 : s_rnge("itprvi", 
				    i__1, "ckbsr_", (ftnlen)2794)) * 40, 
				    segid_len, (ftnlen)40);
			    moved_(&itprvd[(i__1 = iindex * 5 - 5) < 500 && 0 
				    <= i__1 ? i__1 : s_rnge("itprvd", i__1, 
				    "ckbsr_", (ftnlen)2796)], &c__5, descr);
			    *found = TRUE_;

/*                       We can only use the re-use interval once on */
/*                       a given search.  If this search is continued, */
/*                       we'll have to check the list.  Prepare now. */

			    savep = itbeg[(i__1 = iindex - 1) < 100 && 0 <= 
				    i__1 ? i__1 : s_rnge("itbeg", i__1, "ckb"
				    "sr_", (ftnlen)2805)];
			    s_copy(status, "CHECK LIST", (ftnlen)40, (ftnlen)
				    10);
			    chkout_("CKSNS", (ftnlen)5);
			    return 0;
			}

/*                    We needed angular velocity data but didn't have */
/*                    it if we reached this point. */

		    }

/*                 Adjust the expense here. If the expense of the list */
/*                 contains a component due to the cost of finding the */
/*                 unbuffered segment providing data for re-use, subtract */
/*                 that component from the expense. */

		    itexp[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itexp", i__1, "ckbsr_", (ftnlen)2824)] = 
			    itexp[(i__2 = iindex - 1) < 100 && 0 <= i__2 ? 
			    i__2 : s_rnge("itexp", i__2, "ckbsr_", (ftnlen)
			    2824)] - itruex[(i__3 = iindex - 1) < 100 && 0 <= 
			    i__3 ? i__3 : s_rnge("itruex", i__3, "ckbsr_", (
			    ftnlen)2824)];
		    itruex[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itruex", i__1, "ckbsr_", (ftnlen)2825)] = 
			    0;

/*                 The re-use interval becomes invalid if it didn't */
/*                 satisfy the request.  The validity flag gets */
/*                 re-set below. */

/*                 At this point, the previous segment is not a candidate */
/*                 to satisfy the request---at least not until we've done */
/*                 some file searches to verify that */

/*                    - The previous segment is still available. */

/*                    - The previous segment hasn't been superseded by a */
/*                      more recently loaded segment. */

/*                 Carry on with the usual search algorithm. */

		    itchkp[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itchkp", i__1, "ckbsr_", (ftnlen)2843)] = 
			    FALSE_;
		}

/*              If the segment list for this instrument is empty, make */
/*              sure the expense is reset to 0. */

		if (itbeg[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
			s_rnge("itbeg", i__1, "ckbsr_", (ftnlen)2851)] == 0) {
		    itexp[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itexp", i__1, "ckbsr_", (ftnlen)2852)] = 
			    0;
		}

/*              Prepare to look at the first segment in the list for */
/*              this instrument. */

		p = itbeg[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
			s_rnge("itbeg", i__1, "ckbsr_", (ftnlen)2859)];
		s_copy(status, "CHECK LIST", (ftnlen)40, (ftnlen)10);
	    }
	} else if (s_cmp(status, "NEW INSTRUMENT", (ftnlen)40, (ftnlen)14) == 
		0) {

/*           New instruments are added to the end of the instrument */
/*           table. If the table is full, one of the current occupants */
/*           must be removed to make room for the new one. */

/*           Setting LFS to one more than the highest current file */
/*           number means the 'OLD FILES' search that follows will */
/*           begin with the last-loaded file. */

/*           There is one way to get here: */

/*           1)  The variable STATUS was set to NEW INSTRUMENT prior */
/*               in CKBSS. */

/*           Find the cheapest slot in the instrument table to store */
/*           the initial information about this instrument. */

/*           NOTE:  This used to be handled by the MAKE ROOM section. */
/*           However, trying to handle this special case there was */
/*           just more trouble than it was worth. */

	    if (nit < 100) {

/*              If the instrument table isn't full, the cheapest place is */
/*              just the next unused row of the table. */

		++nit;
		cheap = nit;
	    } else {

/*              The instrument table is full.  Find the least */
/*              expensive instrument in the table and remove it. */

		cheap = 1;
		minexp = itexp[0];
		i__1 = nit;
		for (i__ = 2; i__ <= i__1; ++i__) {
		    if (itexp[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : 
			    s_rnge("itexp", i__2, "ckbsr_", (ftnlen)2906)] < 
			    minexp) {
			cheap = i__;
			minexp = itexp[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? 
				i__2 : s_rnge("itexp", i__2, "ckbsr_", (
				ftnlen)2908)];
		    }
		}

/*              If there are any segments associated with the */
/*              least expensive instrument, we put them back on the free */
/*              list. */

		head = itbeg[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : 
			s_rnge("itbeg", i__1, "ckbsr_", (ftnlen)2918)];
		if (head > 0) {
		    tail = -lnkprv_(&head, stpool);
		    lnkfsl_(&head, &tail, stpool);
		}
	    }

/*           Set up a table entry for the new instrument. */

	    itins[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge("iti"
		    "ns", i__1, "ckbsr_", (ftnlen)2932)] = scinst;
	    itexp[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge("ite"
		    "xp", i__1, "ckbsr_", (ftnlen)2933)] = 0;
	    ithfs[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge("ith"
		    "fs", i__1, "ckbsr_", (ftnlen)2934)] = ftnum[(i__2 = nft - 
		    1) < 5000 && 0 <= i__2 ? i__2 : s_rnge("ftnum", i__2, 
		    "ckbsr_", (ftnlen)2934)];
	    itlfs[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge("itl"
		    "fs", i__1, "ckbsr_", (ftnlen)2935)] = ftnum[(i__2 = nft - 
		    1) < 5000 && 0 <= i__2 ? i__2 : s_rnge("ftnum", i__2, 
		    "ckbsr_", (ftnlen)2935)] + 1;
	    itbeg[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge("itb"
		    "eg", i__1, "ckbsr_", (ftnlen)2936)] = 0;
	    itchkp[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
		    "itchkp", i__1, "ckbsr_", (ftnlen)2937)] = FALSE_;
	    iindex = cheap;

/*           The following items associated with the re-use interval */
/*           need not be initialized at this point: */

/*              ITRUEX */
/*              ITLB */
/*              ITUB */
/*              ITPRVF */
/*              ITPRVH */
/*              ITPRVI */
/*              ITPRVD */

/*           However, we'll give these items initial values to */
/*           help prevent compilation warnings from zealous */
/*           compilers. */

	    itruex[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
		    "itruex", i__1, "ckbsr_", (ftnlen)2956)] = 0;
	    itlb[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge("itlb",
		     i__1, "ckbsr_", (ftnlen)2957)] = dpmin_();
	    itub[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge("itub",
		     i__1, "ckbsr_", (ftnlen)2958)] = dpmax_();
	    itprvf[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
		    "itprvf", i__1, "ckbsr_", (ftnlen)2959)] = 0;
	    itprvh[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
		    "itprvh", i__1, "ckbsr_", (ftnlen)2960)] = 0;
	    s_copy(itprvi + ((i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : 
		    s_rnge("itprvi", i__1, "ckbsr_", (ftnlen)2961)) * 40, 
		    " ", (ftnlen)40, (ftnlen)1);
	    cleard_(&c__5, &itprvd[(i__1 = cheap * 5 - 5) < 500 && 0 <= i__1 ?
		     i__1 : s_rnge("itprvd", i__1, "ckbsr_", (ftnlen)2962)]);

/*           Now search all of the files for segments relating to */
/*           this instrument. */

	    s_copy(status, "OLD FILES", (ftnlen)40, (ftnlen)9);
	} else if (s_cmp(status, "NEW FILES", (ftnlen)40, (ftnlen)9) == 0) {

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
	    while(ithfs[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
		    s_rnge("ithfs", i__1, "ckbsr_", (ftnlen)2991)] >= ftnum[(
		    i__2 = findex - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge(
		    "ftnum", i__2, "ckbsr_", (ftnlen)2991)]) {
		++findex;
	    }
	    ithfs[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
		    "ithfs", i__1, "ckbsr_", (ftnlen)2997)] = ftnum[(i__2 = 
		    findex - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge("ftnum", 
		    i__2, "ckbsr_", (ftnlen)2997)];
	    dafbfs_(&fthan[(i__1 = findex - 1) < 5000 && 0 <= i__1 ? i__1 : 
		    s_rnge("fthan", i__1, "ckbsr_", (ftnlen)2999)]);
	    if (failed_()) {
		chkout_("CKSNS", (ftnlen)5);
		return 0;
	    }
	    s_copy(status, "NEW SEGMENTS", (ftnlen)40, (ftnlen)12);

/*           The cost of the list contributed by the new file is */
/*           zero so far. */

	    cost = 0;
	} else if (s_cmp(status, "NEW SEGMENTS", (ftnlen)40, (ftnlen)12) == 0)
		 {

/*           New files are searched in forward order. Segments, when */
/*           found, are inserted at the front of the list. Invisible */
/*           segments (initial time > final time) are ignored. */

/*           Each segment examined, whether applicable or not, adds to */
/*           the expense of the list. */

/*           The only way to get here is from the NEW FILES block */
/*           of the IF structure. */
	    daffna_(&fnd);
	    if (failed_()) {
		chkout_("CKSNS", (ftnlen)5);
		return 0;
	    }
	    if (! fnd) {

/*              We're out of segments in the current file.  Decide */
/*              whether we need to examine another new file, or */
/*              whether we're ready to check the list. */

		s_copy(status, "?", (ftnlen)40, (ftnlen)1);
		itexp[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itexp", i__1, "ckbsr_", (ftnlen)3041)] = itexp[(i__2 
			= iindex - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge(
			"itexp", i__2, "ckbsr_", (ftnlen)3041)] + cost;
	    } else {
		dafgs_(descr);
		dafus_(descr, &c__2, &c__6, dcd, icd);
		if (failed_()) {
		    chkout_("CKSNS", (ftnlen)5);
		    return 0;
		}
		if (icd[0] == scinst && dcd[0] <= dcd[1]) {
		    s_copy(doing, "NEW SEGMENTS", (ftnlen)40, (ftnlen)12);
		    s_copy(urgent, "ADD TO FRONT", (ftnlen)40, (ftnlen)12);
		    s_copy(status, "SUSPEND", (ftnlen)40, (ftnlen)7);
		}
		++cost;
	    }

/*           If we haven't reset the status, we'll return for another */
/*           'NEW SEGMENTS' pass. */

	} else if (s_cmp(status, "OLD FILES", (ftnlen)40, (ftnlen)9) == 0) {

/*           When old files must be searched (because the segments in */
/*           the list are inadequate), they should be searched in */
/*           backward order, beginning with the newest old file not */
/*           yet searched.  The segment list will be re-checked */
/*           after each file is searched.  If a match is found, */
/*           the search terminates, so some old files may not be */
/*           searched. */

/*           Begin a backwards search, and prepare to look for */
/*           individual segments from the file. */

/*           You can get to this block in two ways. */

/*           1) We can have a NEW INSTRUMENT. */

/*           2) We have checked the current list (CHECK LIST) for */
/*              this instrument, didn't find an applicable segment and */
/*              have some files left that have not been seached. */
	    findex = nft;
	    while(itlfs[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
		    s_rnge("itlfs", i__1, "ckbsr_", (ftnlen)3093)] <= ftnum[(
		    i__2 = findex - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge(
		    "ftnum", i__2, "ckbsr_", (ftnlen)3093)]) {
		--findex;
	    }
	    dafbbs_(&fthan[(i__1 = findex - 1) < 5000 && 0 <= i__1 ? i__1 : 
		    s_rnge("fthan", i__1, "ckbsr_", (ftnlen)3097)]);
	    if (failed_()) {
		chkout_("CKSNS", (ftnlen)5);
		return 0;
	    }
	    s_copy(status, "OLD SEGMENTS", (ftnlen)40, (ftnlen)12);

/*           The next thing we'll do is search through all the segments */
/*           of this file for those that applicable to this instrument. */
/*           The cost of the list contributed by the current file is */
/*           zero so far. */

	    cost = 0;

/*        Old files are searched in backward order. Segments, when */
/*        found, are inserted at the end of the list. Invisible */
/*        segments (initial time > final time) are ignored. */

/*        Each segment examined, whether applicable or not, adds to */
/*        the expense of the list. */

	} else if (s_cmp(status, "OLD SEGMENTS", (ftnlen)40, (ftnlen)12) == 0)
		 {

/*           There is only one way to get here---from the */
/*           block 'OLD FILES'.  Note we do not add to the */
/*           expense of the list for this instrument until we've */
/*           completely searched this file. */

	    daffpa_(&fnd);
	    if (failed_()) {
		chkout_("CKSNS", (ftnlen)5);
		return 0;
	    }
	    if (! fnd) {

/*              All of the segments in this file have been exhausted. */
/*              Change the lowest file searched indicator for this */
/*              instrument to be the current file, and go check the */
/*              current list. */

		itlfs[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itlfs", i__1, "ckbsr_", (ftnlen)3143)] = ftnum[(i__2 
			= findex - 1) < 5000 && 0 <= i__2 ? i__2 : s_rnge(
			"ftnum", i__2, "ckbsr_", (ftnlen)3143)];
		itexp[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itexp", i__1, "ckbsr_", (ftnlen)3144)] = itexp[(i__2 
			= iindex - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge(
			"itexp", i__2, "ckbsr_", (ftnlen)3144)] + cost;
		p = itbeg[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
			s_rnge("itbeg", i__1, "ckbsr_", (ftnlen)3145)];
		s_copy(status, "CHECK LIST", (ftnlen)40, (ftnlen)10);
	    } else {
		dafgs_(descr);
		dafus_(descr, &c__2, &c__6, dcd, icd);
		if (failed_()) {
		    chkout_("CKSNS", (ftnlen)5);
		    return 0;
		}
		if (icd[0] == scinst && dcd[0] <= dcd[1]) {
		    s_copy(doing, "OLD SEGMENTS", (ftnlen)40, (ftnlen)12);
		    s_copy(urgent, "ADD TO END", (ftnlen)40, (ftnlen)10);
		    s_copy(status, "SUSPEND", (ftnlen)40, (ftnlen)7);
		}
		++cost;
	    }
	} else if (s_cmp(status, "CHECK LIST", (ftnlen)40, (ftnlen)10) == 0) {

/*           Okay, all the new files (and maybe an old file or two) */
/*           have been searched. Time to look at the list of segments */
/*           stored for the instrument, to see if there is one applicable */
/*           to the specified epoch and need for angular velocity data. */

/*           If so, return it.  If not, try another old file.  If there */
/*           are no more old files, give up the ghost. */

/*           There are two ways to get to this point. */

/*           1) From the '?' block. */
/*           2) From the 'OLD SEGMENTS' block. */

/*           For every segment examined, adjust the re-use interval */
/*           associated with the current instrument. */

/*           P always points to the current segment in the list. Reject */
/*           a segment if there is a need for angular velocity data and */
/*           the segment doesn't have it. */

/*           If this is a new search, initialize the re-use interval. */
/*           If we're resuming a search, the re-use interval is invalid. */

	    if (newsch) {
		itlb[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itlb", i__1, "ckbsr_", (ftnlen)3200)] = dpmin_();
		itub[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itub", i__1, "ckbsr_", (ftnlen)3201)] = dpmax_();
	    }
	    while(p > 0) {
		if (newsch) {

/*                 Trim the re-use interval if the request time lies */
/*                 outside of the current segment. */

		    if (reqt > stdcd[(i__1 = (p << 1) - 1) < 200000 && 0 <= 
			    i__1 ? i__1 : s_rnge("stdcd", i__1, "ckbsr_", (
			    ftnlen)3212)]) {

/*                    REQT is to the right of the coverage interval of */
/*                    this segment.  Trim the re-use interval on the */
/*                    left, if necessary. */

/* Computing MAX */
			d__1 = itlb[(i__2 = iindex - 1) < 100 && 0 <= i__2 ? 
				i__2 : s_rnge("itlb", i__2, "ckbsr_", (ftnlen)
				3218)], d__2 = stdcd[(i__3 = (p << 1) - 1) < 
				200000 && 0 <= i__3 ? i__3 : s_rnge("stdcd", 
				i__3, "ckbsr_", (ftnlen)3218)];
			itlb[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
				s_rnge("itlb", i__1, "ckbsr_", (ftnlen)3218)] 
				= max(d__1,d__2);
		    } else if (reqt < stdcd[(i__1 = (p << 1) - 2) < 200000 && 
			    0 <= i__1 ? i__1 : s_rnge("stdcd", i__1, "ckbsr_",
			     (ftnlen)3221)]) {

/*                    REQT is to the left of the coverage interval of */
/*                    this segment.  Trim the re-use interval on the */
/*                    right, if necessary. */

/* Computing MIN */
			d__1 = itub[(i__2 = iindex - 1) < 100 && 0 <= i__2 ? 
				i__2 : s_rnge("itub", i__2, "ckbsr_", (ftnlen)
				3227)], d__2 = stdcd[(i__3 = (p << 1) - 2) < 
				200000 && 0 <= i__3 ? i__3 : s_rnge("stdcd", 
				i__3, "ckbsr_", (ftnlen)3227)];
			itub[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
				s_rnge("itub", i__1, "ckbsr_", (ftnlen)3227)] 
				= min(d__1,d__2);
		    }
		}
		if (omega >= stdcd[(i__1 = (p << 1) - 2) < 200000 && 0 <= 
			i__1 ? i__1 : s_rnge("stdcd", i__1, "ckbsr_", (ftnlen)
			3234)] && alpha <= stdcd[(i__2 = (p << 1) - 1) < 
			200000 && 0 <= i__2 ? i__2 : s_rnge("stdcd", i__2, 
			"ckbsr_", (ftnlen)3234)]) {

/*                 The segment coverage interval intersects the request */
/*                 interval ALPHA:OMEGA. */

		    if (! avneed || sticd[(i__1 = p * 6 - 3) < 600000 && 0 <= 
			    i__1 ? i__1 : s_rnge("sticd", i__1, "ckbsr_", (
			    ftnlen)3240)] != 0) {

/*                    This segment satisfies the request. */

			dafps_(&c__2, &c__6, &stdcd[(i__1 = (p << 1) - 2) < 
				200000 && 0 <= i__1 ? i__1 : s_rnge("stdcd", 
				i__1, "ckbsr_", (ftnlen)3244)], &sticd[(i__2 =
				 p * 6 - 6) < 600000 && 0 <= i__2 ? i__2 : 
				s_rnge("sticd", i__2, "ckbsr_", (ftnlen)3244)]
				, descr);
			s_copy(segid, stidnt + ((i__1 = p - 1) < 100000 && 0 
				<= i__1 ? i__1 : s_rnge("stidnt", i__1, "ckb"
				"sr_", (ftnlen)3247)) * 40, segid_len, (ftnlen)
				40);
			*handle = sthan[(i__1 = p - 1) < 100000 && 0 <= i__1 ?
				 i__1 : s_rnge("sthan", i__1, "ckbsr_", (
				ftnlen)3248)];
			*found = TRUE_;

/*                    If the segment actually contains the request */
/*                    time, and if this is a new search, set the */
/*                    re-use interval.  We require the request time */
/*                    to be in the interior of the interval:  it */
/*                    cannot be one of the endpoints. */

			if (newsch && reqt > stdcd[(i__1 = (p << 1) - 2) < 
				200000 && 0 <= i__1 ? i__1 : s_rnge("stdcd", 
				i__1, "ckbsr_", (ftnlen)3258)] && reqt < 
				stdcd[(i__2 = (p << 1) - 1) < 200000 && 0 <= 
				i__2 ? i__2 : s_rnge("stdcd", i__2, "ckbsr_", 
				(ftnlen)3258)]) {

/*                       Set the re-use interval for the current */
/*                       instrument. */

/* Computing MAX */
			    d__1 = itlb[(i__2 = iindex - 1) < 100 && 0 <= 
				    i__2 ? i__2 : s_rnge("itlb", i__2, "ckbs"
				    "r_", (ftnlen)3265)], d__2 = stdcd[(i__3 = 
				    (p << 1) - 2) < 200000 && 0 <= i__3 ? 
				    i__3 : s_rnge("stdcd", i__3, "ckbsr_", (
				    ftnlen)3265)];
			    itlb[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? 
				    i__1 : s_rnge("itlb", i__1, "ckbsr_", (
				    ftnlen)3265)] = max(d__1,d__2);
/* Computing MIN */
			    d__1 = itub[(i__2 = iindex - 1) < 100 && 0 <= 
				    i__2 ? i__2 : s_rnge("itub", i__2, "ckbs"
				    "r_", (ftnlen)3266)], d__2 = stdcd[(i__3 = 
				    (p << 1) - 1) < 200000 && 0 <= i__3 ? 
				    i__3 : s_rnge("stdcd", i__3, "ckbsr_", (
				    ftnlen)3266)];
			    itub[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? 
				    i__1 : s_rnge("itub", i__1, "ckbsr_", (
				    ftnlen)3266)] = min(d__1,d__2);

/*                       Save the returned output items, in case this */
/*                       segment may satisfy the next request. */

			    itprvh[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? 
				    i__1 : s_rnge("itprvh", i__1, "ckbsr_", (
				    ftnlen)3272)] = *handle;
			    s_copy(itprvi + ((i__1 = iindex - 1) < 100 && 0 <=
				     i__1 ? i__1 : s_rnge("itprvi", i__1, 
				    "ckbsr_", (ftnlen)3273)) * 40, segid, (
				    ftnlen)40, segid_len);
			    itprvf[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? 
				    i__1 : s_rnge("itprvf", i__1, "ckbsr_", (
				    ftnlen)3274)] = sticd[(i__2 = p * 6 - 3) <
				     600000 && 0 <= i__2 ? i__2 : s_rnge(
				    "sticd", i__2, "ckbsr_", (ftnlen)3274)];
			    moved_(descr, &c__5, &itprvd[(i__1 = iindex * 5 - 
				    5) < 500 && 0 <= i__1 ? i__1 : s_rnge(
				    "itprvd", i__1, "ckbsr_", (ftnlen)3276)]);
			    itchkp[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? 
				    i__1 : s_rnge("itchkp", i__1, "ckbsr_", (
				    ftnlen)3278)] = TRUE_;
			}

/*                    Go ahead and move the pointer up before returning */
/*                    so that the search for the next applicable segment */
/*                    will start at the right place. */

			savep = stpool[(i__1 = (p << 1) + 10) < 200012 && 0 <=
				 i__1 ? i__1 : s_rnge("stpool", i__1, "ckbsr_"
				, (ftnlen)3287)];

/*                    Indicate the first pass of this search has been */
/*                    completed. */

			newsch = FALSE_;
			chkout_("CKSNS", (ftnlen)5);
			return 0;
		    }
		}

/*              Get the next node.  We avoid LNKNXT here in order */
/*              to speed up the operation. */

		p = stpool[(i__1 = (p << 1) + 10) < 200012 && 0 <= i__1 ? 
			i__1 : s_rnge("stpool", i__1, "ckbsr_", (ftnlen)3305)]
			;
	    }

/*           If we're still here we didn't have information for this */
/*           instrument in the segment list. */

/*           If there are more files, search them. */
/*           Otherwise, things are hopeless, set the status that way. */

	    if (itlfs[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
		    "itlfs", i__1, "ckbsr_", (ftnlen)3316)] > ftnum[0]) {
		s_copy(status, "OLD FILES", (ftnlen)40, (ftnlen)9);
	    } else {
		s_copy(status, "HOPELESS", (ftnlen)40, (ftnlen)8);
	    }
	} else if (s_cmp(status, "MAKE ROOM", (ftnlen)40, (ftnlen)9) == 0) {

/*           When adding a new segment to a full table, one of the */
/*           current instruments must be dropped.  The ideal */
/*           candidate is the one whose list was constructed at the */
/*           lowest expense.  The candidate should be removed from */
/*           the instrument table, and its list transferred to the */
/*           segment table pool. */

/*           There is ``room'' if the segment table pool contains at */
/*           least one free node. */

/*           It is possible that a single instrument requires more */
/*           than the entire segment table for its own segments. */
/*           Two things might happen in such a case: */

/*              1) If the list under consideration was being added to at */
/*                 the end, then a search is continued without buffering */
/*                 any segments. */

/*              2) If the list was being added to at the beginning, then */
/*                 that means there was a NEW FILES search going on, and */
/*                 so a brand new list is constructed for the instrument, */
/*                 much as in a 'NEW INSTRUMENT' task. */

/*           There are two different ways to get to this point. */

/*              1) From 'ADD TO FRONT' if the segment table pool is full. */
/*              2) From 'ADD TO END' if the segment table pool is full. */

/*           Try to make room by deleting a segment list.  CHEAP will */
/*           be the index of the "cheapest" segment list in the */
/*           instrument table. */

	    minexp = intmax_();
	    cheap = 0;
	    i__1 = nit;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (i__ != iindex) {
		    if (itexp[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : 
			    s_rnge("itexp", i__2, "ckbsr_", (ftnlen)3365)] < 
			    minexp || cheap == 0) {

/*                    This list is the cheapest seen so far, */
/*                    possibly because it's the first one */
/*                    considered.  At the moment, it's as good */
/*                    a candidate for removal as any. */

			cheap = i__;
			minexp = itexp[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? 
				i__2 : s_rnge("itexp", i__2, "ckbsr_", (
				ftnlen)3374)];
		    }
		}
	    }
	    if (cheap == 0) {

/*              If there are no deleteable segments, the Thing To */
/*              Do depends on the task that was suspended before */
/*              entering MAKE ROOM. */

		if (s_cmp(stack + ((i__1 = top - 1) < 2 && 0 <= i__1 ? i__1 : 
			s_rnge("stack", i__1, "ckbsr_", (ftnlen)3389)) * 40, 
			"ADD TO END", (ftnlen)40, (ftnlen)10) == 0) {

/*                 The segment meta-data from the current file cannot */
/*                 be buffered.  We'll search the partial list of */
/*                 segments from this file, then proceed to search */
/*                 the rest of the file and any other old files, until */
/*                 we find an applicable segment or run out of segments. */

		    s_copy(status, "PREPARE PARTIAL LIST", (ftnlen)40, (
			    ftnlen)20);
		} else {

/*                 STACK(TOP) is set to 'ADD TO FRONT'. */

/*                 If there is no room left in the table in the middle */
/*                 of an attempt to add to the front of the list, just */
/*                 start from scratch by effectively initiating a 'NEW */
/*                 INSTRUMENT' task. */

/*                 Return the current list to the segment table pool. */
/*                 Note this list is non-empty. */

		    p = itbeg[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itbeg", i__1, "ckbsr_", (ftnlen)3412)];
		    tail = -lnkprv_(&p, stpool);
		    lnkfsl_(&p, &tail, stpool);

/*                 Re-initialize the table for this instrument, and */
/*                 initiate an 'OLD FILES' search, just as in 'NEW */
/*                 INSTRUMENT'. */

		    itbeg[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itbeg", i__1, "ckbsr_", (ftnlen)3421)] = 
			    0;
		    itexp[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itexp", i__1, "ckbsr_", (ftnlen)3422)] = 
			    0;
		    ithfs[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("ithfs", i__1, "ckbsr_", (ftnlen)3423)] = 
			    ftnum[(i__2 = nft - 1) < 5000 && 0 <= i__2 ? i__2 
			    : s_rnge("ftnum", i__2, "ckbsr_", (ftnlen)3423)];
		    itlfs[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itlfs", i__1, "ckbsr_", (ftnlen)3424)] = 
			    ftnum[(i__2 = nft - 1) < 5000 && 0 <= i__2 ? i__2 
			    : s_rnge("ftnum", i__2, "ckbsr_", (ftnlen)3424)] 
			    + 1;
		    s_copy(status, "OLD FILES", (ftnlen)40, (ftnlen)9);
		}

/*              Unwind the stack; we've set the target states already. */

		top = 0;
	    } else {

/*              Return this cheapest list to the segment pool.  This */
/*              list could be empty. */

		head = itbeg[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : 
			s_rnge("itbeg", i__1, "ckbsr_", (ftnlen)3440)];
		if (head > 0) {
		    tail = -lnkprv_(&head, stpool);
		    lnkfsl_(&head, &tail, stpool);
		}

/*              Fill the deleted instrument's space in the table with */
/*              the final entry in the table. */

		if (cheap != nit) {
		    itins[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itins", i__1, "ckbsr_", (ftnlen)3456)] = 
			    itins[(i__2 = nit - 1) < 100 && 0 <= i__2 ? i__2 :
			     s_rnge("itins", i__2, "ckbsr_", (ftnlen)3456)];
		    itexp[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itexp", i__1, "ckbsr_", (ftnlen)3457)] = 
			    itexp[(i__2 = nit - 1) < 100 && 0 <= i__2 ? i__2 :
			     s_rnge("itexp", i__2, "ckbsr_", (ftnlen)3457)];
		    ithfs[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("ithfs", i__1, "ckbsr_", (ftnlen)3458)] = 
			    ithfs[(i__2 = nit - 1) < 100 && 0 <= i__2 ? i__2 :
			     s_rnge("ithfs", i__2, "ckbsr_", (ftnlen)3458)];
		    itlfs[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itlfs", i__1, "ckbsr_", (ftnlen)3459)] = 
			    itlfs[(i__2 = nit - 1) < 100 && 0 <= i__2 ? i__2 :
			     s_rnge("itlfs", i__2, "ckbsr_", (ftnlen)3459)];
		    itbeg[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itbeg", i__1, "ckbsr_", (ftnlen)3460)] = 
			    itbeg[(i__2 = nit - 1) < 100 && 0 <= i__2 ? i__2 :
			     s_rnge("itbeg", i__2, "ckbsr_", (ftnlen)3460)];
		    itlb[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itlb", i__1, "ckbsr_", (ftnlen)3461)] = 
			    itlb[(i__2 = nit - 1) < 100 && 0 <= i__2 ? i__2 : 
			    s_rnge("itlb", i__2, "ckbsr_", (ftnlen)3461)];
		    itub[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itub", i__1, "ckbsr_", (ftnlen)3462)] = 
			    itub[(i__2 = nit - 1) < 100 && 0 <= i__2 ? i__2 : 
			    s_rnge("itub", i__2, "ckbsr_", (ftnlen)3462)];
		    itprvh[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itprvh", i__1, "ckbsr_", (ftnlen)3463)] = 
			    itprvh[(i__2 = nit - 1) < 100 && 0 <= i__2 ? i__2 
			    : s_rnge("itprvh", i__2, "ckbsr_", (ftnlen)3463)];
		    s_copy(itprvi + ((i__1 = cheap - 1) < 100 && 0 <= i__1 ? 
			    i__1 : s_rnge("itprvi", i__1, "ckbsr_", (ftnlen)
			    3464)) * 40, itprvi + ((i__2 = nit - 1) < 100 && 
			    0 <= i__2 ? i__2 : s_rnge("itprvi", i__2, "ckbsr_"
			    , (ftnlen)3464)) * 40, (ftnlen)40, (ftnlen)40);
		    itprvf[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itprvf", i__1, "ckbsr_", (ftnlen)3465)] = 
			    itprvf[(i__2 = nit - 1) < 100 && 0 <= i__2 ? i__2 
			    : s_rnge("itprvf", i__2, "ckbsr_", (ftnlen)3465)];
		    itchkp[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itchkp", i__1, "ckbsr_", (ftnlen)3466)] = 
			    itchkp[(i__2 = nit - 1) < 100 && 0 <= i__2 ? i__2 
			    : s_rnge("itchkp", i__2, "ckbsr_", (ftnlen)3466)];
		    itruex[(i__1 = cheap - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itruex", i__1, "ckbsr_", (ftnlen)3467)] = 
			    itruex[(i__2 = nit - 1) < 100 && 0 <= i__2 ? i__2 
			    : s_rnge("itruex", i__2, "ckbsr_", (ftnlen)3467)];
		    moved_(&itprvd[(i__1 = nit * 5 - 5) < 500 && 0 <= i__1 ? 
			    i__1 : s_rnge("itprvd", i__1, "ckbsr_", (ftnlen)
			    3469)], &c__5, &itprvd[(i__2 = cheap * 5 - 5) < 
			    500 && 0 <= i__2 ? i__2 : s_rnge("itprvd", i__2, 
			    "ckbsr_", (ftnlen)3469)]);
		}
		if (iindex == nit) {
		    iindex = cheap;
		}

/*              One less instrument now. */

		--nit;
		s_copy(status, "RESUME", (ftnlen)40, (ftnlen)6);
	    }

/*           Either we made room by freeing a non-empty segment list, */
/*           or we're going to work without additional space.  In the */
/*           latter case, the state is now 'OLD FILES' or */
/*           'PREPARE PARTIAL LIST'. */

	} else if (s_cmp(status, "ADD TO FRONT", (ftnlen)40, (ftnlen)12) == 0)
		 {

/*           The current segment information should be linked in at */
/*           the head of the segment list for the current instrument, */
/*           and the pertinent instrument table entry should point */
/*           to the new head of the list. */

/*           The only way to get here is from the block NEW SEGMENTS */
/*           after suspending that task. */
	    if (lnknfn_(stpool) == 0) {
		s_copy(doing, "ADD TO FRONT", (ftnlen)40, (ftnlen)12);
		s_copy(urgent, "MAKE ROOM", (ftnlen)40, (ftnlen)9);
		s_copy(status, "SUSPEND", (ftnlen)40, (ftnlen)7);
	    } else {

/*              Allocate a node and link it to the front of the list */
/*              for the current instrument. */

		lnkan_(stpool, &new__);
		sthan[(i__1 = new__ - 1) < 100000 && 0 <= i__1 ? i__1 : 
			s_rnge("sthan", i__1, "ckbsr_", (ftnlen)3515)] = 
			fthan[(i__2 = findex - 1) < 5000 && 0 <= i__2 ? i__2 :
			 s_rnge("fthan", i__2, "ckbsr_", (ftnlen)3515)];
		dafgn_(stidnt + ((i__1 = new__ - 1) < 100000 && 0 <= i__1 ? 
			i__1 : s_rnge("stidnt", i__1, "ckbsr_", (ftnlen)3517))
			 * 40, (ftnlen)40);
		dafus_(descr, &c__2, &c__6, &stdcd[(i__1 = (new__ << 1) - 2) <
			 200000 && 0 <= i__1 ? i__1 : s_rnge("stdcd", i__1, 
			"ckbsr_", (ftnlen)3519)], &sticd[(i__2 = new__ * 6 - 
			6) < 600000 && 0 <= i__2 ? i__2 : s_rnge("sticd", 
			i__2, "ckbsr_", (ftnlen)3519)]);
		if (failed_()) {
		    chkout_("CKSNS", (ftnlen)5);
		    return 0;
		}

/*              If the current list is empty, this append operation */
/*              is a no-op. */

		lnkilb_(&new__, &itbeg[(i__1 = iindex - 1) < 100 && 0 <= i__1 
			? i__1 : s_rnge("itbeg", i__1, "ckbsr_", (ftnlen)3530)
			], stpool);
		itbeg[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"itbeg", i__1, "ckbsr_", (ftnlen)3531)] = new__;
		s_copy(status, "RESUME", (ftnlen)40, (ftnlen)6);
	    }
	} else if (s_cmp(status, "ADD TO END", (ftnlen)40, (ftnlen)10) == 0) {

/*           The current segment information should be linked in at */
/*           the tail of the segment list for the current instrument. */

/*           The only way to get to this task is from the OLD SEGMENTS */
/*           block after suspending that task. */

	    if (lnknfn_(stpool) == 0) {
		s_copy(doing, "ADD TO END", (ftnlen)40, (ftnlen)10);
		s_copy(urgent, "MAKE ROOM", (ftnlen)40, (ftnlen)9);
		s_copy(status, "SUSPEND", (ftnlen)40, (ftnlen)7);
	    } else {

/*              Allocate a new node in the segment table pool. */

		lnkan_(stpool, &new__);
		sthan[(i__1 = new__ - 1) < 100000 && 0 <= i__1 ? i__1 : 
			s_rnge("sthan", i__1, "ckbsr_", (ftnlen)3558)] = 
			fthan[(i__2 = findex - 1) < 5000 && 0 <= i__2 ? i__2 :
			 s_rnge("fthan", i__2, "ckbsr_", (ftnlen)3558)];
		dafgn_(stidnt + ((i__1 = new__ - 1) < 100000 && 0 <= i__1 ? 
			i__1 : s_rnge("stidnt", i__1, "ckbsr_", (ftnlen)3560))
			 * 40, (ftnlen)40);
		dafus_(descr, &c__2, &c__6, &stdcd[(i__1 = (new__ << 1) - 2) <
			 200000 && 0 <= i__1 ? i__1 : s_rnge("stdcd", i__1, 
			"ckbsr_", (ftnlen)3562)], &sticd[(i__2 = new__ * 6 - 
			6) < 600000 && 0 <= i__2 ? i__2 : s_rnge("sticd", 
			i__2, "ckbsr_", (ftnlen)3562)]);
		if (failed_()) {
		    chkout_("CKSNS", (ftnlen)5);
		    return 0;
		}
		if (itbeg[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
			s_rnge("itbeg", i__1, "ckbsr_", (ftnlen)3569)] <= 0) {

/*                 This is the first node in the list for this */
/*                 instrument. */

		    itbeg[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itbeg", i__1, "ckbsr_", (ftnlen)3574)] = 
			    new__;
		} else {

/*                 Link the new node to the tail of the list. */

		    tail = -lnkprv_(&itbeg[(i__1 = iindex - 1) < 100 && 0 <= 
			    i__1 ? i__1 : s_rnge("itbeg", i__1, "ckbsr_", (
			    ftnlen)3580)], stpool);
		    lnkila_(&tail, &new__, stpool);
		}
		s_copy(status, "RESUME", (ftnlen)40, (ftnlen)6);
	    }
	} else if (s_cmp(status, "PREPARE PARTIAL LIST", (ftnlen)40, (ftnlen)
		20) == 0) {

/*           When the segment table is completely full, continue */
/*           the search by looking through the unchecked portion */
/*           of the segment list for the current instrument, and */
/*           then searching old, unchecked files without buffering */
/*           their segments. */

/*           The only way to get here is from the MAKE ROOM state */
/*           via the block ADD TO END.  If you get here there is no */
/*           free space in the segment table pool. */

/*           At this point, we need to initialize the cost of */
/*           the re-use interval. */

	    itruex[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
		    "itruex", i__1, "ckbsr_", (ftnlen)3605)] = 0;

/*           Find the portion of the current instrument's segment list */
/*           which comes from the current file of interest.  SLBEG */
/*           will point to the beginning of this sublist. */

	    slbeg = itbeg[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
		    s_rnge("itbeg", i__1, "ckbsr_", (ftnlen)3612)];
	    fndhan = FALSE_;
	    while(! fndhan && slbeg > 0) {
		fndhan = sthan[(i__1 = slbeg - 1) < 100000 && 0 <= i__1 ? 
			i__1 : s_rnge("sthan", i__1, "ckbsr_", (ftnlen)3617)] 
			== fthan[(i__2 = findex - 1) < 5000 && 0 <= i__2 ? 
			i__2 : s_rnge("fthan", i__2, "ckbsr_", (ftnlen)3617)];
		if (! fndhan) {

/*                 Get the next node.  We avoid LNKNXT here in order */
/*                 to speed up the operation. */

		    slbeg = stpool[(i__1 = (slbeg << 1) + 10) < 200012 && 0 <=
			     i__1 ? i__1 : s_rnge("stpool", i__1, "ckbsr_", (
			    ftnlen)3624)];
		}
	    }

/*           If the list contains segments from the current file, */
/*           check that portion of the list. */

/*           Otherwise, finish searching old files without buffering */
/*           anything. */

	    if (slbeg > 0) {

/*              The partial list from the current node onwards is to be */
/*              returned to the free list.  Save this node, since */
/*              we'll finish searching the list before freeing the */
/*              partial list. */

		p = slbeg;

/*              Record the fact that we'll need to free the partial list */
/*              later. */

		fresub = TRUE_;

/*              It may be that the partial list we're going to delete is */
/*              the entire segment list for this instrument.  If so, the */
/*              corresponding instrument table entry should be set to */
/*              a non-positive value to indicate an empty segment list. */

		if (p == itbeg[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 :
			 s_rnge("itbeg", i__1, "ckbsr_", (ftnlen)3658)]) {
		    itbeg[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itbeg", i__1, "ckbsr_", (ftnlen)3660)] = 
			    0;

/*                 Also in this case, we must initialize the time */
/*                 bounds for this instrument. */

		    itlb[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itlb", i__1, "ckbsr_", (ftnlen)3666)] = 
			    dpmin_();
		    itub[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itub", i__1, "ckbsr_", (ftnlen)3667)] = 
			    dpmax_();
		}
		s_copy(status, "CHECK PARTIAL LIST", (ftnlen)40, (ftnlen)18);
	    } else {
		s_copy(status, "SEARCH W/O BUFF", (ftnlen)40, (ftnlen)15);
	    }
	} else if (s_cmp(status, "CHECK PARTIAL LIST", (ftnlen)40, (ftnlen)18)
		 == 0) {

/*           The only ways to get here are from the */
/*           'PREPARE PARTIAL LIST' state, or by resuming a search of */
/*           the partial list. */

/*           The portion of the segment list from the current file */
/*           is to be checked. */

/*           BEG points to the current segment in the temporary portion */
/*           of the list. */

/*           Reject a segment if there is a need for angular velocity */
/*           data and the segment doesn't have it. */

	    while(p > 0) {

/*              If this is a new search, update the re-use interval */
/*              and its expense. */

		if (newsch) {

/*                 Every segment seen from the current file contributes */
/*                 to the expense of the re-use interval. */

		    itruex[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("itruex", i__1, "ckbsr_", (ftnlen)3706)] = 
			    itruex[(i__2 = iindex - 1) < 100 && 0 <= i__2 ? 
			    i__2 : s_rnge("itruex", i__2, "ckbsr_", (ftnlen)
			    3706)] + 1;

/*                 Trim the re-use interval if the request time lies */
/*                 outside the coverage of the current segment. */

		    if (reqt > stdcd[(i__1 = (p << 1) - 1) < 200000 && 0 <= 
			    i__1 ? i__1 : s_rnge("stdcd", i__1, "ckbsr_", (
			    ftnlen)3712)]) {

/*                    REQT is to the right of the coverage interval of */
/*                    this segment.  Trim the re-use interval on the */
/*                    left, if necessary. */

/* Computing MAX */
			d__1 = itlb[(i__2 = iindex - 1) < 100 && 0 <= i__2 ? 
				i__2 : s_rnge("itlb", i__2, "ckbsr_", (ftnlen)
				3718)], d__2 = stdcd[(i__3 = (p << 1) - 1) < 
				200000 && 0 <= i__3 ? i__3 : s_rnge("stdcd", 
				i__3, "ckbsr_", (ftnlen)3718)];
			itlb[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
				s_rnge("itlb", i__1, "ckbsr_", (ftnlen)3718)] 
				= max(d__1,d__2);
		    } else if (reqt < stdcd[(i__1 = (p << 1) - 2) < 200000 && 
			    0 <= i__1 ? i__1 : s_rnge("stdcd", i__1, "ckbsr_",
			     (ftnlen)3721)]) {

/*                    REQT is to the left of the coverage interval of */
/*                    this segment.  Trim the re-use interval on the */
/*                    right, if necessary. */

/* Computing MIN */
			d__1 = itub[(i__2 = iindex - 1) < 100 && 0 <= i__2 ? 
				i__2 : s_rnge("itub", i__2, "ckbsr_", (ftnlen)
				3727)], d__2 = stdcd[(i__3 = (p << 1) - 2) < 
				200000 && 0 <= i__3 ? i__3 : s_rnge("stdcd", 
				i__3, "ckbsr_", (ftnlen)3727)];
			itub[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : 
				s_rnge("itub", i__1, "ckbsr_", (ftnlen)3727)] 
				= min(d__1,d__2);
		    }
		}

/*              We've updated the re-use interval if so required. */

		if (omega >= stdcd[(i__1 = (p << 1) - 2) < 200000 && 0 <= 
			i__1 ? i__1 : s_rnge("stdcd", i__1, "ckbsr_", (ftnlen)
			3736)] && alpha <= stdcd[(i__2 = (p << 1) - 1) < 
			200000 && 0 <= i__2 ? i__2 : s_rnge("stdcd", i__2, 
			"ckbsr_", (ftnlen)3736)]) {

/*                 The segment coverage interval intersects the request */
/*                 interval ALPHA:OMEGA. */

		    if (! avneed || sticd[(i__1 = p * 6 - 3) < 600000 && 0 <= 
			    i__1 ? i__1 : s_rnge("sticd", i__1, "ckbsr_", (
			    ftnlen)3742)] != 0) {

/*                    This segment satisfies the request.  Set the */
/*                    output arguments. */

			dafps_(&c__2, &c__6, &stdcd[(i__1 = (p << 1) - 2) < 
				200000 && 0 <= i__1 ? i__1 : s_rnge("stdcd", 
				i__1, "ckbsr_", (ftnlen)3747)], &sticd[(i__2 =
				 p * 6 - 6) < 600000 && 0 <= i__2 ? i__2 : 
				s_rnge("sticd", i__2, "ckbsr_", (ftnlen)3747)]
				, descr);
			s_copy(segid, stidnt + ((i__1 = p - 1) < 100000 && 0 
				<= i__1 ? i__1 : s_rnge("stidnt", i__1, "ckb"
				"sr_", (ftnlen)3750)) * 40, segid_len, (ftnlen)
				40);
			*handle = sthan[(i__1 = p - 1) < 100000 && 0 <= i__1 ?
				 i__1 : s_rnge("sthan", i__1, "ckbsr_", (
				ftnlen)3751)];
			*found = TRUE_;

/*                    If this is the first pass performed for the */
/*                    current search, then we can set the re-use */
/*                    interval.  The re-use interval becomes invalid */
/*                    after the first pass. */

/*                    If the segment actually contains the request */
/*                    time, set the re-use interval.  We require */
/*                    the request time to be in the interior of the */
/*                    interval:  it cannot be one of the endpoints. */

			if (newsch && reqt > stdcd[(i__1 = (p << 1) - 2) < 
				200000 && 0 <= i__1 ? i__1 : s_rnge("stdcd", 
				i__1, "ckbsr_", (ftnlen)3765)] && reqt < 
				stdcd[(i__2 = (p << 1) - 1) < 200000 && 0 <= 
				i__2 ? i__2 : s_rnge("stdcd", i__2, "ckbsr_", 
				(ftnlen)3765)]) {

/*                       Adjust the re-use interval for the current */
/*                       instrument. */

/* Computing MAX */
			    d__1 = itlb[(i__2 = iindex - 1) < 100 && 0 <= 
				    i__2 ? i__2 : s_rnge("itlb", i__2, "ckbs"
				    "r_", (ftnlen)3772)], d__2 = stdcd[(i__3 = 
				    (p << 1) - 2) < 200000 && 0 <= i__3 ? 
				    i__3 : s_rnge("stdcd", i__3, "ckbsr_", (
				    ftnlen)3772)];
			    itlb[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? 
				    i__1 : s_rnge("itlb", i__1, "ckbsr_", (
				    ftnlen)3772)] = max(d__1,d__2);
/* Computing MIN */
			    d__1 = itub[(i__2 = iindex - 1) < 100 && 0 <= 
				    i__2 ? i__2 : s_rnge("itub", i__2, "ckbs"
				    "r_", (ftnlen)3773)], d__2 = stdcd[(i__3 = 
				    (p << 1) - 1) < 200000 && 0 <= i__3 ? 
				    i__3 : s_rnge("stdcd", i__3, "ckbsr_", (
				    ftnlen)3773)];
			    itub[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? 
				    i__1 : s_rnge("itub", i__1, "ckbsr_", (
				    ftnlen)3773)] = min(d__1,d__2);

/*                       Save the returned output items, in case this */
/*                       segment may satisfy the next request. */

			    itprvh[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? 
				    i__1 : s_rnge("itprvh", i__1, "ckbsr_", (
				    ftnlen)3778)] = *handle;
			    s_copy(itprvi + ((i__1 = iindex - 1) < 100 && 0 <=
				     i__1 ? i__1 : s_rnge("itprvi", i__1, 
				    "ckbsr_", (ftnlen)3779)) * 40, segid, (
				    ftnlen)40, segid_len);
			    itprvf[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? 
				    i__1 : s_rnge("itprvf", i__1, "ckbsr_", (
				    ftnlen)3780)] = sticd[(i__2 = p * 6 - 3) <
				     600000 && 0 <= i__2 ? i__2 : s_rnge(
				    "sticd", i__2, "ckbsr_", (ftnlen)3780)];
			    moved_(descr, &c__5, &itprvd[(i__1 = iindex * 5 - 
				    5) < 500 && 0 <= i__1 ? i__1 : s_rnge(
				    "itprvd", i__1, "ckbsr_", (ftnlen)3782)]);
			    itchkp[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? 
				    i__1 : s_rnge("itchkp", i__1, "ckbsr_", (
				    ftnlen)3784)] = TRUE_;

/*                       Update the expense of the list to reflect */
/*                       the cost of locating this segment. */

			    itexp[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? 
				    i__1 : s_rnge("itexp", i__1, "ckbsr_", (
				    ftnlen)3789)] = itexp[(i__2 = iindex - 1) 
				    < 100 && 0 <= i__2 ? i__2 : s_rnge("itexp"
				    , i__2, "ckbsr_", (ftnlen)3789)] + itruex[
				    (i__3 = iindex - 1) < 100 && 0 <= i__3 ? 
				    i__3 : s_rnge("itruex", i__3, "ckbsr_", (
				    ftnlen)3789)];
			}

/*                    We've set the re-use interval. */

/*                    Go ahead and move the pointer up before returning */
/*                    so that the search for the next applicable segment */
/*                    will start at the right place. */

/*                    We avoid LNKNXT here in order to speed up the */
/*                    operation. */

			savep = stpool[(i__1 = (p << 1) + 10) < 200012 && 0 <=
				 i__1 ? i__1 : s_rnge("stpool", i__1, "ckbsr_"
				, (ftnlen)3802)];

/*                    We cannot free the partial list yet, because */
/*                    we may return to search it again if the current */
/*                    segment doesn't have pointing that satisfies */
/*                    the caller's request.  The list will be freed */
/*                    at the start of the next search if it's not */
/*                    freed at the end of this block or in the */
/*                    'SEARCH W/O BUFFERING' block. */

/*                    Indicate the first pass of this search has been */
/*                    completed. */

			newsch = FALSE_;
			chkout_("CKSNS", (ftnlen)5);
			return 0;
		    }

/*                 Getting here implies angular velocity was */
/*                 requested but was not present in the segment. */

		}

/*              The current segment didn't match.  Look at the next */
/*              segment in the list. */

		p = stpool[(i__1 = (p << 1) + 10) < 200012 && 0 <= i__1 ? 
			i__1 : s_rnge("stpool", i__1, "ckbsr_", (ftnlen)3831)]
			;
	    }

/*           We're done looking at the partial list. */

/*           Return the partial list to the segment table pool. */
/*           P at this point is the negative of the list head. */
/*           The list tail is (by the spec of the SPICELIB doubly */
/*           linked list routines) the negative of the predecessor */
/*           of the head. */

/*           Note the list is always non-empty at this point. */

	    i__1 = -p;
	    tail = -lnkprv_(&i__1, stpool);
	    lnkfsl_(&slbeg, &tail, stpool);
	    fresub = FALSE_;

/*           Search the remaining files. */

	    s_copy(status, "SEARCH W/O BUFF", (ftnlen)40, (ftnlen)15);
	} else if (s_cmp(status, "SEARCH W/O BUFF", (ftnlen)40, (ftnlen)15) ==
		 0) {

/*           The only ways to get here are from the */
/*           'PREPARE PARTIAL LIST' and 'CHECK PARTIAL LIST' states. */

/*           When the segment table is full with the current instrument's */
/*           segments and any freed up portions have been checked, */
/*           continue the search for applicable segments in old files, */
/*           without buffering any of the segments in the segment table. */

/*           Recall that a search is already in progress and that a */
/*           segment is currently under consideration (FND = .TRUE.). */

	    while(findex > 0) {
		while(fnd) {
		    if (newsch) {

/*                    Each segment found contributes to the expense of */
/*                    the re-use interval. */

			itruex[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 :
				 s_rnge("itruex", i__1, "ckbsr_", (ftnlen)
				3879)] = itruex[(i__2 = iindex - 1) < 100 && 
				0 <= i__2 ? i__2 : s_rnge("itruex", i__2, 
				"ckbsr_", (ftnlen)3879)] + 1;
		    }
		    dafgs_(descr);
		    dafus_(descr, &c__2, &c__6, dcd, icd);
		    if (failed_()) {
			chkout_("CKSNS", (ftnlen)5);
			return 0;
		    }
		    if (scinst == icd[0]) {

/*                    This is a segment for the instrument of interest. */
			if (newsch) {

/*                       Update the re-use interval for this instrument. */

			    if (reqt > dcd[1]) {

/*                          REQT is to the right of the coverage interval */
/*                          of this segment.  Trim the re-use interval */
/*                          on the left, if necessary. */

/* Computing MAX */
				d__1 = itlb[(i__2 = iindex - 1) < 100 && 0 <= 
					i__2 ? i__2 : s_rnge("itlb", i__2, 
					"ckbsr_", (ftnlen)3906)];
				itlb[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? 
					i__1 : s_rnge("itlb", i__1, "ckbsr_", 
					(ftnlen)3906)] = max(d__1,dcd[1]);
			    } else if (reqt < dcd[0]) {

/*                          REQT is to the left of the coverage interval */
/*                          of this segment.  Trim the re-use interval */
/*                          on the right, if necessary. */

/* Computing MIN */
				d__1 = itub[(i__2 = iindex - 1) < 100 && 0 <= 
					i__2 ? i__2 : s_rnge("itub", i__2, 
					"ckbsr_", (ftnlen)3915)];
				itub[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? 
					i__1 : s_rnge("itub", i__1, "ckbsr_", 
					(ftnlen)3915)] = min(d__1,dcd[0]);
			    }
			}

/*                    We've trimmed the re-use interval if necessary. */

			if (omega >= dcd[0] && alpha <= dcd[1]) {

/*                       The segment coverage interval intersects the */
/*                       request interval ALPHA:OMEGA. */

			    if (! avneed || icd[3] != 0) {

/*                          This segment satisfies the request.  Set */
/*                          the output arguments. */

				dafps_(&c__2, &c__6, dcd, icd, descr);
				dafgn_(segid, segid_len);
				*handle = fthan[(i__1 = findex - 1) < 5000 && 
					0 <= i__1 ? i__1 : s_rnge("fthan", 
					i__1, "ckbsr_", (ftnlen)3939)];
				*found = TRUE_;
				if (newsch) {

/*                             Adjust the re-use interval for the current */
/*                             instrument. */

/* Computing MAX */
				    d__1 = itlb[(i__2 = iindex - 1) < 100 && 
					    0 <= i__2 ? i__2 : s_rnge("itlb", 
					    i__2, "ckbsr_", (ftnlen)3947)];
				    itlb[(i__1 = iindex - 1) < 100 && 0 <= 
					    i__1 ? i__1 : s_rnge("itlb", i__1,
					     "ckbsr_", (ftnlen)3947)] = max(
					    d__1,dcd[0]);
/* Computing MIN */
				    d__1 = itub[(i__2 = iindex - 1) < 100 && 
					    0 <= i__2 ? i__2 : s_rnge("itub", 
					    i__2, "ckbsr_", (ftnlen)3948)];
				    itub[(i__1 = iindex - 1) < 100 && 0 <= 
					    i__1 ? i__1 : s_rnge("itub", i__1,
					     "ckbsr_", (ftnlen)3948)] = min(
					    d__1,dcd[1]);

/*                             Save the returned output items, in case */
/*                             this segment may satisfy the next request. */

				    itprvh[(i__1 = iindex - 1) < 100 && 0 <= 
					    i__1 ? i__1 : s_rnge("itprvh", 
					    i__1, "ckbsr_", (ftnlen)3954)] = *
					    handle;
				    s_copy(itprvi + ((i__1 = iindex - 1) < 
					    100 && 0 <= i__1 ? i__1 : s_rnge(
					    "itprvi", i__1, "ckbsr_", (ftnlen)
					    3955)) * 40, segid, (ftnlen)40, 
					    segid_len);
				    itprvf[(i__1 = iindex - 1) < 100 && 0 <= 
					    i__1 ? i__1 : s_rnge("itprvf", 
					    i__1, "ckbsr_", (ftnlen)3956)] = 
					    icd[3];
				    moved_(descr, &c__5, &itprvd[(i__1 = 
					    iindex * 5 - 5) < 500 && 0 <= 
					    i__1 ? i__1 : s_rnge("itprvd", 
					    i__1, "ckbsr_", (ftnlen)3958)]);
				    itchkp[(i__1 = iindex - 1) < 100 && 0 <= 
					    i__1 ? i__1 : s_rnge("itchkp", 
					    i__1, "ckbsr_", (ftnlen)3961)] = 
					    TRUE_;

/*                             Update the expense of the list to reflect */
/*                             cost of locating this segment. */

				    itexp[(i__1 = iindex - 1) < 100 && 0 <= 
					    i__1 ? i__1 : s_rnge("itexp", 
					    i__1, "ckbsr_", (ftnlen)3967)] = 
					    itexp[(i__2 = iindex - 1) < 100 &&
					     0 <= i__2 ? i__2 : s_rnge("itexp"
					    , i__2, "ckbsr_", (ftnlen)3967)] 
					    + itruex[(i__3 = iindex - 1) < 
					    100 && 0 <= i__3 ? i__3 : s_rnge(
					    "itruex", i__3, "ckbsr_", (ftnlen)
					    3967)];
				}

/*                          The re-use interval is set. */

/*                          Go ahead and point to the next segment in the */
/*                          file in case an attempt is made to continue */
/*                          the search: you want to pick up exactly where */
/*                          you  left off. */

				daffpa_(&fnd);

/*                          Indicate the first pass of this search has */
/*                          been completed. */

				newsch = FALSE_;
				chkout_("CKSNS", (ftnlen)5);
				return 0;
			    }

/*                       Getting here implies angular velocity was */
/*                       requested but was not present in the segment. */

			}

/*                    The current segment's coverage didn't intersect */
/*                    the request interval. */

		    }

/*                 The current segment didn't contain data for the */
/*                 specified instrument. */

/*                 Look at the next segment in the current file. */

		    daffpa_(&fnd);
		}

/*              Try the next oldest file. */

		--findex;
		if (findex > 0) {
		    dafbbs_(&fthan[(i__1 = findex - 1) < 5000 && 0 <= i__1 ? 
			    i__1 : s_rnge("fthan", i__1, "ckbsr_", (ftnlen)
			    4016)]);
		    daffpa_(&fnd);
		}
	    }

/*           There's nothing nowhere if you get to here. */

	    itruex[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
		    "itruex", i__1, "ckbsr_", (ftnlen)4026)] = 0;
	    s_copy(status, "HOPELESS", (ftnlen)40, (ftnlen)8);
	} else if (s_cmp(status, "SUSPEND", (ftnlen)40, (ftnlen)7) == 0) {

/*           When a task is suspended, the current activity is placed on */
/*           a stack, to be restored later. Two levels are provided, */
/*           since some interrupts can be interrupted by others. */

	    ++top;
	    s_copy(stack + ((i__1 = top - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge(
		    "stack", i__1, "ckbsr_", (ftnlen)4037)) * 40, doing, (
		    ftnlen)40, (ftnlen)40);
	    s_copy(status, urgent, (ftnlen)40, (ftnlen)40);
	} else if (s_cmp(status, "RESUME", (ftnlen)40, (ftnlen)6) == 0) {
	    s_copy(status, stack + ((i__1 = top - 1) < 2 && 0 <= i__1 ? i__1 :
		     s_rnge("stack", i__1, "ckbsr_", (ftnlen)4042)) * 40, (
		    ftnlen)40, (ftnlen)40);
	    --top;
	}
    }

/*     Can only get here if status is 'HOPELESS', in which case a */
/*     segment was not found. */

    *found = FALSE_;

/*     If we didn't find a segment, don't attempt to use saved */
/*     outputs from a previous call.  IINDEX will always be set */
/*     at this point.  Also, make sure the expense of the re-use */
/*     interval is zeroed out. */

    if (iindex > 0) {
	itchkp[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge("itchkp"
		, i__1, "ckbsr_", (ftnlen)4063)] = FALSE_;
	itruex[(i__1 = iindex - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge("itruex"
		, i__1, "ckbsr_", (ftnlen)4064)] = 0;
    }

/*     For safety, indicate the first pass of this search has been */
/*     completed.  Normally, we won't return here before CKBSS is */
/*     called again, but it's possible. */

    newsch = FALSE_;
    chkout_("CKSNS", (ftnlen)5);
    return 0;
/* $Procedure      CKHAVE ( C-kernels --- Have some ) */

L_ckhave:
/* $ Abstract */

/*     Determine whether or not any C-kernels are currently loaded. */

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

/*     CK */
/*     DAF */

/* $ Keywords */

/*     C-KERNEL */

/* $ Declarations */

/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FOUND      O   TRUE if at least one C-kernel is loaded. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     FOUND      is returned with the value TRUE if at least one */
/*                C-kernel is currently loaded.  Otherwise it returns */
/*                the value FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This entry point allows the user to query the set of "loaded" */
/*     C-kernels to make sure that at least one C-kernel has been loaded. */
/*     This allows you to avoid making a search of an empty set of */
/*     loaded kernels which forces a SPICELIB error to be signaled. */

/* $ Examples */

/*     Suppose you want to call on of the C-kernel readers, but wish */
/*     to handle the exceptional case of "no kernels loaded" so that */
/*     the SPICELIB exception handling mechanism is avoided in the */
/*     case of an empty set of loaded kernels.  The code fragment */
/*     below shows how you might do this: */

/*        CALL CKHAVE ( LOADED ) */

/*        IF ( LOADED ) THEN */

/*           CALL CKGP ( ... ) */

/*        ELSE */

/*           take some kind of "reasonable action" */

/*        END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.0.2, 28-FEB-2008 (BVS) */

/*        Corrected the contents of the Required_Reading section. */

/* -    SPICELIB Version 4.0.1, 31-OCT-2001 (NJB) */

/*        Typo corrected. */

/* -    SPICELIB Version 4.0.0, 17-FEB-2000 (WLT) */

/*        Added the Entry point CKHAVE */

/* -    SPICELIB Version 3.0.0, 03-MAR-1999 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Determine whether any C-kernels are loaded */

/* -& */
    *found = nft > 0;
    return 0;
} /* ckbsr_ */

/* Subroutine */ int ckbsr_(char *fname, integer *handle, integer *inst, 
	doublereal *sclkdp, doublereal *tol, logical *needav, doublereal *
	descr, char *segid, logical *found, ftnlen fname_len, ftnlen 
	segid_len)
{
    return ckbsr_0_(0, fname, handle, inst, sclkdp, tol, needav, descr, segid,
	     found, fname_len, segid_len);
    }

/* Subroutine */ int cklpf_(char *fname, integer *handle, ftnlen fname_len)
{
    return ckbsr_0_(1, fname, handle, (integer *)0, (doublereal *)0, (
	    doublereal *)0, (logical *)0, (doublereal *)0, (char *)0, (
	    logical *)0, fname_len, (ftnint)0);
    }

/* Subroutine */ int ckupf_(integer *handle)
{
    return ckbsr_0_(2, (char *)0, handle, (integer *)0, (doublereal *)0, (
	    doublereal *)0, (logical *)0, (doublereal *)0, (char *)0, (
	    logical *)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int ckbss_(integer *inst, doublereal *sclkdp, doublereal *
	tol, logical *needav)
{
    return ckbsr_0_(3, (char *)0, (integer *)0, inst, sclkdp, tol, needav, (
	    doublereal *)0, (char *)0, (logical *)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int cksns_(integer *handle, doublereal *descr, char *segid, 
	logical *found, ftnlen segid_len)
{
    return ckbsr_0_(4, (char *)0, handle, (integer *)0, (doublereal *)0, (
	    doublereal *)0, (logical *)0, descr, segid, found, (ftnint)0, 
	    segid_len);
    }

/* Subroutine */ int ckhave_(logical *found)
{
    return ckbsr_0_(5, (char *)0, (integer *)0, (integer *)0, (doublereal *)0,
	     (doublereal *)0, (logical *)0, (doublereal *)0, (char *)0, found,
	     (ftnint)0, (ftnint)0);
    }

