/* spkcov.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure      SPKCOV ( SPK coverage ) */
/* Subroutine */ int spkcov_(char *spk, integer *idcode, doublereal *cover, 
	ftnlen spk_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char arch[80];
    extern /* Subroutine */ int dafgs_(doublereal *), chkin_(char *, ftnlen);
    doublereal descr[5];
    extern /* Subroutine */ int dafus_(doublereal *, integer *, integer *, 
	    doublereal *, integer *), errch_(char *, char *, ftnlen, ftnlen);
    logical found;
    doublereal dc[2];
    integer ic[6];
    extern /* Subroutine */ int daffna_(logical *);
    extern logical failed_(void);
    extern /* Subroutine */ int dafbfs_(integer *);
    integer handle;
    extern /* Subroutine */ int dafcls_(integer *), getfat_(char *, char *, 
	    char *, ftnlen, ftnlen, ftnlen), dafopr_(char *, integer *, 
	    ftnlen), sigerr_(char *, ftnlen), chkout_(char *, ftnlen), 
	    setmsg_(char *, ftnlen), wninsd_(doublereal *, doublereal *, 
	    doublereal *);
    char kertyp[80];
    extern logical return_(void);

/* $ Abstract */

/*     Find the coverage window for a specified ephemeris object in a */
/*     specified SPK file. */

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

/*     CELLS */
/*     DAF */
/*     SPK */
/*     TIME */
/*     WINDOWS */

/* $ Keywords */

/*     EPHEMERIS */
/*     TIME */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     SPK        I   Name of SPK file. */
/*     IDCODE     I   ID code of ephemeris object. */
/*     COVER     I/O  Window giving coverage in SPK for IDCODE. */

/* $ Detailed_Input */

/*     SPK            is the name of an SPK file. */

/*     IDCODE         is the integer ID code of an object for which */
/*                    ephemeris data are expected to exist in the */
/*                    specified SPK file. */

/*     COVER          is an initialized SPICELIB window data structure. */
/*                    COVER optionally may contain coverage data on */
/*                    input; on output, the data already present in */
/*                    COVER will be combined with coverage found for the */
/*                    object designated by IDCODE in the file SPK. */

/*                    If COVER contains no data on input, its size and */
/*                    cardinality still must be initialized. */

/* $ Detailed_Output */

/*     COVER          is a SPICELIB window data structure which */
/*                    represents the merged coverage for IDCODE. This is */
/*                    the set of time intervals for which data for */
/*                    IDCODE are present in the file SPK, merged with */
/*                    the set of time intervals present in COVER on */
/*                    input.  The merged coverage is represented as the */
/*                    union of one or more disjoint time intervals. The */
/*                    window COVER contains the pairs of endpoints of */
/*                    these intervals. */

/*                    The interval endpoints contained in COVER are */
/*                    ephemeris times, expressed as seconds past J2000 */
/*                    TDB. */

/*                    See the Examples section below for a complete */
/*                    example program showing how to retrieve the */
/*                    endpoints from COVER. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input file has transfer format, the error */
/*         SPICE(INVALIDFORMAT) is signaled. */

/*     2)  If the input file is not a transfer file but has architecture */
/*         other than DAF, the error SPICE(BADARCHTYPE) is signaled. */

/*     3)  If the input file is a binary DAF file of type other than */
/*         SPK, the error SPICE(BADFILETYPE) is signaled. */

/*     4)  If the SPK file cannot be opened or read, the error will */
/*         be diagnosed by routines called by this routine. The output */
/*         window will not be modified. */

/*     5)  If the size of the output window argument COVER is */
/*         insufficient to contain the actual number of intervals in the */
/*         coverage window for IDCODE, the error will be diagnosed by */
/*         routines called by this routine. */

/* $ Files */

/*     This routine reads an SPK file. */

/* $ Particulars */

/*     This routine provides an API via which applications can determine */
/*     the coverage a specified SPK file provides for a specified */
/*     ephemeris object. */

/* $ Examples */

/*     1)  This example demonstrates combined usage of SPKCOV and the */
/*         related SPK utility SKOBJ. */

/*         Display the coverage for each object in a specified SPK file. */
/*         Find the set of objects in the file; for each object, find */
/*         and display the coverage. */


/*              PROGRAM IDCOV */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              INTEGER               CARDI */
/*              INTEGER               WNCARD */
/*        C */
/*        C     Local parameters */
/*        C */
/*        C */
/*        C     Declare the coverage window.  Make enough room */
/*        C     for MAXIV intervals. */
/*        C */
/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*              INTEGER               MAXIV */
/*              PARAMETER           ( MAXIV  = 1000 ) */

/*              INTEGER               WINSIZ */
/*              PARAMETER           ( WINSIZ = 2 * MAXIV ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 50 ) */

/*              INTEGER               MAXOBJ */
/*              PARAMETER           ( MAXOBJ = 1000 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(FILSIZ)    LSK */
/*              CHARACTER*(FILSIZ)    SPK */
/*              CHARACTER*(TIMLEN)    TIMSTR */

/*              DOUBLE PRECISION      B */
/*              DOUBLE PRECISION      COVER ( LBCELL : WINSIZ ) */
/*              DOUBLE PRECISION      E */

/*              INTEGER               I */
/*              INTEGER               IDS   ( LBCELL : MAXOBJ ) */
/*              INTEGER               J */
/*              INTEGER               NIV */


/*        C */
/*        C     Load a leapseconds kernel for output time conversion. */
/*        C     SPKCOV itself does not require a leapseconds kernel. */
/*        C */
/*              CALL PROMPT ( 'Name of leapseconds kernel > ', LSK ) */
/*              CALL FURNSH ( LSK ) */

/*        C */
/*        C     Get name of SPK file. */
/*        C */
/*              CALL PROMPT ( 'Name of SPK file           > ', SPK ) */

/*        C */
/*        C     Initialize the set IDS. */
/*        C */
/*              CALL SSIZEI ( MAXOBJ, IDS ) */

/*        C */
/*        C     Initialize the window COVER. */
/*        C */
/*              CALL SSIZED ( WINSIZ, COVER ) */

/*        C */
/*        C     Find the set of objects in the SPK file. */
/*        C */
/*              CALL SPKOBJ ( SPK, IDS ) */

/*        C */
/*        C     We want to display the coverage for each object.  Loop */
/*        C     over the contents of the ID code set, find the coverage */
/*        C     for each item in the set, and display the coverage. */
/*        C */
/*              DO I = 1, CARDI( IDS ) */
/*        C */
/*        C        Find the coverage window for the current */
/*        C        object. Empty the coverage window each time */
/*        C        so we don't include data for the previous object. */
/*        C */
/*                 CALL SCARDD ( 0,   COVER ) */
/*                 CALL SPKCOV ( SPK, IDS(I), COVER ) */

/*        C */
/*        C        Get the number of intervals in the coverage */
/*        C        window. */
/*        C */
/*                 NIV = WNCARD ( COVER ) */

/*        C */
/*        C        Display a simple banner. */
/*        C */
/*                 WRITE (*,*) '========================================' */
/*                 WRITE (*,*) 'Coverage for object ', IDS(I) */

/*        C */
/*        C        Convert the coverage interval start and stop */
/*        C        times to TDB calendar strings. */
/*        C */
/*                 DO J = 1, NIV */
/*        C */
/*        C           Get the endpoints of the Jth interval. */
/*        C */
/*                    CALL WNFETD ( COVER, J, B, E ) */
/*        C */
/*        C           Convert the endpoints to TDB calendar */
/*        C           format time strings and display them. */
/*        C */
/*                    CALL TIMOUT ( B, */
/*             .                    'YYYY MON DD HR:MN:SC.### ' // */
/*             .                    '(TDB) ::TDB', */
/*             .                    TIMSTR                        ) */
/*                    WRITE (*,*) ' ' */
/*                    WRITE (*,*) 'Interval: ', J */
/*                    WRITE (*,*) 'Start:    ', TIMSTR */

/*                    CALL TIMOUT ( E, */
/*             .                    'YYYY MON DD HR:MN:SC.### ' // */
/*             .                    '(TDB) ::TDB', */
/*             .                    TIMSTR                        ) */
/*                    WRITE (*,*) 'Stop:     ', TIMSTR */
/*                    WRITE (*,*) ' ' */

/*                 END DO */

/*                 WRITE (*,*) '========================================' */

/*              END DO */

/*              END */


/*     2) Find the coverage for the object designated by IDCODE */
/*        provided by the set of SPK files loaded via a metakernel. */
/*        (The metakernel must also specify a leapseconds kernel.) */

/*              PROGRAM METCOV */
/*              IMPLICIT NONE */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              INTEGER               WNCARD */

/*        C */
/*        C     Local parameters */
/*        C */
/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*              INTEGER               LNSIZE */
/*              PARAMETER           ( LNSIZE = 80 ) */

/*              INTEGER               MAXCOV */
/*              PARAMETER           ( MAXCOV = 100000 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 50 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(FILSIZ)    FILE */
/*              CHARACTER*(LNSIZE)    IDCH */
/*              CHARACTER*(FILSIZ)    META */
/*              CHARACTER*(FILSIZ)    SOURCE */
/*              CHARACTER*(TIMLEN)    TIMSTR */
/*              CHARACTER*(LNSIZE)    TYPE */

/*              DOUBLE PRECISION      B */
/*              DOUBLE PRECISION      COVER  ( LBCELL : 2*MAXCOV ) */
/*              DOUBLE PRECISION      E */

/*              INTEGER               COUNT */
/*              INTEGER               HANDLE */
/*              INTEGER               I */
/*              INTEGER               IDCODE */
/*              INTEGER               NIV */

/*              LOGICAL               FOUND */

/*        C */
/*        C     Prompt for the metakernel name; load the metakernel. */
/*        C     The metakernel lists the SPK files whose coverage */
/*        C     for IDCODE we'd like to determine.  The metakernel */
/*        C     must also specify a leapseconds kernel. */
/*        C */
/*              CALL PROMPT ( 'Enter name of metakernel > ', META ) */

/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Get the ID code of interest. */
/*        C */
/*              CALL PROMPT ( 'Enter ID code            > ', IDCH ) */

/*              CALL PRSINT ( IDCH,  IDCODE ) */

/*        C */
/*        C     Initialize the coverage window. */
/*        C */
/*              CALL SSIZED ( MAXCOV, COVER ) */

/*        C */
/*        C     Find out how many kernels are loaded.  Loop over the */
/*        C     kernels:  for each loaded SPK file, add its coverage */
/*        C     for IDCODE, if any, to the coverage window. */
/*        C */
/*              CALL KTOTAL ( 'SPK', COUNT ) */

/*              DO I = 1, COUNT */

/*                 CALL KDATA  ( I,       'SPK',   FILE,  TYPE, */
/*             .                 SOURCE,  HANDLE,  FOUND       ) */

/*                 CALL SPKCOV ( FILE,    IDCODE,  COVER ) */

/*              END DO */

/*        C */
/*        C     Display results. */
/*        C */
/*        C     Get the number of intervals in the coverage */
/*        C     window. */
/*        C */
/*              NIV = WNCARD ( COVER ) */

/*        C */
/*        C     Display a simple banner. */
/*        C */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Coverage for object ', IDCODE */

/*        C */
/*        C     Convert the coverage interval start and stop */
/*        C     times to TDB calendar strings. */
/*        C */
/*              DO I = 1, NIV */
/*        C */
/*        C        Get the endpoints of the Ith interval. */
/*        C */
/*                 CALL WNFETD ( COVER, I, B, E ) */
/*        C */
/*        C        Convert the endpoints to TDB calendar */
/*        C        format time strings and display them. */
/*        C */
/*                 CALL TIMOUT ( B, */
/*             .                 'YYYY MON DD HR:MN:SC.### ' // */
/*             .                 '(TDB) ::TDB', */
/*             .                 TIMSTR                        ) */
/*                 WRITE (*,*) ' ' */
/*                 WRITE (*,*) 'Interval: ', I */
/*                 WRITE (*,*) 'Start:    ', TIMSTR */

/*                 CALL TIMOUT ( E, */
/*             .                 'YYYY MON DD HR:MN:SC.### ' // */
/*             .                 '(TDB) ::TDB', */
/*             .                 TIMSTR                        ) */
/*                 WRITE (*,*) 'Stop:     ', TIMSTR */
/*                 WRITE (*,*) ' ' */

/*              END DO */

/*              END */


/* $ Restrictions */

/*     1) If an error occurs while this routine is updating the window */
/*        COVER, the window may be corrupted. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 01-JUL-2014 (NJB) */

/*        Added new index entries. */

/* -    SPICELIB Version 1.0.1, 30-NOV-2007 (NJB) */

/*        Corrected bug in first program in header Examples section: */
/*        program now empties the coverage window prior to collecting */
/*        data for the current object. Updated examples to use WNCARD */
/*        rather than CARDD. */

/* -    SPICELIB Version 1.0.0, 30-DEC-2004 (NJB) */

/* -& */
/* $ Index_Entries */

/*     get coverage window for spk_object */
/*     get coverage start and stop time for spk_object */
/*     get coverage start and stop time for ephemeris_object */
/*     get coverage start and stop time for body */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKCOV", (ftnlen)6);

/*     See whether GETFAT thinks we've got a binary SPK file. */
/*     If not, indicate the specific problem. */

    getfat_(spk, arch, kertyp, spk_len, (ftnlen)80, (ftnlen)80);
    if (s_cmp(arch, "XFR", (ftnlen)80, (ftnlen)3) == 0) {
	setmsg_("Input file # has architecture #. The file must be a binary "
		"SPK file to be readable by this routine.  If the input file "
		"is an SPK file in transfer format, run TOBIN on the file to "
		"convert it to binary format.", (ftnlen)207);
	errch_("#", spk, (ftnlen)1, spk_len);
	errch_("#", arch, (ftnlen)1, (ftnlen)80);
	sigerr_("SPICE(INVALIDFORMAT)", (ftnlen)20);
	chkout_("SPKCOV", (ftnlen)6);
	return 0;
    } else if (s_cmp(arch, "DAF", (ftnlen)80, (ftnlen)3) != 0) {
	setmsg_("Input file # has architecture #. The file must be a binary "
		"SPK file to be readable by this routine.  Binary SPK files h"
		"ave DAF architecture.  If you expected the file to be a bina"
		"ry SPK file, the problem may be due to the file being an old"
		" non-native file lacking binary file format information. It'"
		"s also possible the file has been corrupted.", (ftnlen)343);
	errch_("#", spk, (ftnlen)1, spk_len);
	errch_("#", arch, (ftnlen)1, (ftnlen)80);
	sigerr_("SPICE(INVALIDARCHTYPE)", (ftnlen)22);
	chkout_("SPKCOV", (ftnlen)6);
	return 0;
    } else if (s_cmp(kertyp, "SPK", (ftnlen)80, (ftnlen)3) != 0) {
	setmsg_("Input file # has file type #. The file must be a binary SPK"
		" file to be readable by this routine. If you expected the fi"
		"le to be a binary SPK file, the problem may be due to the fi"
		"le being an old non-native file lacking binary file format i"
		"nformation. It's also possible the file has been corrupted.", 
		(ftnlen)298);
	errch_("#", spk, (ftnlen)1, spk_len);
	errch_("#", kertyp, (ftnlen)1, (ftnlen)80);
	sigerr_("SPICE(INVALIDFILETYPE)", (ftnlen)22);
	chkout_("SPKCOV", (ftnlen)6);
	return 0;
    }

/*     Open the file for reading. */

    dafopr_(spk, &handle, spk_len);
    if (failed_()) {
	chkout_("SPKCOV", (ftnlen)6);
	return 0;
    }

/*     We will examine each segment descriptor in the file, and */
/*     we'll update our coverage bounds according to the data found */
/*     in these descriptors. */

/*     Start a forward search. */

    dafbfs_(&handle);

/*     Find the next DAF array. */

    daffna_(&found);
    while(found && ! failed_()) {

/*        Fetch and unpack the segment descriptor. */

	dafgs_(descr);
	dafus_(descr, &c__2, &c__6, dc, ic);
	if (ic[0] == *idcode) {

/*           This segment is for the body of interest.  Insert the */
/*           coverage bounds into the coverage window. */

	    wninsd_(dc, &dc[1], cover);
	}
	daffna_(&found);
    }

/*     Release the file. */

    dafcls_(&handle);
    chkout_("SPKCOV", (ftnlen)6);
    return 0;
} /* spkcov_ */

