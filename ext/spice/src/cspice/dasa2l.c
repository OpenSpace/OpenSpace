/* dasa2l.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;
static integer c__256 = 256;

/* $Procedure      DASA2L ( DAS, address to physical location ) */
/* Subroutine */ int dasa2l_(integer *handle, integer *type__, integer *
	addrss, integer *clbase, integer *clsize, integer *recno, integer *
	wordno)
{
    /* Initialized data */

    static integer next[3] = { 2,3,1 };
    static integer prev[3] = { 3,1,2 };
    static integer nw[3] = { 1024,128,256 };
    static integer rngloc[3] = { 3,5,7 };
    static logical first = TRUE_;
    static integer nfiles = 0;

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    static integer free, nrec, fidx;
    static logical fast;
    static integer unit, i__, range[2], tbhan[20];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static integer ncomc, ncomr, ndirs;
    static logical known;
    static integer hiaddr;
    extern /* Subroutine */ int dasham_(integer *, char *, ftnlen);
    static integer tbbase[60]	/* was [3][20] */;
    static char access[10];
    static integer dscloc, dirrec[256];
    extern /* Subroutine */ int dashfs_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *);
    static logical samfil;
    static integer mxaddr;
    extern integer isrchi_(integer *, integer *, integer *);
    static integer tbmxad[60]	/* was [3][20] */;
    static logical tbfast[20];
    static integer mxclrc;
    extern /* Subroutine */ int dashlu_(integer *, integer *), errfnm_(char *,
	     integer *, ftnlen);
    static integer lstrec[3];
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    static integer prvhan;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    static integer nresvc, tbsize[60]	/* was [3][20] */, nxtrec;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), dasrri_(integer *, integer *, integer *, 
	    integer *, integer *);
    static logical rdonly;
    static integer lstwrd[3], nresvr, ntypes, curtyp, prvtyp;

/* $ Abstract */

/*     Map a DAS address to a physical location in the DAS file */
/*     it refers to. */

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
/*     TRANSFORMATION */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   DAS file handle. */
/*     TYPE       I   Data type specifier. */
/*     ADDRSS     I   DAS address of a word of data type TYPE. */
/*     CLBASE, */
/*     CLSIZE     O   Cluster base record number and size. */
/*     RECNO, */
/*     WORDNO     O   Record/word pair corresponding to ADDRSS. */
/*     CHAR       P   Parameter indicating character data type. */
/*     DP         P   Parameter indicating double precision data type. */
/*     INT        P   Parameter indicating integer data type. */

/* $ Detailed_Input */

/*     HANDLE         is the file handle of an open DAS file. */

/*     TYPE           is a data type specifier.  TYPE may be any of */
/*                    the parameters */

/*                       CHAR */
/*                       DP */
/*                       INT */

/*                    which indicate `character', `double precision', */
/*                    and `integer' respectively. */


/*     ADDRSS         is the address in a DAS of a word of data */
/*                    type TYPE.  For each data type (double precision, */
/*                    integer, or character), addresses range */
/*                    from 1 to the maximum current value for that type, */
/*                    which is available from DAFRFR. */

/* $ Detailed_Output */

/*     CLBASE, */
/*     CLSIZE         are, respectively, the base record number and */
/*                    size, in records, of the cluster containing the */
/*                    word corresponding to ADDRSS.  The cluster spans */
/*                    records numbered CLBASE through CLBASE + */
/*                    CLSIZE - 1. */

/*     RECNO, */
/*     WORD           are, respectively, the number of the physical */
/*                    record and the number of the word within the */
/*                    record that correspond to ADDRSS.  Word numbers */
/*                    start at 1 and go up to NC, ND, or NI in */
/*                    character, double precision, or integer records */
/*                    respectively. */

/* $ Parameters */

/*     CHAR, */
/*     DP, */
/*     INT            are data type specifiers which indicate */
/*                    `character', `double precision', and `integer' */
/*                    respectively.  These parameters are used in */
/*                    all DAS routines that require a data type */
/*                    specifier as input. */

/* $ Exceptions */

/*     1)  If TYPE is not recognized, the error SPICE(DASINVALIDTYPE) */
/*         will be signalled. */

/*     2)  ADDRSS must be between 1 and LAST inclusive, where LAST */
/*         is last address in the DAS for a word of the specified */
/*         type.  If ADDRSS is out of range, the error */
/*         SPICE(DASNOSUCHADDRESS) will be signalled. */

/*     3)  If this routine fails to find directory information for */
/*         the input address, the error SPICE(NOSUCHRECORD) will be */
/*         signalled. */

/*     4)  If the input handle is invalid, the error will be diagnosed */
/*         by routines called by this routine. */


/*     If any of the above exceptions occur, the output arguments may */
/*     contain bogus information. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     The DAS architecture allows a programmer to think of the data */
/*     within a DAS file as three one-dimensional arrays:  one of */
/*     double precision numbers, one of integers, and one of characters. */
/*     This model allows a programmer to ask the DAS system for the */
/*     `nth double precision number (or integer, or character) in the */
/*     file'. */

/*     DAS files are Fortran direct access files, so to find the */
/*     `nth double precision number', you must have the number of the */
/*     record containing it and the `word number', or position, within */
/*     the record of the double precision number.  This routine finds */
/*     the record/word number pair that specify the physical location */
/*     in a DAS file corresponding to a DAS address. */

/*     As opposed to DAFs, the mapping of addresses to physical locations */
/*     for a DAS file depends on the organization of data in the file. */
/*     Given a fixed set of DAS format parameters, the physical location */
/*     of the nth double precision number can depend on how many integer */
/*     and character records have been written prior to the record */
/*     containing that double precision number. */

/*     The cluster information output from this routine allows the */
/*     caller to substantially reduce the number of directory reads */
/*     required to read a from range of addresses that spans */
/*     multiple physical records; the reading program only need call */
/*     this routine once per cluster read, rather than once per */
/*     physical record read. */

/* $ Examples */

/*     1)  Use this routine to read integers from a range of */
/*         addresses.  This is done in the routine DASRDI. */

/*            C */
/*            C     Decide how many integers to read. */
/*            C */
/*                  NUMINT = LAST - FIRST + 1 */
/*                  NREAD  = 0 */

/*            C */
/*            C     Find out the physical location of the first */
/*            C     integer.  If FIRST is invalid, DASA2L will take care */
/*            C     of the problem. */
/*            C */

/*                  CALL DASA2L (  HANDLE,  INT,     FIRST, */
/*                 .               CLBASE,  CLSIZE,  RECNO,  WORDNO  ) */

/*            C */
/*            C     Read as much data from record RECNO as necessary. */
/*            C */
/*                  N  =  MIN ( NUMINT,  NWI - WORDNO + 1 ) */

/*                  CALL DASRRI ( HANDLE, RECNO, WORDNO, WORDNO + N-1, */
/*                 .              DATA                                 ) */

/*                  NREAD  =  N */
/*                  RECNO  =  RECNO + 1 */

/*            C */
/*            C     Read from as many additional records as necessary. */
/*            C */
/*                  DO WHILE ( NREAD .LT. NUMINT ) */
/*            C */
/*            C        At this point, RECNO is the correct number of the */
/*            C        record to read from next.  CLBASE is the number */
/*            C        of the first record of the cluster we're about */
/*            C        to read from. */
/*            C */

/*                     IF (  RECNO  .LT.  ( CLBASE + CLSIZE )  ) THEN */
/*            C */
/*            C           We can continue reading from the current */
/*            C           cluster. */
/*            C */
/*                        N  =  MIN ( NUMINT - NREAD,  NWI ) */

/*                        CALL DASRRI (  HANDLE, */
/*                 .                     RECNO, */
/*                 .                     1, */
/*                 .                     N, */
/*                 .                     DATA ( NREAD + 1 )   ) */

/*                        NREAD   =   NREAD + N */
/*                        RECNO   =   RECNO + 1 */


/*                     ELSE */
/*            C */
/*            C           We must find the next integer cluster to */
/*            C           read from.  The first integer in this */
/*            C           cluster has address FIRST + NREAD. */
/*            C */
/*                        CALL DASA2L ( HANDLE, */
/*                 .                    INT, */
/*                 .                    FIRST + NREAD, */
/*                 .                    CLBASE, */
/*                 .                    CLSIZE, */
/*                 .                    RECNO, */
/*                 .                    WORDNO  ) */

/*                     END IF */

/*                  END DO */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer (JPL) */
/*     N.J. Bachman   (JPL) */
/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.1 20-NOV-2001 (NJB) */

/*        Comment fix:  diagram showing directory record pointers */
/*        incorrectly showed element 2 of the record as a backward */
/*        pointer.  The element is actually a forward pointer. */

/* -    SPICELIB Version 1.2.0 03-JUL-1996 (NJB) */

/*        Bug fix:  calculation to determine whether file is segregated */
/*        has been fixed. */

/* -    SPICELIB Version 1.1.1 19-DEC-1995 (NJB) */

/*        Corrected title of permuted index entry section. */

/* -    SPICELIB Version 1.1.0, 03-NOV-1995 (NJB) */

/*        Re-written to optimize address calculations for segregated, */
/*        read-only files. */

/* -    SPICELIB Version 1.0.1, 26-OCT-1993 (KRG) */

/*        Fixed a typo in the $ Brief_I/O section of the header. */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     map DAS logical address to physical location */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0 03-JUL-1996 (NJB) */

/*        Bug fix:  calculation to determine whether file is segregated */
/*        has been fixed.  An incorrect variable name used in a bound */
/*        calculation resulted in an incorrect determination of whether */
/*        a file was segregated, and caused arithmetic overflow for */
/*        files with large maximum addresses. */

/*        In the previous version, the number of DAS words in a cluster */
/*        was incorrectly calculated as the product of the maximum */
/*        address of the cluster's data type and the number of words of */
/*        that data type in a DAS record.  The correct product involves */
/*        the number of records in the cluster and the number of words of */
/*        that data type in a DAS record. */

/* -    SPICELIB Version 1.1.0, 03-NOV-1995 (NJB) */

/*        Re-written to optimize address calculations for segregated, */
/*        read-only files. */

/* -    SPICELIB Version 1.0.1, 26-OCT-1993 (KRG) */

/*        Fixed a typo in the $ Brief_I/O section of the header. */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Words per data record, for each data type: */


/*     Directory pointer locations */


/*     Directory address range locations */


/*     Indices of lowest and highest addresses in a `range array': */


/*     Location of first type descriptor */


/*     Access word length */


/*     File table size */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     NEXT and PREV map the DAS data type codes to their */
/*     successors and predecessors, respectively. */


/*     Discovery check-in is used in this routine. */


/*     DAS files have the following general structure: */

/*           +------------------------+ */
/*           |      file record       | */
/*           +------------------------+ */
/*           |    reserved records    | */
/*           |                        | */
/*           +------------------------+ */
/*           |     comment records    | */
/*           |                        | */
/*           |                        | */
/*           |                        | */
/*           +------------------------+ */
/*           | first data directory   | */
/*           +------------------------+ */
/*           |      data records      | */
/*           |                        | */
/*           |                        | */
/*           |                        | */
/*           |                        | */
/*           +------------------------+ */
/*                       . */
/*                       . */
/*           +------------------------+ */
/*           | last data directory    | */
/*           +------------------------+ */
/*           |     data records       | */
/*           |                        | */
/*           |                        | */
/*           +------------------------+ */


/*        Within each DAS data record, word numbers start at one and */
/*        increase up to NWI, NWD, or NWC:  the number of words in an */
/*        integer, double precision, or character data record. */


/*           +--------------------------------+ */
/*           |       |       |   ...  |       | */
/*           +--------------------------------+ */
/*               1      2                NWD */

/*           +--------------------------------+ */
/*           |   |   |       ...          |   | */
/*           +--------------------------------+ */
/*             1   2                       NWI */

/*           +------------------------------------+ */
/*           | | |           ...                | | */
/*           +------------------------------------+ */
/*            1 2                               NWC */


/*        Directories are single records that describe the data */
/*        types of data records that follow.  The directories */
/*        in a DAS file form a doubly linked list:  each directory */
/*        contains forward and backward pointers to the next and */
/*        previous directories. */

/*        Each directory also contains, for each data type, the lowest */
/*        and highest logical address occurring in any of the records */
/*        described by the directory. */

/*        Following the pointers and address range information is */
/*        a sequence of data type descriptors.  These descriptors */
/*        indicate the data type of data records following the */
/*        directory record.  Each descriptor gives the data type */
/*        of a maximal set of contiguous data records, all having the */
/*        same type.  By `maximal set' we mean that no data records of */
/*        the same type bound the set of records in question. */

/*        Pictorially, the structure of a directory is as follows: */

/*           +----------------------------------------------------+ */
/*           | <pointers> | <address ranges> | <type descriptors> | */
/*           +----------------------------------------------------+ */

/*        where the <pointers> section looks like */

/*           +-----------------------------------------+ */
/*           | <backward pointer> | <forward pointer>  | */
/*           +-----------------------------------------+ */

/*        the <address ranges> section looks like */

/*           +-------------------------------------------+ */
/*           | <char range> | <d.p. range> | <int range> | */
/*           +-------------------------------------------+ */

/*        and each range looks like one of: */

/*           +------------------------------------------------+ */
/*           | <lowest char address> | <highest char address> | */
/*           +------------------------------------------------+ */

/*           +------------------------------------------------+ */
/*           | <lowest d.p. address> | <highest d.p. address> | */
/*           +------------------------------------------------+ */

/*           +------------------------------------------------+ */
/*           | <lowest int address>  | <highest int address>  | */
/*           +------------------------------------------------+ */

/*        The type descriptors implement a run-length encoding */
/*        scheme.  The first element of the series of descriptors */
/*        occupies two integers:  it contains a type code and a count. */
/*        The rest of the descriptors are just signed counts; the data */
/*        types of the records they describe are deduced from the sign */
/*        of the count and the data type of the previous descriptor. */
/*        The method of finding the data type for a given descriptor */
/*        in terms of its predecessor is as follows:  if the sign of a */
/*        descriptor is positive, the type of that descriptor is the */
/*        successor of the type of the preceding descriptor in the */
/*        sequence of types below.  If the sign of a descriptor is */
/*        negative, the type of the descriptor is the predecessor of the */
/*        type of the preceding descriptor. */

/*           C  -->  D  -->  I  -->  C */

/*        For example, if the preceding type is `I', and a descriptor */
/*        contains the number 16, the type of the descriptor is `C', */
/*        whereas if the descriptor contained the number -800, the type */
/*        of the descriptor would be `D'. */


/*     Make sure the data type is valid. */

    if (*type__ < 1 || *type__ > 3) {
	chkin_("DASA2L", (ftnlen)6);
	dashlu_(handle, &unit);
	setmsg_("Invalid data type: #.  File was #", (ftnlen)33);
	errint_("#", type__, (ftnlen)1);
	errfnm_("#", &unit, (ftnlen)1);
	sigerr_("SPICE(DASINVALIDTYPE)", (ftnlen)21);
	chkout_("DASA2L", (ftnlen)6);
	return 0;
    }

/*     Decide whether we're looking at the same file as we did on */
/*     the last call. */

    if (first) {
	samfil = FALSE_;
	fast = FALSE_;
	prvhan = *handle;
	first = FALSE_;
    } else {
	samfil = *handle == prvhan;
	prvhan = *handle;
    }

/*     We have a special case if we're looking at a `fast' file */
/*     that we saw on the last call.  When we say a file is fast, */
/*     we're implying that it's open for read access only and that it's */
/*     segregated.  In this case, we can do an address calculation */
/*     without looking up any information from the file. */

    if (samfil && fast) {
	*clbase = tbbase[(i__1 = *type__ + fidx * 3 - 4) < 60 && 0 <= i__1 ? 
		i__1 : s_rnge("tbbase", i__1, "dasa2l_", (ftnlen)666)];
	*clsize = tbsize[(i__1 = *type__ + fidx * 3 - 4) < 60 && 0 <= i__1 ? 
		i__1 : s_rnge("tbsize", i__1, "dasa2l_", (ftnlen)667)];
	mxaddr = tbmxad[(i__1 = *type__ + fidx * 3 - 4) < 60 && 0 <= i__1 ? 
		i__1 : s_rnge("tbmxad", i__1, "dasa2l_", (ftnlen)668)];
	hiaddr = *clsize * nw[(i__1 = *type__ - 1) < 3 && 0 <= i__1 ? i__1 : 
		s_rnge("nw", i__1, "dasa2l_", (ftnlen)669)];

/*        Make sure that ADDRSS points to an existing location. */

	if (*addrss < 1 || *addrss > mxaddr) {
	    chkin_("DASA2L", (ftnlen)6);
	    dashlu_(handle, &unit);
	    setmsg_("ADDRSS was #; valid range for type # is # to #.  File w"
		    "as #", (ftnlen)59);
	    errint_("#", addrss, (ftnlen)1);
	    errint_("#", type__, (ftnlen)1);
	    errint_("#", &c__1, (ftnlen)1);
	    errint_("#", &mxaddr, (ftnlen)1);
	    errfnm_("#", &unit, (ftnlen)1);
	    sigerr_("SPICE(DASNOSUCHADDRESS)", (ftnlen)23);
	    chkout_("DASA2L", (ftnlen)6);
	    return 0;
	}
    } else {

/*        If the current file is not the same one we looked at on the */
/*        last call, find out whether the file is on record in our file */
/*        table.  Add the file to the table if necessary.  Bump the */
/*        oldest file in the table if there's no room. */

	if (! samfil) {
	    fidx = isrchi_(handle, &nfiles, tbhan);
	    known = fidx > 0;
	    if (known) {

/*              The file is in our list. */

		fast = tbfast[(i__1 = fidx - 1) < 20 && 0 <= i__1 ? i__1 : 
			s_rnge("tbfast", i__1, "dasa2l_", (ftnlen)708)];
		if (fast) {

/*                 This is a segregated, read-only file.  Look up the */
/*                 saved information we'll need to calculate addresses. */

		    *clbase = tbbase[(i__1 = *type__ + fidx * 3 - 4) < 60 && 
			    0 <= i__1 ? i__1 : s_rnge("tbbase", i__1, "dasa2"
			    "l_", (ftnlen)715)];
		    *clsize = tbsize[(i__1 = *type__ + fidx * 3 - 4) < 60 && 
			    0 <= i__1 ? i__1 : s_rnge("tbsize", i__1, "dasa2"
			    "l_", (ftnlen)716)];
		    mxaddr = tbmxad[(i__1 = *type__ + fidx * 3 - 4) < 60 && 0 
			    <= i__1 ? i__1 : s_rnge("tbmxad", i__1, "dasa2l_",
			     (ftnlen)717)];
		    hiaddr = *clsize * nw[(i__1 = *type__ - 1) < 3 && 0 <= 
			    i__1 ? i__1 : s_rnge("nw", i__1, "dasa2l_", (
			    ftnlen)718)];

/*                 Make sure that ADDRSS points to an existing location. */

		    if (*addrss < 1 || *addrss > mxaddr) {
			chkin_("DASA2L", (ftnlen)6);
			dashlu_(handle, &unit);
			setmsg_("ADDRSS was #; valid range for  type # is # "
				"to #.  File was #", (ftnlen)60);
			errint_("#", addrss, (ftnlen)1);
			errint_("#", type__, (ftnlen)1);
			errint_("#", &c__1, (ftnlen)1);
			errint_("#", &mxaddr, (ftnlen)1);
			errfnm_("#", &unit, (ftnlen)1);
			sigerr_("SPICE(DASNOSUCHADDRESS)", (ftnlen)23);
			chkout_("DASA2L", (ftnlen)6);
			return 0;
		    }
		}

/*              FAST is set. */

	    }

/*           KNOWN is set. */

	}

/*        SAMFIL, FAST, and KNOWN are set.  If the file is the same one */
/*        we saw on the last call, the state variables FAST, and KNOWN */
/*        retain their values from the previous call. */

/*        FIDX is set at this point only if we're looking at a known */
/*        file. */

/*        Unless the file is recognized and known to be a fast file, we */
/*        look up all metadata for the file. */

	if (! (known && fast)) {
	    if (! known) {

/*              This file is not in our list.  If the list is not full, */
/*              append the file to the list.  If the list is full, */
/*              replace the oldest (first) file with this one. */

		if (nfiles < 20) {
		    ++nfiles;
		    fidx = nfiles;
		} else {
		    fidx = 1;
		}
		tbhan[(i__1 = fidx - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge(
			"tbhan", i__1, "dasa2l_", (ftnlen)781)] = *handle;

/*              Find out whether the file is open for read or write */
/*              access.  We consider the file to be `slow' until we find */
/*              out otherwise.  The contents of the arrays TBHIGH, */
/*              TBBASE, TBSIZE, and TBMXAD are left undefined for slow */
/*              files. */

		dasham_(handle, access, (ftnlen)10);
		rdonly = s_cmp(access, "READ", (ftnlen)10, (ftnlen)4) == 0;
		fast = FALSE_;
		tbfast[(i__1 = fidx - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge(
			"tbfast", i__1, "dasa2l_", (ftnlen)794)] = fast;

/*              We'll set the flag KNOWN at the end of the outer IF */
/*              block. */

	    } else {

/*              We set RDONLY to .FALSE. for any known file that is */
/*              not fast.  It's actually possible for a read-only file */
/*              to be unsegregated, but this is expected to be a rare */
/*              case, one that's not worth complicating this routine */
/*              further for. */

		rdonly = FALSE_;
	    }

/*           RDONLY is set. */

/*           FIDX is now set whether or not the current file is known. */

/*           Get the number of reserved records, comment records, and */
/*           the current last address of the data type TYPE from the */
/*           file  summary. */

	    dashfs_(handle, &nresvr, &nresvc, &ncomr, &ncomc, &free, &tbmxad[(
		    i__1 = fidx * 3 - 3) < 60 && 0 <= i__1 ? i__1 : s_rnge(
		    "tbmxad", i__1, "dasa2l_", (ftnlen)821)], lstrec, lstwrd);
	    mxaddr = tbmxad[(i__1 = *type__ + fidx * 3 - 4) < 60 && 0 <= i__1 
		    ? i__1 : s_rnge("tbmxad", i__1, "dasa2l_", (ftnlen)831)];

/*           Make sure that ADDRSS points to an existing location. */

	    if (*addrss < 1 || *addrss > mxaddr) {
		chkin_("DASA2L", (ftnlen)6);
		dashlu_(handle, &unit);
		setmsg_("ADDRSS was #; valid range for  type # is # to #.  F"
			"ile was #", (ftnlen)60);
		errint_("#", addrss, (ftnlen)1);
		errint_("#", type__, (ftnlen)1);
		errint_("#", &c__1, (ftnlen)1);
		errint_("#", &mxaddr, (ftnlen)1);
		errfnm_("#", &unit, (ftnlen)1);
		sigerr_("SPICE(DASNOSUCHADDRESS)", (ftnlen)23);
		chkout_("DASA2L", (ftnlen)6);
		return 0;
	    }

/*           Find out which directory describes the cluster containing */
/*           this word.  To do this, we must traverse the directory */
/*           list.  The first directory record comes right after the */
/*           last comment record.  (Don't forget the file record when */
/*           counting the predecessors of the directory record.) */

/*           Note that we don't need to worry about not finding a */
/*           directory record that contains the address we're looking */
/*           for, since we've already checked that the address is in */
/*           range. */

/*           Keep track of the number of directory records we see.  We'll */
/*           use this later to determine whether we've got a segregated */
/*           file. */

	    nrec = nresvr + ncomr + 2;
	    ndirs = 1;
	    i__3 = rngloc[(i__2 = *type__ - 1) < 3 && 0 <= i__2 ? i__2 : 
		    s_rnge("rngloc", i__2, "dasa2l_", (ftnlen)872)] + 1;
	    dasrri_(handle, &nrec, &rngloc[(i__1 = *type__ - 1) < 3 && 0 <= 
		    i__1 ? i__1 : s_rnge("rngloc", i__1, "dasa2l_", (ftnlen)
		    872)], &i__3, range);
	    while(range[1] < *addrss) {

/*              The record number of the next directory is the forward */
/*              pointer in the current directory record.  Update NREC */
/*              with this pointer.  Get the address range for the */
/*              specified type covered by this next directory record. */

		dasrri_(handle, &nrec, &c__2, &c__2, &nxtrec);
		nrec = nxtrec;
		++ndirs;
		i__3 = rngloc[(i__2 = *type__ - 1) < 3 && 0 <= i__2 ? i__2 : 
			s_rnge("rngloc", i__2, "dasa2l_", (ftnlen)891)] + 1;
		dasrri_(handle, &nrec, &rngloc[(i__1 = *type__ - 1) < 3 && 0 
			<= i__1 ? i__1 : s_rnge("rngloc", i__1, "dasa2l_", (
			ftnlen)891)], &i__3, range);
	    }

/*           NREC is now the record number of the directory that contains */
/*           the type descriptor for the address we're looking for. */

/*           Our next task is to find the descriptor for the cluster */
/*           containing the input address.  To do this, we must examine */
/*           the directory record in `left-to-right' order.  As we do so, */
/*           we'll keep track of the highest address of type TYPE */
/*           occurring in the clusters whose descriptors we've seen. */
/*           The variable HIADDR will contain this address. */

	    dasrri_(handle, &nrec, &c__1, &c__256, dirrec);

/*           In the process of finding the physical location */
/*           corresponding to ADDRSS, we'll find the record number of the */
/*           base of the cluster containing ADDRSS.  We'll start out by */
/*           initializing this value with the number of the first data */
/*           record of the next cluster. */

	    *clbase = nrec + 1;

/*           We'll initialize HIADDR with the value preceding the lowest */
/*           address of type TYPE described by the current directory. */

	    hiaddr = dirrec[(i__2 = rngloc[(i__1 = *type__ - 1) < 3 && 0 <= 
		    i__1 ? i__1 : s_rnge("rngloc", i__1, "dasa2l_", (ftnlen)
		    925)] - 1) < 256 && 0 <= i__2 ? i__2 : s_rnge("dirrec", 
		    i__2, "dasa2l_", (ftnlen)925)] - 1;

/*           Initialize the number of records described by the last seen */
/*           type descriptor.  This number, when added to CLBASE, should */
/*           yield the number of the first record of the current cluster; */
/*           that's why it's initialized to 0. */

	    *clsize = 0;

/*           Now find the descriptor for the cluster containing ADDRSS. */
/*           Read descriptors until we get to the one that describes the */
/*           record containing ADDRSS.  Keep track of descriptor data */
/*           types as we go.  Also count the descriptors. */

/*           At this point, HIADDR is less than ADDRSS, so the loop will */
/*           always be executed at least once. */

	    prvtyp = prev[(i__1 = dirrec[8] - 1) < 3 && 0 <= i__1 ? i__1 : 
		    s_rnge("prev", i__1, "dasa2l_", (ftnlen)944)];
	    dscloc = 10;
	    while(hiaddr < *addrss) {

/*              Update CLBASE so that it is the record number of the */
/*              first record of the current cluster. */

		*clbase += *clsize;

/*              Find the type of the current descriptor. */

		if (dirrec[(i__1 = dscloc - 1) < 256 && 0 <= i__1 ? i__1 : 
			s_rnge("dirrec", i__1, "dasa2l_", (ftnlen)957)] > 0) {
		    curtyp = next[(i__1 = prvtyp - 1) < 3 && 0 <= i__1 ? i__1 
			    : s_rnge("next", i__1, "dasa2l_", (ftnlen)958)];
		} else {
		    curtyp = prev[(i__1 = prvtyp - 1) < 3 && 0 <= i__1 ? i__1 
			    : s_rnge("prev", i__1, "dasa2l_", (ftnlen)960)];
		}

/*              Forgetting to update PRVTYP is a Very Bad Thing (VBT). */

		prvtyp = curtyp;

/*              If the current descriptor is of the type we're interested */
/*              in, update the highest address count. */

		if (curtyp == *type__) {
		    hiaddr += nw[(i__1 = *type__ - 1) < 3 && 0 <= i__1 ? i__1 
			    : s_rnge("nw", i__1, "dasa2l_", (ftnlen)973)] * (
			    i__3 = dirrec[(i__2 = dscloc - 1) < 256 && 0 <= 
			    i__2 ? i__2 : s_rnge("dirrec", i__2, "dasa2l_", (
			    ftnlen)973)], abs(i__3));
		}

/*              Compute the number of records described by the current */
/*              descriptor.  Update the descriptor location. */

		*clsize = (i__2 = dirrec[(i__1 = dscloc - 1) < 256 && 0 <= 
			i__1 ? i__1 : s_rnge("dirrec", i__1, "dasa2l_", (
			ftnlen)980)], abs(i__2));
		++dscloc;
	    }

/*           If we have an unknown read-only file, see whether the file */
/*           is segregated.  If it is, we'll be able to compute */
/*           addresses much faster for subsequent reads to this file. */

	    if (rdonly && ! known) {
		if (ndirs == 1) {

/*                 If this file is segregated, there are at most three */
/*                 cluster descriptors, and each one points to a cluster */
/*                 containing all records of the corresponding data type. */
/*                 For each data type having a non-zero maximum address, */
/*                 the size of the corresponding cluster must be large */
/*                 enough to hold all addresses of that type. */

		    ntypes = 0;
		    for (i__ = 1; i__ <= 3; ++i__) {
			if (tbmxad[(i__1 = i__ + fidx * 3 - 4) < 60 && 0 <= 
				i__1 ? i__1 : s_rnge("tbmxad", i__1, "dasa2l_"
				, (ftnlen)1005)] > 0) {
			    ++ntypes;
			}
		    }

/*                 Now look at the first NTYPES cluster descriptors, */
/*                 collecting cluster bases and sizes as we go. */

		    mxclrc = nrec + 1;
		    prvtyp = prev[(i__1 = dirrec[8] - 1) < 3 && 0 <= i__1 ? 
			    i__1 : s_rnge("prev", i__1, "dasa2l_", (ftnlen)
			    1016)];
		    dscloc = 10;
		    fast = TRUE_;
		    while(dscloc <= ntypes + 9 && fast) {

/*                    Find the type of the current descriptor. */

			if (dirrec[(i__1 = dscloc - 1) < 256 && 0 <= i__1 ? 
				i__1 : s_rnge("dirrec", i__1, "dasa2l_", (
				ftnlen)1025)] > 0) {
			    curtyp = next[(i__1 = prvtyp - 1) < 3 && 0 <= 
				    i__1 ? i__1 : s_rnge("next", i__1, "dasa"
				    "2l_", (ftnlen)1026)];
			} else {
			    curtyp = prev[(i__1 = prvtyp - 1) < 3 && 0 <= 
				    i__1 ? i__1 : s_rnge("prev", i__1, "dasa"
				    "2l_", (ftnlen)1028)];
			}
			prvtyp = curtyp;
			tbbase[(i__1 = curtyp + fidx * 3 - 4) < 60 && 0 <= 
				i__1 ? i__1 : s_rnge("tbbase", i__1, "dasa2l_"
				, (ftnlen)1032)] = mxclrc;
			tbsize[(i__1 = curtyp + fidx * 3 - 4) < 60 && 0 <= 
				i__1 ? i__1 : s_rnge("tbsize", i__1, "dasa2l_"
				, (ftnlen)1033)] = (i__3 = dirrec[(i__2 = 
				dscloc - 1) < 256 && 0 <= i__2 ? i__2 : 
				s_rnge("dirrec", i__2, "dasa2l_", (ftnlen)
				1033)], abs(i__3));
			mxclrc += tbsize[(i__1 = curtyp + fidx * 3 - 4) < 60 
				&& 0 <= i__1 ? i__1 : s_rnge("tbsize", i__1, 
				"dasa2l_", (ftnlen)1034)];
			fast = tbmxad[(i__1 = curtyp + fidx * 3 - 4) < 60 && 
				0 <= i__1 ? i__1 : s_rnge("tbmxad", i__1, 
				"dasa2l_", (ftnlen)1037)] <= tbsize[(i__2 = 
				curtyp + fidx * 3 - 4) < 60 && 0 <= i__2 ? 
				i__2 : s_rnge("tbsize", i__2, "dasa2l_", (
				ftnlen)1037)] * nw[(i__3 = curtyp - 1) < 3 && 
				0 <= i__3 ? i__3 : s_rnge("nw", i__3, "dasa2"
				"l_", (ftnlen)1037)];
			++dscloc;
		    }

/*                 FAST is set. */

		} else {

/*                 The file has more than one directory record. */

		    fast = FALSE_;
		}

/*              If the file was unknown, readonly, and had one directory */
/*              record, we determined whether it was a fast file. */


	    } else {

/*              The file was already known and wasn't fast, or is not */
/*              readonly. */

		fast = FALSE_;
	    }

/*           FAST is set. */

	}

/*        This is the end of the `.NOT. ( KNOWN .AND. FAST )' case. */

/*        At this point, we've set or looked up CLBASE, CLSIZE, MXADDR, */
/*        and HIADDR. */

/*        If the file was unknown, we set TBHAN, TBRDON, and TBFAST. */
/*        If the file was unknown and turned out to be fast, we set */
/*        TBBASE, TBSIZE, TBHIGH, and TBMXAD as well. */

/*        At this point, it's safe to indicate that the file is known. */

	known = TRUE_;
    }

/*     At this point, */

/*        -- CLBASE is properly set:  it is the record number of the */
/*           first record of the cluster containing ADDRSS. */

/*        -- CLSIZE is properly set:  it is the size of the cluster */
/*           containing ADDRSS. */

/*        -- HIADDR is the last logical address in the cluster */
/*           containing ADDRSS. */

/*     Now we must find the physical record and word corresponding */
/*     to ADDRSS.  The structure of the cluster containing ADDRSS and */
/*     HIADDR is shown below: */

/*        +--------------------------------------+ */
/*        |                                      |  Record # CLBASE */
/*        +--------------------------------------+ */
/*                           . */
/*                           . */
/*                           . */
/*        +--------------------------------------+ */
/*        |      |ADDRSS|                        |  Record # RECNO */
/*        +--------------------------------------+ */
/*                           . */
/*                           . */
/*                           . */
/*        +--------------------------------------+  Record # */
/*        |                               |HIADDR| */
/*        +--------------------------------------+  CLBASE + CLSIZE - 1 */



    *recno = *clbase + *clsize - 1 - (hiaddr - *addrss) / nw[(i__1 = *type__ 
	    - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("nw", i__1, "dasa2l_", (
	    ftnlen)1122)];
    *wordno = *addrss - (*addrss - 1) / nw[(i__1 = *type__ - 1) < 3 && 0 <= 
	    i__1 ? i__1 : s_rnge("nw", i__1, "dasa2l_", (ftnlen)1125)] * nw[(
	    i__2 = *type__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("nw", i__2, 
	    "dasa2l_", (ftnlen)1125)];
    return 0;
} /* dasa2l_ */

