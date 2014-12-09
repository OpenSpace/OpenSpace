/* spk14b.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__14 = 14;
static integer c__1 = 1;
static integer c__3 = 3;

/* $Procedure      SPK14B ( SPK type 14: Begin a segment.) */
/* Subroutine */ int spk14b_(integer *handle, char *segid, integer *body, 
	integer *center, char *frame, doublereal *first, doublereal *last, 
	integer *chbdeg, ftnlen segid_len, ftnlen frame_len)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal descr[5];
    extern logical failed_(void);
    doublereal dcoeff;
    integer ncoeff;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), sgbwfs_(integer *, doublereal *, char *, integer *, 
	    doublereal *, integer *, integer *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen), spkpds_(integer *, 
	    integer *, char *, integer *, doublereal *, doublereal *, 
	    doublereal *, ftnlen);
    extern logical return_(void);
    integer pktsiz;

/* $ Abstract */

/*     Begin a type 14 SPK segment in the SPK file associated with */
/*     HANDLE. See also SPK14A and SPK14E. */

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

/*     SPK */

/* $ Declarations */

/* $ Abstract */

/*     Parameter declarations for the generic segments subroutines. */

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

/*      DAF Required Reading */

/* $ Keywords */

/*       GENERIC SEGMENTS */

/* $ Particulars */

/*     This include file contains the parameters used by the generic */
/*     segments subroutines, SGxxxx. A generic segment is a */
/*     generalization of a DAF array which places a particular structure */
/*     on the data contained in the array, as described below. */

/*     This file defines the mnemonics that are used for the index types */
/*     allowed in generic segments as well as mnemonics for the meta data */
/*     items which are used to describe a generic segment. */

/*     A DAF generic segment contains several logical data partitions: */

/*        1) A partition for constant values to be associated with each */
/*           data packet in the segment. */

/*        2) A partition for the data packets. */

/*        3) A partition for reference values. */

/*        4) A partition for a packet directory, if the segment contains */
/*           variable sized packets. */

/*        5) A partition for a reference value directory. */

/*        6) A reserved partition that is not currently used. This */
/*           partition is only for the use of the NAIF group at the Jet */
/*           Propulsion Laboratory (JPL). */

/*        7) A partition for the meta data which describes the locations */
/*           and sizes of other partitions as well as providing some */
/*           additional descriptive information about the generic */
/*           segment. */

/*                 +============================+ */
/*                 |         Constants          | */
/*                 +============================+ */
/*                 |          Packet 1          | */
/*                 |----------------------------| */
/*                 |          Packet 2          | */
/*                 |----------------------------| */
/*                 |              .             | */
/*                 |              .             | */
/*                 |              .             | */
/*                 |----------------------------| */
/*                 |          Packet N          | */
/*                 +============================+ */
/*                 |      Reference Values      | */
/*                 +============================+ */
/*                 |      Packet Directory      | */
/*                 +============================+ */
/*                 |    Reference  Directory    | */
/*                 +============================+ */
/*                 |       Reserved  Area       | */
/*                 +============================+ */
/*                 |     Segment Meta Data      | */
/*                 +----------------------------+ */

/*     Only the placement of the meta data at the end of a generic */
/*     segment is required. The other data partitions may occur in any */
/*     order in the generic segment because the meta data will contain */
/*     pointers to their appropriate locations within the generic */
/*     segment. */

/*     The meta data for a generic segment should only be obtained */
/*     through use of the subroutine SGMETA. The meta data should not be */
/*     written through any mechanism other than the ending of a generic */
/*     segment begun by SGBWFS or SGBWVS using SGWES. */

/* $ Restrictions */

/*     1) If new reference index types are added, the new type(s) should */
/*        be defined to be the consecutive integer(s) after the last */
/*        defined reference index type used. In this way a value for */
/*        the maximum allowed index type may be maintained. This value */
/*        must also be updated if new reference index types are added. */

/*     2) If new meta data items are needed, mnemonics for them must be */
/*        added to the end of the current list of mnemonics and before */
/*        the NMETA mnemonic. In this way compatibility with files having */
/*        a different, but smaller, number of meta data items may be */
/*        maintained. See the description and example below. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */
/*     K.R. Gehringer    (JPL) */
/*     W.L. Taber        (JPL) */
/*     F.S. Turner       (JPL) */

/* $ Literature_References */

/*     Generic Segments Required Reading. */
/*     DAF Required Reading. */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 28-JAN-2004 (NJB) */

/*        Header update: equations for comptutations of packet indices */
/*        for the cases of index types 0 and 1 were corrected. */

/* -    SPICELIB Version 1.1.0, 25-09-98 (FST) */

/*        Added parameter MNMETA, the minimum number of meta data items */
/*        that must be present in a generic DAF segment. */

/* -    SPICELIB Version 1.0.0, 04-03-95 (KRG) (WLT) */

/* -& */

/*     Mnemonics for the type of reference value index. */

/*     Two forms of indexing are provided: */

/*        1) An implicit form of indexing based on using two values, a */
/*           starting value, which will have an index of 1, and a step */
/*           size between reference values, which are used to compute an */
/*           index and a reference value associated with a specified key */
/*           value. See the descriptions of the implicit types below for */
/*           the particular formula used in each case. */

/*        2) An explicit form of indexing based on a reference value for */
/*           each data packet. */


/*     Reference Index Type 0 */
/*     ---------------------- */

/*     Implied index. The index and reference value of a data packet */
/*     associated with a specified key value are computed from the two */
/*     generic segment reference values using the formula below. The two */
/*     generic segment reference values, REF(1) and REF(2), represent, */
/*     respectively, a starting value and a step size between reference */
/*     values. The index of the data packet associated with a key value */
/*     of VALUE is given by: */

/*                          /    VALUE - REF(1)    \ */
/*        INDEX = 1  +  INT | -------------------- | */
/*                          \        REF(2)        / */

/*     and the reference value associated with VALUE is given by: */

/*        REFVAL = REF(1) + DBLE (INDEX-1) * REF(2) */


/*     Reference Index Type 1 */
/*     ---------------------- */

/*     Implied index. The index and reference value of a data packet */
/*     associated with a specified key value are computed from the two */
/*     generic segment reference values using the formula below. The two */
/*     generic segment reference values, REF(1) and REF(2), represent, */
/*     respectively, a starting value and a step size between reference */
/*     values. The index of the data packet associated with a key value */
/*     of VALUE is given by: */

/*                          /          VALUE - REF(1)    \ */
/*        INDEX = 1  +  INT | 0.5 + -------------------- | */
/*                          \              REF(2)        / */


/*     and the reference value associated with VALUE is given by: */

/*        REFVAL = REF(1) + DBLE (INDEX-1) * REF(2) */

/*     We get the larger index in the event that VALUE is halfway between */
/*     X(I) and X(I+1), where X(I) = BUFFER(1) + DBLE (I-1) * REFDAT(2). */


/*     Reference Index Type 2 */
/*     ---------------------- */

/*     Explicit index. In this case the number of packets must equal the */
/*     number of reference values. The index of the packet associated */
/*     with a key value of VALUE is the index of the last reference item */
/*     that is strictly less than VALUE. The reference values must be in */
/*     ascending order, REF(I) < REF(I+1). */


/*     Reference Index Type 3 */
/*     ---------------------- */

/*     Explicit index. In this case the number of packets must equal the */
/*     number of reference values. The index of the packet associated */
/*     with a key value of VALUE is the index of the last reference item */
/*     that is less than or equal to VALUE. The reference values must be */
/*     in ascending order, REF(I) < REF(I+1). */


/*     Reference Index Type 4 */
/*     ---------------------- */

/*     Explicit index. In this case the number of packets must equal the */
/*     number of reference values. The index of the packet associated */
/*     with a key value of VALUE is the index of the reference item */
/*     that is closest to the value of VALUE. In the event of a "tie" */
/*     the larger index is selected. The reference values must be in */
/*     ascending order, REF(I) < REF(I+1). */


/*     These parameters define the valid range for the index types. An */
/*     index type code, MYTYPE, for a generic segment must satisfy the */
/*     relation MNIDXT <= MYTYPE <= MXIDXT. */


/*     The following meta data items will appear in all generic segments. */
/*     Other meta data items may be added if a need arises. */

/*       1)  CONBAS  Base Address of the constants in a generic segment. */

/*       2)  NCON    Number of constants in a generic segment. */

/*       3)  RDRBAS  Base Address of the reference directory for a */
/*                   generic segment. */

/*       4)  NRDR    Number of items in the reference directory of a */
/*                   generic segment. */

/*       5)  RDRTYP  Type of the reference directory 0, 1, 2 ... for a */
/*                   generic segment. */

/*       6)  REFBAS  Base Address of the reference items for a generic */
/*                   segment. */

/*       7)  NREF    Number of reference items in a generic segment. */

/*       8)  PDRBAS  Base Address of the Packet Directory for a generic */
/*                   segment. */

/*       9)  NPDR    Number of items in the Packet Directory of a generic */
/*                   segment. */

/*      10)  PDRTYP  Type of the packet directory 0, 1, ... for a generic */
/*                   segment. */

/*      11)  PKTBAS  Base Address of the Packets for a generic segment. */

/*      12)  NPKT    Number of Packets in a generic segment. */

/*      13)  RSVBAS  Base Address of the Reserved Area in a generic */
/*                   segment. */

/*      14)  NRSV    Number of items in the reserved area of a generic */
/*                   segment. */

/*      15)  PKTSZ   Size of the packets for a segment with fixed width */
/*                   data packets or the size of the largest packet for a */
/*                   segment with variable width data packets. */

/*      16)  PKTOFF  Offset of the packet data from the start of a packet */
/*                   record. Each data packet is placed into a packet */
/*                   record which may have some bookkeeping information */
/*                   prepended to the data for use by the generic */
/*                   segments software. */

/*      17)  NMETA   Number of meta data items in a generic segment. */

/*     Meta Data Item  1 */
/*     ----------------- */


/*     Meta Data Item  2 */
/*     ----------------- */


/*     Meta Data Item  3 */
/*     ----------------- */


/*     Meta Data Item  4 */
/*     ----------------- */


/*     Meta Data Item  5 */
/*     ----------------- */


/*     Meta Data Item  6 */
/*     ----------------- */


/*     Meta Data Item  7 */
/*     ----------------- */


/*     Meta Data Item  8 */
/*     ----------------- */


/*     Meta Data Item  9 */
/*     ----------------- */


/*     Meta Data Item 10 */
/*     ----------------- */


/*     Meta Data Item 11 */
/*     ----------------- */


/*     Meta Data Item 12 */
/*     ----------------- */


/*     Meta Data Item 13 */
/*     ----------------- */


/*     Meta Data Item 14 */
/*     ----------------- */


/*     Meta Data Item 15 */
/*     ----------------- */


/*     Meta Data Item 16 */
/*     ----------------- */


/*     If new meta data items are to be added to this list, they should */
/*     be added above this comment block as described below. */

/*        INTEGER               NEW1 */
/*        PARAMETER           ( NEW1   = PKTOFF + 1 ) */

/*        INTEGER               NEW2 */
/*        PARAMETER           ( NEW2   = NEW1   + 1 ) */

/*        INTEGER               NEWEST */
/*        PARAMETER           ( NEWEST = NEW2   + 1 ) */

/*     and then the value of NMETA must be changed as well to be: */

/*        INTEGER               NMETA */
/*        PARAMETER           ( NMETA  = NEWEST + 1 ) */

/*     Meta Data Item 17 */
/*     ----------------- */


/*     Maximum number of meta data items. This is always set equal to */
/*     NMETA. */


/*     Minimum number of meta data items that must be present in a DAF */
/*     generic segment.  This number is to remain fixed even if more */
/*     meta data items are added for compatibility with old DAF files. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   The handle of an SPK file open for writing. */
/*     SEGID      I   The string to use for segment identifier. */
/*     BODY       I   The NAIF ID code for the body of the segment. */
/*     CENTER     I   The center of motion for BODY. */
/*     FRAME      I   The reference frame for this segment. */
/*     FIRST      I   The first epoch for which the segment is valid. */
/*     LAST       I   The last epoch for which the segment is valid. */
/*     CHBDEG     I   The degree of the Chebyshev Polynomial used. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of an SPK file that has been */
/*              opened for writing. */

/*     SEGID    is the segment identifier. An SPK segment identifier */
/*              may contain up to 40 printing ASCII characters. */

/*     BODY     is the SPICE ID for the body whose states are */
/*              to be recorded in an SPK file. */

/*     CENTER   is the SPICE ID for the center of motion associated */
/*              with BODY. */

/*     FRAME    is the reference frame that states are referenced to, */
/*              for example 'J2000'. */

/*     FIRST    is the starting epoch, in seconds past J2000, for */
/*              the ephemeris data to be placed into the segment. */

/*     LAST     is the ending epoch, in seconds past J2000, for */
/*              the ephemeris data to be placed into the segment. */

/*     CHBDEG   is the degree of the Chebyshev Polynomials used to */
/*              represent the ephemeris information stored in the */
/*              segment. */

/* $ Detailed_Output */

/*     None.    The input data is used to create the segment summary for */
/*              the segment being started in the SPK file associated with */
/*              HANDLE. */

/*              See the $ Particulars section for details about the */
/*              structure of a type 14 SPK segment. */

/* $ Parameters */

/*     This subroutine makes use of parameters defined in the file */
/*     'sgparam.inc'. */

/* $ Exceptions */

/*     1) If the degree of the Chebyshev Polynomial to be used for this */
/*        segment is negative, the error SPICE(INVALIDARGUMENT) will */
/*        be signaled. */

/*     2) Errors in the structure or content of the inputs other than the */
/*        degree of the Chebyshev Polynomial are diagnosed by routines */
/*        called by this one. */

/*     3) File access errors are diagnosed by routines in the call tree */
/*        of this routine. */

/* $ Files */

/*      See HANDLE in the $ Detailed_Input section. */

/* $ Particulars */

/*     This routine begins writing a type 14 SPK segment to the open SPK */
/*     file that is associated with HANDLE. The file must have been */
/*     opened with write access. */

/*     This routine is one of a set of three routines for creating and */
/*     adding data to type 14 SPK segments. These routines are: */

/*        SPK14B: Begin a type 14 SPK segment. This routine must be */
/*                called before any data may be added to a type 14 */
/*                segment. */

/*        SPK14A: Add data to a type 14 SPK segment. This routine may be */
/*                called any number of times after a call to SPK14B to */
/*                add type 14 records to the SPK segment that was */
/*                started. */

/*        SPK14E: End a type 14 SPK segment. This routine is called to */
/*                make the type 14 segment a permanent addition to the */
/*                SPK file. Once this routine is called, no further type */
/*                14 records may be added to the segment. A new segment */
/*                must be started. */

/*     A type 14 SPK segment consists of coefficient sets for fixed order */
/*     Chebyshev polynomials over consecutive time intervals, where the */
/*     time intervals need not all be of the same length. The Chebyshev */
/*     polynomials represent the position, X, Y, and Z coordinates, and */
/*     the velocities, dX/dt, dY/dt, and dZ/dt, of BODY relative to */
/*     CENTER. */

/*     The ephemeris data supplied to the type 14 SPK writer is packed */
/*     into an array as a sequence of records, */

/*        ----------------------------------------------------- */
/*        | Record 1 | Record 2 | ... | Record N-1 | Record N | */
/*        ----------------------------------------------------- */

/*     with each record has the following format. */

/*           ------------------------------------------------ */
/*           |  The midpoint of the approximation interval  | */
/*           ------------------------------------------------ */
/*           |  The radius of the approximation interval    | */
/*           ------------------------------------------------ */
/*           |  CHBDEG+1 coefficients for the X coordinate  | */
/*           ------------------------------------------------ */
/*           |  CHBDEG+1 coefficients for the Y coordinate  | */
/*           ------------------------------------------------ */
/*           |  CHBDEG+1 coefficients for the Z coordinate  | */
/*           ------------------------------------------------ */
/*           |  CHBDEG+1 coefficients for the X velocity    | */
/*           ------------------------------------------------ */
/*           |  CHBDEG+1 coefficients for the Y velocity    | */
/*           ------------------------------------------------ */
/*           |  CHBDEG+1 coefficients for the Z velocity    | */
/*           ------------------------------------------------ */

/* $ Examples */

/*     Assume we have the following for each of the examples that */
/*     follow. */

/*        HANDLE   is the handle of an SPK file opened with write */
/*                 access. */

/*        SEGID    is a character string of no more than 40 characters */
/*                 which provides a pedigree for the data in the SPK */
/*                 segment we will create. */

/*        BODY     is the SPICE ID code for the body whose ephemeris */
/*                 is to be placed into the file. */

/*        CENTER   is the center of motion for the ephemeris of BODY. */

/*        REFFRM   is the name of the SPICE reference frame for the */
/*                 ephemeris. */

/*        FIRST    is the starting epoch, in seconds past J2000, for */
/*                 the ephemeris data to be placed into the segment. */

/*        LAST     is the ending epoch, in seconds past J2000, for */
/*                 the ephemeris data to be placed into the segment. */

/*     Example 1: */

/*        For this example, we also assume that: */

/*           N        is the number of type 14 records that we want to */
/*                    put into a segment in an SPK file. */

/*           RECRDS   contains N type 14 records packaged for the SPK */
/*                    file. */

/*           ETSTRT   contains the initial epochs for each of the */
/*                    records contained in RECRDS, where */

/*                       ETSTRT(I) < ETSTRT(I+1), I = 1, N-1 */

/*                       ETSTRT(1) <= FIRST, ETSTRT(N) < LAST */

/*                       ETSTRT(I+1), I = 1, N-1, is the ending epoch for */
/*                       record I as well as the initial epoch for record */
/*                       I+1. */

/*        Then the following code fragment demonstrates how to create a */
/*        type 14 SPK segment if all of the data for the segment is */
/*        available at one time. */

/*        C */
/*        C     Begin the segment. */
/*        C */
/*              CALL SPK14B ( HANDLE, SEGID, BODY, CENTER, REFFRM, */
/*             .              FIRST,  LAST,  CHBDEG                ) */
/*        C */
/*        C     Add the data to the segment all at once. */
/*        C */
/*              CALL SPK14A ( HANDLE, N, RECRDS, ETSTRT ) */
/*        C */
/*        C     End the segment, making the segment a permanent addition */
/*        C     to the SPK file. */
/*        C */
/*              CALL SPK14E ( HANDLE ) */

/*     Example 2: */

/*        In this example we want to add type 14 SPK records, as */
/*        described above in the $ Particulars section, to the segment */
/*        being written as they are generated.  The ability to write the */
/*        records in this way is useful if computer memory is limited. It */
/*        may also be convenient from a programming perspective to write */
/*        the records one at a time. */

/*        For this example, assume that we want to generate N type 14 SPK */
/*        records, one for each of N time intervals, writing them all to */
/*        the same segment in the SPK file. Let */

/*           N        be the number of type 14 records that we want to */
/*                    generate and put into a segment in an SPK file. */

/*           RECORD   be an array with enough room to hold a single type */
/*                    14 record, i.e. RECORD should have dimension at */
/*                    least 6 * (CHBDEG + 1 ) + 2. */

/*           START    be an array of N times that are the beginning */
/*                    epochs for each of the intervals of interest. The */
/*                    times should be in increasing order and the start */
/*                    time for the first interval should equal the */
/*                    starting time for the segment. */

/*                       START(I) < START(I+1), I = 1, N-1 */

/*                       START(1) = FIRST */

/*           STOP     be an array of N times that are the ending epochs */
/*                    for each of the intervals of interest. The times */
/*                    should be in increasing order and the stop time for */
/*                    interval I should equal the start time for interval */
/*                    I+1, i.e., we want to have continuous coverage in */
/*                    time across all of the records. Also, the stop time */
/*                    for the last interval should equal the ending time */
/*                    for the segment. */

/*                       STOP(I) < STOP(I+1), I = 1, N-1 */

/*                       STOP(I) = START(I+1), I = 1, N-1 */

/*                       STOP(N) = LAST */

/*           GENREC( TIME1, TIME2, RECORD ) */

/*                    be a subroutine that generates a type 14 SPK record */
/*                    for a time interval specified by TIME1 and TIME2. */

/*        Then the following code fragment demonstrates how to create a */
/*        type 14 SPK segment if all of the data for the segment is not */
/*        available at one time. */

/*        C */
/*        C     Begin the segment. */
/*        C */
/*              CALL SPK14B ( HANDLE, SEGID, BODY, CENTER, REFFRM, */
/*             .              FIRST,  LAST,  CHBDEG                ) */

/*        C */
/*        C     Generate the records and write them to the segment in the */
/*        C     SPK file one at at time. */
/*        C */
/*              DO I = 1, N */

/*                 CALL GENREC ( START(I), STOP(I), RECORD ) */
/*                 CALL SPK14A ( HANDLE, 1, RECORD, START(I) ) */

/*              END DO */

/*        C */
/*        C     End the segment, making the segment a permanent addition */
/*        C     to the SPK file. */
/*        C */
/*              CALL SPK14E ( HANDLE ) */

/* $ Restrictions */

/*     The SPK file must be open with write access. */

/*     Only one segment may be written to a particular SPK file at a */
/*     time. All of the data for the segment must be written and the */
/*     segment must be ended before another segment may be started in */
/*     the file. */

/* $ Literature_References */

/*      None. */

/* $ Author_and_Institution */

/*      K.R. Gehringer      (JPL) */
/*      B.V. Semenov        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 10-FEB-2014 (BVS) */

/*        Removed comments from the Declarations section. */

/* -    SPICELIB Version 1.0.1, 30-OCT-2006 (BVS) */

/*        Deleted "inertial" from the FRAME description in the Brief_I/O */
/*        section of the header. */

/* -    SPICELIB Version 1.0.0, 06-MAR-1995 (KRG) */

/* -& */
/* $ Index_Entries */

/*     begin writing a type_14 spk segment */

/* -& */

/*     SPICELIB functions */


/*     Local Parameters */

/*     DAF ND and NI values for SPK files. */


/*     Length of an SPK descriptor. */


/*     Length of a state. */


/*     The type of this segment */


/*     The number of constants: */


/*     Local variables */


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPK14B", (ftnlen)6);
    }

/*     First, check the degree of the polynomial to be sure that it is */
/*     not negative. */

    if (*chbdeg < 0) {
	setmsg_("The degree of the Chebyshev Polynomial was negative, #. The"
		" degree of the polynomial must be greater than or equal to z"
		"ero.", (ftnlen)123);
	errint_("#", chbdeg, (ftnlen)1);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("SPK14B", (ftnlen)6);
	return 0;
    }

/*     Create a descriptor for the segment we are about to write. */

    spkpds_(body, center, frame, &c__14, first, last, descr, frame_len);
    if (failed_()) {
	chkout_("SPK14B", (ftnlen)6);
	return 0;
    }

/*     We've got a valid descriptor, so compute a few things and begin */
/*     the segment. */

    ncoeff = *chbdeg + 1;
    pktsiz = ncoeff * 6 + 2;
    dcoeff = (doublereal) ncoeff;

/*     For this data type, we want to use an explicit reference value */
/*     index where the reference epochs are in increasing order. We also */
/*     want to have as the index for a particular request epoch the index */
/*     of the greatest reference epoch less than or equal to the request */
/*     epoch. These characteristics are prescribed by the mnemonic EXPLE. */
/*     See the include file 'sgparam.inc' for more details. */

    sgbwfs_(handle, descr, segid, &c__1, &dcoeff, &pktsiz, &c__3, segid_len);

/*     No need to check FAILED() here, since all we do is check out. */
/*     Leave it up to the caller. */

    chkout_("SPK14B", (ftnlen)6);
    return 0;
} /* spk14b_ */

