/* spkw10.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__10 = 10;
static integer c__8 = 8;
static integer c__14 = 14;
static integer c__4 = 4;
static integer c__1 = 1;

/* $Procedure      SPKW10 (SPK - write a type 10 segment ) */
/* Subroutine */ int spkw10_(integer *handle, integer *body, integer *center, 
	char *frame, doublereal *first, doublereal *last, char *segid, 
	doublereal *consts, integer *n, doublereal *elems, doublereal *epochs,
	 ftnlen frame_len, ftnlen segid_len)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer base;
    doublereal dnut[4];
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal descr[6];
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *),
	     sgwes_(integer *);
    integer npkts;
    extern logical failed_(void);
    doublereal packet[14];
    integer nepoch;
    extern /* Subroutine */ int sgbwfs_(integer *, doublereal *, char *, 
	    integer *, doublereal *, integer *, integer *, ftnlen), chkout_(
	    char *, ftnlen), sgwfpk_(integer *, integer *, doublereal *, 
	    integer *, doublereal *), spkpds_(integer *, integer *, char *, 
	    integer *, doublereal *, doublereal *, doublereal *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int zzwahr_(doublereal *, doublereal *);

/* $ Abstract */

/*     Write an SPK type 10 segment to the DAF open and attached to */
/*     the input HANDLE. */

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

/*       SPK */

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

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      HANDLE     I   The handle of a DAF file open for writing. */
/*      BODY       I   The NAIF ID code for the body of the segment. */
/*      CENTER     I   The center of motion for BODY. */
/*      FRAME      I   The reference frame for this segment. */
/*      FIRST      I   The first epoch for which the segment is valid. */
/*      LAST       I   The last  epoch for which the segment is valid. */
/*      SEGID      I   The string to use for segment identifier. */
/*      CONSTS     I   The array of geophysical constants for the segment */
/*      N          I   The number of element/epoch pairs to be stored */
/*      ELEMS      I   The collection of "two-line" element sets. */
/*      EPOCHS     I   The epochs associated with the element sets. */

/* $ Detailed_Input */

/*     HANDLE      is the file handle of an SPK file that has been */
/*                 opened for writing by SPCOPN, DAFOPN, or DAFOPW. */

/*     BODY        is the SPICE ID for the body whose states are */
/*                 to be recorded in an SPK file. */

/*     CENTER      is the SPICE ID for the center of motion associated */
/*                 with BODY. */

/*     FRAME       is the reference frame that states are referenced to, */
/*                 for example 'J2000'. */

/*     FIRST       are the bounds on the ephemeris times, expressed as */
/*     LAST        seconds past J2000, for which the states can be used */
/*                 to interpolate a state for BODY. */

/*     SEGID       is the segment identifier. An SPK segment identifier */
/*                 may contain up to 40 characters. */

/*     CONSTS      are the geophysical constants needed for evaluation */
/*                 of the two line elements sets.  The order of these */
/*                 constants must be: */

/*                 CONSTS(1) = J2 gravitational harmonic for earth */
/*                 CONSTS(2) = J3 gravitational harmonic for earth */
/*                 CONSTS(3) = J4 gravitational harmonic for earth */
/*                 CONSTS(4) = Square root of the GM for earth where GM */
/*                             is expressed in earth radii cubed per */
/*                             minutes squared */
/*                 CONSTS(5) = Equatorial radius of the earth in km */
/*                 CONSTS(6) = Low altitude bound for atmospheric */
/*                             model in km */
/*                 CONSTS(7) = High altitude bound for atmospheric */
/*                             model in km */
/*                 CONSTS(8) = Distance units/earth radius (normally 1) */

/*     N           is the number of "two-line" element sets  and epochs */
/*                 to be stored in the segment. */

/*     ELEMS       contains a time-ordered array of two-line elements */
/*                 as supplied in NORAD two-line element files.  The */
/*                 I'th set of elements should be stored as shown here: */

/*                    BASE = (I-1)*10 */

/*                    ELEMS ( BASE + 1 )  = NDT20 */
/*                    ELEMS ( BASE + 2 )  = NDD60 */
/*                    ELEMS ( BASE + 3 )  = BSTAR */
/*                    ELEMS ( BASE + 4 )  = INCL */
/*                    ELEMS ( BASE + 5 )  = NODE0 */
/*                    ELEMS ( BASE + 6 )  = ECC */
/*                    ELEMS ( BASE + 7 )  = OMEGA */
/*                    ELEMS ( BASE + 8 )  = MO */
/*                    ELEMS ( BASE + 9 )  = NO */
/*                    ELEMS ( BASE + 10 ) = EPOCH */

/*                 The meaning of these variables is defined by the */
/*                 format of the two-line element files available from */
/*                 NORAD */

/*     EPOCHS      contains the epochs (ephemeris seconds past J2000) */
/*                 corresponding to the elements in ELEMS.  The I'th */
/*                 epoch must equal the epoch of the I'th element set */
/*                 Epochs must form a strictly increasing sequence. */

/* $ Detailed_Output */

/*     None.       The data input is stored in an SPK segment in the */
/*                 DAF connected to the input HANDLE. */

/* $ Parameters */

/*     None. */

/* $ Particulars */

/*     This routine writes a type 10 SPK segment to the DAF open */
/*     for writing that is attached to HANDLE.  A routine, GETELM, that */
/*     reads two-line element data from files distributed by */
/*     NORAD is available from NAIF. */

/* $ Examples */

/*     Suppose that you have collected the two-line element data */
/*     and geophysical constants as prescribed above.  The following */
/*     code fragment demonstrates how you could go about creating */
/*     a type 10 SPK segment. */

/*        Open a new SPK file using DAF and get a file handle. */

/*        BODY   = <integer code for the body> */
/*        CENTER = <integer code for central body for the trajectory> */
/*        FRAME  = 'J2000' */
/*        SEGID  = <string that gives the bodies name> */

/*        FNAME  = 'SAMPLE.SPK' */
/*        ND     =  2 */
/*        NI     =  6 */
/*        IFNAME = 'SAMPLE SPK FILE FOR PRIVATE USE' */
/*        RESV   =  0 */

/*        CALL DAFONW ( FNAME, 'SPK', ND, NI, IFNAME, RESV, HANDLE ) */


/*        Add the type 10 data. */

/*        CALL SPKW10 ( HANDLE, BODY,   CENTER, FRAME,  FIRST, LAST, */
/*       .              SEGID,  CONSTS, N,      ELEMS,  EPOCHS      ) */

/*        Close the DAF properly. */

/*        CALL DAFCLS ( HANDLE ) */

/* $ Restrictions */

/*     None. */

/* $ Exceptions */

/*     1) Errors in the structure or content of the inputs are */
/*        diagnosed by routines called by this one. */

/*     2) File access errors are diagnosed by routines in the */
/*        call tree of this routine. */

/* $ Files */

/*      None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 2006-OCT-30 (BVS) */

/*        Deleted "inertial" from the FRAME description in the Brief_I/O */
/*        section of the header. */

/* -    SPICELIB Version 1.0.1, 1999-JUN-21 (WLT) */

/*        Cleaned up the header. */

/* -    SPICELIB Version 1.0.0, 1994-JAN-5 (WLT) */

/* -& */
/* $ Index_Entries */

/*     WRITE A TYPE 10 SPK SEGMENT */

/* -& */

/*     Spicelib functions */


/*     Local Variables */


/*     The type of this segment */


/*     The number of geophysical constants: */


/*     The number of elements per two-line set: */


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKW10", (ftnlen)6);

/*     First we need to create a descriptor for the segment */
/*     we are about to write. */

    spkpds_(body, center, frame, &c__10, first, last, descr, frame_len);
    if (failed_()) {
	chkout_("SPKW10", (ftnlen)6);
	return 0;
    }

/*     We've got a valid descriptor, write the data to a DAF */
/*     segment using the generic segment writer. */

    npkts = *n;
    nepoch = *n;
    sgbwfs_(handle, descr, segid, &c__8, consts, &c__14, &c__4, segid_len);
    i__1 = nepoch;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Move the elements into the next packet. */

	base = (i__ - 1) * 10;
	moved_(&elems[base], &c__10, packet);

/*        For each epoch, we need to get the nutation in obliquity, */
/*        nutation in longitude and mean obliquity. */

	zzwahr_(&epochs[i__ - 1], dnut);
	packet[11] = dnut[0];
	packet[10] = dnut[1];
	packet[13] = dnut[2];
	packet[12] = dnut[3];

/*        Now write the packet into the generic segment. */

	sgwfpk_(handle, &c__1, packet, &c__1, &epochs[i__ - 1]);
    }
    sgwes_(handle);
    chkout_("SPKW10", (ftnlen)6);
    return 0;
} /* spkw10_ */

