/* spk14e.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      SPK14E ( SPK type 14: End a segment. ) */
/* Subroutine */ int spk14e_(integer *handle)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen), sgwes_(integer *), 
	    chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     End the type 14 SPK segment currently being written to the SPK */
/*     file associated with HANDLE. See also SPK14B and SPK14E. */

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

/*     SPK */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   The handle of an SPK file open for writing. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of an SPK file that has been */
/*              opened for writing, and to which a type 14 segment is */
/*              being written. */

/* $ Detailed_Output */

/*     None.    The type 14 segment in the SPK file associated with */
/*              HANDLE will be ended, making the addition of the data */
/*              to the file permanent. */

/*              See the $ Particulars section for details about the */
/*              structure of a type 14 SPK segment. */

/* $ Parameters */

/*     None. */

/* $ Particulars */

/*     This routine ends a type 14 SPK segment which is being written to */
/*     the SPK file associated with HANDLE. Ending the SPK segment is a */
/*     necessary step in the process of making the data a permanent part */
/*     of the SPK file. */

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
/*     into an array as a sequence of logical records, */

/*        ----------------------------------------------------- */
/*        | Record 1 | Record 2 | ... | Record N-1 | Record N | */
/*        ----------------------------------------------------- */

/*     with each record has the following format. */

/*           ------------------------------------------------ */
/*           |  the midpoint of the approximation interval  | */
/*           ------------------------------------------------ */
/*           |   the radius of the approximation interval   | */
/*           ------------------------------------------------ */
/*           |  CHBDEG+1 coefficients for the X coordinate  | */
/*           ------------------------------------------------ */
/*           |  CHBDEG+1 coefficients for the Y coordinate  | */
/*           ------------------------------------------------ */
/*           |  CHBDEG+1 coefficients for the Z coordinate  | */
/*           ------------------------------------------------ */
/*           |   CHBDEG+1 coefficients for the X velocity   | */
/*           ------------------------------------------------ */
/*           |   CHBDEG+1 coefficients for the Y velocity   | */
/*           ------------------------------------------------ */
/*           |   CHBDEG+1 coefficients for the Z velocity   | */
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

/*        REFFRM   is the name of the SPICE inertial reference frame */
/*                 for the ephemeris. */

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
/*             .              FIRST, LAST, CHBDEG                  ) */

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

/*     1) The type 14 SPK segment being closed must have been started by */
/*        the routine SPK14B, the routine which begins a type 14 SPK */
/*        segment. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     See the argument HANDLE. */

/* $ Author_and_Institution */

/*     K.R. Gehringer      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 06-MAR-1995 (KRG) */

/* -& */
/* $ Index_Entries */

/*     end a type_14 spk segment */

/* -& */

/*     Spicelib functions */


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPK14E", (ftnlen)6);
    }

/*     This is simple, just call the routine which ends a generic */
/*     segment. */

    sgwes_(handle);

/*     No need to check FAILED() here, since all we do is check out. */
/*     Leave it up to the caller. */
    chkout_("SPK14E", (ftnlen)6);
    return 0;
} /* spk14e_ */

