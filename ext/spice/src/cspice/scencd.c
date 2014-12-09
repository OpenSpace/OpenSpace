/* scencd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__9999 = 9999;

/* $Procedure      SCENCD ( Encode spacecraft clock ) */
/* Subroutine */ int scencd_(integer *sc, char *sclkch, doublereal *sclkdp, 
	ftnlen sclkch_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;
    doublereal d__1;

    /* Builtin functions */
    double d_nint(doublereal *);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    extern integer cpos_(char *, char *, integer *, ftnlen, ftnlen);
    integer part, i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    doublereal ticks;
    integer pnter;
    char error[25];
    doublereal pstop[9999];
    extern logical failed_(void);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), scpart_(integer *, 
	    integer *, doublereal *, doublereal *), chkout_(char *, ftnlen), 
	    nparsi_(char *, integer *, char *, integer *, ftnlen, ftnlen), 
	    sctiks_(integer *, char *, doublereal *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen);
    integer nparts;
    doublereal pstart[9999];
    extern logical return_(void);
    doublereal ptotls[9999];
    integer pos;

/* $ Abstract */

/*     Encode character representation of spacecraft clock time into a */
/*     double precision number. */

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

/*     SCLK */

/* $ Keywords */

/*     CONVERSION */
/*     TIME */

/* $ Declarations */
/* $ Abstract */

/*     Include file sclk.inc */

/*     SPICE private file intended solely for the support of SPICE */
/*     routines.  Users should not include this file directly due */
/*     to the volatile nature of this file */

/*     The parameters below define sizes and limits used by */
/*     the SCLK system. */

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

/* $ Parameters */

/*     See the declaration section below. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 24-MAY-2010 (NJB) */

/*        Increased value of maximum coefficient record count */
/*        parameter MXCOEF from 10K to 50K. */

/* -    SPICELIB Version 1.0.0, 11-FEB-2008 (NJB) */

/* -& */

/*     Number of supported SCLK field delimiters: */


/*     Supported SCLK string field delimiters: */


/*     Maximum number of partitions: */


/*     Partition string length. */

/*     Since the maximum number of partitions is given by MXPART is */
/*     9999, PRTSTR needs at most 4 characters for the partition number */
/*     and one character for the slash. */


/*     Maximum number of coefficient records: */


/*     Maximum number of fields in an SCLK string: */


/*     Length of strings used to represent D.P. */
/*     numbers: */


/*     Maximum number of supported parallel time systems: */


/*     End of include file sclk.inc */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     SC         I   NAIF spacecraft identification code. */
/*     SCLKCH     I   Character representation of a spacecraft clock. */
/*     SCLKDP     O   Encoded representation of the clock count. */
/*     MXPART     P   Maximum number of spacecraft clock partitions. */

/* $ Detailed_Input */

/*     SC         is the standard NAIF ID of the spacecraft whose clock's */
/*                time is being encoded. */

/*     SCLKCH     is the character representation of some spacecraft's */
/*                clock count. */

/*                SCLKCH will have the following general format: */

/*                             'pp/sclk_string', or just */
/*                                'sclk_string' */

/*                'pp' is an integer greater than or equal to one */
/*                and is called the partition number. */

/*                Each mission is divided into some number of partitions. */
/*                A new partition starts when the spacecraft clock */
/*                resets, either to zero, or to some other */
/*                value. Thus, the first partition for any mission */
/*                starts with launch, and ends with the first clock */
/*                reset. The second partition starts immediately when */
/*                the first stopped, and so on. */

/*                In order to be completely unambiguous about a */
/*                particular time, you need to specify a partition number */
/*                along with the standard clock string. */

/*                Information about when partitions occur for different */
/*                missions is contained in a spacecraft clock kernel */
/*                file, which needs to be loaded into the kernel pool, */
/*                using the routines CLPOOL and FURNSH. */

/*                The routine SCPART is used to read the partition */
/*                start and stop times, in encoded units of SCLK (called */
/*                "ticks" -- see SCLKDP below) from the kernel file. */

/*                If the partition number is included, it must be */
/*                separated from the rest of the string by a '/'. */
/*                Any number of spaces may separate the partition number, */
/*                the '/', and the rest of the clock string. */


/*                If the partition number is omitted, a default partition */
/*                will be assumed. The default partition is the lowest- */
/*                numbered partition that contains the given clock time. */
/*                If the clock time does not fall in any of the */
/*                partition boundaries then an error is signaled. */


/*                'sclk_string' is a spacecraft specific clock string. */
/*                Using Galileo as an example, the full format is */

/*                               wwwwwwww:xx:y:z */

/*                where z is a mod-8 counter (values 0-7) which */
/*                increments approximately once every 8 1/3 ms., y is a */
/*                mod-10 counter (values 0-9) which increments once */
/*                every time z turns over, i.e., approximately once every */
/*                66 2/3 ms., xx is a mod-91 (values 0-90) counter */
/*                which increments once every time y turns over, i.e., */
/*                once every 2/3 seconds. wwwwwwww is the Real-Time Image */
/*                Count (RIM), which increments once every time xx turns */
/*                over, i.e., once every 60 2/3 seconds. The roll-over */
/*                expression for the RIM is 16777215, which corresponds */
/*                to approximately 32 years. */

/*                wwwwwwww, xx, y, and z are referred to interchangeably */
/*                as the fields or components of the spacecraft clock. */
/*                SCLK components may be separated by any of these */
/*                five characters: ' '  ':'  ','  '-'  '.' */
/*                Any number of spaces can separate the components and */
/*                the delimiters. The presence of the RIM component */
/*                is required. Successive components may be omitted, and */
/*                in such cases are assumed to represent zero values. */

/*                Values for the individual components may exceed the */
/*                maximum expected values. For instance, '0:0:0:9' is */
/*                an acceptable Galileo clock string, and will convert */
/*                to the same number of ticks as '0:0:1:1'. */

/*                Consecutive delimiters containing no intervening digits */
/*                are treated as if they delimit zero components. */

/*                Trailing zeros should always be included to match the */
/*                length of the counter.  For example, a Galileo clock */
/*                count of '25684.90' should not be represented as */
/*                '25684.9'. */

/*                Some spacecraft clock components have offset, or */
/*                starting, values different from zero.  For example, */
/*                with an offset value of 1, a mod 20 counter would */
/*                cycle from 1 to 20 instead of from 0 to 19. */

/*                See the SCLK required reading for a detailed */
/*                description of the Voyager and Mars Observer clock */
/*                formats. */


/* $ Detailed_Output */

/*     SCLKDP     is the double precision encoding of SCLKCH. */

/*                The encoding is such that order and proximity will be */
/*                preserved. That is, if t1, t2, and t3 are spacecraft */
/*                clock times, and t1*, t2*, and t3* are their encodings, */
/*                then if */

/*                              t1 < t2 < t3, and */

/*                t2 is closer to t1 than to t3, you will have the result */
/*                that */

/*                             t1* < t2* < t3*, and */

/*                t2* is closer to t1* than to t3*. */

/*                The units of encoded SCLK are "ticks since the start of */
/*                the mission", where a "tick" is defined to be the */
/*                shortest time increment expressible by a particular */
/*                spacecraft's clock. */

/*                Each clock string without partition number represents */
/*                a certain number of ticks, but you need to include */
/*                partition information to determine the relative */
/*                position of that time in relation to the start of the */
/*                mission. */

/*                Since the end time of one partition is coincident */
/*                with the begin time of the next, there are two */
/*                different representations for this instant, and they */
/*                will both yield the same encoding. */

/*                For example, if partition 1 has an end time of t1, and */
/*                partition 2 has a begin time of t2, then if we did */

/*                   CALL SCENCD ( '1/t1', SC, X ) and */
/*                   CALL SCENCD ( '2/t2', SC, Y ), then */

/*                                  X = Y. */

/*                The individual routines TIKSnn, where nn is the */
/*                clock type code, contain more detailed information */
/*                on the conversion process. */

/* $ Parameters */

/*     MXPART     is the maximum number of spacecraft clock partitions */
/*                expected in the kernel file for any one spacecraft. */
/*                See the INCLUDE file sclk.inc for this parameter's */
/*                value. */

/* $ Exceptions */

/*     1) If the number of partitions in the kernel file for spacecraft */
/*        SC exceeds the parameter MXPART, the error */
/*        'SPICE(TOOMANYPARTS)' is signaled. */


/*     If a partition number is included in the SCLK string, the */
/*     following exceptions may occur: */

/*     2) If the partition number cannot be parsed as an integer, the */
/*        error 'SPICE(BADPARTNUMBER)' is signaled. */

/*     3) If the partition number is not in the range of the number of */
/*        partitions found in the kernel pool, the error */
/*        'SPICE(BADPARTNUMBER)' is signaled. */

/*     4) If the clock count does not fall in the boundaries of the */
/*        specified partition, the error 'SPICE(NOTINPART)' is */
/*        signaled. */


/*     If a partition number is not included in the SCLK string, the */
/*     following exception may occur. */

/*     5) If the clock count does not fall in the boundaries of any */
/*        partition found in the kernel pool, the error */
/*        'SPICE(NOPARTITION)' is signaled. */

/*     The following error is signaled by a routine called by SCENCD */

/*     6)  If any of the extracted clock components cannot be parsed as */
/*         integers, or the string has too many components, or the value */
/*         of one of the components is less than the offset value, then */
/*         the error SPICE(INVALIDSCLKSTRING) is signaled. */

/* $ Files */

/*     A kernel file containing spacecraft clock partition information */
/*     for the desired spacecraft must be loaded, using the routines */
/*     CLPOOL and FURNSH, before calling this routine. */

/* $ Particulars */

/*     In general, it is difficult to compare spacecraft clock counts */
/*     numerically since there are too many clock components for a */
/*     single comparison.  This routine provides a method of assigning a */
/*     single double precision number to a spacecraft's clock count, */
/*     given one of its character representations. */

/*     The routine SCDECD performs the inverse operation to SCENCD, */
/*     converting an encoded double precision number to character format. */

/*     To convert the string to ticks since the start of the mission, */
/*     SCENCD */

/*        1) Converts the non-partition portion of the string to */
/*           ticks, using the routine SCTIKS. */

/*        2) Determines the partition number for the clock time, */
/*           either by getting it directly from the input string, or */
/*           determining the default partition if none was specified. */

/*        3) Includes partition start and stop times, which are also */
/*           measured in ticks, to compute the number of ticks */
/*           since the beginning of the mission of the clock time. */

/* $ Examples */

/*      Double precision encodings of spacecraft clock counts are used to */
/*      tag pointing data in the C-kernel. */

/*      In the following example, pointing for a sequence of images from */
/*      the Voyager 2 narrow angle camera is requested from the C-kernel */
/*      using an array of character spacecraft clock counts as input. */
/*      The clock counts attached to the output are then decoded to */
/*      character and compared with the input strings. */

/*            CHARACTER*(25)     SCLKIN   ( 4 ) */
/*            CHARACTER*(25)     SCLKOUT */
/*            CHARACTER*(25)     CLKTOL */

/*            DOUBLE PRECISION   TIMEIN */
/*            DOUBLE PRECISION   TIMOUT */
/*            DOUBLE PRECISION   CMAT     ( 3, 3 ) */

/*            INTEGER            NPICS */
/*            INTEGER            SC */

/*            DATA  NPICS     /  4                   / */

/*            DATA  SCLKIN    / '2 / 20538:39:768', */
/*           .                  '2 / 20543:21:768', */
/*           .                  '2 / 20550:37', */
/*           .                  '2 / 20561:59'       / */

/*            DATA  CLKTOL   /  '      0:01:000'     / */

/*      C */
/*      C     The instrument we want pointing for is the Voyager 2 */
/*      C     narrow angle camera.  The reference frame we want is */
/*      C     J2000. The spacecraft is Voyager 2. */
/*      C */
/*            INST = -32001 */
/*            REF  = 'J2000' */
/*            SC   = -32 */

/*      C */
/*      C     Load the appropriate files. We need */
/*      C */
/*      C     1) CK file containing pointing data. */
/*      C     2) Spacecraft clock kernel file, for SCENCD and SCDECD. */
/*      C */
/*            CALL CKLPF  ( 'VGR2NA.CK' ) */
/*            CALL CLPOOL */
/*            CALL FURNSH ( 'SCLK.KER'  ) */

/*      C */
/*      C     Convert the tolerance string to ticks. */
/*      C */
/*            CALL SCTIKS ( SC, CLKTOL, TOL ) */

/*            DO I = 1, NPICS */

/*               CALL SCENCD ( SC, SCLKIN( I ), TIMEIN ) */

/*               CALL CKGP   ( INST, TIMEIN, TOL, REF, CMAT, TIMOUT, */
/*           .                 FOUND ) */

/*               CALL SCDECD ( SC, TIMOUT, SCLKOUT ) */

/*               WRITE (*,*) */
/*               WRITE (*,*) 'Input  s/c clock count: ', SCLKIN( I ) */
/*               WRITE (*,*) 'Output s/c clock count: ', SCLKOUT */
/*               WRITE (*,*) 'Output C-Matrix:        ', CMAT */
/*               WRITE (*,*) */

/*            END DO */

/*     The output from such a program might look like: */


/*            Input  s/c clock count:  2 / 20538:39:768 */
/*            Output s/c clock count:  2/20538:39:768 */
/*            Output C-Matrix:  'first C-matrix' */

/*            Input  s/c clock count:  2 / 20543:21:768 */
/*            Output s/c clock count:  2/20543:22:768 */
/*            Output C-Matrix:  'second C-matrix' */

/*            Input  s/c clock count:  2 / 20550:37 */
/*            Output s/c clock count:  2/20550:36:768 */
/*            Output C-Matrix:  'third C-matrix' */

/*            Input  s/c clock count:  2 / 20561:59 */
/*            Output s/c clock count:  2/20561:58:768 */
/*            Output C-Matrix:  'fourth C-matrix' */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman (JPL) */
/*     J.M. Lynch   (JPL) */
/*     R.E. Thurman (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 28-FEB-2014 (BVS) */

/*        Added FAILED checks to prevent passing uninitialized values to */
/*        ANINT, which can causing numeric exceptions on some */
/*        environments. */

/* -    SPICELIB Version 1.1.0, 05-FEB-2008 (NJB) */

/*        The values of the parameter MXPART is now */
/*        provided by the INCLUDE file sclk.inc. */

/* -    SPICELIB Version 1.0.2, 22-AUG-2006 (EDW) */

/*        Replaced references to LDPOOL with references */
/*        to FURNSH. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 03-SEP-1990 (JML) (RET) */

/* -& */
/* $ Index_Entries */

/*     encode spacecraft_clock */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SCENCD", (ftnlen)6);
    }

/*     Convert the non-partition portion of the clock string to ticks. */

    pos = cpos_(sclkch, "/", &c__1, sclkch_len, (ftnlen)1);
    i__1 = pos;
    sctiks_(sc, sclkch + i__1, &ticks, sclkch_len - i__1);
    if (failed_()) {
	chkout_("SCENCD", (ftnlen)6);
	return 0;
    }
    ticks = d_nint(&ticks);

/*     Read the partition start and stop times (in ticks) for this */
/*     mission. Error if there are too many of them. */

    scpart_(sc, &nparts, pstart, pstop);
    if (failed_()) {
	chkout_("SCENCD", (ftnlen)6);
	return 0;
    }
    if (nparts > 9999) {
	setmsg_("The number of partitions, #, for spacecraft # exceeds the v"
		"alue for parameter MXPART, #.", (ftnlen)88);
	errint_("#", &nparts, (ftnlen)1);
	errint_("#", sc, (ftnlen)1);
	errint_("#", &c__9999, (ftnlen)1);
	sigerr_("SPICE(TOOMANYPARTS)", (ftnlen)19);
	chkout_("SCENCD", (ftnlen)6);
	return 0;
    }

/*     PSTART and PSTOP represent integers but are read from the */
/*     kernel pool as double precision numbers. Make them whole */
/*     numbers so that logical tests may be performed with them. */

    i__1 = nparts;
    for (i__ = 1; i__ <= i__1; ++i__) {
	pstop[(i__2 = i__ - 1) < 9999 && 0 <= i__2 ? i__2 : s_rnge("pstop", 
		i__2, "scencd_", (ftnlen)500)] = d_nint(&pstop[(i__3 = i__ - 
		1) < 9999 && 0 <= i__3 ? i__3 : s_rnge("pstop", i__3, "scenc"
		"d_", (ftnlen)500)]);
	pstart[(i__2 = i__ - 1) < 9999 && 0 <= i__2 ? i__2 : s_rnge("pstart", 
		i__2, "scencd_", (ftnlen)501)] = d_nint(&pstart[(i__3 = i__ - 
		1) < 9999 && 0 <= i__3 ? i__3 : s_rnge("pstart", i__3, "scen"
		"cd_", (ftnlen)501)]);
    }
/*     For each partition, compute the total number of ticks in that */
/*     partition plus all preceding partitions. */

    d__1 = pstop[0] - pstart[0];
    ptotls[0] = d_nint(&d__1);
    i__1 = nparts;
    for (i__ = 2; i__ <= i__1; ++i__) {
	d__1 = ptotls[(i__3 = i__ - 2) < 9999 && 0 <= i__3 ? i__3 : s_rnge(
		"ptotls", i__3, "scencd_", (ftnlen)512)] + pstop[(i__4 = i__ 
		- 1) < 9999 && 0 <= i__4 ? i__4 : s_rnge("pstop", i__4, "sce"
		"ncd_", (ftnlen)512)] - pstart[(i__5 = i__ - 1) < 9999 && 0 <= 
		i__5 ? i__5 : s_rnge("pstart", i__5, "scencd_", (ftnlen)512)];
	ptotls[(i__2 = i__ - 1) < 9999 && 0 <= i__2 ? i__2 : s_rnge("ptotls", 
		i__2, "scencd_", (ftnlen)512)] = d_nint(&d__1);
    }

/*     Determine the partition number for the input clock string: */

/*        If it was included in the string make sure it's valid for */
/*        this mission. */

/*           Error if */

/*           1) The partition number can't be parsed. */
/*           2) The partition number is not in the range 1 to the number */
/*              of partitions. */
/*           3) The clock count does not fall in the boundaries of the */
/*              specified partition. */

/*        If it wasn't included, determine the default partition for */
/*        this clock count. */

/*           Error if */

/*           1) The clock count does not fall in the boundaries of any */
/*              of the partitions. */


    if (pos == 1) {
	setmsg_("Unable to parse the partition number from SCLK string #.", (
		ftnlen)56);
	errch_("#", sclkch, (ftnlen)1, sclkch_len);
	sigerr_("SPICE(BADPARTNUMBER)", (ftnlen)20);
	chkout_("SCENCD", (ftnlen)6);
	return 0;
    }
    if (pos > 1) {
	part = 0;
	nparsi_(sclkch, &part, error, &pnter, pos - 1, (ftnlen)25);
	if (s_cmp(error, " ", (ftnlen)25, (ftnlen)1) != 0) {
	    setmsg_("Unable to parse the partition number from SCLK string #."
		    , (ftnlen)56);
	    errch_("#", sclkch, (ftnlen)1, sclkch_len);
	    sigerr_("SPICE(BADPARTNUMBER)", (ftnlen)20);
	    chkout_("SCENCD", (ftnlen)6);
	    return 0;
	} else if (part <= 0 || part > nparts) {
	    setmsg_("Partition number # taken from SCLK string # is not in a"
		    "cceptable range 1 to #.", (ftnlen)78);
	    errint_("#", &part, (ftnlen)1);
	    errch_("#", sclkch, (ftnlen)1, sclkch_len);
	    errint_("#", &nparts, (ftnlen)1);
	    sigerr_("SPICE(BADPARTNUMBER)", (ftnlen)20);
	    chkout_("SCENCD", (ftnlen)6);
	    return 0;
	} else if (ticks < pstart[(i__1 = part - 1) < 9999 && 0 <= i__1 ? 
		i__1 : s_rnge("pstart", i__1, "scencd_", (ftnlen)575)] || 
		ticks > pstop[(i__2 = part - 1) < 9999 && 0 <= i__2 ? i__2 : 
		s_rnge("pstop", i__2, "scencd_", (ftnlen)575)]) {
	    setmsg_("SCLK count # does not fall in the boundaries of partiti"
		    "on number #.", (ftnlen)67);
	    errch_("#", sclkch, (ftnlen)1, sclkch_len);
	    errint_("#", &part, (ftnlen)1);
	    sigerr_("SPICE(NOTINPART)", (ftnlen)16);
	    chkout_("SCENCD", (ftnlen)6);
	    return 0;
	}
    } else {
	part = 1;
	while(part <= nparts && (ticks < pstart[(i__1 = part - 1) < 9999 && 0 
		<= i__1 ? i__1 : s_rnge("pstart", i__1, "scencd_", (ftnlen)
		592)] || ticks > pstop[(i__2 = part - 1) < 9999 && 0 <= i__2 ?
		 i__2 : s_rnge("pstop", i__2, "scencd_", (ftnlen)592)])) {
	    ++part;
	}
	if (part > nparts) {
	    setmsg_("SCLK count # does not fall in the boundaries of any of "
		    "the partitions for spacecraft #.", (ftnlen)87);
	    errch_("#", sclkch, (ftnlen)1, sclkch_len);
	    errint_("#", sc, (ftnlen)1);
	    sigerr_("SPICE(NOPARTITION)", (ftnlen)18);
	    chkout_("SCENCD", (ftnlen)6);
	    return 0;
	}
    }

/*     Now we have a valid partition number, and the number of ticks for */
/*     the clock string. To convert to ticks since the start of the */
/*     mission, add in the total number of ticks in preceding partitions */
/*     and subtract off the starting ticks value for this partition. */

    if (part > 1) {
	*sclkdp = ticks - pstart[(i__1 = part - 1) < 9999 && 0 <= i__1 ? i__1 
		: s_rnge("pstart", i__1, "scencd_", (ftnlen)622)] + ptotls[(
		i__2 = part - 2) < 9999 && 0 <= i__2 ? i__2 : s_rnge("ptotls",
		 i__2, "scencd_", (ftnlen)622)];
    } else {
	*sclkdp = ticks - pstart[(i__1 = part - 1) < 9999 && 0 <= i__1 ? i__1 
		: s_rnge("pstart", i__1, "scencd_", (ftnlen)624)];
    }
    chkout_("SCENCD", (ftnlen)6);
    return 0;
} /* scencd_ */

