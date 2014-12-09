/* sctype.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;

/* $Procedure      SCTYPE ( SCLK type ) */
integer sctype_(integer *sc)
{
    /* Initialized data */

    static logical first = TRUE_;
    static logical nodata = TRUE_;
    static integer oldsc = 0;

    /* System generated locals */
    integer ret_val, i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer type__;
    extern /* Subroutine */ int zzcvpool_(char *, integer *, logical *, 
	    ftnlen), zzctruin_(integer *);
    integer n;
    extern /* Subroutine */ int scli01_(char *, integer *, integer *, integer 
	    *, integer *, ftnlen), chkin_(char *, ftnlen), repmi_(char *, 
	    char *, integer *, char *, ftnlen, ftnlen, ftnlen);
    extern logical failed_(void);
    char kvname[60];
    logical update;
    extern /* Subroutine */ int chkout_(char *, ftnlen), suffix_(char *, 
	    integer *, char *, ftnlen, ftnlen);
    extern logical return_(void);
    static integer usrctr[2];
    extern /* Subroutine */ int swpool_(char *, integer *, char *, ftnlen, 
	    ftnlen);

/* $ Abstract */

/*     Return the spacecraft clock type for a specified spacecraft. */

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

/*     TIME */

/* $ Declarations */
/* $ Abstract */

/*     This include file defines the dimension of the counter */
/*     array used by various SPICE subsystems to uniquely identify */
/*     changes in their states. */

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

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to uniquely identify */
/*                 changes in their states. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */

/*     End of include file. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     SC         I   NAIF spacecraft ID code. */

/*     The function returns the spacecraft clock type associated with the */
/*     spacecraft specified by SC. */

/* $ Detailed_Input */

/*     SC             is a NAIF ID code for a spacecraft, whose */
/*                    spacecraft clock `type' is desired. */

/* $ Detailed_Output */

/*     The function returns the spacecraft clock type associated with the */
/*     spacecraft specified by SC. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the kernel variable that assigns a SCLK type to the */
/*         spacecraft specified by SC is not found in the kernel pool, */
/*         the error is diagnosed by routines called by this routine. */
/*         SCTYPE returns the value 0 in this case. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The raison d'etre of this routine is that it consolidates the code */
/*     that maps spacecraft ID's to clock types.  While any routine may */
/*     call SCTYPE, it is unlikely that there will be a need for */
/*     non-SPICELIB routines to call this routine directly. */

/* $ Examples */

/*     1)  Find the SCLK type for Galileo. */

/*            During program initialization, we load a SCLK kernel file */
/*            into the kernel pool.  We will pretend that the name of */
/*            this file is GLLSCLK.KER.  You must use the actual name of */
/*            an SCLK kernel that is accessible by your program to try */
/*            this example. */

/*                C */
/*                C     Load the SCLK kernel. */
/*                C */
/*                      CALL FURNSH ( 'GLLSCLK.KER' ) */
/*                                 . */
/*                                 . */
/*                                 . */
/*                C */
/*                C     Print out the clock type for Galileo. */
/*                C */
/*                      TYPE = SCTYPE ( -77 ) */

/*                      PRINT *, 'Galileo clock type is ', TYPE */


/*     2)  Find the SCLK type for Mars Observer. */


/*                C */
/*                C     Load the SCLK kernel. */
/*                C */
/*                      CALL FURNSH ( 'MOSCLK.KER' ) */
/*                                 . */
/*                                 . */
/*                                 . */
/*                C */
/*                C     Print out the clock type for Mars Observer. */
/*                C */
/*                      TYPE = SCTYPE ( -94 ) */

/*                      PRINT *, 'Mars Observer clock type is ', TYPE */

/* $ Restrictions */

/*     This routine assumes that an SCLK kernel appropriate to the */
/*     spacecraft specified by SC has been loaded into the kernel pool. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     J.M. Lynch     (JPL) */
/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 09-SEP-2013 (BVS) */

/*        Updated to keep track of the POOL counter and call ZZCVPOOL. */

/* -    SPICELIB Version 1.2.0, 05-MAR-2009 (NJB) */

/*        Bug fix: this routine now keeps track of whether its */
/*        kernel pool look-up succeeded. If not, a kernel pool */
/*        lookup is attempted on the next call to this routine. */

/* -    SPICELIB Version 1.1.1, 22-AUG-2006 (EDW) */

/*        Replaced references to LDPOOL with references */
/*        to FURNSH. */

/* -    SPICELIB Version 1.1.0, 22-MAR-1993 (JML) */

/*        1) The routine now uses the kernel pool watch capability. */

/*        2) The routine now returns a value of zero if RETURN is */
/*           true on entry. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 04-SEP-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     spacecraft_clock type */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	ret_val = 0;
	return ret_val;
    }
    chkin_("SCTYPE", (ftnlen)6);

/*     On the first pass through the subroutine, or if the spacecraft */
/*     ID code changes, set a watch on the SCLK kernel variable for */
/*     the current clock type. */

    if (first || *sc != oldsc) {

/*        Construct the name of the kernel variable that is needed. */

	s_copy(kvname, "SCLK_DATA_TYPE", (ftnlen)60, (ftnlen)14);
	suffix_("_#", &c__0, kvname, (ftnlen)2, (ftnlen)60);
	i__1 = -(*sc);
	repmi_(kvname, "#", &i__1, kvname, (ftnlen)60, (ftnlen)1, (ftnlen)60);

/*        Set a watch on the kernel variable needed. */

	swpool_("SCTYPE", &c__1, kvname, (ftnlen)6, (ftnlen)60);

/*        Keep track of the last spacecraft ID encountered. */

	oldsc = *sc;

/*        Initialize the local POOL counter to user value. */

	zzctruin_(usrctr);
	first = FALSE_;
    }

/*     If the kernel pool variable that this routine uses has */
/*     been updated, or if the spacecraft id code changes, look */
/*     up the new value from the kernel pool. */

    zzcvpool_("SCTYPE", usrctr, &update, (ftnlen)6);
    if (update || nodata) {

/*        Find the clock type for the specified mission. */

	type__ = 0;
	scli01_("SCLK_DATA_TYPE", sc, &c__1, &n, &type__, (ftnlen)14);
	if (failed_()) {
	    nodata = TRUE_;
	    ret_val = 0;
	    chkout_("SCTYPE", (ftnlen)6);
	    return ret_val;
	}
	nodata = FALSE_;
    }
    ret_val = type__;
    chkout_("SCTYPE", (ftnlen)6);
    return ret_val;
} /* sctype_ */

