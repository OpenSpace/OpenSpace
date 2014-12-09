/* kerman.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;
static integer c__20 = 20;
static integer c__0 = 0;
static integer c_n1 = -1;
static integer c__500 = 500;
static integer c__2 = 2;
static integer c__4 = 4;
static integer c__5 = 5;
static integer c__6 = 6;
static integer c__7 = 7;
static integer c__8 = 8;
static integer c__9 = 9;
static integer c__10 = 10;

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

/* Subroutine */ int kerman_0_(int n__, char *commnd, char *infile__, char *
	error, ftnlen commnd_len, ftnlen infile_len, ftnlen error_len)
{
    /* Initialized data */

    static integer nfiles = 0;
    static logical first = TRUE_;
    static char synval[80*9] = "                                            "
	    "                                    " "                         "
	    "                                                       " "      "
	    "                                                                "
	    "          " "                                                   "
	    "                             " "                                "
	    "                                                " "             "
	    "                                                                "
	    "   " "EK #word[ekfile]                                          "
	    "                      " "LEAPSECONDS #word[leapfile]            "
	    "                                         " "SCLK KERNEL #word[sc"
	    "lkfile]                                                     ";

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    static integer need;
    static char file[127], name__[32];
    static integer clen;
    extern logical have_(char *, ftnlen);
    static integer left, reqd, nseg;
    static char indx[4], pval[32*4];
    static integer hits;
    static char size[32], type__[32];
    static logical quit;
    extern /* Subroutine */ int zzeksinf_(integer *, integer *, char *, 
	    integer *, char *, integer *, ftnlen, ftnlen);
    static integer i__, j, k;
    extern integer cardc_(char *, ftnlen);
    static integer l, n;
    extern /* Subroutine */ int clgai_(integer *, char *, integer *, integer *
	    , ftnlen), clgac_(integer *, char *, char *, ftnlen, ftnlen);
    static integer r__;
    static char cname[80], break__[80];
    static integer headr[5];
    extern /* Subroutine */ int eklef_(char *, integer *, ftnlen), clnid_(
	    integer *, integer *, logical *);
    static integer space;
    extern logical match_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static integer tcode, ncomc;
    extern /* Subroutine */ int ekuef_(integer *);
    static char rname[6], tname[32];
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen), clnew_(char *, integer *, integer *, 
	    integer *, integer *, integer *, logical *, logical *, integer *, 
	    ftnlen);
    static logical found;
    static integer csize, ncols, ncomr;
    static logical cnull;
    static integer right, width[5], ctype;
    extern integer ltrim_(char *, ftnlen);
    static integer count;
    extern integer rtrim_(char *, ftnlen);
    static integer sizes[5];
    static char style[80];
    extern /* Subroutine */ int clnum_(integer *);
    static logical justr[5];
    extern /* Subroutine */ int m2chck_(char *, char *, integer *, char *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen), m2getc_(char *, char *, 
	    logical *, char *, ftnlen, ftnlen, ftnlen), m2ints_(integer *, 
	    char *, integer *, char *, ftnlen, ftnlen);
    static integer id, nb;
    static char bs[1];
    extern logical m2xist_(char *, ftnlen);
    static integer nh, sb, handle;
    static char ifname[60], tabnam[64], tabcol[80*506], rnamec[7], cnames[64*
	    100];
    static integer handls[20], segdsc[24];
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen), 
	    eknseg_(integer *);
    extern /* Subroutine */ int gcolmn_();
    extern integer isrchi_(integer *, integer *, integer *);
    extern /* Subroutine */ int pagput_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int nspwln_(char *, ftnlen);
    static char synkey[32*9];
    static integer synptr[9];
    static char ekfils[127*20], thisfl[127], messge[300], idword[8];
    static integer cdscrs[1100]	/* was [11][100] */, widest, totalc, nresvr, 
	    nresvc;
    static logical cindxd;
    static char spcial[4*5], lsttab[32];
    static integer colids[506], lmarge, ordvec[500];
    static logical presrv[5];
    extern /* Subroutine */ int replch_(char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen), prefix_(char *, integer *, char *
	    , ftnlen, ftnlen), chkout_(char *, ftnlen), expool_(char *, 
	    logical *, ftnlen), repmct_(char *, char *, integer *, char *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen), clunld_(integer *), 
	    ldpool_(char *, ftnlen);
    static integer nid;
    extern /* Subroutine */ int dasfnh_(char *, integer *, ftnlen);
    static integer col, seg, ids[5];
    extern /* Subroutine */ int remlac_(integer *, integer *, char *, integer 
	    *, ftnlen), nspglr_(integer *, integer *), nspmrg_(char *, ftnlen)
	    , suffix_(char *, integer *, char *, ftnlen, ftnlen), pagrst_(
	    void), pagset_(char *, integer *, ftnlen), ssizec_(integer *, 
	    char *, ftnlen), ssizei_(integer *, integer *), appndc_(char *, 
	    char *, ftnlen, ftnlen), appndi_(integer *, integer *), pagscn_(
	    char *, ftnlen), scolmn_(integer *, integer *, char *, ftnlen), 
	    tabrpt_(integer *, integer *, integer *, integer *, logical *, 
	    logical *, char *, integer *, integer *, U_fp, ftnlen), orderc_(
	    char *, integer *, integer *, ftnlen);
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int pagsft_(void), dasrfr_(integer *, char *, 
	    char *, integer *, integer *, integer *, integer *, ftnlen, 
	    ftnlen), nspshc_(integer *, logical *), bbputc_1__(char *, char *,
	     integer *, char *, ftnlen, ftnlen, ftnlen), nicepr_1__(char *, 
	    char *, S_fp, ftnlen, ftnlen);


/*     Version 2.4.0, 26-SEP-2005 */

/*        Minor bug fix: replaced FILE with INFILE in the RTRIM call */
/*        constructing "The file # is not listed ..." error message. */

/*     Version 2.3.0, 21-JUN-1999 */

/*        Added RETURN before first entry points. */

/*     Version 2.2.0, 22-APR-1997 */

/*        Declared PAGPUT external */

/*     Version 2.1.0  14-SEP-1995 */

/*        Variable INDEX removed. */

/*     Version 2.0.0  23-AUG-1995 */

/*        The widest string in a string column is no longer supplied */
/*        by the EK summary stuff.  We just set the value WIDEST */
/*        to 24. */


/*     This routine handles the loading of E-kernels, leapsecond and */
/*     SCLK kernels. */


/*     Passable routines */


/*     Parameters that contain the routine name for use in check-in, */
/*     check-out, and error messages. */


/*     SPICELIB functions */


/*     E-kernel functions */


/*     Meta/2 Functions */


/*     Interface to the SPICELIB error handling. */


/*     Ek include files. */

/* +============================================================== */
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


/*     Include Section:  EK Column Descriptor Parameters */

/*        ekcoldsc.inc Version 6    23-AUG-1995 (NJB) */


/*     Note:  The column descriptor size parameter CDSCSZ  is */
/*     declared separately in the include section CDSIZE$INC.FOR. */

/*     Offset of column descriptors, relative to start of segment */
/*     integer address range.  This number, when added to the last */
/*     integer address preceding the segment, yields the DAS integer */
/*     base address of the first column descriptor.  Currently, this */
/*     offset is exactly the size of a segment descriptor.  The */
/*     parameter SDSCSZ, which defines the size of a segment descriptor, */
/*     is declared in the include file eksegdsc.inc. */


/*     Size of column descriptor */


/*     Indices of various pieces of column descriptors: */


/*     CLSIDX is the index of the column's class code.  (We use the */
/*     word `class' to distinguish this item from the column's data */
/*     type.) */


/*     TYPIDX is the index of the column's data type code (CHR, INT, DP, */
/*     or TIME).  The type is actually implied by the class, but it */
/*     will frequently be convenient to look up the type directly. */



/*     LENIDX is the index of the column's string length value, if the */
/*     column has character type.  A value of IFALSE in this element of */
/*     the descriptor indicates that the strings have variable length. */


/*     SIZIDX is the index of the column's element size value.  This */
/*     descriptor element is meaningful for columns with fixed-size */
/*     entries.  For variable-sized columns, this value is IFALSE. */


/*     NAMIDX is the index of the base address of the column's name. */


/*     IXTIDX is the data type of the column's index.  IXTIDX */
/*     contains a type value only if the column is indexed. For columns */
/*     that are not indexed, the location IXTIDX contains the boolean */
/*     value IFALSE. */


/*     IXPIDX is a pointer to the column's index.  IXTPDX contains a */
/*     meaningful value only if the column is indexed.  The */
/*     interpretation of the pointer depends on the data type of the */
/*     index. */


/*     NFLIDX is the index of a flag indicating whether nulls are */
/*     permitted in the column.  The value at location NFLIDX is */
/*     ITRUE if nulls are permitted and IFALSE otherwise. */


/*     ORDIDX is the index of the column's ordinal position in the */
/*     list of columns belonging to the column's parent segment. */


/*     METIDX is the index of the column's integer metadata pointer. */
/*     This pointer is a DAS integer address. */


/*     The last position in the column descriptor is reserved.  No */
/*     parameter is defined to point to this location. */


/*     End Include Section:  EK Column Descriptor Parameters */

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


/*     Include Section:  EK Segment Descriptor Parameters */

/*        eksegdsc.inc  Version 8  06-NOV-1995 (NJB) */


/*     All `base addresses' referred to below are the addresses */
/*     *preceding* the item the base applies to.  This convention */
/*     enables simplied address calculations in many cases. */

/*     Size of segment descriptor.  Note:  the include file ekcoldsc.inc */
/*     must be updated if this parameter is changed.  The parameter */
/*     CDOFF in that file should be kept equal to SDSCSZ. */


/*     Index of the segment type code: */


/*     Index of the segment's number.  This number is the segment's */
/*     index in the list of segments contained in the EK to which */
/*     the segment belongs. */


/*     Index of the DAS integer base address of the segment's integer */
/*     meta-data: */


/*     Index of the DAS character base address of the table name: */


/*     Index of the segment's column count: */


/*     Index of the segment's record count: */


/*     Index of the root page number of the record tree: */


/*     Index of the root page number of the character data page tree: */


/*     Index of the root page number of the double precision data page */
/*     tree: */


/*     Index of the root page number of the integer data page tree: */


/*     Index of the `modified' flag: */


/*     Index of the `initialized' flag: */


/*     Index of the shadowing flag: */


/*     Index of the companion file handle: */


/*     Index of the companion segment number: */


/*     The next three items are, respectively, the page numbers of the */
/*     last character, d.p., and integer data pages allocated by the */
/*     segment: */


/*     The next three items are, respectively, the page-relative */
/*     indices of the last DAS word in use in the segment's */
/*     last character, d.p., and integer data pages: */


/*     Index of the DAS character base address of the column name list: */


/*     The last descriptor element is reserved for future use.  No */
/*     parameter is defined to point to this location. */


/*     End Include Section:  EK Segment Descriptor Parameters */

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


/*     Include Section:  EK Boolean Enumerated Type */


/*        ekbool.inc Version 1   21-DEC-1994 (NJB) */


/*     Within the EK system, boolean values sometimes must be */
/*     represented by integer or character codes.  The codes and their */
/*     meanings are listed below. */

/*     Integer code indicating `true': */


/*     Integer code indicating `false': */


/*     Character code indicating `true': */


/*     Character code indicating `false': */


/*     End Include Section:  EK Boolean Enumerated Type */

/* +============================================================== */

/*     Meta/2 syntax definition variables. */

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


/*     Include Section:  EK Data Types */

/*        ektype.inc Version 1  27-DEC-1994 (NJB) */


/*     Within the EK system, data types of EK column contents are */
/*     represented by integer codes.  The codes and their meanings */
/*     are listed below. */

/*     Integer codes are also used within the DAS system to indicate */
/*     data types; the EK system makes no assumptions about compatibility */
/*     between the codes used here and those used in the DAS system. */


/*     Character type: */


/*     Double precision type: */


/*     Integer type: */


/*     `Time' type: */

/*     Within the EK system, time values are represented as ephemeris */
/*     seconds past J2000 (TDB), and double precision numbers are used */
/*     to store these values.  However, since time values require special */
/*     treatment both on input and output, and since the `TIME' column */
/*     has a special role in the EK specification and code, time values */
/*     are identified as a type distinct from double precision numbers. */


/*     End Include Section:  EK Data Types */


/*     E-kernel column type definitions */


/*     INTEGER               CH */
/*     PARAMETER           ( CH   = 1 ) */

/*     INTEGER               DP */
/*     PARAMETER           ( DP   = 2 ) */

/*     INTEGER               INT */
/*     PARAMETER           ( INT  = 3 ) */

/*     INTEGER               TIME */
/*     PARAMETER           ( TIME = 4 ) */

/*     Local Parameters */

/*     FILSIZ   is the maximum number of characters allowed for a */
/*              filename */

/*     LNGSIZ   is the maximum number of characters allowed for */
/*              use in reporting the columns associated with a given */
/*              file. */

/*     MAXFIL   is the maximum number of E-kernels that can be loaded */
/*              at any one time. */

/*     NNAMES   is the maximum number of names/headings that can appear */
/*              in a report of loaded files and columns. */

/*     MAXCOL   is the maximum number of columns that may be present */
/*              in any segment of an E-kernel */

/*     LNSIZE   is the standard text line length. */


/*     Initialization logical */


/*     Loaded file database (shared between entry points) */


/*     Local Variables */


/*     INTEGER               IFALSE */
/*     PARAMETER           ( IFALSE = -1 ) */


/*     Variables needed by NSPEKS */


/*     Save everything. */


/*     Initial Values */

    /* Parameter adjustments */
    if (error) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_nspld;
	case 2: goto L_nspuld;
	case 3: goto L_nspeks;
	case 4: goto L_nspekc;
	}

    return 0;

/*  Load an E-, leapsecond, or sclk kernel. */


L_nspld:

/*     Standard Spicelib error handling. */

    s_copy(rname, "NSPLD", (ftnlen)6, (ftnlen)5);
    s_copy(rnamec, "NSPLD:", (ftnlen)7, (ftnlen)6);
    if (return_()) {
	return 0;
    }
    chkin_(rname, (ftnlen)6);

/*     On the first pass establish the syntax that this routine */
/*     is responsible for recognizing. */

    if (first) {
	first = FALSE_;
	*(unsigned char *)bs = '@';
	for (i__ = 1; i__ <= 100; ++i__) {
	    s_copy(cnames + (((i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : 
		    s_rnge("cnames", i__1, "kerman_", (ftnlen)361)) << 6), 
		    " ", (ftnlen)64, (ftnlen)1);
	}
	for (i__ = 1; i__ <= 3; ++i__) {
	    replch_(synval + ((i__1 = i__ + 5) < 9 && 0 <= i__1 ? i__1 : 
		    s_rnge("synval", i__1, "kerman_", (ftnlen)366)) * 80, 
		    "#", bs, synval + ((i__2 = i__ + 5) < 9 && 0 <= i__2 ? 
		    i__2 : s_rnge("synval", i__2, "kerman_", (ftnlen)366)) * 
		    80, (ftnlen)80, (ftnlen)1, (ftnlen)1, (ftnlen)80);
	}
	m2ints_(&c__3, synkey, synptr, synval, (ftnlen)32, (ftnlen)80);
    }

/*     See if this command matches a known syntax.  If it doesn't */
/*     there is no point in hanging around. */

    m2chck_(commnd, synkey, synptr, synval, error, commnd_len, (ftnlen)32, (
	    ftnlen)80, error_len);
    if (have_(error, error_len)) {
	prefix_(rnamec, &c__1, error, (ftnlen)7, error_len);
	chkout_(rname, (ftnlen)6);
	return 0;
    }
    if (m2xist_("ekfile", (ftnlen)6)) {

/*        We need to have a leapseconds kernel loaded before */
/*        we can load an E-kernel. */

	expool_("DELTET/DELTA_AT", &found, (ftnlen)15);
	if (! found) {
	    s_copy(error, "Before an E-kernel can be loaded, you must load a"
		    " leapseconds kernel.  ", error_len, (ftnlen)71);
	    chkout_(rname, (ftnlen)6);
	    return 0;
	}
	m2getc_("ekfile", commnd, &found, file, (ftnlen)6, commnd_len, (
		ftnlen)127);

/*        See if we already have this file. */

	if (isrchc_(file, &nfiles, ekfils, (ftnlen)127, (ftnlen)127) > 0) {
	    chkout_(rname, (ftnlen)6);
	    return 0;
	}

/*        Make sure there is room for this file. */

	if (nfiles == 20) {
	    s_copy(error, "The maximum number of E-kernels that can loaded a"
		    "t open by INSPEKT at one time is #.  That number has alr"
		    "eady been reached. You will need to unload one of the fi"
		    "les that have already been loaded before you will be abl"
		    "e to load any other files. ", error_len, (ftnlen)244);
	    repmct_(error, "#", &c__20, "L", error, error_len, (ftnlen)1, (
		    ftnlen)1, error_len);
	    prefix_(rnamec, &c__1, error, (ftnlen)7, error_len);
	    chkout_(rname, (ftnlen)6);
	    return 0;
	}

/*        Load the file as an e-kernel. */

	eklef_(file, &handle, rtrim_(file, (ftnlen)127));
	if (have_(error, error_len)) {
	    prefix_(rnamec, &c__1, error, (ftnlen)7, error_len);
	    chkout_(rname, (ftnlen)6);
	    return 0;
	}

/*        Store the name of this file. */

	++nfiles;
	s_copy(ekfils + ((i__1 = nfiles - 1) < 20 && 0 <= i__1 ? i__1 : 
		s_rnge("ekfils", i__1, "kerman_", (ftnlen)442)) * 127, file, (
		ftnlen)127, (ftnlen)127);

/*        Determine how many segments are in the file we just loaded. */

	nseg = eknseg_(&handle);

/*        For each segment in the newly loaded file ... */

	i__1 = nseg;
	for (seg = 1; seg <= i__1; ++seg) {
	    s_copy(tabnam, " ", (ftnlen)64, (ftnlen)1);
	    for (i__ = 1; i__ <= 100; ++i__) {
		s_copy(cnames + (((i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 :
			 s_rnge("cnames", i__2, "kerman_", (ftnlen)457)) << 6)
			, " ", (ftnlen)64, (ftnlen)1);
	    }
	    zzeksinf_(&handle, &seg, tabnam, segdsc, cnames, cdscrs, (ftnlen)
		    64, (ftnlen)64);

/*           Add each column name to the list of columns held by the */
/*           column manager. */

	    ncols = segdsc[4];
	    i__2 = ncols;
	    for (col = 1; col <= i__2; ++col) {

/*              We need to make the column name include table it */
/*              belongs to (a fully qualified column name). */

		prefix_(".", &c__0, cnames + (((i__3 = col - 1) < 100 && 0 <= 
			i__3 ? i__3 : s_rnge("cnames", i__3, "kerman_", (
			ftnlen)475)) << 6), (ftnlen)1, (ftnlen)64);
		prefix_(tabnam, &c__0, cnames + (((i__3 = col - 1) < 100 && 0 
			<= i__3 ? i__3 : s_rnge("cnames", i__3, "kerman_", (
			ftnlen)476)) << 6), (ftnlen)64, (ftnlen)64);
		cindxd = cdscrs[(i__3 = col * 11 - 6) < 1100 && 0 <= i__3 ? 
			i__3 : s_rnge("cdscrs", i__3, "kerman_", (ftnlen)478)]
			 != -1;
		cnull = cdscrs[(i__3 = col * 11 - 4) < 1100 && 0 <= i__3 ? 
			i__3 : s_rnge("cdscrs", i__3, "kerman_", (ftnlen)479)]
			 != -1;
		ctype = cdscrs[(i__3 = col * 11 - 10) < 1100 && 0 <= i__3 ? 
			i__3 : s_rnge("cdscrs", i__3, "kerman_", (ftnlen)481)]
			;
		clen = cdscrs[(i__3 = col * 11 - 9) < 1100 && 0 <= i__3 ? 
			i__3 : s_rnge("cdscrs", i__3, "kerman_", (ftnlen)482)]
			;
		csize = cdscrs[(i__3 = col * 11 - 8) < 1100 && 0 <= i__3 ? 
			i__3 : s_rnge("cdscrs", i__3, "kerman_", (ftnlen)483)]
			;

/*              This is what used to be here, but the item NBLIDX */
/*              vanished by design.  We now just set this so something */
/*              reasonable.  24 seemed like the reasonable thing at */
/*              the time.  (See the column manager and do a bit of */
/*              code diving to see what this is used for.) */

/*              WIDEST    = CDSCRS ( NBLIDX, COL ) */

		widest = 24;
		clnew_(cnames + (((i__3 = col - 1) < 100 && 0 <= i__3 ? i__3 :
			 s_rnge("cnames", i__3, "kerman_", (ftnlen)496)) << 6)
			, &handle, &ctype, &clen, &widest, &csize, &cindxd, &
			cnull, &id, (ftnlen)64);
	    }
	}

/*        If anything went wrong, unload the file. */

	if (have_(error, error_len)) {
	    prefix_(rnamec, &c__1, error, (ftnlen)7, error_len);
	    ekuef_(&handle);
	    clunld_(&handle);
	    --nfiles;
	    chkout_(rname, (ftnlen)6);
	    return 0;
	}
    } else if (m2xist_("leapfile", (ftnlen)8)) {
	m2getc_("leapfile", commnd, &found, file, (ftnlen)8, commnd_len, (
		ftnlen)127);
	ldpool_(file, (ftnlen)127);
	bbputc_1__("POST", "LEAPSECONDS", &c__1, file, (ftnlen)4, (ftnlen)11, 
		(ftnlen)127);
    } else if (m2xist_("sclkfile", (ftnlen)8)) {
	m2getc_("sclkfile", commnd, &found, file, (ftnlen)8, commnd_len, (
		ftnlen)127);
	ldpool_(file, (ftnlen)127);
	bbputc_1__("APPEND", "SCLK", &c__1, file, (ftnlen)6, (ftnlen)4, (
		ftnlen)127);
    } else {
	s_copy(error, "The input command was unrecognized and somehow got to"
		" an \"impossible\" place in KERMAN.FOR", error_len, (ftnlen)
		89);
    }
    if (have_(error, error_len)) {
	prefix_(rnamec, &c__1, error, (ftnlen)7, error_len);
	chkout_(rname, (ftnlen)6);
	return 0;
    }
    chkout_(rname, (ftnlen)6);
    return 0;

/*  Unload an E-kernel from the list of known files. */


L_nspuld:
    s_copy(rname, "NSPULD", (ftnlen)6, (ftnlen)6);
    s_copy(rnamec, "NSPULD:", (ftnlen)7, (ftnlen)7);
    if (return_()) {
	return 0;
    }
    chkin_(rname, (ftnlen)6);
    j = isrchc_(infile__, &nfiles, ekfils, infile_len, (ftnlen)127);
    if (j == 0) {
	s_copy(error, "The file # is not listed among those files that have "
		"been loaded. ", error_len, (ftnlen)66);
	repmc_(error, "#", infile__, error, error_len, (ftnlen)1, rtrim_(
		infile__, infile_len), error_len);
	chkout_(rname, (ftnlen)6);
	return 0;
    }

/*     Get the handle associated with this file. */

    dasfnh_(infile__, &handle, rtrim_(infile__, infile_len));
    if (have_(error, error_len)) {
	chkout_(rname, (ftnlen)6);
	return 0;
    }

/*     Now unload the file, and detach its handle from any columns to */
/*     which it might be attached. */

    ekuef_(&handle);
    clunld_(&handle);

/*     Finally remove this file from our internal list of files. */

    remlac_(&c__1, &j, ekfils, &nfiles, (ftnlen)127);
    chkout_(rname, (ftnlen)6);
    return 0;

/*  Create a report regarding currently loaded kernels/columns. */


L_nspeks:

/*     Version 2.0  Aug 3, 1995 */

/*        This routine was rewritten to provide a more friendly */
/*        kernel summary. */

/*     ---B. Taber */

/*     This routine displays the currently loaded E-kernels. */

    s_copy(rname, "NSPEKS", (ftnlen)6, (ftnlen)6);
    s_copy(rnamec, "NSPEKS:", (ftnlen)7, (ftnlen)7);
    if (return_()) {
	return 0;
    }

/*     write (*,*) 'Checking in:' */

    chkin_(rname, (ftnlen)6);
    if (nfiles <= 0) {
	nspwln_(" ", (ftnlen)1);
	nspwln_("There are no E-kernels loaded now.", (ftnlen)34);
	nspwln_(" ", (ftnlen)1);
	chkout_(rname, (ftnlen)6);
	return 0;
    }

/*     First thing we do is set up the NICEPR_1 style string */
/*     to be used in creation of summary headers. */

/*     write (*,*) 'Fetching margins: ' */
    nspglr_(&left, &right);
    nspmrg_(style, (ftnlen)80);
    suffix_("FLAG", &c__1, style, (ftnlen)4, (ftnlen)80);
    suffix_("E-kernel:", &c__1, style, (ftnlen)9, (ftnlen)80);

/*     Reset the output page, title frequency and header frequency */
/*     values. */

/*     write (*,*) 'Resetting page and setting up page attributes:' */

    pagrst_();
    pagset_("TITLEFREQUENCY", &c__0, (ftnlen)14);
    pagset_("HEADERFREQUENCY", &c__0, (ftnlen)15);
    pagset_("NOSPACEFOOTER", &c__1, (ftnlen)13);
    pagset_("FOOTERFREQUENCY", &c_n1, (ftnlen)15);
    s_copy(pval, "CH", (ftnlen)32, (ftnlen)2);
    s_copy(pval + 32, "D.P.", (ftnlen)32, (ftnlen)4);
    s_copy(pval + 64, "INTEGER", (ftnlen)32, (ftnlen)7);
    s_copy(pval + 96, "TIME", (ftnlen)32, (ftnlen)4);
    lmarge = 1;
    space = 1;

/*     Next we set up the the column id codes, sizes, */
/*     default widths, justifications, component preservation, */
/*     and special marker attributes for each column. */

    headr[0] = 1;
    headr[1] = 2;
    headr[2] = 3;
    headr[3] = 4;
    headr[4] = 5;
    sizes[0] = 1;
    sizes[1] = 1;
    sizes[2] = 1;
    sizes[3] = 1;
    sizes[4] = 1;
    width[0] = 16;
    width[1] = 16;
    width[2] = 8;
    width[3] = 8;
    width[4] = 6;
    need = width[0] + width[1] + width[2] + width[3] + width[4] + 4;
    right = min(right,need);
    pagset_("PAGEWIDTH", &right, (ftnlen)9);
    reqd = width[2] + width[3] + width[4] + 4;

/*     If the page width is less than default needed, we reset the */
/*     widths of the first two columns so they will fit in available */
/*     space. */

    if (right < need) {
	width[0] = (right - reqd) / 2;
	width[1] = width[0];
    }
    justr[0] = FALSE_;
    justr[1] = FALSE_;
    justr[2] = FALSE_;
    justr[3] = TRUE_;
    justr[4] = TRUE_;
    presrv[0] = TRUE_;
    presrv[1] = TRUE_;
    presrv[2] = TRUE_;
    presrv[3] = TRUE_;
    presrv[4] = TRUE_;
    s_copy(spcial, " ", (ftnlen)4, (ftnlen)1);
    s_copy(spcial + 4, " ", (ftnlen)4, (ftnlen)1);
    s_copy(spcial + 8, " ", (ftnlen)4, (ftnlen)1);
    s_copy(spcial + 12, " ", (ftnlen)4, (ftnlen)1);
    s_copy(spcial + 16, " ", (ftnlen)4, (ftnlen)1);

/*     write (*,*) 'Starting file loop:' */

    i__1 = nfiles;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Get the handle associated with this file, and get the */
/*        number of ID's currently known. */

	dasfnh_(ekfils + ((i__2 = i__ - 1) < 20 && 0 <= i__2 ? i__2 : s_rnge(
		"ekfils", i__2, "kerman_", (ftnlen)738)) * 127, &handle, (
		ftnlen)127);
	clnum_(&nid);
/*        write (*,*) 'File: ', I, 'Handle: ', HANDLE */

/*        Now empty out the table/column data for this file. */

/*        write (*,*) 'Empty out the column collector.' */
	ssizec_(&c__500, tabcol, (ftnlen)80);
	ssizei_(&c__500, colids);

/*        Cycle over all column id's to determine if they */
/*        are attached to this particular file. */

/*        write (*,*) 'Beginning Column search:  ', NID, ' Columns' */
	i__2 = nid;
	for (j = 1; j <= i__2; ++j) {
	    clnid_(&j, &id, &found);
	    clgai_(&id, "HANDLES", &nh, handls, (ftnlen)7);
	    if (isrchi_(&handle, &nh, handls) > 0) {

/*              This column is associated with this file.  Store */
/*              its name and id-code for the next section of code. */

/*              write (*,*) 'Column id and associated handle match.' */

		clgac_(&id, "NAME", cname, (ftnlen)4, (ftnlen)80);
		appndc_(cname, tabcol, (ftnlen)80, (ftnlen)80);
		appndi_(&id, colids);
	    }
	}

/*        Layout the pages.  We perform a soft page reset */
/*        so that the various sections will be empty. */
/*        Note this doesn't affect frequency parameter */
/*        or other geometry attributes of pages. */

/*        write (*,*) 'Creating page: Title:' */

	pagscn_("TITLE", (ftnlen)5);
	pagput_(" ", (ftnlen)1);
	pagput_("Summary of Loaded E-kernels", (ftnlen)27);
	pagput_(" ", (ftnlen)1);

/*        write (*,*) 'Creating page: Header' */

/*        Set up the various items needed for the report header. */

	pagscn_("HEADER", (ftnlen)6);
	pagput_(" ", (ftnlen)1);
	nicepr_1__(ekfils + ((i__2 = i__ - 1) < 20 && 0 <= i__2 ? i__2 : 
		s_rnge("ekfils", i__2, "kerman_", (ftnlen)791)) * 127, style, 
		(S_fp)pagput_, (ftnlen)127, (ftnlen)80);
	pagput_(" ", (ftnlen)1);
	scolmn_(&c__1, &c__1, "Table Name", (ftnlen)10);
	scolmn_(&c__2, &c__1, "Column Name", (ftnlen)11);
	scolmn_(&c__3, &c__1, "Type", (ftnlen)4);
	scolmn_(&c__4, &c__1, "Size", (ftnlen)4);
	scolmn_(&c__5, &c__1, "Index", (ftnlen)5);

/*        write (*,*) 'Creating page: Column headings' */

	tabrpt_(&c__5, headr, sizes, width, justr, presrv, spcial, &lmarge, &
		space, (U_fp)gcolmn_, (ftnlen)4);
	s_copy(break__, "==================================================="
		"=============================", (ftnlen)80, (ftnlen)80);
	pagput_(break__, right);

/*        Now set the page section to the body portion for */
/*        preparing to fill in the e-kernel summary. */

/*        write (*,*) 'Creating page: Body of report:' */
	pagscn_("BODY", (ftnlen)4);
	n = cardc_(tabcol, (ftnlen)80);
	orderc_(tabcol + 480, &n, ordvec, (ftnlen)80);
	s_copy(lsttab, " ", (ftnlen)32, (ftnlen)1);
	i__2 = n;
	for (j = 1; j <= i__2; ++j) {
	    k = ordvec[(i__3 = j - 1) < 500 && 0 <= i__3 ? i__3 : s_rnge(
		    "ordvec", i__3, "kerman_", (ftnlen)826)];
	    clgac_(&colids[(i__3 = k + 5) < 506 && 0 <= i__3 ? i__3 : s_rnge(
		    "colids", i__3, "kerman_", (ftnlen)828)], "TABLE", tname, 
		    (ftnlen)5, (ftnlen)32);
	    clgac_(&colids[(i__3 = k + 5) < 506 && 0 <= i__3 ? i__3 : s_rnge(
		    "colids", i__3, "kerman_", (ftnlen)829)], "NAME", cname, (
		    ftnlen)4, (ftnlen)80);
	    clgac_(&colids[(i__3 = k + 5) < 506 && 0 <= i__3 ? i__3 : s_rnge(
		    "colids", i__3, "kerman_", (ftnlen)830)], "SIZE", size, (
		    ftnlen)4, (ftnlen)32);
	    clgac_(&colids[(i__3 = k + 5) < 506 && 0 <= i__3 ? i__3 : s_rnge(
		    "colids", i__3, "kerman_", (ftnlen)831)], "INDEXED", indx,
		     (ftnlen)7, (ftnlen)4);

/*           Note:  There is only one type associated with each */
/*           handle.  Thus TCODE does not need to be an array. */

	    clgai_(&colids[(i__3 = k + 5) < 506 && 0 <= i__3 ? i__3 : s_rnge(
		    "colids", i__3, "kerman_", (ftnlen)836)], "TYPE", &count, 
		    &tcode, (ftnlen)4);
	    if (s_cmp(tname, lsttab, (ftnlen)32, (ftnlen)32) == 0) {
		s_copy(tname, " ", (ftnlen)32, (ftnlen)1);
	    } else if (s_cmp(lsttab, " ", (ftnlen)32, (ftnlen)1) != 0) {
		pagput_(" ", (ftnlen)1);
		s_copy(lsttab, tname, (ftnlen)32, (ftnlen)32);
	    } else {
		s_copy(lsttab, tname, (ftnlen)32, (ftnlen)32);
	    }
	    nb = pos_(cname, ".", &c__1, (ftnlen)80, (ftnlen)1) + 1;
	    s_copy(name__, cname + (nb - 1), (ftnlen)32, 80 - (nb - 1));
	    if (tcode == 1) {
		clgac_(&colids[(i__3 = k + 5) < 506 && 0 <= i__3 ? i__3 : 
			s_rnge("colids", i__3, "kerman_", (ftnlen)852)], 
			"TYPE", type__, (ftnlen)4, (ftnlen)32);
		sb = pos_(type__, "*", &c__1, (ftnlen)32, (ftnlen)1);
		s_copy(pval, "CH", (ftnlen)32, (ftnlen)2);
		suffix_(type__ + (sb - 1), &c__0, pval, 32 - (sb - 1), (
			ftnlen)32);
	    }
	    scolmn_(&c__6, &c__1, tname, (ftnlen)32);
	    scolmn_(&c__7, &c__1, name__, (ftnlen)32);
	    scolmn_(&c__8, &c__1, pval + (((i__3 = tcode - 1) < 4 && 0 <= 
		    i__3 ? i__3 : s_rnge("pval", i__3, "kerman_", (ftnlen)860)
		    ) << 5), (ftnlen)32);
	    scolmn_(&c__9, &c__1, size, (ftnlen)32);
	    scolmn_(&c__10, &c__1, indx, (ftnlen)4);
	    ids[0] = 6;
	    ids[1] = 7;
	    ids[2] = 8;
	    ids[3] = 9;
	    ids[4] = 10;

/*           write (*,*) 'Creating next row:' */
/*           write (*,*) TNAME */
/*           write (*,*) NAME */
/*           write (*,*) PVAL(TCODE) */
/*           write (*,*) SIZE */
/*           write (*,*) INDX */

	    tabrpt_(&c__5, ids, sizes, width, justr, presrv, spcial, &lmarge, 
		    &space, (U_fp)gcolmn_, (ftnlen)4);
/*           write (*,*) 'Row created.' */

	}

/*        Do a soft page reset so for the next file to be displayed */

/*        write (*,*) 'Performing soft page reset.' */
	pagsft_();
	pagrst_();
	pagset_("TITLEFREQUENCY", &c_n1, (ftnlen)14);
	pagset_("HEADERFREQUENCY", &c__0, (ftnlen)15);
	pagset_("NOSPACEFOOTER", &c__1, (ftnlen)13);
	pagset_("FOOTERFREQUENCY", &c_n1, (ftnlen)15);
    }
    chkout_(rname, (ftnlen)6);
    return 0;
/* $Procedure      NSPEKC ( Inspekt the comments from EK files ) */

L_nspekc:
/*     This entry point examines each file that matches the */
/*     template given by INFILE and if comments exist for the */
/*     file, they are displayed. */
/*     Version 1.0.0 25-AUG-1995 (WLT) */
    chkin_("NSPEKC", (ftnlen)6);
    totalc = 0;
    s_copy(thisfl, " ", (ftnlen)127, (ftnlen)1);
/*     We might not need the style string, but it doesn't hurt to */
/*     get it. */
    nspmrg_(style, (ftnlen)80);
/*     If there are no loaded E-kernels say so and return. */
    if (nfiles == 0) {
	s_copy(messge, "There are no E-kernels loaded now. ", (ftnlen)300, (
		ftnlen)35);
	nicepr_1__(messge, style, (S_fp)nspwln_, (ftnlen)300, (ftnlen)80);
	chkout_("NSPEKC", (ftnlen)6);
	return 0;
    }
/*     Count the number of characters present in the files */
/*     that match the template. */
    r__ = rtrim_(infile__, infile_len);
    l = ltrim_(infile__, infile_len);
    i__1 = nfiles;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (match_(ekfils + ((i__2 = i__ - 1) < 20 && 0 <= i__2 ? i__2 : 
		s_rnge("ekfils", i__2, "kerman_", (ftnlen)945)) * 127, 
		infile__ + (l - 1), (ftnlen)127, r__ - (l - 1))) {
	    dasfnh_(ekfils + ((i__2 = i__ - 1) < 20 && 0 <= i__2 ? i__2 : 
		    s_rnge("ekfils", i__2, "kerman_", (ftnlen)947)) * 127, &
		    handle, (ftnlen)127);
	    dasrfr_(&handle, idword, ifname, &nresvr, &nresvc, &ncomr, &ncomc,
		     (ftnlen)8, (ftnlen)60);
	    totalc += ncomc;
	    ++hits;
	    s_copy(thisfl, ekfils + ((i__2 = i__ - 1) < 20 && 0 <= i__2 ? 
		    i__2 : s_rnge("ekfils", i__2, "kerman_", (ftnlen)955)) * 
		    127, (ftnlen)127, (ftnlen)127);
	}
    }
/*     If we didn't get any characters there several possible */
/*     reasons.  We can look at HITS to see why and form a */
/*     grammatically reasonable message. */
    if (totalc == 0) {
	if (hits == 0) {
	    s_copy(messge, "There are no E-kernels loaded whose file name ma"
		    "tches the supplied template '#'.", (ftnlen)300, (ftnlen)
		    80);
	    repmc_(messge, "#", infile__ + (l - 1), messge, (ftnlen)300, (
		    ftnlen)1, r__ - (l - 1), (ftnlen)300);
	} else if (hits == 1) {
	    s_copy(messge, "There are no comments present in the file '#'. ", 
		    (ftnlen)300, (ftnlen)47);
	    repmc_(messge, "#", thisfl, messge, (ftnlen)300, (ftnlen)1, (
		    ftnlen)127, (ftnlen)300);
	} else if (hits == 2) {
	    s_copy(messge, "There are no comments present in either of the #"
		    " files that match the supplied template. ", (ftnlen)300, (
		    ftnlen)89);
	    repmct_(messge, "#", &hits, "L", messge, (ftnlen)300, (ftnlen)1, (
		    ftnlen)1, (ftnlen)300);
	} else {
	    s_copy(messge, "There are no comments present in any of the # fi"
		    "les that match the supplied template. ", (ftnlen)300, (
		    ftnlen)86);
	    repmct_(messge, "#", &hits, "L", messge, (ftnlen)300, (ftnlen)1, (
		    ftnlen)1, (ftnlen)300);
	}
	nicepr_1__(messge, style, (S_fp)nspwln_, (ftnlen)300, (ftnlen)80);
	chkout_("NSPEKC", (ftnlen)6);
	return 0;
    }
/*     Ok. We've got something.  Set up the output page to receive */
/*     the comments a file at a time. */
    suffix_("FLAG E-kernel:", &c__1, style, (ftnlen)14, (ftnlen)80);
    i__1 = nfiles;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (match_(ekfils + ((i__2 = i__ - 1) < 20 && 0 <= i__2 ? i__2 : 
		s_rnge("ekfils", i__2, "kerman_", (ftnlen)1012)) * 127, 
		infile__ + (l - 1), (ftnlen)127, r__ - (l - 1))) {
	    dasfnh_(ekfils + ((i__2 = i__ - 1) < 20 && 0 <= i__2 ? i__2 : 
		    s_rnge("ekfils", i__2, "kerman_", (ftnlen)1014)) * 127, &
		    handle, (ftnlen)127);
	    dasrfr_(&handle, idword, ifname, &nresvr, &nresvc, &ncomr, &ncomc,
		     (ftnlen)8, (ftnlen)60);
	    if (ncomc == 0) {
		s_copy(messge, "# contains no comments.", (ftnlen)300, (
			ftnlen)23);
		repmc_(messge, "#", ekfils + ((i__2 = i__ - 1) < 20 && 0 <= 
			i__2 ? i__2 : s_rnge("ekfils", i__2, "kerman_", (
			ftnlen)1023)) * 127, messge, (ftnlen)300, (ftnlen)1, (
			ftnlen)127, (ftnlen)300);
		nspwln_(" ", (ftnlen)1);
		nicepr_1__(messge, style, (S_fp)nspwln_, (ftnlen)300, (ftnlen)
			80);
	    } else {
		pagrst_();
		pagscn_("HEADER", (ftnlen)6);
		pagset_("TITLEFREQUENCY", &c__0, (ftnlen)14);
		pagset_("HEADERFREQUENCY", &c__0, (ftnlen)15);
		pagset_("NOSPACEFOOTER", &c__1, (ftnlen)13);
		pagset_("FOOTERFREQUENCY", &c_n1, (ftnlen)15);
		pagput_(" ", (ftnlen)1);
		nicepr_1__(ekfils + ((i__2 = i__ - 1) < 20 && 0 <= i__2 ? 
			i__2 : s_rnge("ekfils", i__2, "kerman_", (ftnlen)1038)
			) * 127, style, (S_fp)pagput_, (ftnlen)127, (ftnlen)
			80);
		pagput_(" ", (ftnlen)1);
		nspshc_(&handle, &quit);
		if (quit) {
		    nspwln_(" ", (ftnlen)1);
		    chkout_("NSPEKC", (ftnlen)6);
		    return 0;
		}
	    }
	}
    }
    nspwln_(" ", (ftnlen)1);
    chkout_("NSPEKC", (ftnlen)6);
    return 0;
} /* kerman_ */

/* Subroutine */ int kerman_(char *commnd, char *infile__, char *error, 
	ftnlen commnd_len, ftnlen infile_len, ftnlen error_len)
{
    return kerman_0_(0, commnd, infile__, error, commnd_len, infile_len, 
	    error_len);
    }

/* Subroutine */ int nspld_(char *commnd, char *error, ftnlen commnd_len, 
	ftnlen error_len)
{
    return kerman_0_(1, commnd, (char *)0, error, commnd_len, (ftnint)0, 
	    error_len);
    }

/* Subroutine */ int nspuld_(char *infile__, char *error, ftnlen infile_len, 
	ftnlen error_len)
{
    return kerman_0_(2, (char *)0, infile__, error, (ftnint)0, infile_len, 
	    error_len);
    }

/* Subroutine */ int nspeks_(void)
{
    return kerman_0_(3, (char *)0, (char *)0, (char *)0, (ftnint)0, (ftnint)0,
	     (ftnint)0);
    }

/* Subroutine */ int nspekc_(char *infile__, ftnlen infile_len)
{
    return kerman_0_(4, (char *)0, infile__, (char *)0, (ftnint)0, infile_len,
	     (ftnint)0);
    }

