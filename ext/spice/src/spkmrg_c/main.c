/* spkmerge.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6000 = 6000;
static integer c__10000 = 10000;
static integer c__60000 = 60000;
static integer c__40000 = 40000;
static integer c__0 = 0;
static integer c__2 = 2;
static integer c__1 = 1;
static integer c__3 = 3;
static integer c__6 = 6;
static integer c__5 = 5;

/* $Program     SPKMERGE */

/* Main program */ MAIN__(void)
{
    /* System generated locals */
    address a__1[2];
    integer i__1, i__2, i__3[2], i__4, i__5;
    doublereal d__1, d__2;
    char ch__1[100];
    cllist cl__1;
    alist al__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer f_rew(alist *), f_clos(cllist *);

    /* Local variables */
    static logical data;
    static char name__[300];
    static logical same;
    static char line[350];
    static integer body[10000], nval;
    static doublereal tvec[6], wind[40006];
    static logical used;
    static integer nsrc, nspk, unit, work[10000];
    static char text[350];
    static integer i__, j, k;
    extern integer cardd_(doublereal *);
    static integer m;
    static char bodch[16];
    static integer n;
    extern /* Subroutine */ int dafgn_(char *, ftnlen), dafgs_(doublereal *), 
	    rdcmd_(char *, char *, integer *, char *, ftnlen, ftnlen, ftnlen),
	     spcac_(integer *, integer *, char *, char *, ftnlen, ftnlen), 
	    chkin_(char *, ftnlen), spcec_(integer *, integer *), dafus_(
	    doublereal *, integer *, integer *, doublereal *, integer *), 
	    errch_(char *, char *, ftnlen, ftnlen);
    static integer gbody[10000];
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen);
    extern doublereal dpmin_(void);
    static doublereal gwind[40006];
    extern doublereal dpmax_(void);
    extern /* Subroutine */ int dpfmt_(doublereal *, char *, char *, ftnlen, 
	    ftnlen);
    static integer nbody;
    static char value[300];
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), copyd_(doublereal *, doublereal *), 
	    movei_(integer *, integer *, integer *);
    static integer pnter;
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int et2utc_(doublereal *, char *, integer *, char 
	    *, ftnlen, ftnlen);
    static doublereal dc[2];
    static integer ic[6];
    extern /* Subroutine */ int dafbbs_(integer *), str2et_(char *, 
	    doublereal *, ftnlen), daffpa_(logical *), dafcls_(integer *), 
	    scardc_(integer *, char *, ftnlen), scardd_(integer *, doublereal 
	    *);
    static char cmdval[300*6006];
    static doublereal addwin[40006], begtim;
    static char segnam[40];
    static integer oldhan;
    extern /* Subroutine */ int getcml_(char *, ftnlen);
    extern integer isrchi_(integer *, integer *, integer *);
    static char begsym[100];
    static doublereal endtim, locval[60006], locwin[40006];
    static char cmdsym[100*6006];
    static doublereal segwin[40006];
    static integer cmdptr[6006];
    static doublereal winval[60006];
    static integer newhan, ngbody;
    static doublereal segsum[5];
    static char symbol[100], locsym[16*10006], tstamp[22];
    static integer locptr[10006], cmtunt, logunt;
    static logical commnt;
    extern /* Subroutine */ int trcoff_(void);
    extern /* Character */ VOID crtptr_(char *, ftnlen, char *, integer *, 
	    char *, ftnlen, ftnlen);
    static char srcsym[100];
    static doublereal tmpwin[40006];
    extern /* Subroutine */ int erract_(char *, char *, ftnlen, ftnlen);
    static integer winptr[10006];
    extern /* Subroutine */ int ssizec_(integer *, char *, ftnlen);
    static char winsym[16*10006];
    extern /* Subroutine */ int ssizei_(integer *, integer *), ssized_(
	    integer *, doublereal *), cputim_(doublereal *), tkvrsn_(char *, 
	    char *, ftnlen, ftnlen), tostdo_(char *, ftnlen), prompt_(char *, 
	    char *, ftnlen, ftnlen), sygetc_(char *, char *, integer *, char *
	    , integer *, char *, logical *, ftnlen, ftnlen, ftnlen, ftnlen), 
	    setmsg_(char *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), byebye_(char *, ftnlen), furnsh_(char *, ftnlen), 
	    spkopn_(char *, char *, integer *, integer *, ftnlen, ftnlen), 
	    txtopn_(char *, integer *, ftnlen), txtops_(integer *), writln_(
	    char *, integer *, ftnlen), wrdnln_(char *, char *, integer *, 
	    integer *, ftnlen, ftnlen), scardi_(integer *, integer *), 
	    wninsd_(doublereal *, doublereal *, doublereal *), lparsm_(char *,
	     char *, integer *, integer *, char *, ftnlen, ftnlen, ftnlen), 
	    nparsi_(char *, integer *, char *, integer *, ftnlen, ftnlen), 
	    dafopr_(char *, integer *, ftnlen), wnintd_(doublereal *, 
	    doublereal *, doublereal *), errint_(char *, integer *, ftnlen), 
	    intstr_(integer *, char *, ftnlen), sygetd_(char *, char *, 
	    integer *, doublereal *, integer *, doublereal *, logical *, 
	    ftnlen, ftnlen), wndifd_(doublereal *, doublereal *, doublereal *)
	    , spksub_(integer *, doublereal *, char *, doublereal *, 
	    doublereal *, integer *, ftnlen), wnunid_(doublereal *, 
	    doublereal *, doublereal *);
    static integer bod;
    extern /* Subroutine */ int syputd_(char *, doublereal *, integer *, char 
	    *, integer *, doublereal *, ftnlen, ftnlen), suffix_(char *, 
	    integer *, char *, ftnlen, ftnlen);
    static logical fnd, all;
    extern /* Subroutine */ int txtopr_(char *, integer *, ftnlen);
    static logical log__;
    static char val[300*6000], err[80], tkv[8];


/* $ Abstract */

/*     SPKMERGE creates new SPK files by merging entire or subsets of */
/*     one or more existing SPK files. */

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

/*     SPKMERGE User's Guide */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Input */

/*     SPKMERGE requires the name of a command file. The file name can */
/*     be entered on the command line, or entered in response to a */
/*     prompt. */

/*     Refer to the SPKMERGE User's Guide for information on creating the */
/*     command file. */

/* $ Particulars */

/*     Refer to the SPKMERGE User's Guide. */

/* $ Examples */

/*     Refer to the SPKMERGE User's Guide. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     M.J. Spencer   (JPL) */

/* $ Version */

/* -    SPICE Toolkit Version 3.4.0, 17-JAN-2014 (BVS) */

/*        Increased the following parameters: */

/*           MAXBOD -- from 1000 to 10000 */

/*           MAXSYM -- from 500 to 6000 */

/*           MAXVAL -- from 500 to 6000 */

/*           WINSIZ -- from 4000 to MAXBOD * 4 */

/*           WNSSIZ -- from 6000 to MAXBOD * 6 */

/*           VALLEN -- from 200 to 300 */

/* -    SPICE Toolkit Version 3.3.0, 02-NOV-2006 (BVS) */

/*        Replaced LDPOOL with FURNSH. */

/* -    SPICE Toolkit Version 3.2.1, 03-NOV-2000 (EDW) */

/*        Added a BYEBYE( 'SUCCESS' ) call at program's end. */

/* -    Spice Toolkit Version 3.2.0 18-NOV-1999 (FST) */

/*        The source code documentation for SPKMERGE has been greatly */
/*        enhanced.  The WORK array is now properly initialized, which */
/*        prevents the program from looping infinitely in some cases. */
/*        Integer parameters have been defined to improve the ease of */
/*        interpreting the values assigned to the WORK array. */

/* -    Spice Toolkit Version 3.1.0 10-NOV-1998 (FST) */

/*        The call to DAFOPN was replaced with SPKOPN to set the */
/*        ID_WORD to 'DAF/SPK ' as opposed to the ambiguous 'NAIF/DAF'. */
/*        The calls to UTC2ET were replaced with STR2ET to allow a */
/*        more flexible number of time string inputs. TDB (ET) time is */
/*        now an acceptable input. */

/* -    Spice Toolkit Version 3.0.0 25-SEP-1997 (WLT) */

/*         The call to WNFLTD was commented out so that small */
/*         segments are not exclude from the merge operation. */

/* -    Spice Toolkit Version 2.1.0 09-JAN-1997 (WLT) */

/*         Replaced calls to DPFMT_1 to DPFMT. */

/* -    Spice Toolkit Version 2.0.0 30-MAY-1996 (WLT) */

/*         Writes to logical units were replaced by calls to */
/*         WRITLN so as to make the program more portable. */

/* -    Beta Version 1.2.0, 12-JAN-1995 (MJS) */

/*       Fixed the filter of redundant data. Last version incorrectly */
/*       stored the previous data for a body in the WIN symbol table. */

/*       Changed line */

/*           CALL SYPUTD (BODCH,  SEGWIN(1), N, */
/*          .             WINSYM, WINPTR,    WINVAL) */

/*       to */

/*           CALL SYPUTD (BODCH,  SEGWIN(1), CARDD(SEGWIN), */
/*          .             WINSYM, WINPTR,    WINVAL) */

/* -    Beta Version 1.1.0, 21-OCT-1994 (MJS) */

/*       This version summarizes bodies correctly in the log file. */
/*       Previously, non-consecutive bodies in the BODY array that have */
/*       identical windows of coverage were not shown correctly. */

/* -    Beta Version 1.0.0, 29-JUN-1994 (MJS) */

/* -& */

/*     SPICELIB functions */


/*     Other functions */


/*     Local parameters */


/*     The command file comment that indicates the start */
/*     of SPKMERGE commands. */


/*     The command file keyword that indicates the UTC or TDB */
/*     time interval start. */


/*     The command file keyword that indicates the ID codes for */
/*     the SPK bodies of interest. */


/*     The command file keyword that instructs SPKMERGE to include */
/*     the comments from the listed source SPK. */


/*     The command file comment that indicates the end of the */
/*     SPKMERGE commands. */


/*     The command file keyword that indicates the UTC or TDB */
/*     time interval stop. */


/*     The command file keyword that indicates the name of an ASCII */
/*     text file to include as comments in the new merged SPK. */


/*     The command file keyword that instructs SPKMERGE to produce a */
/*     log file. */


/*     The command file keyword that indicates the name of the */
/*     leapseconds kernel to load. */


/*     The command file keyword that indicates the name of the */
/*     SPK file to create. */


/*     The command file keyword that indicates the name of a */
/*     source SPK file to be read. */


/*     SPKMERGE Version. */


/*     The value of the WORK array that indicates that data for the */
/*     body ID code stored in the parallel array BODY has been merged */
/*     in from the current source SPK. */


/*     Cell parameter. */


/*     The maximum number of bodies that SPKMERGE is capable of */
/*     processing. */


/*     The maximum number of entries in the command file symbol table. */


/*     The maximum number of values in the command file symbol table. */


/*     The number of double precision components in the SPK segment */
/*     descriptor. */


/*     The number of integer components in the SPK segment descriptor. */


/*     The value of the WORK array that indicates that the body ID code */
/*     stored in the parallel array BODY is to be placed into the log */
/*     file at the next write. */


/*     The value of the WORK array that indicates that data for the */
/*     body ID code stored in the parallel array BODY was not taken */
/*     from this source SPK file. */


/*     The maximum length of character strings that are fetched from the */
/*     command file symbol table. */


/*     Twice the maximum number of intervals that may be stored in the */
/*     window data structures. This number should be at least: */
/*     (2 * MAXBOD). */


/*     The maximum number of data elements that may be stored in the */
/*     symbol tables that store window data structures for each body ID */
/*     code.  This number should be at least: (6 * MAXBOD). */


/*     Log file line lengths. Consistent with LINLEN in RDCMD and */
/*     CPARSE_2. */


/*     Local variables */


/*     Character string used to hold the root of a symbol in the command */
/*     file symbol table. */


/*     This string stores the body ID codes converted from integers to */
/*     string.  These are then used as symbols in the LOC and WIN symbol */
/*     table. */


/*     The symbol component of the command file symbol table. */
/*     The command file symbol table contains the contents of */
/*     the successfully parsed SPKMERGE command file.  The */
/*     symbols are constructed in pieces separated by '~''s to */
/*     replicate the layered structure of the SPKMERGE command file. */


/*     The value component of the command file symbol table. See */
/*     CMDSYM for an explanation of the command file symbol table. */


/*     The string used to collect any error messages returned from */
/*     NPARSI. */


/*     Miscellaneous line of text for storing the command line, etc. */


/*     The symbol component of the LOC symbol table.  The LOC symbol */
/*     table stores the window data structures for each body ID code */
/*     that is merged into the new SPK file from a particular source SPK. */
/*     Only the interval pairs from the window for each body ID code are */
/*     actually stored in the symbol table.  The cardinality is easily */
/*     recovered with the retrieve routine, SYGETD. */


/*     Miscellaneous character string that stores the names of source SPK */
/*     files, log files, etc. */


/*     The character string used to store the name of an SPK segment. */


/*     Character strings used to store symbols constructed to retrieve */
/*     values from the command file symbol table. */


/*     Miscellaneous line of text for storing the lines headed for */
/*     STDOUT. */


/*     String that holds the toolkit version. */


/*     Character string that stores the system time stamp. */


/*     An array of characters that is used to store values retrieved */
/*     from the command file symbol table. */


/*     Single string used to store individual results retrieved from */
/*     the command file symbol table. */


/*     The symbol component of the WIN symbol table. The WIN symbol */
/*     table stores the window data structures for each body ID code */
/*     that is merged into the new SPK file. Only the interval pairs */
/*     from the window for each body ID code are actually stored in */
/*     the symbol table.  The cardinality is easily recovered with */
/*     the retrieve routine, SYGETD. */


/*     An instance of a WINDOW used to hold the time coverage of a */
/*     particular body. */


/*     The requested start time for a merge interval. */


/*     The double precision components of an SPK segment descriptor. */


/*     The final time for a requested merge interval. */


/*     The WINDOW that holds the global requested merge time intervals. */


/*     The value component of the LOC symbol table. See LOCSYM for a */
/*     complete description of the LOC symbol table. */


/*     An instance of a WINDOW that is used to hold the time coverage */
/*     of a particular body. */


/*     A packed DAF segment summary descriptor. */


/*     Another instance of a WINDOW used to hold the time coverage of a */
/*     particular body. */


/*     Yet another instance of a WINDOW used to hold the time coverage of */
/*     a particular body. */


/*     Time vector that holds the results of the call to CPUTIM. */


/*     A WINDOW data structure used to hold the requested requested */
/*     intervals of coverage for a particular source SPK.  This is the */
/*     local analogue of GWIND. */


/*     The value component of the WIN symbol table. See WINSYM for a */
/*     complete description of the WIN symbol table. */


/*     Body ID code. */


/*     Integer array used to hold the body ID codes of bodies that are to */
/*     be considered for merging at the local level. */


/*     The pointer component of the command file symbol table. See CMDSYM */
/*     for a detailed description of this symbol table. */


/*     Logical unit for the comment file. */


/*     Integer array used to hold the body ID codes of bodies that are to */
/*     be considered for merging at the global level. */


/*     Integer components of a SPK segment descriptor. */


/*     The pointer component of the LOC symbol table.  See LOCSYM for a */
/*     detailed description of this symbol table. */


/*     Logical unit for the log file. */


/*     The number of bodies considered for merging for the current source */
/*     SPK. */


/*     SPK file handle. */


/*     Number of bodies to be merged, listed at the global level. */


/*     SPK files to create counter. */


/*     SPK source file counter. */


/*     SPK file handle. */


/*     NPARSI pointer. */


/*     The pointer component to the WIN symbol table. See WINSYM for a */
/*     detailed description of this symbol table. */


/*     The WORK array parallels the BODY array, and indicates the status */
/*     of the merge for each body from the current source SPK. */
/*     Acceptable values are: */

/*        UNUSED - If WORK(I) = UNUSED then the body associated with */
/*                 BODY(I) has yet to provide data to the merged SPK */
/*                 from the current source SPK. */

/*        ISUSED - If WORK(I) = ISUSED then the body associated with */
/*                 BODY(I) has contributed data to the merged SPK from */
/*                 the current source SPK. */

/*        TOLOG  - If WORK(I) = TOLOG then the body associated with */
/*                 BODY(I) is about to have it's contributions logged. */


/*     This logical indicates whether or not every body meeting the */
/*     time requirements from the current source SPK is to be considered */
/*     for inclusion. */


/*     This logical indicates whether or not comments from the current */
/*     source SPK are to be included in the new SPK.  If .TRUE. then */
/*     comments from the source SPK are to be included in the new file. */


/*     This logical indicates whether or not the new SPK file contains */
/*     any data.  If it happens that none of the source files meet the */
/*     requested bodies or time coverages, then DATA will be .FALSE. */


/*     Indicates whether or not the current source SPK has contributed */
/*     data to the new SPK. */


/*     Save all. */


/*     Set error handling mode. */

    trcoff_();
    erract_("SET", "ABORT", (ftnlen)3, (ftnlen)5);

/*     Initialize all cells. */

    ssizec_(&c__6000, cmdsym, (ftnlen)100);
    ssizei_(&c__6000, cmdptr);
    ssizec_(&c__6000, cmdval, (ftnlen)300);
    ssizec_(&c__10000, winsym, (ftnlen)16);
    ssizei_(&c__10000, winptr);
    ssized_(&c__60000, winval);
    ssizec_(&c__10000, locsym, (ftnlen)16);
    ssizei_(&c__10000, locptr);
    ssized_(&c__60000, locval);
    ssized_(&c__40000, segwin);
    ssized_(&c__40000, addwin);
    ssized_(&c__40000, tmpwin);
    ssized_(&c__40000, locwin);
    ssized_(&c__40000, wind);
    ssized_(&c__40000, gwind);

/*     Initialize the WORK array.  Set all the values to UNUSED. */

    for (i__ = 1; i__ <= 10000; ++i__) {
	work[(i__1 = i__ - 1) < 10000 && 0 <= i__1 ? i__1 : s_rnge("work", 
		i__1, "spkmerge_", (ftnlen)719)] = -1;
    }

/*     Get the current time and date. This info will be stored in the log */
/*     file. */

/*               1234567890123456789012 */
    s_copy(tstamp, "    -  -  /  :  :     ", (ftnlen)22, (ftnlen)22);
    cputim_(tvec);
    dpfmt_(tvec, "YYYY", tstamp, (ftnlen)4, (ftnlen)4);
    dpfmt_(&tvec[1], "0M", tstamp + 5, (ftnlen)2, (ftnlen)2);
    dpfmt_(&tvec[2], "0D", tstamp + 8, (ftnlen)2, (ftnlen)2);
    dpfmt_(&tvec[3], "0h", tstamp + 11, (ftnlen)2, (ftnlen)2);
    dpfmt_(&tvec[4], "0m", tstamp + 14, (ftnlen)2, (ftnlen)2);
    dpfmt_(&tvec[5], "0s.tt", tstamp + 17, (ftnlen)5, (ftnlen)5);
    s_copy(text, "SPKMERGE -- SPK Merge Tool, Version #, SPICE Toolkit #", (
	    ftnlen)350, (ftnlen)54);
    tkvrsn_("toolkit", tkv, (ftnlen)7, (ftnlen)8);
    repmc_(text, "#", "3.4", text, (ftnlen)350, (ftnlen)1, (ftnlen)3, (ftnlen)
	    350);
    repmc_(text, "#", tkv, text, (ftnlen)350, (ftnlen)1, (ftnlen)8, (ftnlen)
	    350);
    tostdo_(text, (ftnlen)350);
    tostdo_(" ", (ftnlen)1);

/*     Check the command line for the name of the command file. */

    getcml_(line, (ftnlen)350);

/*     If the line is empty, then prompt the user for the name of the */
/*     command file to process. */

    if (s_cmp(line, " ", (ftnlen)350, (ftnlen)1) == 0) {
	tostdo_("Enter the name of the command file", (ftnlen)34);
	tostdo_(" ", (ftnlen)1);
	prompt_("> ", line, (ftnlen)2, (ftnlen)350);
	tostdo_(" ", (ftnlen)1);
    }

/*     Parse the command file and store the contents in key/value */
/*     pairs in the command symbol table. */

    rdcmd_(line, cmdsym, cmdptr, cmdval, (ftnlen)350, (ftnlen)100, (ftnlen)
	    300);

/*     Fetch the LSK file name from the command file symbol table. */

    sygetc_("LEAPSECONDS_KERNEL", cmdsym, cmdptr, cmdval, &n, value, &fnd, (
	    ftnlen)18, (ftnlen)100, (ftnlen)300, (ftnlen)300);
    if (! fnd) {
	chkin_("SPKMERGE", (ftnlen)8);
	setmsg_("This error is never supposed to occur. Please contact NAIF.",
		 (ftnlen)59);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("SPKMERGE", (ftnlen)8);
	byebye_("FAILURE", (ftnlen)7);
    }

/*     Load the LSK. */

    furnsh_(value, (ftnlen)300);

/*     Start with the first SPK. */

    nspk = 0;

/*     Fetch the name of the first SPK to create out of the */
/*     command file symbol table. */

    sygetc_("SPK_KERNEL", cmdsym, cmdptr, cmdval, &nval, val, &fnd, (ftnlen)
	    10, (ftnlen)100, (ftnlen)300, (ftnlen)300);
    if (! fnd) {
	chkin_("SPKMERGE", (ftnlen)8);
	setmsg_("This error is never supposed to occur. Please contact NAIF.",
		 (ftnlen)59);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("SPKMERGE", (ftnlen)8);
	byebye_("FAILURE", (ftnlen)7);
    }

/*     Loop over the number of SPK files we are to create. Note: We are */
/*     using NVAL as a temporary variable, since we will make the same */
/*     call to SYGETC at the end of the DO WHILE loop. */

    while(nspk < nval) {

/*        Increment the NSPK file counter. */

	++nspk;

/*        Open the new file that we are about to create. */

	spkopn_(val + ((i__1 = nspk - 1) < 6000 && 0 <= i__1 ? i__1 : s_rnge(
		"val", i__1, "spkmerge_", (ftnlen)829)) * 300, "SPKMERGE", &
		c__0, &newhan, (ftnlen)300, (ftnlen)8);

/*        Set the DATA flag to .FALSE. for this new SPK file. */
/*        If it remains .FALSE. we will know to warn the user */
/*        that the output file contains no data. */

	data = FALSE_;

/*        Let the user know which SPK file we are creating. */

/* Writing concatenation */
	i__3[0] = 9, a__1[0] = "Creating ";
	i__3[1] = rtrim_(val + ((i__2 = nspk - 1) < 6000 && 0 <= i__2 ? i__2 :
		 s_rnge("val", i__2, "spkmerge_", (ftnlen)841)) * 300, (
		ftnlen)300), a__1[1] = val + ((i__1 = nspk - 1) < 6000 && 0 <=
		 i__1 ? i__1 : s_rnge("val", i__1, "spkmerge_", (ftnlen)841)) 
		* 300;
	s_cat(text, a__1, i__3, &c__2, (ftnlen)350);
	tostdo_(text, (ftnlen)350);

/*        Determine whether or not we are to create a log file. */
/*        Build the keyword that should contain the name of the */
/*        desired log for this new SPK. */


	crtptr_(ch__1, (ftnlen)100, "SPK_KERNEL", &nspk, "LOG_FILE", (ftnlen)
		10, (ftnlen)8);
	s_copy(symbol, ch__1, (ftnlen)100, (ftnlen)100);

/*        Retrieve the name of the log file. */

	sygetc_(symbol, cmdsym, cmdptr, cmdval, &n, value, &fnd, (ftnlen)100, 
		(ftnlen)100, (ftnlen)300, (ftnlen)300);

/*        Check to see whether we actually found the log file key in */
/*        the command file symbol table. */

	if (fnd) {

/*           If so, create the new log file. */

	    txtopn_(value, &logunt, (ftnlen)300);

/*           Set LOG to .TRUE. to let us know that we are keeping */
/*           a log.  Set NAME to the name of the log file we just */
/*           created. */

	    log__ = TRUE_;
	    s_copy(name__, value, (ftnlen)300, (ftnlen)300);
	} else {

/*           If the log file wasn't explicitly requested in the command */
/*           file, then open a scratch file.  We do this since we will */
/*           include the log in the comment region of the new SPK file. */

	    txtops_(&logunt);

/*           Set LOG to .FALSE. so that we know we're writing to */
/*           the scratch file instead. */

	    log__ = FALSE_;
	}

/*        Put a header on the log file. Include the name of the */
/*        SPK file we're creating: VAL(NSPK). */

	s_copy(line, "; $ LOG FILE", (ftnlen)350, (ftnlen)12);
	repmc_(line, "$", val + ((i__1 = nspk - 1) < 6000 && 0 <= i__1 ? i__1 
		: s_rnge("val", i__1, "spkmerge_", (ftnlen)899)) * 300, line, 
		(ftnlen)350, (ftnlen)1, (ftnlen)300, (ftnlen)350);
	writln_(line, &logunt, (ftnlen)350);
	writln_(" ", &logunt, (ftnlen)1);

/*        Add the time and date of creation. */

	s_copy(line, "; Created $.", (ftnlen)350, (ftnlen)12);
	repmc_(line, "$", tstamp, line, (ftnlen)350, (ftnlen)1, (ftnlen)22, (
		ftnlen)350);
	writln_(line, &logunt, (ftnlen)350);
	writln_(";", &logunt, (ftnlen)1);
	writln_("; BEGIN SPKMERGE COMMANDS", &logunt, (ftnlen)25);
	writln_(" ", &logunt, (ftnlen)1);

/*        Retrieve the name of the LSK used in the command file. */

	sygetc_("LEAPSECONDS_KERNEL", cmdsym, cmdptr, cmdval, &n, value, &fnd,
		 (ftnlen)18, (ftnlen)100, (ftnlen)300, (ftnlen)300);

/*        Write out the leapseconds file name to the log file. */

	wrdnln_("LEAPSECONDS_KERNEL", value, &c__1, &logunt, (ftnlen)18, (
		ftnlen)300);
	writln_(" ", &logunt, (ftnlen)1);

/*        Write the name of the SPK file that is being created. */

	wrdnln_("SPK_KERNEL", val + ((i__1 = nspk - 1) < 6000 && 0 <= i__1 ? 
		i__1 : s_rnge("val", i__1, "spkmerge_", (ftnlen)930)) * 300, &
		c__1, &logunt, (ftnlen)10, (ftnlen)300);

/*        If we are writing a log file, include its name. */

	if (log__) {
	    wrdnln_("LOG_FILE", name__, &c__3, &logunt, (ftnlen)8, (ftnlen)
		    300);
	}

/*        Include files are text files to include into the comment region */
/*        of the new SPK file. Build the name of the symbol that contains */
/*        all the include files for this SPK file. */

	crtptr_(ch__1, (ftnlen)100, "SPK_KERNEL", &nspk, "INCLUDE_TEXT_FILE", 
		(ftnlen)10, (ftnlen)17);
	s_copy(symbol, ch__1, (ftnlen)100, (ftnlen)100);

/*        Fetch the include file names. */

	sygetc_(symbol, cmdsym, cmdptr, cmdval, &nval, val, &fnd, (ftnlen)100,
		 (ftnlen)100, (ftnlen)300, (ftnlen)300);

/*        If we find some, then write their names in the log file. */

	if (fnd) {
	    i__1 = nval;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		wrdnln_("INCLUDE_TEXT_FILE", val + ((i__2 = i__ - 1) < 6000 &&
			 0 <= i__2 ? i__2 : s_rnge("val", i__2, "spkmerge_", (
			ftnlen)956)) * 300, &c__3, &logunt, (ftnlen)17, (
			ftnlen)300);
	    }
	}

/*        Open a scratch file for any comments we'll add. */

	txtops_(&cmtunt);

/*        Initialize the WIN symbol table and GWIND, as we are about to */
/*        retrieve the global bodies and times requested for this body. */

	scardc_(&c__0, winsym, (ftnlen)16);
	scardi_(&c__0, winptr);
	scardd_(&c__0, winval);
	scardd_(&c__0, gwind);

/*        Construct the key for this SPK's global BEGIN_TIME value. */

	crtptr_(ch__1, (ftnlen)100, "SPK_KERNEL", &nspk, "BEGIN_TIME", (
		ftnlen)10, (ftnlen)10);
	s_copy(begsym, ch__1, (ftnlen)100, (ftnlen)100);

/*        Fetch the global BEGIN_TIME time strings. */

	sygetc_(begsym, cmdsym, cmdptr, cmdval, &nval, val, &fnd, (ftnlen)100,
		 (ftnlen)100, (ftnlen)300, (ftnlen)300);

/*        If values are found, store them in the GWIND window. */

	if (fnd) {

/*           Loop over all the number of values in the BEGIN_TIME */
/*           symbol. */

	    i__1 = nval;
	    for (i__ = 1; i__ <= i__1; ++i__) {

/*              Convert the BEGIN_TIME value into ET. */

		str2et_(val + ((i__2 = i__ - 1) < 6000 && 0 <= i__2 ? i__2 : 
			s_rnge("val", i__2, "spkmerge_", (ftnlen)998)) * 300, 
			&begtim, (ftnlen)300);

/*              Now create the keyword for the companion END_TIME. */
/*              Note this is built from the value of I and the preceding */
/*              BEGSYM.  So we know we are getting the one and only mate */
/*              to BEGTIM. */

		crtptr_(ch__1, (ftnlen)100, begsym, &i__, "END_TIME", (ftnlen)
			100, (ftnlen)8);
		s_copy(symbol, ch__1, (ftnlen)100, (ftnlen)100);

/*              Fetch the END_TIME value from the command symbol table. */

		sygetc_(symbol, cmdsym, cmdptr, cmdval, &n, value, &fnd, (
			ftnlen)100, (ftnlen)100, (ftnlen)300, (ftnlen)300);
		if (! fnd) {
		    chkin_("SPKMERGE", (ftnlen)8);
		    setmsg_("This error is never supposed to occur Please co"
			    "ntact NAIF.", (ftnlen)58);
		    sigerr_("SPICE(BUG)", (ftnlen)10);
		    chkout_("SPKMERGE", (ftnlen)8);
		    byebye_("FAILURE", (ftnlen)7);
		}

/*              Convert the END_TIME string to ET. */

		str2et_(value, &endtim, (ftnlen)300);

/*              Put this interval into the global time window, GWIND. */

		wninsd_(&begtim, &endtim, gwind);
	    }

/*        If there are no values to store in GWIND, then just set the */
/*        bounds to DPMIN() and DPMAX(), i.e. collect all allowable ET */
/*        times. */

	} else {
	    d__1 = dpmin_();
	    d__2 = dpmax_();
	    wninsd_(&d__1, &d__2, gwind);
	}

/*        Now find all the global bodies that are to be merged into */
/*        the new file.  First build the keyword for the global bodies */
/*        destined for this SPK file. */

	crtptr_(ch__1, (ftnlen)100, "SPK_KERNEL", &nspk, "BODIES", (ftnlen)10,
		 (ftnlen)6);
	s_copy(symbol, ch__1, (ftnlen)100, (ftnlen)100);

/*        Fetch the values from the command symbol table. */

	sygetc_(symbol, cmdsym, cmdptr, cmdval, &n, value, &fnd, (ftnlen)100, 
		(ftnlen)100, (ftnlen)300, (ftnlen)300);

/*        If global bodies are found store them in the GBODY array. */

	if (fnd) {

/*           Parse the bodies character string. */

	    lparsm_(value, ", ", &c__10000, &ngbody, val, (ftnlen)300, (
		    ftnlen)2, (ftnlen)300);
	    i__1 = ngbody;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		nparsi_(val + ((i__2 = i__ - 1) < 6000 && 0 <= i__2 ? i__2 : 
			s_rnge("val", i__2, "spkmerge_", (ftnlen)1070)) * 300,
			 &gbody[(i__4 = i__ - 1) < 10000 && 0 <= i__4 ? i__4 :
			 s_rnge("gbody", i__4, "spkmerge_", (ftnlen)1070)], 
			err, &pnter, (ftnlen)300, (ftnlen)80);
	    }

/*        Otherwise, set NGBODY to 0, to indicate that there are no */
/*        global body merge requests. */

	} else {
	    ngbody = 0;
	}

/*        Start with the first SPK source file.  Build the symbol name */
/*        for the source SPKs and fetch their names from the command */
/*        symbol table. */

	crtptr_(ch__1, (ftnlen)100, "SPK_KERNEL", &nspk, "SOURCE_SPK_KERNEL", 
		(ftnlen)10, (ftnlen)17);
	s_copy(srcsym, ch__1, (ftnlen)100, (ftnlen)100);
	sygetc_(srcsym, cmdsym, cmdptr, cmdval, &nval, val, &fnd, (ftnlen)100,
		 (ftnlen)100, (ftnlen)300, (ftnlen)300);
	if (! fnd) {
	    chkin_("SPKMERGE", (ftnlen)8);
	    setmsg_("This error is never supposed to occur. Please contact N"
		    "AIF.", (ftnlen)59);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("SPKMERGE", (ftnlen)8);
	    byebye_("FAILURE", (ftnlen)7);
	}
	nsrc = 0;

/*        Loop over all the source SPK files. */

	while(nsrc < nval) {

/*           Increment the NSRC source SPK file counter. */

	    ++nsrc;

/*           Store the name of the source file in NAME. */

	    s_copy(name__, val + ((i__1 = nsrc - 1) < 6000 && 0 <= i__1 ? 
		    i__1 : s_rnge("val", i__1, "spkmerge_", (ftnlen)1118)) * 
		    300, (ftnlen)300, (ftnlen)300);

/*           Open the SPK file. */

	    dafopr_(name__, &oldhan, (ftnlen)300);

/*           Initialize USED to .FALSE. If we actually use data from */
/*           this source SPK file, then USED will be set to .TRUE. */

	    used = FALSE_;

/*           Determine the local bodies and times requested for this */
/*           source SPK from the command file.  Initialize the LOC symbol */
/*           table and the WIND window structure. */

	    scardc_(&c__0, locsym, (ftnlen)16);
	    scardi_(&c__0, locptr);
	    scardd_(&c__0, locval);
	    scardd_(&c__0, wind);

/*           Start by extracting the local BEGIN_TIME. First build the */
/*           keyword to go to the symbol table with. */

	    crtptr_(ch__1, (ftnlen)100, srcsym, &nsrc, "BEGIN_TIME", (ftnlen)
		    100, (ftnlen)10);
	    s_copy(begsym, ch__1, (ftnlen)100, (ftnlen)100);

/*           Now fetch the values from the command symbol table. */

	    sygetc_(begsym, cmdsym, cmdptr, cmdval, &nval, val, &fnd, (ftnlen)
		    100, (ftnlen)100, (ftnlen)300, (ftnlen)300);

/*           If local time windows are specified, then put all of the */
/*           intervals into the local time schedule WIND. */

	    if (fnd) {

/*              Loop over all the retrieved 'BEGIN_TIME' strings. */

		i__1 = nval;
		for (i__ = 1; i__ <= i__1; ++i__) {

/*                 Convert the start time string into ET. */

		    str2et_(val + ((i__2 = i__ - 1) < 6000 && 0 <= i__2 ? 
			    i__2 : s_rnge("val", i__2, "spkmerge_", (ftnlen)
			    1167)) * 300, &begtim, (ftnlen)300);

/*                 Find the companion 'END_TIME' value and retrieve it */
/*                 from the command file symbol table. */

		    crtptr_(ch__1, (ftnlen)100, begsym, &i__, "END_TIME", (
			    ftnlen)100, (ftnlen)8);
		    s_copy(symbol, ch__1, (ftnlen)100, (ftnlen)100);
		    sygetc_(symbol, cmdsym, cmdptr, cmdval, &n, value, &fnd, (
			    ftnlen)100, (ftnlen)100, (ftnlen)300, (ftnlen)300)
			    ;
		    if (! fnd) {
			chkin_("SPKMERGE", (ftnlen)8);
			setmsg_("This error is never supposed to occur. Plea"
				"se contact NAIF.", (ftnlen)59);
			sigerr_("SPICE(BUG)", (ftnlen)10);
			chkout_("SPKMERGE", (ftnlen)8);
			byebye_("FAILURE", (ftnlen)7);
		    }

/*                 Convert the end time string into ET. */

		    str2et_(value, &endtim, (ftnlen)300);

/*                 Insert the window into the local window WIND. */

		    wninsd_(&begtim, &endtim, wind);
		}

/*           If no local time windows were specified use the global */
/*           window. */

	    } else {
		copyd_(gwind, wind);
	    }

/*           Now retrieve the list of local bodies process for this */
/*           source SPK file.  First create the keyword to retrieve from */
/*           the command file symbol table, then fetch the values. */

	    crtptr_(ch__1, (ftnlen)100, srcsym, &nsrc, "BODIES", (ftnlen)100, 
		    (ftnlen)6);
	    s_copy(symbol, ch__1, (ftnlen)100, (ftnlen)100);
	    sygetc_(symbol, cmdsym, cmdptr, cmdval, &n, value, &fnd, (ftnlen)
		    100, (ftnlen)100, (ftnlen)300, (ftnlen)300);

/*           If we find values, parse the string and store the ID codes */
/*           in the local body array, BODY.  The number of local bodies */
/*           to process is stored in the variable NBODY. */

	    if (fnd) {
		lparsm_(value, ", ", &c__10000, &nbody, val, (ftnlen)300, (
			ftnlen)2, (ftnlen)300);
		i__1 = nbody;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    nparsi_(val + ((i__2 = i__ - 1) < 6000 && 0 <= i__2 ? 
			    i__2 : s_rnge("val", i__2, "spkmerge_", (ftnlen)
			    1226)) * 300, &body[(i__4 = i__ - 1) < 10000 && 0 
			    <= i__4 ? i__4 : s_rnge("body", i__4, "spkmerge_",
			     (ftnlen)1226)], err, &pnter, (ftnlen)300, (
			    ftnlen)80);
		}

/*           Otherwise, copy the global body array into the local one. */

	    } else {
		movei_(gbody, &ngbody, body);
		nbody = ngbody;
	    }

/*           If NBODY is 0, then grab all the bodies in the file */
/*           satisfying the time constraints.  Set ALL to .TRUE. to */
/*           indicate this state. */

	    if (nbody == 0) {
		all = TRUE_;
	    } else {
		all = FALSE_;
	    }

/*           Loop over all bodies listed and set WORK to UNUSED. */

	    i__1 = nbody;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		work[(i__2 = i__ - 1) < 10000 && 0 <= i__2 ? i__2 : s_rnge(
			"work", i__2, "spkmerge_", (ftnlen)1258)] = -1;
	    }

/*           Start from the last segment in the source SPK kernel. */
/*           Use the DAF array search routines to locate the */
/*           descriptors of interest. */

	    dafbbs_(&oldhan);
	    daffpa_(&fnd);

/*           Loop over all segments in the source file. Note: FND will */
/*           be reused in the loop below for other purposes.  The call */
/*           to DAFFPA just prior to loop exit will return the proper */
/*           value of FND to continue the loop. */

	    while(fnd) {

/*              Initialize the windows we will be using. */

		scardd_(&c__0, segwin);
		scardd_(&c__0, tmpwin);
		scardd_(&c__0, locwin);
		scardd_(&c__0, addwin);

/*              Retrieve the segment name and descriptor. Unpack the */
/*              descriptor. */

		dafgn_(segnam, (ftnlen)40);
		dafgs_(segsum);
		dafus_(segsum, &c__2, &c__6, dc, ic);

/*              Put the segment time boundaries into the SEGWIN */
/*              schedule. */

		wninsd_(dc, &dc[1], segwin);

/*              Intersect the segment time coverage with the times */
/*              stored in the local time window, WIND.  This will */
/*              return the desired times to subset from this segment. */

		wnintd_(segwin, wind, tmpwin);

/*              Copy the results of this intersection back into the */
/*              SEGWIN window. */

		copyd_(tmpwin, segwin);

/*              Store the IDCODE for the TARGET from the segment */
/*              descriptor in BOD. */

		bod = ic[0];

/*              See if BOD is one of the bodies that were requested */
/*              for the merge.  Note: If NBODY is 0, this will */
/*              immediately return 0. */

		j = isrchi_(&bod, &nbody, body);

/*              Now check to see if we are to include this body. */
/*              If ALL is .TRUE. or J is something other than 0 */
/*              then we have to consider including it. Otherwise just */
/*              ignore and proceed to the next segment. */

		if (all || j != 0) {

/*                 If ALL is .TRUE. and J is 0, we use start using the */
/*                 BODY array to hold values that we have looked at */
/*                 already. */

		    if (all && j == 0) {

/*                    Check to see if the number of bodies we are */
/*                    dealing with is greater than the BODY array */
/*                    can hold. */

			if (nbody < 10000) {

/*                       Augment NBODY to keep it as an accurate count */
/*                       of the number of elements stored in the BODY */
/*                       ARRAY. */

			    ++nbody;
			    j = nbody;

/*                       Store the current body in the BODY array. */

			    body[(i__1 = j - 1) < 10000 && 0 <= i__1 ? i__1 : 
				    s_rnge("body", i__1, "spkmerge_", (ftnlen)
				    1358)] = bod;

/*                    If the number of bodies exceeds MAXBOD, signal an */
/*                    error as this is the maximum number of BODIES */
/*                    SPKMERGE can process. */

			} else {
			    chkin_("SPKMERGE", (ftnlen)8);
			    setmsg_("The number of bodies to subset from sou"
				    "rce SPK # exceeds #.  This is the maximu"
				    "m number of bodies SPKMERGE can handle.", 
				    (ftnlen)118);
			    errch_("#", name__, (ftnlen)1, (ftnlen)300);
			    errint_("#", &c__10000, (ftnlen)1);
			    sigerr_("SPKMERGE(TOOMANYBODIES)", (ftnlen)23);
			    chkout_("SPKMERGE", (ftnlen)8);
			    byebye_("FAILURE", (ftnlen)7);
			}
		    }

/*                 Convert the body ID code for this segment into */
/*                 a string (keyword). */

		    intstr_(&bod, bodch, (ftnlen)16);

/*                 Retrieve the windows in time that have already */
/*                 been written to the new SPK file. */

		    sygetd_(bodch, winsym, winptr, winval, &n, &tmpwin[6], &
			    fnd, (ftnlen)16, (ftnlen)16);

/*                 Set N to 0, if no windows for this body are found. */

		    if (! fnd) {
			n = 0;
		    }

/*                 Initialize the cell size that holds the current */
/*                 windows. */

		    scardd_(&n, tmpwin);

/*                 Take the window from the available segment coverage, */
/*                 and subtract the times already placed into the SPK */
/*                 file. Place the results in ADDWIN. */

		    wndifd_(segwin, tmpwin, addwin);

/*                 In versions of the program prior to version 3.0.0 */
/*                 the following filter was performed on the ADD */
/*                 window.  At the time of the update to 3.0.0 this */
/*                 appears to create more problems than it solves. */
/*                 Hence the call to WNFLTD is commented out. */

/*                        WLT --- Sep 25, 1997 */

/*                  CALL WNFLTD (1.0D0,  ADDWIN) */

/*                 Determine how many "intervals" we need to include from */
/*                 this file. */

		    n = cardd_(addwin);

/*                 A positive N indicates we need to copy data from this */
/*                 segment into the new SPK file. */

		    if (n > 0) {

/*                    Set USED to TRUE, since we have used data from this */
/*                    source file. */

			used = TRUE_;

/*                    Set WORK(J) to ISUSED to indicate we included */
/*                    data for the BODY from this source SPK file. */

			work[(i__1 = j - 1) < 10000 && 0 <= i__1 ? i__1 : 
				s_rnge("work", i__1, "spkmerge_", (ftnlen)
				1448)] = 0;

/*                    Add the new segments to the SPK file. Note: */
/*                    the loop moves in steps of two to account */
/*                    for the way intervals are stored in the ADDWIN */
/*                    window. */

			i__1 = n;
			for (i__ = 1; i__ <= i__1; i__ += 2) {
			    spksub_(&oldhan, segsum, segnam, &addwin[(i__2 = 
				    i__ + 5) < 40006 && 0 <= i__2 ? i__2 : 
				    s_rnge("addwin", i__2, "spkmerge_", (
				    ftnlen)1457)], &addwin[(i__4 = i__ + 6) < 
				    40006 && 0 <= i__4 ? i__4 : s_rnge("addw"
				    "in", i__4, "spkmerge_", (ftnlen)1457)], &
				    newhan, (ftnlen)40);
			}

/*                    Now union the windows TMPWIN (windows already */
/*                    present in the new file) and ADDWIN (windows we */
/*                    just added to the file).  Store the results in */
/*                    SEGWIN. */

			wnunid_(tmpwin, addwin, segwin);

/*                    Add the results into the symbol table, overwriting */
/*                    what was previously present. */

			i__1 = cardd_(segwin);
			syputd_(bodch, &segwin[6], &i__1, winsym, winptr, 
				winval, (ftnlen)16, (ftnlen)16);

/*                    Fetch the windows from the LOC symbol table related */
/*                    to this body.  We do this to keep track of all the */
/*                    time coverages of all bodies we pull out of this */
/*                    source SPK. */

			sygetd_(bodch, locsym, locptr, locval, &n, &locwin[6],
				 &fnd, (ftnlen)16, (ftnlen)16);

/*                    If any are found then union the windows just added */
/*                    to the SPK with those from the LOC symbol table. */

			if (fnd) {

/*                       Initialize the cardinality of the LOCWIN */
/*                       window. */

			    scardd_(&n, locwin);

/*                       Union the contents of LOCWIN with the newly */
/*                       added segments windows, ADDWIN.  Store the */
/*                       results in TMPWIN. */

			    wnunid_(locwin, addwin, tmpwin);

/*                       Copy TMPWIN onto ADDWIN. */

			    copyd_(tmpwin, addwin);
			}

/*                    Place the new windows back into the LOC symbol */
/*                    table. */

			i__1 = cardd_(addwin);
			syputd_(bodch, &addwin[6], &i__1, locsym, locptr, 
				locval, (ftnlen)16, (ftnlen)16);
		    }
		}

/*              Retrieve the previous segment from this source SPK file. */
/*              Note: FND is set back to what the DO WHILE loop exit */
/*              condition expects. */

		daffpa_(&fnd);
	    }

/*           Add the name of the source file to the log file if data in */
/*           the file was used. */

	    if (used) {

/*              Set the DATA flag to .TRUE.  This indicates that we added */
/*              data to the new SPK file.  If DATA remains .FALSE.  then */
/*              we know that no data has been added to the new SPK. */

		data = TRUE_;

/*              Create the key that determines whether to include */
/*              comments from this source SPK file in the newly */
/*              created SPK, then fetch the value. */

		crtptr_(ch__1, (ftnlen)100, srcsym, &nsrc, "INCLUDE_COMMENTS",
			 (ftnlen)100, (ftnlen)16);
		s_copy(symbol, ch__1, (ftnlen)100, (ftnlen)100);
		sygetc_(symbol, cmdsym, cmdptr, cmdval, &n, value, &fnd, (
			ftnlen)100, (ftnlen)100, (ftnlen)300, (ftnlen)300);

/*              Extract the comments if we need to.  Do this only if */
/*              the first character of VALUE is 'Y' or 'y'. */

		if (fnd && (*(unsigned char *)value == 'Y' || *(unsigned char 
			*)value == 'y')) {

/*                 Extract the comments and append them to the text */
/*                 file at CMTUNT. */

		    spcec_(&oldhan, &cmtunt);

/*                 Set COMMNT to .TRUE. to indicate that we are */
/*                 including comments from this source SPK file. */

		    commnt = TRUE_;
		} else {

/*                 Otherwise set COMMNT to .FALSE. */

		    commnt = FALSE_;
		}

/*              Write the bodies and the times. Summarize them if */
/*              possible. */

		i__ = 1;

/*              Find the index of the first body in the BODY array which */
/*              contributed data to the new SPK file. */

		while(i__ <= nbody && work[(i__1 = i__ - 1) < 10000 && 0 <= 
			i__1 ? i__1 : s_rnge("work", i__1, "spkmerge_", (
			ftnlen)1591)] == -1) {
		    ++i__;
		}

/*              Loop over the remaining values of I... */

		while(i__ <= nbody) {

/*                 Write the name of the source SPK file to the LOG file. */

		    wrdnln_("SOURCE_SPK_KERNEL", name__, &c__3, &logunt, (
			    ftnlen)17, (ftnlen)300);

/*                 Write out whether the comments from this source file */
/*                 were included in the output SPK. */

		    if (commnt) {
			s_copy(value, "YES", (ftnlen)300, (ftnlen)3);
		    } else {
			s_copy(value, "NO", (ftnlen)300, (ftnlen)2);
		    }
		    wrdnln_("INCLUDE_COMMENTS", value, &c__5, &logunt, (
			    ftnlen)16, (ftnlen)300);

/*                 Now convert the ID code of the body we are currently */
/*                 examining to a string. */

		    intstr_(&body[(i__1 = i__ - 1) < 10000 && 0 <= i__1 ? 
			    i__1 : s_rnge("body", i__1, "spkmerge_", (ftnlen)
			    1625)], bodch, (ftnlen)16);

/*                 Fetch the time windows stored in the LOC symbol */
/*                 table for this body.  Place the windows into TMPWIN. */

		    sygetd_(bodch, locsym, locptr, locval, &n, &tmpwin[6], &
			    fnd, (ftnlen)16, (ftnlen)16);

/*                 Find any other remaining bodies that have the same */
/*                 time coverage as this one. */

		    i__1 = nbody;
		    for (j = i__; j <= i__1; ++j) {

/*                    Only consider the current body if included into the */
/*                    new SPK from this source file. */

			if (work[(i__2 = j - 1) < 10000 && 0 <= i__2 ? i__2 : 
				s_rnge("work", i__2, "spkmerge_", (ftnlen)
				1644)] == 0) {

/*                       Retrieve the time windows associated with the */
/*                       current body.  Place them into SEGWIN. */

			    intstr_(&body[(i__2 = j - 1) < 10000 && 0 <= i__2 
				    ? i__2 : s_rnge("body", i__2, "spkmerge_",
				     (ftnlen)1650)], bodch, (ftnlen)16);
			    sygetd_(bodch, locsym, locptr, locval, &m, &
				    segwin[6], &fnd, (ftnlen)16, (ftnlen)16);

/*                       Now, only consider these two bodies as */
/*                       equivalent if they have the same coverage. First */
/*                       check to see if the number of windows agree. */

			    if (m == n) {
				same = TRUE_;

/*                          Now check to see if each entry in the */
/*                          windows match. */

				i__2 = n;
				for (k = 1; k <= i__2; ++k) {
				    same = same && segwin[(i__4 = k + 5) < 
					    40006 && 0 <= i__4 ? i__4 : 
					    s_rnge("segwin", i__4, "spkmerge_"
					    , (ftnlen)1668)] == tmpwin[(i__5 =
					     k + 5) < 40006 && 0 <= i__5 ? 
					    i__5 : s_rnge("tmpwin", i__5, 
					    "spkmerge_", (ftnlen)1668)];
				}
			    } else {
				same = FALSE_;
			    }

/*                       If this and the original body have the same */
/*                       coverage, then set work of J to 1. */

			    if (same) {
				work[(i__2 = j - 1) < 10000 && 0 <= i__2 ? 
					i__2 : s_rnge("work", i__2, "spkmerg"
					"e_", (ftnlen)1683)] = 1;
			    }
			}
		    }

/*                 Build up the lines of text for the log file. */

		    s_copy(value, " ", (ftnlen)300, (ftnlen)1);

/*                 Build the BODIES line as it will be written to the */
/*                 LOG file. */

		    i__1 = nbody;
		    for (j = i__; j <= i__1; ++j) {

/*                    We need only consider bodies whose WORK value */
/*                    is 1. */

			if (work[(i__2 = j - 1) < 10000 && 0 <= i__2 ? i__2 : 
				s_rnge("work", i__2, "spkmerge_", (ftnlen)
				1706)] == 1) {

/*                       Restore the WORK value back to -1 to indicate we */
/*                       finished logging coverage for this body. */

			    work[(i__2 = j - 1) < 10000 && 0 <= i__2 ? i__2 : 
				    s_rnge("work", i__2, "spkmerge_", (ftnlen)
				    1712)] = -1;
			    suffix_(" #,", &c__0, value, (ftnlen)3, (ftnlen)
				    300);
			    repmi_(value, "#", &body[(i__2 = j - 1) < 10000 &&
				     0 <= i__2 ? i__2 : s_rnge("body", i__2, 
				    "spkmerge_", (ftnlen)1715)], value, (
				    ftnlen)300, (ftnlen)1, (ftnlen)300);
			}
		    }

/*                 Find the end of the BODIES list string. */

		    j = rtrim_(value, (ftnlen)300);

/*                 Remove the last ',' from the VALUE string. */

		    *(unsigned char *)&value[j - 1] = ' ';

/*                 Dump the keyword and value to the LOG file. */

		    wrdnln_("BODIES", value, &c__5, &logunt, (ftnlen)6, (
			    ftnlen)300);

/*                 Now write all the windows of time included for these */
/*                 bodies to the LOG file.  The times stored in the */
/*                 LOG are in UTC. */

		    i__1 = n;
		    for (j = 1; j <= i__1; j += 2) {
			et2utc_(&tmpwin[(i__2 = j + 5) < 40006 && 0 <= i__2 ? 
				i__2 : s_rnge("tmpwin", i__2, "spkmerge_", (
				ftnlen)1743)], "C", &c__3, value, (ftnlen)1, (
				ftnlen)300);
			wrdnln_("BEGIN_TIME", value, &c__5, &logunt, (ftnlen)
				10, (ftnlen)300);
			et2utc_(&tmpwin[(i__2 = j + 6) < 40006 && 0 <= i__2 ? 
				i__2 : s_rnge("tmpwin", i__2, "spkmerge_", (
				ftnlen)1746)], "C", &c__3, value, (ftnlen)1, (
				ftnlen)300);
			wrdnln_("END_TIME", value, &c__5, &logunt, (ftnlen)8, 
				(ftnlen)300);
		    }

/*                 Now find the next body for processing. */

		    while(i__ <= nbody && work[(i__1 = i__ - 1) < 10000 && 0 
			    <= i__1 ? i__1 : s_rnge("work", i__1, "spkmerge_",
			     (ftnlen)1754)] == -1) {
			++i__;
		    }
		}
	    }

/*           Next source SPK file.  Note FND will be restored to the */
/*           value it should assume.  Close the last source file to */
/*           free up resources. */

	    dafcls_(&oldhan);
	    sygetc_(srcsym, cmdsym, cmdptr, cmdval, &nval, val, &fnd, (ftnlen)
		    100, (ftnlen)100, (ftnlen)300, (ftnlen)300);
	}

/*        Add the trailing end marker to the log file */

	writln_(" ", &logunt, (ftnlen)1);
	writln_("; END SPKMERGE COMMANDS", &logunt, (ftnlen)23);

/*        Add the log file to the comment area. */

	al__1.aerr = 0;
	al__1.aunit = logunt;
	f_rew(&al__1);
	spcac_(&newhan, &logunt, " ", " ", (ftnlen)1, (ftnlen)1);
	cl__1.cerr = 0;
	cl__1.cunit = logunt;
	cl__1.csta = 0;
	f_clos(&cl__1);

/*        Add the include files to the comment area. */

	crtptr_(ch__1, (ftnlen)100, "SPK_KERNEL", &nspk, "INCLUDE_TEXT_FILE", 
		(ftnlen)10, (ftnlen)17);
	s_copy(symbol, ch__1, (ftnlen)100, (ftnlen)100);
	sygetc_(symbol, cmdsym, cmdptr, cmdval, &nval, val, &fnd, (ftnlen)100,
		 (ftnlen)100, (ftnlen)300, (ftnlen)300);
	if (fnd) {
	    i__1 = nval;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		txtopr_(val + ((i__2 = i__ - 1) < 6000 && 0 <= i__2 ? i__2 : 
			s_rnge("val", i__2, "spkmerge_", (ftnlen)1794)) * 300,
			 &unit, (ftnlen)300);
		spcac_(&newhan, &unit, " ", " ", (ftnlen)1, (ftnlen)1);
		cl__1.cerr = 0;
		cl__1.cunit = unit;
		cl__1.csta = 0;
		f_clos(&cl__1);
	    }
	}

/*        Add any comments to the include file. */

	al__1.aerr = 0;
	al__1.aunit = cmtunt;
	f_rew(&al__1);
	spcac_(&newhan, &cmtunt, " ", " ", (ftnlen)1, (ftnlen)1);
	cl__1.cerr = 0;
	cl__1.cunit = cmtunt;
	cl__1.csta = 0;
	f_clos(&cl__1);

/*        For this file to be useful, it should have some data. */

	if (! data) {
	    tostdo_("New SPK file contains no data!", (ftnlen)30);
	}
	tostdo_(" ", (ftnlen)1);

/*        Next SPK file. */

	dafcls_(&newhan);
	sygetc_("SPK_KERNEL", cmdsym, cmdptr, cmdval, &nval, val, &fnd, (
		ftnlen)10, (ftnlen)100, (ftnlen)300, (ftnlen)300);
    }
    byebye_("SUCCESS", (ftnlen)7);
    return 0;
} /* MAIN__ */

/* Main program alias */ int spkmerge_ () { MAIN__ (); return 0; }
