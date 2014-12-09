/* brief.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;
static integer c__100 = 100;
static integer c__1 = 1;
static integer c__4 = 4;
static integer c_b212 = 100000;
static integer c_b214 = 2000000;
static integer c__2 = 2;
static integer c__0 = 0;
static integer c__5 = 5;
static integer c__3 = 3;

/* $Program BRIEF ( BRIEF SPK or binary PCK summary ) */
/* Main program */ MAIN__(void)
{
    /* System generated locals */
    address a__1[4], a__2[2], a__3[3], a__4[5];
    integer i__1, i__2[4], i__3, i__4[2], i__5[3], i__6[5];
    char ch__1[25021], ch__2[25013], ch__3[79], ch__4[157], ch__5[80], ch__6[
	    158];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    logical l_le(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char arch[80], fall[8], fsec[8];
    static integer objg[2], nbod;
    static char fday[8];
    static logical keep;
    static char line[80];
    static logical help;
    static char fmin[8];
    static integer cent, nobj;
    static logical gdsp;
    static integer body[6], item;
    static doublereal when, from;
    static integer last;
    static logical cntr, sdsp, tdsp;
    static integer size, frst;
    static logical vrsn;
    static char pass1[25000], pass2[25000];
    static integer b, c__, e, i__, j;
    extern integer cardd_(doublereal *);
    static integer n, o;
    extern logical idmch_(integer *, integer *);
    static integer t;
    extern /* Subroutine */ int dafgs_(doublereal *), etcal_(doublereal *, 
	    char *, ftnlen), kdata_(integer *, char *, char *, char *, char *,
	     integer *, logical *, ftnlen, ftnlen, ftnlen, ftnlen);
    static integer frame;
    static logical obnam;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char fhelp[8];
    static integer fcent;
    extern logical beint_(char *, ftnlen);
    static logical sfile;
    static integer objct[3];
    static char ftsec[8], objnm[64];
    static integer class__;
    extern logical idset_(integer *, integer *);
    static char ffrom[8], frmat[32*2*2*2*2];
    static logical found;
    static char fcntr[8];
    static integer value;
    static logical timat;
    static char ftdsp[8];
    static logical havto;
    static char fhour[8], ftutc[8], ftdoy[8];
    static logical round;
    static char error[80];
    static logical first;
    extern integer rtrim_(char *, ftnlen);
    static integer start;
    static char fvrsn[8], fsort[8];
    static logical nonly, ulist;
    static char htext[80*50];
    static integer ndone;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen), 
	    rdnbl_(char *, char *, logical *, ftnlen, ftnlen), bodn2c_(char *,
	     integer *, logical *, ftnlen);
    static char utext[80*21];
    extern /* Subroutine */ int swapd_(doublereal *, doublereal *), dafus_(
	    doublereal *, integer *, integer *, doublereal *, integer *), 
	    objcf2_(I_fp, integer *, integer *, integer *, integer *), 
	    objcf1_(I_fp, integer *, integer *, integer *), rndem_(char *, 
	    logical *, integer *, integer *, doublereal *, doublereal *, char 
	    *, integer *, doublereal *, ftnlen, ftnlen);
    static doublereal dc[2];
    static integer ic[6], id;
    extern /* Subroutine */ int lx4sgn_(char *, integer *, integer *, integer 
	    *, ftnlen), daffna_(logical *);
    static integer hi, jg;
    static char etstr1[64];
    static logical at;
    static char etstr2[64];
    extern /* Subroutine */ int objadd_(integer *, integer *, integer *), 
	    dafbfs_(integer *);
    static logical ok;
    static integer handle;
    extern /* Subroutine */ int dafcls_(integer *);
    extern integer bigger_();
    static char fsbbod[8], fsbcen[8];
    extern integer objact_(integer *);
    static char fobnam[8];
    static doublereal to;
    static char objnam[64];
    static integer savbeg;
    static char fsfile[8];
    static logical havfil;
    static char objnmg[64];
    static integer objctg[3], object[3], savend, clssid;
    extern logical wnincd_(doublereal *, doublereal *, doublereal *);
    static logical havfrm;
    static char fmtpic[32];
    static integer objlis[400006], bodlst[706];
    static char srcfil[255];
    static doublereal filwin[100006], intval;
    extern integer intmin_(void), intmax_(void);
    static integer objsiz;
    static char fgroup[8], fnonly[8];
    static logical fromto;
    static char kertyp[80];
    extern logical exists_(char *, ftnlen);
    static char string[255];
    static doublereal winval[2000006];
    extern /* Subroutine */ int writit_(char *, ftnlen);
    static integer winptr[100006];
    static doublereal segwin[8];
    static char winsym[64*100006];
    static doublereal tmpwin[100006], segsum[6];
    static char timtyp[32], tabchr[1];
    static logical gotspk, gotpck;
    static integer scount, sindex;
    static char filtyp[32], source[255];
    extern /* Subroutine */ int erract_(char *, char *, ftnlen, ftnlen), 
	    objinl_(integer *, integer *, integer *), tkvrsn_(char *, char *, 
	    ftnlen, ftnlen), suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen), getcml_(char *, ftnlen), fndnwd_(char *, integer *, 
	    integer *, integer *, ftnlen), getfat_(char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen), furnsh_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), sigerr_(char *, ftnlen), replch_(char *, char *, char *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen), cltext_(char *, ftnlen);
    static integer beg;
    extern /* Subroutine */ int ktotal_(char *, integer *, ftnlen), tparse_(
	    char *, doublereal *, char *, ftnlen, ftnlen), nparsi_(char *, 
	    integer *, char *, integer *, ftnlen, ftnlen), namfrm_(char *, 
	    integer *, ftnlen), frinfo_(integer *, integer *, integer *, 
	    integer *, logical *), ssizec_(integer *, char *, ftnlen);
    static integer bod[2];
    extern /* Subroutine */ int ssizei_(integer *, integer *);
    static integer end, gap;
    static logical all;
    extern integer inc_();
    static char fat[8];
    static integer obj[2], ref;
    static logical fnd, eof;
    extern /* Subroutine */ int ssized_(integer *, doublereal *), dafopr_(
	    char *, integer *, ftnlen), scardd_(integer *, doublereal *), 
	    objsbf_(L_fp, integer *, integer *, integer *, logical *), 
	    wninsd_(doublereal *, doublereal *, doublereal *);
    static char fto[8];
    extern /* Subroutine */ int objfnd_(integer *, integer *, integer *, 
	    integer *, logical *), maknam_(integer *, integer *, logical *, 
	    char *, char *, ftnlen, ftnlen), sygetd_(char *, char *, integer *
	    , doublereal *, integer *, doublereal *, logical *, ftnlen, 
	    ftnlen), wnunid_(doublereal *, doublereal *, doublereal *), 
	    syputd_(char *, doublereal *, integer *, char *, integer *, 
	    doublereal *, ftnlen, ftnlen), objnth_(integer *, integer *, 
	    integer *, logical *);
    static integer num;
    extern /* Subroutine */ int filtem_(char *, logical *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, char *, 
	    integer *, doublereal *, ftnlen, ftnlen);
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);
    static integer low;
    extern /* Subroutine */ int objget_(integer *, integer *, integer *);
    static char tkv[64];
    static integer ptr;
    extern /* Subroutine */ int objcmp_(integer *), objset_(integer *, 
	    integer *, integer *), disply_(char *, logical *, logical *, 
	    logical *, logical *, integer *, char *, integer *, doublereal *, 
	    char *, char *, ftnlen, ftnlen, ftnlen, ftnlen), scardc_(integer *
	    , char *, ftnlen), scardi_(integer *, integer *), objmod_(integer 
	    *, integer *, integer *, integer *), chkout_(char *, ftnlen), 
	    byebye_(char *, ftnlen);

/* $ Abstract */

/*     BRIEF is a command-line utility program that displays a contents */
/*     and time coverage summary for one or more binary SPK or binary */
/*     PCK files. */

/*     For more information see BRIEF User's Guide. */

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

/*     BRIEF User's Guide */
/*     SPK Required Reading */
/*     PCK Required Reading */

/* $ Keywords */

/*     FILES */
/*     UTILITY */

/* $ Parameters */

/*     See these include files. */

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

/* $ Author_and_Institution */

/*     W.L. Taber     (NAIF) */
/*     B.V. Semenov   (NAIF) */

/* $ Version */

/* -    BRIEF Version 4.0.0, 08-SEP-2010 (BVS) */

/*        Moved WINRM from main program to this include file. */

/* -    BRIEF Version 3.0.0, 14-JAN-2008 (BVS) */

/*        Increased MAXBOD to 100,000 (from 20,000). */

/*        Increased CMDSIZ to 25,000 (from 4,000). */

/*        Updated version string and changed its format to */
/*        '#.#.#, Month DD, YYYY' (from '#.#.#'). */

/* -    BRIEF Version 1.0.0, 14-MAR-1996 (WLT) */

/*        Initial release. */

/* -& */

/*     The Version is stored as a string. */


/*     MAXUSE is the maximum number of bodies that can be explicitly */
/*     specified on the command line for brief summaries. */


/*     The longest command line that can be accommodated is */
/*     given by CMDSIZ */


/*     The maximum number of bodies that can be summarized is stored */
/*     in the parameter MAXBOD */


/*     The average number of intervals per body */


/*     The largest expected window */


/*     Room in the DP symbol table that holds all windows for all */
/*     objects. */


/*     End of inlcude file. */


/* $ Exceptions */

/*     1) If a file to be summarized is in transfer format, the error */
/*        SPICE(TRANSFERFORMAT) is signaled. */

/*     2) If a file to be summarized is a binary EK, the error */
/*        SPICE(EKFILE) is signaled. */

/*     3) If a file to be summarized is not a DAF, binary EK, or */
/*        transfer format file, the error SPICE(BADARCHITECTURE) is */
/*        signaled. */

/*     4) If a file to be summarized is a CK file, the error */
/*        SPICE(CKFILE) is signaled. */

/*     5) If a file to be summarized is a DAF but not an SPK, binary */
/*        PCK, of a CK file, the error SPICE(BADKERNELTYPE) is signaled. */

/*     6) If any of the UTC command lines options was provided but */
/*        an LSK file wasn't, an error related to time conversion is */
/*        signaled by the routines in the calling tree of the program. */

/*     7) If any operation on the objects storing summary information */
/*        fails, an error is signaled by the routines in the calling */
/*        tree of the program. */

/*     8) If no binary SPK or PCK files were provided on the command */
/*        line and/or list file, the error SPICE(NOFILES) is signaled. */

/*     9) If the list file specified with -f does not exists, the error */
/*        SPICE(BADLISTFILENAME) is signaled. */

/*     10) If the file specified with -f is a kernel, the error */
/*        SPICE(NOTATEXTFILE) is signaled. */

/*     11) If SPKs and PCKs are provided in the same run, the error */
/*        SPICE(NOCANDOSPKSPCKS) is signaled. */

/*     12) If -f is not followed by a file name, the error */
/*        SPICE(NOLISTFILENAME) is signaled. */

/*     13) If TPARSE fails to parse time provided with -from, the error */
/*         SPICE(BADFROMTIME) is signaled. */

/*     14) If -from is not followed by a time string, the error */
/*        SPICE(NOFROMTIME) is signaled. */

/*     13) If TPARSE fails to parse time provided with -to, the error */
/*        SPICE(BADTOTIME) is signaled. */

/*     14) If -to is not followed by a time string, the error */
/*        SPICE(NOTOTIME) is signaled. */

/*     13) If TPARSE fails to parse time provided with -at, the error */
/*        SPICE(BADATTIME) is signaled.                   ) */

/*     14) If -at is not followed by a time string, the error */
/*        SPICE(NOATTIME) is signaled. */

/* $ Files */

/*     See User's Guide. */

/* $ Particulars */

/*     See User's Guide. */

/* $ Examples */

/*     See User's Guide. */

/* $ Restrictions */

/*     See User's Guide. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Nachman   (JPL) */
/*     B.V. Semenov   (JPL) */
/*     W.L. Taber     (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    BRIEF Version 4.0.0, 08-SEP-2010 (BVS) */

/*        Added sorted-by-time tabular output (new option -s). */

/*        Moved WINRM parameter definition to brief.inc. */

/* -    BRIEF Version 3.0.0, 14-JAN-2008 (BVS)(NJB)(EDW) */

/*        New capabilities including: summarizing binary PCK files; */
/*        loading text kernels; summarizing SPK files provided in */
/*        meta-kernels; displaying times as UTC (-utc), UTC/DOY */
/*        (-utcdoy), and ET seconds (-etsec); detecting unsupported */
/*        kernel types (CK, EK, transfer format, etc.); processing */
/*        reversed ID range and time boundaries. */

/*        Report format changes including: new format of the version */
/*        string; shorter usage message for blank command line; modified */
/*        help message for -h; more legible "No data" reports for object */
/*        and time constraints; PCK-specific wording for PCK summary */
/*        reports. */

/*        Previous functionality fixes including: eliminating empty */
/*        reports when no files were provided; processing TABs in list */
/*        files; accepting negative numbers in ID range assignments; */
/*        processing body filters specifying the same body in different */
/*        ways; reporting "No data" for time constraints; restricting */
/*        help display to only times when -h is provided. */

/*        Bug fixes including: hanging up when no value was provided */
/*        after -from, -to, -at, or -f; logic expression error in the */
/*        assignment FROMTO = HAVFRM .AND. HAVTO .AND. FROM .LE. TO */
/*        (caused failure on IFORT). */

/*        Internal buffer size increases including: MAXBOD to 100,000 */
/*        (from 20,000); CMDSIZ to 25,000 (from 4,000). */

/*        Completely rewrite of the User's Guide. */

/* -    BRIEF Version 2.4.0, 11-NOV-2005 (BVS) */

/*        Removed copyright note from usage display for old */
/*        MAC environments. */

/* -    BRIEF Version 2.3.1, 28-OCT-2005 (EDW) */

/*        Edited the DISPLY and OBJFND subroutine to */
/*        remove a duplicate argument (used as both input */
/*        and output) from an OBJNXT call. Optimized versions */
/*        of such calls can fail on some platforms. */

/* -    BRIEF Version 2.3.0, 13-MAY-2004 (NJB) */

/*        Added subroutine-scope SAVE statement to support use of CSPICE */
/*        under cygwin.  Also added SAVE statement to subroutine DISPLY. */

/* -    BRIEF Version 2.2.0, 25-JUN-2002 (BVS) */

/*        Increased the number of bodies (MAXBOD) to 20,000. */
/*        Cleaned up version strings in this section. */

/* -    BRIEF Version 2.1.5, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are WIN-NT */

/* -    BRIEF Version 2.1.4, 21-SEP-1999 (NJB) */

/*        A bogus environment line was deleted.  Some */
/*        typos were corrected. */

/* -    BRIEF Version 2.1.3, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    BRIEF Version 2.1.2, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    BRIEF Version 2.1.1, 03-APR-1998 (NJB) */

/*        Added type declarations for the functions BIGGER and INC. */
/*        This was done to enable porting to the PC/Linux/Fort77 */
/*        environment. */

/* -    BRIEF Version 2.1.0, 21-JUL-1997 (WLT) */

/*        Added a header and modified the program so that it will */
/*        print out the version of SPICELIB it was linked against. */

/* -    BRIEF Version 2.0.0, 14-MAR-1996 (WLT) */

/*        The program was completely re-written from Mike Spencer's */
/*        original version.  Only the name is the same. */

/* -& */

/*     Passed Functions */

/* $ Abstract */

/*    Constants required by the family of "object" routines. */

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

/*     OBJECTS */

/* $ Parameters */

/*     LBCELL   is the lower bound for all cells used throughout */
/*              the SPICE library.. */

/*     NULL     is a constant used to indicate that a particular */
/*              object in a list is unused. */

/*     RMPOBJ   is the slot in the object list that tells how */
/*              many values are stored for each object.  I.E. */
/*              the number of values stored for each object */
/*              in an object list OBJLIS is OBJLIS(RMPOBJ). */

/*     NACTIV   is the slot in an object list that tells hows */
/*              many objects in the list are currently active. */
/*              In otherwords the number of active objects */
/*              in the object list OBJLIS is OBJLIS(NACTIV) */

/*     LSTID    is the slot in an object list that gives the */
/*              last object unique ID that was assigned. */
/*              In otherwords, the value of the last unique */
/*              object ID code in the object list OBJLIS */
/*              is OBJLIS(LSTID). */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Not Applicable */

/* $ Particulars */

/*     This include file contains the parameters used by the */
/*     family of object routines. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 23-FEB-1996 (WLT) */


/* -& */

/*     Spicelib Functions */


/*     Parameters */


/*     Other Parameters */


/*     Local variables */


/*     GETFAT arguments */


/*     Command line and parsing variables. */


/*     Control Flags */


/*     The block of character strings declared below are */
/*     for the recognized options the user can supply */
/*     on the command line. */


/*     FNONLY is the string used for the number onlys */
/*             command line flag. */

/*     FOBNAM is the string used for the order by */
/*             body name command line flag. */

/*     FTDSP  is the string used for the tabular display */
/*             command line flag. */

/*     FCNTR  is the string used for the include centers */
/*             command line flag. */

/*     FHELP  is the string used for the help command line flag. */

/*     FVRSN  is the string used for the version command line flag. */

/*     FALL   is the string used for the combine all files */
/*             command line flag. */

/*     FGROUP is the string used for grouping lines by coverage */
/*            in tabular output. */

/*     FSORT  is the string used for sorting lines by times for */
/*            each body in tabular output. */

/*     FSBBOD is the select by body id flag. */

/*     FSBCEN is the select by center id flag. */

/*     FSEC   is the flag used to indicate that coverages should */
/*            be rounded inward to the nearest second. */

/*     FMIN   is the flag used to indicate that coverages should */
/*            be rounded inward to the nearest minute. */

/*     FHOUR  is the flag used to indicate that coverages should */
/*            be rounded inward to the nearest hour. */

/*     FDAY   is the flag used to indicate that coverages should */
/*            be rounded inward to the nearest day. */

/*     FAT    is the flag to restrict the summary to only those */
/*            objects that have coverage at a specific epoch. */

/*     FFROM  is the flag to indicate the lower bound of an interval */
/*            that the coverage for an object must contain */

/*     FTO    is the flag to indicate the upper bound of an interval */
/*            that the coverage for an object must contain */

/*     FSFILE is the flag to indicate that a file of SPK's to read */
/*            is available. */

/*     FTUTC  is the flag to indicate that times must be displayed as */
/*            calendar UTC. */

/*     FTDOY  is the flag to indicate that times must be displayed as */
/*            DOY UTC. */

/*     FTSEC  is the flag to indicate that times must be displayed as */
/*            ET seconds. */


/*     Parsed Control Values. */


/*     The next block of variables are used to determine how */
/*     objects will be represented in the output.  This is */
/*     discussed in more detail below when the format strings */
/*     are defined. */


/*     The items below are used for the explicitly specified */
/*     list of bodies to summarize.  The enumeration IDCODE */
/*     through MATCHS are the components associated with */
/*     each item of the explicit list. */


/*     IDCODE */
/*     BEGNAM */
/*     ENDNAM */
/*     MATCHS */


/*     The next two parameters are simply utility parameters */
/*     and shouldn't be messed with. */


/*     The actual object variables are below. */


/*     The objects encountered in reading through the files. */


/*     Help Text Variables */


/*     Window and Window Table Variables. */


/*     DAF Variables */


/*     Segment summary components */


/*     Utility Variables */


/*     String indicating the output time type. */


/*     Counter of how many SPKs and PCKs were processed. */


/*     TAB character */


/*     Flags indicating whether at least one SPK and and at least */
/*     one PCK have been processed. */


/*     Count and index of SPKs loaded via meta-kernels. */


/*     KDATA outputs */


/*     Strings to hold output from and to ETs */


/*     Saved variables */

/*     The SAVE statement that appears here causes f2c to create local */
/*     variables with static duration. This enables the CSPICE version */
/*     of brief to run under cygwin. */


/*     SPICELIB error handling. */

    chkin_("BRIEF", (ftnlen)5);
    erract_("SET", "ABORT", (ftnlen)3, (ftnlen)5);

/*     "patterns" that can be used to display names of objects are: */

/*     Pattern 1 (p1)    Name (xxxx) */
/*     Pattern 2 (p2)    xxxx */
/*     Pattern 3 (p3)    xxxx Name */

/*     Where xxxx stands for the numeric code for the object. */
/*     Note: if a name cannot be found for an object pattern 2 */
/*     is automatically used. */

/*     The "names" of objects are made by a combination of one or */
/*     possibly two body names.  The format used to create the */
/*     name of the object is driven by the options that are */
/*     in force from the command line. */

/*     Here's where the various format parameters go and the */
/*     corresponding formats.  These formats will be passed */
/*     to the display module along with the values of */
/*     n, c, o, and t. The display module has the responsibility */
/*     of taking apart the format and constructing appropriate */
/*     names. */

/*             n  c  o  t */

    s_copy(frmat, "p1", (ftnlen)32, (ftnlen)2);
    s_copy(frmat + 32, "p2", (ftnlen)32, (ftnlen)2);
    s_copy(frmat + 64, "p1 w.r.t. p1", (ftnlen)32, (ftnlen)12);
    s_copy(frmat + 96, "p2 w.r.t. p2", (ftnlen)32, (ftnlen)12);
    s_copy(frmat + 128, "p1", (ftnlen)32, (ftnlen)2);
    s_copy(frmat + 160, "p2", (ftnlen)32, (ftnlen)2);
    s_copy(frmat + 192, "p1 w.r.t. p1", (ftnlen)32, (ftnlen)12);
    s_copy(frmat + 224, "p2 w.r.t. p2", (ftnlen)32, (ftnlen)12);
    s_copy(frmat + 256, "p3", (ftnlen)32, (ftnlen)2);
    s_copy(frmat + 288, "p2", (ftnlen)32, (ftnlen)2);
    s_copy(frmat + 320, "p3 w.r.t. p3", (ftnlen)32, (ftnlen)12);
    s_copy(frmat + 352, "p2 w.r.t. p2", (ftnlen)32, (ftnlen)12);
    s_copy(frmat + 384, "p1", (ftnlen)32, (ftnlen)2);
    s_copy(frmat + 416, "p2", (ftnlen)32, (ftnlen)2);
    s_copy(frmat + 448, "p1 w.r.t. p1", (ftnlen)32, (ftnlen)12);
    s_copy(frmat + 480, "p2 w.r.t. p2", (ftnlen)32, (ftnlen)12);

/*     Usage text. */

    s_copy(utext, "   BRIEF is a command-line utility program that displays "
	    "a summary for", (ftnlen)80, (ftnlen)70);
    s_copy(utext + 80, "   one or more binary SPK or binary PCK files. The p"
	    "rogram usage is:", (ftnlen)80, (ftnlen)68);
    s_copy(utext + 160, " ", (ftnlen)80, (ftnlen)1);
    s_copy(utext + 240, "      % brief [-options] file [file ...]", (ftnlen)
	    80, (ftnlen)40);
    s_copy(utext + 320, " ", (ftnlen)80, (ftnlen)1);
    s_copy(utext + 400, "   The most useful options are shown below. For the"
	    " complete set of", (ftnlen)80, (ftnlen)67);
    s_copy(utext + 480, "   options, run BRIEF with the -h option. The order"
	    " of options is not", (ftnlen)80, (ftnlen)69);
    s_copy(utext + 560, "   significant. The case of option keys is signific"
	    "ant: they must be", (ftnlen)80, (ftnlen)68);
    s_copy(utext + 640, "   lowercase as shown below.", (ftnlen)80, (ftnlen)
	    28);
    s_copy(utext + 720, " ", (ftnlen)80, (ftnlen)1);
    s_copy(utext + 800, "      -c           display centers of motion/relati"
	    "ve-to frames", (ftnlen)80, (ftnlen)63);
    s_copy(utext + 880, "      -t           display summary in a tabular for"
	    "mat", (ftnlen)80, (ftnlen)54);
    s_copy(utext + 960, "      -a           treat all files as a single file",
	     (ftnlen)80, (ftnlen)51);
    s_copy(utext + 1040, "      -utc         display times in UTC calendar d"
	    "ate format (needs LSK)", (ftnlen)80, (ftnlen)72);
    s_copy(utext + 1120, "      -utcdoy      display times in UTC day-of-yea"
	    "r format (needs LSK)", (ftnlen)80, (ftnlen)70);
    s_copy(utext + 1200, "      -etsec       display times as ET seconds pas"
	    "t J2000", (ftnlen)80, (ftnlen)57);
    s_copy(utext + 1280, " ", (ftnlen)80, (ftnlen)1);
    s_copy(utext + 1360, "   An LSK file must be provided on the command lin"
	    "e to display times in", (ftnlen)80, (ftnlen)71);
    s_copy(utext + 1440, "   UTC formats. FK file(s) must be provided on the"
	    " command line to", (ftnlen)80, (ftnlen)66);
    s_copy(utext + 1520, "   display names of any frames that are not built "
	    "into the Toolkit.", (ftnlen)80, (ftnlen)67);
    s_copy(utext + 1600, " ", (ftnlen)80, (ftnlen)1);

/*     Help text. */

    s_copy(htext, "   BRIEF is a command-line utility program that displays "
	    "a summary for", (ftnlen)80, (ftnlen)70);
    s_copy(htext + 80, "   one or more binary SPK or binary PCK files. The p"
	    "rogram usage is:", (ftnlen)80, (ftnlen)68);
    s_copy(htext + 160, " ", (ftnlen)80, (ftnlen)1);
    s_copy(htext + 240, "      % brief [-options] file [file ...]", (ftnlen)
	    80, (ftnlen)40);
    s_copy(htext + 320, " ", (ftnlen)80, (ftnlen)1);
    s_copy(htext + 400, "   File names and options can be specified in any o"
	    "rder. Text kernels", (ftnlen)80, (ftnlen)69);
    s_copy(htext + 480, "   needed for time conversions and/or body/frame ID"
	    "-to-name conversions can", (ftnlen)80, (ftnlen)75);
    s_copy(htext + 560, "   be specified in addition to the SPK or PCK file("
	    "s) to be summarized.", (ftnlen)80, (ftnlen)71);
    s_copy(htext + 640, "   SPK files and PCK files cannot be summarized at "
	    "the same time.", (ftnlen)80, (ftnlen)65);
    s_copy(htext + 720, " ", (ftnlen)80, (ftnlen)1);
    s_copy(htext + 800, "   Options are shown below. The order of options is"
	    " not significant.", (ftnlen)80, (ftnlen)68);
    s_copy(htext + 880, "   The case of option keys is significant: they mus"
	    "t be lowercase as", (ftnlen)80, (ftnlen)68);
    s_copy(htext + 960, "   shown below.", (ftnlen)80, (ftnlen)15);
    s_copy(htext + 1040, " ", (ftnlen)80, (ftnlen)1);
    s_copy(htext + 1120, "      -t           display summary in a tabular fo"
	    "rmat", (ftnlen)80, (ftnlen)54);
    s_copy(htext + 1200, "      -a           treat all files as a single file"
	    , (ftnlen)80, (ftnlen)51);
    s_copy(htext + 1280, "      -c           display centers of motion/relat"
	    "ive-to frames", (ftnlen)80, (ftnlen)63);
    s_copy(htext + 1360, " ", (ftnlen)80, (ftnlen)1);
    s_copy(htext + 1440, "      -utc         display times in UTC calendar d"
	    "ate format (needs LSK)", (ftnlen)80, (ftnlen)72);
    s_copy(htext + 1520, "      -utcdoy      display times in UTC day-of-yea"
	    "r format (needs LSK)", (ftnlen)80, (ftnlen)70);
    s_copy(htext + 1600, "      -etsec       display times as ET seconds pas"
	    "t J2000", (ftnlen)80, (ftnlen)57);
    s_copy(htext + 1680, " ", (ftnlen)80, (ftnlen)1);
    s_copy(htext + 1760, "      -sec         display times \"rounded inwar"
	    "d\" to second", (ftnlen)80, (ftnlen)59);
    s_copy(htext + 1840, "      -min         display times \"rounded inwar"
	    "d\" to minute", (ftnlen)80, (ftnlen)59);
    s_copy(htext + 1920, "      -hour        display times \"rounded inwar"
	    "d\" to hour", (ftnlen)80, (ftnlen)57);
    s_copy(htext + 2000, "      -day         display times \"rounded inwar"
	    "d\" to day", (ftnlen)80, (ftnlen)56);
    s_copy(htext + 2080, " ", (ftnlen)80, (ftnlen)1);
    s_copy(htext + 2160, "      -s           display summary sorted by start"
	    " time for each body/frame", (ftnlen)80, (ftnlen)75);
    s_copy(htext + 2240, "      -g           display summary grouped by cove"
	    "rage", (ftnlen)80, (ftnlen)54);
    s_copy(htext + 2320, "      -n           display bodies/frames using onl"
	    "y numeric id-codes", (ftnlen)80, (ftnlen)68);
    s_copy(htext + 2400, "      -o           display summary ordered by body"
	    "/frame name", (ftnlen)80, (ftnlen)61);
    s_copy(htext + 2480, " ", (ftnlen)80, (ftnlen)1);
    s_copy(htext + 2560, "      -[bod]       display summary for body [bod]", 
	    (ftnlen)80, (ftnlen)49);
    s_copy(htext + 2640, "      -sb[bod]     display summary for body [bod]", 
	    (ftnlen)80, (ftnlen)49);
    s_copy(htext + 2720, "      -sc[cen]     display summary for center of m"
	    "otion/relative-to frame [cen]", (ftnlen)80, (ftnlen)79);
    s_copy(htext + 2800, "      -at [time]   display summary if coverage con"
	    "tains epoch [time]", (ftnlen)80, (ftnlen)68);
    s_copy(htext + 2880, "      -from [beg]  display summary if coverage con"
	    "tains interval [beg]:[end]", (ftnlen)80, (ftnlen)76);
    s_copy(htext + 2960, "      -to [end]    display summary if coverage con"
	    "tains interval [beg]:[end]", (ftnlen)80, (ftnlen)76);
    s_copy(htext + 3040, " ", (ftnlen)80, (ftnlen)1);
    s_copy(htext + 3120, "      -f [list]    summarize kernels listed in the"
	    " [list] file", (ftnlen)80, (ftnlen)62);
    s_copy(htext + 3200, " ", (ftnlen)80, (ftnlen)1);
    s_copy(htext + 3280, "      -h           display help", (ftnlen)80, (
	    ftnlen)31);
    s_copy(htext + 3360, "      -v           display version", (ftnlen)80, (
	    ftnlen)34);
    s_copy(htext + 3440, " ", (ftnlen)80, (ftnlen)1);
    s_copy(htext + 3520, "   An LSK file must be provided on the command lin"
	    "e to display times in", (ftnlen)80, (ftnlen)71);
    s_copy(htext + 3600, "   UTC formats. FK file(s) must be provided on the"
	    " command line to", (ftnlen)80, (ftnlen)66);
    s_copy(htext + 3680, "   display names of any frames that are not built "
	    "into the Toolkit.", (ftnlen)80, (ftnlen)67);
    s_copy(htext + 3760, " ", (ftnlen)80, (ftnlen)1);
    s_copy(htext + 3840, "   See the BRIEF User's Guide for more information."
	    , (ftnlen)80, (ftnlen)51);
    s_copy(htext + 3920, " ", (ftnlen)80, (ftnlen)1);

/*     Command line keys. */

    s_copy(fnonly, "-n", (ftnlen)8, (ftnlen)2);
    s_copy(fobnam, "-o", (ftnlen)8, (ftnlen)2);
    s_copy(ftdsp, "-t", (ftnlen)8, (ftnlen)2);
    s_copy(fcntr, "-c", (ftnlen)8, (ftnlen)2);
    s_copy(fhelp, "-h", (ftnlen)8, (ftnlen)2);
    s_copy(fvrsn, "-v", (ftnlen)8, (ftnlen)2);
    s_copy(fall, "-a", (ftnlen)8, (ftnlen)2);
    s_copy(fsbbod, "-sb", (ftnlen)8, (ftnlen)3);
    s_copy(fsbcen, "-sc", (ftnlen)8, (ftnlen)3);
    s_copy(fsec, "-sec", (ftnlen)8, (ftnlen)4);
    s_copy(fmin, "-min", (ftnlen)8, (ftnlen)4);
    s_copy(fhour, "-hour", (ftnlen)8, (ftnlen)5);
    s_copy(fday, "-day", (ftnlen)8, (ftnlen)4);
    s_copy(fat, "-at", (ftnlen)8, (ftnlen)3);
    s_copy(ffrom, "-from", (ftnlen)8, (ftnlen)5);
    s_copy(fto, "-to", (ftnlen)8, (ftnlen)3);
    s_copy(fgroup, "-g", (ftnlen)8, (ftnlen)2);
    s_copy(fsort, "-s", (ftnlen)8, (ftnlen)2);
    s_copy(fsfile, "-f", (ftnlen)8, (ftnlen)2);
    s_copy(ftutc, "-utc", (ftnlen)8, (ftnlen)4);
    s_copy(ftdoy, "-utcdoy", (ftnlen)8, (ftnlen)7);
    s_copy(ftsec, "-etsec", (ftnlen)8, (ftnlen)6);

/*     Initial values. */

    nonly = FALSE_;
    obnam = FALSE_;
    tdsp = FALSE_;
    gdsp = FALSE_;
    sdsp = FALSE_;
    cntr = FALSE_;
    help = FALSE_;
    vrsn = FALSE_;
    havfil = FALSE_;
    all = FALSE_;
    ulist = FALSE_;
    round = FALSE_;
    timat = FALSE_;
    havfrm = FALSE_;
    havto = FALSE_;
    fromto = FALSE_;
    at = FALSE_;
    sfile = FALSE_;
    objsiz = 2;
    intval = 86400.;
    s_copy(timtyp, "ETCAL", (ftnlen)32, (ftnlen)5);
    ndone = 0;
    *(unsigned char *)tabchr = '\t';
    gotspk = FALSE_;
    gotpck = FALSE_;
    s_copy(srcfil, " ", (ftnlen)255, (ftnlen)1);
    s_copy(pass2, " ", (ftnlen)25000, (ftnlen)1);
    s_copy(pass1, " ", (ftnlen)25000, (ftnlen)1);

/*     Initialize the list of objects that we will search for. */

    objinl_(&c__6, &c__100, bodlst);

/*     Construct and display program version. */

    tkvrsn_("TOOLKIT", tkv, (ftnlen)7, (ftnlen)64);
    s_copy(line, "BRIEF -- Version", (ftnlen)80, (ftnlen)16);
    suffix_("4.0.0, September 8, 2010        ", &c__1, line, (ftnlen)32, (
	    ftnlen)80);
    suffix_("-- Toolkit Version", &c__1, line, (ftnlen)18, (ftnlen)80);
    suffix_(tkv, &c__1, line, (ftnlen)64, (ftnlen)80);
    writit_(" ", (ftnlen)1);
    writit_(line, (ftnlen)80);
    writit_(" ", (ftnlen)1);

/*     Get command line. */

    getcml_(pass1, (ftnlen)25000);

/*     If command line is blank, display usage and stop. */

    if (s_cmp(pass1, " ", (ftnlen)25000, (ftnlen)1) == 0) {
	for (i__ = 1; i__ <= 21; ++i__) {
	    writit_(utext + ((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : 
		    s_rnge("utext", i__1, "brief_", (ftnlen)932)) * 80, (
		    ftnlen)80);
	}
	s_stop("", (ftnlen)0);
    }

/*     Do first scan of the command line for files. Load those of them */
/*     that appear to be text kernels. Make a note of SPKs, binary PCKs */
/*     and list file. */

    start = 1;
    fndnwd_(pass1, &start, &beg, &end, (ftnlen)25000);
    while(beg != 0) {

/*        Check if this word starts with "-". If it doesn't, consider it */
/*        to be a file name. */

	if (*(unsigned char *)&pass1[beg - 1] != '-') {

/*           Check if this file exists. */

	    if (exists_(pass1 + (beg - 1), end - (beg - 1))) {

/*              Get this file's architecture and type. */

		getfat_(pass1 + (beg - 1), arch, kertyp, end - (beg - 1), (
			ftnlen)80, (ftnlen)80);

/*              If it's not a DAF, a DAS, a transfer format, or an */
/*              obsolete type 1 text EK, load it into the POOL as */
/*              ZZLDKER would. */

		if (s_cmp(arch, "XFR", (ftnlen)80, (ftnlen)3) != 0 && s_cmp(
			arch, "DEC", (ftnlen)80, (ftnlen)3) != 0 && s_cmp(
			arch, "TE1", (ftnlen)80, (ftnlen)3) != 0 && s_cmp(
			arch, "DAF", (ftnlen)80, (ftnlen)3) != 0 && s_cmp(
			arch, "DAS", (ftnlen)80, (ftnlen)3) != 0) {
		    furnsh_(pass1 + (beg - 1), end - (beg - 1));
		}

/*              If it's a PCK or an SPK set corresponding flags. */

		if (s_cmp(arch, "DAF", (ftnlen)80, (ftnlen)3) == 0 && s_cmp(
			kertyp, "SPK", (ftnlen)80, (ftnlen)3) == 0) {
		    gotspk = TRUE_;
		}
		if (s_cmp(arch, "DAF", (ftnlen)80, (ftnlen)3) == 0 && s_cmp(
			kertyp, "PCK", (ftnlen)80, (ftnlen)3) == 0) {
		    gotpck = TRUE_;
		}
	    }
	} else if (s_cmp(pass1 + (beg - 1), fsfile, end - (beg - 1), (ftnlen)
		8) == 0) {

/*           If we have a list file, save it's name as we will need to */
/*           scan it for text kernels and PCKs/SPKs as well. */

	    start = end + 1;
	    fndnwd_(pass1, &start, &beg, &end, (ftnlen)25000);
	    if (beg > 0) {
		sfile = TRUE_;
		s_copy(srcfil, pass1 + (beg - 1), (ftnlen)255, end - (beg - 1)
			);
	    } else {
		end = start - 1;
	    }
	}

/*        Find next word. */

	start = end + 1;
	fndnwd_(pass1, &start, &beg, &end, (ftnlen)25000);
    }

/*     If a list file was provided, scan it for text kernels, SPKs and */
/*     binary PCKs. */

    if (sfile) {

/*        Check if list files exists. */

	if (! exists_(srcfil, (ftnlen)255)) {
	    setmsg_("List file '#' provided with -f option  does not exist.", 
		    (ftnlen)54);
	    errch_("#", srcfil, (ftnlen)1, (ftnlen)255);
	    sigerr_("SPICE(BADLISTFILENAME)", (ftnlen)22);
	}

/*        Check if an SPK OR another kernel was provided in with -f. */
/*        Complain and stop if so. */

	getfat_(srcfil, arch, kertyp, (ftnlen)255, (ftnlen)80, (ftnlen)80);
	if (s_cmp(arch, "?", (ftnlen)80, (ftnlen)1) != 0) {
	    setmsg_("The file '#' provided with -f option is a kernel file o"
		    "f architecture '#' and type '#'. Only plain text files c"
		    "ontaining lists of kernels should be provided with this "
		    "option.", (ftnlen)174);
	    errch_("#", srcfil, (ftnlen)1, (ftnlen)255);
	    errch_("#", arch, (ftnlen)1, (ftnlen)80);
	    errch_("#", kertyp, (ftnlen)1, (ftnlen)80);
	    sigerr_("SPICE(NOTATEXTFILE)", (ftnlen)19);
	}

/*        Read non-blank lines from the list file until EOF. */

	rdnbl_(srcfil, pass2, &eof, (ftnlen)255, (ftnlen)25000);
	while(! eof) {

/*           Remove TABs with spaces on the current line. */

	    replch_(pass2, tabchr, " ", pass2, (ftnlen)25000, (ftnlen)1, (
		    ftnlen)1, (ftnlen)25000);

/*           Scan line for words. */

	    start = 1;
	    fndnwd_(pass2, &start, &beg, &end, (ftnlen)25000);
	    while(beg != 0) {

/*              Assume this word is a file name. Check if this file */
/*              exists. */

		if (exists_(pass2 + (beg - 1), end - (beg - 1))) {

/*                 Get this file's architecture and type. */

		    getfat_(pass2 + (beg - 1), arch, kertyp, end - (beg - 1), 
			    (ftnlen)80, (ftnlen)80);

/*                 If it's not a DAF, a DAS, a transfer format, or an */
/*                 obsolete type 1 text EK, load it into the POOL as */
/*                 ZZLDKER would. */

		    if (s_cmp(arch, "XFR", (ftnlen)80, (ftnlen)3) != 0 && 
			    s_cmp(arch, "DEC", (ftnlen)80, (ftnlen)3) != 0 && 
			    s_cmp(arch, "TE1", (ftnlen)80, (ftnlen)3) != 0 && 
			    s_cmp(arch, "DAF", (ftnlen)80, (ftnlen)3) != 0 && 
			    s_cmp(arch, "DAS", (ftnlen)80, (ftnlen)3) != 0) {
			furnsh_(pass2 + (beg - 1), end - (beg - 1));
		    }

/*                 If it's a PCK or an SPK set corresponding flags. */

		    if (s_cmp(arch, "DAF", (ftnlen)80, (ftnlen)3) == 0 && 
			    s_cmp(kertyp, "SPK", (ftnlen)80, (ftnlen)3) == 0) 
			    {
			gotspk = TRUE_;
		    }
		    if (s_cmp(arch, "DAF", (ftnlen)80, (ftnlen)3) == 0 && 
			    s_cmp(kertyp, "PCK", (ftnlen)80, (ftnlen)3) == 0) 
			    {
			gotpck = TRUE_;
		    }
		}

/*              Get next word from the line. */

		start = end + 1;
		fndnwd_(pass2, &start, &beg, &end, (ftnlen)25000);
	    }

/*           Get next non-blank line. */

	    rdnbl_(srcfil, pass2, &eof, (ftnlen)255, (ftnlen)25000);
	}

/*        Close list file. */

	cltext_(srcfil, (ftnlen)255);
    }

/*     Get the count of that SPKs were loaded via meta-kernels that were */
/*     provided on the command line and/or in the list file. */

    ktotal_("SPK", &scount, (ftnlen)3);
    sindex = 0;

/*     Reset SPK flag is this count is not 0. */

    if (scount != 0) {
	gotspk = TRUE_;
    }

/*     Check if both SPKs and PCKs were given to the program. If so, */
/*     signal an error. */

    if (gotspk && gotpck) {
	setmsg_("The program cannot summarize binary SPK and PCK files in th"
		"e same run.", (ftnlen)70);
	sigerr_("SPICE(NOCANDOSPKSPCKS)", (ftnlen)22);
    }

/*     Scan command line again to process options and accumulate all */
/*     file names in separate string. */

    s_copy(pass2, " ", (ftnlen)25000, (ftnlen)1);
    start = 1;
    fndnwd_(pass1, &start, &beg, &end, (ftnlen)25000);
    while(beg > 0) {

/*        All options begin with a '-'. If this item doesn't it is */
/*        presumed to be a file. */

	if (*(unsigned char *)&pass1[beg - 1] != '-') {
	    suffix_(pass1 + (beg - 1), &c__1, pass2, end - (beg - 1), (ftnlen)
		    25000);
	    havfil = TRUE_;
	} else if (s_cmp(pass1 + (beg - 1), fnonly, end - (beg - 1), (ftnlen)
		8) == 0) {

/*           Display numbers only. */

	    nonly = TRUE_;
	    obnam = FALSE_;
	} else if (s_cmp(pass1 + (beg - 1), fsfile, end - (beg - 1), (ftnlen)
		8) == 0) {

/*           Get file names from a list file. */

	    start = end + 1;
	    fndnwd_(pass1, &start, &beg, &end, (ftnlen)25000);
	    if (beg > 0) {
		sfile = TRUE_;
		s_copy(srcfil, pass1 + (beg - 1), (ftnlen)255, end - (beg - 1)
			);
	    } else {
		setmsg_("No file name was following the -f option.", (ftnlen)
			41);
		sigerr_("SPICE(NOLISTFILENAME)", (ftnlen)21);
	    }
	} else if (s_cmp(pass1 + (beg - 1), ftutc, end - (beg - 1), (ftnlen)8)
		 == 0) {

/*           Display time as calendar UTC. */

	    s_copy(timtyp, "UTCCAL", (ftnlen)32, (ftnlen)6);
	} else if (s_cmp(pass1 + (beg - 1), ftdoy, end - (beg - 1), (ftnlen)8)
		 == 0) {

/*           Display times as DOY UTC. */

	    s_copy(timtyp, "UTCDOY", (ftnlen)32, (ftnlen)6);
	} else if (s_cmp(pass1 + (beg - 1), ftsec, end - (beg - 1), (ftnlen)8)
		 == 0) {

/*           Display times as ET seconds. */

	    s_copy(timtyp, "ETSEC", (ftnlen)32, (ftnlen)5);
	} else if (s_cmp(pass1 + (beg - 1), fobnam, end - (beg - 1), (ftnlen)
		8) == 0) {

/*           Sort by the name of the object. */

	    obnam = ! nonly;
	} else if (s_cmp(pass1 + (beg - 1), ftdsp, end - (beg - 1), (ftnlen)8)
		 == 0) {

/*           Present output with a tabular display. */

	    tdsp = TRUE_;
	} else if (s_cmp(pass1 + (beg - 1), fgroup, end - (beg - 1), (ftnlen)
		8) == 0) {

/*           Group similar coverages in a tabular display. */

	    gdsp = TRUE_;
	} else if (s_cmp(pass1 + (beg - 1), fsort, end - (beg - 1), (ftnlen)8)
		 == 0) {

/*           Sort coverages by start time in a tabular display. */

	    sdsp = TRUE_;
	} else if (s_cmp(pass1 + (beg - 1), fcntr, end - (beg - 1), (ftnlen)8)
		 == 0) {

/*           Combine bodies and centers to make a single object. */

	    cntr = TRUE_;
	    objsiz = 3;
	} else if (s_cmp(pass1 + (beg - 1), fhelp, end - (beg - 1), (ftnlen)8)
		 == 0) {

/*           Print the help. */

	    help = TRUE_;
	} else if (s_cmp(pass1 + (beg - 1), fvrsn, end - (beg - 1), (ftnlen)8)
		 == 0) {

/*           Show the version. */

	    vrsn = TRUE_;
	} else if (s_cmp(pass1 + (beg - 1), fall, end - (beg - 1), (ftnlen)8) 
		== 0) {

/*           Combine files. */

	    all = TRUE_;
	} else if (s_cmp(pass1 + (beg - 1), ffrom, end - (beg - 1), (ftnlen)8)
		 == 0) {

/*           Filter by coverage over interval FROM:TO. */

	    start = end + 1;
	    fndnwd_(pass1, &start, &beg, &end, (ftnlen)25000);
	    if (beg > 0) {
		tparse_(pass1 + (beg - 1), &from, error, end - (beg - 1), (
			ftnlen)80);
		if (s_cmp(error, " ", (ftnlen)80, (ftnlen)1) == 0) {
		    havfrm = TRUE_;
		} else {
		    setmsg_("'From' time '#' provided with the -from option "
			    "is not a valid time string.", (ftnlen)74);
		    errch_("#", pass1 + (beg - 1), (ftnlen)1, end - (beg - 1))
			    ;
		    sigerr_("SPICE(BADFROMTIME)", (ftnlen)18);
		}
	    } else {
		setmsg_("No 'from' time was following the -from option.", (
			ftnlen)46);
		sigerr_("SPICE(NOFROMTIME)", (ftnlen)17);
	    }
	} else if (s_cmp(pass1 + (beg - 1), fto, end - (beg - 1), (ftnlen)8) 
		== 0) {

/*           Filter by coverage over interval FROM:TO. */

	    start = end + 1;
	    fndnwd_(pass1, &start, &beg, &end, (ftnlen)25000);
	    if (beg > 0) {
		tparse_(pass1 + (beg - 1), &to, error, end - (beg - 1), (
			ftnlen)80);
		if (s_cmp(error, " ", (ftnlen)80, (ftnlen)1) == 0) {
		    havto = TRUE_;
		} else {
		    setmsg_("'To' time '#' provided with the -to option is n"
			    "ot a valid time string.", (ftnlen)70);
		    errch_("#", pass1 + (beg - 1), (ftnlen)1, end - (beg - 1))
			    ;
		    sigerr_("SPICE(BADTOTIME)", (ftnlen)16);
		}
	    } else {
		setmsg_("No 'to' time was following the -to option.", (ftnlen)
			42);
		sigerr_("SPICE(NOTOTIME)", (ftnlen)15);
	    }
	} else if (s_cmp(pass1 + (beg - 1), fat, end - (beg - 1), (ftnlen)8) 
		== 0) {

/*           Filter by coverage over a specific epoch. */

	    start = end + 1;
	    fndnwd_(pass1, &start, &beg, &end, (ftnlen)25000);
	    if (beg > 0) {
		tparse_(pass1 + (beg - 1), &when, error, end - (beg - 1), (
			ftnlen)80);
		if (s_cmp(error, " ", (ftnlen)80, (ftnlen)1) == 0) {
		    at = TRUE_;
		} else {
		    setmsg_("'At' time '#' provided with the -at option is n"
			    "ot a valid time string.", (ftnlen)70);
		    errch_("#", pass1 + (beg - 1), (ftnlen)1, end - (beg - 1))
			    ;
		    sigerr_("SPICE(BADATTIME)", (ftnlen)16);
		}
	    } else {
		setmsg_("No 'at' time was following the -at option.", (ftnlen)
			42);
		sigerr_("SPICE(NOATTIME)", (ftnlen)15);
	    }
	} else if (s_cmp(pass1 + (beg - 1), fsec, end - (beg - 1), (ftnlen)8) 
		== 0) {

/*           Round inward to the nearest second. */

	    round = TRUE_;
	    intval = min(intval,1.);
	} else if (s_cmp(pass1 + (beg - 1), fmin, end - (beg - 1), (ftnlen)8) 
		== 0) {

/*           Round inward to the nearest minute. */

	    round = TRUE_;
	    intval = min(intval,60.);
	} else if (s_cmp(pass1 + (beg - 1), fhour, end - (beg - 1), (ftnlen)8)
		 == 0) {

/*           Round inward to the nearest hour. */

	    round = TRUE_;
	    intval = min(intval,3600.);
	} else if (s_cmp(pass1 + (beg - 1), fday, end - (beg - 1), (ftnlen)8) 
		== 0) {

/*           Round inward to the nearest day. */

	    round = TRUE_;
	    intval = min(intval,86400.);
	} else if (beg == end) {

/*           '-' by itself is no an option. */

	    writit_("'-' by itself is not a recognized option.", (ftnlen)41);
	    beg = i_len(pass1, (ftnlen)25000);
	} else {

/*           This is a restriction flag. Find out the object that we */
/*           want to restrict relative to. */

	    savbeg = beg;
	    savend = end;
	    if (s_cmp(pass1 + (beg - 1), fsbcen, (ftnlen)3, (ftnlen)8) == 0 &&
		     beg + 2 < end) {
		ref = 2;
		beg += 3;
	    } else if (s_cmp(pass1 + (beg - 1), fsbbod, (ftnlen)3, (ftnlen)8) 
		    == 0 && beg + 2 < end) {
		ref = 1;
		beg += 3;
	    } else {
		ref = 1;
		++beg;
	    }

/*           Items that begin with '-' that are not one of */
/*           the previous items are regarded as a request */
/*           to summarize only for particular items.  However, */
/*           we need to recognize this item.  If we don't recognize */
/*           it we say so. */

	    if (beint_(pass1 + (beg - 1), end - (beg - 1))) {
		nparsi_(pass1 + (beg - 1), &value, error, &ptr, end - (beg - 
			1), (ftnlen)80);
		ulist = TRUE_;
		body[0] = value;
		body[1] = value;
		body[2] = beg;
		body[3] = end;
		body[4] = ref;
		body[5] = 0;
		objadd_(body, bodlst, bod);
	    } else if (*(unsigned char *)&pass1[beg - 1] == '[' && *(unsigned 
		    char *)&pass1[end - 1] == ']' && pos_(pass1, ":", &beg, 
		    end, (ftnlen)1) > 0 && end - beg >= 3) {

/*              Here's the deal, the integer range should be specified */
/*              as [low:hi].  We allow either end to be left out */
/*              to indicate no restriction on that end of the interval. */

		low = intmin_();
		hi = intmax_();
		b = beg + 1;
		ok = FALSE_;
		lx4sgn_(pass1, &b, &e, &size, (ftnlen)25000);
		if (size > 0) {
		    nparsi_(pass1 + (b - 1), &low, error, &ptr, e - (b - 1), (
			    ftnlen)80);
		    b = e + 2;
		    if (b == end) {
			ok = TRUE_;
		    } else {
			e = end - 1;
			if (beint_(pass1 + (b - 1), e - (b - 1))) {
			    nparsi_(pass1 + (b - 1), &hi, error, &ptr, e - (b 
				    - 1), (ftnlen)80);
			    ok = TRUE_;
			}
		    }

/*              There was not an initial integer for a lower */
/*              bound of the numeric range.  See if there's an upper */
/*              bound.. */

		} else {
		    b = beg + 2;
		    e = end - 1;
		    if (beint_(pass1 + (b - 1), e - (b - 1))) {
			nparsi_(pass1 + (b - 1), &hi, error, &ptr, e - (b - 1)
				, (ftnlen)80);
			ok = TRUE_;
		    }
		}
		if (! ok) {
/* Writing concatenation */
		    i__2[0] = 1, a__1[0] = "'";
		    i__2[1] = savend - (savbeg - 1), a__1[1] = pass1 + (
			    savbeg - 1);
		    i__2[2] = 2, a__1[2] = "' ";
		    i__2[3] = 18, a__1[3] = "is not recognized.";
		    s_cat(ch__1, a__1, i__2, &c__4, (ftnlen)25021);
		    writit_(ch__1, savend - (savbeg - 1) + 21);
		} else {
		    ulist = TRUE_;
		    body[0] = min(low,hi);
		    body[1] = max(low,hi);
		    body[2] = beg;
		    body[3] = end;
		    body[5] = 0;
		    body[4] = ref;
		    objadd_(body, bodlst, bod);
		}
	    } else {

/*              Restriction is given as a name. Depending on whether */
/*              this run summarizes SPKs and PCKs, map it to ID using */
/*              BODN2C or FRINFO. (Note that GOTSPK and GOTPCK cannot be */
/*              both .TRUE. at this point but they can both be .FALSE.; */
/*              if they are both .FALSE. the program will eventually */
/*              fall through to signaling exception that no files were */
/*              provided.) */

		if (gotspk) {

/*                 See if it is a name that we can recognize 'as is'. */

		    bodn2c_(pass1 + (beg - 1), &value, &fnd, end - (beg - 1));
		    if (! fnd) {

/*                    If not, replace underscores with blanks and try */
/*                    again. */

			replch_(pass1 + (beg - 1), "_", " ", pass1 + (beg - 1)
				, end - (beg - 1), (ftnlen)1, (ftnlen)1, end 
				- (beg - 1));
			bodn2c_(pass1 + (beg - 1), &value, &fnd, end - (beg - 
				1));
		    }
		}
		if (gotpck) {

/*                 Map frame name to ID. If frame is unknown, FRMNAM will */
/*                 return 0. */

		    namfrm_(pass1 + (beg - 1), &value, end - (beg - 1));
		    if (value != 0) {
			fnd = TRUE_;
		    } else {
			fnd = FALSE_;
		    }

/*                 If this restriction is on the body, we need the frame */
/*                 class ID rather than the frame ID. */

		    if (ref == 1) {
			frinfo_(&value, &fcent, &class__, &clssid, &fnd);
			value = clssid;
		    }
		}

/*              Set restriction or report bad option only if some */
/*              files will be summarized. */

		if (gotspk || gotpck) {
		    if (fnd) {
			ulist = TRUE_;
			body[0] = value;
			body[1] = value;
			body[2] = beg;
			body[3] = end;
			body[5] = 0;
			body[4] = ref;
			objadd_(body, bodlst, bod);
		    } else {
/* Writing concatenation */
			i__2[0] = 1, a__1[0] = "'";
			i__2[1] = savend - (savbeg - 1), a__1[1] = pass1 + (
				savbeg - 1);
			i__2[2] = 2, a__1[2] = "' ";
			i__2[3] = 18, a__1[3] = "is not recognized.";
			s_cat(ch__1, a__1, i__2, &c__4, (ftnlen)25021);
			writit_(ch__1, savend - (savbeg - 1) + 21);
		    }
		}
	    }
	}

/*        Locate the next word. */

	start = end + 1;
	fndnwd_(pass1, &start, &beg, &end, (ftnlen)25000);
    }

/*     If only version display was requested, stop because it has */
/*     already been displayed. */

    if (vrsn) {
	s_stop("", (ftnlen)0);
    }

/*     If help display was requsted, display help text and stop. */

    if (help) {
	for (i__ = 1; i__ <= 50; ++i__) {
	    writit_(htext + ((i__1 = i__ - 1) < 50 && 0 <= i__1 ? i__1 : 
		    s_rnge("htext", i__1, "brief_", (ftnlen)1588)) * 80, (
		    ftnlen)80);
	}
	s_stop("", (ftnlen)0);
    }

/*     Reset rounding flag if time should be output in any format */
/*     other than calendar ET (default). */

    if (s_cmp(timtyp, "ETCAL", (ftnlen)32, (ftnlen)5) != 0) {
	round = FALSE_;
    }

/*     Determine what the user wants us to do for time filtering. */
/*     "-from" and "-to" together give us a range (whether or not they */
/*     are specified in the right order). "-from" or "-to" by itself is */
/*     equivalent to "-at". "-at" takes precedence over any combination */
/*     of "-from" and "-to". */

    if (havfrm && havto) {
	if (from > to) {
	    swapd_(&from, &to);
	}
	fromto = TRUE_;
    } else if (havfrm && ! havto) {
	to = from;
	fromto = TRUE_;
    } else if (! havfrm && havto) {
	from = to;
	fromto = TRUE_;
    }
    if (at) {
	from = when;
	to = when;
	fromto = TRUE_;
    }

/*     The following are used as indexes into the 4-dimensional */
/*     array used to hold the "formats" for the names of objects. */

    num = 1;
    o = 1;
    t = 1;
    c__ = 1;
    if (nonly) {
	num = 2;
    }
    if (obnam) {
	o = 2;
    }
    if (tdsp) {
	t = 2;
    }
    if (cntr) {
	c__ = 2;
    }
    s_copy(fmtpic, frmat + (((i__1 = num + (c__ + (o + (t << 1) << 1) << 1) - 
	    15) < 16 && 0 <= i__1 ? i__1 : s_rnge("frmat", i__1, "brief_", (
	    ftnlen)1661)) << 5), (ftnlen)32, (ftnlen)32);

/*     Initialize the cells. */

    ssizec_(&c_b212, winsym, (ftnlen)64);
    ssizei_(&c_b212, winptr);
    ssized_(&c_b214, winval);
    ssized_(&c_b212, filwin);
    ssized_(&c__2, segwin);
    ssized_(&c_b212, tmpwin);
    objinl_(&objsiz, &c_b212, objlis);

/*     Now run through the files specified on the command line and/or */
/*     the list file and collect summary information. */

    start = 1;
    fndnwd_(pass2, &start, &beg, &end, (ftnlen)25000);

/*     If we didn't have anything in the PASS2 string, we must have a */
/*     list of files in a file of SPK names.  Read the first line of */
/*     that file. */

    if (beg == 0 && sfile) {
	s_copy(pass2, " ", (ftnlen)25000, (ftnlen)1);
	rdnbl_(srcfil, pass2, &eof, (ftnlen)255, (ftnlen)25000);
	replch_(pass2, tabchr, " ", pass2, (ftnlen)25000, (ftnlen)1, (ftnlen)
		1, (ftnlen)25000);
	if (! eof) {
	    fndnwd_(pass2, &c__1, &beg, &end, (ftnlen)25000);
	}
    }

/*     Process files one by one until we run out of file names. */

    while(beg != 0) {

/*        Get this file's architecture and type. */

	getfat_(pass2 + (beg - 1), arch, kertyp, end - (beg - 1), (ftnlen)80, 
		(ftnlen)80);

/*        Is this a DAF file? */

	if (s_cmp(arch, "DAF", (ftnlen)80, (ftnlen)3) != 0) {

/*           Apparently not. Depending on the file architecture, either */
/*           signal an error or do nothing (i.e. proceed to the next */
/*           file). */

	    if (s_cmp(arch, "XFR", (ftnlen)80, (ftnlen)3) == 0 || s_cmp(arch, 
		    "DEC", (ftnlen)80, (ftnlen)3) == 0) {

/*              Signal an error, reporting that this is a transfer file. */

		setmsg_("Kernel '#' is in transfer format. It must be conver"
			"ted to binary format to be summarized using BRIEF.", (
			ftnlen)101);
		errch_("#", pass2 + (beg - 1), (ftnlen)1, end - (beg - 1));
		sigerr_("SPICE(TRANSFERFORMAT)", (ftnlen)21);
	    } else if (s_cmp(arch, "DAS", (ftnlen)80, (ftnlen)3) == 0 && 
		    s_cmp(kertyp, "EK", (ftnlen)80, (ftnlen)2) == 0) {

/*              Signal an error, reporting that this is an EK file. */

		setmsg_("Kernel '#' is a binary EK and, therefore, cannot be"
			" summarized using BRIEF. Use SPACIT or INSPEKT to su"
			"mmarize this file.", (ftnlen)121);
		errch_("#", pass2 + (beg - 1), (ftnlen)1, end - (beg - 1));
		errch_("#", arch, (ftnlen)1, (ftnlen)80);
		errch_("#", kertyp, (ftnlen)1, (ftnlen)80);
		sigerr_("SPICE(EKFILE)", (ftnlen)13);
	    } else if (s_cmp(arch, "DAS", (ftnlen)80, (ftnlen)3) == 0 || 
		    s_cmp(arch, "TE1", (ftnlen)80, (ftnlen)3) == 0) {

/*              Signal an error, reporting that this is a file with */
/*              a wrong architecture. */

		setmsg_("Kernel '#' is not a binary SPK or PCK file and, the"
			"refore, cannot be summarized using BRIEF. It is a '#"
			"/#' file.", (ftnlen)112);
		errch_("#", pass2 + (beg - 1), (ftnlen)1, end - (beg - 1));
		errch_("#", arch, (ftnlen)1, (ftnlen)80);
		errch_("#", kertyp, (ftnlen)1, (ftnlen)80);
		sigerr_("SPICE(BADARCHITECTURE)", (ftnlen)22);
	    } else {

/*              Do nothing for all other architectures -- KPL, ASC, TXT, */
/*              and ?,  -- assuming that this file either has already */
/*              been loaded (if its name came from the command line) or */
/*              can be ignored (if its name came from the list file). */

	    }
	} else {

/*           This file is a DAF file. Report an error for all DAFs that */
/*           are not SPKs or PCKs. */

	    if (s_cmp(kertyp, "CK", (ftnlen)80, (ftnlen)2) == 0) {
		setmsg_("Kernel '#' is a CK and, therefore, cannot be summar"
			"ized using BRIEF. Use CKBRIEF to summarize this file."
			, (ftnlen)104);
		errch_("#", pass2 + (beg - 1), (ftnlen)1, end - (beg - 1));
		sigerr_("SPICE(CKFILE)", (ftnlen)13);
	    } else if (s_cmp(kertyp, "SPK", (ftnlen)80, (ftnlen)3) != 0 && 
		    s_cmp(kertyp, "PCK", (ftnlen)80, (ftnlen)3) != 0) {
		setmsg_("Kernel '#' is a DAF-based binary '#' file; kernel m"
			"ust be SPK or PCK in order for BRIEF to summarize it."
			, (ftnlen)104);
		errch_("#", pass2 + (beg - 1), (ftnlen)1, end - (beg - 1));
		errch_("#", kertyp, (ftnlen)1, (ftnlen)80);
		sigerr_("SPICE(BADKERNELTYPE)", (ftnlen)20);
	    }
	}

/*        Collect summary info and display it only for binary SPKs and */
/*        PCKs. */

	if (s_cmp(arch, "DAF", (ftnlen)80, (ftnlen)3) == 0 && (s_cmp(kertyp, 
		"SPK", (ftnlen)80, (ftnlen)3) == 0 || s_cmp(kertyp, "PCK", (
		ftnlen)80, (ftnlen)3) == 0)) {

/*           Open the next file. Start forward search and find next */
/*           segment. */

	    dafopr_(pass2 + (beg - 1), &handle, end - (beg - 1));
	    dafbfs_(&handle);
	    daffna_(&fnd);
	    while(fnd) {
		scardd_(&c__0, segwin);

/*              Unpack segment descriptor and set body, center, and */
/*              frame IDs. For PCKs frame ID is not applicable, so force */
/*              it to 1 (J2000) to avoid '*' in summary display. */

		dafgs_(segsum);
		if (s_cmp(kertyp, "SPK", (ftnlen)80, (ftnlen)3) == 0) {
		    dafus_(segsum, &c__2, &c__6, dc, ic);
		    id = ic[0];
		    cent = ic[1];
		    frame = ic[2];
		} else if (s_cmp(kertyp, "PCK", (ftnlen)80, (ftnlen)3) == 0) {
		    dafus_(segsum, &c__2, &c__5, dc, ic);
		    id = ic[0];
		    cent = ic[1];
		    frame = 1;
		}
		object[0] = id;
		object[1] = cent;

/*              If we are summarizing from a list of input bodies, */
/*              find out if this body is in the list to summarize. */

		if (ulist) {

/*                 Use the "object search by function" to see if */
/*                 we've got the current ID in the list of bodies */
/*                 to restrict the summary to. */

		    keep = idset_(&id, &cent);
		    objsbf_((L_fp)idmch_, &c__1, bodlst, bod, &keep);

/*                 We will need the object BOD in a bit to update */
/*                 the number of times we've encountered this */
/*                 body-center pair. */

		} else {
		    keep = TRUE_;
		}

/*              If this segment is a keeper, construct the window */
/*              for the segment. */

		if (keep) {
		    wninsd_(dc, &dc[1], segwin);

/*                 Determine the class of the reference frame */
/*                 attached to this body.  We report only 2, */
/*                 inertial (1) and non-inertial (2).  If at any */
/*                 time the reference frame for this object is */
/*                 non-inertial we never go back to inertial. */

		    class__ = 0;
		    frinfo_(&frame, &fcent, &class__, &clssid, &found);
		    if (class__ != 1) {
			class__ = 2;
		    }
		    object[(i__1 = objsiz - 1) < 3 && 0 <= i__1 ? i__1 : 
			    s_rnge("object", i__1, "brief_", (ftnlen)1888)] = 
			    class__;

/*                 See if we already have this "object".  If not */
/*                 add it to the list of objects and increment the */
/*                 count of objects. */

		    objfnd_(object, &c__1, objlis, obj, &found);
		    if (! found) {
			objadd_(object, objlis, obj);
		    } else {

/*                    Update the reference frame information. */

			objcf2_((I_fp)bigger_, &class__, obj, &objsiz, objlis)
				;
		    }

/*                 Construct the "name" we will use to represent */
/*                 this "object" in the symbol table of windows. */

		    maknam_(object, &objsiz, &obnam, kertyp, objnam, (ftnlen)
			    80, (ftnlen)64);

/*                 Look up the window of coverage already stored for */
/*                 this object. */

		    sygetd_(objnam, winsym, winptr, winval, &n, &filwin[6], &
			    fnd, (ftnlen)64, (ftnlen)64);
		    if (! fnd) {
			n = 0;
		    }
		    scardd_(&n, filwin);

/*                 Union the current segment interval with the current */
/*                 window for this object, put the result back into */
/*                 the table of windows. */

		    wnunid_(segwin, filwin, tmpwin);
		    n = cardd_(tmpwin);
		    syputd_(objnam, &tmpwin[6], &n, winsym, winptr, winval, (
			    ftnlen)64, (ftnlen)64);

/*                 Update the BODLST if we are using a list to */
/*                 filter the summary. */

		    if (ulist) {
			if (fromto) {
			    if (wnincd_(&from, &to, tmpwin)) {

/*                          This block used to call OBJCF1 on the BOD */
/*                          object returned by an earlier call to */
/*                          OBJSBF. This resulted in only that one */
/*                          object being "tagged" as matching the */
/*                          current body/center, producing unnecessary */
/*                          "There is no data for:" reports for other */
/*                          matching restrictions. The loop below checks */
/*                          every object for match and "tags" all those */
/*                          that do match. */

				nbod = objact_(bodlst);
				size = bodlst[3] - 1;
				i__1 = nbod;
				for (i__ = 1; i__ <= i__1; ++i__) {
				    objnth_(bodlst, &i__, bod, &found);
				    if (found && idmch_(&bodlst[(i__3 = bod[0]
					     + 6) < 706 && 0 <= i__3 ? i__3 : 
					    s_rnge("bodlst", i__3, "brief_", (
					    ftnlen)1962)], &size)) {
					objcf1_((I_fp)inc_, bod, &c__6, 
						bodlst);
				    }
				}
			    }
			} else {

/*                       This block used to call OBJCF1 on the BOD */
/*                       object returned by an earlier call to OBJSBF. */
/*                       This resulted in only that one object being */
/*                       "tagged" as matching the current body/center, */
/*                       producing unnecessary "There is no data for:" */
/*                       reports for other matching restrictions. The */
/*                       loop below checks every object for match and */
/*                       "tags" all those that do match. */

			    nbod = objact_(bodlst);
			    size = bodlst[3] - 1;
			    i__1 = nbod;
			    for (i__ = 1; i__ <= i__1; ++i__) {
				objnth_(bodlst, &i__, bod, &found);
				if (found && idmch_(&bodlst[(i__3 = bod[0] + 
					6) < 706 && 0 <= i__3 ? i__3 : s_rnge(
					"bodlst", i__3, "brief_", (ftnlen)
					1986)], &size)) {
				    objcf1_((I_fp)inc_, bod, &c__6, bodlst);
				}
			    }
			}
		    }
		}

/*              Find the next segment in the kernel. */

		daffna_(&fnd);
	    }
	    dafcls_(&handle);
	    ++ndone;
	    if (! all) {

/*              If the use specified time restrictions, we filter */
/*              the collected objects so that we limit them */
/*              to those that have coverage during the times specified. */

		if (at || fromto) {
		    filtem_(kertyp, &obnam, objlis, &from, &to, filwin, 
			    tmpwin, winsym, winptr, winval, (ftnlen)80, (
			    ftnlen)64);
		}

/*              If the intervals are supposed to be rounded inward, */
/*              now is the time to do it. */

		if (round) {
		    rndem_(kertyp, &obnam, objlis, &objsiz, &intval, filwin, 
			    winsym, winptr, winval, (ftnlen)80, (ftnlen)64);
		}

/*              That's the end of the filtering.  Now display */
/*              what we've got. */

		writit_(" ", (ftnlen)1);
/* Writing concatenation */
		i__4[0] = 13, a__2[0] = "Summary for: ";
		i__4[1] = end - (beg - 1), a__2[1] = pass2 + (beg - 1);
		s_cat(ch__2, a__2, i__4, &c__2, (ftnlen)25013);
		writit_(ch__2, end - (beg - 1) + 13);
		writit_(" ", (ftnlen)1);
		if (ulist) {

/*                 If any objects are not represented in this */
/*                 file say so. */

		    nbod = objact_(bodlst);
		    first = TRUE_;
		    i__1 = nbod;
		    for (i__ = 1; i__ <= i__1; ++i__) {
			objnth_(bodlst, &i__, bod, &fnd);
			objget_(bod, bodlst, body);
			if (body[5] == 0) {
			    b = body[2];
			    e = body[3];
			    frst = body[0];
			    last = body[1];
			    item = body[4];

/*                       See if we had a range. */

			    if (frst == last) {
				if (gotspk) {
				    if (item == 1) {
/* Writing concatenation */
					i__5[0] = 12, a__3[0] = "  for body '"
						;
					i__5[1] = e - (b - 1), a__3[1] = 
						pass1 + (b - 1);
					i__5[2] = 1, a__3[2] = "'";
					s_cat(string, a__3, i__5, &c__3, (
						ftnlen)255);
				    } else {
/* Writing concatenation */
					i__5[0] = 17, a__3[0] = "  w.r.t. ce"
						"nter '";
					i__5[1] = e - (b - 1), a__3[1] = 
						pass1 + (b - 1);
					i__5[2] = 1, a__3[2] = "'";
					s_cat(string, a__3, i__5, &c__3, (
						ftnlen)255);
				    }
				} else if (gotpck) {
				    if (item == 1) {
/* Writing concatenation */
					i__5[0] = 13, a__3[0] = "  for frame"
						" '";
					i__5[1] = e - (b - 1), a__3[1] = 
						pass1 + (b - 1);
					i__5[2] = 1, a__3[2] = "'";
					s_cat(string, a__3, i__5, &c__3, (
						ftnlen)255);
				    } else {
/* Writing concatenation */
					i__5[0] = 16, a__3[0] = "  w.r.t. fr"
						"ame '";
					i__5[1] = e - (b - 1), a__3[1] = 
						pass1 + (b - 1);
					i__5[2] = 1, a__3[2] = "'";
					s_cat(string, a__3, i__5, &c__3, (
						ftnlen)255);
				    }
				} else {
				    if (item == 1) {
/* Writing concatenation */
					i__5[0] = 10, a__3[0] = "  for ID '";
					i__5[1] = e - (b - 1), a__3[1] = 
						pass1 + (b - 1);
					i__5[2] = 1, a__3[2] = "'";
					s_cat(string, a__3, i__5, &c__3, (
						ftnlen)255);
				    } else {
/* Writing concatenation */
					i__5[0] = 13, a__3[0] = "  w.r.t. ID"
						" '";
					i__5[1] = e - (b - 1), a__3[1] = 
						pass1 + (b - 1);
					i__5[2] = 1, a__3[2] = "'";
					s_cat(string, a__3, i__5, &c__3, (
						ftnlen)255);
				    }
				}
			    } else {
				if (gotspk) {
				    if (item == 1) {
/* Writing concatenation */
					i__5[0] = 17, a__3[0] = "  for bodie"
						"s in '";
					i__5[1] = e - (b - 1), a__3[1] = 
						pass1 + (b - 1);
					i__5[2] = 10, a__3[2] = "' ID range";
					s_cat(string, a__3, i__5, &c__3, (
						ftnlen)255);
				    } else {
/* Writing concatenation */
					i__5[0] = 21, a__3[0] = "  w.r.t. ce"
						"nters in '";
					i__5[1] = e - (b - 1), a__3[1] = 
						pass1 + (b - 1);
					i__5[2] = 10, a__3[2] = "' ID range";
					s_cat(string, a__3, i__5, &c__3, (
						ftnlen)255);
				    }
				} else if (gotpck) {
				    if (item == 1) {
/* Writing concatenation */
					i__5[0] = 17, a__3[0] = "  for frame"
						"s in '";
					i__5[1] = e - (b - 1), a__3[1] = 
						pass1 + (b - 1);
					i__5[2] = 10, a__3[2] = "' ID range";
					s_cat(string, a__3, i__5, &c__3, (
						ftnlen)255);
				    } else {
/* Writing concatenation */
					i__5[0] = 20, a__3[0] = "  w.r.t. fr"
						"ames in '";
					i__5[1] = e - (b - 1), a__3[1] = 
						pass1 + (b - 1);
					i__5[2] = 10, a__3[2] = "' ID range";
					s_cat(string, a__3, i__5, &c__3, (
						ftnlen)255);
				    }
				} else {
				    if (item == 1) {
/* Writing concatenation */
					i__5[0] = 14, a__3[0] = "  for IDs i"
						"n '";
					i__5[1] = e - (b - 1), a__3[1] = 
						pass1 + (b - 1);
					i__5[2] = 10, a__3[2] = "' ID range";
					s_cat(string, a__3, i__5, &c__3, (
						ftnlen)255);
				    } else {
/* Writing concatenation */
					i__5[0] = 17, a__3[0] = "  w.r.t. ID"
						"s in '";
					i__5[1] = e - (b - 1), a__3[1] = 
						pass1 + (b - 1);
					i__5[2] = 10, a__3[2] = "' ID range";
					s_cat(string, a__3, i__5, &c__3, (
						ftnlen)255);
				    }
				}
			    }
			    if (at) {
				etcal_(&from, etstr1, (ftnlen)64);
/* Writing concatenation */
				i__5[0] = 14, a__3[0] = " covering ET '";
				i__5[1] = rtrim_(etstr1, (ftnlen)64), a__3[1] 
					= etstr1;
				i__5[2] = 1, a__3[2] = "'";
				s_cat(ch__3, a__3, i__5, &c__3, (ftnlen)79);
				suffix_(ch__3, &c__0, string, rtrim_(etstr1, (
					ftnlen)64) + 15, (ftnlen)255);
			    } else if (fromto) {
				etcal_(&from, etstr1, (ftnlen)64);
				etcal_(&to, etstr2, (ftnlen)64);
/* Writing concatenation */
				i__6[0] = 19, a__4[0] = " covering from ET '";
				i__6[1] = rtrim_(etstr1, (ftnlen)64), a__4[1] 
					= etstr1;
				i__6[2] = 9, a__4[2] = "' to ET '";
				i__6[3] = rtrim_(etstr2, (ftnlen)64), a__4[3] 
					= etstr2;
				i__6[4] = 1, a__4[4] = "'";
				s_cat(ch__4, a__4, i__6, &c__5, (ftnlen)157);
				suffix_(ch__4, &c__0, string, rtrim_(etstr1, (
					ftnlen)64) + 28 + rtrim_(etstr2, (
					ftnlen)64) + 1, (ftnlen)255);
			    }
			    if (first) {
				first = FALSE_;
				writit_("There is no data: ", (ftnlen)18);
			    }
			    writit_(string, (ftnlen)255);
			}
		    }
		    if (! first) {
			writit_(" ", (ftnlen)1);
		    }
		}

/*              Compress object list and get the number of objects that */
/*              we still have in it. */

		objcmp_(objlis);
		nobj = objact_(objlis);

/*              If the number of objects is zero, check if filtering by */
/*              time and body have occurred and finish "No data ..." */
/*              reporting. */

		if (nobj == 0 && ! ulist) {
		    writit_("There is no data: ", (ftnlen)18);
		    if (at) {
			etcal_(&from, etstr1, (ftnlen)64);
/* Writing concatenation */
			i__5[0] = 15, a__3[0] = "  covering ET '";
			i__5[1] = rtrim_(etstr1, (ftnlen)64), a__3[1] = 
				etstr1;
			i__5[2] = 1, a__3[2] = "'";
			s_cat(ch__5, a__3, i__5, &c__3, (ftnlen)80);
			writit_(ch__5, rtrim_(etstr1, (ftnlen)64) + 16);
			writit_(" ", (ftnlen)1);
		    } else if (fromto) {
			etcal_(&from, etstr1, (ftnlen)64);
			etcal_(&to, etstr2, (ftnlen)64);
/* Writing concatenation */
			i__6[0] = 20, a__4[0] = "  covering from ET '";
			i__6[1] = rtrim_(etstr1, (ftnlen)64), a__4[1] = 
				etstr1;
			i__6[2] = 9, a__4[2] = "' to ET '";
			i__6[3] = rtrim_(etstr2, (ftnlen)64), a__4[3] = 
				etstr2;
			i__6[4] = 1, a__4[4] = "'";
			s_cat(ch__6, a__4, i__6, &c__5, (ftnlen)158);
			writit_(ch__6, rtrim_(etstr1, (ftnlen)64) + 29 + 
				rtrim_(etstr2, (ftnlen)64) + 1);
			writit_(" ", (ftnlen)1);
		    }
		}

/*              We are going to sort the object list.  We just rip off */
/*              the Shell sort algorithm to accomplish this. */

		gap = nobj / 2;
		while(gap > 0) {
		    i__1 = nobj;
		    for (i__ = gap + 1; i__ <= i__1; ++i__) {
			j = i__ - gap;
			while(j > 0) {
			    jg = j + gap;
			    objnth_(objlis, &j, obj, &found);
			    objnth_(objlis, &jg, objg, &found);
			    objget_(obj, objlis, objct);
			    objget_(objg, objlis, objctg);
			    maknam_(objct, &objsiz, &obnam, kertyp, objnm, (
				    ftnlen)80, (ftnlen)64);
			    maknam_(objctg, &objsiz, &obnam, kertyp, objnmg, (
				    ftnlen)80, (ftnlen)64);
			    if (l_le(objnm, objnmg, (ftnlen)64, (ftnlen)64)) {
				j = 0;
			    } else {
				objset_(objg, objct, objlis);
				objset_(obj, objctg, objlis);
			    }
			    j -= gap;
			}
		    }
		    gap /= 2;
		}

/*              display what we have so far and re-initialize */
/*              the window table. */

		disply_(fmtpic, &tdsp, &gdsp, &sdsp, &obnam, objlis, winsym, 
			winptr, winval, timtyp, kertyp, (ftnlen)32, (ftnlen)
			64, (ftnlen)32, (ftnlen)80);
		scardc_(&c__0, winsym, (ftnlen)64);
		scardi_(&c__0, winptr);
		scardd_(&c__0, winval);
		scardd_(&c__0, filwin);
		scardd_(&c__0, segwin);
		scardd_(&c__0, tmpwin);
		objinl_(&objsiz, &c_b212, objlis);
		nbod = objact_(bodlst);
		i__1 = nbod;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    objnth_(bodlst, &i__, bod, &found);
		    objmod_(bod, &c__6, &c__0, bodlst);
		}
	    }
	}

/*        Get the next file. */

	start = end + 1;
	fndnwd_(pass2, &start, &beg, &end, (ftnlen)25000);

/*        If we ran out of file names in the PASS2 list, we */
/*        look in the source file for the next file (if there */
/*        is such a file). */

	if (beg == 0 && sfile) {
	    s_copy(pass2, " ", (ftnlen)25000, (ftnlen)1);
	    rdnbl_(srcfil, pass2, &eof, (ftnlen)255, (ftnlen)25000);
	    replch_(pass2, tabchr, " ", pass2, (ftnlen)25000, (ftnlen)1, (
		    ftnlen)1, (ftnlen)25000);
	    if (! eof) {
		fndnwd_(pass2, &c__1, &beg, &end, (ftnlen)25000);
	    } else {
		sfile = FALSE_;
	    }
	}

/*        If we didn't find the next file in the list file, look for it */
/*        among SPKs loaded via meta-kernels. */

	if (beg == 0 && scount != 0) {
	    ++sindex;
	    kdata_(&sindex, "SPK", pass2, filtyp, source, &handle, &found, (
		    ftnlen)3, (ftnlen)25000, (ftnlen)32, (ftnlen)255);
	    if (found) {
		fndnwd_(pass2, &c__1, &beg, &end, (ftnlen)25000);
		gotspk = TRUE_;
	    }
	}
    }

/*     Check if any SPKs and PCKs files were processed. If not, signal */
/*     an error. */

    if (ndone == 0) {
	setmsg_("No binary SPK or PCK files to summarize were provided to th"
		"e program.", (ftnlen)69);
	sigerr_("SPICE(NOFILES)", (ftnlen)14);
    }

/*     Display summary for all files as treated as one. */

    if (all) {
	writit_(" ", (ftnlen)1);
	writit_("Summary for all files.", (ftnlen)22);
	writit_(" ", (ftnlen)1);

/*        Reset KERTYP based on GOTSPK and GOTPCK. */

	if (gotspk) {
	    s_copy(kertyp, "SPK", (ftnlen)80, (ftnlen)3);
	} else if (gotpck) {
	    s_copy(kertyp, "PCK", (ftnlen)80, (ftnlen)3);
	} else {
	    s_copy(kertyp, "UNK", (ftnlen)80, (ftnlen)3);
	}

/*        If the use specified time restrictions, we filter */
/*        the collected objects so that we limit them */
/*        to those that have coverage during the times specified. */

	if (at || fromto) {
	    filtem_(kertyp, &obnam, objlis, &from, &to, filwin, tmpwin, 
		    winsym, winptr, winval, (ftnlen)80, (ftnlen)64);
	}
	if (round) {

/*           Round all of the intervals of all windows. */

	    rndem_(kertyp, &obnam, objlis, &objsiz, &intval, filwin, winsym, 
		    winptr, winval, (ftnlen)80, (ftnlen)64);
	}

/*        That's the end of the filtering.  Now display */
/*        what we've got. */

	if (ulist) {

/*           If any objects are not represented in this */
/*           file say so. */

	    first = TRUE_;
	    nbod = objact_(bodlst);
	    i__1 = nbod;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		objnth_(bodlst, &i__, bod, &fnd);
		objget_(bod, bodlst, body);
		if (body[5] == 0) {
		    b = body[2];
		    e = body[3];
		    frst = body[0];
		    last = body[1];
		    item = body[4];

/*                 See if we had a range. */

		    if (frst == last) {
			if (gotspk) {
			    if (item == 1) {
/* Writing concatenation */
				i__5[0] = 12, a__3[0] = "  for body '";
				i__5[1] = e - (b - 1), a__3[1] = pass1 + (b - 
					1);
				i__5[2] = 1, a__3[2] = "'";
				s_cat(string, a__3, i__5, &c__3, (ftnlen)255);
			    } else {
/* Writing concatenation */
				i__5[0] = 17, a__3[0] = "  w.r.t. center '";
				i__5[1] = e - (b - 1), a__3[1] = pass1 + (b - 
					1);
				i__5[2] = 1, a__3[2] = "'";
				s_cat(string, a__3, i__5, &c__3, (ftnlen)255);
			    }
			} else if (gotpck) {
			    if (item == 1) {
/* Writing concatenation */
				i__5[0] = 13, a__3[0] = "  for frame '";
				i__5[1] = e - (b - 1), a__3[1] = pass1 + (b - 
					1);
				i__5[2] = 1, a__3[2] = "'";
				s_cat(string, a__3, i__5, &c__3, (ftnlen)255);
			    } else {
/* Writing concatenation */
				i__5[0] = 16, a__3[0] = "  w.r.t. frame '";
				i__5[1] = e - (b - 1), a__3[1] = pass1 + (b - 
					1);
				i__5[2] = 1, a__3[2] = "'";
				s_cat(string, a__3, i__5, &c__3, (ftnlen)255);
			    }
			} else {
			    if (item == 1) {
/* Writing concatenation */
				i__5[0] = 10, a__3[0] = "  for ID '";
				i__5[1] = e - (b - 1), a__3[1] = pass1 + (b - 
					1);
				i__5[2] = 1, a__3[2] = "'";
				s_cat(string, a__3, i__5, &c__3, (ftnlen)255);
			    } else {
/* Writing concatenation */
				i__5[0] = 13, a__3[0] = "  w.r.t. ID '";
				i__5[1] = e - (b - 1), a__3[1] = pass1 + (b - 
					1);
				i__5[2] = 1, a__3[2] = "'";
				s_cat(string, a__3, i__5, &c__3, (ftnlen)255);
			    }
			}
		    } else {
			if (gotspk) {
			    if (item == 1) {
/* Writing concatenation */
				i__5[0] = 17, a__3[0] = "  for bodies in '";
				i__5[1] = e - (b - 1), a__3[1] = pass1 + (b - 
					1);
				i__5[2] = 10, a__3[2] = "' ID range";
				s_cat(string, a__3, i__5, &c__3, (ftnlen)255);
			    } else {
/* Writing concatenation */
				i__5[0] = 21, a__3[0] = "  w.r.t. centers in"
					" '";
				i__5[1] = e - (b - 1), a__3[1] = pass1 + (b - 
					1);
				i__5[2] = 10, a__3[2] = "' ID range";
				s_cat(string, a__3, i__5, &c__3, (ftnlen)255);
			    }
			} else if (gotpck) {
			    if (item == 1) {
/* Writing concatenation */
				i__5[0] = 17, a__3[0] = "  for frames in '";
				i__5[1] = e - (b - 1), a__3[1] = pass1 + (b - 
					1);
				i__5[2] = 10, a__3[2] = "' ID range";
				s_cat(string, a__3, i__5, &c__3, (ftnlen)255);
			    } else {
/* Writing concatenation */
				i__5[0] = 20, a__3[0] = "  w.r.t. frames in '"
					;
				i__5[1] = e - (b - 1), a__3[1] = pass1 + (b - 
					1);
				i__5[2] = 10, a__3[2] = "' ID range";
				s_cat(string, a__3, i__5, &c__3, (ftnlen)255);
			    }
			} else {
			    if (item == 1) {
/* Writing concatenation */
				i__5[0] = 14, a__3[0] = "  for IDs in '";
				i__5[1] = e - (b - 1), a__3[1] = pass1 + (b - 
					1);
				i__5[2] = 10, a__3[2] = "' ID range";
				s_cat(string, a__3, i__5, &c__3, (ftnlen)255);
			    } else {
/* Writing concatenation */
				i__5[0] = 17, a__3[0] = "  w.r.t. IDs in '";
				i__5[1] = e - (b - 1), a__3[1] = pass1 + (b - 
					1);
				i__5[2] = 10, a__3[2] = "' ID range";
				s_cat(string, a__3, i__5, &c__3, (ftnlen)255);
			    }
			}
		    }
		    if (at) {
			etcal_(&from, etstr1, (ftnlen)64);
/* Writing concatenation */
			i__5[0] = 14, a__3[0] = " covering ET '";
			i__5[1] = rtrim_(etstr1, (ftnlen)64), a__3[1] = 
				etstr1;
			i__5[2] = 1, a__3[2] = "'";
			s_cat(ch__3, a__3, i__5, &c__3, (ftnlen)79);
			suffix_(ch__3, &c__0, string, rtrim_(etstr1, (ftnlen)
				64) + 15, (ftnlen)255);
		    } else if (fromto) {
			etcal_(&from, etstr1, (ftnlen)64);
			etcal_(&to, etstr2, (ftnlen)64);
/* Writing concatenation */
			i__6[0] = 19, a__4[0] = " covering from ET '";
			i__6[1] = rtrim_(etstr1, (ftnlen)64), a__4[1] = 
				etstr1;
			i__6[2] = 9, a__4[2] = "' to ET '";
			i__6[3] = rtrim_(etstr2, (ftnlen)64), a__4[3] = 
				etstr2;
			i__6[4] = 1, a__4[4] = "'";
			s_cat(ch__4, a__4, i__6, &c__5, (ftnlen)157);
			suffix_(ch__4, &c__0, string, rtrim_(etstr1, (ftnlen)
				64) + 28 + rtrim_(etstr2, (ftnlen)64) + 1, (
				ftnlen)255);
		    }
		    if (first) {
			first = FALSE_;
			writit_("There is no data: ", (ftnlen)18);
		    }
		    writit_(string, (ftnlen)255);
		}
	    }
	    if (! first) {
		writit_(" ", (ftnlen)1);
	    }
	}

/*        Compress object list and get the number of objects that */
/*        we still have in it. */

	objcmp_(objlis);
	nobj = objact_(objlis);

/*        If the number of objects is zero, check if filtering by */
/*        time and body have occurred and finish "No data ..." */
/*        reporting. */

	if (nobj == 0 && ! ulist) {
	    writit_("There is no data: ", (ftnlen)18);
	    if (at) {
		etcal_(&from, etstr1, (ftnlen)64);
/* Writing concatenation */
		i__5[0] = 15, a__3[0] = "  covering ET '";
		i__5[1] = rtrim_(etstr1, (ftnlen)64), a__3[1] = etstr1;
		i__5[2] = 1, a__3[2] = "'";
		s_cat(ch__5, a__3, i__5, &c__3, (ftnlen)80);
		writit_(ch__5, rtrim_(etstr1, (ftnlen)64) + 16);
		writit_(" ", (ftnlen)1);
	    } else if (fromto) {
		etcal_(&from, etstr1, (ftnlen)64);
		etcal_(&to, etstr2, (ftnlen)64);
/* Writing concatenation */
		i__6[0] = 20, a__4[0] = "  covering from ET '";
		i__6[1] = rtrim_(etstr1, (ftnlen)64), a__4[1] = etstr1;
		i__6[2] = 9, a__4[2] = "' to ET '";
		i__6[3] = rtrim_(etstr2, (ftnlen)64), a__4[3] = etstr2;
		i__6[4] = 1, a__4[4] = "'";
		s_cat(ch__6, a__4, i__6, &c__5, (ftnlen)158);
		writit_(ch__6, rtrim_(etstr1, (ftnlen)64) + 29 + rtrim_(
			etstr2, (ftnlen)64) + 1);
		writit_(" ", (ftnlen)1);
	    }
	}

/*        We are going to sort the object list.  We just rip off */
/*        the Shell sort algorithm to accomplish this. */

	gap = nobj / 2;
	while(gap > 0) {
	    i__1 = nobj;
	    for (i__ = gap + 1; i__ <= i__1; ++i__) {
		j = i__ - gap;
		while(j > 0) {
		    jg = j + gap;
		    objnth_(objlis, &j, obj, &found);
		    objnth_(objlis, &jg, objg, &found);
		    objget_(obj, objlis, objct);
		    objget_(objg, objlis, objctg);
		    maknam_(objct, &objsiz, &obnam, kertyp, objnm, (ftnlen)80,
			     (ftnlen)64);
		    maknam_(objctg, &objsiz, &obnam, kertyp, objnmg, (ftnlen)
			    80, (ftnlen)64);
		    if (l_le(objnm, objnmg, (ftnlen)64, (ftnlen)64)) {
			j = 0;
		    } else {
			objset_(objg, objct, objlis);
			objset_(obj, objctg, objlis);
		    }
		    j -= gap;
		}
	    }
	    gap /= 2;
	}

/*        display what we have. */

	disply_(fmtpic, &tdsp, &gdsp, &sdsp, &obnam, objlis, winsym, winptr, 
		winval, timtyp, kertyp, (ftnlen)32, (ftnlen)64, (ftnlen)32, (
		ftnlen)80);
    }

/*     Check out. */

    chkout_("BRIEF", (ftnlen)5);

/*     Return with success status. */

    byebye_("SUCCESS", (ftnlen)7);
    return 0;
} /* MAIN__ */


/*     Miscellaneous functions that don't deserve to have their own */
/*     source files. */

integer inc_(integer *a)
{
    /* System generated locals */
    integer ret_val;

    ret_val = *a + 1;
    return ret_val;
} /* inc_ */

integer bigger_(integer *a, integer *b)
{
    /* System generated locals */
    integer ret_val;

    ret_val = max(*a,*b);
    return ret_val;
} /* bigger_ */

logical mchboc_0_(int n__, integer *object, integer *size, integer *id, 
	integer *cent)
{
    /* System generated locals */
    integer i__1, i__2;
    logical ret_val;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static integer ref[2];

    /* Parameter adjustments */
    if (object) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_idmch;
	case 2: goto L_idset;
	}

    ret_val = FALSE_;
    return ret_val;

L_idmch:
    ret_val = object[0] <= ref[(i__1 = object[4] - 1) < 2 && 0 <= i__1 ? i__1 
	    : s_rnge("ref", i__1, "mchboc_", (ftnlen)2618)] && object[1] >= 
	    ref[(i__2 = object[4] - 1) < 2 && 0 <= i__2 ? i__2 : s_rnge("ref",
	     i__2, "mchboc_", (ftnlen)2618)];
    return ret_val;

L_idset:
    ref[0] = *id;
    ref[1] = *cent;
    ret_val = TRUE_;
    return ret_val;
} /* mchboc_ */

logical mchboc_(integer *object, integer *size, integer *id, integer *cent)
{
    return mchboc_0_(0, object, size, id, cent);
    }

logical idmch_(integer *object, integer *size)
{
    return mchboc_0_(1, object, size, (integer *)0, (integer *)0);
    }

logical idset_(integer *id, integer *cent)
{
    return mchboc_0_(2, (integer *)0, (integer *)0, id, cent);
    }

/* Main program alias */ int brief_ () { MAIN__ (); return 0; }
