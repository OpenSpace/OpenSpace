/* bboard_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__100 = 100;
static integer c__5000 = 5000;
static integer c__300 = 300;
static integer c__404 = 404;
static integer c__50 = 50;

/* $Procedure BBOARD ( Bulletin board ) */
/* Subroutine */ int bboard_0_(int n__, char *action, char *item, integer *n, 
	integer *ivals, doublereal *dvals, char *cvals, char *sval, ftnlen 
	action_len, ftnlen item_len, ftnlen cvals_len, ftnlen sval_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char nbuf[32*106];
    static integer pbuf[410];
    static char vbuf[100*51], what[32];
    static integer i__;
    static char cntab[32*106], dntab[32*106];
    static integer cptab[106], dptab[106];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char intab[32*106];
    static integer iptab[106];
    static doublereal dvtab[5006];
    static char cvtab[255*306], which[32];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    static integer ivtab[5006];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen), 
	    sydelc_(char *, char *, integer *, char *, ftnlen, ftnlen, ftnlen)
	    , sydeld_(char *, char *, integer *, doublereal *, ftnlen, ftnlen)
	    ;
    extern integer sydimc_(char *, char *, integer *, char *, ftnlen, ftnlen, 
	    ftnlen), sydimd_(char *, char *, integer *, doublereal *, ftnlen, 
	    ftnlen);
    extern /* Subroutine */ int sydeli_(char *, char *, integer *, integer *, 
	    ftnlen, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, ftnlen);
    extern integer sydimi_(char *, char *, integer *, integer *, ftnlen, 
	    ftnlen);
    extern /* Subroutine */ int sygetd_(char *, char *, integer *, doublereal 
	    *, integer *, doublereal *, logical *, ftnlen, ftnlen), sygetc_(
	    char *, char *, integer *, char *, integer *, char *, logical *, 
	    ftnlen, ftnlen, ftnlen, ftnlen), ssizec_(integer *, char *, 
	    ftnlen), setmsg_(char *, ftnlen), syenqd_(char *, doublereal *, 
	    char *, integer *, doublereal *, ftnlen, ftnlen), syenqc_(char *, 
	    char *, char *, integer *, char *, ftnlen, ftnlen, ftnlen, ftnlen)
	    , sygeti_(char *, char *, integer *, integer *, integer *, 
	    integer *, logical *, ftnlen, ftnlen), cmprss_(char *, integer *, 
	    char *, char *, ftnlen, ftnlen, ftnlen), syenqi_(char *, integer *
	    , char *, integer *, integer *, ftnlen, ftnlen), sypshc_(char *, 
	    char *, char *, integer *, char *, ftnlen, ftnlen, ftnlen, ftnlen)
	    , sypshd_(char *, doublereal *, char *, integer *, doublereal *, 
	    ftnlen, ftnlen), ssizei_(integer *, integer *), ssized_(integer *,
	     doublereal *), sypopc_(char *, char *, integer *, char *, char *,
	     logical *, ftnlen, ftnlen, ftnlen, ftnlen), sypopd_(char *, char 
	    *, integer *, doublereal *, doublereal *, logical *, ftnlen, 
	    ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int sypshi_(char *, integer *, char *, integer *, 
	    integer *, ftnlen, ftnlen), sypopi_(char *, char *, integer *, 
	    integer *, integer *, logical *, ftnlen, ftnlen), sbget_1__(char *
	    , char *, integer *, char *, char *, integer *, ftnlen, ftnlen, 
	    ftnlen, ftnlen), syputc_(char *, char *, integer *, char *, 
	    integer *, char *, ftnlen, ftnlen, ftnlen, ftnlen), syputd_(char *
	    , doublereal *, integer *, char *, integer *, doublereal *, 
	    ftnlen, ftnlen), sbrem_1__(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), syputi_(char *, integer *, integer *, 
	    char *, integer *, integer *, ftnlen, ftnlen), sbset_1__(char *, 
	    char *, char *, integer *, char *, ftnlen, ftnlen, ftnlen, ftnlen)
	    ;
    static logical fnd;
    static integer pos;
    extern /* Subroutine */ int sbinit_1__(integer *, integer *, integer *, 
	    char *, integer *, char *, ftnlen, ftnlen);

/* $ Abstract */

/*     Maintain a global bulletin board for use by application */
/*     programs. */

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

/*     BBOARD */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ACTION     I   Action to be taken. */
/*     ITEM       I   Item to be posted or retrieved. */
/*     N         I,O  Number of values posted or retrieved. */
/*     IVALS     I,O  Integer values. */
/*     DVALS     I,O  Double precision values */
/*     CVALS     I,O  Character values. */
/*     SVAL      I,O  String value. */
/*     MAXNL      P   Maximum name length. */
/*     MAXCL      P   Maximum character length. */
/*     MAXI       P   Maximum number of integer items. */
/*     MAXD       P   Maximum number of double precision items. */
/*     MAXC       P   Maximum number of character items. */
/*     MAXS       P   Maximum number of string items. */
/*     MAXIV      P   Maximum number of integer values. */
/*     MAXDV      P   Maximum number of double precision values. */
/*     MAXCV      P   Maximum number of character values. */
/*     MAXCHR     P   Maximum number of string characters. */

/* $ Detailed_Input */

/*     ACTION     is used by entry points BBPUT and BBGET to indicate */
/*                a specific action to be taken. Possible actions */
/*                are 'POST', 'COPY', 'TAKE', 'PUSH', 'APPEND', and */
/*                'POP'. */

/*     ITEM       is the name of an item to be posted, retrieved, */
/*                removed, and so on. Names are case-sensitive, but */
/*                leading and embedded blanks are ignored. */

/*     N          on input is the number of values to be posted. */

/*     IVALS, */
/*     DVALS, */
/*     CVALS,     on input are values to be associated with a specific */
/*                integer, DP, or character item on the board. */

/*     SVAL       on input is a string value to be associated with a */
/*                specific string item on the board. */

/* $ Detailed_Output */

/*     N          on output is the number of values being returned, */
/*                or the number of values associated with an item. */

/*     IVALS, */
/*     DVALS, */
/*     CVALS,     on output are values associated with a specific */
/*                integer, DP, or character item on the board. */

/*     SVAL       on output is a string value associated with a */
/*                specific string item on the board. */

/* $ Parameters */

/*     MAXNL      is the maximum number of characters that can make */
/*                up an item name. */

/*     MAXCL      is the declared length of the individual values */
/*                of character items. That is, each multi-valued */
/*                character item is equivalent to a CHARACTER*(MAXCL) */
/*                array. */

/*     MAXI, */
/*     MAXD, */
/*     MAXC, */
/*     MAXS,      are the maximum numbers of items of each type */
/*                (integer, DP, character, and string) that can be */
/*                stored simultaneously. */

/*     MAXIV, */
/*     MAXDV, */
/*     MAXCV      are the maximum numbers of values of each type */
/*                (integer, DP, and character) that can be stored */
/*                simultaneously. MAXIV, MAXDV, and MAXCV must be */
/*                at least as large as MAXI, MAXD, and MAXC, */
/*                respectively. (Note that string items are are */
/*                not multi-valued.) */

/*     MAXCHR     is the maximum number characters that can be used */
/*                to store string items at any one time. MAXCHR must */
/*                be an integer multiple of 100. */

/* $ Exceptions */

/*     1) If BBOARD is called directly, the error 'SPICE(BOGUSENTRY)' */
/*        is signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*           BBOARD implements a global storage area, which may be */
/*           used by the individual modules of an application program */
/*           communicate with each other. The metaphor for this area */
/*           is a bulletin board: modules may leave messages (called */
/*           `items') on this board, to be copied, modified, or removed */
/*           by other modules. */

/*     Types */

/*           The board can contain four types of items: integer, double */
/*           precision (DP), character, and string. The first three types */
/*           may be multi-valued: for example, a single integer item may */
/*           be associated with more than one integer value. Thus, the */
/*           board may be used to store arrays as well as scalar values. */

/*           Each string item may contain only one value: however, */
/*           the number of characters in this value may be anywhere */
/*           between one and the total size of the string buffer */
/*           (parameter MAXCHR). */

/*     Posting items */

/*           An item may be posted on the board by calling BBPUTx */
/*           (where x indicates the type of the item: I, D, C, or S). */
/*           For example, the call */

/*              IMAGES(1) = '22421.36' */
/*              IMAGES(2) = '22421.39' */
/*              IMAGES(3) = '22421.45' */

/*              CALL BBPUTC ( 'POST', 'IMAGE QUEUE', 3, IMAGES ) */

/*           creates an item with the name 'IMAGE QUEUE', which contains */
/*           the three character values in the array IMAGES. If an item */
/*           with that name already exists, it is replaced. */

/*     Item names */

/*           Item names are case-sensitive, but blanks are ignored. */
/*           The item 'IMAGE QUEUE' may be accessed under any of the */
/*           following names. */

/*              'IMAGE QUEUE' */
/*              'IMAGEQUEUE' */
/*              '  IMAGE   QUEUE  ' */

/*           On the other hand, the names */

/*              'Image queue' */
/*              'image queue' */
/*              'Image Queue' */

/*           all refer to distinct items. */

/*           The same item name may be applied to one item of each */
/*           type. This allows you to associate values of different */
/*           types under a single name, as in the following example. */

/*              IMAGES(  1) = '22421.36' */
/*              BODIES(  1) =  801 */
/*              RADII (1,1) =  1600.D0 */
/*              RADII (2,1) =  1600.D0 */
/*              RADII (3,1) =  1600.D0 */

/*              IMAGES(  2) = '22427.19' */
/*              BODIES(  2) =  899 */
/*              RADII (1,2) =  25295.D0 */
/*              RADII (2,2) =  25295.D0 */
/*              RADII (3,2) =  24738.D0 */

/*              DESCR       = 'Preliminary NINA testing, 4/12/89' */

/*              CALL BBPUTC ( 'POST', 'IMAGE QUEUE', 2, IMAGES ) */
/*              CALL BBPUTI ( 'POST', 'IMAGE QUEUE', 2, BODIES ) */
/*              CALL BBPUTD ( 'POST', 'IMAGE QUEUE', 6, RADII  ) */
/*              CALL BBPUTS ( 'POST', 'IMAGE QUEUE',    DESCR  ) */

/*     Copying items */

/*           Once an item has been posted, its values may be copied */
/*           by calling BBGETx. For example, the call */

/*              CALL BBGETC ( 'COPY', 'IMAGE QUEUE', N, IMAGES ) */

/*           copies the values associated with the character item */
/*           'IMAGE QUEUE' into the character array IMAGES. All of */
/*           the values associated with the item are returned. */

/*     Taking items */

/*           When an item is copied, its values remain intact, ready */
/*           to be copied by other modules. Posted items may also be */
/*           taken by calling BBGETx. For example, the call */

/*              CALL BBGETD ( 'TAKE', 'IMAGE QUEUE', N, IMAGES ) */

/*           returns the values just as the previous call did; however, */
/*           following this call, the item is no longer on the board. */

/*     Removing items */

/*           It is possible to remove an item without copying its values, */
/*           by calling BBREMx. For example, the calls */

/*              CALL BBREMC ( 'IMAGE QUEUE' ) */
/*              CALL BBREMI ( 'IMAGE QUEUE' ) */
/*              CALL BBREMD ( 'IMAGE QUEUE' ) */
/*              CALL BBREMS ( 'IMAGE QUEUE' ) */

/*           removes these items from the board without allocating space */
/*           for the return of any values. Removing an item that is not */
/*           on the board does not cause an error. */

/*     Stacks and Queues */

/*           The list of values associated with a multi-valued item */
/*           may be thought of as a stack or queue. The values can be */
/*           popped (copied and removed) from this list in pieces, */
/*           instead of all at once. Thus, the images in 'IMAGE QUEUE' */
/*           can be processed as shown below. */

/*              DO I = 1, 2 */
/*                 CALL BBGETC ( 'POP', 'IMAGE QUEUE', 1, IMAGE ) */
/*                 CALL BBGETI ( 'POP', 'IMAGE QUEUE', 1, BODY  ) */
/*                 CALL BBGETD ( 'POP', 'IMAGE QUEUE', 3, RADII ) */
/*                  . */
/*                  . */

/*              END DO */

/*           Values may be added to the beginning of the value list */
/*           (treating it as a push-down stack), or to the end of the */
/*           list (treating it as a queue). The following sequence */

/*              CALL BBPUTI ( 'POST', 'SEQUENCE', 1, 5 ) */

/*              DATA(1) = 1 */
/*              DATA(2) = 2 */
/*              DATA(3) = 3 */
/*              DATA(4) = 4 */
/*              CALL BBPUTI ( 'PUSH', 'SEQUENCE', 4, DATA ) */

/*              DATA(1) = 6 */
/*              DATA(2) = 7 */
/*              DATA(3) = 8 */
/*              DATA(4) = 9 */
/*              CALL BBPUTI ( 'APPEND', 'SEQUENCE', 4, DATA ) */

/*           creates an integer item 'SEQUENCE' containing the numbers */
/*           1-9 in order. */

/*           Pushing or appending values onto an item that doesn't exist */
/*           causes the item to be created. */

/*     Finding items */

/*           Attempting to copy, take, or pop values from an item not on */
/*           the board results in an error (which is reported through the */
/*           normal SPICELIB error handling mechanism). The presence of */
/*           an item may be confirmed by calling BBFNDx. For example, */
/*           the call */

/*              CALL BBFNDI ( 'SEQUENCE', N ) */

/*           returns a value of 9 in N, because 'SEQUENCE' contains nine */
/*           values. Items not on the board contain zero values. (Note */
/*           that BBFNDS, which finds string items, can only return one */
/*           or zero.) */

/*     Clearing the board */

/*           The entire board may be cleared at any time by calling */
/*           BBCLR, */

/*              CALL BBCLR */

/*           The board MUST be cleared at least once (usually by the */
/*           main module of the calling program) before any items can */
/*           be posted. */

/*     Storage */

/*           Because standard Fortran-77 does not allow storage to be */
/*           allocated dynamically, the storage used by the bulletin */
/*           board must be allocated when BBOARD is compiled, by */
/*           setting the values of the parameters MAXNL, MAXCL, MAXI, */
/*           MAXD, MAXS, MAXIV, MAXDV, MAXCV, and MAXCHR. */

/* $ Examples */

/*     Consider the following program, */

/*        PROGRAM SIMPLE */

/*        CALL BBCLR */

/*        CALL READ */
/*        CALL BBFNDS ( 'NAME', N ) */

/*        DO WHILE ( N .GT. 0 ) */
/*           CALL LOOK_UP */
/*           CALL COMPUTE */
/*           CALL PRINT */

/*           CALL READ */
/*           CALL BBFNDS ( 'NAME', N ) */
/*        END DO */

/*        END */

/*     which calls four modules: */

/*        READ      which reads the name of a picture file from the */
/*                  standard input file, and places it on the bulletin */
/*                  board as string item 'NAME'. */

/*        LOOK_UP   which looks up the spacecraft event time, filter */
/*                  number, exposure time, and pointing angles for the */
/*                  picture in the picture file. */

/*        COMPUTE   which computes the equivalent pointing in two */
/*                  auxiliary coordinate systems. */

/*        PRINT     which prints everything to the standard output file. */

/*     The program begins by clearing the bulletin board. This prepares */
/*     the board for use by the rest of the program. */

/*     READ begins by removing item NAME from the board. It then attempts */
/*     to read the name of the next picture file. If successful, it posts */
/*     the name. (If not the board will not contain the item, and the */
/*     program will terminate.) */

/*        SUBROUTINE READ */

/*        CHARACTER*128   FILE */
/*        INTEGER         IOSTAT */

/*        CALL BBREMS ( 'NAME' ) */
/*        READ (*,*,IOSTAT=IOSTAT) FILE */

/*        IF ( IOSTAT .EQ. 0 ) THEN */
/*           CALL BBPUTS ( 'POST', 'NAME', FILE ) */
/*        END IF */

/*        RETURN */
/*        END */

/*     LOOK_UP uses the name of the file as an index into a database */
/*     (the details of which are not important). It retrieves the items */
/*     of interest from the database, and posts them on the board. */
/*     (Note that the spacecraft event time is posted in UTC and ET.) */


/*        SUBROUTINE LOOK_UP */

/*        CALL BBGETS ( 'COPY', 'NAME', INDEX ) */
/*         . */
/*         . */

/*        CALL BBPUTS ( 'POST', 'S/C EVENT (UTC)',    UTC   ) */
/*        CALL BBPUTD ( 'POST', 'S/C EVENT (ET)',  1, ET    ) */
/*        CALL BBPUTI ( 'POST', 'FILTER NUMBER',   1, FNUM  ) */
/*        CALL BBPUTD ( 'POST', 'EXPOSURE',        1, EXP   ) */
/*        CALL BBPUTD ( 'POST', 'POINTING (CCT)',  3, CCT   ) */

/*        RETURN */
/*        END */

/*     COMPUTE begins with the nominal (Clock, Cone, Twist) pointing */
/*     and the spacecraft event time, and computes the equivalent */
/*     pointing in two other systems: Azimuth, Elevation, Twist; and */
/*     Right ascension, Declination, Twist. (Again, the details are not */
/*     important.) These are stored on the board. */

/*     The conversion depends on an optional bias angle, which may */
/*     or may not be posted. If not found, it defaults to zero. */

/*        SUBROUTINE COMPUTE */
/*         . */
/*         . */

/*        CALL BBGETD ( 'COPY', 'POINTING (CCT)', N, CCT ) */
/*        CALL BBGETD ( 'COPY', 'S/C EVENT (ET)', N, ET  ) */

/*        CALL BBFNDD ( 'BIAS', N ) */
/*        IF ( N .EQ. 1 ) THEN */
/*           CALL BBGETD ( 'COPY', 'BIAS', N, BIAS ) */
/*        ELSE */
/*           BIAS = 0.D0 */
/*        END IF */
/*         . */
/*         . */

/*        CALL BBPUTD ( 'POST', 'POINTING (AET)', 3, AET ) */
/*        CALL BBPUTD ( 'POST', 'POINTING (RDT)', 3, RDT ) */

/*        RETURN */
/*        END */

/*     PRINT simply retrieves the items from the board and writes */
/*     them to the standard output file. The items are removed from */
/*     the board as their values are printed, freeing space for use */
/*     by other parts of the program. (This is largely a preventative */
/*     measure: it is not necessary for the program as it stands, */
/*     but it could become important as the program undergoes further */
/*     development.) */

/*        SUBROUTINE PRINT */
/*         . */
/*         . */

/*        CALL BBGETS ( 'TAKE', 'NAME', STRING ) */
/*        WRITE (*,*) */
/*        WRITE (*,*) STRING */

/*         . */
/*         . */

/*        CALL BBGETS ( 'TAKE', 'POINTING (RDT)', N, NUMBERS ) */
/*        WRITE (*,*) ( NUMBERS(I), I = 1, N ) */

/*        RETURN */
/*        END */

/* $ Restrictions */

/*     1) The values of parameters MAXNL and MAXCL must not be smaller */
/*        than the value of parameter MINLEN in subroutine ENCHAR. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart  (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 5-APR-1989 (DMT) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

/*     Integer, DP, and character items are stored in symbol tables. */
/*     Later, they should be stored in card catalogs, when the necessary */
/*     routines have been completed. */

/*     Strings are stored in a string buffer. */

/*     Actions, where input, are compressed and converted to uppercase */
/*     (WHAT). Item names are compressed (WHICH). */


/*     Save everything between calls. */


/*     Standard SPICE error handling. */

    /* Parameter adjustments */
    if (ivals) {
	}
    if (dvals) {
	}
    if (cvals) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_bbputi_1;
	case 2: goto L_bbputd_1;
	case 3: goto L_bbputc_1;
	case 4: goto L_bbputs_1;
	case 5: goto L_bbgeti_1;
	case 6: goto L_bbgetd_1;
	case 7: goto L_bbgetc_1;
	case 8: goto L_bbgets_1;
	case 9: goto L_bbremi_1;
	case 10: goto L_bbremd_1;
	case 11: goto L_bbremc_1;
	case 12: goto L_bbrems_1;
	case 13: goto L_bbfndi_1;
	case 14: goto L_bbfndd_1;
	case 15: goto L_bbfndc_1;
	case 16: goto L_bbfnds_1;
	case 17: goto L_bbclr_1;
	}

    if (return_()) {
	return 0;
    } else {
	chkin_("BBOARD", (ftnlen)6);
    }
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("BBOARD", (ftnlen)6);
    return 0;
/* $Procedure BBPUTI ( Bulletin board, put, integer ) */

L_bbputi_1:
/* $ Abstract */

/*     Put one or more values on the board, associated with */
/*     an integer item. */

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

/*     BBOARD */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         ACTION */
/*     CHARACTER*(*)         ITEM */
/*     INTEGER               N */
/*     INTEGER               IVALS */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ACTION     I   Action: 'POST', 'PUSH', or 'APPEND'. */
/*     ITEM       I   Item name. */
/*     N          I   Number of values to be posted. */
/*     IVALS      I   Values to be posted. */

/* $ Detailed_Input */

/*     ACTION      specifies an action to be taken. Possible actions */
/*                 are 'POST', 'PUSH', and 'APPEND'. */

/*     ITEM        is the name of an integer item, which may or */
/*                 may not be on the board already. */

/*     N           is the number of values to be associated with the */
/*                 specified item. */

/*     IVALS       are the values. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See BBOARD. */

/* $ Exceptions */

/*     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)' */
/*        is signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     'POST' creates a new item, containing the specified values. */
/*     (If an item of the same type with the same name already exists, */
/*     it is replaced.) */

/*     'PUSH' modifies the list of values associated with an existing */
/*     item by placing items at the beginning of the list (treating the */
/*     list as a push-down stack). */

/*     'APPEND' modifies the list of values associated with an existing */
/*     item by placing items at the end of the list (treating the list */
/*     as a queue). */

/*     Both 'PUSH' and 'APPEND' will create a new item if the specified */
/*     item does not exist. */

/* $ Examples */

/*     See BBOARD. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart  (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 5-APR-1989 (DMT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("BBPUTI_1", (ftnlen)8);
    }

/*     Compress spaces, change cases, as needed. */

    cmprss_(" ", &c__0, action, what, (ftnlen)1, action_len, (ftnlen)32);
    ucase_(what, what, (ftnlen)32, (ftnlen)32);
    cmprss_(" ", &c__0, item, which, (ftnlen)1, item_len, (ftnlen)32);

/*     The real work is done by the symbol table routines. (Later, */
/*     it will be done by the card catalog routines.) Note that */
/*     items must be pushed and appended one at a time. */

    if (s_cmp(what, "POST", (ftnlen)32, (ftnlen)4) == 0) {
	syputi_(which, ivals, n, intab, iptab, ivtab, (ftnlen)32, (ftnlen)32);
    } else if (s_cmp(what, "PUSH", (ftnlen)32, (ftnlen)4) == 0) {
	for (i__ = *n; i__ >= 1; --i__) {
	    sypshi_(which, &ivals[i__ - 1], intab, iptab, ivtab, (ftnlen)32, (
		    ftnlen)32);
	}
    } else if (s_cmp(what, "APPEND", (ftnlen)32, (ftnlen)6) == 0) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    syenqi_(which, &ivals[i__ - 1], intab, iptab, ivtab, (ftnlen)32, (
		    ftnlen)32);
	}
    } else {
	setmsg_("Sorry, # is not a legal action.", (ftnlen)31);
	errch_("#", what, (ftnlen)1, (ftnlen)32);
	sigerr_("SPICE(UNNATURALACT)", (ftnlen)19);
    }
    chkout_("BBPUTI_1", (ftnlen)8);
    return 0;
/* $Procedure BBPUTD ( Bulletin board, put, DP ) */

L_bbputd_1:
/* $ Abstract */

/*     Put one or more values on the board, associated with */
/*     a DP item. */

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

/*     BBOARD */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         ACTION */
/*     CHARACTER*(*)         ITEM */
/*     INTEGER               N */
/*     DOUBLE PRECISION      DVALS */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ACTION     I   Action: 'POST', 'PUSH', or 'APPEND'. */
/*     ITEM       I   Item name. */
/*     N          I   Number of values to be posted. */
/*     DVALS      I   Values to be posted. */

/* $ Detailed_Input */

/*     ACTION      specifies an action to be taken. Possible actions */
/*                 are 'POST', 'PUSH', and 'APPEND'. */

/*     ITEM        is the name of a DP item, which may or */
/*                 may not be on the board already. */

/*     N           is the number of values to be associated with the */
/*                 specified item. */

/*     DVALS       are the values. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See BBOARD. */

/* $ Exceptions */

/*     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)' */
/*        is signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     'POST' creates a new item, containing the specified values. */
/*     (If an item of the same type with the same name already exists, */
/*     it is replaced.) */

/*     'PUSH' modifies the list of values associated with an existing */
/*     item by placing items at the beginning of the list (treating the */
/*     list as a push-down stack). */

/*     'APPEND' modifies the list of values associated with an existing */
/*     item by placing items at the end of the list (treating the list */
/*     as a queue). */

/*     Both 'PUSH' and 'APPEND' will create a new item if the specified */
/*     item does not exist. */

/* $ Examples */

/*     See BBOARD. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart  (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 5-APR-1989 (DMT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("BBPUTD_1", (ftnlen)8);
    }

/*     Compress spaces, change cases, as needed. */

    cmprss_(" ", &c__0, action, what, (ftnlen)1, action_len, (ftnlen)32);
    ucase_(what, what, (ftnlen)32, (ftnlen)32);
    cmprss_(" ", &c__0, item, which, (ftnlen)1, item_len, (ftnlen)32);

/*     The real work is done by the symbol table routines. (Later, */
/*     it will be done by the card catalog routines.) Note that */
/*     items must be pushed and appended one at a time. */

    if (s_cmp(what, "POST", (ftnlen)32, (ftnlen)4) == 0) {
	syputd_(which, dvals, n, dntab, dptab, dvtab, (ftnlen)32, (ftnlen)32);
    } else if (s_cmp(what, "PUSH", (ftnlen)32, (ftnlen)4) == 0) {
	for (i__ = *n; i__ >= 1; --i__) {
	    sypshd_(which, &dvals[i__ - 1], dntab, dptab, dvtab, (ftnlen)32, (
		    ftnlen)32);
	}
    } else if (s_cmp(what, "APPEND", (ftnlen)32, (ftnlen)6) == 0) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    syenqd_(which, &dvals[i__ - 1], dntab, dptab, dvtab, (ftnlen)32, (
		    ftnlen)32);
	}
    } else {
	setmsg_("Sorry, # is not a legal action.", (ftnlen)31);
	errch_("#", what, (ftnlen)1, (ftnlen)32);
	sigerr_("SPICE(UNNATURALACT)", (ftnlen)19);
    }
    chkout_("BBPUTD_1", (ftnlen)8);
    return 0;
/* $Procedure BBPUTC ( Bulletin board, put, character ) */

L_bbputc_1:
/* $ Abstract */

/*     Put one or more values on the board, associated with */
/*     a character item. */

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

/*     BBOARD */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         ACTION */
/*     CHARACTER*(*)         ITEM */
/*     INTEGER               N */
/*     CHARACTER*(*)         CVALS */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ACTION     I   Action: 'POST', 'PUSH', or 'APPEND'. */
/*     ITEM       I   Item name. */
/*     N          I   Number of values to be posted. */
/*     CVALS      I   Values to be posted. */

/* $ Detailed_Input */

/*     ACTION      specifies an action to be taken. Possible actions */
/*                 are 'POST', 'PUSH', and 'APPEND'. */

/*     ITEM        is the name of a character item, which may or */
/*                 may not be on the board already. */

/*     N           is the number of values to be associated with the */
/*                 specified item. */

/*     CVALS       are the values. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See BBOARD. */

/* $ Exceptions */

/*     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)' */
/*        is signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     'POST' creates a new item, containing the specified values. */
/*     (If an item of the same type with the same name already exists, */
/*     it is replaced.) */

/*     'PUSH' modifies the list of values associated with an existing */
/*     item by placing items at the beginning of the list (treating the */
/*     list as a push-down stack). */

/*     'APPEND' modifies the list of values associated with an existing */
/*     item by placing items at the end of the list (treating the list */
/*     as a queue). */

/*     Both 'PUSH' and 'APPEND' will create a new item if the specified */
/*     item does not exist. */

/* $ Examples */

/*     See BBOARD. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart  (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 5-APR-1989 (DMT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("BBPUTC_1", (ftnlen)8);
    }

/*     Compress spaces, change cases, as needed. */

    cmprss_(" ", &c__0, action, what, (ftnlen)1, action_len, (ftnlen)32);
    ucase_(what, what, (ftnlen)32, (ftnlen)32);
    cmprss_(" ", &c__0, item, which, (ftnlen)1, item_len, (ftnlen)32);

/*     The real work is done by the symbol table routines. (Later, */
/*     it will be done by the card catalog routines.) Note that */
/*     items must be pushed and appended one at a time. */

    if (s_cmp(what, "POST", (ftnlen)32, (ftnlen)4) == 0) {
	syputc_(which, cvals, n, cntab, cptab, cvtab, (ftnlen)32, cvals_len, (
		ftnlen)32, (ftnlen)255);
    } else if (s_cmp(what, "PUSH", (ftnlen)32, (ftnlen)4) == 0) {
	for (i__ = *n; i__ >= 1; --i__) {
	    sypshc_(which, cvals + (i__ - 1) * cvals_len, cntab, cptab, cvtab,
		     (ftnlen)32, cvals_len, (ftnlen)32, (ftnlen)255);
	}
    } else if (s_cmp(what, "APPEND", (ftnlen)32, (ftnlen)6) == 0) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    syenqc_(which, cvals + (i__ - 1) * cvals_len, cntab, cptab, cvtab,
		     (ftnlen)32, cvals_len, (ftnlen)32, (ftnlen)255);
	}
    } else {
	setmsg_("Sorry, # is not a legal action.", (ftnlen)31);
	errch_("#", what, (ftnlen)1, (ftnlen)32);
	sigerr_("SPICE(UNNATURALACT)", (ftnlen)19);
    }
    chkout_("BBPUTC_1", (ftnlen)8);
    return 0;
/* $Procedure BBPUTS ( Bulletin board, put, string ) */

L_bbputs_1:
/* $ Abstract */

/*     Put a value on the board, associated with a string item. */

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

/*     BBOARD */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         ACTION */
/*     CHARACTER*(*)         ITEM */
/*     INTEGER               N */
/*     CHARACTER*(*)         SVAL */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ACTION     I   Action: 'POST'. */
/*     ITEM       I   Item name. */
/*     SVAL       I   Value to be posted. */

/* $ Detailed_Input */

/*     ACTION      specifies an action to be taken. Currently, the */
/*                 only possible action is 'POST'. */

/*     ITEM        is the name of a string item, which may or */
/*                 may not be on the board already. */

/*     SVAL        is the value to be associated with the specified */
/*                 item. Trailing blanks are ignored. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See BBOARD. */

/* $ Exceptions */

/*     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)' */
/*        is signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     'POST' creates a new item, containing the specified value. */
/*     (If an item of the same type with the same name already exists, */
/*     it is replaced.) */

/* $ Examples */

/*     See BBOARD. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart  (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 5-APR-1989 (DMT) */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("BBPUTS_1", (ftnlen)8);
    }
    cmprss_(" ", &c__0, action, what, (ftnlen)1, action_len, (ftnlen)32);
    ucase_(what, what, (ftnlen)32, (ftnlen)32);
    cmprss_(" ", &c__0, item, which, (ftnlen)1, item_len, (ftnlen)32);
    if (s_cmp(what, "POST", (ftnlen)32, (ftnlen)4) == 0) {
	sbset_1__(which, sval, nbuf, pbuf, vbuf, (ftnlen)32, sval_len, (
		ftnlen)32, (ftnlen)100);
    } else {
	setmsg_("Sorry, # is not a legal action.", (ftnlen)31);
	errch_("#", what, (ftnlen)1, (ftnlen)32);
	sigerr_("SPICE(UNNATURALACT)", (ftnlen)19);
    }
    chkout_("BBPUTS_1", (ftnlen)8);
    return 0;
/* $Procedure BBGETI ( Bulletin board, get, integer ) */

L_bbgeti_1:
/* $ Abstract */

/*     Get one or more values from the board, associated with */
/*     an integer item. */

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

/*     BBOARD */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         ACTION */
/*     CHARACTER*(*)         ITEM */
/*     INTEGER               N */
/*     INTEGER               IVALS */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ACTION     I   Action: 'COPY', 'TAKE', or 'POP'. */
/*     ITEM       I   Item name. */
/*     N         I,O  Number of values returned. */
/*     IVALS      O   Values. */

/* $ Detailed_Input */

/*     ACTION      specifies an action to be taken. Possible actions */
/*                 are 'COPY', 'TAKE', and 'POP'. */

/*     ITEM        is the name of an integer item, which must be */
/*                 on the board. */

/*     N           on input is the number of values to be popped. */


/* $ Detailed_Output */

/*     N           on output is the number of values returned. */

/*     IVALS       are some or all of the values associated with the */
/*                 specified item. */

/* $ Parameters */

/*     See BBOARD. */

/* $ Exceptions */

/*     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)' */
/*        is signalled. */

/*     2) If ITEM is not found, or if the number of values to be popped */
/*        is smaller than the number of values associated with the item, */
/*        the error 'SPICE(ALLGONE)' is signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     'COPY' returns all of the values associated with the specified */
/*     item. The number of values is returned in N. Copying an item */
/*     leaves the item intact. */

/*     'TAKE' returns all of the values associated with the specified */
/*     item. The number of values is returned in N. Unlike copying, */
/*     taking an item removes the item from the board. */

/*     'POP' takes some of the values associated with the specified */
/*     item. Items are taken from the front of the list; the remaining */
/*     values are left intact. The number of values to be popped is */
/*     specified in N. Popping the final value of an item removes the */
/*     item from the board. */

/* $ Examples */

/*     See BBOARD. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart  (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 5-APR-1989 (DMT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("BBGETI_1", (ftnlen)8);
    }

/*     Compress spaces, change cases, as needed. */

    cmprss_(" ", &c__0, action, what, (ftnlen)1, action_len, (ftnlen)32);
    ucase_(what, what, (ftnlen)32, (ftnlen)32);
    cmprss_(" ", &c__0, item, which, (ftnlen)1, item_len, (ftnlen)32);

/*     The real work is done by the symbol table routines. (Later, */
/*     it will be done by the card catalog routines.) Note that */
/*     items must be popped one at a time. */

    if (s_cmp(what, "COPY", (ftnlen)32, (ftnlen)4) == 0 || s_cmp(what, "TAKE",
	     (ftnlen)32, (ftnlen)4) == 0) {
	sygeti_(which, intab, iptab, ivtab, n, ivals, &fnd, (ftnlen)32, (
		ftnlen)32);
	if (! fnd) {
	    setmsg_("Could not find item #.", (ftnlen)22);
	    errch_("#", which, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(ALLGONE)", (ftnlen)14);
	} else if (s_cmp(what, "TAKE", (ftnlen)32, (ftnlen)4) == 0) {
	    sydeli_(which, intab, iptab, ivtab, (ftnlen)32, (ftnlen)32);
	}
    } else if (s_cmp(what, "POP", (ftnlen)32, (ftnlen)3) == 0) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    sypopi_(which, intab, iptab, ivtab, &ivals[i__ - 1], &fnd, (
		    ftnlen)32, (ftnlen)32);
	}
	if (! fnd) {
	    setmsg_("Could not find item #.", (ftnlen)22);
	    errch_("#", which, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(ALLGONE)", (ftnlen)14);
	}
    } else {
	setmsg_("Sorry, # is not a legal action.", (ftnlen)31);
	errch_("#", what, (ftnlen)1, (ftnlen)32);
	sigerr_("SPICE(UNNATURALACT)", (ftnlen)19);
    }
    chkout_("BBGETI_1", (ftnlen)8);
    return 0;
/* $Procedure BBGETD ( Bulletin board, get, DP ) */

L_bbgetd_1:
/* $ Abstract */

/*     Get one or more values from the board, associated with */
/*     a DP item. */

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

/*     BBOARD */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         ACTION */
/*     CHARACTER*(*)         ITEM */
/*     INTEGER               N */
/*     DOUBLE PRECISION      DVALS */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ACTION     I   Action: 'COPY', 'TAKE', or 'POP'. */
/*     ITEM       I   Item name. */
/*     N         I,O  Number of values returned. */
/*     DVALS      O   Values. */

/* $ Detailed_Input */

/*     ACTION      specifies an action to be taken. Possible actions */
/*                 are 'COPY', 'TAKE', and 'POP'. */

/*     ITEM        is the name of a DP item, which must be */
/*                 on the board. */

/*     N           on input is the number of values to be popped. */


/* $ Detailed_Output */

/*     N           on output is the number of values returned. */

/*     DVALS       are some or all of the values associated with the */
/*                 specified item. */

/* $ Parameters */

/*     See BBOARD. */

/* $ Exceptions */

/*     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)' */
/*        is signalled. */

/*     2) If ITEM is not found, or if the number of values to be popped */
/*        is smaller than the number of values associated with the item, */
/*        the error 'SPICE(ALLGONE)' is signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     'COPY' returns all of the values associated with the specified */
/*     item. The number of values is returned in N. Copying an item */
/*     leaves the item intact. */

/*     'TAKE' returns all of the values associated with the specified */
/*     item. The number of values is returned in N. Unlike copying, */
/*     taking an item removes the item from the board. */

/*     'POP' takes some of the values associated with the specified */
/*     item. Items are taken from the front of the list; the remaining */
/*     values are left intact. The number of values to be popped is */
/*     specified in N. Popping the final value of an item removes the */
/*     item from the board. */

/* $ Examples */

/*     See BBOARD. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart  (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 5-APR-1989 (DMT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("BBGETD_1", (ftnlen)8);
    }

/*     Compress spaces, change cases, as needed. */

    cmprss_(" ", &c__0, action, what, (ftnlen)1, action_len, (ftnlen)32);
    ucase_(what, what, (ftnlen)32, (ftnlen)32);
    cmprss_(" ", &c__0, item, which, (ftnlen)1, item_len, (ftnlen)32);

/*     The real work is done by the symbol table routines. (Later, */
/*     it will be done by the card catalog routines.) Note that */
/*     items must be popped one at a time. */

    if (s_cmp(what, "COPY", (ftnlen)32, (ftnlen)4) == 0 || s_cmp(what, "TAKE",
	     (ftnlen)32, (ftnlen)4) == 0) {
	sygetd_(which, dntab, dptab, dvtab, n, dvals, &fnd, (ftnlen)32, (
		ftnlen)32);
	if (! fnd) {
	    setmsg_("Could not find item #.", (ftnlen)22);
	    errch_("#", which, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(ALLGONE)", (ftnlen)14);
	} else if (s_cmp(what, "TAKE", (ftnlen)32, (ftnlen)4) == 0) {
	    sydeld_(which, dntab, dptab, dvtab, (ftnlen)32, (ftnlen)32);
	}
    } else if (s_cmp(what, "POP", (ftnlen)32, (ftnlen)3) == 0) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    sypopd_(which, dntab, dptab, dvtab, &dvals[i__ - 1], &fnd, (
		    ftnlen)32, (ftnlen)32);
	}
	if (! fnd) {
	    setmsg_("Could not find item #.", (ftnlen)22);
	    errch_("#", which, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(ALLGONE)", (ftnlen)14);
	}
    } else {
	setmsg_("Sorry, # is not a legal action.", (ftnlen)31);
	errch_("#", what, (ftnlen)1, (ftnlen)32);
	sigerr_("SPICE(UNNATURALACT)", (ftnlen)19);
    }
    chkout_("BBGETD_1", (ftnlen)8);
    return 0;
/* $Procedure BBGETC ( Bulletin board, get, character ) */

L_bbgetc_1:
/* $ Abstract */

/*     Get one or more values from the board, associated with */
/*     a character item. */

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

/*     BBOARD */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         ACTION */
/*     CHARACTER*(*)         ITEM */
/*     INTEGER               N */
/*     CHARACTER*(*)         CVALS */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ACTION     I   Action: 'COPY', 'TAKE', or 'POP'. */
/*     ITEM       I   Item name. */
/*     N         I,O  Number of values returned. */
/*     CVALS      O   Values. */

/* $ Detailed_Input */

/*     ACTION      specifies an action to be taken. Possible actions */
/*                 are 'COPY', 'TAKE', and 'POP'. */

/*     ITEM        is the name of a character item, which must be */
/*                 on the board. */

/*     N           on input is the number of values to be popped. */


/* $ Detailed_Output */

/*     N           on output is the number of values returned. */

/*     CVALS       are some or all of the values associated with the */
/*                 specified item. */

/* $ Parameters */

/*     See BBOARD. */

/* $ Exceptions */

/*     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)' */
/*        is signalled. */

/*     2) If ITEM is not found, or if the number of values to be popped */
/*        is smaller than the number of values associated with the item, */
/*        the error 'SPICE(ALLGONE)' is signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     'COPY' returns all of the values associated with the specified */
/*     item. The number of values is returned in N. Copying an item */
/*     leaves the item intact. */

/*     'TAKE' returns all of the values associated with the specified */
/*     item. The number of values is returned in N. Unlike copying, */
/*     taking an item removes the item from the board. */

/*     'POP' takes some of the values associated with the specified */
/*     item. Items are taken from the front of the list; the remaining */
/*     values are left intact. The number of values to be popped is */
/*     specified in N. Popping the final value of an item removes the */
/*     item from the board. */

/* $ Examples */

/*     See BBOARD. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart  (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 5-APR-1989 (DMT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("BBGETC_1", (ftnlen)8);
    }

/*     Compress spaces, change cases, as needed. */

    cmprss_(" ", &c__0, action, what, (ftnlen)1, action_len, (ftnlen)32);
    ucase_(what, what, (ftnlen)32, (ftnlen)32);
    cmprss_(" ", &c__0, item, which, (ftnlen)1, item_len, (ftnlen)32);

/*     The real work is done by the symbol table routines. (Later, */
/*     it will be done by the card catalog routines.) Note that */
/*     items must be popped one at a time. */

    if (s_cmp(what, "COPY", (ftnlen)32, (ftnlen)4) == 0 || s_cmp(what, "TAKE",
	     (ftnlen)32, (ftnlen)4) == 0) {
	sygetc_(which, cntab, cptab, cvtab, n, cvals, &fnd, (ftnlen)32, (
		ftnlen)32, (ftnlen)255, cvals_len);
	if (! fnd) {
	    setmsg_("Could not find item #.", (ftnlen)22);
	    errch_("#", which, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(ALLGONE)", (ftnlen)14);
	} else if (s_cmp(what, "TAKE", (ftnlen)32, (ftnlen)4) == 0) {
	    sydelc_(which, cntab, cptab, cvtab, (ftnlen)32, (ftnlen)32, (
		    ftnlen)255);
	}
    } else if (s_cmp(what, "POP", (ftnlen)32, (ftnlen)3) == 0) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    sypopc_(which, cntab, cptab, cvtab, cvals + (i__ - 1) * cvals_len,
		     &fnd, (ftnlen)32, (ftnlen)32, (ftnlen)255, cvals_len);
	}
	if (! fnd) {
	    setmsg_("Could not find item #.", (ftnlen)22);
	    errch_("#", which, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(ALLGONE)", (ftnlen)14);
	}
    } else {
	setmsg_("Sorry, # is not a legal action.", (ftnlen)31);
	errch_("#", what, (ftnlen)1, (ftnlen)32);
	sigerr_("SPICE(UNNATURALACT)", (ftnlen)19);
    }
    chkout_("BBGETC_1", (ftnlen)8);
    return 0;
/* $Procedure BBGETS ( Bulletin board, get, string ) */

L_bbgets_1:
/* $ Abstract */

/*     Get a value from the board, associated with a string item. */

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

/*     BBOARD */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         ACTION */
/*     CHARACTER*(*)         ITEM */
/*     CHARACTER*(*)         SVAL */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ACTION     I   Action: 'COPY' or 'TAKE'. */
/*     ITEM       I   Item name. */
/*     SVAL       O   Value. */

/* $ Detailed_Input */

/*     ACTION      specifies an action to be taken. Possible actions */
/*                 are 'COPY' and 'TAKE'. */

/*     ITEM        is the name of a string item, which must be */
/*                 on the board. */

/* $ Detailed_Output */

/*     SVAL        is the value associated with the specified item. */

/* $ Parameters */

/*     See BBOARD. */

/* $ Exceptions */

/*     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)' */
/*        is signalled. */

/*     2) If ITEM is not found, the error 'SPICE(ALLGONE)' is signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     'COPY' returns the value associated with the specified item. */
/*     Copying an item leaves the item intact. */

/*     'TAKE' returns the value associated with the specified item. */
/*     Unlike copying, taking an item removes the item from the board. */

/* $ Examples */

/*     See BBOARD. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart  (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 5-APR-1989 (DMT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("BBGETS_1", (ftnlen)8);
    }
    cmprss_(" ", &c__0, action, what, (ftnlen)1, action_len, (ftnlen)32);
    ucase_(what, what, (ftnlen)32, (ftnlen)32);
    cmprss_(" ", &c__0, item, which, (ftnlen)1, item_len, (ftnlen)32);
    if (s_cmp(what, "COPY", (ftnlen)32, (ftnlen)4) == 0 || s_cmp(what, "TAKE",
	     (ftnlen)32, (ftnlen)4) == 0) {
	sbget_1__(which, nbuf, pbuf, vbuf, sval, &pos, (ftnlen)32, (ftnlen)32,
		 (ftnlen)100, sval_len);
	if (pos == 0) {
	    setmsg_("Could not find item #.", (ftnlen)22);
	    errch_("#", which, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(ALLGONE)", (ftnlen)14);
	} else if (s_cmp(what, "TAKE", (ftnlen)32, (ftnlen)4) == 0) {
	    sbrem_1__(which, nbuf, pbuf, vbuf, (ftnlen)32, (ftnlen)32, (
		    ftnlen)100);
	}
    } else {
	setmsg_("Sorry, # is not a legal action.", (ftnlen)31);
	errch_("#", what, (ftnlen)1, (ftnlen)32);
	sigerr_("SPICE(UNNATURALACT)", (ftnlen)19);
    }
    chkout_("BBGETS_1", (ftnlen)8);
    return 0;
/* $Procedure BBREMI ( Bulletin board, remove, integer ) */

L_bbremi_1:
/* $ Abstract */

/*     Remove an integer item from the board. */

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

/*     BBOARD */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         ITEM */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ITEM       I   Item name. */

/* $ Detailed_Input */

/*     ITEM        is the name of an integer item, which may or */
/*                 may not be on the board. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See BBOARD. */

/* $ Exceptions */

/*     1) If ITEM is not recognized, the board is not changed. */
/*        No error occurs. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Items may also be removed by calling BBGETI, using 'TAKE' */
/*     or 'POP'. However, BBREMI allows you to remove an item without */
/*     providing space for its values. Also, it does not cause an */
/*     error if the item is not on the board. */

/* $ Examples */

/*     See BBOARD. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart  (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 5-APR-1989 (DMT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("BBREMI_1", (ftnlen)8);
    }

/*     Compress spaces as needed. */

    cmprss_(" ", &c__0, item, which, (ftnlen)1, item_len, (ftnlen)32);

/*     The real work is done by a symbol table routine. (Later, */
/*     it will be done by a card catalog routine.) */

    sydeli_(which, intab, iptab, ivtab, (ftnlen)32, (ftnlen)32);
    chkout_("BBREMI_1", (ftnlen)8);
    return 0;
/* $Procedure BBREMD ( Bulletin board, remove, DP ) */

L_bbremd_1:
/* $ Abstract */

/*     Remove a DP item from the board. */

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

/*     BBOARD */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         ITEM */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ITEM       I   Item name. */

/* $ Detailed_Input */

/*     ITEM        is the name of a DP item, which may or */
/*                 may not be on the board. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See BBOARD. */

/* $ Exceptions */

/*     1) If ITEM is not recognized, the board is not changed. */
/*        No error occurs. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Items may also be removed by calling BBGETD, using 'TAKE' */
/*     or 'POP'. However, BBREMD allows you to remove an item without */
/*     providing space for its values. Also, it does not cause an */
/*     error if the item is not on the board. */

/* $ Examples */

/*     See BBOARD. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart  (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 5-APR-1989 (DMT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("BBREMD_1", (ftnlen)8);
    }

/*     Compress spaces as needed. */

    cmprss_(" ", &c__0, item, which, (ftnlen)1, item_len, (ftnlen)32);

/*     The real work is done by a symbol table routine. (Later, */
/*     it will be done by a card catalog routine.) */

    sydeld_(which, dntab, dptab, dvtab, (ftnlen)32, (ftnlen)32);
    chkout_("BBREMD_1", (ftnlen)8);
    return 0;
/* $Procedure BBREMC ( Bulletin board, remove, character ) */

L_bbremc_1:
/* $ Abstract */

/*     Remove a character item from the board. */

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

/*     BBOARD */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         ITEM */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ITEM       I   Item name. */

/* $ Detailed_Input */

/*     ITEM        is the name of a character item, which may or */
/*                 may not be on the board. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See BBOARD. */

/* $ Exceptions */

/*     1) If ITEM is not recognized, the board is not changed. */
/*        No error occurs. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Items may also be removed by calling BBGETC, using 'TAKE' */
/*     or 'POP'. However, BBREMC allows you to remove an item without */
/*     providing space for its values. Also, it does not cause an */
/*     error if the item is not on the board. */

/* $ Examples */

/*     See BBOARD. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart  (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 5-APR-1989 (DMT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("BBREMC_1", (ftnlen)8);
    }

/*     Compress spaces as needed. */

    cmprss_(" ", &c__0, item, which, (ftnlen)1, item_len, (ftnlen)32);

/*     The real work is done by a symbol table routine. (Later, */
/*     it will be done by a card catalog routine.) */

    sydelc_(which, cntab, cptab, cvtab, (ftnlen)32, (ftnlen)32, (ftnlen)255);
    chkout_("BBREMC_1", (ftnlen)8);
    return 0;
/* $Procedure BBREMS ( Bulletin board, remove, string ) */

L_bbrems_1:
/* $ Abstract */

/*     Remove a string item from the board. */

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

/*     BBOARD */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         ITEM */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ITEM       I   Item name. */

/* $ Detailed_Input */

/*     ITEM        is the name of a string item, which may or */
/*                 may not be on the board. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See BBOARD. */

/* $ Exceptions */

/*     1) If ITEM is not recognized, the board is not changed. */
/*        No error occurs. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Items may also be removed by calling BBGETS, using 'TAKE'. */
/*     However, BBREMS allows you to remove an item without */
/*     providing space for its value. Also, it does not cause an */
/*     error if the item is not on the board. */

/* $ Examples */

/*     See BBOARD. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart  (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 5-APR-1989 (DMT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("BBREMS_1", (ftnlen)8);
    }
    cmprss_(" ", &c__0, item, which, (ftnlen)1, item_len, (ftnlen)32);
    sbrem_1__(which, nbuf, pbuf, vbuf, (ftnlen)32, (ftnlen)32, (ftnlen)100);
    chkout_("BBREMS_1", (ftnlen)8);
    return 0;
/* $Procedure BBFNDI ( Bulletin board, find, integer ) */

L_bbfndi_1:
/* $ Abstract */

/*     Find an integer item on the board. */

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

/*     BBOARD */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         ITEM */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ITEM       I   Item name. */
/*     N          O   Number of values. */

/* $ Detailed_Input */

/*     ITEM        is the name of an integer item, which may or */
/*                 may not be on the board. */

/* $ Detailed_Output */

/*     N           is the number of values associated with the item. */
/*                 If the item is not on the board, N is zero. */

/* $ Parameters */

/*     See BBOARD. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     BBFNDI has two main uses: */

/*        1) To confirm that an item exists before attempting to */
/*           copy or take its values (anticipating a possible error). */

/*        2) To determine the number of values associated with an */
/*           item, so that the right number of values can be popped */
/*           from the value list. */

/* $ Examples */

/*     See BBOARD. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart  (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 5-APR-1989 (DMT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("BBFNDI_1", (ftnlen)8);
    }

/*     Compress spaces as needed. */

    cmprss_(" ", &c__0, item, which, (ftnlen)1, item_len, (ftnlen)32);

/*     The real work is done by a symbol table routine. (Later, */
/*     it will be done by a card catalog routine.) */

    *n = sydimi_(which, intab, iptab, ivtab, (ftnlen)32, (ftnlen)32);
    chkout_("BBFNDI_1", (ftnlen)8);
    return 0;
/* $Procedure BBFNDD ( Bulletin board, find, DP ) */

L_bbfndd_1:
/* $ Abstract */

/*     Find a DP item on the board. */

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

/*     BBOARD */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         ITEM */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ITEM       I   Item name. */
/*     N          O   Number of values. */

/* $ Detailed_Input */

/*     ITEM        is the name of a DP item, which may or */
/*                 may not be on the board. */

/* $ Detailed_Output */

/*     N           is the number of values associated with the item. */
/*                 If the item is not on the board, N is zero. */

/* $ Parameters */

/*     See BBOARD. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     BBFNDD has two main uses: */

/*        1) To confirm that an item exists before attempting to */
/*           copy or take its values (anticipating a possible error). */

/*        2) To determine the number of values associated with an */
/*           item, so that the right number of values can be popped */
/*           from the value list. */

/* $ Examples */

/*     See BBOARD. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart  (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 5-APR-1989 (DMT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("BBFNDD_1", (ftnlen)8);
    }

/*     Compress spaces as needed. */

    cmprss_(" ", &c__0, item, which, (ftnlen)1, item_len, (ftnlen)32);

/*     The real work is done by a symbol table routine. (Later, */
/*     it will be done by a card catalog routine.) */

    *n = sydimd_(which, dntab, dptab, dvtab, (ftnlen)32, (ftnlen)32);
    chkout_("BBFNDD_1", (ftnlen)8);
    return 0;
/* $Procedure BBFNDC ( Bulletin board, find, character ) */

L_bbfndc_1:
/* $ Abstract */

/*     Find a character item on the board. */

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

/*     BBOARD */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         ITEM */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ITEM       I   Item name. */
/*     N          O   Number of values. */

/* $ Detailed_Input */

/*     ITEM        is the name of a character item, which may or */
/*                 may not be on the board. */

/* $ Detailed_Output */

/*     N           is the number of values associated with the item. */
/*                 If the item is not on the board, N is zero. */

/* $ Parameters */

/*     See BBOARD. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     BBFNDC has two main uses: */

/*        1) To confirm that an item exists before attempting to */
/*           copy or take its values (anticipating a possible error). */

/*        2) To determine the number of values associated with an */
/*           item, so that the right number of values can be popped */
/*           from the value list. */

/* $ Examples */

/*     See BBOARD. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart  (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 5-APR-1989 (DMT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("BBFNDC_1", (ftnlen)8);
    }

/*     Compress spaces as needed. */

    cmprss_(" ", &c__0, item, which, (ftnlen)1, item_len, (ftnlen)32);

/*     The real work is done by a symbol table routine. (Later, */
/*     it will be done by a card catalog routine.) */

    *n = sydimc_(which, cntab, cptab, cvtab, (ftnlen)32, (ftnlen)32, (ftnlen)
	    255);
    chkout_("BBFNDC_1", (ftnlen)8);
    return 0;
/* $Procedure BBFNDS ( Bulletin board, find, string ) */

L_bbfnds_1:
/* $ Abstract */

/*     Find a string item on the board. */

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

/*     BBOARD */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         ITEM */
/*     INTEGER               N */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ITEM       I   Item name. */
/*     N          O   Number of values. */

/* $ Detailed_Input */

/*     ITEM        is the name of a string item, which may or */
/*                 may not be on the board. */

/* $ Detailed_Output */

/*     N           is the number of values associated with the item. */
/*                 If the item is on the board, N is one. Otherwise */
/*                 N is zero. */

/* $ Parameters */

/*     See BBOARD. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     BBFNDS is used mainly to confirm that an item exists before */
/*     attempting to copy or take its value (anticipating a possible */
/*     error). */

/* $ Examples */

/*     See BBOARD. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart  (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 5-APR-1989 (DMT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("BBFNDS_1", (ftnlen)8);
    }
    cmprss_(" ", &c__0, item, which, (ftnlen)1, item_len, (ftnlen)32);
    sbget_1__(which, nbuf, pbuf, vbuf, what, &pos, (ftnlen)32, (ftnlen)32, (
	    ftnlen)100, (ftnlen)32);
    if (pos > 0) {
	*n = 1;
    } else {
	*n = 0;
    }
    chkout_("BBFNDS_1", (ftnlen)8);
    return 0;
/* $Procedure BBCLR ( Bulletin board, clear ) */

L_bbclr_1:
/* $ Abstract */

/*     Clear the entire board. */

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

/*     BBOARD */

/* $ Keywords */

/*     UTILITY */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See BBOARD. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     BBCLR clears the entire bulletin board. The board MUST be */
/*     cleared at least once before any items can be posted. */
/*     This is usually done by the main module of the calling */
/*     program, during program initialization. */

/* $ Examples */

/*     See BBOARD. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart  (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 5-APR-1989 (DMT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("BBCLR_1", (ftnlen)7);
    }

/*     Wipe out all three symbol tables. */

    ssizec_(&c__100, intab, (ftnlen)32);
    ssizei_(&c__100, iptab);
    ssizei_(&c__5000, ivtab);
    ssizec_(&c__100, dntab, (ftnlen)32);
    ssizei_(&c__100, dptab);
    ssized_(&c__5000, dvtab);
    ssizec_(&c__100, cntab, (ftnlen)32);
    ssizei_(&c__100, cptab);
    ssizec_(&c__300, cvtab, (ftnlen)255);

/*     Re-initialize the string buffer. */

    sbinit_1__(&c__100, &c__404, &c__50, nbuf, pbuf, vbuf, (ftnlen)32, (
	    ftnlen)100);
    chkout_("BBCLR_1", (ftnlen)7);
    return 0;
} /* bboard_ */

/* Subroutine */ int bboard_(char *action, char *item, integer *n, integer *
	ivals, doublereal *dvals, char *cvals, char *sval, ftnlen action_len, 
	ftnlen item_len, ftnlen cvals_len, ftnlen sval_len)
{
    return bboard_0_(0, action, item, n, ivals, dvals, cvals, sval, 
	    action_len, item_len, cvals_len, sval_len);
    }

/* Subroutine */ int bbputi_1__(char *action, char *item, integer *n, integer 
	*ivals, ftnlen action_len, ftnlen item_len)
{
    return bboard_0_(1, action, item, n, ivals, (doublereal *)0, (char *)0, (
	    char *)0, action_len, item_len, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int bbputd_1__(char *action, char *item, integer *n, 
	doublereal *dvals, ftnlen action_len, ftnlen item_len)
{
    return bboard_0_(2, action, item, n, (integer *)0, dvals, (char *)0, (
	    char *)0, action_len, item_len, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int bbputc_1__(char *action, char *item, integer *n, char *
	cvals, ftnlen action_len, ftnlen item_len, ftnlen cvals_len)
{
    return bboard_0_(3, action, item, n, (integer *)0, (doublereal *)0, cvals,
	     (char *)0, action_len, item_len, cvals_len, (ftnint)0);
    }

/* Subroutine */ int bbputs_1__(char *action, char *item, char *sval, ftnlen 
	action_len, ftnlen item_len, ftnlen sval_len)
{
    return bboard_0_(4, action, item, (integer *)0, (integer *)0, (doublereal 
	    *)0, (char *)0, sval, action_len, item_len, (ftnint)0, sval_len);
    }

/* Subroutine */ int bbgeti_1__(char *action, char *item, integer *n, integer 
	*ivals, ftnlen action_len, ftnlen item_len)
{
    return bboard_0_(5, action, item, n, ivals, (doublereal *)0, (char *)0, (
	    char *)0, action_len, item_len, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int bbgetd_1__(char *action, char *item, integer *n, 
	doublereal *dvals, ftnlen action_len, ftnlen item_len)
{
    return bboard_0_(6, action, item, n, (integer *)0, dvals, (char *)0, (
	    char *)0, action_len, item_len, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int bbgetc_1__(char *action, char *item, integer *n, char *
	cvals, ftnlen action_len, ftnlen item_len, ftnlen cvals_len)
{
    return bboard_0_(7, action, item, n, (integer *)0, (doublereal *)0, cvals,
	     (char *)0, action_len, item_len, cvals_len, (ftnint)0);
    }

/* Subroutine */ int bbgets_1__(char *action, char *item, char *sval, ftnlen 
	action_len, ftnlen item_len, ftnlen sval_len)
{
    return bboard_0_(8, action, item, (integer *)0, (integer *)0, (doublereal 
	    *)0, (char *)0, sval, action_len, item_len, (ftnint)0, sval_len);
    }

/* Subroutine */ int bbremi_1__(char *item, ftnlen item_len)
{
    return bboard_0_(9, (char *)0, item, (integer *)0, (integer *)0, (
	    doublereal *)0, (char *)0, (char *)0, (ftnint)0, item_len, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int bbremd_1__(char *item, ftnlen item_len)
{
    return bboard_0_(10, (char *)0, item, (integer *)0, (integer *)0, (
	    doublereal *)0, (char *)0, (char *)0, (ftnint)0, item_len, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int bbremc_1__(char *item, ftnlen item_len)
{
    return bboard_0_(11, (char *)0, item, (integer *)0, (integer *)0, (
	    doublereal *)0, (char *)0, (char *)0, (ftnint)0, item_len, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int bbrems_1__(char *item, ftnlen item_len)
{
    return bboard_0_(12, (char *)0, item, (integer *)0, (integer *)0, (
	    doublereal *)0, (char *)0, (char *)0, (ftnint)0, item_len, (
	    ftnint)0, (ftnint)0);
    }

/* Subroutine */ int bbfndi_1__(char *item, integer *n, ftnlen item_len)
{
    return bboard_0_(13, (char *)0, item, n, (integer *)0, (doublereal *)0, (
	    char *)0, (char *)0, (ftnint)0, item_len, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int bbfndd_1__(char *item, integer *n, ftnlen item_len)
{
    return bboard_0_(14, (char *)0, item, n, (integer *)0, (doublereal *)0, (
	    char *)0, (char *)0, (ftnint)0, item_len, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int bbfndc_1__(char *item, integer *n, ftnlen item_len)
{
    return bboard_0_(15, (char *)0, item, n, (integer *)0, (doublereal *)0, (
	    char *)0, (char *)0, (ftnint)0, item_len, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int bbfnds_1__(char *item, integer *n, ftnlen item_len)
{
    return bboard_0_(16, (char *)0, item, n, (integer *)0, (doublereal *)0, (
	    char *)0, (char *)0, (ftnint)0, item_len, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int bbclr_1__(void)
{
    return bboard_0_(17, (char *)0, (char *)0, (integer *)0, (integer *)0, (
	    doublereal *)0, (char *)0, (char *)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0);
    }

