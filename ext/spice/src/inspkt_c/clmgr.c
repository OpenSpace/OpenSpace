/* clmgr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__400 = 400;
static integer c__8000 = 8000;
static integer c__1 = 1;
static integer c__0 = 0;
static integer c__8 = 8;
static integer c__40 = 40;
static integer c__2 = 2;
static integer c__4 = 4;
static integer c__10 = 10;

/* Subroutine */ int clmgr_0_(int n__, char *name__, integer *id, integer *
	num, integer *handle, logical *null, logical *indx, integer *ctype, 
	integer *csize, char *attr, integer *clen, integer *iattr, char *
	cattr, logical *found, char *cols, char *query, char *error, integer *
	wdth, char *corctn, integer *widest, ftnlen name_len, ftnlen attr_len,
	 ftnlen cattr_len, ftnlen cols_len, ftnlen query_len, ftnlen 
	error_len, ftnlen corctn_len)
{
    /* Initialized data */

    static logical aquery = FALSE_;
    static logical first = TRUE_;
    static logical unprep = TRUE_;
    static char pvalue[64*4] = "CHARACTER*(#)                               "
	    "                    " "DOUBLE PRECISION                         "
	    "                       " "INTEGER                               "
	    "                          " "TIME                               "
	    "                             ";

    /* System generated locals */
    address a__1[2], a__2[4];
    integer i__1, i__2, i__3, i__4, i__5[2], i__6[4];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen), i_indx(char *, char *, ftnlen, ftnlen), i_len(
	    char *, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int ekgc_(integer *, integer *, integer *, char *,
	     logical *, logical *, ftnlen), ekgd_(integer *, integer *, 
	    integer *, doublereal *, logical *, logical *);
    static integer qbeg[400], high;
    extern /* Subroutine */ int ekgi_(integer *, integer *, integer *, 
	    integer *, logical *, logical *), scan_(char *, char *, integer *,
	     integer *, integer *, integer *, integer *, integer *, integer *,
	     integer *, ftnlen, ftnlen);
    static integer keep[1], qend[400], best[16];
    static char nams[64*406], qcol[64], item[64];
    static integer myid;
    static char fmts[128*400], tfmt[128];
    static integer ptrs[406];
    extern /* Subroutine */ int zzekencd_(char *, integer *, char *, 
	    doublereal *, logical *, char *, integer *, ftnlen, ftnlen, 
	    ftnlen), zzekpcol_(char *, integer *, char *, char *, char *, 
	    integer *, char *, integer *, logical *, char *, ftnlen, ftnlen, 
	    ftnlen, ftnlen, ftnlen, ftnlen);
    static integer b, e, i__, j, l, n;
    extern integer cardi_(integer *);
    static char table[64];
    static doublereal x;
    static integer w;
    static logical avail[400];
    static char lname[64];
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen);
    static char rname[6], names[64*400];
    static integer hndls[8006], scope;
    static char eqryc[2000];
    static doublereal eqryd[100];
    static char marks[4*1];
    static integer eqryi[27875];
    static char known[64*406];
    extern integer rtrim_(char *, ftnlen);
    static integer sizes[400], ident[400];
    static char mycol[128];
    static integer nqids, qcids[400], types[400], count, start;
    static char justs[8*400];
    static logical gotit;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), repmc_(char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen), errch_(char *, char *, ftnlen, 
	    ftnlen);
    static char mystr[1024];
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen), 
	    fmtdp_(doublereal *, char *, char *, ftnlen, ftnlen);
    static integer nb;
    extern logical failed_(void);
    static integer te;
    static logical ok;
    static char cnames[64*400];
    extern integer brckti_(integer *, integer *, integer *), isrchi_(integer *
	    , integer *, integer *);
    extern logical return_(void);
    static char aliass[64*400], tables[64*400];
    static integer nactiv, lenths[400], widths[400];
    static logical active[400], indexd[400], nullok[400];
    static integer amount[400];
    static char errmsg[1024];
    static integer errptr, currnt, cutoff, scores[16], highst, nmarks, mrklen[
	    1], pnters[5], ntokns;
    static char myalis[64], qalias[64*400], qcolmn[64*400], talias[64];
    static integer colidx, qtindx[400], tabidx, han;
    static logical badqry, gotone, gotfre, nulval;
    extern /* Subroutine */ int ssizec_(integer *, char *, ftnlen), ssizei_(
	    integer *, integer *), setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), intstr_(integer *, char *, ftnlen), nspcht_(char *, 
	    integer *, ftnlen), bbgetc_1__(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), syptri_(char *, char *, integer *, 
	    integer *, integer *, integer *, logical *, ftnlen, ftnlen), 
	    suffix_(char *, integer *, char *, ftnlen, ftnlen), sygeti_(char *
	    , char *, integer *, integer *, integer *, integer *, logical *, 
	    ftnlen, ftnlen), sypshi_(char *, integer *, char *, integer *, 
	    integer *, ftnlen, ftnlen), syputi_(char *, integer *, integer *, 
	    char *, integer *, integer *, ftnlen, ftnlen), sytrni_(char *, 
	    integer *, integer *, char *, integer *, integer *, ftnlen, 
	    ftnlen), sypopi_(char *, char *, integer *, integer *, integer *, 
	    logical *, ftnlen, ftnlen), prefix_(char *, integer *, char *, 
	    ftnlen, ftnlen), scanpr_(integer *, char *, integer *, integer *, 
	    ftnlen), scansl_(integer *, integer *, integer *, integer *, 
	    integer *, integer *), fndnwd_(char *, integer *, integer *, 
	    integer *, ftnlen), eksrch_(integer *, char *, doublereal *, 
	    integer *, logical *, char *, ftnlen, ftnlen), eknelt_(integer *, 
	    integer *, integer *), fmtint_(integer *, char *, char *, ftnlen, 
	    ftnlen), fmttim_(doublereal *, char *, char *, ftnlen, ftnlen), 
	    appndc_(char *, char *, ftnlen, ftnlen), bestwd_(char *, char *, 
	    integer *, integer *, integer *, char *, ftnlen, ftnlen, ftnlen);


/*     This routine manages the attributes and evaluation of columns. */
/*     It acts as an umbrella over a collection of entry points that */
/*     allow programs to create, remove, modify attributes of and */
/*     fetch the value of a column associated with a particular */
/*     index associated with a query.  Some assumptions are implicit */
/*     in the design of this routine.  For example it is assumed */
/*     that only legitimate values are PUT to the "setting" */
/*     entry points of this routine. */

/* -    Version 7.1  10-APR-2000 */

/*       Declared SCAN to be external */

/* -    Version 7.0  16-NOV-1995 */

/*       We replaced the definition of IMIN to point to the */
/*       value EQIMIN that is included via the file ekquery.inc */

/* -    Version 6.0    16-OCT-1995 */

/*     When a new column is supplied (via CLNEW) but the column already */
/*     exists, we now change the width only if the column is of type */
/*     character. */

/* -    Version 5.0    7-SEP-1995 */

/*     A 'default' Integer and D.P. format were added to the formats */

/*     The behaviour is the same as the behaviour for TIME columns. */

/* -    Version 4.0   21-AUG-1995  Editorial Comment. */

/*     The concept that setting time format only affected new columns */
/*     as they were loaded proved to be too difficult for the poor */
/*     computer challenged user. */

/*     Now, when a new column is loaded and is of type TIME, the value */
/*     of the format set is DEFAULT.  The width is set to -1.  The actual */
/*     width is determined by calling NSPCHT to construct the width */
/*     and truncates this to 40 at max. */

/*     If specific calls are made to set the format to something other */
/*     than DEFAULT, the width is set at that time to a positive */
/*     value and that width shall be used until the user sets the */
/*     width to some other value or changes the format again. */

/*     If the value of FMTS(ID) = 'DEFAULT' for a time column, the */
/*     program looks up the actual format when it is called for */
/*     the called for locations are CLGAC and CLPVAL.  Also when */
/*     width is requested in CLGAC, CLGAI and CLPVAL. */


/* -    Version 3.0   3-Aug-1995  The interface to the EK library */
/*     changed and NJB modified this routine to reflect that */
/*     change. */

/* -    Version 2.0.  Fixed the problem with an unitialized variable */
/*     D.  D is no longer used. */


/*     Include files for parameters used in the declarations below: */

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


/*     Include Section:  EK Encoded Query Internal Parameters */

/*        ekquery.inc  Version 3    16-NOV-1995 (NJB) */

/*           Updated to reflect increased value of MAXCON in */
/*           ekqlimit.inc. */

/*        ekquery.inc  Version 2    03-AUG-1995 (NJB) */

/*           Updated to support representation of the SELECT clause. */


/*        ekquery.inc  Version 1    12-JAN-1995 (NJB) */


/*     An encoded EK query is an abstract data type implemented */
/*     as an integer cell, along with a double precision cell and */
/*     a character string.  The d.p. cell and string contain numeric */
/*     and string values from the query string represented by the */
/*     encoded query. */

/*     The parameters in this file are intended for use only by the */
/*     EK encoded query access routines.  Callers of EK routines should */
/*     not use these parameters. */

/*     The following parameters are indices of specified elements */
/*     in the integer portion of the encoded query. */

/*     Encoded query architecture type: */


/*     `Name resolution' consists of: */

/*        - Verifying existence of tables:  any table names listed */
/*          in the FROM clause of a query must be loaded. */

/*        - Validating table aliases used to qualify column names. */

/*        - Verifying existence of columns and obtaining data types */
/*          for columns. */

/*        - Setting data type codes for literal values in the encoded */
/*          query. */

/*        - Checking consistency of operators and operand data types. */

/*        - Making sure unqualified column names are unambiguous. */

/*        - For constraints, mapping the table names used to qualify */
/*          column names to the ordinal position in the FROM clause */
/*          of the corresponding table. */


/*     Initialization status---this flag indicates whether the encoded */
/*     query has been initialized.  Values are ITRUE or IFALSE.  See the */
/*     include file ekbool.inc for parameter values. */


/*     Parse status---this flag indicates whether the parsing operation */
/*     that produced an encoded query has been completed. Values are */
/*     ITRUE or IFALSE. */


/*     Name resolution status---this flag indicates whether names */
/*     have been resolved in an encoded query.  Values are ITRUE or */
/*     IFALSE. */


/*     Time resolution status---this flag indicates whether time values */
/*     have been resolved in an encoded query.  Time resolution */
/*     consists of converting strings representing time values to ET. */
/*     Values of the status are ITRUE or IFALSE. */


/*     Semantic check status---this flag indicates whether semantic */
/*     checking of constraints has been performed. */


/*     Number of tables specified in FROM clause: */


/*     Number of constraints in query: */


/*     A special value is used to indicate the `maximal' constraint--- */
/*     one that logically cannot be satisfied.  If the constraints */
/*     are equivalent to the maximal constraint, the location EQNCNS */
/*     is assigned the value EQMXML */


/*     Number of constraint conjunctions: */


/*     Number of order-by columns: */


/*     Number of SELECT columns: */


/*     Size of double precision buffer: */


/*     `Free' pointer into double precision buffer: */


/*     Size of character string buffer: */


/*     `Free' pointer into character string buffer: */


/*     The following four base pointers will be valid after a query */
/*     has been parsed: */

/*     Base pointer for SELECT column descriptors: */


/*     Base pointer for constraint descriptors: */


/*     Base pointer for conjunction sizes: */


/*     Base pointer for order-by column descriptors: */


/*     After the quantities named above, the integer array contains */
/*     series of descriptors for tables, constraints, and order-by */
/*     columns, as well as a list of `conjunction sizes'---that is, */
/*     the sizes of the groups of constraints that form conjunctions, */
/*     after the input query has been re-arranged as a disjunction of */
/*     conjunctions of constraints. */


/*     The offsets of specific elements within descriptors are */
/*     parameterized. The base addresses of the descriptors themselves */
/*     must be  calculated using the counts and sizes of the items */
/*     preceding them. */

/*     A diagram of the structure of the variable-size portion of the */
/*     integer array is shown below: */


/*        +-------------------------------------+ */
/*        | Fixed-size portion of encoded query | */
/*        +-------------------------------------+ */
/*        |         Encoded FROM clause         | */
/*        +-------------------------------------+ */
/*        |      Encoded constraint clause      | */
/*        +-------------------------------------+ */
/*        |          Conjunction sizes          | */
/*        +-------------------------------------+ */
/*        |       Encoded ORDER BY clause       | */
/*        +-------------------------------------+ */
/*        |        Encoded SELECT clause        | */
/*        +-------------------------------------+ */


/*     Value Descriptors */
/*     ---------------- */

/*     In order to discuss the various descriptors below, we'll make use */
/*     of sub-structures called `value descriptors'.  These descriptors */
/*     come in two flavors:  character and double precision.  For */
/*     strings, a descriptor is a set of begin and end pointers that */
/*     indicate the location of the string in the character portion of an */
/*     encoded query, along with the begin and end pointers for the */
/*     corresponding lexeme in the original query.  The pointers are set */
/*     to zero when they are not in use, for example if they refer to an */
/*     optional lexeme that did not appear in the input query. */

/*     All value descriptors start with a data type indicator; values */
/*     are from ektype.inc.  Integer and time values are referred to */
/*     by double precision descriptors. */

/*     Parameters for string value descriptor elements: */


/*     Numeric value descriptors are similar to those for string values, */
/*     the difference being that they have only one pointer to the value */
/*     they represent.  This pointer is the index of the value in the */
/*     encoded query's numeric buffer. */


/*     All value descriptors have the same size.  In order to allow */
/*     table descriptors to have the same size as value descriptors, */
/*     we include an extra element in the descriptor. */


/*     Column Descriptors */
/*     ----------------- */

/*     Each column descriptor consists of a character descriptor for the */
/*     name of the column, followed by an index, which gives the ordinal */
/*     position of the column in the logical table to which the column */
/*     belongs.  The index element is filled in during name resolution. */


/*     Table Descriptors */
/*     ----------------- */

/*     Each table descriptor consists of a character descriptor for the */
/*     name of the table, followed by an index, which gives the ordinal */
/*     position of the table in the FROM clause in the original query. */
/*     The index element is filled in during name resolution.  Aliases */
/*     and table names have identical descriptor structures. */


/*     Constraint descriptors */
/*     ------------------ */

/*     Each constraint is characterized by: */

/*        - A code indicating whether the constraint compares values */
/*          in two columns or the value in a column and a literal */
/*          value.  The values of this element are EQCOL and EQVAL. */



/*        - A descriptor for the table used to qualify the */
/*          column name on the left side of the constraint. */


/*        - A character value descriptor for the column name on the left */
/*          side of the query. */


/*        - An operator code indicating the relational operator used */
/*          in the constraint. */


/*        If the constraint compares values from two columns, the */
/*        next items are table and column name descriptors that apply to */
/*        the column named on the right side of the relational operator. */


/*        If the constraint has a literal value on the right side, the */
/*        operator code is followed by... */

/*        - a value descriptor. */


/*        - Size of a constraint descriptor: */


/*     Conjunction sizes */
/*     ----------------- */

/*     The size of each conjunction of constraints occupies a single */
/*     integer. */




/*     Order-by Column Descriptors */
/*     --------------------------- */

/*     Each order-by column descriptor contains descriptors for */
/*     the table containing the column and for the name of the column */
/*     itself; one additional element is used to indicate the direction */
/*     of the ordering (ascending vs descending). */


/*        - The last integer in the descriptor indicates whether the */
/*          order direction is ascending or descending. */


/*        - Size of an order-by column descriptor: */


/*     Codes indicating sense of ordering (ascending vs descending): */


/*     SELECT Column Descriptors */
/*     --------------------------- */

/*     Each SELECT column descriptor contains descriptors for */
/*     the table containing the column and for the name of the column */
/*     itself. */


/*        - Size of a SELECT column descriptor: */


/*     Miscellaneous parameters: */


/*     EQIMIN is the minimum size of the integer portion of */
/*     an encoded query.  EQIMIN depends on the parameters */

/*        MAXTAB */
/*        MAXCON */
/*        MAXORD */
/*        MAXSEL */

/*     all of which are declared in the include file ekqlimit.inc. */
/*     The functional definition of EQIMIN is: */

/*     INTEGER               EQIMIN */
/*     PARAMETER           ( EQIMIN =   EQVBAS */
/*    .                              +  MAXTAB * EQVDSZ * 2 */
/*    .                              +  MAXCON * EQCDSZ */
/*    .                              +  MAXCON */
/*    .                              +  MAXORD * EQODSZ */
/*    .                              +  MAXSEL * EQSDSZ     ) */


/*     End Include Section:  EK Encoded Query Internal Parameters */

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


/*     Include Section:  EK Query Limit Parameters */

/*        ekqlimit.inc  Version 3    16-NOV-1995 (NJB) */

/*           Parameter MAXCON increased to 1000. */

/*        ekqlimit.inc  Version 2    01-AUG-1995 (NJB) */

/*           Updated to support SELECT clause. */


/*        ekqlimit.inc  Version 1    07-FEB-1995 (NJB) */


/*     These limits apply to character string queries input to the */
/*     EK scanner.  This limits are part of the EK system's user */
/*     interface:  the values should be advertised in the EK required */
/*     reading document. */


/*     Maximum length of an input query:  MAXQRY.  This value is */
/*     currently set to twenty-five 80-character lines. */


/*     Maximum number of columns that may be listed in the */
/*     `order-by clause' of a query:  MAXSEL.  MAXSEL = 50. */


/*     Maximum number of tables that may be listed in the `FROM */
/*     clause' of a query: MAXTAB. */


/*     Maximum number of relational expressions that may be listed */
/*     in the `constraint clause' of a query: MAXCON. */

/*     This limit applies to a query when it is represented in */
/*     `normalized form': that is, the constraints have been */
/*     expressed as a disjunction of conjunctions of relational */
/*     expressions. The number of relational expressions in a query */
/*     that has been expanded in this fashion may be greater than */
/*     the number of relations in the query as orginally written. */
/*     For example, the expression */

/*             ( ( A LT 1 ) OR ( B GT 2 ) ) */
/*        AND */
/*             ( ( C NE 3 ) OR ( D EQ 4 ) ) */

/*     which contains 4 relational expressions, expands to the */
/*     equivalent normalized constraint */

/*             (  ( A LT 1 ) AND ( C NE 3 )  ) */
/*        OR */
/*             (  ( A LT 1 ) AND ( D EQ 4 )  ) */
/*        OR */
/*             (  ( B GT 2 ) AND ( C NE 3 )  ) */
/*        OR */
/*             (  ( B GT 2 ) AND ( D EQ 4 )  ) */

/*     which contains eight relational expressions. */



/*     MXJOIN is the maximum number of tables that can be joined. */


/*     MXJCON is the maximum number of join constraints allowed. */


/*     Maximum number of order-by columns that may be used in the */
/*     `order-by clause' of a query: MAXORD. MAXORD = 10. */


/*     Maximum number of tokens in a query: 500. Tokens are reserved */
/*     words, column names, parentheses, and values. Literal strings */
/*     and time values count as single tokens. */


/*     Maximum number of numeric tokens in a query: */


/*     Maximum total length of character tokens in a query: */


/*     Maximum length of literal string values allowed in queries: */
/*     MAXSTR. */


/*     End Include Section:  EK Query Limit Parameters */


/*     Spicelib functions */


/*     Routine name */


/*     Parameters */


/*     Parameters that are used to define the sizes of the attributes */
/*     and number of columns. */


/*     Attributes that are associated with each column */


/*     The variable AMOUNT contains the number of components associated */
/*     with a column at the "current" row of a query. */


/*     The variable AQUERY contains a logical that contains the truth */
/*     value of the assertion "there is an active query". */


/*     We shall need local storage to hold the fetch from a string */
/*     data type.  The maximum such string that can be in an */
/*     E-kernel is 1024 characters long. */


/*     These are the names of attributes that can be set or */
/*     fetched. */


/*     If a column has a variable number of components the size */
/*     stored for it will be -1.  We use a parameter to keep */
/*     track of this. */


/*     Impossible handles and segment numbers are 0. */


/*     The following are the allowed types for the columns */


/*     Print values for null and unavailable columns */


/*     The next set of parameters are the default formats and widths */
/*     for columns. */


/*     Next we have the print values that shall be used for the */
/*     various types of columns. */


/*     The variables below are shared between the entry points */
/*     CLSCOP and CLPVAL to keep track of the number of rows that */
/*     have been examined as the result of a query. */

/*     CURRNT is the number of the matching row that is currently */
/*            being examined */

/*     LASTRD is the row number where the EK-reader begins pointing as */
/*            the result of a processed valid query (this could be either */
/*            zero or one depending upon how the routine EKFIND is */
/*            implemented.  Right now its zero,  the EK-reader */
/*            isn't pointing at any matching row as the result of */
/*            processing a valid query. */


/*     SCOPE  is the total number of rows that fit a query condition. */


/*     The following variable is used to look up the default time */
/*     format stored in the bulletin board. */


/*     The variables below allow CLSPEL to diagnose correct spellings */
/*     for mispelled column names. */


/*     Declarations for interfacing with the query parser and */
/*     EKSRCH   and EKG* are given below. */


/*     This parameter is the minimum size of the integer portion of */
/*     an encoded query: */


/*     The following cell, string and array make comprise the data */
/*     structure used to represent encoded queries: */


/*     The items below are used for taking apart the list of columns */
/*     supplied to CLSCOP. */


/*     Storage for query relative items. */


/*     Initialization control variables. */


/*     Local Variables */


/*     qqqqqqqq */

/*     INTEGER               D */

/*     Initial Values */

    /* Parameter adjustments */
    if (iattr) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_clgac;
	case 2: goto L_clgai;
	case 3: goto L_clnew;
	case 4: goto L_clnid;
	case 5: goto L_clnum;
	case 6: goto L_cln2id;
	case 7: goto L_clpac;
	case 8: goto L_clpai;
	case 9: goto L_clunld;
	case 10: goto L_clscop;
	case 11: goto L_cladv;
	case 12: goto L_clsrow;
	case 13: goto L_clncmp;
	case 14: goto L_clpval;
	case 15: goto L_clspel;
	case 16: goto L_clgqal;
	case 17: goto L_clq2id;
	}

    return 0;
/* $Procedure CLGAC ( Column, Get attribute character. ) */

L_clgac:
/* $ Abstract */

/*     Fetch the character attributes of the column with the specified */
/*     ID code. */

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

/* -& */
    if (return_()) {
	return 0;
    }
    s_copy(rname, "CLGAC", (ftnlen)6, (ftnlen)5);
    chkin_(rname, (ftnlen)6);
    if (first) {
	first = FALSE_;
	ssizec_(&c__400, nams, (ftnlen)64);
	ssizei_(&c__400, ptrs);
	ssizei_(&c__8000, hndls);
	nactiv = 0;
	for (i__ = 1; i__ <= 400; ++i__) {
	    active[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("acti"
		    "ve", i__1, "clmgr_", (ftnlen)534)] = FALSE_;
	}
    }
    if (*id < 1 || *id > 400) {
	setmsg_("The ID, #, used to specified a column is out of bounds. To "
		"be considered as an active column, ID must be between 1 and "
		"# inclusive.", (ftnlen)131);
	errint_("#", id, (ftnlen)1);
	errint_("#", &c__400, (ftnlen)1);
	sigerr_("INSPEKT(BADCOLUMNID)", (ftnlen)20);
	chkout_(rname, (ftnlen)6);
	return 0;
    }
    if (! active[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("active",
	     i__1, "clmgr_", (ftnlen)555)]) {
	setmsg_("The column specified by the ID, #, is not active now.", (
		ftnlen)53);
	errint_("#", id, (ftnlen)1);
	sigerr_("INSPEKT(INACTIVEID)", (ftnlen)19);
	chkout_(rname, (ftnlen)6);
	return 0;
    }

/*     See which attribute is requested and load the value into */
/*     the output string. */

    if (s_cmp(attr, "FORMAT", attr_len, (ftnlen)6) == 0) {
	if (s_cmp(fmts + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		s_rnge("fmts", i__1, "clmgr_", (ftnlen)572)) << 7), "DEFAULT",
		 (ftnlen)128, (ftnlen)7) == 0) {
	    if (types[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
		    "types", i__1, "clmgr_", (ftnlen)574)] == 4) {
		s_copy(item, "TIMEFMT", (ftnlen)64, (ftnlen)7);
	    } else if (types[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("types", i__1, "clmgr_", (ftnlen)576)] == 3) {
		s_copy(item, "INTFMT", (ftnlen)64, (ftnlen)6);
	    } else if (types[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("types", i__1, "clmgr_", (ftnlen)578)] == 2) {
		s_copy(item, "DPFMT", (ftnlen)64, (ftnlen)5);
	    } else if (types[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("types", i__1, "clmgr_", (ftnlen)580)] == 1) {
		s_copy(item, "CHFMT", (ftnlen)64, (ftnlen)5);
	    }
	    bbgetc_1__("COPY", item, &n, cattr, (ftnlen)4, (ftnlen)64, 
		    cattr_len);
	} else {
	    s_copy(cattr, fmts + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 
		    : s_rnge("fmts", i__1, "clmgr_", (ftnlen)587)) << 7), 
		    cattr_len, (ftnlen)128);
	}
    } else if (s_cmp(attr, "TYPE", attr_len, (ftnlen)4) == 0) {
	if (lenths[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("lent"
		"hs", i__1, "clmgr_", (ftnlen)592)] > 0) {
	    repmi_(pvalue + (((i__2 = types[(i__1 = *id - 1) < 400 && 0 <= 
		    i__1 ? i__1 : s_rnge("types", i__1, "clmgr_", (ftnlen)593)
		    ] - 1) < 4 && 0 <= i__2 ? i__2 : s_rnge("pvalue", i__2, 
		    "clmgr_", (ftnlen)593)) << 6), "#", &lenths[(i__3 = *id - 
		    1) < 400 && 0 <= i__3 ? i__3 : s_rnge("lenths", i__3, 
		    "clmgr_", (ftnlen)593)], cattr, (ftnlen)64, (ftnlen)1, 
		    cattr_len);
	} else {
	    repmc_(pvalue + (((i__2 = types[(i__1 = *id - 1) < 400 && 0 <= 
		    i__1 ? i__1 : s_rnge("types", i__1, "clmgr_", (ftnlen)595)
		    ] - 1) < 4 && 0 <= i__2 ? i__2 : s_rnge("pvalue", i__2, 
		    "clmgr_", (ftnlen)595)) << 6), "#", "*", cattr, (ftnlen)
		    64, (ftnlen)1, (ftnlen)1, cattr_len);
	}
    } else if (s_cmp(attr, "ID", attr_len, (ftnlen)2) == 0) {
	intstr_(id, cattr, cattr_len);
    } else if (s_cmp(attr, "ALIAS", attr_len, (ftnlen)5) == 0) {
	s_copy(cattr, aliass + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		s_rnge("aliass", i__1, "clmgr_", (ftnlen)604)) << 6), 
		cattr_len, (ftnlen)64);
    } else if (s_cmp(attr, "TABLE", attr_len, (ftnlen)5) == 0) {
	s_copy(cattr, tables + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		s_rnge("tables", i__1, "clmgr_", (ftnlen)608)) << 6), 
		cattr_len, (ftnlen)64);
    } else if (s_cmp(attr, "JUSTIFICATION", attr_len, (ftnlen)13) == 0) {
	s_copy(cattr, justs + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		s_rnge("justs", i__1, "clmgr_", (ftnlen)612)) << 3), 
		cattr_len, (ftnlen)8);
    } else if (s_cmp(attr, "COLNAM", attr_len, (ftnlen)6) == 0) {
	s_copy(cattr, cnames + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		s_rnge("cnames", i__1, "clmgr_", (ftnlen)616)) << 6), 
		cattr_len, (ftnlen)64);
    } else if (s_cmp(attr, "WIDTH", attr_len, (ftnlen)5) == 0) {
	if (types[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("types",
		 i__1, "clmgr_", (ftnlen)620)] == 4 && s_cmp(fmts + (((i__2 = 
		*id - 1) < 400 && 0 <= i__2 ? i__2 : s_rnge("fmts", i__2, 
		"clmgr_", (ftnlen)620)) << 7), "DEFAULT", (ftnlen)128, (
		ftnlen)7) == 0 && widths[(i__3 = *id - 1) < 400 && 0 <= i__3 ?
		 i__3 : s_rnge("widths", i__3, "clmgr_", (ftnlen)620)] < 0) {
	    w = 32;
	    bbgetc_1__("COPY", "TIMEFMT", &n, tfmt, (ftnlen)4, (ftnlen)7, (
		    ftnlen)128);
	    nspcht_(tfmt, &w, (ftnlen)128);
	    intstr_(&w, cattr, cattr_len);
	} else if (s_cmp(fmts + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 :
		 s_rnge("fmts", i__1, "clmgr_", (ftnlen)629)) << 7), "DEFAULT"
		, (ftnlen)128, (ftnlen)7) == 0) {
	    if (types[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
		    "types", i__1, "clmgr_", (ftnlen)631)] == 3) {
		s_copy(item, "INTFMT", (ftnlen)64, (ftnlen)6);
	    } else if (types[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("types", i__1, "clmgr_", (ftnlen)633)] == 2) {
		s_copy(item, "DPFMT", (ftnlen)64, (ftnlen)5);
	    } else if (types[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("types", i__1, "clmgr_", (ftnlen)635)] == 1) {
		s_copy(item, "CHFMT", (ftnlen)64, (ftnlen)5);
	    }
	    bbgetc_1__("COPY", item, &n, tfmt, (ftnlen)4, (ftnlen)64, (ftnlen)
		    128);
/* Computing MAX */
	    i__1 = 8, i__2 = rtrim_(tfmt, (ftnlen)128);
	    w = max(i__1,i__2);
	    intstr_(&w, cattr, cattr_len);
	} else {
	    intstr_(&widths[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("widths", i__1, "clmgr_", (ftnlen)646)], cattr, 
		    cattr_len);
	}
    } else if (s_cmp(attr, "INDEXED", attr_len, (ftnlen)7) == 0) {
	if (indexd[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("inde"
		"xd", i__1, "clmgr_", (ftnlen)651)]) {
	    s_copy(cattr, "YES", cattr_len, (ftnlen)3);
	} else {
	    s_copy(cattr, "NO", cattr_len, (ftnlen)2);
	}
    } else if (s_cmp(attr, "NULLOK", attr_len, (ftnlen)6) == 0) {
	if (nullok[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("null"
		"ok", i__1, "clmgr_", (ftnlen)659)]) {
	    s_copy(cattr, "YES", cattr_len, (ftnlen)3);
	} else {
	    s_copy(cattr, "NO", cattr_len, (ftnlen)2);
	}
    } else if (s_cmp(attr, "SIZE", attr_len, (ftnlen)4) == 0) {
	if (sizes[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("sizes",
		 i__1, "clmgr_", (ftnlen)667)] == -1) {
	    s_copy(cattr, "VARIABLE", cattr_len, (ftnlen)8);
	} else {
	    intstr_(&sizes[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("sizes", i__1, "clmgr_", (ftnlen)670)], cattr, 
		    cattr_len);
	}
    } else if (s_cmp(attr, "HANDLES", attr_len, (ftnlen)7) == 0) {
	i__ = 1;
	s_copy(cattr, " ", cattr_len, (ftnlen)1);

/*        Fetch the first handle associated with this ID. */

	syptri_(names + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "clmgr_", (ftnlen)680)) << 6), nams, ptrs, 
		hndls, &start, &n, &gotone, (ftnlen)64, (ftnlen)64);
	i__1 = start + n - 1;
	for (i__ = start; i__ <= i__1; ++i__) {

/*           Now as long as we get another handle, append it */
/*           as a string to the list of handles associated with */
/*           this ID. */

	    j = hndls[(i__2 = i__ + 5) < 8006 && 0 <= i__2 ? i__2 : s_rnge(
		    "hndls", i__2, "clmgr_", (ftnlen)690)];
	    repmc_(cattr, "#", ",", cattr, cattr_len, (ftnlen)1, (ftnlen)1, 
		    cattr_len);
	    suffix_("#", &c__1, cattr, (ftnlen)1, cattr_len);
	    repmi_(cattr, "#", &j, cattr, cattr_len, (ftnlen)1, cattr_len);
	    suffix_("#", &c__0, cattr, (ftnlen)1, cattr_len);
	}
	repmc_(cattr, "#", " ", cattr, cattr_len, (ftnlen)1, (ftnlen)1, 
		cattr_len);
    } else if (s_cmp(attr, "NAME", attr_len, (ftnlen)4) == 0) {
	s_copy(cattr, names + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		s_rnge("names", i__1, "clmgr_", (ftnlen)703)) << 6), 
		cattr_len, (ftnlen)64);
    } else {
	setmsg_("The attribute requested, #, is not a recognized column attr"
		"ibute. ", (ftnlen)66);
	errch_("#", attr, (ftnlen)1, attr_len);
	sigerr_("INSPEKT(UNKNOWNCOLATTRIBUTE)", (ftnlen)28);
	chkout_(rname, (ftnlen)6);
	return 0;
    }
    chkout_(rname, (ftnlen)6);
    return 0;
/* $Procedure CLGAI ( Column, get attribute integer ) */

L_clgai:
/* $ Abstract */

/*     This entry point fetches an integer attribute associated with */
/*     a column. */

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

/* -& */
    if (return_()) {
	return 0;
    }
    s_copy(rname, "CLGAI", (ftnlen)6, (ftnlen)5);
    chkin_(rname, (ftnlen)6);
    if (first) {
	first = FALSE_;
	ssizec_(&c__400, nams, (ftnlen)64);
	ssizei_(&c__400, ptrs);
	ssizei_(&c__8000, hndls);
	nactiv = 0;
	for (i__ = 1; i__ <= 400; ++i__) {
	    active[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("acti"
		    "ve", i__1, "clmgr_", (ftnlen)779)] = FALSE_;
	}
    }
    if (*id < 1 || *id > 400) {
	setmsg_("The ID, #, used to specify a column is out of bounds. To be"
		" considered as an active column, ID must be between 1 and # "
		"inclusive.", (ftnlen)129);
	errint_("#", id, (ftnlen)1);
	errint_("#", &c__400, (ftnlen)1);
	sigerr_("INSPEKT(BADCOLUMNID)", (ftnlen)20);
	chkout_(rname, (ftnlen)6);
	return 0;
    }
    if (! active[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("active",
	     i__1, "clmgr_", (ftnlen)800)]) {
	setmsg_("The column specified by the ID, #, is not active now.", (
		ftnlen)53);
	errint_("#", id, (ftnlen)1);
	sigerr_("INSPEKT(INACTIVEID)", (ftnlen)19);
	chkout_(rname, (ftnlen)6);
	return 0;
    }

/*     See which attribute is requested and load the value into */
/*     the output string.  Normally, the number of attributes */
/*     returned will be one.  If this isn't true. We adjust the count */
/*     when we find out that it is not. */

    *num = 1;
    if (s_cmp(attr, "ID", attr_len, (ftnlen)2) == 0) {
	iattr[0] = *id;
    } else if (s_cmp(attr, "WIDTH", attr_len, (ftnlen)5) == 0) {
	if (types[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("types",
		 i__1, "clmgr_", (ftnlen)825)] == 4 && s_cmp(fmts + (((i__2 = 
		*id - 1) < 400 && 0 <= i__2 ? i__2 : s_rnge("fmts", i__2, 
		"clmgr_", (ftnlen)825)) << 7), "DEFAULT", (ftnlen)128, (
		ftnlen)7) == 0 && widths[(i__3 = *id - 1) < 400 && 0 <= i__3 ?
		 i__3 : s_rnge("widths", i__3, "clmgr_", (ftnlen)825)] < 0) {
	    w = 32;
	    s_copy(item, "TIMEFMT", (ftnlen)64, (ftnlen)7);
	    bbgetc_1__("COPY", item, &n, tfmt, (ftnlen)4, (ftnlen)64, (ftnlen)
		    128);
	    nspcht_(tfmt, &w, (ftnlen)128);
	    iattr[0] = w;
	} else if (s_cmp(fmts + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 :
		 s_rnge("fmts", i__1, "clmgr_", (ftnlen)834)) << 7), "DEFAULT"
		, (ftnlen)128, (ftnlen)7) == 0) {
	    if (types[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
		    "types", i__1, "clmgr_", (ftnlen)836)] == 3) {
		s_copy(item, "INTFMT", (ftnlen)64, (ftnlen)6);
	    } else if (types[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("types", i__1, "clmgr_", (ftnlen)838)] == 2) {
		s_copy(item, "DPFMT", (ftnlen)64, (ftnlen)5);
	    } else if (types[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("types", i__1, "clmgr_", (ftnlen)840)] == 1) {
		s_copy(item, "CHFMT", (ftnlen)64, (ftnlen)5);
	    }
	    bbgetc_1__("COPY", item, &n, tfmt, (ftnlen)4, (ftnlen)64, (ftnlen)
		    128);
/* Computing MAX */
	    i__1 = 8, i__2 = rtrim_(tfmt, (ftnlen)128);
	    w = max(i__1,i__2);
	    iattr[0] = w;
	} else {
	    iattr[0] = widths[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("widths", i__1, "clmgr_", (ftnlen)850)];
	}
    } else if (s_cmp(attr, "SIZE", attr_len, (ftnlen)4) == 0) {
	iattr[0] = sizes[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
		"sizes", i__1, "clmgr_", (ftnlen)855)];
    } else if (s_cmp(attr, "HANDLES", attr_len, (ftnlen)7) == 0) {
	sygeti_(names + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "clmgr_", (ftnlen)859)) << 6), nams, ptrs, 
		hndls, num, iattr, &gotone, (ftnlen)64, (ftnlen)64);
    } else if (s_cmp(attr, "TYPE", attr_len, (ftnlen)4) == 0) {
	iattr[0] = types[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
		"types", i__1, "clmgr_", (ftnlen)865)];
    } else {
	setmsg_("The attribute requested, #, is not a recognized integer col"
		"umn attribute. ", (ftnlen)74);
	errch_("#", attr, (ftnlen)1, attr_len);
	sigerr_("INSPEKT(UNKNOWNCOLATTRIBUTE)", (ftnlen)28);
	chkout_(rname, (ftnlen)6);
	return 0;
    }
    chkout_(rname, (ftnlen)6);
    return 0;
/* $Procedure      CLNEW ( Allocate a new column ) */

L_clnew:
/* $ Abstract */

/*     Given the basic column attributes, this routine enters this */
/*     column in the column database and returns the ID assigned to */
/*     this column. */

    if (return_()) {
	return 0;
    }
    s_copy(rname, "CLNEW", (ftnlen)6, (ftnlen)5);
    chkin_(rname, (ftnlen)6);
    if (first) {
	first = FALSE_;
	ssizec_(&c__400, nams, (ftnlen)64);
	ssizei_(&c__400, ptrs);
	ssizei_(&c__8000, hndls);
	nactiv = 0;
	for (i__ = 1; i__ <= 400; ++i__) {
	    active[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("acti"
		    "ve", i__1, "clmgr_", (ftnlen)912)] = FALSE_;
	}
    }

/*     Loading a new column deactivates any existing query. */

    aquery = FALSE_;
    nqids = 0;

/*     First see if this column already exists.  If it does we */
/*     shall simply update it's values (with a bit of checking */
/*     thrown in for good measure). */

    gotone = FALSE_;
    gotfre = FALSE_;
    i__ = 1;
    while(! gotone && i__ <= 400) {
	if (active[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("acti"
		"ve", i__1, "clmgr_", (ftnlen)935)]) {
	    if (s_cmp(name__, names + (((i__1 = i__ - 1) < 400 && 0 <= i__1 ? 
		    i__1 : s_rnge("names", i__1, "clmgr_", (ftnlen)937)) << 6)
		    , name_len, (ftnlen)64) == 0) {
		gotone = TRUE_;
		*id = i__;
	    }
	} else if (! gotfre) {
	    *id = i__;
	    gotfre = TRUE_;
	}
	++i__;
    }

/*     OK. At this point we've either located the input name */
/*     or a slot to put it (if one exists).  Now simply update */
/*     the values (or set them for the first time) for this */
/*     column. */

    if (gotone) {

/*        This guy is a repeat, we only need to update values. */

	types[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("types", 
		i__1, "clmgr_", (ftnlen)963)] = *ctype;
	sizes[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("sizes", 
		i__1, "clmgr_", (ftnlen)964)] = *csize;
	indexd[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("indexd", 
		i__1, "clmgr_", (ftnlen)965)] = *indx;
	nullok[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("nullok", 
		i__1, "clmgr_", (ftnlen)966)] = *null;
	amount[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("amount", 
		i__1, "clmgr_", (ftnlen)967)] = *csize;
	lenths[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("lenths", 
		i__1, "clmgr_", (ftnlen)968)] = *clen;

/*        Note we only want to update the width in the case of */
/*        character columns.  For everything else there is nothing */
/*        to do. */

	if (*ctype == 1) {
/* Computing MAX */
	    i__4 = widths[(i__2 = *id - 1) < 400 && 0 <= i__2 ? i__2 : s_rnge(
		    "widths", i__2, "clmgr_", (ftnlen)975)];
	    i__3 = max(i__4,*widest);
	    widths[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("widt"
		    "hs", i__1, "clmgr_", (ftnlen)975)] = brckti_(&i__3, &c__8,
		     &c__40);
	}

/*        Set the collection of handle associated with this */
/*        column name (but only if this handle is not already */
/*        attached to this name). */

	syptri_(name__, nams, ptrs, hndls, &start, &n, &ok, name_len, (ftnlen)
		64);
	if (isrchi_(handle, &n, &hndls[(i__1 = start + 5) < 8006 && 0 <= i__1 
		? i__1 : s_rnge("hndls", i__1, "clmgr_", (ftnlen)985)]) == 0) 
		{
	    sypshi_(name__, handle, nams, ptrs, hndls, name_len, (ftnlen)64);
	}
    } else if (gotfre) {

/*        This is a new column.  Activivate this id and fill in */
/*        all of the appropriate values. */

	++nactiv;
	active[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("active", 
		i__1, "clmgr_", (ftnlen)996)] = TRUE_;
	types[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("types", 
		i__1, "clmgr_", (ftnlen)997)] = *ctype;
	sizes[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("sizes", 
		i__1, "clmgr_", (ftnlen)998)] = *csize;
	indexd[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("indexd", 
		i__1, "clmgr_", (ftnlen)999)] = *indx;
	nullok[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("nullok", 
		i__1, "clmgr_", (ftnlen)1000)] = *null;
	amount[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("amount", 
		i__1, "clmgr_", (ftnlen)1001)] = *csize;
	s_copy(names + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
		"names", i__1, "clmgr_", (ftnlen)1002)) << 6), name__, (
		ftnlen)64, name_len);
	lenths[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("lenths", 
		i__1, "clmgr_", (ftnlen)1003)] = *clen;

/*        We need to parse the table name from the from the column name. */

	te = i_indx(name__, ".", name_len, (ftnlen)1) - 1;
	nb = te + 2;
	if (te > 1) {
	    s_copy(tables + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("tables", i__1, "clmgr_", (ftnlen)1011)) << 6), 
		    name__, (ftnlen)64, te);
	} else {
	    s_copy(tables + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("tables", i__1, "clmgr_", (ftnlen)1013)) << 6), 
		    " ", (ftnlen)64, (ftnlen)1);
	}
	syputi_(name__, handle, &c__1, nams, ptrs, hndls, name_len, (ftnlen)
		64);

/*        Finally, set the default values for this column */

	if (nb < i_len(name__, name_len)) {
	    s_copy(aliass + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("aliass", i__1, "clmgr_", (ftnlen)1021)) << 6), 
		    name__ + (nb - 1), (ftnlen)64, name_len - (nb - 1));
	    s_copy(cnames + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("cnames", i__1, "clmgr_", (ftnlen)1022)) << 6), 
		    name__ + (nb - 1), (ftnlen)64, name_len - (nb - 1));
	} else {
	    s_copy(aliass + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("aliass", i__1, "clmgr_", (ftnlen)1024)) << 6), 
		    name__, (ftnlen)64, name_len);
	    s_copy(cnames + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("cnames", i__1, "clmgr_", (ftnlen)1025)) << 6), 
		    name__, (ftnlen)64, name_len);
	}
	if (*ctype == 3) {
	    s_copy(fmts + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("fmts", i__1, "clmgr_", (ftnlen)1030)) << 7), 
		    "DEFAULT", (ftnlen)128, (ftnlen)7);
/* Computing MAX */
	    i__3 = 8, i__4 = rtrim_(fmts + (((i__2 = *id - 1) < 400 && 0 <= 
		    i__2 ? i__2 : s_rnge("fmts", i__2, "clmgr_", (ftnlen)1031)
		    ) << 7), (ftnlen)128);
	    widths[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("widt"
		    "hs", i__1, "clmgr_", (ftnlen)1031)] = max(i__3,i__4);
	    s_copy(justs + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("justs", i__1, "clmgr_", (ftnlen)1032)) << 3), 
		    "RIGHT", (ftnlen)8, (ftnlen)5);
	} else if (*ctype == 4) {
	    s_copy(fmts + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("fmts", i__1, "clmgr_", (ftnlen)1037)) << 7), 
		    "DEFAULT", (ftnlen)128, (ftnlen)7);
	    widths[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("widt"
		    "hs", i__1, "clmgr_", (ftnlen)1038)] = -1;
	    s_copy(justs + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("justs", i__1, "clmgr_", (ftnlen)1039)) << 3), 
		    "LEFT", (ftnlen)8, (ftnlen)4);
	} else if (*ctype == 2) {
	    s_copy(fmts + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("fmts", i__1, "clmgr_", (ftnlen)1043)) << 7), 
		    "DEFAULT", (ftnlen)128, (ftnlen)7);
	    widths[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("widt"
		    "hs", i__1, "clmgr_", (ftnlen)1044)] = 22;
	    s_copy(justs + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("justs", i__1, "clmgr_", (ftnlen)1045)) << 3), 
		    "RIGHT", (ftnlen)8, (ftnlen)5);
	} else if (*ctype == 1) {

/*           This must be a character type of column. */
	    widths[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("widt"
		    "hs", i__1, "clmgr_", (ftnlen)1050)] = brckti_(widest, &
		    c__8, &c__40);
	    s_copy(fmts + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("fmts", i__1, "clmgr_", (ftnlen)1051)) << 7), 
		    " ", (ftnlen)128, (ftnlen)1);
	    s_copy(justs + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("justs", i__1, "clmgr_", (ftnlen)1052)) << 3), 
		    "LEFT", (ftnlen)8, (ftnlen)4);
	} else {
	    setmsg_("The column type supplied is not one of the recognized c"
		    "olumn types. The value supplied was #. ", (ftnlen)94);
	    errint_("#", ctype, (ftnlen)1);
	    sigerr_("INSPEKT(BADCOLUMNTYPE)", (ftnlen)22);
	    chkout_(rname, (ftnlen)6);
	    return 0;
	}
    } else {

/*        The tables must be full.  There's nothing we can do but */
/*        signal an error. */

	setmsg_("There is no room left for creating another column in the co"
		"lumn manager.  To remedy this situation, you might try unloa"
		"ding some of the loaded E-kernels.  If this is not a viable "
		"option, the parameter, MAXNAM , (it controls the maximum num"
		"ber ofcolumns that can be maintained by the column manager) "
		"will need to be increased. Unfortunately, this means editing"
		" source code and rebuilding INSPEKT.  We at NAIF apologize f"
		"or this inconvenience and ask that you notify us of this pro"
		"blem. ", (ftnlen)485);
	sigerr_("INSPEKT(COLUMNBUFFERFULL)", (ftnlen)25);
	chkout_(rname, (ftnlen)6);
	return 0;
    }
    chkout_(rname, (ftnlen)6);
    return 0;
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

/* $Procedure CLNID ( Column, Get N'th ID ) */

L_clnid:
/* $ Abstract */

/*     This routine returns the ID of the NUM'th column stored in the */
/*     column manager. */

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

/* -& */
    if (return_()) {
	return 0;
    }
    s_copy(rname, "CLNID", (ftnlen)6, (ftnlen)5);
    chkin_(rname, (ftnlen)6);
    if (first) {
	first = FALSE_;
	ssizec_(&c__400, nams, (ftnlen)64);
	ssizei_(&c__400, ptrs);
	ssizei_(&c__8000, hndls);
	nactiv = 0;
	for (i__ = 1; i__ <= 400; ++i__) {
	    active[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("acti"
		    "ve", i__1, "clmgr_", (ftnlen)1175)] = FALSE_;
	}
    }
    *found = FALSE_;

/*     Nothing very smart here, simply locate the NUM'th */
/*     array element of ACTIVE that has a value of .TRUE. */

    count = 0;
    for (i__ = 1; i__ <= 400; ++i__) {
	if (active[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("acti"
		"ve", i__1, "clmgr_", (ftnlen)1190)]) {
	    ++count;
	    if (count == *num) {
		*id = i__;
		*found = TRUE_;
	    }
	}
    }
    chkout_(rname, (ftnlen)6);
    return 0;
/* $Procedure CLNUM ( Column, number active ) */

L_clnum:
/* $ Abstract */

/*     Return the number of active columns */

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

/* -& */
    if (return_()) {
	return 0;
    }
    s_copy(rname, "CLNUM", (ftnlen)6, (ftnlen)5);
    chkin_(rname, (ftnlen)6);
    if (first) {
	first = FALSE_;
	ssizec_(&c__400, nams, (ftnlen)64);
	ssizei_(&c__400, ptrs);
	ssizei_(&c__8000, hndls);
	nactiv = 0;
	for (i__ = 1; i__ <= 400; ++i__) {
	    active[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("acti"
		    "ve", i__1, "clmgr_", (ftnlen)1258)] = FALSE_;
	}
    }
    *num = nactiv;
    chkout_(rname, (ftnlen)6);
    return 0;
/* $Procedure CLN2ID ( Column, translate name to ID ) */

L_cln2id:
/* $ Abstract */

/*     Locate the ID associated with this name (provided there is */
/*     such an ID). */

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

/* -& */
    if (return_()) {
	return 0;
    }
    s_copy(rname, "CLN2ID", (ftnlen)6, (ftnlen)6);
    chkin_(rname, (ftnlen)6);
    if (first) {
	first = FALSE_;
	ssizec_(&c__400, nams, (ftnlen)64);
	ssizei_(&c__400, ptrs);
	ssizei_(&c__8000, hndls);
	nactiv = 0;
	for (i__ = 1; i__ <= 400; ++i__) {
	    active[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("acti"
		    "ve", i__1, "clmgr_", (ftnlen)1326)] = FALSE_;
	}
    }

/*     We don't do anything very sophisticated here. We simply */
/*     look for the name of interest among the those names */
/*     that are active. */

    for (i__ = 1; i__ <= 400; ++i__) {
	if (active[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("acti"
		"ve", i__1, "clmgr_", (ftnlen)1337)] && s_cmp(name__, names + (
		((i__2 = i__ - 1) < 400 && 0 <= i__2 ? i__2 : s_rnge("names", 
		i__2, "clmgr_", (ftnlen)1337)) << 6), name_len, (ftnlen)64) ==
		 0) {
	    *id = i__;
	    *found = TRUE_;
	    chkout_(rname, (ftnlen)6);
	    return 0;
	}
    }

/*     If we get to this point, we didn't find an ID for this */
/*     column name.  Set ID to -1.  Any attempts to use this */
/*     ID for something will then result in an error. */

    *found = FALSE_;
    *id = -1;
    chkout_(rname, (ftnlen)6);
    return 0;
/* $Procedure CLPAC ( Column, put attribute---character) */

L_clpac:
/* $ Abstract */

/*     Set one of the settable character column attributes (there are */
/*     only three right now---alias, format and justification). */

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

/* -& */
    if (return_()) {
	return 0;
    }
    s_copy(rname, "CLPAC", (ftnlen)6, (ftnlen)5);
    chkin_(rname, (ftnlen)6);
    if (first) {
	first = FALSE_;
	ssizec_(&c__400, nams, (ftnlen)64);
	ssizei_(&c__400, ptrs);
	ssizei_(&c__8000, hndls);
	nactiv = 0;
	for (i__ = 1; i__ <= 400; ++i__) {
	    active[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("acti"
		    "ve", i__1, "clmgr_", (ftnlen)1417)] = FALSE_;
	}
    }

/*     Check to make sure that the ID is a recognized one. */

    if (*id < 1 || *id > 400) {
	setmsg_("The ID, #, used to specified a column is out of bounds. To "
		"be considered as an active column, ID must be between 1 and "
		"# inclusive.", (ftnlen)131);
	errint_("#", id, (ftnlen)1);
	errint_("#", &c__400, (ftnlen)1);
	sigerr_("INSPEKT(BADCOLUMNID)", (ftnlen)20);
	chkout_(rname, (ftnlen)6);
	return 0;
    }
    if (! active[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("active",
	     i__1, "clmgr_", (ftnlen)1439)]) {
	setmsg_("The column specified by the ID, #, is not active now.", (
		ftnlen)53);
	errint_("#", id, (ftnlen)1);
	sigerr_("INSPEKT(INACTIVEID)", (ftnlen)19);
	chkout_(rname, (ftnlen)6);
	return 0;
    }
    if (s_cmp(attr, "FORMAT", attr_len, (ftnlen)6) == 0) {

/*        We shall adjust the widths of this column if the format */
/*        type is TIME or INT or DP. */

	if (types[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("types",
		 i__1, "clmgr_", (ftnlen)1457)] == 4) {

/*           Check to make sure that the format is do-able. */

	    ljust_(cattr, tfmt, cattr_len, (ftnlen)128);
	    ucase_(tfmt, tfmt, (ftnlen)128, (ftnlen)128);
	    if (s_cmp(tfmt, "DEFAULT", (ftnlen)128, (ftnlen)7) == 0) {
		s_copy(fmts + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
			s_rnge("fmts", i__1, "clmgr_", (ftnlen)1466)) << 7), 
			"DEFAULT", (ftnlen)128, (ftnlen)7);
		widths[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
			"widths", i__1, "clmgr_", (ftnlen)1467)] = -1;
	    } else {
		nspcht_(cattr, &w, cattr_len);
		if (failed_()) {
		    chkout_(rname, (ftnlen)6);
		    return 0;
		}
		s_copy(fmts + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
			s_rnge("fmts", i__1, "clmgr_", (ftnlen)1477)) << 7), 
			cattr, (ftnlen)128, cattr_len);
		widths[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
			"widths", i__1, "clmgr_", (ftnlen)1478)] = max(8,w);
	    }
	} else if (types[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
		"types", i__1, "clmgr_", (ftnlen)1484)] == 3 || types[(i__2 = 
		*id - 1) < 400 && 0 <= i__2 ? i__2 : s_rnge("types", i__2, 
		"clmgr_", (ftnlen)1484)] == 2) {
	    s_copy(fmts + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("fmts", i__1, "clmgr_", (ftnlen)1486)) << 7), 
		    cattr, (ftnlen)128, cattr_len);
	    ljust_(cattr, tfmt, cattr_len, (ftnlen)128);
	    ucase_(tfmt, tfmt, (ftnlen)128, (ftnlen)128);
	    if (s_cmp(tfmt, "DEFAULT", (ftnlen)128, (ftnlen)7) == 0) {
		s_copy(fmts + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
			s_rnge("fmts", i__1, "clmgr_", (ftnlen)1493)) << 7), 
			"DEFAULT", (ftnlen)128, (ftnlen)7);
		widths[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
			"widths", i__1, "clmgr_", (ftnlen)1494)] = -1;
	    } else {
		s_copy(fmts + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
			s_rnge("fmts", i__1, "clmgr_", (ftnlen)1496)) << 7), 
			cattr, (ftnlen)128, cattr_len);
/* Computing MAX */
		i__2 = 8, i__3 = rtrim_(cattr, cattr_len);
		widths[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
			"widths", i__1, "clmgr_", (ftnlen)1497)] = max(i__2,
			i__3);
	    }
	} else {
	    s_copy(fmts + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("fmts", i__1, "clmgr_", (ftnlen)1502)) << 7), 
		    cattr, (ftnlen)128, cattr_len);
	}
    } else if (s_cmp(attr, "ALIAS", attr_len, (ftnlen)5) == 0) {
	s_copy(aliass + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
		"aliass", i__1, "clmgr_", (ftnlen)1508)) << 6), cattr, (
		ftnlen)64, cattr_len);
    } else if (s_cmp(attr, "JUSTIFICATION", attr_len, (ftnlen)13) == 0) {
	s_copy(justs + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
		"justs", i__1, "clmgr_", (ftnlen)1512)) << 3), cattr, (ftnlen)
		8, cattr_len);
    } else {
	setmsg_("The attribute specified, #, cannotbe set as a character col"
		"umn attribute. ", (ftnlen)74);
	errch_("#", attr, (ftnlen)1, attr_len);
	sigerr_("INSPEKT(UNSETABLECHARATTR)", (ftnlen)26);
	chkout_(rname, (ftnlen)6);
	return 0;
    }
    chkout_(rname, (ftnlen)6);
    return 0;
/* $Procedure CLPAI ( Column, put attribute---integer) */

L_clpai:
/* $ Abstract */

/*     Set one of the integer attributes associated with the input */
/*     ID.  (There is only one right now --- width). */

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

/* -& */
    if (return_()) {
	return 0;
    }
    s_copy(rname, "CLPAI", (ftnlen)6, (ftnlen)5);
    chkin_(rname, (ftnlen)6);
    if (first) {
	first = FALSE_;
	ssizec_(&c__400, nams, (ftnlen)64);
	ssizei_(&c__400, ptrs);
	ssizei_(&c__8000, hndls);
	nactiv = 0;
	for (i__ = 1; i__ <= 400; ++i__) {
	    active[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("acti"
		    "ve", i__1, "clmgr_", (ftnlen)1588)] = FALSE_;
	}
    }
    if (*id < 1 || *id > 400) {
	setmsg_("The ID, #, used to specified a column is out of bounds. To "
		"be considered as an active column, ID must be between 1 and "
		"# inclusive.", (ftnlen)131);
	errint_("#", id, (ftnlen)1);
	errint_("#", &c__400, (ftnlen)1);
	sigerr_("INSPEKT(BADCOLUMNID)", (ftnlen)20);
	chkout_(rname, (ftnlen)6);
	return 0;
    }
    if (! active[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("active",
	     i__1, "clmgr_", (ftnlen)1609)]) {
	setmsg_("The column specified by the ID, #, is not active now.", (
		ftnlen)53);
	errint_("#", id, (ftnlen)1);
	sigerr_("INSPEKT(INACTIVEID)", (ftnlen)19);
	chkout_(rname, (ftnlen)6);
	return 0;
    }
    if (s_cmp(attr, "WIDTH", attr_len, (ftnlen)5) == 0) {
	widths[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("widths", 
		i__1, "clmgr_", (ftnlen)1623)] = iattr[0];
    } else {
	setmsg_("The attribute specified, #, cannot beset as an integer colu"
		"mn attribute. ", (ftnlen)73);
	errch_("#", attr, (ftnlen)1, attr_len);
	sigerr_("INSPEKT(UNKNOWNCOLATTRIBUTE)", (ftnlen)28);
	chkout_(rname, (ftnlen)6);
	return 0;
    }
    chkout_(rname, (ftnlen)6);
    return 0;
/* $Procedure CLUNLD ( Column, unload ) */

L_clunld:
/* $ Abstract */

/*     Disassociate a handle from all columns */

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

/* -& */
    if (return_()) {
	return 0;
    }
    s_copy(rname, "CLUNLD", (ftnlen)6, (ftnlen)6);
    chkin_(rname, (ftnlen)6);
    if (first) {
	first = FALSE_;
	ssizec_(&c__400, nams, (ftnlen)64);
	ssizei_(&c__400, ptrs);
	ssizei_(&c__8000, hndls);
	nactiv = 0;
	for (i__ = 1; i__ <= 400; ++i__) {
	    active[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("acti"
		    "ve", i__1, "clmgr_", (ftnlen)1695)] = FALSE_;
	}
    }

/*     Look at every id.  If it is active, remove this handle */
/*     from the list associated with the id. */

    for (i__ = 1; i__ <= 400; ++i__) {
	if (active[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("acti"
		"ve", i__1, "clmgr_", (ftnlen)1705)]) {

/*           Look up the handle pointer and count associated */
/*           with this name. */

	    syptri_(names + (((i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("names", i__1, "clmgr_", (ftnlen)1710)) << 6), 
		    nams, ptrs, hndls, &start, &n, &gotone, (ftnlen)64, (
		    ftnlen)64);

/*           If our handle is among those attached to this column */
/*           we remove it from the list of attached handles. */
/*           If it was the only handle attached to this column */
/*           we deactivate this id. */

	    j = isrchi_(handle, &n, &hndls[(i__1 = start + 5) < 8006 && 0 <= 
		    i__1 ? i__1 : s_rnge("hndls", i__1, "clmgr_", (ftnlen)
		    1719)]);
	    if (j > 0) {

/*              Swap the first handle with this one and then */
/*              pop it from the collection of values associated */
/*              with this ID. */

		sytrni_(names + (((i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 :
			 s_rnge("names", i__1, "clmgr_", (ftnlen)1727)) << 6),
			 &c__1, &j, nams, ptrs, hndls, (ftnlen)64, (ftnlen)64)
			;
		sypopi_(names + (((i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 :
			 s_rnge("names", i__1, "clmgr_", (ftnlen)1731)) << 6),
			 nams, ptrs, hndls, &han, &gotone, (ftnlen)64, (
			ftnlen)64);

/*              If there was only one handle, deactivate this ID. */

		if (n == 1) {
		    active[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : 
			    s_rnge("active", i__1, "clmgr_", (ftnlen)1738)] = 
			    FALSE_;
		    --nactiv;
		}
	    }
	}
    }
    chkout_(rname, (ftnlen)6);
    return 0;
/* $Procedure CLSCOP ( Column set scope ) */

L_clscop:
/* $ Abstract */

/*     Set the scope of rows for which data can be returned. */


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

/* $ Brief I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      COLS       I   A string giving all columns to be fetched. */
/*      QUERY      I   A query that sets the rows in the EK to fetch */
/*      CSIZE      O   The number of columns present in COLS */
/*      IATTR      O   The query-relative idcodes of the columns. */
/*      NUM        O   The number of rows that match the query. */
/*      ERROR      O   Diagnoistic if something is bad. */

/* -& */

/*     Take care of the usual setup stuff. */

    if (return_()) {
	return 0;
    }
    s_copy(rname, "CLSCOP", (ftnlen)6, (ftnlen)6);
    chkin_(rname, (ftnlen)6);
    if (first) {
	first = FALSE_;
	ssizec_(&c__400, nams, (ftnlen)64);
	ssizei_(&c__400, ptrs);
	ssizei_(&c__8000, hndls);
	nactiv = 0;
	for (i__ = 1; i__ <= 400; ++i__) {
	    active[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("acti"
		    "ve", i__1, "clmgr_", (ftnlen)1821)] = FALSE_;
	}
    }

/*     Any calls to CLSCOP wipe out any previous query relative */
/*     information. */

    nqids = 0;
    for (i__ = 1; i__ <= 400; ++i__) {
	qcids[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("qcids", 
		i__1, "clmgr_", (ftnlen)1833)] = -1;
	qtindx[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("qtindx", 
		i__1, "clmgr_", (ftnlen)1834)] = 0;
	s_copy(qalias + (((i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
		"qalias", i__1, "clmgr_", (ftnlen)1835)) << 6), "<bad alias>",
		 (ftnlen)64, (ftnlen)11);
	s_copy(qcolmn + (((i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
		"qcolmn", i__1, "clmgr_", (ftnlen)1836)) << 6), " ", (ftnlen)
		64, (ftnlen)1);
    }

/*     First thing to do is to encode the query. */

    zzekencd_(query, eqryi, eqryc, eqryd, &badqry, errmsg, &errptr, query_len,
	     (ftnlen)2000, (ftnlen)1024);
    if (badqry) {
	s_copy(error, errmsg, error_len, (ftnlen)1024);
	*num = 0;
	aquery = FALSE_;
	*csize = 0;
	nqids = 0;
	prefix_("CLSCOP:", &c__1, error, (ftnlen)7, error_len);
	chkout_(rname, (ftnlen)6);
	return 0;
    }

/*     Next we need to resolve the columns */
/*     that are requested.  There are two cases.  Wildcard requests */
/*     and explicit lists. */

    if (s_cmp(cols, "*", cols_len, (ftnlen)1) == 0) {
	s_copy(error, "CLSCOP: Sorry but we can't handle SELECT * yet.", 
		error_len, (ftnlen)47);
    } else {

/*        First thing we need to do is take apart the list */
/*        of columns.  We use SCANIT to accomplish this task. */
/*        Here are the delimiters we allow.  ' ' and ',' */

	if (unprep) {
	    unprep = FALSE_;
	    s_copy(marks, ",", (ftnlen)4, (ftnlen)1);
	    nmarks = 1;

/*           We will select from the identified tokens in the */
/*           columns only the stuff that isn't a recognized */
/*           delimiter. */

	    keep[0] = 0;
	    scanpr_(&nmarks, marks, mrklen, pnters, (ftnlen)4);
	}
	if (*(unsigned char *)cols == ',') {
/* Writing concatenation */
	    i__5[0] = 85, a__1[0] = "There is a column name missing in the s"
		    "et of columns to fetch: \"<column missing here>";
	    i__5[1] = cols_len, a__1[1] = cols;
	    s_cat(error, a__1, i__5, &c__2, error_len);
	    suffix_("\"", &c__1, error, (ftnlen)1, error_len);
	    *num = 0;
	    aquery = FALSE_;
	    *csize = 0;
	    nqids = 0;
	    chkout_(rname, (ftnlen)6);
	    return 0;
	}
	start = 1;
	scan_(cols, marks, mrklen, pnters, &c__400, &start, &ntokns, ident, 
		qbeg, qend, cols_len, (ftnlen)4);
	scansl_(keep, &c__1, &ntokns, ident, qbeg, qend);

/*        Now for each of the remaining tokens we should have */
/*        columns (possibly with aliases).  We need to check */
/*        each one for its alias and then resolve the column */
/*        for table index, and name. */

	i__1 = ntokns;
	for (l = 1; l <= i__1; ++l) {

/*           Take apart this token to get the qualified column */
/*           name and any alias that might be attached to it. */

	    b = qbeg[(i__2 = l - 1) < 400 && 0 <= i__2 ? i__2 : s_rnge("qbeg",
		     i__2, "clmgr_", (ftnlen)1929)];
	    e = qend[(i__2 = l - 1) < 400 && 0 <= i__2 ? i__2 : s_rnge("qend",
		     i__2, "clmgr_", (ftnlen)1930)];
	    s_copy(mycol, cols + (b - 1), (ftnlen)128, e - (b - 1));
	    s_copy(myalis, " ", (ftnlen)64, (ftnlen)1);
	    if (s_cmp(mycol, " ", (ftnlen)128, (ftnlen)1) == 0) {
		if (b > 1) {
/* Writing concatenation */
		    i__6[0] = 64, a__2[0] = "There is a column name missing "
			    "in the set of columns to fetch: \"";
		    i__6[1] = b - 1, a__2[1] = cols;
		    i__6[2] = 22, a__2[2] = "<column missing here> ";
		    i__6[3] = cols_len - (b - 1), a__2[3] = cols + (b - 1);
		    s_cat(error, a__2, i__6, &c__4, error_len);
		} else {
/* Writing concatenation */
		    i__5[0] = 85, a__1[0] = "There is a column name missing "
			    "in the set of columns to fetch: \"<column missin"
			    "g here>";
		    i__5[1] = cols_len - (b - 1), a__1[1] = cols + (b - 1);
		    s_cat(error, a__1, i__5, &c__2, error_len);
		}
		suffix_("\"", &c__1, error, (ftnlen)1, error_len);
		*num = 0;
		aquery = FALSE_;
		*csize = 0;
		nqids = 0;
		chkout_(rname, (ftnlen)6);
		return 0;
	    }
	    start = 1;
	    fndnwd_(mycol, &start, &b, &e, (ftnlen)128);
	    i__2 = e;
	    ljust_(mycol + i__2, myalis, 128 - i__2, (ftnlen)64);
	    s_copy(qcol, mycol + (b - 1), (ftnlen)64, e - (b - 1));

/*           Find out all the interesting attributes of this column. */

	    zzekpcol_(qcol, eqryi, eqryc, table, talias, &tabidx, lname, &
		    colidx, &badqry, errmsg, (ftnlen)64, (ftnlen)2000, (
		    ftnlen)64, (ftnlen)64, (ftnlen)64, (ftnlen)1024);
	    if (badqry) {
		s_copy(error, errmsg, error_len, (ftnlen)1024);
		*num = 0;
		aquery = FALSE_;
		*csize = 0;
		nqids = 0;
		prefix_("CLSCOP:", &c__1, error, (ftnlen)7, error_len);
		chkout_(rname, (ftnlen)6);
		return 0;
	    }

/*           Construct the name that will be recognized by the */
/*           stuff in this routine.  Save the column name. We'll need */
/*           it later when we start fetching data. */

	    s_copy(qcolmn + (((i__2 = l - 1) < 400 && 0 <= i__2 ? i__2 : 
		    s_rnge("qcolmn", i__2, "clmgr_", (ftnlen)1991)) << 6), 
		    lname, (ftnlen)64, (ftnlen)64);
	    prefix_(".", &c__0, lname, (ftnlen)1, (ftnlen)64);
	    prefix_(table, &c__0, lname, (ftnlen)64, (ftnlen)64);

/*           Search for this guy in the list of loaded columns. */

	    j = 1;
	    gotit = FALSE_;
	    while(j < 400 && ! gotit) {
		if (active[(i__2 = j - 1) < 400 && 0 <= i__2 ? i__2 : s_rnge(
			"active", i__2, "clmgr_", (ftnlen)2002)] && s_cmp(
			lname, names + (((i__3 = j - 1) < 400 && 0 <= i__3 ? 
			i__3 : s_rnge("names", i__3, "clmgr_", (ftnlen)2002)) 
			<< 6), (ftnlen)64, (ftnlen)64) == 0) {
		    myid = j;
		    gotit = TRUE_;
		}
		++j;
	    }

/*           We should never get the error below, but just in case... */

	    if (! gotit) {
		s_copy(error, "A serious problem has occurred.  We got back "
			"the qualified name of a column from the EK library, "
			"but this column is not listed in the Column Manager'"
			"s list of columns. This should never happen. The nam"
			"e of the column was: '#', the fully qualified name w"
			"as: '#'. Please save your log file ", error_len, (
			ftnlen)288);
		repmc_(error, "#", qcolmn + (((i__2 = l - 1) < 400 && 0 <= 
			i__2 ? i__2 : s_rnge("qcolmn", i__2, "clmgr_", (
			ftnlen)2025)) << 6), error, error_len, (ftnlen)1, (
			ftnlen)64, error_len);
		repmc_(error, "#", lname, error, error_len, (ftnlen)1, (
			ftnlen)64, error_len);
		*num = 0;
		aquery = FALSE_;
		*csize = 0;
		nqids = 0;
		chkout_(rname, (ftnlen)6);
		return 0;
	    }

/*           If we get here we have identified this column. */
/*           We need to store its ID, table index, column name, */
/*           and SCOPE alias. */

	    ++nqids;
	    *csize = nqids;
	    qcids[(i__2 = l - 1) < 400 && 0 <= i__2 ? i__2 : s_rnge("qcids", 
		    i__2, "clmgr_", (ftnlen)2044)] = myid;
	    qtindx[(i__2 = l - 1) < 400 && 0 <= i__2 ? i__2 : s_rnge("qtindx",
		     i__2, "clmgr_", (ftnlen)2045)] = tabidx;
	    iattr[l - 1] = l;
	    if (s_cmp(myalis, " ", (ftnlen)64, (ftnlen)1) != 0) {

/*              If the user supplied an alias in the query we use */
/*              that. */

		s_copy(qalias + (((i__2 = l - 1) < 400 && 0 <= i__2 ? i__2 : 
			s_rnge("qalias", i__2, "clmgr_", (ftnlen)2053)) << 6),
			 myalis, (ftnlen)64, (ftnlen)64);
	    } else {

/*              Otherwise we just use a provided alias. */

		s_copy(qalias + (((i__2 = l - 1) < 400 && 0 <= i__2 ? i__2 : 
			s_rnge("qalias", i__2, "clmgr_", (ftnlen)2059)) << 6),
			 aliass + (((i__3 = myid - 1) < 400 && 0 <= i__3 ? 
			i__3 : s_rnge("aliass", i__3, "clmgr_", (ftnlen)2059))
			 << 6), (ftnlen)64, (ftnlen)64);
	    }
	}
    }

/*     All of the columns have now been resolved. Issue the search */

    eksrch_(eqryi, eqryc, eqryd, num, &badqry, errmsg, (ftnlen)2000, (ftnlen)
	    1024);
    if (badqry) {
	s_copy(error, errmsg, error_len, (ftnlen)1024);
	*num = 0;
	aquery = FALSE_;
	*csize = 0;
	nqids = 0;
	*csize = 0;
	prefix_("CLSCOP:", &c__1, error, (ftnlen)7, error_len);
	chkout_(rname, (ftnlen)6);
	return 0;
    }

/*     At this point we have an active query. */

    aquery = TRUE_;

/*     Until the row advance routine is called there are */
/*     no rows available for examination. */

    for (i__ = 1; i__ <= 400; ++i__) {
	avail[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("avail", 
		i__1, "clmgr_", (ftnlen)2098)] = FALSE_;
    }

/*     We need to save the number of rows found and the */
/*     number of rows that have been read so far. */

    scope = *num;
    currnt = 0;
    chkout_(rname, (ftnlen)6);
    return 0;

/*     Entry for advancing the row from which columns can be fetched */


L_cladv:

/*     Advance the row pointed to by the currently active query. */

    if (return_()) {
	return 0;
    }
    s_copy(rname, "CLADV", (ftnlen)6, (ftnlen)5);
    chkin_(rname, (ftnlen)6);
    if (first) {
	first = FALSE_;
	ssizec_(&c__400, nams, (ftnlen)64);
	ssizei_(&c__400, ptrs);
	ssizei_(&c__8000, hndls);
	nactiv = 0;
	for (i__ = 1; i__ <= 400; ++i__) {
	    active[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("acti"
		    "ve", i__1, "clmgr_", (ftnlen)2142)] = FALSE_;
	}
    }
    if (aquery) {
	++currnt;
	*found = currnt <= scope;
    } else {
	*found = FALSE_;
	chkout_(rname, (ftnlen)6);
	return 0;
    }

/*     Determine which columns are available  (Yes, we could check the */
/*     flag GOTONE outside the loop, but it's just not worth */
/*     the extra code.) */

    i__1 = nqids;
    for (i__ = 1; i__ <= i__1; ++i__) {
	myid = qcids[(i__2 = i__ - 1) < 400 && 0 <= i__2 ? i__2 : s_rnge(
		"qcids", i__2, "clmgr_", (ftnlen)2169)];
	amount[(i__2 = myid - 1) < 400 && 0 <= i__2 ? i__2 : s_rnge("amount", 
		i__2, "clmgr_", (ftnlen)2170)] = sizes[(i__3 = myid - 1) < 
		400 && 0 <= i__3 ? i__3 : s_rnge("sizes", i__3, "clmgr_", (
		ftnlen)2170)];
	avail[(i__2 = myid - 1) < 400 && 0 <= i__2 ? i__2 : s_rnge("avail", 
		i__2, "clmgr_", (ftnlen)2171)] = TRUE_;
    }

/*     That's all folks. */

    chkout_(rname, (ftnlen)6);
    return 0;

L_clsrow:

/*     Set the current row of an active query to a specific value. */
/*     This is only valid if there has been a successful call to */
/*     the routine CLSCOP. */

    if (return_()) {
	return 0;
    }
    s_copy(rname, "CLSROW", (ftnlen)6, (ftnlen)6);
    chkin_(rname, (ftnlen)6);

/*     If we don't have an active query we can just bag this and */
/*     return */

    if (! aquery) {
	*found = FALSE_;
	chkout_(rname, (ftnlen)6);
	return 0;
    }

/*     If the row requested doesn't exist for this query we can */
/*     bail out now as well. */

    if (*num > scope || *num < 0) {
	*found = FALSE_;
	chkout_(rname, (ftnlen)6);
	return 0;
    }
    *found = TRUE_;
    currnt = *num;

/*     Set the availability and amounts for each of the columns. */

    i__1 = nqids;
    for (i__ = 1; i__ <= i__1; ++i__) {
	myid = qcids[(i__2 = i__ - 1) < 400 && 0 <= i__2 ? i__2 : s_rnge(
		"qcids", i__2, "clmgr_", (ftnlen)2221)];
	avail[(i__2 = myid - 1) < 400 && 0 <= i__2 ? i__2 : s_rnge("avail", 
		i__2, "clmgr_", (ftnlen)2222)] = active[(i__3 = myid - 1) < 
		400 && 0 <= i__3 ? i__3 : s_rnge("active", i__3, "clmgr_", (
		ftnlen)2222)];
	amount[(i__2 = myid - 1) < 400 && 0 <= i__2 ? i__2 : s_rnge("amount", 
		i__2, "clmgr_", (ftnlen)2223)] = sizes[(i__3 = myid - 1) < 
		400 && 0 <= i__3 ? i__3 : s_rnge("sizes", i__3, "clmgr_", (
		ftnlen)2223)];
    }
    chkout_(rname, (ftnlen)6);
    return 0;
/* $Procedure CLNCMP ( Column, number of components ) */

L_clncmp:
/*     Get the number of number of components associated with the */
/*     specified column for the row number indicated for an active */
/*     query. This should always be called after CLADV */
/*     and before calling CLPVAL for the column specified by ID. */
/*     Note that ID is the Query Relative ID. */
    if (return_()) {
	return 0;
    }
    s_copy(rname, "CLNCMP", (ftnlen)6, (ftnlen)6);
    chkin_(rname, (ftnlen)6);
    if (first) {
	first = FALSE_;
	ssizec_(&c__400, nams, (ftnlen)64);
	ssizei_(&c__400, ptrs);
	ssizei_(&c__8000, hndls);
	nactiv = 0;
	for (i__ = 1; i__ <= 400; ++i__) {
	    active[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("acti"
		    "ve", i__1, "clmgr_", (ftnlen)2261)] = FALSE_;
	}
    }
/*     Until we know otherwise the number of components available */
/*     is zero. */
    *num = 0;
/*     Make sure the ID is a legitimate one at this point of time. */
    if (*id < 1 || *id > nqids) {
	setmsg_("The ID, #, used to specified a column is out of bounds. To "
		"be considered as an active column, ID must be between 1 and "
		"# inclusive.", (ftnlen)131);
	errint_("#", id, (ftnlen)1);
	errint_("#", &c__400, (ftnlen)1);
	sigerr_("INSPEKT(BADCOLUMNID)", (ftnlen)20);
	chkout_(rname, (ftnlen)6);
	return 0;
    }
    myid = qcids[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("qcids", 
	    i__1, "clmgr_", (ftnlen)2288)];
    if (! active[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("active"
	    , i__1, "clmgr_", (ftnlen)2290)]) {
	setmsg_("The column specified by the ID, #, is not active now.", (
		ftnlen)53);
	errint_("#", id, (ftnlen)1);
	sigerr_("INSPEKT(INACTIVEID)", (ftnlen)19);
	chkout_(rname, (ftnlen)6);
	return 0;
    }
/*     If we do not have an active query, then NUM = 0 (we have */
/*     already set it) and we can quit now. */
    if (! aquery || ! avail[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : 
	    s_rnge("avail", i__1, "clmgr_", (ftnlen)2304)]) {
	chkout_(rname, (ftnlen)6);
	return 0;
    }
/*     If this item is of variable size we will need to look up it's */
/*     current size. */
    if (amount[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("amount", 
	    i__1, "clmgr_", (ftnlen)2314)] == -1) {
	eknelt_(id, &currnt, &amount[(i__1 = myid - 1) < 400 && 0 <= i__1 ? 
		i__1 : s_rnge("amount", i__1, "clmgr_", (ftnlen)2315)]);
    }
    if (amount[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("amount", 
	    i__1, "clmgr_", (ftnlen)2318)] == 0) {
	avail[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("avail", 
		i__1, "clmgr_", (ftnlen)2319)] = FALSE_;
    }
    *num = amount[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("amou"
	    "nt", i__1, "clmgr_", (ftnlen)2322)];
    chkout_(rname, (ftnlen)6);
    return 0;
/*     entry CLPVAL  get the print value for a column */

L_clpval:
/*     This routine returns the print value for the column specified */
/*     by ID as well as the width associated with this column.  If */
/*     there is no value for ID it is returned as a blank.  If it has */
/*     a NULL value the string <null> is returned. */
    if (return_()) {
	return 0;
    }
    s_copy(rname, "CLPVAL", (ftnlen)6, (ftnlen)6);
    chkin_(rname, (ftnlen)6);
/*     Just in case we bail out early, set the values associated */
/*     with CATTR and WDTH. */
    s_copy(cattr, " ", cattr_len, (ftnlen)1);
    *wdth = 0;
    if (first) {
	first = FALSE_;
	ssizec_(&c__400, nams, (ftnlen)64);
	ssizei_(&c__400, ptrs);
	ssizei_(&c__8000, hndls);
	nactiv = 0;
	for (i__ = 1; i__ <= 400; ++i__) {
	    active[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("acti"
		    "ve", i__1, "clmgr_", (ftnlen)2365)] = FALSE_;
	}
    }
    if (*id < 1 || *id > nqids) {
	setmsg_("The ID, #, used to specified a column is out of bounds. To "
		"be considered as an active column, ID must be between 1 and "
		"# inclusive.", (ftnlen)131);
	errint_("#", id, (ftnlen)1);
	errint_("#", &c__400, (ftnlen)1);
	sigerr_("INSPEKT(BADCOLUMNID)", (ftnlen)20);
	chkout_(rname, (ftnlen)6);
	return 0;
    }
/*     Look up the global column ID value. */
    myid = qcids[(i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("qcids", 
	    i__1, "clmgr_", (ftnlen)2388)];
/*     This had better be active, but it doesn't hurt to check. */
    if (! active[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("active"
	    , i__1, "clmgr_", (ftnlen)2392)]) {
	setmsg_("The column specified by the ID, #, is not active now.", (
		ftnlen)53);
	errint_("#", id, (ftnlen)1);
	sigerr_("INSPEKT(INACTIVEID)", (ftnlen)19);
	chkout_(rname, (ftnlen)6);
	return 0;
    }
/*     OK. We are ready to rock 'n roll. */
    *wdth = widths[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("wid"
	    "ths", i__1, "clmgr_", (ftnlen)2407)];
    if (*wdth < 0 && types[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : 
	    s_rnge("types", i__1, "clmgr_", (ftnlen)2409)] == 4) {
	bbgetc_1__("COPY", "TIMEFMT", &n, tfmt, (ftnlen)4, (ftnlen)7, (ftnlen)
		128);
	nspcht_(tfmt, wdth, (ftnlen)128);
    }
/*     If the column was unavailable, say so. */
    if (! avail[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("avail", 
	    i__1, "clmgr_", (ftnlen)2416)]) {
	if (*num <= 1) {
	    s_copy(cattr, "<absent>", cattr_len, (ftnlen)8);
	} else {
	    s_copy(cattr, " ", cattr_len, (ftnlen)1);
	}
	chkout_(rname, (ftnlen)6);
	return 0;
    }
/*     If we go beyond the number of components available */
/*     return a blank. */
    if (*num > amount[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
	    "amount", i__1, "clmgr_", (ftnlen)2430)]) {
	s_copy(cattr, " ", cattr_len, (ftnlen)1);
	chkout_(rname, (ftnlen)6);
	return 0;
    }
/*     Ok.  We should be in range and actually have something to */
/*     fetch. */
    if (types[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("types", 
	    i__1, "clmgr_", (ftnlen)2441)] == 1) {
	s_copy(mystr, " ", (ftnlen)1024, (ftnlen)1);
	ekgc_(id, &currnt, num, mystr, &nulval, &gotone, (ftnlen)1024);
    } else if (types[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
	    "types", i__1, "clmgr_", (ftnlen)2446)] == 2) {
	ekgd_(id, &currnt, num, &x, &nulval, &gotone);
    } else if (types[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
	    "types", i__1, "clmgr_", (ftnlen)2450)] == 3) {
	ekgi_(id, &currnt, num, &i__, &nulval, &gotone);
    } else if (types[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
	    "types", i__1, "clmgr_", (ftnlen)2454)] == 4) {
	ekgd_(id, &currnt, num, &x, &nulval, &gotone);
    }
    if (! gotone) {
	avail[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("avail", 
		i__1, "clmgr_", (ftnlen)2463)] = FALSE_;
	if (*num <= 1) {
	    s_copy(cattr, "<absent>", cattr_len, (ftnlen)8);
	} else {
	    s_copy(cattr, " ", cattr_len, (ftnlen)1);
	}
	chkout_(rname, (ftnlen)6);
	return 0;
    }
/*     We have to treat columns with null values too. */
    if (nulval) {
	if (*num == 1) {
	    s_copy(cattr, "<null>", cattr_len, (ftnlen)6);
	} else {
	    s_copy(cattr, " ", cattr_len, (ftnlen)1);
	}
	chkout_(rname, (ftnlen)6);
	return 0;
    }
/*     Finally, it is time to produce a print string for this */
/*     value. */
    if (types[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("types", 
	    i__1, "clmgr_", (ftnlen)2492)] == 1) {
	s_copy(cattr, mystr, cattr_len, (ftnlen)1024);
    } else if (types[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
	    "types", i__1, "clmgr_", (ftnlen)2496)] == 3) {
	if (s_cmp(fmts + (((i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : 
		s_rnge("fmts", i__1, "clmgr_", (ftnlen)2498)) << 7), "DEFAULT"
		, (ftnlen)128, (ftnlen)7) == 0) {
	    bbgetc_1__("COPY", "INTFMT", &n, tfmt, (ftnlen)4, (ftnlen)6, (
		    ftnlen)128);
	} else {
	    s_copy(tfmt, fmts + (((i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 
		    : s_rnge("fmts", i__1, "clmgr_", (ftnlen)2501)) << 7), (
		    ftnlen)128, (ftnlen)128);
	}
	fmtint_(&i__, tfmt, cattr, (ftnlen)128, cattr_len);
    } else if (types[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
	    "types", i__1, "clmgr_", (ftnlen)2506)] == 2) {
	if (s_cmp(fmts + (((i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : 
		s_rnge("fmts", i__1, "clmgr_", (ftnlen)2508)) << 7), "DEFAULT"
		, (ftnlen)128, (ftnlen)7) == 0) {
	    bbgetc_1__("COPY", "DPFMT", &n, tfmt, (ftnlen)4, (ftnlen)5, (
		    ftnlen)128);
	} else {
	    s_copy(tfmt, fmts + (((i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 
		    : s_rnge("fmts", i__1, "clmgr_", (ftnlen)2511)) << 7), (
		    ftnlen)128, (ftnlen)128);
	}
	fmtdp_(&x, tfmt, cattr, (ftnlen)128, cattr_len);
    } else if (types[(i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
	    "types", i__1, "clmgr_", (ftnlen)2516)] == 4) {
	if (s_cmp(fmts + (((i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 : 
		s_rnge("fmts", i__1, "clmgr_", (ftnlen)2518)) << 7), "DEFAULT"
		, (ftnlen)128, (ftnlen)7) == 0) {
	    bbgetc_1__("COPY", "TIMEFMT", &n, tfmt, (ftnlen)4, (ftnlen)7, (
		    ftnlen)128);
	} else {
	    s_copy(tfmt, fmts + (((i__1 = myid - 1) < 400 && 0 <= i__1 ? i__1 
		    : s_rnge("fmts", i__1, "clmgr_", (ftnlen)2521)) << 7), (
		    ftnlen)128, (ftnlen)128);
	}
	fmttim_(&x, tfmt, cattr, (ftnlen)128, cattr_len);
    }
    chkout_(rname, (ftnlen)6);
    return 0;
/*     The following entry point finds the column name that most */
/*     closely spells the input name. */

L_clspel:
    ssizec_(&c__400, known, (ftnlen)64);
    ssizei_(&c__10, best);
    ssizei_(&c__10, scores);
    for (i__ = 1; i__ <= 400; ++i__) {
	if (active[(i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge("acti"
		"ve", i__1, "clmgr_", (ftnlen)2545)]) {
	    appndc_(names + (((i__1 = i__ - 1) < 400 && 0 <= i__1 ? i__1 : 
		    s_rnge("names", i__1, "clmgr_", (ftnlen)2546)) << 6), 
		    known, (ftnlen)64, (ftnlen)64);
	}
    }
    cutoff = 70;
    bestwd_(name__, known, &cutoff, best, scores, error, name_len, (ftnlen)64,
	     error_len);
    high = -1;
    i__1 = cardi_(best);
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (best[(i__2 = i__ + 5) < 16 && 0 <= i__2 ? i__2 : s_rnge("best", 
		i__2, "clmgr_", (ftnlen)2559)] > high) {
	    highst = i__;
	    high = best[(i__2 = i__ + 5) < 16 && 0 <= i__2 ? i__2 : s_rnge(
		    "best", i__2, "clmgr_", (ftnlen)2561)];
	}
    }
    if (high >= cutoff) {
	*found = TRUE_;
	s_copy(corctn, known + (((i__1 = highst + 5) < 406 && 0 <= i__1 ? 
		i__1 : s_rnge("known", i__1, "clmgr_", (ftnlen)2568)) << 6), 
		corctn_len, (ftnlen)64);
    } else {
	*found = FALSE_;
	s_copy(corctn, " ", corctn_len, (ftnlen)1);
    }
    return 0;
/* $Procedure CLGQAL ( Column --- get query relative alias ) */

L_clgqal:
/* $ Abstract */

/*     This entry point returns the query alias for a column if there */
/*     is an active query. */
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

/* -& */
    s_copy(rname, "CLGQAL", (ftnlen)6, (ftnlen)6);
/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_(rname, (ftnlen)6);

/*     A column ID has a query alias only if there is an active */
/*     query. */

    if (! aquery) {
	s_copy(cattr, " ", cattr_len, (ftnlen)1);
	chkout_(rname, (ftnlen)6);
	return 0;
    }

/*     Check to make sure the ID is in range. */

    if (*id < 0 || *id > nqids) {
	s_copy(cattr, " ", cattr_len, (ftnlen)1);
	chkout_(rname, (ftnlen)6);
	return 0;
    }
    s_copy(cattr, qalias + (((i__1 = *id - 1) < 400 && 0 <= i__1 ? i__1 : 
	    s_rnge("qalias", i__1, "clmgr_", (ftnlen)2644)) << 6), cattr_len, 
	    (ftnlen)64);
    chkout_(rname, (ftnlen)6);
    return 0;
/* $Procedure CLQ2ID (Column, query id to global column id.) */

L_clq2id:
/* $ Abstract */

/*     Map a query relative ID to the corresponding global */
/*     column ID. */

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

/* $ Brief I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      CSIZE      I   The query relative ID of some column */
/*      ID         O   The corresponding global column ID. */
/* -& */
    if (aquery && *csize > 0 && *csize <= nqids) {
	*id = qcids[(i__1 = *csize - 1) < 400 && 0 <= i__1 ? i__1 : s_rnge(
		"qcids", i__1, "clmgr_", (ftnlen)2696)];
    } else {
	*id = -1;
    }
    return 0;
} /* clmgr_ */

/* Subroutine */ int clmgr_(char *name__, integer *id, integer *num, integer *
	handle, logical *null, logical *indx, integer *ctype, integer *csize, 
	char *attr, integer *clen, integer *iattr, char *cattr, logical *
	found, char *cols, char *query, char *error, integer *wdth, char *
	corctn, integer *widest, ftnlen name_len, ftnlen attr_len, ftnlen 
	cattr_len, ftnlen cols_len, ftnlen query_len, ftnlen error_len, 
	ftnlen corctn_len)
{
    return clmgr_0_(0, name__, id, num, handle, null, indx, ctype, csize, 
	    attr, clen, iattr, cattr, found, cols, query, error, wdth, corctn,
	     widest, name_len, attr_len, cattr_len, cols_len, query_len, 
	    error_len, corctn_len);
    }

/* Subroutine */ int clgac_(integer *id, char *attr, char *cattr, ftnlen 
	attr_len, ftnlen cattr_len)
{
    return clmgr_0_(1, (char *)0, id, (integer *)0, (integer *)0, (logical *)
	    0, (logical *)0, (integer *)0, (integer *)0, attr, (integer *)0, (
	    integer *)0, cattr, (logical *)0, (char *)0, (char *)0, (char *)0,
	     (integer *)0, (char *)0, (integer *)0, (ftnint)0, attr_len, 
	    cattr_len, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int clgai_(integer *id, char *attr, integer *num, integer *
	iattr, ftnlen attr_len)
{
    return clmgr_0_(2, (char *)0, id, num, (integer *)0, (logical *)0, (
	    logical *)0, (integer *)0, (integer *)0, attr, (integer *)0, 
	    iattr, (char *)0, (logical *)0, (char *)0, (char *)0, (char *)0, (
	    integer *)0, (char *)0, (integer *)0, (ftnint)0, attr_len, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int clnew_(char *name__, integer *handle, integer *ctype, 
	integer *clen, integer *widest, integer *csize, logical *indx, 
	logical *null, integer *id, ftnlen name_len)
{
    return clmgr_0_(3, name__, id, (integer *)0, handle, null, indx, ctype, 
	    csize, (char *)0, clen, (integer *)0, (char *)0, (logical *)0, (
	    char *)0, (char *)0, (char *)0, (integer *)0, (char *)0, widest, 
	    name_len, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0);
    }

/* Subroutine */ int clnid_(integer *num, integer *id, logical *found)
{
    return clmgr_0_(4, (char *)0, id, num, (integer *)0, (logical *)0, (
	    logical *)0, (integer *)0, (integer *)0, (char *)0, (integer *)0, 
	    (integer *)0, (char *)0, found, (char *)0, (char *)0, (char *)0, (
	    integer *)0, (char *)0, (integer *)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int clnum_(integer *num)
{
    return clmgr_0_(5, (char *)0, (integer *)0, num, (integer *)0, (logical *)
	    0, (logical *)0, (integer *)0, (integer *)0, (char *)0, (integer *
	    )0, (integer *)0, (char *)0, (logical *)0, (char *)0, (char *)0, (
	    char *)0, (integer *)0, (char *)0, (integer *)0, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int cln2id_(char *name__, integer *id, logical *found, 
	ftnlen name_len)
{
    return clmgr_0_(6, name__, id, (integer *)0, (integer *)0, (logical *)0, (
	    logical *)0, (integer *)0, (integer *)0, (char *)0, (integer *)0, 
	    (integer *)0, (char *)0, found, (char *)0, (char *)0, (char *)0, (
	    integer *)0, (char *)0, (integer *)0, name_len, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int clpac_(integer *id, char *attr, char *cattr, ftnlen 
	attr_len, ftnlen cattr_len)
{
    return clmgr_0_(7, (char *)0, id, (integer *)0, (integer *)0, (logical *)
	    0, (logical *)0, (integer *)0, (integer *)0, attr, (integer *)0, (
	    integer *)0, cattr, (logical *)0, (char *)0, (char *)0, (char *)0,
	     (integer *)0, (char *)0, (integer *)0, (ftnint)0, attr_len, 
	    cattr_len, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int clpai_(integer *id, char *attr, integer *iattr, ftnlen 
	attr_len)
{
    return clmgr_0_(8, (char *)0, id, (integer *)0, (integer *)0, (logical *)
	    0, (logical *)0, (integer *)0, (integer *)0, attr, (integer *)0, 
	    iattr, (char *)0, (logical *)0, (char *)0, (char *)0, (char *)0, (
	    integer *)0, (char *)0, (integer *)0, (ftnint)0, attr_len, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int clunld_(integer *handle)
{
    return clmgr_0_(9, (char *)0, (integer *)0, (integer *)0, handle, (
	    logical *)0, (logical *)0, (integer *)0, (integer *)0, (char *)0, 
	    (integer *)0, (integer *)0, (char *)0, (logical *)0, (char *)0, (
	    char *)0, (char *)0, (integer *)0, (char *)0, (integer *)0, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0);
    }

/* Subroutine */ int clscop_(char *cols, char *query, integer *csize, integer 
	*iattr, integer *num, char *error, ftnlen cols_len, ftnlen query_len, 
	ftnlen error_len)
{
    return clmgr_0_(10, (char *)0, (integer *)0, num, (integer *)0, (logical *
	    )0, (logical *)0, (integer *)0, csize, (char *)0, (integer *)0, 
	    iattr, (char *)0, (logical *)0, cols, query, error, (integer *)0, 
	    (char *)0, (integer *)0, (ftnint)0, (ftnint)0, (ftnint)0, 
	    cols_len, query_len, error_len, (ftnint)0);
    }

/* Subroutine */ int cladv_(logical *found)
{
    return clmgr_0_(11, (char *)0, (integer *)0, (integer *)0, (integer *)0, (
	    logical *)0, (logical *)0, (integer *)0, (integer *)0, (char *)0, 
	    (integer *)0, (integer *)0, (char *)0, found, (char *)0, (char *)
	    0, (char *)0, (integer *)0, (char *)0, (integer *)0, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int clsrow_(integer *num, logical *found)
{
    return clmgr_0_(12, (char *)0, (integer *)0, num, (integer *)0, (logical *
	    )0, (logical *)0, (integer *)0, (integer *)0, (char *)0, (integer 
	    *)0, (integer *)0, (char *)0, found, (char *)0, (char *)0, (char *
	    )0, (integer *)0, (char *)0, (integer *)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int clncmp_(integer *id, integer *num)
{
    return clmgr_0_(13, (char *)0, id, num, (integer *)0, (logical *)0, (
	    logical *)0, (integer *)0, (integer *)0, (char *)0, (integer *)0, 
	    (integer *)0, (char *)0, (logical *)0, (char *)0, (char *)0, (
	    char *)0, (integer *)0, (char *)0, (integer *)0, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int clpval_(integer *id, integer *num, char *cattr, integer *
	wdth, ftnlen cattr_len)
{
    return clmgr_0_(14, (char *)0, id, num, (integer *)0, (logical *)0, (
	    logical *)0, (integer *)0, (integer *)0, (char *)0, (integer *)0, 
	    (integer *)0, cattr, (logical *)0, (char *)0, (char *)0, (char *)
	    0, wdth, (char *)0, (integer *)0, (ftnint)0, (ftnint)0, cattr_len,
	     (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int clspel_(char *name__, logical *found, char *corctn, char 
	*error, ftnlen name_len, ftnlen corctn_len, ftnlen error_len)
{
    return clmgr_0_(15, name__, (integer *)0, (integer *)0, (integer *)0, (
	    logical *)0, (logical *)0, (integer *)0, (integer *)0, (char *)0, 
	    (integer *)0, (integer *)0, (char *)0, found, (char *)0, (char *)
	    0, error, (integer *)0, corctn, (integer *)0, name_len, (ftnint)0,
	     (ftnint)0, (ftnint)0, (ftnint)0, error_len, corctn_len);
    }

/* Subroutine */ int clgqal_(integer *id, char *cattr, ftnlen cattr_len)
{
    return clmgr_0_(16, (char *)0, id, (integer *)0, (integer *)0, (logical *)
	    0, (logical *)0, (integer *)0, (integer *)0, (char *)0, (integer *
	    )0, (integer *)0, cattr, (logical *)0, (char *)0, (char *)0, (
	    char *)0, (integer *)0, (char *)0, (integer *)0, (ftnint)0, (
	    ftnint)0, cattr_len, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int clq2id_(integer *csize, integer *id)
{
    return clmgr_0_(17, (char *)0, id, (integer *)0, (integer *)0, (logical *)
	    0, (logical *)0, (integer *)0, csize, (char *)0, (integer *)0, (
	    integer *)0, (char *)0, (logical *)0, (char *)0, (char *)0, (char 
	    *)0, (integer *)0, (char *)0, (integer *)0, (ftnint)0, (ftnint)0, 
	    (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

