/* m2pars.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__100 = 100;
static integer c__400 = 400;

/* $Procedure M2PARS ( META/2 --- Parsing utility. ) */
/* Subroutine */ int m2pars_0_(int n__, char *name__, integer *b, integer *e, 
	integer *nth, logical *found, integer *size, ftnlen name_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer vals[406], temp[400], ptrs[106], i__, j, k;
    extern integer cardc_(char *, ftnlen);
    static char names[32*106];
    static integer avals[406], total;
    static logical gotit;
    static integer aptrs[106], begend[2];
    static char anames[32*106], myname[32];
    extern integer sydimi_(char *, char *, integer *, integer *, ftnlen, 
	    ftnlen);
    extern /* Subroutine */ int ssizec_(integer *, char *, ftnlen), syfeti_(
	    integer *, char *, integer *, integer *, char *, logical *, 
	    ftnlen, ftnlen), sygeti_(char *, char *, integer *, integer *, 
	    integer *, integer *, logical *, ftnlen, ftnlen), ssizei_(integer 
	    *, integer *), syenqi_(char *, integer *, char *, integer *, 
	    integer *, ftnlen, ftnlen), syseli_(char *, integer *, integer *, 
	    char *, integer *, integer *, integer *, logical *, ftnlen, 
	    ftnlen), syputi_(char *, integer *, integer *, char *, integer *, 
	    integer *, ftnlen, ftnlen);

/* $ Abstract */

/*     M2PARS serves as an umbrella subroutine for a series of entry */
/*     points that serve as a storage utility for parsed words of */
/*     a string that matches a META/2 language statement template. */

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

/*     META/2 --- A language specification language. */

/* $ Keywords */

/*     META/2 */
/*     PARSING */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Entry Points */
/*     --------  ---  -------------------------------------------------- */
/*     NAME       I   M2SAVE, M2VGET */
/*     B         I/O  M2SAVE, M2VGET */
/*     E         I/O  M2SAVE, M2VGET */
/*     NTH        O   M2VGET */
/*     FOUND      O   M2VGET */
/*     SIZE       O   M2VSIZ */

/*     MXNAME     P   Maximum number of named variables that can be saved */
/*     MXVALS     P   Maximum number of variable values that can be saved */

/* $ Detailed_Input */

/*     NAME       is the variable name associated with some META/2 */
/*                template word. */

/*     B          is the index of the beginning of a word in a string */
/*                that matches the template word associated with NAME. */

/*     E          is the index of the ending of a word in a string */
/*                that matches the template word associated with NAME. */

/*     NTH        is the number of the matching substring to to locate */
/*                in the table of parsed matches. */

/* $ Detailed_Output */

/*     B          is the index of the beginning of a word in a string */
/*                that matches the template word associated with NAME. */

/*     E          is the index of the ending of a word in a string */
/*                that matches the template word associated with NAME. */

/*     FOUND      is a logical flag that is returned .TRUE. if */
/*                a specified named template word matched a word */
/*                in  string.  Otherwise it returns .FALSE. */

/*     SIZE       is the size of the set of words that matched */
/*                a particular named META/2 template word. */

/* $ Parameters */

/*     MXNAME     is the maximum number of named template variables that */
/*                can be saved at any time. */

/*     MXVALS     is the maximum number of name template variable values */
/*                that can be saved at any time. */

/* $ Exceptions */

/*     1) If the number of named template variables or the total number */
/*        of values exceeds the space allotted for these items, an error */
/*        will be diagnosed and signalled by a routine in the call */
/*        tree of this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine serves as an umbrella subroutine for a collection */
/*     of related entry points that are used to manage storage of */
/*     parsed words in strings that match a META/2 language template. */

/*     These entry points cooperate to allow simple parsing of statements */
/*     that match META/2 templates.  To understand how this cooperation */
/*     takes place, you need to understand how strings are matched */
/*     against META/2 templates.  General META/2 templates are composed */
/*     of collections of simple templates organized via the placement */
/*     of grouping  symbols.  These groups are called switches. See the */
/*     META/2 required reading from a more complete description of */
/*     switches. */

/*     Attempts to match a string with a META/2 template proceed from */
/*     left to right in both the string and template.  When a switch */
/*     is encountered, an attempt is made to match the remaining */
/*     portion of the string with each simple template until a match */
/*     attempt succeeds or all attempts fail. */

/*     The parsing portion of META/2 lies in the simple template */
/*     matching module.  As words in the input string are matched with */
/*     words in the simple template, their boundaries in the string */
/*     are saved in tables located in this routine.  These boundaries */
/*     can then be retrieved by the name attached to the META/2 template */
/*     word they matched. */

/*     Since a string can match any of several templates within a switch, */
/*     several match attempts in a switch may fail before the matching */
/*     simple template is encountered.  As a result, there needs to be */
/*     a mechanism for accumulating parsed matches until a full simple */
/*     template is matched.  At that point the accumulated matches need */
/*     to be moved to a more stable storage area.  In this way the */
/*     string can be parsed as it is matched. */


/*     The entry points and the functions they serve are listed here: */

/*     M2SAVE   this entry point is used to store the beginning and */
/*              ending indexes of a word in a string that matches */
/*              a named variable/word in a META/2 language template. */

/*     M2PCLR   this entry point is used to clear all stored information */
/*              in the tables containing substring indexes for matched */
/*              META/2 template words. */

/*     M2TCLR   this entry point is used to clear information stored */
/*              in the temporary accumulation tables that store */
/*              indexes for the beginning and ending of matched */
/*              META/2 template words from simple templates. */

/*     M2KEEP   is the routine that transfers the accumulated matches to */
/*              the finished set of parsed matches. */

/*     M2VGET   retrieves the N'th substring boundaries (of a string */
/*              that matches a META/2 template) that correspond to a */
/*              specific named word of the matching META/2 template. */

/*     M2VSIZ   retrieve the number of pair of indices marking beginnings */
/*              and endings of string words that matched a particular */
/*              named template word. */

/*     Related routines exist.  For use in logical expressions: */

/*        M2XIST(NAME) will be .TRUE. if there is a marked substring */
/*        that matches a META/2 template word having name NAME. */

/*     To determine the number of substrings associated with a given */
/*     named template word use the function: */

/*        M2HAVE ( NAME ) */

/*     To extract the n'th word or first word associated with a */
/*     named template word */

/*        CALL M2SELC ( NAME, STRING, NTH, FOUND, WORD ) */
/*        CALL M2GETC ( NAME, STRING,      FOUND, WORD ) */

/*     To extract and parse the n'th integer or the first integer */
/*     associated with a named template word */

/*        CALL M2SELI ( NAME, STRING, NTH, FOUND, INT  ) */
/*        CALL M2GETI ( NAME, STRING,      FOUND, INT  ) */

/*     To extract and parse the n'th number or first number  associated */
/*     with a named template word */

/*        CALL M2SELD ( NAME, STRING, NTH, FOUND, DP   ) */
/*        CALL M2GETD ( NAME, STRING,      FOUND, DP   ) */

/* $ Examples */

/*     The average user will never need to call any of the entry points */
/*     to this routine.  However, it may be desirable to design a routine */
/*     that makes use of the entry points to this routine.  Example */
/*     routines are outlined in each of the individual entry points. */

/* $ Restrictions */

/*     See individual entry points. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/* -    Beta Version 1.0.0, 22-NOV-1991 (WLT) */

/* -& */

/*     Spicelib Functions. */


/*     Private Parameters */


/*     Local Variables. */

    switch(n__) {
	case 1: goto L_m2save;
	case 2: goto L_m2pclr;
	case 3: goto L_m2tclr;
	case 4: goto L_m2keep;
	case 5: goto L_m2vget;
	case 6: goto L_m2vsiz;
	}

    return 0;
/* $Procedure M2SAVE ( META/2 --- save substring boundaries ) */

L_m2save:
/* $ Abstract */

/*     Store the substring boundaries of a word that matches a META/2 */
/*     template word. */

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

/*     META/2 --- A language specification language. */

/* $ Keywords */

/*     PARSING */
/*     UTILITITY */

/* $ Declarations */

/*     CHARACTER*(*)         NAME */
/*     INTEGER               B */
/*     INTEGER               E */


/*     INTEGER               MXNAME */
/*     PARAMETER           ( MXNAME  = 100 ) */

/*     INTEGER               MXVALS */
/*     PARAMETER           ( MXVALS  = 400 ) */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     NAME       I   The name of a template word that was matched. */
/*     B          I   The beginning of the matching substring. */
/*     E          I   The ending of the matching substring. */

/*     MXNAME     P   Maximum number of named variables that can be saved */
/*     MXVALS     P   Maximum number of variable values that can be saved */

/* $ Detailed_Input */

/*     NAME       is the name associated with a particular META/2 */
/*                template word that has been matched against some */
/*                word in a string. */

/*     B          is the beginning index of a word in a string that */
/*                matched the META/2 template word associated with */
/*                NAME. */

/*     E          is the ending index of a word in a string that */
/*                matched the META/2 template word associated with */
/*                NAME. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     MXNAME     is the maximum number of named template variables that */
/*                can be saved at any time. */

/*     MXVALS     is the maximum number of named template variable values */
/*                that can be saved at any time. */

/* $ Exceptions */

/*     1) If the table for storing string endpoints is unable to store */
/*        the input endpoints, the error will be diagnosed and signalled */
/*        by a routine in this routine's call-tree. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine provides the META/2 matching routines a place to */
/*     deposit the boundaries of words that match named META/2 template */
/*     words.  It is not intendend for direct use by general users. */

/* $ Examples */

/*     See the routine M2WMCH for an example of the use of this routine. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/* -    Beta Version 1.0.0, 25-NOV-1991 (WLT) */

/* -& */
/* $ Index_Entry */

/*     Save the boundaries of words matching META/2 template words */

/* -& */
    if (first) {
	first = FALSE_;

/*        Initialize the keepers table. */

	ssizec_(&c__100, names, (ftnlen)32);
	ssizei_(&c__100, ptrs);
	ssizei_(&c__400, vals);

/*        Initialize the accumulation table */

	ssizec_(&c__100, anames, (ftnlen)32);
	ssizei_(&c__100, aptrs);
	ssizei_(&c__400, avals);
    }

/*     Enque the new string boundaries in the accumulation table. */

    syenqi_(name__, b, anames, aptrs, avals, name_len, (ftnlen)32);
    syenqi_(name__, e, anames, aptrs, avals, name_len, (ftnlen)32);
    return 0;
/* $Procedure      M2PCLR ( META/2 --- Parse table clear ) */

L_m2pclr:
/* $ Abstract */

/*     Clear both the accumulation and parse tables. */

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

/*     META/2 --- a language specification language. */

/* $ Keywords */

/*     PARSING */
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

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine clears all tables used by M2PARS that store beginning */
/*     and ends of words that match names META/2 template words. */
/*     It should never be called directly by users. */

/* $ Examples */

/*     None.  See the routine M2GMCH for the only instance of use of this */
/*     routine. */

/* $ Restrictions */

/*     User's should not call this routine directly.  It is intended */
/*     only for use as utility for META/2 software. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/* -    Beta Version 1.0.0, 25-NOV-1991 (WLT) */

/* -& */

/* $ Index_Entry */

/*     Clear the parse META/2 parse tables */

/* -& */
    first = FALSE_;

/*     Initialize the keepers table. */

    ssizec_(&c__100, names, (ftnlen)32);
    ssizei_(&c__100, ptrs);
    ssizei_(&c__400, vals);

/*     Initialize the accumulation table */

    ssizec_(&c__100, anames, (ftnlen)32);
    ssizei_(&c__100, aptrs);
    ssizei_(&c__400, avals);
    return 0;
/* $Procedure      M2TCLR ( META/2 --- Temporary parse table clear ) */

L_m2tclr:
/* $ Abstract */

/*     Clear both the accumulation (temporary) parse table. */

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

/*     META/2 --- a language specification language. */

/* $ Keywords */

/*     PARSING */
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

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine clears the temporary (accumulation) table used */
/*     by M2PARS that stores beginning and ends of words that match */
/*     names META/2 template words.  It should never be called directly */
/*     by users. */

/* $ Examples */

/*     None.  See the routine M2GMCH for the only instance of use of this */
/*     routine. */

/* $ Restrictions */

/*     User's should not call this routine directly.  It is intended */
/*     only for use as utility for META/2 software. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/* -    Beta Version 1.0.0, 25-NOV-1991 (WLT) */

/* -& */

/* $ Index_Entry */

/*     Clear the parse META/2 temporary parse tables */

/* -& */

/*     Just in case, we initialize the keepers table if it hasn't been */
/*     initialized already. */

    if (first) {
	first = FALSE_;

/*        Initialize the keepers table. */

	ssizec_(&c__100, names, (ftnlen)32);
	ssizei_(&c__100, ptrs);
	ssizei_(&c__400, vals);
    }

/*     Initialize the accumulation table */

    ssizec_(&c__100, anames, (ftnlen)32);
    ssizei_(&c__100, aptrs);
    ssizei_(&c__400, avals);
    return 0;
/* $Procedure M2KEEP ( META/2 --- Keep temporary parse table values ) */

L_m2keep:
/* $ Abstract */

/*     Copy names/value associations from the temporary (accumulation) */
/*     parse table to the long-term parse table. */

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

/*     META/2 --- a language specification language. */

/* $ Keywords */

/*     PARSING */
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

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine copies values from the temporary (accumulation) */
/*     parse tables into the long-term parse tables used by M2GMCH. */
/*     It should never be called directly by users. */

/* $ Examples */

/*     None.  See the routine M2GMCH for the only instance of use of this */
/*     routine. */

/* $ Restrictions */

/*     User's should not call this routine directly.  It is intended */
/*     only for use as utility for META/2 software. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/* -    Beta Version 1.0.0, 25-NOV-1991 (WLT) */

/* -& */

/* $ Index_Entry */

/*     Keep values in the META/2 temporary parse tables */

/* -& */

/*     For each entry in the accumulation table... */

    i__1 = cardc_(anames, (ftnlen)32);
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Find out its name, */

	syfeti_(&i__, anames, aptrs, avals, myname, &gotit, (ftnlen)32, (
		ftnlen)32);
	if (gotit) {

/*           ...extract the values. */

	    sygeti_(myname, anames, aptrs, avals, &total, temp, &gotit, (
		    ftnlen)32, (ftnlen)32);

/*           and put them in the keepers table. */

	    syputi_(myname, temp, &total, names, ptrs, vals, (ftnlen)32, (
		    ftnlen)32);
	}
    }
    return 0;
/* $Procedure M2VGET ( META/2 --- Get variable ) */

L_m2vget:
/* $ Abstract */

/*     Retrieve the boundaries of the Nth substring word that matches a */
/*     named META/2 template word. */

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

/*     META/2 --- a language specification language */

/* $ Keywords */

/*     PARSING */
/*     RETRIEVAL */

/* $ Declarations */

/*     CHARACTER*(*)         NAME */
/*     INTEGER               NTH */
/*     LOGICAL               FOUND */
/*     INTEGER               B */
/*     INTEGER               E */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     NAME       I   is the name of some variable in the parse table. */
/*     NTH        I   is the number of the substring boundary to get. */
/*     FOUND      O   is returned .TRUE. if requested data can be found. */
/*     B          O   is the beginning index of the matched word. */
/*     E          O   is the ending index of the matched word. */

/* $ Detailed_Input */

/*     NAME       is the name attached to some META/2 template word */
/*                that may have successfully matched a word in a */
/*                string. */

/*     NTH        is the number (in sequence) of the word substring to */
/*                locate that matched the names META/2 template word. */

/* $ Detailed_Output */

/*     FOUND      is .TRUE. if the requested information was available */
/*                in the parse table.  Otherwise it is returned .FALSE. */

/*     B          is the beginning of the word in the string */
/*                corresponding to the requested information. */

/*     E          is the ending of the word in the string corresponding */
/*                to the requested information. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the requested variable is not present in the table or */
/*        the requested substring bounds are not available (for example */
/*        you ask for the 4th word boundaries and there are only */
/*        3 word boundaries) then FOUND will be returned as .FALSE. */
/*        and the values of B and E will be returned unchanged. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Once a string has been matched against a META/2 template, it */
/*     usually must be parsed to determine the information content of */
/*     the string.  By designing the META/2 language so that the needed */
/*     information corresponds to named words of the META/2 template, */
/*     this routine can be used to aid in the parsing of the matched */
/*     string. */

/*     It is intended that this routine not be called very often */
/*     by programmers who make use of the META/2 interface.  More */
/*     convenient high level routines exist that perform the most */
/*     frequently needed parsing functions.  Nevertheless, it may */
/*     sometimes be more convenient to make direct use of this routine. */

/*     META/2 templates allow for "variable length" words such as */
/*     (3:5)@int.  A template that matches a template with such a */
/*     META/2 word will have several words that match the (3:5)@int */
/*     template word.  If the template word is named as in */

/*          (3:5)@int[COEFFICIENTS] */

/*     you can ask for the first, second, third, etc word of the */
/*     string that matched this particular word.  The call below */
/*     will locate the word index boundaries for you. */

/*         CALL M2VGET ( 'COEFFICIENTS', NTH, FOUND, B, E ) */

/*     You will then need to process as needed the string  STRING(B:E) to */
/*     determine the actual information present in the matching string. */

/* $ Examples */

/*     Suppose you wished to collect all of the string words that matched */
/*     the named META/2 template word MYWORDS.  The code below would */
/*     do the job.  (This assumes that you have declared the array */
/*     WORDS to be sufficiently large to hold all of the matching words.) */

/*     C */
/*     C   Start with the first word... */
/*     C */
/*         I = 1 */
/*         CALL M2VGET ( 'MYWORDS', I, FOUND, B, E ) */

/*         DO WHILE ( FOUND ) */

/*            WORDS(I)  =   STRING(B:E) */

/*      C */
/*      C     ... and continue collecting words until no more are found. */
/*      C */
/*            I         =   I + 1 */
/*            CALL M2VGET ( 'MYWORDS', I, FOUND, B, E ) */

/*         END DO */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/* -    Beta Version 1.0.0, 25-NOV-1991 (WLT) */

/* -& */

/* $ Index_Entry */

/*     Get word boundaries for words matching META/2 templates */

/* -& */

/*     Look up any parsed values. */

    *found = FALSE_;
    j = (*nth << 1) - 1;
    k = *nth << 1;
    syseli_(name__, &j, &k, names, ptrs, vals, begend, found, name_len, (
	    ftnlen)32);
    if (*found) {
	*b = begend[0];
	*e = begend[1];
    }

/*     That's all folks.... */

    return 0;
/* $Procedure M2VSIZ ( META/2 --- matched variable template size ) */

L_m2vsiz:
/* $ Abstract */

/*     Determine the size of the collection of words from a string that */
/*     matched a named META/2 template word. */

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

/*     META/2 --- a language specification language. */

/* $ Keywords */

/*     PARSING */

/* $ Declarations */

/*     CHARACTER*(*)         NAME */
/*     INTEGER               SIZE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     NAME       I   The name of some META/2 template word. */
/*     SIZE       O   The number of string words that matched the word. */

/* $ Detailed_Input */

/*     NAME       is the name of some named META/2 template word that a */
/*                string has been matched against. */

/* $ Detailed_Output */

/*     SIZE       is the size (number of members) of the collection of */
/*                words that matched the named META/2 template word */
/*                specified by NAME.  If NAME does not appear in the */
/*                parse table, SIZE will be returned as zero. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This entry point returns the number of words in a string that */
/*     matched a named META/2 template word.  A function, M2HAVE, */
/*     also exists that returns this number and may be more convenient */
/*     in some cases. */

/* $ Examples */

/*     Suppose you wished to collect all of the words that matched */
/*     a META/2 template word with name 'MYWORD'.  You might use */
/*     this entry point to help determine loop boundaries. */

/*           CALL M2VSIZ ( 'MYWORD', SIZE ) */

/*           DO I = 1, SIZE */
/*              CALL M2VGET ( 'MYWORD', I, FOUND, B, E, ) */
/*              WORDS(I)    =  STRING(B:E) */
/*           END DO */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/* -    Beta Version 1.0.0, 26-NOV-1991 (WLT) */

/* -& */

/* $ Index_Entry */

/*     Find the number of words matching a META/2 template word */

/* -& */

/*     Just look up the number of word boundaries and divide by two. */

    total = sydimi_(name__, names, ptrs, vals, name_len, (ftnlen)32);
    *size = total / 2;
    return 0;
} /* m2pars_ */

/* Subroutine */ int m2pars_(char *name__, integer *b, integer *e, integer *
	nth, logical *found, integer *size, ftnlen name_len)
{
    return m2pars_0_(0, name__, b, e, nth, found, size, name_len);
    }

/* Subroutine */ int m2save_(char *name__, integer *b, integer *e, ftnlen 
	name_len)
{
    return m2pars_0_(1, name__, b, e, (integer *)0, (logical *)0, (integer *)
	    0, name_len);
    }

/* Subroutine */ int m2pclr_(void)
{
    return m2pars_0_(2, (char *)0, (integer *)0, (integer *)0, (integer *)0, (
	    logical *)0, (integer *)0, (ftnint)0);
    }

/* Subroutine */ int m2tclr_(void)
{
    return m2pars_0_(3, (char *)0, (integer *)0, (integer *)0, (integer *)0, (
	    logical *)0, (integer *)0, (ftnint)0);
    }

/* Subroutine */ int m2keep_(void)
{
    return m2pars_0_(4, (char *)0, (integer *)0, (integer *)0, (integer *)0, (
	    logical *)0, (integer *)0, (ftnint)0);
    }

/* Subroutine */ int m2vget_(char *name__, integer *nth, logical *found, 
	integer *b, integer *e, ftnlen name_len)
{
    return m2pars_0_(5, name__, b, e, nth, found, (integer *)0, name_len);
    }

/* Subroutine */ int m2vsiz_(char *name__, integer *size, ftnlen name_len)
{
    return m2pars_0_(6, name__, (integer *)0, (integer *)0, (integer *)0, (
	    logical *)0, size, name_len);
    }

