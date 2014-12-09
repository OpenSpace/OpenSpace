/* params.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__13 = 13;
static integer c__80 = 80;
static integer c__8 = 8;
static integer c__0 = 0;
static integer c__3 = 3;
static integer c__6 = 6;
static integer c__10 = 10;
static integer c__5 = 5;
static integer c__1 = 1;

/* $Procedure      PARAMS ( Manage SUBTeX parameters ) */
/* Subroutine */ int params_(char *action, char *name__, integer *value, 
	ftnlen action_len, ftnlen name_len)
{
    /* Initialized data */

    static logical init = FALSE_;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char what[4];
    static integer ptrs[19];
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen);
    static char names[16*19];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    logical found;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    static integer values[19];
    extern /* Subroutine */ int ssizec_(integer *, char *, ftnlen), setmsg_(
	    char *, ftnlen), ssizei_(integer *, integer *), synthi_(char *, 
	    integer *, char *, integer *, integer *, integer *, logical *, 
	    ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int syseti_(char *, integer *, char *, integer *, 
	    integer *, ftnlen, ftnlen);
    char who[17];

/* $ Abstract */

/*     Set or get the values of adjustable SUBTeX parameters. */

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

/*     SUBTeX */

/* $ Keywords */

/*     SUBTeX */

/* $ Declarations */
/* $ Detailed_Input */

/*     ACTION      determines whether the value of the parameter */
/*                 is to be set ('SET') or returned ('GET'). */

/*     NAME        is the name of the parameter to be assigned or */
/*                 queried. Recognized parameters and their default */
/*                 values are listed below. */

/*                    Name                 Default value */
/*                    -----------------    ------------- */
/*                    PAGEWIDTH                       80 */
/*                    LEFTSKIP                         8 */
/*                    RIGHTSKIP                        0 */
/*                    LITERALINDENT                    3 */
/*                    ITEMINDENT                       6 */
/*                    ITEMSKIP                         3 */
/*                    VARNAMESIZE                     10 */
/*                    VARNAMESKIP                      3 */
/*                    VARTYPESIZE                      5 */
/*                    VARTYPESKIP                      3 */
/*                    PARAMNAMESIZE                   10 */
/*                    PARAMNAMESKIP                    3 */
/*                    LISTINDEX                        1 */

/*     VALUE       is the new value to be assigned to the specified */
/*                 parameter whenever ACTION is 'SET'. */

/* $ Detailed_Output */

/*     VALUE       is the current value of the specified parameter */
/*                 whenever ACTION is 'GET'. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ACTION     I   'SET' or 'GET'. */
/*     NAME       I   Parameter name. */
/*     VALUE     I/O  Parameter value. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If ACTION is not recognized, the error 'SUBTeX(BADPARAMACTION)' */
/*        is signalled. */

/*     2) If NAME is not recognized, the error 'SUBTeX(BADPARAMNAME)' */
/*        is signalled. */

/* $ Particulars */

/*     The values of the adjustable parameters are typically set by */
/*     the calling program (FORTeX, for example) according to its */
/*     special needs. The current values are retrieved by SUBTeX as */
/*     required. */

/*     The value of LISTINDEX is reset to one by the control sequence */
/*     @newlist. */

/* $ Examples */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/* $Include SUBTeX.REFS */

/* $ Author_and_Institution */

/*     I.M. Underwood (JPL) */

/* $ Version */

/*     Beta Version 1.0.0, 11-JUN-1988 (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling */

    if (return_()) {
	return 0;
    } else {
	chkin_("PARAMS", (ftnlen)6);
    }

/*     Initialize the table if necessary. */

    if (! init) {
	ssizec_(&c__13, names, (ftnlen)16);
	ssizei_(&c__13, ptrs);
	ssizei_(&c__13, values);
	syseti_("PAGEWIDTH    ", &c__80, names, ptrs, values, (ftnlen)13, (
		ftnlen)16);
	syseti_("LEFTSKIP     ", &c__8, names, ptrs, values, (ftnlen)13, (
		ftnlen)16);
	syseti_("RIGHTSKIP    ", &c__0, names, ptrs, values, (ftnlen)13, (
		ftnlen)16);
	syseti_("LITERALINDENT", &c__3, names, ptrs, values, (ftnlen)13, (
		ftnlen)16);
	syseti_("ITEMINDENT   ", &c__6, names, ptrs, values, (ftnlen)13, (
		ftnlen)16);
	syseti_("ITEMSKIP     ", &c__3, names, ptrs, values, (ftnlen)13, (
		ftnlen)16);
	syseti_("VARNAMESIZE  ", &c__10, names, ptrs, values, (ftnlen)13, (
		ftnlen)16);
	syseti_("VARNAMESKIP  ", &c__3, names, ptrs, values, (ftnlen)13, (
		ftnlen)16);
	syseti_("VARTYPESIZE  ", &c__5, names, ptrs, values, (ftnlen)13, (
		ftnlen)16);
	syseti_("VARTYPESKIP  ", &c__3, names, ptrs, values, (ftnlen)13, (
		ftnlen)16);
	syseti_("PARAMNAMESIZE", &c__10, names, ptrs, values, (ftnlen)13, (
		ftnlen)16);
	syseti_("PARAMNAMESKIP", &c__3, names, ptrs, values, (ftnlen)13, (
		ftnlen)16);
	syseti_("LISTINDEX    ", &c__1, names, ptrs, values, (ftnlen)13, (
		ftnlen)16);
	init = TRUE_;
    }

/*     Shake or bake? */

    ucase_(action, what, action_len, (ftnlen)4);
    ucase_(name__, who, name_len, (ftnlen)17);
    if (s_cmp(what, "SET", (ftnlen)4, (ftnlen)3) == 0) {
	syseti_(who, value, names, ptrs, values, (ftnlen)17, (ftnlen)16);
    } else if (s_cmp(what, "GET", (ftnlen)4, (ftnlen)3) == 0) {
	synthi_(who, &c__1, names, ptrs, values, value, &found, (ftnlen)17, (
		ftnlen)16);
	if (! found) {
	    setmsg_("Trying to retrieve #", (ftnlen)20);
	    errch_("#", who, (ftnlen)1, (ftnlen)17);
	    sigerr_("SUBTeX(BADPARAMNAME)", (ftnlen)20);
	}
    } else {
	setmsg_("Trying to #", (ftnlen)11);
	errch_("#", what, (ftnlen)1, (ftnlen)4);
	sigerr_("SUBTeX(BADPARAMACTION)", (ftnlen)22);
    }
    chkout_("PARAMS", (ftnlen)6);
    return 0;
} /* params_ */

