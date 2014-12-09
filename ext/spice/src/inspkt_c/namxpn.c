/* namxpn.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;

/* $Procedure      NAMXPN ( name expansion ) */
/* Subroutine */ int namxpn_(char *commnd, char *pref, char *error, ftnlen 
	commnd_len, ftnlen pref_len, ftnlen error_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    char name__[64], item[64*2];
    integer comp;
    extern integer cpos_(char *, char *, integer *, ftnlen, ftnlen);
    char part[64*2], word[64];
    integer b, e, i__, j;
    extern /* Subroutine */ int clgac_(integer *, char *, char *, ftnlen, 
	    ftnlen);
    integer n, q, r__;
    extern /* Subroutine */ int lcase_(char *, char *, ftnlen, ftnlen), 
	    clnid_(integer *, integer *, logical *), ucase_(char *, char *, 
	    ftnlen, ftnlen), repmc_(char *, char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen, ftnlen);
    logical found;
    extern /* Subroutine */ int clnum_(integer *);
    extern integer rtrim_(char *, ftnlen);
    integer start;
    logical subst;
    extern logical matchi_(char *, char *, char *, char *, ftnlen, ftnlen, 
	    ftnlen, ftnlen);
    integer matchs;
    char period[1];
    extern /* Subroutine */ int repsub_(char *, integer *, integer *, char *, 
	    char *, ftnlen, ftnlen, ftnlen), suffix_(char *, integer *, char *
	    , ftnlen, ftnlen), nthuqt_(char *, integer *, char *, char *, 
	    integer *, ftnlen, ftnlen, ftnlen);
    integer nid, loc;
    char sub[64*2];
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);

/* $ Abstract */

/*    The routine examines the unquoted words in a string to */
/*    see if any contain wildcard characters.  For each word */
/*    for which such a character is present, the routine */
/*    replaces the word by a matching table/column name. */

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

/*     INSPEKT */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     COMMND     I   A string that may have column patterns. */
/*     PREF       I   Search for table/column if word is unqualified */
/*     COMMND     O   The same string with patterns replaced by names. */
/*     ERROR      O   Blank unless something goes wrong. */

/* $ Detailed_Input */

/*     COMMND     is a string that may be part or all of an INSPEKT */
/*                command.  The string is searched for unquoted */
/*                words that contain wildcard characters '*' and '%'. */

/*     PREF       if a word is not a qualified column name (i.e. */
/*                does not contain a period) PREF indicates how */
/*                to attempt to match it against known items. */

/*                If PREF equals 'COLUMN' unqualified words are */
/*                matched only against known columns.  If PREF */
/*                is equal to 'TABLE', unqualified words are matched */
/*                only against known tables.  If PREF is any other */
/*                value, both table and column names are checked */
/*                for matches. */

/*                The routine is not sensitive to the case of PREF. */

/* $ Detailed_Output */

/*     COMMND     is the input string with patterns replaced by */
/*                matching column names.  If a pattern cannot be */
/*                uniquely matched it is left unchanged.  However, */
/*                this is regarded as an error condition and will */
/*                be diagnosed in ERROR. */

/*     ERROR      is a string indicating whether or not everything */
/*                went well in the attempt to match patterns against */
/*                known column names.  If every pattern could be */
/*                uniquely matched, ERROR is returned blank. Otherwise */
/*                a diagnosis of the problem is returned in ERROR(1). */
/*                ERROR(2) is not altered by this routine. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) If a pattern appears in the input string, but cannot */
/*        be uniquely matched against a known table/column name, a */
/*        diagnosis of the failure to match is recorded in ERROR(1) */

/* $ Particulars */

/*     This is a utility routine for INSPEKT that allows the processing */
/*     of words that are regarded as patterns.  Patterns are assumed */
/*     to have two or more characters.  One of these must be either */
/*     the asterisk '*' or the percent '%' character.  Moreover if a */
/*     period is present in the word, these characters must follow */
/*     the period. */

/*     If such a word is present the portion of it that follows */
/*     a period '.' is matched against the list of known column names. */
/*     If there is a unique match, the portion of the word that follows */
/*     the period is replaced by the matching name. */

/*     Patterns and column names are case insensitive as far as */
/*     matching is concerned. */

/* $ Examples */

/*     Suppose that there are 4 known columns present in the */
/*     column manager: */

/*        EMPLOYEE, MANAGER, EMPLOYEE_SALARY, DEPARTMENT. */

/*     If the query is entered */

/*        SELECT EMP*E, MAN*, *SAL*, DEP* FROM TABLE */
/*        WHERE *SAL* > 10000 */
/*        ORDER BY DEP*, EMP*E; */

/*     It will be expanded to */

/*        SELECT EMPLOYEE, MANAGER, EMPLOYEE_SALARY, DEPARTMENT */
/*        FROM TABLE WHERE EMPLOYEE_SALARY > 10000 ORDER */
/*        BY DEPARTMENT, EMPLOYEE; */

/*     Note that the query */

/*        SELECT EMP*, MAN*, *SAL*, DEP* FROM TABLE; */

/*     Results in an error diagnosis because both 'EMPLOYEE' and */
/*     'EMPLOYEE_SALARY' match the pattern 'EMP*' */


/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*       W.L. Taber      (JPL) */

/* $ Literature_References */

/*       None. */

/* $ Version */

/* -    Inspekt Version 1.0.0, 1-NOV-1995 (WLT) */


/* -& */

/*     SPICELIB Functions */


/*     Inspekt Functions */


/*     Local Variables */


/*     Look up the number of columns that are currently */
/*     active. */

    clnum_(&n);

/*     For each unquoted word in the command see if it contains */
/*     one of the wildcard characters.  If it does we see if there */
/*     is a matching column name. */

    i__ = 1;
    nthuqt_(commnd, &i__, " ", word, &loc, commnd_len, (ftnlen)1, (ftnlen)64);
    while(s_cmp(word, " ", (ftnlen)64, (ftnlen)1) != 0) {

/*        Get the last non-blank character. */

	r__ = rtrim_(word, (ftnlen)64);
	if (r__ > 1) {

/*           We need to take apart the word to see if there */
/*           are one or two components.  We use the period (.) */
/*           to make this determination.  The stuff before */
/*           the period is always a table, the stuff after is */
/*           the column.  If there isn't a period, we let the */
/*           user decide if we need to look in the columns, tables */
/*           or both for resolution of the pattern match. */

	    start = pos_(word, ".", &c__1, (ftnlen)64, (ftnlen)1);
	    b = loc;
	    e = loc + r__ - 1;

/*           Until we learn differently, we assume we don't have */
/*           to perform any substitutions. */

	    subst = FALSE_;
	    if (start == 0) {
		s_copy(part, word, (ftnlen)64, (ftnlen)64);
		*(unsigned char *)period = ' ';
		s_copy(part + 64, " ", (ftnlen)64, (ftnlen)1);
		ucase_(pref, item, pref_len, (ftnlen)64);
		ucase_(pref, item + 64, pref_len, (ftnlen)64);
	    } else if (start == 1) {
		s_copy(part, " ", (ftnlen)64, (ftnlen)1);
		*(unsigned char *)period = '.';
		i__1 = start;
		s_copy(part + 64, word + i__1, (ftnlen)64, 64 - i__1);
		s_copy(item, "TABLE ", (ftnlen)64, (ftnlen)6);
		s_copy(item + 64, "COLUMN", (ftnlen)64, (ftnlen)6);
	    } else if (start < r__) {
		s_copy(part, word, (ftnlen)64, start - 1);
		*(unsigned char *)period = '.';
		i__1 = start;
		s_copy(part + 64, word + i__1, (ftnlen)64, 64 - i__1);
		s_copy(item, "TABLE ", (ftnlen)64, (ftnlen)6);
		s_copy(item + 64, "COLUMN", (ftnlen)64, (ftnlen)6);
	    } else {
		s_copy(part, word, (ftnlen)64, start - 1);
		*(unsigned char *)period = '.';
		s_copy(part + 64, " ", (ftnlen)64, (ftnlen)1);
		s_copy(item, "TABLE ", (ftnlen)64, (ftnlen)6);
		s_copy(item + 64, "COLUMN", (ftnlen)64, (ftnlen)6);
	    }

/*           Now for each of the components, look for */
/*           matching names. (Provide of course that a pattern */
/*           is in fact a template). */

	    for (comp = 1; comp <= 2; ++comp) {
		s_copy(sub + (((i__1 = comp - 1) < 2 && 0 <= i__1 ? i__1 : 
			s_rnge("sub", i__1, "namxpn_", (ftnlen)300)) << 6), 
			part + (((i__2 = comp - 1) < 2 && 0 <= i__2 ? i__2 : 
			s_rnge("part", i__2, "namxpn_", (ftnlen)300)) << 6), (
			ftnlen)64, (ftnlen)64);
		matchs = 0;
		if (cpos_(part + (((i__1 = comp - 1) < 2 && 0 <= i__1 ? i__1 :
			 s_rnge("part", i__1, "namxpn_", (ftnlen)303)) << 6), 
			"%*", &c__1, (ftnlen)64, (ftnlen)2) > 0) {

/*                 Wildcards are present in this component so we */
/*                 regard it as a template. */

/*                 Unless we specifically said that this component */
/*                 should be regarded as a table name, we look in */
/*                 the columns to see if we have a match there. */

		    if (s_cmp(item + (((i__1 = comp - 1) < 2 && 0 <= i__1 ? 
			    i__1 : s_rnge("item", i__1, "namxpn_", (ftnlen)
			    312)) << 6), "TABLE", (ftnlen)64, (ftnlen)5) != 0)
			     {
			i__1 = n;
			for (j = 1; j <= i__1; ++j) {
			    clnid_(&j, &nid, &found);
			    clgac_(&nid, "COLNAM", name__, (ftnlen)6, (ftnlen)
				    64);
			    if (matchi_(name__, part + (((i__2 = comp - 1) < 
				    2 && 0 <= i__2 ? i__2 : s_rnge("part", 
				    i__2, "namxpn_", (ftnlen)317)) << 6), 
				    "*", "%", (ftnlen)64, (ftnlen)64, (ftnlen)
				    1, (ftnlen)1)) {
				if (s_cmp(name__, sub + (((i__2 = comp - 1) < 
					2 && 0 <= i__2 ? i__2 : s_rnge("sub", 
					i__2, "namxpn_", (ftnlen)319)) << 6), 
					(ftnlen)64, (ftnlen)64) != 0) {
				    ++matchs;
				    s_copy(sub + (((i__2 = comp - 1) < 2 && 0 
					    <= i__2 ? i__2 : s_rnge("sub", 
					    i__2, "namxpn_", (ftnlen)321)) << 
					    6), name__, (ftnlen)64, (ftnlen)
					    64);
				    q = rtrim_(name__, (ftnlen)64);
				}
			    }
			}
		    }

/*                 Unless we specifically ask for just columns */
/*                 we look up possible matching taable names. */

		    if (s_cmp(item + (((i__1 = comp - 1) < 2 && 0 <= i__1 ? 
			    i__1 : s_rnge("item", i__1, "namxpn_", (ftnlen)
			    332)) << 6), "COLUMN", (ftnlen)64, (ftnlen)6) != 
			    0) {
			i__1 = n;
			for (j = 1; j <= i__1; ++j) {
			    clnid_(&j, &nid, &found);
			    clgac_(&nid, "TABLE", name__, (ftnlen)5, (ftnlen)
				    64);
			    if (matchi_(name__, part + (((i__2 = comp - 1) < 
				    2 && 0 <= i__2 ? i__2 : s_rnge("part", 
				    i__2, "namxpn_", (ftnlen)338)) << 6), 
				    "*", "%", (ftnlen)64, (ftnlen)64, (ftnlen)
				    1, (ftnlen)1)) {
				if (s_cmp(name__, sub + (((i__2 = comp - 1) < 
					2 && 0 <= i__2 ? i__2 : s_rnge("sub", 
					i__2, "namxpn_", (ftnlen)340)) << 6), 
					(ftnlen)64, (ftnlen)64) != 0) {
				    ++matchs;
				    s_copy(sub + (((i__2 = comp - 1) < 2 && 0 
					    <= i__2 ? i__2 : s_rnge("sub", 
					    i__2, "namxpn_", (ftnlen)342)) << 
					    6), name__, (ftnlen)64, (ftnlen)
					    64);
				    q = rtrim_(name__, (ftnlen)64);
				}
			    }
			}
		    }

/*                 Now see how many matches we had.  If not exactly */
/*                 one, this word can't be expanded.  We regard this */
/*                 as an error since the only unquoted words that */
/*                 can have wild cards in a query are columns */
/*                 and table names. */

		    if (matchs == 0) {
			s_copy(error, "The pattern '#' does not match the na"
				"me of any currently available @s. ", 
				error_len, (ftnlen)71);
		    } else if (matchs > 1) {
			s_copy(error, "The pattern '#' matches more than one"
				" @ name.  The pattern needs to be more speci"
				"fic. ", error_len, (ftnlen)86);
		    }
		    if (matchs != 1) {
			lcase_(item + (((i__1 = comp - 1) < 2 && 0 <= i__1 ? 
				i__1 : s_rnge("item", i__1, "namxpn_", (
				ftnlen)374)) << 6), item + (((i__2 = comp - 1)
				 < 2 && 0 <= i__2 ? i__2 : s_rnge("item", 
				i__2, "namxpn_", (ftnlen)374)) << 6), (ftnlen)
				64, (ftnlen)64);
			repmc_(error, "@", item + (((i__1 = comp - 1) < 2 && 
				0 <= i__1 ? i__1 : s_rnge("item", i__1, "nam"
				"xpn_", (ftnlen)375)) << 6), error, error_len, 
				(ftnlen)1, (ftnlen)64, error_len);
			repmc_(error, "#", part + (((i__1 = comp - 1) < 2 && 
				0 <= i__1 ? i__1 : s_rnge("part", i__1, "nam"
				"xpn_", (ftnlen)376)) << 6), error, error_len, 
				(ftnlen)1, (ftnlen)64, error_len);
			return 0;
		    } else {
			subst = TRUE_;
		    }
		}
	    }

/*           That takes care of all checks for the current word.  If */
/*           we need to make a substitution, now is the time to do it. */

	    if (subst) {
		s_copy(word, sub, (ftnlen)64, (ftnlen)64);
		suffix_(period, &c__0, word, (ftnlen)1, (ftnlen)64);
		suffix_(sub + 64, &c__0, word, (ftnlen)64, (ftnlen)64);
		q = rtrim_(word, (ftnlen)64);
		repsub_(commnd, &b, &e, word, commnd, commnd_len, q, 
			commnd_len);
	    }
	}

/*        Now fetch the next word in the command. */

	++i__;
	nthuqt_(commnd, &i__, " ", word, &loc, commnd_len, (ftnlen)1, (ftnlen)
		64);
    }

/*     For debugging for the moment we write out the command */
/*     so we can see what's going on. */

    return 0;
} /* namxpn_ */

