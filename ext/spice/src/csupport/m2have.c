/* m2have.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      M2HAVE ( META/2 --- How many matches do we have ) */
integer m2have_(char *name__, ftnlen name_len)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static integer size;
    extern /* Subroutine */ int m2vsiz_(char *, integer *, ftnlen);

/* $ Abstract */

/*     Find the number of matches there were for a particular named */
/*     META/2 template word. */

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

/*     META/2 */
/*     PARSING */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     NAME       I   The name of a META/2 template word. */

/*     M2HAVE is returned with the number of words that matched the */
/*     template word specified by NAME. */

/* $ Detailed_Input */

/*     NAME       is the name of a META/2 template word that may have */
/*                been matched by a call to M2GMCH.  The case of NAME */
/*                is significant.  'BOB', 'Bob', and 'bob' will be */
/*                regarded as different names. */

/* $ Detailed_Output */

/*     M2HAVE     is the number of matches that were made agains */
/*                the named META/2 template word specified by NAME. */
/*                If there were no matches, M2HAVE is returned as zero. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Once a string has matched some META/2 template, one normally */
/*     needs to retrieve the information in the string.  In some cases */
/*     the META/2 template will allow for a variable number of */
/*     matches with a particular template word.  To extract the */
/*     information in the string, it is necessary to determine how many */
/*     words matched the template word in question. */

/*     This routine exists so that you can easily find out how many */
/*     matches there were for a particular named template word. */

/* $ Examples */

/*     Suppose that a string is known to have matched the following */
/*     META/2 template. */

/*        FIND UNION OF (2:)@name[sets] */

/*     To accurately carry out the task specified by this string, */
/*     you will need to find the "names" of the sets specified. */

/*        NSETS = M2HAVE('sets') */

/*        CALL M2GETC ( 'sets', STRING, 1, FOUND, NAME ) */

/*           copy the named set into the set UNION. */

/*        DO I = 2, NSETS */

/*           CALL M2GETC ( 'sets', STRING, I, FOUND, NAME ) */

/*           form the union of UNION with the set specified by NAME */

/*        END DO */

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


/* -    Beta Version 1.0.0, 27-NOV-1991 (WLT) */

/* -& */

/* $ Index_Entry */

/*     Check for the presence of a named match in the META/2 tables. */

/* -& */

/*     Local variables */


/*     Find out how many endpoints were matched, and put the answer into */
/*     M2HAVE. */

    m2vsiz_(name__, &size, name_len);
    ret_val = size;
    return ret_val;
} /* m2have_ */

