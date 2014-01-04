/* m2xist.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      M2XIST ( META/2 --- does a named template word exist ) */
logical m2xist_(char *name__, ftnlen name_len)
{
    /* System generated locals */
    logical ret_val;

    /* Local variables */
    static integer size;
    extern /* Subroutine */ int m2vsiz_(char *, integer *, ftnlen);

/* $ Abstract */

/*     Determine whether or not a named template word has been matched */
/*     and had the corresponding matching word boundaries stored in */
/*     the META/2 parse tables. */

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

/* $ Detailed_Input */

/*     NAME       is the name of a META/2 template word that may have */
/*                been matched by a call to M2GMCH.  The case of NAME */
/*                is significant.  'BOB', 'Bob', and 'bob' will be */
/*                regarded as different names. */

/* $ Detailed_Output */

/*     M2XIST     is returned .TRUE. if the named template word has */
/*                been stored in the META/2 parse table.  Otherwise */
/*                it is returned .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Once a string has matched some META/2 template, it is often */
/*     necessary to determine which template has been matched or which */
/*     sub-template has been matched before a program can respond */
/*     appropriately.  In some cases, the mere existance of a match */
/*     is sufficient to determine the action a routine should take. */

/*     This routine exists so that you can easily find out whether a */
/*     match for a particular named template word occurred. */

/* $ Examples */

/*     Suppose that a string command was expected to match one of the */
/*     following two templates. */

/*        'FIND SEPARATION[separation] */
/*              (2:2){ OF    @int[body1] @int[body2] */
/*                   | FROM  @int[observer]          }' */

/*        'FIND DISTANCE[distance] */
/*              (2:2){ BETWEEN @int[body1]  @int[body2] */
/*                   | FROM    @int[observer]  }' */

/*     The action a routine will take will depend upon which template */
/*     was actually matched.  But since we know that we have a match */
/*     of one of these templates,  the work of extracting the bodies */
/*     and observer can be common to both types of strings. */


/*           CALL M2GETI ( 'body1',    STRING, FOUND, BODY1 ) */
/*           CALL M2GETI ( 'body2',    STRING, FOUND, BODY2 ) */
/*           CALL M2GETI ( 'observer', STRING, FOUND, OBS   ) */
/*     C */
/*     C     Look up the apparent states of the bodies relative */
/*     C     to the specified observer. */
/*     C */
/*           CALL SPKEZ ( BODY1, ET, 'J2000', 'LT+S', OBS, STATE1, LT ) */
/*           CALL SPKEZ ( BODY2, ET, 'J2000', 'LT+S', OBS, STATE2, LT ) */

/*     C */
/*     C     Now compute the ANSWER based upon whether separation or */
/*     C     distance was specified. */
/*     C */
/*           IF ( M2XIST('separation') ) THEN */

/*              ANSWER = VSEP  ( STATE1, STATE2 ) */

/*           ELSE IF ( M2XIST('distance') ) THEN */

/*              ANSWER = VDIST ( STATE1, STATE2 ) */

/*           END IF */

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


/*     Find out how many endpoints were matched.  The NAME is there */
/*     if SIZE is greater than 0. */

    m2vsiz_(name__, &size, name_len);
    ret_val = size > 0;
    return ret_val;
} /* m2xist_ */

