/* langua.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

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

/* Subroutine */ int langua_0_(int n__, char *string, ftnlen string_len)
{
    /* Initialized data */

    static char lang[32] = "ENGLISH                         ";

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    ljust_(char *, char *, ftnlen, ftnlen);


/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/*     This subroutine is used by CMLOOP to store the language that */
/*     is currently used by the user's program.  You may freely use */
/*     it throughout the rest of your program if you make your */
/*     program language sensitive. */

    switch(n__) {
	case 1: goto L_setlan;
	case 2: goto L_getlan;
	}

    s_copy(string, " ", string_len, (ftnlen)1);
    return 0;

/*     The SETLAN entry point is used for setting the language. */


L_setlan:
    ljust_(string, lang, string_len, (ftnlen)32);
    ucase_(lang, lang, (ftnlen)32, (ftnlen)32);
    return 0;

/*     Use the GETLAN entry point to get the language. */


L_getlan:
    s_copy(string, lang, string_len, (ftnlen)32);
    return 0;
} /* langua_ */

/* Subroutine */ int langua_(char *string, ftnlen string_len)
{
    return langua_0_(0, string, string_len);
    }

/* Subroutine */ int setlan_(char *string, ftnlen string_len)
{
    return langua_0_(1, string, string_len);
    }

/* Subroutine */ int getlan_(char *string, ftnlen string_len)
{
    return langua_0_(2, string, string_len);
    }

