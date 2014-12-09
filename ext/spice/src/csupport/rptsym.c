/* rptsym.f -- translated by f2c (version 19980913).
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

/* Subroutine */ int rptsym_0_(int n__, integer *id, integer *comp, char *
	string, integer *wdth, char *name__, char *def, char *value, ftnlen 
	string_len, ftnlen name_len, ftnlen def_len, ftnlen value_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern integer rtrim_(char *, ftnlen);
    static char symdef[1000], symnam[32], symval[1000];


/*     This routine is a utility for setting and retrieving symbol */
/*     names, definitions and expanded values.  It is intended that */
/*     this be used by a higher level routine that fetches symbol */
/*     definitions one at a time, puts the definition here and */
/*     passes the routine RETSYM to a formatting routine. */

/*     The ENTRY point SETSYM allows you to set the symbol and its */
/*     values. */

/*     The ENTRY point RETSYM returns the last set values.  To */
/*     request a portion of a symbol you supply the following */
/*     values for ID and COMP */

/*        1,1 for the symbol name */
/*        2,1 for the symbol definition */
/*        2,2 or 3,1 for the symbol expanded value. */

/*     If used with the routine TABRPT you can then easily display */
/*     symbols as: */

/*        name    definition    fully_expanded_value */

/*     or */

/*        name    definition */
/*                fully_expanded_value. */

    switch(n__) {
	case 1: goto L_setsym;
	case 2: goto L_retsym;
	}

    return 0;

L_setsym:
    s_copy(symnam, name__, (ftnlen)32, name_len);
    s_copy(symdef, def, (ftnlen)1000, def_len);
    s_copy(symval, value, (ftnlen)1000, value_len);
    return 0;

L_retsym:
    if (*id == 1) {
	if (*comp != 1) {
	    s_copy(string, " ", string_len, (ftnlen)1);
	} else {
	    s_copy(string, symnam, string_len, (ftnlen)32);
	}
    } else if (*id == 2) {
	if (*comp == 1) {
	    s_copy(string, symdef, string_len, (ftnlen)1000);
	} else if (*comp == 2) {
	    s_copy(string, symval, string_len, (ftnlen)1000);
	} else {
	    s_copy(string, " ", string_len, (ftnlen)1);
	}
    } else if (*id == 3) {
	if (*comp == 1) {
	    s_copy(string, symval, string_len, (ftnlen)1000);
	} else {
	    s_copy(string, " ", string_len, (ftnlen)1);
	}
    }
    *wdth = rtrim_(string, string_len);
    return 0;
} /* rptsym_ */

/* Subroutine */ int rptsym_(integer *id, integer *comp, char *string, 
	integer *wdth, char *name__, char *def, char *value, ftnlen 
	string_len, ftnlen name_len, ftnlen def_len, ftnlen value_len)
{
    return rptsym_0_(0, id, comp, string, wdth, name__, def, value, 
	    string_len, name_len, def_len, value_len);
    }

/* Subroutine */ int setsym_(char *name__, char *def, char *value, ftnlen 
	name_len, ftnlen def_len, ftnlen value_len)
{
    return rptsym_0_(1, (integer *)0, (integer *)0, (char *)0, (integer *)0, 
	    name__, def, value, (ftnint)0, name_len, def_len, value_len);
    }

/* Subroutine */ int retsym_(integer *id, integer *comp, char *string, 
	integer *wdth, ftnlen string_len)
{
    return rptsym_0_(2, id, comp, string, wdth, (char *)0, (char *)0, (char *)
	    0, string_len, (ftnint)0, (ftnint)0, (ftnint)0);
    }

