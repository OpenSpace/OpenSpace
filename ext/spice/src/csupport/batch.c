/* batch.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      BATCH (Tell whether or not a program is in batch mode) */
logical batch_0_(int n__)
{
    /* Initialized data */

    static logical inbtch = FALSE_;

    /* System generated locals */
    logical ret_val;

/* $ Abstract */

/*     This function returns information regarding the interactive */
/*     status of a program.  If BATCH is TRUE the function is considered */
/*     to be in background mode.  If BATCH is FALSE the function is */
/*     considered to be in interactive mode. */

/*     To set a program in batch mode call the entry point SETBAT. */
/*     To set a program in interactive mode call SETMOD */

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

/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 20-NOV-1995 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */

/*     Entry points. */


/*     Local Variable */

    switch(n__) {
	case 1: goto L_setmod;
	case 2: goto L_setbat;
	}

    ret_val = inbtch;
    return ret_val;
/* $Procedure SETMOD (Set the reader to interative mode.) */

L_setmod:
/* $ Abstact */
/*     Set NXTCOM to interactive mode.  In puts that are expected to */
/*     come from the keyboard generate an result in a prompt for input */
/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */

    inbtch = FALSE_;
    ret_val = TRUE_;
    return ret_val;
/* $Procedure SETBAT (Set the reader to interative mode.) */

L_setbat:
/* $ Abstact */
/*     Set NXTCOM to interactive mode.  In puts that are expected to */
/*     come from the keyboard generate an result in a prompt for input */
/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */

    inbtch = TRUE_;
    ret_val = TRUE_;
    return ret_val;
} /* batch_ */

logical batch_(void)
{
    return batch_0_(0);
    }

logical setmod_(void)
{
    return batch_0_(1);
    }

logical setbat_(void)
{
    return batch_0_(2);
    }

