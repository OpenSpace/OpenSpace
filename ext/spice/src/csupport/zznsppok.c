/* zznsppok.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZNSPPOK (Private Routine -- NSPIO Port) */
integer zznsppok_(char *port, integer *nports, char *ports, ftnlen port_len, 
	ftnlen ports_len)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    integer id;
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);

/* $ Abstract */

/*     Find the integer associated with an NSPIO PORT string. */

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

/*     TEXT */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     PORT       I   is a string indicating the port to find. */
/*     NPORTS     I   is the number of ports in the PORTS array. */
/*     PORTS      I   an array of strings containing the possible ports. */

/*     The function returns an integer that represents the position */
/*     of PORT in the PORTS array. */

/* $ Detailed_Input */

/*     PORT       is the name of a port supported by the NSPIO */
/*                umbrella, and is an entry in the PORTS array. */

/*     NPORTS     is the number of entries in the PORTS arrray. */

/*     PORTS      is a list of acceptable PORTs supported by the */
/*                NSPIO umbrella. */

/* $ Detailed_Output */

/*     The function returns an integer that represents the position */
/*     of PORT in the PORTS array.  This integer is used in NSPIO */
/*     to access information in parallel arrays. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If PORT is not found in the PORTS array, then the error */
/*        NSPIO(UNKNOWNPORT) is signaled. */

/* $ Particulars */

/*     This private routine is simply a place to consolidate the */
/*     PORT to integer code conversion. */

/* $ Examples */

/*     This routine is a simple private routine. See NSPIO and its */
/*     entry points for samples of its usage. */

/* $ Restrictions */

/*     1) NPORTS must not be greater than the number of available */
/*        members of the PORTS array, else memory violation will */
/*        occur. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    NSPIO Version 2.0.0, 01-FEB-2000 (FST) */


/* -& */

/*     SPICELIB Functions */


/*     Local Variables */


/*     Find PORT in the PORTS array. */

    id = isrchc_(port, nports, ports, port_len, ports_len);

/*     Set ZZNSPPOK to the return value. */

    ret_val = id;

/*     Check to see if we were able to find the integer ID of PORT. */
/*     If not, use discovery check in/out and signal an error. */

    if (id == 0) {
	chkin_("ZZNSPPOK", (ftnlen)8);
	setmsg_("$ is an unrecognized port.", (ftnlen)26);
	errch_("$", port, (ftnlen)1, port_len);
	sigerr_("NSPIO(UNKNOWNPORT)", (ftnlen)18);
	chkout_("ZZNSPPOK", (ftnlen)8);
    }
    return ret_val;
} /* zznsppok_ */

