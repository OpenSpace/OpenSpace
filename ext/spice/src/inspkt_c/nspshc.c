/* nspshc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__12 = 12;

/* $Procedure      NSPSHC ( Inspekt Show comments) */
/* Subroutine */ int nspshc_(integer *handle, logical *quit)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    char ch__1[1];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    char line[255];
    integer nrec;
    logical send, some;
    integer null, i__, j;
    extern integer cardc_(char *, ftnlen);
    integer fetch;
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    integer recno, ncomr, toget;
    extern integer ltrim_(char *, ftnlen);
    integer putat;
    char style[132];
    extern logical failed_(void);
    char ifname[60];
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen);
    char crecrd[1024];
    extern /* Subroutine */ int dasioc_(char *, integer *, integer *, char *, 
	    ftnlen, ftnlen);
    char buffer[132*18];
    extern /* Subroutine */ int pagscn_(char *, ftnlen), dashlu_(integer *, 
	    integer *);
    logical didpmt;
    extern /* Subroutine */ int dasrfr_(integer *, char *, char *, integer *, 
	    integer *, integer *, integer *, ftnlen, ftnlen);
    integer daslun;
    char idword[8];
    extern /* Subroutine */ int pagpmt_(logical *, char *, ftnlen);
    integer nresvc;
    extern /* Subroutine */ int ssizec_(integer *, char *, ftnlen), pagput_(
	    char *, ftnlen), nspmrg_(char *, ftnlen), suffix_(char *, integer 
	    *, char *, ftnlen, ftnlen);
    char respns[32];
    integer nresvr;
    extern /* Subroutine */ int nicebt_1__(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen);

/* $ Abstract */

/*     Send the contents of the comment section of a file to */
/*     the Inspekt output ports (via NICEPR). */

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

/*      None. */

/* $ Keywords */

/*       INSPEKT */
/*       COMMENTS */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      HANDLE     I   The handle of a DAS file open for reading. */
/*      QUIT       O   Flag indicating whether user quit reading comments */
/* $ Detailed_Input */

/*     HANDLE      is the handle of a DAS file open for reading. */

/* $ Detailed_Output */

/*     QUIT        is a logical indicating that the user interrupted */
/*                 the output of comments. */

/* $ Parameters */

/*      None. */

/* $ Files */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/*     1) */

/* $ Particulars */

/*     This is a utility program for Inspekt (although with minor */
/*     modifications other programs might find this useful. */

/*     It takes the handle of an open DAS and sends the comments */
/*     a line at at time to the page manager. */

/*     If the page manager has been set up to pause after completion */
/*     of a page, the program determines if the user typed Q indicating */
/*     that s(he)'d like to stop viewing comments. */

/*     If Q is returned by the page manager, this routine abandons */
/*     reading and display of comments for the current file. */

/* $ Examples */

/*     Suppose you had a bunch of DAS files queued up for dumping */
/*     comments, and that you wanted to just send them to */
/*     Inspekt's output devices.  The following shows */
/*     how you could address the problem of a user abandoning */
/*     output. */

/*        DO WHILE ( more DAS files ) */

/*           Get the next DAS file handle */

/*           CALL NSPSHC ( HANDLE, QUIT ) */

/*           IF ( QUIT ) THEN */
/*              set more DAS files to false. */
/*           ELSE */
/*              determine if there are more DAS files */
/*           END IF */

/*        END DO */

/*     You could always get more creative and see if the user */
/*     was abandoning all reading or maybe just the last file. */

/* $ Restrictions */

/*     You must set up the page manager with whatever page size */
/*     and widths you find to be desirable.  However, this routine */
/*     assumes that it can get the current page width from the */
/*     NSPMRG routine and uses this to format strings. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 24-AUG-1995 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Display DAS comments in Inspekt. */

/* -& */

/*     SPICELIB Functions */



/*     First determine the logical unit attached to this DAS and */
/*     get the attributes stored in the file record. */

    dashlu_(handle, &daslun);
    dasrfr_(handle, idword, ifname, &nresvr, &nresvc, &ncomr, &toget, (ftnlen)
	    8, (ftnlen)60);
    *quit = FALSE_;
    null = 0;
    if (failed_()) {
	return 0;
    }

/*     The first comment record is located just after the */
/*     file and reserved records. */

    recno = nresvr + 2;

/*     There is currently nothing to output. */

    s_copy(line, " ", (ftnlen)255, (ftnlen)1);
    putat = 1;
    some = FALSE_;
    send = FALSE_;

/*     Get the current margins and use the null character for the */
/*     hard space. */

    nspmrg_(style, (ftnlen)132);
    suffix_("HARDSPACE", &c__1, style, (ftnlen)9, (ftnlen)132);
    *(unsigned char *)&ch__1[0] = null;
    suffix_(ch__1, &c__1, style, (ftnlen)1, (ftnlen)132);

/*     Finally, all comments go to the body section of the */
/*     page. */

    pagscn_("BODY", (ftnlen)4);
    i__1 = ncomr;
    for (nrec = 1; nrec <= i__1; ++nrec) {
	dasioc_("READ", &daslun, &recno, crecrd, (ftnlen)4, (ftnlen)1024);
	if (failed_()) {
	    return 0;
	}
	++recno;
	fetch = min(1024,toget);
	toget -= fetch;
	i__2 = fetch;
	for (i__ = 1; i__ <= i__2; ++i__) {

/*           Lines in the comment area are delimited by the */
/*           null character (CHAR(0)).  See if we've reached */
/*           the end of a line. */

	    if (*(unsigned char *)&crecrd[i__ - 1] == null) {
		i__3 = ltrim_(line, (ftnlen)255) - 1;
		for (j = 1; j <= i__3; ++j) {
		    *(unsigned char *)&line[j - 1] = (char) null;
		}
		ssizec_(&c__12, buffer, (ftnlen)132);
		nicebt_1__(line, style, buffer, (ftnlen)255, (ftnlen)132, (
			ftnlen)132);
		s_copy(line, " ", (ftnlen)255, (ftnlen)1);
		putat = 1;
		some = FALSE_;
		send = TRUE_;
	    } else if (putat == 132) {

/*              The line might fill up.  If it does we just have */
/*              to shop out what we have. */

		i__3 = ltrim_(line, (ftnlen)255) - 1;
		for (j = 1; j <= i__3; ++j) {
		    *(unsigned char *)&line[j - 1] = (char) null;
		}
		ssizec_(&c__12, buffer, (ftnlen)132);
		nicebt_1__(line, style, buffer, (ftnlen)255, (ftnlen)132, (
			ftnlen)132);
		s_copy(line, " ", (ftnlen)255, (ftnlen)1);
		*(unsigned char *)line = *(unsigned char *)&crecrd[i__ - 1];
		putat = 2;
		some = TRUE_;
		send = TRUE_;
	    } else {

/*              This is what usually happens.  Tack the next character */
/*              on the the end of the line and set the pointer for */
/*              the next place to put things. */

		*(unsigned char *)&line[putat - 1] = *(unsigned char *)&
			crecrd[i__ - 1];
		++putat;
		some = TRUE_;
		send = FALSE_;
	    }
	    if (send) {
		if (cardc_(buffer, (ftnlen)132) == 0) {
		    s_copy(buffer + 792, " ", (ftnlen)132, (ftnlen)1);
		    scardc_(&c__1, buffer, (ftnlen)132);
		}
		i__3 = cardc_(buffer, (ftnlen)132);
		for (j = 1; j <= i__3; ++j) {
		    pagput_(buffer + ((i__4 = j + 5) < 18 && 0 <= i__4 ? i__4 
			    : s_rnge("buffer", i__4, "nspshc_", (ftnlen)326)) 
			    * 132, (ftnlen)132);
		    pagpmt_(&didpmt, respns, (ftnlen)32);
		    ucase_(respns, respns, (ftnlen)32, (ftnlen)32);

/*                 We give the user the chance to bail out of */
/*                 a dump of comments. */

		    if (didpmt && s_cmp(respns, "Q", (ftnlen)32, (ftnlen)1) ==
			     0) {
			*quit = TRUE_;
			return 0;
		    }
		}
	    }
	}
    }

/*     If there are some comments that have been accumulated */
/*     we need to send them to the page manager. */

    if (some) {

/*        We keep the spaces at the beginning of the line */
/*        as hard spaces. */

	i__1 = ltrim_(line, (ftnlen)255) - 1;
	for (j = 1; j <= i__1; ++j) {
	    *(unsigned char *)&line[j - 1] = (char) null;
	}

/*        Use NICEBT to format this line. */

	ssizec_(&c__12, buffer, (ftnlen)132);
	nicebt_1__(line, style, buffer, (ftnlen)255, (ftnlen)132, (ftnlen)132)
		;

/*        Now ship out the contents of BUFFER. */

	i__1 = cardc_(buffer, (ftnlen)132);
	for (j = 1; j <= i__1; ++j) {
	    pagput_(buffer + ((i__2 = j + 5) < 18 && 0 <= i__2 ? i__2 : 
		    s_rnge("buffer", i__2, "nspshc_", (ftnlen)369)) * 132, (
		    ftnlen)132);
	    pagpmt_(&didpmt, respns, (ftnlen)32);
	    ucase_(respns, respns, (ftnlen)32, (ftnlen)32);

/*           We give the user the chance to bail out of */
/*           a dump of comments. */

	    if (didpmt && s_cmp(respns, "Q", (ftnlen)32, (ftnlen)1) == 0) {
		*quit = TRUE_;
		return 0;
	    }
	}
    }
    return 0;
} /* nspshc_ */

