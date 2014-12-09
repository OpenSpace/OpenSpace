/* parcml.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;

/* $Procedure      PARCML ( Parse command line ) */
/* Subroutine */ int parcml_(char *line, integer *nkeys, char *clkeys, 
	logical *clflag, char *clvals, logical *found, char *unprsd, ftnlen 
	line_len, ftnlen clkeys_len, ftnlen clvals_len, ftnlen unprsd_len)
{
    /* System generated locals */
    address a__1[2];
    integer i__1, i__2[2];
    char ch__1[2049];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    static char hkey[2048];
    static integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char hline[2048];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    static integer clidx;
    static char lngwd[2048], uline[2048];
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static integer begpos;
    static char hlngwd[2048];
    static integer pclidx, endpos;
    extern /* Subroutine */ int chkout_(char *, ftnlen), nextwd_(char *, char 
	    *, char *, ftnlen, ftnlen, ftnlen);
    extern logical return_(void);
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);

/* $ Abstract */

/*     Parse a command-line like string in the "key value key value ..." */
/*     format with keys provided in any order and any letter case */
/*     (lower, upper, mixed) and return values of requested keys. */

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

/*     PARSING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LINE      I/O  Input command-line like string. */
/*     NKEYS      I   Number of keys to look for. */
/*     CLKEYS     I   Keys to look for. */
/*     CLFLAG     O   "A particular key found" flags. */
/*     CLVALS     O   Key values. */
/*     FOUND      O   "At least one key found" flag. */
/*     UNPRSD     O   Beginning part of the LINE that was not parsed */
/*     LLNSIZ     P   Size of longest sub-string that can be processed. */

/* $ Detailed_Input */

/*     LINE        is the input command-line like string in the "key */
/*                 value key value ..." format. The line should start */
/*                 with one of the keys provided in CLKEYS as the */
/*                 routine ignores any words before the first recognized */
/*                 key. */

/*                 To avoid limiting the size of the input string that */
/*                 can be processed, this routine uses LINE as the work */
/*                 buffer; it modifies LINE in the process of execution, */
/*                 and sets it to blank before return. */

/*     NKEYS       is the number of keys to look for provided in the */
/*                 CLKEYS array. */

/*     CLKEYS      is an array of keys to look for. Individual keys */
/*                 must be left-justified string consisting of any */
/*                 printable the characters except lower-case letters */
/*                 and blanks. */

/* $ Detailed_Output */

/*     LINE        is set to blank on the output. */

/*     CLFLAG      are the "key found" flags; set to TRUE if */
/*                 corresponding key was found. */

/*     CLVALS      are the key values; if a key wasn't found, its value */
/*                 set to a blank string. */

/*     FOUND       is set to .TRUE. if at least one key was found. */
/*                 Otherwise it is set to .FALSE. */

/*     UNPRSD      is the beginning part of the LINE, preceeding the */
/*                 first recognized key, that was ignored by this */
/*                 routine. */

/* $ Parameters */

/*     LLNSIZ      is the size of the internal buffer that holds a */
/*                 portion of the input string that is being examined. */
/*                 It limits the maximum total length of a front and */
/*                 back blank-padded, blank-separated sub-string */
/*                 containing a key, the value that follows it, and the */
/*                 next key (e.g. ' key value key ') that this routine */
/*                 can correctly process. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine modifies the input string. It returns it set to */
/*     blank. */

/*     The case of the keys in the input string is not significant. */

/*     The order of keys in the input string is not significant. */

/*     If any key appears in the string more than once, only the */
/*     last value of that key is returned. */

/*     The part of the line from the start up to the first recognized */
/*     key is returned in the UNPRSD argument. */

/* $ Examples */

/*     If CLKEYS are */

/*        CLKEYS(1) = '-SETUP' */
/*        CLKEYS(2) = '-TO' */
/*        CLKEYS(3) = '-FROM' */
/*        CLKEYS(4) = '-HELP' */

/*     then: */

/*     line '-setup my.file -FROM utc -TO sclk' */
/*     will be parsed as */

/*        CLFLAG(1) = .TRUE.       CLVALS(1) = 'my.file' */
/*        CLFLAG(2) = .TRUE.       CLVALS(2) = 'utc' */
/*        CLFLAG(3) = .TRUE.       CLVALS(3) = 'sclk' */
/*        CLFLAG(4) = .FALSE.      CLVALS(4) = ' ' */
/*        UNPRSD    = ' ' */
/*        FOUND = .TRUE. */

/*     line '-bogus -setup my.file -FROM utc -TO sclk' */
/*     will be parsed as */

/*        CLFLAG(1) = .TRUE.       CLVALS(1) = 'my.file' */
/*        CLFLAG(2) = .TRUE.       CLVALS(2) = 'utc' */
/*        CLFLAG(3) = .TRUE.       CLVALS(3) = 'sclk' */
/*        CLFLAG(4) = .FALSE.      CLVALS(4) = ' ' */
/*        UNPRSD    = '-bogus' */
/*        FOUND = .TRUE. */

/*     line 'why not -setup my.file -FROM utc -TO sclk' */
/*     will be parsed as */

/*        CLFLAG(1) = .TRUE.       CLVALS(1) = 'my.file' */
/*        CLFLAG(2) = .TRUE.       CLVALS(2) = 'utc' */
/*        CLFLAG(3) = .TRUE.       CLVALS(3) = 'sclk' */
/*        CLFLAG(4) = .FALSE.      CLVALS(4) = ' ' */
/*        UNPRSD    = 'why not' */
/*        FOUND = .TRUE. */

/*     line '-SETUP my.file -setup your.file' */
/*     will be parsed as */

/*        CLFLAG(1) = .TRUE.       CLVALS(1) = 'your.file' */
/*        CLFLAG(2) = .FALSE.      CLVALS(2) = ' ' */
/*        CLFLAG(3) = .FALSE.      CLVALS(3) = ' ' */
/*        CLFLAG(4) = .FALSE.      CLVALS(4) = ' ' */
/*        UNPRSD    = ' ' */
/*        FOUND = .TRUE. */

/*     line '-setup my.file -SeTuP your.file' */
/*     will be parsed as */

/*        CLFLAG(1) = .TRUE.       CLVALS(1) = 'your.file' */
/*        CLFLAG(2) = .FALSE.      CLVALS(2) = ' ' */
/*        CLFLAG(3) = .FALSE.      CLVALS(3) = ' ' */
/*        CLFLAG(4) = .FALSE.      CLVALS(4) = ' ' */
/*        UNPRSD    = ' ' */
/*        FOUND = .TRUE. */

/*     line '-help' */
/*     will be parsed as */

/*        CLFLAG(1) = .FALSE.      CLVALS(1) = ' ' */
/*        CLFLAG(2) = .FALSE.      CLVALS(2) = ' ' */
/*        CLFLAG(3) = .FALSE.      CLVALS(3) = ' ' */
/*        CLFLAG(4) = .TRUE.       CLVALS(4) = ' ' */
/*        UNPRSD    = ' ' */
/*        FOUND = .TRUE. */

/*     and so on. */

/* $ Restrictions */

/*     This routine cannot process input lines with any ' -key value */
/*     -key ' sub-string that is longer than LLNSIZ. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SUPPORT Version 1.0.0, 15-FEB-2012 (BVS) */

/* -& */

/*     Local variables. */


/*     Save everything to prevent potential memory problems in f2c'ed */
/*     version. */


/*     SPICELIB functions. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("PARCML", (ftnlen)6);
    }

/*     Set initial values of keys to blanks and flags to .FALSE. */

    i__1 = *nkeys;
    for (i__ = 1; i__ <= i__1; ++i__) {
	clflag[i__ - 1] = FALSE_;
	s_copy(clvals + (i__ - 1) * clvals_len, " ", clvals_len, (ftnlen)1);
    }
    *found = FALSE_;

/*     Parsing loop. We will set the sub-string buffer HLINE to as many */
/*     characters from the input line as it will fit, starting with the */
/*     initial part of the line on the first iteration and resetting to */
/*     sub-strings starting at the first character of each value after */
/*     the previous key-value pair was processed, and will pick at HLINE */
/*     word by word looking for recognized keys. The loop will */
/*     continue until we reach the end of the string -- all key-value */
/*     pairs were processed and the sub-string buffer HLINE was set to */
/*     blank. */

    s_copy(hline, line, (ftnlen)2048, line_len);
    pclidx = 0;
    clidx = 0;
    s_copy(unprsd, line, unprsd_len, line_len);
    while(s_cmp(hline, " ", (ftnlen)2048, (ftnlen)1) != 0) {

/*        Get next word; uppercase it; look for it in the input keys */
/*        array. */

	nextwd_(hline, lngwd, hline, (ftnlen)2048, (ftnlen)2048, (ftnlen)2048)
		;
	ucase_(lngwd, hlngwd, (ftnlen)2048, (ftnlen)2048);
	clidx = isrchc_(hlngwd, nkeys, clkeys, (ftnlen)2048, clkeys_len);

/*        Is the token that we found a recognized key? */

	if (clidx != 0) {

/*           Yes, it is. Is it the first key that we have found? */

	    if (pclidx != 0) {

/*              No it is not. We need to save the value of the previous */
/*              key. */

/*              Compute the begin and end positions of the sub-string */
/*              that contains the previous value by looking for the */
/*              previous and current keys in the upper-cased remainder of */
/*              the input line. */

/*              The begin position is the position of the previous key */
/*              plus its length. The end position is the position of the */
/*              front-n-back blank-padded current key. */

		ucase_(line, uline, line_len, (ftnlen)2048);
		begpos = pos_(uline, clkeys + (pclidx - 1) * clkeys_len, &
			c__1, (ftnlen)2048, rtrim_(clkeys + (pclidx - 1) * 
			clkeys_len, clkeys_len)) + rtrim_(clkeys + (pclidx - 
			1) * clkeys_len, clkeys_len);
/* Writing concatenation */
		i__2[0] = 1, a__1[0] = " ";
		i__2[1] = rtrim_(clkeys + (clidx - 1) * clkeys_len, 
			clkeys_len), a__1[1] = clkeys + (clidx - 1) * 
			clkeys_len;
		s_cat(hkey, a__1, i__2, &c__2, (ftnlen)2048);
/* Writing concatenation */
		i__2[0] = 2048, a__1[0] = uline;
		i__2[1] = 1, a__1[1] = " ";
		s_cat(ch__1, a__1, i__2, &c__2, (ftnlen)2049);
		endpos = pos_(ch__1, hkey, &begpos, (ftnlen)2049, rtrim_(hkey,
			 (ftnlen)2048) + 1);

/*              Extract the value, left-justify it, and RTRIM it. Set */
/*              "value found" flag to .TRUE. */

		s_copy(clvals + (pclidx - 1) * clvals_len, line + (begpos - 1)
			, clvals_len, endpos - (begpos - 1));
		ljust_(clvals + (pclidx - 1) * clvals_len, clvals + (pclidx - 
			1) * clvals_len, clvals_len, clvals_len);
		s_copy(clvals + (pclidx - 1) * clvals_len, clvals + (pclidx - 
			1) * clvals_len, clvals_len, rtrim_(clvals + (pclidx 
			- 1) * clvals_len, clvals_len));
		clflag[pclidx - 1] = TRUE_;

/*              Check whether we already parsed the whole line. It will */
/*              be so if the remainder of the buffer holding the */
/*              sub-string that we examine word-by-word is a blank */
/*              string. */

		if (s_cmp(hline, " ", (ftnlen)2048, (ftnlen)1) != 0) {

/*                 No, we did not parse the whole line yet. There is */
/*                 more stuff to parse and we reset the temporary */
/*                 sub-string buffer to hold the part of the input string */
/*                 starting with the first character after the current */
/*                 key -- the end position plus the length of the */
/*                 current key. */


		    i__1 = endpos + 1 + rtrim_(clkeys + (clidx - 1) * 
			    clkeys_len, clkeys_len) - 1;
		    s_copy(hline, line + i__1, (ftnlen)2048, line_len - i__1);
		}

/*              Now reset the line to its portion starting with the */
/*              first character of the current key. */

		i__1 = endpos;
		s_copy(line, line + i__1, line_len, line_len - i__1);
	    } else {

/*              This is the first key that we have found. Set UNPRSD */
/*              to the part of the line from the start to this key. */

		ucase_(line, uline, line_len, (ftnlen)2048);
/* Writing concatenation */
		i__2[0] = 1, a__1[0] = " ";
		i__2[1] = rtrim_(clkeys + (clidx - 1) * clkeys_len, 
			clkeys_len), a__1[1] = clkeys + (clidx - 1) * 
			clkeys_len;
		s_cat(hkey, a__1, i__2, &c__2, (ftnlen)2048);
/* Writing concatenation */
		i__2[0] = 1, a__1[0] = " ";
		i__2[1] = 2048, a__1[1] = uline;
		s_cat(ch__1, a__1, i__2, &c__2, (ftnlen)2049);
		begpos = pos_(ch__1, hkey, &c__1, (ftnlen)2049, rtrim_(hkey, (
			ftnlen)2048) + 1);
		if (begpos <= 1) {
		    s_copy(unprsd, " ", unprsd_len, (ftnlen)1);
		} else {
		    s_copy(unprsd, line, unprsd_len, begpos - 1);
		}
	    }

/*           Save the current key index in as previous. */

	    pclidx = clidx;
	}
    }

/*     If we found at least one recognized key, we need to save the last */
/*     value. */

    if (pclidx != 0) {

/*        Set "found any" output flag and "found previous key" flags to */
/*        .TRUE. */

	*found = TRUE_;
	clflag[pclidx - 1] = TRUE_;

/*        Check if there was any value following the last key (there was */
/*        if the non-blank length of what's left in the line starting */
/*        with the last key if greater than the non-blank length of the */
/*        last key). */

	if (rtrim_(line, line_len) > rtrim_(clkeys + (pclidx - 1) * 
		clkeys_len, clkeys_len)) {

/*           Compute begin position of, extract, left justify and */
/*           RTRIM the last value. */

	    ucase_(line, uline, line_len, (ftnlen)2048);
	    begpos = pos_(uline, clkeys + (pclidx - 1) * clkeys_len, &c__1, (
		    ftnlen)2048, rtrim_(clkeys + (pclidx - 1) * clkeys_len, 
		    clkeys_len)) + rtrim_(clkeys + (pclidx - 1) * clkeys_len, 
		    clkeys_len);
	    s_copy(clvals + (pclidx - 1) * clvals_len, line + (begpos - 1), 
		    clvals_len, line_len - (begpos - 1));
	    ljust_(clvals + (pclidx - 1) * clvals_len, clvals + (pclidx - 1) *
		     clvals_len, clvals_len, clvals_len);
	    s_copy(clvals + (pclidx - 1) * clvals_len, clvals + (pclidx - 1) *
		     clvals_len, clvals_len, rtrim_(clvals + (pclidx - 1) * 
		    clvals_len, clvals_len));
	} else {

/*           The key was the last thing on the line. So, it's value is */
/*           blank. */

	    s_copy(clvals + (pclidx - 1) * clvals_len, " ", clvals_len, (
		    ftnlen)1);
	}
    }
    chkout_("PARCML", (ftnlen)6);
    return 0;
} /* parcml_ */

