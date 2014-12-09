/* dafacu.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2000 = 2000;
static integer c__1 = 1;

/* $Procedure DAFACU ( DAF add comments from a logical unit ) */
/* Subroutine */ int dafacu_(integer *comlun, char *begmrk, char *endmrk, 
	logical *insbln, integer *handle, ftnlen begmrk_len, ftnlen 
	endmrk_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    olist o__1;
    cllist cl__1;
    alist al__1;
    inlist ioin__1;

    /* Builtin functions */
    integer f_inqu(inlist *), f_open(olist *);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), f_clos(cllist *), s_rnge(
	    char *, integer, char *, integer), f_rew(alist *);

    /* Local variables */
    integer free;
    char line[1000];
    logical more;
    extern /* Subroutine */ int dafac_(integer *, integer *, char *, ftnlen);
    integer i__, j;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer bward, fward;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    integer ncomr;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    integer nd;
    extern logical failed_(void);
    integer ni;
    extern /* Subroutine */ int readla_(integer *, integer *, integer *, char 
	    *, logical *, ftnlen), dafsih_(integer *, char *, ftnlen);
    char ifname[60];
    extern /* Subroutine */ int dafrfr_(integer *, integer *, integer *, char 
	    *, integer *, integer *, integer *, ftnlen), readln_(integer *, 
	    char *, logical *, ftnlen);
    logical opened;
    static char combuf[1000*2000];
    extern integer lastnb_(char *, ftnlen);
    integer length, intchr;
    extern /* Subroutine */ int errfnm_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen);
    integer numcom;
    extern /* Subroutine */ int chkout_(char *, ftnlen), getlun_(integer *);
    integer iostat;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    integer scrlun;
    extern /* Subroutine */ int writla_(integer *, char *, integer *, ftnlen);
    extern logical return_(void);
    logical eof;

/* $ Abstract */

/*     Add comments to an open binary DAF from an opened text file */
/*     attached to a Fortran logical unit. */

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

/*     None. */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*      COMLUN    I   Logical unit of the open comment text file. */
/*      BEGMRK    I   The begin comments marker to be used. */
/*      ENDMRK    I   The end comments marker to be used. */
/*      INSBLN    I   A flag indicating whether to insert a blank line. */
/*      HANDLE    I   Handle of a DAF opened with write access. */

/* $ Detailed_Input */

/*     COMLUN   The Fortran logical unit of a previously opened text */
/*              file which contains comments that are to be added to */
/*              the comment area of a binary DAF. */

/*     BEGMRK   A marker which identifies the beginning of the comments */
/*              in the comment text file. This marker must appear on a */
/*              line by itself and leading and trailing blanks are not */
/*              significant. The marker is case sensitive. */

/*              The line immediately following this marker is the first */
/*              comment line to be placed into the comment area of the */
/*              binary DAF. */

/*              If the begin marker is blank, BEGMRK .EQ. ' ', then the */
/*              comments are assumed to start at the current location */
/*              in the comment text file. */

/*     ENDMRK   A marker which identifies the end of the comments in the */
/*              comment text file. This marker must appear on a line by */
/*              itself and leading and trailing blanks are not */
/*              significant. The marker is case sensitive. */

/*              The line immediately preceeding this marker is the last */
/*              comment line to be placed into the comment area of the */
/*              binary DAF file. */

/*              If the end marker is blank, ENDMRK .EQ. ' ', then the */
/*              comments are assumed to stop at the end of the comment */
/*              text file. */

/*     INSBLN   A logical flag which indicates whether a blank line is */
/*              to be inserted into the comment area of the binary DAF */
/*              attached to HANDLE before any comments are added to the */
/*              comment area of the file. This is to provide a simple */
/*              mechanism for separating any comments already contained */
/*              in the comment area of a DAF from those comments that */
/*              are being added. */

/*              If the comment area of a binary DAF is empty, the value */
/*              of this flag is not significant, the comments are simply */
/*              be placed into the comment area. */

/*     HANDLE   The file handle for a binary DAF file that has been */
/*              opened with write access. The comments from the text */
/*              file are placed into the comment area of this file. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)   If the input logical unit COMLUN is not positive or there */
/*          is not an opened file attached to it, the error */
/*          SPICE(INVALIDARGUMENT) will be signalled. */

/*     2)   If the INQUIRE on the logical unit to see if there is a */
/*          file attached fails, the error SPICE(INQUIREFAILED) will */
/*          be signalled. */

/*     3)   If the scratch file for temporarily holding the comments */
/*          culled from the text file cannot be opened, then the */
/*          error SPICE(FILEOPENFAILED) will be signalled. */

/*     4)   If a nonprinting ASCII character is encountered in the */
/*          comments, the error SPICE(ILLEGALCHARACTER) will be */
/*          signalled. */

/*     5)   If the begin marker cannot be found in the text file, the */
/*          error SPICE(MARKERNOTFOUND) will be signalled. */

/*     6)   If the end marker cannot be found in the text file, the */
/*          error SPICE(MARKERNOTFOUND) will be signalled. */

/* $ Files */

/*     1)   See parameters COMLUN and HANDLE in the $ Detailed_Inputs */
/*          section. */

/*     2)   A scratch file is used to temporarily hold the comments */
/*          culled from the comment text file. This is so we do not */
/*          have to find the place where we started searching for */
/*          comments in the text file. */

/* $ Particulars */

/*     This routine will place all lines between two specified markers, */
/*     a `begin comments marker' and an `end comments marker,' in a */
/*     text file into the comment area of a binary DAF attached to */
/*     HANDLE. If the `begin comments marker' is blank, then the */
/*     comments are asumed to start at the current location in the */
/*     comment text file attached to COMLUN. If the `end comments */
/*     marker' is blank, then the comments are assumed to stop at the */
/*     end of the comment text file attached to COMLUN. */

/* $ Examples */

/*     We will be using the files `jabber.txt', 'batty.txt', and */
/*     `wndrland.daf' in the examples which follow. */

/*     `wndrland.daf' is a binary DAF file with an empty comment area */
/*                    into which we are going to place the entire file */
/*                    `jabber.txt' and a selected portion of the file */
/*                    `batty.txt'. */

/*     `jabber.txt'   is a text file that is to be placed into the */
/*                    comment area of the binary DAF file `wndrland.daf'. */

/*     `batty.txt'    is a text file from which will have a selected */
/*                    portion of its text placed into the comment area */
/*                    of the binary DAF file `wndrland.daf'. */

/*     Let -BOF- and -EOF- denote the beginning and end of a file, */
/*     respectively. */

/*     The file `jabber.txt' contains: */

/*        -BOF- */
/*                  The Jabberwock */

/*        'Twas brillig, and the slithy toves */
/*              Did gyre and gimble in the wabe; */
/*        All mimsy were the borogoves, */
/*              And the mome raths outgrabe. */

/*        ``Beware the Jabberwock, my son! */
/*              The jaws that bite, the claws that catch!'' */

/*        And as in uffish thought he stood, */
/*              The Jabberwock, with eyes of flame, */
/*        Came whiffling through the tulgey wood, */
/*              And burbled as it came! */

/*        One, two! One, two! And through and through */
/*              The vorpal blade went snicker-snack! */
/*        He left it dead, and with its head */
/*              He went galumphing back. */

/*        ``And hast thou slain the Jabberwock? */
/*              Come to my arms, my beamish boy! */
/*        O frabjous day! Callooh! Callay!'' */
/*              He chortled in his joy. */

/*               Through the Looking-Glass */
/*               Lewis Carroll */
/*        -EOF- */

/*     The file `batty.txt' contains: */

/*        -BOF- */
/*        This file contains a brief poem about bats. */

/*        BEGIN bat poem */
/*        Twinkle, twinkle, little bat! */
/*        How I wonder what you're at! */
/*        Up above the world you fly! */
/*        Like a teatray in the sky. */

/*               Alice's Adventures in Wonderland */
/*               Lewis Carroll */
/*        END bat poem */

/*        And that's that for bats. */
/*        -EOF- */

/*     Let */

/*           JABLUN   be the logical unit for the file `jabber.txt' */
/*           BATLUN   be the logical unit for the file `batty.txt' */
/*     and */
/*           HANDLE   be the DAF handle for the file `wndrland.daf' */

/*     The code fragment */

/*     C */
/*     C      Open the files. */
/*     C */
/*            CALL DAFOPW ( `wndrland.daf', HANDLE ) */
/*            CALL TXTOPR ( `jabber.txt'  , JABLUN ) */
/*            CALL TXTOPR ( `batty.txt'   , BATLUN ) */
/*     C */
/*     C      Initialize the markers for the file `jabber.txt'. We want */
/*     C      to include the entire file, so both markers are blank. */
/*     C */
/*            BEGMRK = ' ' */
/*            ENDMRK = ' ' */
/*            INSBLN = .TRUE. */
/*     C */
/*     C      Add the comments from the file 'jabber.txt' */
/*     C */
/*            CALL DAFACU ( JABLUN, BEGMRK, ENDMRK, INSBLN, HANDLE ) */
/*     C */
/*     C      Initialize the markers for the file `batty.txt'. We want */
/*     C      to include the bat poem only, so we define the begin and */
/*     C      end markere accordingly. */
/*     C */
/*            BEGMRK = 'BEGIN bat poem' */
/*            ENDMRK = 'END bat poem' */
/*            INSBLN = .TRUE. */
/*     C */
/*     C      Add the comments from the file 'batty.txt' */
/*     C */
/*            CALL DAFACU ( BATLUN, BEGMRK, ENDMRK, INSBLN, HANDLE ) */
/*     C */
/*     C      Close the files. */

/*            CLOSE       ( JABLUN ) */
/*            CLOSE       ( BATLUN ) */
/*            CALL DAFCLS ( HANDLE ) */

/*     will create a comment area in `wndrland.daf' which contains: */

/*        -BOC- */
/*                  The Jabberwock */

/*        'Twas brillig, and the slithy toves */
/*              Did gyre and gimble in the wabe; */
/*        All mimsy were the borogoves, */
/*              And the mome raths outgrabe. */

/*        ``Beware the Jabberwock, my son! */
/*              The jaws that bite, the claws that catch!'' */

/*        And as in uffish thought he stood, */
/*              The Jabberwock, with eyes of flame, */
/*        Came whiffling through the tulgey wood, */
/*              And burbled as it came! */

/*        One, two! One, two! And through and through */
/*              The vorpal blade went snicker-snack! */
/*        He left it dead, and with its head */
/*              He went galumphing back. */

/*        ``And hast thou slain the Jabberwock? */
/*              Come to my arms, my beamish boy! */
/*        O frabjous day! Callooh! Callay!'' */
/*              He chortled in his joy. */

/*               Through the Looking-Glass */
/*               Lewis Carroll */

/*        Twinkle, twinkle, little bat! */
/*        How I wonder what you're at! */
/*        Up above the world you fly! */
/*        Like a teatray in the sky. */

/*               Alice's Adventures in Wonderland */
/*               Lewis Carroll */
/*        -EOC- */

/*     where -BOC- and -EOC- represent the beginning and end of the */
/*     comments, respectively. */

/* $ Restrictions */

/*     1) The begin comments marker, BEGMRK, and the end comments marker, */
/*        ENDMRK, must each appear alone on a line in the comment text */
/*        file, if they are not blank. */

/*     2) The maximum length of a text line in a comment file is */
/*        specified by the LINLEN parameter defined below. Currently */
/*        this values is 1000 characters. */

/*     3) The maximum length of a single line comment in the comment */
/*        area is specified by the parameter LINLEN defined below. */
/*        Currently this value is 1000 characters. */

/*     4) This routine uses constants that are specific to the ASCII */
/*        character sequence. The results of using this routine with */
/*        a different character sequence are unpredictable. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer (JPL) */

/* $ Version */

/* -    Support Version 1.3.0, 01-NOV-2006 (NJB) (EDW) */

/*        Changed storage duration of array COMBUF to "saved" to */
/*        prevent memory problems on the PC/Windows/Visual C platform. */

/* -    Support Version 1.2.0, 16-NOV-2001 (BVS) (FST) */

/*        Buffer line size (LINLEN) was increased from 255 to 1000 */
/*        characters to make it consistent the line size in SPC */
/*        routines. */

/*        Removed an unnecesary call to DAFHLU, as this routine */
/*        does not interact with the DAF attached to HANDLE at */
/*        the unit level. */

/* -    Beta Version 1.1.1, 23-JAN-1999 (BVS) */

/*        Buffer size (BUFSIZ) was increases from 22 to 2000 lines. */

/* -    Beta Version 1.1.0, 18-JAN-1996 (KRG) */

/*        Added a test and errors for checking to see whether COMLUN */
/*        was actually attached to an ASCII text file when this routine */
/*        was called. */

/* -    Beta Version 1.0.0, 4-JAN-1993 (KRG) */

/* -& */
/* $ Index_Entries */

/*      add comments from a logical unit to a daf file */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     Set the value for the maximum length of a text line. */


/*     Set the length of a DAF file internal filename. */


/*     Set the size of the comment buffer. */


/*     Maximum and minimum decimal values for the printable ASCII */
/*     characters. */


/*     Local variables */


/*     Saved variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DAFACU", (ftnlen)6);
    }

/*     Verify that the DAF file attached to HANDLE is opened with write */
/*     access. */

    dafsih_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("DAFACU", (ftnlen)6);
	return 0;
    }

/*     Logical units must be positive. If it is not, signal an error. */

    if (*comlun <= 0) {
	setmsg_("# is not a valid logical unit. Logical units must be positi"
		"ve.", (ftnlen)62);
	errint_("#", comlun, (ftnlen)1);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("DAFACU", (ftnlen)6);
	return 0;
    }

/*     Verify that there is an open ASCII text file attached to COMLUN. */

    ioin__1.inerr = 1;
    ioin__1.inunit = *comlun;
    ioin__1.infile = 0;
    ioin__1.inex = 0;
    ioin__1.inopen = &opened;
    ioin__1.innum = 0;
    ioin__1.innamed = 0;
    ioin__1.inname = 0;
    ioin__1.inacc = 0;
    ioin__1.inseq = 0;
    ioin__1.indir = 0;
    ioin__1.infmt = 0;
    ioin__1.inform = 0;
    ioin__1.inunf = 0;
    ioin__1.inrecl = 0;
    ioin__1.innrec = 0;
    ioin__1.inblank = 0;
    iostat = f_inqu(&ioin__1);
    if (iostat != 0) {
	setmsg_("The INQUIRE on logical unit # failed. The value of IOSTAT w"
		"as #.", (ftnlen)64);
	errint_("#", comlun, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(INQUIREFAILED)", (ftnlen)20);
	chkout_("DAFACU", (ftnlen)6);
	return 0;
    }
    if (! opened) {
	setmsg_("There is no open file attached to logical unit #, so no com"
		"ments could be read.", (ftnlen)79);
	errint_("#", comlun, (ftnlen)1);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("DAFACU", (ftnlen)6);
	return 0;
    }

/*     Read the file record of the DAF attached to HANDLE. We get back */
/*     some stuff that we do not use. */

    dafrfr_(handle, &nd, &ni, ifname, &fward, &bward, &free, (ftnlen)60);
    if (failed_()) {
	chkout_("DAFACU", (ftnlen)6);
	return 0;
    }

/*     Compute the number of comment records. */

    ncomr = fward - 2;

/*     Get an available logical unit for the comment scratch file. */

    getlun_(&scrlun);
    if (failed_()) {
	chkout_("DAFACU", (ftnlen)6);
	return 0;
    }

/*     Attempt to open the comment scratch file. */

    o__1.oerr = 1;
    o__1.ounit = scrlun;
    o__1.ofnm = 0;
    o__1.orl = 0;
    o__1.osta = "SCRATCH";
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    iostat = f_open(&o__1);
    if (iostat != 0) {
	setmsg_("Attempt to open a temporary file failed. IOSTAT = #.", (
		ftnlen)52);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(FILEOPENFAILED)", (ftnlen)21);
	chkout_("DAFACU", (ftnlen)6);
	return 0;
    }

/*     Start looking for the begin comment marker. If the begin marker */
/*     is a blank line, then the comments begin on the first line of the */
/*     comment file. Otherwise, the comments begin on the line */
/*     immediately following the line which contains the begin comments */
/*     marker. */

    s_copy(line, " ", (ftnlen)1000, (ftnlen)1);
    eof = FALSE_;
    while(s_cmp(line, begmrk, (ftnlen)1000, begmrk_len) != 0) {
	readln_(comlun, line, &eof, (ftnlen)1000);
	ljust_(line, line, (ftnlen)1000, (ftnlen)1000);
	if (failed_()) {
	    cl__1.cerr = 0;
	    cl__1.cunit = scrlun;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    chkout_("DAFACU", (ftnlen)6);
	    return 0;
	}

/*        If we have encountered the end of file  here, we have a */
/*        problem: We did not find the begin comments marker in the */
/*        text file. So, set an appropriate error message and signal */
/*        the error. don't forget to close the scratch file. */

	if (eof) {
	    cl__1.cerr = 0;
	    cl__1.cunit = scrlun;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    setmsg_("The begin comments marker '#' was not found in the comm"
		    "ent file '#'.", (ftnlen)68);
	    errch_("#", begmrk, (ftnlen)1, begmrk_len);
	    errfnm_("#", comlun, (ftnlen)1);
	    sigerr_("SPICE(MARKERNOTFOUND)", (ftnlen)21);
	    chkout_("DAFACU", (ftnlen)6);
	    return 0;
	}
    }

/*     Begin reading in the comment lines from the comment file, */
/*     placing them a buffer at a time into the temporary file. */
/*     We also scan each line for non printing characters. */

    s_copy(line, " ", (ftnlen)1000, (ftnlen)1);
    if (s_cmp(endmrk, " ", endmrk_len, (ftnlen)1) == 0) {

/*        If the end mark is blank, then we want to go until we hit the */
/*        end of the comment file. */

	while(! eof) {
	    numcom = 0;
	    readla_(comlun, &c__2000, &numcom, combuf, &eof, (ftnlen)1000);
	    if (failed_()) {
		cl__1.cerr = 0;
		cl__1.cunit = scrlun;
		cl__1.csta = 0;
		f_clos(&cl__1);
		chkout_("DAFACU", (ftnlen)6);
		return 0;
	    }

/*           If we got some comments, we need to scan them for non- */
/*           printing characters. */

	    if (numcom > 0) {
		i__1 = numcom;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    length = lastnb_(combuf + ((i__2 = i__ - 1) < 2000 && 0 <=
			     i__2 ? i__2 : s_rnge("combuf", i__2, "dafacu_", (
			    ftnlen)622)) * 1000, (ftnlen)1000);

/*                 Scan the comment line for non printinig characters. */

		    i__2 = length;
		    for (j = 1; j <= i__2; ++j) {

/*                    Check to see that the characters in the buffer */
/*                    are all printing ASCII characters. The bounds */
/*                    for printing ASCII characters are given by */
/*                    MAXPCH and MINPCH, which are defined in the */
/*                    $ Local Parameters section of the header. */

			intchr = *(unsigned char *)&combuf[((i__3 = i__ - 1) <
				 2000 && 0 <= i__3 ? i__3 : s_rnge("combuf", 
				i__3, "dafacu_", (ftnlen)634)) * 1000 + (j - 
				1)];
			if (intchr > 126 || intchr < 32) {
			    cl__1.cerr = 0;
			    cl__1.cunit = scrlun;
			    cl__1.csta = 0;
			    f_clos(&cl__1);
			    setmsg_("A nonprinting character was encountered"
				    " in the comments. Value: #", (ftnlen)65);
			    errint_("#", &intchr, (ftnlen)1);
			    sigerr_("SPICE(ILLEGALCHARACTER)", (ftnlen)23);
			    chkout_("DAFACU", (ftnlen)6);
			    return 0;
			}
		    }
		}

/*              Write the comments to the temporary file. */

		writla_(&numcom, combuf, &scrlun, (ftnlen)1000);
	    }
	    if (failed_()) {
		cl__1.cerr = 0;
		cl__1.cunit = scrlun;
		cl__1.csta = 0;
		f_clos(&cl__1);
		chkout_("DAFACU", (ftnlen)6);
		return 0;
	    }
	}
    } else {

/*        The endmark is non blank, then  we want to go until we find a */
/*        line in the comment file that matches the end mark that was */
/*        entered. */

	more = TRUE_;
	while(more) {
	    numcom = 0;
	    readla_(comlun, &c__2000, &numcom, combuf, &eof, (ftnlen)1000);
	    if (failed_()) {
		cl__1.cerr = 0;
		cl__1.cunit = scrlun;
		cl__1.csta = 0;
		f_clos(&cl__1);
		chkout_("DAFACU", (ftnlen)6);
		return 0;
	    }

/*           Look for ENDMRK in the current buffer, if we got some */
/*           comments. */

	    if (numcom > 0) {
		i__ = 1;
		while(more && i__ <= numcom) {
		    s_copy(line, combuf + ((i__1 = i__ - 1) < 2000 && 0 <= 
			    i__1 ? i__1 : s_rnge("combuf", i__1, "dafacu_", (
			    ftnlen)697)) * 1000, (ftnlen)1000, (ftnlen)1000);
		    ljust_(line, line, (ftnlen)1000, (ftnlen)1000);
		    if (s_cmp(line, endmrk, (ftnlen)1000, endmrk_len) == 0) {
			more = FALSE_;
			numcom = i__ - 1;
		    } else {
			++i__;
		    }
		}
	    }

/*           If we still have some comments, we need to scan them for */
/*           non printing characters. */

	    if (numcom > 0) {
		i__1 = numcom;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    length = lastnb_(combuf + ((i__2 = i__ - 1) < 2000 && 0 <=
			     i__2 ? i__2 : s_rnge("combuf", i__2, "dafacu_", (
			    ftnlen)722)) * 1000, (ftnlen)1000);

/*                 Scan the comment line for non printinig characters. */

		    i__2 = length;
		    for (j = 1; j <= i__2; ++j) {

/*                    Check to see that the characters in the buffer */
/*                    are all printing ASCII characters. The bounds */
/*                    for printing ASCII characters are given by */
/*                    MAXPCH and MINPCH, which are defined in the */
/*                    $ Local Parameters section of the header. */

			intchr = *(unsigned char *)&combuf[((i__3 = i__ - 1) <
				 2000 && 0 <= i__3 ? i__3 : s_rnge("combuf", 
				i__3, "dafacu_", (ftnlen)734)) * 1000 + (j - 
				1)];
			if (intchr > 126 || intchr < 32) {
			    cl__1.cerr = 0;
			    cl__1.cunit = scrlun;
			    cl__1.csta = 0;
			    f_clos(&cl__1);
			    setmsg_("A nonprinting character was encountered"
				    " in the comment buffer. Value: #", (
				    ftnlen)71);
			    errint_("#", &intchr, (ftnlen)1);
			    sigerr_("SPICE(ILLEGALCHARACTER)", (ftnlen)23);
			    chkout_("DAFACU", (ftnlen)6);
			    return 0;
			}
		    }
		}

/*              Write the comments to the temporary file. */

		writla_(&numcom, combuf, &scrlun, (ftnlen)1000);
	    }
	    if (failed_()) {
		cl__1.cerr = 0;
		cl__1.cunit = scrlun;
		cl__1.csta = 0;
		f_clos(&cl__1);
		chkout_("DAFACU", (ftnlen)6);
		return 0;
	    }

/*           If we have encountered the end of file here, we have a */
/*           problem: We did not find the end comments marker in the */
/*           text file. So, set an appropriate error message and */
/*           signal the error. */

	    if (more && eof) {
		cl__1.cerr = 0;
		cl__1.cunit = scrlun;
		cl__1.csta = 0;
		f_clos(&cl__1);
		setmsg_("The end comments marker '#' was not found in the co"
			"mment file '#'.", (ftnlen)66);
		errch_("#", endmrk, (ftnlen)1, endmrk_len);
		errfnm_("#", comlun, (ftnlen)1);
		sigerr_("SPICE(MARKERNOTFOUND)", (ftnlen)21);
		chkout_("DAFACU", (ftnlen)6);
		return 0;
	    }
	}
    }

/*     If we made it to here, we have culled all of the comments out of */
/*     the text file and they were all OK. So we need to add all of the */
/*     comments to the DAF comment area now. */

/*     If we are supposed to insert a blank line to separate the current */
/*     addition from any previously stored comments, and there are */
/*     comments already in the comment area, indicated by NCOMR > 0, then */
/*     we insert the blank line. Otherwise, just add the comments. */

    if (*insbln && ncomr > 0) {
	dafac_(handle, &c__1, " ", (ftnlen)1);
	if (failed_()) {
	    cl__1.cerr = 0;
	    cl__1.cunit = scrlun;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    chkout_("DAFACU", (ftnlen)6);
	    return 0;
	}
    }

/*     Rewind the scratch file to get ready to put the comments into the */
/*     comment area. */

    al__1.aerr = 0;
    al__1.aunit = scrlun;
    f_rew(&al__1);

/*     Begin reading through the scratch file, placing the comment lines */
/*     into the comment area of the DAF file a buffer at a time */

    eof = FALSE_;
    while(! eof) {
	numcom = 0;

/*        Read in a buffer of comment lines. */

	readla_(&scrlun, &c__2000, &numcom, combuf, &eof, (ftnlen)1000);

/*        If we got some, add them to the comment area of the DAF file. */

	if (numcom > 0) {
	    dafac_(handle, &numcom, combuf, (ftnlen)1000);
	}
	if (failed_()) {
	    cl__1.cerr = 0;
	    cl__1.cunit = scrlun;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    chkout_("DAFACU", (ftnlen)6);
	    return 0;
	}
    }

/*     Close the scratch file before exiting, it's the only one we */
/*     opened. */

    cl__1.cerr = 0;
    cl__1.cunit = scrlun;
    cl__1.csta = 0;
    f_clos(&cl__1);
    chkout_("DAFACU", (ftnlen)6);
    return 0;
} /* dafacu_ */

