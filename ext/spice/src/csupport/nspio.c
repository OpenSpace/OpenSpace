/* nspio.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__8 = 8;
static integer c__6 = 6;

/* $Procedure NSPIO (Inspekt I/O Manager) */
/* Subroutine */ int nspio_0_(int n__, char *line, char *port, char *name__, 
	logical *status, logical *ok, ftnlen line_len, ftnlen port_len, 
	ftnlen name_len)
{
    /* Initialized data */

    static char ports[32*8] = "SCREEN                          " "LOG       "
	    "                      " "SAVE                            " "UTIL"
	    "ITY                         " "ERROR                           " 
	    "AUX1                            " "AUX2                        "
	    "    " "AUX3                            ";
    static char files[255*8] = "                                            "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                   " "                                          "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                     " "                                        "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                       " "                                      "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                         " "                                    "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                           " "                                  "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                             " "                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                               " "                              "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                 ";
    static integer units[8] = { 6,0,0,0,0,0,0,0 };
    static logical active[8] = { TRUE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,
	    FALSE_,FALSE_ };
    static logical open[8] = { TRUE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,
	    FALSE_,FALSE_ };
    static logical suspnd[8] = { FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,
	    FALSE_,FALSE_ };
    static logical erropf = FALSE_;

    /* System generated locals */
    integer i__1, i__2, i__3;
    cllist cl__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), f_clos(cllist *);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern integer zznsppok_(char *, integer *, char *, ftnlen, ftnlen);
    static integer r__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), zztxtopn_(char *, 
	    integer *, logical *, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    static integer id;
    extern logical failed_(void);
    static integer to;
    static char messge[400];
    static logical openok;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), trnlat_(char *, char *, ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int writln_(char *, integer *, ftnlen), txtopn_(
	    char *, integer *, ftnlen);

/* $ Abstract */

/*    Manage file and screen logging information for Inspekt. */

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
/*     LINE       I   NSPWLN */
/*     PORT       I   NSPOPN, NSPIOH, NSPIOA, NSPGST, NSPPST, NSPIOC */
/*                    NSPIOS, NSPIOR, NSPPFL */
/*     NAME      I/O  NSPOPN, NSPPFL */
/*     STATUS    I/O  NSPGST, NSPPST */
/*     OK         O   NSPIOR */

/* $ Detailed_Input */

/*     LINE       is a string of text that is to be written to all the */
/*                open, active, non-suspended ports. */

/*     PORT       is a string that indicates the name of a port on which */
/*                to perform an operation.  Acceptable values are: */

/*                 Standard Output Port: */

/*                   'SCREEN' */

/*                 File Based Ports: */

/*                   'LOG' */
/*                   'SAVE' */
/*                   'UTILITY' */
/*                   'ERROR' */
/*                   'AUX1' */
/*                   'AUX2' */
/*                   'AUX3' */

/*     NAME       The name of a file to create and attach to a file */
/*                based port. */

/*     STATUS     An array of logicals that configures the status of */
/*                a port.  Acceptable values are as follows: */

/*                   STATUS(1) - Activity Status: */
/*                               .TRUE.  - the port is active */
/*                               .FALSE. - the port is inactive */

/*                   STATUS(2) - Open Status: */
/*                               .TRUE.  - the port is open */
/*                               .FALSE. - the port is closed */

/*                   STATUS(3) - Suspend Status: */
/*                               .TRUE.  - I/O on this port is suspended */
/*                               .FALSE. - I/O can proceed on this port */

/* $ Detailed_Output */

/*     NAME       The name of a file attached to a file based port. */

/*     STATUS     An array of logicals that describes the status of */
/*                a port.  A description of the values follows: */

/*                   STATUS(1) - Activity Status: */
/*                               .TRUE.  - the port is active */
/*                               .FALSE. - the port is inactive */

/*                   STATUS(2) - Open Status: */
/*                               .TRUE.  - the port is open */
/*                               .FALSE. - the port is closed */

/*                   STATUS(3) - Suspend Status: */
/*                               .TRUE.  - I/O on this port is suspended */
/*                               .FALSE. - I/O can proceed on this port */

/*     OK         is a logical that indicates whether the attempt to */
/*                reopen a suspended port succeeded. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     1) This umbrella may be configured to simultaneously access */
/*        NPORTS number of files.  They are all opened using the */
/*        SPICELIB routine TXTOPN. */

/* $ Exceptions */

/*     1) If the bogus entry point NSPIO is called directly, then the */
/*        error NSPIO(BOGUSENTRY) is signalled. */

/*     2) See entry points «Entry Points» for exceptions specific to */
/*        them. */

/* $ Particulars */

/*     NSPIO is an umbrella that functions as an I/O manager.  It */
/*     is capable of interfacing with STDOUT as well as several */
/*     files at once.  To accomplish these management tasks, the */
/*     following entry points are provided: */

/*        NSPOPN - Open a port. */

/*        NSPIOH - Inhibit access to a port. */
/*        NSPIOA - Activate an inhibited port. */

/*        NSPGST - Get the status of a port. */
/*        NSPPST - Put the status of a port. */

/*        NSPIOS - Suspend access to a port. */
/*        NSPIOR - Reopen a suspended port. */

/*        NSPWLN - Write a line of text to all accessible ports. */

/*        NSPEND - Close all ports and reset the state of the I/O */
/*                 manager to the default. */

/*        NSPPFL - Retrieve the name of the file associated with a port. */

/*        NSPIOC - Close a port. */

/*     The following ports are provided for usage: */

/*        Standard Output Port: */

/*          'SCREEN' */

/*        File Based Ports: */

/*          'LOG' */
/*          'SAVE' */
/*          'UTILITY' */
/*          'ERROR' */
/*          'AUX1' */
/*          'AUX2' */
/*          'AUX3' */

/*     By default the SCREEN port is open and ready to receive lines */
/*     of text.  All of the file based ports are closed until opened */
/*     with NSPOPN. */

/*     NSPEND is provided to close all open ports and reset the I/O */
/*     manager back to its default state.  If the SCREEN port is */
/*     accessible for writing, then when the LOG, SAVE, and ERROR ports */
/*     are closed a message indicating where they may be found is */
/*     written to the screen port.  The ERROR port is a special case, */
/*     since if it was unsuccessfully opened, when NSPEND attempts to */
/*     close this port it writes a brief diagnostic indicating the */
/*     open failure. */

/*     The suspend and reopen entry points are provided for backwards */
/*     compatibility and should not be used in developing new code. */

/* $ Examples */

/*     See INSPEKT for examples. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */
/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    NSPIO Version 2.0.0, 01-FEB-2000 (FST) */

/*        Added headers and ports ERROR, AUX1, AUX2, and AUX3. */

/* -    NSPIO Version 1.0.0, 15-ARP-1996 (WLT) */


/* -& */

/*     SPICELIB Functions */


/*     Other Functions */


/*     Local Parameters */


/*     Error File Port Integer Code. */


/*     Log File Port Integer Code. */


/*     The number of total ports supported by this version of NSPIO. */


/*     The logical unit that is associated with STDOUT. */


/*     The maximum filename string length. */


/*     The maximum length of a message. */


/*     The maximum length of a word. */


/*     Spool Port Integer Code. */


/*     Screen Port Integer Code. */


/*     Local Variables */


/*     Save all local variables. */


/*     Initialize the PORT configuration arrays. */

    /* Parameter adjustments */
    if (status) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_nspopn;
	case 2: goto L_nspioh;
	case 3: goto L_nspioa;
	case 4: goto L_nspgst;
	case 5: goto L_nsppst;
	case 6: goto L_nspioc;
	case 7: goto L_nspios;
	case 8: goto L_nspior;
	case 9: goto L_nspwln;
	case 10: goto L_nspend;
	case 11: goto L_nsppfl;
	}


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("NSPIO", (ftnlen)5);
	sigerr_("NSPIO(BOGUSENTRY)", (ftnlen)17);
	chkout_("NSPIO", (ftnlen)5);
    }
    return 0;
/* $Procedure NSPOPN ( Inspekt I/O Manager -- Open Port ) */

L_nspopn:
/* $ Abstract */

/*     Open a new port. */

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

/*     CHARACTER*(*)         PORT */
/*     CHARACTER*(*)         NAME */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     PORT       I   String specifying which port to open. */
/*     NAME       I   The name of the file to open and attach to PORT. */

/* $ Detailed_Input */

/*     PORT       is a string that indicates the name of a port on which */
/*                to perform an operation.  Acceptable values are: */

/*                 Standard Output Port: */

/*                   'SCREEN' */

/*                 File Based Ports: */

/*                   'LOG' */
/*                   'SAVE' */
/*                   'UTILITY' */
/*                   'ERROR' */
/*                   'AUX1' */
/*                   'AUX2' */
/*                   'AUX3' */

/*     NAME       The name of a file to create and attach to a file */
/*                based port. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See NSPIO. */

/* $ Files */

/*     1) If PORT is a file-based port then this routine will open */
/*        a file with the SPICE routine TXTOPN. */

/*     2) If PORT is already attached to a file, then this file */
/*        is closed before PORT is attached to a new file. */

/* $ Exceptions */

/*     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT) */
/*        is signaled by ZZNSPPOK.  Note, in this case, the status of */
/*        all ports remains the same. */

/*     2) If PORT is file based and already open, then NSPOPN closes */
/*        the file attached to PORT and opens the requested new one. */

/*     3) If PORT is 'SCREEN', then this entry point does nothing. */

/*     4) If PORT is 'ERROR', then if an error occurs opening the */
/*        file, this routine simply leaves the port unopen and */
/*        returns. */

/*     4) Any errors that occur in opening the files not associated with */
/*        the 'SCREEN' and 'ERROR' ports are processed by TXTOPN. */

/* $ Particulars */

/*     See NSPIO. */

/* $ Examples */

/*     See NSPIO. */

/* $ Restrictions */

/*     1) NAME should point to a non-existant file that can be opened */
/*        for write access. */

/*     2) NAME should be a string of less than SIZFIL characters in */
/*        length. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    NSPIO Version 2.0.0, 01-FEB-2000 (FST) */


/* -& */

/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("NSPOPN", (ftnlen)6);
    }

/*     Find the integer associated with PORT. */

    id = zznsppok_(port, &c__8, ports, port_len, (ftnlen)32);

/*     See if an error has been signaled. If so, do nothing */
/*     further and return. */

    if (failed_()) {
	chkout_("NSPOPN", (ftnlen)6);
	return 0;
    }

/*     First check to see whether we are dealing with the SCREEN */
/*     port.  If we are return, do nothing and return. */

    if (id == 1) {
	chkout_("NSPOPN", (ftnlen)6);
	return 0;
    }

/*     Now at this point we have a request to open a file based */
/*     port.  Check first to see if it is already open. */

    if (open[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("open", i__1, 
	    "nspio_", (ftnlen)540)]) {

/*        If the file attached to PORT is already open, close it */
/*        before attaching this new file to it. */

	cl__1.cerr = 0;
	cl__1.cunit = units[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "nspio_", (ftnlen)546)];
	cl__1.csta = 0;
	f_clos(&cl__1);

/*        Now reset PORT's status. */

	active[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("active", 
		i__1, "nspio_", (ftnlen)551)] = FALSE_;
	open[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("open", i__1, 
		"nspio_", (ftnlen)552)] = FALSE_;
	suspnd[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("suspnd", 
		i__1, "nspio_", (ftnlen)553)] = FALSE_;
	s_copy(files + ((i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge(
		"files", i__1, "nspio_", (ftnlen)554)) * 255, " ", (ftnlen)
		255, (ftnlen)1);
    }

/*     Check to see if we are opening the ERROR port.  We treat */
/*     this port differently from the other file based ports, since */
/*     if an error occurs opening the file, no error is signaled. */
/*     The port is simply not opened. */

    if (id == 5) {

/*        Assume there is will be no error opening the file. */

	erropf = FALSE_;

/*        Attempt to open the file. */

	r__ = rtrim_(name__, name_len);
	zztxtopn_(name__, &units[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : 
		s_rnge("units", i__1, "nspio_", (ftnlen)575)], &openok, r__);

/*        If the OPEN process failed, then clear the status of the */
/*        port and return. */

	if (! openok) {
	    active[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("active", 
		    i__1, "nspio_", (ftnlen)583)] = FALSE_;
	    open[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("open", 
		    i__1, "nspio_", (ftnlen)584)] = FALSE_;
	    suspnd[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("suspnd", 
		    i__1, "nspio_", (ftnlen)585)] = FALSE_;

/*           Leave FILES(ID) set, so that the name of the file can */
/*           be reported. */

	    s_copy(files + ((i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge(
		    "files", i__1, "nspio_", (ftnlen)592)) * 255, name__, (
		    ftnlen)255, name_len);

/*           Before returning, set ERROPF to .TRUE., since */
/*           this will facilitate the creation of the warning */
/*           message when NSPEND is invoked. */

	    erropf = TRUE_;
	    chkout_("NSPOPN", (ftnlen)6);
	    return 0;
	}

/*     Consider all other file based ports.  For these ports we will */
/*     signal errors if TXTOPN is incapable of opening the file. */

    } else {

/*        Open the new file. */

	r__ = rtrim_(name__, name_len);
	txtopn_(name__, &units[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : 
		s_rnge("units", i__1, "nspio_", (ftnlen)615)], r__);

/*        Check FAILED(). If an error has occurred, clear PORT status, */
/*        check out and return. */

	if (failed_()) {
	    active[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("active", 
		    i__1, "nspio_", (ftnlen)623)] = FALSE_;
	    open[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("open", 
		    i__1, "nspio_", (ftnlen)624)] = FALSE_;
	    suspnd[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("suspnd", 
		    i__1, "nspio_", (ftnlen)625)] = FALSE_;
	    s_copy(files + ((i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge(
		    "files", i__1, "nspio_", (ftnlen)626)) * 255, " ", (
		    ftnlen)255, (ftnlen)1);
	    chkout_("NSPOPN", (ftnlen)6);
	    return 0;
	}
    }

/*     If we made it this far, then the file was opened successfully. */
/*     Set PORT status to reflect successful open. */

    active[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("active", i__1, 
	    "nspio_", (ftnlen)638)] = TRUE_;
    open[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("open", i__1, "nsp"
	    "io_", (ftnlen)639)] = TRUE_;
    suspnd[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("suspnd", i__1, 
	    "nspio_", (ftnlen)640)] = FALSE_;
    s_copy(files + ((i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("files", 
	    i__1, "nspio_", (ftnlen)641)) * 255, name__, (ftnlen)255, 
	    name_len);
    chkout_("NSPOPN", (ftnlen)6);
    return 0;
/* $Procedure NSPIOH ( Inspekt I/O Manager -- Inhibit Port ) */

L_nspioh:
/* $ Abstract */

/*     Inhibit output to a port. */

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

/*     CHARACTER*(*)         PORT */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     PORT       I   String specifying which port to inhibit. */

/* $ Detailed_Input */

/*     PORT       is a string that indicates the name of a port on which */
/*                to perform an operation.  Acceptable values are: */

/*                 Standard Output Port: */

/*                   'SCREEN' */

/*                 File Based Ports: */

/*                   'LOG' */
/*                   'SAVE' */
/*                   'UTILITY' */
/*                   'ERROR' */
/*                   'AUX1' */
/*                   'AUX2' */
/*                   'AUX3' */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See NSPIO. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT) */
/*        is signaled by ZZNSPPOK. */

/*     2) If PORT is already inhibited, then it remains inhibited. */

/* $ Particulars */

/*     See NSPIO. */

/* $ Examples */

/*     See NSPIO. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    NSPIO Version 2.0.0, 01-FEB-2000 (FST) */


/* -& */

/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("NSPIOH", (ftnlen)6);
    }

/*     Find the integer associated with PORT. */

    id = zznsppok_(port, &c__8, ports, port_len, (ftnlen)32);

/*     Inhibit I/O to the port, if no error was signaled. Note - if */
/*     the port is already inhibited, then this does not change it's */
/*     state. */

    if (! failed_()) {
	active[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("active", 
		i__1, "nspio_", (ftnlen)787)] = FALSE_;
    }
    chkout_("NSPIOH", (ftnlen)6);
    return 0;
/* $Procedure NSPIOA ( Inspekt I/O Manager -- Activate Port ) */

L_nspioa:
/* $ Abstract */

/*     Activate a port. */

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

/*     CHARACTER*(*)         PORT */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     PORT       I   String specifying which port to activate. */

/* $ Detailed_Input */

/*     PORT       is a string that indicates the name of a port on which */
/*                to perform an operation.  Acceptable values are: */

/*                 Standard Output Port: */

/*                   'SCREEN' */

/*                 File Based Ports: */

/*                   'LOG' */
/*                   'SAVE' */
/*                   'UTILITY' */
/*                   'ERROR' */
/*                   'AUX1' */
/*                   'AUX2' */
/*                   'AUX3' */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See NSPIO. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT) */
/*        is signaled by ZZNSPPOK. */

/*     2) If PORT is already active, then PORT remains active. */

/* $ Particulars */

/*     See NSPIO. */

/* $ Examples */

/*     See NSPIO. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    NSPIO Version 2.0.0, 02-FEB-2000 (FST) */


/* -& */

/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("NSPIOA", (ftnlen)6);
    }

/*     Find the integer associated with PORT. */

    id = zznsppok_(port, &c__8, ports, port_len, (ftnlen)32);

/*     Activate the port, if no error was signaled. Note - if PORT was */
/*     already activated, then it will remain activated. */

    if (! failed_()) {
	active[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("active", 
		i__1, "nspio_", (ftnlen)934)] = TRUE_;
    }
    chkout_("NSPIOA", (ftnlen)6);
    return 0;
/* $Procedure NSPGST ( Inspekt I/O Manager -- Get Port Status ) */

L_nspgst:
/* $ Abstract */

/*     Get the current status of a port. */

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

/*     CHARACTER*(*)         PORT */
/*     LOGICAL               STATUS ( 3 ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     PORT       I   String specifying which port to fetch status. */
/*     STATUS     O   An array of logicals that indicates port status. */

/* $ Detailed_Input */

/*     PORT       is a string that indicates the name of a port on which */
/*                to perform an operation.  Acceptable values are: */

/*                 Standard Output Port: */

/*                   'SCREEN' */

/*                 File Based Ports: */

/*                   'LOG' */
/*                   'SAVE' */
/*                   'UTILITY' */
/*                   'ERROR' */
/*                   'AUX1' */
/*                   'AUX2' */
/*                   'AUX3' */

/* $ Detailed_Output */

/*     STATUS     An array of logicals that describes the status of */
/*                a port.  A description of the values follows: */

/*                   STATUS(1) - Activity Status: */
/*                               .TRUE.  - the port is active */
/*                               .FALSE. - the port is inactive */

/*                   STATUS(2) - Open Status: */
/*                               .TRUE.  - the port is open */
/*                               .FALSE. - the port is closed */

/*                   STATUS(3) - Suspend Status: */
/*                               .TRUE.  - I/O on this port is suspended */
/*                               .FALSE. - I/O can proceed on this port */


/* $ Parameters */

/*     See NSPIO. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT) */
/*        is signaled by ZZNSPPOK.  In the event this happens */
/*        the routine does not alter the contents of STATUS. */

/* $ Particulars */

/*     See NSPIO. */

/* $ Examples */

/*     See NSPIO. */

/* $ Restrictions */

/*     1) STATUS must be an array with space for 3 logicals. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    NSPIO Version 2.0.0, 03-FEB-2000 (FST) */


/* -& */

/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("NSPGST", (ftnlen)6);
    }

/*     Find the integer associated with PORT. */

    id = zznsppok_(port, &c__8, ports, port_len, (ftnlen)32);

/*     Return the status of the port if no error was signaled. */

    if (! failed_()) {
	status[0] = active[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge(
		"active", i__1, "nspio_", (ftnlen)1094)];
	status[1] = open[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge(
		"open", i__1, "nspio_", (ftnlen)1095)];
	status[2] = suspnd[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge(
		"suspnd", i__1, "nspio_", (ftnlen)1096)];
    }
    chkout_("NSPGST", (ftnlen)6);
    return 0;
/* $Procedure NSPPST ( Inspekt I/O Manager -- Put Port Status ) */

L_nsppst:
/* $ Abstract */

/*     Put the status of a port. */

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

/*     CHARACTER*(*)         PORT */
/*     LOGICAL               STATUS ( 3 ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     PORT       I   String specifying which port to receive status. */
/*     STATUS     O   An array of logicals that indicates port status. */

/* $ Detailed_Input */

/*     PORT       is a string that indicates the name of a port on which */
/*                to perform an operation.  Acceptable values are: */

/*                 Standard Output Port: */

/*                   'SCREEN' */

/*                 File Based Ports: */

/*                   'LOG' */
/*                   'SAVE' */
/*                   'UTILITY' */
/*                   'ERROR' */
/*                   'AUX1' */
/*                   'AUX2' */
/*                   'AUX3' */

/*     STATUS     An array of logicals that describes the status of */
/*                a port.  A description of the values follows: */

/*                   STATUS(1) - Activity Status: */
/*                               .TRUE.  - the port is active */
/*                               .FALSE. - the port is inactive */

/*                   STATUS(2) - Open Status: */
/*                               .TRUE.  - the port is open */
/*                               .FALSE. - the port is closed */

/*                   STATUS(3) - Suspend Status: */
/*                               .TRUE.  - I/O on this port is suspended */
/*                               .FALSE. - I/O can proceed on this port */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See NSPIO. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT) */
/*        is signaled by ZZNSPPOK.  In the event this happens */
/*        the routine does not alter the status of any PORT. */

/* $ Particulars */

/*     See NSPIO. */

/* $ Examples */

/*     See NSPIO. */

/* $ Restrictions */

/*     1) The STATUS array must provide at least 3 logicals. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    NSPIO Version 2.0.0, 03-FEB-2000 (FST) */


/* -& */

/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("NSPPST", (ftnlen)6);
    }

/*     Find the integer associated with PORT. */

    id = zznsppok_(port, &c__8, ports, port_len, (ftnlen)32);

/*     Set the status of the port if no error was signaled. */

    if (! failed_()) {
	active[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("active", 
		i__1, "nspio_", (ftnlen)1258)] = status[0];
	open[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("open", i__1, 
		"nspio_", (ftnlen)1259)] = status[1];
	suspnd[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("suspnd", 
		i__1, "nspio_", (ftnlen)1260)] = status[2];
    }
    chkout_("NSPPST", (ftnlen)6);
    return 0;
/* $Procedure NSPIOC ( Inspekt I/O Manager -- Close Port ) */

L_nspioc:
/* $ Abstract */

/*    Close a port. */

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

/*     CHARACTER*(*)         PORT */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     PORT       I   String specifying which port to close. */

/* $ Detailed_Input */

/*     PORT       is a string that indicates the name of a port on which */
/*                to perform an operation.  Acceptable values are: */

/*                 Standard Output Port: */

/*                   'SCREEN' */

/*                 File Based Ports: */

/*                   'LOG' */
/*                   'SAVE' */
/*                   'UTILITY' */
/*                   'ERROR' */
/*                   'AUX1' */
/*                   'AUX2' */
/*                   'AUX3' */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See NSPIO. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT) */
/*        is signaled by ZZNSPPOK.  In the event this happens */
/*        the routine does not alter the contents of STATUS. */

/*     2) If PORT is already closed, then this routine does nothing, */
/*        and simply returns. */

/*     3) Attempting to "close" the screen port will have no effect. */

/* $ Particulars */

/*     See NSPIO. */

/* $ Examples */

/*     See NSPIO. */

/* $ Restrictions */

/*     1) PORT must refer to a file based port. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    NSPIO Version 2.0.0, 03-FEB-2000 (FST) */


/* -& */

/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("NSPIOC", (ftnlen)6);
    }

/*     Find the integer associated with PORT. */

    id = zznsppok_(port, &c__8, ports, port_len, (ftnlen)32);

/*     Check FAILED() to see if an error was signaled, or if */
/*     ID refers to the SCREEN port.  In either case, return without */
/*     doing anything. */

    if (failed_() || id == 1) {
	chkout_("NSPIOC", (ftnlen)6);
	return 0;
    }

/*     Now check to see if the port is currently closed or if the */
/*     requested port to close is the SCREEN port. */

    if (! open[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("open", i__1, 
	    "nspio_", (ftnlen)1420)] || id == 1) {
	chkout_("NSPIOC", (ftnlen)6);
	return 0;
    }

/*     If we make it this far, then we were given an open file */
/*     based port.  Close the port and reset its status. */

    cl__1.cerr = 0;
    cl__1.cunit = units[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge(
	    "units", i__1, "nspio_", (ftnlen)1431)];
    cl__1.csta = 0;
    f_clos(&cl__1);
    active[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("active", i__1, 
	    "nspio_", (ftnlen)1432)] = FALSE_;
    open[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("open", i__1, "nsp"
	    "io_", (ftnlen)1433)] = FALSE_;
    suspnd[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("suspnd", i__1, 
	    "nspio_", (ftnlen)1434)] = FALSE_;
    s_copy(files + ((i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("files", 
	    i__1, "nspio_", (ftnlen)1435)) * 255, " ", (ftnlen)255, (ftnlen)1)
	    ;

/*     If we have closed the error file, then clear ERROPF. */

    if (id == 5) {
	erropf = FALSE_;
    }
    chkout_("NSPIOC", (ftnlen)6);
    return 0;
/* $Procedure NSPIOS ( Inspekt I/O Manager -- Suspend Port ) */

L_nspios:
/* $ Abstract */

/*     Suspend a port. */

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

/*     CHARACTER*(*)         PORT */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     PORT       I   String specifying which port to suspend. */

/* $ Detailed_Input */

/*     PORT       is a string that indicates the name of a port on which */
/*                to perform an operation.  Acceptable values are: */

/*                 Standard Output Port: */

/*                   'SCREEN' */

/*                 File Based Ports: */

/*                   'LOG' */
/*                   'SAVE' */
/*                   'UTILITY' */
/*                   'ERROR' */
/*                   'AUX1' */
/*                   'AUX2' */
/*                   'AUX3' */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See NSPIO. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT) */
/*        is signaled by ZZNSPPOK.  In the event this happens */
/*        the routine does not alter the contents of STATUS. */

/*     2) If PORT is already has it's I/O suspended, then it will */
/*        remain suspended. */

/* $ Particulars */

/*     See NSPIO. */

/* $ Examples */

/*     See NSPIO. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    NSPIO Version 2.0.0, 08-FEB-2000 (FST) */


/* -& */

/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("NSPIOS", (ftnlen)6);
    }

/*     Find the integer associated with PORT. */

    id = zznsppok_(port, &c__8, ports, port_len, (ftnlen)32);

/*     Suspend I/O on the port, if no error was signaled. Note - if */
/*     PORT was already suspended, then it will remain suspended. */

    if (! failed_()) {

/*        Suspend I/O on PORT. */

	suspnd[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("suspnd", 
		i__1, "nspio_", (ftnlen)1594)] = TRUE_;
    }
    chkout_("NSPIOS", (ftnlen)6);
    return 0;
/* $Procedure NSPIOR ( Inspekt I/O Manager -- Reopen Port ) */

L_nspior:
/* $ Abstract */

/*     Reopen a suspended port. */

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

/*     CHARACTER*(*)         PORT */
/*     LOGICAL               OK */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     PORT       I   String specifying which port to re-open. */
/*     OK         O   logical that indicates a successful re-open. */

/* $ Detailed_Input */

/*     PORT       is a string that indicates the name of a port on which */
/*                to perform an operation.  Acceptable values are: */

/*                 Standard Output Port: */

/*                   'SCREEN' */

/*                 File Based Ports: */

/*                   'LOG' */
/*                   'SAVE' */
/*                   'UTILITY' */
/*                   'ERROR' */
/*                   'AUX1' */
/*                   'AUX2' */
/*                   'AUX3' */

/* $ Detailed_Output */

/*     OK         is a logical that indicates whether the attempt to */
/*                reopen a suspended port succeeded. */

/* $ Parameters */

/*     See NSPIO. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT) */
/*        is signaled by ZZNSPPOK. */

/*     2) If PORT is already not suspended, then PORT remains so and */
/*        OK is returned as .FALSE. */

/* $ Particulars */

/*     See NSPIO. */

/* $ Examples */

/*     See NSPIO. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    NSPIO Version 2.0.0, 08-FEB-2000 (FST) */


/* -& */

/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("NSPIOR", (ftnlen)6);
    }

/*     Find the integer associated with PORT. */

    id = zznsppok_(port, &c__8, ports, port_len, (ftnlen)32);

/*     See if an error has been signaled. If so, do nothing */
/*     further and return. */

    if (failed_()) {
	chkout_("NSPIOR", (ftnlen)6);
	return 0;
    }

/*     Check to see if PORT is currently suspended. */

    if (! suspnd[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("suspnd", 
	    i__1, "nspio_", (ftnlen)1753)]) {

/*        If it's not, then set OK to .FALSE. and return */

	*ok = FALSE_;
	chkout_("NSPIOR", (ftnlen)6);
	return 0;
    }

/*     Suspend I/O to this port. */

    suspnd[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("suspnd", i__1, 
	    "nspio_", (ftnlen)1767)] = FALSE_;
    chkout_("NSPIOR", (ftnlen)6);
    return 0;
/* $Procedure NSPWLN ( Inspekt I/O Manager -- Write Line ) */

L_nspwln:
/* $ Abstract */

/*     Write a line to all open and active ports. */

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

/*     CHARACTER*(*)         LINE */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LINE       I   is a line of text to be written to available ports. */

/* $ Detailed_Input */

/*     LINE       is a string of text that is to be written to all the */
/*                open, active, non-suspended ports. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See NSPIO. */

/* $ Files */

/*     1) This routine will write to any files associated with ports */
/*        that are open, active, and not suspended when NSPWLN is */
/*        called. */

/* $ Exceptions */

/*     1) If an error occurs writing the line to a particular port, */
/*        then this routine closes that port, resets its status, and */
/*        continues writing LINE to the other ports. */

/*     2) Any errors are signaled by routines in the call tree of */
/*        NSPWLN. */

/* $ Particulars */

/*     See NSPIO. */

/* $ Examples */

/*     See NSPIO. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    NSPIO Version 2.0.0, 08-FEB-2000 (FST) */


/* -& */

/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("NSPWLN", (ftnlen)6);
    }

/*     Write to all the open, active, and non-suspended ports. */

    for (id = 1; id <= 8; ++id) {
	if (! suspnd[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("suspnd"
		, i__1, "nspio_", (ftnlen)1896)] && active[(i__2 = id - 1) < 
		8 && 0 <= i__2 ? i__2 : s_rnge("active", i__2, "nspio_", (
		ftnlen)1896)] && open[(i__3 = id - 1) < 8 && 0 <= i__3 ? i__3 
		: s_rnge("open", i__3, "nspio_", (ftnlen)1896)]) {

/*           Write the line to this port. */

	    to = units[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("uni"
		    "ts", i__1, "nspio_", (ftnlen)1903)];
	    writln_(line, &to, line_len);

/*           Check for and process any errors. */

	    if (id != 1 && failed_()) {

/*              If we have encountered an error then close the */
/*              file and reset the port status.  Note we do not */
/*              need to reset error status to continue, since */
/*              WRITLN does not check RETURN(). */

		cl__1.cerr = 0;
		cl__1.cunit = units[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : 
			s_rnge("units", i__1, "nspio_", (ftnlen)1918)];
		cl__1.csta = 0;
		f_clos(&cl__1);
		active[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("act"
			"ive", i__1, "nspio_", (ftnlen)1919)] = FALSE_;
		open[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("open", 
			i__1, "nspio_", (ftnlen)1920)] = FALSE_;
		suspnd[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("sus"
			"pnd", i__1, "nspio_", (ftnlen)1921)] = FALSE_;
		s_copy(files + ((i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : 
			s_rnge("files", i__1, "nspio_", (ftnlen)1922)) * 255, 
			" ", (ftnlen)255, (ftnlen)1);
	    }
	}
    }
    chkout_("NSPWLN", (ftnlen)6);
    return 0;
/* $Procedure NSPEND ( Inspekt I/O Manager -- Finished with I/O ) */

L_nspend:
/* $ Abstract */

/*     The final entry point handles closing files and informing */
/*     the user of the location of these files. */

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

/*     None. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See NSPIO. */

/* $ Files */

/*     1) This routine closes the files attached to all open ports. */

/* $ Exceptions */

/*     1) If the SCREEN port is not open, it simply closes the port */
/*        and does not write any notifications. */

/* $ Particulars */

/*     See NSPIO. */

/* $ Examples */

/*     See NSPIO. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    NSPIO Version 2.0.0, 09-FEB-2000 (FST) */


/* -& */

/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("NSPEND", (ftnlen)6);
    }

/*     If the LOG port is open, then notify the user about it's */
/*     location, and close it. */

    if (open[1]) {
	trnlat_("LOGFILWRITTENTO", messge, (ftnlen)15, (ftnlen)400);
	if (! suspnd[0] && active[0] && open[0]) {

/*           Write the message. */

	    writln_(" ", &c__6, (ftnlen)1);
	    r__ = rtrim_(messge, (ftnlen)400);
	    writln_(messge, &c__6, r__);
	    r__ = rtrim_(files + 255, (ftnlen)255);
	    writln_(files + 255, &c__6, r__);
	}
    }

/*     If the SAVE port is open, then notify the user about it's */
/*     location, and close it. */

    if (open[3]) {
	trnlat_("SAVFILWRITTENTO", messge, (ftnlen)15, (ftnlen)400);
	if (! suspnd[0] && active[0] && open[0]) {

/*           Write the message. */

	    writln_(" ", &c__6, (ftnlen)1);
	    r__ = rtrim_(messge, (ftnlen)400);
	    writln_(messge, &c__6, r__);
	    r__ = rtrim_(files + 765, (ftnlen)255);
	    writln_(files + 765, &c__6, r__);
	}
    }

/*     If the ERROR port is open, then notify the user about it's */
/*     location, and close it. */

    if (open[4]) {
	trnlat_("ERRFILWRITTENTO", messge, (ftnlen)15, (ftnlen)400);
	if (! suspnd[0] && active[0] && open[0]) {

/*           Write the message. */

	    writln_(" ", &c__6, (ftnlen)1);
	    r__ = rtrim_(messge, (ftnlen)400);
	    writln_(messge, &c__6, r__);
	    r__ = rtrim_(files + 1020, (ftnlen)255);
	    writln_(files + 1020, &c__6, r__);
	}
    } else if (erropf) {
	trnlat_("ERRFILWRITEFAIL", messge, (ftnlen)15, (ftnlen)400);
	if (! suspnd[0] && active[0] && open[0]) {

/*           Write the message. */

	    writln_(" ", &c__6, (ftnlen)1);
	    r__ = rtrim_(messge, (ftnlen)400);
	    writln_(messge, &c__6, r__);
	    r__ = rtrim_(files + 1020, (ftnlen)255);
	    writln_(files + 1020, &c__6, r__);
	}
    }

/*     Close all ports and restore NSPIO status to it's uninitialized */
/*     state.  First handle the screen port, since it's an exception. */

    active[0] = TRUE_;
    open[0] = TRUE_;
    suspnd[0] = FALSE_;

/*     Now reset the file based ports. */

    for (id = 2; id <= 8; ++id) {

/*        Close the file associated with the port if it's open. */

	if (open[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("open", 
		i__1, "nspio_", (ftnlen)2163)]) {
	    cl__1.cerr = 0;
	    cl__1.cunit = units[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : 
		    s_rnge("units", i__1, "nspio_", (ftnlen)2165)];
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	}

/*        Restore original port status. */

	units[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("units", i__1, 
		"nspio_", (ftnlen)2171)] = 0;
	s_copy(files + ((i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge(
		"files", i__1, "nspio_", (ftnlen)2172)) * 255, " ", (ftnlen)
		255, (ftnlen)1);
	active[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("active", 
		i__1, "nspio_", (ftnlen)2173)] = FALSE_;
	open[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("open", i__1, 
		"nspio_", (ftnlen)2174)] = FALSE_;
	suspnd[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("suspnd", 
		i__1, "nspio_", (ftnlen)2175)] = FALSE_;
    }
    chkout_("NSPEND", (ftnlen)6);
    return 0;
/* $Procedure NSPPFL ( Inspekt I/O Manager -- Fetch file name ) */

L_nsppfl:
/* $ Abstract */

/*     Get the name of the file associated with a port. */

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


/*     CHARACTER*(*)         PORT */
/*     CHARACTER*(*)         NAME */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     PORT       I   String that indicates the name of the port. */
/*     NAME       O   String holding the filename associated with PORT. */

/* $ Detailed_Input */

/*     PORT       is a string that indicates the name of a port on which */
/*                to perform an operation.  Acceptable values are: */

/*                 Standard Output Port: */

/*                   'SCREEN' */

/*                 File Based Ports: */

/*                   'LOG' */
/*                   'SAVE' */
/*                   'UTILITY' */
/*                   'ERROR' */
/*                   'AUX1' */
/*                   'AUX2' */
/*                   'AUX3' */

/* $ Detailed_Output */

/*     NAME       The name of a file attached to a file based port. */

/* $ Parameters */

/*     See NSPIO. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT) */
/*        is signaled by ZZNSPPOK and NAME is set to ' '. */

/*     2) If PORT is 'SCREEN' then NSPPFL sets NAME to ' '. */

/*     3) If PORT is INACTIVE, SUSPENDED, or CLOSED, then NAME is */
/*        returned as ' '. */

/* $ Particulars */

/*     See NSPIO. */

/* $ Examples */

/*     See NSPIO. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    NSPIO Version 2.0.0, 09-FEB-2000 (FST) */


/* -& */

/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("NSPPFL", (ftnlen)6);
    }

/*     Find the integer associated with PORT. */

    id = zznsppok_(port, &c__8, ports, port_len, (ftnlen)32);

/*     See if an error has been signaled. If so, clear NAME */
/*     and return. */

    if (failed_()) {
	s_copy(name__, " ", name_len, (ftnlen)1);
	chkout_("NSPPFL", (ftnlen)6);
	return 0;

/*     If the ID refers to an active, open, non-suspended port, then */
/*     set NAME to the name of the file.  Note: in the case when PORT */
/*     is 'SCREEN', the corresponding entry in the FILES array is ' '. */

    } else if (! suspnd[(i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge(
	    "suspnd", i__1, "nspio_", (ftnlen)2338)] && active[(i__2 = id - 1)
	     < 8 && 0 <= i__2 ? i__2 : s_rnge("active", i__2, "nspio_", (
	    ftnlen)2338)] && open[(i__3 = id - 1) < 8 && 0 <= i__3 ? i__3 : 
	    s_rnge("open", i__3, "nspio_", (ftnlen)2338)]) {
	s_copy(name__, files + ((i__1 = id - 1) < 8 && 0 <= i__1 ? i__1 : 
		s_rnge("files", i__1, "nspio_", (ftnlen)2342)) * 255, 
		name_len, (ftnlen)255);

/*     If PORT is inactive, suspended or closed, set NAME to ' '. */

    } else {
	s_copy(name__, " ", name_len, (ftnlen)1);
    }
    chkout_("NSPPFL", (ftnlen)6);
    return 0;
} /* nspio_ */

/* Subroutine */ int nspio_(char *line, char *port, char *name__, logical *
	status, logical *ok, ftnlen line_len, ftnlen port_len, ftnlen 
	name_len)
{
    return nspio_0_(0, line, port, name__, status, ok, line_len, port_len, 
	    name_len);
    }

/* Subroutine */ int nspopn_(char *port, char *name__, ftnlen port_len, 
	ftnlen name_len)
{
    return nspio_0_(1, (char *)0, port, name__, (logical *)0, (logical *)0, (
	    ftnint)0, port_len, name_len);
    }

/* Subroutine */ int nspioh_(char *port, ftnlen port_len)
{
    return nspio_0_(2, (char *)0, port, (char *)0, (logical *)0, (logical *)0,
	     (ftnint)0, port_len, (ftnint)0);
    }

/* Subroutine */ int nspioa_(char *port, ftnlen port_len)
{
    return nspio_0_(3, (char *)0, port, (char *)0, (logical *)0, (logical *)0,
	     (ftnint)0, port_len, (ftnint)0);
    }

/* Subroutine */ int nspgst_(char *port, logical *status, ftnlen port_len)
{
    return nspio_0_(4, (char *)0, port, (char *)0, status, (logical *)0, (
	    ftnint)0, port_len, (ftnint)0);
    }

/* Subroutine */ int nsppst_(char *port, logical *status, ftnlen port_len)
{
    return nspio_0_(5, (char *)0, port, (char *)0, status, (logical *)0, (
	    ftnint)0, port_len, (ftnint)0);
    }

/* Subroutine */ int nspioc_(char *port, ftnlen port_len)
{
    return nspio_0_(6, (char *)0, port, (char *)0, (logical *)0, (logical *)0,
	     (ftnint)0, port_len, (ftnint)0);
    }

/* Subroutine */ int nspios_(char *port, ftnlen port_len)
{
    return nspio_0_(7, (char *)0, port, (char *)0, (logical *)0, (logical *)0,
	     (ftnint)0, port_len, (ftnint)0);
    }

/* Subroutine */ int nspior_(char *port, logical *ok, ftnlen port_len)
{
    return nspio_0_(8, (char *)0, port, (char *)0, (logical *)0, ok, (ftnint)
	    0, port_len, (ftnint)0);
    }

/* Subroutine */ int nspwln_(char *line, ftnlen line_len)
{
    return nspio_0_(9, line, (char *)0, (char *)0, (logical *)0, (logical *)0,
	     line_len, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int nspend_(void)
{
    return nspio_0_(10, (char *)0, (char *)0, (char *)0, (logical *)0, (
	    logical *)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int nsppfl_(char *port, char *name__, ftnlen port_len, 
	ftnlen name_len)
{
    return nspio_0_(11, (char *)0, port, name__, (logical *)0, (logical *)0, (
	    ftnint)0, port_len, name_len);
    }

