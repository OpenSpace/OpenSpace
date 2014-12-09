/* trnlat.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__28 = 28;

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

/* Subroutine */ int trnlat_(char *phrase, char *messge, ftnlen phrase_len, 
	ftnlen messge_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);

    /* Local variables */
    static char lang[32];
    static integer item;
    static char title[32*28];
    extern integer bsrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static char french[400*28], german[400*28];
    extern /* Subroutine */ int getlan_(char *, ftnlen), orderc_(char *, 
	    integer *, integer *, ftnlen), reordc_(integer *, integer *, char 
	    *, ftnlen);
    static char englsh[400*28];
    static integer iorder[28];
    static char russan[400*28];


/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/*     This is a language dependent routine. */


/*     Spicelib functions */


/*     Local parameters and variables */

    if (first) {
	first = FALSE_;
	s_copy(title, "ERRFLAG", (ftnlen)32, (ftnlen)7);
	s_copy(englsh, "-Oops!-", (ftnlen)400, (ftnlen)7);
	s_copy(french, "--%-Sacre^Bleu!!-%^^:", (ftnlen)400, (ftnlen)21);
	s_copy(german, "--%-Achtung!!-%^^:", (ftnlen)400, (ftnlen)18);
	s_copy(russan, "--%-ERROR-%^^:", (ftnlen)400, (ftnlen)14);
	s_copy(title + 32, "EXIT", (ftnlen)32, (ftnlen)4);
	s_copy(englsh + 400, "EXIT", (ftnlen)400, (ftnlen)4);
	s_copy(french + 400, "SORTIE", (ftnlen)400, (ftnlen)6);
	s_copy(german + 400, "EXIT", (ftnlen)400, (ftnlen)4);
	s_copy(russan + 400, "EXIT", (ftnlen)400, (ftnlen)4);
	s_copy(title + 64, "START", (ftnlen)32, (ftnlen)5);
	s_copy(englsh + 800, "START", (ftnlen)400, (ftnlen)5);
	s_copy(french + 800, "COMMENCER", (ftnlen)400, (ftnlen)9);
	s_copy(german + 800, "START", (ftnlen)400, (ftnlen)5);
	s_copy(russan + 800, "START", (ftnlen)400, (ftnlen)5);
	s_copy(title + 96, "STOP", (ftnlen)32, (ftnlen)4);
	s_copy(englsh + 1200, "STOP", (ftnlen)400, (ftnlen)4);
	s_copy(french + 1200, "ARETE", (ftnlen)400, (ftnlen)5);
	s_copy(german + 1200, "STOP", (ftnlen)400, (ftnlen)4);
	s_copy(russan + 1200, "STOP", (ftnlen)400, (ftnlen)4);
	s_copy(title + 128, "DEMO", (ftnlen)32, (ftnlen)4);
	s_copy(englsh + 1600, "DEMO", (ftnlen)400, (ftnlen)4);
	s_copy(french + 1600, "MONTRER", (ftnlen)400, (ftnlen)7);
	s_copy(german + 1600, "DEMO", (ftnlen)400, (ftnlen)4);
	s_copy(russan + 1600, "DEMO", (ftnlen)400, (ftnlen)4);
	s_copy(title + 160, "PAUSE", (ftnlen)32, (ftnlen)5);
	s_copy(englsh + 2000, "PAUSE", (ftnlen)400, (ftnlen)5);
	s_copy(french + 2000, "PAUSE", (ftnlen)400, (ftnlen)5);
	s_copy(german + 2000, "PAUSE", (ftnlen)400, (ftnlen)5);
	s_copy(russan + 2000, "PAUSE", (ftnlen)400, (ftnlen)5);
	s_copy(title + 192, "WAIT", (ftnlen)32, (ftnlen)4);
	s_copy(englsh + 2400, "WAIT", (ftnlen)400, (ftnlen)4);
	s_copy(french + 2400, "ATTENDRE", (ftnlen)400, (ftnlen)8);
	s_copy(german + 2400, "WAIT", (ftnlen)400, (ftnlen)4);
	s_copy(russan + 2400, "WAIT", (ftnlen)400, (ftnlen)4);
	s_copy(title + 224, "QUIT", (ftnlen)32, (ftnlen)4);
	s_copy(englsh + 2800, "QUIT", (ftnlen)400, (ftnlen)4);
	s_copy(french + 2800, "ARETE", (ftnlen)400, (ftnlen)5);
	s_copy(german + 2800, "QUIT", (ftnlen)400, (ftnlen)4);
	s_copy(russan + 2800, "QUIT", (ftnlen)400, (ftnlen)4);
	s_copy(title + 256, "DEFPROMPT", (ftnlen)32, (ftnlen)9);
	s_copy(englsh + 3200, "Yes? >", (ftnlen)400, (ftnlen)6);
	s_copy(french + 3200, "Oui? >", (ftnlen)400, (ftnlen)6);
	s_copy(german + 3200, "Ja? >", (ftnlen)400, (ftnlen)5);
	s_copy(russan + 3200, "Dah? >", (ftnlen)400, (ftnlen)6);
	s_copy(title + 288, "MISSINGFILELONG", (ftnlen)32, (ftnlen)15);
	s_copy(englsh + 3600, "No command sequence file was specified in the"
		" START command. ", (ftnlen)400, (ftnlen)61);
	s_copy(french + 3600, "Le fichier command sequence n'est pas present"
		" dans le command \"COMMENCER\". ", (ftnlen)400, (ftnlen)75);
	s_copy(german + 3600, "No command sequence file was specified in the"
		" START command. ", (ftnlen)400, (ftnlen)61);
	s_copy(russan + 3600, "No command sequence file was specified in the"
		" START command. ", (ftnlen)400, (ftnlen)61);
	s_copy(title + 320, "MISSINGFILESHORT", (ftnlen)32, (ftnlen)16);
	s_copy(englsh + 4000, "Missing_File_Name", (ftnlen)400, (ftnlen)17);
	s_copy(french + 4000, "Nom_de_fichier_abscent", (ftnlen)400, (ftnlen)
		22);
	s_copy(german + 4000, "Missing_File_Name", (ftnlen)400, (ftnlen)17);
	s_copy(russan + 4000, "Missing_File_Name", (ftnlen)400, (ftnlen)17);
	s_copy(title + 352, "UNABLETOSTART", (ftnlen)32, (ftnlen)13);
	s_copy(englsh + 4400, "Unable_To_Start_File", (ftnlen)400, (ftnlen)20)
		;
	s_copy(french + 4400, "Unable_To_Start_File", (ftnlen)400, (ftnlen)20)
		;
	s_copy(german + 4400, "Unable_To_Start_File", (ftnlen)400, (ftnlen)20)
		;
	s_copy(russan + 4400, "Unable_To_Start_File", (ftnlen)400, (ftnlen)20)
		;
	s_copy(title + 384, "COMBUFFULLLNG", (ftnlen)32, (ftnlen)13);
	s_copy(englsh + 4800, "The designer of the program has inadvertantly"
		" exceeded the internal command buffer.  Please keep your ses"
		"sion log and report this problem to NAIF. ", (ftnlen)400, (
		ftnlen)147);
	s_copy(french + 4800, "The designer of the program has inadvertantly"
		" exceeded the internal command buffer.  Please keep your ses"
		"sion log and report this problem to NAIF. ", (ftnlen)400, (
		ftnlen)147);
	s_copy(german + 4800, "The designer of the program has inadvertantly"
		" exceeded the internal command buffer.  Please keep your ses"
		"sion log and report this problem to NAIF. ", (ftnlen)400, (
		ftnlen)147);
	s_copy(russan + 4800, "The designer of the program has inadvertantly"
		" exceeded the internal command buffer.  Please keep your ses"
		"sion log and report this problem to NAIF. ", (ftnlen)400, (
		ftnlen)147);
	s_copy(title + 416, "COMBUFFULLSHT", (ftnlen)32, (ftnlen)13);
	s_copy(englsh + 5200, "Command_Buffer_Full", (ftnlen)400, (ftnlen)19);
	s_copy(french + 5200, "Command_Buffer_Full", (ftnlen)400, (ftnlen)19);
	s_copy(german + 5200, "Command_Buffer_Full", (ftnlen)400, (ftnlen)19);
	s_copy(russan + 5200, "Command_Buffer_Full", (ftnlen)400, (ftnlen)19);
	s_copy(title + 448, "NESTINGTOODEEP", (ftnlen)32, (ftnlen)14);
	s_copy(englsh + 5600, "The command sequence contained in # could not"
		" be started. There are already # command sequences files tha"
		"t have been started without resolution. This is the limit on"
		" the number of active command sequence files that can be act"
		"ive at any time. ", (ftnlen)400, (ftnlen)242);
	s_copy(french + 5600, "The command sequence contained in # could not"
		" be started. There are already # command sequences files tha"
		"t have been started without resolution. This is the limit on"
		" the number of active command sequence files that can be act"
		"ive at any time. ", (ftnlen)400, (ftnlen)242);
	s_copy(german + 5600, "The command sequence contained in # could not"
		" be started. There are already # command sequences files tha"
		"t have been started without resolution. This is the limit on"
		" the number of active command sequence files that can be act"
		"ive at any time. ", (ftnlen)400, (ftnlen)242);
	s_copy(russan + 5600, "The command sequence contained in # could not"
		" be started. There are already # command sequences files tha"
		"t have been started without resolution. This is the limit on"
		" the number of active command sequence files that can be act"
		"ive at any time. ", (ftnlen)400, (ftnlen)242);
	s_copy(title + 480, "NOLOGUNITSFREE", (ftnlen)32, (ftnlen)14);
	s_copy(englsh + 6000, "The command sequence contained in # could not"
		" be started. There are no FORTRAN logical units available th"
		"at can be attached to the file. A possible cause for this pr"
		"oblem is that there are too many files already in use by the"
		" program. ", (ftnlen)400, (ftnlen)235);
	s_copy(french + 6000, "The command sequence contained in # could not"
		" be started. There are no FORTRAN logical units available th"
		"at can be attached to the file. A possible cause for this pr"
		"oblem is that there are too many files already in use by the"
		" program. ", (ftnlen)400, (ftnlen)235);
	s_copy(german + 6000, "The command sequence contained in # could not"
		" be started. There are no FORTRAN logical units available th"
		"at can be attached to the file. A possible cause for this pr"
		"oblem is that there are too many files already in use by the"
		" program. ", (ftnlen)400, (ftnlen)235);
	s_copy(russan + 6000, "The command sequence contained in # could not"
		" be started. There are no FORTRAN logical units available th"
		"at can be attached to the file. A possible cause for this pr"
		"oblem is that there are too many files already in use by the"
		" program. ", (ftnlen)400, (ftnlen)235);
	s_copy(title + 512, "FILENOTEXIST", (ftnlen)32, (ftnlen)12);
	s_copy(englsh + 6400, "The file \"#\" could not be started. It doesn"
		"'t exist. ", (ftnlen)400, (ftnlen)53);
	s_copy(french + 6400, "The file \"#\" could not be started. It doesn"
		"'t exist. ", (ftnlen)400, (ftnlen)53);
	s_copy(german + 6400, "The file \"#\" could not be started. It doesn"
		"'t exist. ", (ftnlen)400, (ftnlen)53);
	s_copy(russan + 6400, "The file \"#\" could not be started. It doesn"
		"'t exist. ", (ftnlen)400, (ftnlen)53);
	s_copy(title + 544, "COMFILEOPENERROR", (ftnlen)32, (ftnlen)16);
	s_copy(englsh + 6800, "The command sequence contained in # could not"
		" be started. An error occurred while attempting to open the "
		"file. ", (ftnlen)400, (ftnlen)111);
	s_copy(french + 6800, "The command sequence contained in # could not"
		" be started. An error occurred while attempting to open the "
		"file. ", (ftnlen)400, (ftnlen)111);
	s_copy(german + 6800, "The command sequence contained in # could not"
		" be started. An error occurred while attempting to open the "
		"file. ", (ftnlen)400, (ftnlen)111);
	s_copy(russan + 6800, "The command sequence contained in # could not"
		" be started. An error occurred while attempting to open the "
		"file. ", (ftnlen)400, (ftnlen)111);
	s_copy(title + 576, "LOGFILWRITTENTO", (ftnlen)32, (ftnlen)15);
	s_copy(englsh + 7200, "The log file has been written to: ", (ftnlen)
		400, (ftnlen)34);
	s_copy(french + 7200, "Le fichier de log s'est ecrivee : ", (ftnlen)
		400, (ftnlen)34);
	s_copy(german + 7200, "Das logenfile hass bin written to: ", (ftnlen)
		400, (ftnlen)35);
	s_copy(russan + 7200, "The log file has been written to: ", (ftnlen)
		400, (ftnlen)34);
	s_copy(title + 608, "SAVFILWRITTENTO", (ftnlen)32, (ftnlen)15);
	s_copy(englsh + 7600, "The save file has been written to: ", (ftnlen)
		400, (ftnlen)35);
	s_copy(french + 7600, "Le fichier de garde s'est ecrivee : ", (ftnlen)
		400, (ftnlen)36);
	s_copy(german + 7600, "Das savenfile hass bin written to: ", (ftnlen)
		400, (ftnlen)35);
	s_copy(russan + 7600, "The save file has been written to: ", (ftnlen)
		400, (ftnlen)35);
	s_copy(title + 640, "UNABLETOWRITETOFILE", (ftnlen)32, (ftnlen)19);
	s_copy(englsh + 8000, "I was unable to write to the file: /cr/cr(3:3"
		") # /cr/cr(-3;-3) The value of IOSTAT that was returned as a"
		" diagnosis of the problem was: /cr/cr(3:3) # /cr/cr(-3;-3) T"
		"his file is now closed. No further attempts will be made to "
		"write to it. ", (ftnlen)400, (ftnlen)238);
	s_copy(french + 8000, "I was unable to write to the file: /cr/cr(3:3"
		") # /cr/cr(-3;-3) The value of IOSTAT that was returned as a"
		" diagnosis of the problem was: /cr/cr(3:3) # /cr/cr(-3;-3) T"
		"his file is now closed. No further attempts will be made to "
		"write to it. ", (ftnlen)400, (ftnlen)238);
	s_copy(german + 8000, "I was unable to write to the file: /cr/cr(3:3"
		") # /cr/cr(-3;-3) The value of IOSTAT that was returned as a"
		" diagnosis of the problem was: /cr/cr(3:3) # /cr/cr(-3;-3) T"
		"his file is now closed. No further attempts will be made to "
		"write to it. ", (ftnlen)400, (ftnlen)238);
	s_copy(russan + 8000, "I was unable to write to the file: /cr/cr(3:3"
		") # /cr/cr(-3;-3) The value of IOSTAT that was returned as a"
		" diagnosis of the problem was: /cr/cr(3:3) # /cr/cr(-3;-3) T"
		"his file is now closed. No further attempts will be made to "
		"write to it. ", (ftnlen)400, (ftnlen)238);
	s_copy(title + 672, "WARNING", (ftnlen)32, (ftnlen)7);
	s_copy(englsh + 8400, "Warning:", (ftnlen)400, (ftnlen)8);
	s_copy(french + 8400, "Attention: ", (ftnlen)400, (ftnlen)11);
	s_copy(german + 8400, "Achtung: ", (ftnlen)400, (ftnlen)9);
	s_copy(russan + 8400, "Hey!! ", (ftnlen)400, (ftnlen)6);
	s_copy(title + 704, "CANNOTOPENLOG", (ftnlen)32, (ftnlen)13);
	s_copy(englsh + 8800, "An error occurred while attempting to open th"
		"e log file. It will not be possible to keep a log of this se"
		"ssion. No further attempts to log commands will be attempted"
		". /cr/cr The cause of the failure to open the log file was d"
		"iagnosed to be: /cr/cr(3:3) ", (ftnlen)400, (ftnlen)253);
	s_copy(french + 8800, "An error occurred while attempting to open ze"
		"e log file. It will not be possible to keep a log of this se"
		"ssion. No further attempts to log commands will be attempted"
		". /cr/cr Zee cause of zee failure to open zee log file was d"
		"iagnosed to be: /cr/cr(3:3) ", (ftnlen)400, (ftnlen)253);
	s_copy(german + 8800, "An error occurred while attempting to open th"
		"e log file. It will not be possible to keep a log of this se"
		"ssion. No further attempts to log commands will be attempted"
		". /cr/cr The cause of the failure to open the log file was d"
		"iagnosed to be: /cr/cr(3:3) ", (ftnlen)400, (ftnlen)253);
	s_copy(german + 8800, "An error occurred while attempting to open th"
		"e log file. It will not be possible to keep a log of this se"
		"ssion. No further attempts to log commands will be attempted"
		". /cr/cr The cause of the failure to open the log file was d"
		"iagnosed to be: /cr/cr(3:3) ", (ftnlen)400, (ftnlen)253);
	s_copy(title + 736, "NOMOREDIAGNOSTICS", (ftnlen)32, (ftnlen)17);
	s_copy(englsh + 9200, "Sorry, no further diagnostics are available.", 
		(ftnlen)400, (ftnlen)44);
	s_copy(french + 9200, "Mon ami, I am so sorry. I can say no more abo"
		"ut zee error I reported earlier.", (ftnlen)400, (ftnlen)77);
	s_copy(german + 9200, "No further diagnostics are available.", (
		ftnlen)400, (ftnlen)37);
	s_copy(russan + 9200, "Sorry, no further diagnostics are available.", 
		(ftnlen)400, (ftnlen)44);
	s_copy(title + 768, "DONT", (ftnlen)32, (ftnlen)4);
	s_copy(englsh + 9600, "NO", (ftnlen)400, (ftnlen)2);
	s_copy(french + 9600, "NO", (ftnlen)400, (ftnlen)2);
	s_copy(german + 9600, "NEIN", (ftnlen)400, (ftnlen)4);
	s_copy(russan + 9600, "NYET", (ftnlen)400, (ftnlen)4);
	s_copy(title + 800, "ECHO", (ftnlen)32, (ftnlen)4);
	s_copy(englsh + 10000, "ECHO", (ftnlen)400, (ftnlen)4);
	s_copy(french + 10000, "ECHO", (ftnlen)400, (ftnlen)4);
	s_copy(german + 10000, "ECHO", (ftnlen)400, (ftnlen)4);
	s_copy(russan + 10000, "ECHO", (ftnlen)400, (ftnlen)4);
	s_copy(title + 832, "ERRFILWRITTENTO", (ftnlen)32, (ftnlen)15);
	s_copy(englsh + 10400, "The error file has been written to: ", (
		ftnlen)400, (ftnlen)36);
	s_copy(french + 10400, "The error file has been written to: ", (
		ftnlen)400, (ftnlen)36);
	s_copy(german + 10400, "The error file has been written to: ", (
		ftnlen)400, (ftnlen)36);
	s_copy(russan + 10400, "The error file has been written to: ", (
		ftnlen)400, (ftnlen)36);
	s_copy(title + 864, "ERRFILWRITEFAIL", (ftnlen)32, (ftnlen)15);
	s_copy(englsh + 10800, "WARNING--Unable to create the errorfile: ", (
		ftnlen)400, (ftnlen)41);
	s_copy(french + 10800, "WARNING--Unable to create the errorfile: ", (
		ftnlen)400, (ftnlen)41);
	s_copy(german + 10800, "WARNING--Unable to create the errorfile: ", (
		ftnlen)400, (ftnlen)41);
	s_copy(russan + 10800, "WARNING--Unable to create the errorfile: ", (
		ftnlen)400, (ftnlen)41);
	orderc_(title, &c__28, iorder, (ftnlen)32);
	reordc_(iorder, &c__28, title, (ftnlen)32);
	reordc_(iorder, &c__28, englsh, (ftnlen)400);
	reordc_(iorder, &c__28, french, (ftnlen)400);
	reordc_(iorder, &c__28, german, (ftnlen)400);
	reordc_(iorder, &c__28, russan, (ftnlen)400);
    }
    item = bsrchc_(phrase, &c__28, title, phrase_len, (ftnlen)32);

/*     Look up the current language to be used. */

    getlan_(lang, (ftnlen)32);
    if (item == 0) {
	s_copy(messge, phrase, messge_len, phrase_len);
    } else if (s_cmp(lang, "FRENCH", (ftnlen)32, (ftnlen)6) == 0) {
	s_copy(messge, french + ((i__1 = item - 1) < 28 && 0 <= i__1 ? i__1 : 
		s_rnge("french", i__1, "trnlat_", (ftnlen)426)) * 400, 
		messge_len, (ftnlen)400);
    } else if (s_cmp(lang, "GERMAN", (ftnlen)32, (ftnlen)6) == 0) {
	s_copy(messge, german + ((i__1 = item - 1) < 28 && 0 <= i__1 ? i__1 : 
		s_rnge("german", i__1, "trnlat_", (ftnlen)428)) * 400, 
		messge_len, (ftnlen)400);
    } else if (s_cmp(lang, "RUSSIAN", (ftnlen)32, (ftnlen)7) == 0) {
	s_copy(messge, russan + ((i__1 = item - 1) < 28 && 0 <= i__1 ? i__1 : 
		s_rnge("russan", i__1, "trnlat_", (ftnlen)430)) * 400, 
		messge_len, (ftnlen)400);
    } else {
	s_copy(messge, englsh + ((i__1 = item - 1) < 28 && 0 <= i__1 ? i__1 : 
		s_rnge("englsh", i__1, "trnlat_", (ftnlen)432)) * 400, 
		messge_len, (ftnlen)400);
    }
    return 0;
} /* trnlat_ */

