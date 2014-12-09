/* zzhlp013.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP013 ( private help text ) */
/* Subroutine */ int zzhlp013_(integer *begin, integer *finish, char *text, 
	ftnlen text_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__, j;
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen);

/* $ Abstract */

/*     Fill out a portion of the help text needed by percy. */

/*     Private routine intended solely for the support of Inspekt */

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

/*     PRIVATE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BEGIN      O   Indexes of begins of text help */
/*     FINISH     O   Indexes of ends of text help */
/*     TEXT       O   A block of text help. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This routine simply fills begin and end markers as well */
/*     as actual text for a block of help text for percy. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    Inspekt Version 1.0.0, 1-AUG-1997 (WLT) */


/* -& */
    j = finish[0];
    i__ = begin[0];
    finish[0] = j;
    begin[0] = i__;
    repmc_(text, "*", "*", text, text_len, (ftnlen)1, (ftnlen)1, text_len);
    s_copy(text + text_len * 1213, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1214, "E-kernels can be \"unloaded.\" When an E"
	    "-kernel is unloaded,", text_len, (ftnlen)58);
    s_copy(text + text_len * 1215, "Inspekt \"forgets\" about the existence "
	    "of the kernel. Data in the kernel", text_len, (ftnlen)71);
    s_copy(text + text_len * 1216, "can not be retrieved or manipulated with"
	    "out first", text_len, (ftnlen)49);
    s_copy(text + text_len * 1217, "re-loading it.", text_len, (ftnlen)14);
    s_copy(text + text_len * 1218, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1219, "To load or unload an E-kernel, type", 
	    text_len, (ftnlen)35);
    s_copy(text + text_len * 1220, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1221, "LOAD EK (filename of E-kernel)", text_len,
	     (ftnlen)30);
    s_copy(text + text_len * 1222, "UNLOAD  (filename of previously loaded E"
	    "-kernel).", text_len, (ftnlen)49);
    s_copy(text + text_len * 1223, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1224, "The filename used in the UNLOAD command "
	    "must be the same as the filename used to", text_len, (ftnlen)80);
    s_copy(text + text_len * 1225, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1226, "Two other files may be loaded: a leapsec"
	    "onds kernel and an SCLK kernel. These ar", text_len, (ftnlen)80);
    s_copy(text + text_len * 1227, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1228, "LOAD LEAPSECONDS (filename of leapsecond"
	    "s kernel)", text_len, (ftnlen)49);
    s_copy(text + text_len * 1229, "LOAD SCLK KERNEL (filename of SCLK kerne"
	    "l)", text_len, (ftnlen)42);
    s_copy(text + text_len * 1230, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1231, "You can avoid having to load leapseconds"
	    " or SCLK kernels by", text_len, (ftnlen)59);
    s_copy(text + text_len * 1232, "setting up the environment variables SCL"
	    "K and LEAPSECONDS to", text_len, (ftnlen)60);
    s_copy(text + text_len * 1233, "point to the corresponding kernel prior "
	    "to starting Inspekt.", text_len, (ftnlen)60);
    s_copy(text + text_len * 1234, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1235, "Leapsecond and SCLK kernels cannot be un"
	    "loaded.", text_len, (ftnlen)47);
    s_copy(text + text_len * 1236, "However, you can load a different leapse"
	    "conds or SCLK kernel.", text_len, (ftnlen)61);
    s_copy(text + text_len * 1237, "When a new SLCK or leapseconds kernel is"
	    " loaded Inspekt behaves", text_len, (ftnlen)63);
    s_copy(text + text_len * 1238, "as if you had never loaded the previous "
	    "SCLK", text_len, (ftnlen)44);
    s_copy(text + text_len * 1239, "or leapseconds kernel. Only the data in "
	    "the freshly loaded kernel", text_len, (ftnlen)65);
    s_copy(text + text_len * 1240, "will be used by Inspekt.", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 1241, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1242, "There are two commands for determining w"
	    "hat kernels have been loaded.", text_len, (ftnlen)69);
    s_copy(text + text_len * 1243, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1244, "SHOW KERNELS;", text_len, (ftnlen)13);
    s_copy(text + text_len * 1245, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1246, "displays which E-kernels have been loaded"
	    , text_len, (ftnlen)41);
    s_copy(text + text_len * 1247, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1248, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1249, "SHOW ENVIRONMENT", text_len, (ftnlen)16);
    s_copy(text + text_len * 1250, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1251, "displays which leapseconds and SCLK kern"
	    "els (if any) have been", text_len, (ftnlen)62);
    s_copy(text + text_len * 1252, "loaded along with other information abou"
	    "t the current Inspekt", text_len, (ftnlen)61);
    s_copy(text + text_len * 1253, "settings.", text_len, (ftnlen)9);
    s_copy(text + text_len * 1254, "@@Kernels            --- LOAD", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 1255, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1256, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1257, "Environment Variables", text_len, (ftnlen)
	    21);
    finish[28] = 1258;
    begin[29] = 1259;
    s_copy(text + text_len * 1258, "Listed below are the limits for various "
	    "aspects of Inspekt.", text_len, (ftnlen)59);
    s_copy(text + text_len * 1259, "@setparamsize{Total number of Columns}", 
	    text_len, (ftnlen)38);
    s_copy(text + text_len * 1260, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1261, "@param  Loaded E-kernels.", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 1262, "a maximum of 20 E-kernels may be loaded "
	    "at once.", text_len, (ftnlen)48);
    s_copy(text + text_len * 1263, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1264, "@param Total number of columns.", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 1265, "a maximum of 500 columns may", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 1266, "be present in all of the loaded kernels", 
	    text_len, (ftnlen)39);
    s_copy(text + text_len * 1267, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1268, "@param Page Width.", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 1269, "the page must be at least 40 characters "
	    "wide and", text_len, (ftnlen)48);
    s_copy(text + text_len * 1270, "no  more than 132 characters wide.", 
	    text_len, (ftnlen)34);
    s_copy(text + text_len * 1271, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1272, "@param  Column width.", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 1273, "a column must be at least 8 characters w"
	    "ide and no", text_len, (ftnlen)50);
    s_copy(text + text_len * 1274, "more than 80 characters wide.", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 1275, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1276, "@param  Command Size.", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 1277, "A command can contain no more than 1760 "
	    "character", text_len, (ftnlen)49);
    s_copy(text + text_len * 1278, "(it should fit on one 24 by 80 character"
	    " screen).", text_len, (ftnlen)49);
    s_copy(text + text_len * 1279, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1280, "@param Events per Report.", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 1281, "A default limit of 100 rows will be pres"
	    "ented in", text_len, (ftnlen)48);
    s_copy(text + text_len * 1282, "any report.  You can override this limit"
	    " with the", text_len, (ftnlen)49);
    s_copy(text + text_len * 1283, "SET DELUGE command or at the time a repo"
	    "rt is", text_len, (ftnlen)45);
    s_copy(text + text_len * 1284, "ready for output.", text_len, (ftnlen)17);
    s_copy(text + text_len * 1285, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1286, "@@Limits", text_len, (ftnlen)8);
    s_copy(text + text_len * 1287, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1288, "Help", text_len, (ftnlen)4);
    finish[29] = 1289;
    begin[30] = 1290;
    s_copy(text + text_len * 1289, "To view events in a report, you need to "
	    "issue a \"SELECT\" command. The form of", text_len, (ftnlen)77);
    s_copy(text + text_len * 1290, "this command is shown below (\"WHERE\" a"
	    "nd \"ORDER BY\" clauses are optional).", text_len, (ftnlen)74);
    s_copy(text + text_len * 1291, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1292, "SELECT a comma delimited", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 1293, "       list of unambiguous", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 1294, "       column names", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 1295, "FROM   a comma delimited", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 1296, "       list of tables-alias pairs", 
	    text_len, (ftnlen)33);
    s_copy(text + text_len * 1297, "WHERE    condition_1", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 1298, "   AND/OR condition_2", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 1299, "   ...", text_len, (ftnlen)6);
    s_copy(text + text_len * 1300, "   AND/OR condition_n", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 1301, "ORDER BY a comma delimited", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 1302, "         list of unambiguous", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 1303, "         column names", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 1304, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1305, "All but one of the conditions in the \"W"
	    "HERE\" clause have the form:", text_len, (ftnlen)66);
    s_copy(text + text_len * 1306, "@literal", text_len, (ftnlen)8);
    return 0;
} /* zzhlp013_ */

