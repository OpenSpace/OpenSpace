/* zzhlp012.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP012 ( private help text ) */
/* Subroutine */ int zzhlp012_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 1128, "tables may have columns having the same "
	    "name.  For this reason", text_len, (ftnlen)62);
    s_copy(text + text_len * 1129, "when you select items to be displayed in"
	    " a report via a", text_len, (ftnlen)55);
    s_copy(text + text_len * 1130, "SELECT or SAMPLE ... SELECT statement yo"
	    "u must specify which", text_len, (ftnlen)60);
    s_copy(text + text_len * 1131, "table the report is to be drawn from.  Y"
	    "ou do this via the", text_len, (ftnlen)58);
    s_copy(text + text_len * 1132, "FROM clause of the SELECT statement", 
	    text_len, (ftnlen)35);
    s_copy(text + text_len * 1133, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1134, "select  ...", text_len, (ftnlen)11);
    s_copy(text + text_len * 1135, "FROM    TABLE_1 [alias_1]", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 1136, "     [, TABLE_2 [alias_2] ...]", text_len,
	     (ftnlen)30);
    s_copy(text + text_len * 1137, "where   ...", text_len, (ftnlen)11);
    s_copy(text + text_len * 1138, "oder by ...", text_len, (ftnlen)11);
    s_copy(text + text_len * 1139, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1140, "The simplest commands involve only one t"
	    "able.  In such cases", text_len, (ftnlen)60);
    s_copy(text + text_len * 1141, "there is usually no need to supply an al"
	    "ias for the", text_len, (ftnlen)51);
    s_copy(text + text_len * 1142, "table specified in the FROM clause.", 
	    text_len, (ftnlen)35);
    s_copy(text + text_len * 1143, "@@From Clause", text_len, (ftnlen)13);
    s_copy(text + text_len * 1144, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1145, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1146, "Combining Tables", text_len, (ftnlen)16);
    s_copy(text + text_len * 1147, "Looking at Data    --- SELECT", text_len, 
	    (ftnlen)29);
    finish[23] = 1148;
    begin[24] = 1149;
    s_copy(text + text_len * 1148, "We anticipate that E-kernels may become "
	    "quite large. As a result", text_len, (ftnlen)64);
    s_copy(text + text_len * 1149, "the number of events that satisfy some m"
	    "atching criteria given in", text_len, (ftnlen)65);
    s_copy(text + text_len * 1150, "a  SELECT command might be very large.  "
	    "Since Inspekt does not yet", text_len, (ftnlen)66);
    s_copy(text + text_len * 1151, "support  a UNIX-like \"more\" function a"
	    "nd does not allow you to", text_len, (ftnlen)62);
    s_copy(text + text_len * 1152, "interrupt some task (via a key sequence "
	    "such as CTRL-C),  Inspekt", text_len, (ftnlen)65);
    s_copy(text + text_len * 1153, "has a user adjustable DATA-DELUGE WARNIN"
	    "G level.  Reports will  be", text_len, (ftnlen)66);
    s_copy(text + text_len * 1154, "generated automatically in response to a"
	    " SELECT command only if", text_len, (ftnlen)63);
    s_copy(text + text_len * 1155, "the number  of matching events is less t"
	    "han the data-deluge warning", text_len, (ftnlen)67);
    s_copy(text + text_len * 1156, "level.  If the number  of matching event"
	    "s is greater than this", text_len, (ftnlen)62);
    s_copy(text + text_len * 1157, "level, you will be notified and  given t"
	    "he option of producing the", text_len, (ftnlen)66);
    s_copy(text + text_len * 1158, "report, viewing a subsample of the repor"
	    "t, or cancelling the report.", text_len, (ftnlen)68);
    s_copy(text + text_len * 1159, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1160, "To set the deluge warning level, type th"
	    "e command.", text_len, (ftnlen)50);
    s_copy(text + text_len * 1161, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1162, "   SET DELUGE WARNING integer", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 1163, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1164, "If you take no action the warning level "
	    "has value 100.", text_len, (ftnlen)54);
    s_copy(text + text_len * 1165, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1166, "To see the current data-deluge warning l"
	    "evel type the command", text_len, (ftnlen)61);
    s_copy(text + text_len * 1167, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1168, "SHOW FORMAT", text_len, (ftnlen)11);
    s_copy(text + text_len * 1169, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1170, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1171, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1172, "@@Getting Too Much Data", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 1173, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1174, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1175, "Sampling Data", text_len, (ftnlen)13);
    s_copy(text + text_len * 1176, "SHOW FORMAT   ...", text_len, (ftnlen)17);
    finish[24] = 1177;
    begin[25] = 1178;
    s_copy(text + text_len * 1177, "@@Glossary", text_len, (ftnlen)10);
    s_copy(text + text_len * 1178, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1179, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1180, "Column", text_len, (ftnlen)6);
    s_copy(text + text_len * 1181, "Patterns", text_len, (ftnlen)8);
    s_copy(text + text_len * 1182, "Query", text_len, (ftnlen)5);
    s_copy(text + text_len * 1183, "Reports", text_len, (ftnlen)7);
    s_copy(text + text_len * 1184, "Symbol", text_len, (ftnlen)6);
    finish[25] = 1185;
    begin[26] = 1186;
    s_copy(text + text_len * 1185, "The only attribute that you can set that"
	    " globally affects headers", text_len, (ftnlen)65);
    s_copy(text + text_len * 1186, "is the header frequency.  To see the cur"
	    "rent frequency type the", text_len, (ftnlen)63);
    s_copy(text + text_len * 1187, "command", text_len, (ftnlen)7);
    s_copy(text + text_len * 1188, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1189, "SHOW PAGE;", text_len, (ftnlen)10);
    s_copy(text + text_len * 1190, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1191, "@@Headers", text_len, (ftnlen)9);
    s_copy(text + text_len * 1192, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1193, "Help", text_len, (ftnlen)4);
    finish[26] = 1194;
    begin[27] = 1195;
    s_copy(text + text_len * 1194, "@@Help", text_len, (ftnlen)6);
    s_copy(text + text_len * 1195, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1196, "About Help", text_len, (ftnlen)10);
    s_copy(text + text_len * 1197, "Typing Commands", text_len, (ftnlen)15);
    s_copy(text + text_len * 1198, "Kernels            --- LOAD", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 1199, "Looking at Data    --- SELECT", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 1200, "Setting up Inspekt --- SET", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 1201, "Current Settings   --- SHOW", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 1202, "Saving Work        --- SAVE TO", text_len,
	     (ftnlen)30);
    s_copy(text + text_len * 1203, "Errors", text_len, (ftnlen)6);
    s_copy(text + text_len * 1204, "Syntax Summaries", text_len, (ftnlen)16);
    s_copy(text + text_len * 1205, "Limits", text_len, (ftnlen)6);
    s_copy(text + text_len * 1206, "Glossary", text_len, (ftnlen)8);
    s_copy(text + text_len * 1207, "Problems, Suggestions", text_len, (ftnlen)
	    21);
    finish[27] = 1208;
    begin[28] = 1209;
    s_copy(text + text_len * 1208, "SPICE data is stored in data files calle"
	    "d kernels.  To make the", text_len, (ftnlen)63);
    s_copy(text + text_len * 1209, "data in these kernels available for retr"
	    "ieval and manipulation by", text_len, (ftnlen)65);
    s_copy(text + text_len * 1210, "Inspekt, you need to \"load\" the kernel"
	    "s.  When you load a kernel,", text_len, (ftnlen)65);
    s_copy(text + text_len * 1211, "Inspekt opens the file, reads the file, "
	    "and stores some (or all) of", text_len, (ftnlen)67);
    s_copy(text + text_len * 1212, "its contents in Inspekt's memory.", 
	    text_len, (ftnlen)33);
    return 0;
} /* zzhlp012_ */

