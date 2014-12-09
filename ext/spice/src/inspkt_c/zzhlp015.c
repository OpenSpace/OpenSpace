/* zzhlp015.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP015 ( private help text ) */
/* Subroutine */ int zzhlp015_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 1401, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1402, "If picture does NOT have a leading zero "
	    "and", text_len, (ftnlen)43);
    s_copy(text + text_len * 1403, "the integer portion is not large enough "
	    "to fill up", text_len, (ftnlen)50);
    s_copy(text + text_len * 1404, "the space specified by the picture, outp"
	    "ut will be blank", text_len, (ftnlen)56);
    s_copy(text + text_len * 1405, "padded between the sign (if one is requi"
	    "red", text_len, (ftnlen)43);
    s_copy(text + text_len * 1406, "and the first character of the integer p"
	    "art of the", text_len, (ftnlen)50);
    s_copy(text + text_len * 1407, "number.", text_len, (ftnlen)7);
    s_copy(text + text_len * 1408, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1409, "If a decimal point ( '.' ) is present in"
	    " the picture it", text_len, (ftnlen)55);
    s_copy(text + text_len * 1410, "will be present following the integer po"
	    "rtion of", text_len, (ftnlen)48);
    s_copy(text + text_len * 1411, "output. Moreover, the decimal portion of"
	    " the output will", text_len, (ftnlen)56);
    s_copy(text + text_len * 1412, "contain the same number of digits as the"
	    "re are", text_len, (ftnlen)46);
    s_copy(text + text_len * 1413, "non-blank characters following the decim"
	    "al point in", text_len, (ftnlen)51);
    s_copy(text + text_len * 1414, "the picture.  However, if the picture co"
	    "ntains more than", text_len, (ftnlen)56);
    s_copy(text + text_len * 1415, "14 characters following the decimal poin"
	    "t, the", text_len, (ftnlen)46);
    s_copy(text + text_len * 1416, "characters in output that follow the 14 "
	    "position to", text_len, (ftnlen)51);
    s_copy(text + text_len * 1417, "the right of the decimal point will be z"
	    "eros.", text_len, (ftnlen)45);
    s_copy(text + text_len * 1418, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1419, "@@Numeric Formats", text_len, (ftnlen)17);
    s_copy(text + text_len * 1420, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1421, "Help", text_len, (ftnlen)4);
    finish[32] = 1422;
    begin[33] = 1423;
    s_copy(text + text_len * 1422, "You may control the order in which event"
	    "s are reported by a SELECT", text_len, (ftnlen)66);
    s_copy(text + text_len * 1423, "command through use of the \"ORDER BY\" "
	    "clause that is an optional", text_len, (ftnlen)64);
    s_copy(text + text_len * 1424, "part of all SELECT commands. The default"
	    " ordering is by the TIME", text_len, (ftnlen)64);
    s_copy(text + text_len * 1425, "column that is present in every E-kernel"
	    ". To order your output", text_len, (ftnlen)62);
    s_copy(text + text_len * 1426, "based upon some other column issue the S"
	    "ELECT command with the", text_len, (ftnlen)62);
    s_copy(text + text_len * 1427, "clause", text_len, (ftnlen)6);
    s_copy(text + text_len * 1428, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1429, "ORDER BY column(s) of choice", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 1430, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1431, "as part of your command.  Column names s"
	    "hould be separated by commas", text_len, (ftnlen)68);
    s_copy(text + text_len * 1432, "(,).  Note that the \"ORDER BY\" clause "
	    "works faster with indexed", text_len, (ftnlen)63);
    s_copy(text + text_len * 1433, "columns.  To see which columns are index"
	    "ed you can use any of", text_len, (ftnlen)61);
    s_copy(text + text_len * 1434, "the commands:", text_len, (ftnlen)13);
    s_copy(text + text_len * 1435, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1436, "SHOW INDEXES", text_len, (ftnlen)12);
    s_copy(text + text_len * 1437, "SHOW KERNELS", text_len, (ftnlen)12);
    s_copy(text + text_len * 1438, "SHOW SUMMARY", text_len, (ftnlen)12);
    s_copy(text + text_len * 1439, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1440, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1441, "@@Order By", text_len, (ftnlen)10);
    s_copy(text + text_len * 1442, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1443, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1444, "Looking at Data    --- SELECT", text_len, 
	    (ftnlen)29);
    finish[33] = 1445;
    begin[34] = 1446;
    s_copy(text + text_len * 1445, "@@Other Settings", text_len, (ftnlen)16);
    s_copy(text + text_len * 1446, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1447, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1448, "Autoadjust", text_len, (ftnlen)10);
    s_copy(text + text_len * 1449, "Deluge Warning", text_len, (ftnlen)14);
    s_copy(text + text_len * 1450, "Echoing Translated Commands", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 1451, "Making Help Wait", text_len, (ftnlen)16);
    s_copy(text + text_len * 1452, "Setting the Editor", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 1453, "Default Floating Format", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 1454, "Default Integer Format", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 1455, "Default Time Format", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 1456, " ", text_len, (ftnlen)1);
    finish[34] = 1457;
    begin[35] = 1458;
    s_copy(text + text_len * 1457, "It is often easier to describe a set of "
	    "character by specifying some", text_len, (ftnlen)68);
    s_copy(text + text_len * 1458, "common pattern shared by all of the stri"
	    "ngs of interest.  For this reason", text_len, (ftnlen)73);
    s_copy(text + text_len * 1459, "you can use a LIKE relation in the WHERE"
	    " clause of a SELECT command.", text_len, (ftnlen)68);
    s_copy(text + text_len * 1460, "You specify a like condition as shown be"
	    "low:", text_len, (ftnlen)44);
    s_copy(text + text_len * 1461, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1462, "column_name LIKE \"pattern\"", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 1463, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1464, "where \"pattern\" is a pattern that the "
	    "column must match for the", text_len, (ftnlen)63);
    s_copy(text + text_len * 1465, "LIKE-condition to be satisfied. Note tha"
	    "t the pattern must be enclosed", text_len, (ftnlen)70);
    s_copy(text + text_len * 1466, "in quotes (\").   There are two special "
	    "characters that may appear in a", text_len, (ftnlen)70);
    s_copy(text + text_len * 1467, "pattern. The asterisk '*' matches any su"
	    "bstring. The percent mark '%'", text_len, (ftnlen)69);
    s_copy(text + text_len * 1468, "matches any character.  If a pattern doe"
	    "s not begin/end with the asterisk,", text_len, (ftnlen)74);
    s_copy(text + text_len * 1469, "the column value must begin/end with the"
	    " the pattern character in order", text_len, (ftnlen)71);
    s_copy(text + text_len * 1470, "to match the pattern.  pattern matching "
	    "is case insensitive.", text_len, (ftnlen)60);
    s_copy(text + text_len * 1471, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1472, "Examples:  SPECIAL          matches S*C%"
	    "%L", text_len, (ftnlen)42);
    s_copy(text + text_len * 1473, "           SPECIAL does not match   S*C%"
	    "%L%", text_len, (ftnlen)43);
    s_copy(text + text_len * 1474, "           SPECIAL          matches %PE%"
	    "%AL", text_len, (ftnlen)43);
    s_copy(text + text_len * 1475, "           SPECIAL          matches S*L", 
	    text_len, (ftnlen)39);
    s_copy(text + text_len * 1476, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1477, "@@Pattern Matching", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 1478, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1479, "Help", text_len, (ftnlen)4);
    finish[35] = 1480;
    begin[36] = 1481;
    s_copy(text + text_len * 1480, "Patterns are sequences of characters tha"
	    "t are used to", text_len, (ftnlen)53);
    s_copy(text + text_len * 1481, "select a word or phrase from some list o"
	    "f words or phrases.", text_len, (ftnlen)59);
    s_copy(text + text_len * 1482, "The pattern begins at the first non-blan"
	    "k character in", text_len, (ftnlen)54);
    s_copy(text + text_len * 1483, "the character sequence.  The pattern end"
	    "s at the last non-blank", text_len, (ftnlen)63);
    s_copy(text + text_len * 1484, "character of the sequence.", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 1485, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1486, "There are two special characters that ma"
	    "y appear in a pattern", text_len, (ftnlen)61);
    s_copy(text + text_len * 1487, "they are '*' the substring \"wild card\""
	    " and '%' the", text_len, (ftnlen)50);
    s_copy(text + text_len * 1488, "character \"wild card\".", text_len, (
	    ftnlen)22);
    return 0;
} /* zzhlp015_ */

