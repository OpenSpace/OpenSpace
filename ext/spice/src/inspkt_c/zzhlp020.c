/* zzhlp020.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP020 ( private help text ) */
/* Subroutine */ int zzhlp020_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 1850, "You may also control the placement of th"
	    "e report title using the command:", text_len, (ftnlen)73);
    s_copy(text + text_len * 1851, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1852, "SET TITLE JUSTIFICATION (1:1){ LEFT | CE"
	    "NTER | RIGHT }", text_len, (ftnlen)54);
    s_copy(text + text_len * 1853, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1854, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1855, "You can control how often the title are "
	    "placed in your report  by", text_len, (ftnlen)65);
    s_copy(text + text_len * 1856, "issuing a SET TITLE/HEADER FREQUENCY com"
	    "mand.  The syntax for this is:", text_len, (ftnlen)70);
    s_copy(text + text_len * 1857, "@exliteral", text_len, (ftnlen)10);
    s_copy(text + text_len * 1858, "SET TITLE  FREQUENCY (1:1){ 0", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 1859, "                          | 1ST", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 1860, "                          | FIRST", 
	    text_len, (ftnlen)33);
    s_copy(text + text_len * 1861, "                          | ALL", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 1862, "                          | EVERY @int(1"
	    ":)", text_len, (ftnlen)42);
    s_copy(text + text_len * 1863, "                          }", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 1864, "!endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1865, "The values mean respectively: on none of"
	    " the pages; on the first page only; on", text_len, (ftnlen)78);
    s_copy(text + text_len * 1866, "every page, and on the first page and ev"
	    "ery nth page following the first  page.", text_len, (ftnlen)79);
    s_copy(text + text_len * 1867, "(See SET PAGE ...for a description of th"
	    "e output page ).", text_len, (ftnlen)56);
    s_copy(text + text_len * 1868, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1869, "@@SET TITLE  ...", text_len, (ftnlen)16);
    s_copy(text + text_len * 1870, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1871, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1872, "SET HEADER  ...", text_len, (ftnlen)15);
    finish[49] = 1873;
    begin[50] = 1874;
    s_copy(text + text_len * 1873, "When you type", text_len, (ftnlen)13);
    s_copy(text + text_len * 1874, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1875, "EDIT command", text_len, (ftnlen)12);
    s_copy(text + text_len * 1876, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1877, "Inspekt writes the specified command to "
	    "a file, starts up some", text_len, (ftnlen)62);
    s_copy(text + text_len * 1878, "text editor on your system and loads the"
	    " file into the editor", text_len, (ftnlen)61);
    s_copy(text + text_len * 1879, "window.  However, the editor selected mi"
	    "ght not be one you are", text_len, (ftnlen)62);
    s_copy(text + text_len * 1880, "familiar with.  This can be terribly fru"
	    "strating because you may", text_len, (ftnlen)64);
    s_copy(text + text_len * 1881, "not know how to get the editor to carry "
	    "out any familiar task.", text_len, (ftnlen)62);
    s_copy(text + text_len * 1882, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1883, "You can determine the default editor by "
	    "typing", text_len, (ftnlen)46);
    s_copy(text + text_len * 1884, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1885, "SHOW ENVIRONMENT", text_len, (ftnlen)16);
    s_copy(text + text_len * 1886, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1887, "If the editor listed there is not the on"
	    "e you prefer you can", text_len, (ftnlen)60);
    s_copy(text + text_len * 1888, "change to some other editor by typing", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 1889, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1890, "SET EDITOR edit_launching_command", 
	    text_len, (ftnlen)33);
    s_copy(text + text_len * 1891, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1892, "where \"edit_launching_command\" is the "
	    "command you type to", text_len, (ftnlen)57);
    s_copy(text + text_len * 1893, "invoke your favorite editor on your comp"
	    "uter.  If you use an alias", text_len, (ftnlen)66);
    s_copy(text + text_len * 1894, "for this command, you should look up wha"
	    "t this alias translates", text_len, (ftnlen)63);
    s_copy(text + text_len * 1895, "to and use that translation.", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 1896, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1897, "@@Setting The Editor", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 1898, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1899, "Help", text_len, (ftnlen)4);
    finish[50] = 1900;
    begin[51] = 1901;
    s_copy(text + text_len * 1900, "Inspekt allows you to adjust the working"
	    " session environment", text_len, (ftnlen)60);
    s_copy(text + text_len * 1901, "as well as the appearance of its outputs"
	    " to suit your needs.", text_len, (ftnlen)60);
    s_copy(text + text_len * 1902, "You may control:", text_len, (ftnlen)16);
    s_copy(text + text_len * 1903, "@newlist", text_len, (ftnlen)8);
    s_copy(text + text_len * 1904, "@numitem the format of reports that resu"
	    "lt from the SELECT command;", text_len, (ftnlen)67);
    s_copy(text + text_len * 1905, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1906, "@numitem the format used when printing t"
	    "ime strings;", text_len, (ftnlen)52);
    s_copy(text + text_len * 1907, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1908, "@numitem the width and justification(rig"
	    "ht/left) of columns that", text_len, (ftnlen)64);
    s_copy(text + text_len * 1909, "appear in reports;", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 1910, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1911, "@numitem the size of the \"page\" on whi"
	    "ch reports appear;", text_len, (ftnlen)56);
    s_copy(text + text_len * 1912, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1913, "@numitem the frequency with which report"
	    " titles and report headers appear.", text_len, (ftnlen)74);
    s_copy(text + text_len * 1914, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1915, "To change current settings use the \"SE"
	    "T\" command. To examine current", text_len, (ftnlen)68);
    s_copy(text + text_len * 1916, "settings use the \"SHOW\" command.", 
	    text_len, (ftnlen)32);
    s_copy(text + text_len * 1917, "@@Setting up Inspekt --- SET", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 1918, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1919, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1920, "Columns", text_len, (ftnlen)7);
    s_copy(text + text_len * 1921, "Display Area", text_len, (ftnlen)12);
    s_copy(text + text_len * 1922, "Reports", text_len, (ftnlen)7);
    s_copy(text + text_len * 1923, "SET COLUMN ...", text_len, (ftnlen)14);
    s_copy(text + text_len * 1924, "SET FORMAT ...", text_len, (ftnlen)14);
    s_copy(text + text_len * 1925, "SET HEADER ...", text_len, (ftnlen)14);
    s_copy(text + text_len * 1926, "SET PAGE   ...", text_len, (ftnlen)14);
    s_copy(text + text_len * 1927, "SET TIME   ...", text_len, (ftnlen)14);
    s_copy(text + text_len * 1928, "SET TITLE  ...", text_len, (ftnlen)14);
    s_copy(text + text_len * 1929, "Time Formats", text_len, (ftnlen)12);
    s_copy(text + text_len * 1930, "Numeric Formats", text_len, (ftnlen)15);
    s_copy(text + text_len * 1931, "Other Settings", text_len, (ftnlen)14);
    s_copy(text + text_len * 1932, "Current Settings   --- SHOW", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 1933, " ", text_len, (ftnlen)1);
    finish[51] = 1934;
    begin[52] = 1935;
    s_copy(text + text_len * 1934, "If you already know the title of a help "
	    "topic you'd like to see, you don't", text_len, (ftnlen)74);
    s_copy(text + text_len * 1935, "have to navigate through the help system"
	    "s series of menus.  Instead, you", text_len, (ftnlen)72);
    s_copy(text + text_len * 1936, "can bring up the help topic immediately."
	    "  To do this you add the topic", text_len, (ftnlen)70);
    s_copy(text + text_len * 1937, "title to the HELP command.", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 1938, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1939, "Inspekt> HELP topic title;", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 1940, "|endliteral", text_len, (ftnlen)11);
    return 0;
} /* zzhlp020_ */

