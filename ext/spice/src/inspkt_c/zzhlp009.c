/* zzhlp009.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP009 ( private help text ) */
/* Subroutine */ int zzhlp009_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 858, "There are two versions of Delimited repor"
	    "ts: preserved and", text_len, (ftnlen)58);
    s_copy(text + text_len * 859, "non-preserved. The two forms are identica"
	    "l if all of the", text_len, (ftnlen)56);
    s_copy(text + text_len * 860, "columns of the report are scalar valued. "
	    "However, when some", text_len, (ftnlen)59);
    s_copy(text + text_len * 861, "column has more than one component, the t"
	    "wo reports are", text_len, (ftnlen)55);
    s_copy(text + text_len * 862, "different.", text_len, (ftnlen)10);
    s_copy(text + text_len * 863, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 864, "If you specify the report to be DELIMITED"
	    ", all componets of a column", text_len, (ftnlen)68);
    s_copy(text + text_len * 865, "are placed together in the output separat"
	    "ed by a space character.", text_len, (ftnlen)65);
    s_copy(text + text_len * 866, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 867, "If you specify the report to be DELIMITED"
	    " PRESERVED, each column is", text_len, (ftnlen)67);
    s_copy(text + text_len * 868, "regarded as having the same number of com"
	    "ponents -- the maximum number", text_len, (ftnlen)70);
    s_copy(text + text_len * 869, "of components of all columns in the row o"
	    "f the report. If n is the", text_len, (ftnlen)66);
    s_copy(text + text_len * 870, "maximum number of components for any requ"
	    "ested column of a matching", text_len, (ftnlen)67);
    s_copy(text + text_len * 871, "row, the row is written out in n-lines. T"
	    "he first line contains the", text_len, (ftnlen)67);
    s_copy(text + text_len * 872, "first component of each column, the secon"
	    "d line contains the second", text_len, (ftnlen)67);
    s_copy(text + text_len * 873, "component of each column, and so on. If a"
	    " column does not have an i'th", text_len, (ftnlen)70);
    s_copy(text + text_len * 874, "component, a blank character (unquoted) i"
	    "s written for that component.", text_len, (ftnlen)70);
    s_copy(text + text_len * 875, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 876, "@@Delimited Format", text_len, (ftnlen)18);
    s_copy(text + text_len * 877, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 878, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 879, "Reports", text_len, (ftnlen)7);
    finish[13] = 880;
    begin[14] = 881;
    s_copy(text + text_len * 880, "To see the current data-deluge warning le"
	    "vel type the command", text_len, (ftnlen)61);
    s_copy(text + text_len * 881, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 882, "SHOW FORMAT", text_len, (ftnlen)11);
    s_copy(text + text_len * 883, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 884, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 885, "To set the deluge warning level, type the"
	    " command.", text_len, (ftnlen)50);
    s_copy(text + text_len * 886, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 887, "SET DELUGE WARNING integer", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 888, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 889, "If you take no action the warning level h"
	    "as value 100.", text_len, (ftnlen)54);
    s_copy(text + text_len * 890, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 891, "@@Deluge Warning", text_len, (ftnlen)16);
    s_copy(text + text_len * 892, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 893, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 894, "Getting Too Much Data", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 895, "Sampling Data", text_len, (ftnlen)13);
    finish[14] = 896;
    begin[15] = 897;
    s_copy(text + text_len * 896, "The area of the terminal or terminal wind"
	    "ow where reports are", text_len, (ftnlen)61);
    s_copy(text + text_len * 897, "displayed is called the output page. Repo"
	    "rts may span several", text_len, (ftnlen)61);
    s_copy(text + text_len * 898, "output pages.  Inspekt allows you to adju"
	    "st the shape of the output", text_len, (ftnlen)67);
    s_copy(text + text_len * 899, "page and to control the titles and header"
	    "s that appear on the", text_len, (ftnlen)61);
    s_copy(text + text_len * 900, "page. The commands that allow you to mani"
	    "pulate the output page", text_len, (ftnlen)63);
    s_copy(text + text_len * 901, "are shown below.", text_len, (ftnlen)16);
    s_copy(text + text_len * 902, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 903, "   SET PAGE WIDTH number", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 904, "   SET PAGE HEIGHT number", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 905, "   SET PAGE TITLE title", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 906, "   SET TITLE JUSTIFICATION justification", 
	    text_len, (ftnlen)40);
    s_copy(text + text_len * 907, "   SET TITLE FREQUENCY frequency", 
	    text_len, (ftnlen)32);
    s_copy(text + text_len * 908, "   SET HEADER FREQUENCY frequency", 
	    text_len, (ftnlen)33);
    s_copy(text + text_len * 909, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 910, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 911, "The output page is described in terms of "
	    "printable fixed size", text_len, (ftnlen)61);
    s_copy(text + text_len * 912, "characters. By default it is 80 character"
	    "s wide and 24 characters", text_len, (ftnlen)65);
    s_copy(text + text_len * 913, "tall. The output page is a subset of  a p"
	    "age region that is", text_len, (ftnlen)59);
    s_copy(text + text_len * 914, "between 40 and 132 characters wide and at"
	    " least 22 characters tall.", text_len, (ftnlen)67);
    s_copy(text + text_len * 915, "(The maximum page height allowed is the m"
	    "aximum positive integer", text_len, (ftnlen)64);
    s_copy(text + text_len * 916, "that your computer supports)", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 917, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 918, "The page width is used to determine how m"
	    "uch text can fit across", text_len, (ftnlen)64);
    s_copy(text + text_len * 919, "your terminal window.  The page height is"
	    " used to determine how", text_len, (ftnlen)63);
    s_copy(text + text_len * 920, "many lines of text can be written before "
	    "displaying a page title", text_len, (ftnlen)64);
    s_copy(text + text_len * 921, "or report header.", text_len, (ftnlen)17);
    s_copy(text + text_len * 922, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 923, "@@Display Area", text_len, (ftnlen)14);
    s_copy(text + text_len * 924, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 925, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 926, "Reports", text_len, (ftnlen)7);
    s_copy(text + text_len * 927, "SET TIME   ...", text_len, (ftnlen)14);
    s_copy(text + text_len * 928, "SET HEADER ...", text_len, (ftnlen)14);
    s_copy(text + text_len * 929, "SET PAGE   ...", text_len, (ftnlen)14);
    finish[15] = 930;
    begin[16] = 931;
    s_copy(text + text_len * 930, "Although symbols can be a big help when u"
	    "sed well, they can", text_len, (ftnlen)59);
    s_copy(text + text_len * 931, "also interfere with what you think you've"
	    " typed.  Usually you", text_len, (ftnlen)61);
    s_copy(text + text_len * 932, "discover this problem as the result of an"
	    " error message.", text_len, (ftnlen)56);
    s_copy(text + text_len * 933, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 934, "You may want to see how Inspekt translate"
	    "d what you typed before it", text_len, (ftnlen)67);
    s_copy(text + text_len * 935, "gets around to acting on the command.  To"
	    " do this you can", text_len, (ftnlen)57);
    s_copy(text + text_len * 936, "\"turn on\" command echoing.  Do this by "
	    "typing", text_len, (ftnlen)45);
    s_copy(text + text_len * 937, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 938, "ECHO", text_len, (ftnlen)4);
    s_copy(text + text_len * 939, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 940, "Having issued this command, when a new co"
	    "mmand is issued that", text_len, (ftnlen)61);
    s_copy(text + text_len * 941, "contains symbols, Inspekt will \"echo\" t"
	    "he translated command", text_len, (ftnlen)60);
    s_copy(text + text_len * 942, "so that you can see how Inspekt has inter"
	    "preted your command.", text_len, (ftnlen)61);
    s_copy(text + text_len * 943, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 944, "To disable the echoing of commands type:", 
	    text_len, (ftnlen)40);
    s_copy(text + text_len * 945, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 946, "NO ECHO", text_len, (ftnlen)7);
    s_copy(text + text_len * 947, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 948, " ", text_len, (ftnlen)1);
    return 0;
} /* zzhlp009_ */

