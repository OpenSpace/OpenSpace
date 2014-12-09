/* zzhlp010.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP010 ( private help text ) */
/* Subroutine */ int zzhlp010_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 949, "You can see the current status of command"
	    " echoing by issuing", text_len, (ftnlen)60);
    s_copy(text + text_len * 950, "a show environment command.", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 951, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 952, "SHOW ENVIRONMENT", text_len, (ftnlen)16);
    s_copy(text + text_len * 953, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 954, "@@Echoing Translated Commands", text_len, (
	    ftnlen)29);
    s_copy(text + text_len * 955, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 956, "Help", text_len, (ftnlen)4);
    finish[16] = 957;
    begin[17] = 958;
    s_copy(text + text_len * 957, "Inspekt maintains a \"stack\" of the twen"
	    "ty most recently entered", text_len, (ftnlen)63);
    s_copy(text + text_len * 958, "commands.  Each new command is placed on"
	    " \"top\" of the other commands.", text_len, (ftnlen)69);
    s_copy(text + text_len * 959, "Once a command has twenty or more command"
	    "s on top of it, it is lost", text_len, (ftnlen)67);
    s_copy(text + text_len * 960, "and can not be retrieved.", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 961, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 962, "Commands in the stack are numbered from 1"
	    " to 20.  The most recently", text_len, (ftnlen)67);
    s_copy(text + text_len * 963, "issued command (the one on top) is number"
	    " 1.  The command issued", text_len, (ftnlen)64);
    s_copy(text + text_len * 964, "just prior to number 1,  is number 2, and"
	    " so on.", text_len, (ftnlen)48);
    s_copy(text + text_len * 965, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 966, "You can manipulate this stack with the co"
	    "mmands below.  Most", text_len, (ftnlen)60);
    s_copy(text + text_len * 967, "\"stack\" manipulation commands require t"
	    "hat you specify a particular", text_len, (ftnlen)67);
    s_copy(text + text_len * 968, "command.  You can do this by specifying t"
	    "he number of the command", text_len, (ftnlen)65);
    s_copy(text + text_len * 969, "(the number provide by RECALL) or by spec"
	    "ifying a pattern. If you", text_len, (ftnlen)65);
    s_copy(text + text_len * 970, "specify a command via a number, the numbe"
	    "r must be between 1 and", text_len, (ftnlen)64);
    s_copy(text + text_len * 971, "20. If you specify the command via a patt"
	    "ern, you are specifying", text_len, (ftnlen)64);
    s_copy(text + text_len * 972, "the last command whose first line matches"
	    " the pattern.  As a", text_len, (ftnlen)60);
    s_copy(text + text_len * 973, "convenience, the wild card '*' is automat"
	    "ically appended to the", text_len, (ftnlen)63);
    s_copy(text + text_len * 974, "specification pattern.  You can not suppr"
	    "ess the appended wild", text_len, (ftnlen)62);
    s_copy(text + text_len * 975, "card.", text_len, (ftnlen)5);
    s_copy(text + text_len * 976, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 977, "@setparamsize{RECALL pattern}", text_len, (
	    ftnlen)29);
    s_copy(text + text_len * 978, "@param  RECALL.", text_len, (ftnlen)15);
    s_copy(text + text_len * 979, "displays the commands in the reverse orde"
	    "r from the way you have", text_len, (ftnlen)64);
    s_copy(text + text_len * 980, "entered them.  The commands are numbered "
	    "so that you may easily", text_len, (ftnlen)63);
    s_copy(text + text_len * 981, "refer to them.  The stack is left unchang"
	    "ed.", text_len, (ftnlen)44);
    s_copy(text + text_len * 982, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 983, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 984, "@param RECALL number @cr", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 985, "       RECALL pattern.", text_len, (ftnlen)
	    22);
    s_copy(text + text_len * 986, "displays a specific command.  The stack i"
	    "s left unchanged.", text_len, (ftnlen)58);
    s_copy(text + text_len * 987, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 988, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 989, "@param DO number @cr", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 990, "       DO pattern.", text_len, (ftnlen)18);
    s_copy(text + text_len * 991, "re-executes the command specified. The re"
	    "-executed command is placed", text_len, (ftnlen)68);
    s_copy(text + text_len * 992, "on the top of the command stack.", 
	    text_len, (ftnlen)32);
    s_copy(text + text_len * 993, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 994, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 995, "@param EDIT number @cr", text_len, (ftnlen)
	    22);
    s_copy(text + text_len * 996, "       EDIT pattern.", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 997, "invokes your system editor and allows you"
	    " to use the editor", text_len, (ftnlen)59);
    s_copy(text + text_len * 998, "to modify the command.  When you exit the"
	    " editor, the new command", text_len, (ftnlen)65);
    s_copy(text + text_len * 999, "is executed and placed on the top of the "
	    "command stack.", text_len, (ftnlen)55);
    s_copy(text + text_len * 1000, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1001, "@@Editing Commands", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 1002, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1003, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1004, "Pattern Matching", text_len, (ftnlen)16);
    s_copy(text + text_len * 1005, "Setting The Editor", text_len, (ftnlen)18)
	    ;
    finish[17] = 1006;
    begin[18] = 1007;
    s_copy(text + text_len * 1006, "Inspekt recognizes two environment varia"
	    "bles: LEAPSECONDS and SCLK.", text_len, (ftnlen)67);
    s_copy(text + text_len * 1007, "If you want to make them available to In"
	    "spekt, you should create", text_len, (ftnlen)64);
    s_copy(text + text_len * 1008, "one or both of them prior to running Ins"
	    "pekt.", text_len, (ftnlen)45);
    s_copy(text + text_len * 1009, "@setparamsize{LEAPSECONDS}", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 1010, "@param LEAPSECONDS.", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 1011, "should point to a SPICE leapseconds kern"
	    "el.", text_len, (ftnlen)43);
    s_copy(text + text_len * 1012, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1013, "@param SCLK.", text_len, (ftnlen)12);
    s_copy(text + text_len * 1014, "should point to a SPICE Spacecraft Clock"
	    " kernel.", text_len, (ftnlen)48);
    s_copy(text + text_len * 1015, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1016, "Inspekt looks for these environment vari"
	    "ables at program initialization.", text_len, (ftnlen)72);
    s_copy(text + text_len * 1017, "If they are present, Inspekt will load t"
	    "he kernels pointed to by", text_len, (ftnlen)64);
    s_copy(text + text_len * 1018, "the environment variables.  This is the "
	    "only time these variables", text_len, (ftnlen)65);
    s_copy(text + text_len * 1019, "can be used by Inspekt.  You may not use"
	    " them in a LOAD command.", text_len, (ftnlen)64);
    s_copy(text + text_len * 1020, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1021, "If you should decide to use these enviro"
	    "nment variables make sure", text_len, (ftnlen)65);
    s_copy(text + text_len * 1022, "that they evaluate to the full path name"
	    " of the kernels.  You may", text_len, (ftnlen)65);
    s_copy(text + text_len * 1023, "set these  variables with C-shell comman"
	    "d \"setenv\"", text_len, (ftnlen)50);
    s_copy(text + (text_len << 10), "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1025, "setenv LEAPSECONDS (full path of leapsec"
	    "onds kernel)", text_len, (ftnlen)52);
    s_copy(text + text_len * 1026, "setenv SCLK        (full path of spacecr"
	    "aft clock kernel)", text_len, (ftnlen)57);
    s_copy(text + text_len * 1027, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1028, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1029, "In DCL you use logical variables instead"
	    " of environment variables.", text_len, (ftnlen)66);
    s_copy(text + text_len * 1030, "You set the logical variables using the "
	    "define command.", text_len, (ftnlen)55);
    s_copy(text + text_len * 1031, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1032, "DEFIND LEAPSECONDS DISK:[DIR.ECT.ORY]lea"
	    "p_file.ker", text_len, (ftnlen)50);
    s_copy(text + text_len * 1033, "DEFINE SCLK        DISK:[DIR.ECT.ORY]scl"
	    "k_file.ker", text_len, (ftnlen)50);
    s_copy(text + text_len * 1034, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1035, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1036, "@@Environment Variables", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 1037, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1038, "Help", text_len, (ftnlen)4);
    finish[18] = 1039;
    begin[19] = 1040;
    s_copy(text + text_len * 1039, "Every now and then, you will type a comm"
	    "and that Inspekt cannot", text_len, (ftnlen)63);
    return 0;
} /* zzhlp010_ */

