/* zzhlp028.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP028 ( private help text ) */
/* Subroutine */ int zzhlp028_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 2602, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2603, "Help", text_len, (ftnlen)4);
    finish[65] = 2604;
    begin[66] = 2605;
    s_copy(text + text_len * 2604, "Listed here is the formal syntax for eve"
	    "ry", text_len, (ftnlen)42);
    s_copy(text + text_len * 2605, "command recognized by Inspekt.", text_len,
	     (ftnlen)30);
    s_copy(text + text_len * 2606, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2607, "@exliteral", text_len, (ftnlen)10);
    s_copy(text + text_len * 2608, "DEFINE   @name", text_len, (ftnlen)14);
    s_copy(text + text_len * 2609, "DEFINE   @name (1:)@word", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 2610, "UNDEFINE @name", text_len, (ftnlen)14);
    s_copy(text + text_len * 2611, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2612, "DISCARD", text_len, (ftnlen)7);
    s_copy(text + text_len * 2613, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2614, "DO @word", text_len, (ftnlen)8);
    s_copy(text + text_len * 2615, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2616, "ECHO", text_len, (ftnlen)4);
    s_copy(text + text_len * 2617, "NO ECHO", text_len, (ftnlen)7);
    s_copy(text + text_len * 2618, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2619, "EDIT @word", text_len, (ftnlen)10);
    s_copy(text + text_len * 2620, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2621, "EXIT", text_len, (ftnlen)4);
    s_copy(text + text_len * 2622, "HELP", text_len, (ftnlen)4);
    s_copy(text + text_len * 2623, "LOAD EK @word", text_len, (ftnlen)13);
    s_copy(text + text_len * 2624, "LOAD LEAPSECONDS @word", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 2625, "LOAD SCLK KERNEL @word", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 2626, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2627, "RECALL", text_len, (ftnlen)6);
    s_copy(text + text_len * 2628, "RECALL @word", text_len, (ftnlen)12);
    s_copy(text + text_len * 2629, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2630, "SAVE TO @word", text_len, (ftnlen)13);
    s_copy(text + text_len * 2631, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2632, "SAMPLE @int(1:)  SELECT (1:100)@word", 
	    text_len, (ftnlen)36);
    s_copy(text + text_len * 2633, "                 FROM   (1:)@word", 
	    text_len, (ftnlen)33);
    s_copy(text + text_len * 2634, "                (0:1){ WHERE    (1:)@wor"
	    "d    }", text_len, (ftnlen)46);
    s_copy(text + text_len * 2635, "                (0:1){ ORDER BY (1:100)@"
	    "word }", text_len, (ftnlen)46);
    s_copy(text + text_len * 2636, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2637, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2638, "SAMPLE          (1:1){ FIRST @int(1:)", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 2639, "                     | LAST  @int(1:)", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 2640, "                     }", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 2641, "                 SELECT (1:100)@word", 
	    text_len, (ftnlen)36);
    s_copy(text + text_len * 2642, "                 FROM   (1:)@word", 
	    text_len, (ftnlen)33);
    s_copy(text + text_len * 2643, "                (0:1){ WHERE    (1:)@wor"
	    "d    }", text_len, (ftnlen)46);
    s_copy(text + text_len * 2644, "                (0:1){ ORDER BY (1:100)@"
	    "word }", text_len, (ftnlen)46);
    s_copy(text + text_len * 2645, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2646, "SAMPLE @int(1:) (1:1){ UP TO       @int("
	    "0:100) EVERY @int(1:)", text_len, (ftnlen)61);
    s_copy(text + text_len * 2647, "                     | UP TO       @int("
	    "0:100)", text_len, (ftnlen)46);
    s_copy(text + text_len * 2648, "                     | STARTING AT @int("
	    "0:100) EVERY @int(1:)", text_len, (ftnlen)61);
    s_copy(text + text_len * 2649, "                     | STARTING AT @int("
	    "0:100)", text_len, (ftnlen)46);
    s_copy(text + text_len * 2650, "                     | CENTER   AT @int("
	    "0:100) EVERY @int(1:)", text_len, (ftnlen)61);
    s_copy(text + text_len * 2651, "                     | CENTER   AT @int("
	    "0:100)", text_len, (ftnlen)46);
    s_copy(text + text_len * 2652, "                     | FROM @int(0:100) "
	    "TO   @int(0:100)", text_len, (ftnlen)56);
    s_copy(text + text_len * 2653, "                     }", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 2654, "                 SELECT (1:100)@word", 
	    text_len, (ftnlen)36);
    s_copy(text + text_len * 2655, "                 FROM   (1:)@word", 
	    text_len, (ftnlen)33);
    s_copy(text + text_len * 2656, "                (0:1){ WHERE    (1:)@wor"
	    "d    }", text_len, (ftnlen)46);
    s_copy(text + text_len * 2657, "                (0:1){ ORDER BY (1:100)@"
	    "word }", text_len, (ftnlen)46);
    s_copy(text + text_len * 2658, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2659, "SELECT (1:100)@word FROM (1:)@word", 
	    text_len, (ftnlen)34);
    s_copy(text + text_len * 2660, "                    (0:1){ WHERE    (1:)"
	    "@word    }", text_len, (ftnlen)50);
    s_copy(text + text_len * 2661, "                    (0:1){ ORDER BY (1:1"
	    "00)@word }", text_len, (ftnlen)50);
    s_copy(text + text_len * 2662, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2663, "SET AUTOADJUST (1:1){ OFF | ASK | ON }", 
	    text_len, (ftnlen)38);
    s_copy(text + text_len * 2664, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2665, "SET COLUMN @word FORMAT (1:)@word }", 
	    text_len, (ftnlen)35);
    s_copy(text + text_len * 2666, "SET COLUMN @word HEADING (1:)@word", 
	    text_len, (ftnlen)34);
    s_copy(text + text_len * 2667, "SET COLUMN @word JUSTIFICATION (1:1){ LE"
	    "FT  | JUSTIFICATION }", text_len, (ftnlen)61);
    s_copy(text + text_len * 2668, "SET COLUMN @word WIDTH @int", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 2669, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2670, "SET DEFAULT TIME     FORMAT (1:)@word", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 2671, "SET DEFAULT INTEGER  FORMAT @word", 
	    text_len, (ftnlen)33);
    s_copy(text + text_len * 2672, "SET DEFAULT FLOATING FORMAT @word", 
	    text_len, (ftnlen)33);
    s_copy(text + text_len * 2673, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2674, "SET DELUGE WARNING @int(1:)", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 2675, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2676, "SET EDITOR (1:)@word", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 2677, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2678, "SET FORMAT (0:1){ SPACED | MARKED } TABU"
	    "LAR (0:1){ PRESERVED }", text_len, (ftnlen)62);
    s_copy(text + text_len * 2679, "SET FORMAT FLAGGED (0:1){ PRESERVED }", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 2680, "SET FORMAT MARK @word(%)", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 2681, "SET FORMAT VERBATIM", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 2682, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2683, "SET FORMAT DELIMITED (0:1){ PRESERVED }", 
	    text_len, (ftnlen)39);
    s_copy(text + text_len * 2684, "                     (0:2){ DELIMITER @w"
	    "ord(SPACE|%)", text_len, (ftnlen)52);
    s_copy(text + text_len * 2685, "                          | QUOTE @word("
	    "%) }", text_len, (ftnlen)44);
    s_copy(text + text_len * 2686, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2687, "SET HEADER FREQUENCY (1:1){ 0", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 2688, "                          | FIRST | 1ST", 
	    text_len, (ftnlen)39);
    s_copy(text + text_len * 2689, "                          | ALL", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 2690, "                          | EVERY @int(2"
	    ":) }", text_len, (ftnlen)44);
    s_copy(text + text_len * 2691, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2692, "SET HELP WAIT", text_len, (ftnlen)13);
    s_copy(text + text_len * 2693, "SET HELP NO WAIT", text_len, (ftnlen)16);
    s_copy(text + text_len * 2694, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2695, "SET PAGE HEIGHT @int", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 2696, "SET PAGE TITLE (1:)@word", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 2697, "SET PAGE WIDTH @int", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 2698, " ", text_len, (ftnlen)1);
    return 0;
} /* zzhlp028_ */

