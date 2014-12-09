/* zzhlp001.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP001 ( private help text ) */
/* Subroutine */ int zzhlp001_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 91, "or a command that evaluates to one of thes"
	    "e commands as a result of", text_len, (ftnlen)67);
    s_copy(text + text_len * 92, "symbol substitution.", text_len, (ftnlen)20)
	    ;
    s_copy(text + text_len * 93, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 94, "If there is information that a procedure n"
	    "eeds, you can pass that", text_len, (ftnlen)65);
    s_copy(text + text_len * 95, "information to the procedure by creating o"
	    "ne or more symbols that", text_len, (ftnlen)65);
    s_copy(text + text_len * 96, "evaluate to the needed information.", 
	    text_len, (ftnlen)35);
    s_copy(text + text_len * 97, "@@Collecting Commands In Files", text_len, (
	    ftnlen)30);
    s_copy(text + text_len * 98, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 99, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 100, "Using Symbols", text_len, (ftnlen)13);
    finish[2] = 101;
    begin[3] = 102;
    s_copy(text + text_len * 101, "When we collect data about events (or any"
	    " other set of objects)", text_len, (ftnlen)63);
    s_copy(text + text_len * 102, "a decision must be made about what aspect"
	    "s of the event shall", text_len, (ftnlen)61);
    s_copy(text + text_len * 103, "be recorded.  Usually some of these aspec"
	    "ts (attributes) of events", text_len, (ftnlen)66);
    s_copy(text + text_len * 104, "will change from one event to the next.  "
	    "It is the variation in these", text_len, (ftnlen)69);
    s_copy(text + text_len * 105, "attributes that allow us to distinguish o"
	    "ne event from another.", text_len, (ftnlen)63);
    s_copy(text + text_len * 106, "An individual attribute of the event is c"
	    "alled a column of the event.", text_len, (ftnlen)69);
    s_copy(text + text_len * 107, "This term arises from the common way in w"
	    "hich data is presented", text_len, (ftnlen)63);
    s_copy(text + text_len * 108, "on a page.  If we list all of the events "
	    "we have", text_len, (ftnlen)48);
    s_copy(text + text_len * 109, "recorded on a sheet of paper (or a termin"
	    "al) so that the attributes", text_len, (ftnlen)67);
    s_copy(text + text_len * 110, "for each event are always listed in the s"
	    "ame order from left to right,", text_len, (ftnlen)70);
    s_copy(text + text_len * 111, "then the attributes for different events "
	    "appear in columns on", text_len, (ftnlen)61);
    s_copy(text + text_len * 112, "the page.", text_len, (ftnlen)9);
    s_copy(text + text_len * 113, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 114, "@@Column", text_len, (ftnlen)8);
    s_copy(text + text_len * 115, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 116, "Help", text_len, (ftnlen)4);
    finish[3] = 117;
    begin[4] = 118;
    s_copy(text + text_len * 117, "When someone creates a table and column n"
	    "ames for the table they", text_len, (ftnlen)64);
    s_copy(text + text_len * 118, "are presented with the following problem."
	    "  The various names", text_len, (ftnlen)60);
    s_copy(text + text_len * 119, "should be meaningful and they should be e"
	    "asy to type.  If an easy", text_len, (ftnlen)65);
    s_copy(text + text_len * 120, "to type name is not meaningful, it won't "
	    "be very useful in describing", text_len, (ftnlen)69);
    s_copy(text + text_len * 121, "the table or a column.  Since tables and "
	    "columns are usually meant", text_len, (ftnlen)66);
    s_copy(text + text_len * 122, "to exist for a long time, table producers"
	    " often err on the side", text_len, (ftnlen)63);
    s_copy(text + text_len * 123, "of creating names that are meaningful but"
	    " are a bit difficult", text_len, (ftnlen)61);
    s_copy(text + text_len * 124, "to type.", text_len, (ftnlen)8);
    s_copy(text + text_len * 125, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 126, "Inspekt helps you deal with the problem o"
	    "f minimizing the amount", text_len, (ftnlen)64);
    s_copy(text + text_len * 127, "of text you have to type to specify a tab"
	    "le or column by allowing", text_len, (ftnlen)65);
    s_copy(text + (text_len << 7), "you to use a pattern instead of the full"
	    " name.  For example", text_len, (ftnlen)59);
    s_copy(text + text_len * 129, "suppose that a column has the name EMPLOY"
	    "EE_SALARY.  If there are no", text_len, (ftnlen)68);
    s_copy(text + text_len * 130, "other columns that start with \"EMP\", yo"
	    "u can specify this column by", text_len, (ftnlen)67);
    s_copy(text + text_len * 131, "typing \"EMP*\".  If there are several co"
	    "lumn names that start with", text_len, (ftnlen)65);
    s_copy(text + text_len * 132, "\"EMPLOYEE_\" but only one column name en"
	    "ds with \"SALARY\" you can", text_len, (ftnlen)63);
    s_copy(text + text_len * 133, "specify the column by typing \"*_SAL*\". "
	    " As long as only one name", text_len, (ftnlen)64);
    s_copy(text + text_len * 134, "matches the pattern, Inspekt will recogni"
	    "ze the name and treat the", text_len, (ftnlen)66);
    s_copy(text + text_len * 135, "pattern as if you had typed the full name."
	    , text_len, (ftnlen)42);
    s_copy(text + text_len * 136, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 137, "For example suppose that you have loaded "
	    "an E-kernel that contains", text_len, (ftnlen)66);
    s_copy(text + text_len * 138, "two tables \"EMPLOYEE_DATA\"  and \"DEPAR"
	    "TMENT_DATA\".  Moreover", text_len, (ftnlen)60);
    s_copy(text + text_len * 139, "suppose that the first table contains the"
	    " following columns:", text_len, (ftnlen)60);
    s_copy(text + text_len * 140, "\"EMPLOYEE_NAME\",  \"SUPERVISOR\", \"HIR"
	    "E_DATE\", \"SALARY\", \"DEPARTMENT\".", text_len, (ftnlen)68);
    s_copy(text + text_len * 141, "Suppose the second table has the followin"
	    "g columns:", text_len, (ftnlen)51);
    s_copy(text + text_len * 142, "\"DEPARTMENT_NAME\", \"OPERATING_EXPENSE"
	    "S\", \"MANAGER\", \"LOCATION\".", text_len, (ftnlen)63);
    s_copy(text + text_len * 143, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 144, "Then the following query", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 145, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 146, "SELECT EMP*, SAL*, DEP* FROM EMP* WHERE S"
	    "AL* > 30000;", text_len, (ftnlen)53);
    s_copy(text + text_len * 147, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 148, "is equivalent to the much longer command", 
	    text_len, (ftnlen)40);
    s_copy(text + text_len * 149, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 150, "SELECT EMPLOYEE_NAME, SALARY, DEPARTMENT", 
	    text_len, (ftnlen)40);
    s_copy(text + text_len * 151, "FROM EMPLOYEE_DATA", text_len, (ftnlen)18);
    s_copy(text + text_len * 152, "WHERE SALARY > 30000;", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 153, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 154, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 155, "You can patterns only for column and tabl"
	    "e names.  You may not", text_len, (ftnlen)62);
    s_copy(text + text_len * 156, "use them to abbreviate other words of the"
	    " Inspekt command language.", text_len, (ftnlen)67);
    s_copy(text + text_len * 157, "If some language words or phrases seem to"
	    " be a bit long to type", text_len, (ftnlen)63);
    s_copy(text + text_len * 158, "you should consider creating a symbol for"
	    " those words or phrases.", text_len, (ftnlen)65);
    s_copy(text + text_len * 159, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 160, "@@Column and Table Abbreviations", 
	    text_len, (ftnlen)32);
    s_copy(text + text_len * 161, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 162, "Pattern Matching", text_len, (ftnlen)16);
    s_copy(text + text_len * 163, "Using Symbols", text_len, (ftnlen)13);
    s_copy(text + text_len * 164, "Special Symbols --- Queries", text_len, (
	    ftnlen)27);
    finish[4] = 165;
    begin[5] = 166;
    s_copy(text + text_len * 165, "To see a list of all columns that are cur"
	    "rently available", text_len, (ftnlen)57);
    s_copy(text + text_len * 166, "in Inspekt, type the command", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 167, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 168, "SHOW SUMMARY", text_len, (ftnlen)12);
    s_copy(text + text_len * 169, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 170, "You will be presented with a list of colu"
	    "mn names in the form", text_len, (ftnlen)61);
    s_copy(text + text_len * 171, "\"table_name.column_name\" where \"table_"
	    "name\" is the name of the", text_len, (ftnlen)62);
    s_copy(text + text_len * 172, "table to which  the column belongs and \""
	    "column_name\" is the name", text_len, (ftnlen)64);
    s_copy(text + text_len * 173, "of the column.  To see attributes for a p"
	    "articular column, type", text_len, (ftnlen)63);
    s_copy(text + text_len * 174, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 175, "  SHOW COLUMN column_name", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 176, "or", text_len, (ftnlen)2);
    s_copy(text + text_len * 177, "  SHOW COLUMN table_name.column_name", 
	    text_len, (ftnlen)36);
    s_copy(text + text_len * 178, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 179, "The second form is required only if there"
	    " are two or more loaded", text_len, (ftnlen)64);
    s_copy(text + text_len * 180, "tables that have the same column.", 
	    text_len, (ftnlen)33);
    s_copy(text + text_len * 181, " ", text_len, (ftnlen)1);
    return 0;
} /* zzhlp001_ */

