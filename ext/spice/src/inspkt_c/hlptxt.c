/* hlptxt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

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

/* $Procedure      HLPTXT ( Text for a help system ) */
/* Subroutine */ int hlptxt_0_(int n__, integer *n, char *buffer, ftnlen 
	buffer_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer size;
    static char text[80*3056];
    static integer i__, j, begin[76];
    extern integer sizec_(char *, ftnlen);
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen);
    static integer finish[76];
    extern /* Subroutine */ int ssizec_(integer *, char *, ftnlen), zzhlp000_(
	    integer *, integer *, char *, ftnlen), zzhlp001_(integer *, 
	    integer *, char *, ftnlen), zzhlp002_(integer *, integer *, char *
	    , ftnlen), zzhlp003_(integer *, integer *, char *, ftnlen), 
	    zzhlp004_(integer *, integer *, char *, ftnlen), zzhlp005_(
	    integer *, integer *, char *, ftnlen), zzhlp006_(integer *, 
	    integer *, char *, ftnlen), zzhlp007_(integer *, integer *, char *
	    , ftnlen), zzhlp008_(integer *, integer *, char *, ftnlen), 
	    zzhlp009_(integer *, integer *, char *, ftnlen), zzhlp010_(
	    integer *, integer *, char *, ftnlen), zzhlp011_(integer *, 
	    integer *, char *, ftnlen), zzhlp012_(integer *, integer *, char *
	    , ftnlen), zzhlp013_(integer *, integer *, char *, ftnlen), 
	    zzhlp014_(integer *, integer *, char *, ftnlen), zzhlp015_(
	    integer *, integer *, char *, ftnlen), zzhlp016_(integer *, 
	    integer *, char *, ftnlen), zzhlp017_(integer *, integer *, char *
	    , ftnlen), zzhlp018_(integer *, integer *, char *, ftnlen), 
	    zzhlp019_(integer *, integer *, char *, ftnlen), zzhlp020_(
	    integer *, integer *, char *, ftnlen), zzhlp021_(integer *, 
	    integer *, char *, ftnlen), zzhlp022_(integer *, integer *, char *
	    , ftnlen), zzhlp023_(integer *, integer *, char *, ftnlen), 
	    zzhlp024_(integer *, integer *, char *, ftnlen), zzhlp025_(
	    integer *, integer *, char *, ftnlen), zzhlp026_(integer *, 
	    integer *, char *, ftnlen), zzhlp027_(integer *, integer *, char *
	    , ftnlen), zzhlp028_(integer *, integer *, char *, ftnlen), 
	    zzhlp029_(integer *, integer *, char *, ftnlen), zzhlp030_(
	    integer *, integer *, char *, ftnlen), zzhlp031_(integer *, 
	    integer *, char *, ftnlen), zzhlp032_(integer *, integer *, char *
	    , ftnlen);

/*     This routine maintains a collection of text messages */
/*     that are intended for use in a program's help system. */

/*     The program was built with version 1.0 of the */
/*     SPICE program BuildHelp. */

/*     There are two entry points. */

/*     HLPTXT  returns buffers of text by numeric index */
/*     HLPSIZ  returns the total number of help messages */
/*             available. */

/*     The source files used to create this program were: */

/*      AboutHelp.hlp */
/*      Autoadjust.hlp */
/*      CollectingCommands.hlp */
/*      Column.hlp */
/*      ColumnTableAbbrev.hlp */
/*      Columns.hlp */
/*      CombiningTables.hlp */
/*      ConditionalOperators.hlp */
/*      CurrentSettings.hlp */
/*      CustomFormats.hlp */
/*      DefaultFloatingFormat.hlp */
/*      DefaultIntegerFormat.hlp */
/*      DefaultTimeFormat.hlp */
/*      DelimitedFormat.hlp */
/*      DelugeWarning.hlp */
/*      DisplayArea.hlp */
/*      EchoingTranslatedCommands.hlp */
/*      EditingCommands.hlp */
/*      EnvironmentVariables.hlp */
/*      Errors.hlp */
/*      ExampleTimeFormats.hlp */
/*      ExampleWhereClause.hlp */
/*      FlaggedFormat.hlp */
/*      FromClause.hlp */
/*      GettingTooMuchData.hlp */
/*      Glossary.hlp */
/*      Headers.hlp */
/*      Help.hlp */
/*      Kernels.hlp */
/*      Limits.hlp */
/*      LookingatData.hlp */
/*      MakingHelpWait.hlp */
/*      NumericFormats.hlp */
/*      OrderBy.hlp */
/*      OtherSettings.hlp */
/*      PatternMatching.hlp */
/*      Patterns.hlp */
/*      ProblemsSuggestions.hlp */
/*      Query.hlp */
/*      Reports.hlp */
/*      SamplingData.hlp */
/*      SavingWork.hlp */
/*      SelectClause.hlp */
/*      SetColumn.hlp */
/*      SetFormat.hlp */
/*      SetFormatMark.hlp */
/*      SetHeader.hlp */
/*      SetPage.hlp */
/*      SetTime.hlp */
/*      SetTitle.hlp */
/*      SettingTheEditor.hlp */
/*      SettingUpInspekt.hlp */
/*      ShortCuttoTopics.hlp */
/*      ShowColumn.hlp */
/*      ShowComments.hlp */
/*      ShowEnvironment.hlp */
/*      ShowFormat.hlp */
/*      ShowIndexed.hlp */
/*      ShowKernels.hlp */
/*      ShowPage.hlp */
/*      ShowSummary.hlp */
/*      SpecialSymbolsQueries.hlp */
/*      SpecifyingStrings.hlp */
/*      SpecifyingTimes.hlp */
/*      Symbol.hlp */
/*      SyntaxDescriptionLanguage.hlp */
/*      SyntaxSummaries.hlp */
/*      Table.hlp */
/*      TabularFormat.hlp */
/*      TabularFormatMark.hlp */
/*      TimeFormats.hlp */
/*      Titles.hlp */
/*      TypingCommands.hlp */
/*      UsingSymbols.hlp */
/*      VerbatimFormat.hlp */
/*      WhereClause.hlp */

/*     Spicelib Functions */
/*     Local Variables */
    /* Parameter adjustments */
    if (buffer) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_hlpsiz;
	}


/*     Reset the text buffer */

    size = sizec_(buffer, buffer_len);
    ssizec_(&size, buffer, buffer_len);
    if (*n < 1 || *n > 76) {

/*        There is no help for the value of N, simply */
/*        return with nothing in the buffer. */

	return 0;
    }
    if (! first) {

/*        Look up the help for this value of N */

	j = 0;
	i__3 = finish[(i__2 = *n - 1) < 76 && 0 <= i__2 ? i__2 : s_rnge("fin"
		"ish", i__2, "hlptxt_", (ftnlen)182)];
	for (i__ = begin[(i__1 = *n - 1) < 76 && 0 <= i__1 ? i__1 : s_rnge(
		"begin", i__1, "hlptxt_", (ftnlen)182)]; i__ <= i__3; ++i__) {
	    ++j;
	    s_copy(buffer + (j + 5) * buffer_len, text + ((i__1 = i__ - 1) < 
		    3056 && 0 <= i__1 ? i__1 : s_rnge("text", i__1, "hlptxt_",
		     (ftnlen)184)) * 80, buffer_len, (ftnlen)80);
	}
	scardc_(&j, buffer, buffer_len);
	return 0;
    }
    first = FALSE_;
    zzhlp000_(begin, finish, text, (ftnlen)80);
    zzhlp001_(begin, finish, text, (ftnlen)80);
    zzhlp002_(begin, finish, text, (ftnlen)80);
    zzhlp003_(begin, finish, text, (ftnlen)80);
    zzhlp004_(begin, finish, text, (ftnlen)80);
    zzhlp005_(begin, finish, text, (ftnlen)80);
    zzhlp006_(begin, finish, text, (ftnlen)80);
    zzhlp007_(begin, finish, text, (ftnlen)80);
    zzhlp008_(begin, finish, text, (ftnlen)80);
    zzhlp009_(begin, finish, text, (ftnlen)80);
    zzhlp010_(begin, finish, text, (ftnlen)80);
    zzhlp011_(begin, finish, text, (ftnlen)80);
    zzhlp012_(begin, finish, text, (ftnlen)80);
    zzhlp013_(begin, finish, text, (ftnlen)80);
    zzhlp014_(begin, finish, text, (ftnlen)80);
    zzhlp015_(begin, finish, text, (ftnlen)80);
    zzhlp016_(begin, finish, text, (ftnlen)80);
    zzhlp017_(begin, finish, text, (ftnlen)80);
    zzhlp018_(begin, finish, text, (ftnlen)80);
    zzhlp019_(begin, finish, text, (ftnlen)80);
    zzhlp020_(begin, finish, text, (ftnlen)80);
    zzhlp021_(begin, finish, text, (ftnlen)80);
    zzhlp022_(begin, finish, text, (ftnlen)80);
    zzhlp023_(begin, finish, text, (ftnlen)80);
    zzhlp024_(begin, finish, text, (ftnlen)80);
    zzhlp025_(begin, finish, text, (ftnlen)80);
    zzhlp026_(begin, finish, text, (ftnlen)80);
    zzhlp027_(begin, finish, text, (ftnlen)80);
    zzhlp028_(begin, finish, text, (ftnlen)80);
    zzhlp029_(begin, finish, text, (ftnlen)80);
    zzhlp030_(begin, finish, text, (ftnlen)80);
    zzhlp031_(begin, finish, text, (ftnlen)80);
    zzhlp032_(begin, finish, text, (ftnlen)80);

/*     Look up the help for this value of N */

    j = 0;
    i__2 = finish[(i__1 = *n - 1) < 76 && 0 <= i__1 ? i__1 : s_rnge("finish", 
	    i__1, "hlptxt_", (ftnlen)231)];
    for (i__ = begin[(i__3 = *n - 1) < 76 && 0 <= i__3 ? i__3 : s_rnge("begin"
	    , i__3, "hlptxt_", (ftnlen)231)]; i__ <= i__2; ++i__) {
	++j;
	s_copy(buffer + (j + 5) * buffer_len, text + ((i__3 = i__ - 1) < 3056 
		&& 0 <= i__3 ? i__3 : s_rnge("text", i__3, "hlptxt_", (ftnlen)
		233)) * 80, buffer_len, (ftnlen)80);
    }
    scardc_(&j, buffer, buffer_len);
    return 0;
/*     Return the total number of text messages. */

L_hlpsiz:
    *n = 76;
    return 0;
} /* hlptxt_ */

/* Subroutine */ int hlptxt_(integer *n, char *buffer, ftnlen buffer_len)
{
    return hlptxt_0_(0, n, buffer, buffer_len);
    }

/* Subroutine */ int hlpsiz_(integer *n)
{
    return hlptxt_0_(1, n, (char *)0, (ftnint)0);
    }

