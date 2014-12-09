/* tpictr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      TPICTR ( Create a Time Format Picture ) */
/* Subroutine */ int tpictr_(char *sample, char *pictur, logical *ok, char *
	error, ftnlen sample_len, ftnlen pictur_len, ftnlen error_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    doublereal tvec[10];
    logical mods;
    char type__[5];
    integer ntvec;
    logical succes, yabbrv;
    char modify[8*5];
    extern /* Subroutine */ int tpartv_(char *, doublereal *, integer *, char 
	    *, char *, logical *, logical *, logical *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen);

/* $ Abstract */

/*     Given a sample time string, create a time format picture */
/*     suitable for use by the routine TIMOUT. */

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

/*      None. */

/* $ Keywords */

/*      TIME */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SAMPLE     I   is a sample date time string */
/*     PICTUR     O   is a format picture that describes SAMPLE */
/*     OK         O   indicates success or failure to parse SAMPLE */
/*     ERROR      O   a diagnostic returned if SAMPLE cannot be parsed */

/* $ Detailed_Input */

/*     SAMPLE     is a representative time string that to use */
/*                as a model to format time strings. */

/* $ Detailed_Output */

/*     PICTUR     is a format picture suitable for use with the SPICE */
/*                routine TIMOUT.  This picture when used to format */
/*                the appropriate  epoch via TIMOUT will yield the same */
/*                time components in the same order as the components */
/*                in SAMPLE. */

/*                Picture should be declared to be at least 80 characters */
/*                in length.  If Picture is not sufficiently large */
/*                to contain the format picture, the picture will */
/*                be truncated on the right. */

/*     OK         is a logical flag.  If all of the components of SAMPLE */
/*                are recognizable, OK will be returned with the value */
/*                TRUE.  If some part of PICTUR cannot be parsed, */
/*                OK will be returned with the value FALSE. */

/*     ERROR      is a diagnostic message  that indicates what part of */
/*                SAMPLE was not recognizable.  If SAMPLE can be */
/*                successfully parsed, OK will be TRUE and ERROR will */
/*                be returned as a blank string.  If ERROR does not */
/*                have sufficient room (up to 400 characters) to */
/*                contain the full message, the message will be truncated */
/*                on the right. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) All problems with the inputs are diagnosed via OK and ERROR. */

/*     2) If a format picture can not be created from the sample */
/*        time string, PICTUR is returned as a blank string. */

/* $ Particulars */

/*     Although the routine TIMOUT provides SPICE users with a great */
/*     deal of flexibility in formatting time strings, users must */
/*     master the means by which a time picture is constructed */
/*     suitable for use by TIMOUT. */

/*     This routine allows SPICE users to supply a sample time string */
/*     from which a corresponding time format picture can be created, */
/*     freeing users from the task of mastering the intricacies of */
/*     the routine TIMOUT. */

/*     Note that TIMOUT can produce many time strings whose patterns */
/*     can not be discerned by this routine.  When such outputs are */
/*     called for, the user must consult TIMOUT and construct the */
/*     appropriate format picture "by hand".  However, these exceptional */
/*     formats are not widely used and are not generally recognizable */
/*     to an uninitiated reader. */

/* $ Examples */

/*     Suppose you need to print epochs corresponding to some */
/*     events and you wish the epochs to have the same arrangement */
/*     of components as in the string '10:23 P.M. PDT January 3, 1993' */

/*     The following subroutine call will construct the appropriate */
/*     format picture for use with TIMOUT. */

/*     CALL TPICTR ( '10:23 P.M. PDT January 3, 1993', PICTUR, OK, ERROR) */

/*     The resulting picture is: */

/*        'AP:MN AMPM PDT Month DD, YYYY ::UTC-7' */

/*     This picture can be used with TIMOUT to format a sequence */
/*     of epochs, ET(1),...,ET(N) (given as ephemeris seconds past J2000) */
/*     as shown in the loop below: */

/*        DO I = 1, N */
/*           CALL TIMOUT ( ET(I), PICTUR, STRING ) */
/*           WRITE (*,*) 'Epoch: ', I, ' --- ', STRING */
/*        END DO */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 16-MAR-1999 (WLT) */

/*        Corrected a minor spelling error in the header comments. */

/* -    SPICELIB Version 1.0.0, 10-AUG-1996 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Use a sample time string to produce a time format picture */

/* -& */

/*     This routine is really just a front for one aspect of */
/*     the routine TPARTV. */

    s_copy(error, " ", error_len, (ftnlen)1);
    tpartv_(sample, tvec, &ntvec, type__, modify, &mods, &yabbrv, &succes, 
	    pictur, error, sample_len, (ftnlen)5, (ftnlen)8, pictur_len, 
	    error_len);
    if (s_cmp(pictur, " ", pictur_len, (ftnlen)1) == 0) {
	*ok = FALSE_;
    } else {
	*ok = TRUE_;
	s_copy(error, " ", error_len, (ftnlen)1);
    }
    return 0;
} /* tpictr_ */

