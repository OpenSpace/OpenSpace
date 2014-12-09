/*

-Procedure etcal_c ( Convert ET to Calendar format )

-Abstract

   Convert from an ephemeris epoch measured in seconds past
   the epoch of J2000 to a calendar string format using a
   formal calendar free of leapseconds.

-Disclaimer

   THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
   CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
   GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
   ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
   PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
   TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
   WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
   PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
   SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
   SOFTWARE AND RELATED MATERIALS, HOWEVER USED.

   IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
   BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
   LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
   INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
   REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
   REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.

   RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
   THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
   CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
   ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.

-Required_Reading

   None.

-Keywords

   TIME

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void etcal_c ( SpiceDouble   et,
                  SpiceInt      lenout,
                  SpiceChar   * string )

/*

-Brief_I/O

   Variable  I/O  Description
   --------  ---  --------------------------------------------------
   et         I   Ephemeris time measured in seconds past J2000.
   lenout     I   Length of output string.
   string     O   A standard calendar representation of et.

-Detailed_Input

   et       is an epoch measured in ephemeris seconds
            past the epoch of J2000.

   lenout   is the user defined length of the output string.
            The value should be at least 48 characters.

-Detailed_Output

   string   is a calendar string representing the input ephemeris
            epoch.  This string is based upon extending the
            Gregorian Calendar backward and forward indefinitely
            keeping the same rules for determining leap years.
            Moreover, there is no accounting for leapseconds.

            To be sure that all of the date can be stored in
            string, it should be declared to have length at
            least 48 characters.

            The string will have the following format

               year (era) mon day, hr:mn:sc.sss

            Where:

               year --- is the year
               era  --- is the chronological era associated with
                        the date.  For years after 999 A.D.
                        the era is omitted.  For years
                        between 1 A.D. and 999 A.D. (inclusive)
                        era is the string 'A.D.' For epochs
                        before 1 A.D. Jan 1 00:00:00, era is
                        given as 'B.C.' and the year is converted
                        to years before the "Christian Era".
                        The last B.C. epoch is

                          1 B.C. DEC 31, 23:59:59.999

                        The first A.D. epoch (which occurs .001
                        seconds after the last B.C. epoch) is:

                           1 A.D. JAN 1, 00:00:00.000

                        Note: there is no year 0 A.D. or 0 B.C.
               mon  --- is a 3-letter abbreviation for the month
                        in all capital letters.
               day  --- is the day of the month
               hr   --- is the hour of the day (between 0 and 23)
                        leading zeros are added to hr if the
                        numeric value is less than 10.
               mn   --- is the minute of the hour (0 to 59)
                        leading zeros are added to mn if the
                        numeric value is less than 10.
               sc.sss   is the second of the minute to 3 decimal
                        places ( 0 to 59.999).  Leading zeros
                        are added if the numeric value is less
                        than 10.  Seconds are truncated, not
                        rounded.


-Parameters

   None.

-Exceptions

   Error free.

   1) If the input et is so large that the corresponding
      number of days since 1 A.D. Jan 1, 00:00:00 is
      within 1 of overflowing or underflowing an integer,
      et will not be converted to the correct string
      representation rather, the string returned will
      state that the epoch was before or after the day
      that is min_integer_val + 1 or max_integer_val - 1 days
      after 1 A.D. Jan 1, 00:00:00.

   2) If the output string is not sufficiently long to hold
      the full date, it will be truncated on the right.

-Files

   None.

-Particulars

   This is an error free routine for converting ephemeris epochs
   represented as seconds past the J2000 epoch to formal
   calendar strings based upon the Gregorian Calendar.  This formal
   time is often useful when one needs a human recognizable
   form of an ephemeris epoch.  There is no accounting for leap
   seconds in the output times produced.

   Note: The calendar epochs produced are not the same as the
         UTC calendar epochs that correspond to et. The strings
         produced by this routine may vary from the corresponding
         UTC epochs by more than 1 minute.

   This routine can be used in creating error messages or
   in routines and programs in which one prefers to report
   times without employing leapseconds to produce exact UTC
   epochs.


-Examples

   Suppose you wish to  report that no data is
   available at a particular ephemeris epoch et.  The following
   code shows how you might accomplish this task.

   #define STRLEN   48

   etcal_c  ( et, STRLEN, string )

   printf ( "There is no data available for the body " );
   printf ( "at requested time: %s ( %f )", string, et );


-Restrictions

   One must keep in mind when using this routine that
   ancient times are not based upon the Gregorian
   calendar.  For example the 0 point of the Julian
   Date system is 4713 B.C. Jan 1, 12:00:00 on the Julian
   Calendar.  If one formalized the Gregorian calendar
   and extended it indefinitely, the zero point of the Julian
   date system corresponds to 4714 B.C. NOV 24 12:00:00 on
   the Gregorian calendar.  There are several reasons for this.
   Leap years in the Julian calendar occur every
   4 years (including *all* centuries).  Moreover,  the
   Gregorian calendar "effectively" begins on 15 Oct, 1582 A.D.
   which is 5 Oct, 1582 A.D. in the Julian Calendar.

   Therefore you must be careful in your interpretation
   of ancient dates produced by this routine.

-Literature_References

   1. "From Sundial to Atomic Clocks---Understanding Time and
       Frequency" by James Jespersen and Jane Fitz-Randolph
       Dover Publications, Inc. New York (1982).

-Author_and_Institution

   W.L. Taber      (JPL)
   K.R. Gehringer  (JPL)
   E.D. Wright     (JPL)

-Version

   -CSPICE Version 1.0.0, 5-MAR-1998   (EDW)

-Index_Entries

   Convert ephemeris time to a formal calendar date

-&
*/

{ /* Begin etcal_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "etcal_c" );


   /*
   Make sure the output string has at least enough room for one
   output character and a null terminator.  Also check for a null
   pointer.
   */

   CHKOSTR ( CHK_STANDARD, "etcal_c", string, lenout );


   etcal_( ( doublereal  * ) &et,
           ( char        * ) string,
           ( ftnlen        ) lenout - 1 );


   /* Convert the output string to C. */

   F2C_ConvertStr( lenout, string );


   chkout_c ( "etcal_c" );

} /* End etcal_c */
