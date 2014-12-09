/*

-Procedure timout_c ( Time Output )

-Abstract

   This routine converts an input epoch represented in TDB seconds
   past the TDB epoch of J2000 to a character string formatted to
   the specifications of a user's format picture.

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

   TIME

-Keywords

   TIME

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void timout_c ( SpiceDouble       et,
                   ConstSpiceChar  * pictur,
                   SpiceInt          lenout,
                   SpiceChar       * output )

/*

-Brief_I/O

   Variable  I/O  Description
   --------  ---  --------------------------------------------------
   et         I   An epoch in seconds past the ephemeris epoch J2000.
   pictur     I   A format specification for the output string.
   lenout     I   The length of the output string plus 1.
   output     O   A string representation of the input epoch.

-Detailed_Input

   et         a double precision representation of time in seconds
              past the ephemeris epoch J2000.

   pictur     is a string that specifies how the output should be
              presented.  The string is made up of various markers
              that stand for various components associated with
              a time.

              There are five types of markers that may appear in a
              format picture.  These are String Markers, Numeric
              Markers, Meta markers, Modifier Markers and Literal
              Markers.

              The pictur string is examined and the various markers
              are identified. The output time string is constructed
              by replacing each of the identified markers with
              an appropriate time component.

              The various markers and their meanings are discussed
              in the Particulars section below.

   lenout     The allowed length of the output string.  This length
              must large enough to hold the output string plus the
              terminator.  If the output string is expected to have x
              characters, lenout needs to be x + 1.

-Detailed_Output

   output     is the input epoch converted to the string format 
              described by pictur

-Parameters

   None.

-Exceptions

   1) The error SPICE(EMPTYSTRING) is signaled if the input
      string does not contain at least one character, since the
      input string cannot be converted to a Fortran-style string
      in this case.
      
   2) The error SPICE(NULLPOINTER) is signaled if either of 
      the input or output string pointers is null.

   3) The user must pass a value indicating the length of the output
      string.  If this value is not at least 2, the error
      SPICE(STRINGTOOSHORT) is signaled.

   The user's processing environment must be properly initialized by
   loading a leapseconds kernel via the routine furnsh_c before calling
   this routine.  If a leapsecond kernel has not been loaded, an error
   will be signaled by routines called by timout_c.

-Files

   A leapseconds kernel must be "loaded" via the routine furnsh_c
   prior to calling timout_c.

-Particulars


   A format picture is simply a string of letters that lets
   timout_c know where various components of a time representation
   should be placed during creation of the time string.
   Here's an example of such a picture:

      MON DD,YYYY  HR:MN:SC.#### (TDB) ::TDB

   Here is a sample of the times that would be created by
   using this format.

      JAN 12,1992  12:28:18.2772 (TDB)
      FEB 13,1994  23:18:25.2882 (TDB)
      AUG 21,1995  00:02:00.1881 (TDB)

   As you can see from the samples above, the format picture
   specifies that every time string created should begin
   with a three-letter abbreviation for the month, followed
   by a space and the day of the month. The day of month is
   followed immediately by a comma and the year. The year
   component is followed by two spaces. Next are the output hours,
   represented as a two digit integer, a colon, minutes as
   a two digit integer, another colon, and seconds rounded
   to 4 decimal places and having a two digit integer part.
   This is followed by a space and the string "(TDB)". The
   special marker "::TDB" in the time picture is an
   ``invisible'' marker. It is used to specify the time
   system that should be used in creating the time string
   (in this case Barycentric Dynamical Time).

   timout_c does not recognize all of the parts
   of the time format picture in the example above. The list
   of recognized parts and unrecognized parts are listed in
   the table below.

     Recognized       Unrecognized
     ----------       ------------
     "MON"            " "
     "DD"             ","
     "YYYY"           "  "
     "HR"             ":"
     "MN"             "(TDB)"
     "SC"
     ".####"
     "::TDB"

   The unrecognized parts are called literal markers.  They are
   copied exactly as they appear in pictur into the output string.
   The recognized parts of the picture are replaced by a
   component of time or, as in the case of `::TDB' are used
   as instructions about the overall properties of the time
   string.

   The full list of recognized markers, their classification
   and meaning are given below.

   MARKER       CLASS     MEANING
   -----------  --------  -----------------------------------------
   ".##..."     modifier  represent a numeric component that
                          immediately precedes this in a decimal
                          format.  Number of decimal places
                          equals the number of "#" characters
   "::GCAL"     meta      dates are reported in Gregorian Calendar
   "::JCAL"     meta      dates are reported in Julian Calendar
   "::MCAL"     meta      dates after 15 October, 1582 are reported
                          in Gregorian Calendar, before that
                          dates are reported in Julian Calendar

   "::RND"      meta      round output to places specified by
                          least significant component

   "::TDB"      meta      all components should be TDB

   "::TDT"      meta      all components should be TDT

   "::TRNC"     meta      truncate all output components (default)
   "::UTC"      meta      all components should be UTC (default)
   "::UTC+h:m"  meta      all components in UTC offset by +h (hours)
                          and +m (minutes) so as to allow time zones.
   "::UTC-h:m"  meta      all components in UTC offset by -h (hours)
                          and -m (minutes) so as to allow time zones.
   "AMPM"       string    String (either "A.M."  or "P.M.")
                          indicating whether hours are before
                          or after noon.
   "ampm"       string    String (either "a.m."  or "p.m.")
                          indicating whether hours are before
                          or after noon.
   "AP"         numeric   AM/PM equivalents of the hour component
                          of a time.
   "DD"         numeric   Day of month
   "DOY"        numeric   Day of year
   "ERA"        string    String (either "B.C."  or "A.D.") giving
                          era associated with an epoch.
   "era"        string    String (either "b.c."  or "a.d.") giving
                          era associated with an epoch.
   "HR"         numeric   hour component of time
   "JULIAND"    numeric   julian date component of time
   "MM"         numeric   numeric representation of month component
   "MN"         numeric   minute component of time
   "MON"        string    upper case three letter abbreviation for
                          month
   "Mon"        string    capitalized three letter abbreviation for
                          month
   "mon"        string    lower case three letter abbreviation for
                          month
   "MONTH"      string    upper case full name of month
   "Month"      string    capitalized full name of month
   "month"      string    lower case full name of month
   "SC"         numeric   seconds component of time
   "SP1950"     numeric   seconds past 1950 component of time
   "SP2000"     numeric   seconds past 2000 component of time
   "YR"         numeric   last two digits of year component of time
   "YYYY"       numeric   year component of time
   "WEEKDAY"    string    upper case day of week
   "Weekday"    string    capitalized day of week
   "weekday"    string    lower case day of week
   "WKD"        string    upper case three letter abbreviation for
                          day of week.
   "Wkd"        string    capitalized three letter abbreviation for
                          day of week.
   "wkd"        string    lower case three letter abbreviation for
                          day of week.

   String Markers

      String markers are portions of the format picture that
      will be replaced with a character string representing the
      corresponding component of a time.

   Numeric Markers

      Numeric markers are portions of the format picture that
      will be replaced with a decimal string that represents
      the corresponding component of a time.

   Meta Markers

      Meta markers (listed under the class ``meta'' in the
      table above) are used to indicate `global' properties of
      your time string. You may specify time scale and how
      rounding should be performed on the components of time
      in your output string. Meta markers may be placed anywhere
      in your format picture. They do not contribute to placement
      of characters in output time strings. Also there are no
      restrictions on how many meta markers you may place in
      the format picture. However, if you supply conflicting
      `meta' markers (for example ::TDT and ::TDB) in your
      picture the first marker listed (in left to right order)
      overrules the conflicting marker that appears later in
      the picture.

   Modifier Markers

      The numeric markers listed in the table above stand
      for integers unless they are modified through use of a
      modifier marker. The strings

         .#
         .##
         .###
         .####

      are used to this end. When a numeric marker is followed
      immediately by one of these modifiers, the corresponding
      time component will be written with the number of decimal
      places indicated by number of successive occurrences of
      the character "#". Any numeric token may be modified.

   Rounding vs. Truncation

      The meta markers ::TRNC and ::RND allow you to control
      how the output time picture is rounded. If you specify
      ::TRNC all components of time are simply truncated to
      the precision specified by the marker and any modifier.
      If you specify ::RND the output time is rounded to the
      least significant component of the format picture. The
      default action is truncation.

      Whether an output time string should be rounded or
      truncated depends upon what you plan to do with the
      string. For example suppose you simply want to get the
      calendar date associated with a time and not the time of
      day. Then you probably do not want round your output.
      Rounding 1992 Dec 31, 13:12:00 to the nearest day
      produces 1993 Jan 1. Thus in this case rounding is probably
      not appropriate.

      However, if you are producing output for plotting using
      Julian Date, seconds past an 1950 or or seconds past
      2000, you will probably want your output rounded so as
      to produce a smoother plot.

   Time Zones

      The meta markers ::UTC+h:m  and ::UTC-h:m  allow you
      offset UTC times so that you may represent times in
      a time zone other than GMT.  For example you can
      output times in Pacific Standard time by placing the
      meta-marker ::UTC-8 in your format picture.

      For example if you use the picture

         YYYY Mon DD, HR:MN:SC ::UTC

      You will get output strings such as:

         1995 Jan 03, 12:00:00

      If you use the picture


         YYYY Mon DD, HR:MN:SC ::UTC-8

      You will get output strings such as:

         1995 Jan 03, 04:00:00

      Finally, if you use the picture

         YYYY Mon DD, HR:MN:SC ::UTC-8:15

      You will get output string

         1995 Jan 03, 03:45:00

      Note that the minutes are always added or subtracted
      based on the sign present in the time zone specifier.
      In the case of ::UTC+h:m, minutes are added. In the
      case ::UTC-h:m, minutes are subtracted.

      The unsigned part of the hours component can be no more
      than 12.  The unsigned part of the minutes can be no more
      than 59.

   Calendars

      The calendar currently used by western countries is the
      Gregorian Calendar.  This calendar begins on Oct 15, 1582.
      Prior to Gregorian Calendar the Julian calendar was used
      The last Julian calendar date prior to the beginning
      of the Gregorian Calendar is Oct 5, 1582.

      The primary difference between the Julian and Gregorian
      calendars is in the determination of leap years.
      Nevertheless both can be formally extended backward and
      forward in time indefinitely.

      By default timout_c uses the Gregorian Calendar (::GCAL) in the
      determination of the output string.  However, you may
      specify that timout_c use the Julian Calendar (::JCAL) or a
      mixture of both (::MCAL).  If you specify ::MCAL, epochs
      that occur after the beginning of the Gregorian Calendar
      will be represented using the Gregorian Calendar, epochs
      prior to the beginning of the Gregorian calendar will
      be represented using the Julian Calendar.

   Getting Software to Construct Pictures for You

      Although it is not difficult to construct time format
      pictures, you do need to be aware of the various markers
      that may appear in a format picture.

      There is an alternative means for getting a format picture.
      The routine tpictr_c constructs format pictures from a sample
      time string.  For example suppose you would like your
      time strings to look like the basic pattern of the string
      below.

         "Fri Jul 26 12:22:09 PDT 1996"

      You can call tpictr_c with this string, and it will create
      the appropriate pictur for use with timout_c.

         tpictr_c ( "Fri Jul 26 12:22:09 PDT 1996", pictur, OK )

      The result will be:

         "Wkd Mon DD HR:MN:SC (PDT) ::UTC-7"

      Note: not every date that you can read is interpretable
      by tpictr_c.  For example, you might be able to understand
      that 19960212121116 is Feb 12 1996, 12:11:16.  However,
      tpictr_c cannot recognize this string.  Thus it is important
      to check the logical OK to make sure that tpictr_c was able
      to understand the time picture you provided.

      Even thought tpictr_c can not recognize every time pattern
      that has been used by various people, it does recognize
      nearly all patterns that you use when you want to communicate
      outside your particular circle of colleagues.

-Examples

   Suppose you need to create time strings similar to the
   default time string produced by the UNIX utility "date"
   (for example a string of the form "Thu Aug 01 09:47:16 PDT 1996")

   Make the following string assignment:

     pictur = "Wkd Mon DD HH:MN:SC PDT YYYY ::UTC-7";

   (Note the meta marker ::UTC-7 is used to adjust the output
   time system from UTC to PDT.  Also note that the substring PDT
   is a literal marker.  Without it, the time system would not
   appear in the output time string.

   Now for each time et for which an output time string is required
   make the call to timout_c below, and write the time string.

      timout_c ( et, pictur, lenout, string );
      printf ( "%s\n", string);

   Alternatively, you can let the routine tpictr_c create the timout_c
   time picture for you.

     tpictr_c ( "Thu Aug 01 09:47:16 PDT 1996", pictur, OK );

     if ( OK )
        {
        timout_c ( et, pictur, lenout, string );
        printf ( "%s\n", string);
        }

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   W.L. Taber     (JPL)

-Version

   -CSPICE Version 1.1.1, 14-AUG-2006   (EDW)

      Edited incorrect description of output. Replace mention of
      ldpool_c with furnsh_c.

   -CSPICE Version 1.1.0, 09-FEB-1998 (NJB)

      Re-implemented routine without dynamically allocated, temporary 
      strings.  Updated the Exceptions header section.
 
   -CSPICE Version 1.0.0, 25-OCT-1997   (EDW)

-Index_Entries

   Convert and format d.p. seconds past J2000 as a string

-&
*/

{ /* Begin timout_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "timout_c");


   /*
   Check the input string pictur to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "timout_c", pictur );

   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "timout_c", output, lenout );
   
   /*
   Call our beloved f2c'd routine
   */
   timout_( ( doublereal * ) &et ,
            ( char       * ) pictur,
            ( char       * ) output,
            ( ftnlen       ) strlen(pictur),
            ( ftnlen       ) lenout-1        );

   /*
   The string returned, output, is a Fortranish type string.
   Convert the string to C type. 
   */
   F2C_ConvertStr ( lenout, output );


   chkout_c ( "timout_c");

} /* End timout_c */
