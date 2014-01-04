/*

-Procedure et2utc_c ( Ephemeris Time to UTC )

-Abstract

   Convert an input time from ephemeris seconds past J2000
   to Calendar, Day-of-Year, or Julian Date format, UTC.

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

   void et2utc_c (  SpiceDouble       et,
                    ConstSpiceChar  * format,
                    SpiceInt          prec,
                    SpiceInt          lenout,
                    SpiceChar       * utcstr   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   et         I   Input epoch, given in ephemeris seconds past J2000.
   format     I   Format of output epoch.
   prec       I   Digits of precision in fractional seconds or days.
   lenout     I   The length of the output string plus 1.
   utcstr     O   Output time string, UTC.

-Detailed_Input

   et          is the input epoch, ephemeris seconds past J2000.

   format      is the format of the output time string. It may be
               any of the following:


                 "C"      Calendar format, UTC.

                 "D"      Day-of-Year format, UTC.

                 "J"      Julian Date format, UTC.

                 "ISOC"   ISO Calendar format, UTC.

                 "ISOD"   ISO Day-of-Year format, UTC.

   prec        is the number of digits of precision to which
               fractional seconds (for Calendar and Day-of-Year
               formats) or days (for Julian Date format) are to
               be computed. If PREC is zero or smaller, no decimal
               point is appended to the output string. If PREC is
               greater than 14, it is treated as 14.

   lenout      The allowed length of the output string.  This length
               must large enough to hold the output string plus the
               null terminator.  If the output string is expected to
               have x characters, lenout must be x + 1.

-Detailed_Output

   utcstr      is the output time string equivalent to the input
               epoch, in the specified format.  Some examples are
               shown below.

                     "C"      "1986 APR 12 16:31:09.814"
                     "D"      "1986-102 // 16:31:12.814"
                     "J"      "JD 2446533.18834276"
                     "ISOC"   "1987-04-12T16:31:12.814"
                     "ISOD"   "1987-102T16:31:12.814"

               If an error occurs, utcstr is not changed.

               Fractional seconds, or for Julian dates, fractional
               days, are rounded to the precision level specified
               by the input argument `prec'.

               utcstr should be declared to be at least
               20 + prec characters in length to ensure
               sufficient room to hold calendar strings
               for modern epochs.  For epochs prior to
               1000 A.D. at least 24 + prec characters in
               length are required to hold the output
               calendar string.

               For epochs prior to 1000 A.D. Jan 1 calendar
               and day of year formats are returned with the
               era (A.D. or B.C.) attached to the year.  For
               example

                    "877 A.D. MAR 17 13:29:11.829"
                    "471 B.C. Jan 01 12:00:00.000"
                    "471 B.C. 001 // 12:00:00.000"

               ISO formats do not support the inclusion of
               an era.  For years prior to 1 A.D. an error
               will be signaled if ISO format has been requested.

-Parameters

   None.

-Exceptions

   1) If the format for the output string is not recognized, the
      error SPICE(INVALIDTIMEFORMAT) is signaled.

   2) If prec is less than or equal to zero, it is treated as
      zero.  If prec is greater than 14, it is treated as 14.

   3) If one of the ISO formats is specified (ISOC or ISOD) but
      the year corresponding to ET is prior to 1  A.D. on the
      Gregorian Calendar, the error SPICE(YEAROUTOFRANGE) will
      be signaled.

   4) Epochs prior to 15 Oct, 1582 on the Gregorian calendar (the
      calendar commonly used in western societies) are returned in the
      "extended" Gregorian Calendar.  To convert epochs to the Julian
      calendar see the header of the function gr2jul_ in the file
      jul2gr.c.

   5) This routine does not attempt to account for variations
      in the length of the second that were in effect prior
      to Jan 1, 1972.  For days prior to that date, we assume
      there are exactly 86400 ephemeris seconds. Consequently
      the UTC gregorian calendar strings produced for epochs
      prior to Jan 1, 1972 differ from the corresponding
      TDB calendar strings by approximately 41.18 seconds.
      (TDB gregorian calendar strings are produced by the
      routine ETCAL).

   6) The error SPICE(NULLPOINTER) is signaled if either of
      the input or output string pointers is null.

   7) If the input string has length zero, the error SPICE(EMPTYSTRING)
      will be signaled.

   8) The caller must pass a value indicating the length of the output
      string.  If this value is not at least 2, the error
      SPICE(STRINGTOOSHORT) is signaled.

   9) The user's processing environment must be properly initialized by
      loading a leapseconds kernel via the routine furnsh_c before
      calling this routine.  If a leapsecond kernel has not been
      loaded, an error will be signaled by a routine in the call tree
      of this routine.

-Files

   A leapseconds kernel must be loaded via furnsh_c prior to
   calling this routine.  The kernel need be loaded only  
   once during a program run. 

-Particulars
 
   This routine handles the task of converting a double precision
   representation of an epoch to a character string suitable for human
   consumption.  The more general routine timout_c may also be used to
   convert ET to time strings.

-Examples

   The numerical results shown for these examples may differ across
   platforms.  The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   Let the value of ET be -527644192.5403653 ephemeris seconds
   past J2000. Assuming that the nominal values in the kernel pool
   have not been altered, the following calls

      et2utc_c ( et, "C", 0, lenout, utcstr );
      et2utc_c ( et, "C", 3, lenout, utcstr );
      et2utc_c ( et, "D", 5, lenout, utcstr );
      et2utc_c ( et, "J", 7, lenout, utcstr );

   produce the following output strings

      1983 APR 13 12:09:14
      1983 APR 13 12:09:14.274
      1983-103 // 12:09:14.27400
      JD 2445438.0064152

   respectively, where lenout is the length of utcstr.

   An example program.

      #include <stdio.h>

      #include "SpiceUsr.h"

      int main()
      {
         /. 
         Local variables 
         ./
         SpiceDouble        et =  -527644192.5403653;

         SpiceChar        * leap    = "naif0007.tls";
         ConstSpiceChar   * format;
         SpiceChar        * utcstr;

         SpiceInt           prec;

         SpiceInt           lenout = 35;

         format             = "J";
         prec               =  6;

         /. 
         Load the leapseconds kernel. 
         ./
         furnsh_c ( leap );
         et2utc_c (  et , format, prec, lenout, utcstr );

         printf ( "ET:               %18.7f\n"
                  "Converted output: %s\n", 
                   et,
                   utcstr                       );

         return ( 0 );
      }


   The program output is

      ET:               -527644192.5403653
      Converted output: JD 2445438.006415


-Restrictions

   None.

-Literature_References

   Jesperson and Fitz-Randolph, From Sundials to Atomic Clocks,
   Dover Publications, New York, 1977.

-Author_and_Institution

   C.H. Acton      (JPL)
   N.J. Bachman    (JPL)
   B.V. Semenov    (JPL)
   W.L. Taber      (JPL)
   W.M. Owen       (JPL)
   I.M. Underwood  (JPL)
   E.D. Wright     (JPL)

-Version

   -CSPICE Version 1.1.5, 06-APR-2009 (NJB)

      Header was updated to state that fractional
      seconds or days are rounded in the output
      string.

   -CSPICE Version 1.1.4, 28-JAN-2008   (BVS)

      Fixed typo in the ISOC example string in Detailed_Output.

   -CSPICE Version 1.1.3, 16-JAN-2008   (EDW)

      Corrected typos in header titles:
      
      Detailed Input to Detailed_Input
      Detailed Output to Detailed_Output
      
   -CSPICE Version 1.1.2, 11-JAN-2006 (EDW)

      Added a CHKFSTR check call on the 'format' input string.

   -CSPICE Version 1.1.1, 29-JUL-2003 (NJB) (CHA)

      Various header changes were made to improve clarity and 
      more fully explain the routine's functionality.

   -CSPICE Version 1.1.0, 09-FEB-1998 (NJB) (EDW)

      Re-implemented routine without dynamically allocated, temporary
      strings. Added Exceptions section and corrected typo in chkout_c
      module name.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries

   ephemeris time to utc

-&
*/

{ /* Begin et2utc_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "et2utc_c" );

   /*
   Check the input strings to make sure the pointers are non-null
   and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "et2utc_c", format  );

   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "et2utc_c", utcstr, lenout );


   /*
   Call the f2c'd Fortran routine.
   */
   et2utc_( ( doublereal * ) &et,
            ( char       * ) format,
            ( integer    * ) &prec,
            ( char       * ) utcstr,
            ( ftnlen       ) strlen(format),
            ( ftnlen       ) lenout-1        );

   /*
   The string returned, utcstr, is a Fortranish type string.
   Convert the string to C type.
   */
   F2C_ConvertStr ( lenout, utcstr );

   chkout_c ( "et2utc_c" );


} /* End et2utc_c */
