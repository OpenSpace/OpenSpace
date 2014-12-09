/*

-Procedure tparse_c ( Parse a UTC time string )

-Abstract
 
   Parse a time string and return seconds past the J2000 epoch 
   on a formal calendar. 
 
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
 
   PARSING, TIME 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   

   void tparse_c ( ConstSpiceChar  * string,
                   SpiceInt          lenout,
                   SpiceDouble     * sp2000,
                   SpiceChar       * errmsg  ) 
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   string     I   Input time string, UTC. 
   lenout     I   Available space in output error message string.
   sp2000     O   Equivalent UTC seconds past J2000.
   errmsg     O   Descriptive error message. 
 
-Detailed_Input
 
   string      is an input time string, containing a Calendar or
               Julian Date.  It may be in several different formats
               and can make use of abbreviations. Several example
               strings and the times that they translate to are listed
               below.
               
   lenout      is the maximum number of characters, including the 
               terminating null, that may be written to the output
               error message string.
               
-Detailed_Output
 
   sp2000      is the equivalent of UTC, expressed in UTC 
               seconds past J2000. If an error occurs, or if 
               the input time string is ambiguous, sp2000 is not 
               changed. 

   errmsg      is a descriptive error message, which is empty when 
               no error occurs. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) 

-Files
 
   None. 
 
-Particulars
 
   The input string is examined and the various components of a date
   are identified: julian date, year, month, day of year, day of month,
   hour, minutes, seconds.  These items are assumed to be components on
   a calendar that contains no leapseconds (i.e. every day is assumed
   to have exactly 86400 seconds).

   tparse_c recognizes a wide range of standard time formats. The
   examples section contains a list of several common strings that are
   recognized and their interpretation. tparse_c relies on the lower
   lever routine TPARTV to interpret the input string.

   Here is a brief summary of some of the basic rules used in the
   interpretation of strings.

   1)  Unless the substring JD or jd is present the string is assumed to 
       be a calendar format (day-month-year or year and day of year).
       If the substring JD or jd is present, the string is assumed to
       represent a julian date.

   2)  If the julian date specifier is not present, any integer greater
       than 999 is regarded as being a year specification.

   3)  A dash '-' can represent a minus sign only if it is precedes the
       first digit in the string and the string contains the julian
       date specifier (JD).  (No negative years, months, days, etc are
       allowed).

   4)  Numeric components of a time string must be separated 
       by a character that is not a digit or decimal point. 
       Only one decimal component is allowed.  For example 
       1994219.12819 is sometimes interpreted as the 
       219th day of 1994 + 0.12819 days.  tparse_c does not 
       support such strings. 

       No exponential components are allowed.  For example you 
       can't input 1993 Jun 23 23:00:01.202E-4 you have 
       to explicitly list all zeros that follow the decimal 
       point: i.e.  1993 Jun 23 23:00:00.0001202 

   5)  The single colon (:) when used to separate numeric 
       components of a string is interpreted as separating 
       Hours, Minutes, and Seconds of time. 

   6)  If a double slash (//) or double colon (::) follows 
       a pair of integers, those integers are assumed  to 
       represent the year and day of year. 

   7)  A quote followed by an integer less than 100 is regarded 
       as an abbreviated year.  For example: '93 would be regarded 
       as the 93rd year of the reference century.  See TEXPYR 
       for further discussion of abbreviated years. 

    8) An integer followed by "B.C." or "A.D." is regarded as 
       a year in the era associated with that abbreviation. 

    9) All dates are regarded as belonging to the extended 
       Gregorian Calendar (the Gregorian calendar is the calendar 
       currently used by western society).  See the routine JUL2GR 
       for  converting from Julian Calendar to the 
       Gregorian Calendar. 
       western society). 

   10) When the size of the integer components does not clearly 
       specify a year the following patterns are assumed 

       Calendar Format 

           Year Month Day 
           Month Day Year 
           Year Day Month 

           Where Month is the name of a month, not its numeric 
           value. 

           When integer components are separated by slashes (/) 
           as in 3/4/5.  Month, Day, Year is assumed (2005 March 4) 

        Day of Year Format. 

           If a day of year marker is present (// or ::) the 
           pattern 

           I-I// or I-I:: (where I stands for and integer) 
           is interpreted as Year Day-of-Year. However, I-I/ is 
           regarded as ambiguous. 

   To understand the complete list of strings that can be understood 
   by tparse_c you need to examine TPARTV and read the appendix to 
   the TIME required reading entitled "Parsing Time Strings" 

   tparse_c does not support the specification of time system 
   such as TDT or TDB; AM/PM specifications of time; or time 
   zones (such as PDT, UTC+7:20, etc.). 

   If some part of the time string is not recognized or if 
   the meaning of the components are not clear, an error string 
   is constructed that explains the problem with the string. 

   Since the routine is works by breaking the input string into 
   a sequence of tokens whose meanings are determined by position 
   and magnitude, you can supply strings such as 1993 FEB 35 and 
   have this correctly interpreted as March 7, 1993.  However, 
   this default action can be modified so that only "proper" 
   calendar dates and times are recognized.  To do this call 
   the routine TPARCH as shown below: 

      TPARCH ( "YES" ) 

   This will cause the routine to treat dates and times with 
   components outside the normal range as errors. 

   To return to the default behavior 

      TPARCH ( "NO" ) 
 
-Examples
 
   The following are examples of valid inputs to TPARSE: 



   ISO (T) Formats. 

   String                        Year Mon  DOY DOM  HR Min Sec 
   ----------------------------  ---- ---  --- ---  -- --- ------ 
   1996-12-18T12:28:28           1996 Dec   na  18  12  28 28 
   1986-01-18T12                 1986 Jan   na  18  12  00 00 
   1986-01-18T12:19              1986 Jan   na  18  12  19 00 
   1986-01-18T12:19:52.18        1986 Jan   na  18  12  19 52.18 
   1995-08T18:28:12              1995  na  008  na  18  28 12 
   1995-18T                      1995  na  018  na  00  00 00 


   Calendar Formats. 

   String                        Year   Mon DOM  HR Min  Sec 
   ----------------------------  ----   --- ---  -- ---  ------ 
   Tue Aug  6 11:10:57  1996     1996   Aug  06  11  10  57 
   1 DEC 1997 12:28:29.192       1997   Dec  01  12  28  29.192 
   2/3/1996 17:18:12.002         1996   Feb  03  17  18  12.002 
   Mar 2 12:18:17.287 1993       1993   Mar  02  12  18  17.287 
   1992 11:18:28  3 Jul          1992   Jul  03  11  18  28 
   June 12, 1989 01:21           1989   Jun  12  01  21  00 
   1978/3/12 23:28:59.29         1978   Mar  12  23  28  59.29 
   17JUN1982 18:28:28            1982   Jun  17  18  28  28 
   13:28:28.128 1992 27 Jun      1992   Jun  27  13  28  28.128 
   1972 27 jun 12:29             1972   Jun  27  12  29  00 
   '93 Jan 23 12:29:47.289       1993*  Jan  23  12  29  47.289 
   27 Jan 3, 19:12:28.182        2027*  Jan  03  19  12  28.182 
   23 A.D. APR 4, 18:28:29.29    0023** Apr  04  18  28  29.29 
   18 B.C. Jun 3, 12:29:28.291   -017** Jun  03  12  29  28.291 
   29 Jun  30 12:29:29.298       2029+  Jun  30  12  29  29.298 
   29 Jun '30 12:29:29.298       2030*  Jun  29  12  29  29.298 

   Day of Year Formats 

   String                        Year  DOY HR Min Sec 
   ----------------------------  ----  --- -- --- ------ 
   1997-162::12:18:28.827        1997  162 12  18 28.827 
   162-1996/12:28:28.287         1996  162 12  28 28.287 
   1993-321/12:28:28.287         1993  231 12  28 28.287 
   1992 183// 12 18 19           1992  183 12  18 19 
   17:28:01.287 1992-272//       1992  272 17  28 01.287 
   17:28:01.282 272-1994//       1994  272 17  28 01.282 
   '92-271/ 12:28:30.291         1992* 271 12  28 30.291 
   92-182/ 18:28:28.281          1992* 182 18  28 28.281 
   182-92/ 12:29:29.192          0182+ 092 12  29 29.192 
   182-'92/ 12:28:29.182         1992  182 12  28 29.182 


   Julian Date Strings 

   jd 28272.291                  Julian Date   28272.291 
   2451515.2981 (JD)             Julian Date 2451515.2981 
   2451515.2981 JD               Julian Date 2451515.2981 

                                Abbreviations Used in Tables 

                                   na    --- Not Applicable 
                                   Mon   --- Month 
                                   DOY   --- Day of Year 
                                   DOM   --- Day of Month 
                                   Wkday --- Weekday 
                                   Hr    --- Hour 
                                   Min   --- Minutes 
                                   Sec   --- Sec 

   * The default interpretation of a year that has been abbreviated 
   with a leading quote as in 'xy (such as '92) is to treat 
   the year as 19xy if xy > 68 and to treat it is 20xy otherwise. 
   Thus '70 is interpreted as 1970 and '67 is treated as 2067. 
   However, you may change the "split point" and centuries through 
   use of the SPICE routine tsetyr_c which is an entry point in 
   the SPICE module TEXPYR.  See that routine for a discussion of 
   how you may reset the split point. 

   ** All epochs are regarded as belonging to the Gregorian 
   calendar.  We formally extend the Gregorian calendar backward 
   and forward in time for all epochs.  If you have epochs belonging 
   to the Julian Calendar, consult the routines TPARTV and JUL2GR 
   for a discussion concerning conversions to the Gregorian 
   calendar and ET. 

   +  When a day of year format or calendar format string is 
   input and neither of integer components of the date 
   is greater than 1000, the first integer 
   is regarded as being the year. 

   Any integer greater than 1000 
   is regarded as a year specification. Thus 1001-1821//12:28:28 
   is interpreted as specifying two years and will be rejected 
   as ambiguous. 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   J.M. Lynch      (JPL) 
   W.M. Owen       (JPL) 
   M.J. Spencer    (JPL) 
   I.M. Underwood  (JPL) 
   W.L. Taber      (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 5-JUN-1999 (NJB)(JML)(WMO)(MJS)(IMU)(WLT)

-Index_Entries
 
   parse a utc time string 
 
-&
*/

{ /* Begin tparse_c */



   /*
   Use discovery check-in.
   */


   /*
   Check the input time string to make sure the pointer is non-null and
   the string length is non-zero.
   */
   CHKFSTR ( CHK_DISCOVER, "tparse_c", string );


   /*
   Check the output error message string to make sure the pointer is 
   non-null and the string length is at least 2.
   */
   CHKOSTR ( CHK_DISCOVER, "tparse_c", errmsg, lenout );


   /*
   Call the f2c'd routine.
   */ 

   tparse_ (  ( char        * ) string,
              ( doublereal  * ) sp2000,
              ( char        * ) errmsg,
              ( ftnlen        ) strlen(string),
              ( ftnlen        ) lenout-1        );

   /*
   Convert the error message from Fortran to C style.
   */
   F2C_ConvertStr ( lenout, errmsg );
   
   
} /* End tparse_c */

