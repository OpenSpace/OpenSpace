/*
 
-Procedure gcpool_c (Get character data from the kernel pool)
 
-Abstract
 
   Return the character value of a kernel variable from the
   kernel pool.
 
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
 
   KERNEL
 
-Keywords
 
   CONSTANTS
   FILES
 
*/
 
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
 
 
   void gcpool_c ( ConstSpiceChar * name,
                   SpiceInt         start,
                   SpiceInt         room,
                   SpiceInt         lenout,
                   SpiceInt       * n,
                   void           * cvals,
                   SpiceBoolean   * found )
 
/*
 
-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   name       I   Name of the variable whose value is to be returned.
   start      I   Which component to start retrieving for name
   room       I   The largest number of values to return.
   lenout     I   The length of the output string.
   n          O   Number of values returned for name.
   cvals      O   Values associated with name.
   found      O   True if variable is in pool.
 
-Detailed_Input
 
   name       is the name of the variable whose values are to be
              returned. If the variable is not in the pool with
              character type, found will be SPICEFALSE.
 
   start      is the index of the first component of name to return.
              The index follows the C convention of being 0 based.
              If start is less than 0, it will be treated as 0.  If
              start is greater than the total number of components
              available for name, no values will be returned (n will
              be set to zero).  However, found will still be set to
              SPICETRUE
 
   room       is the maximum number of components that should be
              returned for this variable.  (Usually it is the amount
              of room available in the array cvals). If room is
              less than 1 the error SPICE(BADARRAYSIZE) will be
              signaled.
 
   lenout     The allowed length of the output string.  This length
              must large enough to hold the output string plus the
              terminator.  If the output string is expected to have x
              characters, lenout needs to be x + 1. 
 
-Detailed_Output
 
   n          is the number of values associated with name that
              are returned.  It will always be less than or equal
              to room.
 
              If name is not in the pool with character type, no
              value is given to n.
 
   cvals      is the array of values associated with name.
              If name is not in the pool with character type, no
              values are given to the elements of cvals.
 
              If the length of cvals is less than the length of
              strings stored in the kernel pool (see MAXCHR) the
              values returned will be truncated on the right.
 
   found      is SPICETRUE if the variable is in the pool and has
              character type, SPICEFALSE if it is not.
 
-Parameters
 
   None.
 
-Exceptions
 
   1) If the value of room is less than one the error
      SPICE(BADARRAYSIZE) is signaled.
 
   2) If cvals has declared length less than the size of a
      string to be returned, the value will be truncated on
      the right.  See MAXCHR in pool.c for the maximum stored size of
      string variables.
 
   3) If the input string pointer is null, the error SPICE(NULLPOINTER)
      will be signaled.
 
   4) If the input string has length zero, the error SPICE(EMPTYSTRING)
      will be signaled.
 
   5) If the output string has length less than two characters, it
      is too short to contain one character of output data plus a null
      terminator, so it cannot be passed to the underlying Fortran
      routine.  In this event, the error SPICE(STRINGTOOSHORT) is
      signaled.
 
-Files
 
   None.
 
-Particulars
 
   This routine provides the user interface to retrieving
   character data stored in the kernel pool.  This interface
   allows you to retrieve the data associated with a variable
   in multiple accesses.  Under some circumstances this alleviates
   the problem of having to know in advance the maximum amount
   of space needed to accommodate all kernel variables.
 
   However, this method of access does come with a price. It is
   always more efficient to retrieve all of the data associated
   with a kernel pool data in one call than it is to retrieve
   it in sections.
 
   C requires the length of the output character array to be defined
   prior to calling the converted gcpool_c routine.  The size of the
   cvals output array is user defined and passed as the variable
   lenout.
 
   Also see the entry points gdpool_c and gipool_c.
 
-Examples
 
   The following code fragment demonstrates how the data stored
   in a kernel pool variable can be retrieved in pieces.  Using the
   kernel "test.ker" which contains
 
   \begindata
 
   CTEST_VAL = ('LARRY', 'MOE', 'CURLY' )
 
   ITEST_VAL = ( 3141, 186, 282 )
 
   DTEST_VAL = ( 3.1415, 186. , 282.397 )
 
 
   The program...
 
   #include <stdio.h>
   #include <string.h>
 
   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
 
   #define LENOUT 20
   #define NUMVALS 2
   #define START   1
 
   void main()
      {
 
      SpiceInt          n;
      SpiceChar         cvals[NUMVALS][LENOUT];
      SpiceBoolean      found;
      SpiceInt          i;
 
 
       ldpool_c ( "test.ker" );
 
 
       /.
       Get 2 values (NUMVALs) starting at the second value
       in the list (START).  Each value will be of length LENOUT.
       ./
 
       gcpool_c ( "CTEST_VAL", START, NUMVALS, LENOUT, &n, cvals,
                  &found );
 
       for ( i = 0; i < NUMVALS; i++ )
          {
          printf("%s\n", cvals[i] );
          }
 
       exit(0);
      }
 
 
   Will give output of
   MOE
   CURLY
 
 
-Restrictions
 
   None.
 
-Literature_References
 
   None.
 
-Author_and_Institution
 
   W.L. Taber  (JPL)
 
-Version

   -CSPICE Version 2.2.1 07-SEP-2007   (EDW)

      Edited the 'lenout' description in the Detailed_Input to
      remove the recommendation of 32 as a general use value
      for 'lenout'.

   -CSPICE Version 2.2.0 18-MAY-2001   (WLT)

      Added a cast to (char *) in the call to F2C_ConvertStrArr.
 
   -CSPICE Version 2.1.0 22-JUN-1999   (EDW)
 
      Added local variable to return boolean/logical values.  This
      fix allows the routine to function if int and long are different
      sizes.
 
   -CSPICE Version 2.0.3 09-FEB-1998   (EDW)
 
      Removed the output dynamically allocated string.  Conversion
      of cval from string to array now accomplished via the
      F2C_ConvertStrArray call.
 
   -CSPICE Version 2.0.2 01-FEB-1998   (EDW)
 
      Removed the input and work dynamically allocated strings.
 
   -CSPICE Version 2.0.1 28-JAN-1998   (EDW)
 
      The start parameter is now zero based as per C convention.
      Adjusted the amount of memory for the strings to lenout-1.
 
   -CSPICE Version 2.0.0 07-JAN-1998   (EDW)
 
     The routine now function properly for room > 1.  Previously
     only a single value could be returned.
 
   -CSPICE Version 1.0.0 23-OCT-1997   (EDW)
 
 
-Index_Entries
 
   RETURN the character value of a pooled kernel variable
   RETURN the string value of a pooled kernel variable
 
-&
*/
 
{ /* Begin gcpool_c */
 
 
   /*
   Local variables.
   */
   logical            yes;
 
 
   /* The index is zero based here but not in gcpool_. */
   start = start + 1;
 
 
   /*
   Participate in error tracing.
   */
   chkin_c ( "gcpool_c");
 
 
   /*
   Check the input string utcstr to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "gcpool_c", name );
 
 
   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "gcpool_c", cvals, lenout );
 
 
 
   /*
   Call the f2c'd routine
   */
 
   gcpool_( ( char    * ) name,
            ( integer * ) &start,
            ( integer * ) &room,
            ( integer * ) n,
            ( char    * ) cvals,
            ( logical * ) &yes,
            ( ftnlen    ) strlen(name),
            ( ftnlen    ) lenout - 1 );
 
  
   /* Cast back to a SpiceBoolean. */
   *found = yes;

   if ( *found )
   {
      /*
      cvals now contains the requested data in a single string
      lenout * n long.  We need to reform cvals into an array
      of n strings each lenout long.
      */
      F2C_ConvertTrStrArr ( *n, lenout, (char *)cvals );
   }
 
 
   /* Done.  Checkout. */
   chkout_c ( "gcpool_c");
 
} /* End gcpool_c */
 
 
