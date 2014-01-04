/*
 
-Procedure gnpool_c (Get names of kernel pool variables)
 
-Abstract
 
   Return names of kernel variables matching a specified template.
 
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
 
   #include <stdlib.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
 
 
   void gnpool_c ( ConstSpiceChar    * name,
                   SpiceInt            start,
                   SpiceInt            room,
                   SpiceInt            lenout,
                   SpiceInt          * n,
                   void              * kvars,
                   SpiceBoolean      * found  )
/*
 
-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   name       I   Template that names should match.
   start      I   Index of first matching name to retrieve.
   room       I   The largest number of values to return.
   lenout     I   Length of strings in output array kvars.
   n          O   Number of values returned for name.
   kvars      O   Kernel pool variables whose names match name.
   found      O   True if there is at least one match.
 
-Detailed_Input
 
   name       is a matchi_c template which will be used when searching
              for variable names in the kernel pool.  The characters
              '*' and '%' are used for the wild string and wild
              characters respectively.  For details of string
              pattern matching see the header of the routine matchi_c.
              'name' is restricted to a length of 32 characters or less.
 
   start      is the index of the first variable name to return that
              matches the name template.  The matching names are
              assigned indices ranging from 0 to NVAR-1, where NVAR is
              the number of matching names.  The index of a name does
              not indicate how it compares alphabetically to another
              name.
 
              If start is less than 0, it will be treated as 0.  If
              start is greater than the total number of matching
              variable names, no values will be returned and N will
              be set to zero.  However, found will still be set to
              SPICETRUE.
 
 
   room       is the maximum number of variable names that should
              be returned for this template.  If room is less than 1
              the error SPICE(BADARRAYSIZE) will be signaled.
 
   lenout     is the length of strings in the output array kvars.  This
              length includes room for the terminating null in each
              string.  To ensure that the output names are not
              truncated, lenout should be at least 33.
 
 
-Detailed_Output
 
   n          is the number of variable names matching name that are
              returned.  It will always be less than or equal to
              room.
 
              If no variable names match name, n is set to zero.
 
 
   kvars      is an array of kernel pool variables whose names match
              the template name and which have indices ranging from
              start to start+n-1.
 
              Note that in general the names returned in kvars are
              not sorted.
 
              If no variables match name, no values are assigned to
              the elements of kvars.
 
              If the length of kvars is less than the length of the
              variable names, the values returned will be truncated
              on the right.
 
              The declaration of kvars should be equivalent to
 
                 SpiceChar  kvars [room][lenout];
 
 
   found      is SPICETRUE if the some variable name in the kernel pool
              matches name, SPICEFALSE if it is not.
 
-Parameters
 
   None.
 
-Exceptions
 
   1) If the value of room is less than one, the error
      SPICE(BADARRAYSIZE) is signaled.
 
   2) If kvars has declared length less than the size (including
      terminating null character) of a name to be returned, the name
      will be truncated on the right.  The parameter MAXCHR sets
      the maximum stored size of string variables.
 
   3) If either the input or output string pointers are null, the error
      SPICE(NULLPOINTER) will be signaled.
 
   4) If the input string has length zero, the error SPICE(EMPTYSTRING)
      will be signaled.
 
   5) The caller must pass a value indicating the length of the output
      string.  If this value is not at least 2, the error
      SPICE(STRINGTOOSHORT) will be signaled.

   6) The error 'SPICE(BADVARNAME)' signals if the kernel pool
      variable name length exceeds 32.
 
-Files
 
   None.
 
-Particulars
 
   This routine provides the user interface for retrieving the names
   of kernel pool variables. This interface allows you to retrieve
   the names matching a template via multiple accesses.  Under some
   circumstances this alleviates the problem of having to know in
   advance the maximum amount of space needed to accommodate all
   matching names.
 
   However, this method of access does come with a price. It is
   always more efficient to retrieve all of the data associated with
   a kernel pool variable in one call than it is to retrieve it in
   sections.  The parameter MAXVAR defines the upper bound on the
   number of possible matching names.
 
-Examples
 
 
   The following code demonstrates how the names of kernel pool
   variables matching a template can be retrieved in pieces.
 
      #include <stdio.h>
      #include "SpiceUsr.h"
 
      int main()
      {
 
         #define  ROOM           3
         #define  LNSIZE         81
         #define  TEMPLATE       "BODY599*"
 
         SpiceBoolean            found;
 
         SpiceChar               kvars [ROOM][LNSIZE];
 
         SpiceInt                i;
         SpiceInt                n;
         SpiceInt                start;
 
         /.
         Load the data in the file "typical.ker" into the kernel pool.
         ./
         ldpool_c ( "typical.ker" );
 
         /.
         Print the names of kernel variables that match TEMPLATE.
         ./
 
         start  =  0;
 
         gnpool_c ( TEMPLATE, start, ROOM, LNSIZE, &n, kvars, &found );
 
         if ( !found )
         {
            printf ( "There are no matching variables in the "
                     "kernel pool\n"                           );
         }
         else
         {
            printf ( "Kernel pool variables:\n"
                     "\n"                       );
 
            for ( i = 0;  i < n;  i++ )
            {
               printf ( "   %s\n", kvars[i] );
            }
 
            while ( n == ROOM )
            {
               start += n;
 
               gnpool_c ( TEMPLATE,  start,  ROOM,   LNSIZE,
                          &n,        kvars,  &found          );
 
               for ( i = 0;  i < n;  i++ )
               {
                  printf ( "   %s\n", kvars[i] );
               }
            }
         }
         /.
         This is the end of the if block following the first gnpool_c
         call.
         ./
 
         return ( 0 );
      }
 
 
-Restrictions
 
   None.
 
-Literature_References
 
   None.
 
-Author_and_Institution
 
   N.J. Bachman   (JPL)
   W.L. Taber     (JPL)
 
-Version

   -CSPICE Version 1.1.1,  10-FEB-2010 (EDW)

      Added mention of the restriction on kernel pool variable 
      names to 32 characters or less.

   -CSPICE Version 1.1.0, 18-MAY-2001 (WLT)

      Added a cast to (char *) in the call to F2C_ConvertStrArr
 
   -CSPICE Version 1.0.0, 08-JUN-1999 (NJB) (WLT)
 
-Index_Entries
 
   return names of kernel pool variables matching a template
 
-&
*/
 
{ /* Begin gnpool_c */
 
 
 
   /*
   Local variables
   */
   SpiceChar             * endptr;
   SpiceChar             * strptr;
 
   SpiceInt                fstart;
   SpiceInt                i;
 
   logical                 fnd;
 
 
 
   /*
   Participate in error tracing.
   */
   chkin_c ( "gnpool_c" );
 
 
   /*
   Check the input string to make sure the pointer is non-null and
   the string is non-empty.
   */
   CHKFSTR ( CHK_STANDARD, "gnpool_c", name );
 
 
   /*
   Check the output string array to make sure the pointer is non-null
   and that each string has room for at least one character plus a null
   terminator.
   */
   CHKOSTR ( CHK_STANDARD, "gnpool_c", kvars, lenout );
 
 
   /*
   Call the f2c'd routine.  First map the start index to the Fortran
   style range of 1 : #of matching strings.
   */
 
   fstart = start + 1;
 
   gnpool_ (  ( char      * ) name,
              ( integer   * ) &fstart,
              ( integer   * ) &room,
              ( integer   * ) n,
              ( char      * ) kvars,
              ( logical   * ) &fnd,
              ( ftnlen      ) strlen(name),
              ( ftnlen      ) lenout-1      );
 
   /*
   Convert the output array from Fortran to C style.
   */
   F2C_ConvertStrArr ( *n, lenout, (char * ) kvars );
 
   /*
   Eliminate any trailing white space left by F2C_ConvertStrArr.
   */
 
   for ( i = 0;  i < *n;  i++ )
   {
      strptr   =   ( (SpiceChar *) kvars ) + i*lenout;
      endptr   =   strptr + lenout - 2;
 
      if ( *endptr == BLANK )
      {
         /*
         The last data character in this string is blank, so there is
         trailing white space to remove.  Treat the first lenout-1
         characters of the string as a Fortran string to be converted.
         The length expected by F2C_ConvertStr is the C string length,
         so we pass in lenout.
         */
         F2C_ConvertStr ( lenout, strptr );
      }
   }
 
   /*
   Set the SpiceBoolean found flag.
   */
 
   *found = fnd;
 
 
   chkout_c ( "gnpool_c" );
 
} /* End gnpool_c */
