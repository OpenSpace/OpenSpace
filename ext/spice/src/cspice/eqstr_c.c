/*

-Procedure eqstr_c ( Equivalent strings )

-Abstract

   Determine whether two strings are equivalent.

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

   ALPHANUMERIC
   ASCII
   CHARACTER
   COMPARE
   PARSING
   SEARCH
   STRING
   TEXT

*/
   #include <ctype.h>
   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   
   
   SpiceBoolean eqstr_c ( ConstSpiceChar * a,  ConstSpiceChar * b )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   a,
   b          I   Arbitrary character strings.

   The function returns SPICETRUE if A and B are equivalent.

-Detailed_Input

   a,
   b           are arbitrary character strings.

-Detailed_Output

   The function returns TRUE if A and B are equivalent: that is,
   if A and B contain  the same characters in the same order,
   when white space characters are ignored and uppercase and lowercase
   characters are considered equal.

   White space characters are those in the set

      { ' ', '\f', '\n', '\r', '\t', '\v' }

   Note that this specification differs from that of the Fortran version
   of this routine, which considers the blank ( ' ' ) to be the only
   white space character.

-Parameters

   None.

-Files

   None.

-Exceptions

    1) If either input string pointer is null, the error 
       SPICE(NULLPOINTER) will be signaled.


-Particulars

   This routine is provided for those cases in which two strings
   must be compared, and in which allowances are to be made for
   extra (leading, trailing, and embedded) blanks and differences
   in case. For the most part,

      if ( eqstr_c ( A, B ) )
         .
         .

   is true whenever

      cmprss_c ( ' ', 0, a,        MAXLEN, tempa );
      ucase_c  (            tempa, MAXLEN, tempa );

      cmprss_c ( ' ', 0, b,        MAXLEN, tempb );
      ucase_c  (            tempb, MAXLEN, tempb );


      if ( !strcmp ( tempa, tempb ) )
         .
         .

   is true. There are two important differences, however.

      1) The single reference to eqstr_c is much simpler to
         write, and simpler to understand.

      2) The reference to eqstr_c does not require any temporary
         storage, nor does it require that the strings a and b
         be changed. This feature is especially useful when
         comparing strings recieved as subprogram arguments
         against strings stored internally within the subprogram.


-Examples


    Usage
    --------------------------------------------

       All of the following are TRUE.

          eqstr_c ( "A short string   ",
                    "ashortstring"        );

          eqstr_c ( "Embedded        blanks",
                    "Em be dd ed bl an ks"    );

          eqstr_c ( "Embedded        blanks",
                    "   Embeddedblanks"    );

          eqstr_c ( " ",
                    "          " );


       All of the following are FALSE.

          eqstr_c ( "One word left out",
                    "WORD LEFT OUT"      );

          eqstr_c ( "Extra [] delimiters",
                    "extradelimiters"      );

          eqstr_c ( "Testing 1, 2, 3",
                    "TESTING123"       );


    Use
    --------------------------------------------

       The following illustrates a typical use for eqstr_c.

          #include "SpiceUsr.h"
              .
              .
              .
          SpiceChar * greeting ( SpiceChar *who )
          {

             if ( eqstr_c ( who, "Steve" ) )
             {
                return ( "Yes, sir?" );
             }
             else if ( eqstr_c ( who, "Chuck" ) )
             {
                return ( "What can I do for you?" );
             }
             else
             {
                return ( "Whaddya want?" );
             }
          }

       Note that all of the following calls will elicit the
       greeting "Yes, sir?":

          greeting ( "STEVE" );
          greeting ( "steve" );
          greeting ( "Steve" );
          greeting ( "sTEVE" );
          greeting ( " S T E V E " );

-Restrictions

   None.

-Literature_References

   1)  "American National Standard for Programming Languages -- C,"
        Published by the American National Standards Institute, 1990.
        Section 7.3.1.9., p. 104.

-Author_and_Institution

   N.J. Bachman    (JPL)
   I.M. Underwood  (JPL)

-Version

   -CSPICE Version 1.3.0, 27-AUG-1999   (NJB)

      Added check for null input strings.  Added logic to handle the
      case where at least one input string is empty.
      
   -CSPICE Version 1.2.0, 24-FEB-1999 (NJB)

       Arguments passed to isspace are now cast to unsigned char to 
       suppress compilation warnings on some systems.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)

       Initial assignment of return value added to suppress compilation
       warnings on some systems.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)

       Based on SPICELIB Version 1.2.0, 03-AUG-1994 (NJB)

-Index_Entries

   equivalent strings

-&
*/

{ /* Begin eqstr_c */

   /*
   Local constants
   */
   #define  LBOUND     (   (SpiceInt) 'a'            )
   #define  UBOUND     (   (SpiceInt) 'z'            )
   #define  DELTA      ( ( (SpiceInt) 'A' ) - LBOUND )


   /*
   Local variables
   */
   SpiceBoolean            done;
   SpiceBoolean            retval;

   ConstSpiceChar         * pa;
   ConstSpiceChar         * pb;

   SpiceInt                ca;
   SpiceInt                cb;

   SpiceInt                lenA;
   SpiceInt                lenB;


   /*
   Initialize the return value retval in order to make certain
   compilers happy.  This initial value is not used later; retval
   is set explicitly in each case below.
   */
   retval = SPICEFALSE;


   /*
   Check the input string pointers to make sure they're non-null.
   */
   CHKPTR_VAL ( CHK_DISCOVER, "eqstr_c", a, retval );
   CHKPTR_VAL ( CHK_DISCOVER, "eqstr_c", b, retval );
   

   /*
   The general plan is to move a pair of pointers (PA, PB)
   through strings A and B, skipping blank characters and
   comparing others one-for-one.

      Repeat:

         If (A is blank) then
            Increment A

         Else if (B is blank) then
            Increment B

         Else
            If (A and B are equivalent) then
               Increment A and B
            Else
               Return FALSE

         If (A and B are past end) then
            Return TRUE

         Else if (A or B is past end and other is non-blank) then
            Return FALSE

         Else if (A or B is past end and other is blank) then
            Return TRUE

   Note that no pointer gets incremented more than once on each
   pass through the loop.

   On the other hand, in many cases the strings will be exactly
   equal. If so, why knock ourselves out?
   */

   if ( !strcmp( a, b ) )
   {
      return ( SPICETRUE );
   }

   pa    =  a;
   pb    =  b;
   lenA  =  strlen(a);
   lenB  =  strlen(b);


   /*
   The possibility of an input string being empty does not occur in
   Fortran, but it does here.  Handle these cases (the case where both
   are empty was handled by the strcmp test above).
   */
   
   if (  ( lenA == 0 ) && ( lenB > 0 )  )
   {
      return ( SPICEFALSE );
   } 
   
   if (  ( lenB == 0 ) && ( lenA > 0 )  )
   {
      return ( SPICEFALSE );
   } 
   

   /*
   On with the normal path.
   */

   done  =  SPICEFALSE;


   while ( !done )
   {

      /*
      At this point, we're guaranteed that strings a and b have more
      characters to examine, that is:

        ( pa <= a+lenA-1 )   and   ( pb <= b+lenB-1 )

      */


      if (  isspace( (unsigned char) *pa )  )
      {
         pa++;
      }
      else if (  isspace( (unsigned char) *pb)  )
      {
         pb++;
      }
      else
      {

         ca = (SpiceInt)(*pa);
         cb = (SpiceInt)(*pb);

         if ( ( ca >= LBOUND ) && ( ca <= UBOUND ) )
         {
            ca = ca + DELTA;
         }

         if ( ( cb >= LBOUND ) && ( cb <= UBOUND ) )
         {
            cb = cb + DELTA;
         }

         if ( ca == cb )
         {
            pa++;
            pb++;
         }
         else
         {
            /*
            We now know the strings don't match.
            */
            retval = SPICEFALSE;
            done   = SPICETRUE;
         }
      }

      if ( !done )
      {
         /*
         At this point, the strings still match and we've advanced
         at least one of the pointers.
         */


         if (  ( (SpiceInt)(pa-a) )  ==  lenA  )
         {
            /*
            There are no more characters in string a to examine.  The
            rest of string b had better be white space, or else we had
            better be at the end of string b.
            */

            if ( ( (SpiceInt)(pb-b) )  ==  lenB  )
            {
               /*
               We've seen all of string b.
               */

               retval = SPICETRUE;
               done   = SPICETRUE;
            }
            else if ( iswhsp_c(pb) )
            {
               retval = SPICETRUE;
               done   = SPICETRUE;
            }
            else
            {
               retval = SPICEFALSE;
               done   = SPICETRUE;
            }
         }
         /*
         End of "no more characters in string a" case.
         */

         else if (  ( (SpiceInt)(pb-b) )  ==  lenB  )
         {
            /*
            There are no more characters in string b to examine.  The
            rest of string a had better be white space.
            */
            if ( iswhsp_c(pa) )
            {
               retval = SPICETRUE;
               done   = SPICETRUE;
            }
            else
            {
               retval = SPICEFALSE;
               done   = SPICETRUE;
            }
         }

         /*
         End of "no more characters in string b" case.
         */
      }
      /*
      At this point, we've handled the cases where at least one
      string is out of characters.  If such a case occurred, done
      has been set to SPICETRUE.
      */

   }
   /*
   End of while loop. retval has been set.
   */

   return (retval);


} /* End eqstr_c */
