/*

-Procedure bodc2s_c ( Body ID code to string translation )

-Abstract

   Translate a body ID code to either the corresponding name or if no
   name to ID code mapping exists, the string representation of the 
   body ID value.

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

   NAIF_IDS

-Keywords

   BODY
   CONVERSION

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #undef   bodc2s_c

   void bodc2s_c ( SpiceInt        code,
                   SpiceInt        lenout,
                   SpiceChar     * name )

/*

-Brief_I/O

   Variable  I/O  Description
   --------  ---  --------------------------------------------------
   code       I   Integer ID code to translate to a string.
   lenout     I   Maximum length of output name.
   name       O   String corresponding to 'code'.
 
-Detailed_Input

   code        the integer code for a body: planet, satellite, 
               barycenter, spacecraft, asteroid, comet, or 
               other ephemeris object.

   lenout      is the maximum allowed length of the output name,
               including the terminating null character.  For example,
               if the caller wishes to be able to accept a 32-character
               name, lenout must be set to (at least) 33. The current
               maximum name length is 32 characters, so a value of 33
               for lenout will suffice.

-Detailed_Output

   name        the string name of the body identified by 'code'
               if a mapping between 'code' and a body name exists
               within SPICE.

               If 'code' has more than one translation, then the
               most recently defined 'name' corresponding to 'code'
               is returned. 'name' will have the exact format (case
               and blanks) as when the name/code pair was defined.

               If the input value of 'code' does not map to a body
               name, 'name' returns with the string representation
               of 'code'.

-Parameters

   None.

-Exceptions

   1) If the output string pointer is null, the error SPICE(NULLPOINTER)
      is signaled.
   
   2) If the output string has length less than two characters, it 
      is too short to contain one character of output data plus a null  
      terminator, so it cannot be passed to the underlying Fortran 
      routine.  In this event, the error SPICE(STRINGTOOSHORT) is
      signaled.
 
-Files

   Body-name mappings may be defined at run time by loading text
   kernels containing kernel variable assignments of the form

      NAIF_BODY_NAME += ( <name 1>, ... )
      NAIF_BODY_CODE += ( <code 1>, ... )

   See NAIF_ID.REQ for details.

-Particulars

   bodc2s_c is one of five related subroutines,

      bods2c_c      Body string to code
      bodc2s_c      Body code to string
      bodn2c_c      Body name to code
      bodc2n_c      Body code to name
      boddef_c      Body name/code definition

   bods2c_c, bodc2s_c, bodn2c_c, and bodc2n_c perform translations between 
   body names and their corresponding integer ID codes which are 
   used in SPICE files and routines.

   bods2c_c is a slightly more general version of bodn2c_c: support
   for strings containing ID codes in string format enables a caller
   to identify a body using a string, even when no name is
   associated with that body.

   bodc2s_c is a general version of bodc2n_c; the routine returns either
   the name assigned in the body ID to name mapping or a string
   representation of the CODE value if no mapping exists.

   boddef_c assigns a body name to ID mapping. The mapping has priority 
   in name-to-ID and ID-to-name translations.

   Refer to NAIF_ID.REQ for the list of name/code associations built into
   SPICE, and for details concerning adding new name/code
   associations at run time by loading text kernels.

-Examples

   Apply the BODC2S call to several IDs representing codes
   included in the default SPICE ID-name lists and codes not
   included in the list.

   #include <stdio.h>
   #include "SpiceUsr.h"
   #define  LEN               32

   int main()
      {

      /.
      Assign an array of body ID codes. Not all the listed codes
      map to a body name.
      ./

      SpiceInt                code[] = { 399, 0, 3, -77, 
                                         11, -1, 6000001 };
 
      SpiceInt                lenout = LEN;
      SpiceChar               name [LEN];
      SpiceInt                i;

      /.
      Loop over the 'code' array, call bodc2s_c for each
      element of 'code'.
      ./

      for (i=0; i<7; i++ )
         {
         (void) bodc2s_c ( code[i], lenout, name );
         printf("%ld   %s\n", code[i], name);
         }

      return ( 0 );
      }

    Given these codes, bodc2s_c returns the following 'name' strings:

          Code        Name
          -------     -------------------
              399     'EARTH'
                0     'SOLAR SYSTEM BARYCENTER'
                3     'EARTH BARYCENTER'
              -77     'GALILEO ORBITER'
               11     '11'
               -1     'GEOTAIL'
          6000001     '6000001'

   The codes 11 and 6000001 did not map to a name so the call
   returns as 'name' the string expression of the codes.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

  E.D. Wright     (JPL)

-Version

    -CSPICE Version 1.0.0, 24-APR-2010 (EDW)

-Index_Entries

   body id code to string

-&
*/

{ /* Begin bodc2s_c */


   /*
   Local variables
   */   

   /*
   Participate in error tracing.
   */
   chkin_c ( "bodc2s_c");


   /*
   Make sure the output name has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "bodc2s_c", name, lenout );


   /*
   Call the f2c'd routine.
   */
   (void) bodc2s_( ( integer * )  &code,
                   ( char    * )  name,
                   ( ftnlen    )  lenout-1 );
   
   /*
   Convert the Fortran string to a C string by placing a null
   after the last non-blank character.  This operation is valid
   whether or not the CSPICE routine signaled an error.
   */
   F2C_ConvertStr ( lenout, name );

   chkout_c ( "bodc2s_c");

} /* End bodc2s_c */
