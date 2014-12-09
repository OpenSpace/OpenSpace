/*

-Procedure bodc2n_c ( Body ID code to name translation )

-Abstract

   Translate the SPICE integer code of a body into a common name
   for that body.

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
   #undef   bodc2n_c

   void bodc2n_c ( SpiceInt        code,
                   SpiceInt        lenout,
                   SpiceChar     * name,
                   SpiceBoolean  * found   )

/*

-Brief_I/O

   Variable  I/O  Description
   --------  ---  --------------------------------------------------
   code       I   Integer ID code to be translated into a name.
   lenout     I   Maximum length of output name.
   name       O   A common name for the body identified by code.
   found      O   True if translated, otherwise false.

-Detailed_Input

   code        is an integer code for a body ---
               a planet, satellite, barycenter, spacecraft,
               asteroid, comet, or other ephemeris object.

   lenout      is the maximum allowed length of the output name,
               including the terminating null character.  For example,
               if the caller wishes to be able to accept a 32-character
               name, lenout must be set to (at least) 33.  The current
               maximum name length is 32 characters, so a value of 33
               for lenout will suffice.

-Detailed_Output

   name        is a common name of the body identified by code.
               If code has more than one translation, then the
               most recently defined name corresponding to code
               is returned.  'name' will have the exact format (case
               and blanks) as when the name/code pair was defined.

               No more than lenout characters, including the
               terminating null, will be written to name.  A terminating
               null will always be written.

   found       is SPICETRUE if code has a translation.  Otherwise, found
               is SPICEFALSE.

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

   None.

-Particulars

   bodc2n_c is one of five related subroutines,

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

   1.  Suppose you ran the utility program SPACIT to summarize
       an SPK ephemeris file and the following data was output
       to the terminal screen.

          ----------------------------------------------------------
          Segment identifier: JPL archive 21354
          Body        : -77                         Center     : 399
          From        : 1990 DEC 08 18:00:00.000
          To          : 1990 DEC 10 21:10:00.000
          Reference   : DE-200                      SPK Type    :1
          ----------------------------------------------------------

      You could write a program to translate the body codes
      shown in the SPACIT output:

         #define MAXLEN 32
                .
                .
                .
         bodc2n_c ( -77, MAXLEN, body,   found );
         bodc2n_c ( 399, MAXLEN, center, found );

         if ( found )
            {
            printf ( "body:    -77 =  %s\n", body   );
            printf ( "center:  399 =  %s\n", center );
            }

      You could also read the body and center codes directly from
      the SPK files, using the appropriate DAF routines, and then
      translate them, as above.


   2.  In this example, we assume that boddef_c has not been called,
       so only the set of default name/code pairs has
       been defined.

       Given these names, bodn2c_c will return the following codes:

          Name                         Code    Found?
          ------------------------   ------    ------
          "EARTH"                       399    Yes
          "  Earth "                    399    Yes
          "EMB"                           3    Yes
          "Solar System Barycenter"       0    Yes
          "SolarSystemBarycenter"         -    No
          "SSB"                           0    Yes
          "Voyager 2"                   -32    Yes
          "U.S.S. Enterprise"             -    No
          " "                             -    No
          "Halley's Comet"                -    No


       Given these codes, bodc2n_c will return the following names:

          Code        Name                        Found?
          -------     -------------------         ------
          399         "EARTH"                     Yes
            0         "SOLAR SYSTEM BARYCENTER"   Yes
            3         "EARTH BARYCENTER"          Yes
          -77         "GALILEO ORBITER"           Yes
           11          -                          No
           -1          -                          No

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman    (JPL)
   K.R. Gehringer  (JPL)
   B.V. Semenov    (JPL)

-Version

   -CSPICE Version 2.2.2, 24-APR-2010 (EDW) 

       Edit to Particulars section to document the bodc2s_c routine.
       Minor edit to code comments eliminating typo.

   -CSPICE Version 2.2.1, 27-FEB-2008 (BVS)

       Corrected the contents of the Required_Reading section of 
       the header.

   -CSPICE Version 2.2.0, 02-SEP-1999 (NJB)  
   
      Local type logical variable now used for found flag used in
      interface of bodc2n_.
            
   -CSPICE Version 2.1.1, 25-MAR-1998 (EDW)
     
      Minor corrections to header.

   -CSPICE Version 2.1.0, 09-FEB-1998 (NJB)

      Re-implemented routine without dynamically allocated, temporary 
      strings.  Updated the Exceptions header section.
 
   -CSPICE Version 2.0.1, 16-JAN-1998 (EDW)

       Corrected and clarified header entries.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)

      Based on SPICELIB Version 1.0.0, 23-JAN-1996 (KRG)

-Index_Entries

   body id code to name

-&
*/

{ /* Begin bodc2n_c */


   /*
   Local variables
   */
   logical                 fnd;
   
   
   /*
   Participate in error tracing.
   */
   chkin_c ( "bodc2n_c");


   /*
   Make sure the output name has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "bodc2n_c", name, lenout );


   /*
   Call the f2c'd routine.
   */
   bodc2n_( ( integer * )  &code,
            ( char    * )  name,
            ( logical * )  &fnd,
            ( ftnlen    )  lenout-1 );


   /*
   Assign the SpiceBoolean found flag.
   */
   
   *found = fnd;
   
   
   /*
   Convert the Fortran string to a C string by placing a null
   after the last non-blank character.  This operation is valid
   whether or not the CSPICE routine signaled an error.
   */
   F2C_ConvertStr ( lenout, name );


   chkout_c ( "bodc2n_c");

} /* End bodc2n_c */
