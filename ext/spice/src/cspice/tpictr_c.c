/*

-Procedure tpictr_c ( Create a Time Format Picture )

-Abstract

   Given a sample time string, create a time format picture
   suitable for use by the routine timout_c.

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
   #include "SpiceZmc.h"
   #include "SpiceZst.h"
   

   void tpictr_c ( ConstSpiceChar * sample,
                   SpiceInt         lenout,
                   SpiceInt         lenerr,
                   SpiceChar      * pictur,
                   SpiceBoolean   * ok,
                   SpiceChar      * errmsg )
/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   sample     I   A sample time string.
   lenout     I   The length for the output picture string.
   lenerr     I   The length for the output error string.
   pictur     O   A format picture that describes sample.
   ok         O   Flag indicating whether sample parsed successfully.
   errmsg     O   Diagnostic returned if sample cannot be parsed.

-Detailed_Input


   sample     is a representative time string to use as a model to
              format time strings.

   lenout     is the allowed length for the output picture.  This length
              must large enough to hold the output string plus the null
              terminator.  If the output string is expected to have x
              characters, lenout needs to be x + 1.  80 is a reasonable
              value for lenout (79 characters plus the null
              terminator).  

   lenerr     is the allowed length for the output error string.    
   
   
-Detailed_Output


   pictur     is a format picture suitable for use with the SPICE
              routine timout_c.  This picture, when used to format an
              epoch via timout_c, will yield the same time components in
              the same order as the components in sample.

   ok         is a logical flag indicating whether the input format
              sample could be parsed. If all of the components of
              sample are recognizable, ok will be returned with the
              value SPICEFALSE.  If some part of pictur cannot be
              parsed, ok will be returned with the value SPICEFALSE.

   errmsg     is a diagnostic message that indicates what part of
              sample was not recognizable.  If sample was successfully
              parsed, ok will be SPICEFALSE and errmsg will be
              returned as an empty string.

-Parameters

   None.

-Files

   None.

-Exceptions

   Error free.

   1) All problems with the inputs are diagnosed via ok and errmsg.

   2) If a format picture can not be created from the sample
      time string, pictur is returned as a blank string.

-Particulars

   Although the routine timout_c provides CSPICE users with a great
   deal of flexibility in formatting time strings, users must
   master the means by which a time picture is constructed
   suitable for use by timout_c.

   This routine allows CSPICE users to supply a sample time string
   from which a corresponding time format picture can be created,
   freeing users from the task of mastering the intricacies of
   the routine timout_c.

   Note that timout_c can produce many time strings whose patterns
   can not be discerned by this routine.  When such outputs are
   called for, the user must consult timout_c and construct the
   appropriate format picture "by hand."  However, these exceptional
   formats are not widely used and are not generally recognizable
   to an uninitiated reader.

-Examples

   Suppose you need to print epochs corresponding to some events and
   you wish the epochs to have the same arrangement of components as in
   the string "10:23 P.M. PDT January 3, 1993".

   The following subroutine call will construct the appropriate format
   picture for use with timout_c.

   tpictr_c ( "10:23 P.M. PDT January 3, 1993",
               lenout, lenerr, pictur, &ok, errmsg );

   The resulting picture is:

      "AP:MN AMPM PDT Month DD, YYYY ::UTC-7"

   This picture can be used with timout_c to format a sequence
   of epochs, et[0],...,et[n-1] (given as ephemeris seconds past J2000)
   as shown in the loop below:

      #include "SpiceUsr.h"
          .
          .
          .
      for ( i = 0; i < n; i++ )
      {
         timout_c ( et[i], pictur, string );
         printf ( "Epoch: %d --- %s\n", i, string );
      }

-Restrictions

   None.

-Author_and_Institution
   
   W.L. Taber      (JPL)
   E.D. Wright     (JPL)

-Literature_References

   None.

-Version

   -CSPICE Version 1.0.0, 23-JUL-1999   (EDW) (WLT)

-Index_Entries

   Use a sample time string to produce a time format picture

-&
*/

{ /* Begin tpictr_c */

   /*
   Local variables
   */
   logical                 okeydoke;

   /*
   Participate in error tracing.
   */
   chkin_c ( "tpictr_c" );


   /*
   Check the input string sample to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "tpictr_c", sample );


   /*
   Make sure the output strings have at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "tpictr_c",  pictur, lenout );
   CHKOSTR ( CHK_STANDARD, "tpictr_c",  errmsg, lenerr );


   /* 
   Call the f2c'd routine. 
   */
   tpictr_( ( char    * ) sample,
            ( char    * ) pictur,
            ( logical * ) &okeydoke,
            ( char    * ) errmsg,
            ( ftnlen    ) strlen( sample ),
            ( ftnlen    ) lenout - 1,
            ( ftnlen    ) lenerr - 1       );
            
   
   /* 
   Convert the output strings to C style.
   */
   F2C_ConvertStr( lenout, pictur );
   F2C_ConvertStr( lenerr, errmsg );


   /*
   Convert the status flag from logical to SpiceBoolean.
   */
   
   *ok = okeydoke;


   chkout_c ( "tpictr_c" );


} /* End tpictr_c */


