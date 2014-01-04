/*

-Procedure card_c ( Cardinality of a cell )

-Abstract
 
   Return the cardinality (current number of elements) in a 
   cell of any data type.
 
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
 
   CELLS 
 
-Keywords
 
   CELLS 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"

   SpiceInt card_c ( SpiceCell  * cell ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   cell       I   Input cell. 
 
   The function returns the cardinality of the input cell. 
 
-Detailed_Input
  
   cell        is a cell of character, double precision, or 
               integer data type.
 
-Detailed_Output
 
   The function returns the cardinality of (current number of elements
   in) the input cell.
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If the input cell has invalid cardinality, the error 
       SPICE(INVALIDCARDINALITY) is signaled.  card_c returns 
       an unspecified value in this case. 
 
   2)  If the input array has invalid size, the error 
       SPICE(INVALIDSIZE) is signaled.  card_c returns 
       an unspecified value in this case. 
 
-Files
 
   None. 
 
-Particulars
 
   This is a generic function which may be used on SpiceCells of
   character, double precision, or integer data type.
 
-Examples
 
   The cardinality function card_c is typically used to process 
   each of the elements of a cell. In the following example, cardc_c 
   is used to step through the individual elements of the character 
   cell names. 
 
      #include "SpiceUsr.h"
           . 
           . 
           . 
      /.
      Declare the cell names with string length LNSIZE and maximum
      number of strings SIZE.
      ./
      SPICECHAR_CELL ( names, SIZE, LNSIZE );
           . 
           . 
           . 
      for ( i = 0;  i < card_c(&names);  i++ )
      {
           . 
           . 
           . 
      } 
 
   In conjunction with the size_c function, card_c may be used 
   to predict (and subsequently avoid) overflows when manipulating 
   cells. In the following example, size_c is used to determine 
   whether the integer cell original can be safely copied into 
   the integer cell save before actually attempting the operation. 
   If original contains more elements than save can hold, then 
   the operation would fail.
 
      if (  card_c(&original)  <=  size_c(&save)  )
      {
         copy_c ( &original, &save );
      }
      else
      {
           . 
           . 
           . 
      }
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
   C.A. Curzon     (JPL) 
   H.A. Neilan     (JPL) 
   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 06-AUG-2002 (NJB) (CAC) (HAN) (WLT) (IMU)

-Index_Entries
 
   cardinality of an integer cell 
 
-&
*/

{ /* Begin card_c */


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return ( cell->card );
   }
   chkin_c ( "card_c" );


   /*
   Initialize the cell if necessary. 
   */
   CELLINIT ( cell );


   /*
   Check the size and cardinality of the input cell. 
   */
   if (  cell->size < 0  )
   {  
      setmsg_c ( "Invalid cell size.  The size was #." );
      errint_c ( "#", cell->size                       );
      sigerr_c ( "SPICE(INVALIDSIZE)"                  );
      chkout_c ( "card_c"                              );

      return   ( cell->card ); 
   }

   else if ( cell->card < 0 )
   { 
      setmsg_c ( "Invalid cell cardinality.  The "  
                 "cardinality was #."                  );
      errint_c ( "#", cell->card                       );
      sigerr_c ( "SPICE(INVALIDCARDINALITY)"           );
      chkout_c ( "card_c"                              );

      return   ( cell->card ); 
   }

   else if ( cell->card  >  cell->size )
   { 
      setmsg_c ( "Invalid cell cardinality; cardinality exceeds "
                 " cell size.  The cardinality was #.  The size "
                 " was #."                                        );
      errint_c ( "#", cell->card                                  );
      errint_c ( "#", cell->size                                  );
      sigerr_c ( "SPICE(INVALIDCARDINALITY)"                      );
      chkout_c ( "card_c"                                         );

      return   ( cell->card ); 
   }


   chkout_c ( "card_c" );

   return ( cell->card ); 


} /* End card_c */

