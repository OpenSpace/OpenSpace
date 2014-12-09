/*

-Procedure reordc_c ( Reorder a character array )

-Abstract
 
   Re-order the elements of an array of character strings 
   according to a given order vector. 
 
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
 
   ARRAY,  SORT 
 
*/

   #include <stdlib.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #include "SpiceZim.h"
   #undef    reordc_c


   void reordc_c ( ConstSpiceInt  * iorder,
                   SpiceInt         ndim,
                   SpiceInt         lenvals,
                   void           * array    ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   iorder     I   Order vector to be used to re-order array. 
   ndim       I   Dimension of array. 
   lenvals    I   String length.
   array     I/O  Array to be re-ordered. 
 
-Detailed_Input
 
   iorder      is the order vector to be used to re-order the input 
               array. The first element of iorder is the index of 
               the first item of the re-ordered array, and so on. 

               Note that the order imposed by reordc_c is not the 
               same order that would be imposed by a sorting 
               routine. In general, the order vector will have 
               been created (by one of the order routines) for 
               a related array, as illustrated in the example below. 

               The elements of iorder range from zero to ndim-1.

   ndim        is the number of elements in the input array. 

   lenvals     is the declared length of the strings in the input
               string array, including null terminators.  The input   
               array should be declared with dimension 

                  [ndim][lenvals]

   array       on input, is an array containing some number of 
               elements in unspecified order. 
 
-Detailed_Output
 
   array       on output, is the same array, with the elements 
               in re-ordered as specified by iorder. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the input string array pointer is null, the error
      SPICE(NULLPOINTER) will be signaled.
 
   2) If the input array string's length is less than 2, the error
      SPICE(STRINGTOOSHORT) will be signaled.

   3) If memory cannot be allocated to create a Fortran-style version of
      the input order vector, the error SPICE(MALLOCFAILED) is signaled.
 
   4) If ndim < 2, this routine executes a no-op.  This case is 
      not an error.

-Files
 
   None. 
 
-Particulars
 
   reordc_c uses a cyclical algorithm to re-order the elements of 
   the array in place. After re-ordering, element iorder[0] of 
   the input array is the first element of the output array, 
   element iorder[1] of the input array is the second element of 
   the output array, and so on. 

   The order vector used by reordc_c is typically created for 
   a related array by one of the order*_c routines, as shown in 
   the example below. 
 
-Examples
 
   In the following example, the order*_c and reord*_c routines are 
   used to sort four related arrays (containing the names, 
   masses, integer ID codes, and visual magnitudes for a group 
   of satellites). This is representative of the typical use of 
   these routines. 

      #include "SpiceUsr.h"
           .
           .
           .
      /.
      Sort the object arrays by name. 
      ./ 

      orderc_c ( namlen, names, n,  iorder ); 

      reordc_c ( iorder, n, namlen, names  );
      reordd_c ( iorder, n,         masses ); 
      reordi_c ( iorder, n,         codes  ); 
      reordd_c ( iorder, n,         vmags  );

-Restrictions
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.0.0, 10-JUL-2002 (NJB) (WLT) (IMU)

-Index_Entries
 
   reorder a character array 
 
-&
*/

{ /* Begin reordc_c */

   /*
   Local variables
   */
   SpiceChar             * fCvalsArr;
   
   SpiceInt                fCvalsLen;
   SpiceInt                i;
   SpiceInt              * ordvec ;
   SpiceInt                vSize;



   /*
   If the input array doesn't have at least two elements, return
   immediately. 
   */
   if ( ndim < 2 )
   {
      return;
   }

   /*
   Use discovery check-in.


   Make sure the input pointer for the string array is non-null 
   and that the length lenvals is sufficient.  
   */
   CHKOSTR ( CHK_DISCOVER, "reordc_c", array, lenvals );
   

   /*
   Create a Fortran-style string array.
   */
   C2F_MapStrArr ( "reordc_c", 
                   ndim, lenvals, array, &fCvalsLen, &fCvalsArr );

   if ( failed_c() )
   {
      return;
   }


   /*
   Get a local copy of the input order vector; map the vector's contents
   to the range 1:ndim. 
   */
   vSize  = ndim * sizeof(SpiceInt);

   ordvec = (SpiceInt *) malloc( vSize );

   if ( ordvec == 0 )
   {
      free ( fCvalsArr );

      chkin_c  ( "reordc_c"                                ); 
      setmsg_c ( "Failure on malloc call to create array "
                 "for Fortran-style order vector.  Tried "
                 "to allocate # bytes."                    );
      errint_c ( "#",  vSize                               );
      sigerr_c ( "SPICE(MALLOCFAILED)"                     );
      chkout_c ( "reordc_c"                                );
      return;
   }

   for ( i = 0;  i < ndim;  i++ )
   {
      ordvec[i] = iorder[i] + 1;
   }


   /*
   Call the f2c'd routine.
   */
   reordc_ (  ( integer    * ) ordvec,
              ( integer    * ) &ndim,
              ( char       * ) fCvalsArr,     
              ( ftnlen       ) fCvalsLen  );

   /*
   Free the dynamically allocated arrays.
   */
   free ( fCvalsArr );
   free ( ordvec    );


} /* End reordc_c */
