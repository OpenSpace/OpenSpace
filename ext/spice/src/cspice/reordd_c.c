/*

-Procedure reordd_c ( Reorder a double precision array )

-Abstract
 
   Re-order the elements of a double precision array according to 
   a given order vector. 
 
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
   #include "SpiceZim.h"
   #undef    reordd_c

   void reordd_c ( ConstSpiceInt      * iorder,
                   SpiceInt             ndim,
                   SpiceDouble        * array  ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   iorder     I   Order vector to be used to re-order array. 
   ndim       I   Dimension of array. 
   array     I/O  Array to be re-ordered. 

-Detailed_Input
 
   iorder      is the order vector to be used to re-order the input 
               array. The first element of iorder is the index of 
               the first item of the re-ordered array, and so on. 

               Note that the order imposed by reordd_c is not the 
               same order that would be imposed by a sorting 
               routine. In general, the order vector will have 
               been created (by one of the order routines) for 
               a related array, as illustrated in the example below. 

   ndim        is the number of elements in the input array. 

   array       on input, is an array containing some number of 
               elements in unspecified order. 
 
-Detailed_Output
 
   array       on output, is the same array, with the elements 
               in re-ordered as specified by iorder. 

-Parameters
 
   None. 
 
-Exceptions
 
   1) If memory cannot be allocated to create a Fortran-style version of
      the input order vector, the error SPICE(MALLOCFAILED) is signaled.
 
   2) If ndim < 2, this routine executes a no-op.  This case is 
      not an error.

-Files
 
   None. 
 
-Particulars
 
   reordd_c uses a cyclical algorithm to re-order the elements of 
   the array in place. After re-ordering, element iorder[0] of 
   the input array is the first element of the output array, 
   element iorder[1] is the input array is the second element of 
   the output array, and so on. 

   The order vector used by reordd_c is typically created for 
   a related array by one of the order routines, as shown in 
   the example below. 
 
-Examples
 
   In the following example, the ORDER and REORD routines are 
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
 
   -CSPICE Version 1.0.0, 11-JAN-2003 (EDW)

      Trivial. Corrected 'Detailed_output' section
      header to 'Detailed_Output'.

   -CSPICE Version 1.0.0, 10-JUL-2002 (NJB)

-Index_Entries
 
   reorder a d.p. array 
 
-&
*/

{ /* Begin reordd_c */

 
   /*
   Local variables 
   */
   SpiceInt                i ;
   SpiceInt              * ordvec;
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
   Get a local copy of the input order vector; map the vector's contents
   to the range 1:ndim. 
   */
   vSize  = ndim * sizeof(SpiceInt);

   ordvec = (SpiceInt *) malloc( vSize );

   if ( ordvec == 0 )
   {
      chkin_c  ( "reordd_c"                                ); 
      setmsg_c ( "Failure on malloc call to create array "
                 "for Fortran-style order vector.  Tried "
                 "to allocate # bytes."                    );
      errint_c ( "#",  vSize                               );
      sigerr_c ( "SPICE(MALLOCFAILED)"                     );
      chkout_c ( "reordd_c"                                );
      return;
   }

   for ( i = 0;  i < ndim;  i++ )
   {
      ordvec[i] = iorder[i] + 1;
   }


   reordd_ ( ( integer    * ) ordvec,
             ( integer    * ) &ndim,
             ( doublereal * ) array );


   free ( ordvec );

} /* End reordd_c */
