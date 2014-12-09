/*

-Procedure vhatg_c ( "V-Hat", unit vector along V, general dimension )

-Abstract
 
   Find the unit vector along a double precision vector of 
   arbitrary dimension. 
 
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
 
   VECTOR 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #undef    vhatg_c
   
   
   void vhatg_c ( ConstSpiceDouble   * v1,
                  SpiceInt             ndim,
                  SpiceDouble        * vout ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   v1         I   Vector to be normalized. 
   ndim       I   Dimension of v1 (and also vout). 
   vout       O   Unit vector v1 / |v1|. 
                  If v1 = 0, vout will also be zero. 
                  vout can overwrite v1. 

-Detailed_Input
 
    v1            This is any double precision vector of arbitrary
                  dimension.  This routine will detect if is V1 the
                  zero vector, and will not attempt to divide by zero.
 
    ndim          is the dimension of V1 (and also VOUT). 
 
-Detailed_Output
 
    vout          contains the unit vector in the direction of v1. If
                  v1 represents the zero vector, then vout will also be
                  the zero vector.  vout may overwrite v1.
 
-Parameters
 
   None. 
 
-Particulars
 
   vhatg_c determines the magnitude of V1 and then divides each 
   component of V1 by the magnitude.  This process is highly stable 
   over the whole range of multi-dimensional vectors. 
 
-Examples
 
   The following table shows how selected v1 maps to vout. 

   v1                    ndim            vout 
   ----------------------------------------------------------------- 
   (5, 12, 0, 0)           4            (5/13, 12/13, 0, 0) 
   (1e-7, 2D-e, 2e-7)      3            (1/3, 2/3, 2/3) 
 
-Restrictions
 
   The relative number of cases whereby floating point overflow may 
   occur is negligible. Thus, no error recovery or reporting scheme 
   is incorporated into this subroutine. 
 
-Exceptions
 
   Error free. 
 
-Files
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   W.M. Owen       (JPL) 
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.0.0, 13-JUL-1999 (NJB) (WMO)

-Index_Entries
 
   unitize a n-dimensional vector 
 
-&
*/

{ /* Begin vhatg_c */


   /*
   Local variables
   */
   SpiceDouble             vmag;
   SpiceInt                i;



   /*
   Obtain the magnitude of v1.
   */
   vmag = vnormg_c ( v1, ndim );
   
   /*
   If vmag is nonzero, then normalize.  Note that this process is
   numerically stable: overflow could only happen if vmag were small,
   but this could only happen if each component of v1 were small.
   In fact, the magnitude of any vector is never less than the
   magnitude of any component.
   */
 
   if ( vmag > 0.0 )
   {
      for (  i = 0;  i < ndim;  i++  )
      {
         vout[i] = v1[i] / vmag;
      }  
   }
   else
   {
      for (  i = 0;  i < ndim;  i++  )
      {
         vout[i] = 0.;
      }
   }

} /* End vhatg_c */

