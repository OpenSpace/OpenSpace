/*

-Procedure unload_c ( Unload a kernel )

-Abstract
 
   Unload a SPICE kernel. 
 
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
 
   KERNEL 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void unload_c ( ConstSpiceChar  * file ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   file       I   The name of a kernel to unload. 
 
-Detailed_Input
 
   file       is the name of a file to unload.  This file 
              should be one loaded through the interface furnsh_c. 
              If the file is not on the list of loaded kernels 
              no action is taken. 
 
              Note that if file is a meta-text kernel, all of 
              the files loaded as a result of loading the meta-text 
              kernel will be unloaded. 
 
-Detailed_Output
 
   None. 
 
-Parameters
 
   None. 
 
-Files
 
   None. 
 
-Exceptions
 
   1) If the specified kernel is not on the list of loaded kernels 
      no action is taken. 
 
   2) If the input file argument pointer is null, the error
      SPICE(NULLPOINTER) will be signaled.
      
   3) If the input file argument pointer is the empty string, the error
      SPICE(EMPTYSTRING) will be signaled.
   
-Particulars
 
   The call 
      
      unload_c ( file ); 
 
   has the effect of "erasing" the last previous call: 
 
      furnsh_c ( file ); 
 
   This interface allows you to unload binary and text kernels. 
   Moreover, if you used a meta-text kernel to set up your 
   working environment, you can unload all of the kernels loaded 
   through the meta-kernel by unloading the meta-kernel. 
 
   Unloading Text or Meta-text Kernels. 
 
   Part of the action of unloading text (or meta-text kernels) is
   clearing the kernel pool and re-loading any kernels that were not in
   the specified set of kernels to unload.  Since loading of text
   kernels is not a very fast process, unloading text kernels takes
   considerably longer than unloading binary kernels.  Moreover, since
   the kernel pool is cleared, any kernel pool variables you have set
   from your program by using one of the interfaces pcpool_c, pdpool_c,
   pipool_c, or lmpool_c will be removed from the kernel pool.  For
   this reason, if you plan to use this feature in your program,
   together with one of the routines specified above, you will need to
   take special precautions to make sure kernel pool variables required
   by your program do not inadvertently disappear.
 
-Examples
 
   Suppose that you wish to compare two different sets of kernels 
   used to describe the geometry of a mission (for example a predict 
   model and a reconstructed model). You can place all of the 
   kernels for one model in one meta-text kernel, and the other set 
   in a second meta-text kernel.  Let's call these predict.mta and 
   actual.mta. 
 
      #include "SpiceUsr.h"
           .
           .
           .
      furnsh_c ( "predct.mta" ); 
 
      /.
      Compute quantities of interest and store them 
      for comparison with results of reconstructed 
      (actual) kernels. 
     
      Now unload the predict model and load the reconstructed 
      model. 
      ./
      unload_c ( "predct.mta" ); 
      furnsh_c ( "actual.mta" ); 
 
      /.
      Re-compute quantities of interest and compare them 
      with the stored quantities. 
      ./
      
      
-Restrictions
 
   See the note regarding the unloading of text and meta-text 
   kernels. 
 
-Author_and_Institution
  
   N.J. Bachman    (JPL)
   W.L. Taber      (JPL) 
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.0.0, 01-SEP-1999 (NJB) (WLT)

-Index_Entries
 
   Unload a SPICE kernel 
 
-&
*/

{ /* Begin unload_c */



   /*
   Participate in error tracing.
   */

   chkin_c ( "unload_c" );


   /*
   Check the input filename to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "unload_c", file );


   /*
   Call the f2c'd Fortran routine.
   */
   unload_ ( ( char   * ) file, 
             ( ftnlen   ) strlen(file) );


   chkout_c ( "unload_c" );

} /* End unload_c */
