/*

-Procedure clpool_c ( Clear the pool of kernel variables )

-Abstract
 
   Remove all variables from the kernel pool. Watches
   on kernel variables are retained.

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

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   

   void clpool_c ( void ) 

/*

-Brief_I/O
 
   None. 
 
-Detailed_Input
 
   None. 
 
-Detailed_Output
 
   None. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) All known agents (those established through swpool_) will 
      be "notified" that their watched variables have been updated 
      whenever clpool_c is called. 
 
-Files
 
   None. 
 
-Particulars
 
   clpool_c clears the pool of kernel variables maintained by 
   the subroutine pool_. All the variables in the pool are deleted. 
   However, all watcher information is retained. 
 
   Each watched variable will be regarded as having been updated. 
   Any agent associated with that variable will have a notice 
   posted for it indicating that its watched variable has been 
   updated. 

   Application programs can delete watches by calling the function
   dwpool_, which is located in the source file pool.c. 
   See the documentation of dwpool_ for details.
 
-Examples
 
   The following code fragment demonstrates how to clear to kernel pool
   to make room for new kernel data.  In this example, three kernels
   are successively loaded and used, after which the kernel pool is
   cleared.  The kernels may collectively contain too much data for the
   kernel pool to hold, but clearing the kernel pool after each use
   makes it possible to use all three.
    
 
      #include "SpiceUsr.h"
            .
            .
            .
      /.
      Store in an array the names of the kernel files whose 
      values will be loaded into the kernel pool.  These are
      SCLK kernels for three different spacecraft clocks.
      ./
      
      ConstSpiceChar        * kerptr [3] = { "vg1_sclk.ker",
                                             "vg2_sclk.ker",
                                             "gll_sclk.ker" };
 
      /.
      Use each kernel in turn.  
      /.
      
      for ( i = 0;  i < 3;  i++ )
      {
         ldpool_c ( kerptr[i] );
         
         [ use SCLK data for ith spacecraft ]
         
         /.
         Clear the kernel pool, making room for new data.
         ./
         
         clpool_c ();
      }
 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   I.M. Underwood  (JPL) 
   W.L. Taber      (JPL) 
 
-Version
  
   -CSPICE Version 1.0.1, 01-JUL-2014 (NJB)

      Updated comments regarding behavior of the watcher
      subsystem.

   -CSPICE Version 1.0.0, 18-JUN-1999 (IMU) (WLT) (NJB)

-Index_Entries
 
   CLEAR the pool of kernel variables 
 
-&
*/

{ /* Begin clpool_c */



   /*
   Participate in error tracing.
   */

   chkin_c ( "clpool_c" );

   /*
   Just call the f2c'd routine.
   */

   clpool_();


   chkout_c ( "clpool_c" );

} /* End clpool_c */


