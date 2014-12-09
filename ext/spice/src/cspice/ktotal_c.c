/*

-Procedure ktotal_c ( Kernel Totals )

-Abstract
 
   Return the current number of kernels that have been loaded 
   via the KEEPER interface that are of a specified type. 
 
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


   void ktotal_c ( ConstSpiceChar   * kind,
                   SpiceInt         * count ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   kind       I   A list of kinds of kernels to count.
   count      O   The number of kernels of type kind.
 
-Detailed_Input
 
   kind       is a list of types of kernels to count when computing
              loaded kernels.  kind should consist of a list of words
              of kernels to examine.  Recognized types are
 
                 SPK  --- All SPK files are counted in the total.
                 CK   --- All CK files are counted in the total. 
                 PCK  --- All binary PCK files are counted in the 
                          total. 
                 EK   --- All EK files are counted in the total. 
                 TEXT --- All text kernels that are not meta-text. 
                          kernels are included in the total. 
                 META --- All meta-text kernels are counted in the 
                          total. 
                 ALL  --- Every type of kernel is counted in the 
                          total. 
 
               kind is case insensitive.  If a word appears in kind 
               that is not one of those listed above, it is ignored. 
 
               See the Examples section for illustrations of the 
               use of kind. 
 
-Detailed_Output
 
   count       is the number of kernels loaded through furnsh_c that 
               belong to the list specified by kind. 
 
-Parameters
 
   None. 
 
-Files
 
   None. 
 
-Exceptions

   1) If a word on the list specified by kind is not recognized 
      it is ignored. 
 
   2) If kind is blank, or none of the words in kind is on the 
      list specified above, count will be returned as zero. 
 
   3) If the input file kind argument pointer is null, the error
      SPICE(NULLPOINTER) will be signaled.
      
   4) If the input file kind argument pointer is the empty string, the 
      error SPICE(EMPTYSTRING) will be signaled.
   
-Particulars
 
   ktotal_c allows you to easily determine the number of kernels 
   loaded via the interface furnsh_c that are of a type of interest. 
 
-Examples
 
   Suppose you wish to determine the number of SPK kernels that 
   have been loaded via the interface furnsh_c.  Assign kind 
   the value "SPK" and call ktotal_c as shown: 
 
      #include "SpiceUsr.h"
           .
           .
           .
      ktotal_c ( "spk", &count ); 
 
      printf ( "The number of loaded SPK files is:  %d\n", count );
       
 
   To determine the number of text kernels that are loaded that 
   are not meta-kernels: 
 
      ktotal_c ( "TEXT", &ntext ); 
 
      printf ( "The number of non-meta-text kernels loaded is: %d\n",
                ntext                                                ); 
 
   To determine the number of SPK, CK and PCK kernels loaded, make the
   following call:
 
      ktotal_c ( "SPK PCK CK", &count );
 
 
   To get a count of all loaded kernels: 
 
      ktotal_c ( "ALL", &count );
 
      printf ( "There are %d SPICE kernels loaded.\n", count ); 
 
 
-Restrictions
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   W.L. Taber      (JPL) 
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.0.0, 01-SEP-1999 (NJB) (WLT)

-Index_Entries
 
   Number of loaded kernels of a given type 
 
-&
*/

{ /* Begin ktotal_c */


   /*
   Use discovery check-in.

   Check the input file kind to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_DISCOVER, "ktotal_c", kind );
   
   
   ktotal_ (  ( char    * ) kind,
              ( integer * ) count,
              ( ftnlen    ) strlen(kind)  );


} /* End ktotal_c */

