/*

-Procedure ekcls_c ( EK, close file )

-Abstract
 
   Close an E-kernel. 
 
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
 
   EK 
 
-Keywords
 
   EK 
   FILES 
   UTILITY 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"

   void ekcls_c ( SpiceInt handle ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   handle     I   EK file handle. 
 
-Detailed_Input
 
   handle         is the file handle of an EK to be closed.  Note 
                  that EKs open for writing must be closed by this 
                  routine in order by be valid. 
 
-Detailed_Output
 
   None. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If the indicated file is not recognized, no error is 
       signalled. 
 
   2)  If an I/O error occurs while reading or writing the indicated 
       file, the error will be diagnosed by routines called by this 
       routine. 
 
-Files
 
   See the EK Required Reading for a discussion of the EK file 
   format. 
 
-Particulars
 
   This routine should be used to close open EK files.  EK files 
   open for writing *must* be closed by this routine in order to be 
   valid.  EK files open for read access should also be closed using 
   this routine. 
 
-Examples
 
   1)  Add data to an existing EK file, then close the file. 
 
          ekopw_c ( "my.ek", &handle );
 
             [add data] 
 
          ekcls_c ( handle ); 
 
 
-Restrictions
 
   1)  No more than CSPICE_DAS_MAX_OPEN_FILES DAS files may be opened 
       simultaneously. 
       
       See SpicePar.h for the value of CSPICE_DAS_MAX_OPEN_FILES. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.1.0, 23-JUL-2001 (NJB)
     
      Removed tab characters from source file.

   -CSPICE Version 1.0.0, 31-MAR-1998 (NJB)
   
      Based on SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)

-Index_Entries
 
   close EK 
 
-&
*/

{ /* Begin ekcls_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "ekcls_c" );


   ekcls_ ( ( integer * ) &handle );


   chkout_c ( "ekcls_c" );

} /* End ekcls_c */
