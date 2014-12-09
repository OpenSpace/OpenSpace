/*

-Procedure eknseg_c ( EK, number of segments in file )

-Abstract
 
   Return the number of segments in a specified EK. 
 
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
   UTILITY 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"


   SpiceInt eknseg_c ( SpiceInt handle )

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   handle     I   EK file handle. 
 
   The function returns the number of segments in the specified 
   E-kernel. 
 
-Detailed_Input
 
   handle         is the handle of an EK file opened for read access. 
 
-Detailed_Output
 
   The function returns the number of segments in the specified 
   E-kernel. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If handle is invalid, the error will be diagnosed by routines 
       called by this routine.  eknseg_c will return the value zero. 
 
   2)  If an I/O error occurs while trying to read the EK, the error 
       will be diagnosed by routines called by this routine. 
       eknseg_c will return the value zero. 
 
-Files
 
   See the description of handle in $Detailed_Input. 
 
-Particulars
 
   This routine is used to support the function of summarizing an 
   EK file.  Given the number of segments in the file, a program 
   can use ekssum_c in a loop to summarize each of them. 
 
-Examples
 
   1)  Open an EK file and count the segments in it. 
 
          ekopr_c ( ekname, &handle );
          n = eknseg_c  ( handle );
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 24-FEB-1999 (NJB)

-Index_Entries
 
   return number of segments in an E-kernel 
 
-&
*/

{ /* Begin eknseg_c */

   /*
   Local variables
   */
   SpiceInt                n;


   /*
   Participate in error tracing.  
   */

   chkin_c ( "eknseg_c" );
   
   /*
   We capture the value returned by eknseg_ rather than return it 
   directly, so we can check out.
   */
   
   n  =  eknseg_ ( (integer *) &handle );
   
   
   /*
   Check out here, since it's our last chance.
   */
   chkout_c ( "eknseg_c" );
   
   
   return (n);
   

} /* End eknseg_c */

