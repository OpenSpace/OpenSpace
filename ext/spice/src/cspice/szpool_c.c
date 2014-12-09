/*

-Procedure szpool_c (Get size limitations of the kernel pool)

-Abstract
 
   Return the kernel pool size limitations. 
 
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
   #include "SpiceZmc.h"

   void szpool_c ( ConstSpiceChar * name,
                   SpiceInt       * n,
                   SpiceBoolean   * found ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   name       I   Name of the parameter to be returned. 
   n          O   Value of parameter specified by name. 
   found      O   SPICETRUE if name is recognized. 
 
-Detailed_Input
 
   name       is the name of a kernel pool size parameter. 
              The following parameters may be specified: 
 
              MAXVAR      is the maximum number of variables that the
                          kernel pool may contain at any one time.
                          MAXVAR should be a prime number.

              MAXLEN      is the maximum length of the variable names
                          that can be stored in the kernel pool.
 
              MAXVAL      is the maximum number of distinct values that
                          may belong to the variables in the kernel
                          pool.  Each variable must have at least one
                          value, and may have any number, so long as
                          the total number does not exceed MAXVAL.
                          MAXVAL must be at least as large as MAXVAR.
 
              MXNOTE      is the maximum number of distinct
                          variable-agents pairs that can be maintained
                          by the kernel pool.  (A variable is "paired"
                          with an agent, if that agent is to be
                          notified whenever the variable is updated.)

              MAXAGT      is the maximum number of agents that can be
                          kept on the distribution list for
                          notification of updates to kernel variables.
 
              MAXCHR      is the maximum number of characters that can
                          be stored in a component of a string valued
                          kernel variable.
 
              MAXLIN      is the maximum number of character strings
                          that can be stored as data for kernel pool
                          variables.
 
              Note that the case of name is insignificant.  Embedded
              blanks are also ignored.
 
-Detailed_Output
 
   n          is the value of the parameter specified by name.  If 
              name is not one of the items specified above, n will 
              be returned with the value 0. 
 
   found      is SPICETRUE if the parameter is recognized and
              SPICEFALSE if it is not. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the specified parameter is not recognized, the value of
      n will be set to zero and found will be set to SPICEFALSE. 
 
-Files
 
   None. 
 
-Particulars
 
   This routine provides a programmatic interface to the parameters
   used to define the capacity limits of kernel pool.  It is not 
   anticipated that most kernel pool users will need to use this 
   routine. 
 
-Examples
 

   1)  The following code fragment demonstrates how to determine the
       size of a kernel reader parameter.
       
    
       #include <stdio.h>
       #include "SpiceUsr.h"

       void main () {
 
          /.
          Local Variables
          ./
          ConstSpiceChar    * varname = "MAXLEN";

          SpiceBoolean        found;

          SpiceInt            n;


          /.
          Make the call to retrieve the value of MAXLEN
          ./
          szpool_c ( varname, &n, &found );
 
          /.
          If MAXLEN parameter was found, print it out
          ./
          if ( found ) {
             printf ( "Kernel parameter found.\n" );
             printf ( "value:\t%s = %d\n", varname, n );
          }
       }

-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman (JPL)
   W.L. Taber   (JPL) 
   H.W. Taylor  (ACT)
 
-Version
 
   -CSPICE Version 2.1.0, 02-SEP-1999 (NJB)  
   
      Local type logical variable now used for found flag used in
      interface of szpool_.
            
   -CSPICE Version 1.0.0, 23-MAR-1999 (HWT)

-Index_Entries
 
   return a kernel pool definition parameter 
 
-&
*/

{ /* Begin szpool_c */

   /*
   Local variables
   */
   logical                 fnd;
   
   
   /*
   Participate in error tracing.
   */
   chkin_c ( "szpool_c" );


   /*
   Check the input string name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "szpool_c", name );


   /*
   Call the f2c'd routine.
   */
   szpool_ ( ( char     * ) name,
             ( integer  * ) n,
             ( logical  * ) &fnd,
             ( ftnlen     ) strlen(name)  );


   /*
   Assign the SpiceBoolean found flag.
   */
   
   *found = fnd;
   
   

   chkout_c ( "szpool_c" );


} /* End szpool_c */
