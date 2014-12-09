/*

-Procedure expool_c ( Confirm the existence of a pool kernel variable )

-Abstract

   Confirm the existence of a kernel variable in the kernel
   pool.

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

   void expool_c ( ConstSpiceChar  * name,
                   SpiceBoolean    * found )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   name       I   Name of the variable whose value is to be returned.
   found      O   True when the variable is in the pool.

-Detailed_Input

   name       is the name of the variable whose values are to be
              returned.

-Detailed_Output

   found      is true whenever the specified variable is included
              in the pool.

-Parameters

   None.

-Exceptions

   1) If the input string pointer is null, the error SPICE(NULLPOINTER)
      will be signaled.

   2) If the input string has length zero, the error SPICE(EMPTYSTRING)
      will be signaled

-Files

   None.

-Particulars

   This routine determines whether or not a numeric kernel pool
   variable exists.  It does not detect the existence of
   string valued kernel pool variables.

   A better routine for determining the existence of kernel pool
   variables is  dtpool_ which determines the
   existence, size and type of kernel pool variables.

-Examples


   expool_c (  "BODY399_RADII", &found  );

    if ( found )
       {
        printf(  "BODY399_RADII is present in the kernel pool\n");
       }


   See bodfnd_c.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   I.M. Underwood  (JPL)
   E.D. Wright     (JPL)

-Version

   -CSPICE Version 1.2.0 22-JUN-1999   (EDW)

      Added local variable to return boolean/logical values.  This
      fix allows the routine to function if int and long are different
      sizes.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)

      Re-implemented routine without dynamically allocated, temporary
      strings.

   -CSPICE Version 1.0.0, 25-OCT-1997   (EDW)

-Index_Entries

   CONFIRM the existence of a pooled kernel variable

-&
*/

{ /* Begin expool_c */

   /*
   Local variables.
   */
   logical         yes;


   /*
   Participate in error tracing.
   */
   chkin_c ( "expool_c" );


   /*
   Check the input string name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "expool_c", name );


   /*
   Call the f2c'd routine.
   */
   expool_( ( char    * ) name,
            ( logical * ) &yes,
            ( ftnlen    ) strlen(name) );


   /* Cast back to a SpiceBoolean. */
   *found = yes;


   /* Done.  Checkout. */
   chkout_c ( "expool_c" );


} /* End expool_c */
