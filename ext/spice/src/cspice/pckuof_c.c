/*

-Procedure pckuof_c ( PCK Kernel, Unload binary file )

-Abstract

   Unload a binary PCK file so that it will no longer be searched by
   the readers.

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

   PCK

-Keywords

   FILES

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void pckuof_c ( SpiceInt handle )

/*

-Brief_I/O

   Variable  I/O  Description
   --------  ---  --------------------------------------------------
   handle     I   Handle of PCK file to be unloaded

-Detailed_Input

   handle     Integer handle assigned to the PCK file when it was
              loaded.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   None.

-Files

   The file referred to by handle is unloaded.

-Particulars

   A PCK file is removed from consideration by the readers during a
   search by a call to pckuof_c.

   The file table entry corresponding to the file referenced by
   handle, is removed and the file is closed.  Any segment table
   entry which came from the specified file is also deleted.

   If the file specified by handle does not appear in the file table,
   nothing happens.

-Examples

   Unload a binary PCK kernel specified by an integer handle, making
   room to load another PCK.

      pck      = "/kernels/gen/pck/earth6.bpc";
      pcklof_c ( pck, &handle );
         .
         .
         .
      pckuof_c ( handle );


   Also see the Example in pckbsr.c or pckbsr.for.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman    (JPL)
   J.M. Lynch      (JPL)
   R.E. Thurman    (JPL)
   K.S. Zukor      (JPL)

-Version

   -CSPICE Version 1.0.0, 08-FEB-1998 (NJB)

      Based on SPICELIB Version 1.0.0, 16-MAR-1994 (KSZ)

-Index_Entries

   unload PCK orientation file

-&
*/

{ /* Begin pckuof_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "pckuof_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   pckuof_ ( ( integer * ) &handle );


   chkout_c ( "pckuof_c" );


} /* End pckuof_c */
