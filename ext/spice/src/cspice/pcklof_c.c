/*

-Procedure pcklof_c ( PCK Kernel, Load binary file )

-Abstract

   Load a binary PCK file for use by the readers.  Return the
   handle of the loaded file which is used by other PCK routines to
   refer to the file.

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
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void pcklof_c ( ConstSpiceChar * filename,
                   SpiceInt       * handle    )

/*

-Brief_I/O

   Variable  I/O  Description
   --------  ---  --------------------------------------------------
   filename   I   Name of the file to be loaded.
   handle     O   Loaded file's handle.

-Detailed_Input

   filename   Character name of the file to be loaded.

-Detailed_Output

   handle     Integer handle assigned to the file upon loading.
              Other PCK routine will subsequently use this number
              to refer to the file.

-Parameters

   None.

-Exceptions

   1) If an attempt is made to load more files than is specified
      by the paramater ftsize defined in pckbsr_, the error
      SPICE(PCKFILETABLEFULL) is signalled.

   2) The error SPICE(EMPTYSTRING) is signalled if the input
      string does not contain at least one character, since the
      input string cannot be converted to a Fortran-style string
      in this case.

   3) The error SPICE(NULLPOINTER) is signalled if the input string
      pointer is null.

   This routine makes use of DAF file system routines and is subject
   to all of the constraints imposed by the DAF fuile system. See
   the DAF Required Reading or individual DAF routines for details.

-Files

   A file specified by filename, to be loaded.  The file is assigned a
   handle by pcklof_c, which will be used by other routines to
   refer to it.

-Particulars

   If there is room for a new file in the file table, pcklof_c creates
   an entry for it, and opens the file for reading.

   Also, if the body table is empty, pcklof_c initializes it, this
   being as good a place as any.

-Examples

   Load a binary PCK kernel and return the integer handle.

      pck      = "/kernels/gen/pck/earth6.bpc";
      pcklof_c ( pck, &handle );

   Also see the Example in PCKLOF.FOR.

-Restrictions

   None.

-Literature_References

   DAF Required Reading

-Author_and_Institution

   K.S. Zukor         (JPL)
   E.D. Wright        (JPL)

-Version

   -CSPICE Version 2.0.1, 20-MAR-1998 (EDW)

      Minor correction to header.

   -CSPICE Version 2.0.0, 08-FEB-1998 (NJB)

      Input argument filename was changed to type ConstSpiceChar *.

      Re-implemented routine without dynamically allocated, temporary
      strings.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries

   load PCK orientation file

-&
*/

{ /* Begin pcklof_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "pcklof_c" );


   /*
   Check the input string filename to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "pcklof_c", filename );


   /*
   Call the f2c'd Fortran routine.
   */
   pcklof_ ( ( char       * )  filename,
             ( integer    * )  handle,
             ( ftnlen       )  strlen(filename)    );


   chkout_c ( "pcklof_c" );


} /* End pcklof_c */
