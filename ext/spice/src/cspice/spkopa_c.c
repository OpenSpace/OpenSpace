/*

-Procedure spkopa_c ( SPK open for addition )

-Abstract
 
  Open an existing SPK file for subsequent write. 
 
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
 
   SPK 
 
-Keywords
 
   SPK 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void spkopa_c ( ConstSpiceChar * file,
                   SpiceInt       * handle ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   file       I   The name of an existing SPK file. 
   handle     O   A handle attached to the SPK file opened to append. 
 
-Detailed_Input
 
   file       is the name of an existing SPK file to which 
              you wish to append additional SPK segments. 
 
-Detailed_Output
 
   handle     is the DAF integer handle that refers to the SPK file
              opened for appending.  
 
-Parameters
 
   None. 
 
-Files
 
   See arguments file and handle. 
 
-Exceptions
 
   1)  If the file specified does not exist the error 
       SPICE(FILENOTFOUND) will be signalled. 
 
   2)  If the file specified is not an SPK file, the error 
       SPICE(FILEISNOTSPK) will be signalled. 
 
   3)  If the string pointer file is null, the error 
       SPICE(NULLPOINTER) will be signaled.
      
   4)  If the string file has length zero, the error 
       SPICE(EMPTYSTRING) will be signaled.

   All other exceptions are determined by routines in the call 
   tree of this routine. If any exceptions arise that prevent 
   opening of the specified file for writing, HANDLE will be 
   returned with the value 0.
 
-Particulars
 
   This file provides an interface for opening existing SPK 
   files for the addition of SPK segments.  If you need 
   to open an new SPK file for writing, call the routine SPKOPN. 
 
-Examples
 
   Suppose you have collected data for a type 05 SPK segment and 
   wish to place the new segment in an existing SPK file.  The 
   code fragment below shows one set of calls that you could perform 
   to make the addition.  (Note that you could add segments of 
   other data types by replacing the call to spkw05_c with a suitably 
   modified call to another spkwXX_c routine.) 
 
   We assume that the following variables have already been 
   assigned the proper values: 
 
      body   (integer)  Body code for ephemeris object. 
      center (integer)  body code for the center of motion 
                        of the body. 
      frame  (string)   the reference frame of the states. 
      first  (d.p.)     first valid time for which states can be 
                        computed in seconds past 2000. 
      last   (d.p.)     last valid time for which states can 
                        be computed in seconds past 2000. 
      gm     (d.p.)     gravitational mass of central body. 
      n      (integer)  number of states and epochs. 
      states (d.p.)     array of states (x,y,z,dx,dy,dz). 
      epochs (d.p.)     array of epochs (seconds past 2000.) 
      segid  (string)   segment identifier 
 
 
      #include "SpiceUsr.h"
         .
         .
         .
   
      /.
      Begin by opening the file. 
      ./
      spkopa_c ( file, &handle );
 
      /.
      Now add the collected data as a new segment. 
      ./
 
      spkw05_c ( handle, body,  center, frame,  first, last, segid, 
                 gm,     n,     states, epochs                      );
 
      /.
      Finally, close the file. 
      ./
 
      spkcls_c ( handle ); 
 
-Restrictions
 
   None. 
 
-Author_and_Institution

   F.S. Turner        (JPL)

-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.0.0, 16-MAR-1999 (FST)

-Index_Entries
 
   Open an existing SPK file for adding segments 
 
-&
*/

{  /* Begin spkopa_c */

   /*
   Participate in error tracing.
   */

   chkin_c ( "spkopa_c" );

   /*
   Check the input string file to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkopa_c", file );

   /*
   Call the f2c'd Fortran routine.
   */
   spkopa_ ( ( char     * )  file, 
             ( integer  * )  handle,
             ( ftnlen     )  strlen(file) );

   chkout_c ( "spkopa_c" );

} /* End spkopa_c */
