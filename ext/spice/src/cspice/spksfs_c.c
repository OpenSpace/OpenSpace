/*

-Procedure spksfs_c ( S/P Kernel, Select file and segment )

-Abstract
 
   Search through loaded SPK files to find the highest-priority segment
   applicable to the body and time specified.

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
 
   EPHEMERIS 
   FILES 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"
   #undef spksfs_c


   void spksfs_c ( SpiceInt        body,
                   SpiceDouble     et,
                   SpiceInt        idlen,
                   SpiceInt      * handle,
                   SpiceDouble     descr [5],
                   SpiceChar     * ident,
                   SpiceBoolean  * found     ) 
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   body       I   Body ID. 
   et         I   Ephemeris time. 
   idlen      I   Length of output segment ID string.
   handle     O   Handle of file containing the applicable segment. 
   descr      O   Descriptor of the applicable segment. 
   ident      O   Identifier of the applicable segment. 
   found      O   Indicates whether or not a segment was found. 
   SIDLEN     P   Maximum length of segment ID.

-Detailed_Input
 
   body       is the NAIF integer code of an ephemeris object, 
              typically a solar system body. 
 
   et         is a time, in seconds past the epoch J2000 TDB. 
 
   idlen      is the allowed length of the output string.  This length
              must large enough to hold the output segment ID plus the
              null terminator. SPK segment identifiers may contain up
              to SIDLEN characters, excluding the null terminator.

-Detailed_Output
 
   handle     is the handle of the SPK file containing a located
              segment.
 
   descr      is the descriptor of a located SPK segment. `descr'
              has length 5.
 
   ident      is the SPK segment identifier of a located SPK segment.
 
   found      indicates whether a requested segment was found or not.
              The other output arguments are valid only if `found'
              is set to SPICETRUE.
 
-Parameters
 
   SIDLEN     is the maximum number of characters in an SPK segment
              identifier, excluding the null terminator.

              SIDLEN is set to 40.
 
-Exceptions
 
   1) If an attempt is made to call spksfs_c when there aren't any 
      SPK files loaded, the error SPICE(NOLOADEDFILES) is signaled.

   2) With the exception of Fortran READ errors, any errors
      that occur while this routine attempts to extract segment
      descriptors from loaded SPK files will be diagnosed and signaled
      by routines in the call tree of this routine.

      Note however that I/O errors occurring during reads of DAF
      double precision records are NOT treated as SPICE errors
      and are not signaled.
 
-Files
 
   All SPK files loaded by furnsh_c or spklef_c are potential search
   targets for spksfs_c.
 
-Particulars
    
   This routine finds the highest-priority segment, in any loaded 
   SPK file, such that the segment provides data for the specified 
   body and epoch. 
 
-Examples
 
 
   1) Find a segment for the Pluto barycenter, with coverage for
      a specified epoch, in a JPL planetary SPK file.

      This example uses the following meta-kernel:


         KPL/MK

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                     Contents
            ---------                     --------
            de421.bsp                     Planetary ephemeris
            naif0010.tls                  Leapseconds


         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp'
                                'naif0010.tls'  )

         \begintext

         End of meta-kernel



      Example code starts here.

         #include <stdio.h>
         #include "SpiceUsr.h"

         int main()
         {
            /.
            Local parameters 
            ./
            #define ND                       2
            #define NI                       6
            #define DSCSIZ                   5
            #define SIDLEN1                 41

            /.
            Local variables 
            ./
            SpiceBoolean            found;

            SpiceChar               segid   [ SIDLEN1 ];
            SpiceChar             * reqtim;

            SpiceDouble             dc      [ ND ];
            SpiceDouble             descr   [ DSCSIZ ];
            SpiceDouble             et;

            SpiceInt                handle;
            SpiceInt                ic      [ NI ];
            SpiceInt                idcode;

            /.
            Load a meta-kernel that specifies a planetary SPK file
            and leapseconds kernel. The contents of this meta-kernel
            are displayed above.
            ./
            furnsh_c ( "spksfs.tm" );

            /.
            Get the NAIF ID code for the Pluto system barycenter.
            This is a built-in ID code, so something's seriously
            wrong if we can't find the code.
            ./
            bodn2c_c ( "PLUTO BARYCENTER", &idcode, &found );

            if ( !found )
            {
               sigerr_c( "SPICE(BUG)" );
            }

            /.
            Pick a request time; convert to seconds past J2000 TDB.
            ./
            reqtim = "2011 FEB 18 UTC";

            str2et_c ( reqtim, &et );

            /.
            Find a loaded segment for the specified body and time. 
            ./
            spksfs_c ( idcode, et, SIDLEN1, &handle, descr, segid, &found );

            if ( !found )
            {
               printf ( "No descriptor was found for ID %ld at "
                        "TDB %24.17e\n", 
                        (long) idcode,
                        et                                       );
            }
            else
            {
               /.
               Display the DAF file handle.
               ./
               printf ( "\n"
                        "DAF handle: %ld\n"
                        "\n",
                        (long)handle        );

               /.
               Display the segment ID.

               Unpack the descriptor. Display the contents.
               ./
               dafus_c ( descr, ND, NI, dc, ic );

               printf ( "Segment found.\n"
                        "   Segment ID:        %s\n"
                        "   Body ID code:      %ld\n"
                        "   Center ID code:    %ld\n"
                        "   Frame ID code:     %ld\n"
                        "   SPK data type:     %ld\n"
                        "   Start time (TDB):  %24.17e\n"
                        "   Stop time  (TDB):  %24.17e\n",
                        segid,
                        (long) ic[0],
                        (long) ic[1],
                        (long) ic[2],
                        (long) ic[3],
                        dc[0],
                        dc[1]                             );
            }

            return ( 0 );
         }

 
      When executed on a PC/Linux/gcc platform, this program 
      produced the following output:
      

         DAF handle: 1

         Segment found.
            Segment ID:        DE-0421LE-0421
            Body ID code:      9
            Center ID code:    0
            Frame ID code:     1
            SPK data type:     2
            Start time (TDB):  -3.16919520000000000e+09
            Stop time  (TDB):   1.69685280000000000e+09
 


-Restrictions

   1) If a Fortran I/O error occurs while this routine searches a
      loaded SPK file, the internal state of SPK segment and file
      selection routines, which are all entry points in the f2c'd
      version for the Fortran routine SPKBSR, may be corrupted.

-Literature_References
 
   SPK Required Reading. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
   R.E. Thurman    (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 05-OCT-2012 (NJB) (RET)

-Index_Entries
 
   select spk file and segment 
 
-&
*/

{  /* Begin spksfs_c */


   /*
   Local variables
   */
   logical                 fnd;


   /*
   Participate in error tracing.
   */
   chkin_c ( "spksfs_c" );


   /*
   Make sure the output segment ID string has at least enough room for
   one output character and a null terminator.  Also check for a null
   pointer.
   */
   CHKOSTR ( CHK_STANDARD, "spksfs_c", ident, idlen );


   /*
   Call the f2c'd routine. 
   */
   spksfs_ ( (integer    *) &body,
             (doublereal *) &et,
             (integer    *) handle,
             (doublereal *) descr,
             (char       *) ident,
             (logical    *) &fnd,
             (ftnlen      ) idlen-1  );

   /*
   Set the output found flag based on the local one of
   type logical. 
   */

   *found = (SpiceBoolean) fnd;


   /*
   Convert the output segment ID string from Fortran to C style. 

   Note that this is necessary even if the found flag is false
   or if a SPICE error was signaled, since it's unsafe to return
   a string without a null terminator.
   */
   F2C_ConvertStr ( idlen, ident );


   chkout_c ( "spksfs_c" );

} /* End spksfs_c */

