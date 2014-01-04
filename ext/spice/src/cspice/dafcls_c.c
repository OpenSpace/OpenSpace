/*

-Procedure dafcls_c ( DAF, close )

-Abstract
 
   Close the DAF associated with a given handle. 
 
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
 
   DAF 
 
-Keywords
 
   DAF 
   FILES 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void dafcls_c ( SpiceInt handle ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   handle     I   Handle of DAF to be closed. 
 
-Detailed_Input
 
   handle      is the file handle of a previously opened DAF file. 
 
-Detailed_Output
 
   None. 
 
-Parameters
 
    None. 
 
-Files
 
   None. 
 
-Exceptions
 
   1) If the specified handle is not known to the DAF subsystem
      (because it does not belong to a file opened via the DAF
      API), nothing happens. 

   2) If this routine is used to close a file whose handle is
      known to the DAF subsystem, and if the file handle is
      attached to a non-DAF file, routines called by this 
      routine signal an error.
 
-Particulars
 
   Because the DAF subsystem must keep track of what files are open at
   any given time, it is important that DAF files be closed only with
   dafcls_c, to prevent the remaining DAF routines from failing,
   sometimes mysteriously.
 
   Note that when a file is opened more than once for read access, 
   dafopr_c returns the same handle each time it is re-opened. 
   Each time the file is closed, dafcls_c checks to see if any other 
   claims on the file are still active before physically closing 
   the file. 
 
-Examples
 
   In the following code fragment, the arrays in a file are examined in
   order to determine whether the file contains any arrays whose names
   begin with the word TEST. The complete names for these arrays are
   printed to the screen. The file is closed at the end of the search.
 
      #include "SpiceUsr.h"
          .
          .
          .
      dafopr_c ( fname, &handle ); 
      dafbfs_c ( handle );
      daffna_c ( &found );
 
      while ( found ) 
      {
         dafgn_c ( name );
 
         if (  strncmp( name, "TEST", 4 ) == 0  )  
         {
            printf ( "%s\n", name ); 
         }
         daffna_c ( &found );
      }
 
      dafcls_c ( handle );
      
 
   Note that if the file has been opened already by a DAF routine 
   at some other place in the calling program, it remains open. 
   This makes it possible to examine files that have been opened for 
   use by other modules without interfering with the operation of 
   those routines. 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   NAIF Document 167.0, "Double Precision Array Files (DAF) 
   Specification and User's Guide" 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
   K.R. Gehringer  (JPL) 
   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
 
-Version
 
   -CSPICE Version 1.0.1, 28-JAN-2004 (NJB) 

      Header update:  the exceptions section now lists the
      case of attempting to close a non-DAF file using this
      routine.

   -CSPICE Version 1.0.0, 01-AUG-1999 (NJB) (KRG) (WLT) (IMU)

-Index_Entries
 
   close daf 
 
-&
*/

{ /* Begin dafcls_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "dafcls_c" );


   dafcls_ ( ( integer * ) &handle  );
   

   chkout_c ( "dafcls_c" );

} /* End dafcls_c */
