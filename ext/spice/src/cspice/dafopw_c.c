/*

-Procedure dafopw_c ( DAF, open for write )

-Abstract
 
   Open a DAF for subsequent write requests. 
 
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
   #include "SpiceZmc.h"
   #include "SpiceZst.h"


   void dafopw_c ( ConstSpiceChar  * fname,
                   SpiceInt        * handle ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   fname      I   Name of DAF to be opened. 
   handle     O   Handle assigned to DAF. 
 
-Detailed_Input
 
   fname       is the name of a DAF to be opened with write 
               access. 
 
-Detailed_Output
 
   handle      is the file handle associated with the file. This 
               handle is used to identify the file in subsequent 
               calls to other DAF routines. 
 
-Parameters
 
   None. 
 
-Files
 
   See argument `fname'. 
 
-Exceptions
 
   1) If the specified file has already been opened, either by 
      the DAF routines or by other code, an error is signaled by 
      routines in the call tree of this routine.  Note that this 
      response is not paralleled by dafopr_c, which allows you 
      to open a DAF for reading even if it is already open for 
      reading. 
 
   2) If the specified file cannot be opened without exceeding 
      the maximum number of files, the error SPICE(DAFFTFULL) 
      is signaled. 
 
   3) If the attempt to read the file's file record fails, the 
      error SPICE(FILEREADFAILED) will be signaled. 
 
   4) If the specified file is not a DAF file, an error is 
      signaled by routines in the call tree of this routine. 
 
   5) If no logical units are available, an error is 
      signaled by routines called by this routine. 
 
   6) If the file does not exist, the error SPICE(FILENOTFOUND) 
      is signaled by routines in the call tree of this routine. 
 
   7) If an I/O error occurs in the process of opening the file, 
      routines in the call tree of this routine signal an error. 
 
   8) If the file name is blank or otherwise inappropriate 
      routines in the call tree of this routine signal an error. 
 
   9) If the file was transferred improperly via FTP, routines 
      in the call tree of this routine signal an error. 
 
  10) If the file utilizes a non-native binary file format, an 
      error is signaled by routines in the call tree of this 
      routine. 

  11) The error SPICE(EMPTYSTRING) is signaled if the file namne
      string does not contain at least one character, since the
      string cannot be converted to a Fortran-style string
      in this case.
      
  12) The error SPICE(NULLPOINTER) is signaled if the input file
      name string pointer is null.
 
-Particulars
 
   Most DAFs require only read access. If you do not need to 
   change the contents of a file, you should open it with dafopr_c. 
   Use dafopw_c when you need to 
 
      -- change (update) one or more summaries, names, or 
         arrays within a file; or 
 
      -- add new arrays to a file. 
 
-Examples
 
   In the following code fragment, dafopw_c is used to open a 
   file, which is then searched for arrays containing data for 
   a particular object. The code for the object is then changed 
   (perhaps to reflect some new convention). 
  
      #include "SpiceUsr.h"
      #include "SpiceZfc.h"

      int main()
      {
         void dafopw_c ( ConstSpiceChar  * fname,
                         SpiceInt        * handle );

         #define  DSCSIZ         5 
         #define  FILSIZ         256 
         #define  LINSIZ         81
         #define  ND             2
         #define  NI             6

         SpiceBoolean            found;

         SpiceChar               fname      [ FILSIZ ];
         SpiceChar               line       [ LINSIZ ];

         SpiceDouble             dc         [ ND ];
         SpiceDouble             sum        [ DSCSIZ ];

         SpiceInt                handle;
         SpiceInt                ic         [ NI ];
         SpiceInt                nd         = ND;
         SpiceInt                new_code;
         SpiceInt                ni         = NI;
         SpiceInt                old_code;


         /.
         Get the file name. 
         ./
         prompt_c ( "Enter name of existing DAF > ", FILSIZ, fname );

         prompt_c ( "Enter ID code to change    > ", LINSIZ, line  );
         prsint_c ( line, &old_code );

         prompt_c ( "Enter replacement code     > ", LINSIZ, line  );
         prsint_c ( line, &new_code );

         /.
         Open the existing DAF file for write access.
         ./
         dafopw_c ( fname, &handle );

         /.
         Start a forward search through the file.
         ./
         dafbfs_c ( handle );

         /.
         Find the first array (segment).
         ./
         daffna_c ( &found );

         while ( found ) 
         {
            /.
            Read and unpack the current DAF array summary
            (aka segment descriptor) sum:
            ./
            dafgs_c ( sum );
            dafus_c ( sum, nd, ni, dc, ic ); 


            if ( ic[0] == old_code ) 
            {
               ic[0] = new_code;

               /.
               Pack the summary array using the updated
               integer array ic.  Note this is an f2c'd
               routine, so the array sizes are passed by
               reference.
               ./
               dafps_ ( &nd, &ni, dc, ic, sum );

               /.
               Replace the segment descriptor in the DAF.
               ./
               dafrs_ ( sum );
            }

            /.
            Find the next segment.
            ./
            daffna_c ( &found );
         }

         /.
         Close the DAF.
         ./
         dafcls_c ( handle );

         return ( 0 );
      }


-Restrictions
 
   1) Only files of the native binary file format may be opened 
      with this routine. 
 
   2) Files opened using this routine must be closed with dafcls_c. 
 
-Literature_References
 
   NAIF Document 167.0, "Double Precision Array Files (DAF) 
   Specification and User's Guide" 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
   K.R. Gehringer  (JPL) 
   J.M. Lynch      (JPL) 
   J.E. McLean     (JPL) 
   W.L. Taber      (JPL) 
   F.S. Turner     (JPL) 
   I.M. Underwood  (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 13-OCT-2004 (NJB) (KRG) (JML) (JEM) (WLT) (FST) (IMU)

-Index_Entries
 
   open existing daf for write 
 
-&
*/

{ /* Begin dafopw_c */


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return; 
   }
   chkin_c ( "dafopw_c" );

   /*
   Check the file name to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "dafopw_c", fname );

   /*
   Let the f2c'd routine do the work.
   */    
   dafopw_ ( ( char     * ) fname,
             ( integer  * ) handle,
             ( ftnlen     ) strlen(fname) );


   chkout_c ( "dafopw_c" );

} /* End dafopw_c */


