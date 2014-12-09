/*

-Procedure eklef_c  ( EK, load event file )

-Abstract
 
   Load an EK file, making it accessible to the EK readers. 
 
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
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void eklef_c ( ConstSpiceChar  * fname,
                  SpiceInt        * handle ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   fname      I   Name of EK file to load. 
   handle     O   File handle of loaded EK file. 
 
-Detailed_Input
 
   fname          is the name of a binary EK file to be loaded. 
 
-Detailed_Output
 
   handle         is the handle of the EK file.  The file is 
                  accessible by the EK reader routines once it 
                  has been loaded. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If the EK file indicated by fname contains a column whose 
       name matches that of a column in an already loaded EK, but 
       whose declared attributes don't match those of the loaded 
       column of the same name, the error SPICE(BADATTRIBUTES) is 
       signalled.  handle is is undefined in this case. 
 
   2)  Loading an EK file that is already loaded does not cause side 
       effects.  The handle already associated with the file will be 
       returned. 
 
   3)  If a file open error occurs, the problem will be diagnosed by 
       routines called by this routine.  handle is undefined in 
       this case. 
 
   4)  If loading the input file would cause the maximum number of 
       loaded EK files to be exceeded, the error 
       SPICE(EKFILETABLEFULL) will be signalled.  handle is 
       undefined in this case.  This routine will attempt to 
       unload the file from the DAS system. 
 
   5)  If loading the input file would cause the maximum number of 
       loaded DAS files to be exceeded, the error will be diagnosed 
       by routines called by this routine.  handle is undefined in 
       this case.  This routine will attempt to unload the file 
       from the DAS system. 
 
   6)  If loading the input file would cause the maximum number of 
       segments allowed in loaded EK files to be exceeded, the error 
       SPICE(EKSEGMENTTABLEFULL) will be signalled.  handle is 
       is undefined in this case.  This routine will attempt to 
       unload the file from the DAS system. 
 
   7)  If loading the input file would cause the maximum number of 
       columns allowed in loaded EK files to be exceeded, the error 
       SPICE(EKCOLDESCTABLEFULL) will be signalled.  handle is 
       is undefined in this case.  This routine will attempt to 
       unload the file from the DAS system. 
 
   8)  If loading the input file would cause the maximum allowed 
       number of columns having distinct attributes in loaded EK 
       files to be exceeded, the error SPICE(EKCOLATTRTABLEFULL) will 
       be signalled.  handle is is undefined in this case.  This 
       routine will attempt to unload the file from the DAS system. 
 
   9)  If loading the input file would cause the maximum number of 
       instrument codes allowed in loaded EK files to be exceeded, 
       the error SPICE(EKIDTABLEFULL) will be signalled.  handle is 
       is undefined in this case.  This routine will attempt to 
       unload the file from the DAS system. 
 
   10) If the input file does not contain at least one segment, the 
       error SPICE(EKNOSEGMENTS) will be signalled. 
 
-Files
 
   This routine loads a binary EK into the CSPICE query system. 
 
-Particulars
 
   This routine makes EK files known to the EK system.  It is 
   necessary to load EK files using this routine in order to 
   query the files using the EK readers. 
 
-Examples
 
   1)  Load three EK files.  During query execution, all files 
       will be searched. 
 
          for ( i = 0;  i < 3;  i++ )
          {
             eklef_c ( ek[i], &handle );
          }
          
          [Perform queries] 
 
 
   2)  Load 25 EK files sequentially, unloading the previous file 
       before each new file is loaded.  Unloading files prevents 
       them from being searched during query execution. 
 
          for ( i = 0;  i < 25;  i++ )
          {
             eklef_c ( ek[i], &handle );
 
             [Perform queries] 
 
             ekuef_c ( handle );
          }

 
-Restrictions
 
   1)  EK files containing columns having the same name but 
       inconsistent declarations are not diagnosed.  Such kernels 
       are invalid in any case. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 26-MAR-1998 (NJB)
   
       Based on SPICELIB Version 1.0.1, 07-JUL-1996 (NJB) 

-Index_Entries
 
   load EK file 
   load E-Kernel 
 
-&
*/

{ /* Begin eklef_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "eklef_c" );

   /*
   Check the file name to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "eklef_c", fname );

   /*
   Call the f2c'd Fortran routine.  Use explicit type casts for every
   type defined by f2c.
   */
   eklef_ (  ( char     * )  fname,
             ( integer  * )  handle, 
             ( ftnlen     )  strlen(fname)  );


   chkout_c ( "eklef_c" );

} /* End eklef_c */
