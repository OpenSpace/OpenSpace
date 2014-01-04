/*

-Procedure getfat_c ( Get file architecture and type )

-Abstract
 
   Determine the file architecture and file type of most SPICE kernel 
   files. 
 
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
 
   None. 
 
-Keywords
 
   KERNEL 
   UTILITY 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void getfat_c ( ConstSpiceChar   * file,
                   SpiceInt           arclen,
                   SpiceInt           typlen,
                   SpiceChar        * arch,
                   SpiceChar        * type   ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   file       I   The name of a file to be examined. 
   arclen     I   Maximum length of output architecture string.
   typlen     I   Maximum length of output type
   string.
   arch       O   The architecture of the kernel file. 
   type       O   The type of the kernel file. 
 
-Detailed_Input
 
   arclen     is the maximum length of output architecture string arch,
              including the terminating null.  For example, if arch
              is to hold strings having 10 characters of actual data,
              arclen should be set to 1l.
              
   typlen     is the maximum length of output file type string type,
              including the terminating null.  For example, if type
              is to hold strings having 10 characters of actual data,
              arclen should be set to 1l.
   
   file       is the name of a SPICE kernel file whose architecture 
              and type are desired. This file must be closed when 
              this routine is called. 
 
-Detailed_Output
 
   arch        is the file architecture of the SPICE kernel file 
               specified by file. If the architecture cannot be 
               determined or is not recognized the value "?" is 
               returned. 
 
               Architectures currently recognized are: 
 
                  DAF - The file is based on the DAF architecture. 
                  DAS - The file is based on the DAS architecture. 
                  XFR - The file is in a SPICE transfer file format. 
                  DEC - The file is an old SPICE decimal text file. 
                  ASC -- An ASCII text file. 
                  KPL -- Kernel Pool File (i.e., a text kernel) 
                  TXT -- An ASCII text file. 
                  TE1 -- Text E-Kernel type 1. 
                   ?  - The architecture could not be determined. 
 
               This variable must be at least 3 characters long. 
 
   type        is the type of the SPICE kernel file. If the type 
               can not be determined the value "?" is returned. 
 
               Kernel file types may be any sequence of at most four 
               printing characters. NAIF has reserved for its use 
               types which contain all upper case letters. 
 
               A file type of "PRE" means that the file is a 
               pre-release file. 
 
               This variable may be at most 4 characters long. 
 
-Parameters
 
   None.
     
-Exceptions
 
   1)  The underlying code translated to C from Fortran effectively
       performs a Fortran INQUIRE on the specified file.  If this
       operation fails for some reason, the error SPICE(INQUIREERROR)
       will be signaled. 

   2)  If the file specified by FILE is already open, the error 
       SPICE(FILECURRENTLYOPEN) will be signaled. 

   3)  If the file specified by FILE does not exist, the error 
       SPICE(NOSUCHFILE) will be signaled. 

   4)  If the attempt to open the file specified by FILE fails, the 
       error SPICE(FILEOPENFAILED) will be signaled. 

   5)  If all attempts to open the file specified by FILE fail, the 
       error SPICE(FILEOPENFAILED) will be signaled. 

   6)  If all attempts to read from the file specified be FILE 
       fail, the error SPICE(FILEREADFAILED) will be signaled. 

   7)  The error SPICE(EMPTYSTRING) is signaled if the input
       string does not contain at least one character, since the
       input string cannot be converted to a Fortran-style string
       in this case.
      
   8)  The error SPICE(NULLPOINTER) is signaled if the input string
       pointer is null.
 
   9)  If either output string pointer is null, the error 
       SPICE(NULLPOINTER) is signaled.
   
   10) If an output string has length less than two characters, it 
       is too short to contain one character of output data plus a null  
       terminator, so it cannot be passed to the underlying Fortran 
       routine.  In this event, the error SPICE(STRINGTOOSHORT) is
       signaled.

 
-Files
 
   The SPICE kernel file specified by FILE is opened and then 
   closed by this routine to determine its file architecture and 
   type. Names of open files should not be passed to this routine. 
 
-Particulars
 
   This subroutine is a support utility routine that determines the 
   architecture and type of a SPICE kernel file. 
 
-Examples
 
   Suppose you wish to write a single routine for loading binary 
   kernels. You can use this routine to determine the type of the 
   file and  then pass the file to the appropriate low level file 
   loader to handle the actual loading of the file. 
 
      getfat_c ( file, arch, type );
 
      if ( eqstr_c( type, "spk" ) )
      {
         spklef_c ( file, &handle );
      }
      
      else if ( eqstr_c( type, "ck" ) )
      {
         cklpf_c ( file, &handle ); 
      }
      
      else if ( eqstr_c( type, "ek" ) )
      {
         eklef ( file, handle ); 
      }
      
      else
      {
         printf ( "%s\n",  
                  "The file could not be identified as aknown\n" 
                  "kernel type.  Did you load the wrong file\n" 
                  "by mistake?"                                 ); 
      }
 
 
-Restrictions
 
   The file to be examined must be closed when this routine is 
   invoked. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   K.R. Gehringer  (JPL) 
   H.A. Neilan     (JPL) 
   W.L. Taber      (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 18-AUG-1998 (NJB)

-Index_Entries
 
   determine the architecture and type of a kernel file 
 
-&
*/

{ /* Begin getfat_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "getfat_c" );

   /*
   Check the input file name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "getfat_c", file );
   
   /*
   Make sure the output architecture string has at least enough room 
   for one output character and a null terminator.  Also check for a 
   null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "getfat_c", arch, arclen );

   /*
   Make sure the output file type string has at least enough room 
   for one output character and a null terminator.  Also check for a 
   null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "getfat_c", type, typlen );

   getfat_ (  ( char   * ) file,
              ( char   * ) arch,
              ( char   * ) type,
              ( ftnlen   ) strlen(file),
              ( ftnlen   ) arclen-1,
              ( ftnlen   ) typlen-1      );
             
             
   /*
   Convert each Fortran output string to a C string by placing a 
   null after the last non-blank character.  This operation is valid
   whether or not the CSPICE routine signaled an error.
   */
   
   F2C_ConvertStr ( arclen, arch );
   F2C_ConvertStr ( typlen, type );
               

   chkout_c ( "getfat_c" );

} /* End getfat_c */
