/*

-Procedure ekops_c ( EK, open scratch file )

-Abstract
 
   Open a scratch (temporary) E-kernel file and prepare the file 
   for writing. 
 
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
   UTILITY 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"

   void ekops_c ( SpiceInt   * handle ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   handle     O   File handle attached to new EK file. 
 
-Detailed_Input
 
   None. 
 
-Detailed_Output
 
   handle         is the EK file handle of the file opened by this
                  routine.  This handle is used to identify the file 
                  to other EK routines. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If the indicated file cannot be opened, the error will be 
       diagnosed by routines called by this routine.  The new file 
       will be deleted. 
 
   2)  If an I/O error occurs while reading or writing the indicated 
       file, the error will be diagnosed by routines called by this 
       routine. 
 
-Files
 
   This routine creates a temporary EK file; the file is deleted
   when the calling program terminates.
   
   See the EK Required Reading for a discussion of the EK file 
   format. 
 
-Particulars
 
   This routine operates by side effects:  it opens and prepares 
   a scratch EK for addition of data.  "Scratch" files are automatically
   deleted when the calling program terminates normally.
    
-Examples
 
   1)  Open a scratch EK.  The EK should be closed via EKCLS. 
       The EK file will be deleted when closed. 
 
 
           ekops_c ( &handle );
 
              [Write/Read EK] 
 
           ekcls_c ( handle ); 
 
 
-Restrictions
 
   1)  No more than CSPICE_DAS_MXOPFL files may be opened   
       simultaneously.  See the header file SpicePar.h for the value of 
       CSPICE_DAS_MXOPFL. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 2-APR-1998

-Index_Entries
 
   open scratch E-kernel 
   open scratch EK 
 
-&
*/

{ /* Begin ekops_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "ekops_c" );


   ekops_ ( ( integer * ) handle );


   chkout_c ( "ekops_c" );

} /* End ekops_c */
