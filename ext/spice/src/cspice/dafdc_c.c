/*

-Procedure dafdc_c ( DAF delete comments )

-Abstract
 
   Delete the entire comment area of a specified DAF file.
 
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
 
   None. 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"

   void dafdc_c ( SpiceInt handle ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   handle     I   The handle of a binary DAF opened for writing. 
 
-Detailed_Input
 
   handle    is the handle of a binary DAF that is to have its entire  
             comment area deleted. The DAF must have been opened  
             with write access. 
 
-Detailed_Output
 
   None. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)   If the binary DAF attached to `handle' is not open with write  
        access, an error will be signaled by a routine called by  
        this routine. 
 
-Files
 
   See argument `handle' in $ Detailed_Input. 
 
-Particulars
 
   A binary DAF contains an area which is reserved for storing
   annotations or descriptive textual information about the data
   contained in a file. This area is referred to as the ``comment
   area'' of the file. The comment area of a DAF is a line oriented
   medium for storing textual information. The comment area preserves
   any leading or embedded white space in the line(s) of text which are
   stored, so that the appearance of the of information will be
   unchanged when it is retrieved (extracted) at some other time.
   Trailing blanks, however, are NOT preserved, due to the way that
   character strings are represented in standard Fortran 77.
 
   This routine will delete the entire comment area from the binary DAF
   attached to `handle'. The size of the binary DAF will remain
   unchanged. The space that was used by the comment records is
   reclaimed:  the data area of the DAF is shifted toward the beginning

 
-Examples
 
   Let  
    
         handle   be the handle of a DAF which has been opened 
                  with write access. 
                   
   The call 
    
         dafdc_c ( handle );
         
   deletes the entire comment area of the binary DAF attached to  
   `handle'. 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL)
   K.R. Gehringer (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 16-NOV-2006 (NJB) (KRG)

-Index_Entries
 
    delete DAF comment area 
 
-&
*/

{ /* Begin dafdc_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "dafdc_c" );


   /*
   Hand off the task to the f2c'd routine. 
   */
   dafdc_ ( (integer *) &handle );



   chkout_c ( "dafdc_c" );

} /* End dafdc_c */
