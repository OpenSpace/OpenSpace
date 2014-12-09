/*

-Procedure ekuef_c  ( EK, unload event file )

-Abstract
 
   Unload an EK file, making its contents inaccessible to the 
   EK reader routines, and clearing space in order to allow other 
   EK files to be loaded. 
 
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

   void ekuef_c ( SpiceInt handle ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   handle     I   Handle of EK file. 
 
-Detailed_Input
 
   handle         is a file handle returned by eklef_c. 
 
-Detailed_Output
 
   None.  See $Particulars for a description of the effect of this 
   routine. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  Unloading a file that is not loaded has no effect. 
 
-Files
 
   This routine unloads a binary EK file from the EK query system. 
 
-Particulars
 
   This routine removes information about an EK file from the 
   EK system, freeing space to increase the number of other EK 
   files that can be loaded.  The file is also unloaded from 
   the DAS system and closed. 
 
-Examples
 
   1)  Load 25 EK files sequentially, unloading the previous file 
       before each new file is loaded.  Unloading files prevents 
       them from being searched during query execution. 
 
          for ( i = 0;  i < 25;  i++ )
          {
             eklef_c ( ek[i], &handle );
 
             [Perform queries] 
 
             ekuef_c ( handle );
          }

 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 26-JUL-1998 (NJB)

      Based on SPICELIB Version 1.0.1, 07-JUL-1996 (NJB)

-Index_Entries
 
   unload EK file 
 
-&
*/

{ /* Begin ekuef_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "ekuef_c" );

   
   /*
   Call the f2c'd Fortran routine.  
   */
   ekuef_ ( (integer *) &handle );


   chkout_c ( "ekuef_c" );

} /* End ekuef_c */
