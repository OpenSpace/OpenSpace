/*

-Procedure ektnam_c  ( EK, return name of loaded table )

-Abstract
 
   Return the name of a specified, loaded table. 
 
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

   void ektnam_c ( SpiceInt     n,
                   SpiceInt     lenout,
                   SpiceChar  * table  ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   n          I   Index of table. 
   lenout     I   Maximum table name length.
   table      O   Name of table. 
 
-Detailed_Input
 
   n              is the index of the table whose name is desired. 
                  The value of n ranges from 0 to one less than the 
                  number of loaded tables. 
 
   lenout         is the maximum allowed table name length, including
                  space for the terminating null character.  Normally
                  the caller should allow enough room for 
                  SPICE_EK_TSTRLN characters; this parameter is 
                  declared in the header SpiceEK.h.
                  
-Detailed_Output
 
   table          is the name of the nth loaded table.  If table
                  is too small to accommodate the name, the name will
                  be truncated on the right.
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If this routine is called when no files are loaded, the 
      error SPICE(NOLOADEDFILES) is signaled. 
 
   2) If the input n is out of range, the error SPICE(INVALDINDEX) 
      is signaled. 
 
   3) If the output string pointer is null, the error SPICE(NULLPOINTER)
      is signaled.
      
   4) If the output string has length less than two characters, it 
      is too short to contain one character of output data plus a null  
      terminator, so it cannot be passed to the underlying Fortran 
      routine.  In this event, the error SPICE(STRINGTOOSHORT) is
      signaled.
      
   5) If the length of table (indicated by lenout) is at least two
      characters but not large enough to contain the output string,
      the output string will be truncated on the right. 

-Files
 
   The returned name is based on the currently loaded EK files. 
 
-Particulars
 
   This routine is a utility that provides the caller with the 
   name of a specified loaded table.  The index of a table with 
   a given name depends on the kernels loaded and possibly on 
   the order in which the files have been loaded. 
 
-Examples
 
   1)  Dump the names of the loaded tables. 
 
       #include "SpiceUsr.h"
          .
          .
          .
       ekntab_c ( &n );

       for ( i = 0;  i < n;  i++ )
       {
          ektnam_c ( i,      table );
          printf   ( "%s\n", table );
       } 

 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.1, 26-MAR-2003 (NJB) 

       Fixed description of exception (5):  replaced "lenout-1"
       with "lenout."  Removed spurious word "clock" from string
       description.

   -CSPICE Version 1.0.0, 07-JAN-2002 (NJB)

-Index_Entries
 
   return name of a loaded table 
 
-&
*/

{ /* Begin ektnam_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "ektnam_c" );


   /*
   Make sure the output table has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "ektnam_c", table, lenout );

   /*
   Map the table index to a Fortran-style index. 
   */   
   n++;

   ektnam_ (  ( integer * ) &n,
              ( char    * ) table,
              ( ftnlen    ) lenout-1  );

   /*
   Convert the Fortran string to a C string by placing a null
   after the last non-blank character.  This operation is valid
   whether or not the CSPICE routine signaled an error.
   */
   F2C_ConvertStr ( lenout, table );


   chkout_c ( "ektnam_c" );

} /* End ektnam_c */
