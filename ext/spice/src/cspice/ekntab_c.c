/*

-Procedure ekntab_c  ( EK, return number of loaded tables )

-Abstract
 
   Return the number of loaded EK tables. 
 
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

   void ekntab_c ( SpiceInt   * n ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   n          O   Number of loaded tables. 
 
-Detailed_Input
 
   None. 
 
-Detailed_Output
 
   n              is the number of loaded tables.  The count refers 
                  to the number of logical tables; if multiple 
                  segments contain data for the same table, these 
                  segments collectively contribute only one table 
                  to the count. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   None. 
 
-Files
 
   The returned count is based on the currently loaded EK files. 
 
-Particulars
 
   This routine is a utility that provides the caller with the 
   number of loaded tables.  Callers of ektnam_c can use this count 
   as the upper bound on set of table indices when looking up table 
   names. 
 
-Examples
 
   1)  Suppose we have the following list of EK files and tables 
       contained in those files: 
 
          File name        Table name 
          ---------        ---------- 
 
          FILE_1.EK        TABLE_1 
                           TABLE_2 
 
          FILE_2.EK        TABLE_1 
                           TABLE_3 
 
          FILE_3.EK        TABLE_2 
                           TABLE_3 
                           TABLE_4 
 
 
       Then after loading these files, the call 
 
          ekntab_c ( &n );
 
       sets n to the value 4. 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 14-OCT-2001 (NJB)

-Index_Entries
 
   return number of loaded tables 
 
-&
*/

{ /* Begin ekntab_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "ekntab_c" );


   ekntab_ (  (integer *) n );


   chkout_c ( "ekntab_c" );

} /* End ekntab_c */ 
