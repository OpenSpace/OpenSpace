/*

-Procedure ekrcei_c ( EK, read column entry element, integer )

-Abstract
 
   Read data from an integer column in a specified EK record. 
 
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
   #include "SpiceZmc.h"

   void ekrcei_c ( SpiceInt           handle,
                   SpiceInt           segno,
                   SpiceInt           recno,
                   ConstSpiceChar   * column,
                   SpiceInt         * nvals,
                   SpiceInt         * ivals,
                   SpiceBoolean     * isnull )
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   handle     I   Handle attached to EK file. 
   segno      I   Index of segment containing record. 
   recno      I   Record from which data is to be read. 
   column     I   Column name. 
   nvals      O   Number of values in column entry. 
   ivals      O   Integer values in column entry. 
   isnull     O   Flag indicating whether column entry is null. 
 
-Detailed_Input
 
   handle         is an EK file handle.  The file may be open for 
                  read or write access.   
 
   segno          is the index of the segment from which data is to 
                  be read.  The first segment in the file has index 0.
 
   recno          is the index of the record from which data is to be 
                  read.  This record number is relative to the start 
                  of the segment indicated by segno; the first 
                  record in the segment has index 0. 
 
   column         is the name of the column from which data is to be 
                  read. 
 
 
-Detailed_Output
 
   nvals, 
   ivals          are, respectively, the number of values found in 
                  the specified column entry and the set of values 
                  themselves. 
 
                  For columns having fixed-size entries, when a  
                  a column entry is null, nvals is still set to the 
                  column entry size.  For columns having variable- 
                  size entries, nvals is set to 1 for null entries. 
 
   isnull         is a logical flag indicating whether the returned  
                  column entry is null.   
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If handle is invalid, the error will be diagnosed by routines 
       called by this routine. 
 
   2)  If segno is out of range, the error will diagnosed by routines 
       called by this routine. 
 
   3)  If recno is out of range, the error will diagnosed by routines 
       called by this routine. 
 
   4)  If column is not the name of a declared column, the error 
       will be diagnosed by routines called by this routine. 
 
   5)  If column specifies a column of whose data type is not 
       integer, the error SPICE(WRONGDATATYPE) will be 
       signaled. 
 
   6)  If column specifies a column of whose class is not 
       an integer class known to this routine, the error 
       SPICE(NOCLASS) will be signaled. 
 
   7)  If an attempt is made to read an uninitialized column entry, 
       the error will be diagnosed by routines called by this  
       routine.  A null entry is considered to be initialized, but 
       entries do not contain null values by default. 
 
   8)  If an I/O error occurs while reading the indicated file, 
       the error will be diagnosed by routines called by this 
       routine. 
 
   9)  If the input column name string pointer is null, the error
       SPICE(NULLPOINTER) will be signaled.

   10) If the input column name string has length zero, the error 
       SPICE(EMPTYSTRING) will be signaled.

-Files
 
   See the EK Required Reading for a discussion of the EK file 
   format. 
 
-Particulars
 
   This routine is a utility that allows an EK file to be read 
   directly without using the high-level query interface. 
 
-Examples
 
   1)  Read the value in the third record of the column ICOL in 
       the fifth segment of an EK file designated by handle. 
          
          #include "SpiceUsr.h"
             .
             .
             .
          ekrcei_c ( handle, 4, 2, "ICOL", &n, &ival, &isnull );
 
-Restrictions
 
   1) EK files open for write access are not necessarily readable. 
      In particular, a column entry can be read only if it has been 
      initialized. The caller is responsible for determining 
      when it is safe to read from files open for write access. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 04-JUL-2000 (NJB)

-Index_Entries
 
   read integer data from EK column 
 
-&
*/

{ /* Begin ekrcei_c */

   /*
   Local variables
   */
   logical                 null;


   /*
   Participate in error tracing.
   */
   chkin_c ( "ekrcei_c" );

   /*
   Map the segment and record numbers to their Fortran-style
   values.  Pass a flag of type logical to ekrcei_.
   */
    
   segno++;
   recno++;

   ekrcei_  (  ( integer * ) &handle,
               ( integer * ) &segno,
               ( integer * ) &recno,
               ( char    * ) column,
               ( integer * ) nvals,
               ( integer * ) ivals,
               ( logical * ) &null,
               ( ftnlen    ) strlen(column) );

   /*
   Set the output null flag. 
   */
   
   *isnull = null;

   chkout_c ( "ekrcei_c" );

} /* End ekrcei_c */
