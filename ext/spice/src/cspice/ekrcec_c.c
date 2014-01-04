/*

-Procedure ekrcec_c ( EK, read column entry element, character )

-Abstract
 
   Read data from a character column in a specified EK record. 
 
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
   #include "SpiceZmc.h"

   void ekrcec_c ( SpiceInt           handle,
                   SpiceInt           segno,
                   SpiceInt           recno,
                   ConstSpiceChar   * column,
                   SpiceInt           lenout,
                   SpiceInt         * nvals,
                   void             * cvals,
                   SpiceBoolean     * isnull )

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   handle     I   Handle attached to EK file. 
   segno      I   Index of segment containing record. 
   recno      I   Record from which data is to be read. 
   column     I   Column name. 
   lenout     I   Maximum length of output strings.
   nvals      O   Number of values in column entry. 
   cvals      O   Character values in column entry. 
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
 
   lenout         is the maximum string length that can be accommodated in
                  the output array cvals.  This length must large enough to 
                  hold the longest element of the specified column entry, 
                  including a null terminator.  If the column element contains
                  strings of length up to n characters, lenout should be set
                  to n + 1. 


-Detailed_Output
 
   nvals, 
   cvals          are, respectively, the number of values found in 
                  the specified column entry and the set of values 
                  themselves.  The array cvals must have sufficient 
                  string length to accommodate the longest string 
                  in the returned column entry. The calling application
                  should declare cvals with dimension

                     [nelts][lenout]
                  
                  where nelts is the maximum number of elements that 
                  occur in any entry of the specified column. 

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
       character, the error SPICE(WRONGDATATYPE) will be 
       signaled. 
 
   6)  If column specifies a column of whose class is not 
       a character class known to this routine, the error 
       SPICE(NOCLASS) will be signaled. 
 
   7)  If an attempt is made to read an uninitialized column entry, 
       the error will be diagnosed by routines called by this  
       routine.  A null entry is considered to be initialized, but 
       entries do not contain null values by default. 
 
   8)  If an I/O error occurs while reading or writing the indicated 
       file, the error will be diagnosed by routines called by this 
       routine. 
 
   9)  If any element of the column entry would be truncated when 
       assigned to an element of cvals, the error will be diagnosed 
       by routines called by this routine. 

   10) If the input column name string pointer is null, the error
       SPICE(NULLPOINTER) will be signaled.

   11) If the input column name string has length zero, the error 
       SPICE(EMPTYSTRING) will be signaled.

   12) If the output string pointer cvals is null, the error SPICE(NULLPOINTER)
       will be signaled.

   13) If the output string length indicated by lenout is less than two 
       characters, it is too short to contain one character of output data
       plus a null terminator, so it cannot be passed to the underlying Fortran
       routine.  In this event, the error SPICE(STRINGTOOSHORT) is
       signaled.
 
-Files
 
   See the EK Required Reading for a discussion of the EK file 
   format. 
 
-Particulars
 
   This routine is a utility that allows an EK file to be read 
   directly without using the high-level query interface. 
 
-Examples
 
   1)  Read the value in the third record of the column ccol in 
       the fifth segment of an EK file designated by handle. 
 
          #include "SpiceUsr.h"
             .
             .
             .
          ekrcec_c ( handle, 4, 2, "CCOL", lenout, &nvals, &cval, &isnull );
 
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

   -CSPICE Version 1.1.0, 21-MAY-2001 (WLT)

       Added a cast to (char *) in the call to  F2C_ConvertStrArr to
       support compilation under C++.
   
   -CSPICE Version 1.0.0, 04-JUL-2000 (NJB)

-Index_Entries
 
   read character data from EK column 
 
-&
*/

{ /* Begin ekrcec_c */


   /*
   Local variables
   */
   logical                 null;


   /*
   Participate in error tracing.
   */
   chkin_c ( "ekrcec_c" );


   /*
   Check the column name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekrcec_c", column );


   /*
   Make sure the output array has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "ekrcec_c", cvals, lenout );

   /*
   Map the segment and record numbers to their Fortran-style
   values.  Pass a flag of type logical to ekrced_.
   */
    
   segno++;
   recno++;


   ekrcec_ ( ( integer * ) &handle,
             ( integer * ) &segno,
             ( integer * ) &recno,
             ( char    * ) column,
             ( integer * ) nvals,
             ( char    * ) cvals,
             ( logical * ) &null,
             ( ftnlen    ) strlen(column),
             ( ftnlen    ) lenout-1        );

   /*
   Convert the output array from Fortran to C style. 
   */
   F2C_ConvertStrArr ( *nvals, lenout, (char *) cvals );


   /*
   Cast the null flag back to a SpiceBoolean. 
   */
   *isnull = null;


   chkout_c ( "ekrcec_c" );

} /* End ekrcec_c */
