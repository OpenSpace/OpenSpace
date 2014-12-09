/*

-Procedure ekgc_c  ( EK, get event data, character )

-Abstract
 
   Return an element of an entry in a column of character 
   type in a specified row. 
 
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
 
   ASSIGNMENT 
   EK 
 
*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void ekgc_c ( SpiceInt          selidx,
                 SpiceInt          row,
                 SpiceInt          elment,
                 SpiceInt          lenout,
                 SpiceChar       * cdata,
                 SpiceBoolean    * null,
                 SpiceBoolean    * found  ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   selidx     I   Index of parent column in SELECT clause. 
   row        I   Row to fetch from. 
   elment     I   Index of element, within column entry, to fetch. 
   lenout     I   Maximum length of column element.
   cdata      O   Character string element of column entry. 
   null       O   Flag indicating whether column entry was null. 
   found      O   Flag indicating whether column was present in row. 
 
-Detailed_Input
 
   selidx         is the SELECT clause index of the column to fetch
                  from.  The range of selidx is from 0 to one less than
                  the number of columns in the SELECT clause.                 
 
   row            is the output row containing the entry to fetch 
                  from.  The range of row is from 0 to one less than
                  the number of rows satisfying the previous query.
 
   elment         is the index of the element of the column entry 
                  to fetch.  The normal range of elment is from 0 to 
                  one less than the size of the column's entry, but 
                  elment is allowed to exceed the number of elements in 
                  the column entry; if it does, found is returned 
                  as SPICEFALSE.  This allows the caller to read data 
                  from the column entry in a loop without checking the 
                  number of available elements first. 
 
                  Null values in variable-sized columns are 
                  considered to have size 1. 

   lenout         is the maximum allowed length of a string that
                  can be fetched into the string cdata.  This length 
                  must large enough to hold the specified element of the 
                  column entry, plus a null terminator.  If the column 
                  element is expected to have x characters, lenout needs
                  to be x + 1. 

 
-Detailed_Output
 
   cdata          is the requested element of the specified column 
                  entry.  If the entry is null, cdata is undefined. 
 
                  If cdata is too short to accommodate the requested 
                  column entry element, the element is truncated on 
                  the right to fit cdata.  
                   
   null           is a logical flag indicating whether the entry 
                  belonging to the specified column in the specified 
                  row is null. 
 
   found          is a logical flag indicating whether the specified 
                  element was found.  If the element does not exist, 
                  found is returned as SPICEFALSE.
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If the input argument elment is less than 0, found is returned as
       SPICEFALSE, and the error SPICE(INVALIDINDEX) is signalled. 
       However, elment is allowed to be greater than or equal to 
       the number of elements in the specified column entry; this allows
       the caller to read data from the column entry in a loop without 
       checking the number of available elements first.  If elment is 
       greater than or equal to the number of available elements, found 
       is returned as SPICEFALSE.
 
   2)  If selidx is outside of the range established by the 
       last query passed to eksrch_, the error SPICE(INVALIDINDEX) 
       will be signalled. 
 
   3)  If the input argument row is less than 0 or greater than or
       equal to the number of rows matching the query, found is returned 
       as SPICEFALSE, and the error SPICE(INVALIDINDEX) is signalled. 
 
   4)  If the specified column does not have character type, the 
       error SPICE(INVALIDTYPE) is signalled. 
 
   5)  If this routine is called when no E-kernels have been loaded, 
       the error SPICE(NOLOADEDFILES) is signalled. 
 
-Files
 
      The EK "query and fetch" suite of functions reads binary `sequence
      component' EK files.  In order for a binary EK file to be 
      accessible to this routine, the file must be `loaded' via a call 
      to the function eklef_c.
 
      Text format EK files cannot be used by this routine; they must
      first be converted by binary format by the NAIF Toolkit utility
      SPACIT.
 
-Particulars
 
   This routine allows retrieval of data from character columns. 
 
   This routine returns one element at a time in order to save the 
   caller from imposing a limit on the size of the column entries 
   that can be handled. 
 
-Examples
 
   1)  Suppose the EK table TAB contains the following columns: 
 
          Column name   Data Type   Size 
          -----------   ---------   ---- 
          CHR_COL_1     CHR         1 
          CHR_COL_2     CHR         VARIABLE 
          CHR_COL_3     CHR         10 
 
 
       Suppose the query 
 
          query = "SELECT CHR_COL_1 FROM TAB"
 
       is issued to ekfind_c via the call 
 
          ekfind_c ( query, lenout, nmrows, error, errmsg );
 
       To fetch and dump column values from the rows that satisfy the 
       query, the loop below could be used.  Note that we don't check 
       the found flags returned by ekgc_c since we know that every 
       entry in column CHR_COL_1 contains one element. 
 
          /.
          Since CHR_COL_1 was the first column selected, 
          the selection index selidx is set to 0. 
          The column is scalar, so the element index eltidx 
          is set to 0.  The variable nmrows is the number of 
          matching rows returned by ekfind_c. 
          ./
 
          selidx = 0;
          eltidx = 0;
 
          for ( row = 0;  row < nmrows;  row++ )
          {
             printf ( "\nRow = %d\n\n", row );
 
             /.
             Fetch values from column CHR_COL_1. 
             ./
             ekgc_c ( selidx,  row,      eltidx,  lenout,
                      cval,    &isnull,  &found           );
                      
             if ( isnull )
             {
                 printf ( "%s\n", "<null>" );
             }
             else
             {
                 printf ( "%s\n", cval );
             }
          } 
 
 
   2)  Suppose the EK table TAB is as in example 1, and we issue 
       the query 
 
          query = "SELECT CHR_COL_1, CHR_COL_2, CHR_COL_3 FROM TAB" 
 
       to ekfind_c via the call 
 
          ekfind_c ( query, lenout, &nmrows, &error, errmsg );
 
       To fetch and dump column values from the rows that satisfy the 
       query, the loop below could be used.  Note that we don't check 
       the found flags returned by ekgc_c since we know in advance how 
       many elements are contained in each column entry we fetch. 
 
 
          for ( row = 0;  row < nmrows;  row++ )
          {
             printf ( "\nRow = %d\n\n", row );
 
             /.
             Fetch values from column CHR_COL_1.  Since 
             CHR_COL_1 was the first column selected, the 
             selection index selidx is set to 0. 
             ./
             
             selidx = 0; 
             eltidx = 0;
             
             ekgc_c ( selidx,    row,      eltidx,  lenout,
                      cvals[0],  &isnull,  &found           ); 

             printf ( "\nColumn = CHR_COL_1\n\n" );
             
             if ( isnull )
             {
                 printf ( "%s\n", "<null>" );
             }
             else
             {
                 printf ( "%s\n", cvals[0] );
             }

 
             /.
             Fetch values from column CHR_COL_2 in the current 
             row.  Since CHR_COL_2 contains variable-size array 
             entries, we call eknelt_c to determine how many 
             elements to fetch. 
             ./
             selidx = 1;
             
             eknelt_c ( selidx, row, &nelt );
             
             eltidx = 0;
             isnull = SPICEFALSE;
             
             while (  ( eltidx < nelt ) && ( !isnull )  )
             {
    
                ekgc_c ( selidx,         row,      eltidx,  lenout,
                         cvals[eltidx],  &isnull,  &found          );
    
                eltidx++;
    
                /.
                If the column entry is null, we'll be kicked 
                out of this loop after the first iteration. 
                ./
             }
    
             printf ( "\nColumn = CHR_COL_2\n\n" );
   
             if ( isnull )
             {
                 printf ( "%s\n", "<null>" );
             }
             else
             {
                 for ( i = 0;  i < nelt;  i++ )
                 {
                    printf ( "%s\n", cvals[i] );
                 }
             }


             /.
             Fetch values from column CHR_COL_3 in the current 
             row.  We need not call eknelt_c since we know how 
             many elements are in each column entry. 
             ./
             selidx = 2;
             eltidx = 0;
             isnull = SPICEFALSE;
     
              
             while (  ( eltidx < 10 ) && ( !isnull )  )
             {
    
                ekgc_c ( selidx,         row,      eltidx,  lenout,
                         cvals[eltidx],  &isnull,  &found          );
    
                eltidx++;
             }


             printf ( "\nColumn = CHR_COL_3\n\n" );
   
             if ( isnull )
             {
                 printf ( "%s\n", "<null>" );
             }
             else
             {
                 for ( i = 0;  i < 10;  i++ )
                 {
                    printf ( "%s\n", cvals[i] );
                 }
             }

          }
 
   3)  See the Examples section of the query routine ekfind_c 
       for an example in which the names and data types of the 
       columns from which to fetch data are not known in advance. 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.1.0, 09-JUL-1998 (NJB)
   
       Bug fix:  now uses local logical variable to capture the
       error flag value returned by the underlying f2c'd routine.

   -CSPICE Version 1.0.0, 27-MAR-1998

       Based on SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)

-Index_Entries
 
   fetch element from character column entry 
 
-&
*/

{ /* Begin ekgc_c */

   /*
   Local variables
   */
   logical                 fnd;


   /*
   Participate in error tracing.
   */
   chkin_c ( "ekgc_c" );


   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "ekgc_c", cdata, lenout );


   /*
   Convert indices to Fortran-style; increment each index.
   */
   selidx ++;
   row    ++;
   elment ++;
   

   /*
   Call the f2c'd routine.
   */
   ekgc_  ( ( integer * ) &selidx,
            ( integer * ) &row,
            ( integer * ) &elment,
            ( char    * ) cdata,
            ( logical * ) null,
            ( logical * ) &fnd,
            ( ftnlen    ) lenout-1 );

   /*
   Convert the Fortran string to a C string by placing a null after the
   last non-blank character.  This operation is valid whether or not the
   SPICELIB routine signaled an error.
   */
   F2C_ConvertStr ( lenout, cdata );


   /*
   Set the SpiceBoolean output found flag.
   */
   
   *found  =  fnd;
   
   
   chkout_c ( "ekgc_c" );

} /* End ekgc_c */
