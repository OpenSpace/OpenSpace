/*

-Procedure ekacec_c ( EK, add character data to column )

-Abstract
 
   Add data to a character column in a specified EK record. 
 
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
   UTILITY 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #include "SpiceZim.h"
   #undef    ekacec_c

   void ekacec_c  ( SpiceInt          handle,
                    SpiceInt          segno,
                    SpiceInt          recno,
                    ConstSpiceChar  * column,
                    SpiceInt          nvals,
                    SpiceInt          vallen,
                    const void      * cvals,
                    SpiceBoolean      isnull )
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   handle     I   EK file handle. 
   segno      I   Index of segment containing record. 
   recno      I   Record to which data is to be added. 
   column     I   Column name. 
   nvals      I   Number of values to add to column. 
   vallen     I   Declared length of character values.
   cvals      I   Character values to add to column. 
   isnull     I   Flag indicating whether column entry is null. 

-Detailed_Input 

   handle         is the handle of an EK file open for write access. 
 
   segno          is the number of the segment to which the record 
                  is to be added.  EK segment numbers range from
                  0 to N-1, where N is the number of segments
                  in the kernel.
 
   recno          is the index of the record to which data is to be 
                  added.  This record number is relative to the start 
                  of the segment indicated by segno; the first 
                  record in the segment has index 0. 
 
   column         is the name of the column to which data is to be 
                  added. 

   
   nvals          is the number of entries in the value to be added to the 
                  specified column.

   vallen         is the length of the strings in the cvals array, where
                  the length includes space for null terminators.  

                  If the  column has fixed-size entries, then nvals 
                  must equal the entry size for the specified column. 
 

   cvals          is the set of values themselves.  The data values are 
                  written into the specified column and record. 
 
                  The array cvals should be declared with dimensions
                  
                     [nelts][vallen]
                     
                  where nelts is greater than or equal to nvals.

   isnull         is a logical flag indicating whether the entry is 
                  null.  If isnull is SPICEFALSE, the column entry 
                  defined by nvals and cvals is added to the 
                  specified kernel file. 
 
                  If isnull is SPICETRUE, nvals and cvals are ignored:
                  no data are written into the specified column entry. 
                  The column entry is marked as a null value.
 
                  If the column has fixed-length, variable-size 
                  entries, the number of entries is considered to 
                  be 1. 
  
-Detailed_Output
 
   None.  See $Particulars for a description of the effect of this 
   routine. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If handle is invalid, the error will be diagnosed by routines 
       called by this routine. 
 
   2)  If segno is out of range, the error will be diagnosed by 
       routines called by this routine. 
 
   3)  If column is not the name of a declared column, the error 
       will be diagnosed by routines called by this routine. 
 
   4)  If column specifies a column of whose data type is not 
       character, the error SPICE(WRONGDATATYPE) will be 
       signaled. 
 
   5)  If recno is out of range, the error will be diagnosed by 
       routines called by this routine. 
 
   6)  If the specified column has fixed-size entries and nvals 
       does not match this size, the error will be diagnosed by 
       routines called by this routine. 
 
   7)  If the specified column has variable-size entries and nvals 
       is non-positive, the error will be diagnosed by routines 
       called by this routine. 
 
   8)  If an attempt is made to add a null value to a column that 
       doesn't take null values, the error will be diagnosed by 
       routines called by this routine. 
 
   9)  If column specifies a column of whose class is not 
       a character class known to this routine, the error 
       SPICE(NOCLASS) will be signaled. 
 
   10) If an I/O error occurs while reading or writing the indicated 
       file, the error will be diagnosed by routines called by this 
       routine. 

   11) If the input string pointer for column is null, the error 
        SPICE(NULLPOINTER) will be signaled.
      
   12) If the input string column name has length zero, the error 
       SPICE(EMPTYSTRING) will be signaled.
 
   13) If the string pointer for cvals is null, the error
       SPICE(NULLPOINTER) will be signaled.
   
   14) If the string length vallen is less than 2, the error 
       SPICE(STRINGTOOSHORT) will be signaled.
    
 
-Files
 
   See the EK Required Reading for a discussion of the EK file 
   format. 
 
-Particulars
 
   This routine operates by side effects:  it modifies the named 
   EK file by adding data to the specified record in the specified 
   column.  Data may be added to a segment in random order; it is not 
   necessary to fill in columns or rows sequentially.  Data may only 
   be added one column entry at a time. 
 
-Examples
 
   1)  Add the value "999" to the third record of the column CCOL in 
       the fifth segment of an EK file designated by HANDLE. 
 
          #include "SpiceUsr.h"
             .
             .
             .
          ekacec_c ( handle, 4, 2, "CCOL", 1, 4, "999", SPICEFALSE );
 
 
   2)  Same as (1), but this time add a null value.  The argument 
       "999" is ignored because the null flag is set to SPICETRUE.
 
          #include "SpiceUsr.h"
             .
             .
             .
          ekacec_c ( handle, 4, 2, "CCOL", 1, 4, "999", SPICETRUE );
 
 
   3)  Add an array cbuff of 10 values to the third record of the 
       column CARRAY in the fifth segment of an EK file designated by 
       handle.  We assume cbuff was declared as shown:

          SpiceChar cbuff[10][CBLEN];
             
 
          #include "SpiceUsr.h"
             .
             .
             .
          ekacec_c ( handle,  4,      2,      "CARRAY", 
                     10,      CBLEN,  cbuff,  SPICEFALSE );
 
 
   4)  A more detailed example:  append a record to a specified 
       segment. 
 
       Suppose we have an E-kernel named order_db.ek which contains 
       records of orders for data products.  The E-kernel has a 
       table called DATAORDERS that consists of the set of columns 
       listed below: 
 
          DATAORDERS 
 
             Column Name     Data Type 
             -----------     --------- 
             ORDER_ID        INTEGER 
             CUSTOMER_ID     INTEGER 
             LAST_NAME       CHARACTER*(*) 
             FIRST_NAME      CHARACTER*(*) 
             ORDER_DATE      TIME 
             COST            DOUBLE PRECISION 
 
       The order database also has a table of items that have been 
       ordered.  The columns of this table are shown below: 
 
          DATAITEMS 
 
             Column Name     Data Type 
             -----------     --------- 
             ITEM_ID         INTEGER 
             ORDER_ID        INTEGER 
             ITEM_NAME       CHARACTER*(*) 
             DESCRIPTION     CHARACTER*(*) 
             PRICE           DOUBLE PRECISION 
 
 
       We'll suppose that the file order_db.ek contains two segments, 
       the first containing the DATAORDERS table and the second 
       containing the DATAITEMS table. 
 
       If we wanted to insert a new record into the DATAORDERS 
       table in position 0, we'd make the following calls: 


          #include "SpiceUsr.h"
              .
              . 
              . 
          /. 
          Open the database for write access.  This call is 
          made when the file already exists.  See ekopn_c for 
          an example of creating a new file. 
          ./
          ekopw_c ( "order_db.ek", &handle ); 
 
          /.   
          Append a new, empty record to the DATAORDERS 
          table. Recall that the DATAORDERS table 
          is in segment number 0.  The call will return 
          the number of the new, empty record. 
          ./ 
          ekappr_c ( handle, 0, &recno );
 
          /.
          At this point, the new record is empty.  A valid EK 
          cannot contain empty records.  We fill in the data 
          here.  Data items are filled in one column at a time. 
          The order in which the columns are filled in is not 
          important.  We use the ekace*_c (add column entry) 
          routines to fill in column entries.  We'll assume 
          that no entries are null.  All entries are scalar, 
          so the entry size is 1. 
          ./ 
          isnull  =  SPICEFALSE;
          size    =  1; 
 
          /.
          The following variables will contain the data for 
          the new record. 
          ./
          ordid    =   10011; 
          custid   =   531;
          lname    =   "scientist"; 
          fname    =   "joe";
          odate    =   "1995-sep-20"; 
          cost     =   5000.;
 
          /.
          Note that the names of the routines called 
          correspond to the data types of the columns:  the 
          last letter of the routine name is C, I, or D, 
          depending on the data type.  Time values are 
          converted to ET for storage. 
          ./ 
          ekacei_c ( handle,  segno,  recno, "order_id", 
                     size,    ordid,  isnull             ); 
 
          ekacei_c ( handle,  segno,  recno, "customer_id", 
                     size,    custid, isnull              ); 
 
          ekacec_c ( handle,  segno,  recno, "last_name", 
                     size,    vallen, lname,  isnull      );
 
          ekacec_c ( handle,  segno,  recno, "first_name", 
                     size,    vallen, fname,  isnull      ); 
  
          utc2et_c ( odate,   &et );
 

          ekaced_c ( handle, segno,  recno, "order_date", 
                     size,   et,     isnull               ); 
 
          ekaced_c ( handle, segno,  recno, "cost", 
                     size,   cost,   isnull               ); 
 
          /.
          Close the file to make the update permanent. 
          ./
          ekcls_c ( handle );

 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 28-AUG-2001 (NJB)

-Index_Entries
 
   add character data to EK column 
   add data to EK 
   write character data to EK column 
 
-&
*/

{ /* Begin ekacec_c */


   /*
   Local variables
   */
   logical                 null;

   SpiceChar            ** cvalsPtr;
   SpiceChar             * fCvalsArr;

   SpiceInt                i;
   SpiceInt                fCvalsLen;


   /*
   Participate in error tracing.
   */
   chkin_c ( "ekacec_c" );

   /*
   Check the column name to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekacec_c", column );

   /*
   Check the value array to make sure the pointer is non-null 
   and the string length is non-zero.  Note:  this check is normally
   done for output strings:  CHKOSTR is the macro that does the job.
   */
   CHKOSTR ( CHK_STANDARD, "ekacec_c", cvals, vallen );

   /*
   We need to make a blank-padded version of the cvals array.
   We'll first allocate an array of character pointers to index
   the values, initialize this array, and use it to produce
   a dynamically allocated array of Fortran-style strings.
   */
   cvalsPtr = ( SpiceChar ** ) malloc ( nvals * sizeof(SpiceChar *) );

   if ( cvalsPtr == 0 )
   {     
      setmsg_c ( "Failure on malloc call to create pointer array "
                 "for column values."                              );
      sigerr_c ( "SPICE(MALLOCFAILED)"                             );
      chkout_c ( "ekacec_c"                                        );
      return;
   }
   
   for ( i = 0;  i < nvals;  i++  )
   {
      cvalsPtr[i] =  (SpiceChar *)cvals  +  ( i * vallen );
   }
   
   C2F_CreateFixStrArr (  nvals, 
                          vallen,
                          ( ConstSpiceChar ** ) cvalsPtr, 
                          &fCvalsLen, 
                          &fCvalsArr                      );
   
   if ( failed_c() )
   {
      free ( cvalsPtr );
      
      chkout_c ( "ekacec_c" );
      return;
   }

   /*
   Map the segment and record numbers to the Fortran range.  Get a
   local logical variable to represent the null flag.
   */
   segno++;
   recno++;

   null = isnull;
   
   ekacec_ ( ( integer    * ) &handle,
             ( integer    * ) &segno,
             ( integer    * ) &recno,
             ( char       * ) column,
             ( integer    * ) &nvals,
             ( char       * ) fCvalsArr,
             ( logical    * ) &null,
             ( ftnlen       ) strlen(column),
             ( ftnlen       ) fCvalsLen        );


   /*
   Clean up our dynamically allocated arrays.
   */
   free ( cvalsPtr     );
   free ( fCvalsArr    );
   

   chkout_c ( "ekacec_c" );

} /* End ekacec_c */










