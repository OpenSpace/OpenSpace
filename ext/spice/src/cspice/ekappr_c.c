/*

-Procedure ekappr_c ( EK, append record onto segment )

-Abstract
 
   Append a new, empty record at the end of a specified E-kernel 
   segment. 
 
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
 
   PRIVATE 
   UTILITY 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void ekappr_c ( SpiceInt     handle,
                   SpiceInt     segno,
                   SpiceInt   * recno  ) 
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   handle     I   File handle. 
   segno      I   Segment number. 
   recno      O   Number of appended record. 
 
-Detailed_Input
 
   handle         is a file handle of an EK open for write access. 
 
   segno          is the number of the segment to which the record 
                  is to be added.  EK segment numbers range from
                  zero to N-1, where N is the number of segments
                  in the kernel.
 
-Detailed_Output
 
   recno          is the number of the record appended by this 
                  routine.  recno is used to identify the record 
                  when writing column entries to it.  EK record
                  numbers range from 0 to N-1, where N is the
                  number of records in the segment containing
                  the record.
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If handle is invalid, the error will be diagnosed by routines 
       called by this routine.  The file will not be modified. 
 
   2)  If segno is out of range, the error SPICE(INVALIDINDEX) 
       will be signalled.  The file will not be modified. 
 
   3)  If an I/O error occurs while reading or writing the indicated 
       file, the error will be diagnosed by routines called by this 
       routine.  The file may be corrupted. 
 
-Files
 
   See the EK Required Reading for a discussion of the EK file 
   format. 
 
-Particulars
 
   This routine operates by side effects:  It appends a new, empty 
   record structure to an EK segment.  The ordinal position of the 
   new record is one greater than the previous number of records in 
   in the segment. 
 
   After a new record has been appended to a segment by this routine, 
   the record must be populated with data using the ekace*_c 
   routines.  EKs are valid only when all of their column entries 
   are initialized. 
 
   To insert a record into a segment at a specified ordinal position, 
   use the routine ekappr_c. 
 
   This routine cannot be used with the "fast write" suite of 
   routines.  See the EK Required Reading for a discussion of the 
   fast writers. 
 
-Examples
 
   1)  Append a record to a specified segment. 
 
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
 
   -CSPICE Version 1.0.0, 09-JAN-2002 (NJB)

-Index_Entries
 
   append record to EK segment 
 
-&
*/

{ /* Begin ekappr_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "ekappr_c" );

   /*
   Convert the segment number to a Fortran index.
   */
   segno++;

   
   ekappr_ (  ( integer * )  &handle,
              ( integer * )  &segno,
              ( integer * )  recno   );

   /*
   Convert the record number to a C style index.
   */

   ( *recno )--;


   chkout_c ( "ekappr_c" );

} /* End ekappr_c */




