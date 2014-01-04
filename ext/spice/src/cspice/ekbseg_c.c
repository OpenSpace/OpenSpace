/*

-Procedure ekbseg_c ( EK, start new segment )

-Abstract
 
   Start a new segment in an E-kernel. 
 
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
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #include "SpiceZim.h"
   #undef   ekbseg_c

   void ekbseg_c ( SpiceInt           handle,
                   ConstSpiceChar   * tabnam,
                   SpiceInt           ncols,
                   SpiceInt           cnmlen,
                   const void       * cnames,
                   SpiceInt           declen,
                   const void       * decls,
                   SpiceInt         * segno  ) 
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   handle     I   File handle. 
   tabnam     I   Table name. 
   ncols      I   Number of columns in the segment. 
   cnmlen     I   Length of names in in column name array.
   cnames     I   Names of columns. 
   declen     I   Length of declaration strings in declaration array.
   decls      I   Declarations of columns. 
   segno      O   Segment number. 
  
-Detailed_Input
 
   handle         the handle of an EK file that is open for writing. 
 
   tabnam         is the name of the EK table to which the current 
                  segment belongs.  All segments in the EK file 
                  designated by handle must have identical column 
                  attributes. tabnam must not exceed SPICE_EK_TNAMSZ
                  characters (see SpiceEK.h) in length.  Case is not 
                  significant. Table names must start with a letter and
                  contain only characters from the set
                  {A-Z,a-z,0-9,$,_}. 
 
   ncols          is the number of columns in a new segment. 

   cnmlen,
   cnames         are, respectively, the length of the column name
                  strings in the column name array, and the base
                  address of the array itself.  The array should have
                  dimensions
                  
                     [ncols][cnmlen]
                     
   declen,
   decls          are, respectively, the length of the declaration
                  strings in the declaration array, and the base 
                  address of the array itself.  The array should have
                  dimensions
                  
                     [ncols][declen]
                      
                  The Ith element of cnames and the Ith element of decls
                  apply to the Ith column in the segment. 
 
                  Column names must not exceed CSPICE_EK_CNAMSZ
                  characters (see SpiceEK.h) in length.  Case is not
                  significant.  Column names must start with a letter 
                  and contain only characters from the set 
                  {A-Z,a-z,0-9,$,_}. 
 
                  The declarations are strings that contain 
                  "keyword=value" assignments that define the 
                  attributes of the columns to which they apply.  The 
                  column attributes that are defined by a column 
                  declaration are: 
 
                     DATATYPE 
                     SIZE 
                     <is the column indexed?> 
                     <does the column allow null values?> 
 
                  The form of a declaration is 
 
                     "DATATYPE  = <type>, 
                      SIZE      = <size>, 
                      INDEXED   = <boolean>, 
                      NULLS_OK  = <boolean>" 
 
                  For example, an indexed, scalar, integer column 
                  that allows null values would have the declaration 
 
                     "DATATYPE  = INTEGER, 
                      SIZE      = 1, 
                      INDEXED   = TRUE, 
                      NULLS_OK  = TRUE" 
 
                  Commas are required to separate the assignments 
                  within declarations; white space is optional; 
                  case is not significant. 
 
                  The order in which the attribute keywords are 
                  listed in declaration is not significant. 
 
                  Every column in a segment must be declared. 
 
                  Each column entry is effectively an array, each 
                  element of which has the declared data type.  The 
                  SIZE keyword indicates how many elements are in 
                  each entry of the column in whose declaration the 
                  keyword appears.  Note that only scalar-valued 
                  columns (those for which SIZE = 1) may be 
                  referenced in query constraints.  A size 
                  assignment has the syntax 
 
                     SIZE = <integer> 
 
                  or 
                     SIZE = VARIABLE 
 
                  The size value defaults to 1 if omitted. 
 
                  The DATATYPE keyword defines the data type of 
                  column entries.  The DATATYPE assignment syntax 
                  has any of the forms 
 
                     DATATYPE = CHARACTER*(<length>) 
                     DATATYPE = CHARACTER*(*) 
                     DATATYPE = DOUBLE PRECISION 
                     DATATYPE = INTEGER 
                     DATATYPE = TIME 
 
                  As the datatype declaration syntax suggests, 
                  character strings may have fixed or variable 
                  length.  Variable-length strings are allowed only 
                  in columns of size 1. 
 
                  Optionally, scalar-valued columns may be indexed. 
                  To create an index for a column, use the assignment 
 
                     INDEXED = TRUE 
 
                  By default, columns are not indexed. 
 
                  Optionally, any column can allow null values.  To 
                  indicate that a column may allow null values, use 
                  the assigment 
 
                     NULLS_OK = TRUE 
 
                  in the column declaration.  By default, null 
                  values are not allowed in column entries. 

  


-Detailed_Output
 
   segno          is the number of the segment to which data is to be
                  added. Segments are numbered from 0 to nseg-1, where
                  nseg is the count of segments in the file.  Segment
                  numbers are used as unique identifiers by other EK
                  access routines.
  
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If handle is invalid, the error will be diagnosed by routines 
       called by this routine. 
 
   2)  If tabnam is more than SPICE_EK_TNAMSZ characters long, the 
       error is diagnosed by routines called by this routine. 
 
   3)  If tabnam contains any nonprintable characters, the error 
       is diagnosed by routines called by this routine. 
 
   4)  If ncols is non-positive or greater than the maximum allowed 
       number SPICE_EK_MXCLSG, the error SPICE(INVALIDCOUNT) is 
       signaled. 
 
   5)  If any column name exceeds SPICE_EK_CNAMSZ characters in 
       length, the error is diagnosed by routines called by this 
       routine. 
 
   6)  If any column name contains non-printable characters, the 
       error is diagnosed by routines called by this routine. 
 
   7)  If a declaration cannot be understood by this routine, the 
       error is diagnosed by routines called by this routine. 
 
   8)  If an non-positive string length or element size is specified, 
       the error is diagnosed by routines called by this routine. 
 
   9)  If an I/O error occurs while reading or writing the indicated 
       file, the error will be diagnosed by routines called by this 
       routine. 

   10) If the input string pointer for the table name is null, the 
       error SPICE(NULLPOINTER) will be signaled.
      
   12) If the input tablen name string has length zero, the error 
       SPICE(EMPTYSTRING) will be signaled.
 
   13) If the string pointer for cnames is null, the error
       SPICE(NULLPOINTER) will be signaled.
   
   14) If the string length cnmlen is less than 2, the error 
       SPICE(STRINGTOOSHORT) will be signaled.

   15) If the string pointer for decls is null, the error
       SPICE(NULLPOINTER) will be signaled.
   
   16) If the string length declen is less than 2, the error 
       SPICE(STRINGTOOSHORT) will be signaled.
   
-Files
 
   See the EK Required Reading for a discussion of the EK file 
   format. 
 
-Particulars
 
   This routine operates by side effects:  it prepares an EK for 
   the addition of a new segment.  It is not necessary to take 
   any special action to `complete' a segment; segments are readable 
   after the completion of any record insertion, deletion, write, 
   or update operation. 
 
-Examples
 
   1)  Suppose we have an E-kernel named ORDER_DB.EK which contains 
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
 
 
       We'll suppose that the file ORDER_DB.EK contains two segments, 
       the first containing the DATAORDERS table and the second 
       containing the DATAITEMS table. 
 
       Below, we show how we'd open a new EK file and start the 
       first of the segments described above. 
 
       
       #include "SpiceUsr.h"
       #include <stdio.h>
       
       
       void main()
       {
          /.
          Constants
          ./
          #define  CNMLEN        SPICE_EK_CSTRLN
          #define  DECLEN        201
          #define  EKNAME        "order_db.ek"
          #define  FNMLEN        50
          #define  IFNAME        "Test EK/Created 20-SEP-1995"
          #define  LNMLEN        50
          #define  LSK           "leapseconds.ker"
          #define  NCOLS         6
          #define  NRESVC        0
          #define  TABLE         "DATAORDERS"
          #define  TNMLEN        CSPICE_EK_TAB_NAM_LEN
          #define  UTCLEN        30
          
          
          /.
          Local variables
          ./
          SpiceBoolean            nlflgs [ NROWS  ];
       
          SpiceChar               cdecls  [ NCOLS ] [ DECLEN ];
          SpiceChar               cnames  [ NCOLS ] [ CNMLEN ];
          SpiceChar               fnames  [ NROWS ] [ FNMLEN ];
          SpiceChar               lnames  [ NROWS ] [ LNMLEN ];
          SpiceChar               dateStr [ UTCLEN ];
        
          SpiceDouble             costs  [ NROWS ];
          SpiceDouble             ets    [ NROWS ];
       
          SpiceInt                cstids [ NROWS ];
          SpiceInt                ordids [ NROWS ];
          SpiceInt                handle;
          SpiceInt                i;
          SpiceInt                segno;
          SpiceInt                sizes  [ NROWS ];
          
          
          /.
          Load a leapseconds kernel for UTC/ET conversion.
          ./
          furnsh_c ( LSK );
          
          /.
          Open a new EK file.  For simplicity, we will not 
          reserve any space for the comment area, so the 
          number of reserved comment characters is zero. 
          The constant IFNAME is the internal file name. 
          ./
          ekopn_c ( EKNAME, IFNAME, NRESVC, &handle );
       
          /.
          Set up the table and column names and declarations 
          for the DATAORDERS segment.  We'll index all of 
          the columns.  All columns are scalar, so we omit 
          the size declaration.  Only the COST column may take 
          null values. 
          ./
          strcpy ( cnames[0], "ORDER_ID"                           );
          strcpy ( cdecls[0], "DATATYPE = INTEGER, INDEXED = TRUE" );
       
          strcpy ( cnames[1], "CUSTOMER_ID"                        );
          strcpy ( cdecls[1], "DATATYPE = INTEGER, INDEXED = TRUE" );
       
          strcpy ( cnames[2], "LAST_NAME"                          ); 
          strcpy ( cdecls[2], "DATATYPE = CHARACTER*(*),"
                              "INDEXED  = TRUE"                    );
       
          strcpy ( cnames[3], "FIRST_NAME"                         );
          strcpy ( cdecls[3], "DATATYPE = CHARACTER*(*),"   
                              "INDEXED  = TRUE"                    );
       
          strcpy ( cnames[4], "ORDER_DATE"                         );
          strcpy ( cdecls[4], "DATATYPE = TIME, INDEXED  = TRUE"   );
       
          strcpy ( cnames[5], "COST"                               );
          strcpy ( cdecls[5], "DATATYPE = DOUBLE PRECISION,"   
                              "INDEXED  = TRUE,"             
                              "NULLS_OK = TRUE"                    );
          /.
          Start the segment. 
          ./
          ekbseg_c ( handle,  TABLE,   NCOLS,   CNMLEN,  
                     cnames,  DECLEN,  cdecls,  &segno  );

          /. 
          Add data to the segment.  No special action 
          is required to finish the segment. 
          ./
             [Data are added via calls to ekappr_c and the 
              ekacec_c, ekaced_c, and ekacei_c routines.  See any 
              of these routines for examples.] 
 
          /.
          At this point, the second segment could be 
          created by an analogous process.  In fact, the 
          second segment could be created at any time; it is 
          not necessary to populate the first segment with 
          data before starting the second segment. 
          ./ 

 
          /. 
          The file must be closed by a call to ekcls_c. 
          ./
          ekcls_c ( handle ); 
       }
  
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.1.0, 12-JUL-2002 (NJB)

      Call to C2F_CreateStrArr_Sig replaced with call to C2F_MapStrArr.

   -CSPICE Version 1.0.0, 17-NOV-2001 (NJB)

-Index_Entries
 
   start new E-kernel segment 
   start new EK segment 
 
-&
*/

{ /* Begin ekbseg_c */



   /*
   Local variables
   */
   SpiceChar             * fCnameArr;
   SpiceChar             * fCdeclArr;

   SpiceInt                fCnameLen;
   SpiceInt                fCdeclLen;

   /*
   Participate in error tracing.
   */
   chkin_c ( "ekbseg_c" );

   /*
   Check the table name to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekbseg_c", tabnam );

   /*
   Check the column name array to make sure the pointer is non-null 
   and the string length is non-zero.  Note:  this check is normally
   done for output strings:  CHKOSTR is the macro that does the job.
   */
   CHKOSTR ( CHK_STANDARD, "ekbseg_c", cnames, cnmlen );

   /*
   Check the declaration array to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKOSTR ( CHK_STANDARD, "ekbseg_c", decls, declen );

   C2F_MapStrArr ( "ekbseg_c", 
                   ncols, cnmlen, cnames, &fCnameLen, &fCnameArr );
   
   if ( failed_c() )
   {
      chkout_c ( "ekbseg_c" );
      return;
   }


   C2F_MapStrArr ( "ekbseg_c", 
                   ncols, declen, decls, &fCdeclLen, &fCdeclArr );
   
   if ( failed_c() )
   {
      free ( fCnameArr );
      
      chkout_c ( "ekbseg_c" );
      return;
   }
   

   /*
   Call the f2c'd Fortran routine.  Use explicit type casts for every
   type defined by f2c.
   */
   ekbseg_ ( ( integer  * ) &handle,
             ( char     * ) tabnam,
             ( integer  * ) &ncols,
             ( char     * ) fCnameArr,
             ( char     * ) fCdeclArr,
             ( integer  * ) segno,
             ( ftnlen     ) strlen(tabnam),
             ( ftnlen     ) fCnameLen,
             ( ftnlen     ) fCdeclLen       );

   /*
   Clean up all of our dynamically allocated arrays.
   */
   free ( fCnameArr );
   free ( fCdeclArr );

   /*
   Map segno to C style range.
   */
   
   (*segno)--;
   
   
   chkout_c ( "ekbseg_c" );

} /* End ekbseg_c */


