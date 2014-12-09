/*

-Procedure eknelt_c  ( EK, get number of elements in column entry )

-Abstract
 
   Return the number of elements in a specified column entry in 
   the current row. 
 
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


   SpiceInt eknelt_c ( SpiceInt  selidx,
                       SpiceInt  row     ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   selidx     I   Index of parent column in SELECT clause. 
   row        I   Row containing element. 
   
   The function returns the number of elements in entry in current row. 
 
-Detailed_Input
 
   selidx         is the SELECT clause index of the column to 
                  fetch from.  The range of selidx is 0 : (nsel-1)
                  inclusive, where nsel is the number of items in
                  the SELECT clause of the current query.
 
   row            is the index of the row containing the element. 
                  This number refers to a member of the set of rows 
                  matching a query.  row must be in the range 
 
                    0 : nmrows-1 
 
                  where nmrows is the matching row count returned 
                  by ekfind_c. 
 
-Detailed_Output
 
   The function returns the number of elements in the column entry 
   belonging to the specified column in the current row. 
 
   Null entries in variable-size columns are considered to have size 1. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If this routine is called when no E-kernels have been loaded, 
       the error SPICE(NOLOADEDFILES) is signalled. 
 
   2)  If selidx is outside of the range established by the 
       last query passed to ekfind_c, the error SPICE(INVALIDINDEX) 
       will be signalled. 
 
   3)  If row is outside of the range established by the 
       last query passed to ekfind_c, the error SPICE(INVALIDINDEX) 
       will be signalled. 
 
-Files
 
   At least one E-kernel must be loaded before queries may be passed to
   the EK system via ekfind_c.
 
-Particulars
 
   This routine is meant to be used in conjunction with the EK 
   fetch entry points ekgc_c, ekgd_c, and ekgi_c.  This routine 
   allows the caller of those routines to determine appropriate 
   loop bounds to use to fetch each column entry in the current row. 
 
-Examples
 
   1)  Suppose the EK table TAB contains the following columns: 
 
 
          Column name   Data Type   Size 
          -----------   ---------   ---- 
          IARRAY        INT         10 
          DARRAY        DP          VARIABLE 
          CARRAY        CHR         VARIABLE 
 
 
       Suppose the query 
 
          QUERY = "SELECT IARRAY, DARRAY, CARRAY FROM TAB" 
 
       is issued to ekfind_c via the call 
 
          ekfind_c ( query, MSGLEN, &nmrows, &error, errmsg );
 
       To fetch and dump column values from the rows that satisfy the 
       query, the loop below could be used.  Note that we don't check 
       the FOUND flags returned by the fetch routines since we know 
       in advance how many elements are contained in each column 
       entry we fetch. 
 
             #include <stdio.h>
             #include "SpiceUsr.h"
             
             #define ISIZE  10
                    .
                    .
                    .
             
              for ( row = 0;  row < nmrows;  row++ )
             {
                printf ( "\nROW  =  %d\n\n", row );
 
                /.
                Fetch values from column IARRAY in the current 
                row.  Since IARRAY was the first column selected, 
                the selection index SELIDX is set to 0. 
                ./
                selidx = 0;
                eltidx = 0;
                isnull = SPICEFALSE;
                
                while (  ( eltidx < ISIZE ) && ( !isnull )  )
                {
                    /.
                   If the column entry is null, we'll be kicked 
                   out of this loop after the first iteration. 
                   ./
                   
                   ekgi_c ( selidx,         row,      eltidx, 
                             ivals[eltidx],  &isnull,  &found );

                   eltidx++; 
                }
                
                printf ( "\nCOLUMN  =  IARRAY\n\n" );
 
                if ( isnull )
                {
                    printf ( "<Null>\n" );
                }
                else
                {
                    for ( i = 0;  i < ISIZE;  i++ )
                   {
                       printf ( "%d\n", ivals[i] );
                   }
                }
 
                /.
                Fetch values from column DARRAY in the current 
                row.  Since DARRAY contains variable-size array 
                elements, we call eknelt_c to determine how many 
                elements to fetch. 
                ./
 
                selidx = 1;
                eltidx = 0;
                nelt   = eknelt_c ( selidx, row );
                isnull = SPICEFALSE;
                
                while (  ( eltidx < nelt ) && ( !isnull )  )
                {
                    /.
                   If the column entry is null, we'll be kicked 
                   out of this loop after the first iteration. 
                   ./
                   
                   ekgd_c ( selidx,         row,      eltidx, 
                             dvals[eltidx],  &isnull,  &found   );

                   eltidx++; 
                }
                
                printf ( "\nCOLUMN  =  DARRAY\n\n" );
 
                if ( isnull )
                {
                    printf ( "<Null>\n" );
                }
                else
                {
                    for ( i = 0;  i < nelt;  i++ )
                   {
                       printf ( "%f\n", dvals[i] );
                   }
                }
 
                /.
                Fetch values from column CARRAY in the current row. 
                ./
 
                selidx = 2;
                eltidx = 0;
                nelt   = eknelt_c ( selidx, row );
                isnull = SPICEFALSE;
                
                while (  ( eltidx < nelt ) && ( !isnull )  )
                {
                    /.
                   If the column entry is null, we'll be kicked 
                   out of this loop after the first iteration. 
                   
                   CVLEN is the declared length of the strings in
                   the cvals array.
                   ./
                   
                   ekgc_c ( selidx,        row,     eltidx, CVLEN, 
                             cvals[eltidx], &isnull, &found         );

                   eltidx++; 
                }
 
                printf ( "\nCOLUMN  =  CARRAY\n\n" );
 
                if ( isnull )
                {
                    printf ( "<Null>\n" );
                }
                else
                {
                    for ( i = 0;  i < nelt;  i++ )
                   {
                       printf ( "%s\n", cvals[i] );
                   }
                }
 
             } 
 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.1.0, 23-JUL-2001 (NJB)
     
      Removed tab characters from source file.

   -CSPICE Version 1.0.0, 24-FEB-1999 (NJB)

-Index_Entries
 
   return the number of elements in a column entry 
 
-&
*/

{ /* Begin eknelt_c */



   /*
   Local variables
   */
   SpiceInt                fIndex;
   SpiceInt                fRow;

   SpiceInt                n;



   /*
   Participate in error tracing.
   */

   chkin_c ( "eknelt_c" );

   /*
   Convert the SELECT clause index and row number to Fortran-style.
   */
   
   fIndex = selidx + 1;
   fRow   = row    + 1;
   
   
   /*
   Get the number of elements from the f2c'd routine.
   */

   eknelt_ ( ( integer * ) &fIndex,
             ( integer * ) &fRow,
             ( integer * ) &n      );
             
 
   /*
   Check out before returning the output value.
   */
   chkout_c ( "eknelt_c" );
   

   return ( n );
   

} /* End eknelt_c */

