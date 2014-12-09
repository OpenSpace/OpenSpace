/*

-Procedure ekpsel_c ( EK, parse SELECT clause )

-Abstract
 
   Parse the SELECT clause of an EK query, returning full particulars 
   concerning each selected item. 
 
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
 
   None. 
 
-Keywords
 
   PRIVATE 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void ekpsel_c ( ConstSpiceChar        * query,
                   SpiceInt                msglen,
                   SpiceInt                tablen,
                   SpiceInt                collen,
                   SpiceInt              * n,
                   SpiceInt              * xbegs,
                   SpiceInt              * xends,
                   SpiceEKDataType       * xtypes,
                   SpiceEKExprClass      * xclass,
                   void                  * tabs,
                   void                  * cols,
                   SpiceBoolean          * error,
                   SpiceChar             * errmsg                     ) 
/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   query      I   EK query. 
   msglen     I   Available space in the output error message string.
   n          O   Number of items in SELECT clause of query. 
   xbegs      O   Begin positions of expressions in SELECT clause. 
   xends      O   End positions of expressions in SELECT clause. 
   xtypes     O   Data types of expressions. 
   xclass     O   Classes of expressions. 
   tabs       O   Names of tables qualifying SELECT columns. 
   cols       O   Names of columns in SELECT clause of query. 
   error      O   Error flag. 
   errmsg     O   Parse error message. 
 
-Detailed_Input
 
   query          is a character string containing an EK query. 
                  EK queries have the general form 
 
                     SELECT <select expr>, <select expr>, ... 
                     FROM <table spec>, <table spec>, ... 
                     [WHERE <constraint list>] 
                     [ORDER BY <order-by column list>] 
 
                  Here the symbol <select expr> indicates any 
                  expression representing an entity that can be 
                  selected.  Commonly, the selected items are 
                  columns, with or without qualifying table names, 
                  having the form 
 
                     <column name> 
                     <table name>.<column name> 
                     <table alias>.<column name> 
 
                  but more general expressions may also be selected. 
                  Examples are functions, such as 
 
                     COUNT(*) 
                     COUNT( <table name>.<column name> ) 
                     MAX  ( <table name>.<column name> ) 
 
                  or expressions involving constants, such as 
 
                     2 * <column name> 
 
 
   msglen         The allowed length for the output message string.  
                  This length must large enough to hold the output 
                  string plus the terminator.  If the output string is 
                  expected to have x characters, msglen needs to be 
                  x + 1.
 
   tablen         The length of the strings in the output table array.  
                  This length must large enough to hold the output 
                  strings plus the terminator.  If the output strings 
                  are expected to have x characters, tablen needs to be 
                  x + 1.  The parameter SPICE_EK_TSTRLN defines a string
                  length sufficient to hold any table name.  This 
                  parameter is defined by SpiceUsr.h.
 
   collen         The length of the strings in the output column array.  
                  This length must large enough to hold the output 
                  strings plus the terminator.  If the output strings 
                  are expected to have x characters, collen needs to be 
                  x + 1.  The parameter SPICE_EK_CSTRLN defines a string
                  length sufficient to hold any table name.  This 
                  parameter is defined by SpiceUsr.h.

-Detailed_Output
 
   n              is the number of items specified in the 
                  SELECT clause of the input query. 
 
   xbegs, 
   xends          are, respectively, arrays of begin and end 
                  positions of expressions designating items in the 
                  SELECT clause of the input query.  The ith 
                  expression is located in the substring 
 
                     query[ xbegs[i] ]...query[ xends[i] ] 
 
 
   xtypes         is an array of values of type SpiceEKDataType giving 
                  types of the expressions in the SELECT clause. 
                  Values and meanings of xtypes are: 
 
                     SPICE_CHR     Character type 
                     SPICE_DP      Double precision type 
                     SPICE_INT     Integer type 
                     SPICE_TIME    Time type 
 
                  The ith element of xtypes refers to the ith 
                  selected item. 
 
                  The data type of an expression indicates which 
                  fetch routine to use to obtain values of the 
                  selected expression.  The mapping of data types 
                  to fetch routines is shown below: 
 
                     SPICE_CHR      ekgc_c
                     SPICE_DP       ekgd_c
                     SPICE_INT      ekgi_c
                     SPICE_TIME     ekgd_c
 
                  Note that time values are stored as d.p. numbers. 
 
 
   xclass         is an array of values of type SpiceEKExprClass giving
                  the classes of the expressions occurring in the SELECT
                  clause of the input query.  Values and meanings of 
                  xclass are: 
 
                     SPICE_EK_EXP_COL     Selected item was a column. 
                                          The column may qualified by a
                                          table name. 
 
                     SPICE_EK_EXP_FUNC    Selected item was a simple 
                                          function invocation of the 
                                          form 
 
                                             F ( <column> ) 
 
                                          or else was 
 
                                             COUNT(*) 
 
                     SPICE_EK_EXP_EXPR    Selected item was a more 
                                          general expression than those 
                                          shown above. 
 
                  The Ith element of xclass refers to the Ith 
                  selected item. 
 
                  When a selected item is a column, the values of 
                  the arguments tabs and cols (discussed below) are 
                  defined. 
 
 
   tabs           is an array of names of tables corresponding to 
                  the columns in the SELECT clause.  The ith element 
                  of tabs corresponds to the table containing the 
                  ith SELECT column.  Table names returned in tabs 
                  are the actual names of tables in loaded EKs, not 
                  aliases supplied in the input query.  Table names 
                  are supplied even if the corresponding column was 
                  unqualified in the input query, as long as the 
                  column name was unambiguous. 
 
                  The contents of tabs[i] are defined if and only if 
                  the returned value of xclass[i] is SPICE_EK_EXP_COL. 
 
 
   cols           is an array containing the columns of the SELECT 
                  clause.  The contents of cols[i] are defined if and 
                  only if the returned value of xclass[i] is 
                  SPICE_EK_EXP_COL. 
 
 
   error          is a logical flag indicating whether the input 
                  query parsed correctly.  The other outputs of this 
                  routine, except for errmsg, are undefined if a 
                  parse error occurred.  error is returned SPICETRUE if 
                  a parse error occurred, SPICEFALSE otherwise. 
 
   errmsg         is a character string describing the cause of a 
                  parse error, if such an error occurred.  Otherwise, 
                  errmsg is returned empty. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  Parse failures do not cause this routine to signal errors; 
       instead, the error and errmsg outputs indicate invalid 
       QUERIES. 
 
   2)  Queries cannot be parsed correctly unless at least one EK 
       is loaded. 
 
-Files
 
   None. 
 
-Particulars
 
   This routine allows callers of the EK fetch routines to determine 
   at run time the attributes of the columns from which data is to be 
   fetched. 
 
-Examples
 
   1)  Use of ekpsel_c to assist in fetching rows matching queries 
       supplied at run time. 
 
       The code fragment shown here does not rely on advance 
       knowledge of the input query or the contents of any loaded EK 
       files. 
 
       To simplify the example, we assume that all columns are scalar-
       valued. 
 
 
          #include "SpiceUsr.h"
          #include <stdio.h>
          #include <string.h>
       
       
          void main()
       {
          /.
          The kernel names that appear here are examples; to use this
          program, you would have to replace these names with those of
          kernels available on your own system.
          ./
          #define EK              "/kernels/galileo/ek/EK97148A.BSE"
          #define LSK             "/kernels/gen/lsk/leapseconds.ker"
          #define MSGLEN          320
          #define LNSIZE          80
          #define TIMELEN         25
          
          SpiceBoolean            error;
          SpiceBoolean            found;
          SpiceBoolean            null;
       
          SpiceChar               cdata  [LNSIZE];
          SpiceChar               cols   [SPICE_EK_MAXQSEL]
                                         [SPICE_EK_CSTRLN];
          SpiceChar               errmsg [MSGLEN];
          SpiceChar               outstr [LNSIZE];
          SpiceChar             * query;
          SpiceChar               tabs   [SPICE_EK_MAXQSEL]
                                         [SPICE_EK_TSTRLN];
          SpiceChar               utc    [TIMELEN];
       
          SpiceDouble             ddata;
          SpiceDouble             tdata;
       
          SpiceEKDataType         xtypes [SPICE_EK_MAXQSEL];
          SpiceEKExprClass        xclass [SPICE_EK_MAXQSEL];
          
          SpiceInt                col;
          SpiceInt                exprlen;
          SpiceInt                handle;
          SpiceInt                idata;
          SpiceInt                n;
          SpiceInt                nmrows;
          SpiceInt                row;
       
          SpiceInt                xbegs  [SPICE_EK_MAXQSEL];
          SpiceInt                xends  [SPICE_EK_MAXQSEL];
       
       
       
          /.
          Load leapseconds and E-kernels.
          ./
          furnsh_c ( LSK ); 
          eklef_c  ( EK, &handle );
       
       
          while ( SPICETRUE )
          {
          
             /.
             Prompt for query.  Parse the SELECT clause using ekpsel_c. 
             ./
             query = prompt_c ( "Enter query > " );
           
             ekpsel_c ( query, 
                        MSGLEN,
                        &n, 
                        xbegs, 
                        xends, 
                        xtypes, 
                        xclass, 
                        tabs, 
                        cols, 
                        &error, 
                        errmsg );
           
             if ( error ) 
             {
                printf ( "Error: <%s>\n", errmsg );
             }
             
             else
             {
                /.
                Submit query to the EK query system.
                ./
                
                ekfind_c ( query, MSGLEN, &nmrows, &error, errmsg );
           
                if ( error )  
                {
                   printf ( "Error found: %s\n", errmsg );
                }
                
                else
                { 
                   printf ( "Number of matching rows = %d\n", nmrows );
                
                   /.
                   Fetch the rows that matched the query. 
                   ./
                   
                   for ( row = 0;  row < nmrows;  row++ )
                   {
                      /.
                      Fetch data from the current row. 
                      ./
                      
                      printf ( "\nROW = %d\n", row );
           
           
                      for ( col = 0;  col < n;  col++ )
                      {
                         /.            
                         Fetch data from the current selected column. 
                         ./
                         
                         if ( xclass[col] == SPICE_EK_EXP_COL )
                         {
                             printf ( "COLUMN = %s.%s\n", 
                                       tabs[col], 
                                       cols[col] );                         
                         }
                         else
                         { 
                            exprlen = xends[col] - xbegs[col] + 1;
                            
                            strncpy ( outstr, query+xbegs[col], 
                                      exprlen                    );
                            
                            outstr[exprlen] = (char)0;
                            
                            printf ( "%s\n", outstr );                          
                         } 
           
           
                         /.
                         Write out the data.
                         ./
                         
                         switch ( xtypes[col] )
                         {
                            case SPICE_CHR:
                             
                               ekgc_c ( col,   row,   0,     LNSIZE,
                                        cdata, &null, &found         );
                                        
                               if ( !null ) 
                               {
                                  printf ( "%s\n", cdata );
                               }
                              
                               break;
                               
           
                            case SPICE_DP:
                             
                               ekgd_c ( col,    row,   0, 
                                        &ddata, &null, &found );
                                        
                               if ( !null ) 
                               {
                                  printf ( "%f\n", ddata );
                               }
                              
                               break;
                               
           
                            case SPICE_INT:
                             
                               ekgi_c ( col,    row,   0, 
                                        &idata, &null, &found );
                                        
                               if ( !null ) 
                               {
                                  printf ( "%d\n", cdata );
                               }
                              
                               break;
                               
           
                            case SPICE_TIME:
                             
                               /.
                               The item is a time value.  Convert it 
                               to UTC for output. 
                               ./
                               
                               ekgd_c ( col,    row,   0, 
                                        &tdata, &null, &found );
                                        
                               if ( !null ) 
                               {
                                  et2utc_c ( tdata,   "C", 3, 
                                             TIMELEN, utc    );
                                   
                                  printf ( "%s\n", utc );
                               }
                              
                               break;
                               
           
                            default:
                            
                               ;
                         }
           
                         /.
                         Handle null values here.
                         ./
                         
                         if ( null ) 
                         { 
                            printf ( "%s\n", "<Null>" );
                         } 
                         
                         /.
                         End of data type switch.
                         ./ 
                         
                      } 
                      /.
                      We're done with the column having index col. 
                      ./
                   }
                   /.
                   We're done with the row having index row. 
                   ./
                }
                /.
                We either processed the query or ekfind_c detected an 
                error. 
                ./
             }
             /.
             We either parsed the SELECT clause or ekpsel_c detected an
             error. 
             ./
             
          }
          
       }
       
          

 
-Restrictions
 
   1)  Currently, column names are the only supported expressions. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman       (JPL) 
 
-Version

   -CSPICE Version 2.1.1, 14-AUG-2006   (EDW)

      Replace mention of ldpool_c with furnsh_c.

   -CSPICE Version 2.1.0, 02-SEP-1999 (NJB)  
   
      Local type logical variable now used for error flag used in
      interface of ekpsel_.
            
   -CSPICE Version 2.0.0, 19-JUL-1999 (NJB)

      The data types of the tabs and cols arguments were changed
      to (void *), and associated string length arguments were added.
      This style of interface for string arrays is now standard within
      CSPICE.
      
      Some corrections of the header comments were made.

   -CSPICE Version 1.0.0, 21-FEB-1999 (NJB)

-Index_Entries
 
   parse select clause of EK query 
 
-&
*/

{ /* Begin ekpsel_c */


   /*
   Local constants
   
   
   XCLASSLEN is the maximum length of a short string indicating the 
   class of a SELECT clause item in a QUERY.  The set of expected 
   strings is defined by the Fortran SPICELIB routine EKPSEL.  The
   current set of strings is {"COL", "FUNC", "EXPR"}.
   */
   #define XCLASSLEN       4
   
   
   /*
   TYPSIZ is the string length associated with the array locXtypes.
   */
   #define TYPSIZ          ( SPICE_EK_TYPLEN + 1 )
   
   
   /*
   EXPSIZ is the string length associated with the array locXclass.
   */
   #define EXPSIZ          ( XCLASSLEN + 1 )
   
   
   /*
   Local variables
   */
   logical                 err;
   
   SpiceChar               locXtypes[SPICE_EK_MXCLSG][TYPSIZ];
   SpiceChar               locXclass[SPICE_EK_MXCLSG][EXPSIZ];
   SpiceChar             * strptr;

   SpiceInt                i;
   SpiceInt                lastnb;



   /*
   Participate in error tracing.
   */

   chkin_c ( "ekpsel_c" );

   /*
   Check the input query string to make sure the pointer is non-null and 
   the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekpsel_c", query );
   

   /*
   Make sure the output error message string has at least enough room 
   for one output character and a null terminator.  Also check for a 
   null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "ekpsel_c", errmsg, msglen );
      
   
   /*
   Call the f2c'd function.
   */
   ekpsel_ ( ( char    * ) query,
             ( integer * ) n,
             ( integer * ) xbegs,
             ( integer * ) xends,
             ( char    * ) locXtypes,
             ( char    * ) locXclass,
             ( char    * ) tabs,
             ( char    * ) cols,
             ( logical * ) &err,
             ( char    * ) errmsg,
             ( ftnlen    ) strlen(query),
             ( ftnlen    ) SPICE_EK_TYPLEN,
             ( ftnlen    ) XCLASSLEN,
             ( ftnlen    ) tablen-1,
             ( ftnlen    ) collen-1,
             ( ftnlen    ) msglen-1          );
             

   /*
   Assign the SpiceBoolean error flag.
   */
   
   *error = err;
   

   if ( failed_c() )
   {
      chkout_c ( "ekpsel_c" );
      return;
   }


   /*
   Convert the error message to a C style string.
   */
   F2C_ConvertStr ( msglen, errmsg );


   /*
   If there was a parse error, the other outputs are undefined.
   */
   if ( *error ) 
   {
      chkout_c ( "ekpsel_c" );
      return;
   }


   /*
   Map the token begin and end indices from Fortran to C style.
   */
   for ( i = 0;  i < *n;  i++ )
   {
      xbegs[i]--;
      xends[i]--;
   }
   
   
   /*
   Map the expression data types from strings to SpiceEKDataType values.  
   First, map the Fortran-style strings returned by ekpsel_ to C
   style strings.
   */
   F2C_ConvertStrArr ( *n, TYPSIZ, (SpiceChar *)locXtypes );
   
   
   for ( i = 0;  i < *n;  i++ )
   {
      if (  eqstr_c( locXtypes[i], "CHR" )  )
      {
         xtypes[i] = SPICE_CHR;
      }

      else if (  eqstr_c( locXtypes[i], "DP" )  )
      {
         xtypes[i] = SPICE_DP;
      }

      else if (  eqstr_c( locXtypes[i], "INT" )  )
      {
         xtypes[i] = SPICE_INT;
      }

      else if (  eqstr_c( locXtypes[i], "TIME" )  )
      {
         xtypes[i] = SPICE_TIME;
      }
      
      else
      {
         setmsg_c ( "Unrecognized data type string <#> returned "
                    "by ekpsel_ for item #."                     );
         errch_c  ( "#",  locXtypes[i]                           );
         errint_c ( "#",  i                                      );
         sigerr_c ( "SPICE(BUG)"                                 );
         chkout_c ( "ekpsel_c"                                   );
         return;
      }
   }
   
   /*
   Map the expression classes from strings to SpiceEKExprClass values.  
   First, map the Fortran-style strings returned by ekpsel_ to C
   style strings.
   */
   F2C_ConvertStrArr ( *n, EXPSIZ, (SpiceChar *)locXclass );
   
   for ( i = 0;  i < *n;  i++ )
   {
      if (  eqstr_c( locXclass[i], "COL" )  )
      {
         xclass[i] = SPICE_EK_EXP_COL;
      }

      else if (  eqstr_c( locXclass[i], "FUNC" )  )
      {
         xclass[i] = SPICE_EK_EXP_FUNC;
      }

      else if (  eqstr_c( locXclass[i], "EXPR" )  )
      {
         xclass[i] = SPICE_EK_EXP_EXPR;
      }

      else
      {
         setmsg_c ( "Unrecognized item class string <#> returned "
                    "by ekpsel_ for item #."                     );
         errch_c  ( "#",  locXclass[i]                           );
         errint_c ( "#",  i                                      );
         sigerr_c ( "SPICE(BUG)"                                 );
         chkout_c ( "ekpsel_c"                                   );
         return;
      }
   }
   

   /*
   Convert the array of table names to a C style array of strings.  
   Null-terminate each string so as to eliminate trailing blanks.
   */
   F2C_ConvertStrArr ( *n, tablen, (SpiceChar *)tabs );

   for ( i = 0;  i < *n;  i++ )
   {
      strptr = ((SpiceChar *)tabs) + i*tablen;
      
      lastnb = F_StrLen ( tablen-1, strptr );
      
      *( strptr + lastnb ) = (char)0;
   }

   /*
   Convert the array of column names to a C style array of strings.
   Null-terminate each string so as to eliminate trailing blanks.
   */
   F2C_ConvertStrArr ( *n, collen, (SpiceChar *)cols );

   for ( i = 0;  i < *n;  i++ )
   {
      strptr = ((SpiceChar *)cols) + i*collen;
      
      lastnb = F_StrLen ( collen-1, strptr );
      
      *( strptr + lastnb ) = (char)0;
   }

   
   chkout_c ( "ekpsel_c" );

} /* End ekpsel_c */
