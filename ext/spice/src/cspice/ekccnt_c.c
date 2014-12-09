/*

-Procedure ekccnt_c  ( EK, column count )

-Abstract
 
   Return the number of distinct columns in a specified, currently 
   loaded table 
 
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

   void ekccnt_c ( ConstSpiceChar  * table,
                   SpiceInt        * ccount ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   table      I   Name of table. 
   ccount     O   Count of distinct, currently loaded columns. 
 
-Detailed_Input
 
   table          is the name of a currently loaded table.  Case 
                  is not significant in the table name. 
 
-Detailed_Output
 
   ccount         is the number of distinct columns in table. 
                  Columns that have the same name but belong to 
                  different segments that are considered to be 
                  portions of the same column, if the segments 
                  containing those columns belong to table. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the specified table is not loaded, the error 
      SPICE(TABLENOTLOADED) is signaled. 
  
   2) If the input string pointer is null, the error
      SPICE(NULLPOINTER) will be signaled.

   3) If the input string has length zero, the error
      SPICE(EMPTYSTRING) will be signaled.

-Files
 
   This routine reads binary "sequence component" EK files.
   In order for a binary EK file to be accessible to this routine,
   the file must be loaded via a call to furnsh_c or the low-level
   EK loader eklef_c.
 
-Particulars
 
   This routine is a utility intended for use in conjunction with 
   the entry point ekcii_c.  These routines can be used to find the 
   names and attributes of the columns that are currently loaded. 
 
-Examples
 
   1)  Dump the names and attributes of the columns in each loaded 
       table.  ekccnt_c is used to obtain column counts. 


          #include "SpiceUsr.h"
          #include "SpiceEK.h"

          #define FILEN           256

          SpiceChar               colnam  [ SPICE_EK_CSTRLN ];
          SpiceChar               ek      [ FILEN ];
          SpiceChar               tabnam  [ SPICE_EK_TSTRLN ];

          SpiceChar             * typstrs [ 4 ] =
                                  {
                                     "CHR", "DP", "INT", "TIME" 
                                  };

          SpiceEKAttDsc           attdsc;

          SpiceInt                i;
          SpiceInt                ncols;
          SpiceInt                ntab;
          SpiceInt                tab;


          prompt_c ( "Enter name of EK to examine > ", FILEN, ek );

          furnsh_c ( ek );

          /. 
          Get the number of loaded tables. 
          ./
          ekntab_c ( &ntab );

          for ( tab = 0;  tab < ntab;  tab++ )
          {
             /.
             Get the name of the current table, and look up 
             the column count for this table. 
             ./
             ektnam_c ( tab, SPICE_EK_TSTRLN, tabnam );

             ekccnt_c ( tabnam, &ncols );

             printf ( "Table = %s\n\n", tabnam );
  

             /.
             For each column in the current table, look up the 
             column's attributes.  The attribute block 
             index parameters are defined in the include file 
             ekattdsc.inc. 
             ./

             for ( i = 0;  i < ncols;  i++ )
             {
                ekcii_c ( tabnam, i, SPICE_EK_CSTRLN, colnam, &attdsc );
 
                printf ( "Column = %s\n", colnam );

 
                /.
                Write out the current column's data type. 
                ./

                printf ( "Type = %s\n", typstrs[(int)attdsc.dtype] );

                if ( attdsc.dtype == SPICE_CHR )
                {
                   if ( attdsc.strlen == SPICE_EK_VARSIZ )
                   {
                      printf ( "String length = VARIABLE\n" );
                   }
                   else
                   {
                       printf ( "String length = %ld\n", 
                               (SpiceInt) attdsc.strlen );
                   }
                }

                /.
                Write out the current column's entry size. 
                ./                
                printf ( "Size = %ld\n", attdsc.size );
 

                /.
                Indicate whether the current column is indexed. 
                ./
                if ( attdsc.indexd == SPICETRUE )
                {
                   printf ( "Indexed.\n" );
                }
                else
                {
                   printf ( "Not indexed.\n" );
                }
 
                /.
                Indicate whether the current column allows 
                null values. 
                ./
                if ( attdsc.nullok == SPICETRUE )
                {
                   printf ( "Null values allowed.\n" );
                }
                else
                {
                   printf ( "Null values not allowed.\n" );
                }
             }
             /.
             We're done with the current column.
             ./
          }
          /.
          We're done with the current table.
          ./

 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 14-OCT-2001 (NJB)

-Index_Entries
 
   return the number of loaded EK columns 
   return the count of loaded EK columns 
 
-&
*/

{ /* Begin ekccnt_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "ekccnt_c" );


   /*
   Check the input string to make sure the pointer
   is non-null and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ekccnt_c", table );

   ekccnt_ (  ( char    * ) table,
              ( integer * ) ccount,
              ( ftnlen    ) strlen(table)  );


   chkout_c ( "ekccnt_c" );

} /* End ekccnt_c */
