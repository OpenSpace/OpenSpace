/*

-Procedure ekcii_c  ( EK, column info by index )

-Abstract
 
   Return attribute information about a column belonging to a loaded 
   EK table, specifying the column by table and index. 
 
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


   void ekcii_c ( ConstSpiceChar   * table,
                  SpiceInt           cindex,
                  SpiceInt           lenout,
                  SpiceChar        * column,
                  SpiceEKAttDsc    * attdsc  ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   table      I   Name of table containing column. 
   cindex     I   Index of column whose attributes are to be found. 
   lenout     I   Maximum allowed length of column name.
   column     O   Name of column. 
   attdsc     O   Column attribute descriptor. 
 
-Detailed_Input
 
   table          is the name of a loaded EK table.  Case is not 
                  significant. 
 
   cindex         is the index, within TABLE's column attribute 
                  table, of the column whose attributes are to be 
                  found.  The indices of the column table entries 
                  range from 0 to ccount-1, where ccount is the value 
                  returned by the entry point ekccnt_c. 
 
   lenout         is the maximum allowed length of the output column
                  name, including the terminating null.  Column names
                  can be accommodated by a character array of length
                  SPICE_EK_CSTRLN.  This constant is declared in the
                  header file SpiceEK.h.

-Detailed_Output
 
   column         is the name of the specified column. 
 
   attdsc         is an EK column attribute descriptor.  See the header
                  file SpiceEK.h for details.
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)  If the specified table is not loaded, the error 
       SPICE(TABLENOTLOADED) is signaled. 
 
   2)  If the input argument cindex is less than 0 or greater 
       than or equal to the number of columns in table, the error 
       SPICE(INVALIDINDEX) is signaled. 
    
   3) If the output string pointer is null, the error SPICE(NULLPOINTER)
      is signaled.
      
   4) If the output string has length less than two characters, it 
      is too short to contain one character of output data plus a null  
      terminator, so it cannot be passed to the underlying Fortran 
      routine.  In this event, the error SPICE(STRINGTOOSHORT) is
      signaled.
      
   5) If the length of column (indicated by lenout) is at least two
      characters but not large enough to contain the output string,
      the output string will be truncated on the right. 

-Files
 
   The returned column name and descriptor are based on the currently 
   loaded EK files. 
 
-Particulars
 
   This routine is a utility that allows a calling routine to 
   determine the attributes of the currently loaded columns. 
 
-Examples
 
   1)  Dump the names and attributes of the columns in each loaded 
       table.  ekcii_c is used to obtain column names and attributes.


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
 
   -CSPICE Version 1.0.1, 26-MAR-2003 (NJB) 

       Fixed description of exception (5):  replaced "lenout-1"
       with "lenout."  Removed spurious word "clock" from string
       description.

   -CSPICE Version 1.0.0, 10-JAN-2002 (NJB)

-Index_Entries
 
   return information on loaded EK column specified by index 
 
-&
*/

{ /* Begin ekcii_c */

   /*
   Local constants
   */
   #define   CLSIDX        0
   #define   TYPIDX        ( CLSIDX + 1 )
   #define   LENIDX        ( TYPIDX + 1 )
   #define   SIZIDX        ( LENIDX + 1 )
   #define   IXTIDX        ( SIZIDX + 1 )
   #define   NULIDX        ( IXTIDX + 1 )
   #define   DSCSIZ        ( NULIDX + 1 )

   /*
   Local variables
   */
   integer                 fAttDsc [ DSCSIZ ];


   /*
   Participate in error tracing.
   */
   chkin_c ( "ekcii_c" );

   /*
   Make sure the output column has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "ekcii_c", column, lenout );

   /*
   Map the column index to a Fortran-style index. 
   */   
   cindex++;

   /*
   Call the underlying f2c'd routine.  We'll get back individual 
   attributes which we'll use to populate the output attribute
   descriptor. 
   */
   ekcii_ (  ( char    * ) table, 
             ( integer * ) &cindex, 
             ( char    * ) column,
             ( integer * ) fAttDsc,
             ( ftnlen    ) strlen(table),
             ( ftnlen    ) lenout-1       );  

   /*
   Convert the output column name to a C-style string. 
   */
   F2C_ConvertStr ( lenout, column );


   /*
   Fill in the output attribute descriptor. 

   Note that the CSPICE integer codes for data types are one less
   than their corresponding codes in SPICELIB.

   The integer code indicating "variable array size" is the same
   in CSPICE and SPICELIB, so the size attribute may be copied directly
   from the integer array fAttDsc.
   */
   attdsc->cclass  = ( SpiceInt        )   fAttDsc[CLSIDX];
   attdsc->dtype   = ( SpiceEKDataType ) ( fAttDsc[TYPIDX] - 1  );
   attdsc->strlen  = ( SpiceInt        )   fAttDsc[LENIDX];
   attdsc->size    = ( SpiceInt        )   fAttDsc[SIZIDX];
   attdsc->indexd  = ( SpiceBoolean    ) ( fAttDsc[IXTIDX] >= 0 );
   attdsc->nullok  = ( SpiceBoolean    ) ( fAttDsc[NULIDX] >= 0 );


   chkout_c ( "ekcii_c" );

} /* End ekcii_c */
