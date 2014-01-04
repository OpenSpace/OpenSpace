/*

-Procedure diff_c ( Difference of two sets )

-Abstract
 
   Take the difference of two sets of any data type to form a third
   set.
 
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
 
   SETS 
 
-Keywords
 
   CELLS, SETS 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void diff_c (  SpiceCell   * a,
                  SpiceCell   * b,
                  SpiceCell   * c  ) 

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   a          I   First input set. 
   b          I   Second input set. 
   c          O   Difference of a and b. 
 
-Detailed_Input
 
   a           is a CSPICE set.  a must be declared as a SpiceCell 
               of data type character, double precision, or integer.

   b           is a CSPICE set, distinct from a.  b must have the 
               same data type as a.
 
-Detailed_Output
 
   c           is a CSPICE set, distinct from sets a and b, which 
               contains the difference of a and b (that is, all of 
               the elements which are in a but NOT in b).  c must 
               have the same data type as a and b.

               When comparing elements of character sets, this routine
               ignores trailing blanks.  Trailing blanks will be 
               trimmed from the members of the output set c.

-Parameters
 
   None. 
 
-Exceptions

   1) If the input set arguments don't have identical data types,
      the error SPICE(TYPEMISMATCH) is signaled.

   2) If the difference of the two sets contains more elements than can
       be contained in the output set, the error SPICE(SETEXCESS) is
       signaled.

   3) If the set arguments have character type and the length of the 
      elements of the output set is less than the maximum of the 
      lengths of the elements of the input sets, the error 
      SPICE(ELEMENTSTOOSHORT) is signaled. 

   4) If either of the input arguments may be unordered or contain 
      duplicates, the error SPICE(NOTASET) is signaled.
 
-Files
 
   None. 

-Particulars

   This is a generic CSPICE set routine; it operates on sets of any
   supported data type.
 
   The difference of two sets contains every element which is 
   in the first set, but NOT in the second. 

         {a,b}      difference  {c,d}     =  {a,b} 
         {a,b,c}                {b,c,d}      {a} 
         {a,b,c,d}              {}           {a,b,c,d} 
         {}                     {a,b,c,d}    {} 
         {}                     {}           {} 

 
-Examples
 
   1) The following code fragment places the difference of the
      character sets planets and asteroids into the character set
      result.


         #include "SpiceUsr.h"
                .
                .
                .
         /.
         Declare the sets with string length NAMLEN and with maximum
         number of elements MAXSIZ.
         ./
         SPICECHAR_CELL ( planets,   MAXSIZ, NAMLEN );
         SPICECHAR_CELL ( asteroids, MAXSIZ, NAMLEN );
         SPICECHAR_CELL ( result,    MAXSIZ, NAMLEN );
                .
                .
                .
         /.
         Compute the difference.
         ./
         diff_c ( &planets, &asteroids, &result );


   2) Repeat example #1, this time using integer sets containing
      ID codes of the bodies of interest.


         #include "SpiceUsr.h"
                .
                .
                .
         /.
         Declare the sets with maximum number of elements MAXSIZ.
         ./
         SPICEINT_CELL ( planets,   MAXSIZ );
         SPICEINT_CELL ( asteroids, MAXSIZ );
         SPICEINT_CELL ( result,    MAXSIZ );
                .
                .
                .
         /.
         Compute the difference.
         ./
         diff_c ( &planets, &asteroids, &result );

-Restrictions
 
   1) The output set must be distinct from both of the input sets. 
      For example, the following calls are invalid. 

         diff_c  ( &current,  &new,      &current );
         diff_c  ( &new,      &current,  &current );

      In each of the examples above, whether or not the subroutine 
      signals an error, the results will almost certainly be wrong. 
      Nearly the same effect can be achieved, however, by placing the 
      result into a temporary set, which is immediately copied back 
      into one of the input sets, as shown below. 

         diff_c  ( &current,  &new,  &temp );
         copy_c  ( &temp,     &new         );

 
   2) String comparisons performed by this routine are Fortran-style:
      trailing blanks in the input sets are ignored. This gives
      consistent behavior with CSPICE code generated by the f2c
      translator, as well as with the Fortran SPICE Toolkit.

      Note that this behavior is not identical to that of the ANSI
      C library functions strcmp and strncmp.

-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
   C.A. Curzon     (JPL) 
   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
 
-Version
 
   -CSPICE Version 1.1.0, 15-FEB-2005 (NJB)

       Bug fix:  loop bound changed from 2 to 3 in loop used
       to free dynamically allocated arrays.

   -CSPICE Version 1.0.0, 08-AUG-2002 (NJB) (CAC) (WLT) (IMU)

-Index_Entries
 
   difference of two sets 
 
-&
*/


{ /* Begin diff_c */


   /*
   Local variables 
   */
   SpiceChar             * fCell[3];

   SpiceInt                fLen [3];
   SpiceInt                i;


   /*
   Standard SPICE error handling. 
   */
   if ( return_c() )
   {
      return;
   }

   chkin_c ( "diff_c" );

   /*
   Make sure data types match. 
   */
   CELLMATCH3 ( CHK_STANDARD, "diff_c", a, b, c );

   /*
   Make sure the input cells are sets.
   */
   CELLISSETCHK2 ( CHK_STANDARD, "diff_c", a, b );

   /*
   Initialize the cells if necessary. 
   */
   CELLINIT3 ( a, b, c );

   /*
   Call the difference routine appropriate for the data type of the
   cells.
   */
   if ( a->dtype == SPICE_CHR )
   {

      /*
      Construct Fortran-style sets suitable for passing to diffc_. 
      */
      C2F_MAP_CELL3 (  "", 
                       a, fCell,   fLen,
                       b, fCell+1, fLen+1,   
                       c, fCell+2, fLen+2  );


      if ( failed_c() )
      {
         chkout_c ( "diff_c" );
         return;
      }


      diffc_ ( (char    * )  fCell[0],
               (char    * )  fCell[1],
               (char    * )  fCell[2],
               (ftnlen    )  fLen[0],
               (ftnlen    )  fLen[1],
               (ftnlen    )  fLen[2]  );


      /*
      Map the diff back to a C style cell. 
      */
      F2C_MAP_CELL ( fCell[2], fLen[2], c );


      /*
      We're done with the dynamically allocated Fortran-style arrays. 
      */
      for ( i = 0;  i < 3;  i++ )
      {
         free ( fCell[i] );
      }

   }

   else if ( a->dtype == SPICE_DP )
   {
      diffd_ ( (doublereal * )  (a->base),
               (doublereal * )  (b->base),
               (doublereal * )  (c->base)  );
      /*
      Sync the output cell. 
      */
      if ( !failed_c() )
      {
         zzsynccl_c ( F2C, c );
      }

   }

   else if ( a->dtype == SPICE_INT )
   {
      diffi_ ( (integer * )  (a->base),
               (integer * )  (b->base),
               (integer * )  (c->base)  );      

      /*
      Sync the output cell. 
      */
      if ( !failed_c() )
      {
         zzsynccl_c ( F2C, c );
      }
   }

   else
   {
     setmsg_c ( "Cell a contains unrecognized data type code #." );
     errint_c ( "#",  (SpiceInt) (a->dtype)                      );
     sigerr_c ( "SPICE(NOTSUPPORTED)"                            );
     chkout_c ( "diff_c"                                         );
     return;
   }


   /*
   Indicate the result is a set. 
   */
   c->isSet = SPICETRUE;


   chkout_c ( "diff_c" );   

} /* End diff_c */
