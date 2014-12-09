/*
 
-Procedure valid_c ( Validate a set )
 
-Abstract
 
   Create a valid CSPICE set from a CSPICE Cell of any data type.
 
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
 
   void valid_c (  SpiceInt      size,
                   SpiceInt      n,
                   SpiceCell   * set   )
 
/*
 
-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   size       I   Size (maximum cardinality) of the set.
   n          I   Initial no. of (possibly non-distinct) elements.
   set       I/O  Set to be validated.
 
-Detailed_Input
 
   size        is the maximum cardinality (number of elements)
               of the set.  size must not exceed the declared size
               of the set's data array.
 
   n           is the number of (possibly non-distinct) elements
               initially contained in the set's data array.
               N cannot be greater than the size of the set.
 
   set         is a CSPICE set.  set must be declared as a character,
               double precision, or integer SpiceCell.
 
               On input, set contains n elements.
 
-Detailed_Output
 
   set         on output is a valid set created from the input set.
 
               To create a valid set, the elements are ordered, and
               duplicate elements are removed.  The set's size and
               cardinality members are assigned their correct values.
 
               The set is ready for use with other set routines.
 
               When validating a character set, trailing blanks are not
               considered significant in process of sorting and
               removing duplicates.  Trailing blanks are not preserved
               on output.

-Parameters
 
   None.
 
-Exceptions
 
   1)   If the size of the set is too small to hold the set
        BEFORE validation, the error SPICE(INVALIDSIZE) is signaled.
        The set is not modified.
 
-Files
 
   None.
 
-Particulars
 
   Because a set is ordered and contains distinct values, to create a
   set from a cell, it is necessary to sort the data array and remove
   duplicates. Once the array has been sorted, duplicate elements
   (adjacent after sorting) are removed. The size and cardinality of
   the set are initialized, and the set is ready to go.
 
   This routine is typically used to create a CSPICE set from a CSPICE
   cell whose array which has been initialized via calls the appnd*_c
   routines, or through compile-time array initializers, or I/O
   statements.  The resulting set can then be used with the other set
   routines.
 
   When a set is constructed from a large set of unordered values,
   it is far more efficient to append the values to the set and
   then validate the set, than to build up the set via calls to the
   insrt*_c routines.  The latter sort the set and remove duplicates
   on each insertion.
 
   Because validation is done in place, there is no chance of
   overflow.
 
-Examples
 
   1) Build a double precision cell via a series of calls to appndd_c.
      Create a set from this set by calling valid_c.
 
         #include "SpiceUsr.h"
 
         int main()
         {
            /.
            Declare the set.  SETSIZ is the maximum capacity of the set.
            ./
            #define SETSIZ          1000000

            SPICEDOUBLE_CELL ( dpSet, SETSIZ );
 
            /.
            INISIZ will be the initial number of elements in the set.
            ./
            #define INISIZ          100000

            /.
            Other local variables:
            ./
            SpiceInt                i;

            /.
            Initialize the cell's data array.  We use bogus values to
            simplify the example.
            ./
            for ( i = 0;  i < INISIZ;  i++ )
            {
               appndd_c (  (SpiceDouble)(-i),  &dpset  );
            }
 
            /.
            Validate the set.  The elements of the set will be arranged
            in increasing order after this call.
            ./
            valid_c ( SETSIZ, INISIZ, &dpSet );
 
            return ( 0 );
         }
 
 
-Restrictions
 
   None.
 
-Literature_References
 
   1) String comparisons performed by this routine are Fortran-style:
      trailing blanks in the input sets are ignored. This gives
      consistent behavior with CSPICE code generated by the f2c
      translator, as well as with the Fortran SPICE Toolkit.

      Note that this behavior is not identical to that of the ANSI
      C library functions strcmp and strncmp.
 
-Author_and_Institution

   N.J. Bachman    (JPL)
   C.A. Curzon     (JPL)
   W.L. Taber      (JPL)
   I.M. Underwood  (JPL)
 
-Version

   -CSPICE Version 1.0.1, 12-NOV-2006 (EDW)
 
      Corrected minor type, the Literature_References header
      lacked the prefix "-".
 
   -CSPICE Version 1.0.0, 08-AUG-2002 (NJB) (CAC) (WLT) (IMU)
 
-Index_Entries
 
   validate a set
 
-&
*/
{
   /*
   Local variables
   */
   SpiceChar             * fCell;
 
   SpiceInt                fLen;
 
 
 
   /*
   Standard SPICE error handling.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "valid_c" );
 
 
   /*
   Unlike most other cell routines, we do not initialize the cell's
   size and cardinality at this point.
   */
 
 
   /*
   Call the valid* routine appropriate for the data type of the set.
   */
   if ( set->dtype == SPICE_CHR )
   {
 
      /*
      Construct a Fortran-style set suitable for passing to validc_.
      */
      C2F_MAP_CELL ( "valid_c", set, &fCell, &fLen );
 

      if ( failed_c() )
      {
         chkout_c ( "valid_c" );
         return;
      }

 
      validc_ ( (integer *)  &size,
                (integer *)  &n,
                (char    *)  fCell,
                (ftnlen   )  fLen    );
 
      /*
      Map the validated set back to a C style set.  This mapping
      sets the size and cardinality members of the cell.
      */
      F2C_MAP_CELL ( fCell, fLen, set );

      /*
      We're done with the dynamically allocated Fortran-style array. 
      */
      free ( fCell );
      
   }
 
   else if ( set->dtype == SPICE_DP )
   {
      validd_ ( (integer     *) &size,
                (integer     *) &n,
                (doublereal  *) (set->base) );
      /*
      Sync the output cell.
      */
      if ( !failed_c() )
      {
         zzsynccl_c ( F2C, set );
      }
 
   }
 
   else if ( set->dtype == SPICE_INT )
   {
      validi_ ( (integer     *) &size,
                (integer     *) &n,
                (integer     *) (set->base) );
 
      /*
      Sync the output cell.
      */
      if ( !failed_c() )
      {
         zzsynccl_c ( F2C, set );
      }
 
   }
 
   else
   {
      setmsg_c ( "Cell set contains unrecognized data type code #." );
      errint_c ( "#",  (SpiceInt) (set->dtype)                      );
      sigerr_c ( "SPICE(NOTSUPPORTED)"                              );
      chkout_c ( "valid_c"                                          );
      return;
   }

 
   /*
   Indicate the result is a set. 
   */
   set->isSet = SPICETRUE;


   chkout_c ( "valid_c" );
}
