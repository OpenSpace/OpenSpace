/*

-Procedure badkpv_c ( Bad Kernel Pool Variable )

-Abstract
 
   Determine if a kernel pool variable is present and if so 
   that it has the correct size and type. 
 
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
 
    ERROR 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   SpiceBoolean badkpv_c ( ConstSpiceChar    *caller,
                           ConstSpiceChar    *name,
                           ConstSpiceChar    *comp,
                           SpiceInt           size,
                           SpiceInt           divby,
                           SpiceChar          type   )

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   caller     I   Name of the routine calling this routine. 
   name       I   Name of a kernel pool variable 
   comp       I   Comparison operator. 
   size       I   Expected size of the kernel pool variable 
   divby      I   A divisor of the size of the kernel pool variable. 
   type       I   Expected type of the kernel pool variable 
 
   The function returns SPICEFALSE if the kernel pool variable is OK.
 
-Detailed_Input
 
   caller     is the name of the routine calling this routine 
              to check correctness of kernel pool variables. 
 
   name       is the name of a kernel pool variable that the 
              calling program expects to be present in the 
              kernel pool. 
 
   comp       is the comparison operator to use when comparing 
              the number of components of the kernel pool variable 
              specified by name with the integer size.  If dim is 
              is the actual size of the kernel pool variable then 
              badkpv_c will check that the sentence 
 
                  dim comp size 
 
              is a true statement.  If it is not a true statement 
              an error will be signaled. 
 
              Allowed values for comp and their meanings are: 
 
                 "="      dim == size 
                 "<"      dim <  size 
                 ">"      dim >  size 
                 "=>"     dim >= size 
                 "<="     dim <= size 
 
 
   size       is an integer to compare with the actual 
              number of components of the kernel pool variable 
              specified by name. 
 
   divby      is an integer that is one of the factors of the 
              actual dimension of the specified kernel pool variable. 
              In other words, it is expected that divby evenly 
              divides the actual dimension of name. In those 
              cases in which the factors of the dimension of name 
              are not important, set divby to 1 in the calling 
              program. 
 
   type       is the expected type of the kernel pool variable. 
              Recognized values are 
 
                'C' for character type 
                'N' for numeric type (integer and double precision) 
 
              The case of type is insignificant.  If the value 
              of TYPE is not one of the 2 values given above 
              no check for the type of the variable will be 
              performed. 
 
 
-Detailed_Output
 
   The function returns the value SPICEFALSE if the kernel pool variable 
   has the expected properties.  Otherwise the routine signals 
   an error and returns the value SPICETRUE.
 
-Parameters
 
   None. 
 
-Files
 
   None. 
 
-Exceptions
 
   1) If the kernel pool variable specified by name is not 
      present in the kernel pool, the error 
      SPICE(VARIABLENOTFOUND) will be signaled and the 
      routine will return the value SPICETRUE.
 
   2) If the comparison operator specified by comp is unrecognized 
      the error SPICE(UNKNOWNCOMPARE) will be signaled and the 
      routine will return the value SPICETRUE.
 
   3) If the comparison of the actual size of the kernel pool 
      variable with size is not satisfied, the error 
      SPICE(BADVARIABLESIZE) will be signaled and the 
      routine will return the value SPICETRUE.
 
   4) If the variable does not have the expected type, the error 
      SPICE(BADVARIABLETYPE) will be signaled and the routine 
      will return the value SPICETRUE.
 
   5) If any input string pointers are null, the error
      SPICE(NULLPOINTER) will be signaled.
 
   6) If any input strings have length zero, the error
      SPICE(EMPTYSTRING) will be signaled.
      
-Particulars
 
   This routine takes care of routine checking that often needs 
   to be done by programs and routines that rely upon kernel 
   pool variables being present and having the correct attributes. 
 
   It checks for the presence of the kernel pool variable and 
   examines the type and dimension of the variable to make sure 
   they conform to the requirements of the calling routine. 
 
-Examples
 
   Suppose that you need to fetch a number of variables 
   from the kernel pool and want to check that the requested 
   items are in fact available prior to performing further 
   computations. The following shows how you might use 
   this routine to handle the details of checking of 
   the various items. 
 
         caller  == "MYROUTINE"
 
      We need some data for body 399 and we expect there to 
      be an even number of items available.  Moreover we 
      expect these items to be numeric. 
 
         name  == "BODY_399_DATA"
         comp  == ">"
         size  ==  1
         divby ==  2
         type  == 'N'
 
      In addition we need the units associated with this data. 
      We expect the units to be character and that the number 
      of components is 1. 
 
         name  == "BODY_399_DATAUNIT"; 
         comp  == "=" 
         size  == 1 
         divby == 1
         type  == 'C'
         
 
      if (   badkpv_c( caller, "BODY_399_DATA",      ">", 1, 2, 'N' ) 
          || badkpv_c( caller, "BODY_399_DATAUNITS", "=", 1, 1, 'C' ) ) 
      {
         chkout_c ( "MYROUTINE" );
         return;
      } 
 
 
-Restrictions
 
   None. 
 
-Author_and_Institution
 
   W.L. Taber      (JPL) 
   N.J. Bachman    (JPL)
   
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.0.0, 07-JUL-2000 (WLT) (NJB)

-Index_Entries
 
   Check the properties of a kernel pool variable 
 
-&
*/

{ /* Begin badkpv_c */

 
   /*
   Local variables
   */
   logical                 isbad;


   /*
   Participate in error tracing.
   */
   chkin_c ( "badkpv_c" );


   /*
   Check the input strings to make sure the pointers are non-null
   and the string lengths are non-zero.
   */
   CHKFSTR_VAL ( CHK_STANDARD, "badkpv_c", caller, SPICETRUE );
   CHKFSTR_VAL ( CHK_STANDARD, "badkpv_c", name,   SPICETRUE );
   CHKFSTR_VAL ( CHK_STANDARD, "badkpv_c", comp,   SPICETRUE );

   /*
   Let the f2c'd routine do all the work.
   */
   isbad = badkpv_ (  (char     *)  caller,
                      (char     *)  name,
                      (char     *)  comp,
                      (integer  *)  &size,
                      (integer  *)  &divby,
                      (char     *)  &type,
                      (ftnlen    )  strlen(caller),
                      (ftnlen    )  strlen(name),
                      (ftnlen    )  strlen(comp),
                      (ftnlen    )  1              );
                      
                      
   chkout_c ( "badkpv_c" );

   return (  (SpiceBoolean) isbad  );

} /* End badkpv_c */
