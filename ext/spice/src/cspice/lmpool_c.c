/*

-Procedure lmpool_c ( Load variables from memory into the pool )

-Abstract
 
   Load the variables contained in an internal buffer into the 
   kernel pool. 
 
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
 
   KERNEL 
 
-Keywords
 
   CONSTANTS 
   FILES 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #include "SpiceZim.h"
   #undef    lmpool_c


   void lmpool_c ( const void  * cvals,
                   SpiceInt      lenvals,
                   SpiceInt      n       ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   cvals      I   An array that contains a SPICE text kernel.
   lenvals    I   Length of strings in cvals.
   n          I   The number of entries in cvals. 
 
-Detailed_Input
 
   cvals          is an array of strings that contains lines of text 
                  that could serve as a SPICE text kernel.  cvals is 
                  declared as follows:
              
                     ConstSpiceChar   cvals [n][lenvals]
              
                  Each string in cvals is null-terminated.
              
   lenvals        is the common length of the strings in cvals,
                  including the terminating nulls.
              
   n              is the number of strings in cvals. 
 
-Detailed_Output
 
   None. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the input string pointer is null, the error SPICE(NULLPOINTER) 
      will be signaled.

   2) If the input string length lenvals is not at least 2, the error
      SPICE(STRINGTOOLSHORT) will be signaled.

   3) The error 'SPICE(BADVARNAME)' signals if a kernel pool
      variable name length exceeds 32.

   4) Other exceptions are diagnosed by routines in the call tree of 
      this routine.
-Files
 
   None. 
 
-Particulars
 
   This routine allows you to store a text kernel in an internal 
   array of your program and load this array into the kernel pool 
   without first storing its contents as a text kernel. 

   Kernel pool variable names are restricted to a length of 32
   characters or less.
 
-Examples
 
   Suppose that your application is not particularly sensitive 
   to the current number of leapseconds but that you would 
   still like to use a relatively recent leapseconds kernel 
   without requiring users to load a leapseconds kernel into 
   the program.  The example below shows how you might set up 
   the initialization portion of your program. 
 
      #include "SpiceUsr.h"
      
      #define LNSIZE          81
      #define NLINES          27
      
      SpiceChar               textbuf[NLINES][LNSIZE] = 
                     {
                        "DELTET/DELTA_T_A = 32.184",
                        "DELTET/K         = 1.657D-3",
                        "DELTET/EB        = 1.671D-2",
                        "DELTET/M         = ( 6.239996 1.99096871D-7 )",
                        "DELTET/DELTA_AT  = ( 10, @1972-JAN-1",
                        "                     11, @1972-JUL-1",
                        "                     12, @1973-JAN-1",
                        "                     13, @1974-JAN-1",
                        "                     14, @1975-JAN-1",
                        "                     15, @1976-JAN-1",
                        "                     16, @1977-JAN-1",
                        "                     17, @1978-JAN-1",
                        "                     18, @1979-JAN-1",
                        "                     19, @1980-JAN-1",
                        "                     20, @1981-JUL-1",
                        "                     21, @1982-JUL-1",
                        "                     22, @1983-JUL-1",
                        "                     23, @1985-JUL-1",
                        "                     24, @1988-JAN-1",
                        "                     25, @1990-JAN-1",
                        "                     26, @1991-JAN-1",
                        "                     27, @1992-JUL-1",
                        "                     28, @1993-JUL-1",
                        "                     29, @1994-JUL-1",
                        "                     30, @1996-JAN-1",
                        "                     31, @1997-JUL-1",
                        "                     32, @1999-JAN-1 )"
                     };
                      
      lmpool_c ( textbuf, LNSIZE, NLINES );
 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   W.L. Taber      (JPL) 
 
-Version

   -CSPICE Version 1.3.1,  10-FEB-2010 (EDW)

      Added mention of the restriction on kernel pool variable 
      names to 32 characters or less.

   -CSPICE Version 1.3.0, 12-JUL-2002 (NJB)

      Call to C2F_CreateStrArr_Sig replaced with call to C2F_MapStrArr.

   -CSPICE Version 1.2.0, 28-AUG-2001 (NJB)

      Const-qualified input array.

   -CSPICE Version 1.1.0, 14-FEB-2000 (NJB)

       Calls to C2F_CreateStrArr replaced with calls to error-signaling 
       version of this routine:  C2F_CreateStrArr_Sig.
      
   -CSPICE Version 1.0.0, 08-JUN-1999 (NJB) (WLT) 

-Index_Entries
 
   Load the kernel pool from an internal text buffer 
 
-&
*/

{ /* Begin lmpool_c */



   /*
   Local variables
   */

   SpiceChar             * fCvalsArr;

   SpiceInt                fCvalsLen;


   /*
   Participate in error tracing.
   */
   chkin_c ( "lmpool_c" );

   /*
   Make sure the input string pointer is non-null and that the
   length lenvals is sufficient.  
   */
   CHKOSTR ( CHK_STANDARD, "lmpool_c", cvals, lenvals );


   /*
   Create a Fortran-style string array.
   */
   C2F_MapStrArr ( "lmpool_c", n, lenvals, cvals, &fCvalsLen, &fCvalsArr );

   if ( failed_c() )
   {
      chkout_c ( "lmpool_c" );
      return;
   }


   /*
   Call the f2c'd routine.
   */
   lmpool_ (  ( char       * ) fCvalsArr,
              ( integer    * ) &n,
              ( ftnlen       ) fCvalsLen );


   /*
   Free the dynamically allocated array.
   */
   free ( fCvalsArr );
   
   chkout_c ( "lmpool_c" );

} /* End lmpool_c */

