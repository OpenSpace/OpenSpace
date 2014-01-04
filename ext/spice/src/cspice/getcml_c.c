/*

-Procedure getcml_c ( Get the command line )

-Abstract

   Store the contents of argv and argc for later access..

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

   UTILITY

*/

   #include <string.h>
   #include <stdlib.h>

   #include "SpiceUsr.h"

   void getcml_c ( SpiceInt     * argc,
                   SpiceChar  *** argv )

/*

-Brief_I/O

   Variable  I/O  Description
   --------  ---  --------------------------------------------------
   argc       O   The number of command line arguments.
   argv       O   The vector of command line arguments.

-Detailed_Input

   None.

-Detailed_Output

   argc      is the number of command line arguments.

   argv      is the vector of space delimited command line arguments.
             Each entry entry contains one argument.  argv[0] is the
             command name.

-Parameters

   None.

-Exceptions

   This routines participates in error tracing but detects no errors.
   Error detection is done in zzgetcml_c.c.

-Files

   None.

-Particulars

   This routine is a wrapper function for zzgetcml_c.c.  getcml_c
   allows a user to access the argv and argc values from any program
   module.

-Examples

   #include <stdio.h>
   #include <stdlib.h>

   #include "SpiceUsr.h"

   void main( int argc, char *argv[] )
   {


      /. Store argv and argc for latter access. ./

      putcml_c (argc, argv );


      ..... other stuff .....
      .....             .....

   }


   void goop ()
   {
      ..... new module .....

      SpiceInt      argc;
      SpiceChar  ** argv;


      .....
      .....

      /. Now get the stored information. ./

      getcml_c ( &argc, &argv );

   }



-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   E.D. Wright    (JPL)

-Version

   -CSPICE Version 1.0.1, 08-FEB-1998   (EDW)

      Routine rewritten to use private routine zzgetcml_c.c.

   -CSPICE Version 1.0.1, 14-JAN-1997   (EDW)

      Replaced a defined variable type for argv with a *** declaration.

   -CSPICE Version 1.0.0,  6-JAN-1997   (EDW)

-Index_Entries

   store/retrieve argc argv

-&
*/

{

   /*
   'zzgetcml_c' does all the real work.  Make the call.  The SPICEFALSE
   boolean indicates the call is comming from getcml_c.c and not
   putcml_c.c
   */

   chkin_c( "getcml_c" );

   zzgetcml_c ( argc, argv, SPICEFALSE );

   chkout_c( "getcml_c" );

}

