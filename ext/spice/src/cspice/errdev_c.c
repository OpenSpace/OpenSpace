/*

-Procedure errdev_c ( Get/Set Error Output Device Name )

-Abstract

   Retrieve or set the name of the current output
   device for error messages.

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

   ERROR

-Keywords

   ERROR

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void errdev_c ( ConstSpiceChar * op,
                   SpiceInt         lenout,
                   SpiceChar      * device )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   op         I   The operation:  "GET" or "SET".
   lenout     I   Length of device for output.
   device    I/O  The device name.

-Detailed_Input

   op      indicates the operation to be performed.  Possible
           values are "GET" and "SET".  "GET" means, "set
           device equal to the name of the current error
           output device"  "SET" means, "set the name of the
           current error output device equal to the value of
           device."

   lenout  the string size of output 'device' when op equals "GET."
           The size described by lenout should be large enough to 
           hold all characters of any possible output string 
           plus 1 (to accommodate the C null terminator).

   device  is an input when op has the value, "SET".  It
           indicates an output device to which error messages
           are to be sent.  Possible values for device are:

            1.    A file name.  Note that the name must not
                  use one of the reserved strings below.

            2.    "SCREEN"    The output will go to the
                   screen.  This is the default device.

            3.    "NULL"      The data will not be output.

            "SCREEN" and "NULL" can be written in mixed
            case.  For example, the following call will work:

            errdev_c ( "SET", lenout, "screEn" );

-Detailed_Output

   device  is an output returning the current error output device
           when 'op' equals "GET."  See "Detailed Input" 
           descriptions of these values.

-Parameters

   None.

-Exceptions

   1) If the input argument op does not indicate a valid operation,
      the error SPICE(INVALIDOPERATION) will be signaled.

   2) When op is "SET", if the input argument device has length greater
      than FILEN characters, the error SPICE(DEVICENAMETOOLONG) will
      be signaled.

   3) The error SPICE(EMPTYSTRING) is signaled if either input string
      does not contain at least one character, since an input string
      cannot be converted to a Fortran-style string in this case.  This
      check always applies to op; it applies to device only when
      device is an input, that is, when op is "SET."

   4) The error SPICE(NULLPOINTER) is signaled if either string pointer
      argument is null.

   5) The caller must pass a value indicating the length of the output
      string, when device is an output.  If this value is not at least
      2, the error SPICE(STRINGTOOSHORT) is signaled.

-Files

   None.

-Particulars

   This routine supports spooling of error messages to log files.

-Examples

   1.  In this example, we select as the output device
       the file, SPUD.DAT.

          /.
          Set the error output device to the file SPUD.DAT:
          ./
          errdev_c ( "SET", lenout, "SPUD.DAT" );


-Restrictions

   This routine has no capability of determining the validity
   of the name of an output device.  Care must be taken
   to ensure that the file named is the correct one.

   The device name is assumed to be no longer than FILEN characters.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman    (JPL)
   H.A. Neilan     (JPL)
   E.D. Wright     (JPL)

-Version

   -CSPICE Version 1.3.1, 25-SEP-2003 (EDW)

      Corrected confusing description of 'lenout' argument.

   -CSPICE Version 1.3.0, 24-JUN-2003 (NJB)

      Bug fix:  case of invalid operation keyword is now 
      diagnosed, as per the Exceptions section of the header.

   -CSPICE Version 1.2.0, 28-AUG-1999 (NJB)

      Bug fix:  changed errprt_ call to call to errdev_.

   -CSPICE Version 1.2.0, 09-FEB-1998 (NJB)

      Re-implemented routine without dynamically allocated, temporary
      strings.  Made various header fixes.

   -CSPICE Version 1.0.1, 30-OCT-1997 (EDW)

      Corrected errors in examples in which the call sequence
      was incorrect.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries

   get/set error output device name

-&
*/


{ /* Begin errdev_c.c */


   /*
   Participate in error tracing.
   */
   if ( return_c() ) 
      {
      return;
      }

   chkin_c ( "errdev_c" );


   /*
   Check the input string op to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "errdev_c", op );


   if( eqstr_c (op, "SET") )
      {

      /*
      The operation is SET. The argument "device" will be an input
      string.
      */
      CHKFSTR ( CHK_STANDARD, "errdev_c", device );

      /*
      Call the f2c'd Fortran routine.
      */
      errdev_ ( ( char   * ) op,
                ( char   * ) device,
                ( ftnlen   ) strlen(op),
                ( ftnlen   ) strlen(device) );

      }

   else if ( eqstr_c (op, "GET") )
      {

      /*
      Operation is GET.  "device" will be an output string.

      Make sure the output string has at least enough room for one
      output character and a null terminator.  Also check for a null
      pointer.
      */
      CHKOSTR ( CHK_STANDARD, "errdev_c", device, lenout );

      /*
      After the routine call, create a C string from the
      Fortran output string.
      */
      errdev_( ( char * ) op,
               ( char * ) device,
               ( ftnlen ) strlen(op),
               ( ftnlen ) lenout-1     );


      F2C_ConvertStr( lenout, device );
      }

   else
      {
      setmsg_c ( "Input argument op had value: # "
                 "Valid choices are GET or SET."   );   
      errch_c  ( "#",  op                          );
      sigerr_c ( "SPICE(INVALIDOPERATION)"         );
      chkout_c ( "errdev_c"                        );
      return;
      }

   chkout_c ( "errdev_c" );

} /* End errdev_c */
