/*

-Procedure setmsg_c  ( Set Long Error Message )

-Abstract

   Set the value of the current long error message.

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


   void setmsg_c ( ConstSpiceChar * message )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   message    I   A long error message.

-Detailed_Input

   message        A ``long'' error message.
                  message is a detailed description of the error.
                  message is supposed to start with the name of the
                  module which detected the error, followed by a
                  colon.  Example:

                     "rdtext_c:  There are no more free logical units"

                  Only the first LMSGLN (see setmsg.c) characters of
                  message are stored; any further characters are
                  truncated.

                  Generally, message will be stored internally by the
                  CSPICE error handling mechanism.  The only exception
                  is the case in which the user has commanded the
                  toolkit to ``ignore'' the error indicated by message.

                  As a default, message will be output to the screen.
                  See the required reading file for a discussion of how
                  to customize toolkit error handling behavior, and
                  in particular, the disposition of message.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   This routine does not detect any errors.

   However, this routine is part of the interface to the
   CSPICE error handling mechanism.  For this reason,
   this routine does not participate in the trace scheme,
   even though it has external references.

-Files

   None.

-Particulars

   The CSPICE routine sigerr_c should always be called
   AFTER this routine is called, when an error is detected.

   The effects of this routine are:

      1.  If acceptance of a new long error message is
          allowed:

          message will be stored internally.  As a result,
          The CSPICE routine, getmsg_ , will be able to
          retrieve message, until message has been ``erased''
          by a call to reset_c, or overwritten by another
          call to setmsg_c.


      2.  If acceptance of a new long error message is not allowed,
          a call to this routine has no effect.

-Examples


    In the following example, an error is signaled because the
    double precision variable x contains an invalid value.  The
    value of x and the maximum allowed value MAXVAL are substituted
    into the error message at the locations indicated by the # signs
    below.

       /.
       Indicate that x is out of range if x is too large.
       ./

       if ( x > MAXVAL )
       {
          setmsg_c ( "Variable x = #; maximum allowed value is #" );
          errdp_c  ( "#",  x                                      );
          errdp_c  ( "#",  MAXVAL                                 );
          sigerr_c ( "SPICE(VALUEOUTOFRANGE)"                     ) ;
          return;
       }


-Restrictions

   sigerr_c must be called once after each call to this routine.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman    (JPL)

-Version

   -CSPICE Version 1.2.1, 25-MAR-1998 (EDW)

      Corrected errors in header.

   -CSPICE Version 1.2.0, 08-FEB-1998 (NJB)

      Re-implemented routine without dynamically allocated, temporary
      strings.  Made various header fixes.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries

   set long error message

-&
*/

{ /* Begin setmsg_c */

   /* Local Variables */

   /*
   Check the input string to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_DISCOVER, "setmsg_c", message );


   /*
   Call the f2c'd Fortran routine.
   */
   setmsg_ ( ( char  * ) message,
             ( ftnlen  ) strlen(message) );


} /* End setmsg_c */
