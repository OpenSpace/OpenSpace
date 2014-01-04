/*

-Procedure sigerr_c ( Signal Error Condition )

-Abstract
 
   Inform the CSPICE error processing mechanism that an error has 
   occurred, and specify the type of error. 
 
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


   void sigerr_c ( ConstSpiceChar * message )

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   msg        I   A short error message. 
 
-Detailed_Input
 
   msg     A ``short'' error message. 
           msg indicates the type of error that has occurred. 
 
           Only the first 25 characters of msg will be stored; 
           additional characters will be truncated. 
 
           Generally, msg will be stored internally by the CSPICE 
           error handling mechanism.  The only exception 
           is the case in which the user has commanded the error 
           handling mechanism to ``ignore'' the error indicated by 
           msg. 
 
           As a default, msg will be output to the standard output. 
           See the required reading file for a discussion of how 
           to customize CSPICE error handling behavior, and 
           in particular, the disposition of msg. 
 
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
 
   First of all, please read the ``required reading'' file. 
   The information below will make a lot more sense if you do. 
 
   This is the routine used by CSPICE to signal the detection 
   of errors. 
 
   Making a call to sigerr_c is the way to inform the error 
   handling mechanism that an error has occurred. 
 
   Specifically, the effects of this routine are: 
 
   1.  If responding to the error indicated by msg has 
       not been disabled: 
 
       a. msg will be stored internally.  As a result, 
          The CSPICE routine, getmsg, will be able to 
          retrieve msg, until msg has been ``erased'' 
          by a call to reset_c, or overwritten by another 
          call to sigerr_c. 
 
       b. An indication of an ``error condition'' will 
          be set internally.  The CSPICE logical 
          function, failed_c, will take the value, SPICETRUE, 
          as a result, until the error condition is 
          negated by a call to reset_c. 
 
       c. All of the error messages that have been selected 
          for automatic output via errprt_c will be output. 
          The set of messages is some subset of { short message, 
          long message, explanation of short message, 
          traceback, and default message }. 
 
       d. If the error response mode is not "RETURN", 
          Setting of the long error message is enabled. 
          You can't re-set the long error message, once 
           it has been set, without first signalling an error. 

       e. In "RETURN" mode, further signalling of error 
          messages, and setting of the long message, are disabled. 
          (These capabilities can be re-enabled by calling RESET). 
 
 
   2.  If the error handling mechanism has been commanded to 
       ``ignore'' the error indicated by msg, the call to sigerr_c 
       has no effect. 
 
   If you wish to set the long error message, call 
   setmsg_c BEFORE calling sigerr_c. 
 
 
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
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
   K.R. Gehringer  (JPL) 
 
-Version

   -CSPICE Version 1.1.0, 23-JUL-2001 (NJB)
     
      Removed tab characters from source file.

   -CSPICE Version 1.2.1, 25-MAR-1998 (EDW)
     
      Minor corrections to header.

   -CSPICE Version 1.2.0, 08-FEB-1998 (NJB)
   
      Re-implemented routine without dynamically allocated, temporary 
      strings.  Made various header fixes.
      
   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries
 
   signal error condition 
 
-&
*/

{ /* Begin sigerr_c */



   /*
   Check the input string to make sure the pointer is non-null 
   and the string length is non-zero.   
   */
   CHKFSTR ( CHK_DISCOVER, "sigerr_c", message );

   /*
   Call the f2c'd Fortran routine.
   */
   sigerr_ ( ( char  * ) message,
             ( ftnlen  ) strlen(message) );


} /* End sigerr_c */
