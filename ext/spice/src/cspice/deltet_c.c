/*

-Procedure deltet_c ( Delta ET, ET - UTC )

-Abstract
 
   Return the value of Delta ET (ET-UTC) for an input epoch. 
 
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
 
   TIME 
   KERNEL 
 
-Keywords
 
   TIME 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void deltet_c ( SpiceDouble      epoch,
                   ConstSpiceChar * eptype,
                   SpiceDouble    * delta ) 

/*

-Brief_I/O 
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   epoch      I   Input epoch (seconds past J2000). 
   eptype     I   Type of input epoch ("UTC" or "ET"). 
   delta      O   Delta ET (ET-UTC) at input epoch. 
 
-Detailed_Input
 
   epoch       is the epoch at which "delta ET" is to be computed. 
               `epoch' may be either UTC or ephemeris seconds past 
               J2000, as specified by EPTYPE. 

   eptype      indicates the type of input epoch. It may be either 
               of the following: 

                  "UTC"    UTC seconds past J2000 UTC.

                  "ET"     Ephemeris seconds past J2000 TDB,
                           also known as barycentric dynamical
                           time (TDB).
 
-Detailed_Output
 
   delta       is the value of 

                  "delta ET" = ET - UTC 

               at the input epoch. This is added to UTC to give 
               ET, or subtracted from ET to give UTC. The routine 
               is reversible: that is, given the following calls, 

                  deltet_c ( utc,      "UTC", &del1 );
                  deltet_c ( utc+del1, "ET",  &del2 );

               the expression 

                  ( del1 == del2 ) 

               is true. 

-Parameters
 
   None. 
 
-Exceptions
 
   1) If the input epoch is not recognized, the error 
      SPICE(INVALIDEPOCH) is signaled. 
 
   2) If the variables necessary for the computation of delta 
      have not been loaded into the kernel pool, the error 
      SPICE(KERNELVARNOTFOUND) is signaled. 
 
   3) If the number of leapseconds in the pool is greater than 
      the local leapseconds buffer size, the error  
      SPICE(BUFFEROVERFLOW) is signaled.    

   4) The error SPICE(EMPTYSTRING) is signaled if the input
      string `eptype' does not contain at least one character, since
      the input string cannot be converted to a Fortran-style string in
      this case.
      
   5) The error SPICE(NULLPOINTER) is signaled if the input string
      pointer is null.

-Files
 
   None. 
 
-Particulars
 
   The value of Delta ET is given by
 
      delta = ( ET - TAI ) + leap seconds 

   where TAI is the atomic time corresponding to the input epoch.
 
-Examples
 
   The following example shows how deltet_c may be used to convert 
   from UTC seconds past J2000 to TDB seconds past J2000. 

      deltet_c ( utcsec, "UTC", &delta );
      et = utcsec + delta 

   The following example shows how deltet_c may be used to convert 
   from ephemeris seconds past J2000 to UTC seconds past J2000. 

      deltet_c ( et, "et", &delta );
      utcsec = et - delta;

   See the TIME Required Reading for further examples. 
 
-Restrictions
 
   The routines str2et_c and timout_c are preferred for conversions 
   between UTC string and ET represented as seconds past J2000 TDB. 

   This routine is provided mainly to provide a method of representing
   an epoch as UTC seconds past J2000.

-Author_and_Institution
 
   N.J. Bachman    (JPL)
   W.M. Owen       (JPL) 
   I.M. Underwood  (JPL) 
 
-Literature_References
 
   [1] Astronomical Almanac. 
 
-Version
 
   -CSPICE Version 1.0.0, 01-AUG-2003 (NJB) (WMO) (IMU)

-Index_Entries
 
   difference between ephemeris time and utc 
 
-&
*/

{ /* Begin deltet_c */


   /*
   Participate in error tracing.
   */
   if ( return_c() ) 
   {
      return;
   }
   chkin_c ( "deltet_c" );


   /*
   Check the input string `eptype' to make sure the pointer is non-null 
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "deltet_c", eptype );


   /*
   Call the f2c'd Fortran routine.
   */
   deltet_ ( ( doublereal * ) &epoch,
             ( char       * ) eptype,
             ( doublereal * ) delta,
             ( ftnlen       ) strlen(eptype) );


   chkout_c ( "deltet_c" );

} /* End deltet_c */
