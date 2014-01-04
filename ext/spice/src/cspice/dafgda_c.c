/* 

-Procedure dafgda_c ( DAF, read data from address )

-Abstract

   Read the double precision data bounded by two addresses within
   a DAF.

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

   DAF

-Keywords

   FILES

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

 
   void dafgda_c ( SpiceInt       handle, 
                   SpiceInt       begin,
                   SpiceInt       end,
                   SpiceDouble  * data )
/*
-Brief_I/O

   Variable  I/O  Description
   --------  ---  --------------------------------------------------
   handle     I   Handle of a DAF.
   begin,
   end        I   Initial, final address within file.
   data       O   Data contained between `begin' and `end'.

-Detailed_Input

   handle      is the handle of a DAF.

   begin,
   end         are the initial and final addresses of a contiguous
               set of double precision numbers within a DAF.
               Presumably, these make up all or part of a particular
               array.
               
               Note that CSPICE DAF addresses begin at 1 as in the 
               FORTRAN version of the SPICE Toolkit.
               
-Detailed_Output

   data        are the double precision data contained between
               the specified addresses within the specified file.

-Parameters

   None.

-Exceptions

   1) If `begin' is zero or negative, the error SPICE(DAFNEGADDR)
      is signaled.

   2) If `begin' > `end', the error SPICE(DAFBEGGTEND)
      is signaled.

   3) If `handle' is invalid, routines in the call tree of dafgda_c
      signal an appropriate error.

   4) If the range of addresses covered between `begin' and `end'
      includes records that do not contain strictly double
      precision data, then the values returned in `data' are
      undefined.  See the Restrictions section below for details.

-Files

   None.

-Particulars

   The principal reason that DAFs are so easy to use is that
   the data in each DAF are considered to be one long contiguous
   set of double precision numbers. You can grab data from anywhere
   within a DAF without knowing (or caring) about the physical
   records in which they are stored.

   This routine replaces dafrda_c as the principal mechanism for
   reading the contents of DAF arrays.

-Examples

   The following code fragment illustrates the use of dafgda_c to read
   data from an array. The array begins with a directory containing 11
   epochs. Each pair of epochs bounds an interval, and each interval is
   covered by a set of eight osculating elements.

      #include "SpiceUsr.h"
      
         .
         .
         .
      
      dafus_c ( sum, nd, ni, dc, ic );
      begin = ic[4];
      end   = ic[5];

      dafgda_c ( handle, begin, begin+10, epochs );

      for ( i = 0;  i < 10;  i++ )
      {
         if (     ( et > epochs[i]   )
              &&  ( et < epochs[i+1] ) ) 
         {
            offset = begin + 11 + (i - 1) * 8;
            dafgda_c ( handle, offset+1, offset+8, elements );
            return;
         }
      }


-Restrictions

   1) There are several types of records in a DAF.  This routine
      is only to be used to read double precision data bounded
      between two DAF addresses.  The range of addresses input
      may not cross data and summary record boundaries.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman    (JPL)

-Version

   -CSPICE Version 1.0.1, 23-JAN-2008 (EDW)

      Removed a spurious and unneeded "-Declarations"
      tag. The tag's presence prevented the HTML API doc
      script from parsing the function description.

   -CSPICE Version 1.0.0, 14-SEP-2006 (NJB)

-Index_Entries

   read data from daf address

-&
*/
 
{ /* Begin dafgda_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "dafgda_c" );
   
   dafgda_ ( ( integer    * ) &handle,
             ( integer    * ) &begin,
             ( integer    * ) &end,
             ( doublereal * ) data );
             
   chkout_c ( "dafgda_c" );
   
} /* End of dafgda_c */
 
