/*

-Procedure vdist_c ( Vector distance )

-Abstract

   Return the distance between two three-dimensional vectors.

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

   VECTOR

*/

   #include "SpiceUsr.h"
   #undef    vdist_c


   SpiceDouble vdist_c ( ConstSpiceDouble v1[3],
                         ConstSpiceDouble v2[3] )

/*

-Brief_I/O

   Variable  I/O  Description
   --------  ---  --------------------------------------------------

   v1,
   v2         I   Two 3-vectors.

   The function returns the distance between v1 and v2.

-Detailed_Input

   v1,
   v2         are two vectors in three-dimensional space, the
              distance between which is desired.

-Detailed_Output

   The function returns the distance between v1 and v2.  This is
   defined as

            ||  v1 - v2  ||,

   where || x || indicates the Euclidean norm of the vector x.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This function is simply shorthand for the code

      vsub_c ( v1, v2, diff );

      dist = vnorm_c ( diff );

   Using this function saves you the annoyance of declaring local
   storage for the difference vector diff.


   The Euclidean norm of a three-dimensional vector (x, y, z) is
   defined as

                                   1/2
           2        2        2
      (   x    +   y    +   z    ).


   This number is the distance of the point (x, y, z) from the
   origin.  If A and B are two vectors whose components are

      ( A(1), A(2), A(3) )    and    ( B(1), B(2), B(3) ),

   then the distance between A and B is the norm of the difference
   A - B, which has components


      (  A(1) - B(1),  A(2) - B(2),  A(3) - B(3)  ).


   A related routine is vdistg_, which computes the distance between
   two vectors of general dimension.

-Examples

   1)  If v1 is

          ( 2.0,  3.0,  0. )

       and v2 is

          ( 5.0,  7.0,  12. ),

       vdist_c (v1, v2) will be 13..


   2)  If VGR2 and NEP are states of the Voyager 2 spacecraft and
       Neptune with respect to some common center at a given time
       ET, then

          vdist_c ( VGR2, NEP )

       yields the distance between the spacecraft and Neptune at time
       ET.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman   (JPL)

-Version

   -CSPICE Version 1.2.0, 22-OCT-1998 (NJB)

      Made input vectors const.  Removed #include of SpiceZfc.h.

   -CSPICE Version 1.1.0, 06-MAR-1998   (EDW)

      Removed non printing character.

   -CSPICE Version 1.0.0, 08-FEB-1998   (EDW)

-Index_Entries

   distance between 3-dimensional vectors

-&
*/

{ /* Begin vdist_c */

   /*
   Local constants
   */

   SpiceDouble    diff[3];


   /* Function Body */

   vsub_c ( v1, v2, diff);


   return  vnorm_c (diff);

} /* End vdist_c */
