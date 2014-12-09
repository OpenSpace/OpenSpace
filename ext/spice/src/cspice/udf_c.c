/*

-Procedure udf_c ( GF, dummy function )

-Abstract
 
   No-op routine for with an argument signature matching udfuns. 
 
-Disclaimer
 
   THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE 
   CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. 
   GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE 
   ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE 
   PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" 
   TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY 
   WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A 
   PARTICULAR USE OR PURPOSE (AS set_c FORTH IN UNITED STATES UCC 
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
 
  None. 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"

   void udf_c ( SpiceDouble   x,
                SpiceDouble * value ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   x         I/O  Double precision value, unused. 
   value     I/O  Double precision value, unused. 
 
-Detailed_Input
 
   x         Double precision value, unused. 
 
   value     Double precision value, unused. 
 
-Detailed_Output
 
  None. 
 
-Parameters
 
  None. 
 
-Exceptions
 
  None. 
 
-Files
 
  None. 
 
-Particulars
 
  The routine performs no evaluations. It exists for GF routines 
  expecting an udfuns argument. In the cases where udfuns is 
  unneeded or unavailable, this routine provides a null operation 
  alternative. 
 
-Examples
 
  None. 
 
-Restrictions
 
  None. 
 
-Literature_References
 
  None. 
 
-Author_and_Institution
 
  E.D. Wright    (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 22-OCT-2013

-Index_Entries
 
 GF dummy function for udfuns signature arguments 
 
-&
*/

{ /* Begin udf_c */

   x      += 0.;

   *value += 0.;

} /* End udf_c */
