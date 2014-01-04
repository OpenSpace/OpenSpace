/*

-Procedure wnsumd_c ( Summary of a double precision window )

-Abstract
 
   Summarize the contents of a double precision window. 
 
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
 
   WINDOWS 
 
-Keywords
 
   WINDOWS 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void wnsumd_c ( SpiceCell      * window,
                   SpiceDouble    * meas,
                   SpiceDouble    * avg,
                   SpiceDouble    * stddev,
                   SpiceInt       * shortest,
                   SpiceInt       * longest   )              
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   window     I   Window to be summarized. 
   meas       O   Total measure of intervals in window. 
   avg        O   Average measure. 
   stddev     O   Standard deviation. 
   shortest, 
   longest    O   Locations of shortest, longest intervals. 
 
-Detailed_Input
 
   window      is a window containing zero or more intervals. 

               window must be declared as a double precision SpiceCell.
 
-Detailed_Output
 
   meas        is the total measure of the intervals in the input 
               window. This is just the sum of the measures of the 
               individual intervals. 

   avg         is the average of the measures of the intervals in 
               the input window. 

   stddev      is the standard deviation of the measures of the 
               intervals in the input window. 

   shortest, 
   longest     are the locations of the shortest and longest 
               intervals in the input window. The shortest interval 
               is 
 
                  [   SPICE_CELL_ELEM_D( window, shortest   ),
                      SPICE_CELL_ELEM_D( window, shortest+1 )   ] 

               and the longest is 

                  [   SPICE_CELL_ELEM_D( window, longest   ),
                      SPICE_CELL_ELEM_D( window, longest+1 )   ] 

               shortest and longest are both zero if the input window 
               contains no intervals. 

               If window contains multiple intervals having the shortest
               length, shortest is the index of the first such interval.
               Likewise for the longest length.

               Indices range from 0 to N-1, where N is the number of
               intervals in the window.

-Parameters
 
   None. 
 
-Exceptions
 
   1) If the input window does not have double precision type,
      the error SPICE(TYPEMISMATCH) is signaled.
  
-Files
 
   None. 
 
-Particulars

   This routine provides a summary of the input window, consisting 
   of the following items: 

      - The measure of the window. 

      - The average and standard deviation of the measures 
        of the individual intervals in the window. 

      - The indices of the left endpoints of the shortest 
        and longest intervals in the window. 

   All of these quantities are zero if the window contains no 
   intervals. 
 
-Examples
 
   Let a contain the intervals 

         [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ] 

   Let b contain the singleton intervals 

         [ 2, 2 ]  [ 9, 9 ]  [ 27, 27 ] 

   The measures of a and b are 

         (3-1) + (11-7) + (27-23) = 10 

   and 

         (2-2) + (9-9) + (27-27) = 0 

   respectively. Each window has three intervals; thus, the average 
   measures of the windows are 10/3 and 0. The standard deviations 
   are 

        ---------------------------------------------- 
       |          2         2          2 
       |     (3-1)  + (11-7)  + (27-23)           2             1/2 
       |     ---------------------------  - (10/3)       = (8/9) 
       |                3 
     \ | 
      \| 

   and 0. Neither window has one "shortest" interval or "longest"
   interval; so the first ones found are returned: shortest and longest
   are 0 and 2 for a, 0 and 0 for b.
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   H.A. Neilan     (JPL) 
   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
 
-Version

   -CSPICE Version 1.0.1, 27-JAN-2009 (EDW)

      Corrected argument names shown in Brief I/O list.
      "short" to "shortest"; "long" to "longest".
 
   -CSPICE Version 1.0.0, 29-JUL-2002 (NJB) (HAN) (WLT) (IMU)

-Index_Entries
 
   summary of a d.p. window 
 
-&
*/

{ /* Begin wnsumd_c */


   /*
   Use discovery check-in.

   Make sure cell data type is d.p. 
   */
   CELLTYPECHK ( CHK_DISCOVER, "wnsumd_c", SPICE_DP, window );

   /*
   Initialize the cell if necessary. 
   */
   CELLINIT ( window );
   
   /*
   Let the f2c'd routine do the work. 
   */
   wnsumd_ ( (doublereal * ) (window->base),
             (doublereal * ) meas, 
             (doublereal * ) avg, 
             (doublereal * ) stddev, 
             (integer    * ) shortest, 
             (integer    * ) longest        );

   /*
   Map shortest and longest from Fortran style to C style indices.
   */
   (*shortest) --;
   (*longest ) --;

} /* End wnsumd_c */
