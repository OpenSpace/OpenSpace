/*

-Procedure getelm_c ( Get the components from two-line elements)

-Abstract
 
  Given a the "lines" of a two-line element set, parse the 
  lines and return the elements in units suitable for use 
  in SPICE software. 
 
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
 
   PARSING 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #undef    getelm_c
   

   void getelm_c ( SpiceInt         frstyr,
                   SpiceInt         lineln,
                   const void     * lines,
                   SpiceDouble    * epoch,
                   SpiceDouble    * elems   ) 
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   frstyr     I   Year of earliest representable two-line elements.
   lineln     I   Length of strings in lines array.
   lines      I   A pair of "lines" containing two-line elements.
   epoch      O   The epoch of the elements in seconds past J2000. 
   elems      O   The elements converted to SPICE units. 
 
-Detailed_Input
 
   frstyr    is the first year possible for two line elements. Since
             two line elements allow only two digits for the year, some
             conventions must be followed concerning which century the
             two digits refer to.  frstyr is the year of the earliest
             representable elements. The two-digit year is mapped to
             the year in the interval from frstyr to frstyr + 99 that
             has the same last two digits as the two digit year in the
             element set.  For example if frstyr is set to 1960  then
             the two digit years are mapped as shown in the table
             below:
 
             Two-line         Maps to 
             element year 
             
                00            2000 
                01            2001 
                02            2002 
                 .              . 
                 .              . 
                 .              . 
                58            2058 
                59            2059 
               -------------------- 
                60            1960 
                61            1961 
                62            1962 
                 .              . 
                 .              . 
                 .              . 
                99            1999 
 
              Note that if Space Command should decide to represent
              years in 21st century as 100 + the last two digits of the
              year (for example: 2015 is represented as 115) instead of
              simply dropping the first two digits of the year, this
              routine will correctly map the year as long as you set
              frstyr to some value between 1900 and 1999.
 
   lines      is a pair of lines of text that comprise a Space command
              ``two-line element'' set.  lines should be declared
              
                 SpiceChar lines[2][lineln];

              These text lines should be the same as they are presented
              in the two-line element files available from Space
              Command (formerly NORAD). Below is an example of a
              two-line set for TOPEX.
 
   TOPEX 
   1 22076U 92052A   97173.53461370 -.00000038  00000-0  10000-3 0   594 
   2 22076  66.0378 163.4372 0008359 278.7732  81.2337 12.80930736227550 
 
 
                 
 
-Detailed_Output
 
   epoch      is the epoch of the two line elements supplied via 
              the input array lines.  Epoch is returned in TDB 
              seconds past J2000. 
 
   elems      is an array containing the elements from the two line 
              set supplied via the array lines.  The elements are 
              in units suitable for use by the CSPICE routine 
              ev2lin_. 
 
              Also note that the elements XNDD6O and BSTAR 
              incorporate the exponential factor present in the 
              input two line elements in LINES.  (See particulars 
              below. 
 
                  ELEMS [ 0 ] = XNDT2O in radians/minute**2 
                  ELEMS [ 1 ] = XNDD6O in radians/minute**3 
                  ELEMS [ 2 ] = BSTAR 
                  ELEMS [ 3 ] = XINCL  in radians 
                  ELEMS [ 4 ] = XNODEO in radians 
                  ELEMS [ 5 ] = EO 
                  ELEMS [ 6 ] = OMEGAO in radians 
                  ELEMS [ 7 ] = XMO    in radians 
                  ELEMS [ 8 ] = XNO    in radians/minute 
                  ELEMS [ 9 ] = EPOCH of the elements in seconds 
                                past ephemeris epoch J2000. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   No checking of the inputs is performed in this routine. However, this
   routine does call other CSPICE routines. If one of these routines
   detects an error it will diagnose it and signal an error.
 
-Files
 
   You must have loaded a SPICE leapseconds kernel into the 
   kernel pool prior to caling this routine. 
 
-Particulars
 
   This routine parses a Space Command Two-line element set and returns
   the orbital elements properly scaled and in units suitable for use
   by other SPICE software.  Input elements look like the following
 
--------------------------------------------------------------------- 
1 22076U 92052A   97173.53461370 -.00000038  00000-0  10000-3 0   594 
2 22076  66.0378 163.4372 0008359 278.7732  81.2337 12.80930736227550 
--------------------------------------------------------------------- 
^ 
123456789012345678901234567890123456789012345678901234567890123456789 
         1         2         3         4         5         6 
 
   The ``raw'' elements in the first and second lines are marked below.
   Note that in several instances exponents and decimal points are
   implied.  Also note that input units are degrees, degrees/day**n and
   revolutions/day.
 
 
                    DAY OF YEAR             NDD60    BSTAR 
                    vvvvvvvvvvvv            vvvvvv   vvvvvv 
--------------------------------------------------------------------- 
1 22076U 92052A   97173.53461370 -.00000038  00000-0  10000-3 0   594 
--------------------------------------------------------------------- 
                  ^^             ^^^^^^^^^^       ^^       ^^ 
                  YEAR             NDT20          IEXP     IBEXP 
 
 
 
   The ``raw'' elements in the second line are marked below 
                 NODE0            OMEGA             N0 
                 vvvvvvvv         vvvvvvvv          vvvvvvvvvvv 
--------------------------------------------------------------------- 
2 22076  66.0378 163.4372 0008359 278.7732  81.2337 12.80930736227550 
--------------------------------------------------------------------- 
        ^^^^^^^^          ^^^^^^^          ^^^^^^^^ 
        Inclination       Eccentricity     M0 
 
   This routine extracts these values ``inserts'' the implied 
   decimal points and exponents and then converts the inputs 
   to units of radians, radians/minute, radians/minute**2, and 
   radians/minute**3 
 
-Examples
 
   Suppose you have a set of two-line elements and an array containing
   the related geophysical constants necessary to evaluate a state.
   The example below shows how you can use this routine together with
   the routine EV2LIN to propagate a state to an epoch of interest.
 
      #include <string.h>
      #include <stdio.h>
      #include "SpiceUsr.h"
      
      SpiceDouble         et;
      SpiceDouble         epoch;
      SpiceInt            frstyr;
          .
          .
          .
      /.
      The parameters below will make it easier to make assignments 
      to the array GEOPHS required by EV2LIN. 
 
         J2  --- location of J2 
         J3  --- location of J3 
         J4  --- location if J4 
         KE  --- location of KE = sqrt(GM) in eart-radii**1.5/MIN 
         QO  --- location of upper bound of atmospheric model in KM 
         SO  --- location of lower bound of atmospheric model in KM 
         ER  --- location of earth equatorial radius in KM. 
         AE  --- location of distance units/earth radius 
      ./
      
      #define  J2 0   
      #define  J3 1   
      #define  J4 2    
      #define  KE 3   
      #define  QO 4   
      #define  SO 5   
      #define  ER 6   
      #define  AE 7  
 
      /.
      We set the lower bound for the years to be the beginning 
      of the space age. 
      ./
      frstyr  =  1957;
 
      /.
      Read in the next two lines from the text file that contains 
      the two-line elements.  We assume that file has been opened 
      properly and that we have set the ``file pointer'' to the 
      correct location for reading the next set of elements. 
      ./
      
      for ( i = 0; i < 2; i++ )
      {
         fgets ( line[i], lineln, textfile );
         line[i][ strlen(line[i]) ] = '\0';
      }

      getelm_c ( frstyr, lineln, line, &epoch, elems ); 
 
 
      /.
      Set up the geophysical quantities.  At last check these 
      were the values used by Space Command. 
      ./
      
      geophs[ J2 ] =    1.082616e-3; 
      geophs[ J3 ] =   -2.53881e-6; 
      geophs[ J4 ] =   -1.65597e-6; 
      geophs[ KE ] =    7.43669161e-2; 
      geophs[ QO ] =  120.0; 
      geophs[ SO ] =   78.0; 
      geophs[ ER ] = 6378.135; 
      geophs[ AE ] =    1.0; 
      
      
      /.
      Now propagate the state using ev2lin_ to the epoch of 
      interest. 
      ./
      ev2lin_ ( &et, geophs, elems, state ); 
 
 
-Restrictions
 
  The format of the two-line elements suffer from a "millenium"
  problem---only two digits are used for the year of the elements. It
  is not clear how Space Command will deal with this problem as the
  year 2000 comes and goes.  We hope that by adjusting the input frstyr
  you should be able to use this routine well into the 21st century.
  However, since we can't predict how others will resolve the millenium
  problem we can't be sure that our approach will be addequate to deal
  with the problem.
 
  The approach taken to mapping the two-digit year to the full year is
  given by the code below. Here, yr is the integer obtained by parsing
  the two-digit year from the first line of the elements.
 
      begyr = (frstyr/100)*100; 
      year  = begyr + yr;
 
      if ( year < frstyr )  
      {
         year += 100;
      }
 
   This mapping will be changed if future two-line element
   representations make this method of computing the full year
   inaccurate.
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL)
   W.L. Taber      (JPL) 
 
-Version

   -CSPICE Version 1.0.1, 15-NOV-2007 (EDW)
   
      Minor edits to example section; the getelm_c call lacked
      the 'lineln' argument, the use of 'et' implied a pointer
      rather than a value.

   -CSPICE Version 1.0.0, 06-AUG-1999 (NJB) (WLT)

-Index_Entries
 
   Parse two-line elements 
 
-&
*/

{ /* Begin getelm_c */


   /*
   Local constants
   */
   #define NELTS           2
   
   
   /*
   Local variables
   */
   SpiceChar            ** cvalsPtr;
   SpiceChar             * fCvalsArr;

   SpiceInt                i;
   SpiceInt                fCvalsLen;

   SpiceStatus             status;

   /*
   Participate in error tracing.
   */
   chkin_c ( "getelm_c" );


   /*
   Check the input line array for null pointer of insufficient string
   length.
   */
   CHKOSTR ( CHK_STANDARD, "getelm_c", lines, lineln );


   /*
   Convert the input string array to a Fortran-style string array.

   We'll first allocate an array of character pointers to index
   the values, initialize this array, and use it to produce
   a dynamically allocated array of Fortran-style strings.
   */

   cvalsPtr = ( SpiceChar ** ) malloc ( NELTS * sizeof(SpiceChar *) );

   if ( cvalsPtr == 0 )
   {
      setmsg_c ( "Failure on malloc call to create pointer array "
                 "for line values."                              );
      sigerr_c ( "SPICE(MALLOCFAILED)"                           );
      chkout_c ( "getelm_c"                                      );
      return;
   }
   
   for ( i = 0;  i < NELTS;  i++  )
   {
      cvalsPtr[i] =  (SpiceChar *)lines  +  ( i * lineln );
   }
   
   status = C2F_CreateStrArr (  NELTS, 
                                ( ConstSpiceChar ** ) cvalsPtr, 
                                &fCvalsLen, 
                                &fCvalsArr                      );
  /* fCvalsArr[2*fCvalsLen] = '\0'; */
   
   if ( status == SPICEFAILURE )
   {
      free ( cvalsPtr );
      
      setmsg_c ( "C to Fortran string array conversion for `lines' "
                 "failed."                                           );
      sigerr_c ( "SPICE(STRINGCONVERROR)"                            );
      chkout_c ( "getelm_c"                                          );
      return;
   }
   
   /*
   Call the f2c'd routine.
   */
   getelm_ (  ( integer    * ) &frstyr,
              ( char       * ) fCvalsArr,
              ( doublereal * ) epoch,
              ( doublereal * ) elems,
              ( ftnlen       ) fCvalsLen  );
   
   /*
   Clean up all of our dynamically allocated arrays.
   */
   free ( cvalsPtr  );
   free ( fCvalsArr );


   chkout_c ( "getelm_c" );

} /* End getelm_c */
