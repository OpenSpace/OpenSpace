/*

-Procedure dascls_c ( DAS, close file )

-Abstract
 
   Close a DAS file. 
 
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
 
   DAS 
 
-Keywords
 
   DAS 
   FILES 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"

   void dascls_c ( SpiceInt handle ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   handle     I   Handle of an open DAS file. 
   FTSIZE     P   Maximum number of simultaneously open DAS files. 
 
-Detailed_Input
 
   handle         is the file handle of an open DAS file. 
 
-Detailed_Output
 
   None.  See $Particulars for a description of the effect of this 
   routine. 
 
-Parameters
 
   FTSIZE         is the maximum number of DAS files that can be 
                  open at any one time.  See the file dasfm.c
                  for details.
 
-Exceptions
 
   Error free. 
 
   1)  If `handle' is not the handle of an open DAS file, no error 
       is signaled. 
 
-Files
 
   See the description of input argument `handle' in $Detailed_Input. 
 
-Particulars
 
   This routine provides the primary recommended method of closing an
   open DAS file.  It is also possible to close a DAS file without
   segregating it by calling daswbr_ and dasllc_. Closing a DAS file by
   any other means may cause the DAS mechanism for keeping track of
   which files are open to fail.  Closing a DAS file that has been
   opened for writing by any other means may result in the production
   of something other than a DAS file.
 
-Examples
 
   1)  Open a new DAS file called TEST.DAS, add 100 d.p. numbers 
       to it, and then close the file. 
 
          #include "SpiceUsr.h"
          #include "SpiceZfc.h"
          #include <string.h>

          int main()
          {
             #define  NMAX           100 

             SpiceChar             * fname;
             SpiceChar             * ftype;
             SpiceChar             * ifname;

             SpiceDouble             ddata  [ NMAX ];

             SpiceInt                handle;
             SpiceInt                i;
             SpiceInt                n;
             SpiceInt                ncomch;


             /.
             We'll give the file the same internal file name
             as the file's actual name.  We don't require any
             comment records.
             ./
             fname  = "TEST.DAS";
             ftype  = "TEST";
             ifname = fname;
             ncomch = 0;

             dasonw_ ( (SpiceChar *) fname,
                       (SpiceChar *) ftype,
                       (SpiceChar *) ifname,
                       (integer   *) &ncomch,
                       (integer   *) &handle,
                       (ftnlen     ) strlen(fname),        
                       (ftnlen     ) strlen(ftype),        
                       (ftnlen     ) strlen(ifname)  );

    
             for ( i = 0;  i < NMAX;  i++ )
             {
                 ddata[i] = (SpiceDouble)i; 
             }

             n = NMAX;

             dasadd_ ( &handle, &n, ddata );

             dascls_c ( handle );

             return ( 0 );
          }       
          
    
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
   K.R. Gehringer (JPL) 
   W.L. Taber     (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 05-OCT-2006 (NJB) (KRG) (WLT)

-Index_Entries
 
   close a DAS file 
 
-&
*/

{ /* Begin dascls_c */


   
   /*
   Participate in error tracing.
   */

   chkin_c ( "dascls_c" );

   /*
   Call the f2c'd Fortran routine.  Use explicit type casts for every
   type defined by f2c.
   */
   dascls_ (  ( integer * ) &handle );


   chkout_c ( "dascls_c" );

} /* End dascls_c */

