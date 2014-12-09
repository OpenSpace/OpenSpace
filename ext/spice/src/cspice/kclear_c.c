/*

-Procedure kclear_c ( Keeper clear )

-Abstract
 
   Clear the KEEPER subsystem: unload all kernels, clear the kernel
   pool, and re-initialize the subsystem. Existing watches on kernel
   variables are retained.
 
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
 
   KERNEL 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void kclear_c ( void ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   None. 
 
-Detailed_Input
 
   None.  This routine operates by side effects.  See Particulars 
   below. 
 
-Detailed_Output
 
   None. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) Any errors that occur when setting a kernel pool watch 
      or checking watched variables will be diagnosed by 
      routines in the call tree of this routine. 
 
-Files
 
   See Particulars. 
 
-Particulars
 
   This entry point allows you re-initialize the KEEPER system with 
   a single call.  The KEEPER system is the kernel management system
   underlying the set of CSPICE APIs

      furnsh_c
      ktotal_c
      kdata_c
      kinfo_c
      kclear_c
      unload_c
  
   This routine unloads all kernels from their kernel-type-specific 
   kernel management subsystems (SPKBSR, CKBSR, etc.), clears the 
   kernel pool, clears KEEPER's internal file database, and re-sets 
   the watch status for the kernel variables used to load kernels 
   via meta-kernels. As a side effect of clearing the kernel pool,
   all watched variables are marked as updated. Note that clearing
   the kernel pool does not delete watchers.
 
   This capability, though implemented in Fortran, is particularly 
   relevant to SPICE implementations such as Icy, for which the 
   state of the KEEPER system persists after any Icy-based IDL 
   script is run. Successive runs of Icy-based scripts may perform 
   in unexpected ways when scripts access data loaded during runs of 
   previous scripts. 
    
   Cleaning up after such programs using explicit unload_c commands is 
   tedious and error-prone.  One call to this routine sets the 
   KEEPER system to its initial state, preventing unintentional 
   interaction between scripts via KEEPER's state. 
 
-Examples
 
   Clear the KEEPER system; check for residual loaded files. 
   We shouldn't find any. 
 
       kclear_c ();
       ktotal_c ( "ALL", &n );

       printf ( "Count of loaded kernels after kclear_c call\n", n );

 
-Restrictions
 
   Calling this routine will wipe out any kernel pool data 
   inserted via the p*pool_c API routines. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
 
-Version
 
   -CSPICE Version 1.0.1, 01-JUL-2014 (NJB)

      The header Particulars section was updated to more
      completely describe the effect of this routine on
      kernel pool watchers. Header section order was corrected.

   -CSPICE Version 1.0.0, 15-NOV-2006 (NJB)

-Index_Entries
 
   Re-initialize the keeper system 
   Clear the keeper system 
   Unload all kernels 
 
-&
*/

{ /* Begin kclear_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "kclear_c" );


   kclear_();


   chkout_c ( "kclear_c" );

} /* End kclear_c */
