/*

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

*/

/* STARTUP PROCEDURE FOR UNIX FORTRAN PROGRAMS */

/*
   This is the main.c file from the f2c libF77 set of source files,
   modified for use within CSPICE.  The changes made were:
   
      - The file SpiceUsr.h is included.
      
      - A call to putcml_c is made to store the command line arguments
        and make them accessible via getcml_ or getcml_c.
        
   This file is intended to be used as the main function for all
   programs in the CSPICE Toolkit.  
   
   
-Version

   -CSPICE Version 2.0.0, 19-DEC-2001 (NJB)

      Updated to support linking CSPICE into executables that
      also link in objects compiled from Fortran, in particular
      ones that perform Fortran I/O.  To enable this odd mix,
      one defines the  preprocessor flag

         MIX_C_AND_FORTRAN
 
      This macro is undefined by default, since the action it invokes
      is usually not desirable.  See the header 

         f2cMang.h

      for further information.


   -CSPICE Version 1.1.0 29-FEB-2000 (NJB)
   
      Updated to use the main.c source code from a newer version of
      f2c, dated 1998-09-13.

      
   -CSPICE Version 1.0.0 28-OCT-1998 (NJB)
   
*/


/*
Optionally include name-mangling macros for f2c external symbols.
*/
#ifdef MIX_C_AND_FORTRAN
   #include "f2cMang.h"
#endif


/*
This header file is included for use within CSPICE.
*/
#include "SpiceUsr.h"

#include "stdio.h"
#include "signal1.h"

#ifndef SIGIOT
#ifdef SIGABRT
#define SIGIOT SIGABRT
#endif
#endif

#ifndef KR_headers
#undef VOID
#include "stdlib.h"
#endif

#ifndef VOID
#define VOID void
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef NO__STDC
#define ONEXIT onexit
extern VOID f_exit();
#else
#ifndef KR_headers
extern void f_exit(void);
#ifndef NO_ONEXIT
#define ONEXIT atexit
extern int atexit(void (*)(void));
#endif
#else
#ifndef NO_ONEXIT
#define ONEXIT onexit
extern VOID f_exit();
#endif
#endif
#endif

#ifdef KR_headers
extern VOID f_init(), sig_die();
extern int MAIN__();
#define Int /* int */
#else
extern void f_init(void), sig_die(char*, int);
extern int MAIN__(void);
#define Int int
#endif

static VOID sigfdie(Sigarg)
{
Use_Sigarg;
sig_die("Floating Exception", 1);
}


static VOID sigidie(Sigarg)
{
Use_Sigarg;
sig_die("IOT Trap", 1);
}

#ifdef SIGQUIT
static VOID sigqdie(Sigarg)
{
Use_Sigarg;
sig_die("Quit signal", 1);
}
#endif


static VOID sigindie(Sigarg)
{
Use_Sigarg;
sig_die("Interrupt", 0);
}

static VOID sigtdie(Sigarg)
{
Use_Sigarg;
sig_die("Killed", 0);
}

#ifdef SIGTRAP
static VOID sigtrdie(Sigarg)
{
Use_Sigarg;
sig_die("Trace trap", 1);
}
#endif


int xargc;
char **xargv;

#ifdef __cplusplus
   }
#endif

#ifdef KR_headers
main(argc, argv) int argc; char **argv;
#else
main(int argc, char **argv)
#endif
{
   /*
   This call was added for use within CSPICE.  It did not appear in the
   original main.c function.
   */
   putcml_c ( argc, argv );

xargc = argc;
xargv = argv;
signal1(SIGFPE, sigfdie);  /* ignore underflow, enable overflow */
#ifdef SIGIOT
signal1(SIGIOT, sigidie);
#endif
#ifdef SIGTRAP
signal1(SIGTRAP, sigtrdie);
#endif
#ifdef SIGQUIT
if(signal1(SIGQUIT,sigqdie) == SIG_IGN)
   signal1(SIGQUIT, SIG_IGN);
#endif
if(signal1(SIGINT, sigindie) == SIG_IGN)
   signal1(SIGINT, SIG_IGN);
signal1(SIGTERM,sigtdie);

#ifdef pdp11
   ldfps(01200); /* detect overflow as an exception */
#endif

f_init();
#ifndef NO_ONEXIT
ONEXIT(f_exit);
#endif
MAIN__();
#ifdef NO_ONEXIT
f_exit();
#endif
exit(0); /* exit(0) rather than return(0) to bypass Cray bug */
return 0;   /* For compilers that complain of missing return values; */
      /* others will complain that this is unreachable code. */
}
