rem 
rem    mkcspice.bat
rem 
rem    Creates cspice.lib for MS Visual C++ and moves it to the
rem    appropriate Toolkit directory.
rem 
rem
rem    Version 3.1.0  19-OCT-2003 (BVS)
rem
rem       added -DNON_ANSI_STDIO compile option.
rem
rem    Version 3.0.0  03-NOV-1999 (NJB)
rem
rem       fixed the last "set cl" command.
rem
rem    Version 2.0.0  26-FEB-1999 (NJB) 
rem
rem      Added OMIT_BLANK_CC preprocessor flag.
rem
rem    Version 1.0.0  29-DEC-1998 (NJB) 
rem

set cl= /c /O2 -D_COMPLEX_DEFINED -DMSDOS -DOMIT_BLANK_CC -DNON_ANSI_STDIO

rem 
rem  The optimization algorithm has a very tough time with zzsecptr.c,
rem  so exempt this routine from optimization.
rem 

rename zzsecprt.c zzsecprt.x

rem
rem  Compile everything else.
rem

for %%f in (*.c) do cl %%f 

rem
rem  Set the cl variable to omit optimization.  Compile zzsecprt.c.
rem 

set cl= /c -D_COMPLEX_DEFINED -DMSDOS -DOMIT_BLANK_CC

rename zzsecprt.x zzsecprt.c

cl zzsecprt.c 

dir /b *.obj > temp.lst

link -lib /out:cspice.lib  @temp.lst

move cspice.lib  ..\..\lib

del *.obj

del temp.lst

