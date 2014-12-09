rem 
rem    mkckbref.bat
rem 
rem    Creates ckbrief.exe for MS Visual C++ and moves it to the
rem    appropriate Toolkit directory.
rem 
rem
rem    Version 1.1.0  19-OCT-2003 (BVS)
rem
rem       added -DNON_ANSI_STDIO compile option.
rem
rem    Version 1.0.0  06-SEP-1999 (NJB) 
rem 
rem       Adapted from mkinspkt.bat.
rem
rem


set cl= /c /O2 -D_COMPLEX_DEFINED -DMSDOS -DNON_ANSI_STDIO

copy ckbrief.pgm main.c

for %%f in (*.c) do cl %%f 

dir /b *.obj > temp.lst

link -lib /out:ckbrief.lib  @temp.lst

copy main.x ckbrief.c

cl ckbrief.c

link /STACK:16000000 ckbrief.obj ckbrief.lib ..\..\lib\csupport.lib ..\..\lib\cspice.lib

move ckbrief.exe  ..\..\exe

del *.obj
del ckbrief.lib
del temp.lst

