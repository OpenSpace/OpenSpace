rem 
rem    mkchrnos.bat
rem 
rem    Creates chronos.exe for MS Visual C++ and moves it to the
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

copy chronos.pgm main.c

for %%f in (*.c) do cl %%f 

dir /b *.obj > temp.lst

link -lib /out:chronos.lib  @temp.lst

copy main.x chronos.c

cl chronos.c

link /STACK:16000000 chronos.obj chronos.lib ..\..\lib\csupport.lib ..\..\lib\cspice.lib

move chronos.exe  ..\..\exe

del *.obj
del chronos.lib
del temp.lst

