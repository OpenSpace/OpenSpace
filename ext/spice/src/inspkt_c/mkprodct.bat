rem 
rem    mkinspkt.bat
rem 
rem    Creates inspekt.exe for MS Visual C++ and moves it to the
rem    appropriate Toolkit directory.
rem 
rem
rem    Version 2.1.0  19-OCT-2003 (BVS)
rem
rem       added -DNON_ANSI_STDIO compile option.
rem
rem    Version 2.0.0  02-MAR-1999 (NJB) 
rem 
rem       Added link option to set stack size to 16Mb.
rem
rem    Version 1.0.0  29-DEC-1998 (NJB) 
rem


set cl= /c /O2 -D_COMPLEX_DEFINED -DMSDOS -DNON_ANSI_STDIO

copy inspekt.pgm main.c

for %%f in (*.c) do cl %%f 

dir /b *.obj > temp.lst

link -lib /out:inspekt.lib  @temp.lst

copy main.x inspekt.c

cl inspekt.c

link /STACK:16000000 inspekt.obj inspekt.lib ..\..\lib\csupport.lib ..\..\lib\cspice.lib

move inspekt.exe  ..\..\exe

del *.obj
del inspekt.lib
del temp.lst

