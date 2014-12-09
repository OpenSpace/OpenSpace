rem 
rem    mktobin.bat
rem 
rem    Creates tobin.exe for MS Visual C++ and moves it to the
rem    appropriate Toolkit directory.
rem 
rem
rem    Version 1.1.0  19-OCT-2003 (BVS)
rem
rem       added -DNON_ANSI_STDIO compile option.
rem
rem    Version 1.0.0  29-DEC-1998 (NJB) 
rem


set cl= /c /O2 -D_COMPLEX_DEFINED -DMSDOS -DNON_ANSI_STDIO

copy tobin.pgm main.c

for %%f in (*.c) do cl %%f 

dir /b *.obj > temp.lst

link -lib /out:tobin.lib  @temp.lst

copy main.x tobin.c

cl tobin.c

link tobin.obj tobin.lib ..\..\lib\csupport.lib ..\..\lib\cspice.lib

move tobin.exe  ..\..\exe

del *.obj
del tobin.lib
del temp.lst

