rem 
rem    mkfrmdiff.bat
rem 
rem    Creates frmdiff.exe for MS Visual C++ and moves it to the
rem    appropriate Toolkit directory.
rem 
rem
rem    Version 1.1.0  19-OCT-2003 (BVS)
rem
rem       added -DNON_ANSI_STDIO compile option.
rem
rem    Version 1.0.0  25-Feb-2000 (NJB) 
rem


set cl= /c /O2 -D_COMPLEX_DEFINED -DMSDOS -DNON_ANSI_STDIO

copy frmdiff.pgm main.c

for %%f in (*.c) do cl %%f 

dir /b *.obj > temp.lst

link -lib /out:frmdiff.lib  @temp.lst

copy main.x frmdiff.c

cl frmdiff.c

link frmdiff.obj frmdiff.lib ..\..\lib\csupport.lib ..\..\lib\cspice.lib

move frmdiff.exe  ..\..\exe

del *.obj
del frmdiff.lib
del temp.lst

