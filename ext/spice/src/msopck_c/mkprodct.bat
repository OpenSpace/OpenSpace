rem 
rem    mkmsopck.bat
rem 
rem    Creates msopck.exe for MS Visual C++ and moves it to the
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

copy msopck.pgm main.c

for %%f in (*.c) do cl %%f 

dir /b *.obj > temp.lst

link -lib /out:msopck.lib  @temp.lst

copy main.x msopck.c

cl msopck.c

link msopck.obj msopck.lib ..\..\lib\csupport.lib ..\..\lib\cspice.lib

move msopck.exe  ..\..\exe

del *.obj
del msopck.lib
del temp.lst

