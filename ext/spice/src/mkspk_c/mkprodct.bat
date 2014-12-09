rem 
rem    mkmkspk.bat
rem 
rem    Creates mkspk.exe for MS Visual C++ and moves it to the
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

copy mkspk.pgm main.c

for %%f in (*.c) do cl %%f 

dir /b *.obj > temp.lst

link -lib /out:mkspk.lib  @temp.lst

copy main.x mkspk.c

cl mkspk.c

link mkspk.obj mkspk.lib ..\..\lib\csupport.lib ..\..\lib\cspice.lib

move mkspk.exe  ..\..\exe

del *.obj
del mkspk.lib
del temp.lst

