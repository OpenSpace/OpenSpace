rem 
rem    mkversn.bat
rem 
rem    Creates version.exe for MS Visual C++ and moves it to the
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

copy version.pgm main.c

for %%f in (*.c) do cl %%f 

dir /b *.obj > temp.lst

link -lib /out:version.lib  @temp.lst

copy main.x version.c

cl version.c

link version.obj version.lib ..\..\lib\csupport.lib ..\..\lib\cspice.lib

move version.exe  ..\..\exe

del *.obj
del version.lib
del temp.lst

