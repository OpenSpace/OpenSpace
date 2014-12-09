rem 
rem    mkspkdiff.bat
rem 
rem    Creates spkdiff.exe for MS Visual C++ and moves it to the
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

copy spkdiff.pgm main.c

for %%f in (*.c) do cl %%f 

dir /b *.obj > temp.lst

link -lib /out:spkdiff.lib  @temp.lst

copy main.x spkdiff.c

cl spkdiff.c

link spkdiff.obj spkdiff.lib ..\..\lib\csupport.lib ..\..\lib\cspice.lib

move spkdiff.exe  ..\..\exe

del *.obj
del spkdiff.lib
del temp.lst

