rem 
rem    mkcspkmrg.bat
rem 
rem    Creates spkmerge.exe for MS Visual C++ and moves it to the
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

copy spkmerge.pgm main.c

for %%f in (*.c) do cl %%f 

dir /b *.obj > temp.lst

link -lib /out:spkmerge.lib  @temp.lst

copy main.x spkmerge.c

cl spkmerge.c

link spkmerge.obj spkmerge.lib ..\..\lib\csupport.lib ..\..\lib\cspice.lib

move spkmerge.exe  ..\..\exe

del *.obj
del spkmerge.lib
del temp.lst

