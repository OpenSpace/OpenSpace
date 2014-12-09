rem 
rem    mkcbrief.bat
rem 
rem    Creates brief.exe for MS Visual C++ and moves it to the
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

copy brief.pgm main.c

for %%f in (*.c) do cl %%f 

dir /b *.obj > temp.lst

link -lib /out:brief.lib  @temp.lst

copy main.x brief.c

cl brief.c

link /STACK:16000000 brief.obj brief.lib ..\..\lib\csupport.lib ..\..\lib\cspice.lib

move brief.exe  ..\..\exe

del *.obj
del brief.lib
del temp.lst

