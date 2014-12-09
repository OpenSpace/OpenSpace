rem 
rem    mkcook.bat
rem 
rem    Creates the executables
rem       
rem       simple.exe
rem       states.exe
rem       subpt.exe
rem       tictoc.exe
rem       
rem    for MS Visual C++ and moves them to the
rem    appropriate Toolkit directory.
rem 
rem
rem    Version 1.1.0  19-OCT-2003 (BVS)
rem
rem       added -DNON_ANSI_STDIO compile option.
rem
rem    Version 1.0.0  22-SEP-1999 (NJB) 
rem


set cl= /O2 -D_COMPLEX_DEFINED -DMSDOS -DNON_ANSI_STDIO

copy simple.pgm  simple.c
cl simple.c ..\..\lib\csupport.lib ..\..\lib\cspice.lib

move simple.exe  ..\..\exe


copy states.pgm  states.c
cl states.c ..\..\lib\csupport.lib ..\..\lib\cspice.lib

move states.exe  ..\..\exe


copy tictoc.pgm  tictoc.c
cl tictoc.c ..\..\lib\csupport.lib ..\..\lib\cspice.lib

move tictoc.exe  ..\..\exe


copy subpt.pgm  subpt.c
cl subpt.c ..\..\lib\csupport.lib ..\..\lib\cspice.lib

move subpt.exe  ..\..\exe

rem
rem  Clean up.
rem

del *.c


