#! /bin/csh
#
#   PC-LINUX 64bit version.
#
#   This script is a more or less generic library/executable
#   builder for CSPICE products.  It assumes that it is executed
#   from one of the "product" directories in a tree that looks like
#   the one displayed below:
#
#                      package
#                         |
#                         |
#       +------+------+------+------+------+
#       |      |      |      |      |      | 
#     data    doc    etc    exe    lib    src
#                                          |
#                                          |
#                         +----------+----------+------- ... ------+
#                         |          |          |                  |
#                     product_1  product_2  product_3    ...   product_n
#
#   Here's the basic strategy:
#
#     1)  Compile all of the .c files in the current directory
#
#     2)  If there are no .pgm files in the current directory this
#         is assumed to be a library source directory.  The name
#         of the library is the same as the name of the product.
#         The library is placed in the "lib" directory in the tree
#         above.  The script is then done.
# 
#         If there are .pgm files and there were some .c
#         files compiled the objects are gathered together in the
#         current directory into a library called locallib.a.
#
#     3)  If any *.pgm files exist in the current directory, compile 
#         them and add their objects to locallib.a.  Create a C main
#         program file from the uniform CSPICE main program main.x.
#         Compile this main program and link its object with locallib.a,
#         ../../cspice.a and ../../csupport.a. The output 
#         executables have an empty extension.  The executables are
#         placed in the "exe" directory in the tree above.
#         
#   The environment variable TKCOMPILEOPTIONS containing compile options 
#   is optionally set. If it is set prior to executing this script,
#   those options are used. It it is not set, it is set within this
#   script as a local variable.
#
#   References:
#   ===========
#
#   "Unix Power Tools", page 11.02 
#      Use the "\" character to unalias a command temporarily.
#        
#   "A Practical Guide to the Unix System"
#
#   "The Unix C Shell Field Guide"
#
#   Change History:
#   ===============
#
#   Version 6.2.0  Feb. 14, 2008  Boris Semenov 
#
#      Added -fPIC option.
#
#   Version 6.1.0  November 13, 2006  Boris Semenov
#
#      Updated for 64bit. Put -O2 back in.
#
#   Version 6.0.0  April 20, 2000  Bill Taber
#
#      Removed O2 optimization as it caused some loops to 
#      not terminate.    
#
#   Version 5.0.0  Feb. 09, 1999  Nat Bachman
#
#      Now uses O2 optimization.      
#
#   Version 4.0.0  Nov. 02, 1998  Nat Bachman
#
#      Updated to use an environment variable to designate the C
#      compiler to use.
#
#   Version 3.0.0  Oct. 31, 1998  Nat Bachman
#
#      Updated to make use of uniform C main routine main.x.
#
#   Version 2.0.0  Feb. 04, 1998  Nat Bachman
#
#      Modified to handle C code.  Sun/Solaris/Native cc Version.
#
#   Version 1.0.0  Dec  8, 1995  Bill Taber
#


#
#   If there are any main programs in the directory, prepare them
#   for use together with the "uniform" main.x routine.  We copy
#   each main program to a file whose name terminates in _main.c.
#   We then make a copy of main.x having its name made of the tail of
#   the original .pgm file and an extension of .px.  When we compile
#   the main programs, we'll look for this .px extension rather than
#   the orginal .pgm.
#
\ls *.pgm >& /dev/null

if ( $status == 0 ) then

   echo " "

   foreach MAIN ( *.pgm )
   
#     
#     Copy the orginal source file for the main program into a regular
#     source file which will be included in the local library.
#
#     Create a "main" source file having the name <product>.px
#     from the generic main program source file main.x.
#
      set STEM    = $MAIN:r
      set TARGET  = $STEM.px
      
      \cp $MAIN  "$STEM"_main.c
      \cp main.x  $TARGET

endif


#
#  Choose your compiler.
#
if ( $?TKCOMPILER ) then

   echo " "
   echo "      Using compiler: "
   echo "      $TKCOMPILER"

else

   set TKCOMPILER  =  "gcc"
   echo " "
   echo "      Setting default compiler:"
   echo $TKCOMPILER
   
endif


#
#  What compile options do we want to use? If they were 
#  set somewhere else, use those values.  The same goes
#  for link options.
#
if ( $?TKCOMPILEOPTIONS ) then
   echo " "
   echo "      Using compile options: "
   echo "      $TKCOMPILEOPTIONS"
else
#
#  Options:
#
#     -ansi              Compile source as ANSI C
#
#     -DNON_UNIX_STDIO   Don't assume standard Unix stdio.h 
#                        implementation
#
#     -fPIC              position-independent code
#
   set TKCOMPILEOPTIONS = "-c -ansi -m64 -O2 -fPIC -DNON_UNIX_STDIO"
   echo " "
   echo "      Setting default compile options:"
   echo "      $TKCOMPILEOPTIONS"
endif

if ( $?TKLINKOPTIONS ) then
   echo " "
   echo "      Using link options: "
   echo "      $TKLINKOPTIONS"
else
   set TKLINKOPTIONS = "-lm -m64"
   echo " "
   echo "      Setting default link options:"
   echo "      $TKLINKOPTIONS"
endif

echo " "

#
#   Determine a provisional LIBRARY name.
#
   foreach item ( `pwd` )
      set LIBRARY = "../../lib/"$item:t
   end

#
#  Are there any *.c files that need to be compiled?
#
\ls *.c >& /dev/null

if ( $status == 0 ) then

   foreach SRCFILE ( *.c )
      echo "      Compiling: "   $SRCFILE
      $TKCOMPILER $TKCOMPILEOPTIONS $SRCFILE
   end

endif


echo " "

#
#  If object files exist, we need to create an object library.
#

\ls *.pgm >& /dev/null

if ( $status == 0 ) then
   set LIBRARY = "locallib"
endif

\ls *.o >& /dev/null

if ( $status == 0 ) then

   echo "      Inserting objects in the library $LIBRARY ..."
   ar  crv $LIBRARY.a *.o
   ranlib  $LIBRARY.a
   \rm                *.o    
   echo " "

endif

#
#  If there are any main programs in the directory, compile
#  them. If they have their own locallib.a link with it in addition
#  to the default libraries.
#

\ls *.pgm >& /dev/null

if ( $status == 0 ) then

   echo " "

   foreach MAIN ( *.px )
   
      set STEM    = $MAIN:r
      set TARGET  = $STEM.c
      set MAINOBJ = $STEM.o
      set EXECUT = ../../exe/$STEM
   
      cp $MAIN $TARGET
   
      echo "      Compiling and linking: " $MAIN
      
      if ( -e locallib.a ) then

         $TKCOMPILER    $TKCOMPILEOPTIONS $TARGET
         $TKCOMPILER -o $EXECUT           $MAINOBJ             \
                                          locallib.a           \
                                          ../../lib/csupport.a \
                                          ../../lib/cspice.a   \
                                          $TKLINKOPTIONS    

         \rm $TARGET
         \rm $MAINOBJ
         \rm locallib.a 

      else

         echo "Compiling and linking: "   $MAIN     
         $TKCOMPILER    $TKCOMPILEOPTIONS $TARGET
         $TKCOMPILER -o $EXECUT           $MAINOBJ             \
                                          ../../lib/csupport.a \
                                          ../../lib/cspice.a   \
                                         $TKLINKOPTIONS
 
         \rm $TARGET
         \rm $MAINOBJ

      endif

   end

endif

#
#  Cleanup.
#

echo " "

\ls *.o >& /dev/null

if ( $status == 0 ) then
   \rm *.o
endif

\ls *.px >& /dev/null

if ( $status == 0 ) then
   \rm *.px
endif

\ls *_main.c >& /dev/null

if ( $status == 0 ) then
   \rm *_main.c
endif


exit 0

   
