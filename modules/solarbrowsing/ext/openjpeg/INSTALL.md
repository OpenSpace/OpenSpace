
# OpenJPEG installation

The build method maintained by OpenJPEG is [CMake](https://cmake.org/).

## UNIX/LINUX - MacOS (terminal) - WINDOWS (cygwin, MinGW)

To build the library, type from source tree directory:
```
mkdir build
cd build
cmake ..
make
```
Binaries are then located in the 'bin' directory.

To install the library, type with root privileges:
```
make install
make clean
```

To build the html documentation, you need doxygen to be installed on your system.
It will create an "html" directory in TOP\_LEVEL/build/doc)
```
make doc
```

Main available cmake flags:
  * To specify the install path: '-DCMAKE\_INSTALL\_PREFIX=/path'
  * To build the shared libraries and links the executables against it: '-DBUILD\_SHARED\_LIBS:bool=on' (default: 'ON')
> Note: when using this option, static libraries are not built and executables are dynamically linked.
  * To build the CODEC executables: '-DBUILD\_CODEC:bool=on' (default: 'ON')
  * [OBSOLETE] To build the MJ2 executables: '-DBUILD\_MJ2:bool=on' (default: 'OFF')
  * [OBSOLETE] To build the JPWL executables and JPWL library: '-DBUILD\_JPWL:bool=on' (default: 'OFF')
  * [OBSOLETE] To build the JPIP client (java compiler recommended) library and executables: '-DBUILD\_JPIP:bool=on' (default: 'OFF')
  * [OBSOLETE] To build the JPIP server (need fcgi) library and executables: '-DBUILD\_JPIP\_SERVER:bool=on' (default: 'OFF')
  * To enable testing (and automatic result upload to http://my.cdash.org/index.php?project=OPENJPEG):
```
cmake . -DBUILD_TESTING:BOOL=ON -DOPJ_DATA_ROOT:PATH='path/to/the/data/directory' -DBUILDNAME:STRING='name_of_the_build'
make
make Experimental
```
Note : test data is available on the following github repo: https://github.com/uclouvain/openjpeg-data

If '-DOPJ\_DATA\_ROOT:PATH' option is omitted, test files will be automatically searched in '${CMAKE\_SOURCE\_DIR}/../data'.

Note 2 : to execute the encoding test suite, kakadu binaries are needed to decode encoded image and compare it to the baseline. Kakadu binaries are freely available for non-commercial purposes at http://www.kakadusoftware.com. kdu\_expand will need to be in your PATH for cmake to find it.

Note 3 : OpenJPEG encoder and decoder (not the library itself !) depends on several libraries: png, tiff, lcms, z. If these libraries are not found on the system, they are automatically built from the versions available in the source tree. You can force the use of these embedded version with BUILD\_THIRDPARTY:BOOL=ON. On a Debian-like system you can also simply install these libraries with:
```
sudo apt-get install liblcms2-dev  libtiff-dev libpng-dev libz-dev
```

Note 4 : On MacOS, if it does not work, try adding the following flag to the cmake command :
```
-DCMAKE_OSX_ARCHITECTURES:STRING=i386
```

## MacOS (XCode) - WINDOWS (VisualStudio, etc)

You can use cmake to generate the project files for the IDE you are using (VC2010, XCode, etc).
Type 'cmake --help' for available generators on your platform.

# Using OpenJPEG

To use openjpeg exported cmake file, simply create your application doing:

```
$ cat CMakeLists.txt
find_package(OpenJPEG REQUIRED)
include_directories(${OPENJPEG_INCLUDE_DIRS})
add_executable(myapp myapp.c)
target_link_libraries(myapp ${OPENJPEG_LIBRARIES})
```
