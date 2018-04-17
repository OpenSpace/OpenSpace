## Process this file with automake to produce Makefile.in
#
# $Id: Makefile.am,v 3.11 2016/01/14 23:02:38 gordon Exp $
#
# Author: Paul_Kunz@slac.stanford.edu
#
# The following set, else would follow GNU conventions.  Add
# "no-dependencies" if your compiler doesn't support dependency
# generation like gcc does
AUTOMAKE_OPTIONS = foreign # no-dependencies


# Trick to add options to aclocal command
ACLOCAL = aclocal -I config/m4

SUBDIRS = vs.net

## Subsitution of ac_check_package
R_LIB_PATH = @RDFLAGS@

## Subsitution from pfkeb_cxx_lib_path
CXX_LIB_PATH = @CXX_LIB_PATH@

MSVC_FILES = MSconfig.h

EXTRA_DIST = config CHANGES README.INSTALL License.txt file1.pha CMakeLists.txt FindCFITSIO.cmake $(MSVC_FILES)


bin_PROGRAMS = cookbook

cookbook_SOURCES = cookbook.cxx

cookbook_LDADD = libCCfits.la
cookbook_LDFLAGS = -R $(R_LIB_PATH) -R $(CXX_LIB_PATH)

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = CCfits.pc

## The following is needed for compiling cookbook.cxx
INCLUDES = -I$(top_srcdir)/..

## The library to be built
lib_LTLIBRARIES = libCCfits.la

libCCfits_la_SOURCES =				\
	AsciiTable.cxx				\
	BinTable.cxx				\
	Column.cxx				\
	ColumnCreator.cxx			\
	ColumnData.cxx				\
        ColumnVectorData.cxx                    \
        ExtHDU.cxx				\
	FITS.cxx				\
	FITSBase.cxx				\
	FITSUtil.cxx				\
	FitsError.cxx				\
	HDU.cxx					\
	HDUCreator.cxx				\
	KeyData.cxx				\
        Keyword.cxx				\
	KeywordCreator.cxx			\
	PHDU.cxx				\
	Table.cxx

# This will tell shared library which STD C++ library to use without
# needing the user to use LD_LIBRARY_PATH environment variable
libCCfits_la_LIBADD = $(LIBSTDCPP)
libCCfits_la_LDFLAGS = -R $(R_LIB_PATH) -R $(CXX_LIB_PATH)

libCCfits_ladir = $(pkgincludedir)

libCCfits_la_HEADERS =				\
	AsciiTable.h				\
	BinTable.h				\
	CCfits					\
	CCfits.h				\
	Column.h				\
	ColumnT.h				\
	ColumnCreator.h				\
	ColumnData.h				\
	ColumnVectorData.h			\
        ExtHDU.h				\
        ExtHDUT.h                               \
	FITS.h					\
	FITSBase.h				\
	FITSUtil.h				\
	FITSUtilT.h				\
	FitsError.h				\
	HDU.h					\
	HDUCreator.h				\
	Image.h					\
	ImageExt.h				\
	KeyData.h				\
        Keyword.h				\
        KeywordT.h				\
	KeywordCreator.h			\
	NewKeyword.h				\
	PHDU.h					\
        PHDUT.h                                 \
	PrimaryHDU.h				\
	Table.h

run_cookbook:
	rm -rf *.fit
	cookbook.C

## The following is for Mac OS X.  
## It has not yet been tested.
## This workaround was contributed by Chris Jones
## <cdj@mail.lns.cornell.edu>

bundle:
	c++ -bundle -I. -DHAVE_STRSTREAM -o libCCfits.a *.cxx

## commented out in configure.in INCLUDES = -I@STLPORT@

docs:
	doxygen Doxyfile

clean-local:
	-rm -rf SunWS_cache
