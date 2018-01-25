# Find the CFITSIO library
#
# This module defines these variables:
#
#   CFITSIO_FOUND
#      True if the CFITSIO library was found.
#   CFITSIO_LIBRARY
#      The location of the CFITSIO library.
#   CFITSIO_INCLUDE_DIR
#      The include path of the CFITSIO library. 

#
# Find the header file
#
FIND_PATH(CFITSIO_INCLUDE_DIR fitsio.h)

#
# Find the library
#
FIND_LIBRARY(CFITSIO_LIBRARY cfitsio)

SET(CFITSIO_FOUND false)
IF(CFITSIO_INCLUDE_DIR AND CFITSIO_LIBRARY)
   SET(CFITSIO_FOUND true)
ENDIF(CFITSIO_INCLUDE_DIR AND CFITSIO_LIBRARY)

IF(CFITSIO_FOUND)
   MESSAGE(STATUS "Found CFITSIO: ${CFITSIO_LIBRARY}")
ELSE(CFITSIO_FOUND)
   MESSAGE(FATAL_ERROR "Could not find the CFITSIO library")
ENDIF(CFITSIO_FOUND)
