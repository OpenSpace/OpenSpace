##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2026                                                                #
#                                                                                        #
# Permission is hereby granted, free of charge, to any person obtaining a copy of this   #
# software and associated documentation files (the "Software"), to deal in the Software  #
# without restriction, including without limitation the rights to use, copy, modify,     #
# merge, publish, distribute, sublicense, and/or sell copies of the Software, and to     #
# permit persons to whom the Software is furnished to do so, subject to the following    #
# conditions:                                                                            #
#                                                                                        #
# The above copyright notice and this permission notice shall be included in all copies  #
# or substantial portions of the Software.                                               #
#                                                                                        #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,    #
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A          #
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT     #
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF   #
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE   #
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                          #
##########################################################################################

# CPACK_PROJECT_CONFIG_FILE. CPack includes this once per generator, with CPACK_GENERATOR
# set to the one that is about to run, which makes it the only place a per-generator
# decision can be made: everything in CPackConfig.cmake applies to all of them at once.
#
# Only the DEB and RPM packages want the tree relocated into /opt (see the comment on
# OPENSPACE_INSTALL_ROOT in support/cmake/packaging.cmake). Setting
# CPACK_PACKAGING_INSTALL_PREFIX unconditionally instead looks like it works, because the
# archive generators ignore it and keep packing the tree at the root of the archive - but
# the External generator honours it, and stages into
# <staging>/opt/OpenSpace-<version>/bin rather than <staging>/bin. support/cmake/
# appimage.cmake then copies that whole thing into AppDir/usr and linuxdeploy fails with
# "No such file or directory: AppDir/usr/bin/OpenSpace".
#
# Every other generator is left alone rather than being given an explicit "/", so each one
# keeps whatever default it has - which matters on Windows, where NSIS derives the install
# location from CPACK_PACKAGE_INSTALL_DIRECTORY instead.
if (CPACK_GENERATOR STREQUAL "DEB" OR CPACK_GENERATOR STREQUAL "RPM")
  set(CPACK_PACKAGING_INSTALL_PREFIX "${CPACK_OPENSPACE_INSTALL_ROOT}")
endif ()
