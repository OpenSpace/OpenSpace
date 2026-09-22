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

# CPACK_EXTERNAL_PACKAGE_SCRIPT for the AppImage generator. CPack runs this in script mode
# after it has staged the install tree, with every CPACK_* variable from CPackConfig.cmake
# in scope. Because CPACK_MONOLITHIC_INSTALL is on, CPACK_TEMPORARY_DIRECTORY holds the
# complete install tree in its packaged layout: bin/, data/, config/, modules/, scripts/,
# shaders/ and openspace.cfg side by side.
#
# CPack has no AppImage generator, so the actual work is done by linuxdeploy: it walks the
# ELF dependencies of the executable, copies the non-system libraries into the AppDir,
# patches their RPATHs, deploys the Qt plugins through its "qt" plugin and finally calls
# appimagetool. See support/cmake/packaging.cmake for where the tools are located.

if (NOT CPACK_OPENSPACE_LINUXDEPLOY_EXECUTABLE OR
    NOT CPACK_OPENSPACE_LINUXDEPLOY_PLUGIN_QT OR
    NOT CPACK_OPENSPACE_APPIMAGETOOL_EXECUTABLE)
  message(FATAL_ERROR
    "Building an AppImage requires linuxdeploy, its Qt plugin, and appimagetool to be "
    "available. Download them from\n"
    "  https://github.com/linuxdeploy/linuxdeploy/releases\n"
    "  https://github.com/linuxdeploy/linuxdeploy-plugin-qt/releases\n"
    "  https://github.com/AppImage/appimagetool/releases\n"
    "make them executable, place them on the PATH, and configure again. Found so far:\n"
    "  linuxdeploy:            ${CPACK_OPENSPACE_LINUXDEPLOY_EXECUTABLE}\n"
    "  linuxdeploy-plugin-qt:  ${CPACK_OPENSPACE_LINUXDEPLOY_PLUGIN_QT}\n"
    "  appimagetool:           ${CPACK_OPENSPACE_APPIMAGETOOL_EXECUTABLE}"
  )
endif ()

set(APPDIR "${CPACK_TOPLEVEL_DIRECTORY}/AppDir")
set(APPIMAGE_NAME "${CPACK_PACKAGE_FILE_NAME}.AppImage")

message(STATUS "Assembling AppDir in '${APPDIR}'")
file(REMOVE_RECURSE "${APPDIR}")
file(MAKE_DIRECTORY "${APPDIR}/usr")

# The whole staged tree becomes AppDir/usr, which puts openspace.cfg one directory above
# the executable. That is what findConfiguration() expects when it walks up from ${BIN},
# and it is the layout that the AppRun re-creates through symlinks in a writable directory
file(COPY "${CPACK_TEMPORARY_DIRECTORY}/" DESTINATION "${APPDIR}/usr")

# The tools are AppImages themselves, and APPIMAGE_EXTRACT_AND_RUN is what lets them run
# inside containers and CI images that have no FUSE
set(APPIMAGE_ENVIRONMENT "APPIMAGE_EXTRACT_AND_RUN=1")
if (CPACK_OPENSPACE_QMAKE_EXECUTABLE)
  # Without this the Qt plugin uses whatever qmake happens to be on the PATH, which on a
  # machine with an unrelated Qt SDK installed deploys plugins from a different Qt build
  # than the one OpenSpace is linked against. That mismatch only shows up at runtime as a
  # refusal to load the QPA plugin
  list(APPEND APPIMAGE_ENVIRONMENT "QMAKE=${CPACK_OPENSPACE_QMAKE_EXECUTABLE}")
endif ()
if (CPACK_OPENSPACE_QT_ROOT)
  list(APPEND APPIMAGE_ENVIRONMENT "QTDIR=${CPACK_OPENSPACE_QT_ROOT}")
endif ()

# See CPACK_OPENSPACE_APPIMAGE_FORCED_LIBRARIES in support/cmake/packaging.cmake for why
# some libraries have to be deployed explicitly
set(FORCED_LIBRARY_ARGUMENTS "")
foreach (library ${CPACK_OPENSPACE_APPIMAGE_FORCED_LIBRARIES})
  list(APPEND FORCED_LIBRARY_ARGUMENTS --library "${library}")
endforeach ()

# Deploying and packing are two separate steps rather than one linuxdeploy call with
# --output appimage, because the Qt libraries have to be removed from the AppDir in
# between. See the cleanup below for why
message(STATUS "Deploying dependencies into the AppDir")
execute_process(
  COMMAND ${CMAKE_COMMAND} -E env ${APPIMAGE_ENVIRONMENT}
    "${CPACK_OPENSPACE_LINUXDEPLOY_EXECUTABLE}"
      --appdir "${APPDIR}"
      # linuxdeploy validates the desktop file: the first token of Exec has to match the
      # basename of the deployed executable, and Icon has to match --icon-filename
      --executable "${APPDIR}/usr/bin/OpenSpace"
      --desktop-file "${CPACK_OPENSPACE_APPIMAGE_DESKTOP_FILE}"
      --icon-file "${CPACK_OPENSPACE_APPIMAGE_ICON}"
      --icon-filename "openspace"
      --custom-apprun "${CPACK_OPENSPACE_APPIMAGE_APPRUN}"
      ${FORCED_LIBRARY_ARGUMENTS}
      --plugin qt
  WORKING_DIRECTORY "${CPACK_TOPLEVEL_DIRECTORY}"
  RESULT_VARIABLE LINUXDEPLOY_RESULT
)
if (NOT LINUXDEPLOY_RESULT EQUAL 0)
  message(FATAL_ERROR "linuxdeploy failed with exit code ${LINUXDEPLOY_RESULT}")
endif ()

# The install rules place the Qt libraries and plugins next to the executable so that the
# archive and the DEB/RPM packages work through an $ORIGIN RPATH. Once linuxdeploy has run
# they are in the way: its Qt plugin has deployed its own copies into usr/lib and
# usr/plugins, patched their RPATHs so they find the rest of the bundled libraries, and
# written a usr/bin/qt.conf pointing at them. The unpatched leftovers in usr/bin would be
# found first through that same $ORIGIN and shadow them. They cannot be deleted any
# earlier: with a vcpkg Qt they are the only copies on the machine that linuxdeploy can
# resolve the executable's Qt dependencies against
file(GLOB STALE_QT_LIBRARIES
  "${APPDIR}/usr/bin/libQt6*.so*"
  "${APPDIR}/usr/bin/libicu*.so*"
)
foreach (library ${STALE_QT_LIBRARIES})
  file(REMOVE "${library}")
endforeach ()
foreach (pluginDirectory
  platforms platforminputcontexts platformthemes imageformats iconengines generic
  styles tls xcbglintegrations egldeviceintegrations wayland-decoration-client
  wayland-graphics-integration-client wayland-shell-integration
)
  file(REMOVE_RECURSE "${APPDIR}/usr/bin/${pluginDirectory}")
endforeach ()

# The CEF runtime needs the opposite treatment. libcef.so is an ordinary dependency of the
# executable as far as linuxdeploy is concerned. Unlike the Qt libraries above, the copy
# we want to keep is the one put in usr/bin next to its resources
file(REMOVE "${APPDIR}/usr/lib/libcef.so")

# Every library that is not inside the AppDir has to be present on the user's machine
set(HOST_LIBRARY_SCAN [[
for f in "$APPDIR"/usr/bin/OpenSpace "$APPDIR"/usr/bin/*.so* "$APPDIR"/usr/lib/*.so* \
         "$APPDIR"/usr/plugins/*/*.so; do
  [ -f "$f" ] && ldd "$f" 2>/dev/null
done | awk -v d="$APPDIR" '$2 == "=>" && $3 ~ /^\// && index($3, d) != 1 {print $1}' |
  sort -u | tr '\n' ' '
]])
execute_process(
  COMMAND ${CMAKE_COMMAND} -E env "APPDIR=${APPDIR}" sh -c "${HOST_LIBRARY_SCAN}"
  OUTPUT_VARIABLE HOST_PROVIDED_LIBRARIES
  ERROR_QUIET
  OUTPUT_STRIP_TRAILING_WHITESPACE
)
if (HOST_PROVIDED_LIBRARIES)
  string(REPLACE " " ";" HOST_PROVIDED_LIST "${HOST_PROVIDED_LIBRARIES}")
  list(LENGTH HOST_PROVIDED_LIST HOST_PROVIDED_COUNT)
  message(STATUS
    "The AppImage expects ${HOST_PROVIDED_COUNT} libraries from the host: "
    "${HOST_PROVIDED_LIBRARIES}"
  )
endif ()

# See OPENSPACE_APPIMAGE_RUNTIME_FILE in support/cmake/packaging.cmake: without a static
# runtime the image can only be mounted on a host that has libfuse2
set(RUNTIME_ARGUMENTS "")
if (CPACK_OPENSPACE_APPIMAGE_RUNTIME_FILE)
  list(APPEND RUNTIME_ARGUMENTS
    --runtime-file "${CPACK_OPENSPACE_APPIMAGE_RUNTIME_FILE}"
  )
else ()
  message(STATUS
    "No AppImage runtime was provided, so appimagetool embeds its own. Hosts without "
    "libfuse2 have to run the result with --appimage-extract-and-run"
  )
endif ()

message(STATUS "Packing '${APPIMAGE_NAME}'")
execute_process(
  COMMAND ${CMAKE_COMMAND} -E env ${APPIMAGE_ENVIRONMENT} "ARCH=x86_64"
    "${CPACK_OPENSPACE_APPIMAGETOOL_EXECUTABLE}" ${RUNTIME_ARGUMENTS}
    "${APPDIR}" "${APPIMAGE_NAME}"
  WORKING_DIRECTORY "${CPACK_TOPLEVEL_DIRECTORY}"
  RESULT_VARIABLE APPIMAGETOOL_RESULT
)
if (NOT APPIMAGETOOL_RESULT EQUAL 0)
  message(FATAL_ERROR "appimagetool failed with exit code ${APPIMAGETOOL_RESULT}")
endif ()

set(BUILT_APPIMAGE "${CPACK_TOPLEVEL_DIRECTORY}/${APPIMAGE_NAME}")
if (NOT EXISTS "${BUILT_APPIMAGE}")
  # Some appimagetool versions ignore OUTPUT and derive the name from the desktop file
  file(GLOB CANDIDATES "${CPACK_TOPLEVEL_DIRECTORY}/*.AppImage")
  list(LENGTH CANDIDATES CANDIDATE_COUNT)
  if (CANDIDATE_COUNT EQUAL 1)
    list(GET CANDIDATES 0 CANDIDATE)
    file(RENAME "${CANDIDATE}" "${BUILT_APPIMAGE}")
  else ()
    message(FATAL_ERROR
      "linuxdeploy reported success but no AppImage was found in "
      "'${CPACK_TOPLEVEL_DIRECTORY}'"
    )
  endif ()
endif ()

# An AppImage that is not executable is of no use to anyone, and the AppDir is thrown away
# with the rest of the staging directory, so the bit has to be set on the file itself
file(CHMOD "${BUILT_APPIMAGE}"
  PERMISSIONS
    OWNER_READ OWNER_WRITE OWNER_EXECUTE
    GROUP_READ GROUP_EXECUTE
    WORLD_READ WORLD_EXECUTE
)

# CPack copies everything listed here out of the staging directory and into the build
# directory once this script returns
set(CPACK_EXTERNAL_BUILT_PACKAGES "${BUILT_APPIMAGE}")
message(STATUS "Created '${APPIMAGE_NAME}'")
