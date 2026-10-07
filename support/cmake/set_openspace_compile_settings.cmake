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

if (MSVC)
  option(OPTIMIZATION_ENABLE_OTHER_OPTIMIZATIONS "Enable other optimizations, like LTCG, intrinsics, etc")

  include(${CMAKE_CURRENT_LIST_DIR}/platforms/msvc.cmake)
elseif (CMAKE_CXX_COMPILER_ID MATCHES ".*Clang")
  include(${CMAKE_CURRENT_LIST_DIR}/platforms/clang.cmake)
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  include(${CMAKE_CURRENT_LIST_DIR}/platforms/gcc.cmake)
else ()
  message(FATAL_ERROR "Unknown compiler ${CMAKE_CXX_COMPILER_ID}")
endif ()

function (set_openspace_compile_settings target)
  target_compile_features(${target} PRIVATE cxx_std_23)
  internal__set_compile_options(${target})

  if (OPENSPACE_ENABLE_EDIT_CONTINUE)
    target_compile_options(${target} PRIVATE
      "/ZI"       # Edit and continue support
    )
  endif ()
endfunction ()
