##########################################################################################
#                                                                                        #
# Common Compile Settings                                                                #
#                                                                                        #
# Copyright (c) 2025-2026                                                                #
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

# Last pass through the MSVC warning list:
# https://gcc.gnu.org/onlinedocs/gcc-13.4.0/gcc/Warning-Options.html


function (internal__set_compile_options target)
  target_compile_options(${target} PRIVATE
    "-ggdb"
    "-march=x86-64-v3"
  )

  target_compile_options(${target} PRIVATE
    "-Wall"
    "-Wextra"
    "-Wpedantic"
    "-Walloc-zero"
    "-Wcast-qual"
    "-Wdate-time"
    "-Wduplicated-branches"
    "-Wduplicated-cond"
    "-Wenum-conversion"
    "-Wformat=2"
    "-Wformat-signedness"
    "-Wimplicit-fallthrough"
    "-Winvalid-utf8"
    "-Wlogical-op"
    "-Wmain"
    "-Wno-changes-meaning"
    "-Wshadow"
    "-Wundef"
    "-Wunused"
    "-Wuninitialized"
    "-Wvla"
    "-Wzero-as-null-pointer-constant"
    )

  target_compile_options(${target} PRIVATE
    "-Wno-attributes"
    "-Wno-missing-field-initializers"
  )
endfunction()
