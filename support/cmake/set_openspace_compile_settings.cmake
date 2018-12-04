##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2018                                                                #
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

function (set_openspace_compile_settings project)
    set_property(TARGET ${project} PROPERTY CXX_STANDARD 17)
    set_property(TARGET ${project} PROPERTY CXX_STANDARD_REQUIRED ON)

    if (MSVC)
        target_compile_options(
            ${project}
            PRIVATE
            "/ZI"       # Edit and continue support
            "/MP"       # Multi-threading support
            "/W4"       # Highest warning level
            "/w44062"   # enumerator 'identifier' in a switch of enum 'enumeration' is not handled
            "/wd4127"   # conditional expression is constant
            "/wd4201"   # nonstandard extension used : nameless struct/union
            "/w44255"   # 'function': no function prototype given: converting '()' to '(void)'
            "/w44263"   # 'function': member function does not override any base class virtual member function
            "/w44264"   # 'virtual_function': no override available for virtual member function from base 'class'; function is hidden
            "/w44265"   # 'class': class has virtual functions, but destructor is not virtual
            "/w44266"   # 'function': no override available for virtual member function from base 'type'; function is hidden
            "/w44289"   # nonstandard extension used : 'var' : loop control variable declared in the for-loop is used outside the for-loop scope
            "/w44296"   # 'operator': expression is always false
            "/w44311"   # 'variable' : pointer truncation from 'type' to 'type'
            "/w44339"   # 'type' : use of undefined type detected in CLR meta-data - use of this type may lead to a runtime exception
            "/w44342"   # behavior change: 'function' called, but a member operator was called in previous versions
            "/w44350"   # behavior change: 'member1' called instead of 'member2'
            "/w44431"   # missing type specifier - int assumed. Note: C no longer supports default-int
            "/w44471"   # a forward declaration of an unscoped enumeration must have an underlying type (int assumed)
            "/wd4505"   # unreferenced local function has been removed
            "/w44545"   # expression before comma evaluates to a function which is missing an argument list
            "/w44546"   # function call before comma missing argument list
            "/w44547"   # 'operator': operator before comma has no effect; expected operator with side-effect
            "/w44548"   # expression before comma has no effect; expected expression with side-effect
            "/w44549"   # 'operator': operator before comma has no effect; did you intend 'operator'?
            "/w44555"   # expression has no effect; expected expression with side-effect
            # This is disabled until GLM is updated to version 0.9.9 that removes occurrance of this warning
            # "/w44574"   # 'identifier' is defined to be '0': did you mean to use '#if identifier'?
            "/w44608"   # 'symbol1' has already been initialized by another union member in the initializer list, 'symbol2'
            "/w44619"   # #pragma warning: there is no warning number 'number'
            "/w44628"   # digraphs not supported with -Ze. Character sequence 'digraph' not interpreted as alternate token for 'char'
            "/w44640"   # 'instance': construction of local static object is not thread-safe
            "/w44905"   # wide string literal cast to 'LPSTR'
            "/w44906"   # string literal cast to 'LPWSTR'
            "/w44946"   # reinterpret_cast used between related classes: 'class1' and 'class2'
            "/w44986"   # 'symbol': exception specification does not match previous declaration
            "/w44988"   # 'symbol': variable declared outside class/function scope
            "/std:c++latest"
            "/permissive-"
            "/Zc:twoPhase-"  # Used to prevent C:\Program Files (x86)\Windows Kits\8.1\Include\um\combaseapi.h(229): error C2187: syntax error: 'identifier' was unexpected here
                             # This is a bug in Visual Studio 15.3 and can be removed with the next version:
                             # https://developercommunity.visualstudio.com/content/problem/94419/vs-2017-153-with-permissive-shows-error-c2187-in-c.html
            "/Zc:strictStrings-"    # Windows header don't adhere to this
            "/Zc:__cplusplus" # Correctly set the __cplusplus macro
        )
        if (OPENSPACE_WARNINGS_AS_ERRORS)
            target_compile_options(${project} PRIVATE "/WX")
        endif ()

        # Boost as of 1.64 still uses unary_function unless we define this
        target_compile_definitions(${project} PRIVATE "_HAS_AUTO_PTR_ETC")

        target_compile_definitions(${project} PRIVATE "NOMINMAX")
    elseif (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
        if (OPENSPACE_WARNINGS_AS_ERRORS)
            target_compile_options(${project} PRIVATE "-Werror")
        endif ()

        target_compile_options(
            ${project}
            PRIVATE
            "-stdlib=libc++"
            "-Wall"
            "-Wextra"
            "-Wabstract-vbase-init"
            "-Warray-bounds-pointer-arithmetic"
            "-Wassign-enum"
            "-Wauto-import"
            "-Wbad-function-cast"
            "-Wbitfield-constant-conversion"
            "-Wcast-calling-convention"
            "-Wcast-qual"
            "-Wchar-subscripts"
            "-Wcomma"
            "-Wcomment"
            "-Wcomplex-component-init"
            "-Wconditional-uninitialized"
            "-Wdate-time"
            "-Wdeprecated-implementations"
            "-Wdollar-in-identifier-extension"
            "-Wduplicate-enum"
            "-Wduplicate-method-match"
            "-Wempty-body"
            "-Wformat-pedantic"
            "-Wheader-hygiene"
            "-Widiomatic-parentheses"
            "-Wimplicit-fallthrough"
            "-Wimport-preprocessor-directive-pedantic"
            "-Winconsistent-missing-override"
            "-Winfinite-recursion"
            "-Wkeyword-macro"
            "-Wlanguage-extension-token"
            "-Wloop-analysis"
            "-Wmethod-signatures"
            "-Wmicrosoft-end-of-file"
            "-Wmicrosoft-enum-forward-reference"
            "-Wmicrosoft-fixed-enum"
            "-Wmicrosoft-flexible-array"
            "-Wmismatched-tags"
            "-Wmissing-field-initializers"
            "-Wmissing-noreturn"
            "-Wnewline-eof"
            "-Wnon-virtual-dtor"
            "-Wold-style-cast"
            "-Woverloaded-virtual"
            "-Wpessimizing-move"
            "-Wpointer-arith"
            "-Wpragmas"
            "-Wredundant-move"
            "-Wreorder"
            "-Wsemicolon-before-method-body"
            # "-Wshadow-field"
            "-Wshadow-field-in-constructor"
            # "-Wshadow-all"  Add this again once the Properties don't throw warnings --abock
            "-Wshift-sign-overflow"
            "-Wshorten-64-to-32"
            "-Wsign-compare"
            "-Wstring-conversion"
            "-Wtautological-compare"
            "-Wthread-safety"
            "-Wundef"
            "-Wundefined-reinterpret-cast"
            "-Wuninitialized"
            "-Wunneeded-internal-declaration"
            "-Wunneeded-member-function"
            "-Wunreachable-code-break"
            "-Wunreachable-code-loop-increment"
            "-Wunreachable-code-return"
            "-Wunused-exception-parameter"
            "-Wunused-label"
            "-Wunused-local-typedef"
            "-Wunused-macros"
            "-Wunused-parameter"
            "-Wunused-private-field"
            "-Wunused-result"
            "-Wunused-variable"
            "-Wused-but-marked-unused"
            "-Wvariadic-macros"
            "-Wvla"
            "-Wzero-length-array"
            "-Wno-missing-braces"
        )
    elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
        target_compile_options(
            ${project}
            PRIVATE
            "-ggdb"
            "-Wall"
            "-Wextra"
            "-Wpedantic"
            "-Wunused-parameter"
            "-Wuninitialized"
            "-Wsuggest-attribute=const"
            "-Wsuggest-final-types"
            "-Wsuggest-final-methods"
            "-Wsuggest-override"
            "-Walloc-zero"
            "-Wduplicated-cond"
            "-Wshadow"
            "-Wundef"
            "-Wcast-qual"
            "-Wzero-as-null-pointer-constant"
            "-Wdate-time"
            "-Wuseless-cast"
            "-Wlogical-op"
            "-Wint-in-bool-context"

            "-Wno-write-strings"
            "-Wnon-virtual-dtor"
            "-Wold-style-cast"
            "-Woverloaded-virtual"
            "-Wno-long-long"
        )

        if (OPENSPACE_WARNINGS_AS_ERRORS)
            target_compile_options(${project} PRIVATE "-Werror")
        endif ()
    else ()
        message("Compiler not handled in set_openspace_compile_settings.cmake")
    endif ()
endfunction ()
