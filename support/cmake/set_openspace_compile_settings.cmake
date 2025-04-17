##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2025                                                                #
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

function (set_openspace_compile_settings target)
  # Switching to cxx_std_23 triggers a bug in Clang17
  # https://github.com/llvm/llvm-project/issues/61415
  target_compile_features(${target} PUBLIC cxx_std_20)

  set(MSVC_WARNINGS
    "/MP"       # Multi-threading support
    "/W4"       # Highest warning level
    "/w44062"   # missing case label
    "/w44165"   # 'HRESULT' is being converted to 'bool'
    "/w44242"   # conversion from 'type1' to 'type2', possible loss of data
    "/w44254"   # conversion from 'type1' to 'type2', possible loss of data
    "/w44263"   # member function does not override any base class virtual member function
    "/w44265"   # class has virtual functions, but destructor is not virtual
    "/w44287"   # unsigned/negative constant mismatch
    "/w44289"   # using for-loop variable outside of loop
    "/w44296"   # expression is always true/false
    "/w44437"   # dynamic_cast could fail in some contexts
    "/w44545"   # expression before comma evaluates to a function missing an argument list
    "/w44547"   # operator before comma has no effect
    "/w44548"   # operator before comma has no effect
    "/w44549"   # operator before comma has no effect
    "/w44555"   # expression has no effect; expected expression with side-effect
    "/w44574"   # 'identifier' is defined to be '0': did you mean to use '#if identifier'?
    "/w44619"   # #pragma warning: there is no warning number 'number'
    "/w44643"   # Forward declaring 'identifier' in namespace std is not permitted
    "/w44800"   # Implicit conversion from 'type' to bool. Possible information loss
    "/w44822"   # local class member function does not have a body
    "/w44841"   # non-standard extension used: compound member designator used in offsetof
    "/w44842"   # the result of 'offsetof' applied to a type using multiple inheritance is
                # not guaranteed to be consistent between compiler releases
    "/w44946"   # reinterpret_cast used between related classes: 'class1' and 'class2'
    "/w44986"   # exception specification does not match previous declaration
    "/w44987"   # nonstandard extension used: 'throw (...)'
    "/w45022"   # multiple move constructors specified
    "/w45023"   # multiple move assignment operators specified
    "/w45031"   # #pragma warning(pop): likely mismatch, popping warning state pushed in
                # different file
    "/w45032"   # detected #pragma warning(push) with no #pragma warning(pop)
    "/w45038"   # data member 'member1' will be initialized after data member 'member2'
    "/w45041"   # out-of-line definition for constexpr data is deprecated
    "/w45042"   # function declarations at block scope cannot be specified 'inline'
    "/w45204"   # virtual class has non-virtual trivial destructor
    "/w45233"   # explicit lambda capture 'identifier' is not used
    "/w45340"   # attribute is ignored in this syntactic position
    "/w45243"   # using incomplete class 'class-name' can cause potential one definition
                # rule violation due to ABI limitation
    "/w45245"   # unreferenced function with internal linkage has been removed
    "/w45249"   # 'bitfield' of type 'enumeration_name' has named enumerators with values
                # that cannot be represented in the given bit field width of
                # 'bitfield_width'.
    "/w45258"   # explicit capture of 'symbol' is not required for this use
    "/w45259"   # explicit specialization requires 'template <>'
    "/w45262"   # implicit fall-through occurs here
    "/w45263"   # calling 'std::move' on a temporary object prevents copy elision
    "/w45264"   # 'const' variable is not used
    "/w45266"   # 'const' qualifier on return type has no effect
    "/wd4127"   # conditional expression is constant [raised by: websocketpp]
    "/wd4201"   # nonstandard extension used : nameless struct/union  [raised by: GLM]
    "/wd5030"   # attribute 'attribute' is not recognized  [raised by: codegen]
    "/permissive-"
    "/Zc:__cplusplus" # Correctly set the __cplusplus macro
  )
  if (OPENSPACE_OPTIMIZATION_ENABLE_AVX)
    set(MSVC_WARNINGS ${MSVC_WARNINGS} "/arch:AVX")
  endif ()
  if (OPENSPACE_OPTIMIZATION_ENABLE_AVX2)
    set(MSVC_WARNINGS ${MSVC_WARNINGS} "/arch:AVX2")
  endif ()
  if (OPENSPACE_OPTIMIZATION_ENABLE_AVX512)
    set(MSVC_WARNINGS ${MSVC_WARNINGS} "/arch:AVX512")
  endif ()
  if (OPENSPACE_OPTIMIZATION_ENABLE_OTHER_OPTIMIZATIONS)
    set(MSVC_WARNINGS ${MSVC_WARNINGS}
      "/Oi" # usage of intrinsic functions
      "/GL" # Whole program optimization
    )
  else ()
    if (GHOUL_ENABLE_EDIT_CONTINUE)
      set(MSVC_WARNINGS ${MSVC_WARNINGS}
        "/ZI"       # Edit and continue support
      )
    endif ()
  endif ()

  set(CLANG_WARNINGS
    "-Wall"
    "-Wextra"
    "-Wmost"
    "-Wpedantic"

    "-Wabstract-vbase-init"
    "-Walloca"
    "-Wanon-enum-enum-conversion"
    "-Warray-bounds-pointer-arithmetic"
    "-Wassign-enum"
    "-Wbad-function-cast"
    "-Wbinary-literal"
    "-Wbind-to-temporary-copy"
    "-Wbitfield-constant-conversion"
    "-Wbool-conversions"
    "-Wcast-align"
    "-Wcast-qual"
    "-Wcomma"
    "-Wconditional-uninitialized"
    "-Wdate-time"
    "-Wdeprecated-dynamic-exception-spec"
    "-Wdeprecated-this-capture"
    "-Wdivision-by-zero"
    "-Wdtor-name"
    "-Wduplicate-decl-specifier"
    "-Wduplicate-enum"
    "-Wduplicate-method-arg"
    "-Wduplicate-method-match"
    "-Wextra-semi"
    "-Wfloat-overflow-conversion"
    "-Wfloat-zero-conversion"
    "-Wformat"
    "-Wformat-non-iso"
    "-Wformat-nonliteral"
    "-Wformat-pedantic"
    "-Wformat-type-confusion"
    "-Wheader-hygiene"
    "-Widiomatic-parentheses"
    "-Wimplicit"
    "-Wimplicit-fallthrough"
    "-Wloop-analysis"
    "-Wmain"
    "-Wmethod-signatures"
    "-Wmissing-noreturn"
    "-Wmove"
    "-Wnon-virtual-dtor"
    "-Wold-style-cast"
    "-Wpointer-arith"
    "-Wpragmas"
    "-Wrange-loop-analysis"
    "-Wreorder"
    "-Wshadow-all"
    "-Wshift-sign-overflow"
    "-Wshorten-64-to-32"
    "-Wsometimes-uninitialized"
    "-Wstring-conversion"
    "-Wsuggest-destructor-override"
    "-Wsuggest-override"
    "-Wtautological-compare"
    "-Wtautological-constant-in-range-compare"
    "-Wtautological-constant-out-of-range-compare"
    "-Wthread-safety"
    "-Wtype-limits"
    "-Wundef"
    "-Wundefined-reinterpret-cast"
    "-Wuninitialized-const-reference"
    "-Wunneeded-internal-declaration"
    "-Wunneeded-member-function"
    "-Wunreachable-code"
    "-Wunreachable-code-break"
    "-Wunreachable-code-return"
    "-Wunused"
    "-Wunused-const-variable"
    "-Wunused-exception-parameter"
    "-Wunused-macros"
    "-Wunused-result"
    "-Wvariadic-macros"
    "-Wvla"
    "-Wzero-as-null-pointer-constant"

    "-Wno-attributes"
    "-Wno-deprecated-enum-enum-conversion"
    "-Wno-missing-braces"
    "-Wno-sign-compare"
    "-Wno-suggest-destructor-override"
    "-Wno-unknown-attributes"

    # This should be removed as soon as https://github.com/g-truc/glm/issues/1349 is
    # addressed
    "-Wno-defaulted-function-deleted"
  )


  set(GCC_WARNINGS
    "-ggdb"
    "-Wall"
    "-Wextra"
    "-Wpedantic"

    "-Walloc-zero"
    "-Wcast-qual"
    "-Wdate-time"
    "-Wduplicated-branches"
    "-Wduplicated-cond"
    "-Wformat"
    "-Wlogical-op"
    "-Wmain"
    "-Wnon-virtual-dtor"
    "-Wold-style-cast"
    "-Woverloaded-virtual"
    "-Wshadow"
    "-Wsuggest-override"
    "-Wtautological-compare"
    "-Wtype-limits"
    "-Wundef"
    "-Wunused"
    "-Wuninitialized"
    "-Wvla"
    "-Wzero-as-null-pointer-constant"

    "-Wno-attributes"
    "-Wno-deprecated-copy"
    "-Wno-deprecated-enum-enum-conversion"
    "-Wno-float-equal"
    "-Wno-long-long"
    "-Wno-missing-field-initializers"
    "-Wno-sign-compare"
    "-Wno-unknown-attributes"
    "-Wno-write-strings"
  )

  if (MSVC)
    target_compile_options(${target} PRIVATE ${MSVC_WARNINGS})

    # Boost as of 1.64 still uses unary_function unless we define this
    target_compile_definitions(${target}
      PRIVATE "_HAS_AUTO_PTR_ETC"
      PUBLIC "_CRT_SECURE_NO_WARNINGS"
      PRIVATE "NOMINMAX"
      PRIVATE "WIN32_LEAN_AND_MEAN"
      PRIVATE "VC_EXTRALEAN"
    )
  elseif (NOT LINUX AND CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    # Apple has "deprecated" OpenGL and offers nothing but warnings instead
    target_compile_definitions(${target} PRIVATE "GL_SILENCE_DEPRECATION")

    target_compile_options(${target} PRIVATE ${CLANG_WARNINGS})
  elseif (UNIX AND CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    target_compile_options(${target} PRIVATE ${CLANG_WARNINGS})
  elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
    target_compile_options(${target} PRIVATE ${GCC_WARNINGS})
  else ()
    message("Compiler not handled in set_openspace_compile_settings.cmake")
  endif ()
endfunction ()
