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
# https://releases.llvm.org/20.1.0/tools/clang/docs/DiagnosticsReference.html


function (internal__set_compile_options target)
  target_compile_options(${target} PRIVATE
    "-march=x86-64-v3"
  )

  target_compile_options(${target} PRIVATE
    "-Wall"
    "-Wextra"
    "-Wmost"
    "-Wpedantic"
    "-Wabstract-vbase-init"
    "-Walloca"
    "-Wanon-enum-enum-conversion"
    "-Warray-bounds-pointer-arithmetic"
    "-Wassign-enum"
    "-Watomic-implicit-seq-cst"
    "-Watomic-properties"
    "-Wbad-function-cast"
    "-Wbind-to-temporary-copy"
    "-Wbit-int-extension"
    "-Wbitfield-enum-conversion"
    "-Wbitwise-instead-of-logical"
    "-Wbitwise-op-parentheses"
    "-Wbool-conversion"
    "-Wcalled-once-parameter"
    "-Wcast-align"
    "-Wcast-function-type"
    "-Wcast-function-type-strict"
    "-Wcast-qual"
    "-Wclass-varargs"
    "-Wcomma"
    "-Wcompletion-handler"
    "-Wcomplex-component-init"
    "-Wcompound-token-split"
    "-Wconditional-uninitialized"
    "-Wconsumed"
    "-Wconversion"
    "-Wdate-time"
    "-Wdeclaration-after-statement"
    "-Wdeprecated-copy"
    "-Wdeprecated-copy-dtor"
    "-Wdeprecated-dynamic-exception-spec"
    "-Wdeprecated-implementations"
    "-Wdeprecated-redundant-constexpr-static-def"
    "-Wdirect-ivar-access"
    "-Wdivision-by-zero"
    "-Wdocumentation"
    "-Wdocumentation-pedantic"
    "-Wdocumentation-unknown-command"
    "-Wdollar-in-identifier-extension"
    "-Wdtor-name"
    "-Wduplicate-decl-specifier"
    "-Wduplicate-enum"
    "-Wduplicate-method-arg"
    "-Wduplicate-method-match"
    "-Wdynamic-exception-spec"
    "-Wembedded-directive"
    "-Wempty-translation-unit"
    "-Wenum-conversion"
    "-Wextra-semi"
    "-Wextra-semi-stmt"
    "-Wflexible-array-extensions"
    "-Wfloat-conversion"
    "-Wformat-non-iso"
    "-Wformat-nonliteral"
    "-Wformat-pedantic"
    "-Wformat-type-confusion"
    "-Wfour-char-constants"
    "-Wheader-hygiene"
    "-Widiomatic-parentheses"
    "-Wimplicit-fallthrough"
    "-Wimport-preprocessor-directive-pedantic"
    "-Wincompatible-function-pointer-types-strict"
    "-Wincomplete-module"
    "-Winconsistent-missing-destructor-override"
    "-Winvalid-or-nonexistent-directory"
    "-Winvalid-utf8"
    "-Wkeyword-macro"
    "-Wlanguage-extension-token"
    "-Wlogical-op-parentheses"
    "-Wloop-analysis"
    "-Wmain"
    "-Wmethod-signatures"
    "-Wmissing-field-initializers"
    "-Wmissing-method-return-type"
    "-Wmissing-variable-declarations"
    "-Wnarrowing"
    "-Wnewline-eof"
    "-Wnon-virtual-dtor"
    "-Wnull-pointer-arithmetic"
    "-Wnull-pointer-subtraction"
    "-Wnullable-to-nonnull-conversion"
    "-Wold-style-cast"
    "-Wover-aligned"
    "-Woverlength-strings"
    "-Woverriding-method-mismatch"
    "-Wpointer-arith"
    "-Wpragma-pack"
    "-Wpragmas"
    "-Wrange-loop-analysis"
    "-Wreserved-user-defined-literal"
    "-Wsemicolon-before-method-body"
    "-Wshadow"
    "-Wshadow-all"
    "-Wshift-sign-overflow"
    "-Wshorten-64-to-32"
    "-Wsign-compare"
    "-Wsigned-enum-bitfield"
    "-Wsometimes-uninitialized"
    "-Wstatic-in-inline"
    "-Wstring-conversion"
    "-Wsuggest-destructor-override"
    "-Wsuggest-override"
    "-Wtautological-constant-in-range-compare"
    "-Wtautological-constant-out-of-range-compare"
    "-Wtautological-type-limit-compare"
    "-Wtautological-value-range-compare"
    "-Wthread-safety"
    "-Wthread-safety-verbose"
    "-Wtype-limits"
    "-Wunaligned-access"
    "-Wundef"
    "-Wundefined-func-template"
    "-Wundefined-internal-type"
    "-Wundefined-reinterpret-cast"
    "-Wunnamed-type-template-args"
    "-Wunneeded-internal-declaration"
    "-Wunneeded-member-function"
    "-Wunreachable-code-aggressive"
    "-Wunused-const-variable"
    "-Wunused-exception-parameter"
    "-Wunused-member-function"
    "-Wunused-parameter"
    "-Wunused-result"
    "-Wused-but-marked-unused"
    "-Wvariadic-macros"
    "-Wvla"
    "-Wzero-as-null-pointer-constant"
    "-Wzero-length-array"
  )

  target_compile_options(${target} PRIVATE
    "-Wno-c++98-compat-unnamed-type-template-args"
    "-Wno-c++98-compat-bind-to-temporary-copy"
    "-Wno-deprecated-copy-with-dtor"
    "-Wno-deprecated-enum-enum-conversion"
    "-Wno-reserved-macro-identifier"
    "-Wno-missing-prototypes"
    "-Wno-implicit-int-float-conversion"
    "-Wno-sign-conversion"
    "-Wno-unknown-attributes"
  )

  # Warnings introduced in Clang version 18
  if (CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL "18")
    target_compile_options(${target} PRIVATE
      "-Wauto-decl-extensions"
      "-Wthread-safety-reference-return"
    )
  endif ()

  # Warnings introduced in Clang version 19
  if (CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL "19")
    target_compile_options(${target} PRIVATE
      "-Wformat-signedness"
      "-Wfunction-effects"

      "-Wno-missing-designated-field-initializers"
    )
  endif ()

  # Warnings introduced in Clang version 20
  if (CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL "20")
    target_compile_options(${target} PRIVATE
      "-Wdecls-in-multiple-modules"
    )
  endif ()
endfunction()
