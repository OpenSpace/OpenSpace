/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 *****************************************************************************************
 * The Linux/Mac code is taken from Sarang Baheti                                        *
 * www.nullptr.me/2013/04/14/generating-stack-trace-on-os-x/                             *
 ****************************************************************************************/

#include <ghoul/misc/stacktrace.h>

#ifdef __unix__
#include <cstdio>
#include <cstdlib>
#include <cxxabi.h>
#include <execinfo.h>
#endif // __unix__

namespace ghoul {

#ifdef WIN32
std::vector<std::string> stackTrace(std::stacktrace trace) {
#else // ^^^^ WIN32 // !WIN32 vvvv
std::vector<std::string> stackTrace() {
#endif // WIN32
    std::vector<std::string> stackFrames;

#ifdef __unix__
    constexpr int MaxCallStackDepth = 128;

    int callstack[MaxCallStackDepth] = {};

    // Get the full stacktrace
    const int nFrames = backtrace(reinterpret_cast<void**>(callstack), MaxCallStackDepth);

    // Unmangle the stacktrace to get it in a human-readable format
    char** strs = backtrace_symbols(reinterpret_cast<void**>(callstack), nFrames);

    stackFrames.reserve(nFrames);
    for (int i = 0; i < nFrames; i++) {
        const int MaxFunctionSymbolLength = 1024;
        const int MaxModuleNameLength = 1024;
        const int MaxAddressLength = 48;

        // Typically this is how the backtrace looks like:
        //
        // 0   <app/lib-name>     0x0000000100000e98 _Z5tracev + 72
        // 1   <app/lib-name>     0x00000001000015c1 _ZNK7functorclEv + 17
        // 2   <app/lib-name>     0x0000000100000f71 _Z3fn0v + 17
        // 3   <app/lib-name>     0x0000000100000f89 _Z3fn1v + 9
        // 4   <app/lib-name>     0x0000000100000f99 _Z3fn2v + 9
        // 5   <app/lib-name>     0x0000000100000fa9 _Z3fn3v + 9
        // 6   <app/lib-name>     0x0000000100000fb9 _Z3fn4v + 9
        // 7   <app/lib-name>     0x0000000100000fc9 _Z3fn5v + 9
        // 8   <app/lib-name>     0x0000000100000fd9 _Z3fn6v + 9
        // 9   <app/lib-name>     0x0000000100001018 main + 56
        // 10  libdyld.dylib      0x00007fff91b647e1 start + 0

        // Split the string, take out chunks out of stack trace. We are primarily
        // interested in module, function and address
        std::vector<char> moduleName = std::vector<char>(MaxModuleNameLength);
        std::vector<char> addr = std::vector<char>(MaxAddressLength);
        std::vector<char> functionSymbol = std::vector<char>(MaxFunctionSymbolLength);
        int offset = 0;
        sscanf(
            strs[i],
            "%*s %1024s %48s %1024s %*s %d",
            moduleName.data(),
            addr.data(),
            functionSymbol.data(),
            &offset
        );

        int isValidCppName = 0;
        // If this is a C++ library, the symbol will be demangled. On success function
        // returns 0
        char* functionName = abi::__cxa_demangle(
            functionSymbol.data(),
            nullptr,
            nullptr,
            &isValidCppName
        );

        constexpr int MaxStackFrameSize = 4096;
        std::array<char, MaxStackFrameSize> stackFrame = {};

        if (functionName) {
            sprintf(
                stackFrame.data(),
                "(%s)\t0x%s — %s + %d",
                moduleName.data(),
                addr.data(),
                functionName,
                offset
            );
            free(functionName);
        }

        stackFrames.emplace_back(stackFrame.data());
    }
    free(strs);
#elif WIN32  // ^^^^ __unix__ // !__unix__ vvvv
    // Note that in order for the stackframes to work correctly on client machines,
    // `_NT_SYMBOL_PATH` has to be defined as an environment variable

    stackFrames.reserve(trace.size());
    for (const std::stacktrace_entry& e : trace) {
        stackFrames.push_back(std::to_string(e));
    }
#endif // __unix__

    return stackFrames;
}

} // namespace ghoul
