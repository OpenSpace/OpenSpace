/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/misc/stacktrace.h>

#ifdef __unix__
#include <cstdio>
#include <cstdlib>
#include <cxxabi.h>
#include <execinfo.h>
#endif // __unix__

#ifdef WIN32
#include <Windows.h>
#include <dbghelp.h>
#include <array>
#include <cstddef>
#include <format>
#include <string_view>
#endif // WIN32

namespace openspace {

#ifdef WIN32

namespace {

// The maximum number of frames that are unwound from a context
constexpr int MaxContextStackDepth = 128;

#if defined(_M_X64) || defined(_M_ARM64)

/**
 * Returns a reference to the instruction pointer of the passed \p context. The register
 * that holds it is named differently for each architecture, but `RtlVirtualUnwind` works
 * the same way for all of them.
 */
DWORD64& instructionPointer(CONTEXT& context) {
#ifdef _M_ARM64
    return context.Pc;
#else // ^^^^ _M_ARM64 // _M_X64 vvvv
    return context.Rip;
#endif // _M_ARM64
}

/**
 * Converts the \p address into a human-readable description of the function that contains
 * it. If the symbols for the address are not available, the name of the module and the
 * offset into it are returned instead, which can be resolved offline.
 */
std::string describeAddress(HANDLE process, DWORD64 address) {
    // SYMBOL_INFO is a variable-length structure whose name is stored directly behind it
    constexpr DWORD MaxNameLength = 1024;
    std::array<std::byte, sizeof(SYMBOL_INFO) + MaxNameLength> storage = {};
    SYMBOL_INFO* symbol = reinterpret_cast<SYMBOL_INFO*>(storage.data());
    symbol->SizeOfStruct = sizeof(SYMBOL_INFO);
    symbol->MaxNameLen = MaxNameLength;

    DWORD64 symbolOffset = 0;
    const bool hasSymbol = SymFromAddr(process, address, &symbolOffset, symbol);

    IMAGEHLP_LINE64 line = {
        .SizeOfStruct = sizeof(IMAGEHLP_LINE64)
    };
    DWORD lineOffset = 0;
    const bool hasLine = SymGetLineFromAddr64(process, address, &lineOffset, &line);

    std::string result;
    if (hasLine) {
        result = std::format("{}({}): ", line.FileName, line.LineNumber);
    }

    if (hasSymbol) {
        result += std::format("{}+0x{:X}", symbol->Name, symbolOffset);
        return result;
    }

    // Without symbols the module name and the offset into it are the best we can do
    const DWORD64 moduleBase = SymGetModuleBase64(process, address);
    if (moduleBase == 0) {
        result += std::format("0x{:X}", address);
        return result;
    }

    std::array<char, MAX_PATH> module = {};
    GetModuleFileNameA(
        reinterpret_cast<HMODULE>(moduleBase),
        module.data(),
        static_cast<DWORD>(module.size())
    );
    std::string_view name = module.data();
    if (const size_t it = name.find_last_of("\\/");  it != std::string_view::npos) {
        name = name.substr(it + 1);
    }
    result += std::format("{}+0x{:X}", name, address - moduleBase);
    return result;
}

#endif // defined(_M_X64) || defined(_M_ARM64)

} // namespace

std::vector<std::string> stackTraceFromContext([[maybe_unused]] const void* context) {
    std::vector<std::string> stackFrames;

#if defined(_M_X64) || defined(_M_ARM64)
    if (!context) {
        return stackFrames;
    }

    const HANDLE process = GetCurrentProcess();
    SymSetOptions(SymGetOptions() | SYMOPT_LOAD_LINES | SYMOPT_UNDNAME);
    // In order for this to work on client machines, `_NT_SYMBOL_PATH` has to be defined
    // as an environment variable. Passing a nullptr as the search path here makes DbgHelp
    // use it
    SymInitialize(process, nullptr, TRUE);

    // RtlVirtualUnwind writes the unwound state back into the context it is given, so we
    // have to work on a copy of the one we were handed
    CONTEXT ctx = *static_cast<const CONTEXT*>(context);

    for (int i = 0; i < MaxContextStackDepth; i++) {
        const DWORD64 address = instructionPointer(ctx);
        if (address == 0) {
            break;
        }

        stackFrames.push_back(describeAddress(process, address));

        DWORD64 imageBase = 0;
        PRUNTIME_FUNCTION function = RtlLookupFunctionEntry(address, &imageBase, nullptr);
        if (!function) {
            // There is no unwind information for this address, so this is as far as we
            // can walk
            break;
        }

        PVOID handlerData = nullptr;
        DWORD64 establisherFrame = 0;
        RtlVirtualUnwind(
            UNW_FLAG_NHANDLER,
            imageBase,
            address,
            function,
            &ctx,
            &handlerData,
            &establisherFrame,
            nullptr
        );
    }
#endif // defined(_M_X64) || defined(_M_ARM64)

    return stackFrames;
}

#endif // WIN32

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

} // namespace openspace
