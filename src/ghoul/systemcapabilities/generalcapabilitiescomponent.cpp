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
 ****************************************************************************************/

#include <ghoul/systemcapabilities/generalcapabilitiescomponent.h>

#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <array>
#include <cstring>
#include <exception>
#include <sstream>
#include <utility>

#ifdef WIN32
#include <intrin.h>
#include <tchar.h>
#include <Windows.h>
#pragma comment(lib, "User32.lib")
#pragma comment(lib, "Kernel32.lib")
typedef void (WINAPI* PGNSI)(LPSYSTEM_INFO);
typedef BOOL (WINAPI *PGPI)(DWORD, DWORD, DWORD, DWORD, DWORD);
#else // ^^^^ WIN32 // !WIN32 vvvv
#include <sys/types.h>
#include <sys/sysinfo.h>
#include <sys/utsname.h>
#include <cstring>
#endif // WIN32

namespace ghoul {

template <>
std::string to_string(
           const systemcapabilities::GeneralCapabilitiesComponent::OperatingSystem& value)
{
    using OS = systemcapabilities::GeneralCapabilitiesComponent::OperatingSystem;
    switch (value) {
        case OS::Windows10or11:               return "Windows 10/11";
        case OS::WindowsServer2016:           return "Windows Server 2016";
        case OS::WindowsVista:                return "Windows Vista";
        case OS::WindowsServer2008:           return "Windows Server 2008";
        case OS::Windows7:                    return "Windows 7";
        case OS::WindowsServer2008R2:         return "Windows Server 2008 R2";
        case OS::Windows8:                    return "Windows 8";
        case OS::WindowsServer2012:           return "Windows Server 2012";
        case OS::Windows81:                   return "Windows 8.1";
        case OS::WindowsServer2012R2:         return "Windows Server 2012 R2";
        case OS::WindowsServer2003R2:         return "Windows Server 2003 R2";
        case OS::WindowsStorageServer2003:    return "Windows Storage Server 2003";
        case OS::WindowsXPProfx64:            return "Windows XP Professional x64";
        case OS::WindowsServer2003:           return "Windows Server 2003";
        case OS::WindowsXPHome:               return "Windows XP Home Edition";
        case OS::WindowsXPProf:               return "Windows XP Professional Edition";
        case OS::Windows2000Prof:             return "Windows 2000 Professional";
        case OS::Windows2000DatacenterServer: return "Windows 2000 Datacenter Server";
        case OS::Windows2000AdvancedServer:   return "Windows 2000 Advanced Server";
        case OS::Windows2000Server:           return "Windows 2000 Server";
        case OS::Linux: // @TODO we need more variety here
            return "Linux";
        case OS::MacOS:
            // @TODO we need more variety here
            return "MacOS";
        case OS::Unknown:
            return "";
        default:
            throw MissingCaseException();
    }
}
} // namespace ghoul

namespace ghoul::systemcapabilities {

GeneralCapabilitiesComponent::GeneralCapabilitiesComponentError::
    GeneralCapabilitiesComponentError(std::string msg)
    : RuntimeError(std::move(msg), "GeneralCapabilitiesComponent")
{}

GeneralCapabilitiesComponent::OperatingSystemError::OperatingSystemError(std::string desc,
                                                                     std::string errorMsg)
    : GeneralCapabilitiesComponentError(std::format("{}. Error: {}", desc, errorMsg))
    , description(std::move(desc))
    , errorMessage(std::move(errorMsg))
{}

GeneralCapabilitiesComponent::MainMemoryError::MainMemoryError(std::string msg)
    : GeneralCapabilitiesComponentError(std::move(msg))
{}

void GeneralCapabilitiesComponent::detectCapabilities() {
    clearCapabilities();
    detectOS();
    detectMemory();
    detectCPU();
}

void GeneralCapabilitiesComponent::clearCapabilities() {
    _operatingSystem = OperatingSystem::Unknown;
    _operatingSystemExtra.clear();
    _fullOperatingSystem.clear();
    _installedMainMemory = 0;
    _cpu.clear();
    _cores = 0;
    _cacheLineSize = 0;
    _L2Associativity = 0;
    _cacheSize = 0;
    _extensions.clear();
}

void GeneralCapabilitiesComponent::detectOS() {
#ifdef WIN32
    // This code is taken from
    // http://msdn.microsoft.com/en-us/library/windows/desktop/ms724429%28v=vs.85%29.aspx
    // All rights remain with their original copyright owners
    OSVERSIONINFOEX osVersionInfo;
    std::memset(&osVersionInfo, 0, sizeof(OSVERSIONINFOEX));
    osVersionInfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);

    SYSTEM_INFO systemInfo;
    std::memset(&systemInfo, 0, sizeof(SYSTEM_INFO));

#pragma warning (push)
#pragma warning (disable : 4996)
    BOOL osVersionInfoEx = GetVersionEx(reinterpret_cast<OSVERSIONINFO*>(&osVersionInfo));
#pragma warning (pop)

    if (osVersionInfoEx == 0) {
        DWORD error = GetLastError();
        LPTSTR errorBuffer = nullptr;
        FormatMessage(
            FORMAT_MESSAGE_FROM_SYSTEM |
            FORMAT_MESSAGE_ALLOCATE_BUFFER |
            FORMAT_MESSAGE_IGNORE_INSERTS,
            nullptr,
            error,
            MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
            reinterpret_cast<LPTSTR>(&errorBuffer),
            0,
            nullptr
        );
        if (errorBuffer) {
            std::string errorMsg(errorBuffer);
            LocalFree(errorBuffer);
            throw OperatingSystemError(
                "Retrieving OS version failed. 'GetVersionEx' returned 0",
                errorMsg
            );
        }
        throw OperatingSystemError(
            "Retrieving OS version failed. 'GetVersionEx' returned 0",
            ""
        );
    }
    HMODULE module = GetModuleHandle(TEXT("kernel32.dll"));
    if (module == nullptr) {
        DWORD error = GetLastError();
        LPTSTR errorBuffer = nullptr;
        FormatMessage(
            FORMAT_MESSAGE_FROM_SYSTEM |
            FORMAT_MESSAGE_ALLOCATE_BUFFER |
            FORMAT_MESSAGE_IGNORE_INSERTS,
            nullptr,
            error,
            MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
            reinterpret_cast<LPTSTR>(&errorBuffer),
            0,
            nullptr
        );
        if (errorBuffer != nullptr) {
            std::string errorMsg(errorBuffer);
            LocalFree(errorBuffer);
            throw OperatingSystemError(
                "Kernel32.dll handle could not be found. 'GetModuleHandle' returned 0",
                errorMsg
            );
        }
        throw OperatingSystemError(
            "Kernel32.dll handle could not be found. 'GetModuleHandle' returned 0",
            ""
        );
    }
    PGNSI procedureGetNativeSystemInfo = reinterpret_cast<PGNSI>(GetProcAddress(
        module,
        "GetNativeSystemInfo"
    ));
    if (procedureGetNativeSystemInfo) {
        procedureGetNativeSystemInfo(&systemInfo);
    }
    else {
        GetSystemInfo(&systemInfo);
    }

    std::stringstream resultStream;
    if ((osVersionInfo.dwPlatformId == VER_PLATFORM_WIN32_NT) &&
        (osVersionInfo.dwMajorVersion > 4))
    {
        // From Microsoft:
        // https://msdn.microsoft.com/en-us/library/windows/desktop/ms724832(v=vs.85).aspx
        // For applications that have been manifested for Windows 8.1 or Windows 10.
        // Applications not manifested for Windows 8.1 or Windows 10 will return the
        // Windows 8 OS version value (6.2)
        if (osVersionInfo.dwMajorVersion == 10) {
            if (osVersionInfo.dwMinorVersion == 0) {
                if (osVersionInfo.wProductType == VER_NT_WORKSTATION) {
                    _operatingSystem = OperatingSystem::Windows10or11;
                }
                else {
                    _operatingSystem = OperatingSystem::WindowsServer2016;
                }
            }
        }
        else if (osVersionInfo.dwMajorVersion == 6) {
            if (osVersionInfo.dwMinorVersion == 0) {
                if (osVersionInfo.wProductType == VER_NT_WORKSTATION) {
                    _operatingSystem = OperatingSystem::WindowsVista;
                }
                else {
                    _operatingSystem = OperatingSystem::WindowsServer2008;
                }
            }
            else if (osVersionInfo.dwMinorVersion == 1) {
                if (osVersionInfo.wProductType == VER_NT_WORKSTATION) {
                    _operatingSystem = OperatingSystem::Windows7;
                }
                else {
                    _operatingSystem = OperatingSystem::WindowsServer2008R2;
                }
            }
            else if (osVersionInfo.dwMinorVersion == 2) {
                if (osVersionInfo.wProductType == VER_NT_WORKSTATION) {
                    _operatingSystem = OperatingSystem::Windows8;
                }
                else {
                    _operatingSystem = OperatingSystem::WindowsServer2012;
                }
            }
            else if (osVersionInfo.dwMinorVersion == 3) {
                if (osVersionInfo.wProductType == VER_NT_WORKSTATION) {
                    _operatingSystem = OperatingSystem::Windows81;
                }
                else {
                    _operatingSystem = OperatingSystem::WindowsServer2012R2;
                }
            }
        }
        else if (osVersionInfo.dwMajorVersion == 5 && osVersionInfo.dwMinorVersion == 2) {
            if (GetSystemMetrics(SM_SERVERR2)) {
                _operatingSystem = OperatingSystem::WindowsServer2003R2;
            }
            else if (osVersionInfo.wSuiteMask & VER_SUITE_STORAGE_SERVER) {
                _operatingSystem = OperatingSystem::WindowsStorageServer2003;
            }
            else if (osVersionInfo.wProductType == VER_NT_WORKSTATION &&
                     systemInfo.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64)
            {
                _operatingSystem = OperatingSystem::WindowsXPProfx64;
            }
            else {
                _operatingSystem = OperatingSystem::WindowsServer2003;
            }
        }
        else if (osVersionInfo.dwMajorVersion == 5 && osVersionInfo.dwMinorVersion == 1) {
            if (osVersionInfo.wSuiteMask & VER_SUITE_PERSONAL) {
                _operatingSystem = OperatingSystem::WindowsXPHome;
            }
            else {
                _operatingSystem = OperatingSystem::WindowsXPProf;
            }
        }
        else if (osVersionInfo.dwMajorVersion == 5 && osVersionInfo.dwMinorVersion == 0) {
            if (osVersionInfo.wProductType == VER_NT_WORKSTATION) {
                _operatingSystem = OperatingSystem::Windows2000Prof;
            }
            else {
                if (osVersionInfo.wSuiteMask & VER_SUITE_DATACENTER) {
                    _operatingSystem = OperatingSystem::Windows2000DatacenterServer;
                }
                else if (osVersionInfo.wSuiteMask & VER_SUITE_ENTERPRISE) {
                    _operatingSystem = OperatingSystem::Windows2000AdvancedServer;
                }
                else {
                    _operatingSystem = OperatingSystem::Windows2000Server;
                }
            }
        }

        resultStream << "(build " << osVersionInfo.dwBuildNumber << ")";

        if (osVersionInfo.dwMajorVersion >= 6) {
            if (systemInfo.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64) {
                resultStream << ", 64-bit";
            }
            else if (systemInfo.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_INTEL) {
                resultStream << ", 32-bit";
            }
        }
    }
    else {
        resultStream << "OS detection failed. Version of Windows is too old";
    }

    _operatingSystemExtra = resultStream.str();
    _fullOperatingSystem = to_string(_operatingSystem) + ' ' + _operatingSystemExtra;
#else // ^^^^ WIN32 // !WIN32 vvvv
    utsname name;
    const int res = uname(&name);
    if (res != 0) {
        throw OperatingSystemError(
            "OS detection failed. 'uname' returned non-null value", std::to_string(res)
        );
    }

    _operatingSystem = OperatingSystem::Unknown;
    _operatingSystemExtra = std::format(
        "{} {} {} {}", name.sysname, name.release, name.version, name.machine
    );
    _fullOperatingSystem = _operatingSystemExtra;
#endif // WIN32
}

void GeneralCapabilitiesComponent::detectMemory() {
#ifdef WIN32
    try {
        std::string memory;
        // This function might fail if the process has insufficient priviledges to access
        // the WMI on Windows
        queryWMI("Win32_ComputerSystem", "TotalPhysicalMemory", memory);
        std::stringstream convert;
        convert << memory;
        unsigned long long value;
        convert >> value;
        _installedMainMemory = static_cast<unsigned int>((value / 1024) / 1024);
    }
    catch (const std::runtime_error& e) {
        LWARNINGC("GeneralCapabilitiesComponent", e.what());
    }
#else // ^^^^ WIN32 // !WIN32 vvvv
    struct sysinfo memInfo;
    sysinfo(&memInfo);
    _installedMainMemory = static_cast<unsigned int>((memInfo.totalram / 1024) / 1024);
#endif // WIN32
}

void GeneralCapabilitiesComponent::detectCPU() {
    // @TODO This function needs cleanup ---abock
#ifdef WIN32

#ifndef _M_ARM64
    constexpr std::array<std::string_view, 32> szFeatures = {
        "fpu", "vme", "de", "pse", "tsc", "msr", "pae", "mce", "cx8", "apic", "Unknown1",
        "sep", "mtrr", "pge", "mca", "cmov", "pat", "pse36", "psn", "clflush", "Unknown2",
        "ds", "acpi", "mmx", "fxsr", "sse", "sse2", "ss", "ht", "tm", "Unknown4", "pbe"
    };

    // __cpuid with an InfoType argument of 0 returns the number of valid Ids in
    // CPUInfo[0] and the CPU identification string in the other three array elements. The
    // CPU identification string is not in linear order. The code below arranges the
    // information in a human readable form
    int CPUInfo[4] = { -1 };
    __cpuid(CPUInfo, 0);
    unsigned nIds = CPUInfo[0];

    // Get the information associated with each valid Id
    int nFeatureInfo = 0;
    bool hasSSE3NewInstructions = false;
    bool hasMonitorMWait = false;
    bool hasCplQualifiedDebugStore = false;
    bool hasThermalMonitor2 = false;
    for (unsigned i = 0; i <= nIds; i++) {
        __cpuid(CPUInfo, i);

        // Interpret CPU feature information
        if (i == 1) {
            hasSSE3NewInstructions = (CPUInfo[2] & 0x1);
            hasMonitorMWait = (CPUInfo[2] & 0x8);
            hasCplQualifiedDebugStore = (CPUInfo[2] & 0x10);
            hasThermalMonitor2 = (CPUInfo[2] & 0x100);
            nFeatureInfo = CPUInfo[3];
        }
    }

    // Calling __cpuid with 0x80000000 as the InfoType argument gets the number of valid
    // extended IDs
    __cpuid(CPUInfo, 0x80000000);
    unsigned nExIds = CPUInfo[0];

    char CPUBrandString[64];
    std::memset(CPUBrandString, 0, sizeof(CPUBrandString));

    // Get the information associated with each extended ID
    for (unsigned i = 0x80000000; i <= nExIds; i++) {
        __cpuid(CPUInfo, i);

        // Interpret CPU brand string and cache information
        if (i == 0x80000002) {
            std::memcpy(CPUBrandString, CPUInfo, sizeof(CPUInfo));
        }
        else if (i == 0x80000003) {
            std::memcpy(CPUBrandString + 16, CPUInfo, sizeof(CPUInfo));
        }
        else if (i == 0x80000004) {
            std::memcpy(CPUBrandString + 32, CPUInfo, sizeof(CPUInfo));
        }
        else if (i == 0x80000006) {
            _cacheLineSize = CPUInfo[2] & 0xFF;
            _L2Associativity = (CPUInfo[2] >> 12) & 0xF;
            _cacheSize = (CPUInfo[2] >> 16) & 0xFFFF;
        }
    }

    // Get extensions list
    std::stringstream extensions;
    if (hasSSE3NewInstructions) {
        extensions << "sse3 ";
    }
    if (hasMonitorMWait) {
        // @TODO:  "MONITOR/MWAIT" is this correct? ---jonasstrandstedt
        extensions << "mwait ";
    }
    if (hasCplQualifiedDebugStore) {
        extensions << "ds_cpl ";
    }
    if (hasThermalMonitor2) {
        extensions << "tm2 ";
    }

    for (size_t i = 0; i < szFeatures.size(); i++, nIds <<= 1) {
        if (nFeatureInfo & nIds) {
            extensions << szFeatures[i] << " ";
        }
    }

    // Set CPU name
    _cpu = CPUBrandString;

    // Set extensions and remove trailing ", "
    _extensions = extensions.str();
    if (_extensions.length() > 1) {
        _extensions = _extensions.substr(0, _extensions.length() - 1);
    }

    // Get the cores
    SYSTEM_INFO systemInfo;
    GetNativeSystemInfo(&systemInfo);
    _cores = systemInfo.dwNumberOfProcessors;
#else // ^^^^ WIN32 // _M_ARM64 vvvv
    _cpu = "arm64";

    SYSTEM_INFO systemInfo;
    GetNativeSystemInfo(&systemInfo);
    _cores = systemInfo.dwNumberOfProcessors;
#endif // _M_ARM64
#else // ^^^^ WIN32 // !WIN32 vvvv
    FILE* file = nullptr;
    const unsigned int maxSize = 2048;
    char line[maxSize];

    // We must use c-style file opening because /proc is no ordinary filesystem
    file = fopen("/proc/cpuinfo", "r");
    if (file) {
        while (fgets(line, maxSize, file) != nullptr) {
            if (strncmp(line, "processor", 9) == 0) {
                _cores++;
            }
            if (strncmp(line, "model name", 10) == 0) {
                _cpu = line;
                _cpu = _cpu.substr(18, _cpu.length()-19);
            }
            if (strncmp(line, "cache size", 10) == 0) {
                std::string tmp = line;
                tmp = tmp.substr(13, tmp.length() - 14);
                _cacheSize = static_cast<unsigned int>(strtol(tmp.c_str(), nullptr, 0));

            }
            if (strncmp(line, "flags", 5) == 0) {
                _extensions = line;
                _extensions = _extensions.substr(9, _extensions.length() - 10);
            }
            memset(line, 0, maxSize);
        }
        fclose(file);
    }

    file = fopen("/sys/devices/system/cpu/cpu0/cache/index0/coherency_line_size", "r");
    if (file) {
        if (fgets(line, maxSize, file) != nullptr) {
            _cacheLineSize = static_cast<unsigned int>(strtol(line, nullptr, 0));
        }
        fclose(file);
    }

    file = fopen("/sys/devices/system/cpu/cpu0/cache/index0/ways_of_associativity", "r");
    if (file) {
        if (fgets(line, maxSize, file) != nullptr) {
            _L2Associativity = static_cast<unsigned int>(strtol(line, nullptr, 0));
        }
        fclose(file);
    }
#endif // WIN32
}

std::vector<SystemCapabilitiesComponent::CapabilityInformation>
GeneralCapabilitiesComponent::capabilities() const
{
    return {
        { "Operating System", operatingSystemString(), Verbosity::Minimal },
        { "CPU", _cpu, Verbosity::Default },
        { "Cores", std::to_string(_cores), Verbosity::Default },
        { "Cache line size", std::to_string(_cacheLineSize), Verbosity::Full },
        { "L2 Associativity", std::to_string(_L2Associativity), Verbosity::Full },
        { "Cache size", std::to_string(_cacheSize) + " KB", Verbosity::Full },
        { "Extensions", _extensions,Verbosity::Full },
        {
            "Main Memory",
            std::to_string(_installedMainMemory) + " MB",
            Verbosity::Default
        }
    };
}

GeneralCapabilitiesComponent::OperatingSystem
GeneralCapabilitiesComponent::operatingSystem() const
{
    return _operatingSystem;
}

std::string GeneralCapabilitiesComponent::operatingSystemString() const {
    return to_string(_operatingSystem);
}

const std::string& GeneralCapabilitiesComponent::fullOperatingSystem() const {
    return _fullOperatingSystem;
}

unsigned int GeneralCapabilitiesComponent::installedMainMemory() const {
    return _installedMainMemory;
}

unsigned int GeneralCapabilitiesComponent::cores() const {
    return _cores;
}

unsigned int GeneralCapabilitiesComponent::cacheLineSize() const {
    return _cacheLineSize;
}

unsigned int GeneralCapabilitiesComponent::L2Associativity() const {
    return _L2Associativity;
}

unsigned int GeneralCapabilitiesComponent::cacheSize() const {
    return _cacheSize;
}

const std::string& GeneralCapabilitiesComponent::extensions() const {
    return _extensions;
}

std::string_view GeneralCapabilitiesComponent::name() const {
    return "CPU";
}

} // namespace ghoul::systemcapabilities
