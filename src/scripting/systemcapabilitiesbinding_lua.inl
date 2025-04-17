/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <ghoul/lua/lua_helper.h>

namespace {

/**
 * This function returns a string identifying the currently running operating system. For
 * Windows, the string is 'windows', for MacOS, it is 'osx', and for Linux it is 'linux'.
 * For any other operating system, this function returns 'other'.
 */
[[codegen::luawrap("os")]] std::string operatingSystem() {
    using OS = ghoul::systemcapabilities::GeneralCapabilitiesComponent::OperatingSystem;
    OS os = CpuCap.operatingSystem();

    switch (os) {
        case OS::Windows10or11:
        case OS::WindowsServer2016:
        case OS::WindowsVista:
        case OS::WindowsServer2008:
        case OS::Windows7:
        case OS::WindowsServer2008R2:
        case OS::Windows8:
        case OS::WindowsServer2012:
        case OS::Windows81:
        case OS::WindowsServer2012R2:
        case OS::WindowsServer2003R2:
        case OS::WindowsStorageServer2003:
        case OS::WindowsXPProfx64:
        case OS::WindowsServer2003:
        case OS::WindowsXPHome:
        case OS::WindowsXPProf:
        case OS::Windows2000Prof:
        case OS::Windows2000DatacenterServer:
        case OS::Windows2000AdvancedServer:
        case OS::Windows2000Server:
            return "windows";
        case OS::Linux:
            return "linux";
        case OS::MacOS:
            return "macos";
        default:
            return "other";
    }
}

/**
 * Returns the operating system as a string. The exact format of the returned string is
 * implementation and operating system-dependent but it should contain the manufacturer
 * and the version.
 */
[[codegen::luawrap]] std::string fullOperatingSystem() {
    std::string os = CpuCap.fullOperatingSystem();
    return os;
}

/**
 * Returns the amount of available, installed main memory (RAM) on the system in MB.
 */
[[codegen::luawrap]] int installedMainMemory() {
    int memory = static_cast<int>(CpuCap.installedMainMemory());
    return memory;
}

// Returns the number of cores.
[[codegen::luawrap]] int cores() {
    int nCores = static_cast<int>(CpuCap.cores());
    return nCores;
}

// Returns the cache line size.
[[codegen::luawrap]] int cacheLineSize() {
    int cls = static_cast<int>(CpuCap.cacheLineSize());
    return cls;
}

// Returns the L2 associativity.
[[codegen::luawrap("L2Associativity")]] int l2Associativity() {
    int assoc = static_cast<int>(CpuCap.L2Associativity());
    return assoc;
}

// Returns the cache size.
[[codegen::luawrap]] int cacheSize() {
    int cs = static_cast<int>(CpuCap.cacheSize());
    return cs;
}

// Returns all supported exteions as comma-separated string.
[[codegen::luawrap("extensions")]] std::string cpuExtensions() {
    std::string ext = CpuCap.extensions();
    return ext;
}

/**
 * Tests whether the current instance supports the passed OpenGL version. The parameter
 * has to have the form 'X.Y' or 'X.Y.Z'.
 */
[[codegen::luawrap]] bool hasOpenGLVersion(std::string version) {
    std::vector<std::string> components = ghoul::tokenizeString(version);
    if (components.size() != 2 && components.size() != 3) {
        throw ghoul::lua::LuaError("Malformed version string");
    }

    for (const std::string& i : components) {
        for (char c : i) {
            if (!std::isdigit(c)) {
                throw ghoul::lua::LuaError("Malformed version string");
            }
        }
    }

    const int major = std::stoi(components[0]);
    const int minor = std::stoi(components[1]);
    const int release = components.size() == 3 ? std::stoi(components[2]) : 0;
    const ghoul::systemcapabilities::Version ver = { major, minor, release };

    const bool supported = OpenGLCap.openGLVersion() >= ver;
    return supported;
}

// Returns the maximum OpenGL version that is supported on this platform.
[[codegen::luawrap]] std::string openGLVersion() {
    ghoul::systemcapabilities::Version version = OpenGLCap.openGLVersion();
    return ghoul::to_string(version);
}

/**
 * Returns the value of a call to `glGetString(GL_VENDOR)`. This will give
 * detailed information about the vendor of the main graphics card. This string can be
 * used if the automatic Vendor detection failed.
 */
[[codegen::luawrap]] std::string glslCompiler() {
    std::string compiler = OpenGLCap.glslCompiler();
    return compiler;
}

// Returns the vendor of the main graphics card.
[[codegen::luawrap]] std::string gpuVendor() {
    std::string_view vendor = OpenGLCap.gpuVendorString();
    return std::string(vendor);
}

// Returns all available extensions as a list of names.
[[codegen::luawrap("extensions")]] std::vector<std::string> gpuExtensions() {
    std::vector<std::string> extensions = OpenGLCap.extensions();
    return extensions;
}

// Checks is a specific `extension` is supported or not.
[[codegen::luawrap]] bool isExtensionSupported(std::string extension) {
    bool isSupported = OpenGLCap.isExtensionSupported(extension);
    return isSupported;
}

/**
 * Returns the maximum number of texture units that are available on the main graphics
 * card.
 */
[[codegen::luawrap]] int maxTextureUnits() {
    int units = OpenGLCap.maxTextureUnits();
    return units;
}

// Returns the largest dimension for a 2D texture on this graphics card.
[[codegen::luawrap]] int max2DTextureSize() {
    int size = OpenGLCap.max2DTextureSize();
    return size;
}

// Returns the largest dimension for a 3D texture on this graphics card.
[[codegen::luawrap]] int max3DTextureSize() {
    int size = OpenGLCap.max3DTextureSize();
    return size;
}

/**
 * Returns the maximum number of atomic counter buffer bindings that are available on the
 * main graphics card.
 */
[[codegen::luawrap]] int maxAtomicCounterBufferBindings() {
    int bindings = OpenGLCap.maxAtomicCounterBufferBindings();
    return bindings;
}

/**
 * Returns the maximum number of shader storage bindings that are available on the main
 * graphics card.
 */
[[codegen::luawrap]] int maxShaderStorageBufferBindings() {
    int bindings = OpenGLCap.maxShaderStorageBufferBindings();
    return bindings;
}

/**
 * Returns the maximum number of uniform buffer bindings that are available on the main
 * graphics card.
 */
[[codegen::luawrap]] int maxUniformBufferBindings() {
    int bindings = OpenGLCap.maxUniformBufferBindings();
    return bindings;
}

#include "systemcapabilitiesbinding_lua_codegen.cpp"

} // namespace
