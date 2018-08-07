/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/scripting/systemcapabilitiesbinding.h>

#include <openspace/scripting/lualibrary.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/misc.h>
#include <ghoul/systemcapabilities/generalcapabilitiescomponent.h>
#include <ghoul/systemcapabilities/openglcapabilitiescomponent.h>
#include <cctype>

using namespace ghoul::lua;
using namespace ghoul::systemcapabilities;

namespace luascripting::general {

int operatingSystem(lua_State* L) {
    using OS = ghoul::systemcapabilities::GeneralCapabilitiesComponent::OperatingSystem;
    OS os = CpuCap.operatingSystem();

    switch (os) {
        case OS::Windows10:
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
            ghoul::lua::push(L, "windows");
            break;
        case OS::Linux:
            ghoul::lua::push(L, "linux");
            break;
        case OS::MacOS:
            ghoul::lua::push(L, "macos");
            break;
        default:
            ghoul::lua::push(L, "other");
            break;
    }

    return 1;
}

int fullOperatingSystem(lua_State* L) {
    ghoul::lua::push(L, CpuCap.fullOperatingSystem());
    return 1;
}

int installedMainMemory(lua_State* L) {
    ghoul::lua::push(L, CpuCap.installedMainMemory());
    return 1;
}

int cores(lua_State* L) {
    ghoul::lua::push(L, CpuCap.cores());
    return 1;
}

int cacheLineSize(lua_State* L) {
    ghoul::lua::push(L, CpuCap.cacheLineSize());
    return 1;
}

int L2Associativity(lua_State* L) {
    ghoul::lua::push(L, CpuCap.L2Associativity());
    return 1;
}

int cacheSize(lua_State* L) {
    ghoul::lua::push(L, CpuCap.cacheSize());
    return 1;
}

int extensions(lua_State* L) {
    ghoul::lua::push(L, CpuCap.extensions());
    return 1;
}

} // namespace luascripting::general

namespace luascripting::opengl {

int hasOpenGLVersion(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::hasOpenGLVersion");

    std::vector<std::string> v = ghoul::tokenizeString(
        ghoul::lua::value<std::string>(L, 1, ghoul::lua::PopValue::Yes)
    );
    if (v.size() != 2 && v.size() != 3) {
        LERRORC(
            "hasVersion",
            fmt::format("{}: Malformed version string", ghoul::lua::errorLocation(L))
        );
        return 0;
    }

    for (const std::string& i : v) {
        for (char c : i) {
            if (!std::isdigit(c)) {
                LERRORC(
                    "hasVersion",
                    fmt::format(
                        "{}: Malformed version string", ghoul::lua::errorLocation(L)
                    )
                );
                return 0;
            }
        }
    }

    const int major = std::stoi(v[0]);
    const int minor = std::stoi(v[1]);
    const int release = v.size() == 3 ? std::stoi(v[2]) : 0;
    const Version version = { major, minor, release };

    const bool supported = OpenGLCap.openGLVersion() >= version;

    ghoul::lua::push(L, supported);
    return 1;
}

int openGLVersion(lua_State* L) {
    ghoul::lua::push(L, ghoul::to_string(OpenGLCap.openGLVersion()));
    return 1;
}

int glslCompiler(lua_State* L) {
    ghoul::lua::push(L, OpenGLCap.glslCompiler());
    return 1;
}

int gpuVendor(lua_State* L) {
    ghoul::lua::push(L, OpenGLCap.gpuVendorString());
    return 1;
}

int extensions(lua_State* L) {
    const std::vector<std::string>& extensions = OpenGLCap.extensions();

    lua_newtable(L);

    for (size_t i = 1; i <= extensions.size(); ++i) {
        ghoul::lua::push(L, extensions[i]);
        lua_rawseti(L, -2, i);
    }
    return 1;
}

int isExtensionSupported(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::hasExtension");

    std::string extension = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
    );

    ghoul::lua::push(L, OpenGLCap.isExtensionSupported(extension));
    return 1;
}

int maxTextureUnits(lua_State* L) {
    ghoul::lua::push(L, OpenGLCap.maxTextureUnits());
    return 1;
}

int max2DTextureSize(lua_State* L) {
    ghoul::lua::push(L, OpenGLCap.max2DTextureSize());
    return 1;
}

int max3DTextureSize(lua_State* L) {
    ghoul::lua::push(L, OpenGLCap.max3DTextureSize());
    return 1;
}

int maxAtomicCounterBufferBindings(lua_State* L) {
    ghoul::lua::push(L, OpenGLCap.maxAtomicCounterBufferBindings());
    return 1;
}

int maxShaderStorageBufferBindings(lua_State* L) {
    ghoul::lua::push(L, OpenGLCap.maxShaderStorageBufferBindings());
    return 1;
}

int maxUniformBufferBindings(lua_State* L) {
    ghoul::lua::push(L, OpenGLCap.maxUniformBufferBindings());
    return 1;
}


} // namespace luascripting::opengl

namespace openspace::scripting {

LuaLibrary generalSystemCapabilities() {
    return {
        "systemCapabilities",
        {
            {
                "os",
                &luascripting::general::operatingSystem,
                {},
                "",
                "This function returns a string identifying the currently running "
                "operating system. For Windows, the string is 'windows', for MacOS, it "
                "is 'osx', and for Linux it is 'linux'. For any other operating system, "
                "this function returns 'other'."
            },
            {
                "fullOperatingSystem",
                &luascripting::general::fullOperatingSystem,
                {},
                "",
                "Returns the operating system as a string. The exact format of the "
                "returned string is implementation and operating system-dependent but it "
                "should contain the manufacturer and the version."
            },
            {
                "installedMainMemory",
                &luascripting::general::installedMainMemory,
                {},
                "",
                "Returns the amount of available, installed main memory (RAM) on the "
                "system in MB."
            },
            {
                "cores",
                &luascripting::general::cores,
                {},
                "",
                "Returns the number of cores."
            },
            {
                "cacheLineSize",
                &luascripting::general::cacheLineSize,
                {},
                "",
                "Returns the cache line size."
            },
            {
                "L2Associativity",
                &luascripting::general::L2Associativity,
                {},
                "",
                "Returns the L2 associativity."
            },
            {
                "cacheSize",
                &luascripting::general::cacheSize,
                {},
                "",
                "Returns the cache size."
            },
            {
                "extensions",
                &luascripting::general::extensions,
                {},
                "",
                "Returns all supported exteions as comma-separated string."
            }
        }
    };
}

LuaLibrary openglSystemCapabilities() {
    return {
        "openglCapabilities",
        {
            {
                "hasOpenGLVersion",
                &luascripting::opengl::hasOpenGLVersion,
                {},
                "string",
                "Tests whether the current instance supports the passed OpenGL version. "
                "The parameter has to have the form 'X.Y' or 'X.Y.Z'."
            },
            {
                "openGLVersion",
                &luascripting::opengl::openGLVersion,
                {},
                "",
                "Returns the maximum OpenGL version that is supported on this platform."
            },
            {
                "glslCompiler",
                &luascripting::opengl::glslCompiler,
                {},
                "",
                "Returns the value of a call to <code>glGetString(GL_VENDOR)</code>. "
                "This will give detailed information about the vendor of the main "
                "graphics card. This string can be used if the automatic Vendor "
                "detection failed."
            },
            {
                "gpuVendor",
                &luascripting::opengl::gpuVendor,
                {},
                "",
                "Returns the vendor of the main graphics card."
            },
            {
                "extensions",
                &luascripting::opengl::extensions,
                {},
                "",
                "Returns all available extensions as a list of names."
            },
            {
                "isExtensionSupported",
                &luascripting::opengl::isExtensionSupported,
                {},
                "string",
                "Checks is a specific <code>extension</code> is supported or not."
            },
            {
                "maxTextureUnits",
                &luascripting::opengl::maxTextureUnits,
                {},
                "",
                "Returns the maximum number of texture units that are available on the "
                "main graphics card."
            },
            {
                "max2DTextureSize",
                &luascripting::opengl::max2DTextureSize,
                {},
                "",
                "Returns the largest dimension for a 2D texture on this graphics card."
            },
            {
                "max3DTextureSize",
                &luascripting::opengl::max3DTextureSize,
                {},
                "",
                "Returns the largest dimension for a 3D texture on this graphics card."
            },
            {
                "maxAtomicCounterBufferBindings",
                &luascripting::opengl::maxAtomicCounterBufferBindings,
                {},
                "",
                "Returns the maximum number of atomic counter buffer bindings that are "
                "available on the main graphics card."
            },
            {
                "maxShaderStorageBufferBindings",
                &luascripting::opengl::maxShaderStorageBufferBindings,
                {},
                "",
                "Returns the maximum number of shader storage bindings that are "
                "available on the main graphics card."
            },
            {
                "maxUniformBufferBindings",
                &luascripting::opengl::maxUniformBufferBindings,
                {},
                "",
                "Returns the maximum number of uniform buffer bindings that are "
                "available on the main graphics card."
            }
        }
    };
}

} // namespace openspace::scripting
