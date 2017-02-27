/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/scripting/script_helper.h>
#include <openspace/scripting/scriptengine.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/misc.h>
#include <ghoul/systemcapabilities/generalcapabilitiescomponent.h>
#include <ghoul/systemcapabilities/openglcapabilitiescomponent.h>

#include <cctype>

using namespace ghoul::lua;
using namespace ghoul::systemcapabilities;

namespace luascripting {
namespace general {

int operatingSystem(lua_State* L) {
    lua_pushstring(L, CpuCap.operatingSystemString().c_str());
    return 1;
}

int fullOperatingSystem(lua_State* L) {
    lua_pushstring(L, CpuCap.fullOperatingSystem().c_str());
    return 1;
}

int installedMainMemory(lua_State* L) {
    lua_pushnumber(L, CpuCap.installedMainMemory());
    return 1;
}

int cores(lua_State* L) {
    lua_pushnumber(L, CpuCap.cores());
    return 1;
}
    
int cacheLineSize(lua_State* L) {
    lua_pushnumber(L, CpuCap.cacheLineSize());
    return 1;
}

int L2Associativity(lua_State* L) {
    lua_pushnumber(L, CpuCap.L2Associativity());
    return 1;
}
    
int cacheSize(lua_State* L) {
    lua_pushnumber(L, CpuCap.cacheSize());
    return 1;
}
    
int extensions(lua_State* L) {
    lua_pushstring(L, CpuCap.extensions().c_str());
    return 1;

}
    
} // namespace general

namespace opengl {
    
int hasOpenGLVersion(lua_State* L) {
    int nArguments = lua_gettop(L);
    SCRIPT_CHECK_ARGUMENTS("hasVersion", L, 1, nArguments);
    
    std::vector<std::string> v = ghoul::tokenizeString(luaL_checkstring(L, -1));
    if (v.size() != 2 && v.size() != 3) {
        LERRORC("hasVersion", ghoul::lua::errorLocation(L) << "Malformed version string");
        return 0;
    }
    
    for (const std::string& i : v) {
        for (char c : i) {
            if (!std::isdigit(c)) {
                LERRORC(
                    "hasVersion",
                    ghoul::lua::errorLocation(L) << "Malformed version string"
                );
                return 0;
                
            }
        }
    }

    int major = std::stoi(v[0]);
    int minor = std::stoi(v[1]);
    int release = v.size() == 3 ? std::stoi(v[2]) : 0;
    OpenGLCapabilitiesComponent::Version version = { major, minor, release };
    
    bool supported = OpenGLCap.openGLVersion() >= version;
    
    lua_pushboolean(L, supported);
    
    return 1;
}
    
int openGLVersion(lua_State* L) {
    lua_pushstring(L, OpenGLCap.openGLVersion().toString().c_str());
    return 1;
}
    
int glslCompiler(lua_State* L) {
    lua_pushstring(L, OpenGLCap.glslCompiler().c_str());
    return 1;
}
    
int gpuVendor(lua_State* L) {
    lua_pushstring(L, OpenGLCap.gpuVendorString().c_str());
    return 1;
}
    
int extensions(lua_State* L) {
    const std::vector<std::string>& extensions = OpenGLCap.extensions();
    
    lua_newtable(L);
    
    for (int i = 1; i <= extensions.size(); ++i) {
        lua_pushstring(L, extensions[i].c_str());
        lua_rawseti(L, -2, i);
    }
    return 1;
}
    
int isExtensionSupported(lua_State* L) {
    int nArguments = lua_gettop(L);
    SCRIPT_CHECK_ARGUMENTS("hasVersion", L, 1, nArguments);

    std::string extension = luaL_checkstring(L, -1);
    
    lua_pushboolean(L, OpenGLCap.isExtensionSupported(extension));
    return 1;
}
    
int maxTextureUnits(lua_State* L) {
    lua_pushnumber(L, OpenGLCap.maxTextureUnits());
    return 1;
}
    
int max2DTextureSize(lua_State* L) {
    lua_pushnumber(L, OpenGLCap.max2DTextureSize());
    return 1;
}
    
int max3DTextureSize(lua_State* L) {
    lua_pushnumber(L, OpenGLCap.max3DTextureSize());
    return 1;
}
    
int maxAtomicCounterBufferBindings(lua_State* L) {
    lua_pushnumber(L, OpenGLCap.maxAtomicCounterBufferBindings());
    return 1;
}
    
int maxShaderStorageBufferBindings(lua_State* L) {
    lua_pushnumber(L, OpenGLCap.maxShaderStorageBufferBindings());
    return 1;
}
    
int maxUniformBufferBindings(lua_State* L) {
    lua_pushnumber(L, OpenGLCap.maxUniformBufferBindings());
    return 1;
}
    

} // namespace opengl
} // namespace luascripting

namespace openspace {
namespace scripting {

LuaLibrary generalSystemCapabilities() {
    return {
        "systemCapabilities",
        {
            {
                "operatingSystem",
                &luascripting::general::operatingSystem,
                "",
                "Returns a parsed string of the operating system type, for example "
                "Windows, Linux, MacOS, or others, together with the specific version, "
                "where available."
            },
            {
                "fullOperatingSystem",
                &luascripting::general::fullOperatingSystem,
                "",
                "Returns the operating system as a string. The exact format of the "
                "returned string is implementation and operating system-dependent but it "
                "should contain the manufacturer and the version."
            },
            {
                "installedMainMemory",
                &luascripting::general::installedMainMemory,
                "",
                "Returns the amount of available, installed main memory (RAM) on the "
                "system in MB."
            },
            {
                "cores",
                &luascripting::general::cores,
                "",
                "Returns the number of cores."
            },
            {
                "cacheLineSize",
                &luascripting::general::cacheLineSize,
                "",
                "Returns the cache line size."
            },
            {
                "L2Associativity",
                &luascripting::general::L2Associativity,
                "",
                "Returns the L2 associativity."
            },
            {
                "cacheSize",
                &luascripting::general::cacheSize,
                "",
                "Returns the cache size."
            },
            {
                "extensions",
                &luascripting::general::extensions,
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
                "string",
                "Tests whether the current instance supports the passed OpenGL version. "
                "The parameter has to have the form 'X.Y' or 'X.Y.Z'."
            },
            {
                "openGLVersion",
                &luascripting::opengl::openGLVersion,
                "",
                "Returns the maximum OpenGL version that is supported on this platform."
            },
            {
                "glslCompiler",
                &luascripting::opengl::glslCompiler,
                "",
                "Returns the value of a call to <code>glGetString(GL_VENDOR)</code>. "
                "This will give detailed information about the vendor of the main "
                "graphics card. This string can be used if the automatic Vendor "
                "detection failed."
            },
            {
                "gpuVendor",
                &luascripting::opengl::gpuVendor,
                "",
                "Returns the vendor of the main graphics card."
            },
            {
                "extensions",
                &luascripting::opengl::extensions,
                "",
                "Returns all available extensions as a list of names."
            },
            {
                "isExtensionSupported",
                &luascripting::opengl::isExtensionSupported,
                "string",
                "Checks is a specific <code>extension</code> is supported or not."
            },
            {
                "maxTextureUnits",
                &luascripting::opengl::maxTextureUnits,
                "",
                "Returns the maximum number of texture units that are available on the "
                "main graphics card."
            },
            {
                "max2DTextureSize",
                &luascripting::opengl::max2DTextureSize,
                "",
                "Returns the largest dimension for a 2D texture on this graphics card."
            },
            {
                "max3DTextureSize",
                &luascripting::opengl::max3DTextureSize,
                "",
                "Returns the largest dimension for a 3D texture on this graphics card."
            },
            {
                "maxAtomicCounterBufferBindings",
                &luascripting::opengl::maxAtomicCounterBufferBindings,
                "",
                "Returns the maximum number of atomic counter buffer bindings that are "
                "available on the main graphics card."
            },
            {
                "maxShaderStorageBufferBindings",
                &luascripting::opengl::maxShaderStorageBufferBindings,
                "",
                "Returns the maximum number of shader storage bindings that are "
                "available on the main graphics card."
            },
            {
                "maxUniformBufferBindings",
                &luascripting::opengl::maxUniformBufferBindings,
                "",
                "Returns the maximum number of uniform buffer bindings that are "
                "available on the main graphics card."
            }
        }
    };
}

} // namespace scripting
} // namespace openspace
