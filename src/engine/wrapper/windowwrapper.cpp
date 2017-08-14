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

#include <openspace/engine/wrapper/windowwrapper.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/scripting/scriptengine.h>

#include <ghoul/misc/exception.h>
#include <string>

namespace luascriptfunctions {

int setSynchronization(lua_State* L) {
    int nArguments = lua_gettop(L);
    if (nArguments != 1) {
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);
    }

    bool b = lua_toboolean(L, -1) != 0;
    OsEng.windowWrapper().setSynchronization(b);
    return 0;
}

} // namespace luascriptfunctions

namespace openspace {

WindowWrapper::WindowWrapperException::WindowWrapperException(const std::string& msg)
    : ghoul::RuntimeError(msg, "WindowWrapper")
{}

WindowWrapper::WindowWrapper()
    : properties::PropertyOwner({ "WindowWrapper" })
{}

scripting::LuaLibrary WindowWrapper::luaLibrary() {
    return {
        "cluster",
        {
            {
                "setSynchronization",
                &luascriptfunctions::setSynchronization,
                {},
                "bool",
                "Enables or disables the frame synchronization of the cluster. If the "
                "synchronization is enabled, the computers in the cluster will swap "
                "their backbuffers at the same time, either supported by hardware or by "
                "network signals."
            }
        }
    };
}

void WindowWrapper::terminate() {}
    
void WindowWrapper::setBarrier(bool) {}

void WindowWrapper::setSynchronization(bool) {}
    
void WindowWrapper::clearAllWindows(const glm::vec4&) {}

bool WindowWrapper::windowHasResized() const {
    return false;
}
    
double WindowWrapper::averageDeltaTime() const {
    return 0.0;
}

double WindowWrapper::deltaTime() const {
    return 0.0;
}
    
glm::vec2 WindowWrapper::mousePosition() const {
    return glm::vec2(0.f);
}
    
uint32_t WindowWrapper::mouseButtons(int) const {
    return uint32_t(0);
}
    
glm::ivec2 WindowWrapper::currentWindowSize() const {
    return glm::ivec2(0);
}
    
glm::ivec2 WindowWrapper::currentWindowResolution() const {
    return currentWindowSize();
}

glm::ivec2 WindowWrapper::currentDrawBufferResolution() const {
    return currentWindowSize();
}
    
glm::vec2 WindowWrapper::dpiScaling() const {
    return glm::vec2(1.f);
}

int WindowWrapper::currentNumberOfAaSamples() const {
    return 1;
}

bool WindowWrapper::isRegularRendering() const {
    return true;
}

bool WindowWrapper::hasGuiWindow() const {
    return false;
}

bool WindowWrapper::isGuiWindow() const {
    return false;
}

bool WindowWrapper::isMaster() const {
    return false;
}

bool WindowWrapper::isSwapGroupMaster() const {
    return false;
}

bool WindowWrapper::isUsingSwapGroups() const {
    return false;
}


glm::mat4 WindowWrapper::viewProjectionMatrix() const {
    return glm::mat4(1.f);
}

glm::mat4 WindowWrapper::modelMatrix() const {
    return glm::mat4(1.f);
}
    
void WindowWrapper::setNearFarClippingPlane(float, float) {}

void WindowWrapper::setEyeSeparationDistance(float) {}
    
glm::ivec4 WindowWrapper::viewportPixelCoordinates() const {
    return glm::ivec4(
        0,
        currentWindowResolution().x,
        0,
        currentWindowResolution().y
    );
}
    
    
bool WindowWrapper::isExternalControlConnected() const {
    return false;
}
    
void WindowWrapper::sendMessageToExternalControl(const std::vector<char>&) const {
}
    
bool WindowWrapper::isSimpleRendering() const {
    return true;
}
    
void WindowWrapper::takeScreenshot(bool) const {}
    
} // namespace openspace
