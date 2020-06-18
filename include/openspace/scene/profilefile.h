/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_CORE___PROFILEFILE___H__
#define __OPENSPACE_CORE___PROFILEFILE___H__

#include <string>
#include <istream>
#include <fstream>
#include <functional>
#include <optional>
#include <variant>
#include <vector>

#include <ghoul/systemcapabilities/version.h>

namespace ghoul { class Dictionary; }
namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

namespace documentation { struct Documentation; }
namespace scripting { struct LuaLibrary; }

struct ProfileStruct {
    // Version
    struct Version {
        int major = 0;
        int minor = 0;
        int patch = 0;
    };
    Version version = { 1, 0, 0 };

    struct Module {
        std::string name;
        std::string loadedInstruction;
        std::string notLoadedInstruction;
    };
    std::vector<Module> modules;

    struct Asset {
        enum class Type {
            Require,
            Request
        };

        std::string path;
        Type type;
        std::string name;
    };
    std::vector<Asset> assets;

    struct Property {
        enum class SetType {
            SetPropertyValue,
            SetPropertyValueSingle
        };

        SetType setType;
        std::string name;
        std::string value;
    };
    std::vector<Property> properties;

    struct Keybinding {
        std::string key; // @TODO (abock, 2020-06-16) change to key+action
        std::string documentation;
        std::string name;
        std::string guiPath;
        bool isLocal;
        std::string script;
    };
    std::vector<Keybinding> keybindings;

    struct Time {
        enum class Type {
            Absolute,
            Relative
        };

        Type type;
        std::string time;
    };
    Time time;

    struct CameraNavState {
        static constexpr const char* Type = "setNavigationState";

        std::string anchor;
        std::string aim;
        std::string referenceFrame;
        std::string position; // @TODO (abock, 2020-06-17) change to vec3
        std::string up;// @TODO (abock, 2020-06-17) change to vec3
        std::string yaw;
        std::string pitch;
    };
    struct CameraGoToGeo {
        static constexpr const char* Type = "goToGeo";

        std::string anchor;
        double latitude;
        double longitude;
        std::optional<double> altitude;
    };
    std::variant<CameraNavState, CameraGoToGeo> camera;

    std::vector<std::string> markNodes;
};

std::string serialize(const ProfileStruct& ps);
ProfileStruct deserialize(const std::vector<std::string>& content);
 
std::string convertToSceneFile(const ProfileStruct& ps);

ProfileStruct readFromFile(const std::string& filename);

} // namespace openspace

#endif // __OPENSPACE_CORE___PROFILEFILE___H__
