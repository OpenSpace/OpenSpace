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

const size_t timeLinesExpected = 1;
const size_t cameraLinesExpected = 1;
const size_t moduleFieldsExpected = 3;
const size_t assetFieldsExpected = 3;
const size_t propertyFieldsExpected = 3;
const size_t keybindingFieldsExpected = 6;
const size_t timeFieldsExpected = 2;
const size_t cameraNavigationFieldsExpected = 8;
const size_t cameraGeoFieldsExpected = 5;
const size_t markNodesFieldsExpected = 1;

const size_t moduleFieldName = 0;
const size_t moduleFieldLoaded = 1;
const size_t moduleFieldNotLoaded = 2;
const size_t assetFieldName = 0;
const size_t assetFieldReqd = 1;
const size_t propertyFieldType = 0;
const size_t propertyFieldName = 1;
const size_t propertyFieldValue = 2;
const size_t keybindingFieldKey = 0;
const size_t keybindingFieldDoc = 1;
const size_t keybindingFieldName = 2;
const size_t keybindingFieldGuiPath = 3;
const size_t keybindingFieldLocal = 4;
const size_t keybindingFieldCommand = 5;
const size_t timeFieldType = 0;
const size_t timeFieldSet = 1;
const size_t cameraFieldType = 0;
const size_t cameraNavigationFieldAnchor = 1;
const size_t cameraNavigationFieldAim = 2;
const size_t cameraNavigationFieldRef = 3;
const size_t cameraNavigationFieldPosition = 4;
const size_t cameraNavigationFieldUp = 5;
const size_t cameraNavigationFieldYaw = 6;
const size_t cameraNavigationFieldPitch = 7;
const size_t cameraGeoFieldAnchor = 1;
const size_t cameraGeoFieldLatitude = 2;
const size_t cameraGeoFieldLongitude = 3;
const size_t cameraGeoFieldAltitude = 4;

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


class ProfileFile {
public:
    ProfileStruct profile;
    /**
     * Constructs object by reading the contents of a profile file and populates vector
     * containers for all sections. This only pulls individual line entries into their
     * proper sections; it does not parse the tab-delimited fields of each line.
     * \param filename The profile file to read
     */
    ProfileFile(const std::string& filename);
};

} // namespace openspace

#endif // __OPENSPACE_CORE___PROFILEFILE___H__
