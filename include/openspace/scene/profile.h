/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_CORE___PROFILE___H__
#define __OPENSPACE_CORE___PROFILE___H__

#include <openspace/engine/globals.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/util/keys.h>
#include <ghoul/glm.h>
#include <ghoul/misc/exception.h>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace openspace {

namespace interaction { struct NavigationState; }

namespace scripting { struct LuaLibrary; }

class Profile {
public:
    struct ParsingError : public ghoul::RuntimeError {
        enum class Severity { Info, Warning, Error };

        explicit ParsingError(Severity severity_, std::string msg);

        Severity severity;
    };

    // Version
    struct Version {
        int major = 0;
        int minor = 0;
    };
    struct Module {
        std::string name;
        std::optional<std::string> loadedInstruction;
        std::optional<std::string> notLoadedInstruction;
    };
    struct Meta {
        std::optional<std::string> name;
        std::optional<std::string> version;
        std::optional<std::string> description;
        std::optional<std::string> author;
        std::optional<std::string> url;
        std::optional<std::string> license;
    };

    struct Property {
        enum class SetType {
            SetPropertyValue,
            SetPropertyValueSingle
        };

        SetType setType = SetType::SetPropertyValue;
        std::string name;
        std::string value;
    };

    struct Action {
        std::string identifier;
        std::string documentation;
        std::string name;
        std::string guiPath;
        bool isLocal = false;
        std::string script;
    };

    struct Keybinding {
        KeyWithModifier key;
        std::string action;
    };

    struct Time {
        enum class Type {
            Absolute,
            Relative
        };

        Type type = Type::Relative;
        std::string value;
        bool startPaused = false;
    };

    struct CameraGoToNode {
        static constexpr std::string_view Type = "goToNode";

        std::string anchor;
        std::optional<double> height;
    };

    struct CameraNavState {
        static constexpr std::string_view Type = "setNavigationState";

        std::string anchor;
        std::optional<std::string> aim;
        std::string referenceFrame;
        glm::dvec3 position;
        std::optional<glm::dvec3> up;
        std::optional<double> yaw;
        std::optional<double> pitch;
    };

    struct CameraGoToGeo {
        static constexpr std::string_view Type = "goToGeo";

        std::string anchor;
        double latitude = 0.0;
        double longitude = 0.0;
        std::optional<double> altitude;
    };

    using CameraType = std::variant<CameraGoToNode, CameraNavState, CameraGoToGeo>;

    Profile() = default;
    explicit Profile(const std::filesystem::path& path);
    std::string serialize() const;

    /**
     * Saves all current settings, starting from the profile that was loaded at startup,
     * and all of the property & asset changes that were made since startup.
     */
    void saveCurrentSettingsToProfile(const properties::PropertyOwner& rootOwner,
        std::string currentTime, interaction::NavigationState navState);

    /**
     * Adds a new asset and checks for duplicates unless the `ignoreUpdates` member is
     * set to `true`.
     */
    void addAsset(const std::string& path);

    /// Removes an asset unless the `ignoreUpdates` member is set to `true`
    void removeAsset(const std::string& path);

    static constexpr Version CurrentVersion = Version{ 1, 3 };

    Version version = CurrentVersion;
    std::vector<Module> modules;
    std::optional<Meta> meta;
    std::vector<std::string> assets;
    std::vector<Property> properties;
    std::vector<Action> actions;
    std::vector<Keybinding> keybindings;
    std::optional<Time> time;
    std::vector<double> deltaTimes;
    std::optional<CameraType> camera;
    std::vector<std::string> markNodes;
    std::vector<std::string> additionalScripts;

    bool ignoreUpdates = false;

    /**
     * Returns the Lua library that contains all Lua functions available to provide
     * profile functionality.
     *
     * \return The Lua library that contains all Lua functions available for profiles
     */
    static scripting::LuaLibrary luaLibrary();
};

} // namespace openspace

#endif // __OPENSPACE_CORE___PROFILE___H__
