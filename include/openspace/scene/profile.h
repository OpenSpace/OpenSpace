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

#ifndef __OPENSPACE_CORE___PROFILE___H__
#define __OPENSPACE_CORE___PROFILE___H__

#include <openspace/engine/globals.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/util/keys.h>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace openspace {

namespace scripting { struct LuaLibrary; }

class Profile {
public:
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
    struct Asset {
        std::string path;
        std::optional<std::string> name;
    };
    struct Property {
        enum class SetType {
            SetPropertyValue,
            SetPropertyValueSingle
        };

        SetType setType;
        std::string name;
        std::string value;
    };
    struct Keybinding {
        KeyWithModifier key;
        std::string documentation;
        std::string name;
        std::string guiPath;
        bool isLocal;
        std::string script;
    };
    struct Time {
        enum class Type {
            Absolute,
            Relative
        };

        Type type;
        std::string value;
    };
    struct CameraNavState {
        static constexpr const char* Type = "setNavigationState";

        std::string anchor;
        std::string aim;
        std::string referenceFrame;
        glm::dvec3 position;
        std::optional<glm::dvec3> up;
        std::optional<double> yaw;
        std::optional<double> pitch;
    };
    struct CameraGoToGeo {
        static constexpr const char* Type = "goToGeo";

        std::string anchor;
        double latitude;
        double longitude;
        std::optional<double> altitude;
    };
    using CameraType = std::variant<CameraNavState, CameraGoToGeo>;

    Profile() = default;
    explicit Profile(const std::string& content);
    std::string serialize() const;

    std::string convertToScene() const;

    /**
     * Saves all current settings, starting from the profile that was loaded at startup,
     * and all of the property & asset changes that were made since startup.
     */
    void saveCurrentSettingsToProfile(const properties::PropertyOwner& rootOwner,
        std::string currentTime, 
        interaction::NavigationHandler::NavigationState navState);

    /// If the value passed to this function is 'true', the addAsset and removeAsset
    /// functions will be no-ops instead
    void setIgnoreUpdates(bool ignoreUpdates);

    /// Adds a new asset and checks for duplicates
    void addAsset(const std::string& path);

    /// Removes an asset
    void removeAsset(const std::string& path);

    /**
     * Returns the Lua library that contains all Lua functions available to provide
     * profile functionality.
     * \return The Lua library that contains all Lua functions available for profiles
     */
    static scripting::LuaLibrary luaLibrary();

private:
    static constexpr const Version CurrentVersion = Version { 1, 0 };

    Version version = CurrentVersion;
    std::vector<Module> modules;
    std::optional<Meta> meta;
    std::vector<Asset> assets;
    std::vector<Property> properties;
    std::vector<Keybinding> keybindings;
    std::optional<Time> time;
    std::vector<double> deltaTimes;
    std::optional<CameraType> camera;
    std::vector<std::string> markNodes;
    std::vector<std::string> additionalScripts;

    bool _ignoreUpdates = false;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___PROFILE___H__
