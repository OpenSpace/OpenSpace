/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

        explicit ParsingError(Severity severity, std::string msg);

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

        SetType setType;
        std::string name;
        std::string value;

    };
    struct Action {
        std::string identifier;
        std::string documentation;
        std::string name;
        std::string guiPath;
        bool isLocal;
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

        Type type;
        std::string value;
    };
    struct CameraNavState {
        static constexpr const char* Type = "setNavigationState";

        std::string anchor;
        std::optional<std::string> aim;
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

    /**
     * Saves all current settings, starting from the profile that was loaded at startup,
     * and all of the property & asset changes that were made since startup.
     */
    void saveCurrentSettingsToProfile(const properties::PropertyOwner& rootOwner,
        std::string currentTime, interaction::NavigationState navState);

    /// Adds a new asset and checks for duplicates unless the `ignoreUpdates` member is
    /// set to `true`
    void addAsset(const std::string& path);

    /// Removes an asset unless the `ignoreUpdates` member is set to `true`
    void removeAsset(const std::string& path);

    static constexpr const Version CurrentVersion = Version{ 1, 1 };

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
     * \return The Lua library that contains all Lua functions available for profiles
     */
    static scripting::LuaLibrary luaLibrary();

    const std::string file_subName_assets = "";
    const std::string assetFileExtension = ".asset";
};

/**
 * convertToAsset_* functions extract a section (* as section name) and returns its
 * asset-ified version as a string. This allows the profile section to be processed
 * by the asset loader. Having individual sections allows for controlling the order of
 * processing/loading the profile. It also makes it possible for changing the way
 * sections are processed.
 */
/**
 * Function to process the meta information included in the profile
 * 
 * \param profile The profile that should be processed
 * 
 * \return The string representation of the provided profile's meta section, ready
 *         to be loaded as an asset
 */
std::string convertToAsset_meta(const Profile& p);

/**
 * Function to process the assets that are included in the profile
 *
 * \param profile The profile that should be processed
 *
 * \return The string representation of the provided profile's asset section, ready
 *         to be loaded as an asset
 */
std::string convertToAsset_includedAssets(const Profile& p);

/**
 * Function to process the modules that may be included in the profile, and the commands
 * to execute if they are/aren't loaded
 *
 * \param profile The profile that should be processed
 *
 * \return The string representation of the provided profile's modules section, ready
 *         to be loaded as an asset
 */
std::string convertToAsset_modules(const Profile& p);

/**
 * Function to process the actions and keybindings that are included in the profile.
 * The actions and keybindings are separate but closely-related sections of a profile
 *
 * \param profile The profile that should be processed
 *
 * \return The string representation of the provided profile's actions and keybindings
 *         sections, ready to be loaded as an asset
 */
std::string convertToAsset_actionsKeybinds(const Profile& p);

/**
 * Function to process the time setting that is included in the profile
 *
 * \param profile The profile that should be processed
 *
 * \return The string representation of the provided profile's time section, ready
 *         to be loaded as an asset
 */
std::string convertToAsset_time(const Profile& p);

/**
 * Function to process the delta time settings that are included in the profile
 *
 * \param profile The profile that should be processed
 *
 * \return The string representation of the provided profile's delta times section, ready
 *         to be loaded as an asset
 */
std::string convertToAsset_deltaTimes(const Profile& p);

/**
 * Function to process the mark-interesting-nodes that are included in the profile
 *
 * \param profile The profile that should be processed
 *
 * \return The string representation of the provided profile's mark-interesting-nodes
 *         section, ready to be loaded as an asset
 */
std::string convertToAsset_markNodes(const Profile& p);

/**
 * Function to process the properties that are included in the profile
 *
 * \param profile The profile that should be processed
 *
 * \return The string representation of the provided profile's properties section, ready
 *         to be loaded as an asset
 */
std::string convertToAsset_properties(const Profile& p);

/**
 * Function to process the initial camera orientation that is included in the profile
 *
 * \param profile The profile that should be processed
 *
 * \return The string representation of the provided profile's camera section, ready
 *         to be loaded as an asset
 */
std::string convertToAsset_camera(const Profile& p);

/**
 * Function to process the additional scripts command section that is included in the
 * profile
 *
 * \param profile The profile that should be processed
 *
 * \return The string representation of the provided profile's additional scripts
 *         section, ready to be loaded as an asset
 */
std::string convertToAsset_additionalScripts(const Profile& p);

} // namespace openspace

#endif // __OPENSPACE_CORE___PROFILE___H__
