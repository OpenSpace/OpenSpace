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

#include <openspace/scene/profile.h>

#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scenelicensewriter.h>
#include <openspace/scene/sceneinitializer.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/camera.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/misc.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/programobject.h>
#include <string>
#include <stack>
#include <optional>

#include "profile_lua.inl"

namespace openspace {

namespace {
    constexpr const char* _loggerCat = "Profile";
    constexpr const char* KeyIdentifier = "Identifier";
    constexpr const char* KeyParent = "Parent";
    
    constexpr const char* headerVersion = "#Version";
    constexpr const char* headerModule = "#Module";
    constexpr const char* headerAsset = "#Asset";
    constexpr const char* headerProperty = "#Property";
    constexpr const char* headerKeybinding = "#Keybinding";
    constexpr const char* headerTime = "#Time";
    constexpr const char* headerCamera = "#Camera";
    constexpr const char* headerMarkNodes = "#MarkNodes";

    // Helper structs for the visitor pattern of the std::variant
    template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
    template <class... Ts> overloaded(Ts...)->overloaded<Ts...>;

    struct ProfileParsingError : public ghoul::RuntimeError {
        explicit ProfileParsingError(std::string msg)
            : ghoul::RuntimeError(std::move(msg), "profileFile")
        {}

        ProfileParsingError(unsigned int lineNum, std::string msg)
            : ghoul::RuntimeError(
                fmt::format("Error @ line {}: {}", lineNum, std::move(msg)),
                "profileFile"
            )
        {}
    };

    const std::map<Profile::AssetEventType, std::string> AssetEventTypeString{
        { Profile::AssetEventType::Add, "add" },
        { Profile::AssetEventType::Require, "required" },
        { Profile::AssetEventType::Request, "requested" },
        { Profile::AssetEventType::Remove,  "removed" },
        { Profile::AssetEventType::Ignore,  "ignored" }
    };

    void handleChangedAdd(std::vector<Profile::AssetEvent>& base, unsigned int changedIdx,
                          std::vector<Profile::AssetEvent>& changed, std::string asset)
    {
        // @TODO:  Replace the next for loop with std::any_of or std::all_of

        bool addThisAsset = true;
        // Check base profile to see if has already been added there
        for (const Profile::AssetEvent& b : base) {
            if (b.name == asset) {
                if (b.eventType == Profile::AssetEventType::Require
                    || b.eventType == Profile::AssetEventType::Request)
                {
                    addThisAsset = false;
                    break;
                }
            }
        }

        // Check changed asset commands only prior to this one to see if already added
        for (unsigned int i = 0; i < changedIdx; i++) {
            if (changed[i].name == asset) {
                addThisAsset = false;
                break;
            }
        }

        if (addThisAsset) {
            Profile::AssetEvent ae = {
                std::move(asset),
                Profile::AssetEventType::Request
            };
            base.push_back(ae);
        }
    }

    std::string recurseForFullName(properties::PropertyOwner* po) {
        if (po == nullptr) {
            return "";
        }
        std::string name = recurseForFullName(po->owner()) + po->identifier();
        if (!name.empty()) {
            return name + ".";
        }
        else {
            return "";
        }
    }

    void checkForChangedProps(std::vector<properties::Property*>& changedList,
                              properties::PropertyOwner* po)
    {
        if (po) {
            for (properties::PropertyOwner* subOwner : po->propertySubOwners()) {
                checkForChangedProps(changedList, subOwner);
            }
            for (properties::Property* p : po->properties()) {
                if (p->hasChanged()) {
                    changedList.push_back(p);
                }
            }
        }
    }

    enum class Section {
        None,
        Version,
        Module,
        Asset,
        Property,
        Keybinding,
        Time,
        Camera,
        MarkNodes
    };

    //struct ParsingContext {
    //    std::string filename;
    //    int lineNumber;
    //};

    Section parseSection(const std::string& line, int lineNumber) {
        if (line == headerVersion) { return Section::Version; }
        if (line == headerModule) { return Section::Module; }
        if (line == headerAsset) { return Section::Asset; }
        if (line == headerProperty) { return Section::Property; }
        if (line == headerKeybinding) { return Section::Keybinding; }
        if (line == headerTime) { return Section::Time; }
        if (line == headerCamera) { return Section::Camera; }
        if (line == headerMarkNodes) { return Section::MarkNodes; }

        throw ProfileParsingError(
            lineNumber,
            fmt::format("Invalid section header: {}", line)
        );
    }

    [[ nodiscard ]] ProfileData::Version parseVersion(const std::string& line, int lineNumber) {
        std::vector<std::string> parts = ghoul::tokenizeString(line, '.');
        if (parts.empty() || parts.size() > 3) {
            throw ProfileParsingError(
                lineNumber,
                fmt::format("Expected 1-3 version components, got {}", parts.size())
            );
        }

        ProfileData::Version version;
        version.major = std::stoi(parts[0]);
        if (parts.size() > 1) {
            version.minor = std::stoi(parts[1]);
        }
        if (parts.size() > 2) {
            version.patch = std::stoi(parts[2]);
        }
        return version;
    }

    [[ nodiscard ]] ProfileData::Module parseModule(const std::string& line, int lineNumber) {
        std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
        if (fields.size() != 3) {
            throw ProfileParsingError(
                lineNumber,
                fmt::format("Expected 3 fields in a Module entry, got {}", fields.size())
            );
        }
        ProfileData::Module m;
        m.name = fields[0];
        m.loadedInstruction = fields[1];
        m.notLoadedInstruction = fields[2];
        return m;
    }

    [[ nodiscard ]] ProfileData::Asset parseAsset(const std::string& line, int lineNumber) {
        std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
        if (fields.size() != 3) {
            throw ProfileParsingError(
                lineNumber,
                fmt::format("Expected 3 fields in an Asset entry, got {}", fields.size())
            );
        }

        ProfileData::Asset a;
        a.path = fields[0];
        a.type = [&](const std::string& type) -> ProfileData::Asset::Type {
            if (type == "require") {
                return ProfileData::Asset::Type::Require;
            }
            if (type == "request") {
                return ProfileData::Asset::Type::Request;
            }
            throw ProfileParsingError(
                lineNumber,
                fmt::format("Expected asset type 'require' or 'request', got {}", type)
            );
        }(fields[1]);
        a.name = fields[2];
        return a;
    }

    [[ nodiscard ]] ProfileData::Property parseProperty(const std::string& line, int lineNumber) {
        std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
        if (fields.size() != 3) {
            throw ProfileParsingError(
                lineNumber,
                fmt::format("Expected 3 fields in Property entry, got {}", fields.size())
            );
        }
        ProfileData::Property p;
        p.setType = [&](const std::string& type) -> ProfileData::Property::SetType {
            if (type == "setPropertyValue") {
                return ProfileData::Property::SetType::SetPropertyValue;
            }
            if (type == "setPropertyValueSingle") {
                return ProfileData::Property::SetType::SetPropertyValueSingle;
            }
            throw ProfileParsingError(
                lineNumber,
                fmt::format(
                    "Expected property set type 'setPropertyValue' or "
                    "'setPropertyValueSingle', got {}",
                    type
                )
            );
        }(fields[0]);
        p.name = fields[1];
        p.value = fields[2];
        return p;
    }

    [[ nodiscard ]] ProfileData::Keybinding parseKeybinding(const std::string& line, int lineNumber) {
        std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
        if (fields.size() != 6) {
            throw ProfileParsingError(
                lineNumber,
                fmt::format("Expected 6 fields in Keybinding entry, got {}", fields.size())
            );
        }
        ProfileData::Keybinding kb;
        kb.key = fields[0];
        kb.documentation = fields[1];
        kb.name = fields[2];
        kb.guiPath = fields[3];
        kb.isLocal = [&](const std::string& local) -> bool {
            if (local == "false") {
                return false;
            }
            if (local == "true") {
                return true;
            }
            throw ProfileParsingError(
                lineNumber,
                fmt::format("Expected 'false' or 'true' for the local path, got {}", local)
            );
        }(fields[4]);
        kb.script = fields[5];
        return kb;
    }

    [[ nodiscard ]] ProfileData::Time parseTime(const std::string& line, int lineNumber) {
        std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
        if (fields.size() != 2) {
            throw ProfileParsingError(
                lineNumber,
                fmt::format("Expected 2 fields in Time entry, got {}", fields.size())
            );
        }
        ProfileData::Time time;
        time.type = [&](const std::string& type) -> ProfileData::Time::Type {
            if (type == "absolute") {
                return ProfileData::Time::Type::Absolute;
            }
            if (type == "relative") {
                return ProfileData::Time::Type::Relative;
            }
            throw ProfileParsingError(
                lineNumber,
                fmt::format("Expected 'absolute' or 'relative' for the type, got {}", type)
            );
        }(fields[0]);
        time.time = fields[1];
        return time;
    }

    [[ nodiscard ]] ProfileData::CameraType parseCamera(const std::string& line, int lineNumber) {
        std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
        if (fields.empty()) {
            throw ProfileParsingError(lineNumber, "No values specified for Camera location");
        }
        ProfileData::CameraType camera = [&](const std::string& type) ->
            std::variant<ProfileData::CameraNavState, ProfileData::CameraGoToGeo>
        {
            if (type == ProfileData::CameraNavState::Type) {
                if (fields.size() != 8) {
                    throw ProfileParsingError(
                        lineNumber,
                        fmt::format(
                            "Expected 8 fields in the Camera entry, got {}", fields.size()
                        )
                    );
                }

                ProfileData::CameraNavState camera;
                camera.anchor = fields[1];
                camera.aim = fields[2];
                camera.referenceFrame = fields[3];
                camera.position = fields[4];
                camera.up = fields[5];
                camera.yaw = fields[6];
                camera.pitch = fields[7];
                return camera;
            }
            if (type == ProfileData::CameraGoToGeo::Type) {
                if (fields.size() != 5) {
                    throw ProfileParsingError(
                        lineNumber,
                        fmt::format(
                            "Expected 5 fields in the Camera entry, got {}", fields.size()
                        )
                    );
                }

                ProfileData::CameraGoToGeo camera;
                camera.anchor = fields[1];
                camera.latitude = std::stod(fields[2]);
                camera.longitude = std::stod(fields[3]);
                if (!fields[4].empty()) {
                    camera.altitude = std::stod(fields[4]);
                }
                return camera;
            }
            throw ProfileParsingError(
                lineNumber,
                fmt::format(
                    "Expected 'setNavigationState' or 'goToGeo' for the type, got {}",
                    fields[0]
                )
            );
        }(fields[0]);

        return camera;
    }

    [[ nodiscard ]] std::string parseMarkNodes(const std::string& line, int) {
        return line;
    }
} // namespace

void Profile::saveCurrentSettingsToProfile() {
    profile.version = ProfileData::CurrentVersion;

    //
    // Update properties
    //
    //std::vector<SceneGraphNode*> nodes =
    //    global::renderEngine.scene()->allSceneGraphNodes();
    std::vector<properties::Property*> changedProps;

    //for (SceneGraphNode* n : nodes) {
    checkForChangedProps(changedProps, &global::rootPropertyOwner);
    //}
    std::vector<std::string> formattedLines;

    for (properties::Property* prop : changedProps) {
        ProfileData::Property p;
        p.setType = ProfileData::Property::SetType::SetPropertyValueSingle;
        p.name = recurseForFullName(prop->owner()) + prop->identifier();
        p.value = prop->getStringValue();
        profile.properties.push_back(std::move(p));
    }

    //
    // add current time to profile file
    //
    ProfileData::Time time;
    time.time = global::timeManager.time().ISO8601();
    time.type = ProfileData::Time::Type::Absolute;
    profile.time = std::move(time);

    // Camera
    interaction::NavigationHandler::NavigationState nav =
        global::navigationHandler.navigationState();

    ProfileData::CameraNavState camera;
    camera.anchor = nav.anchor;
    camera.aim = nav.aim;
    camera.referenceFrame = nav.referenceFrame;
    camera.position = fmt::format(
        "{},{},{}",
        nav.position.x, nav.position.y, nav.position.z
    );
    if (nav.up.has_value()) {
        camera.up = fmt::format(
            "{},{},{}",
            nav.up->x, nav.up->y, nav.up->z
        );
    }
    camera.yaw = std::to_string(nav.yaw);
    camera.pitch = std::to_string(nav.pitch);
    profile.camera = std::move(camera);
}

void Profile::setIgnoreUpdates(bool ignoreUpdates) {
    _ignoreUpdates = ignoreUpdates;
}

void Profile::addAsset(const std::string& path) {
    if (_ignoreUpdates) {
        return;
    }

    const auto it = std::find_if(
        profile.assets.begin(),
        profile.assets.end(),
        [path](const ProfileData::Asset& a) { return a.path == path; }
    );

    if (it != profile.assets.end()) {
        // Asset already existed, so nothing to do here
        return;
    }

    ProfileData::Asset a;
    a.path = path;
    a.type = ProfileData::Asset::Type::Require;
    profile.assets.push_back(std::move(a));
}

void Profile::removeAsset(const std::string& path) {
    if (_ignoreUpdates) {
        return;
    }

    const auto it = std::find_if(
        profile.assets.begin(),
        profile.assets.end(),
        [path](const ProfileData::Asset& a) { return a.path == path; }
    );

    if (it == profile.assets.end()) {
        throw ghoul::RuntimeError(fmt::format(
            "Tried to remove non-existing asset '{}'", path
        ));
    }

    profile.assets.erase(it);
}

scripting::LuaLibrary Profile::luaLibrary() {
    return {
        "",
        {
            {
                "saveSettingsToProfile",
                &luascriptfunctions::saveSettingsToProfile,
                {},
                "[string, bool]",
                "Collects all changes that have been made since startup, including all "
                "property changes and assets required, requested, or removed. All "
                "changes will be added to the profile that OpenSpace was started with, "
                "and the new saved file will contain all of this information. If the "
                "arugment is provided, the settings will be saved into new profile with "
                "that name. If the argument is blank, the current profile will be saved "
                "to a backup file and the original profile will be overwritten. The "
                "second argument determines if a file that already exists should be "
                "overwritten, which is 'false' by default"
            }
        }
    };
}

std::string Profile::serialize() const {
    std::string output;
    output += fmt::format("{}\n", headerVersion);
    output += fmt::format(
        "{}.{}.{}\n",
        profile.version.major, profile.version.minor, profile.version.patch
    );

    if (!profile.modules.empty()) {
        output += fmt::format("\n{}\n", headerModule);
        for (const ProfileData::Module& m : profile.modules) {
            output += fmt::format(
                "{}\t{}\t{}\n",
                m.name, m.loadedInstruction, m.notLoadedInstruction
            );
        }
    }

    if (!profile.assets.empty()) {
        output += fmt::format("\n{}\n", headerAsset);
        for (const ProfileData::Asset& a : profile.assets) {
            const std::string type = [](ProfileData::Asset::Type t) {
                switch (t) {
                case ProfileData::Asset::Type::Require: return "require";
                case ProfileData::Asset::Type::Request: return "request";
                default: throw ghoul::MissingCaseException();
                }
            }(a.type);
            output += fmt::format("{}\t{}\t{}\n", a.path, type, a.name);
        }
    }

    if (!profile.properties.empty()) {
        output += fmt::format("\n{}\n", headerProperty);
        for (const ProfileData::Property& p : profile.properties) {
            const std::string type = [](ProfileData::Property::SetType t) {
                switch (t) {
                case ProfileData::Property::SetType::SetPropertyValue:
                    return "setPropertyValue";
                case ProfileData::Property::SetType::SetPropertyValueSingle:
                    return "setPropertyValueSingle";
                default:
                    throw ghoul::MissingCaseException();
                }
            }(p.setType);
            output += fmt::format("{}\t{}\t{}\n", type, p.name, p.value);
        }
    }

    if (!profile.keybindings.empty()) {
        output += fmt::format("\n{}\n", headerKeybinding);
        for (const ProfileData::Keybinding& k : profile.keybindings) {
            const std::string local = k.isLocal ? "true" : "false";
            output += fmt::format(
                "{}\t{}\t{}\t{}\t{}\t{}\n",
                k.key, k.documentation, k.name, k.guiPath, local, k.script
            );
        }
    }

    output += fmt::format("\n{}\n", headerTime);
    {
        const std::string type = [](ProfileData::Time::Type t) {
            switch (t) {
            case ProfileData::Time::Type::Absolute: return "absolute";
            case ProfileData::Time::Type::Relative: return "relative";
            default: throw ghoul::MissingCaseException();
            }
        }(profile.time.type);
        output += fmt::format("{}\t{}\n", type, profile.time.time);
    }

    output += fmt::format("\n{}\n", headerCamera);
    output += std::visit(
        overloaded{
            [](const ProfileData::CameraNavState& camera) {
                return fmt::format(
                    "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\n",
                    ProfileData::CameraNavState::Type,
                    camera.anchor, camera.aim, camera.referenceFrame, camera.position,
                    camera.up, camera.yaw, camera.pitch
                );
            },
            [](const ProfileData::CameraGoToGeo& camera) {
                std::string altitude;
                if (camera.altitude.has_value()) {
                    altitude = std::to_string(*camera.altitude);
                }

                return fmt::format(
                    "{}\t{}\t{}\t{}\t{}\n",
                    ProfileData::CameraGoToGeo::Type,
                    camera.anchor, camera.latitude, camera.longitude, altitude
                );
            }
        },
        profile.camera
    );

    if (!profile.markNodes.empty()) {
        output += fmt::format("\n{}\n", headerMarkNodes);
        for (const std::string& n : profile.markNodes) {
            output += fmt::format("{}\n", n);
        }
    }

    return output;
}

Profile::Profile(const std::vector<std::string>& content) {
    Section currentSection = Section::None;
    bool foundVersion = false;
    bool foundCamera = false;

    for (int lineNum = 1; lineNum <= static_cast<int>(content.size()); ++lineNum) {
        std::string line = content[lineNum - 1];
        if (std::all_of(line.begin(), line.end(), ::isspace)) {
            currentSection = Section::None;
            continue;
        }

        switch (currentSection) {
        case Section::None:
            currentSection = parseSection(line, lineNum);
            break;
        case Section::Version:
            profile.version = parseVersion(line, lineNum);
            foundVersion = true;
            break;
        case Section::Module:
        {
            ProfileData::Module m = parseModule(line, lineNum);
            profile.modules.push_back(std::move(m));
            break;
        }
        case Section::Asset:
        {
            ProfileData::Asset a = parseAsset(line, lineNum);
            profile.assets.push_back(std::move(a));
            break;
        }
        case Section::Property:
        {
            ProfileData::Property p = parseProperty(line, lineNum);
            profile.properties.push_back(std::move(p));
            break;
        }
        case Section::Keybinding:
        {
            ProfileData::Keybinding kb = parseKeybinding(line, lineNum);
            profile.keybindings.push_back(std::move(kb));
            break;
        }
        case Section::Time:
            profile.time = parseTime(line, lineNum);
            break;
        case Section::Camera:
            profile.camera = parseCamera(line, lineNum);
            foundCamera = true;
            break;
        case Section::MarkNodes:
        {
            std::string m = parseMarkNodes(line, lineNum);
            profile.markNodes.push_back(std::move(m));
            break;
        }
        default:
            throw ghoul::MissingCaseException();
        }
    }

    if (!foundVersion) {
        throw ghoul::RuntimeError(
            "Did not find Version information when loading profile"
        );
    }

    if (!foundCamera) {
        throw ghoul::RuntimeError(
            "Did not find Camera information when loading profile"
        );
    }
}

std::string Profile::convertToScene() const {
    ZoneScoped

    std::string output;

    // Modules
    for (const ProfileData::Module& m : profile.modules) {
        output += fmt::format(
            "if openspace.modules.isLoaded(\"{}\") then {} else {} end\n",
            m.name, m.loadedInstruction, m.notLoadedInstruction
        );
    }

    // Assets
    for (const ProfileData::Asset& a : profile.assets) {
        if (!a.name.empty()) {
            output += fmt::format("local {} = ", a.name);
        }
        std::string type = [](ProfileData::Asset::Type t) {
            switch (t) {
                case ProfileData::Asset::Type::Request: return "request";
                case ProfileData::Asset::Type::Require: return "require";
                default: throw ghoul::MissingCaseException();
            }
        }(a.type);

        output += fmt::format("asset.{}(\"{}\");\n", type, a.path);
    }

    output += "asset.onInitialize(function()\n";
    // Keybindings
    for (const ProfileData::Keybinding& k : profile.keybindings) {
        const std::string name = k.name.empty() ? k.key : k.name;
        output += fmt::format(
            k.isLocal ?
            "openspace.bindKeyLocal(\"{}\", {}, [[{}]], [[{}]], [[{}]]);\n" :
            "openspace.bindKey(\"{}\", {}, [[{}]], [[{}]], [[{}]]);\n",
            k.key, k.script, k.documentation, k.name.empty() ? k.key : k.name, k.guiPath
        );
    }

    // Time
    switch (profile.time.type) {
        case ProfileData::Time::Type::Absolute:
            output += fmt::format("openspace.time.setTime(\"{}\")\n", profile.time.time);
            break;
        case ProfileData::Time::Type::Relative:
            output += "local now = openspace.time.currentWallTime();\n";
            output += fmt::format(
                "local prev = openspace.time.advancedTime(now, \"{}\");\n",
                profile.time.time
            );
            output += "openspace.time.setTime(prev);\n";
            break;
        case ProfileData::Time::Type::None:
            output += "openspace.time.setTime(openspace.time.currentWallTime());\n";
            break;
        default:
            throw ghoul::MissingCaseException();
    }

    // Mark Nodes
    {
        std::string nodes;
        for (const std::string& n : profile.markNodes) {
            nodes += fmt::format("[[ {} ]],", n);
        }
        output += fmt::format("openspace.markInterestingNodes({{ {} }});\n", nodes);
    }

    // Properties
    for (const ProfileData::Property& p : profile.properties) {
        switch (p.setType) {
            case ProfileData::Property::SetType::SetPropertyValue:
                output += fmt::format(
                    "openspace.setPropertyValue(\"{}\", {});\n",
                    p.name, p.value
                );
                break;
            case ProfileData::Property::SetType::SetPropertyValueSingle:
                output += fmt::format(
                    "openspace.setPropertyValueSingle(\"{}\", {});\n",
                    p.name, p.value
                );
                break;
            default:
                throw ghoul::MissingCaseException();
        }
    }

    // Camera
    output += std::visit(
        overloaded{
            [](const ProfileData::CameraNavState& camera) {
                std::string result;
                result += "openspace.navigation.setNavigationState({";
                result += fmt::format("Anchor = {}, ", camera.anchor);
                if (!camera.aim.empty()) {
                    result += fmt::format("Aim = {}, ", camera.aim);
                }
                if (!camera.referenceFrame.empty()) {
                    result += fmt::format("ReferenceFrame = {}, ", camera.referenceFrame);
                }
                result += fmt::format("Position = {{ {} }}, ", camera.position);
                if (!camera.up.empty()) {
                    result += fmt::format("Up = {{ {} }}, ", camera.up);
                }
                if (!camera.yaw.empty()) {
                    result += fmt::format("Yaw = {}, ", camera.yaw);
                }
                if (!camera.pitch.empty()) {
                    result += fmt::format("Pitch = {} ", camera.pitch);
                }
                result += "})\n";
                return result;
            },
            [](const ProfileData::CameraGoToGeo& camera) {
                if (camera.altitude.has_value()) {
                    return fmt::format(
                        "openspace.globebrowsing.goToGeo({}, {}, {}, {});\n",
                        camera.anchor, camera.latitude, camera.longitude, *camera.altitude
                    );
                }
                else {
                    return fmt::format(
                        "openspace.globebrowsing.goToGeo({}, {}, {});\n",
                        camera.anchor, camera.latitude, camera.longitude
                    );
                }
            }
        },
        profile.camera
    );
    output += "end)\n";

    return output;
}


}  // namespace openspace
