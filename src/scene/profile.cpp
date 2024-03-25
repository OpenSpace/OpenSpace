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

#include <openspace/scene/profile.h>

#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/navigationstate.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/properties/property.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/scene/scene.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/misc/assert.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/stringhelper.h>
#include <ctime>
#include <filesystem>
#include <set>
#include <json/json.hpp>

#include "profile_lua.inl"

namespace openspace {

namespace {
    // Helper structs for the visitor pattern of the std::variant
    template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
    template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

    std::vector<properties::Property*> changedProperties(
                                                      const properties::PropertyOwner& po)
    {
        std::vector<properties::Property*> res;
        for (properties::PropertyOwner* subOwner : po.propertySubOwners()) {
            std::vector<properties::Property*> ps = changedProperties(*subOwner);
            res.insert(res.end(), ps.begin(), ps.end());
        }
        for (properties::Property* p : po.properties()) {
            if (p->hasChanged()) {
                res.push_back(p);
            }
        }
        return res;
    }

    void checkValue(const nlohmann::json& j, const std::string& key,
                    bool (nlohmann::json::*checkFunc)() const,
                    std::string_view keyPrefix, bool isOptional)
    {
        if (j.find(key) == j.end()) {
            if (!isOptional) {
                throw Profile::ParsingError(
                    Profile::ParsingError::Severity::Error,
                    std::format("'{}.{}' field is missing", keyPrefix, key)
                );
            }
        }
        else {
            const nlohmann::json& value = j[key];
            if (!(value.*checkFunc)()) {
                std::string type = [](auto c) {
                    if (c == &nlohmann::json::is_string) { return "a string"; }
                    else if (c == &nlohmann::json::is_number) { return "a number"; }
                    else if (c == &nlohmann::json::is_object) { return "an object"; }
                    else if (c == &nlohmann::json::is_array) { return "an array"; }
                    else if (c == &nlohmann::json::is_boolean) { return "a boolean"; }
                    else {
                        throw ghoul::MissingCaseException();
                    }
                }(checkFunc);

                throw Profile::ParsingError(
                    Profile::ParsingError::Severity::Error,
                    std::format("'{}.{}' must be {}", keyPrefix, key, type)
                );
            }
        }
    }

    void checkExtraKeys(const nlohmann::json& j, std::string_view prefix,
                        const std::set<std::string>& allowedKeys)
    {
        for (auto& [key, _] : j.items()) {
            if (allowedKeys.find(key) == allowedKeys.end()) {
                LINFOC(
                    "Profile",
                    std::format("Key '{}' not supported in '{}'", key, prefix)
                );
            }
        }
    }
} // namespace

//
// Current version:
//

void to_json(nlohmann::json& j, const Profile::Version& v) {
    j["major"] = v.major;
    j["minor"] = v.minor;
}

void from_json(const nlohmann::json& j, Profile::Version& v) {
    checkValue(j, "major", &nlohmann::json::is_number, "version", false);
    checkValue(j, "minor", &nlohmann::json::is_number, "version", false);
    checkExtraKeys(j, "version", { "major", "minor" });

    j["major"].get_to(v.major);
    j["minor"].get_to(v.minor);
}

void to_json(nlohmann::json& j, const Profile::Module& v) {
    j["name"] = v.name;
    if (v.loadedInstruction.has_value()) {
        j["loadedInstruction"] = *v.loadedInstruction;
    }
    if (v.notLoadedInstruction.has_value()) {
        j["notLoadedInstruction"] = *v.notLoadedInstruction;
    }
}

void from_json(const nlohmann::json& j, Profile::Module& v) {
    checkValue(j, "name", &nlohmann::json::is_string, "module", false);
    checkValue(j, "loadedInstruction", &nlohmann::json::is_string, "module", true);
    checkValue(j, "notLoadedInstruction", &nlohmann::json::is_string, "module", true);
    checkExtraKeys(j, "module", { "name", "loadedInstruction", "notLoadedInstruction" });

    j["name"].get_to(v.name);
    if (j.find("loadedInstruction") != j.end()) {
        v.loadedInstruction = j["loadedInstruction"].get<std::string>();
    }
    if (j.find("notLoadedInstruction") != j.end()) {
        v.notLoadedInstruction = j["notLoadedInstruction"].get<std::string>();
    }
}

void to_json(nlohmann::json& j, const Profile::Meta& v) {
    if (v.name.has_value()) {
        j["name"] = *v.name;
    }
    if (v.version.has_value()) {
        j["version"] = *v.version;
    }
    if (v.description.has_value()) {
        j["description"] = *v.description;
    }
    if (v.author.has_value()) {
        j["author"] = *v.author;
    }
    if (v.url.has_value()) {
        j["url"] = *v.url;
    }
    if (v.license.has_value()) {
        j["license"] = *v.license;
    }
}

void from_json(const nlohmann::json& j, Profile::Meta& v) {
    checkValue(j, "name", &nlohmann::json::is_string, "meta", true);
    checkValue(j, "version", &nlohmann::json::is_string, "meta", true);
    checkValue(j, "description", &nlohmann::json::is_string, "meta", true);
    checkValue(j, "author", &nlohmann::json::is_string, "meta", true);
    checkValue(j, "url", &nlohmann::json::is_string, "meta", true);
    checkValue(j, "license", &nlohmann::json::is_string, "meta", true);
    checkExtraKeys(
        j,
        "meta",
        { "name", "version", "description", "author", "url", "license" }
    );

    if (j.find("name") != j.end()) {
        v.name = j["name"].get<std::string>();
    }
    if (j.find("version") != j.end()) {
        v.version = j["version"].get<std::string>();
    }
    if (j.find("description") != j.end()) {
        v.description = j["description"].get<std::string>();
    }
    if (j.find("author") != j.end()) {
        v.author = j["author"].get<std::string>();
    }
    if (j.find("url") != j.end()) {
        v.url = j["url"].get<std::string>();
    }
    if (j.find("license") != j.end()) {
        v.license = j["license"].get<std::string>();
    }
}

void to_json(nlohmann::json& j, const Profile::Property::SetType& v) {
    j = [](Profile::Property::SetType t) {
        switch (t) {
            case Profile::Property::SetType::SetPropertyValue:
                return "setPropertyValue";
            case Profile::Property::SetType::SetPropertyValueSingle:
                return "setPropertyValueSingle";
            default:
                throw ghoul::MissingCaseException();
        }
    }(v);
}

void from_json(const nlohmann::json& j, Profile::Property::SetType& v) {
    const std::string value = j.get<std::string>();
    if (value == "setPropertyValue") {
        v = Profile::Property::SetType::SetPropertyValue;
    }
    else if (value == "setPropertyValueSingle") {
        v = Profile::Property::SetType::SetPropertyValueSingle;
    }
    else {
        throw Profile::ParsingError(
            Profile::ParsingError::Severity::Error, "Unknown property set type"
        );
    }
}

void to_json(nlohmann::json& j, const Profile::Property& v) {
    j["type"] = v.setType;
    j["name"] = v.name;
    j["value"] = v.value;
}

void from_json(const nlohmann::json& j, Profile::Property& v) {
    checkValue(j, "type", &nlohmann::json::is_string, "property", false);
    checkValue(j, "name", &nlohmann::json::is_string, "property", false);
    checkValue(j, "value", &nlohmann::json::is_string, "property", false);
    checkExtraKeys(j, "property", { "type", "name", "value" });

    j["type"].get_to(v.setType);
    j["name"].get_to(v.name);
    j["value"].get_to(v.value);
}

void to_json(nlohmann::json& j, const Profile::Action& v) {
    j["identifier"] = v.identifier;
    j["documentation"] = v.documentation;
    j["name"] = v.name;
    j["gui_path"] = v.guiPath;
    j["is_local"] = v.isLocal;
    j["script"] = v.script;
}

void from_json(const nlohmann::json& j, Profile::Action& v) {
    checkValue(j, "identifier", &nlohmann::json::is_string, "action", false);
    checkValue(j, "documentation", &nlohmann::json::is_string, "action", false);
    checkValue(j, "name", &nlohmann::json::is_string, "action", false);
    checkValue(j, "gui_path", &nlohmann::json::is_string, "action", false);
    checkValue(j, "is_local", &nlohmann::json::is_boolean, "action", false);
    checkValue(j, "script", &nlohmann::json::is_string, "action", false);
    checkExtraKeys(
        j,
        "action",
        { "identifier", "documentation", "name", "gui_path", "is_local", "script" }
    );

    j["identifier"].get_to(v.identifier);
    j["documentation"].get_to(v.documentation);
    j["name"].get_to(v.name);
    j["gui_path"].get_to(v.guiPath);
    j["is_local"].get_to(v.isLocal);
    j["script"].get_to(v.script);
}

void to_json(nlohmann::json& j, const Profile::Keybinding& v) {
    j["key"] = keyToString(v.key);
    j["action"] = v.action;
}

void from_json(const nlohmann::json& j, Profile::Keybinding& v) {
    checkValue(j, "key", &nlohmann::json::is_string, "keybinding", false);
    checkValue(j, "action", &nlohmann::json::is_string, "keybinding", false);
    checkExtraKeys(j, "keybinding", { "key", "action" });

    v.key = stringToKey(j.at("key").get<std::string>());
    j["action"].get_to(v.action);
}

void to_json(nlohmann::json& j, const Profile::Time::Type& v) {
    j = [](Profile::Time::Type t) {
        switch (t) {
            case Profile::Time::Type::Absolute: return "absolute";
            case Profile::Time::Type::Relative: return "relative";
            default:                            throw ghoul::MissingCaseException();
        }
    }(v);
}

void from_json(const nlohmann::json& j, Profile::Time::Type& v) {
    const std::string value = j.get<std::string>();
    if (value == "absolute") {
        v = Profile::Time::Type::Absolute;
    }
    else if (value == "relative") {
        v = Profile::Time::Type::Relative;
    }
    else {
        throw Profile::ParsingError(
            Profile::ParsingError::Severity::Error, "Unknown time type"
        );
    }
}

void to_json(nlohmann::json& j, const Profile::Time& v) {
    j["type"] = v.type;
    j["value"] = v.value;
    j["is_paused"] = v.startPaused;
}

void from_json(const nlohmann::json& j, Profile::Time& v) {
    checkValue(j, "type", &nlohmann::json::is_string, "time", false);
    checkValue(j, "value", &nlohmann::json::is_string, "time", false);
    checkValue(j, "is_paused", &nlohmann::json::is_boolean, "time", false);
    checkExtraKeys(j, "time", { "type", "value", "is_paused" });

    j["type"].get_to(v.type);
    j["value"].get_to(v.value);
    j["is_paused"].get_to(v.startPaused);
}

void to_json(nlohmann::json& j, const Profile::CameraGoToNode& v) {
    j["type"] = Profile::CameraGoToNode::Type;
    j["anchor"] = v.anchor;
    if (v.height.has_value()) {
        j["height"] = *v.height;
    }
}

void from_json(const nlohmann::json& j, Profile::CameraGoToNode& v) {
    ghoul_assert(
        j.at("type").get<std::string>() == Profile::CameraGoToNode::Type,
        "Wrong type for Camera"
    );

    checkValue(j, "anchor", &nlohmann::json::is_string, "camera", false);
    checkValue(j, "height", &nlohmann::json::is_number, "camera", true);
    checkExtraKeys(
        j,
        "camera",
        { "type", "anchor", "height"}
    );

    j["anchor"].get_to(v.anchor);
    if (j.find("height") != j.end()) {
        v.height = j["height"].get<double>();
    }
}

void to_json(nlohmann::json& j, const Profile::CameraNavState& v) {
    j["type"] = Profile::CameraNavState::Type;
    j["anchor"] = v.anchor;
    if (v.aim.has_value()) {
        j["aim"] = *v.aim;
    }
    j["frame"] = v.referenceFrame;
    const nlohmann::json p {
        { "x", v.position.x },
        { "y", v.position.y },
        { "z", v.position.z }
    };
    j["position"] = p;
    if (v.up.has_value()) {
        const nlohmann::json u {
            { "x", v.up->x },
            { "y", v.up->y },
            { "z", v.up->z }
        };
        j["up"] = u;
    }
    if (v.yaw.has_value()) {
        j["yaw"] = *v.yaw;
    }
    if (v.pitch.has_value()) {
        j["pitch"] = *v.pitch;
    }
}

void from_json(const nlohmann::json& j, Profile::CameraNavState& v) {
    ghoul_assert(
        j.at("type").get<std::string>() == Profile::CameraNavState::Type,
        "Wrong type for Camera"
    );

    checkValue(j, "anchor", &nlohmann::json::is_string, "camera", false);
    checkValue(j, "aim", &nlohmann::json::is_string, "camera", true);
    checkValue(j, "frame", &nlohmann::json::is_string, "camera", false);
    checkValue(j, "position", &nlohmann::json::is_object, "camera", false);
    checkValue(j["position"], "x", &nlohmann::json::is_number, "camera.position", false);
    checkValue(j["position"], "y", &nlohmann::json::is_number, "camera.position", false);
    checkValue(j["position"], "z", &nlohmann::json::is_number, "camera.position", false);
    checkExtraKeys(j["position"], "camera.position", { "x", "y", "z" });
    checkValue(j, "up", &nlohmann::json::is_object, "camera", true);
    if (j.find("up") != j.end()) {
        checkValue(j["up"], "x", &nlohmann::json::is_number, "camera.up", false);
        checkValue(j["up"], "y", &nlohmann::json::is_number, "camera.up", false);
        checkValue(j["up"], "z", &nlohmann::json::is_number, "camera.up", false);
        checkExtraKeys(j["up"], "camera.up", { "x", "y", "z" });
    }
    checkValue(j, "yaw", &nlohmann::json::is_number, "camera", true);
    checkValue(j, "pitch", &nlohmann::json::is_number, "camera", true);
    checkExtraKeys(
        j,
        "camera",
        { "type", "anchor", "aim", "frame", "position", "up", "yaw", "pitch" }
    );

    j["anchor"].get_to(v.anchor);
    if (j.find("aim") != j.end()) {
        v.aim = j["aim"].get<std::string>();
    }
    j["frame"].get_to(v.referenceFrame);
    nlohmann::json p = j["position"];
    p["x"].get_to(v.position.x);
    p["y"].get_to(v.position.y);
    p["z"].get_to(v.position.z);

    if (j.find("up") != j.end()) {
        nlohmann::json u = j["up"];
        glm::dvec3 up;
        u["x"].get_to(up.x);
        u["y"].get_to(up.y);
        u["z"].get_to(up.z);
        v.up = up;
    }

    if (j.find("yaw") != j.end()) {
        v.yaw = j["yaw"].get<double>();
    }

    if (j.find("pitch") != j.end()) {
        v.pitch = j["pitch"].get<double>();
    }
}

void to_json(nlohmann::json& j, const Profile::CameraGoToGeo& v) {
    j["type"] = Profile::CameraGoToGeo::Type;
    j["anchor"] = v.anchor;
    j["latitude"] = v.latitude;
    j["longitude"] = v.longitude;
    if (v.altitude.has_value()) {
        j["altitude"] = *v.altitude;
    }
}

void from_json(const nlohmann::json& j, Profile::CameraGoToGeo& v) {
    ghoul_assert(
        j.at("type").get<std::string>() == Profile::CameraGoToGeo::Type,
        "Wrong type for Camera"
    );

    checkValue(j, "anchor", &nlohmann::json::is_string, "camera", false);
    checkValue(j, "latitude", &nlohmann::json::is_number, "camera", false);
    checkValue(j, "longitude", &nlohmann::json::is_number, "camera", false);
    checkValue(j, "altitude", &nlohmann::json::is_number, "camera", true);
    checkExtraKeys(
        j,
        "camera",
        { "type", "anchor", "latitude", "longitude", "altitude" }
    );

    j["anchor"].get_to(v.anchor);
    j["latitude"].get_to(v.latitude);
    j["longitude"].get_to(v.longitude);

    if (j.find("altitude") != j.end()) {
        v.altitude = j["altitude"].get<double>();
    }
}

// In these namespaces we defined the structs as they used to be defined in the older
// versions. That way, we can keep the from_json files as they were originally written too
namespace version10 {

struct Keybinding {
    KeyWithModifier key;
    std::string documentation;
    std::string name;
    std::string guiPath;
    bool isLocal = true;
    std::string script;
};

void from_json(const nlohmann::json& j, version10::Keybinding& v) {
    checkValue(j, "key", &nlohmann::json::is_string, "keybinding", false);
    checkValue(j, "documentation", &nlohmann::json::is_string, "keybinding", false);
    checkValue(j, "name", &nlohmann::json::is_string, "keybinding", false);
    checkValue(j, "gui_path", &nlohmann::json::is_string, "keybinding", false);
    checkValue(j, "is_local", &nlohmann::json::is_boolean, "keybinding", false);
    checkValue(j, "script", &nlohmann::json::is_string, "keybinding", false);
    checkExtraKeys(
        j,
        "keybinding",
        { "key", "documentation", "name", "gui_path", "is_local", "script" }
    );

    std::string key = j.at("key").get<std::string>();
    if (key == "KP0") {
        key = "KP_0";
    }
    else if (key == "KP1") {
        key = "KP_1";
    }
    else if (key == "KP2") {
        key = "KP_2";
    }
    else if (key == "KP3") {
        key = "KP_3";
    }
    else if (key == "KP4") {
        key = "KP_4";
    }
    else if (key == "KP5") {
        key = "KP_5";
    }
    else if (key == "KP6") {
        key = "KP_6";
    }
    else if (key == "KP7") {
        key = "KP_7";
    }
    else if (key == "KP8") {
        key = "KP_8";
    }
    else if (key == "KP9") {
        key = "KP_9";
    }

    v.key = stringToKey(key);
    j["documentation"].get_to(v.documentation);
    j["name"].get_to(v.name);
    j["gui_path"].get_to(v.guiPath);
    j["is_local"].get_to(v.isLocal);
    j["script"].get_to(v.script);
}

void convertVersion10to11(nlohmann::json& profile) {
    // Version 1.1 introduced actions and remove Lua function calling from keybindings
    profile["version"] = Profile::Version{ 1, 1 };

    if (profile.find("keybindings") == profile.end()) {
        // We didn't find any keybindings, so there is nothing to do
        return;
    }

    std::vector<Profile::Action> actions;
    std::vector<Profile::Keybinding> keybindings;

    std::vector<version10::Keybinding> kbs =
        profile.at("keybindings").get<std::vector<version10::Keybinding>>();
    for (size_t i = 0; i < kbs.size(); i++) {
        version10::Keybinding& kb = kbs[i];
        const std::string identifier = std::format("profile.keybind.{}", i);

        Profile::Action action;
        action.identifier = identifier;
        action.documentation = std::move(kb.documentation);
        action.name = std::move(kb.name);
        action.guiPath = std::move(kb.guiPath);
        action.isLocal = kb.isLocal;
        action.script = std::move(kb.script);
        actions.push_back(std::move(action));

        Profile::Keybinding keybinding;
        keybinding.key = kb.key;
        keybinding.action = identifier;
        keybindings.push_back(keybinding);
    }

    profile["actions"] = actions;
    profile["keybindings"] = keybindings;
}

} // namespace version10

namespace version11 {

void convertVersion11to12(nlohmann::json& profile) {
    // Version 1.2 introduced a state whether the delta time starts out as paused
    profile["version"] = Profile::Version{ 1, 2 };

    // The default value is that we don't start out as paused
    if (profile.find("time") != profile.end()) {
        profile["time"]["is_paused"] = false;
    }
}

} // namespace version11

namespace version12 {

void convertVersion12to13(nlohmann::json& profile) {
    // Version 1.3 introduced to GoToNode camera initial position
    profile["version"] = Profile::Version{ 1, 3 };
}

} // namespace version12

Profile::ParsingError::ParsingError(Severity severity_, std::string msg)
    : ghoul::RuntimeError(std::move(msg), "profile")
    , severity(severity_)
{}

void Profile::saveCurrentSettingsToProfile(const properties::PropertyOwner& rootOwner,
                                           std::string currentTime,
                                           interaction::NavigationState navState)
{
    version = Profile::CurrentVersion;

    // Update properties
    const std::vector<properties::Property*> ps = changedProperties(rootOwner);

    for (properties::Property* prop : ps) {
        Property p;
        p.setType = Property::SetType::SetPropertyValueSingle;
        p.name = prop->fullyQualifiedIdentifier();
        p.value = prop->stringValue();
        properties.push_back(std::move(p));
    }

    // Add current time to profile file
    Time t;
    t.value = std::move(currentTime);
    t.type = Time::Type::Absolute;
    time = t;

    // Delta times
    std::vector<double> dts = global::timeManager->deltaTimeSteps();
    deltaTimes = std::move(dts);

    // Camera
    CameraNavState c;
    c.anchor = navState.anchor;
    c.aim = navState.aim;
    c.referenceFrame = navState.referenceFrame;
    c.position = navState.position;
    c.up = navState.up;
    c.yaw = navState.yaw;
    c.pitch = navState.pitch;
    camera = std::move(c);
}

void Profile::addAsset(const std::string& path) {
    ZoneScoped;

    if (ignoreUpdates) {
        return;
    }

    const auto it = std::find(assets.cbegin(), assets.cend(), path);
    if (it == assets.end()) {
        assets.push_back(path);
    }
}

void Profile::removeAsset(const std::string& path) {
    ZoneScoped;

    if (ignoreUpdates) {
        return;
    }

    const auto it = std::find(assets.cbegin(), assets.cend(), path);
    if (it != assets.end()) {
        assets.erase(it);
    }
}

std::string Profile::serialize() const {
    nlohmann::json r;
    r["version"] = version;
    if (!modules.empty()) {
        r["modules"] = modules;
    }
    if (meta.has_value()) {
        r["meta"] = *meta;
    }
    if (!assets.empty()) {
        r["assets"] = assets;
    }
    if (!properties.empty()) {
        r["properties"] = properties;
    }
    if (!actions.empty()) {
        r["actions"] = actions;
    }
    if (!keybindings.empty()) {
        r["keybindings"] = keybindings;
    }
    if (time.has_value()) {
        r["time"] = *time;
    }
    if (!deltaTimes.empty()) {
        r["delta_times"] = deltaTimes;
    }
    if (camera.has_value()) {
        r["camera"] = std::visit(
            overloaded {
                [](const CameraGoToNode& c) { return nlohmann::json(c); },
                [](const CameraNavState& c) { return nlohmann::json(c); },
                [](const CameraGoToGeo& c) { return nlohmann::json(c); }
            },
            *camera
        );
    }

    if (!markNodes.empty()) {
        r["mark_nodes"] = markNodes;
    }
    if (!additionalScripts.empty()) {
        r["additional_scripts"] = additionalScripts;
    }

    return r.dump(2);
}

Profile::Profile(const std::filesystem::path& path) {
    ghoul_assert(std::filesystem::is_regular_file(path), "Path must exist");

    std::ifstream inFile;
    try {
        inFile.open(path, std::ifstream::in);
    }
    catch (const std::ifstream::failure& e) {
        throw ghoul::RuntimeError(std::format(
            "Exception opening profile file for read '{}': {}", path, e.what()
        ));
    }

    const std::string content = std::string(
        std::istreambuf_iterator<char>(inFile),
        std::istreambuf_iterator<char>()
    );

    try {
        nlohmann::json profile = nlohmann::json::parse(content);
        profile.at("version").get_to(version);

        // Update the file format in steps
        if (version.major == 1 && version.minor == 0) {
            version10::convertVersion10to11(profile);
            profile["version"].get_to(version);
        }

        if (version.major == 1 && version.minor == 1) {
            version11::convertVersion11to12(profile);
            profile["version"].get_to(version);
        }

        if (version.major == 1 && version.minor == 2) {
            version12::convertVersion12to13(profile);
            profile["version"].get_to(version);
        }


        if (profile.find("modules") != profile.end()) {
            profile["modules"].get_to(modules);
        }
        if (profile.find("meta") != profile.end()) {
            meta = profile["meta"].get<Meta>();
        }
        if (profile.find("assets") != profile.end()) {
            profile["assets"].get_to(assets);
        }
        if (profile.find("properties") != profile.end()) {
            profile["properties"].get_to(properties);
        }
        if (profile.find("actions") != profile.end()) {
            profile["actions"].get_to(actions);
        }
        if (profile.find("keybindings") != profile.end()) {
            profile["keybindings"].get_to(keybindings);
        }
        if (profile.find("time") != profile.end()) {
            Profile::Time t;
            profile["time"].get_to(t);
            time = t;
        }
        if (profile.find("delta_times") != profile.end()) {
            profile["delta_times"].get_to(deltaTimes);
        }
        if (profile.find("camera") != profile.end()) {
            nlohmann::json c = profile.at("camera");
            if (c["type"].get<std::string>() == CameraGoToNode::Type) {
                camera = c.get<CameraGoToNode>();
            }
            else if (c["type"].get<std::string>() == CameraNavState::Type) {
                camera = c.get<CameraNavState>();
            }
            else if (c["type"].get<std::string>() == CameraGoToGeo::Type) {
                camera = c.get<CameraGoToGeo>();
            }
            else {
                throw ParsingError(ParsingError::Severity::Error, "Unknown camera type");
            }
        }
        if (profile.find("mark_nodes") != profile.end()) {
            profile["mark_nodes"].get_to(markNodes);
        }
        if (profile.find("additional_scripts") != profile.end()) {
            profile["additional_scripts"].get_to(additionalScripts);
        }
    }
    catch (const nlohmann::json::exception& e) {
        std::string err = e.what();
        throw ParsingError(ParsingError::Severity::Error, std::move(err));
    }
}

scripting::LuaLibrary Profile::luaLibrary() {
    return {
        "",
        {
            codegen::lua::SaveSettingsToProfile
        }
    };
}

}  // namespace openspace
