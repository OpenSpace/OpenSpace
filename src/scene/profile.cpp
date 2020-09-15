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

#include <openspace/scripting/lualibrary.h>
#include <openspace/properties/property.h>
#include <openspace/properties/propertyowner.h>
#include <ghoul/misc/assert.h>
#include <ghoul/fmt.h>
#include <ghoul/misc/misc.h>
#include <ghoul/misc/profiling.h>
#include <json/json.hpp>

#include "profile_lua.inl"

namespace openspace {

namespace {
    struct ProfileParsingError : public ghoul::RuntimeError {
        explicit ProfileParsingError(std::string msg)
            : ghoul::RuntimeError(std::move(msg), "profileFile")
        {}
    };
}

void to_json(nlohmann::json& j, const Profile::Version& v) {
    j["major"] = v.major;
    j["minor"] = v.minor;
}

void from_json(const nlohmann::json& j, Profile::Version& v) {
    j.at("major").get_to(v.major);
    j.at("minor").get_to(v.minor);
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
    j.at("name").get_to(v.name);
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

void to_json(nlohmann::json& j, const Profile::Asset& v) {
    j["path"] = v.path;
    if (v.name.has_value()) {
        j["name"] = *v.name;
    }
}

void from_json(const nlohmann::json& j, Profile::Asset& v) {
    j.at("path").get_to(v.path);
    if (j.find("name") != j.end()) {
        v.name = j["name"].get<std::string>();
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
    std::string value = j.get<std::string>();
    if (value == "setPropertyValue") {
        v = Profile::Property::SetType::SetPropertyValue;
    }
    else if (value == "setPropertyValueSingle") {
        v = Profile::Property::SetType::SetPropertyValueSingle;
    }
    else {
        throw ProfileParsingError("Unknown property set type");
    }
}

void to_json(nlohmann::json& j, const Profile::Property& v) {
    j["type"] = v.setType;
    j["name"] = v.name;
    j["value"] = v.value;
}

void from_json(const nlohmann::json& j, Profile::Property& v) {
    j.at("type").get_to(v.setType);
    j.at("name").get_to(v.name);
    j.at("value").get_to(v.value);
}

void to_json(nlohmann::json& j, const Profile::Keybinding& v) {
    j["key"] = ghoul::to_string(v.key);
    j["documentation"] = v.documentation;
    j["name"] = v.name;
    j["gui_path"] = v.guiPath;
    j["is_local"] = v.isLocal;
    j["script"] = v.script;
}

void from_json(const nlohmann::json& j, Profile::Keybinding& v) {
    v.key = stringToKey(j.at("key").get<std::string>());
    j.at("documentation").get_to(v.documentation);
    j.at("name").get_to(v.name);
    j.at("gui_path").get_to(v.guiPath);
    j.at("is_local").get_to(v.isLocal);
    j.at("script").get_to(v.script);
}

void to_json(nlohmann::json& j, const Profile::Time::Type& v) {
    j = [](Profile::Time::Type t) {
        switch (t) {
            case Profile::Time::Type::Absolute:
                return "absolute";
            case Profile::Time::Type::Relative:
                return "relative";
            default:
                throw ghoul::MissingCaseException();
        }
    }(v);
}

void from_json(const nlohmann::json& j, Profile::Time::Type& v) {
    std::string value = j.get<std::string>();
    if (value == "absolute") {
        v = Profile::Time::Type::Absolute;
    }
    else if (value == "relative") {
        v = Profile::Time::Type::Relative;
    }
    else {
        throw ProfileParsingError("Unknown time type");
    }
}

void to_json(nlohmann::json& j, const Profile::Time& v) {
    j["type"] = v.type;
    j["value"] = v.value;
}

void from_json(const nlohmann::json& j, Profile::Time& v) {
    j.at("type").get_to(v.type);
    j.at("value").get_to(v.value);
}

void to_json(nlohmann::json& j, const Profile::CameraNavState& v) {
    j["type"] = Profile::CameraNavState::Type;
    j["anchor"] = v.anchor;
    if (v.aim.has_value()) {
        j["aim"] = *v.aim;
    }
    j["frame"] = v.referenceFrame;
    nlohmann::json p{
        { "x", v.position.x },
        { "y", v.position.y },
        { "z", v.position.z }
    };
    j["position"] = p;
    if (v.up.has_value()) {
        nlohmann::json u{
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

    j.at("anchor").get_to(v.anchor);
    if (j.find("aim") != j.end()) {
        v.aim = j["aim"].get<std::string>();
    }
    j.at("frame").get_to(v.referenceFrame);
    nlohmann::json p = j.at("position");
    p.at("x").get_to(v.position.x);
    p.at("y").get_to(v.position.y);
    p.at("z").get_to(v.position.z);

    if (j.find("up") != j.end()) {
        nlohmann::json u = j.at("up");
        glm::dvec3 up;
        u.at("x").get_to(up.x);
        u.at("y").get_to(up.y);
        u.at("z").get_to(up.z);
        v.up = up;
    }

    if (j.find("yaw") != j.end()) {
        v.yaw = j.at("yaw").get<double>();
    }

    if (j.find("pitch") != j.end()) {
        v.pitch = j.at("pitch").get<double>();
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

    j.at("anchor").get_to(v.anchor);
    j.at("latitude").get_to(v.latitude);
    j.at("longitude").get_to(v.longitude);

    if (j.find("altitude") != j.end()) {
        v.altitude = j.at("altitude").get<double>();
    }
}


namespace {
    constexpr const char* _loggerCat = "Profile";
    
    //constexpr const char* headerVersion = "#Version";
    //constexpr const char* headerMeta = "#Meta";
    //constexpr const char* headerModule = "#Module";
    //constexpr const char* headerAsset = "#Asset";
    //constexpr const char* headerProperty = "#Property";
    //constexpr const char* headerKeybinding = "#Keybinding";
    //constexpr const char* headerTime = "#Time";
    //constexpr const char* headerDeltaTimes = "#DeltaTimes";
    //constexpr const char* headerCamera = "#Camera";
    //constexpr const char* headerMarkNodes = "#MarkNodes";
    //constexpr const char* headerAdditionalScripts = "#AdditionalScripts";

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

    //enum class Section {
    //    None,
    //    Version,
    //    Meta,
    //    Module,
    //    Asset,
    //    Property,
    //    Keybinding,
    //    Time,
    //    DeltaTimes,
    //    Camera,
    //    MarkNodes,
    //    AdditionalScripts
    //};

    //Section parseSection(const std::string& line, int lineNumber) {
    //    if (line == headerVersion) { return Section::Version; }
    //    if (line == headerMeta) { return Section::Meta; }
    //    if (line == headerModule) { return Section::Module; }
    //    if (line == headerAsset) { return Section::Asset; }
    //    if (line == headerProperty) { return Section::Property; }
    //    if (line == headerKeybinding) { return Section::Keybinding; }
    //    if (line == headerTime) { return Section::Time; }
    //    if (line == headerDeltaTimes) { return Section::DeltaTimes; }
    //    if (line == headerCamera) { return Section::Camera; }
    //    if (line == headerMarkNodes) { return Section::MarkNodes; }
    //    if (line == headerAdditionalScripts) { return Section::AdditionalScripts; }

    //    throw ProfileParsingError(
    //        lineNumber,
    //        fmt::format("Invalid section header: {}", line)
    //    );
    //}

    //[[ nodiscard ]] Profile::Version parseVersion(const std::string& line, int lineNumber)
    //{
    //    std::vector<std::string> parts = ghoul::tokenizeString(line, '.');
    //    if (parts.size() > 2) {
    //        throw ProfileParsingError(
    //            lineNumber,
    //            fmt::format("Expected 1-2 version components, got {}", parts.size())
    //        );
    //    }

    //    try {
    //        Profile::Version version;
    //        if (parts.empty()) {
    //            version.major = std::stoi(line);
    //        }
    //        else {
    //            version.major = std::stoi(parts[0]);
    //        }
    //        if (parts.size() > 1) {
    //            version.minor = std::stoi(parts[1]);
    //        }
    //        return version;
    //    }
    //    catch (const std::invalid_argument&) {
    //        throw ProfileParsingError(
    //            lineNumber,
    //            "Error parsing Version. Version number is not a number"
    //        );
    //    }
    //}

    //[[ nodiscard ]] Profile::Module parseModule(const std::string& line, int lineNumber) {
    //    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
    //    if (fields.size() != 3) {
    //        throw ProfileParsingError(
    //            lineNumber,
    //            fmt::format("Expected 3 fields in a Module entry, got {}", fields.size())
    //        );
    //    }
    //    Profile::Module m;
    //    m.name = fields[0];
    //    m.loadedInstruction = fields[1];
    //    m.notLoadedInstruction = fields[2];
    //    return m;
    //}

    //enum class MetaLineType {
    //    Name,
    //    Version,
    //    Description,
    //    Author,
    //    URL,
    //    License
    //};

    //[[ nodiscard ]] std::pair<MetaLineType, std::string> parseMeta(
    //                                                               const std::string& line,
    //                                                                       int lineNumber)
    //{
    //    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
    //    if (fields.size() < 2) {
    //        throw ProfileParsingError(
    //            lineNumber,
    //            fmt::format("Expected 2 fields in a Meta line, got {}", fields.size())
    //        );
    //    }

    //    const std::string type = fields[0];

    //    // Users are allowed to use \t in their lines, meaning that the fields could
    //    // contain more than the 2 elements that we expected
    //    fields.erase(fields.begin());
    //    const std::string content = ghoul::join(fields, "\t");

    //    if (type == "Name") {
    //        return { MetaLineType::Name, content };
    //    }
    //    else if (type == "Version") {
    //        return { MetaLineType::Version, content };
    //    }
    //    else if (type == "Description") {
    //        return { MetaLineType::Description, content };
    //    }
    //    else if (type == "Author") {
    //        return { MetaLineType::Author, content };
    //    }
    //    else if (type == "URL") {
    //        return { MetaLineType::URL, content };
    //    }
    //    else if (type == "License") {
    //        return { MetaLineType::License, content };
    //    }
    //    else {
    //        throw ProfileParsingError(
    //            lineNumber,
    //            fmt::format("Unknown meta line type '{}'", type)
    //        );
    //    }
    //}

    //[[ nodiscard ]] Profile::Asset parseAsset(const std::string& line, int lineNumber) {
    //    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
    //    if (fields.size() != 2) {
    //        throw ProfileParsingError(
    //            lineNumber,
    //            fmt::format("Expected 2 fields in an Asset entry, got {}", fields.size())
    //        );
    //    }

    //    Profile::Asset a;
    //    a.path = fields[0];
    //    a.name = fields[1];
    //    return a;
    //}

    //[[ nodiscard ]] Profile::Property parseProperty(const std::string& line, int lineNumber) {
    //    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
    //    if (fields.size() != 3) {
    //        throw ProfileParsingError(
    //            lineNumber,
    //            fmt::format("Expected 3 fields in Property entry, got {}", fields.size())
    //        );
    //    }
    //    Profile::Property p;
    //    p.setType = [&](const std::string& type) -> Profile::Property::SetType {
    //        if (type == "setPropertyValue") {
    //            return Profile::Property::SetType::SetPropertyValue;
    //        }
    //        if (type == "setPropertyValueSingle") {
    //            return Profile::Property::SetType::SetPropertyValueSingle;
    //        }
    //        throw ProfileParsingError(
    //            lineNumber,
    //            fmt::format(
    //                "Expected property set type 'setPropertyValue' or "
    //                "'setPropertyValueSingle', got '{}'",
    //                type
    //            )
    //        );
    //    }(fields[0]);
    //    p.name = fields[1];
    //    p.value = fields[2];
    //    return p;
    //}

    //[[ nodiscard ]] Profile::Keybinding parseKeybinding(const std::string& line, int lineNumber) {
    //    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
    //    if (fields.size() != 6) {
    //        throw ProfileParsingError(
    //            lineNumber,
    //            fmt::format("Expected 6 fields in Keybinding entry, got {}", fields.size())
    //        );
    //    }
    //    Profile::Keybinding kb;
    //    try {
    //        kb.key = stringToKey(fields[0]);
    //    }
    //    catch (const ghoul::RuntimeError& e) {
    //        throw ProfileParsingError(lineNumber, e.what());
    //    }
    //    kb.documentation = fields[1];
    //    kb.name = fields[2];
    //    kb.guiPath = fields[3];
    //    kb.isLocal = [&](const std::string& local) -> bool {
    //        if (local == "false") {
    //            return false;
    //        }
    //        if (local == "true") {
    //            return true;
    //        }
    //        throw ProfileParsingError(
    //            lineNumber,
    //            fmt::format("Expected 'false' or 'true' for the local path, got {}", local)
    //        );
    //    }(fields[4]);
    //    kb.script = fields[5];
    //    return kb;
    //}

    //[[ nodiscard ]] Profile::Time parseTime(const std::string& line, int lineNumber) {
    //    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
    //    if (fields.size() != 2) {
    //        throw ProfileParsingError(
    //            lineNumber,
    //            fmt::format("Expected 2 fields in Time entry, got {}", fields.size())
    //        );
    //    }
    //    Profile::Time time;
    //    time.type = [&](const std::string& type) -> Profile::Time::Type {
    //        if (type == "absolute") {
    //            return Profile::Time::Type::Absolute;
    //        }
    //        if (type == "relative") {
    //            return Profile::Time::Type::Relative;
    //        }
    //        throw ProfileParsingError(
    //            lineNumber,
    //            fmt::format("Expected 'absolute' or 'relative' for the type, got {}", type)
    //        );
    //    }(fields[0]);
    //    time.time = fields[1];
    //    return time;
    //}

    //[[ nodiscard ]] double parseDeltaTime(const std::string& line, int lineNumber) {
    //    try {
    //        return std::stod(line);
    //    }
    //    catch (const std::invalid_argument&) {
    //        throw ProfileParsingError(
    //            lineNumber,
    //            fmt::format("Expected a number for delta time entry, got '{}'", line)
    //        );
    //    }
    //}

    //[[ nodiscard ]] Profile::CameraType parseCamera(const std::string& line, int lineNumber) {
    //    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
    //    Profile::CameraType camera = [&](const std::string& type) ->
    //        std::variant<Profile::CameraNavState, Profile::CameraGoToGeo>
    //    {
    //        if (type == Profile::CameraNavState::Type) {
    //            if (fields.size() != 8) {
    //                throw ProfileParsingError(
    //                    lineNumber,
    //                    fmt::format(
    //                        "Expected 8 fields in the Camera entry, got {}", fields.size()
    //                    )
    //                );
    //            }

    //            Profile::CameraNavState camera;
    //            camera.anchor = fields[1];
    //            camera.aim = fields[2];
    //            camera.referenceFrame = fields[3];

    //            std::vector<std::string> position = ghoul::tokenizeString(fields[4], ' ');
    //            if (position.size() != 3) {
    //                throw ProfileParsingError(
    //                    lineNumber,
    //                    fmt::format(
    //                        "Expected 3 fields for the camera's position, got {}",
    //                        position.size()
    //                    )
    //                );
    //            }
    //            try {
    //                camera.position = glm::dvec3(
    //                    std::stod(position[0]),
    //                    std::stod(position[1]),
    //                    std::stod(position[2])
    //                );
    //            }
    //            catch (const std::invalid_argument&) {
    //                throw ProfileParsingError(
    //                    lineNumber,
    //                    "Camera's position components must be numbers"
    //                );
    //            }

    //            std::vector<std::string> up = ghoul::tokenizeString(fields[5], ' ');
    //            if (up.size() != 0 && up.size() != 3) {
    //                throw ProfileParsingError(
    //                    lineNumber,
    //                    fmt::format(
    //                        "Expected 0 or 3 fields for the camera's up vector, got {}",
    //                        up.size()
    //                    )
    //                );
    //            }
    //            if (up.size() == 3) {
    //                try {
    //                    camera.up = glm::dvec3(
    //                        std::stod(up[0]),
    //                        std::stod(up[1]),
    //                        std::stod(up[2])
    //                    );
    //                }
    //                catch (const std::invalid_argument&) {
    //                    throw ProfileParsingError(
    //                        lineNumber,
    //                        "Camera's up vector components must be numbers"
    //                    );
    //                }
    //            }

    //            if (!fields[6].empty()) {
    //                try {
    //                    camera.yaw = std::stod(fields[6]);
    //                }
    //                catch (const std::invalid_argument&) {
    //                    throw ProfileParsingError(
    //                        lineNumber,
    //                        "Camera's yaw value must be a number"
    //                    );
    //                }
    //            }

    //            if (!fields[7].empty()) {
    //                try {
    //                    camera.pitch = std::stod(fields[7]);
    //                }
    //                catch (const std::invalid_argument&) {
    //                    throw ProfileParsingError(
    //                        lineNumber,
    //                        "Camera's pitch value must be a number"
    //                    );
    //                }
    //            }
    //            return camera;
    //        }
    //        if (type == Profile::CameraGoToGeo::Type) {
    //            if (fields.size() != 5) {
    //                throw ProfileParsingError(
    //                    lineNumber,
    //                    fmt::format(
    //                        "Expected 5 fields in the Camera entry, got {}", fields.size()
    //                    )
    //                );
    //            }

    //            Profile::CameraGoToGeo camera;
    //            camera.anchor = fields[1];
    //            camera.latitude = std::stod(fields[2]);
    //            camera.longitude = std::stod(fields[3]);
    //            if (!fields[4].empty()) {
    //                camera.altitude = std::stod(fields[4]);
    //            }
    //            return camera;
    //        }
    //        throw ProfileParsingError(
    //            lineNumber,
    //            fmt::format(
    //                "Expected 'setNavigationState' or 'goToGeo' for the type, got {}",
    //                fields[0]
    //            )
    //        );
    //    }(fields[0]);

    //    return camera;
    //}

    //[[ nodiscard ]] std::string parseMarkNodes(const std::string& line, int) {
    //    return line;
    //}

    //[[ nodiscard ]] std::string parseAdditionalScript(const std::string& line, int) {
    //    return line;
    //}
} // namespace

void Profile::saveCurrentSettingsToProfile(const properties::PropertyOwner& rootOwner,
                                           std::string currentTime,
                                 interaction::NavigationHandler::NavigationState navState)
{
    version = Profile::CurrentVersion;

    //
    // Update properties
    //
    std::vector<properties::Property*> ps = changedProperties(rootOwner);

    for (properties::Property* prop : ps) {
        Property p;
        p.setType = Property::SetType::SetPropertyValueSingle;
        p.name = prop->fullyQualifiedIdentifier();
        p.value = prop->getStringValue();
        properties.push_back(std::move(p));
    }

    //
    // add current time to profile file
    //
    Time t;
    t.value = std::move(currentTime);
    t.type = Time::Type::Absolute;
    time = t;

    // Delta times
    std::vector<double> dts = global::timeManager.deltaTimeSteps();
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

void Profile::setIgnoreUpdates(bool ignoreUpdates) {
    _ignoreUpdates = ignoreUpdates;
}

void Profile::addAsset(const std::string& path) {
    ZoneScoped

    if (_ignoreUpdates) {
        return;
    }

    const auto it = std::find_if(
        assets.begin(),
        assets.end(),
        [path](const Asset& a) { return a.path == path; }
    );

    if (it != assets.end()) {
        // Asset already existed, so nothing to do here
        return;
    }

    Asset a;
    a.path = path;
    assets.push_back(std::move(a));
}

void Profile::removeAsset(const std::string& path) {
    ZoneScoped

    if (_ignoreUpdates) {
        return;
    }

    const auto it = std::find_if(
        assets.cbegin(),
        assets.cend(),
        [path](const Asset& a) { return a.path == path; }
    );

    if (it == assets.end()) {
        throw ghoul::RuntimeError(fmt::format(
            "Tried to remove non-existing asset '{}'", path
        ));
    }

    assets.erase(it);
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
                [](const CameraNavState& camera) {
                    return nlohmann::json(camera);
                },
                [](const Profile::CameraGoToGeo& camera) {
                    return nlohmann::json(camera);
                }
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

Profile::Profile(const std::string& content) {
    try {
        nlohmann::json profile = nlohmann::json::parse(content);

        profile.at("version").get_to(version);
        if (profile.find("modules") != profile.end()) {
            profile.at("modules").get_to(modules);
        }
        if (profile.find("meta") != profile.end()) {
            meta = profile.at("meta").get<Meta>();
        }
        if (profile.find("assets") != profile.end()) {
            profile.at("assets").get_to(assets);
        }
        if (profile.find("properties") != profile.end()) {
            profile.at("properties").get_to(properties);
        }
        if (profile.find("keybindings") != profile.end()) {
            profile.at("keybindings").get_to(keybindings);
        }
        if (profile.find("time") != profile.end()) {
            time = profile.at("time").get<Time>();
        }
        if (profile.find("delta_times") != profile.end()) {
            profile.at("delta_times").get_to(deltaTimes);
        }
        if (profile.find("camera") != profile.end()) {
            nlohmann::json c = profile.at("camera");
            if (c.at("type") == CameraNavState::Type) {
                camera = c.get<CameraNavState>();
            }
            else if (c.at("type") == CameraGoToGeo::Type) {
                camera = c.get<CameraGoToGeo>();
            }
            else {
                throw ProfileParsingError("Unknown camera type");
            }
        }
        if (profile.find("mark_nodes") != profile.end()) {
            profile.at("mark_nodes").get_to(markNodes);
        }
        if (profile.find("additional_scripts") != profile.end()) {
            profile.at("additional_scripts").get_to(additionalScripts);
        }
    }
    catch (const nlohmann::json::exception& e) {
        std::string err = e.what();
        throw ProfileParsingError(err);
    }

    //Section currentSection = Section::None;
    //bool foundVersion = false;
    //bool foundMeta = false;
    //bool foundTime = false;
    //bool foundCamera = false;

    //for (int lineNum = 1; lineNum <= static_cast<int>(content.size()); ++lineNum) {
    //    std::string line = content[lineNum - 1];
    //    if (std::all_of(line.begin(), line.end(), ::isspace)) {
    //        currentSection = Section::None;
    //        continue;
    //    }

    //    if (currentSection != Section::None && line[0] == '#') {
    //        throw ProfileParsingError(
    //            lineNum,
    //            "Sections in profile must be separated by empty lines"
    //        );
    //    }

    //    switch (currentSection) {
    //        case Section::None:
    //            currentSection = parseSection(line, lineNum);

    //            if (!foundVersion && currentSection != Section::Version) {
    //                throw ProfileParsingError(
    //                    lineNum,
    //                    fmt::format(
    //                        "First header in the file must be Version, but got {}", line
    //                    )
    //                );
    //            }

    //            if (currentSection == Section::Meta && foundMeta) {
    //                throw ProfileParsingError(
    //                    lineNum,
    //                    "Meta section can only appear once per profile"
    //                );
    //            }
    //            break;
    //        case Section::Version:
    //            if (foundVersion) {
    //                throw ProfileParsingError(
    //                    lineNum,
    //                    "Version section can only appear once per profile"
    //                );
    //            }

    //            version = parseVersion(line, lineNum);
    //            foundVersion = true;
    //            break;
    //        case Section::Meta:
    //        {
    //            if (!meta.has_value()) {
    //                meta = Meta();
    //            }

    //            std::pair<MetaLineType, std::string> m = parseMeta(line, lineNum);
    //            switch (m.first) {
    //                case MetaLineType::Name:
    //                    if (!meta->name.empty()) {
    //                        throw ProfileParsingError(
    //                            lineNum,
    //                            "Meta information 'Name' specified twice"
    //                        );
    //                    }
    //                    meta->name = m.second;
    //                    break;
    //                case MetaLineType::Version:
    //                    if (!meta->version.empty()) {
    //                        throw ProfileParsingError(
    //                            lineNum,
    //                            "Meta information 'Version' specified twice"
    //                        );
    //                    }
    //                    meta->version = m.second;
    //                    break;
    //                case MetaLineType::Description:
    //                    if (!meta->description.empty()) {
    //                        throw ProfileParsingError(
    //                            lineNum,
    //                            "Meta information 'Description' specified twice"
    //                        );
    //                    }
    //                    meta->description = m.second;
    //                    break;
    //                case MetaLineType::Author:
    //                    if (!meta->author.empty()) {
    //                        throw ProfileParsingError(
    //                            lineNum,
    //                            "Meta information 'Author' specified twice"
    //                        );
    //                    }
    //                    meta->author = m.second;
    //                    break;
    //                case MetaLineType::URL:
    //                    if (!meta->url.empty()) {
    //                        throw ProfileParsingError(
    //                            lineNum,
    //                            "Meta information 'URL' specified twice"
    //                        );
    //                    }
    //                    meta->url = m.second;
    //                    break;
    //                case MetaLineType::License:
    //                    if (!meta->license.empty()) {
    //                        throw ProfileParsingError(
    //                            lineNum,
    //                            "Meta information 'License' specified twice"
    //                        );
    //                    }
    //                    meta->license = m.second;
    //                    break;
    //                default:
    //                    throw ghoul::MissingCaseException();
    //            }
    //            foundMeta = true;
    //            break;
    //        }
    //        case Section::Module:
    //        {
    //            Module m = parseModule(line, lineNum);
    //            modules.push_back(std::move(m));
    //            break;
    //        }
    //        case Section::Asset:
    //        {
    //            Asset a = parseAsset(line, lineNum);
    //            assets.push_back(std::move(a));
    //            break;
    //        }
    //        case Section::Property:
    //        {
    //            Property p = parseProperty(line, lineNum);
    //            properties.push_back(std::move(p));
    //            break;
    //        }
    //        case Section::Keybinding:
    //        {
    //            Keybinding kb = parseKeybinding(line, lineNum);
    //            keybindings.push_back(std::move(kb));
    //            break;
    //        }
    //        case Section::Time:
    //            if (foundTime) {
    //                throw ProfileParsingError(
    //                    lineNum,
    //                    "Time section can only appear once per profile"
    //                );
    //            }

    //            time = parseTime(line, lineNum);
    //            foundTime = true;
    //            break;
    //        case Section::DeltaTimes:
    //        {
    //            const double d = parseDeltaTime(line, lineNum);
    //            deltaTimes.push_back(d);
    //            break;
    //        }
    //        case Section::Camera:
    //            if (foundCamera) {
    //                throw ProfileParsingError(
    //                    lineNum,
    //                    "Camera section can only appear once per profile"
    //                );
    //            }

    //            camera = parseCamera(line, lineNum);
    //            foundCamera = true;
    //            break;
    //        case Section::MarkNodes:
    //        {
    //            std::string m = parseMarkNodes(line, lineNum);
    //            markNodes.push_back(std::move(m));
    //            break;
    //        }
    //        case Section::AdditionalScripts:
    //        {
    //            std::string a = parseAdditionalScript(line, lineNum);
    //            additionalScripts.push_back(std::move(a));
    //            break;
    //        }
    //        default:
    //            throw ghoul::MissingCaseException();
    //    }
    //}

    //if (!foundVersion) {
    //    throw ghoul::RuntimeError(
    //        "Did not find Version information when loading profile"
    //    );
    //}
}

std::string Profile::convertToScene() const {
    ZoneScoped

    std::string output;

    if (meta.has_value()) {
        output += "asset.meta = {";
        if (meta->name.has_value()) {
            output += fmt::format("  Name = {},", *meta->name);
        }
        if (meta->version.has_value()) {
            output += fmt::format("  Version = {},", *meta->version);
        }
        if (meta->description.has_value()) {
            output += fmt::format("  Description = {},", *meta->description);
        }
        if (meta->author.has_value()) {
            output += fmt::format("  Author = {},", *meta->author);
        }
        if (meta->url.has_value()) {
            output += fmt::format("  URL = {},", *meta->url);
        }
        if (meta->license.has_value()) {
            output += fmt::format("  License = {},", *meta->license);
        }

        output += "}";
    }

    // Modules
    for (const Module& m : modules) {
        output += fmt::format(
            "if openspace.modules.isLoaded(\"{}\") then {} else {} end\n",
            m.name, *m.loadedInstruction, *m.notLoadedInstruction
        );
    }

    // Assets
    for (const Asset& a : assets) {
        if (a.name.has_value()) {
            output += fmt::format("local {} = asset.require(\"{}\");\n", *a.name, a.path);
        }
        else {
            output += fmt::format("asset.require(\"{}\");\n", a.path);
        }
    }

    output += "asset.onInitialize(function()\n";
    // Keybindings
    for (const Keybinding& k : keybindings) {
        const std::string key = ghoul::to_string(k.key);
        const std::string name = k.name.empty() ? key : k.name;
        output += fmt::format(
            k.isLocal ?
            "openspace.bindKeyLocal(\"{}\", {}, [[{}]], [[{}]], [[{}]]);\n" :
            "openspace.bindKey(\"{}\", {}, [[{}]], [[{}]], [[{}]]);\n",
            key, k.script, k.documentation, name, k.guiPath
        );
    }

    // Time
    switch (time->type) {
        case Time::Type::Absolute:
            output += fmt::format("openspace.time.setTime(\"{}\")\n", time->value);
            break;
        case Time::Type::Relative:
            output += "local now = openspace.time.currentWallTime();\n";
            output += fmt::format(
                "local prev = openspace.time.advancedTime(now, \"{}\");\n", time->value
            );
            output += "openspace.time.setTime(prev);\n";
            break;
        default:
            throw ghoul::MissingCaseException();
    }

    // Delta Times
    {
        std::string times;
        for (const double d : deltaTimes) {
            times += fmt::format("{} ,", d);
        }
        output += fmt::format("openspace.time.setDeltaTimeSteps({{ {} }});\n", times);
    }

    // Mark Nodes
    {
        std::string nodes;
        for (const std::string& n : markNodes) {
            nodes += fmt::format("[[{}]],", n);
        }
        output += fmt::format("openspace.markInterestingNodes({{ {} }});\n", nodes);
    }

    // Properties
    for (const Property& p : properties) {
        switch (p.setType) {
            case Property::SetType::SetPropertyValue:
                output += fmt::format(
                    "openspace.setPropertyValue(\"{}\", {});\n",
                    p.name, p.value
                );
                break;
            case Property::SetType::SetPropertyValueSingle:
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
    if (camera.has_value()) {
        output += std::visit(
            overloaded {
                [](const CameraNavState& camera) {
                    std::string result;
                    result += "openspace.navigation.setNavigationState({";
                    result += fmt::format("Anchor = {}, ", camera.anchor);
                    if (camera.aim.has_value()) {
                        result += fmt::format("Aim = {}, ", *camera.aim);
                    }
                    if (!camera.referenceFrame.empty()) {
                        result += fmt::format("ReferenceFrame = {}, ", camera.referenceFrame);
                    }
                    result += fmt::format(
                        "Position = {{ {}, {}, {} }}, ",
                        camera.position.x, camera.position.y, camera.position.z
                    );
                    if (camera.up.has_value()) {
                        result += fmt::format(
                            "Up = {{ {}, {}, {} }}, ", 
                            camera.up->x, camera.up->y, camera.up->z
                        );
                    }
                    if (camera.yaw.has_value()) {
                        result += fmt::format("Yaw = {}, ", *camera.yaw);
                    }
                    if (camera.pitch.has_value()) {
                        result += fmt::format("Pitch = {} ", *camera.pitch);
                    }
                    result += "})\n";
                    return result;
                },
                [](const CameraGoToGeo& camera) {
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
            *camera
        );
    }

    for (const std::string& a : additionalScripts) {
        output += fmt::format("{}\n", a);
    }

    output += "end)\n";

    return output;
}


}  // namespace openspace
