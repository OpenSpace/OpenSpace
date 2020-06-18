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

#include <openspace/scene/profilefile.h>

#include <ghoul/misc/assert.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/misc.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/fmt.h>

namespace openspace {

namespace {
    constexpr const char* _loggerCat = "ProfileFile";
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

    struct ProfileError : public ghoul::RuntimeError {
        explicit ProfileError(std::string msg)
            : ghoul::RuntimeError(std::move(msg), "profileFile")
        {}

        ProfileError(unsigned int lineNum, std::string msg)
            : ghoul::RuntimeError(
                fmt::format("Error @ line {}: {}", lineNum, std::move(msg)),
                "profileFile"
            )
        {}
    };

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

    Section parseSection(const std::string& line, int lineNumber) {
        if (line == headerVersion) { return Section::Version; }
        if (line == headerModule) { return Section::Module; }
        if (line == headerAsset) { return Section::Asset; }
        if (line == headerProperty) { return Section::Property; }
        if (line == headerKeybinding) { return Section::Keybinding; }
        if (line == headerTime) { return Section::Time; }
        if (line == headerCamera) { return Section::Camera; }
        if (line == headerMarkNodes) { return Section::MarkNodes; }

        throw ProfileError(lineNumber, fmt::format("Invalid section header: {}", line));
    }

    void parseVersion(ProfileData& ps, const std::string& line, int lineNumber) {
        std::vector<std::string> parts = ghoul::tokenizeString(line, '.');
        if (parts.empty() || parts.size() > 3) {
            throw ProfileError(
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
        ps.version = std::move(version);
    }

    void parseModule(ProfileData& ps, const std::string& line, int lineNumber) {
        std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
        if (fields.size() != 3) {
            throw ProfileError(
                lineNumber,
                fmt::format("Expected 3 fields in a Module entry, got {}", fields.size())
            );
        }
        ProfileData::Module m;
        m.name = fields[0];
        m.loadedInstruction = fields[1];
        m.notLoadedInstruction = fields[2];
        ps.modules.push_back(std::move(m));
    }

    void parseAsset(ProfileData& ps, const std::string& line, int lineNumber) {
        std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
        if (fields.size() != 3) {
            throw ProfileError(
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
            throw ProfileError(
                lineNumber,
                fmt::format("Expected asset type 'require' or 'request', got {}", type)
            );
        }(fields[1]);
        a.name = fields[2];
        ps.assets.push_back(std::move(a));
    }

    void parseProperty(ProfileData& ps, const std::string& line, int lineNumber) {
        std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
        if (fields.size() != 3) {
            throw ProfileError(
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
            throw ProfileError(
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
        ps.properties.push_back(std::move(p));
    }

    void parseKeybinding(ProfileData& ps, const std::string& line, int lineNumber) {
        std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
        if (fields.size() != 6) {
            throw ProfileError(
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
            throw ProfileError(
                lineNumber,
                fmt::format("Expected 'false' or 'true' for the local path, got {}", local)
            );
        }(fields[4]);
        kb.script = fields[5];
        ps.keybindings.push_back(std::move(kb));
    }

    void parseTime(ProfileData& ps, const std::string& line, int lineNumber) {
        std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
        if (fields.size() != 2) {
            throw ProfileError(
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
            throw ProfileError(
                lineNumber,
                fmt::format("Expected 'absolute' or 'relative' for the type, got {}", type)
            );
        }(fields[0]);
        time.time = fields[1];
        ps.time = std::move(time);
    }

    void parseCamera(ProfileData& ps, const std::string& line, int lineNumber) {
        std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
        if (fields.empty()) {
            throw ProfileError(lineNumber, "No values specified for Camera location");
        }
        ps.camera = [&](const std::string& type) ->
            std::variant<ProfileData::CameraNavState, ProfileData::CameraGoToGeo>
        {
            if (type == ProfileData::CameraNavState::Type) {
                if (fields.size() != 8) {
                    throw ProfileError(
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
                    throw ProfileError(
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
            throw ProfileError(
                lineNumber,
                fmt::format(
                    "Expected 'setNavigationState' or 'goToGeo' for the type, got {}",
                    fields[0]
                )
            );
        }(fields[0]);
    }

    void parseMarkNodes(ProfileData& ps, const std::string& line, int) {
        ps.markNodes.push_back(line);
    }
} // namespace

std::string serialize(const ProfileData& ps) {
    std::string output;
    output += fmt::format("{}\n", headerVersion);
    output += fmt::format(
        "{}.{}.{}\n",
        ps.version.major, ps.version.minor, ps.version.patch
    );

    output += fmt::format("\n{}\n", headerModule);
    for (const ProfileData::Module& m : ps.modules) {
        output += fmt::format(
            "{}\t{}\t{}\n",
            m.name, m.loadedInstruction, m.notLoadedInstruction
        );
    }

    output += fmt::format("\n{}\n", headerAsset);
    for (const ProfileData::Asset& a : ps.assets) {
        const std::string type = [](ProfileData::Asset::Type t) {
            switch (t) {
                case ProfileData::Asset::Type::Require: return "require";
                case ProfileData::Asset::Type::Request: return "request";
                default: throw ghoul::MissingCaseException();
            }
        }(a.type);
        output += fmt::format("{}\t{}\n", a.path, type);
    }

    output += fmt::format("\n{}\n", headerProperty);
    for (const ProfileData::Property& p : ps.properties) {
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

    output += fmt::format("\n{}\n", headerKeybinding);
    for (const ProfileData::Keybinding& k : ps.keybindings) {
        const std::string local = k.isLocal ? "true" : "false";
        output += fmt::format(
            "{}\t{}\t{}\t{}\t{}\t{}\n",
            k.key, k.documentation, k.name, k.guiPath, local, k.script
        );
    }

    output += fmt::format("\n{}\n", headerTime);
    {
        const std::string type = [](ProfileData::Time::Type t) {
            switch (t) {
                case ProfileData::Time::Type::Absolute: return "absolute";
                case ProfileData::Time::Type::Relative: return "relative";
                default: throw ghoul::MissingCaseException();
            }
        }(ps.time.type);
        output += fmt::format("{}\t{}\n", type, ps.time.time);
    }

    output += fmt::format("\n{}\n", headerCamera);
    output += std::visit(
        overloaded {
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
        ps.camera
    );

    output += fmt::format("\n{}\n", headerMarkNodes);
    for (const std::string& n : ps.markNodes) {
        output += fmt::format("{}\n", n);
    }

    return output;
}

ProfileData deserialize(const std::vector<std::string>& content) {
    ProfileData result;

    Section currentSection = Section::None;
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
                parseVersion(result, line, lineNum);
                break;
            case Section::Module:
                parseModule(result, line, lineNum);
                break;
            case Section::Asset:
                parseAsset(result, line, lineNum);
                break;
            case Section::Property:
                parseProperty(result, line, lineNum);
                break;
            case Section::Keybinding:
                parseKeybinding(result, line, lineNum);
                break;
            case Section::Time:
                parseTime(result, line, lineNum);
                break;
            case Section::Camera:
                parseCamera(result, line, lineNum);
                break;
            case Section::MarkNodes:
                parseMarkNodes(result, line, lineNum);
                break;
            default:
                throw ghoul::MissingCaseException();
        }
    }

    return result;
}

std::string convertToSceneFile(const ProfileData& ps) {
    ZoneScoped

    std::string output;

    // Modules
    for (const ProfileData::Module& m : ps.modules) {
        output += fmt::format(
            "if openspace.modules.isLoaded(\"{}\") then {} else {} end\n",
            m.name, m.loadedInstruction, m.notLoadedInstruction
        );
    }

    // Assets
    for (const ProfileData::Asset& a : ps.assets) {
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
    for (const ProfileData::Keybinding& k : ps.keybindings) {
        const std::string name = k.name.empty() ? k.key : k.name;
        output += fmt::format(
            k.isLocal ?
                "openspace.bindKeyLocal(\"{}\", {}, [[{}]], [[{}]], [[{}]]);\n" :
                "openspace.bindKey(\"{}\", {}, [[{}]], [[{}]], [[{}]]);\n",
            k.key, k.script, k.documentation, k.name.empty() ? k.key : k.name, k.guiPath
        );
    }

    // Time
    switch (ps.time.type) {
        case ProfileData::Time::Type::Absolute:
            output += fmt::format("openspace.time.setTime(\"{}\")\n", ps.time.time);
            break;
        case ProfileData::Time::Type::Relative:
            output += "local now = openspace.time.currentWallTime();\n";
            output += fmt::format(
                "local prev = openspace.time.advancedTime(now, \"{}\");\n", ps.time.time
            );
            output += "openspace.time.setTime(prev);\n";
        default:
            throw ghoul::MissingCaseException();
    }

    // Mark Nodes
    {
        std::string nodes;
        for (const std::string& n : ps.markNodes) {
            nodes += fmt::format("[[ {} ]],", n);
        }
        output += fmt::format("openspace.markInterestingNodes({{ {} }});\n", nodes);
    }

    // Properties
    for (const ProfileData::Property& p : ps.properties) {
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
        overloaded {
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
        ps.camera
    );
    output += "end)\n";

    return output;
}

}  // namespace openspace
