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

#include <openspace/scripting/lualibrary.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/misc.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/fmt.h>
#include <string>
#include <iomanip>
#include <cctype>

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
    template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

    struct ProfileError : public ghoul::RuntimeError {
        explicit ProfileError(unsigned int lineNum, std::string msg)
            : ghoul::RuntimeError(
                fmt::format("Error @ line {}: {}", lineNum, std::move(msg)),
                "profileFile"
            )
        {}
    };
} // namespace

namespace openspace {

std::string serialize(const ProfileStruct& ps) {
    std::string output;
    output += fmt::format("{}\n", headerVersion);
    output += fmt::format("{}.{}.{}\n", ps.version.major, ps.version.minor, ps.version.patch);

    output += fmt::format("\n{}\n", headerModule);
    for (const ProfileStruct::Module& m : ps.modules) {
        output += fmt::format("{}\t{}\t{}\n", m.name, m.loadedInstruction, m.notLoadedInstruction);
    }

    output += fmt::format("\n{}\n", headerAsset);
    for (const ProfileStruct::Asset& a : ps.assets) {
        const std::string type = [](ProfileStruct::Asset::Type t) {
            switch (t) {
            case ProfileStruct::Asset::Type::Require: return "require";
            case ProfileStruct::Asset::Type::Request: return "request";
            default: throw ghoul::MissingCaseException();
            }
        }(a.type);
        output += fmt::format("{}\t{}\n", a.path, type);
    }

    output += fmt::format("\n{}\n", headerProperty);
    for (const ProfileStruct::Property& p : ps.properties) {
        const std::string type = [](ProfileStruct::Property::SetType t) {
            switch (t) {
            case ProfileStruct::Property::SetType::SetPropertyValue:
                return "setPropertyValue";
            case ProfileStruct::Property::SetType::SetPropertyValueSingle:
                return "setPropertyValueSingle";
            }
        }(p.setType);
        output += fmt::format("{}\t{}\t{}\n", type, p.name, p.value);
    }

    output += fmt::format("\n{}\n", headerKeybinding);
    for (const ProfileStruct::Keybinding& k : ps.keybindings) {
        const std::string local = k.isLocal ? "true" : "false";
        output += fmt::format(
            "{}\t{}\t{}\t{}\t{}\t{}\n",
            k.key, k.documentation, k.name, k.guiPath, local, k.script
        );
    }

    output += fmt::format("\n{}\n", headerTime);
    {
        const std::string type = [](ProfileStruct::Time::Type t) {
            switch (t) {
            case ProfileStruct::Time::Type::Absolute: return "absolute";
            case ProfileStruct::Time::Type::Relative: return "relative";
            default: throw ghoul::MissingCaseException();
            }
        }(ps.time.type);
        output += fmt::format("{}\t{}\n", type, ps.time.time);
    }

    output += fmt::format("\n{}\n", headerCamera);
    output += std::visit(
        overloaded {
            [](const ProfileStruct::CameraNavState& camera) {
                return fmt::format(
                    "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\n",
                    ProfileStruct::CameraNavState::Type,
                    camera.anchor, camera.aim, camera.referenceFrame, camera.position,
                    camera.up, camera.yaw, camera.pitch
                );
            },
            [](const ProfileStruct::CameraGoToGeo& camera) {
                std::string altitude;
                if (camera.altitude.has_value()) {
                    altitude = std::to_string(*camera.altitude);
                }

                return fmt::format(
                    "{}\t{}\t{}\t{}\t{}\n",
                    ProfileStruct::CameraGoToGeo::Type,
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

ProfileStruct deserialize(const std::string& filename) {
    return ProfileStruct();
}

std::string convertToSceneFile(const ProfileStruct& ps) {
    ZoneScoped

    std::string output;

    // Modules
    for (const ProfileStruct::Module& m : ps.modules) {
        output += fmt::format(
            "if openspace.modules.isLoaded(\"{}\") then {} else {} end\n",
            m.name, m.loadedInstruction, m.notLoadedInstruction
        );
    }

    // Assets
    for (const ProfileStruct::Asset& a : ps.assets) {
        if (!a.name.empty()) {
            output += fmt::format("local {} = ", a.name);
        }
        std::string type = [](ProfileStruct::Asset::Type t) {
            switch (t) {
                case ProfileStruct::Asset::Type::Request: return "request";
                case ProfileStruct::Asset::Type::Require: return "require";
                default: throw ghoul::MissingCaseException();
            }
        }(a.type);

        output += fmt::format("asset.{}(\"{}\");\n", type, a.path);
    }

    output += "asset.onInitialize(function()\n";
    // Keybindings
    for (const ProfileStruct::Keybinding& k : ps.keybindings) {
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
        case ProfileStruct::Time::Type::Absolute:
            output += fmt::format("openspace.time.setTime(\"{}\")\n", ps.time.time);
            break;
        case ProfileStruct::Time::Type::Relative:
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
    for (const ProfileStruct::Property& p : ps.properties) {
        constexpr const char* regular = "openspace.setPropertyValue(\"{}\", {});\n";
        constexpr const char* single = "openspace.setPropertyValueSingle(\"{}\", {});\n";

        switch (p.setType) {
            case ProfileStruct::Property::SetType::SetPropertyValue:
                output += fmt::format(
                    "openspace.setPropertyValue(\"{}\", {});\n",
                    p.name, p.value
                );
                break;
            case ProfileStruct::Property::SetType::SetPropertyValueSingle:
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
            [](const ProfileStruct::CameraNavState& camera) {
                std::string result;
                result += "  openspace.navigation.setNavigationState({";
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
            [](const ProfileStruct::CameraGoToGeo& camera) {
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

ProfileFile::ProfileFile(std::string filename) {
    clearAllFields();
    _lineNum = 1;
    std::ifstream inFile;

    try {
        inFile.open(filename, std::ifstream::in);
    }
    catch (const std::ifstream::failure& e) {
        throw ghoul::RuntimeError(fmt::format(
            "Exception opening profile file for read: {} ({})", filename, e.what()),
            "profileFile"
        );
    }

    try {
        std::string line;
        bool insideSection = false;
        while (std::getline(inFile, line)) {
            processIndividualLine(insideSection, line);
            _lineNum++;
        }
    }
    catch (const std::ifstream::failure& e) {
        throw ProfileError(
            _lineNum,
            fmt::format("Read error using getline in: {} ({})", filename, e.what()
        ));
    }
}

void ProfileFile::processIndividualLine(bool& insideSection, std::string line) {
    if (insideSection) {
        if (std::all_of(line.begin(), line.end(), ::isspace)) {
            insideSection = false;
        }
        else {
            if (parseCurrentSection != nullptr) {
                (this->*parseCurrentSection)(line);
            }
        }
    }
    else if (line.substr(0, 1) == "#") {
        determineSection(line);
        insideSection = true;
    }
}

void ProfileFile::writeToFile(const std::string& filename) const {
    if (filename.find('/') != std::string::npos) {
        LERROR("Profile filename must not contain path (/) elements");
        return;
    }
    else if (filename.find(':') != std::string::npos) {
        LERROR("Profile filename must not contain path (:) elements");
        return;
    }
    else if (filename.find('.') != std::string::npos) {
        LERROR("Only provide the filename to save without file extension");
        return;
    }
    const std::string absFilename = absPath("${ASSETS}/" + filename + ".profile");

    if (FileSys.fileExists(absFilename)) {
        LERROR(fmt::format(
            "Unable to save profile '{}'. File of same name already exists.",
            absFilename.c_str()
        ));
        return;
    }

    std::ofstream outFile;
    // @TODO (abock, 2020-06-15) Replace with non-throwing stream
    try {
        outFile.open(absFilename, std::ofstream::out);
    }
    catch (const std::ofstream::failure& e) {
        LERROR(fmt::format(
            "Exception opening profile file for write: {} ({})", absFilename, e.what()
        ));
    }

    try {
        outFile << serialize(profile);
    }
    catch (const std::ofstream::failure& e) {
        LERROR("Data write error to file: "
            + absFilename + " (" + e.what() + ")");
    }

    outFile.close();
}


const std::string& ProfileFile::version() const {
    return _version;
}

void ProfileFile::setVersion(std::string v) {
    _version = std::move(v);
}

void ProfileFile::clearAllFields() {
    _numLinesVersion = 0;
    _numLinesTime = 0;
    _numLinesCamera = 0;

    _version.clear();
    _time.clear();
    _camera.clear();
    _modules.clear();
    _assets.clear();
    _properties.clear();
    _keybindings.clear();
    _markNodes.clear();

    profile = ProfileStruct();
}

void ProfileFile::determineSection(std::string line) {
    if (line == headerVersion) {
        parseCurrentSection = &ProfileFile::parseVersion;
    }
    else if (line == headerModule) {
        parseCurrentSection = &ProfileFile::parseModule;
    }
    else if (line == headerAsset) {
        parseCurrentSection = &ProfileFile::parseAsset;
    }
    else if (line == headerProperty) {
        parseCurrentSection = &ProfileFile::parseProperty;
    }
    else if (line == headerKeybinding) {
        parseCurrentSection = &ProfileFile::parseKeybinding;
    }
    else if (line == headerTime) {
        parseCurrentSection = &ProfileFile::parseTime;
    }
    else if (line == headerCamera) {
        parseCurrentSection = &ProfileFile::parseCamera;
    }
    else if (line == headerMarkNodes) {
        parseCurrentSection = &ProfileFile::parseMarkNodes;
    }
    else {
        throw ProfileError(
            _lineNum,
            fmt::format("Invalid section header '{}'", line)
        );
    }
}

std::string ProfileFile::time() const {
    return _time;
}

std::string ProfileFile::camera() const {
    return _camera;
}

ProfileFile::Lines ProfileFile::modules() const {
    return _modules;
}

ProfileFile::Lines ProfileFile::assets() const {
    return _assets;
}

ProfileFile::Lines ProfileFile::properties() const {
    return _properties;
}

ProfileFile::Lines ProfileFile::keybindings() const {
    return _keybindings;
}

ProfileFile::Lines ProfileFile::markNodes() const {
    return _markNodes;
}

void ProfileFile::parseVersion(std::string line) {
    constexpr const size_t VersionLinesExpected = 1;

    if (++_numLinesVersion > VersionLinesExpected) {
        throw ProfileError(_lineNum, "Too many lines in Version section");
    }
    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
    if (fields.size() > 1) {
        throw ProfileError(_lineNum, "No tabs allowed in Version entry");
    }
    _version = line;

    //
    // New
    //
    std::vector<std::string> parts = ghoul::tokenizeString(line, '.');
    if (parts.empty() || parts.size() > 3) {
        throw ProfileError(
            _lineNum,
            fmt::format("Expected 1-3 version components, got {}", parts.size())
        );
    }

    ProfileStruct::Version version;

    version.major = std::stoi(parts[0]);
    if (parts.size() > 1) {
        version.minor = std::stoi(parts[1]);
    }
    if (parts.size() > 2) {
        version.patch = std::stoi(parts[2]);
    }
    profile.version = std::move(version);
}

void ProfileFile::parseModule(std::string line) {
    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');

    if (fields.size() != moduleFieldsExpected) {
        throw ProfileError(
            _lineNum,
            fmt::format("{} fields required in a Module entry", moduleFieldsExpected)
        );
    }
    std::vector<std::string> standard = {
        "module name",
        "",
        ""
    };
    verifyRequiredFields("Module", fields, standard, moduleFieldsExpected);
    _modules.push_back(line);


    //
    // New
    //
    if (fields.size() != 3) {
        throw ProfileError(
            _lineNum,
            fmt::format("Expected 3 fields in a Module entry, got {}", fields.size())
        );
    }
    ProfileStruct::Module m;
    m.name = fields[0];
    m.loadedInstruction = fields[1];
    m.notLoadedInstruction = fields[2];
    profile.modules.push_back(std::move(m));
}

void ProfileFile::parseAsset(std::string line) {
    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
    if (fields.size() != assetFieldsExpected) {
        throw ProfileError(
            _lineNum,
            fmt::format("{} fields required in an Asset entry", assetFieldsExpected)
        );
    }
    std::vector<std::string> standard = {
        "asset name",
        "",
        ""
    };
    verifyRequiredFields("Asset", fields, standard, assetFieldsExpected);
    _assets.push_back(line);


    //
    // New
    //
    if (fields.size() != 3) {
        throw ProfileError(
            _lineNum,
            fmt::format("Expected 3 fields in an Asset entry, got {}", fields.size())
        );
    }

    ProfileStruct::Asset a;
    a.path = fields[0];
    a.type = [&](const std::string& type) -> ProfileStruct::Asset::Type {
        if (type == "require") {
            return ProfileStruct::Asset::Type::Require;
        }
        if (type == "request") {
            return ProfileStruct::Asset::Type::Request;
        }
        throw ProfileError(
            _lineNum,
            fmt::format("Expected asset type 'require' or 'request', got {}", type)
        );
    }(fields[1]);
    a.name = fields[2];
    profile.assets.push_back(std::move(a));
}

void ProfileFile::parseProperty(std::string line) {
    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');

    if (fields.size() != propertyFieldsExpected) {
        throw ProfileError(
            _lineNum,
            fmt::format("{} fields required in Property entry", propertyFieldsExpected)
        );
    }
    std::vector<std::string> standard = {
        "set command",
        "name",
        "value"
    };
    verifyRequiredFields("Property", fields, standard, propertyFieldsExpected);
    _properties.push_back(line);


    //
    // New
    //
    if (fields.size() != 3) {
        throw ProfileError(
            _lineNum,
            fmt::format("Expected 3 fields in Property entry, got {}", fields.size())
        );
    }
    ProfileStruct::Property p;
    p.setType = [&](const std::string& type) -> ProfileStruct::Property::SetType {
        if (type == "setPropertyValue") {
            return ProfileStruct::Property::SetType::SetPropertyValue;
        }
        if (type == "setPropertyValueSingle") {
            return ProfileStruct::Property::SetType::SetPropertyValueSingle;
        }
        throw ProfileError(
            _lineNum,
            fmt::format(
                "Expected property set type 'setPropertyValue' or "
                "'setPropertyValueSingle', got {}",
                type
            )
        );
    }(fields[0]);
    p.name = fields[1];
    p.value = fields[2];
    profile.properties.push_back(std::move(p));
}

void ProfileFile::parseKeybinding(std::string line) {
    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');

    if (fields.size() != keybindingFieldsExpected) {
        throw ProfileError(
            _lineNum,
            fmt::format("{} fields required in Keybinding entry", keybindingFieldsExpected)
        );
    }
    std::vector<std::string> standard = {
        "key",
        "documentation",
        "name",
        "GuiPath",
        "local(T/F)",
        "script to execute"
    };
    verifyRequiredFields("Keybinding", fields, standard, keybindingFieldsExpected);
    _keybindings.push_back(line);

    
    //
    // New
    //
    if (fields.size() != 6) {
        throw ProfileError(
            _lineNum,
            fmt::format("Expected 6 fields in Keybinding entry, got {}", fields.size())
        );
    }
    ProfileStruct::Keybinding kb;
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
            _lineNum,
            fmt::format("Expected 'false' or 'true' for the local path, got {}", local)
        );
    }(fields[4]);
    kb.script = fields[5];
    profile.keybindings.push_back(std::move(kb));
}

void ProfileFile::parseTime(std::string line) {
    if (++_numLinesTime > timeLinesExpected) {
        throw ProfileError(_lineNum, "Too many lines in Time section");
    }

    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
    if (fields.size() != timeFieldsExpected) {
        throw ProfileError(
            _lineNum,
            fmt::format("{} fields required in Time entry", timeFieldsExpected)
        );
    }
    std::vector<std::string> standard = {
        "time set type",
        "time value to set"
    };
    verifyRequiredFields("Time", fields, standard, timeFieldsExpected);
    _time = line;


    //
    // New
    //
    if (fields.size() != 2) {
        throw ProfileError(
            _lineNum,
            fmt::format("Expected 2 fields in Time entry, got {}", fields.size())
        );
    }
    ProfileStruct::Time time;
    time.type = [&](const std::string& type) -> ProfileStruct::Time::Type {
        if (type == "absolute") {
            return ProfileStruct::Time::Type::Absolute;
        }
        if (type == "relative") {
            return ProfileStruct::Time::Type::Relative;
        }
        throw ProfileError(
            _lineNum,
            fmt::format("Expected 'absolute' or 'relative' for the type, got {}", type)
        );
    }(fields[0]);
    time.time = fields[1];
    profile.time = std::move(time);
}

void ProfileFile::parseCamera(std::string line) {
    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');

    if (++_numLinesCamera > cameraLinesExpected) {
        throw ProfileError(_lineNum, "Too many lines in Camera section");
    }
    
    if (fields.size() == cameraNavigationFieldsExpected) {
        std::vector<std::string> standard = {
            "Type of camera set (setNavigationState)",
            "setNavigationState Anchor",
            "",
            "",
            "setNavigationState position vector",
            "",
            "",
            ""
        };
        verifyRequiredFields("Camera navigation", fields, standard,
            cameraNavigationFieldsExpected);
    }
    else if (fields.size() == cameraGeoFieldsExpected) {
        std::vector<std::string> standard = {
            "Type of camera set (goToGeo)",
            "",
            "Camera goToGeo Latitude",
            "Camera goToGeo Longitude",
            ""
        };
        verifyRequiredFields("Camera goToGeo", fields, standard,
            cameraGeoFieldsExpected);
    }
    else {
        throw ProfileError(
            _lineNum,
            fmt::format(
                "{} or {} fields required in Camera entry",
                cameraNavigationFieldsExpected, cameraGeoFieldsExpected
            )
        );
    }
    _camera = line;


    //
    // New
    //
    if (fields.empty()) {
        throw ProfileError(_lineNum, "No values specified for Camera location");
    }
    if (fields[0] == ProfileStruct::CameraNavState::Type) {
        if (fields.size() != 8) {
            throw ProfileError(
                _lineNum,
                fmt::format(
                    "Expected 8 fields in the Camera entry, got {}", fields.size()
                )
            );
        }

        ProfileStruct::CameraNavState camera;
        camera.anchor = fields[1];
        camera.aim = fields[2];
        camera.referenceFrame = fields[3];
        camera.position = fields[4];
        camera.up = fields[5];
        camera.yaw = fields[6];
        camera.pitch = fields[7];
        profile.camera = std::move(camera);
        return;
    }
    if (fields[0] == ProfileStruct::CameraGoToGeo::Type) {
        if (fields.size() != 5) {
            throw ProfileError(
                _lineNum,
                fmt::format(
                    "Expected 5 fields in the Camera entry, got {}", fields.size()
                )
            );
        }

        ProfileStruct::CameraGoToGeo camera;
        camera.anchor = fields[1];
        camera.latitude = std::stod(fields[2]);
        camera.longitude = std::stod(fields[3]);
        if (!fields[4].empty()) {
            camera.altitude = std::stod(fields[4]);
        }
        profile.camera = std::move(camera);
        return;
    }
    throw ProfileError(
        _lineNum,
        fmt::format(
            "Expected 'setNavigationState' or 'goToGeo' for the type, got {}", fields[0]
        )
    );
}

void ProfileFile::parseMarkNodes(std::string line) {
    std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');

    if (fields.size() != markNodesFieldsExpected) {
        throw ProfileError(
            _lineNum,
            fmt::format("{} field required in a Mark Nodes entry", markNodesFieldsExpected)
        );
    }
    std::vector<std::string> standard = { "Mark Interesting Node name" };
    verifyRequiredFields("Mark Interesting Nodes", fields, standard,
        markNodesFieldsExpected);
    _markNodes.push_back(line);


    //
    // New
    //
    profile.markNodes.push_back(line);
}

void ProfileFile::verifyRequiredFields(std::string sectionName,
                                       std::vector<std::string> fields,
                                       std::vector<std::string> standard,
                                       unsigned int nFields)
{
    for (unsigned int i = 0; i < fields.size(); i++) {
        if (!standard[i].empty() && fields[i].empty()) {
            std::string errMsg = sectionName + " " + standard[i];
            errMsg += "(arg " + std::to_string(i) + "/";
            errMsg += std::to_string(nFields) + ") is required";
            throw ProfileError(_lineNum, std::move(errMsg));
        }
    }
}

void ProfileFile::updateTime(std::string line) {
    _time = std::move(line);
}

void ProfileFile::updateCamera(std::string line) {
    _camera = std::move(line);
}

void ProfileFile::addModuleLine(std::string line) {
    _modules.push_back(std::move(line));
}

void ProfileFile::addAssetLine(std::string line) {
    _assets.push_back(std::move(line));
}

void ProfileFile::addPropertyLine(std::string line) {
    _properties.push_back(std::move(line));
}

void ProfileFile::addKeybindingLine(std::string line) {
    _keybindings.push_back(std::move(line));
}

void ProfileFile::addMarkNodesLine(std::string line) {
    _markNodes.push_back(std::move(line));
}

void ProfileFile::clearAssets() {
    _assets.clear();
}

}  // namespace openspace
