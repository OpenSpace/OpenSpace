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
#include <ghoul/fmt.h>
#include <ghoul/misc/misc.h>
#include <ghoul/misc/profiling.h>

#include "profile_lua.inl"

namespace openspace {

namespace {
    constexpr const char* _loggerCat = "Profile";
    
    constexpr const char* headerVersion = "#Version";
    constexpr const char* headerMeta = "#Meta";
    constexpr const char* headerModule = "#Module";
    constexpr const char* headerAsset = "#Asset";
    constexpr const char* headerProperty = "#Property";
    constexpr const char* headerKeybinding = "#Keybinding";
    constexpr const char* headerTime = "#Time";
    constexpr const char* headerDeltaTimes = "#DeltaTimes";
    constexpr const char* headerCamera = "#Camera";
    constexpr const char* headerMarkNodes = "#MarkNodes";
    constexpr const char* headerAdditionalScripts = "#AdditionalScripts";

    // Helper structs for the visitor pattern of the std::variant
    template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
    template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

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

    enum class Section {
        None,
        Version,
        Meta,
        Module,
        Asset,
        Property,
        Keybinding,
        Time,
        DeltaTimes,
        Camera,
        MarkNodes,
        AdditionalScripts
    };

    Section parseSection(const std::string& line, int lineNumber) {
        if (line == headerVersion) { return Section::Version; }
        if (line == headerMeta) { return Section::Meta; }
        if (line == headerModule) { return Section::Module; }
        if (line == headerAsset) { return Section::Asset; }
        if (line == headerProperty) { return Section::Property; }
        if (line == headerKeybinding) { return Section::Keybinding; }
        if (line == headerTime) { return Section::Time; }
        if (line == headerDeltaTimes) { return Section::DeltaTimes; }
        if (line == headerCamera) { return Section::Camera; }
        if (line == headerMarkNodes) { return Section::MarkNodes; }
        if (line == headerAdditionalScripts) { return Section::AdditionalScripts; }

        throw ProfileParsingError(
            lineNumber,
            fmt::format("Invalid section header: {}", line)
        );
    }

    [[ nodiscard ]] Profile::Version parseVersion(const std::string& line, int lineNumber)
    {
        std::vector<std::string> parts = ghoul::tokenizeString(line, '.');
        if (parts.size() > 2) {
            throw ProfileParsingError(
                lineNumber,
                fmt::format("Expected 1-2 version components, got {}", parts.size())
            );
        }

        try {
            Profile::Version version;
            if (parts.empty()) {
                version.major = std::stoi(line);
            }
            else {
                version.major = std::stoi(parts[0]);
            }
            if (parts.size() > 1) {
                version.minor = std::stoi(parts[1]);
            }
            return version;
        }
        catch (const std::invalid_argument&) {
            throw ProfileParsingError(
                lineNumber,
                "Error parsing Version. Version number is not a number"
            );
        }
    }

    [[ nodiscard ]] Profile::Module parseModule(const std::string& line, int lineNumber) {
        std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
        if (fields.size() != 3) {
            throw ProfileParsingError(
                lineNumber,
                fmt::format("Expected 3 fields in a Module entry, got {}", fields.size())
            );
        }
        Profile::Module m;
        m.name = fields[0];
        m.loadedInstruction = fields[1];
        m.notLoadedInstruction = fields[2];
        return m;
    }

    enum class MetaLineType {
        Name,
        Version,
        Description,
        Author,
        URL,
        License
    };

    [[ nodiscard ]] std::pair<MetaLineType, std::string> parseMeta(
                                                                   const std::string& line,
                                                                           int lineNumber)
    {
        std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
        if (fields.size() < 2) {
            throw ProfileParsingError(
                lineNumber,
                fmt::format("Expected 2 fields in a Meta line, got {}", fields.size())
            );
        }

        const std::string type = fields[0];

        // Users are allowed to use \t in their lines, meaning that the fields could
        // contain more than the 2 elements that we expected
        fields.erase(fields.begin());
        const std::string content = ghoul::join(fields, "\t");

        if (type == "Name") {
            return { MetaLineType::Name, content };
        }
        else if (type == "Version") {
            return { MetaLineType::Version, content };
        }
        else if (type == "Description") {
            return { MetaLineType::Description, content };
        }
        else if (type == "Author") {
            return { MetaLineType::Author, content };
        }
        else if (type == "URL") {
            return { MetaLineType::URL, content };
        }
        else if (type == "License") {
            return { MetaLineType::License, content };
        }
        else {
            throw ProfileParsingError(
                lineNumber,
                fmt::format("Unknown meta line type '{}'", type)
            );
        }
    }

    [[ nodiscard ]] Profile::Asset parseAsset(const std::string& line, int lineNumber) {
        std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
        if (fields.size() != 2) {
            throw ProfileParsingError(
                lineNumber,
                fmt::format("Expected 2 fields in an Asset entry, got {}", fields.size())
            );
        }

        Profile::Asset a;
        a.path = fields[0];
        a.name = fields[1];
        return a;
    }

    [[ nodiscard ]] Profile::Property parseProperty(const std::string& line, int lineNumber) {
        std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
        if (fields.size() != 3) {
            throw ProfileParsingError(
                lineNumber,
                fmt::format("Expected 3 fields in Property entry, got {}", fields.size())
            );
        }
        Profile::Property p;
        p.setType = [&](const std::string& type) -> Profile::Property::SetType {
            if (type == "setPropertyValue") {
                return Profile::Property::SetType::SetPropertyValue;
            }
            if (type == "setPropertyValueSingle") {
                return Profile::Property::SetType::SetPropertyValueSingle;
            }
            throw ProfileParsingError(
                lineNumber,
                fmt::format(
                    "Expected property set type 'setPropertyValue' or "
                    "'setPropertyValueSingle', got '{}'",
                    type
                )
            );
        }(fields[0]);
        p.name = fields[1];
        p.value = fields[2];
        return p;
    }

    [[ nodiscard ]] Profile::Keybinding parseKeybinding(const std::string& line, int lineNumber) {
        std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
        if (fields.size() != 6) {
            throw ProfileParsingError(
                lineNumber,
                fmt::format("Expected 6 fields in Keybinding entry, got {}", fields.size())
            );
        }
        Profile::Keybinding kb;
        try {
            kb.key = stringToKey(fields[0]);
        }
        catch (const ghoul::RuntimeError& e) {
            throw ProfileParsingError(lineNumber, e.what());
        }
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

    [[ nodiscard ]] Profile::Time parseTime(const std::string& line, int lineNumber) {
        std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
        if (fields.size() != 2) {
            throw ProfileParsingError(
                lineNumber,
                fmt::format("Expected 2 fields in Time entry, got {}", fields.size())
            );
        }
        Profile::Time time;
        time.type = [&](const std::string& type) -> Profile::Time::Type {
            if (type == "absolute") {
                return Profile::Time::Type::Absolute;
            }
            if (type == "relative") {
                return Profile::Time::Type::Relative;
            }
            throw ProfileParsingError(
                lineNumber,
                fmt::format("Expected 'absolute' or 'relative' for the type, got {}", type)
            );
        }(fields[0]);
        time.time = fields[1];
        return time;
    }

    [[ nodiscard ]] double parseDeltaTime(const std::string& line, int lineNumber) {
        try {
            return std::stod(line);
        }
        catch (const std::invalid_argument&) {
            throw ProfileParsingError(
                lineNumber,
                fmt::format("Expected a number for delta time entry, got '{}'", line)
            );
        }
    }

    [[ nodiscard ]] Profile::CameraType parseCamera(const std::string& line, int lineNumber) {
        std::vector<std::string> fields = ghoul::tokenizeString(line, '\t');
        Profile::CameraType camera = [&](const std::string& type) ->
            std::variant<Profile::CameraNavState, Profile::CameraGoToGeo>
        {
            if (type == Profile::CameraNavState::Type) {
                if (fields.size() != 8) {
                    throw ProfileParsingError(
                        lineNumber,
                        fmt::format(
                            "Expected 8 fields in the Camera entry, got {}", fields.size()
                        )
                    );
                }

                Profile::CameraNavState camera;
                camera.anchor = fields[1];
                camera.aim = fields[2];
                camera.referenceFrame = fields[3];

                std::vector<std::string> position = ghoul::tokenizeString(fields[4], ' ');
                if (position.size() != 3) {
                    throw ProfileParsingError(
                        lineNumber,
                        fmt::format(
                            "Expected 3 fields for the camera's position, got {}",
                            position.size()
                        )
                    );
                }
                try {
                    camera.position = glm::dvec3(
                        std::stod(position[0]),
                        std::stod(position[1]),
                        std::stod(position[2])
                    );
                }
                catch (const std::invalid_argument&) {
                    throw ProfileParsingError(
                        lineNumber,
                        "Camera's position components must be numbers"
                    );
                }

                std::vector<std::string> up = ghoul::tokenizeString(fields[5], ' ');
                if (up.size() != 0 && up.size() != 3) {
                    throw ProfileParsingError(
                        lineNumber,
                        fmt::format(
                            "Expected 0 or 3 fields for the camera's up vector, got {}",
                            up.size()
                        )
                    );
                }
                if (up.size() == 3) {
                    try {
                        camera.up = glm::dvec3(
                            std::stod(up[0]),
                            std::stod(up[1]),
                            std::stod(up[2])
                        );
                    }
                    catch (const std::invalid_argument&) {
                        throw ProfileParsingError(
                            lineNumber,
                            "Camera's up vector components must be numbers"
                        );
                    }
                }

                if (!fields[6].empty()) {
                    try {
                        camera.yaw = std::stod(fields[6]);
                    }
                    catch (const std::invalid_argument&) {
                        throw ProfileParsingError(
                            lineNumber,
                            "Camera's yaw value must be a number"
                        );
                    }
                }

                if (!fields[7].empty()) {
                    try {
                        camera.pitch = std::stod(fields[7]);
                    }
                    catch (const std::invalid_argument&) {
                        throw ProfileParsingError(
                            lineNumber,
                            "Camera's pitch value must be a number"
                        );
                    }
                }
                return camera;
            }
            if (type == Profile::CameraGoToGeo::Type) {
                if (fields.size() != 5) {
                    throw ProfileParsingError(
                        lineNumber,
                        fmt::format(
                            "Expected 5 fields in the Camera entry, got {}", fields.size()
                        )
                    );
                }

                Profile::CameraGoToGeo camera;
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

    [[ nodiscard ]] std::string parseAdditionalScript(const std::string& line, int) {
        return line;
    }
} // namespace

void Profile::saveCurrentSettingsToProfile(const properties::PropertyOwner& rootOwner,
                                           std::string currentTime,
                                 interaction::NavigationHandler::NavigationState navState)
{
    _version = Profile::CurrentVersion;

    //
    // Update properties
    //
    std::vector<properties::Property*> ps = changedProperties(rootOwner);

    for (properties::Property* prop : ps) {
        Property p;
        p.setType = Property::SetType::SetPropertyValueSingle;
        p.name = prop->fullyQualifiedIdentifier();
        p.value = prop->getStringValue();
        _properties.push_back(std::move(p));
    }

    //
    // add current time to profile file
    //
    Time t;
    t.time = std::move(currentTime);
    t.type = Time::Type::Absolute;
    _time = std::move(t);

    // Delta times
    std::vector<double> dts = global::timeManager.deltaTimeSteps();
    _deltaTimes = std::move(dts);

    // Camera

    CameraNavState c;
    c.anchor = navState.anchor;
    c.aim = navState.aim;
    c.referenceFrame = navState.referenceFrame;
    c.position = navState.position;
    c.up = navState.up;
    c.yaw = navState.yaw;
    c.pitch = navState.pitch;
    _camera = std::move(c);
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
        _assets.begin(),
        _assets.end(),
        [path](const Asset& a) { return a.path == path; }
    );

    if (it != _assets.end()) {
        // Asset already existed, so nothing to do here
        return;
    }

    Asset a;
    a.path = path;
    _assets.push_back(std::move(a));
}

void Profile::removeAsset(const std::string& path) {
    ZoneScoped

    if (_ignoreUpdates) {
        return;
    }

    const auto it = std::find_if(
        _assets.cbegin(),
        _assets.cend(),
        [path](const Asset& a) { return a.path == path; }
    );

    if (it == _assets.end()) {
        throw ghoul::RuntimeError(fmt::format(
            "Tried to remove non-existing asset '{}'", path
        ));
    }

    _assets.erase(it);
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
    output += fmt::format("{}.{}\n", _version.major, _version.minor);

    if (_meta.has_value()) {
        output += fmt::format("\n{}\n", headerMeta);
        if (!_meta->name.empty()) {
            output += fmt::format("Name\t{}\n", _meta->name);
        }
        if (!_meta->version.empty()) {
            output += fmt::format("Version\t{}\n", _meta->version);
        }
        if (!_meta->description.empty()) {
            output += fmt::format("Description\t{}\n", _meta->description);
        }
        if (!_meta->author.empty()) {
            output += fmt::format("Author\t{}\n", _meta->author);
        }
        if (!_meta->url.empty()) {
            output += fmt::format("URL\t{}\n", _meta->url);
        }
        if (!_meta->license.empty()) {
            output += fmt::format("License\t{}\n", _meta->license);
        }
    }

    if (!_modules.empty()) {
        output += fmt::format("\n{}\n", headerModule);
        for (const Module& m : _modules) {
            output += fmt::format(
                "{}\t{}\t{}\n",
                m.name, m.loadedInstruction, m.notLoadedInstruction
            );
        }
    }

    if (!_assets.empty()) {
        output += fmt::format("\n{}\n", headerAsset);
        for (const Asset& a : _assets) {
            output += fmt::format("{}\t{}\n", a.path, a.name);
        }
    }

    if (!_properties.empty()) {
        output += fmt::format("\n{}\n", headerProperty);
        for (const Property& p : _properties) {
            const std::string type = [](Property::SetType t) {
                switch (t) {
                    case Property::SetType::SetPropertyValue:
                        return "setPropertyValue";
                    case Property::SetType::SetPropertyValueSingle:
                        return "setPropertyValueSingle";
                    default:
                        throw ghoul::MissingCaseException();
                }
            }(p.setType);
            output += fmt::format("{}\t{}\t{}\n", type, p.name, p.value);
        }
    }

    if (!_keybindings.empty()) {
        output += fmt::format("\n{}\n", headerKeybinding);
        for (const Keybinding& k : _keybindings) {
            const std::string key = ghoul::to_string(k.key);
            const std::string local = k.isLocal ? "true" : "false";
            output += fmt::format(
                "{}\t{}\t{}\t{}\t{}\t{}\n",
                key, k.documentation, k.name, k.guiPath, local, k.script
            );
        }
    }
    
    if (_time.has_value()) {
        output += fmt::format("\n{}\n", headerTime);
        {
            const std::string type = [](Time::Type t) {
                switch (t) {
                    case Time::Type::Absolute: return "absolute";
                    case Time::Type::Relative: return "relative";
                    default: throw ghoul::MissingCaseException();
                }
            }(_time->type);
            output += fmt::format("{}\t{}\n", type, _time->time);
        }
    }

    if (!_deltaTimes.empty()) {
        output += fmt::format("\n{}\n", headerDeltaTimes);
        for (const double d : _deltaTimes) {
            output += fmt::format("{}\n", d);
        }
    }

    if (_camera.has_value()) {
        output += fmt::format("\n{}\n", headerCamera);
        output += std::visit(
            overloaded {
                [](const CameraNavState& camera) {
                    std::string position = fmt::format(
                        "{}, {}, {}",
                        camera.position.x, camera.position.y, camera.position.z
                    );
                    std::string up = camera.up.has_value() ?
                        fmt::format(
                            "{}, {}, {}", camera.up->x, camera.up->y, camera.up->z
                        ) :
                        "";
                    std::string yaw = camera.yaw.has_value() ?
                        fmt::format("{}", *camera.yaw) :
                        "";
                    std::string pitch = camera.pitch.has_value() ?
                        fmt::format("{}", *camera.pitch) :
                        "";

                    return fmt::format(
                        "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\n",
                        CameraNavState::Type,
                        camera.anchor, camera.aim, camera.referenceFrame, position, up,
                        yaw, pitch
                    );
                },
                [](const Profile::CameraGoToGeo& camera) {
                    if (camera.altitude.has_value()) {
                        return fmt::format(
                            "{}\t{}\t{}\t{}\t{}\n",
                            CameraGoToGeo::Type,
                            camera.anchor, camera.latitude, camera.longitude,
                            *camera.altitude
                        );
                    }
                    else {
                        return fmt::format(
                            "{}\t{}\t{}\t{}\t\n",
                            CameraGoToGeo::Type,
                            camera.anchor, camera.latitude, camera.longitude
                        );
                    }
                }
            },
            *_camera
        );
    }

    if (!_markNodes.empty()) {
        output += fmt::format("\n{}\n", headerMarkNodes);
        for (const std::string& n : _markNodes) {
            output += fmt::format("{}\n", n);
        }
    }

    if (!_additionalScripts.empty()) {
        output += fmt::format("\n{}\n", headerAdditionalScripts);
        for (const std::string& s : _additionalScripts) {
            output += fmt::format("{}\n", s);
        }
    }

    return output;
}

Profile::Profile(const std::vector<std::string>& content) {
    Section currentSection = Section::None;
    bool foundVersion = false;
    bool foundMeta = false;
    bool foundTime = false;
    bool foundCamera = false;

    for (int lineNum = 1; lineNum <= static_cast<int>(content.size()); ++lineNum) {
        std::string line = content[lineNum - 1];
        if (std::all_of(line.begin(), line.end(), ::isspace)) {
            currentSection = Section::None;
            continue;
        }

        if (currentSection != Section::None && line[0] == '#') {
            throw ProfileParsingError(
                lineNum,
                "Sections in profile must be separated by empty lines"
            );
        }

        switch (currentSection) {
            case Section::None:
                currentSection = parseSection(line, lineNum);

                if (!foundVersion && currentSection != Section::Version) {
                    throw ProfileParsingError(
                        lineNum,
                        fmt::format(
                            "First header in the file must be Version, but got {}", line
                        )
                    );
                }

                if (currentSection == Section::Meta && foundMeta) {
                    throw ProfileParsingError(
                        lineNum,
                        "Meta section can only appear once per profile"
                    );
                }
                break;
            case Section::Version:
                if (foundVersion) {
                    throw ProfileParsingError(
                        lineNum,
                        "Version section can only appear once per profile"
                    );
                }

                _version = parseVersion(line, lineNum);
                foundVersion = true;
                break;
            case Section::Meta:
            {
                if (!_meta.has_value()) {
                    _meta = Meta();
                }

                std::pair<MetaLineType, std::string> m = parseMeta(line, lineNum);
                switch (m.first) {
                    case MetaLineType::Name:
                        if (!_meta->name.empty()) {
                            throw ProfileParsingError(
                                lineNum,
                                "Meta information 'Name' specified twice"
                            );
                        }
                        _meta->name = m.second;
                        break;
                    case MetaLineType::Version:
                        if (!_meta->version.empty()) {
                            throw ProfileParsingError(
                                lineNum,
                                "Meta information 'Version' specified twice"
                            );
                        }
                        _meta->version = m.second;
                        break;
                    case MetaLineType::Description:
                        if (!_meta->description.empty()) {
                            throw ProfileParsingError(
                                lineNum,
                                "Meta information 'Description' specified twice"
                            );
                        }
                        _meta->description = m.second;
                        break;
                    case MetaLineType::Author:
                        if (!_meta->author.empty()) {
                            throw ProfileParsingError(
                                lineNum,
                                "Meta information 'Author' specified twice"
                            );
                        }
                        _meta->author = m.second;
                        break;
                    case MetaLineType::URL:
                        if (!_meta->url.empty()) {
                            throw ProfileParsingError(
                                lineNum,
                                "Meta information 'URL' specified twice"
                            );
                        }
                        _meta->url = m.second;
                        break;
                    case MetaLineType::License:
                        if (!_meta->license.empty()) {
                            throw ProfileParsingError(
                                lineNum,
                                "Meta information 'License' specified twice"
                            );
                        }
                        _meta->license = m.second;
                        break;
                    default:
                        throw ghoul::MissingCaseException();
                }
                foundMeta = true;
                break;
            }
            case Section::Module:
            {
                Module m = parseModule(line, lineNum);
                _modules.push_back(std::move(m));
                break;
            }
            case Section::Asset:
            {
                Asset a = parseAsset(line, lineNum);
                _assets.push_back(std::move(a));
                break;
            }
            case Section::Property:
            {
                Property p = parseProperty(line, lineNum);
                _properties.push_back(std::move(p));
                break;
            }
            case Section::Keybinding:
            {
                Keybinding kb = parseKeybinding(line, lineNum);
                _keybindings.push_back(std::move(kb));
                break;
            }
            case Section::Time:
                if (foundTime) {
                    throw ProfileParsingError(
                        lineNum,
                        "Time section can only appear once per profile"
                    );
                }

                _time = parseTime(line, lineNum);
                foundTime = true;
                break;
            case Section::DeltaTimes:
            {
                const double d = parseDeltaTime(line, lineNum);
                _deltaTimes.push_back(d);
                break;
            }
            case Section::Camera:
                if (foundCamera) {
                    throw ProfileParsingError(
                        lineNum,
                        "Camera section can only appear once per profile"
                    );
                }

                _camera = parseCamera(line, lineNum);
                foundCamera = true;
                break;
            case Section::MarkNodes:
            {
                std::string m = parseMarkNodes(line, lineNum);
                _markNodes.push_back(std::move(m));
                break;
            }
            case Section::AdditionalScripts:
            {
                std::string a = parseAdditionalScript(line, lineNum);
                _additionalScripts.push_back(std::move(a));
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
}

std::string Profile::convertToScene() const {
    ZoneScoped

    std::string output;

    if (_meta.has_value()) {
        output += "asset.meta = {";
        if (!_meta->name.empty()) {
            output += fmt::format("  Name = {},", _meta->name);
        }
        if (!_meta->version.empty()) {
            output += fmt::format("  Version = {},", _meta->version);
        }
        if (!_meta->description.empty()) {
            output += fmt::format("  Description = {},", _meta->description);
        }
        if (!_meta->author.empty()) {
            output += fmt::format("  Author = {},", _meta->author);
        }
        if (!_meta->url.empty()) {
            output += fmt::format("  URL = {},", _meta->url);
        }
        if (!_meta->license.empty()) {
            output += fmt::format("  License = {},", _meta->license);
        }

        output += "}";
    }

    // Modules
    for (const Module& m : _modules) {
        output += fmt::format(
            "if openspace.modules.isLoaded(\"{}\") then {} else {} end\n",
            m.name, m.loadedInstruction, m.notLoadedInstruction
        );
    }

    // Assets
    for (const Asset& a : _assets) {
        if (a.name.empty()) {
            output += fmt::format("asset.require(\"{}\");\n", a.path);
        }
        else {
            output += fmt::format("local {} = asset.require(\"{}\");\n", a.name, a.path);
        }
    }

    output += "asset.onInitialize(function()\n";
    // Keybindings
    for (const Keybinding& k : _keybindings) {
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
    switch (_time->type) {
        case Time::Type::Absolute:
            output += fmt::format("openspace.time.setTime(\"{}\")\n", _time->time);
            break;
        case Time::Type::Relative:
            output += "local now = openspace.time.currentWallTime();\n";
            output += fmt::format(
                "local prev = openspace.time.advancedTime(now, \"{}\");\n", _time->time
            );
            output += "openspace.time.setTime(prev);\n";
            break;
        default:
            throw ghoul::MissingCaseException();
    }

    // Delta Times
    {
        std::string times;
        for (const double d : _deltaTimes) {
            times += fmt::format("{} ,", d);
        }
        output += fmt::format("openspace.time.setDeltaTimeSteps({{ {} }});\n", times);
    }

    // Mark Nodes
    {
        std::string nodes;
        for (const std::string& n : _markNodes) {
            nodes += fmt::format("[[{}]],", n);
        }
        output += fmt::format("openspace.markInterestingNodes({{ {} }});\n", nodes);
    }

    // Properties
    for (const Property& p : _properties) {
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
    if (_camera.has_value()) {
        output += std::visit(
            overloaded {
                [](const CameraNavState& camera) {
                    std::string result;
                    result += "openspace.navigation.setNavigationState({";
                    result += fmt::format("Anchor = {}, ", camera.anchor);
                    if (!camera.aim.empty()) {
                        result += fmt::format("Aim = {}, ", camera.aim);
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
            *_camera
        );
    }

    for (const std::string& a : _additionalScripts) {
        output += fmt::format("{}\n", a);
    }

    output += "end)\n";

    return output;
}

Profile::Version Profile::version() const {
    return _version;
}

std::vector<Profile::Module> Profile::modules() const {
    return _modules;
}

std::optional<Profile::Meta> Profile::meta() const {
    return _meta;
}

std::vector<Profile::Asset> Profile::assets() const {
    return _assets;
}

std::vector<Profile::Property> Profile::properties() const {
    return _properties;
}

std::vector<Profile::Keybinding> Profile::keybindings() const {
    return _keybindings;
}

std::optional<Profile::Time> Profile::time() const {
    return _time;
}

std::optional<Profile::CameraType> Profile::camera() const {
    return _camera;
}

std::vector<std::string> Profile::markNodes() const {
    return _markNodes;
}

std::vector<std::string> Profile::additionalScripts() const {
    return _additionalScripts;
}

void Profile::setVersion(Version v) {
    _version = v;
}

void Profile::setModules(std::vector<Module>& m) {
    _modules.clear();
    copy(m.begin(), m.end(), back_inserter(_modules));
}

void Profile::setMeta(Meta m) {
    _meta = m;
}

void Profile::setProperties(std::vector<Property>& p) {
    _properties.clear();
    copy(p.begin(), p.end(), back_inserter(_properties));
}

void Profile::setKeybindings(std::vector<Keybinding>& k) {
    _keybindings.clear();
    copy(k.begin(), k.end(), back_inserter(_keybindings));
}

void Profile::setTime(Time t) {
    _time = t;
}

void Profile::setCamera(CameraType c) {
    _camera = c;
}

void Profile::setMarkNodes(std::vector<std::string>& n) {
    _markNodes.clear();
    copy(n.begin(), n.end(), back_inserter(_markNodes));
}

void Profile::setAdditionalScripts(std::vector<std::string>& s) {
    _additionalScripts.clear();
    copy(s.begin(), s.end(), back_inserter(_additionalScripts));
}

}  // namespace openspace
