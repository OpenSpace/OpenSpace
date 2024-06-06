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

#include <openspace/engine/settings.h>

#include <openspace/engine/configuration.h>
#include <sstream>

namespace openspace {

namespace {
template <typename T>
std::optional<T> get_to(nlohmann::json& obj, const std::string& key)
{
    auto it = obj.find(key);
    if (it != obj.end()) {
        return it->get<T>();
    }
    else {
        return std::nullopt;
    }
}
} // namespace

namespace version1 {
    Settings parseSettings(nlohmann::json json) {
        ghoul_assert(json.at("version").get<int>() == 1, "Wrong value");

        Settings settings;
        settings.hasStartedBefore = get_to<bool>(json, "started-before");
        settings.configuration = get_to<std::string>(json, "config");
        settings.rememberLastConfiguration = get_to<bool>(json, "config-remember");
        settings.profile = get_to<std::string>(json, "profile");
        settings.rememberLastProfile = get_to<bool>(json, "profile-remember");
        std::optional<std::string> visibility = get_to<std::string>(json, "visibility");
        if (visibility.has_value()) {
            if (*visibility == "NoviceUser") {
                settings.visibility = properties::Property::Visibility::NoviceUser;
            }
            else if (*visibility == "User") {
                settings.visibility = properties::Property::Visibility::User;
            }
            else if (*visibility == "AdvancedUser") {
                settings.visibility = properties::Property::Visibility::AdvancedUser;
            }
            else if (*visibility == "Developer") {
                settings.visibility = properties::Property::Visibility::Developer;
            }
            else {
                throw ghoul::RuntimeError(std::format(
                    "Unknown visibility value '{}'", *visibility
                ));
            }
        }
        settings.bypassLauncher = get_to<bool>(json, "bypass");

        std::optional<std::string> layerServer = get_to<std::string>(json, "layerserver");
        if (layerServer.has_value()) {
            settings.layerServer = stringToLayerServer(*layerServer);
        }

        if (auto it = json.find("mrf");  it != json.end()) {
            if (!it->is_object()) {
                throw ghoul::RuntimeError("'mrf' is not an object");
            }
            Settings::MRF mrf;
            mrf.isEnabled = get_to<bool>(*it, "enabled");
            mrf.location = get_to<std::string>(*it, "location");

            if (mrf.isEnabled.has_value() || mrf.location.has_value()) {
                settings.mrf = mrf;
            }
        }

        return settings;
    }
} // namespace version1

std::filesystem::path findSettings(const std::string& filename) {
    // Right now the settings file lives next to the openspace.cfg file

    const std::filesystem::path path = findConfiguration();
    const std::filesystem::path result = path.parent_path() / filename;
    return result;
}

Settings loadSettings(const std::filesystem::path& filename) {
    if (!std::filesystem::is_regular_file(filename)) {
        return Settings();
    }

    std::stringstream buffer;
    {
        const std::ifstream f = std::ifstream(filename);
        buffer << f.rdbuf();
    }
    const std::string contents = buffer.str();

    nlohmann::json setting = nlohmann::json::parse(contents);
    if (setting.empty()) {
        return Settings();
    }

    int version = setting.at("version").get<int>();
    if (version == 1) {
        return version1::parseSettings(setting);
    }

    throw ghoul::RuntimeError(std::format(
        "Unrecognized version for setting: {}", version
    ));
}

void saveSettings(const Settings& settings, const std::filesystem::path& filename) {
    nlohmann::json json = nlohmann::json::object();

    json["version"] = 1;

    if (settings.hasStartedBefore.has_value()) {
        json["started-before"] = *settings.hasStartedBefore;
    }
    if (settings.configuration.has_value()) {
        json["config"] = *settings.configuration;
    }
    if (settings.rememberLastConfiguration.has_value()) {
        json["config-remember"] = *settings.rememberLastConfiguration;
    }
    if (settings.profile.has_value()) {
        json["profile"] = *settings.profile;
    }
    if (settings.rememberLastProfile.has_value()) {
        json["profile-remember"] = *settings.rememberLastProfile;
    }
    if (settings.visibility.has_value()) {
        switch (*settings.visibility) {
            case properties::Property::Visibility::NoviceUser:
                json["visibility"] = "NoviceUser";
                break;
            case properties::Property::Visibility::User:
                json["visibility"] = "User";
                break;
            case properties::Property::Visibility::AdvancedUser:
                json["visibility"] = "AdvancedUser";
                break;
            case properties::Property::Visibility::Developer:
                json["visibility"] = "Developer";
                break;
            default:
                throw ghoul::MissingCaseException();
        }
    }
    if (settings.bypassLauncher.has_value()) {
        json["bypass"] = *settings.bypassLauncher;
    }
    if (settings.layerServer.has_value()) {
        json["layerserver"] = layerServerToString(*settings.layerServer);
    }
    nlohmann::json mrf = nlohmann::json::object();
    if (settings.mrf.isEnabled.has_value()) {
        mrf["enabled"] = *settings.mrf.isEnabled;
    }
    if (settings.mrf.location.has_value()) {
        mrf["location"] = *settings.mrf.location;
    }

    if (!mrf.empty()) {
        json["mrf"] = mrf;
    }

    std::ofstream f = std::ofstream(filename);
    const std::string content = json.dump(2);
    f << content;
}

} // namespace openspace
