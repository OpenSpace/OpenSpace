/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_CORE___SETTINGS___H__
#define __OPENSPACE_CORE___SETTINGS___H__

#include <openspace/engine/configuration.h>
#include <openspace/properties/property.h>
#include <filesystem>
#include <optional>

namespace openspace {

struct Settings {
    auto operator<=>(const Settings&) const = default;

    // Settings that are not configurable by the user and that represent a persistent
    // state for the system
    std::optional<bool> hasStartedBefore;
    std::optional<std::string> lastStartedDate;

    // Configurable settings
    std::optional<std::string> configuration;
    std::optional<bool> rememberLastConfiguration;
    std::optional<std::string> profile;
    std::optional<bool> rememberLastProfile;
    std::optional<properties::Property::Visibility> visibility;
    std::optional<bool> bypassLauncher;
    std::optional<Configuration::LayerServer> layerServer;

    struct MRF {
        auto operator<=>(const MRF&) const = default;

        std::optional<bool> isEnabled;
        std::optional<std::string> location;
    };
    MRF mrf;
};

std::filesystem::path findSettings(const std::string& filename = "settings.json");

Settings loadSettings(const std::filesystem::path& filename = findSettings());
void saveSettings(const Settings& settings, const std::filesystem::path& filename);

} // namespace openspace

#endif // __OPENSPACE_CORE___SETTINGS___H__
