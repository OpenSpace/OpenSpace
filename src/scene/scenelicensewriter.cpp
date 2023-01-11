/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <openspace/scene/scenelicensewriter.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/scene/asset.h>
#include <openspace/scene/assetmanager.h>
#include <openspace/scene/profile.h>
#include <openspace/util/json_helper.h>
#include <ghoul/fmt.h>
#include <ghoul/misc/profiling.h>
#include <sstream>

namespace openspace {

SceneLicenseWriter::SceneLicenseWriter()
    : DocumentationGenerator(
        "Scene Licenses",
        "sceneLicense",
        {
            { "sceneLicenseTemplate",  "${WEB}/documentation/scenelicense.hbs" }
        }
    )
{}

nlohmann::json SceneLicenseWriter::generateJsonJson() const {
    nlohmann::json json;

    std::vector<const Asset*> assets =
        global::openSpaceEngine->assetManager().allAssets();

    int metaTotal = 0;
    int metaCount = 0;
    for (const Asset* asset : assets) {
        std::optional<Asset::MetaInformation> meta = asset->metaInformation();
        if (!meta.has_value()) {
            continue;
        }
        metaTotal++;
    }

    if (global::profile->meta.has_value()) {
        metaTotal++;
        nlohmann::json metaJson;
        metaJson["Name"] = "OpenSpace";
        metaJson["Profile Name"] = global::profile->meta->name.value_or("");
        metaJson["Version"] = global::profile->meta->version.value_or("");
        metaJson["Description"] = global::profile->meta->description.value_or("");
        metaJson["Author"] = global::profile->meta->author.value_or("");
        metaJson["Url"] = global::profile->meta->url.value_or("");
        metaJson["License"] = global::profile->meta->license.value_or("");
        json.push_back(metaJson);
    }

    std::map<std::string, nlohmann::json> licenses;

    for (const Asset* asset : assets) {
        std::optional<Asset::MetaInformation> meta = asset->metaInformation();

        if (!meta.has_value()) {
            continue;
        }

        constexpr std::string_view replStr = R"("{}": "{}", )";
        constexpr std::string_view replStr2 = R"("{}": "{}")";
        nlohmann::json assetJson;

        assetJson["name"] = meta->name;
        assetJson["version"] = meta->version;
        assetJson["description"] = meta->description;
        assetJson["author"] = meta->author;
        assetJson["url"] = meta->url;
        assetJson["license"] = meta->license;
        assetJson["identifiers"] = meta->identifiers;
        assetJson["path"] = asset->path().string();

        licenses[meta->license].push_back(assetJson);
        licenses["name"] = "assetLicenses";
    }
    json.push_back(licenses);
 
    return json;
}

std::string SceneLicenseWriter::generateJson() const {
    ZoneScoped

    std::stringstream json;
    json << "[";

    std::vector<const Asset*> assets =
        global::openSpaceEngine->assetManager().allAssets();

    int metaTotal = 0;
    int metaCount = 0;
    for (const Asset* asset : assets) {
        std::optional<Asset::MetaInformation> meta = asset->metaInformation();
        if (!meta.has_value()) {
            continue;
        }
        metaTotal++;
    }

    if (global::profile->meta.has_value()) {
        metaTotal++;
        constexpr std::string_view replStr = R"("{}": "{}", )";
        constexpr std::string_view replStr2 = R"("{}": "{}")";
        json << "{";
        json << fmt::format(
            replStr,
            "name", escapedJson(global::profile->meta->name.value_or(""))
        );
        json << fmt::format(
            replStr,
            "version", escapedJson(global::profile->meta->version.value_or(""))
        );
        json << fmt::format(
            replStr,
            "description", escapedJson(global::profile->meta->description.value_or(""))
        );
        json << fmt::format(
            replStr,
            "author", escapedJson(global::profile->meta->author.value_or(""))
        );
        json << fmt::format(
            replStr,
            "url", escapedJson(global::profile->meta->url.value_or(""))
        );
        json << fmt::format(
            replStr2,
            "license", escapedJson(global::profile->meta->license.value_or(""))
        );
        json << "}";

        if (++metaCount != metaTotal) {
            json << ",";
        }
    }

    for (const Asset* asset : assets) {
        std::optional<Asset::MetaInformation> meta = asset->metaInformation();

        if (!meta.has_value()) {
            continue;
        }

        constexpr std::string_view replStr = R"("{}": "{}", )";
        constexpr std::string_view replStr2 = R"("{}": "{}")";
        json << "{";
        json << fmt::format(replStr, "name", escapedJson(meta->name));
        json << fmt::format(replStr, "version", escapedJson(meta->version));
        json << fmt::format(replStr, "description", escapedJson(meta->description));
        json << fmt::format(replStr, "author", escapedJson(meta->author));
        json << fmt::format(replStr, "url", escapedJson(meta->url));
        json << fmt::format(replStr, "license", escapedJson(meta->license));
        json << fmt::format(replStr, "identifiers", escapedJson(meta->identifiers));
        json << fmt::format(replStr2, "path", escapedJson(asset->path().string()));
        json << "}";

        metaCount++;
        if (metaCount != metaTotal) {
            json << ",";
        }
    }

    json << "]";
    return json.str();
}

} // namespace openspace
