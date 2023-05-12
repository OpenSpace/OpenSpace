/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

SceneLicenseWriter::SceneLicenseWriter() {}

nlohmann::json SceneLicenseWriter::generateJsonGroupedByLicense() const {
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
        metaJson["name"] = "Profile";
        metaJson["profileName"] = global::profile->meta->name.value_or("");
        metaJson["version"] = global::profile->meta->version.value_or("");
        metaJson["description"] = global::profile->meta->description.value_or("");
        metaJson["author"] = global::profile->meta->author.value_or("");
        metaJson["url"] = global::profile->meta->url.value_or("");
        metaJson["license"] = global::profile->meta->license.value_or("");
        metaJson["type"] = "license";
        json.push_back(std::move(metaJson));
    }

    std::map<std::string, nlohmann::json> assetLicenses;
    for (const Asset* asset : assets) {
        std::optional<Asset::MetaInformation> meta = asset->metaInformation();

        nlohmann::json assetJson;
        if (!meta.has_value()) {
            assetJson["name"] = "";
            assetJson["version"] = "";
            assetJson["description"] = "";
            assetJson["author"] = "";
            assetJson["url"] = "";
            assetJson["license"] = "No license";
            assetJson["identifiers"] = "";
            assetJson["path"] = asset->path().string();

            assetLicenses["noLicense"].push_back(assetJson);
        }
        else {
            std::string license = meta->license == "" ? "No License" : meta->license;
            assetJson["name"] = meta->name;
            assetJson["version"] = meta->version;
            assetJson["description"] = meta->description;
            assetJson["author"] = meta->author;
            assetJson["url"] = meta->url;
            assetJson["license"] = license;
            assetJson["identifiers"] = meta->identifiers;
            assetJson["path"] = asset->path().string();

            assetLicenses[license].push_back(assetJson);
        }
    }
    
    nlohmann::json assetsJson;
    assetsJson["name"] = "Assets";
    assetsJson["type"] = "Licenses";

    for (const std::pair<std::string, nlohmann::json>& assetLicense : assetLicenses) {
        nlohmann::json entry;
        entry["name"] = assetLicense.first;
        entry["assets"] = assetLicense.second;
        sortJson(entry["assets"], "name");
        assetsJson["licenses"].push_back(entry);
    }
    json.push_back(assetsJson);

    nlohmann::json result;
    result["name"] = "Licenses";
    result["data"] = json;
    
    return result;
}

nlohmann::json SceneLicenseWriter::generateJsonList() const {
    nlohmann::json json;

    if (global::profile->meta.has_value()) {
        nlohmann::json profile;
        profile["name"] = global::profile->meta->name.value_or("");
        profile["version"] = global::profile->meta->version.value_or("");
        profile["description"] = global::profile->meta->description.value_or("");
        profile["author"] = global::profile->meta->author.value_or("");
        profile["url"] = global::profile->meta->url.value_or("");
        profile["license"] = global::profile->meta->license.value_or("");
        json.push_back(profile);
    }

    std::vector<const Asset*> assets =
        global::openSpaceEngine->assetManager().allAssets();

    for (const Asset* asset : assets) {
        std::optional<Asset::MetaInformation> meta = asset->metaInformation();

        if (!meta.has_value()) {
            continue;
        }

        nlohmann::json assetJson;
        assetJson["name"] = meta->name;
        assetJson["version"] = meta->version;
        assetJson["description"] = meta->description;
        assetJson["author"] = meta->author;
        assetJson["url"] = meta->url;
        assetJson["license"] = meta->license;
        assetJson["identifiers"] = meta->identifiers;
        assetJson["path"] = asset->path().string();

        json.push_back(assetJson);
    }
    return json;
}
} // namespace openspace
