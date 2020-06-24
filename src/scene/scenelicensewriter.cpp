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

#include <openspace/scene/scenelicensewriter.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/scene/asset.h>
#include <openspace/scene/assetmanager.h>

#include <ghoul/fmt.h>
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

std::string SceneLicenseWriter::generateJson() const {
    std::stringstream json;
    json << "[";

    std::vector<const Asset*> assets =
        global::openSpaceEngine.assetManager().rootAsset().subTreeAssets();

    int metaTotal = 0;
    for (const Asset* asset : assets) {
        std::optional<Asset::MetaInformation> meta = asset->metaInformation();
        if (!meta.has_value()) {
            continue;
        }
        metaTotal++;
    }

    int metaCount = 0;
    for (const Asset* asset : assets) {
        std::optional<Asset::MetaInformation> meta = asset->metaInformation();
        
        if (!meta.has_value()) {
            continue;
        }

        constexpr const char* replStr = R"("{}": "{}", )";
        constexpr const char* replStr2 = R"("{}": "{}")";
        json << "{";
        //json << fmt::format(replStr, "module", escapedJson(license.module));
        json << fmt::format(replStr, "name", escapedJson(meta->name));
        json << fmt::format(replStr, "version", escapedJson(meta->version));
        json << fmt::format(replStr, "description", escapedJson(meta->description));
        //json << fmt::format(replStr, "attribution", escapedJson(license.attribution));
        json << fmt::format(replStr, "author", escapedJson(meta->author));
        json << fmt::format(replStr, "url", escapedJson(meta->url));
        //json << fmt::format(replStr2, "licenseText", escapedJson(license.licenseText));
        json << fmt::format(replStr, "license", escapedJson(meta->license));
        json << fmt::format(replStr2, "path", escapedJson(asset->assetFilePath()));
        json << "}";

        metaCount++;
        if (metaCount != metaTotal) {
            json << ",";
        }
    }

    json << "]";

    std::string jsonString;
    for (const char& c : json.str()) {
        if (c == '\'') {
            jsonString += "\\'";
        }
        else {
            jsonString += c;
        }
    }

    return jsonString;
}

} // namespace openspace
