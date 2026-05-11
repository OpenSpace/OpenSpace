/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/scene/jasset.h>

#include <openspace/json.h>
#include <openspace/util/json_helper.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <ghoul/misc/stringhelper.h>
#include <format>
#include <fstream>
#include <iterator>
#include <ranges>

namespace {
    using namespace openspace;

    struct JAsset {
        std::vector<ghoul::Dictionary> sceneGraphNodes;

        std::vector<std::string> dependencies;
        struct Metadata {
            std::string author;
            std::string description;
            std::string license;
            std::string name;
            std::string version;
        };
        Metadata metadata;
    };

    void from_json(const nlohmann::json& j, JAsset::Metadata& metadata) {
        j["author"].get_to(metadata.author);
        j["description"].get_to(metadata.description);
        j["license"].get_to(metadata.license);
        j["name"].get_to(metadata.name);
        j["asset_version"].get_to(metadata.version);
    }

    void from_json(const nlohmann::json& j, JAsset& jasset) {
        std::vector<nlohmann::json> sceneGraphNodes =
            j["scenegraphnodes"].get<std::vector<nlohmann::json>>();
        for (const nlohmann::json& json : sceneGraphNodes) {
            ghoul::Dictionary d = jsonToDictionary(json);
            d.removeValue("type");
            jasset.sceneGraphNodes.push_back(d);
        }

        j["dependencies"].get_to(jasset.dependencies);
        j["metadata"].get_to(jasset.metadata);
    }

    std::string toLuaScript(const JAsset& jasset) {
        std::vector<std::string> lines;

        for (const std::string& dep : jasset.dependencies) {
            lines.push_back(std::format("asset.require([[{}]])", dep));
        }

        std::vector<std::string> sgns;
        for (size_t i = 0; i < jasset.sceneGraphNodes.size(); i++) {
            const ghoul::Dictionary& d = jasset.sceneGraphNodes[i];
            std::string variable = std::format("sgn{}", i);
            std::string sgn = ghoul::formatLua(d);
            lines.push_back(std::format("local {} = {}", variable, sgn));
            sgns.push_back(variable);
        }

        lines.push_back("asset.onInitialize(function()");
        for (const std::string& sgn : sgns) {
            lines.push_back(std::format("openspace.addSceneGraphNode({})", sgn));
        }
        lines.push_back("end)");

        lines.push_back("asset.onDeinitialize(function()");
        for (const std::string& sgn : sgns | std::views::reverse) {
            lines.push_back(std::format("openspace.removeSceneGraphNode({})", sgn));
        }
        lines.push_back("end)");

        lines.push_back(std::format(
            "asset.meta = {{ Name = [[{}]], Description = [[{}]], Author = [[{}]], "
            "License = [[{}]], Version = [[{}]]}}",
            jasset.metadata.name, jasset.metadata.description, jasset.metadata.author,
            jasset.metadata.license, jasset.metadata.version
        ));

        std::string res = ghoul::join(lines, "\n");
        return res;
    }
} // namespace

namespace openspace {

std::string jassetToLua(const std::filesystem::path& path) {
    ghoul_assert(std::filesystem::exists(path), "Path must exist");

    std::ifstream inFile = std::ifstream(path, std::ifstream::in);
    if (!inFile.good()) {
        throw ghoul::RuntimeError(std::format(
            "Error opening file '{}' path for loading jasset file",
            path
        ));
    }
    const std::string content = std::string(
        std::istreambuf_iterator<char>(inFile),
        std::istreambuf_iterator<char>()
    );

    try {
        nlohmann::json json = nlohmann::json::parse(content);
        JAsset jasset = json.get<JAsset>();
        return toLuaScript(jasset);
    }
    catch (const nlohmann::json::exception& e) {
        std::string err = e.what();
        throw ghoul::RuntimeError(std::move(err), "JAsset");
    }
}

} // namespace openspace
