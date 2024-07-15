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

namespace {

// Adds a IswaCygnet.
[[codegen::luawrap]] void addCygnet(int id = -1, std::string type = "Texture",
                                    std::string group = "")
{
    openspace::IswaManager::ref().addIswaCygnet(id, type, group);
}

// Adds a Screen Space Cygnets.
[[codegen::luawrap]] void addScreenSpaceCygnet(ghoul::Dictionary d) {
    using namespace openspace;

    int id = static_cast<int>(d.value<double>("CygnetId"));
    std::map<int, std::shared_ptr<CygnetInfo>> cygnetInformation =
        IswaManager::ref().cygnetInformation();
    if (cygnetInformation.find(id) == cygnetInformation.end()) {
        throw ghoul::lua::LuaError(
            "Could not find Cygnet with id = " + std::to_string(id)
        );
    }

    std::shared_ptr<CygnetInfo> info = cygnetInformation[id];
    std::string name = info->name;
    int updateInterval = info->updateInterval;
    info->selected = true;

    if (global::renderEngine->screenSpaceRenderable(name)) {
        throw ghoul::lua::LuaError(std::format(
            "A cygnet with the name '{}' already exist", name
        ));
    }
    else {
        d.setValue("Name", name);
        d.setValue("Type", std::string("ScreenSpaceCygnet"));
        d.setValue("UpdateInterval", static_cast<double>(updateInterval));

        std::unique_ptr<ScreenSpaceRenderable> s(
            ScreenSpaceRenderable::createFromDictionary(d)
        );
        global::renderEngine->addScreenSpaceRenderable(std::move(s));
    }
}

// Remove a Cygnets.
[[codegen::luawrap]] void removeCygnet(std::string name) {
    using namespace openspace;
    global::scriptEngine->queueScript(
        "openspace.removeSceneGraphNode('" + name + "')",
        scripting::ScriptEngine::ShouldBeSynchronized::Yes,
        scripting::ScriptEngine::ShouldSendToRemote::Yes
    );
}

// Remove a Screen Space Cygnets.
[[codegen::luawrap]] void removeScreenSpaceCygnet(int id) {
    using namespace openspace;

    std::map<int, std::shared_ptr<CygnetInfo>> cygnetInformation =
        IswaManager::ref().cygnetInformation();
    if (cygnetInformation.find(id) == cygnetInformation.end()) {
        throw ghoul::lua::LuaError(
            "Could not find Cygnet with id = " + std::to_string(id)
        );
    }

    std::shared_ptr<CygnetInfo> info = cygnetInformation[id];
    info->selected = false;

    std::string script = std::format(
        "openspace.unregisterScreenSpaceRenderable('{}');", cygnetInformation[id]->name
    );

    global::scriptEngine->queueScript(
        script,
        scripting::ScriptEngine::ShouldBeSynchronized::Yes,
        scripting::ScriptEngine::ShouldSendToRemote::Yes
    );
}

// Remove a group of Cygnets.
[[codegen::luawrap]] void removeGroup(std::string name) {
    using namespace openspace;

    std::map<std::string, std::shared_ptr<IswaBaseGroup>> groups =
        IswaManager::ref().groups();
    if (groups.find(name) != groups.end()) {
        groups[name]->clearGroup();
    }
}

// Adds a cdf files to choose from.
[[codegen::luawrap]] void addCdfFiles(std::string path) {
    openspace::IswaManager::ref().addCdfFiles(path);
}

// Adds KameleonPlanes from cdf file.
[[codegen::luawrap]] void addKameleonPlanes(std::string group, int pos) {
    openspace::IswaManager::ref().addKameleonCdf(group, pos);
}

// Sets the base url.
[[codegen::luawrap]] void setBaseUrl(std::string url) {
    openspace::IswaManager::ref().setBaseUrl(url);
}

#include "iswamanager_lua_codegen.cpp"

} // namespace
