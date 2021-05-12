/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/downloadmanager.h>
#include <openspace/engine/globals.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <filesystem>

namespace openspace::luascriptfunctions {

/**
 * \ingroup LuaScripts
 * toggleShutdown():
 * Toggles the shutdown mode that will close the application after the countdown timer is
 * reached
 */
int toggleShutdown(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::toggleShutdown");

    global::openSpaceEngine->toggleShutdownMode();

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
* \ingroup LuaScripts
* writeDocumentation():
* Writes out documentation files
*/
int writeDocumentation(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::writeDocumentation");

    global::openSpaceEngine->writeStaticDocumentation();
    global::openSpaceEngine->writeSceneDocumentation();

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
 * \ingroup LuaScripts
 * addVirtualProperty():
 * Adds a virtual property that will set a group of properties
 */
int addVirtualProperty(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 5, 7 }, "lua::addVirtualProperty");

    const std::string& type = ghoul::lua::value<std::string>(L, 1);
    const std::string& name = ghoul::lua::value<std::string>(L, 2);
    const std::string& identifier = ghoul::lua::value<std::string>(L, 3);
    const std::string& description = ghoul::lua::value<std::string>(L, 4);

    std::unique_ptr<properties::Property> prop;
    if (type == "BoolProperty") {
        const bool v = ghoul::lua::value<bool>(L, 5);
        prop = std::make_unique<properties::BoolProperty>(
            properties::Property::PropertyInfo {
                identifier.c_str(),
                name.c_str(),
                description.c_str()
            },
            v
        );
    }
    else if (type == "IntProperty") {
        const int v = ghoul::lua::value<int>(L, 5);
        const int min = ghoul::lua::value<int>(L, 6);
        const int max = ghoul::lua::value<int>(L, 7);

        prop = std::make_unique<properties::IntProperty>(
            properties::Property::PropertyInfo {
                identifier.c_str(),
                name.c_str(),
                description.c_str()
            },
            v,
            min,
            max
        );
    }
    else if (type == "FloatProperty") {
        const float v = ghoul::lua::value<float>(L, 5);
        const float min = ghoul::lua::value<float>(L, 6);
        const float max = ghoul::lua::value<float>(L, 7);

        prop = std::make_unique<properties::FloatProperty>(
            properties::Property::PropertyInfo {
                identifier.c_str(),
                name.c_str(),
                description.c_str()
            },
            v,
            min,
            max
        );
    }
    else if (type == "TriggerProperty") {
        prop = std::make_unique<properties::TriggerProperty>(
            properties::Property::PropertyInfo {
                identifier.c_str(),
                name.c_str(),
                description.c_str()
            }
        );
    }
    else {
        lua_settop(L, 0);
        return ghoul::lua::luaError(L, fmt::format("Unknown property type '{}'", type));
    }

    lua_settop(L, 0);
    global::virtualPropertyManager->addProperty(std::move(prop));

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
* \ingroup LuaScripts
* removeVirtualProperty():
* Removes a previously added virtual property
*/
int removeVirtualProperty(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::removeVirtualProperty");

    const std::string& name = ghoul::lua::value<std::string>(L, 1);
    properties::Property* p = global::virtualPropertyManager->property(name);
    if (p) {
        global::virtualPropertyManager->removeProperty(p);
    }
    else {
        LWARNINGC(
            "removeVirtualProperty",
            fmt::format("Virtual Property with name '{}'' did not exist", name)
        );
    }

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
* \ingroup LuaScripts
* removeAllVirtualProperties():
* Remove all registered virtual properties
*/
int removeAllVirtualProperties(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::removeAllVirtualProperties");

    const std::vector<properties::Property*>& ps =
        global::virtualPropertyManager->properties();
    for (properties::Property* p : ps) {
        global::virtualPropertyManager->removeProperty(p);
        delete p;
    }

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int setScreenshotFolder(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::setScreenshotFolder");

    std::string arg = ghoul::lua::value<std::string>(L);
    lua_pop(L, 0);

    std::string folder = FileSys.absolutePath(arg);
    if (!std::filesystem::exists(folder)) {
        std::filesystem::create_directory(folder);
    }

    FileSys.registerPathToken(
        "${SCREENSHOTS}",
        folder,
        ghoul::filesystem::FileSystem::Override::Yes
    );

    global::windowDelegate->setScreenshotFolder(folder);
    return 0;
}

/**
 * \ingroup LuaScripts
 * addTag()
 * Adds a Tag to a SceneGraphNode
 */
int addTag(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::addTag");

    const std::string& uri = ghoul::lua::value<std::string>(L, 1);
    std::string tag = ghoul::lua::value<std::string>(L, 2);
    lua_settop(L, 0);

    SceneGraphNode* node = global::renderEngine->scene()->sceneGraphNode(uri);
    if (!node) {
        return ghoul::lua::luaError(
            L,
            fmt::format("Unknown scene graph node type '{}'", uri)
        );
    }

    node->addTag(std::move(tag));

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
 * \ingroup LuaScripts
 * removeTag():
 * Removes a tag from a SceneGraphNode
 */
int removeTag(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::addTag");

    const std::string& uri = ghoul::lua::value<std::string>(L, 1);
    const std::string& tag = ghoul::lua::value<std::string>(L, 2);
    lua_settop(L, 0);

    SceneGraphNode* node = global::renderEngine->scene()->sceneGraphNode(uri);
    if (!node) {
        return ghoul::lua::luaError(
            L,
            fmt::format("Unknown scene graph node type '{}'", uri)
        );
    }

    node->removeTag(tag);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
* \ingroup LuaScripts
* downloadFile():
* Downloads a file from Lua interpreter
*/
int downloadFile(lua_State* L) {
    int n = ghoul::lua::checkArgumentsAndThrow(L, {2, 3}, "lua::addTag");

    const std::string& uri = ghoul::lua::value<std::string>(L, 1);
    const std::string& savePath = ghoul::lua::value<std::string>(L, 2);
    bool waitForComplete = false;
    if (n == 3) {
        waitForComplete = ghoul::lua::value<bool>(L, 3);
    }
    lua_settop(L, 0);

    LINFOC("OpenSpaceEngine", fmt::format("Downloading file from {}", uri));
    std::shared_ptr<DownloadManager::FileFuture> future =
        global::downloadManager->downloadFile(
            uri,
            savePath,
            DownloadManager::OverrideFile::Yes,
            DownloadManager::FailOnError::Yes,
            5
        );

    if (waitForComplete) {
        while (!future->isFinished && future->errorMessage.empty() ) {
            //just wait
            LTRACEC("OpenSpaceEngine", fmt::format("waiting {}", future->errorMessage));
        }
    }

    if (!future || !future->isFinished) {
        return ghoul::lua::luaError(
            L,
            future ? "Download failed: " + future->errorMessage : "Download failed"
        );
    }

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
* \ingroup LuaScripts
* createSingleColorImage():
* Creates a one pixel image with a given color and returns the path to the cached file
*/
int createSingleColorImage(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::createSingleColorImage");

    const std::string& name = ghoul::lua::value<std::string>(L, 1);
    const ghoul::Dictionary& d = ghoul::lua::value<ghoul::Dictionary>(L, 2);
    lua_settop(L, 0);

    // @TODO (emmbr 2020-12-18) Verify that the input dictionary is a vec3
    // Would like to clean this up with a more direct use of the Verifier in the future
    const std::string& key = "color";
    ghoul::Dictionary colorDict;
    colorDict.setValue(key, d);
    documentation::TestResult res = documentation::Color3Verifier()(colorDict, key);

    if (!res.success) {
        return ghoul::lua::luaError(
            L,
            "Invalid color. Expected three double values {r, g, b} in range 0 to 1"
        );
    }

    const glm::dvec3 color = colorDict.value<glm::dvec3>(key);

    const std::string& fileName = FileSys.cacheManager()->cachedFilename(
        fmt::format("{}.ppm", name),
        "",
        ghoul::filesystem::CacheManager::Persistent::Yes
    );

    const bool hasCachedFile = FileSys.fileExists(fileName);
    if (hasCachedFile) {
        LDEBUGC("OpenSpaceEngine", fmt::format("Cached file '{}' used", fileName));
        ghoul::lua::push(L, fileName);
        return 1;
    }
    else {
        // Write the color to a ppm file
        static std::mutex fileMutex;
        std::lock_guard guard(fileMutex);
        std::ofstream ppmFile(fileName, std::ofstream::binary | std::ofstream::trunc);

        unsigned int width = 1;
        unsigned int height = 1;
        unsigned int size = width * height;
        std::vector<unsigned char> img(size * 3);
        img[0] = static_cast<unsigned char>(255 * color.r);
        img[1] = static_cast<unsigned char>(255 * color.g);
        img[2] = static_cast<unsigned char>(255 * color.b);

        if (ppmFile.is_open()) {
            ppmFile << "P6" << std::endl;
            ppmFile << width << " " << height << std::endl;
            ppmFile << 255 << std::endl;
            ppmFile.write(reinterpret_cast<char*>(&img[0]), size * 3);
            ppmFile.close();
            ghoul::lua::push(L, fileName);
            return 1;
        }
        else {
            return ghoul::lua::luaError(L, "Could not open ppm file for writing.");
        }
    }
}

int isMaster(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::isMaster");
    ghoul::lua::push(L, global::windowDelegate->isMaster());
    return 1;
}

} // namespace openspace::luascriptfunctions
