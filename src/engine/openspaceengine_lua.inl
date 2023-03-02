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

#include <openspace/openspace.h>
#include <ghoul/misc/csvreader.h>

namespace {

/**
 * Toggles the shutdown mode that will close the application after the countdown timer
 * is reached
 */
[[codegen::luawrap]] void toggleShutdown() {
    openspace::global::openSpaceEngine->toggleShutdownMode();
}

/**
 * Writes out documentation files
 */
[[codegen::luawrap]] void writeDocumentation() {
    openspace::global::openSpaceEngine->writeDocumentation();
}

// Sets the folder used for storing screenshots or session recording frames
[[codegen::luawrap]] void setScreenshotFolder(std::string newFolder) {
    using namespace openspace;

    std::filesystem::path folder = absPath(newFolder);
    if (!std::filesystem::exists(folder)) {
        std::filesystem::create_directory(folder);
    }

    FileSys.registerPathToken(
        "${SCREENSHOTS}",
        folder,
        ghoul::filesystem::FileSystem::Override::Yes
    );

    global::windowDelegate->setScreenshotFolder(folder.string());
}

// Adds a Tag to a SceneGraphNode identified by the provided uri
[[codegen::luawrap]] void addTag(std::string uri, std::string tag) {
    using namespace openspace;

    SceneGraphNode* node = global::renderEngine->scene()->sceneGraphNode(uri);
    if (!node) {
        throw ghoul::lua::LuaError(fmt::format("Unknown scene graph node '{}'", uri));
    }

    node->addTag(std::move(tag));
}

// Removes a tag (second argument) from a scene graph node (first argument)
[[codegen::luawrap]] void removeTag(std::string uri, std::string tag) {
    using namespace openspace;

    SceneGraphNode* node = global::renderEngine->scene()->sceneGraphNode(uri);
    if (!node) {
        throw ghoul::lua::LuaError(fmt::format("Unknown scene graph node '{}'", uri));
    }

    node->removeTag(tag);
}

// Downloads a file from Lua interpreter
[[codegen::luawrap]] void downloadFile(std::string url, std::string savePath,
                                       bool waitForCompletion = false)
{
    using namespace openspace;

    LINFOC("OpenSpaceEngine", fmt::format("Downloading file from {}", url));
    std::shared_ptr<DownloadManager::FileFuture> future =
        global::downloadManager->downloadFile(
            url,
            savePath,
            DownloadManager::OverrideFile::Yes,
            DownloadManager::FailOnError::Yes,
            5
        );

    if (waitForCompletion) {
        while (!future->isFinished && future->errorMessage.empty()) {
            // just wait
            LTRACEC("OpenSpaceEngine", fmt::format("waiting {}", future->errorMessage));
        }
    }
}

} // namespace

// Closing the anoynmous namespace here to allow a unit test to access this function

/**
 * Creates a 1 pixel image with a certain color in the cache folder and returns the path
 * to the file. If a cached file with the given name already exists, the path to that file
 * is returned. The first argument is the name of the file, without extension. The second
 * is the RGB color, given as {r, g, b} with values between 0 and 1.
 */
[[codegen::luawrap]] std::filesystem::path createSingleColorImage(std::string name,
                                                                  glm::dvec3 color)
{
    using namespace openspace;

    // @TODO (emmbr 2020-12-18) Verify that the input dictionary is a vec3
    // Would like to clean this up with a more direct use of the Verifier in the future
    const std::string& key = "color";
    ghoul::Dictionary colorDict;
    colorDict.setValue(key, color);
    documentation::TestResult res = documentation::Color3Verifier()(colorDict, key);

    if (!res.success) {
        throw ghoul::lua::LuaError(
            "Invalid color. Expected three double values {r, g, b} in range 0 to 1"
        );
    }

    std::filesystem::path fileName = FileSys.cacheManager()->cachedFilename(
        name + ".ppm",
        ""
    );
    const bool hasCachedFile = std::filesystem::is_regular_file(fileName);
    if (hasCachedFile) {
        LDEBUGC("OpenSpaceEngine", fmt::format("Cached file '{}' used", fileName));
        return fileName;
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

        if (!ppmFile.is_open()) {
            throw ghoul::lua::LuaError("Could not open ppm file for writing");
        }

        ppmFile << "P6" << std::endl;
        ppmFile << width << " " << height << std::endl;
        ppmFile << 255 << std::endl;
        ppmFile.write(reinterpret_cast<char*>(img.data()), size * 3);
        ppmFile.close();
        return fileName;
    }
}

/**
 * Returns whether the current OpenSpace instance is the master node of a cluster
 * configuration. If this instance is not part of a cluster, this function also returns
 * 'true'.
 */
[[codegen::luawrap]] bool isMaster() {
    return openspace::global::windowDelegate->isMaster();
}

/**
 * This function returns information about the current OpenSpace version. The resulting
 * table has the structure:
 * \code
 * Version = {
 *   Major = <number>
 *   Minor = <number>
 *   Patch = <number>
 * },
 * Commit = <string>
 * Branch = <string>
 * \endcode
 */
[[codegen::luawrap]] ghoul::Dictionary version() {
    ghoul::Dictionary res;

    ghoul::Dictionary version;
    version.setValue("Major", openspace::OPENSPACE_VERSION_MAJOR);
    version.setValue("Minor", openspace::OPENSPACE_VERSION_MINOR);
    version.setValue("Patch", openspace::OPENSPACE_VERSION_PATCH);
    res.setValue("Version", std::move(version));

    res.setValue("Commit", std::string(openspace::OPENSPACE_GIT_COMMIT));
    res.setValue("Branch", std::string(openspace::OPENSPACE_GIT_BRANCH));

    return res;
}

/**
 * Loads the CSV file provided as a parameter and returns it as a vector containing the
 * values of the each row. The inner vector has the same number of values as the CSV has
 * columns. The second parameter controls whether the first entry in the returned outer
 * vector is containing the names of the columns
 */
[[codegen::luawrap]] std::vector<std::vector<std::string>> readCSVFile(
                                                               std::filesystem::path file,
                                                            bool includeFirstLine = false)
{
    if (!std::filesystem::exists(file) || !std::filesystem::is_regular_file(file)) {
        throw ghoul::lua::LuaError(fmt::format("Could not find file {}", file));
    }

    std::vector<std::vector<std::string>> res =
        ghoul::loadCSVFile(file.string(), includeFirstLine);
    return res;
}

#include "openspaceengine_lua_codegen.cpp"
