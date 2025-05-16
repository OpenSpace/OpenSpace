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

#include <modules/softwareintegration/session/session.h>

#include <modules/softwareintegration/softwareintegrationmodule.h>
#include <modules/softwareintegration/utils/syncablestorage.h>
#include <openspace/engine/globals.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/query/query.h>
#include <ghoul/filesystem/filesystem.h>

#include <fstream>
#include <unordered_map>

namespace {

constexpr const char* _loggerCat = "SoftwareIntegrationSession";

}  // namespace

namespace openspace {

// Anonomous namespace
namespace {

const std::string INDENT{"    "};

bool writeToFile(std::filesystem::path path, const std::vector<std::byte>& buffer, std::string& errorMessage) {
    try {
        std::basic_ofstream<std::byte> outFile{path, std::ios::out | std::ios::binary};
        if(!outFile) {
            throw std::ofstream::failure{"Could not open file"};
        }

        const auto bufferSize = static_cast<uint32_t>(buffer.size());
        outFile.write(reinterpret_cast<const std::byte*>(&bufferSize), sizeof(uint32_t));
        outFile.write(buffer.data(), bufferSize);
        outFile.close();
        return true;
    }
    catch (const std::ofstream::failure& e) {
        errorMessage = std::format("Could not write to the file \"{}\". {}.", path.string(), e.what());
        return false;
    }
}

bool readFile(std::filesystem::path path, std::vector<std::byte>& buffer, std::string& errorMessage) {
    try {
        std::basic_ifstream<std::byte> inFile{path, std::ios::out | std::ios::binary};
        if(!inFile) {
            throw std::ifstream::failure{"Could not open file"};
        }

        uint32_t bufferSize;
        inFile.read(reinterpret_cast<std::byte*>(&bufferSize), sizeof(uint32_t));
        buffer.resize(bufferSize);
        inFile.read(buffer.data(), bufferSize);
        inFile.close();
        return true;
    }
    catch (const std::ifstream::failure& e) {
        errorMessage = std::format("Couldn't read the file \"{}\". {}.", path.string(), e.what());
        return false;
    }
}

bool saveSessionData(SyncableStorage& storage,
                     const std::filesystem::path& filePath,
                     std::string& errorMessage)
{
    std::vector<std::byte> byteStream;
    storage.dump(byteStream);

    if (byteStream.size() == 0) {
        errorMessage = "Software Integration Storage is empty.";
        return false;
    }

    if (std::filesystem::exists(filePath)) {
        errorMessage = std::format("The file \"{}\" already exists.", filePath.filename().string());
        return false;
    }

    if (!writeToFile(filePath, byteStream, errorMessage)) {
        return false;
    }

    return true;
}

} // namespace

bool softwareintegration::Session::loadSessionData(SoftwareIntegrationModule* module,
                                  const std::string& filePathString,
                                  std::string& errorMessage
) {
    auto filePath = std::filesystem::path{filePathString};
    if (!std::filesystem::exists(filePath) || !std::filesystem::is_regular_file(filePath)) {
        errorMessage = std::format("File {} doesn't exists...", filePathString);
        LERROR(errorMessage);
        return false;
    }

    std::vector<std::byte> byteStream;

    if (!readFile(filePath, byteStream, errorMessage)) {
        LERROR(errorMessage);
        return false;
    }

    try {
        module->_syncableStorage.store(byteStream);
    }
    catch (const std::exception& e) {
        errorMessage = std::format("Couldn't store loaded data in Software Integration storage", e.what());
        LERROR(errorMessage);
        return false;
    }

    // Set large time steps for the GUI (so you for example 
    // can see the movement of stars at 5000 years/second)
    // Values set in seconds: Real time, 5k years, 
    // 10k year, 50k year, 100k year, 500k year, 1M year
    std::string largeTimeSteps = "{ 1.0, 157680000000.0, 315360000000.0,"
                                    " 1576800000000.0, 3153600000000.0,"
                                    " 15768000000000.0, 3153600000000.0 }";
    global::scriptEngine->queueScript(
        std::format(
            "openspace.time.setDeltaTimeSteps({});",
            largeTimeSteps
        )
    );

    return true;
}

bool softwareintegration::Session::saveSession(const std::string& wantedFileName, std::string& errorMessage) {
    auto softwareIntegrationModule = global::moduleEngine->module<SoftwareIntegrationModule>();
    if (!softwareIntegrationModule) {
        errorMessage = "Software Integration Module not found.";
        return false;
    }

    auto dirPath = absPath("${USER_ASSETS}") / wantedFileName;
    if (std::filesystem::exists(dirPath)) {
        errorMessage = std::format("A saved session with the name \"{}\" already exists.", dirPath.filename().string());
        return false;
    }

    if (!std::filesystem::create_directory(dirPath)) {
        errorMessage = std::format("Could not create the folder \"{}\" in the user assets folder.", dirPath);
        return false;
    }

    auto sessionDataFilePath = dirPath / std::filesystem::path{ dirPath.filename().string() + ".dat" };
    if (
        !saveSessionData(
            softwareIntegrationModule->_syncableStorage,
            sessionDataFilePath,
            errorMessage
        )
    ) {
        return false;
    }

    auto filePath = dirPath / std::filesystem::path{ dirPath.filename().string() + ".asset" };
    std::ofstream assetFile;
    try {
        assetFile.open(filePath);

        assetFile << "local nodes = {\n";

        auto identifiers = softwareIntegrationModule->_syncableStorage.getAllIdentifiers();
        bool isFirstSgn = true;
        for (auto& identifier : identifiers) {
            auto r = renderable(identifier);
            if (r == nullptr) continue;

            if (!isFirstSgn) {
                assetFile << ",\n";
            }
            else {
                isFirstSgn = false;
            }

            auto properties = r->properties();

            assetFile << INDENT << "{\n"
                      << INDENT << INDENT << "GUI = {\n"
                      << INDENT << INDENT << INDENT << "Name = " << r->property("Name")->stringValue() << ",\n"
                      << INDENT << INDENT << INDENT << "Path = \"/Software Integration\"" << "\n"
                      << INDENT << INDENT << "},\n"
                      << INDENT << INDENT << "Identifier = \"" << identifier << "\",\n"
                      << INDENT << INDENT << "Renderable = {\n"
                      << INDENT << INDENT << INDENT << "Identifier = \"" << identifier << "\",\n";

            bool isFirstProp = true;
            for (auto p : properties) {
                if (!p) continue;

                // VOLATILE: This is because option (enum) properties does not play nice when parsed to string
                // Either name them all with a name that includes "Option" or add the name here
                if (p->identifier().find("Option") != std::string::npos) continue;

                if (!isFirstProp) {
                    assetFile << ",\n";
                }
                else {
                    isFirstProp = false;
                }

                assetFile << INDENT << INDENT << INDENT
                          << p->identifier() << " = ";

                std::string valueAsString = p->stringValue();
                
                if (std::string{ p->type().name() }.find("string") == std::string::npos) {
                    for(
                        auto pos = valueAsString.find('[');
                        pos != std::string::npos;
                        pos = valueAsString.find('[', ++pos)
                    ) {
                        valueAsString.replace(pos, 1, "{");
                        valueAsString.insert(++pos, 1, ' ');
                    }
                    
                    for(
                        auto pos = valueAsString.find(']');
                        pos != std::string::npos;
                        pos = valueAsString.find(']', ++pos)
                    ) {
                        valueAsString.replace(pos, 1, "}");
                        valueAsString.insert(pos++, 1, ' ');
                    }
                }

                assetFile << valueAsString;
            }

            assetFile << '\n' << INDENT << INDENT << "}\n"
                      << INDENT << "}";
        }

        assetFile << "\n}\n\n"
                  << "local data = asset.localResource(\"" << sessionDataFilePath.filename().string() << "\")\n"
                  << "asset.onInitialize(function ()\n"
                  << INDENT << "openspace.softwareintegration.loadSessionData(data)\n"
                  << INDENT << "for _, node in ipairs(nodes) do\n"
                  << INDENT << INDENT << "openspace.addSceneGraphNode(node)\n"
                  << INDENT << "end\n"
                  << "end)\n\n"
                  << "asset.onDeinitialize(function ()\n"
                  << INDENT << "for i=1, #nodes do\n"
                  << INDENT << INDENT << "openspace.removeSceneGraphNode(nodes[#nodes + 1 - i].Identifier)\n"
                  << INDENT << "end\n"
                  << "end)\n\n"
                  << "asset.meta = {\n"
                  << INDENT << std::format("Name = \"{} (Software Integration session)\",", wantedFileName)
                  << "\n"
                  << INDENT << "Version = \"1.0\",\n"
                  << INDENT << "Description = [[]],\n"
                  << INDENT << "Author = \"\",\n"
                  << INDENT << "URL = \"\",\n"
                  << INDENT << "License = \"\"\n"
                  << "}\n";

        assetFile.close();
        return true;
    }
    catch (std::ifstream::failure& err) {
        errorMessage = std::format("An error occured when creating the asset file. {}.", err.what());
        return false;
    }
}

}  // namespace openspace
