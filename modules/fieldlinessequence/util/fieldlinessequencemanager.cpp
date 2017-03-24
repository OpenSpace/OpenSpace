/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/fieldlinessequence/util/fieldlinessequencemanager.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
// #include <ghoul/misc/assert.h>

#include <fstream>
#include <algorithm>
#include <memory>

#include <ccmc/Kameleon.h>

#include <modules/fieldlinessequence/util/fieldlinesstate.h>

//#include <glm>
// #include <ccmc/Tracer.h>
// #include "ccmc/Point3f.h"

namespace {
    const std::string _loggerCat = "FieldlinesSequenceManager";
    using RawPath = ghoul::filesystem::FileSystem::RawPath;
    using FileSystem = ghoul::filesystem::FileSystem;
    using Sort = ghoul::filesystem::Directory::Sort;
}

namespace openspace {

FieldlinesSequenceManager::FieldlinesSequenceManager() /*: properties::PropertyOwner("FieldlinesSequenceManager")*/ {}

FieldlinesSequenceManager::~FieldlinesSequenceManager() {}

bool FieldlinesSequenceManager::getSeedPointsFromFile(
        const std::string& path, std::vector<glm::vec3>& outVec) {

    std::string seedPointsSourceFile;
    seedPointsSourceFile = absPath(path);

    if ( !FileSys.fileExists(seedPointsSourceFile, RawPath::Yes) ) {
        LERROR("The file '" << seedPointsSourceFile << "' could not be found!");
        return false;
    }

    std::ifstream seedFile(FileSys.relativePath(seedPointsSourceFile));
    if (!seedFile.good()) {
        LERROR("Could not open seed points file '" << seedPointsSourceFile << "'");
        return false;
    }

    LINFO("Reading seed points from file '" << seedPointsSourceFile << "'");
    std::string line;
    glm::vec3 point;
    while (std::getline(seedFile, line)) {
        std::stringstream s(line);
        s >> point.x;
        s >> point.y;
        s >> point.z;
        outVec.push_back(std::move(point));
    }
    return true;
}

bool FieldlinesSequenceManager::getCdfFilePaths(
        const std::string& pathToCdfDirectory,
        std::vector<std::string>& outCdfFilePaths) {

    std::string absFolderPath;
    absFolderPath = absPath(pathToCdfDirectory);

    if ( !FileSys.directoryExists(absFolderPath) ) {
        LERROR("The folder '" << absFolderPath << "' could not be found!");
        return false;
    }

    // Get absolute path to
    ghoul::filesystem::Directory cdfDirectory(absFolderPath, RawPath::Yes);
    outCdfFilePaths = cdfDirectory.read(FileSystem::Recursive::Yes, Sort::No);

    outCdfFilePaths.erase(std::remove_if(
            outCdfFilePaths.begin(), outCdfFilePaths.end(), [](std::string s) {
                    std::string sub = s.substr(s.length()-4, 4);
                    std::transform(sub.begin(), sub.end(), sub.begin(),
                       [](unsigned char c) { return std::tolower(c); });
                    return sub != ".cdf";
                }), outCdfFilePaths.end());

    return true;
}

bool FieldlinesSequenceManager::traceFieldlinesState(
        const std::string& pathToCdfFile,
        const std::string& tracingVariable,
        const std::vector<glm::vec3>& inSeedPoints,
        FieldlinesState& outFieldlinesStates) {

    std::unique_ptr<ccmc::Kameleon> kameleon = std::make_unique<ccmc::Kameleon>();
    long status = kameleon->open(pathToCdfFile);
    if (status == ccmc::FileReader::OK) {
        LDEBUG("Successfully created a Kameleon Object from file: " << pathToCdfFile);
        kameleon->loadVariable(tracingVariable);
    //     bool isEnlil = kameleon->getModelName() == "enlil"; // TODO, specify in Lua and confirm
        ccmc::Tracer tracer(kameleon.get());
    //     tracer.setMaxIterations(5000); // TODO specify in Lua
    //     tracer.setInnerBoundary(.1f); // TODO specify in Lua
    //     std::vector<Line> fieldlines;
        int lineStart = 0;
        int lineCount = 0;
        for (glm::vec3 seedPoint : inSeedPoints) {
            // A ccmc::Fieldline contains much more info than we need here, but might be
            // needed in future.
            ccmc::Fieldline ccmcFieldline = tracer.bidirectionalTrace(tracingVariable, seedPoint.x, seedPoint.y, seedPoint.z); //TODO convert positions to glm::vec3
            lineCount = ccmcFieldline.size();
            outFieldlinesStates._lineStart.push_back(lineStart);
            outFieldlinesStates._lineStart.push_back(lineCount);
            outFieldlinesStates.reserveSize(lineCount);

            // TODO clean this ugly $*$& up
            for (int i = 0; i < lineCount; ++i) {
                const ccmc::Point3f* vP = &ccmcFieldline.getPositions()[i];
                outFieldlinesStates._vertexPositions.push_back(glm::vec3(vP->component1,vP->component2,vP->component3));
            }

            lineStart += lineCount; // for next iteration (line)
        }
    }

    return true;
}

} // namsepace openspace
