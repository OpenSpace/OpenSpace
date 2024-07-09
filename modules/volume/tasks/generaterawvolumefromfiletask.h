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

#ifndef __OPENSPACE_MODULE_VOLUME___GENERATERAWVOLUMEFROMFILETASK___H__
#define __OPENSPACE_MODULE_VOLUME___GENERATERAWVOLUMEFROMFILETASK___H__

#include <openspace/util/task.h>

#include <ghoul/glm.h>
#include <filesystem>
#include <string>

namespace openspace::volume {

class GenerateRawVolumeFromFileTask : public Task {
public:
    GenerateRawVolumeFromFileTask(const ghoul::Dictionary& dictionary);
    std::string description() override;
    void perform(const Task::ProgressCallback& progressCallback) override;
    static documentation::Documentation Documentation();

private:
    std::filesystem::path _inputFilePath;
    std::filesystem::path _rawVolumeOutputPath;
    std::filesystem::path _dictionaryOutputPath;
    std::string _time;
    std::string _dataValue;

    glm::uvec3 _dimensions = glm::uvec3(0);
    glm::vec3 _lowerDomainBound = glm::vec3(0.f);
    glm::vec3 _upperDomainBound = glm::vec3(0.f);
};

} // namespace openspace::volume

#endif // __OPENSPACE_MODULE_VOLUME___GENERATERAWVOLUMEFROMFILETASK___H__
