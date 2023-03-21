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

#ifndef __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSDATAPREPARATIONTASK___H__
#define __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSDATAPREPARATIONTASK___H__

#include <openspace/util/task.h>

#include <openspace/properties/vector/vec3property.h>
#include <filesystem>
#include <string>

namespace openspace::exoplanets {

class ExoplanetsDataPreparationTask : public Task {
public:
    ExoplanetsDataPreparationTask(const ghoul::Dictionary& dictionary);
    std::string description() override;
    void perform(const Task::ProgressCallback& progressCallback) override;
    static documentation::Documentation documentation();

private:
    std::filesystem::path _inputDataPath;
    std::filesystem::path _inputSpeckPath;
    std::filesystem::path _outputBinPath;
    std::filesystem::path _outputLutPath;
    std::filesystem::path _teffToBvFilePath;

    glm::vec3 starPosition(const std::string& starName);

    // Compute b-v color from teff value using a conversion file
    float bvFromTeff(float teff);
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSDATAPREPARATIONTASK___H__
