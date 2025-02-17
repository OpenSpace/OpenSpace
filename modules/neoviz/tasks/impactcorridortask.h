/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_NEOVIZ___IMPACTCORRIDORTASK___H__
#define __OPENSPACE_MODULE_NEOVIZ___IMPACTCORRIDORTASK___H__

#include <openspace/util/task.h>

#include <filesystem>
#include <string>

namespace openspace::neoviz {

class ImpactCorridorTask : public Task {
public:
    explicit ImpactCorridorTask(const ghoul::Dictionary& dictionary);

    std::string description() override;
    void perform(const Task::ProgressCallback& progressCallback) override;

    static documentation::Documentation documentation();

private:
    struct ImpactCoordinate {
        int id = 0;
        std::string time;
        double latitude = 0.0;  // Degrees
        double longitude = 0.0;
    };

    std::filesystem::path _kernelDirectory;
    std::filesystem::path _outputFilename;
    int _impactDistance;
    std::string _timeIntervalStart;
    std::string _timeIntervalEnd;
    std::vector<ImpactCoordinate> _impactCoordinates;
    int _imageWidth;
    int _imageHeight;
};

} // namespace openspace::neoviz

#endif // __OPENSPACE_MODULE_NEOVIZ___IMPACTCORRIDORTASK___H__
