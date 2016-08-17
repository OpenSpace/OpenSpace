/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <modules/base/rotation/spicerotation.h>

#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>

namespace {
    const std::string _loggerCat = "SpiceRotation";
    //const std::string keyGhosting = "EphmerisGhosting";

    const std::string KeySourceFrame = "SourceFrame";
    const std::string KeyDestinationFrame = "DestinationFrame";
    const std::string KeyKernels = "Kernels";
}

namespace openspace {
    
SpiceRotation::SpiceRotation(const ghoul::Dictionary& dictionary)
    : _sourceFrame("")
    , _destinationFrame("")
    , _rotationMatrix(1.0)
    , _kernelsLoadedSuccessfully(true)
{
    const bool hasSourceFrame = dictionary.getValue(KeySourceFrame, _sourceFrame);
    if (!hasSourceFrame)
        LERROR("SpiceRotation does not contain the key '" << KeySourceFrame << "'");

    const bool hasDestinationFrame = dictionary.getValue(KeyDestinationFrame, _destinationFrame);
    if (!hasDestinationFrame)
        LERROR("SpiceRotation does not contain the key '" << KeyDestinationFrame << "'");

    ghoul::Dictionary kernels;
    dictionary.getValue(KeyKernels, kernels);
    for (size_t i = 1; i <= kernels.size(); ++i) {
        std::string kernel;
        bool success = kernels.getValue(std::to_string(i), kernel);
        if (!success)
            LERROR("'" << KeyKernels << "' has to be an array-style table");

        try {
            SpiceManager::ref().loadKernel(kernel);
            _kernelsLoadedSuccessfully = true;
        }
        catch (const SpiceManager::SpiceException& e) {
            LERROR("Could not load SPICE kernel: " << e.what());
            _kernelsLoadedSuccessfully = false;
        }
    }
}
    
const glm::dmat3& SpiceRotation::matrix() const {
    return _rotationMatrix;
}

void SpiceRotation::update(const UpdateData& data) {
    if (!_kernelsLoadedSuccessfully)
        return;

    // TODO : Need to be checking for real if the frame is fixed since fixed frames
    // do not have CK coverage.
    bool sourceFrameIsFixed = _sourceFrame == "GALACTIC";
    bool destinationFrameIsFixed = _destinationFrame == "GALACTIC";

    bool sourceHasCoverage =
        !_sourceFrame.empty() &&
        SpiceManager::ref().hasFrameId(_sourceFrame) &&
        (SpiceManager::ref().hasCkCoverage(_sourceFrame, data.time) ||
            sourceFrameIsFixed);
    bool destinationHasCoverage =
        !_destinationFrame.empty() &&
        SpiceManager::ref().hasFrameId(_destinationFrame) &&
        (SpiceManager::ref().hasCkCoverage(_destinationFrame, data.time) ||
            destinationFrameIsFixed);

    if (sourceHasCoverage && destinationHasCoverage) {
        _rotationMatrix = SpiceManager::ref().positionTransformMatrix(
            _sourceFrame,
            _destinationFrame,
            data.time);
    }
    else {
        _rotationMatrix = glm::dmat3(1.0);
    }
}

} // namespace openspace