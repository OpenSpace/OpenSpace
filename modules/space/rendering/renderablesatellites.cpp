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

#include <modules/space/rendering/renderablesatellites.h>

#include <modules/space/translation/keplertranslation.h>
#include <modules/space/kepler.h>
#include <modules/space/spacemodule.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/engine/globals.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/misc/csvreader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/logging/logmanager.h>
#include <chrono>
#include <math.h>
#include <filesystem>
#include <fstream>
#include <vector>

namespace {
    constexpr const char* _loggerCat = "Satellites";
}

namespace openspace {

RenderableSatellites::RenderableSatellites(const ghoul::Dictionary& dictionary)
    : RenderableOrbitalKepler(dictionary)
{
    addProperty(_startRenderIdx);
    addProperty(_sizeRender);

    _startRenderIdx.onChange([this]() {
        if ((_numObjects - _startRenderIdx) < _sizeRender) {
            _sizeRender = static_cast<unsigned int>(_numObjects - _startRenderIdx);
        }
        updateBuffers();
    });
    _sizeRender.onChange([this]() {
        if (_sizeRender > (_numObjects - _startRenderIdx)) {
            _startRenderIdx = static_cast<unsigned int>(_numObjects - _sizeRender);
        }
        updateBuffers();
    });
}

void RenderableSatellites::loadData(std::vector<kepler::SatelliteParameters> data) {
    _data.clear();
    _segmentSize.clear();

    _numObjects = data.size();

    if (!_isFileReadinitialized) {
        _isFileReadinitialized = true;
        _startRenderIdx.setMaxValue(static_cast<unsigned int>(_numObjects - 1));
        _sizeRender.setMaxValue(static_cast<unsigned int>(_numObjects));
        if (_sizeRender == 0u) {
            _sizeRender = static_cast<unsigned int>(_numObjects);
        }
    }

    std::string name;
    long long endElement = _startRenderIdx + _sizeRender - 1;
    endElement = (endElement >= _numObjects) ? _numObjects - 1 : endElement;

    if (_startRenderIdx < 0 || _startRenderIdx >= _numObjects) {
        throw ghoul::RuntimeError(fmt::format(
            "Start index {} out of range [0, {}]", _startRenderIdx, _numObjects
        ));
    }
    if (endElement < 0 || endElement >= _numObjects) {
        throw ghoul::RuntimeError(fmt::format(
            "End index {} out of range [0, {}]", endElement, _numObjects
        ));
    }

    // Extract subset that starts at _startRenderIdx and contains _sizeRender obejcts
    data = std::vector<kepler::SatelliteParameters>(
        data.begin() + _startRenderIdx,
        data.begin() + _startRenderIdx + _sizeRender
    );

    _segmentSize = std::vector<size_t>(data.size(), _segmentQuality * 16);
    _data = std::move(data);
}

} // namespace openspace
