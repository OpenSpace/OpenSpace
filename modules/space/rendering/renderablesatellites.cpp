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
#include <modules/space/translation/tletranslation.h>
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

    static const openspace::properties::Property::PropertyInfo SegmentsInfo = {
        "Segments",
        "Segments",
        "The number of segments to use for each orbit ellipse"
    };

    struct [[codegen::Dictionary(RenderableSatellites)]] Parameters {
        // [[codegen::verbatim(SegmentsInfo.description)]]
        double segments;
    };
#include "renderablesatellites_codegen.cpp"
}

namespace openspace {

documentation::Documentation RenderableSatellites::Documentation() {
    return codegen::doc<Parameters>(
        "space_renderablesatellites",
        RenderableOrbitalKepler::Documentation()
    );
}

RenderableSatellites::RenderableSatellites(const ghoul::Dictionary& dictionary)
    : RenderableOrbitalKepler(dictionary)
{
    // Commented out right now as its not super clear how it works with inheritance. We'd
    // probably want a codegen::check function that only does the checking without
    // actually creating a Parameter objects
    // codegen::bake<Parameters>(dictionary);
    addProperty(_startRenderIdx);
    addProperty(_sizeRender);

    _updateStartRenderIdxSelect = [this]() {
        if ((_numObjects - _startRenderIdx) < _sizeRender) {
            _sizeRender = static_cast<unsigned int>(_numObjects - _startRenderIdx);
        }
        updateBuffers();
    };
    _updateRenderSizeSelect = [this]() {
        if (_sizeRender > (_numObjects - _startRenderIdx)) {
            _startRenderIdx = static_cast<unsigned int>(_numObjects - _sizeRender);
        }
        updateBuffers();
    };
    _startRenderIdxCallbackHandle = _startRenderIdx.onChange(_updateStartRenderIdxSelect);
    _sizeRenderCallbackHandle = _sizeRender.onChange(_updateRenderSizeSelect);
}

void RenderableSatellites::readDataFile(const std::string& filename) {
    if (!std::filesystem::is_regular_file(filename)) {
        throw ghoul::RuntimeError(fmt::format(
            "Satellite TLE file {} does not exist", filename
        ));
    }
    _data.clear();
    _segmentSize.clear();

    std::vector<SatelliteKeplerParameters> parameters = readTleFile(filename);
    _numObjects = parameters.size();

    if (!_isFileReadinitialized) {
        _isFileReadinitialized = true;
        initializeFileReading();
    }

    std::string line = "-";
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

    for (size_t i = _startRenderIdx; i <= endElement; i++) {
        SatelliteKeplerParameters param = parameters[i];

        KeplerParameters e;
        e.inclination = param.inclination;
        e.semiMajorAxis = param.semiMajorAxis;
        e.ascendingNode = param.ascendingNode;
        e.eccentricity = param.eccentricity;
        e.argumentOfPeriapsis = param.argumentOfPeriapsis;
        e.meanAnomaly = param.meanAnomaly;
        e.epoch = param.epoch;
        e.period = param.period;

        _data.push_back(e);
        _segmentSize.push_back(_segmentQuality * 16);
    }
}

void RenderableSatellites::initializeFileReading() {
    _startRenderIdx.setMaxValue(static_cast<unsigned int>(_numObjects - 1));
    _sizeRender.setMaxValue(static_cast<unsigned int>(_numObjects));
    if (_sizeRender == 0u) {
        _sizeRender = static_cast<unsigned int>(_numObjects);
    }
}

} // namespace openspace
