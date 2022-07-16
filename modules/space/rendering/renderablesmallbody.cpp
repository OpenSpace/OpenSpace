/****************************************************************************************
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

#include <modules/space/rendering/renderablesmallbody.h>

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
#include <filesystem>
#include <fstream>
#include <math.h>
#include <random>
#include <vector>

namespace {
    constexpr const char* _loggerCat = "SmallSolarSystemBody";

    static const openspace::properties::Property::PropertyInfo ContiguousModeInfo = {
        "ContiguousMode",
        "Contiguous Mode",
        "If enabled, then the contiguous set of objects starting from StartRenderIdx "
        "of size RenderSize will be rendered. If disabled, then the number of objects "
        "defined by UpperLimit will rendered from an evenly dispersed sample of the "
        "full length of the data file."
    };
    static const openspace::properties::Property::PropertyInfo UpperLimitInfo = {
        "UpperLimit",
        "Upper Limit",
        "Upper limit on the number of objects for this renderable, regardless of "
        "how many objects are contained in the data file. Produces an evenly-distributed"
        "sample from the data file."
    };

    struct [[codegen::Dictionary(RenderableSmallBody)]] Parameters {
        // [[codegen::verbatim(ContiguousModeInfo.description)]]
        std::optional<bool> contiguousMode;

        // [[codegen::verbatim(UpperLimitInfo.description)]]
        std::optional<int> upperLimit;
    };
#include "renderablesmallbody_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableSmallBody::Documentation() {
    return codegen::doc<Parameters>(
        "space_renderablesmallbody",
        RenderableOrbitalKepler::Documentation()
    );
}

RenderableSmallBody::RenderableSmallBody(const ghoul::Dictionary& dictionary)
    : RenderableOrbitalKepler(dictionary)
    , _contiguousMode(ContiguousModeInfo, false)
    , _upperLimit(UpperLimitInfo, 1000, 1, 1000000)
{
    codegen::bake<Parameters>(dictionary);

    addProperty(_startRenderIdx);
    addProperty(_sizeRender);
    addProperty(_contiguousMode);
    addProperty(_upperLimit);

    if (dictionary.hasValue<double>(UpperLimitInfo.identifier)) {
        _upperLimit = static_cast<unsigned int>(
            dictionary.value<double>(UpperLimitInfo.identifier));
    }
    else {
        _upperLimit = 0u;
    }

    if (dictionary.hasValue<bool>(ContiguousModeInfo.identifier)) {
        _contiguousMode = dictionary.value<bool>(ContiguousModeInfo.identifier);
    }
    else {
        _contiguousMode = false;
    }

    _updateStartRenderIdxSelect = std::function<void()>([this] {
        if (_contiguousMode) {
            if ((_numObjects - _startRenderIdx) < _sizeRender) {
                _sizeRender = static_cast<unsigned int>(_numObjects - _startRenderIdx);
            }
            _updateDataBuffersAtNextRender = true;
        }
    });
    _updateRenderSizeSelect = std::function<void()>([this] {
        if (_contiguousMode) {
            if (_sizeRender > (_numObjects - _startRenderIdx)) {
                _startRenderIdx = static_cast<unsigned int>(_numObjects - _sizeRender);
            }
            _updateDataBuffersAtNextRender = true;
        }
    });
    _updateRenderUpperLimitSelect = std::function<void()>([this] {
        if (!_contiguousMode) {
            _updateDataBuffersAtNextRender = true;
        }
    });
    _updateContiguousModeSelect = std::function<void()>([this] {
        _updateDataBuffersAtNextRender = true;
    });

    _startRenderIdxCallbackHandle = _startRenderIdx.onChange(_updateStartRenderIdxSelect);
    _sizeRenderCallbackHandle = _sizeRender.onChange(_updateRenderSizeSelect);
    _upperLimitCallbackHandle = _upperLimit.onChange(_updateRenderUpperLimitSelect);
    _contiguousModeCallbackhandle =
        _contiguousMode.onChange(_updateContiguousModeSelect);
}

void RenderableSmallBody::loadData(std::vector<kepler::SatelliteParameters> data) {
    _numObjects = data.size();

    _startRenderIdx.setMaxValue(static_cast<unsigned int>(_numObjects - 1));
    _sizeRender.setMaxValue(static_cast<unsigned int>(_numObjects));
    if (_sizeRender == 0u) {
        _sizeRender = static_cast<unsigned int>(_numObjects);
    }

    _upperLimit.setMaxValue(static_cast<unsigned int>(_numObjects));
    if (_upperLimit == 0u) {
        _upperLimit = static_cast<unsigned int>(_numObjects);
    }

    if (_contiguousMode) {
        if (_startRenderIdx >= data.size() ||
            (_startRenderIdx + _sizeRender) >= data.size())
        {
            throw ghoul::RuntimeError(fmt::format(
                "Tried to load {} objects but only {} are available",
                _startRenderIdx + _sizeRender, data.size()
            ));
        }

        // Extract subset that starts at _startRenderIdx and contains _sizeRender obejcts
        data = std::vector<kepler::SatelliteParameters>(
            data.begin() + _startRenderIdx,
            data.begin() + _startRenderIdx + _sizeRender
        );
    }
    else {
        // First shuffle the whole array
        std::default_random_engine rng;
        std::shuffle(data.begin(), data.end(), rng);

        // Then take the first _sizeRender values
        data = std::vector<kepler::SatelliteParameters>(
            data.begin(), data.begin() + _sizeRender
        );
    }

    _segmentSize.clear();
    for (const kepler::SatelliteParameters& p : data) {
        const double scale = static_cast<double>(_segmentQuality) * 10.0;
        _segmentSize.push_back(
            static_cast<size_t>(scale + (scale / pow(1 - p.eccentricity, 1.2)))
        );
    }

    _data = std::move(data);
}

} // namespace openspace
