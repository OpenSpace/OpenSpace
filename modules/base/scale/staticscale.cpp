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

#include <modules/base/scale/staticscale.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

namespace {
    const char* KeyValue = "Scale";
}

namespace openspace {

documentation::Documentation StaticScale::Documentation() {
    using namespace openspace::documentation;
    return {
        "Static Scaling",
        "base_scale_static",
        {{
            KeyValue,
            new DoubleVerifier,
            "The scaling factor by which the scenegraph node is scaled."
        }}
    };
}

StaticScale::StaticScale()
    : _scaleValue("scale", "Scale", 1.0, 1.0, 1000.0)
{
    addProperty(_scaleValue);
}


StaticScale::StaticScale(const ghoul::Dictionary& dictionary)
    : StaticScale()
{
    documentation::testSpecificationAndThrow(Documentation(), dictionary, "StaticScale");
    
    _scaleValue = static_cast<float>(dictionary.value<double>(KeyValue));
}

double StaticScale::scaleValue() const {
    return _scaleValue;
}

} // namespace openspace
