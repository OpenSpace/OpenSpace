/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_MODULE_BASE___TIMEDEPENDENTSCALE___H__
#define __OPENSPACE_MODULE_BASE___TIMEDEPENDENTSCALE___H__

#include <openspace/scene/scale.h>

#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/stringproperty.h>

namespace openspace {

namespace documentation { struct Documentation; }

class TimeDependentScale : public Scale {
public:
    TimeDependentScale(const ghoul::Dictionary& dictionary);
    glm::dvec3 scaleValue(const UpdateData& data) const override;

    static documentation::Documentation Documentation();

private:
    properties::StringProperty _referenceDate;
    properties::DoubleProperty _speed;
    properties::BoolProperty _clampToPositive;

    mutable bool _cachedReferenceDirty = true;
    mutable double _cachedReference = 0.0; // in seconds past the J2000 epoch
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___TIMEDEPENDENTSCALE___H__
