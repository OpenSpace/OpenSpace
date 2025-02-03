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

#ifndef __OPENSPACE_CORE___FADEABLE___H__
#define __OPENSPACE_CORE___FADEABLE___H__

#include <openspace/properties/scalar/floatproperty.h>

namespace openspace {

/**
 * This class is an interface for all things fadeable in the software; things that need
 * a fade and opacity property, which will be combined into a final opacity value.
 *
 * A Fadeable can also be dependent on the fade value from a specified parent fadeable,
 * so that it fades out together with the parent
 */
class Fadeable {
public:
    Fadeable();
    virtual ~Fadeable() = default;

    void setFade(float fade);
    void setParentFadeable(Fadeable* parent);

    float fade() const;
    virtual bool isVisible() const;

    /// Returns the full opacity constructed from the _opacity and _fade property values
    virtual float opacity() const noexcept;

protected:
    properties::FloatProperty _opacity;
    properties::FloatProperty _fade;

    Fadeable* _parentFadeable = nullptr;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___FADEABLE___H__
