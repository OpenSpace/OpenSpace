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

#ifndef __OPENSPACE_CORE___LIGHTSOURCE___H__
#define __OPENSPACE_CORE___LIGHTSOURCE___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/properties/scalar/boolproperty.h>
#include <ghoul/glm.h>
#include <memory>

namespace ghoul { class Dictionary; }

namespace openspace {

namespace documentation { struct Documentation; }

class SceneGraphNode;
struct RenderData;

class LightSource : public properties::PropertyOwner {
public:
    static std::unique_ptr<LightSource> createFromDictionary(
        const ghoul::Dictionary& dictionary);

    explicit LightSource(const ghoul::Dictionary& dictionary);
    ~LightSource() override = default;

    virtual glm::vec3 directionViewSpace(const RenderData& renderData) const = 0;

    virtual float intensity() const = 0;

    bool isEnabled() const;

    virtual bool initialize();

    static documentation::Documentation Documentation();

private:
    properties::BoolProperty _enabled;
};

}  // namespace openspace

#endif // __OPENSPACE_CORE___LIGHTSOURCE___H__
