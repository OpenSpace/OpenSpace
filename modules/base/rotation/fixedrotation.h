/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_BASE___FIXEDROTATION___H__
#define __OPENSPACE_MODULE_BASE___FIXEDROTATION___H__

#include <openspace/scene/rotation.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/vector/vec3property.h>

#include <ghoul/glm.h>

namespace openspace {

class SceneGraphNode;

namespace documentation { struct Documentation; }

class FixedRotation : public Rotation {
public:
    FixedRotation(const ghoul::Dictionary& dictionary);

    bool initialize() override;

    static documentation::Documentation Documentation();

    glm::dmat3 matrix(const UpdateData& data) const override;

private:
    glm::vec3 xAxis() const;
    glm::vec3 yAxis() const;
    glm::vec3 zAxis() const;

    struct Axis {
        enum Type {
            Unspecified = -1,
            Object,
            Vector,
            OrthogonalVector,
            CoordinateSystemCompletion
        };

        properties::OptionProperty type;
        properties::StringProperty object;
        properties::BoolProperty invertObject;
        properties::Vec3Property vector;
        properties::BoolProperty isOrthogonal;

        SceneGraphNode* node;
    };

    properties::BoolProperty _enabled;

    Axis _xAxis;
    Axis _yAxis;
    Axis _zAxis;

    properties::StringProperty _attachedObject;
    SceneGraphNode* _attachedNode = nullptr;

    ghoul::Dictionary _constructorDictionary;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___FIXEDROTATION___H__
