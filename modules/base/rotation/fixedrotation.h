/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/vector/vec3property.h>

namespace openspace {

class SceneGraphNode;

class FixedRotation : public Rotation {
public:
    explicit FixedRotation(const ghoul::Dictionary& dictionary);

    void initialize() override;

    void update(const UpdateData& data) override;
    glm::dmat3 matrix(const UpdateData& data) const override;

    static openspace::Documentation Documentation();

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

        OptionProperty type;
        StringProperty object;
        BoolProperty invertObject;
        Vec3Property vector;
        BoolProperty isOrthogonal;

        SceneGraphNode* node;
    };

    BoolProperty _enabled;

    Axis _xAxis;
    Axis _yAxis;
    Axis _zAxis;

    StringProperty _attachedObject;
    SceneGraphNode* _attachedNode = nullptr;

    ghoul::Dictionary _constructorDictionary;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___FIXEDROTATION___H__
