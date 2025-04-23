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

#ifndef __OPENSPACE_MODULE_BASE___GLOBEROTATION___H__
#define __OPENSPACE_MODULE_BASE___GLOBEROTATION___H__

#include <openspace/scene/rotation.h>

#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>

namespace openspace {

class SceneGraphNode;

class GlobeRotation : public Rotation {
public:
    explicit GlobeRotation(const ghoul::Dictionary& dictionary);

    void update(const UpdateData& data) override;
    glm::dmat3 matrix(const UpdateData& data) const override;

    static documentation::Documentation Documentation();

private:
    void findNode();
    void setUpdateVariables();

    /**
     * Calculates the position on the surface of the node based on the provided latitude
     * and longitude (in degrees). Returns the position on the surface of the node
     * corresponding to the provided latitude and longitude.
     */
    glm::vec3 computeSurfacePosition(double latitude, double longitude) const;

    properties::StringProperty _sceneGraphNode;
    properties::DoubleProperty _latitude;
    properties::DoubleProperty _longitude;
    properties::DoubleProperty _angle;
    properties::BoolProperty _useHeightmap;
    properties::BoolProperty _useCamera;

    SceneGraphNode* _attachedNode = nullptr;

    mutable bool _matrixIsDirty = true;
    mutable glm::dmat3 _matrix = glm::dmat3(0.0);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___GLOBEROTATION___H__
