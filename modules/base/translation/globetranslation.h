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

#ifndef __OPENSPACE_MODULE_BASE___GLOBETRANSLATION___H__
#define __OPENSPACE_MODULE_BASE___GLOBETRANSLATION___H__

#include <openspace/scene/translation.h>

#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>

namespace openspace {

class SceneGraphNode;

class GlobeTranslation : public Translation {
public:
    explicit GlobeTranslation(const ghoul::Dictionary& dictionary);

    void update(const UpdateData& data) override;
    glm::dvec3 position(const UpdateData& data) const override;

    static documentation::Documentation Documentation();

private:
    void fillAttachedNode();
    void setUpdateVariables();

    properties::StringProperty _sceneGraphNode;
    properties::DoubleProperty _latitude;
    properties::DoubleProperty _longitude;
    properties::DoubleProperty _altitude;
    properties::BoolProperty _useHeightmap;
    properties::BoolProperty _useCamera;
    properties::BoolProperty _useCameraAltitude;

    SceneGraphNode* _attachedNode = nullptr;

    mutable bool _positionIsDirty = true;
    mutable glm::dvec3 _position = glm::dvec3(0.0);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___GLOBETRANSLATION___H__
