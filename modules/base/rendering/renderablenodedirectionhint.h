/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLENODEDIRECTIONHINT___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLENODEDIRECTIONHINT___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/uintproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/glm.h>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

namespace documentation { struct Documentation; }
class Translation;

/**
 * Generates an arrow shape that points from the start node to the
 * end node
 */
class RenderableNodeDirectionHint : public Renderable {
public:
    RenderableNodeDirectionHint(const ghoul::Dictionary& dictionary);
    ~RenderableNodeDirectionHint() override = default;

    static documentation::Documentation Documentation();

private:
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;
    void updateShapeTransforms(const RenderData& data);
    void render(const RenderData& data, RendererTasks& rendererTask) override;

    ghoul::opengl::ProgramObject* _shaderProgram;

    properties::StringProperty _start;
    properties::StringProperty _end;
    properties::Vec3Property _color;

    properties::UIntProperty _segments;
    properties::BoolProperty _invertArrowDirection;

    struct Shading : properties::PropertyOwner {
        Shading();
        properties::BoolProperty enabled;
        properties::FloatProperty ambientIntensity;
        properties::FloatProperty diffuseIntensity;
        properties::FloatProperty specularIntensity;
    };
    Shading _shading;

    properties::FloatProperty _arrowHeadSize;
    properties::FloatProperty _arrowHeadWidthFactor;

    properties::FloatProperty _offsetDistance;
    properties::BoolProperty _useRelativeOffset;
    properties::FloatProperty _length;
    properties::BoolProperty _useRelativeLength;
    properties::FloatProperty _width;

    glm::dmat4 _cylinderTranslation = glm::dmat4(1.0);
    glm::dmat4 _cylinderScale = glm::dmat4(1.0);

    glm::dmat4 _coneTranslation = glm::dmat4(1.0);
    glm::dmat4 _coneScale = glm::dmat4(1.0);

    glm::dmat4 _pointDirectionRotation = glm::dmat4(1.0);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLENODEDIRECTIONHINT___H__
