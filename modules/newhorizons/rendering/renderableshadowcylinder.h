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

#ifndef __OPENSPACE_MODULE_NEWHORIZONS___RENDERABLESHADOWCYLINDER___H__
#define __OPENSPACE_MODULE_NEWHORIZONS___RENDERABLESHADOWCYLINDER___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec4property.h>

#include <openspace/util/spicemanager.h>

#include <ghoul/opengl/ghoul_gl.h>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

struct RenderData;
struct UpdateData;

class RenderableShadowCylinder : public Renderable {
public:
    RenderableShadowCylinder(const ghoul::Dictionary& dictionary);

    bool initialize() override;
    bool deinitialize() override;

    bool isReady() const override;
    void render(const RenderData& data) override;
    void update(const UpdateData& data) override;

private:
    struct CylinderVBOLayout {
        float x, y, z, e;
    };

    void createCylinder(double time);
    properties::IntProperty _numberOfPoints;
    properties::FloatProperty _shadowLength;
    properties::Vec4Property _shadowColor;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
    
    glm::dmat3 _stateMatrix;

    GLuint _vao;
    GLuint _vbo;

    std::vector<CylinderVBOLayout> _vertices;

    std::string _terminatorType;
    std::string _lightSource;
    std::string _observer;
    std::string _body;
    std::string _bodyFrame;
    std::string _mainFrame;
    SpiceManager::AberrationCorrection _aberration;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_NEWHORIZONS___RENDERABLESHADOWCYLINDER___H__
