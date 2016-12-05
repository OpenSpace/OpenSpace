/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __RENDERABLEPATH_H__
#define __RENDERABLEPATH_H__

#include <openspace/rendering/renderable.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>

#include <ghoul/opengl/ghoul_gl.h>

namespace ghoul {
    namespace opengl {
        class ProgramObject;
        class Texture;
    }
}

namespace openspace {

class RenderablePath : public Renderable {
public:
    RenderablePath(const ghoul::Dictionary& dictionary);

    bool initialize() override;
    bool deinitialize() override;

    bool isReady() const override;

    void render(const RenderData& data) override;
    void update(const UpdateData& data) override;

    void calculatePath(std::string observer);
private:
    struct VertexInfo {
        float x, y, z, e;
        //float r, g, b, a;
    };
    void sendToGPU();
    void addPosition(glm::vec3 pos);
    void addColor(glm::vec4 col);

    glm::vec3 _lineColor;
    glm::vec4 _lastPosition;
    properties::FloatProperty _lineWidth;
    properties::BoolProperty _drawLine;

    std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;

    bool _successfullDictionaryFetch;

    std::string _target;
    std::string _observer;
    std::string _frame;

    GLuint _vaoID;
    GLuint _vBufferID;
        
    bool _needsSweep;

    std::vector<VertexInfo> _vertexArray;
        
    float _increment;
    double _start;
    double _stop;
    float _distanceFade;
    int _pointSteps;
};

} // namespace openspace

#endif // __RENDERABLEPATH_H__
