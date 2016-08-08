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

#ifndef __RENDERABLETRAIL_H__
#define __RENDERABLETRAIL_H__

#include <openspace/rendering/renderable.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vectorproperty.h>

#include <ghoul/opengl/ghoul_gl.h>

namespace ghoul {
namespace opengl {
    class ProgramObject;
    class Texture;
}
}

namespace openspace {

class RenderableTrail : public Renderable {
public:
    explicit RenderableTrail(const ghoul::Dictionary& dictionary);

    bool initialize() override;
    bool deinitialize() override;

    bool isReady() const override;

    void render(const RenderData& data) override;
    void update(const UpdateData& data) override;

private:
    struct TrailVBOLayout {
        float x, y, z, e;
    };

    void fullYearSweep(double time);
    void sendToGPU();

    properties::Vec3Property _lineColor;
    properties::FloatProperty _lineFade;
    properties::FloatProperty _lineWidth;
    properties::BoolProperty _showTimestamps;

    std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;

    bool _successfullDictionaryFetch;

    std::string _target;
    std::string _observer;
    std::string _frame;

    float _tropic;
    float _ratio;
    float _day;

    GLuint _vaoID;
    GLuint _vBufferID;

    bool _needsSweep;

    std::vector<TrailVBOLayout> _vertexArray;

    float _increment;
    double _oldTime = 0.0;
    float _distanceFade;
};

} // namespace openspace

#endif // __RENDERABLETRAIL_H__
