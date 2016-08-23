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

#ifndef __RENDERABLETRAILNEW_H__
#define __RENDERABLETRAILNEW_H__

#include <openspace/rendering/renderable.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vectorproperty.h>

#include <openspace/util/timerange.h>

#include <ghoul/opengl/ghoul_gl.h>

namespace ghoul {
namespace opengl {
    class ProgramObject;
    class Texture;
}
}

namespace openspace {

/**
    * This class currently has a temporary name until it is merged
    * with or replaces RenderableTrail.
*/
class RenderableTrailNew : public Renderable {
public:
    explicit RenderableTrailNew(const ghoul::Dictionary& dictionary);

    bool initialize() override;
    bool deinitialize() override;

    bool isReady() const override;

    void render(const RenderData& data) override;
    void update(const UpdateData& data) override;

private:
    void sweepTimeRange();

    void initializeGlobalOpenGLPathData();    
    void initializeLocalOpenGLPathData();

    void deInitializeGlobalOpenGLPathData();
    void deInitializeLocalOpenGLPathData();

    void preRender(int totalNumVerticesToDraw);
    void preRenderSubPathGlobally(const RenderData& renderData);
    void preRenderSubPathLocally(const RenderData& renderData, int totalNumVerticesToDraw);

    void renderLines(GLuint vao, int numberOfVertices);
    void renderPoints(GLuint vao, int numberOfVertices);
    
    // Spice
    std::string _body;
    std::string _observer;
    std::string _frame;

    // Properties
    properties::Vec3Property    _lineColor;
    properties::Vec3Property    _pointColor;
    properties::FloatProperty   _lineFade;
    properties::FloatProperty   _lineWidth;
    properties::FloatProperty   _renderPart;
    properties::BoolProperty    _showTimeStamps;
    properties::BoolProperty    _renderFullTrail;

    // OpenGL
    GLuint _vaoGlobalID;
    GLuint _vBufferGlobalID;

    GLuint _vaoLocalID;
    GLuint _vBufferLocalID;

    // Other
    bool _successfullDictionaryFetch;
    TimeRange _timeRange;
    double _sampleDeltaTime;
    int _subSamples;
    std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;
    std::vector<glm::vec3> _vertexPositionArray;
    
    double _currentTimeClamped; // Time clamped to time range
    glm::dmat4 _modelTransform;
    glm::dvec3 _clampedBodyPosition; // Position of body clamped to time range
};

} // namespace openspace

#endif // __RENDERABLETRAILNEW_H__
