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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLETUBE___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLETUBE___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <ghoul/glm.h>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

namespace documentation { struct Documentation; }

class RenderableTube : public Renderable {
public:
    RenderableTube(const ghoul::Dictionary& dictionary);

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    struct Coordinate {
        double x;
        double y;
        double z;
    };

    struct TimePolygon {
        double timestamp;
        std::vector<Coordinate> points;
    };

    void readDataFile();
    TimePolygon readDataLine(std::ifstream& file);

    void updateTubeData();
    void updateBufferData();

    // Properties
    properties::FloatProperty _lineWidth;
    properties::Vec3Property _lineColor;

    UniformCache(modelViewProjection, color) _uniformCache;

    std::filesystem::path _dataFile;
    std::vector<TimePolygon> _data;
    std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
    GLuint _vaoId = 0;
    GLuint _vboId = 0;
    GLuint _iboId = 0;
    std::vector<float> _vertexArray;
    std::vector<uint8_t> _indexArray;

    bool _prismIsDirty = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLETUBE___H__
