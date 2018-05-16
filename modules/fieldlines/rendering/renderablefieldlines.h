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

#ifndef __OPENSPACE_MODULE_FIELDLINES___RENDERABLEFIELDLINES___H__
#define __OPENSPACE_MODULE_FIELDLINES___RENDERABLEFIELDLINES___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec4property.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/ghoul_gl.h>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

struct LinePoint;

class RenderableFieldlines : public Renderable {
public:
    RenderableFieldlines(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

private:
    typedef std::vector<LinePoint> Line;

    void initializeDefaultPropertyValues();
    std::vector<Line> getFieldlinesData();
    void loadSeedPoints();
    void loadSeedPointsFromFile();
    void loadSeedPointsFromTable();

    std::vector<Line> generateFieldlines();
    std::vector<Line> generateFieldlinesVolumeKameleon();

    properties::FloatProperty _stepSize;
    properties::BoolProperty _classification;
    properties::Vec4Property _fieldlineColor;
    properties::OptionProperty _seedPointSource;
    properties::StringProperty _seedPointSourceFile;

    std::unique_ptr<ghoul::opengl::ProgramObject> _program;

    ghoul::Dictionary _vectorFieldInfo;
    ghoul::Dictionary _fieldlineInfo;
    ghoul::Dictionary _seedPointsInfo;

    bool _seedPointsAreDirty = true;
    bool _fieldLinesAreDirty = true;

    std::vector<glm::vec3> _seedPoints;

    GLuint _fieldlineVAO = 0;
    GLuint _vertexPositionBuffer = 0;

    std::vector<GLint> _lineStart;
    std::vector<GLsizei> _lineCount;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINES___RENDERABLEFIELDLINES___H__
