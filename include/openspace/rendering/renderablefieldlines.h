/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#ifndef __RENDERABLEFIELDLINES_H__
#define __RENDERABLEFIELDLINES_H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalarproperty.h>
#include <openspace/properties/vectorproperty.h>

#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/ghoul_gl.h>

namespace ghoul {
namespace opengl {
	class ProgramObject;
}
}

namespace openspace {
	struct LinePoint;

class RenderableFieldlines : public Renderable {
public:
	RenderableFieldlines(const ghoul::Dictionary& dictionary);

	bool initialize() override;
	bool deinitialize() override;

	bool isReady() const override;

	void render(const RenderData& data) override;
	void update(const UpdateData& data) override;

private:
	typedef std::vector<LinePoint> Line;

	void initializeDefaultPropertyValues();
	//std::vector<std::vector<LinePoint> > getFieldlinesData(std::string filename, ghoul::Dictionary hintsDictionary);
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

	ghoul::opengl::ProgramObject* _program;
	bool _programIsDirty;

	ghoul::Dictionary _vectorFieldInfo;
	ghoul::Dictionary _fieldlineInfo;
	ghoul::Dictionary _seedPointsInfo;

	bool _seedPointsAreDirty;
	bool _fieldLinesAreDirty;

	std::vector<glm::vec3> _seedPoints;

	GLuint _fieldlineVAO;
	GLuint _vertexPositionBuffer;

	std::vector<GLint> _lineStart;
	std::vector<GLsizei> _lineCount;
};

} // namespace openspace

#endif // __RENDERABLEFIELDLINES_H__
