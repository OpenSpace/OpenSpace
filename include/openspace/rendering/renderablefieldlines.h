/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#ifndef RENDERABLEFIELDLINES_H_
#define RENDERABLEFIELDLINES_H_

// open space includes
#include <openspace/rendering/renderable.h>

// ghoul includes
#include <ghoul/opengl/programobject.h>

namespace openspace {
	struct LinePoint;

class RenderableFieldlines : public Renderable {
public:
	RenderableFieldlines(const ghoul::Dictionary& dictionary);
	~RenderableFieldlines();

	bool initialize();
	bool deinitialize();

	bool isReady() const override;

	void render(const RenderData& data) override;

private:
	std::vector<std::vector<LinePoint> > getFieldlinesData(std::string filename, ghoul::Dictionary hintsDictionary);

	std::vector<ghoul::Dictionary> _hintsDictionaries;
	std::vector<std::string> _filenames;
	std::vector<glm::vec3> _seedPoints;

	ghoul::opengl::ProgramObject* _shader;
	GLuint _fieldlineVAO;

	std::vector<GLint> _lineStart;
	std::vector<GLsizei> _lineCount;
};

} // namespace openspace
#endif // RENDERABLEFIELDLINES_H_
