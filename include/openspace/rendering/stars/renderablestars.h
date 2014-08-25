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

#ifndef __RENDERABLESTARS_H__
#define __RENDERABLESTARS_H__

// open space includes
#include <openspace/rendering/renderable.h>
#include <openspace/properties/stringproperty.h>
// ghoul includes
#include <ghoul/opengl/programobject.h>

namespace openspace{

class RenderableStars : public Renderable{
public: 
	RenderableStars(const ghoul::Dictionary& dictionary);
	~RenderableStars();

	bool initialize()   override;
	bool deinitialize() override;

	void render(const Camera* camera, const psc& position) override;
	void update() override;


private:
	std::ifstream& skipToLine(std::ifstream& file, unsigned int num);
	bool readSpeckFile(const std::string& path);

	ghoul::opengl::ProgramObject* _programObject;
	std::string _speckPath;

	GLuint _vboID;
	GLuint _vaoID;
	//GLint positionAttrib;
	int v_size;
};

} // namespace openspace

#endif  // __RENDERABLESTARS_H__