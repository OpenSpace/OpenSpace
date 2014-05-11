/**
Copyright (C) 2012-2014 Jonas Strandstedt

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
#ifndef PLANET_H
#define PLANET_H

// open space includes
#include <openspace/util/powerscaledscalar.h>
#include <openspace/util/vbo_template.h>

// std includes
#include <string>

namespace openspace
{

typedef struct
{
	GLfloat location[4];
	GLfloat tex[2];
	GLfloat normal[3];
	GLubyte padding[28]; // Pads the struct out to 64 bytes for performance increase
} Vertex;
	
class Planet
{
public:
	//initializers
	Planet(PowerScaledScalar radius, int levels = 4);
	~Planet();
	
	void setHeightMap(const std::string &path);
	void render();

private:

	VBO<Vertex> *vbo;
};

} // namespace openspace

#endif
