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

#ifndef __WAVEFRONTOBJECT_H__
#define __WAVEFRONTOBJECT_H__

#include <openspace/rendering/model/modelgeometry.h>
#include <tiny_obj_loader.h>

namespace openspace {

class RenderableModel;

namespace modelgeometry {

class WavefrontGeometry : public ModelGeometry {
public:
	WavefrontGeometry(const ghoul::Dictionary& dictionary);
	~WavefrontGeometry();

    bool initialize(RenderableModel* parent) override;
    void deinitialize() override;
    void render() override;
	
	typedef struct
	{
		GLfloat location[4];
		GLfloat tex[2];
		GLfloat normal[3];
		GLfloat padding[7];
		//GLubyte padding[4]; // Pads the struct out to 64 bytes for performance increase
	} Vertex;
    
	std::vector<tinyobj::shape_t> shapes;
	std::vector<tinyobj::material_t> materials;
protected:
	bool loadObj(const std::string& filename);
    bool loadCachedFile(const std::string& filename);
    bool saveCachedFile(const std::string& filename);
    bool loadModel(const std::string& filename);

private:
    void createSphere();
	
	GLuint _vaoID;
	GLuint _vBufferID;
	GLuint _iBufferID;

	GLuint _tBufferID;
	unsigned int _tsize;
	float* _tarray;

	unsigned int _isize;
	unsigned int _vsize;
	Vertex* _varray;
	int* _iarray;
};

}  // namespace modelgeometry
}  // namespace openspace

#endif // __WAVEFRONTOBJECT_H__
