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

#ifndef __MODELGEOMETRY_H__
#define __MODELGEOMETRY_H__

#include <openspace/properties/propertyowner.h>
#include <openspace/rendering/model/renderablemodel.h>
#include <ghoul/misc/dictionary.h>

namespace openspace {

namespace modelgeometry {

class ModelGeometry : public properties::PropertyOwner {
public:
	static ModelGeometry* createFromDictionary(const ghoul::Dictionary& dictionary);

	ModelGeometry(const ghoul::Dictionary& dictionary);
	virtual ~ModelGeometry();
    virtual bool initialize(RenderableModel* parent);
    virtual void deinitialize();
	void render();
	virtual bool loadModel(const std::string& filename) = 0;

protected:
	RenderableModel* _parent;
	struct Vertex {
		GLfloat location[4];
		GLfloat tex[2];
		GLfloat normal[3];
	};

	bool loadObj(const std::string& filename);
	bool loadCachedFile(const std::string& filename);
	bool saveCachedFile(const std::string& filename);
	
	GLuint _vaoID;
	GLuint _vbo;
	GLuint _ibo;

	std::vector<Vertex> _vertices;
	std::vector<int> _indices;
	std::string _file;
};

}  // namespace modelgeometry
}  // namespace openspace

#endif  // __MODELGEOMETRY_H__
