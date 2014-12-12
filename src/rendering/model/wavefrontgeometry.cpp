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
#include<fstream>

#include <openspace/rendering/model/wavefrontgeometry.h>
#include <openspace/util/constants.h>
#include <ghoul/filesystem/filesystem.h>


namespace {
    const std::string _loggerCat = "WavefrontGeometry";
}

namespace openspace {
namespace modelgeometry {

WavefrontGeometry::WavefrontGeometry(const ghoul::Dictionary& dictionary)
    : ModelGeometry()
	, _isize(0)
	, _vsize(0)
	, _varray(nullptr)
	, _iarray(nullptr)
{
	using constants::scenegraphnode::keyName;

	// The name is passed down from the SceneGraphNode
    std::string name;
    bool success = dictionary.getValue(keyName, name);

	std::string file;
	success = dictionary.getValue(constants::modelgeometry::keyObjFile, file);
	if (!success) {
        LERROR("SimpleSphereGeometry of '" << name << "' did not provide a key '"
			                               << constants::modelgeometry::keyObjFile << "'");
	}
	const std::string filename = FileSys.absolutePath(file);

	std::ifstream ifile(filename.c_str());
	if (ifile){
		LDEBUG("Found file..\n");
		ifile.close();
		loadObj(filename.c_str());
	}
	else {
		LERROR("Did not find file..\n");
	}
}

void WavefrontGeometry::loadObj(const char *filename){
	// temporary 
	int vertexSize = 0;
	int vertexNormalSize = 0;
	int vertexTextureSize = 0;
	int indicesSize = 0;

	float f1, f2, f3;
	int i1, i2, i3, i4, i5, i6, i7, i8, i9;
	char line[150];
	//float maxtex = 0.0;

	FILE *fi;
	// START LINE COUNT
	fi = fopen(filename, "r");
	if (fi == NULL) {
		LERROR("Null Object\n");

	}
	while (fgets(line, 150, fi) != NULL)
	{
		if (sscanf(line, "v %f%f%f", &f1, &f2, &f3)) {
			vertexSize += 4;
		}
		if (sscanf(line, "vn %f%f%f", &f1, &f2, &f3)) {
			vertexNormalSize += 3;
		}
		if (sscanf(line, "vt %f%f%f", &f1, &f2, &f3)) {
			vertexTextureSize += 3;
		}
		if (vertexTextureSize > 0) {
			if (sscanf(line, "f %i/%i/%i %i/%i/%i %i/%i/%i", &i1, &i2, &i3, &i4, &i5, &i6, &i7, &i8, &i9)) {
				indicesSize += 3;
			}
		}
		else {
			if (sscanf(line, "f %i//%i %i//%i %i//%i", &i1, &i2, &i3, &i4, &i5, &i6)) {
				indicesSize += 3;
			}
		}
	}
	// allocate memory for all arrays
	_isize = indicesSize;
	_vsize = indicesSize;

	// float arrays
	float* tempVertexArray = new float[vertexSize];
	float* tempVertexNormalArray = new float[vertexNormalSize];
	float* tempVertexTextureArray = new float[vertexTextureSize];
	_varray = new Vertex[_vsize];

	// int arrays
	_iarray = new int[_isize];
	int *tempNormalIndicesArray = new int[_isize];
	int *tempTextureIndicesArray = new int[_isize];

	// keeping track of the array indexes
	unsigned int i = 0;
	unsigned int n = 0;
	unsigned int m = 0;
	unsigned int w = 0;

	// Go back to beginning of file
	fseek(fi, 0, SEEK_SET);
	while (fgets(line, 150, fi) != NULL){
		if (sscanf(line, "v %f%f%f", &f1, &f2, &f3)){
			(tempVertexArray)[i] = f1;
			i++;
			(tempVertexArray)[i] = f2;
			i++;
			(tempVertexArray)[i] = f3;
			i++;
		}
		if (sscanf(line, "vn %f%f%f", &f1, &f2, &f3)){
			(tempVertexNormalArray)[n] = f1;
			n++;
			(tempVertexNormalArray)[n] = f2;
			n++;
			(tempVertexNormalArray)[n] = f3;
			n++;
		}
		if (sscanf(line, "vt %f%f%f", &f1, &f2, &f3)){
			(tempVertexTextureArray)[w] = f1;
			w++;
			(tempVertexTextureArray)[w] = f2;
			w++;
			(tempVertexTextureArray)[w] = f3;
			w++;
		}
		if (vertexTextureSize > 0){
			if (sscanf(line, "f %i/%i/%i %i/%i/%i %i/%i/%i", &i1, &i2, &i3, &i4, &i5, &i6, &i7, &i8, &i9)){
				(_iarray)[m] = i1 - 1;
				(tempTextureIndicesArray)[m] = i2 - 1;
				(tempNormalIndicesArray)[m]  = i3 - 1;
				m++;
				(_iarray)[m] = i4 - 1;
				(tempTextureIndicesArray)[m] = i5 - 1;
				(tempNormalIndicesArray)[m]  = i6 - 1;
				m++;
				(_iarray)[m] = i7 - 1;
				(tempTextureIndicesArray)[m] = i8 - 1;
				(tempNormalIndicesArray)[m]  = i9 - 1;
				m++;
			}
		}
		else{
			if (sscanf(line, "f %i//%i %i//%i %i//%i", &i1, &i2, &i3, &i4, &i5, &i6)){
				(_iarray)[m] = i1 - 1;
				(tempNormalIndicesArray)[m] = i2 - 1;
				m++;
				(_iarray)[m] = i3 - 1;
				(tempNormalIndicesArray)[m] = i4 - 1;
				m++;
				(_iarray)[m] = i5 - 1;
				(tempNormalIndicesArray)[m] = i6 - 1;
				m++;
			}
		}
	}
	fclose(fi);

	// end of file read
	// creating the vertex array
	i = 0; n = 0; m = 0;
	int normalIndex = 0;
	int textureIndex = 0;
	int vertexIndex = 0;
	while (m < _vsize){
		normalIndex = tempNormalIndicesArray[m] * 3;
		textureIndex = tempTextureIndicesArray[m] * 3;
		vertexIndex = _iarray[m] * 3;
		_iarray[m] = m;

		_varray[m].location[3] = 7; // I need to set this proper at some point. 
		int q = 0;
		while (q < 3){
			_varray[m].location[q] = tempVertexArray[vertexIndex + q];
			_varray[m].normal[q]   = tempVertexNormalArray[normalIndex + q];
			q++;
		}

		if (vertexTextureSize > 0){
			_varray[m].tex[0] = tempVertexTextureArray[textureIndex];
			_varray[m].tex[1] = tempVertexTextureArray[textureIndex + 1];
		}
		else{
			_varray[m].tex[0] = 1.0;
			_varray[m].tex[1] = 1.0;
		}
		m++;
	}
	// free up memory
	delete[] tempVertexArray;
	delete[] tempVertexNormalArray;
	delete[] tempNormalIndicesArray;
	delete[] tempVertexTextureArray;
	delete[] tempTextureIndicesArray;
}


WavefrontGeometry::~WavefrontGeometry(){
}

bool WavefrontGeometry::initialize(RenderableModel* parent){
	bool success = ModelGeometry::initialize(parent);
    createSphere();
	
	if (_isize == 0) return false;
	glGenVertexArrays(1, &_vaoID);
	glGenBuffers(1, &_vBufferID);
	glGenBuffers(1, &_iBufferID);

	glBindVertexArray(_vaoID);
	glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
	glBufferData(GL_ARRAY_BUFFER, _vsize * sizeof(Vertex), _varray, GL_STATIC_DRAW);

	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glEnableVertexAttribArray(2);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(Vertex),
		reinterpret_cast<const GLvoid*>(offsetof(Vertex, location)));
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex),
		reinterpret_cast<const GLvoid*>(offsetof(Vertex, tex)));
	glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex),
		reinterpret_cast<const GLvoid*>(offsetof(Vertex, normal)));

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iBufferID);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, _isize * sizeof(int), _iarray, GL_STATIC_DRAW);
	glBindVertexArray(0);

	GLint errorID = glGetError();
	if (errorID != GL_NO_ERROR) {
		LERROR("OpenGL error: " << glewGetErrorString(errorID));
		return false;
	}
    return success;
}

void WavefrontGeometry::deinitialize(){
	if (_varray)
		delete[] _varray;
	if (_iarray)
		delete[] _iarray;

	glDeleteBuffers(1, &_vBufferID);
	glDeleteBuffers(1, &_iBufferID);
	glDeleteVertexArrays(1, &_vaoID);
    
}

void WavefrontGeometry::render(){
	// render
	glBindVertexArray(_vaoID);  // select first VAO
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iBufferID);
	glDrawElements(GL_TRIANGLES, _isize, GL_UNSIGNED_INT, 0);
	glBindVertexArray(0);
}

void WavefrontGeometry::createSphere(){
   // create the power scaled scalar
	PowerScaledScalar ps = PowerScaledScalar(1.0, 0.0); // will set proper bounding soon.
	_parent->setBoundingSphere(ps);
}

}  // namespace modelgeometry
}  // namespace openspace
