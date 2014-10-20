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

// temporary includes (will fix as soon as I figure out how class hierarchy should work, 
//                     ie after I see model on screen)
#include<fstream>

// open space includes
#include <openspace/rendering/renderablewavefrontobject.h>
#include <openspace/util/constants.h>

#include <ghoul/opengl/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>

#include <openspace/engine/openspaceengine.h>
#include <sgct.h>

namespace {
const std::string _loggerCat = "RenderableWavefrontObject";
}

namespace openspace {
RenderableWavefrontObject::RenderableWavefrontObject(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
	, _colorTexturePath("colorTexture", "Color Texture")
    , _programObject(nullptr)
	, _fovProgram(nullptr)
    , _texture(nullptr)
{
	std::string name;
	bool success = dictionary.getValue(constants::scenegraphnode::keyName, name);
	assert(success);

	std::string path;
	dictionary.getValue(constants::scenegraph::keyPathModule, path);

	std::string texturePath = "";
	if (dictionary.hasKey("Textures.Color")) {
		dictionary.getValue("Textures.Color", texturePath);
		_colorTexturePath = path + "/" + texturePath;
	}

	addProperty(_colorTexturePath);
	_colorTexturePath.onChange(std::bind(&RenderableWavefrontObject::loadTexture, this));

	_mode = GL_TRIANGLES;

	std::string file;
	dictionary.getValue(constants::renderablewavefrontobject::keyObjFile, file);

	const std::string filename = FileSys.absolutePath(file);

	std::cout << "OBJECT LOADER FILENAME : " << filename << std::endl;
	
	std::ifstream ifile(filename.c_str());

	if (ifile){
		LDEBUG("Found file..\n");
		ifile.close();
		loadObj(filename.c_str());
	}else {
		LERROR("Did not find file..\n");
	}

}



void RenderableWavefrontObject::loadObj(const char *filename){
	// temporary 
	int vertexSize = 0;
	int vertexNormalSize = 0;
	int vertexTextureSize = 0;
	int indicesSize = 0;

	float f1, f2, f3;
	int i1, i2, i3, i4, i5, i6, i7, i8, i9;
	char line[150];
	float maxtex = 0.0;

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
	/*	END LINE COUNT */
	// allocate memory for all arrays
	_isize = indicesSize;
	_vsize = indicesSize;

	// float arrays
	float *tempVertexArray = new float[vertexSize];// vertexSize*sizeof(float));
	float *tempVertexNormalArray = new float[vertexNormalSize];
	float *tempVertexTextureArray = new float[vertexTextureSize];
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
				(tempNormalIndicesArray)[m] = i3 - 1;
				m++;
				(_iarray)[m] = i4 - 1;
				(tempTextureIndicesArray)[m] = i5 - 1;
				(tempNormalIndicesArray)[m] = i6 - 1;
				m++;
				(_iarray)[m] = i7 - 1;
				(tempTextureIndicesArray)[m] = i8 - 1;
				(tempNormalIndicesArray)[m] = i9 - 1;
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

		_varray[m].location[3] = 7;
		int q = 0;
		while (q < 3){
			_varray[m].location[q] = tempVertexArray[vertexIndex + q];
			_varray[m].normal[q] = tempVertexNormalArray[normalIndex + q];
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
	delete [] tempVertexArray;
	delete [] tempVertexNormalArray;
	delete [] tempNormalIndicesArray;
	delete [] tempVertexTextureArray;
	delete [] tempTextureIndicesArray;
}

RenderableWavefrontObject::~RenderableWavefrontObject(){
    deinitialize();
}

bool RenderableWavefrontObject::initialize()
{
    bool completeSuccess = true;
    if (_programObject == nullptr)
        completeSuccess
              &= OsEng.ref().configurationManager().getValue("pscShader", _programObject); 

    loadTexture();
    completeSuccess &= (_texture != nullptr);
   //completeSuccess &= _geometry->initialize(this); 

	PowerScaledScalar ps = PowerScaledScalar::PowerScaledScalar(1,7);
	setBoundingSphere(ps);


	GLuint errorID;
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

	errorID = glGetError();
	if (errorID != GL_NO_ERROR) {
		LERROR("OpenGL error: " << glewGetErrorString(errorID));
		return false;
	}
    return completeSuccess;
}

bool RenderableWavefrontObject::deinitialize()
{
	delete[] _varray;
	delete[] _iarray;

	glDeleteBuffers(1, &_vBufferID);
	glDeleteBuffers(1, &_iBufferID);
	glDeleteVertexArrays(1, &_vaoID);

    delete _texture;
    _texture = nullptr;
    return true;
}

void RenderableWavefrontObject::render(const RenderData& data)
{
	if (!_programObject)
		return;
	if (!_texture)
		return;

    // activate shader
    _programObject->activate();

    // fetch data
	psc currentPosition = data.position;
	psc campos          = data.camera.position();
    glm::mat4 camrot    = data.camera.viewRotationMatrix();
   // PowerScaledScalar scaling = camera->scaling();

	
    // scale the planet to appropriate size since the planet is a unit sphere
    glm::mat4 transform = glm::mat4(1);
	glm::mat4 scaler = glm::scale(transform, glm::vec3(0, 0, -1));
	
	//earth needs to be rotated for that to work.
	glm::mat4 rot_x = glm::rotate(transform, 270.f, glm::vec3(1, 0, 0));
	glm::mat4 rot_y = glm::rotate(transform, 90.f, glm::vec3(0, 1, 0));


	glm::mat4 tmp = glm::mat4(1);
	for (int i = 0; i < 3; i++){
		for (int j = 0; j < 3; j++){
			tmp[i][j] = _stateMatrix[i][j];
		}
	}
	transform *= tmp;
	transform *= rot_x;
	//transform *= rot_y;
	
	glm::mat4 modelview = data.camera.viewMatrix()*data.camera.modelMatrix();
	glm::vec4 camSpaceEye = -(modelview*currentPosition.vec4());
	// setup the data to the shader
//	_programObject->setUniform("camdir", camSpaceEye);
	_programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	_programObject->setUniform("ModelTransform", transform);
	setPscUniforms(_programObject, &data.camera, data.position);
	
    // Bind texture
    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _programObject->setUniform("texture1", unit);

    // render
	glBindVertexArray(_vaoID);  // select first VAO
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iBufferID);
	glDrawElements(_mode, _isize, GL_UNSIGNED_INT, 0);
	glBindVertexArray(0);

    // disable shader
    _programObject->deactivate();

	std::string shape, name;
	shape.resize(32);
	name.resize(32);
	std::vector<glm::dvec3> bounds;
	glm::dvec3 boresight;

	bool found = openspace::SpiceManager::ref().getFieldOfView("NH_LORRI", shape, name, boresight, bounds);
	glm::vec4 a(boresight, 1);
	a = transform*a;
	//std::cout << a[0] << " " << a[1] << " " << a[2] << std::endl;
}

void RenderableWavefrontObject::update(const UpdateData& data)
{
	glm::dvec3 position(0, 0, 0);
	// set spice-orientation in accordance to timestamp
	openspace::SpiceManager::ref().getPositionTransformMatrix("NH_SPACECRAFT", "GALACTIC", data.time, _stateMatrix);
	
}

void RenderableWavefrontObject::loadTexture()
{
    delete _texture;
    _texture = nullptr;
    if (_colorTexturePath.value() != "") {
        _texture = ghoul::opengl::loadTexture(absPath(_colorTexturePath));
        if (_texture) {
            LDEBUG("Loaded texture from '" << absPath(_colorTexturePath) << "'");
            _texture->uploadTexture();
        }
    }
}

}  // namespace openspace
