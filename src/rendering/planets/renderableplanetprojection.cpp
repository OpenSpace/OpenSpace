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

// open space includes
#include <openspace/rendering/planets/RenderablePlanetProjection.h>
#include <openspace/util/constants.h>
#include <openspace/rendering/planets/planetgeometryprojection.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>

#include <openspace/engine/openspaceengine.h>
#include <sgct.h>
#include <iomanip> 

#define _USE_MATH_DEFINES
#include <math.h>

namespace {
const std::string _loggerCat = "RenderablePlanetProjection";
}

namespace openspace {

#define printOpenGLError() printOglError(__FILE__, __LINE__)

	int printOglError(char *file, int line)
	{

		GLenum glErr;
		int    retCode = 0;

		glErr = glGetError();
		if (glErr != GL_NO_ERROR)
		{
			printf("glError in file %s @ line %d: %s\n",
				file, line, gluErrorString(glErr));
			retCode = 1;
		}
		return retCode;
	}


RenderablePlanetProjection::RenderablePlanetProjection(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
	, _colorTexturePath("colorTexture", "Color Texture")
	, _projectionTexturePath("colorTexture", "Color Texture")
	, _imageTrigger("imageTrigger", "Image Trigger")
    , _programObject(nullptr)
    , _texture(nullptr)
	, _textureProj(nullptr)
    , _geometry(nullptr)
{
	std::string name;
	bool success = dictionary.getValue(constants::scenegraphnode::keyName, name);
	assert(success);

    std::string path;
    success = dictionary.getValue(constants::scenegraph::keyPathModule, path);
	assert(success);

    ghoul::Dictionary geometryDictionary;
    success = dictionary.getValue(
		constants::renderableplanet::keyGeometry, geometryDictionary);
	if (success) {
		geometryDictionary.setValue(constants::scenegraphnode::keyName, name);
        geometryDictionary.setValue(constants::scenegraph::keyPathModule, path);
		_geometry = planetgeometryprojection::PlanetGeometryProjection::createFromDictionary(geometryDictionary);
	}

	dictionary.getValue(constants::renderableplanet::keyFrame, _target);

    // TODO: textures need to be replaced by a good system similar to the geometry as soon
    // as the requirements are fixed (ab)
    std::string texturePath = "";
	success = dictionary.getValue("Textures.Color", texturePath);
	if (success){
		_colorTexturePath = path + "/" + texturePath; 
	}
	success = dictionary.getValue("Textures.Project", texturePath);
	if (success){
		_projectionTexturePath = path + "/" + texturePath;
	}
	addPropertySubOwner(_geometry);

	addProperty(_imageTrigger);
	_imageTrigger.onChange(std::bind(&RenderablePlanetProjection::imageProject, this));

	addProperty(_colorTexturePath);
	_colorTexturePath.onChange(std::bind(&RenderablePlanetProjection::loadTexture, this));
	addProperty(_projectionTexturePath);
	_projectionTexturePath.onChange(std::bind(&RenderablePlanetProjection::loadTexture, this));
}

RenderablePlanetProjection::~RenderablePlanetProjection(){
    deinitialize();
}



bool RenderablePlanetProjection::initialize(){
	_computeShader = genComputeProg();

    bool completeSuccess = true;
    if (_programObject == nullptr)
        completeSuccess
              &= OsEng.ref().configurationManager().getValue("projectiveProgram", _programObject);

    loadTexture();
    completeSuccess &= (_texture != nullptr);
	completeSuccess &= (_textureProj != nullptr);

    completeSuccess &= _geometry->initialize(this);

    return completeSuccess;
}

bool RenderablePlanetProjection::deinitialize(){
    _geometry->deinitialize();
    delete _geometry;
    _geometry = nullptr;
    delete _texture;
    _texture = nullptr;
	delete _textureProj;
	_textureProj = nullptr;
    return true;
}

bool RenderablePlanetProjection::isReady() const {
	return (_geometry != nullptr);
}
GLuint RenderablePlanetProjection::genComputeProg() {
	// Creating the compute shader, and the program object containing the shader
	GLuint progHandle = glCreateProgram();
	GLuint cs = glCreateShader(GL_COMPUTE_SHADER);

	// In order to write to a texture, we have to introduce it as image2D.
	// local_size_x/y/z layout variables define the work group size.
	// gl_GlobalInvocationID is a uvec3 variable giving the global ID of the thread,
	// gl_LocalInvocationID is the local index within the work group, and
	// gl_WorkGroupID is the work group's index
	const char *csSrc[] = {
		"#version 440\n",
	    "writeonly uniform image2D destTex;\
	     layout (local_size_x = 16, local_size_y = 16) in;\
		 void main() {\
		       ivec2 storePos = ivec2(gl_GlobalInvocationID.xy);\
			   imageStore(destTex, storePos, vec4(1.0,0.0,0.0,1.0));\
		}"
	};

	glShaderSource(cs, 2, csSrc, NULL);
	glCompileShader(cs);
	int rvalue;
	glGetShaderiv(cs, GL_COMPILE_STATUS, &rvalue);
	if (!rvalue) {
		fprintf(stderr, "Error in compiling the compute shader\n");
		GLchar log[10240];
		GLsizei length;
		glGetShaderInfoLog(cs, 10239, &length, log);
		fprintf(stderr, "Compiler log:\n%s\n", log);
		exit(40);
	}
	else{
		printf(" CS COMPILE SUCCESS");
	}
	glAttachShader(progHandle, cs);

	glLinkProgram(progHandle);
	glGetProgramiv(progHandle, GL_LINK_STATUS, &rvalue);
	if (!rvalue) {
		fprintf(stderr, "Error in linking compute shader program\n");
		GLchar log[10240];
		GLsizei length;
		glGetProgramInfoLog(progHandle, 10239, &length, log);
		fprintf(stderr, "Linker log:\n%s\n", log);
		exit(41);
	}
	else{
		printf(" CS LINK SUCCESS");
	}

	return progHandle;
}
void RenderablePlanetProjection::updateTex(){ 

	glUseProgram(_computeShader);
	const GLint location = glGetUniformLocation(_computeShader, "destTex"); // 
	if (location == -1){
		printf("Could not locate uniform location for texture in CS");
	}

	ghoul::opengl::TextureUnit unit;
	unit.activate();
	//_texture->bind();
	glUniform1i(location, unit); 
	GLint format = _texture->internalFormat();
	glBindImageTexture(unit, *_texture, 0, GL_FALSE, 0, GL_WRITE_ONLY, GL_RGBA32UI);

	glDispatchCompute(_texture->width() / 16, _texture->height() / 16, 1); 
	glUseProgram(0);
	printOpenGLError();

}

void RenderablePlanetProjection::render(const RenderData& data)
{
	if (!_programObject) return;
	if (!_textureProj) return;


    // activate shader
    _programObject->activate();

    // scale the planet to appropriate size since the planet is a unit sphere
	_transform = glm::mat4(1);
	
	//earth needs to be rotated for that to work.
	glm::mat4 rot = glm::rotate(_transform, 90.f, glm::vec3(1, 0, 0));
		
	for (int i = 0; i < 3; i++){
		for (int j = 0; j < 3; j++){
			_transform[i][j] = _stateMatrix[i][j];
		}
	}
	_transform = _transform* rot;
	if (_target == "IAU_JUPITER"){ // tmp scale of jupiterx = 0.935126
		_transform *= glm::scale(glm::mat4(1), glm::vec3(1, 0.935126, 1));
	}

	// PROJECTIVE TEXTURING----------------------------------------------------------
	// get fov
	std::string shape, instrument;
	std::vector<glm::dvec3> bounds;
	glm::dvec3 bs;
	bool found = openspace::SpiceManager::ref().getFieldOfView("NH_LORRI", shape, instrument, bs, bounds);
	if (!found) LERROR("Could not locate instrument");

	psc position;
	double lightTime = 0.0;
	SpiceManager::ref().getTargetPosition("NEW HORIZONS", "JUPITER BARYCENTER","GALACTIC", "NONE", _time, position, lightTime);
	position[3] += 3;
	glm::vec3 nh_pos = position.vec3();

	//get up-vecto
	//rotate boresight into correct alignment
	_boresight = _instrumentMatrix*bs;
	glm::vec3 uptmp(_instrumentMatrix*glm::dvec3(data.camera.lookUpVector()));

	//create view matrix
	glm::vec3 e3 = glm::normalize(_boresight);
	glm::vec3 e1 = glm::normalize(glm::cross(uptmp, e3));
	glm::vec3 e2 = glm::normalize(glm::cross(e3, e1));
	
	glm::mat4 projViewMatrix = glm::mat4(     e1.x,                  e2.x,                  e3.x,        0.f,
								              e1.y,                  e2.y,                  e3.y,        0.f,
								              e1.z,                  e2.z,                  e3.z,        0.f, 
								    -glm::dot(e1, nh_pos), -glm::dot(e2, nh_pos), -glm::dot(e3, nh_pos), 1.f); 
	//create perspective projection matrix
	glm::mat4 projProjectionMatrix = glm::perspective(0.2907f, 1.f, 0.2f, 1000000.0f);
	//bias matrix
	glm::mat4 projNormalizationMatrix = glm::mat4(0.5f, 0   , 0   , 0,
												  0   , 0.5f, 0   , 0,
												  0   , 0   , 0.5f, 0,
						                          0.5f, 0.5f, 0.5f, 1 );

	_camScaling = data.camera.scaling();

	_projectorMatrix = projNormalizationMatrix*projProjectionMatrix*projViewMatrix;
    // setup the data to the shader
	_programObject->setUniform("ProjectorMatrix", _projectorMatrix);
	_programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	_programObject->setUniform("ModelTransform", _transform);
	_programObject->setAttribute("boresight", _boresight);
	setPscUniforms(_programObject, &data.camera, data.position);
	
    // Bind texture
    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _programObject->setUniform("texture1", unit); // jupiter

	ghoul::opengl::TextureUnit unit2;
	unit2.activate();
	_textureProj->bind();
	_programObject->setUniform("texture2", unit2); // proj

    // render
    _geometry->render();

    // disable shader
    _programObject->deactivate();

	updateTex();
}

void RenderablePlanetProjection::imageProject(){
		_textureProj->downloadTexture();
		_texture->downloadTexture();

		auto uvToModel = [](float u, float v, float radius[2], float fsegments)->glm::vec4{

			const float fj = u * fsegments;
			const float fi = v * fsegments;

			const float theta = fi * float(M_PI) / fsegments;  // 0 -> PI
			const float phi = fj * float(M_PI) * 2.0f / fsegments;

			const float x = radius[0] * sin(phi) * sin(theta);  //
			const float y = radius[0] * cos(theta);             // up
			const float z = radius[0] * cos(phi) * sin(theta);  //

			return glm::vec4(x, y, z, radius[1]);
		};

		auto uvToIndex = [](const glm::vec2 &uv, int w, int h, int &i, int &j){
			i = static_cast<int>(uv.x * float(w));
			j = static_cast<int>(uv.y * float(h));
		};

		auto inRange = [](int x, int a, int b)->bool{
			return (x >= a && x <= b);
		};

		auto pscToMeter = [](glm::vec4 v1, glm::vec2 v2)->glm::vec4{
			float factor = v2.x * pow(10, v2.y + v1.w);
			return glm::vec4(v1.xyz * factor, 1.0);
		};

		typedef glm::detail::tvec3<glm::detail::uint8> rgb;

		auto bilinear = [inRange](const ghoul::opengl::Texture* dest, const ghoul::opengl::Texture* source, 
			                      float x, float y, float i, float j)->rgb{
			x = (x * float(source->width())); 
			y = (y * float(source->height())); 

			int px = static_cast<int>(std::floor(x)); // floor of x
			int py = static_cast<int>(std::floor(y)); // floor of y
			
			rgb p0, p1, p2, p3;
			//original
			int of = 0;
		
			bool x0 = inRange(px,     1, source->width() - 1);
			bool y0 = inRange(py,     1, source->height() - 1);
			bool x1 = inRange(px + 1, 1, source->width() - 1);
			bool y1 = inRange(py + 1, 1, source->height() - 1);

			p0 = x0 && y0 ? source->texel<rgb>(px + 0, py + 0) : dest->texel<rgb>(i + 0, j + 0);
			p1 = x1 && y0 ? source->texel<rgb>(px + 1, py + 0) : dest->texel<rgb>(i + 1, j + 0);
			p2 = x0 && y1 ? source->texel<rgb>(px + 0, py + 1) : dest->texel<rgb>(i + 0, j + 1);
			p3 = x1 && y1 ? source->texel<rgb>(px + 1, py + 1) : dest->texel<rgb>(i + 1, j + 1);
			

			glm::vec3 p0f = (glm::vec3)p0; 
			glm::vec3 p1f = (glm::vec3)p1;
			glm::vec3 p2f = (glm::vec3)p2;
			glm::vec3 p3f = (glm::vec3)p3;

			float a = x - float(px);
			float b = y - float(py); 
			if (a < 0) a = -a; 
			if (b < 0) b = -b;

			glm::vec3 v1 = p0f*(1-a) + a*p1f;
			glm::vec3 v2 = p2f*(1-a) + a*p3f;

			glm::vec3 v = v1*(1-b) + b*v2;

			return rgb(v[0], v[1], v[2]);
		};


		const float w = _texture->width();
		const float h = _texture->height();
		const float wp = _textureProj->width();
		const float hp = _textureProj->height();

		for (int i = 0; i < w; ++i) {
			for (int j = 0; j < h; ++j) {
				// "Shader code"
				// Texture coordinates
				float u = float(i) / w;
				float v = float(j) / h;

				// Psc scaling
				// Convert texture coordinates to model coordinates
				float radius[2] = { 0.71492f, 8.f };
				glm::vec4 in_position = uvToModel(u, v, radius, 200);
				bool frontfacing = glm::dot(_boresight, glm::vec3((_transform*in_position).xyz)) < 0;

				// Convert psc to meters
				glm::vec4 raw_pos = pscToMeter(in_position, _camScaling);

				// Transform model coordinates to world coordinates
				glm::vec4 projected = _projectorMatrix * _transform  * raw_pos;

				projected.x /= projected.w;
				projected.y /= projected.w;

				// To do : use bilinear interpolation
				int x, y;
				glm::vec2 uv;
				uv.x = projected.x;
				uv.y = projected.y;
				uvToIndex(uv, wp, hp, x, y);

				if (frontfacing && inRange(x, 0, wp - 1) && inRange(y, 0, hp - 1)){
					rgb final(0);

					if (x <= (wp*0.5f)){ // bilinear vs nonbilinear comparison
						final = bilinear(_texture, _textureProj, uv.x, uv.y, i, j);// _textureProj->texel<rgb>(x, y);
					}else{
						final = _textureProj->texel<rgb>(x, y);
					}
					_texture->texel<rgb>(i, j) = final;
				}
			}
		}

		// Upload textures
		//_textureProj->uploadTexture();
		_texture->uploadTexture();

}

void RenderablePlanetProjection::update(const UpdateData& data){
	// set spice-orientation in accordance to timestamp
	_time = data.time;
	openspace::SpiceManager::ref().getPositionTransformMatrix(_target, "GALACTIC", data.time, _stateMatrix);
	openspace::SpiceManager::ref().getPositionTransformMatrix("NH_LORRI", "GALACTIC", data.time, _instrumentMatrix);

}

void RenderablePlanetProjection::loadTexture()
{
    delete _texture;
    _texture = nullptr;
    if (_colorTexturePath.value() != "") {
		_texture = ghoul::io::TextureReader::loadTexture(absPath(_colorTexturePath));
        if (_texture) {
            LDEBUG("Loaded texture from '" << absPath(_colorTexturePath) << "'");
			_texture->uploadTexture();

			// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
			_texture->setFilter(ghoul::opengl::Texture::FilterMode::Nearest);
        }
    }


	delete _textureProj;
	_textureProj = nullptr;
	if (_colorTexturePath.value() != "") {
		_textureProj = ghoul::io::TextureReader::loadTexture(absPath(_projectionTexturePath));
		if (_textureProj) {
			LDEBUG("Loaded texture from '" << absPath(_projectionTexturePath) << "'");
			_textureProj->uploadTexture();

			// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
			_textureProj->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
			_textureProj->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToBorder);

		}
	}
}

}  // namespace openspace
