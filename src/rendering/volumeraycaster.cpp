#include "volumeraycaster.h"

#include <glm/glm.hpp>
#include <ghoul/filesystem/filesystem.h>
#include <iostream>
#include <cmath>
#include <string>

namespace openspace {

GLuint vertexArray = GL_FALSE;
GLuint vertexPositionBuffer = GL_FALSE;
GLuint CubeCenterVbo, SlotPosition;
float _stepSize = 0.05f;
sgct::SharedDouble curr_time(0.0);

void keyCallback(int key, int action);

GLuint CreatePointVbo(float x, float y, float z) {
    float p[] = {x, y, z};
    GLuint vbo;
    glGenBuffers(1, &vbo);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBufferData(GL_ARRAY_BUFFER, sizeof(p), &p[0], GL_STATIC_DRAW);
    return vbo;
}

VolumeRaycaster::VolumeRaycaster() {
	initialize();
}

VolumeRaycaster::~VolumeRaycaster() {}

void VolumeRaycaster::initialize() {
	const GLfloat size = 1.0f;
	const GLfloat vertex_texcoord_data[] = { // square of two triangles (sigh)
					-size, -size, 0.0f, 0.0f, 0.0f,
					 size,	size, 0.0f, 1.0f, 1.0f,
					-size,  size, 0.0f, 0.0f, 1.0f,
					-size, -size, 0.0f, 0.0f, 0.0f,
					 size, -size, 0.0f, 1.0f, 0.0f,
					 size,	size, 0.0f, 1.0f, 1.0f
				};

	glGenVertexArrays(1, &vertexArray); // generate array
	glBindVertexArray(vertexArray); // bind array

	glGenBuffers(1, &vertexPositionBuffer); // generate buffer
	glBindBuffer(GL_ARRAY_BUFFER, vertexPositionBuffer); // bind buffer
	glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_texcoord_data), vertex_texcoord_data, GL_STATIC_DRAW);

	// Vertex positions
	GLuint vertexLocation = 2;
	glEnableVertexAttribArray(vertexLocation);
	glVertexAttribPointer(vertexLocation, 3, GL_FLOAT, GL_FALSE, 5*sizeof(GLfloat), reinterpret_cast<void*>(0));

	// Texture coordinates
	GLuint texcoordLocation = 0;
	glEnableVertexAttribArray(texcoordLocation);
	glVertexAttribPointer(texcoordLocation, 2, GL_FLOAT, GL_FALSE, 5*sizeof(GLfloat), (void*)(3*sizeof(GLfloat)));

	glBindBuffer(GL_ARRAY_BUFFER, 0); //unbind buffer
	glBindVertexArray(0); //unbind array

	myBox = new sgct_utils::SGCTBox(1.0f, sgct_utils::SGCTBox::Regular);

//	------ VOLUME READING ----------------
	std::string tmp = absPath("${BASE_PATH}/openspace-data/skull_256x256x256_8.raw");
	const char* filename = tmp.c_str();
	glm::ivec3 dimensions = glm::ivec3(256);
	_volume = createVolumetexture(filename, dimensions);

//	------ SETUP SHADERS -----------------
	_fboProgram = new ProgramObject("RaycastProgram");
	ShaderObject* vertexShader = new ShaderObject(ShaderObject::ShaderTypeVertex,
			absPath("${BASE_PATH}/shaders/exitpoints.vert"));
	ShaderObject* fragmentShader = new ShaderObject(ShaderObject::ShaderTypeFragment,
			absPath("${BASE_PATH}/shaders/exitpoints.frag"));
	_fboProgram->attachObject(vertexShader);
	_fboProgram->attachObject(fragmentShader);
	_fboProgram->compileShaderObjects();
	_fboProgram->linkProgramObject();

	_twopassProgram = new ProgramObject("RaycastProgram");
	vertexShader = new ShaderObject(ShaderObject::ShaderTypeVertex,
			absPath("${BASE_PATH}/shaders/twopassraycaster.vert"));
	fragmentShader = new ShaderObject(ShaderObject::ShaderTypeFragment,
			absPath("${BASE_PATH}/shaders/twopassraycaster.frag"));
	_twopassProgram->attachObject(vertexShader);
	_twopassProgram->attachObject(fragmentShader);
	_twopassProgram->compileShaderObjects();
	_twopassProgram->linkProgramObject();
	_twopassProgram->setUniform("texBack", 0);
	_twopassProgram->setUniform("texFront", 1);
	_twopassProgram->setUniform("texVolume", 2);
	_twopassProgram->setUniform("screenWidth", sgct::Engine::instance()->getActiveXResolution());
	_twopassProgram->setUniform("screenHeight", sgct::Engine::instance()->getActiveYResolution());
	_twopassProgram->setUniform("stepSize", _stepSize);

	float topPlane, nearPlane;
	topPlane = sgct::Engine::instance()->getActiveWindowPtr()->getCurrentViewport()->getFrustum()->getTop();
	nearPlane = sgct::Engine::instance()->getActiveWindowPtr()->getCurrentViewport()->getFrustum()->getNear();
	float FOV = float(atan(topPlane / nearPlane)) * (180.0 / 3.141592653589793) * 2.0f;
	float focalLength = 1.0f / tan(FOV / 2);
	int x = sgct::Engine::instance()->getActiveXResolution();
	int y = sgct::Engine::instance()->getActiveYResolution();
	CubeCenterVbo = CreatePointVbo(0, 0, 0);

	_singlepassProgram = new ProgramObject("RaycastProgram");
	vertexShader = new ShaderObject(ShaderObject::ShaderTypeVertex,
			absPath("${BASE_PATH}/shaders/singlepassraycaster.vert"));
	fragmentShader = new ShaderObject(ShaderObject::ShaderTypeFragment,
			absPath("${BASE_PATH}/shaders/singlepassraycaster.frag"));
	ShaderObject* geometryShader = new ShaderObject(ShaderObject::ShaderTypeGeometry,
			absPath("${BASE_PATH}/shaders/singlepassraycaster.gs"));
	_singlepassProgram->attachObject(vertexShader);
	_singlepassProgram->attachObject(fragmentShader);
	_singlepassProgram->attachObject(geometryShader);
	_singlepassProgram->compileShaderObjects();
	_singlepassProgram->linkProgramObject();
	_singlepassProgram->setUniform("FocalLength", focalLength);
	_singlepassProgram->setUniform("WindowSize", glm::vec2(x,y));
	_singlepassProgram->setUniform("Density", 2);


	sgct::Engine::instance()->setKeyboardCallbackFunction(keyCallback);
//	------ SETUP FBO ---------------------
	_fbo = new FramebufferObject();
	_fbo->activate();

	_backTexture = new Texture(glm::size3_t(x,y,1));
	_frontTexture = new Texture(glm::size3_t(x,y,1));
	_backTexture->uploadTexture();
	_frontTexture->uploadTexture();
	_fbo->attachTexture(_backTexture, GL_COLOR_ATTACHMENT0);
	_fbo->attachTexture(_frontTexture, GL_COLOR_ATTACHMENT1);

	_fbo->deactivate();
}

void keyCallback(int key, int action) {
	switch( key ) {
		case GLFW_KEY_UP:
		case 'W':
			_stepSize += 0.01;
			std::cout << "Stepsize: " << _stepSize << std::endl;
			break;

		case GLFW_KEY_DOWN:
		case 'S':
			if (_stepSize > 0.015)
				_stepSize -= 0.01;

			std::cout << "Stepsize: " << _stepSize << std::endl;
			break;
	}
}

void VolumeRaycaster::render() {
	float speed = 50.0f;
	curr_time.setVal(sgct::Engine::getTime());
	glm::mat4 scene_mat = glm::rotate( glm::mat4(1.0f), static_cast<float>( curr_time.getVal() ) * speed, glm::vec3(0.0f, 1.0f, 0.0f));
	glm::mat4 modelViewProjectionMatrix = sgct::Engine::instance()->getActiveModelViewProjectionMatrix() * scene_mat;

//	------ TWO PASS ----------------------
//	------ DRAW TO FBO -------------------
	_sgctFBO = FramebufferObject::getActiveObject(); // Save SGCTs main FBO
	_fbo->activate();
	_fboProgram->activate();
	_fboProgram->setUniform("modelViewProjection", modelViewProjectionMatrix);

//	Draw backface
	glDrawBuffer(GL_COLOR_ATTACHMENT0);
	glClearColor(0.2f, 0.2f, 0.2f, 0);
	glClear(GL_COLOR_BUFFER_BIT);
	glEnable(GL_CULL_FACE);
		glCullFace(GL_FRONT);
		myBox->draw();
	glDisable(GL_CULL_FACE);

//	Draw frontface
	glDrawBuffer(GL_COLOR_ATTACHMENT1);
	glClear(GL_COLOR_BUFFER_BIT);
	glClearColor(0.2f, 0.2f, 0.2f, 0);
	glEnable(GL_CULL_FACE);
		glCullFace(GL_BACK);
		myBox->draw();
	glDisable(GL_CULL_FACE);

	_fboProgram->deactivate();
	_fbo->deactivate();

//	------ DRAW TO SCREEN ----------------
	glBindFramebuffer(GL_FRAMEBUFFER, _sgctFBO); // Re-bind SGCTs main FBO
	_twopassProgram->activate();
	_twopassProgram->setUniform("stepSize", _stepSize);

//	 Set textures
	glActiveTexture(GL_TEXTURE0);
	_backTexture->bind();
	glActiveTexture(GL_TEXTURE1);
	_frontTexture->bind();
	glActiveTexture(GL_TEXTURE2);
	_volume->bind();

//	Draw screenquad
	glClearColor(0.2f, 0.2f, 0.2f, 0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glBindVertexArray(vertexArray);
		glDrawArrays(GL_TRIANGLES, 0, 6);
	glBindVertexArray(0);

	_twopassProgram->deactivate();

////	------ SINGLE PASS -------------------
//	glm::mat4 modelView = sgct::Engine::instance()->getActiveModelViewMatrix();
//	glm::vec3 eyePos = *sgct::Engine::instance()->getUserPtr()->getPosPtr();
//	_singlepassProgram->setUniform("modelViewProjection", modelViewProjectionMatrix);
//	_singlepassProgram->setUniform("Modelview", modelView);
//	_singlepassProgram->setUniform("RayOrigin", glm::transpose(modelView)*glm::vec4(eyePos, 1.0));
//	_singlepassProgram->setUniform("ProjectionMatrix", sgct::Engine::instance()->getActiveProjectionMatrix());
//	_singlepassProgram->setUniform("ViewMatrix", sgct::Engine::instance()->getActiveViewMatrix());
//	_singlepassProgram->activate();
//
//	glBindBuffer(GL_ARRAY_BUFFER, CubeCenterVbo);
//	GLuint SlotPosition = 5;
//	glEnableVertexAttribArray(SlotPosition);
//	glVertexAttribPointer(SlotPosition, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), 0);
//	glEnableVertexAttribArray(SlotPosition);
//	glClearColor(0.2f, 0.2f, 0.2f, 0);
//	glClear(GL_COLOR_BUFFER_BIT);
//
//	glActiveTexture(GL_TEXTURE2);
//	_volume->bind();
//
//	glDrawArrays(GL_POINTS, 0, 1);
//
//	_singlepassProgram->deactivate();
}

Texture* VolumeRaycaster::createVolumetexture(const char *filename, glm::ivec3 dimensions) {
	int size = dimensions.x*dimensions.y*dimensions.z;
	GLubyte *data = new GLubyte[size];

    if( FILE *fin = fopen(filename, "rb") ){
    	fread(data, sizeof(unsigned char), size, fin);
    	fclose(fin);
    } else {
    	fprintf( stderr, "Could not open file '%s'\n", filename );
    }

    Texture* texture = new Texture(data, glm::size3_t(dimensions),
				Texture::Format::Red, GL_R8, GL_UNSIGNED_BYTE,
				Texture::FilterMode::Linear, Texture::WrappingMode::ClampToBorder);
    texture->uploadTexture();

	delete []data;
	return texture;
}

}// namespace openspace
