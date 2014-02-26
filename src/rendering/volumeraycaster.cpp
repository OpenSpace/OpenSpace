#include "volumeraycaster.h"

#include <glm/glm.hpp>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/texturereader.h>
#include <iostream>

namespace openspace {

GLuint vertexArray = GL_FALSE;
GLuint vertexPositionBuffer = GL_FALSE;
GLint matrix_loc = -1;
GLuint sgctFBO;

VolumeRaycaster::VolumeRaycaster() {
	initialize();
}

VolumeRaycaster::VolumeRaycaster(Camera* camera) {
	_camera = camera;
	initialize();
}

VolumeRaycaster::~VolumeRaycaster() {}

void VolumeRaycaster::initialize() {
	const GLfloat size = 0.5f;
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

	_test = loadTexture(absPath("${BASE_PATH}/openspace-data/jonas.jpg"));

	if (_test == nullptr)
		std::cout << "Image failed to load" << std::endl;

	_test->uploadTexture();

	myBox = new sgct_utils::SGCTBox(1.5f, sgct_utils::SGCTBox::Regular);

//	------ SETUP SHADERS -----------------
	_program = new ProgramObject("RaycastProgram");

	ShaderObject* vertexShader = new ShaderObject(ShaderObject::ShaderTypeVertex, absPath("${BASE_PATH}/shaders/passthrough.vert"));
	ShaderObject* fragmentShader = new ShaderObject(ShaderObject::ShaderTypeFragment, absPath("${BASE_PATH}/shaders/passthrough.frag"));

	_program->attachObject(vertexShader);
	_program->attachObject(fragmentShader);

//	------ SETUP FBO ---------------------
	_fbo = new FramebufferObject();
	_fbo->activate();

	int x = sgct::Engine::instance()->getActiveXResolution();
	int y = sgct::Engine::instance()->getActiveYResolution();
	_texture = new Texture(glm::size3_t(x,y,1));
	_texture->uploadTexture();

	_fbo->attachTexture(_texture);
	if (_fbo->isComplete())
		std::cout << "All is well" << std::endl;
	else
		std::cout << "Uh oh" << std::endl;

	_fbo->deactivate();
}

void VolumeRaycaster::loadUniforms() {
	glm::mat4 modelViewProjectionMatrix = sgct::Engine::instance()->getActiveModelViewProjectionMatrix();
	_program->setUniform("modelViewProjection", modelViewProjectionMatrix);

	glActiveTexture(GL_TEXTURE0);
	_test->bind();
	_program->setUniform("texJonas", 0);
}

void VolumeRaycaster::render() {
//	------ DRAW TO FBO -------------------
	sgctFBO = FramebufferObject::getActiveObject(); // Save SGCTs main FBO

	_fbo->activate();
	glClearColor(0.2f, 0.2f, 0.2f, 0);
	glClear(GL_COLOR_BUFFER_BIT);
	_program->compileShaderObjects();
	_program->linkProgramObject();
	_program->activate();
	loadUniforms();

	glEnable(GL_CULL_FACE);
		glCullFace(GL_FRONT);
		myBox->draw();
	glDisable(GL_CULL_FACE);

	_program->deactivate();
	_fbo->deactivate();

//	------ DRAW TO SCREEN ----------------
	glBindFramebuffer(GL_FRAMEBUFFER, sgctFBO); // Re-bind SGCTs main FBO
	glClearColor(0.2f, 0.2f, 0.2f, 0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	_program->compileShaderObjects();
	_program->linkProgramObject();
	_program->activate();
	loadUniforms();

	glBindVertexArray(vertexArray);
		glDrawArrays(GL_TRIANGLES, 0, 6);
	glBindVertexArray(0);

	_program->deactivate();
}

}// namespace openspace
