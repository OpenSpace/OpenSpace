#include "volumeraycaster.h"

#include <glm/glm.hpp>
#include <ghoul/filesystem/filesystem.h>
#include <iostream>

namespace openspace {

GLuint vertexArray = GL_FALSE;
GLuint vertexPositionBuffer = GL_FALSE;

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
	const char* filename = "../openspace-data/skull_256x256x256_8.raw";
	int dimensions[3];
	dimensions[0] = 256;dimensions[1] = 256;dimensions[2] = 256;
	createVolumetexture(filename, dimensions);

//	------ SETUP SHADERS -----------------
	_FBOProgram = new ProgramObject("RaycastProgram");
	ShaderObject* vertexShader = new ShaderObject(ShaderObject::ShaderTypeVertex, absPath("${BASE_PATH}/shaders/exitpoints.vert"));
	ShaderObject* fragmentShader = new ShaderObject(ShaderObject::ShaderTypeFragment, absPath("${BASE_PATH}/shaders/exitpoints.frag"));
	_FBOProgram->attachObject(vertexShader);
	_FBOProgram->attachObject(fragmentShader);
	_FBOProgram->compileShaderObjects();
	_FBOProgram->linkProgramObject();

	_screenProgram = new ProgramObject("RaycastProgram");
	vertexShader = new ShaderObject(ShaderObject::ShaderTypeVertex, absPath("${BASE_PATH}/shaders/rendertoscreen.vert"));
	fragmentShader = new ShaderObject(ShaderObject::ShaderTypeFragment, absPath("${BASE_PATH}/shaders/rendertoscreen.frag"));
	_screenProgram->attachObject(vertexShader);
	_screenProgram->attachObject(fragmentShader);
	_screenProgram->compileShaderObjects();
	_screenProgram->linkProgramObject();
	_screenProgram->setUniform("texBack", 0);
	_screenProgram->setUniform("texFront", 1);
	_screenProgram->setUniform("texVolume", 2);
	_screenProgram->setUniform("screenWidth", sgct::Engine::instance()->getActiveXResolution());
	_screenProgram->setUniform("screenHeight", sgct::Engine::instance()->getActiveYResolution());

//	------ SETUP FBO ---------------------
	_fbo = new FramebufferObject();
	_fbo->activate();

	int x = sgct::Engine::instance()->getActiveXResolution();
	int y = sgct::Engine::instance()->getActiveYResolution();
	_backTexture = new Texture(glm::size3_t(x,y,1));
	_frontTexture = new Texture(glm::size3_t(x,y,1));
	_backTexture->uploadTexture();
	_frontTexture->uploadTexture();
	_fbo->attachTexture(_backTexture, GL_COLOR_ATTACHMENT0);
	_fbo->attachTexture(_frontTexture, GL_COLOR_ATTACHMENT1);

	_fbo->deactivate();
}

void VolumeRaycaster::render() {
	glm::mat4 modelViewProjectionMatrix = sgct::Engine::instance()->getActiveModelViewProjectionMatrix();
	_FBOProgram->setUniform("modelViewProjection", modelViewProjectionMatrix);

//	------ DRAW TO FBO -------------------
	_sgctFBO = FramebufferObject::getActiveObject(); // Save SGCTs main FBO
	_fbo->activate();
	_FBOProgram->activate();

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

	_FBOProgram->deactivate();
	_fbo->deactivate();

//	------ DRAW TO SCREEN ----------------
	glBindFramebuffer(GL_FRAMEBUFFER, _sgctFBO); // Re-bind SGCTs main FBO
	_screenProgram->activate();

//	 Set textures
	glActiveTexture(GL_TEXTURE0);
	_backTexture->bind();
	glActiveTexture(GL_TEXTURE1);
	_frontTexture->bind();
	glActiveTexture(GL_TEXTURE2);
	glBindTexture(GL_TEXTURE_3D, _volumeTexture);

//	Draw screenquad
	glClearColor(0.2f, 0.2f, 0.2f, 0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glBindVertexArray(vertexArray);
		glDrawArrays(GL_TRIANGLES, 0, 6);
	glBindVertexArray(0);

	_screenProgram->deactivate();
}

void VolumeRaycaster::createVolumetexture(const char *filename, int dimensions[3]) {
	// Make sure that the texture is not used
	if(glIsTexture(_volumeTexture))
		glDeleteTextures(1, &_volumeTexture);

	int size = dimensions[0]*dimensions[1]*dimensions[2];
	GLubyte *data = new GLubyte[size];

    if( FILE *fin = fopen(filename, "rb") ){
	fread(data, sizeof(unsigned char), size, fin);
      fclose(fin);
    }
    else
      fprintf( stderr, "Could not open file '%s'\n", filename );

	glPixelStorei(GL_UNPACK_ALIGNMENT,1);
	glGenTextures(1, &_volumeTexture);
	glBindTexture(GL_TEXTURE_3D, _volumeTexture);
	glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
	glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
	glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_BORDER);
	glTexImage3D(GL_TEXTURE_3D, 0, GL_R8, dimensions[0], dimensions[1],dimensions[2],0, GL_RED, GL_UNSIGNED_BYTE,data);
	glBindTexture(GL_TEXTURE_3D, 0);
	delete []data;
	std::cout << "Volume texture created" << std::endl;
}

}// namespace openspace
