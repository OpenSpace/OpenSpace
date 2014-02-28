#ifndef VOLUMERAYCASTER_H
#define VOLUMERAYCASTER_H

#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/opengl/texture.h>

#include <sgct.h>
#include <cstdio>

namespace openspace {
using namespace ghoul::opengl;

class VolumeRaycaster {
public:
	VolumeRaycaster();
	~VolumeRaycaster();
	void initialize();
	void render();

private:
	Texture* createVolumetexture(const char *filename, glm::ivec3 dimensions);

	FramebufferObject* _fbo;
	Texture* _backTexture;
	Texture* _frontTexture;
	Texture* _volume;

	GLuint _sgctFBO;

	ProgramObject *_fboProgram, *_twopassProgram, *_singlepassProgram;
	sgct_utils::SGCTBox* myBox;


};

} // namespace openspace

#endif // VOLUMERAYCASTER_H
