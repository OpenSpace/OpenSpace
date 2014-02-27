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
	void createVolumetexture(const char *filename, int dimensions[3]);

	FramebufferObject* _fbo;
	Texture* _backTexture;
	Texture* _frontTexture;
	Texture* _volume;

	GLuint _volumeTexture;
	GLuint _sgctFBO;

	ProgramObject* _FBOProgram;
	ProgramObject* _screenProgram;
	sgct_utils::SGCTBox* myBox;
};

} // namespace openspace

#endif // VOLUMERAYCASTER_H
