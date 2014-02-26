#ifndef VOLUMERAYCASTER_H
#define VOLUMERAYCASTER_H

#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/opengl/texture.h>

#include <util/camera.h>
#include <sgct.h>

namespace openspace {
using namespace ghoul::opengl;

class VolumeRaycaster {
public:
	VolumeRaycaster();
	VolumeRaycaster(Camera* camera);
	~VolumeRaycaster();
	void initialize();
	void render();

private:
	void loadUniforms();

	FramebufferObject* _fbo;
	Texture* _texture;
	Texture* _test;

	Camera* _camera;
	ProgramObject* _program;
	sgct_utils::SGCTBox* myBox;
};

} // namespace openspace

#endif // VOLUMERAYCASTER_H
