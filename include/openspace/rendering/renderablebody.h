#ifndef RENDERABLEBODY_H
#define RENDERABLEBODY_H

// open space includes
#include <openspace/rendering/renderable.h>
#include <openspace/util/sphere.h>

// ghoul includes
#include "ghoul/opengl/programobject.h"
#include "ghoul/opengl/texture.h"

namespace openspace {
/*
class RenderableBody: public Renderable {
public:

	// constructors & destructor
	RenderableBody(const pss &radius);
	~RenderableBody();

	void setProgramObject(ghoul::opengl::ProgramObject *programObject);
	void setTexture(ghoul::opengl::Texture *texture);

	virtual void render(const Camera *camera, const psc &thisPosition);
	virtual void update();

private:
	ghoul::opengl::ProgramObject *programObject_;
	ghoul::opengl::Texture *texture_;
	double rad_;

	gl4::Sphere *planet_;
};
*/
} // namespace openspace

#endif