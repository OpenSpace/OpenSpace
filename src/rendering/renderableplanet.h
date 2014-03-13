#ifndef RENDERABLEPLANET_H
#define RENDERABLEPLANET_H

// open space includes
#include "renderable.h"
#include <util/powerscaledsphere.h>

// ghoul includes
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>

namespace openspace {

class RenderablePlanet: public Renderable {
public:

	// constructors & destructor
	RenderablePlanet();
	~RenderablePlanet();
    
    bool initialize();
    bool initializeWithDictionary(ghoul::Dictionary* dictionary);

	void setProgramObject(ghoul::opengl::ProgramObject *programObject);
	void setTexture(ghoul::opengl::Texture *texture);

	virtual void render(const Camera *camera, const psc &thisPosition);
	virtual void update();

private:
	ghoul::opengl::ProgramObject *programObject_;
	ghoul::opengl::Texture *texture_;
	PowerScaledSphere *planet_;
};

} // namespace openspace

#endif