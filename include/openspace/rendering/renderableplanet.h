#ifndef RENDERABLEPLANET_H
#define RENDERABLEPLANET_H

// open space includes
#include <openspace/rendering/renderable.h>
#include <openspace/util/powerscaledsphere.h>

// ghoul includes
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>

namespace openspace {

class RenderablePlanet: public Renderable {
public:

	// constructors & destructor
	RenderablePlanet(const ghoul::Dictionary& dictionary);
	~RenderablePlanet();

    void setName(std::string name);
    const std::string& name() const override;
    
    bool initialize();
    bool deinitialize();

	void setProgramObject(ghoul::opengl::ProgramObject *programObject = nullptr);
	void setTexture(ghoul::opengl::Texture *texture);

	virtual void render(const Camera *camera, const psc &thisPosition);
	virtual void update();

private:
    std::string _name;

    // shader
    ghoul::opengl::ProgramObject* _programObject;

    // texture
    std::string _texturePath;
    ghoul::opengl::Texture* _texture;

    // Object
    PowerScaledSphere* _planet;
};

} // namespace openspace

#endif