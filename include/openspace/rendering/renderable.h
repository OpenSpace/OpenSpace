#ifndef RENDERABLE_H
#define RENDERABLE_H

// open space includes
#include <openspace/util/psc.h>
#include <openspace/util/pss.h>
#include <openspace/util/camera.h>
#include <ghoul/misc/dictionary.h>

namespace openspace {

class Renderable {
public:
    
	// constructors & destructor
	Renderable(const ghoul::Dictionary& dictionary);
    virtual ~Renderable();
	
    virtual bool initialize() = 0;
    virtual bool deinitialize() = 0;
    
	void setBoundingSphere(const pss &boundingSphere);
	const pss &getBoundingSphere();

	virtual void render(const Camera *camera, const psc &thisPosition) = 0;
	virtual void update();
protected:
	Renderable();
private:
	pss boundingSphere_;

};

} // namespace openspace

#endif