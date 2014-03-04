#ifndef RENDERABLE_H
#define RENDERABLE_H

// open space includes
#include "util/psc.h"
#include "util/pss.h"
#include "util/camera.h"
#include <ghoul/misc/dictionary.h>

namespace openspace {

class Renderable {
public:

	// constructors & destructor
	Renderable();
    virtual ~Renderable();
	
    virtual void initialize(ghoul::Dictionary* dictionary) = 0;
    
	void setBoundingSphere(const pss &boundingSphere);
	const pss &getBoundingSphere();

	virtual void render(const Camera *camera, const psc &thisPosition) = 0;
	virtual void update();

private:
	pss boundingSphere_;

};

} // namespace openspace

#endif