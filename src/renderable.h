#ifndef RENDERABLE_H
#define RENDERABLE_H

// open space includes
#include "object.h"
#include "util/psc.h"
#include "util/pss.h"
#include "camera.h"

namespace openspace {

class Renderable: public Object {
public:

	// constructors & destructor
	Renderable();
	Renderable(const pss &boundingSphere);
	virtual ~Renderable();
	
	void setBoundingSphere(const pss &boundingSphere);
	const pss &getBoundingSphere();

	virtual void render(const Camera *camera, const psc &thisPosition) = 0;
	virtual void update();

private:
	pss boundingSphere_;

};

} // namespace openspace

#endif