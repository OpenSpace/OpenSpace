#ifndef RENDERENGINE_H
#define RENDERENGINE_H

// open space includes
#include "object.h"
#include "camera.h"
#include "scenegraph/scenegraph.h"
#include "interactionhandler.h"
#include "externalcontrol/mouseexternalcontrol.h"
#include "externalcontrol/keyboardexternalcontrol.h"

// ghoul includes
#include "ghoul/logging/logmanager.h"
#include "ghoul/logging/consolelog.h"

namespace openspace {

class RenderEngine: public Object {
public:

	// constructors & destructor
	RenderEngine(int argc, char **argv);
	~RenderEngine();
	
	// sgct wrapped functions
	bool init();
	void preSync();
	void postSyncPreDraw();
	void render();
	void postDraw();
	void keyboardCallback(int key, int action);
	void mouseButtonCallback(int key, int action);
	void mousePosCallback(int x, int y);
	void mouseScrollCallback(int pos);

	// object extensions
	virtual void encode();
	virtual void decode();
	
private:
	double masterTime_;
	Camera *mainCamera_;
	//MouseExternalControl *mouseControl_;
	//KeyboardExternalControl *keyboardControl_;

	SceneGraph *sceneGraph_;
	glm::vec3 eyePosition_;
};

} // namespace openspace

#endif
