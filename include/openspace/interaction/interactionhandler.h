#ifndef INTERACTIONHANDLER_H
#define INTERACTIONHANDLER_H

// open space includes
#include <openspace/util/camera.h>
#include <openspace/interaction/externalcontrol/externalcontrol.h>
#include <openspace/scenegraph/scenegraphnode.h>

// std includes
#include <vector>
#include <thread>
#include <mutex>
#include <memory>

namespace openspace {

class InteractionHandler {
public:
    InteractionHandler(void);
    InteractionHandler(const InteractionHandler& src);
    InteractionHandler& operator=(const InteractionHandler& rhs);
	virtual ~InteractionHandler();

	//static void init();
	//static void deinit();
 //   static InteractionHandler& ref();
	//static bool isInitialized();

	void enable();
	void disable();
	const bool isEnabled() const;

	void connectDevices();
	void addExternalControl(ExternalControl* controller);

	void setCamera(Camera *camera = nullptr);
	Camera * getCamera() const;
	const psc getOrigin() const;
	void lockControls();
	void unlockControls();

	void setFocusNode(SceneGraphNode *node);
	
	void orbit(const glm::quat &rotation);
	void rotate(const glm::quat &rotation);
	void distance(const pss &distance);

	void lookAt(const glm::quat &rotation);
	void setRotation(const glm::quat &rotation);

	void update(const double dt);

	double getDt();

    void keyboardCallback(int key, int action);
    void mouseButtonCallback(int key, int action);
    void mousePositionCallback(int x, int y);
    void mouseScrollWheelCallback(int pos);
	
private:
    glm::vec3 mapToTrackball(glm::vec2 mousePos);

	Camera *camera_;
	bool enabled_;
	SceneGraphNode *node_;
	
	double dt_;

	// used for calling when updating and deallocation
	std::vector<ExternalControl*> controllers_;

	// for locking and unlocking
	std::mutex cameraGuard_;
	
};

} // namespace openspace

#endif
