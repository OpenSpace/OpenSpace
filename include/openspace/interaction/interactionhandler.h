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
#include <map>
#include <functional>

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
	void setOrigin(SceneGraphNode* node);

	Camera * getCamera() const;
	const psc getOrigin() const;
	void lockControls();
	void unlockControls();

	void setFocusNode(SceneGraphNode *node);
	
	void orbit(const glm::quat &rotation);
	void rotate(const glm::quat &rotation);
	void distance(const PowerScaledScalar &distance, size_t iterations = 0);

	void lookAt(const glm::quat &rotation);
	void setRotation(const glm::quat &rotation);

	void update(const double dt);

	double getDt();

    void keyboardCallback(int key, int action);
    void mouseButtonCallback(int key, int action);
    void mousePositionCallback(int x, int y);
    void mouseScrollWheelCallback(int pos);

    void addKeyCallback(int key, std::function<void(void)> f);

	/**
	* Returns the Lua library that contains all Lua functions available to affect the
	* interaction. The functions contained are
	* - openspace::luascriptfunctions::printScreen
	* \return The Lua library that contains all Lua functions available to affect the
	* interaction
	*/
	static scripting::ScriptEngine::LuaLibrary luaLibrary();
	
private:
    glm::vec3 mapToTrackball(glm::vec2 mousePos);
    glm::vec3 mapToCamera(glm::vec3 trackballPos);
    void trackballRotate(int x, int y);

	Camera* camera_;
	bool enabled_;
	SceneGraphNode *node_;
	
	double dt_;


	glm::vec3 _lastTrackballPos;
	bool _leftMouseButtonDown, _isMouseBeingPressedAndHeld;

	// used for calling when updating and deallocation
	std::vector<ExternalControl*> controllers_;

	// for locking and unlocking
	std::mutex cameraGuard_;

	std::multimap<int, std::function<void(void)> > _keyCallbacks;
	
};

} // namespace openspace

#endif
