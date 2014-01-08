
// open space includes
#include "renderengine.h"
#include "deviceidentifier.h"
#include "util/spice.h"
#include "util/planet.h"
#include "util/time.h"
//#include "python/pythonmanager.h"
#include "renderablebody.h"

// ghoul includes
#include "ghoul/opengl/texturereader.h"
#include "ghoul/opengl/texture.h"

// sgct includes
#include "sgct.h"

const std::string _loggerCat = "RenderEngine";

namespace openspace {
	
	RenderEngine::RenderEngine(int argc, char **argv) {

	// initialize all pointers as nullptr.
	mainCamera_ = nullptr;
	//mouseControl_ = nullptr;
	//keyboardControl_ = nullptr;
	sceneGraph_ = nullptr;
}

RenderEngine::~RenderEngine() {

	// finalize python
	//PythonManager::finalize();
	
	// finalize singletons
	if(Spice::isInitialized()) {
		Spice::deinit();
	}
	if(Time::isInitialized()) {
		Time::deinit();
	}
	if(InteractionHandler::isInitialized()) {
		InteractionHandler::deinit();
	}
	if(DeviceIdentifier::isInitialized()) {
		DeviceIdentifier::deinit();
	}
	
	// remove camera
	if(mainCamera_)
		delete mainCamera_;

	// deallocate the scene graph
	if(sceneGraph_)
		delete sceneGraph_;
}

bool RenderEngine::init() {

	// init ghoul logging
	std::string _loggerCat = "RenderEngine::init";
	ghoul::logging::LogManager::initialize(ghoul::logging::LogManager::LogLevel::Info);
    LogMgr.addLog(new ghoul::logging::ConsoleLog);
	
	// init python
	//PythonManager::init();

	// init singletons
	DeviceIdentifier::init();
	DeviceIdentifier::ref().scanDevices();
	InteractionHandler::init();
	InteractionHandler::ref().connectDevices();
	Time::init();
	Spice::init();
	Spice::ref().loadDefaultKernels();

	Planet pl(pss(), 4);
	
	// GL settings
	glEnable (GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);
	glCullFace(GL_BACK);
	
	// set the close clip plane and the far clip plane to extreme values while in development
    sgct::Engine::instance()->setNearAndFarClippingPlanes(0.1f,10000.0f);
    //sgct::Engine::setNearAndFarClippingPlanes(0.1f,10000.0f);
	//sgct::Engine::getPtr()->setNearAndFarClippingPlanes(0.1f,10000.0f);

	// init camera and set position
	mainCamera_ = new Camera();
	mainCamera_->setScaling(glm::vec2(1.0, -8.0));
	mainCamera_->setPosition(psc(0.0,0.0,1.499823,11.0)); // about the distance from the sun to our moon, will be overritten by the scenegraphloader
	
	// if master, setup interaction
	if (sgct::Engine::instance()->isMaster()) {
		InteractionHandler::ref().setCamera(mainCamera_);
		
		// init interactionhandler and mouse interaction
		//keyboardControl_ = new KeyboardExternalControl(RELATIVE_PATH"pyinput/keyboard.py");
		//mouseControl_ = new MouseExternalControl(RELATIVE_PATH"pyinput/mouse.py");
		//InteractionHandler::ref().addExternalControl(mouseControl_); // the interactionhandler is deallocating the object when it terminates
		//InteractionHandler::ref().addExternalControl(keyboardControl_); // the interactionhandler is deallocating the object when it terminates
		
	}

	// init scenegraph
	sceneGraph_ = new SceneGraph();
	sceneGraph_->init();

	// set some random seed, should be random for release
	srand(1000);

	// calculating the maximum field of view for the camera, used to determine visibility of objects in the scene graph
	if(sgct::Engine::instance()->getWindowPtr(0)->isUsingFisheyeRendering()) {

		// fisheye mode, looking upwards to the "dome"
		glm::vec4 viewdir(0,1,0,0);

		// get the tilt and rotate the view
        float tilt = sgct::Engine::instance()->getWindowPtr(0)->getFisheyeTilt();
		//tilt = tilt * 0.0174532925; // degrees to radians
		glm::mat4 tiltMatrix = glm::rotate(glm::mat4(1.0f), tilt, glm::vec3(1.0f,0.0f,0.0f));
		viewdir = tiltMatrix * viewdir;

		// set the tilted view and the FOV
		mainCamera_->setCameraDirection(glm::vec3(viewdir[0],viewdir[1],viewdir[2]));
		//mainCamera_->setMaxFov(sgct_core::SGCTSettings::Instance()->getFisheyeFOV());
        mainCamera_->setMaxFov(sgct::Engine::instance()->getWindowPtr(0)->getFisheyeFOV());
	} else {

		// get corner positions, calculating the forth to easily calculate center 
		glm::vec3 corners[4];
		corners[0] = sgct::Engine::instance()->getWindowPtr(0)->getCurrentViewport()->getViewPlaneCoords(sgct_core::Viewport::LowerLeft);
		corners[1] = sgct::Engine::instance()->getWindowPtr(0)->getCurrentViewport()->getViewPlaneCoords(sgct_core::Viewport::UpperLeft);
		corners[2] = sgct::Engine::instance()->getWindowPtr(0)->getCurrentViewport()->getViewPlaneCoords(sgct_core::Viewport::UpperRight);
		corners[3] = glm::vec3(corners[2][0],corners[0][1],corners[2][2]);
		glm::vec3 center = (corners[0] + corners[1] + corners[2] + corners[3]) / 4.0f;

		// set the eye position, useful during rendering
		eyePosition_ = sgct_core::ClusterManager::instance()->getUserPtr()->getPos();

		// get viewdirection, stores the direction in the camera, used for culling
		glm::vec3 viewdir = glm::normalize(eyePosition_- center);
		mainCamera_->setCameraDirection(-viewdir);

		// set the initial fov to be 0.0 which means everything will be culled
		float maxFov = 0.0f;

		// for each corner
		for(int i = 0; i < 4; ++i) {

			// calculate radians to corner
			glm::vec3 dir = glm::normalize(eyePosition_- corners[i]);
			float radsbetween = acos(glm::dot(viewdir, dir))/(glm::length(viewdir) * glm::length(dir));

			// the angle to a corner is larger than the current maxima
			if (radsbetween > maxFov) {
				maxFov = radsbetween;
			}
		}
		mainCamera_->setMaxFov(maxFov);
	}

	// successfull init
	return true;
}

void RenderEngine::preSync() {
	
	if (sgct::Engine::instance()->isMaster()) {
		// get time variables
		masterTime_ = sgct::Engine::instance()->getTime();
		double dt = sgct::Engine::instance()->getDt();

		// do interaction handling
		InteractionHandler::ref().update(dt);
		// lock controls so camera is not updated during rendering
		InteractionHandler::ref().lockControls();
	}
	
}

void RenderEngine::postSyncPreDraw() {
	
	// converts the quaternion used to rotation matrices
	mainCamera_->compileViewRotationMatrix();

	// update and evaluate the scene starting from the root node
	sceneGraph_->update();
	sceneGraph_->evaluate(mainCamera_);
}

void RenderEngine::render() {
	
	// preparing the camera can only be done in the render function 
	// since the SGCT get matrix functions is only valid in the render function
	glm::mat4 projection = sgct::Engine::instance()->getActiveProjectionMatrix();
	glm::mat4 view = sgct::Engine::instance()->getActiveViewMatrix();
	view = glm::translate(view, eyePosition_); // make sure the eye is in the center

	// setup the camera for the current frame
	mainCamera_->setViewProjectionMatrix(projection*view);

	// render the scene starting from the root node
	sceneGraph_->render(mainCamera_);
	
	if(sgct::Engine::instance()->isMaster()) {
		const glm::vec2 scaling = mainCamera_->getScaling();
		const glm::vec3 viewdirection = mainCamera_->getViewDirection();
		const psc position = mainCamera_->getPosition();
		Freetype::print(sgct_text::FontManager::instance()->getFont( "SGCTFont", 10 ), 10,  50, 
			"Position: (%.5f, %.5f, %.5f, %.5f)", position[0], position[1], position[2], position[3]
		);
		Freetype::print(sgct_text::FontManager::instance()->getFont( "SGCTFont", 10 ), 10,  35, 
			"View direction: (%.3f, %.3f, %.3f)", viewdirection[0], viewdirection[1], viewdirection[2]
		);
		Freetype::print(sgct_text::FontManager::instance()->getFont( "SGCTFont", 10 ), 10,  20, 
			"Scaling: (%.10f, %.2f)", scaling[0], scaling[1]
		);

		psc campos = InteractionHandler::ref().getCamera()->getPosition();
		psc origin = InteractionHandler::ref().getOrigin();
		psc relative = campos - origin;
		pss pssl = relative.length();
		//mainCamera_->setScaling(glm::vec2(pssl[0], -pssl[1]+6));
		//mainCamera_->setScaling(glm::vec2(3000.0, -11.0f));
		Freetype::print(sgct_text::FontManager::instance()->getFont( "SGCTFont", 10 ), 10,  65, 
			"Distance to origin: (%.15f, %.2f)", pssl[0], pssl[1]
		);
	}
}

void RenderEngine::postDraw() {
	
	// unlock controls so the camera can be updated again
	if (sgct::Engine::instance()->isMaster()) {
		InteractionHandler::ref().unlockControls();
	}
	
}

void RenderEngine::keyboardCallback(int key, int action) {
    const double speed = 0.75;
    if (key == 'S') {
        double dt = InteractionHandler::ref().getDt();
        glm::vec3 euler(speed * dt, 0.0, 0.0);
        glm::quat rot = glm::quat(euler);
        InteractionHandler::ref().orbit(rot);
    }
    if (key == 'W') {
        double dt = InteractionHandler::ref().getDt();
        glm::vec3 euler(-speed * dt, 0.0, 0.0);
        glm::quat rot = glm::quat(euler);
        InteractionHandler::ref().orbit(rot);
    }
    if (key == 'A') {
        double dt = InteractionHandler::ref().getDt();
        glm::vec3 euler(0.0, -speed * dt, 0.0);
        glm::quat rot = glm::quat(euler);
        InteractionHandler::ref().orbit(rot);
    }
    if (key == 'D') {
        double dt = InteractionHandler::ref().getDt();
        glm::vec3 euler(0.0, speed * dt, 0.0);
        glm::quat rot = glm::quat(euler);
        InteractionHandler::ref().orbit(rot);
    }
    if (key == 262) {
        double dt = InteractionHandler::ref().getDt();
        glm::vec3 euler(0.0, speed * dt, 0.0);
        glm::quat rot = glm::quat(euler);
        InteractionHandler::ref().rotate(rot);
    }
    if (key == 263) {
        double dt = InteractionHandler::ref().getDt();
        glm::vec3 euler(0.0, -speed * dt, 0.0);
        glm::quat rot = glm::quat(euler);
        InteractionHandler::ref().rotate(rot);
    }
    if (key == 264) {
        double dt = InteractionHandler::ref().getDt();
        glm::vec3 euler(speed * dt, 0.0, 0.0);
        glm::quat rot = glm::quat(euler);
        InteractionHandler::ref().rotate(rot);
    }
    if (key == 265) {
        double dt = InteractionHandler::ref().getDt();
        glm::vec3 euler(-speed * dt, 0.0, 0.0);
        glm::quat rot = glm::quat(euler);
        InteractionHandler::ref().rotate(rot);
    }
    if (key == 'R') {
        double dt = InteractionHandler::ref().getDt();
        pss dist(3 * -speed * dt, 8.0);
        InteractionHandler::ref().distance(dist);
    }
    if (key == 'F') {
        double dt = InteractionHandler::ref().getDt();
        pss dist(3 * speed * dt, 8.0);
        InteractionHandler::ref().distance(dist);
    }
    if (key == '1') {
        SceneGraphNode* earth = sceneGraph_->root()->get("sun");

        InteractionHandler::ref().setFocusNode(earth);
        InteractionHandler::ref().getCamera()->setPosition(earth->getWorldPosition() + psc(0.0, 0.0, 0.5, 10.0));
        InteractionHandler::ref().getCamera()->setCameraDirection(glm::vec3(0.0, 0.0, -1.0));
    }

    if (key == '2') {
        SceneGraphNode* earth = sceneGraph_->root()->get("earth");

        InteractionHandler::ref().setFocusNode(earth);
        InteractionHandler::ref().getCamera()->setPosition(earth->getWorldPosition() + psc(0.0, 0.0, 1.0, 8.0));
        InteractionHandler::ref().getCamera()->setCameraDirection(glm::vec3(0.0, 0.0, -1.0));
    }


    if (key == '3') {
        SceneGraphNode* earth = sceneGraph_->root()->get("moon");

        InteractionHandler::ref().setFocusNode(earth);
        InteractionHandler::ref().getCamera()->setPosition(earth->getWorldPosition() + psc(0.0, 0.0, 0.5, 8.0));
        InteractionHandler::ref().getCamera()->setCameraDirection(glm::vec3(0.0, 0.0, -1.0));
    }

}

void RenderEngine::mouseButtonCallback(int key, int action) {
	//if(mouseControl_ != nullptr) {
	//	mouseControl_->mouseButtonCallback(key,action);
	//}
}

void RenderEngine::mousePosCallback(int x, int y) {
	//if(mouseControl_ != nullptr) {
	//	mouseControl_->mousePosCallback(x,y);
	//}
}

void RenderEngine::mouseScrollCallback(int pos) {
	//if(mouseControl_ != nullptr) {
	//	mouseControl_->mouseScrollCallback(pos);
	//}
}

void RenderEngine::encode() {
	
	// allocate a sgct shared double for syncing
	sgct::SharedDouble *shDouble = new sgct::SharedDouble();

	// sync the time
	shDouble->setVal(masterTime_);
	sharedDataInstance_->writeDouble(shDouble);
	
	// check that the camera has been allocated
	if(mainCamera_ != nullptr) {

		// sync position
		psc campos = mainCamera_->getPosition();
		for(int i = 0; i < 4; i++) {
			shDouble->setVal(campos[i]);
			sharedDataInstance_->writeDouble(shDouble);
		}

		// sync view direction
		glm::quat camrot = mainCamera_->getRotation();
		for(int i = 0; i < 4; i++) {
			shDouble->setVal(camrot[i]);
			sharedDataInstance_->writeDouble(shDouble);
		}
	}
	
	// deallocate
	delete shDouble;
	
}

void RenderEngine::decode() {
	
	// allocate a sgct shared double for syncing
	sgct::SharedDouble *shDouble = new sgct::SharedDouble();
	
	// sync the time
	sharedDataInstance_->readDouble(shDouble);
	masterTime_ = shDouble->getVal();
	
	// check that the camera has been allocated
	if(mainCamera_ != nullptr) {

		// sync position
		psc campos;
		for(int i = 0; i < 4; i++) {
			sharedDataInstance_->readDouble(shDouble);
			campos[i] = shDouble->getVal();
		}
		mainCamera_->setPosition(campos);

		// sync view direction
		glm::quat camrot;
		for(int i = 0; i < 4; i++) {
			sharedDataInstance_->readDouble(shDouble);
			camrot[i] = static_cast<float>(shDouble->getVal());
		}
		mainCamera_->setRotation(camrot);
	}
	
	// deallocate
	delete shDouble;
	
}

} // namespace openspace
