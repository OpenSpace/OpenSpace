/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include "rendering/renderengine.h"

#include "openspaceengine.h"
#include "scenegraph/scenegraph.h"
#include "scenegraph/scenegraphloader.h"
#include "util/camera.h"

#include "sgct.h"

namespace {
    const std::string _loggerCat = "RenderEngine";
}
namespace openspace {
	
RenderEngine::RenderEngine()
    : _mainCamera(nullptr)
    , _sceneGraph(nullptr)
{
}

RenderEngine::~RenderEngine() {
	delete _mainCamera;
	delete _sceneGraph;
}

bool RenderEngine::initialize(const std::string& sceneGraph) {
    // init camera and set position
    _mainCamera = new Camera();
    _mainCamera->setScaling(glm::vec2(1.0, -8.0));
    _mainCamera->setPosition(psc(0.0,0.0,1.499823,11.0)); // about the distance from the sun to our moon, will be overritten by the scenegraphloader

    // if master, setup interaction
    if (sgct::Engine::instance()->isMaster()) {
        OsEng.interactionHandler().setCamera(_mainCamera);

        // init interactionhandler and mouse interaction
        //keyboardControl_ = new KeyboardExternalControl(RELATIVE_PATH"pyinput/keyboard.py");
        //mouseControl_ = new MouseExternalControl(RELATIVE_PATH"pyinput/mouse.py");
        //InteractionHandler::ref().addExternalControl(mouseControl_); // the interactionhandler is deallocating the object when it terminates
        //InteractionHandler::ref().addExternalControl(keyboardControl_); // the interactionhandler is deallocating the object when it terminates

    }

    // init scenegraph
    _sceneGraph = new SceneGraph;
    _sceneGraph->init();
    //_sceneGraph = loadSceneGraph(sceneGraph);

    return true;
}

bool RenderEngine::initializeGL() {
	// GL settings
	glEnable (GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);
	glCullFace(GL_BACK);
	
	// set the close clip plane and the far clip plane to extreme values while in development
    sgct::Engine::instance()->setNearAndFarClippingPlanes(0.1f,100.0f);
    //sgct::Engine::setNearAndFarClippingPlanes(0.1f,10000.0f);
	//sgct::Engine::getPtr()->setNearAndFarClippingPlanes(0.1f,10000.0f);

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
		_mainCamera->setCameraDirection(glm::vec3(viewdir[0],viewdir[1],viewdir[2]));
		//mainCamera_->setMaxFov(sgct_core::SGCTSettings::Instance()->getFisheyeFOV());
        _mainCamera->setMaxFov(sgct::Engine::instance()->getWindowPtr(0)->getFisheyeFOV());
	} else {
		// get corner positions, calculating the forth to easily calculate center 
		glm::vec3 corners[4];
		corners[0] = sgct::Engine::instance()->getWindowPtr(0)->getCurrentViewport()->getViewPlaneCoords(sgct_core::Viewport::LowerLeft);
		corners[1] = sgct::Engine::instance()->getWindowPtr(0)->getCurrentViewport()->getViewPlaneCoords(sgct_core::Viewport::UpperLeft);
		corners[2] = sgct::Engine::instance()->getWindowPtr(0)->getCurrentViewport()->getViewPlaneCoords(sgct_core::Viewport::UpperRight);
		corners[3] = glm::vec3(corners[2][0],corners[0][1],corners[2][2]);
		glm::vec3 center = (corners[0] + corners[1] + corners[2] + corners[3]) / 4.0f;

		// set the eye position, useful during rendering
		const glm::vec3 eyePosition = sgct_core::ClusterManager::instance()->getUserPtr()->getPos();

		// get viewdirection, stores the direction in the camera, used for culling
		glm::vec3 viewdir = glm::normalize(eyePosition- center);
		_mainCamera->setCameraDirection(-viewdir);

		// set the initial fov to be 0.0 which means everything will be culled
		float maxFov = 0.0f;

		// for each corner
		for(int i = 0; i < 4; ++i) {

			// calculate radians to corner
			glm::vec3 dir = glm::normalize(eyePosition- corners[i]);
			float radsbetween = acos(glm::dot(viewdir, dir))/(glm::length(viewdir) * glm::length(dir));

			// the angle to a corner is larger than the current maxima
			if (radsbetween > maxFov) {
				maxFov = radsbetween;
			}
		}
		_mainCamera->setMaxFov(maxFov);
	}

	// successful init
	return true;
}

void RenderEngine::postSynchronizationPreDraw() {
	
	// converts the quaternion used to rotation matrices
	_mainCamera->compileViewRotationMatrix();

	// update and evaluate the scene starting from the root node
	//_sceneGraph->update();
	//_sceneGraph->evaluate(_mainCamera);
}

void RenderEngine::render() {
	
	// preparing the camera can only be done in the render function 
	// since the SGCT get matrix functions is only valid in the render function
	glm::mat4 projection = sgct::Engine::instance()->getActiveProjectionMatrix();
	glm::mat4 view = sgct::Engine::instance()->getActiveViewMatrix();
    const glm::vec3 eyePosition = sgct_core::ClusterManager::instance()->getUserPtr()->getPos();
	view = glm::translate(view, eyePosition); // make sure the eye is in the center

	// setup the camera for the current frame
	//_mainCamera->setViewProjectionMatrix(projection*view);

	// render the scene starting from the root node
	//_sceneGraph->render(_mainCamera);
	/*
	if (sgct::Engine::instance()->isMaster()) {
		const glm::vec2 scaling = _mainCamera->getScaling();
		const glm::vec3 viewdirection = _mainCamera->getViewDirection();
		const psc position = _mainCamera->getPosition();
		Freetype::print(sgct_text::FontManager::instance()->getFont( "SGCTFont", 10 ), 10,  50, 
			"Position: (%.5f, %.5f, %.5f, %.5f)", position[0], position[1], position[2], position[3]
		);
		Freetype::print(sgct_text::FontManager::instance()->getFont( "SGCTFont", 10 ), 10,  35, 
			"View direction: (%.3f, %.3f, %.3f)", viewdirection[0], viewdirection[1], viewdirection[2]
		);
		Freetype::print(sgct_text::FontManager::instance()->getFont( "SGCTFont", 10 ), 10,  20, 
			"Scaling: (%.10f, %.2f)", scaling[0], scaling[1]
		);

        psc campos = _mainCamera->getPosition();
        psc origin = OsEng.interactionHandler().getOrigin();
		//psc campos = InteractionHandler::ref().getCamera()->getPosition();
		//psc origin = InteractionHandler::ref().getOrigin();
		psc relative = campos - origin;
		pss pssl = relative.length();
		//mainCamera_->setScaling(glm::vec2(pssl[0], -pssl[1]+6));
		//mainCamera_->setScaling(glm::vec2(3000.0, -11.0f));
		Freetype::print(sgct_text::FontManager::instance()->getFont( "SGCTFont", 10 ), 10,  65, 
			"Distance to origin: (%.15f, %.2f)", pssl[0], pssl[1]
		);
	}
     */
}

SceneGraph* RenderEngine::sceneGraph() {
    // TODO custom assert (ticket #5)
    assert(_sceneGraph);
    return _sceneGraph;
}


/*
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
*/

} // namespace openspace
