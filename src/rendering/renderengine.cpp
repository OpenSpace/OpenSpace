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

#include <openspace/rendering/renderengine.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/scenegraph/scenegraph.h>
#include <openspace/util/camera.h>
#include <openspace/util/time.h>

// We need to decide where this is set
#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>
#include "sgct.h"

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/lua/lua_helper.h>

#include <array>
#include <fstream>

#include <openspace/abuffer/abufferSingleLinked.h>
#include <openspace/abuffer/abufferfixed.h>
#include <openspace/abuffer/abufferdynamic.h>

namespace {
	const std::string _loggerCat = "RenderEngine";
}

namespace openspace {

namespace luascriptfunctions {

/**
 * \ingroup LuaScripts
 * printImage():
 * Save the rendering to an image file
 */
int printImage(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 0)
		return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);
	sgct::Engine::instance()->takeScreenshot();
	return 0;
}

} // namespace luascriptfunctions


RenderEngine::RenderEngine()
    : _mainCamera(nullptr)
    , _sceneGraph(nullptr)
    , _abuffer(nullptr)
{
}

RenderEngine::~RenderEngine()
{
    delete _mainCamera;
}

bool RenderEngine::initialize()
{
	generateGlslConfig();

    // LDEBUG("RenderEngine::initialize()");
    // init camera and set temporary position and scaling
    _mainCamera = new Camera();
    _mainCamera->setScaling(glm::vec2(1.0, -8.0));
    _mainCamera->setPosition(psc(0.f, 0.f, 1.499823f, 11.f));

    // if master, setup interaction
    //if (sgct::Engine::instance()->isMaster())
        OsEng.interactionHandler().setCamera(_mainCamera);
#if ABUFFER_IMPLEMENTATION == ABUFFER_SINGLE_LINKED
    _abuffer = new ABufferSingleLinked();
#elif ABUFFER_IMPLEMENTATION == ABUFFER_FIXED
    _abuffer = new ABufferFixed();
#elif ABUFFER_IMPLEMENTATION == ABUFFER_DYNAMIC
    _abuffer = new ABufferDynamic();
#endif
    return true;
}

bool RenderEngine::initializeGL()
{
    // LDEBUG("RenderEngine::initializeGL()");
    sgct::SGCTWindow* wPtr = sgct::Engine::instance()->getActiveWindowPtr();

    // TODO:    Fix the power scaled coordinates in such a way that these values can be
    // set
    //          to more realistic values

    // set the close clip plane and the far clip plane to extreme values while in
    // development
     sgct::Engine::instance()->setNearAndFarClippingPlanes(0.01f,10000.0f);
    //  sgct::Engine::instance()->setNearAndFarClippingPlanes(0.1f, 1000.00f);
  //  sgct::Engine::instance()->setNearAndFarClippingPlanes(0.0001f, 100.0f);
   // sgct::Engine::instance()->setNearAndFarClippingPlanes(0.1f, 200.0f);

    // calculating the maximum field of view for the camera, used to
    // determine visibility of objects in the scene graph
    if (wPtr->isUsingFisheyeRendering()) {
        // fisheye mode, looking upwards to the "dome"
        glm::vec4 upDirection(0, 1, 0, 0);

        // get the tilt and rotate the view
        const float tilt = wPtr->getFisheyeTilt();
        glm::mat4 tiltMatrix
              = glm::rotate(glm::mat4(1.0f), tilt, glm::vec3(1.0f, 0.0f, 0.0f));
        const glm::vec4 viewdir = tiltMatrix * upDirection;

        // set the tilted view and the FOV
        _mainCamera->setCameraDirection(glm::vec3(viewdir[0], viewdir[1], viewdir[2]));
        _mainCamera->setMaxFov(wPtr->getFisheyeFOV());
    } else {
        // get corner positions, calculating the forth to easily calculate center
        glm::vec3 corners[4];
        corners[0] = wPtr->getCurrentViewport()->getViewPlaneCoords(
              sgct_core::Viewport::LowerLeft);
        corners[1] = wPtr->getCurrentViewport()->getViewPlaneCoords(
              sgct_core::Viewport::UpperLeft);
        corners[2] = wPtr->getCurrentViewport()->getViewPlaneCoords(
              sgct_core::Viewport::UpperRight);
        corners[3] = glm::vec3(corners[2][0], corners[0][1], corners[2][2]);
        const glm::vec3 center = (corners[0] + corners[1] + corners[2] + corners[3])
                                 / 4.0f;

        // set the eye position, useful during rendering
        const glm::vec3 eyePosition
              = sgct_core::ClusterManager::instance()->getUserPtr()->getPos();

        // get viewdirection, stores the direction in the camera, used for culling
        const glm::vec3 viewdir = glm::normalize(eyePosition - center);
        _mainCamera->setCameraDirection(-viewdir);
        _mainCamera->setLookUpVector(glm::vec3(0.0, 1.0, 0.0));

        // set the initial fov to be 0.0 which means everything will be culled
        float maxFov = 0.0f;

        // for each corner
        for (int i = 0; i < 4; ++i) {
            // calculate radians to corner
            glm::vec3 dir = glm::normalize(eyePosition - corners[i]);
            float radsbetween = acos(glm::dot(viewdir, dir))
                                / (glm::length(viewdir) * glm::length(dir));

            // the angle to a corner is larger than the current maxima
            if (radsbetween > maxFov) {
                maxFov = radsbetween;
            }
        }
	//	std::cout << maxFov << std::endl;
        _mainCamera->setMaxFov(maxFov);
    }

    _abuffer->initialize();

    // successful init
    return true;
}

void RenderEngine::postSynchronizationPreDraw()
{
	sgct_core::SGCTNode * thisNode = sgct_core::ClusterManager::instance()->getThisNodePtr();
	for (unsigned int i = 0; i < thisNode->getNumberOfWindows(); i++)
		if (sgct::Engine::instance()->getWindowPtr(i)->isWindowResized())
			generateGlslConfig();
	// Move time forward.
	//_runtimeData->advanceTimeBy(1, DAY);
	
	//_runtimeData->advanceTimeBy(1, HOUR);
	//_runtimeData->advanceTimeBy(30, MINUTE);
	//_runtimeData->advanceTimeBy(1, MILLISECOND);
	
    // converts the quaternion used to rotation matrices
    _mainCamera->compileViewRotationMatrix();
	UpdateData a = { Time::ref().currentTime(), Time::ref().deltaTime() };

	//std::cout << a.delta << std::endl;
    // update and evaluate the scene starting from the root node
	_sceneGraph->update(a);
    _mainCamera->setCameraDirection(glm::vec3(0, 0, -1));
    _sceneGraph->evaluate(_mainCamera);
}

void RenderEngine::render()
{
    // SGCT resets certain settings
    //glEnable(GL_DEPTH_TEST);
    //glEnable(GL_CULL_FACE);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_CULL_FACE);

    // setup the camera for the current frame
    const glm::vec3 eyePosition
          = sgct_core::ClusterManager::instance()->getUserPtr()->getPos();
    const glm::mat4 view
          = glm::translate(glm::mat4(1.0),
                           eyePosition);  // make sure the eye is in the center
    _mainCamera->setViewProjectionMatrix(
          sgct::Engine::instance()->getActiveModelViewProjectionMatrix() * view);

	_mainCamera->setModelMatrix(
		sgct::Engine::instance()->getModelMatrix());
	
	_mainCamera->setViewMatrix(
		sgct::Engine::instance()->getActiveViewMatrix()* view);

	_mainCamera->setProjectionMatrix(
		sgct::Engine::instance()->getActiveProjectionMatrix());


    // render the scene starting from the root node
    _abuffer->clear();
    _abuffer->preRender();
	_sceneGraph->render({*_mainCamera, psc()});
    _abuffer->postRender();
    _abuffer->resolve();

#ifndef OPENSPACE_VIDEO_EXPORT
    // Print some useful information on the master viewport
    if (sgct::Engine::instance()->isMaster()) {
// Apple usually has retina screens
#ifdef __APPLE__
#define FONT_SIZE 18
#else
#define FONT_SIZE 10
#endif


        const glm::vec2 scaling = _mainCamera->scaling();
        const glm::vec3 viewdirection = _mainCamera->viewDirection();
        const psc position = _mainCamera->position();
        const psc origin = OsEng.interactionHandler().getOrigin();
        const PowerScaledScalar pssl = (position - origin).length();

		/* GUI PRINT */

		std::string&& time = Time::ref().currentTimeUTC().c_str();
		Freetype::print(
			  sgct_text::FontManager::instance()->getFont("SGCTFont", FONT_SIZE),
			  FONT_SIZE, FONT_SIZE * 18, "Date: %s", time.c_str()
			  );
		Freetype::print(
			  sgct_text::FontManager::instance()->getFont("SGCTFont", FONT_SIZE),
			  FONT_SIZE, FONT_SIZE * 16, "Avg. Frametime: %.10f", sgct::Engine::instance()->getAvgDt()
			  );
		Freetype::print(
			  sgct_text::FontManager::instance()->getFont("SGCTFont", FONT_SIZE),
			  FONT_SIZE, FONT_SIZE * 14, "Drawtime: %.10f", sgct::Engine::instance()->getDrawTime()
			  );
		Freetype::print(
			  sgct_text::FontManager::instance()->getFont("SGCTFont", FONT_SIZE),
			  FONT_SIZE, FONT_SIZE * 12, "Frametime: %.10f", sgct::Engine::instance()->getDt()
			  );
        Freetype::print(
              sgct_text::FontManager::instance()->getFont("SGCTFont", FONT_SIZE),
              FONT_SIZE, FONT_SIZE * 10, "Origin: (%.5f, %.5f, %.5f, %.5f)", origin[0],
              origin[1], origin[2], origin[3]);
        Freetype::print(
              sgct_text::FontManager::instance()->getFont("SGCTFont", FONT_SIZE),
              FONT_SIZE, FONT_SIZE * 8, "Camera position: (%.5f, %.5f, %.5f, %.5f)",
              position[0], position[1], position[2], position[3]);
        Freetype::print(
              sgct_text::FontManager::instance()->getFont("SGCTFont", FONT_SIZE),
              FONT_SIZE, FONT_SIZE * 6, "Distance to origin: (%.15f, %.2f)", pssl[0],
              pssl[1]);
        Freetype::print(
              sgct_text::FontManager::instance()->getFont("SGCTFont", FONT_SIZE),
              FONT_SIZE, FONT_SIZE * 4, "View direction: (%.3f, %.3f, %.3f)",
              viewdirection[0], viewdirection[1], viewdirection[2]);
        Freetype::print(
              sgct_text::FontManager::instance()->getFont("SGCTFont", FONT_SIZE),
              FONT_SIZE, FONT_SIZE * 2, "Scaling: (%.10f, %.2f)", scaling[0], scaling[1]);

    }
#endif
    
}

SceneGraph* RenderEngine::sceneGraph()
{
    // TODO custom assert (ticket #5)
    assert(_sceneGraph);
    return _sceneGraph;
}

void RenderEngine::setSceneGraph(SceneGraph* sceneGraph)
{
    _sceneGraph = sceneGraph;
}

void RenderEngine::serialize(std::vector<char>& dataStream, size_t& offset) {
    // TODO: This has to be redone properly (ab) [new class providing methods to serialize
    // variables]

    // _viewRotation
    // _cameraDirection
    // camera->position
    // camera->viewRotationMatrix
    // camera->scaling


    //const glm::vec2 scaling = _mainCamera->scaling();
    //const psc position = _mainCamera->position();
    ////const psc origin = OsEng.interactionHandler().getOrigin();
    ////const pss pssl = (position - origin).length();
    ////_mainCamera->cameraDirection()

    //union storage {
    //    float value;
    //    std::array<char, 4> representation;
    //} s;

    //s.value = _mainCamera->cameraDirection().x;
    //dataStream[offset++] = s.representation[0];
    //dataStream[offset++] = s.representation[1];
    //dataStream[offset++] = s.representation[2];
    //dataStream[offset++] = s.representation[3];

    //s.value = _mainCamera->cameraDirection().y;
    //dataStream[offset++] = s.representation[0];
    //dataStream[offset++] = s.representation[1];
    //dataStream[offset++] = s.representation[2];
    //dataStream[offset++] = s.representation[3];

    //s.value = _mainCamera->cameraDirection().z;
    //dataStream[offset++] = s.representation[0];
    //dataStream[offset++] = s.representation[1];
    //dataStream[offset++] = s.representation[2];
    //dataStream[offset++] = s.representation[3];

    //s.value = _mainCamera->rotation().x;
    //dataStream[offset++] = s.representation[0];
    //dataStream[offset++] = s.representation[1];
    //dataStream[offset++] = s.representation[2];
    //dataStream[offset++] = s.representation[3];

    //s.value = _mainCamera->rotation().y;
    //dataStream[offset++] = s.representation[0];
    //dataStream[offset++] = s.representation[1];
    //dataStream[offset++] = s.representation[2];
    //dataStream[offset++] = s.representation[3];

    //s.value = _mainCamera->rotation().z;
    //dataStream[offset++] = s.representation[0];
    //dataStream[offset++] = s.representation[1];
    //dataStream[offset++] = s.representation[2];
    //dataStream[offset++] = s.representation[3];

    //s.value = _mainCamera->rotation().w;
    //dataStream[offset++] = s.representation[0];
    //dataStream[offset++] = s.representation[1];
    //dataStream[offset++] = s.representation[2];
    //dataStream[offset++] = s.representation[3];

    //s.value = _mainCamera->position().vec4().x;
    //dataStream[offset++] = s.representation[0];
    //dataStream[offset++] = s.representation[1];
    //dataStream[offset++] = s.representation[2];
    //dataStream[offset++] = s.representation[3];

    //s.value = _mainCamera->position().vec4().y;
    //dataStream[offset++] = s.representation[0];
    //dataStream[offset++] = s.representation[1];
    //dataStream[offset++] = s.representation[2];
    //dataStream[offset++] = s.representation[3];

    //s.value = _mainCamera->position().vec4().z;
    //dataStream[offset++] = s.representation[0];
    //dataStream[offset++] = s.representation[1];
    //dataStream[offset++] = s.representation[2];
    //dataStream[offset++] = s.representation[3];

    //s.value = _mainCamera->position().vec4().w;
    //dataStream[offset++] = s.representation[0];
    //dataStream[offset++] = s.representation[1];
    //dataStream[offset++] = s.representation[2];
    //dataStream[offset++] = s.representation[3];
}

void RenderEngine::deserialize(const std::vector<char>& dataStream, size_t& offset) {
    // TODO: This has to be redone properly (ab)

 //   union storage {
 //       float value;
 //       std::array<char, 4> representation;
 //   } s;

 //   glm::vec3 cameraDirection;
 //   s.representation[0] = dataStream[offset++];
 //   s.representation[1] = dataStream[offset++];
 //   s.representation[2] = dataStream[offset++];
 //   s.representation[3] = dataStream[offset++];
 //   cameraDirection.x = s.value;

 //   s.representation[0] = dataStream[offset++];
 //   s.representation[1] = dataStream[offset++];
 //   s.representation[2] = dataStream[offset++];
 //   s.representation[3] = dataStream[offset++];
 //   cameraDirection.y = s.value;

 //   s.representation[0] = dataStream[offset++];
 //   s.representation[1] = dataStream[offset++];
 //   s.representation[2] = dataStream[offset++];
 //   s.representation[3] = dataStream[offset++];
 //   cameraDirection.z = s.value;
 //   _mainCamera->setCameraDirection(cameraDirection);

 //   glm::quat rotation;
 //   s.representation[0] = dataStream[offset++];
 //   s.representation[1] = dataStream[offset++];
 //   s.representation[2] = dataStream[offset++];
 //   s.representation[3] = dataStream[offset++];
 //   rotation.x = s.value;

 //   s.representation[0] = dataStream[offset++];
 //   s.representation[1] = dataStream[offset++];
 //   s.representation[2] = dataStream[offset++];
 //   s.representation[3] = dataStream[offset++];
 //   rotation.y = s.value;

 //   s.representation[0] = dataStream[offset++];
 //   s.representation[1] = dataStream[offset++];
 //   s.representation[2] = dataStream[offset++];
 //   s.representation[3] = dataStream[offset++];
 //   rotation.z = s.value;

 //   s.representation[0] = dataStream[offset++];
 //   s.representation[1] = dataStream[offset++];
 //   s.representation[2] = dataStream[offset++];
 //   s.representation[3] = dataStream[offset++];
 //   rotation.w = s.value;
 //   _mainCamera->setRotation(rotation);

 //   glm::vec4 position;
 //   s.representation[0] = dataStream[offset++];
 //   s.representation[1] = dataStream[offset++];
 //   s.representation[2] = dataStream[offset++];
 //   s.representation[3] = dataStream[offset++];
 //   position.x = s.value;

 //   s.representation[0] = dataStream[offset++];
 //   s.representation[1] = dataStream[offset++];
 //   s.representation[2] = dataStream[offset++];
 //   s.representation[3] = dataStream[offset++];
 //   position.y = s.value;

 //   s.representation[0] = dataStream[offset++];
 //   s.representation[1] = dataStream[offset++];
 //   s.representation[2] = dataStream[offset++];
 //   s.representation[3] = dataStream[offset++];
 //   position.z = s.value;

 //   s.representation[0] = dataStream[offset++];
 //   s.representation[1] = dataStream[offset++];
 //   s.representation[2] = dataStream[offset++];
 //   s.representation[3] = dataStream[offset++];
 //   position.w = s.value;

	//_mainCamera->setPosition(position);
}

Camera* RenderEngine::camera() const {
    return _mainCamera;
}

ABuffer* RenderEngine::abuffer() const {
    return _abuffer;
}

void RenderEngine::generateGlslConfig() {
	LDEBUG("Generating GLSLS config, expect shader recompilation");
	int x1, xSize, y1, ySize;
	sgct::Engine::instance()->
		getActiveWindowPtr()->
		getCurrentViewportPixelCoords(x1, y1, xSize, ySize);

	// TODO: Make this file creation dynamic and better in every way
	// TODO: If the screen size changes it is enough if this file is regenerated to
	// recompile all necessary files
	std::ofstream os(absPath("${SHADERS}/ABuffer/constants.hglsl"));
	os << "#define SCREEN_WIDTH  " << xSize << "\n"
		<< "#define SCREEN_HEIGHT " << ySize << "\n"
		<< "#define MAX_LAYERS " << ABuffer::MAX_LAYERS << "\n"
		<< "#define ABUFFER_SINGLE_LINKED     1\n"
		<< "#define ABUFFER_FIXED             2\n"
		<< "#define ABUFFER_DYNAMIC           3\n"
		<< "#define ABUFFER_IMPLEMENTATION    ABUFFER_SINGLE_LINKED\n";
	os.close();
}

scripting::ScriptEngine::LuaLibrary RenderEngine::luaLibrary() {
	return {
		"",
		{
			{
				"printImage",
				&luascriptfunctions::printImage,
				"printImage(): Renders the current image to a file on disk"
			}
		}
	};

}

}  // namespace openspace
