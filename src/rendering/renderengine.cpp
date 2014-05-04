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

//#include "sgct.h"

#include <ghoul/filesystem/filesystem.h>

namespace {
const std::string _loggerCat = "RenderEngine";
}
namespace openspace {

RenderEngine::RenderEngine()
    : _mainCamera(nullptr)
    , _sceneGraph(nullptr)
{
}

RenderEngine::~RenderEngine()
{
    delete _mainCamera;
}

bool RenderEngine::initialize()
{
    // init camera and set temporary position and scaling
    _mainCamera = new Camera();
    _mainCamera->setScaling(glm::vec2(1.0, -8.0));
    _mainCamera->setPosition(psc(0.0, 0.0, 1.499823, 11.0));

    // if master, setup interaction
    if (sgct::Engine::instance()->isMaster())
        OsEng.interactionHandler().setCamera(_mainCamera);

    return true;
}

bool RenderEngine::initializeGL()
{
    sgct::SGCTWindow* wPtr = sgct::Engine::instance()->getActiveWindowPtr();

    // TODO:    Fix the power scaled coordinates in such a way that these values can be
    // set
    //          to more realistic values

    // set the close clip plane and the far clip plane to extreme values while in
    // development
    // sgct::Engine::instance()->setNearAndFarClippingPlanes(0.1f,100.0f);
    sgct::Engine::instance()->setNearAndFarClippingPlanes(0.00001f, 100.0f);

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
        _mainCamera->setMaxFov(maxFov);
    }

    // successful init
    return true;
}

void RenderEngine::postSynchronizationPreDraw()
{
    // converts the quaternion used to rotation matrices
    _mainCamera->compileViewRotationMatrix();

    // update and evaluate the scene starting from the root node
    _sceneGraph->update();
    _mainCamera->setCameraDirection(glm::vec3(0, 0, -1));
    _sceneGraph->evaluate(_mainCamera);
}

void RenderEngine::render()
{
    // SGCT resets certain settings
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);

    // setup the camera for the current frame
    const glm::vec3 eyePosition
          = sgct_core::ClusterManager::instance()->getUserPtr()->getPos();
    const glm::mat4 view
          = glm::translate(glm::mat4(1.0),
                           eyePosition);  // make sure the eye is in the center
    _mainCamera->setViewProjectionMatrix(
          sgct::Engine::instance()->getActiveModelViewProjectionMatrix() * view);

    // render the scene starting from the root node
    _sceneGraph->render(_mainCamera);

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
        const pss pssl = (position - origin).length();

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
}

std::shared_ptr<SceneGraph> RenderEngine::sceneGraph()
{
    // TODO custom assert (ticket #5)
    assert(_sceneGraph);
    return _sceneGraph;
}

void RenderEngine::setSceneGraph(std::shared_ptr<SceneGraph> sceneGraph)
{
    _sceneGraph = sceneGraph;
}

}  // namespace openspace
