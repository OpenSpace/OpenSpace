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

#include <openspace/abuffer/abuffervisualizer.h>
#include <openspace/abuffer/abuffer.h>
#include <openspace/abuffer/abufferframebuffer.h>
#include <openspace/abuffer/abufferSingleLinked.h>
#include <openspace/abuffer/abufferfixed.h>
#include <openspace/abuffer/abufferdynamic.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/scenegraph/scenegraph.h>
#include <openspace/util/camera.h>
#include <openspace/util/constants.h>
#include <openspace/util/time.h>
#include <openspace/util/screenlog.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/syncbuffer.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/lua/lua_helper.h>

#include <ghoul/io/texture/texturereader.h>
#ifdef GHOUL_USE_DEVIL
#include <ghoul/io/texture/texturereaderdevil.h>
#else
#include <ghoul/io/texture/texturereaderfreeimage.h>
#endif // GHOUL_USE_DEVIL
#include <ghoul/io/texture/texturereadercmap.h>

#include <array>
#include <fstream>
#include <sgct.h>

// ABuffer defines
#define ABUFFER_FRAMEBUFFER 0
#define ABUFFER_SINGLE_LINKED 1
#define ABUFFER_FIXED 2
#define ABUFFER_DYNAMIC 3

#ifdef __APPLE__
#define ABUFFER_IMPLEMENTATION ABUFFER_FRAMEBUFFER
#else
#define ABUFFER_IMPLEMENTATION ABUFFER_SINGLE_LINKED
#endif

namespace {
	const std::string _loggerCat = "RenderEngine";
}

namespace openspace {

namespace luascriptfunctions {

/**
 * \ingroup LuaScripts
 * takeScreenshot():
 * Save the rendering to an image file
 */
int takeScreenshot(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 0)
		return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);
	OsEng.renderEngine().takeScreenshot();
	return 0;
}

/**
* \ingroup LuaScripts
* visualizeABuffer(bool):
* Toggle the visualization of the ABuffer
*/
int visualizeABuffer(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 1)
		return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

	const int type = lua_type(L, -1);
	bool b = lua_toboolean(L, -1) != 0;
	OsEng.renderEngine().toggleVisualizeABuffer(b);
	return 0;
}

} // namespace luascriptfunctions


RenderEngine::RenderEngine()
    : _mainCamera(nullptr)
    , _sceneGraph(nullptr)
    , _abuffer(nullptr)
    , _log(nullptr)
    , _showInfo(true)
    , _showScreenLog(true)
	, _takeScreenshot(false)
	, _visualizeABuffer(false)
	, _visualizer(nullptr)
{
}

RenderEngine::~RenderEngine()
{
    delete _mainCamera;
	if (_visualizer)
		delete _visualizer;
}

bool RenderEngine::initialize()
{
	generateGlslConfig();

    // init camera and set temporary position and scaling
    _mainCamera = new Camera();
    _mainCamera->setScaling(glm::vec2(1.0, -8.0));
    _mainCamera->setPosition(psc(0.f, 0.f, 1.499823f, 11.f));
	OsEng.interactionHandler().setCamera(_mainCamera);

#ifdef GHOUL_USE_DEVIL
	ghoul::io::TextureReader::addReader(new ghoul::io::impl::TextureReaderDevIL);
#else
	ghoul::io::TextureReader::addReader(new ghoul::io::impl::TextureReaderFreeImage);
#endif // GHOUL_USE_DEVIL

	ghoul::io::TextureReader::addReader(new ghoul::io::impl::TextureReaderCMAP);
    
#if ABUFFER_IMPLEMENTATION == ABUFFER_FRAMEBUFFER
    _abuffer = new ABufferFramebuffer();
#elif ABUFFER_IMPLEMENTATION == ABUFFER_SINGLE_LINKED
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

    // TODO:    Fix the power scaled coordinates in such a way that these 
	//			values can be set to more realistic values

    // set the close clip plane and the far clip plane to extreme values while in
    // development
     sgct::Engine::instance()->setNearAndFarClippingPlanes(0.01f,10000.0f);
   // sgct::Engine::instance()->setNearAndFarClippingPlanes(0.1f, 30.0f);

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
		_mainCamera->setLookUpVector(glm::vec3(0.0, 1.0, 0.0));
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

#if 0
// @TODO Remove the ifdef when the next SGCT version is released that requests the
// getUserPtr to get a name parameter ---abock

        // set the eye position, useful during rendering
        const glm::vec3 eyePosition
              = sgct_core::ClusterManager::instance()->getUserPtr("")->getPos();
#else
		const glm::vec3 eyePosition
			  = sgct_core::ClusterManager::instance()->getUserPtr()->getPos();
#endif

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
    
    _abuffer->initialize();
    
	_log = new ScreenLog();
	ghoul::logging::LogManager::ref().addLog(_log);

	_visualizer = new ABufferVisualizer();

    // successful init
    return true;
}

void RenderEngine::postSynchronizationPreDraw()
{
	sgct_core::SGCTNode * thisNode = sgct_core::ClusterManager::instance()->getThisNodePtr();
	bool updateAbuffer = false;
	for (unsigned int i = 0; i < thisNode->getNumberOfWindows(); i++) {
		if (sgct::Engine::instance()->getWindowPtr(i)->isWindowResized()) {
			updateAbuffer = true;
			break;
		}
	}
	if (updateAbuffer) {
		generateGlslConfig();
		_abuffer->reinitialize();
	}
	
    // converts the quaternion used to rotation matrices
    _mainCamera->compileViewRotationMatrix();
	UpdateData a = { Time::ref().currentTime(), Time::ref().deltaTime() };

    // update and evaluate the scene starting from the root node
	_sceneGraph->update(a);
    _sceneGraph->evaluate(_mainCamera);

	// clear the abuffer before rendering the scene
	_abuffer->clear();
}

void RenderEngine::render()
{
	// We need the window pointer
	sgct::SGCTWindow* w = sgct::Engine::instance()->getActiveWindowPtr();
	if (w->isUsingFisheyeRendering())
		_abuffer->clear();

    // SGCT resets certain settings
#ifndef __APPLE__
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_CULL_FACE);
	glDisable(GL_BLEND);
#else
    glEnable(GL_DEPTH_TEST);
    glDisable(GL_CULL_FACE);
    glDisable(GL_BLEND);
#endif
    // setup the camera for the current frame

#if 0
// @TODO: Use this as soon as the new SGCT version is available ---abock
    const glm::vec3 eyePosition
          = sgct_core::ClusterManager::instance()->getUserPtr("")->getPos();
#else
	const glm::vec3 eyePosition
		  = sgct_core::ClusterManager::instance()->getUserPtr()->getPos();
#endif
	//@CHECK  does the dome disparity disappear if this line disappears? ---abock
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
	if (!_visualizeABuffer) {
		_abuffer->preRender();
		_sceneGraph->render({ *_mainCamera, psc() });
		_abuffer->postRender();

		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		_abuffer->resolve();
		glDisable(GL_BLEND);
	}
	else {
		_visualizer->render();
	}
    
#if 1
    
    // Print some useful information on the master viewport
	if (sgct::Engine::instance()->isMaster() && ! w->isUsingFisheyeRendering()) {

		// TODO: Adjust font_size properly when using retina screen
		const int font_size_mono	= 10;
		const int font_size_light	= 8;
		const int font_with_light	= static_cast<int>(font_size_light*0.7);
		const sgct_text::Font* fontLight = sgct_text::FontManager::instance()->getFont(constants::fonts::keyLight, font_size_light);
		const sgct_text::Font* fontMono = sgct_text::FontManager::instance()->getFont(constants::fonts::keyMono, font_size_mono);
		
		if (_showInfo) {
			const sgct_text::Font* font = fontMono;
			int x1, xSize, y1, ySize;
			sgct::Engine::instance()->getActiveWindowPtr()->getCurrentViewportPixelCoords(x1, y1, xSize, ySize);
			int startY = ySize - 2 * font_size_mono;
			const glm::vec2 scaling = _mainCamera->scaling();
			const glm::vec3 viewdirection = _mainCamera->viewDirection();
			const psc position = _mainCamera->position();
			const psc origin = OsEng.interactionHandler().focusNode()->worldPosition();
			const PowerScaledScalar pssl = (position - origin).length();

			// GUI PRINT 
// Using a macro to shorten line length and increase readability
#define PrintText(i, format, ...) Freetype::print(font, 10.f, static_cast<float>(startY - font_size_mono * i * 2), format, __VA_ARGS__);

			int i = 0;
			PrintText(i++, "Date: %s", Time::ref().currentTimeUTC().c_str());
			PrintText(i++, "Avg. Frametime: %.5f", sgct::Engine::instance()->getAvgDt());
			PrintText(i++, "Drawtime:       %.5f", sgct::Engine::instance()->getDrawTime());
			PrintText(i++, "Frametime:      %.5f", sgct::Engine::instance()->getDt());
			PrintText(i++, "Origin:         (% .5f, % .5f, % .5f, % .5f)", origin[0], origin[1], origin[2], origin[3]);
			PrintText(i++, "Cam pos:        (% .5f, % .5f, % .5f, % .5f)", position[0], position[1], position[2], position[3]);
			PrintText(i++, "View dir:       (% .5f, % .5f, % .5f)", viewdirection[0], viewdirection[1], viewdirection[2]);
			PrintText(i++, "Cam->origin:    (% .15f, % .4f)", pssl[0], pssl[1]);
			PrintText(i++, "Scaling:        (% .5f, % .5f)", scaling[0], scaling[1]);
#undef PrintText
		}

		if (_showScreenLog)
		{
			const sgct_text::Font* font = fontLight;
			const int max = 10;
			const int category_length = 20;
			const int msg_length = 140;
			const float ttl = 15.f;
			const float fade = 5.f;
			auto entries = _log->last(max);

			const glm::vec4 white(0.9, 0.9, 0.9, 1);
			const glm::vec4 red(1, 0, 0, 1);
			const glm::vec4 yellow(1, 1, 0, 1);
			const glm::vec4 green(0, 1, 0, 1);
			const glm::vec4 blue(0, 0, 1, 1);

			size_t nr = 1;
			for (auto it = entries.first; it != entries.second; ++it) {
				const ScreenLog::LogEntry* e = &(*it);

				const double t = sgct::Engine::instance()->getTime();
				float diff = static_cast<float>(t - e->timeStamp);

				// Since all log entries are ordered, once one is exceeding TTL, all have
				if (diff > ttl)
					break;

				float alpha = 1;
				float ttf = ttl - fade;
				if (diff > ttf) {
					diff = diff - ttf;
					float p = 0.8f - diff / fade;
					alpha = (p <= 0.f) ? 0.f : pow(p, 0.3f);
				}

				// Since all log entries are ordered, once one exceeds alpha, all have
				if (alpha <= 0.0)
					break;

				std::string lvl = "(" + ghoul::logging::LogManager::stringFromLevel(e->level) + ")";
				Freetype::print(font, 10.f, static_cast<float>(font_size_light * nr * 2), white*alpha, 
					"%-14s %s%s",									// Format
					e->timeString.c_str(),							// Time string
					e->category.substr(0, category_length).c_str(), // Category string (up to category_length)
					e->category.length() > 20 ? "..." : "");		// Pad category with "..." if exceeds category_length

				glm::vec4 color = white;
				if (e->level == ghoul::logging::LogManager::LogLevel::Debug)
					color = green;
				if (e->level == ghoul::logging::LogManager::LogLevel::Warning)
					color = yellow;
				if (e->level == ghoul::logging::LogManager::LogLevel::Error)
					color = red;
				if (e->level == ghoul::logging::LogManager::LogLevel::Fatal)
					color = blue;

				Freetype::print(font, static_cast<float>(10 + 39 * font_with_light), static_cast<float>(font_size_light * nr * 2), color*alpha, "%s", lvl.c_str());
				Freetype::print(font, static_cast<float>(10 + 53 * font_with_light), static_cast<float>(font_size_light * nr * 2), white*alpha, "%s", e->message.substr(0, msg_length).c_str());
				++nr;
			}
		}
	}
#endif
}

void RenderEngine::postDraw() {
	if (_takeScreenshot) {
		sgct::Engine::instance()->takeScreenshot();
		_takeScreenshot = false;
	}
}

void RenderEngine::takeScreenshot() {
	_takeScreenshot = true;
}

void RenderEngine::toggleVisualizeABuffer(bool b) {
	_visualizeABuffer = b;
	if (!_visualizeABuffer)
		return;

	std::vector<ABuffer::fragmentData> _d = _abuffer->pixelData();
	_visualizer->updateData(_d);
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

void RenderEngine::serialize(SyncBuffer* syncBuffer) {
	syncBuffer->encode(_mainCamera->scaling());
	syncBuffer->encode(_mainCamera->position());
	syncBuffer->encode(_mainCamera->viewRotationMatrix());
}

void RenderEngine::deserialize(SyncBuffer* syncBuffer) {
	glm::vec2 scaling;
	psc position;
	glm::mat4 viewRotation;
	syncBuffer->decode(scaling);
	syncBuffer->decode(position);
	syncBuffer->decode(viewRotation);
	_mainCamera->setScaling(scaling);
	_mainCamera->setPosition(position);
	_mainCamera->setViewRotationMatrix(viewRotation);
}

Camera* RenderEngine::camera() const {
    return _mainCamera;
}

ABuffer* RenderEngine::abuffer() const {
    return _abuffer;
}

void RenderEngine::generateGlslConfig() {
	LDEBUG("Generating GLSLS config, expect shader recompilation");
	int xSize = sgct::Engine::instance()->getActiveWindowPtr()->getXFramebufferResolution();;
	int ySize = sgct::Engine::instance()->getActiveWindowPtr()->getYFramebufferResolution();;

	// TODO: Make this file creation dynamic and better in every way
	// TODO: If the screen size changes it is enough if this file is regenerated to
	// recompile all necessary files
	std::ofstream os(absPath("${SHADERS_GENERATED}/constants.hglsl"));
	os << "#ifndef CONSTANTS_HGLSL\n"
		<< "#define CONSTANTS_HGLSL\n"
		<< "#define SCREEN_WIDTH  " << xSize << "\n"
		<< "#define SCREEN_HEIGHT " << ySize << "\n"
		<< "#define MAX_LAYERS " << ABuffer::MAX_LAYERS << "\n"
		<< "#define ABUFFER_FRAMEBUFFER       " << ABUFFER_FRAMEBUFFER << "\n"
		<< "#define ABUFFER_SINGLE_LINKED     " << ABUFFER_SINGLE_LINKED << "\n"
		<< "#define ABUFFER_FIXED             " << ABUFFER_FIXED << "\n"
		<< "#define ABUFFER_DYNAMIC           " << ABUFFER_DYNAMIC << "\n"
		<< "#define ABUFFER_IMPLEMENTATION    " << ABUFFER_IMPLEMENTATION << "\n";
// System specific
#ifdef WIN32
	os << "#define WIN32\n";
#endif
#ifdef __APPLE__
	os << "#define APPLE\n";
#endif
#ifdef __linux__
	os << "#define linux\n";
#endif
    os << "#endif\n";

	os.close();
}

scripting::ScriptEngine::LuaLibrary RenderEngine::luaLibrary() {
	return {
		"",
		{
			{
				"takeScreenshot",
				&luascriptfunctions::takeScreenshot,
				"",
				"Renders the current image to a file on disk"
			},
			{
				"visualizeABuffer",
				&luascriptfunctions::visualizeABuffer,
				"bool",
				"Toggles the visualization of the ABuffer"
			}
		},
	};
}

}  // namespace openspace
