/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifdef OPENSPACE_MODULE_NEWHORIZONS_ENABLED
#include <modules/newhorizons/util/imagesequencer.h>
#endif

#include <openspace/rendering/renderer.h>
#include <openspace/rendering/abufferrenderer.h>
#include <openspace/rendering/framebufferrenderer.h>

#include <modules/base/rendering/screenspaceimage.h>
#include <modules/base/rendering/screenspaceframebuffer.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/scene/scene.h>
#include <openspace/util/camera.h>
#include <openspace/util/time.h>
#include <openspace/util/screenlog.h>
#include <openspace/util/spicemanager.h>
//#include <openspace/rendering/renderablepath.h>
#include <modules/base/rendering/renderablepath.h>
#include <openspace/util/syncbuffer.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/sharedmemory.h>
#include <ghoul/opengl/programobject.h>
#include <openspace/engine/configurationmanager.h>
#include <ghoul/systemcapabilities/systemcapabilities.h>
#include <ghoul/systemcapabilities/openglcapabilitiescomponent.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/glm.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/rendering/screenspacerenderable.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/io/texture/texturewriter.h>
#ifdef GHOUL_USE_DEVIL
#include <ghoul/io/texture/texturereaderdevil.h>
#endif //GHOUL_USE_DEVIL
#ifdef GHOUL_USE_FREEIMAGE
#include <ghoul/io/texture/texturereaderfreeimage.h>
#endif // GHOUL_USE_FREEIMAGE
#include <ghoul/io/texture/texturereadercmap.h>
#include <ghoul/misc/exception.h>

#ifdef GHOUL_USE_SOIL
#include <ghoul/io/texture/texturereadersoil.h>
#include <ghoul/io/texture/texturewritersoil.h>
#endif //GHOUL_USE_SOIL

#include <array>
#include <fstream>
#include <sgct.h>

// These are temporary ---abock
#include <modules/base/ephemeris/spiceephemeris.h>
#include <modules/base/ephemeris/staticephemeris.h>

// ABuffer defines
#define RENDERER_FRAMEBUFFER 0
#define RENDERER_ABUFFER 1

#include "renderengine_lua.inl"

namespace {
	const std::string _loggerCat = "RenderEngine";

    const std::string KeyRenderingMethod = "RenderingMethod";   
    std::chrono::seconds ScreenLogTimeToLive(15);
    const std::string DefaultRenderingMethod = "ABuffer";
    const std::string RenderFsPath = "${SHADERS}/render.frag";
}

namespace openspace {

const std::string RenderEngine::PerformanceMeasurementSharedData =
	"OpenSpacePerformanceMeasurementSharedData";

const std::string RenderEngine::KeyFontMono = "Mono";
const std::string RenderEngine::KeyFontLight = "Light";
    
RenderEngine::RenderEngine()
	: _mainCamera(nullptr)
	, _sceneGraph(nullptr)
	, _renderer(nullptr)
    , _rendererImplementation(RendererImplementation::Invalid)
	, _log(nullptr)
	, _showInfo(true)
	, _showLog(true)
	, _takeScreenshot(false)
	, _doPerformanceMeasurements(false)
	, _performanceMemory(nullptr)
	, _globalBlackOutFactor(1.f)
	, _fadeDuration(2.f)
	, _currentFadeTime(0.f)
	, _fadeDirection(0)
    //    , _sgctRenderStatisticsVisible(false)
{
    _onScreenInformation = {
        glm::vec2(0.f),
        12,
        -1
    };
}

RenderEngine::~RenderEngine() {
	delete _sceneGraph;
	_sceneGraph = nullptr;

	delete _mainCamera;
	delete _performanceMemory;

	if (ghoul::SharedMemory::exists(PerformanceMeasurementSharedData))
		ghoul::SharedMemory::remove(PerformanceMeasurementSharedData);
}

bool RenderEngine::deinitialize() {
	_sceneGraph->clearSceneGraph();
	return true;
}

void RenderEngine::setRendererFromString(const std::string& renderingMethod) {
    _rendererImplementation = rendererFromString(renderingMethod);

    std::unique_ptr<Renderer> newRenderer = nullptr;
    switch (_rendererImplementation) {
    case RendererImplementation::Framebuffer:
		newRenderer = std::make_unique<FramebufferRenderer>();
        break;
    case RendererImplementation::ABuffer:
        newRenderer = std::make_unique<ABufferRenderer>();
        break;
    case RendererImplementation::Invalid:
        LFATAL("Rendering method '" << renderingMethod << "' not among the available "
               << "rendering methods");
    }

    setRenderer(std::move(newRenderer));
}

bool RenderEngine::initialize() {
    std::string renderingMethod = DefaultRenderingMethod;
    
    // If the user specified a rendering method that he would like to use, use that

    if (OsEng.configurationManager().hasKeyAndValue<std::string>(KeyRenderingMethod)) {
        renderingMethod = OsEng.configurationManager().value<std::string>(KeyRenderingMethod);
	} else {
        using Version = ghoul::systemcapabilities::OpenGLCapabilitiesComponent::Version;

        // The default rendering method has a requirement of OpenGL 4.3, so if we are
        // below that, we will fall back to frame buffer operation
        if (OpenGLCap.openGLVersion() < Version{4,3,0}) {
            LINFO("Falling back to framebuffer implementation due to OpenGL limitations");
            renderingMethod = "Framebuffer";
        }
    }

	LINFO("Seting renderer from string: " << renderingMethod);
	setRendererFromString(renderingMethod);

	// init camera and set temporary position and scaling
	_mainCamera = new Camera();
	_mainCamera->setScaling(glm::vec2(1.0, -8.0));
	_mainCamera->setPosition(psc(0.f, 0.f, 1.499823f, 11.f));

    OsEng.interactionHandler().setCamera(_mainCamera);
    if (_renderer) {
        _renderer->setCamera(_mainCamera);
    }

#ifdef GHOUL_USE_DEVIL
    ghoul::io::TextureReader::ref().addReader(std::make_shared<ghoul::io::TextureReaderDevIL>());
#endif // GHOUL_USE_DEVIL
#ifdef GHOUL_USE_FREEIMAGE
    ghoul::io::TextureReader::ref().addReader(std::make_shared<ghoul::io::TextureReaderFreeImage>());
#endif // GHOUL_USE_FREEIMAGE
#ifdef GHOUL_USE_SOIL
    ghoul::io::TextureReader::ref().addReader(std::make_shared<ghoul::io::TextureReaderSOIL>());
    ghoul::io::TextureWriter::ref().addWriter(std::make_shared<ghoul::io::TextureWriterSOIL>());
#endif // GHOUL_USE_SOIL
  
    ghoul::io::TextureReader::ref().addReader(std::make_shared<ghoul::io::TextureReaderCMAP>());
    ssr = std::make_shared<ScreenSpaceFramebuffer>();
    // ssr = std::make_shared<ScreenSpaceImage>("${OPENSPACE_DATA}/test2.jpg");
    registerScreenSpaceRenderable(ssr);
    // registerScreenSpaceRenderable(std::make_shared<ScreenSpaceImage>("${OPENSPACE_DATA}/test3.jpg"));
    // registerScreenSpaceRenderable(std::make_shared<ScreenSpaceFramebuffer>());

	return true;
}

bool RenderEngine::initializeGL() {
	// TODO:    Fix the power scaled coordinates in such a way that these 
	//			values can be set to more realistic values

	// set the close clip plane and the far clip plane to extreme values while in
	// development
    OsEng.windowWrapper().setNearFarClippingPlane(0.001f, 1000.f);
    
    
    try {
        const float fontSizeTime = 15.f;
        _fontDate = OsEng.fontManager().font(KeyFontMono, fontSizeTime);
        const float fontSizeMono = 10.f;
        _fontInfo = OsEng.fontManager().font(KeyFontMono, fontSizeMono);
        const float fontSizeLight = 8.f;
        _fontLog = OsEng.fontManager().font(KeyFontLight, fontSizeLight);
    }
    catch (const ghoul::fontrendering::Font::FreeTypeException& e) {
        LERROR(e.what());
        throw;
    }
   
    
    
    // ALL OF THIS HAS TO BE CHECKED
    // ---abock
    
    
//	sgct::Engine::instance()->setNearAndFarClippingPlanes(0.001f, 1000.0f);
	// sgct::Engine::instance()->setNearAndFarClippingPlanes(0.1f, 30.0f);

	// calculating the maximum field of view for the camera, used to
	// determine visibility of objects in the scene graph
/*    if (sgct::Engine::instance()->getCurrentRenderTarget() == sgct::Engine::NonLinearBuffer) {
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
	}
	else {*/
		// get corner positions, calculating the forth to easily calculate center
		
  //      glm::vec3 corners[4];
  //      sgct::SGCTWindow* wPtr = sgct::Engine::instance()->getWindowPtr(0);
  //      sgct_core::BaseViewport* vp = wPtr->getViewport(0);
  //      sgct_core::SGCTProjectionPlane* projectionPlane = vp->getProjectionPlane();

  //      corners[0] = *(projectionPlane->getCoordinatePtr(sgct_core::SGCTProjectionPlane::LowerLeft));
  //      corners[1] = *(projectionPlane->getCoordinatePtr(sgct_core::SGCTProjectionPlane::UpperLeft));
  //      corners[2] = *(projectionPlane->getCoordinatePtr(sgct_core::SGCTProjectionPlane::UpperRight));
  //      corners[3] = glm::vec3(corners[2][0], corners[0][1], corners[2][2]);
  //       
  //      const glm::vec3 center = (corners[0] + corners[1] + corners[2] + corners[3]);
		////	
		//const glm::vec3 eyePosition = sgct_core::ClusterManager::instance()->getDefaultUserPtr()->getPos();
		////// get viewdirection, stores the direction in the camera, used for culling
		//const glm::vec3 viewdir = glm::normalize(eyePosition - center);

        //const glm::vec3 upVector = corners[0] - corners[1];

        
		//_mainCamera->setCameraDirection(glm::normalize(-viewdir));
     _mainCamera->setCameraDirection(glm::vec3(0.f, 0.f, -1.f));
		//_mainCamera->setLookUpVector(glm::normalize(upVector));
        _mainCamera->setLookUpVector(glm::vec3(0.f, 1.f, 0.f));

		// set the initial fov to be 0.0 which means everything will be culled
		//float maxFov = 0.0f;
        float maxFov = std::numeric_limits<float>::max();

		//// for each corner
		//for (int i = 0; i < 4; ++i) {
		//	// calculate radians to corner
		//	glm::vec3 dir = glm::normalize(eyePosition - corners[i]);
		//	float radsbetween = acos(glm::dot(viewdir, dir))
		//		/ (glm::length(viewdir) * glm::length(dir));

		//	// the angle to a corner is larger than the current maxima
		//	if (radsbetween > maxFov) {
		//		maxFov = radsbetween;
		//	}
		//}
		_mainCamera->setMaxFov(maxFov);
    //}

    LINFO("Initializing Log");
    std::unique_ptr<ScreenLog> log = std::make_unique<ScreenLog>(ScreenLogTimeToLive);
    _log = log.get();
    ghoul::logging::LogManager::ref().addLog(std::move(log));

    LINFO("Finished initializing GL");
	return true;
}

void RenderEngine::preSynchronization() {
	if (_mainCamera)
		_mainCamera->preSynchronization();
}

void RenderEngine::postSynchronizationPreDraw() {
	//temporary fade funtionality
	if (_fadeDirection != 0) {
		if (_currentFadeTime > _fadeDuration){
			_fadeDirection = 0;
			_globalBlackOutFactor = fminf(1.f, fmaxf(0.f, _globalBlackOutFactor));
		} 
		else {
			if (_fadeDirection < 0)
				_globalBlackOutFactor = glm::smoothstep(1.f, 0.f, _currentFadeTime / _fadeDuration);
			else
				_globalBlackOutFactor = glm::smoothstep(0.f, 1.f, _currentFadeTime / _fadeDuration);
            _currentFadeTime += static_cast<float>(OsEng.windowWrapper().averageDeltaTime());
		}
	}

	if (_mainCamera)
		_mainCamera->postSynchronizationPreDraw();

	bool windowResized = OsEng.windowWrapper().windowHasResized();

	if (windowResized) {
		glm::ivec2 res = OsEng.windowWrapper().currentDrawBufferResolution();
		_renderer->setResolution(res);
		ghoul::fontrendering::FontRenderer::defaultRenderer().setWindowSize(glm::vec2(res));
	}

	// converts the quaternion used to rotation matrices
    if (_mainCamera)
        _mainCamera->compileViewRotationMatrix();

	// update and evaluate the scene starting from the root node
	_sceneGraph->update({
		Time::ref().currentTime(),
        Time::ref().timeJumped(),
		Time::ref().deltaTime(),
		_doPerformanceMeasurements
	});
	_sceneGraph->evaluate(_mainCamera);

	_renderer->update();

	for (auto program : _programs) {
		if (program->isDirty()) {
			program->rebuildFromFile();
		}
	}

	for (auto screenspacerenderable : _screenSpaceRenderables) {
		screenspacerenderable->update();
	}
	//Allow focus node to update camera (enables camera-following)
	//FIX LATER: THIS CAUSES MASTER NODE TO BE ONE FRAME AHEAD OF SLAVES
	//if (const SceneGraphNode* node = OsEng.ref().interactionHandler().focusNode()){
		//node->updateCamera(_mainCamera);
	//}

}

void RenderEngine::render(const glm::mat4 &projectionMatrix, const glm::mat4 &viewMatrix) {
	_mainCamera->setViewMatrix(viewMatrix);
	_mainCamera->setProjectionMatrix(projectionMatrix);


	if (!(OsEng.isMaster() && _disableMasterRendering)) {
        _renderer->render(_globalBlackOutFactor, _doPerformanceMeasurements);
	}

	// Print some useful information on the master viewport
	if (OsEng.isMaster() && OsEng.windowWrapper().isSimpleRendering()) {
		if (_showInfo) {
			renderInformation();
		}
		if (_showLog) {
			renderScreenLog();
		}
	}
	
	for (auto screenSpaceRenderable : _screenSpaceRenderables) {
		if(screenSpaceRenderable->isEnabled())
			screenSpaceRenderable->render();
	}
}

void RenderEngine::postDraw() {
    if (Time::ref().timeJumped())
        Time::ref().setTimeJumped(false);
	if (_takeScreenshot) {
        OsEng.windowWrapper().takeScreenshot();
		_takeScreenshot = false;
	}

	if (_doPerformanceMeasurements)
		storePerformanceMeasurements();
}

void RenderEngine::takeScreenshot() {
	_takeScreenshot = true;
}

void RenderEngine::toggleInfoText(bool b) {
	_showInfo = b;
}

Scene* RenderEngine::scene() {
	// TODO custom assert (ticket #5)
	assert(_sceneGraph);
	return _sceneGraph;
}

void RenderEngine::setSceneGraph(Scene* sceneGraph) {
	_sceneGraph = sceneGraph;
}

void RenderEngine::serialize(SyncBuffer* syncBuffer) {
	if (_mainCamera){
		_mainCamera->serialize(syncBuffer);
	}


    syncBuffer->encode(_onScreenInformation._node);
    syncBuffer->encode(_onScreenInformation._position.x);
    syncBuffer->encode(_onScreenInformation._position.y);
    syncBuffer->encode(_onScreenInformation._size);
}

void RenderEngine::deserialize(SyncBuffer* syncBuffer) {
	if (_mainCamera){
		_mainCamera->deserialize(syncBuffer);
	}
    syncBuffer->decode(_onScreenInformation._node);
    syncBuffer->decode(_onScreenInformation._position.x);
    syncBuffer->decode(_onScreenInformation._position.y);
    syncBuffer->decode(_onScreenInformation._size);

}

Camera* RenderEngine::camera() const {
	return _mainCamera;
}

Renderer* RenderEngine::renderer() const {
	return _renderer.get();
}

RenderEngine::RendererImplementation RenderEngine::rendererImplementation() const {
    return _rendererImplementation;
}

float RenderEngine::globalBlackOutFactor() {
	return _globalBlackOutFactor;
}

void RenderEngine::setGlobalBlackOutFactor(float opacity) {
	_globalBlackOutFactor = opacity;
}

void RenderEngine::startFading(int direction, float fadeDuration) {
	_fadeDirection = direction;
	_fadeDuration = fadeDuration;
	_currentFadeTime = 0.f;
}

/**
 * Build a program object for rendering with the used renderer
 */
std::unique_ptr<ghoul::opengl::ProgramObject> RenderEngine::buildRenderProgram(
	std::string name,
	std::string vsPath,
	std::string fsPath,
	const ghoul::Dictionary& data) {

	ghoul::Dictionary dict = data;

    // set path to the current renderer's main fragment shader
	dict.setValue("rendererData", _rendererData);
    // parameterize the main fragment shader program with specific contents.
    // fsPath should point to a shader file defining a Fragment getFragment() function
    // instead of a void main() setting glFragColor, glFragDepth, etc.
    dict.setValue("fragmentPath", fsPath);

    std::unique_ptr<ghoul::opengl::ProgramObject> program = ghoul::opengl::ProgramObject::Build(
		name,
		vsPath,
		RenderFsPath,
		dict);

    if (program) {
        _programs.push_back(program.get());
    }
	return program;
}

/**
 * Build a program object for rendering with the used renderer
 */
std::unique_ptr<ghoul::opengl::ProgramObject> RenderEngine::buildRenderProgram(
	std::string name,
	std::string vsPath,
	std::string fsPath,
	std::string csPath,
	const ghoul::Dictionary& data) {

	ghoul::Dictionary dict = data;
	dict.setValue("rendererData", _rendererData);

    // parameterize the main fragment shader program with specific contents.
    // fsPath should point to a shader file defining a Fragment getFragment() function
    // instead of a void main() setting glFragColor, glFragDepth, etc.
    dict.setValue("fragmentPath", fsPath);

	std::unique_ptr<ghoul::opengl::ProgramObject> program = ghoul::opengl::ProgramObject::Build(
		name,
		vsPath,
		RenderFsPath,
		csPath,
		dict);

    if (program) {
        _programs.push_back(program.get());
    }
	return program;
}

void RenderEngine::removeRenderProgram(const std::unique_ptr<ghoul::opengl::ProgramObject>& program) {
	ghoul::opengl::ProgramObject* ptr = program.get();
    auto it = std::find(
		_programs.begin(),
		_programs.end(),
		ptr
		);

	if (it != _programs.end()) {
		_programs.erase(it);
	}
}

/**
 * Set renderer data
 * Called from the renderer, whenever it needs to update
 * the dictionary of all rendering programs.
 */
void RenderEngine::setRendererData(const ghoul::Dictionary& data) {
	_rendererData = data;
	for (auto program : _programs) {
		ghoul::Dictionary dict = program->dictionary();
		dict.setValue("rendererData", _rendererData);
		program->setDictionary(dict);
	}
}

/**
 * Set renderer
 */
void RenderEngine::setRenderer(std::unique_ptr<Renderer> renderer) {
	glm::ivec2 res = OsEng.windowWrapper().currentDrawBufferResolution();

	if (_renderer) {
		_renderer->deinitialize();
	}

	_renderer = std::move(renderer);
    _renderer->setResolution(res);
	_renderer->initialize();
    _renderer->setCamera(_mainCamera);
    _renderer->setScene(_sceneGraph);
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
				"setRenderer",
				&luascriptfunctions::setRenderer,
				"string",
				"Sets the renderer (ABuffer or FrameBuffer)"
			},
			{
				"showRenderInformation",
				&luascriptfunctions::showRenderInformation,
				"bool",
				"Toggles the showing of render information on-screen text"
			},
			{
				"setPerformanceMeasurement",
				&luascriptfunctions::setPerformanceMeasurement,
				"bool",
				"Sets the performance measurements"
			},
		    {
			    "fadeIn",
			    &luascriptfunctions::fadeIn,
			    "number",
			    "",
                true
		    },
		    //also temporary @JK
		    {
			    "fadeOut",
			    &luascriptfunctions::fadeOut,
			    "number",
			    "",
                true
		    },
		},
	};
}

void RenderEngine::setPerformanceMeasurements(bool performanceMeasurements) {
	_doPerformanceMeasurements = performanceMeasurements;
}

bool RenderEngine::doesPerformanceMeasurements() const {
	return _doPerformanceMeasurements;
}

void RenderEngine::storePerformanceMeasurements() {
	const int8_t Version = 0;
	const int nValues = 250;
	const int lengthName = 256;
	const int maxValues = 256;

	struct PerformanceLayout {
		int8_t version;
		int32_t nValuesPerEntry;
		int32_t nEntries;
		int32_t maxNameLength;
		int32_t maxEntries;

		struct PerformanceLayoutEntry {
			char name[lengthName];
			float renderTime[nValues];
			float updateRenderable[nValues];
			float updateEphemeris[nValues];

			int32_t currentRenderTime;
			int32_t currentUpdateRenderable;
			int32_t currentUpdateEphemeris;
		};

		PerformanceLayoutEntry entries[maxValues];
	};

	const int nNodes = static_cast<int>(scene()->allSceneGraphNodes().size());
	if (!_performanceMemory) {

		// Compute the total size
		const int totalSize = sizeof(int8_t) + 4 * sizeof(int32_t) +
			maxValues * sizeof(PerformanceLayout::PerformanceLayoutEntry);
		LINFO("Create shared memory of " << totalSize << " bytes");

        try {
            ghoul::SharedMemory::remove(PerformanceMeasurementSharedData);
        }
        catch (const ghoul::SharedMemory::SharedMemoryError& e) {
            LINFOC(e.component, e.what());
        }
        
		ghoul::SharedMemory::create(PerformanceMeasurementSharedData, totalSize);
		_performanceMemory = new ghoul::SharedMemory(PerformanceMeasurementSharedData);

        void* ptr = _performanceMemory->memory();
		PerformanceLayout* layout = reinterpret_cast<PerformanceLayout*>(ptr);
		layout->version = Version;
		layout->nValuesPerEntry = nValues;
		layout->nEntries = nNodes;
		layout->maxNameLength = lengthName;
		layout->maxEntries = maxValues;

		memset(layout->entries, 0, maxValues * sizeof(PerformanceLayout::PerformanceLayoutEntry));

		for (int i = 0; i < nNodes; ++i) {
			SceneGraphNode* node = scene()->allSceneGraphNodes()[i];

			memset(layout->entries[i].name, 0, lengthName);
#ifdef _MSC_VER
            strcpy_s(layout->entries[i].name, node->name().length() + 1, node->name().c_str());
#else
            strcpy(layout->entries[i].name, node->name().c_str());
#endif

			layout->entries[i].currentRenderTime = 0;
			layout->entries[i].currentUpdateRenderable = 0;
			layout->entries[i].currentUpdateEphemeris = 0;
		}
	}

    void* ptr = _performanceMemory->memory();
	PerformanceLayout* layout = reinterpret_cast<PerformanceLayout*>(ptr);
	_performanceMemory->acquireLock();
	for (int i = 0; i < nNodes; ++i) {
		SceneGraphNode* node = scene()->allSceneGraphNodes()[i];
		SceneGraphNode::PerformanceRecord r = node->performanceRecord();
		PerformanceLayout::PerformanceLayoutEntry& entry = layout->entries[i];

		entry.renderTime[entry.currentRenderTime] = r.renderTime / 1000.f;
		entry.updateEphemeris[entry.currentUpdateEphemeris] = r.updateTimeEphemeris / 1000.f;
		entry.updateRenderable[entry.currentUpdateRenderable] = r.updateTimeRenderable / 1000.f;

		entry.currentRenderTime = (entry.currentRenderTime + 1) % nValues;
		entry.currentUpdateEphemeris = (entry.currentUpdateEphemeris + 1) % nValues;
		entry.currentUpdateRenderable = (entry.currentUpdateRenderable + 1) % nValues;
	}
	_performanceMemory->releaseLock();
}

// This method is temporary and will be removed once the scalegraph is in effect ---abock
void RenderEngine::changeViewPoint(std::string origin) {
    SceneGraphNode* solarSystemBarycenterNode = scene()->sceneGraphNode("SolarSystemBarycenter");
    SceneGraphNode* plutoBarycenterNode = scene()->sceneGraphNode("PlutoBarycenter");
    SceneGraphNode* newHorizonsNode = scene()->sceneGraphNode("NewHorizons");
	SceneGraphNode* newHorizonsPathNodeJ = scene()->sceneGraphNode("NewHorizonsPathJupiter");
	SceneGraphNode* newHorizonsPathNodeP = scene()->sceneGraphNode("NewHorizonsPathPluto");
	RenderablePath* nhPath;

    SceneGraphNode* jupiterBarycenterNode = scene()->sceneGraphNode("JupiterBarycenter");

	//SceneGraphNode* newHorizonsGhostNode = scene()->sceneGraphNode("NewHorizonsGhost");
	//SceneGraphNode* dawnNode = scene()->sceneGraphNode("Dawn");
	//SceneGraphNode* vestaNode = scene()->sceneGraphNode("Vesta");

    if (solarSystemBarycenterNode == nullptr || plutoBarycenterNode == nullptr || 
		newHorizonsNode == nullptr || jupiterBarycenterNode == nullptr 
		//||	dawnNode == nullptr 
		//||  vestaNode == nullptr
		) {
	    LERROR("Necessary nodes does not exist");
		return;
    }

    if (origin == "Pluto") {
		if (newHorizonsPathNodeP) {
			Renderable* R = newHorizonsPathNodeP->renderable();
			newHorizonsPathNodeP->setParent(plutoBarycenterNode);
			nhPath = static_cast<RenderablePath*>(R);
			nhPath->calculatePath("PLUTO BARYCENTER");
		}

		plutoBarycenterNode->setParent(scene()->sceneGraphNode("SolarSystem"));
		plutoBarycenterNode->setEphemeris(new StaticEphemeris);
		
		solarSystemBarycenterNode->setParent(plutoBarycenterNode);
		newHorizonsNode->setParent(plutoBarycenterNode);
		//newHorizonsGhostNode->setParent(plutoBarycenterNode);

		//dawnNode->setParent(plutoBarycenterNode);
		//vestaNode->setParent(plutoBarycenterNode);

		//newHorizonsTrailNode->setParent(plutoBarycenterNode);
		ghoul::Dictionary solarDictionary =
		{
			{ std::string("Type"), std::string("Spice") },
			{ std::string("Body"), std::string("SUN") },
			{ std::string("Reference"), std::string("GALACTIC") },
			{ std::string("Observer"), std::string("PLUTO BARYCENTER") },
			{ std::string("Kernels"), ghoul::Dictionary() }
		};
        
        ghoul::Dictionary jupiterDictionary =
        {
            { std::string("Type"), std::string("Spice") },
            { std::string("Body"), std::string("JUPITER BARYCENTER") },
            { std::string("Reference"), std::string("GALACTIC") },
            { std::string("Observer"), std::string("PLUTO BARYCENTER") },
            { std::string("Kernels"), ghoul::Dictionary() }
        };

        ghoul::Dictionary newHorizonsDictionary =
        {
            { std::string("Type"), std::string("Spice") },
            { std::string("Body"), std::string("NEW HORIZONS") },
            { std::string("Reference"), std::string("GALACTIC") },
            { std::string("Observer"), std::string("PLUTO BARYCENTER") },
            { std::string("Kernels"), ghoul::Dictionary() }
        };

		solarSystemBarycenterNode->setEphemeris(new SpiceEphemeris(solarDictionary));
		jupiterBarycenterNode->setEphemeris(new SpiceEphemeris(jupiterDictionary));
        newHorizonsNode->setEphemeris(new SpiceEphemeris(newHorizonsDictionary));
		//newHorizonsTrailNode->setEphemeris(new SpiceEphemeris(newHorizonsDictionary));


		//ghoul::Dictionary dawnDictionary =
        //{
        //    { std::string("Type"), std::string("Spice") },
        //    { std::string("Body"), std::string("DAWN") },
        //    { std::string("Reference"), std::string("GALACTIC") },
        //    { std::string("Observer"), std::string("PLUTO BARYCENTER") },
        //    { std::string("Kernels"), ghoul::Dictionary() }
        //};
        //dawnNode->setEphemeris(new SpiceEphemeris(dawnDictionary));
		//
		//ghoul::Dictionary vestaDictionary =
		//{
		//	  { std::string("Type"), std::string("Spice") },
		//	  { std::string("Body"), std::string("VESTA") },
		//	  { std::string("Reference"), std::string("GALACTIC") },
		//	  { std::string("Observer"), std::string("PLUTO BARYCENTER") },
		//	  { std::string("Kernels"), ghoul::Dictionary() }
		//};
		//vestaNode->setEphemeris(new SpiceEphemeris(vestaDictionary));

		
		//ghoul::Dictionary newHorizonsGhostDictionary =
		//{
		//	{ std::string("Type"), std::string("Spice") },
		//	{ std::string("Body"), std::string("NEW HORIZONS") },
		//	{ std::string("EphmerisGhosting"), std::string("TRUE") },
		//	{ std::string("Reference"), std::string("GALACTIC") },
		//	{ std::string("Observer"), std::string("PLUTO BARYCENTER") },
		//	{ std::string("Kernels"), ghoul::Dictionary() }
		//};
		//newHorizonsGhostNode->setEphemeris(new SpiceEphemeris(newHorizonsGhostDictionary));
		
        return;
    }
    if (origin == "Sun") {
		solarSystemBarycenterNode->setParent(scene()->sceneGraphNode("SolarSystem"));

		plutoBarycenterNode->setParent(solarSystemBarycenterNode);
		jupiterBarycenterNode->setParent(solarSystemBarycenterNode);
		newHorizonsNode->setParent(solarSystemBarycenterNode);
		//newHorizonsGhostNode->setParent(solarSystemBarycenterNode);

		//newHorizonsTrailNode->setParent(solarSystemBarycenterNode);
		//dawnNode->setParent(solarSystemBarycenterNode);
		//vestaNode->setParent(solarSystemBarycenterNode);

        ghoul::Dictionary plutoDictionary =
        {
            { std::string("Type"), std::string("Spice") },
            { std::string("Body"), std::string("PLUTO BARYCENTER") },
            { std::string("Reference"), std::string("GALACTIC") },
            { std::string("Observer"), std::string("SUN") },
            { std::string("Kernels"), ghoul::Dictionary() }
        };
        ghoul::Dictionary jupiterDictionary =
        {
            { std::string("Type"), std::string("Spice") },
            { std::string("Body"), std::string("JUPITER BARYCENTER") },
            { std::string("Reference"), std::string("GALACTIC") },
            { std::string("Observer"), std::string("SUN") },
            { std::string("Kernels"), ghoul::Dictionary() }
        };
        
        solarSystemBarycenterNode->setEphemeris(new StaticEphemeris);
        jupiterBarycenterNode->setEphemeris(new SpiceEphemeris(jupiterDictionary));
        plutoBarycenterNode->setEphemeris(new SpiceEphemeris(plutoDictionary));

        ghoul::Dictionary newHorizonsDictionary =
        {
            { std::string("Type"), std::string("Spice") },
            { std::string("Body"), std::string("NEW HORIZONS") },
            { std::string("Reference"), std::string("GALACTIC") },
            { std::string("Observer"), std::string("SUN") },
            { std::string("Kernels"), ghoul::Dictionary() }
        };
        newHorizonsNode->setEphemeris(new SpiceEphemeris(newHorizonsDictionary));
		//newHorizonsTrailNode->setEphemeris(new SpiceEphemeris(newHorizonsDictionary));

        
		//ghoul::Dictionary dawnDictionary =
		//{
		//	{ std::string("Type"), std::string("Spice") },
		//	{ std::string("Body"), std::string("DAWN") },
		//	{ std::string("Reference"), std::string("GALACTIC") },
		//	{ std::string("Observer"), std::string("SUN") },
		//	{ std::string("Kernels"), ghoul::Dictionary() }
		//};
		//dawnNode->setEphemeris(new SpiceEphemeris(dawnDictionary));
		//
		//ghoul::Dictionary vestaDictionary =
		//{
		//	{ std::string("Type"), std::string("Spice") },
		//	{ std::string("Body"), std::string("VESTA") },
		//	{ std::string("Reference"), std::string("GALACTIC") },
		//	{ std::string("Observer"), std::string("SUN") },
		//	{ std::string("Kernels"), ghoul::Dictionary() }
		//};
		//vestaNode->setEphemeris(new SpiceEphemeris(vestaDictionary));
		
		
		//ghoul::Dictionary newHorizonsGhostDictionary =
		//{
		//	{ std::string("Type"), std::string("Spice") },
		//	{ std::string("Body"), std::string("NEW HORIZONS") },
		//	{ std::string("EphmerisGhosting"), std::string("TRUE") },
		//	{ std::string("Reference"), std::string("GALACTIC") },
		//	{ std::string("Observer"), std::string("JUPITER BARYCENTER") },
		//	{ std::string("Kernels"), ghoul::Dictionary() }
		//};
		//newHorizonsGhostNode->setEphemeris(new SpiceEphemeris(newHorizonsGhostDictionary));
		
        return;
    }
    if (origin == "Jupiter") {
		if (newHorizonsPathNodeJ) {
			Renderable* R = newHorizonsPathNodeJ->renderable();
			newHorizonsPathNodeJ->setParent(jupiterBarycenterNode);
			nhPath = static_cast<RenderablePath*>(R);
			nhPath->calculatePath("JUPITER BARYCENTER");
		}

		jupiterBarycenterNode->setParent(scene()->sceneGraphNode("SolarSystem"));
		jupiterBarycenterNode->setEphemeris(new StaticEphemeris);

		solarSystemBarycenterNode->setParent(jupiterBarycenterNode);
		newHorizonsNode->setParent(jupiterBarycenterNode);
		//newHorizonsTrailNode->setParent(jupiterBarycenterNode);

		//dawnNode->setParent(jupiterBarycenterNode);
		//vestaNode->setParent(jupiterBarycenterNode);


		ghoul::Dictionary solarDictionary =
		{
			{ std::string("Type"), std::string("Spice") },
			{ std::string("Body"), std::string("SUN") },
			{ std::string("Reference"), std::string("GALACTIC") },
			{ std::string("Observer"), std::string("JUPITER BARYCENTER") },
			{ std::string("Kernels"), ghoul::Dictionary() }
		};

		ghoul::Dictionary plutoDictionary =
		{
			{ std::string("Type"), std::string("Spice") },
			{ std::string("Body"), std::string("PlUTO BARYCENTER") },
			{ std::string("Reference"), std::string("GALACTIC") },
			{ std::string("Observer"), std::string("JUPITER BARYCENTER") },
			{ std::string("Kernels"), ghoul::Dictionary() }
		};

		ghoul::Dictionary newHorizonsDictionary =
		{
			{ std::string("Type"), std::string("Spice") },
			{ std::string("Body"), std::string("NEW HORIZONS") },
			{ std::string("Reference"), std::string("GALACTIC") },
			{ std::string("Observer"), std::string("JUPITER BARYCENTER") },
			{ std::string("Kernels"), ghoul::Dictionary() }
		};
		solarSystemBarycenterNode->setEphemeris(new SpiceEphemeris(solarDictionary));
		plutoBarycenterNode->setEphemeris(new SpiceEphemeris(plutoDictionary));
		newHorizonsNode->setEphemeris(new SpiceEphemeris(newHorizonsDictionary));
		//newHorizonsGhostNode->setParent(jupiterBarycenterNode);
		//newHorizonsTrailNode->setEphemeris(new SpiceEphemeris(newHorizonsDictionary));


		//ghoul::Dictionary dawnDictionary =
		//{
		//	{ std::string("Type"), std::string("Spice") },
		//	{ std::string("Body"), std::string("DAWN") },
		//	{ std::string("Reference"), std::string("GALACTIC") },
		//	{ std::string("Observer"), std::string("JUPITER BARYCENTER") },
		//	{ std::string("Kernels"), ghoul::Dictionary() }
		//};
		//dawnNode->setEphemeris(new SpiceEphemeris(dawnDictionary));
		//
		//ghoul::Dictionary vestaDictionary =
		//{
		//	{ std::string("Type"), std::string("Spice") },
		//	{ std::string("Body"), std::string("VESTA") },
		//	{ std::string("Reference"), std::string("GALACTIC") },
		//	{ std::string("Observer"), std::string("JUPITER BARYCENTER") },
		//	{ std::string("Kernels"), ghoul::Dictionary() }
		//};
		//vestaNode->setEphemeris(new SpiceEphemeris(vestaDictionary));


		
		//ghoul::Dictionary newHorizonsGhostDictionary =
		//{
		//	{ std::string("Type"), std::string("Spice") },
		//	{ std::string("Body"), std::string("NEW HORIZONS") },
		//	{ std::string("EphmerisGhosting"), std::string("TRUE") },
		//	{ std::string("Reference"), std::string("GALACTIC") },
		//	{ std::string("Observer"), std::string("JUPITER BARYCENTER") },
		//	{ std::string("Kernels"), ghoul::Dictionary() }
		//};
		//newHorizonsGhostNode->setEphemeris(new SpiceEphemeris(newHorizonsGhostDictionary));
		//newHorizonsGhostNode->setParent(jupiterBarycenterNode);

	
        return;
    }
	//if (origin == "Vesta") {
	//	
	//	vestaNode->setParent(scene()->sceneGraphNode("SolarSystem"));
	//	vestaNode->setEphemeris(new StaticEphemeris);
	//
	//	solarSystemBarycenterNode->setParent(vestaNode);
	//	newHorizonsNode->setParent(vestaNode);
	//
	//	dawnNode->setParent(vestaNode);
	//	plutoBarycenterNode->setParent(vestaNode);
	//
	//
	//	ghoul::Dictionary plutoDictionary =
	//	{
	//		{ std::string("Type"), std::string("Spice") },
	//		{ std::string("Body"), std::string("PLUTO BARYCENTER") },
	//		{ std::string("Reference"), std::string("GALACTIC") },
	//		{ std::string("Observer"), std::string("VESTA") },
	//		{ std::string("Kernels"), ghoul::Dictionary() }
	//	};
	//	ghoul::Dictionary solarDictionary =
	//	{
	//		{ std::string("Type"), std::string("Spice") },
	//		{ std::string("Body"), std::string("SUN") },
	//		{ std::string("Reference"), std::string("GALACTIC") },
	//		{ std::string("Observer"), std::string("VESTA") },
	//		{ std::string("Kernels"), ghoul::Dictionary() }
	//	};
	//
	//	ghoul::Dictionary jupiterDictionary =
	//	{
	//		{ std::string("Type"), std::string("Spice") },
	//		{ std::string("Body"), std::string("JUPITER BARYCENTER") },
	//		{ std::string("Reference"), std::string("GALACTIC") },
	//		{ std::string("Observer"), std::string("VESTA") },
	//		{ std::string("Kernels"), ghoul::Dictionary() }
	//	};
	//
	//	solarSystemBarycenterNode->setEphemeris(new SpiceEphemeris(solarDictionary));
	//	plutoBarycenterNode->setEphemeris(new SpiceEphemeris(plutoDictionary));
	//	jupiterBarycenterNode->setEphemeris(new SpiceEphemeris(jupiterDictionary));
	//
	//	ghoul::Dictionary newHorizonsDictionary =
	//	{
	//		{ std::string("Type"), std::string("Spice") },
	//		{ std::string("Body"), std::string("NEW HORIZONS") },
	//		{ std::string("Reference"), std::string("GALACTIC") },
	//		{ std::string("Observer"), std::string("VESTA") },
	//		{ std::string("Kernels"), ghoul::Dictionary() }
	//	};
	//	newHorizonsNode->setEphemeris(new SpiceEphemeris(newHorizonsDictionary));
	//
	//	ghoul::Dictionary dawnDictionary =
	//	{
	//		{ std::string("Type"), std::string("Spice") },
	//		{ std::string("Body"), std::string("DAWN") },
	//		{ std::string("Reference"), std::string("GALACTIC") },
	//		{ std::string("Observer"), std::string("VESTA") },
	//		{ std::string("Kernels"), ghoul::Dictionary() }
	//	};
	//	dawnNode->setEphemeris(new SpiceEphemeris(dawnDictionary));
	//	vestaNode->setEphemeris(new StaticEphemeris);
	//
	//	return;
	//}

	if (origin == "67P") {
		SceneGraphNode* rosettaNode = scene()->sceneGraphNode("Rosetta");
		SceneGraphNode* cgNode = scene()->sceneGraphNode("67P");
		//jupiterBarycenterNode->setParent(solarSystemBarycenterNode);
		//plutoBarycenterNode->setParent(solarSystemBarycenterNode);
		solarSystemBarycenterNode->setParent(cgNode);
		rosettaNode->setParent(cgNode);
		
		ghoul::Dictionary solarDictionary =
			{
			{ std::string("Type"), std::string("Spice") },
				{ std::string("Body"), std::string("SUN") },
				{ std::string("Reference"), std::string("GALACTIC") },
				{ std::string("Observer"), std::string("CHURYUMOV-GERASIMENKO") },
				{ std::string("Kernels"), ghoul::Dictionary() }
			};
		solarSystemBarycenterNode->setEphemeris(new SpiceEphemeris(solarDictionary));
		
		ghoul::Dictionary rosettaDictionary =
			{
			{ std::string("Type"), std::string("Spice") },
				{ std::string("Body"), std::string("ROSETTA") },
				{ std::string("Reference"), std::string("GALACTIC") },
				{ std::string("Observer"), std::string("CHURYUMOV-GERASIMENKO") },
				{ std::string("Kernels"), ghoul::Dictionary() }
			};
		
		cgNode->setParent(scene()->sceneGraphNode("SolarSystem"));
		rosettaNode->setEphemeris(new SpiceEphemeris(rosettaDictionary));
		cgNode->setEphemeris(new StaticEphemeris);
		
		return;
		
	}

    LFATAL("This function is being misused with an argument of '" << origin << "'");
}

void RenderEngine::setDisableRenderingOnMaster(bool enabled) {
    _disableMasterRendering = enabled;
}

void RenderEngine::registerScreenSpaceRenderable(std::shared_ptr<ScreenSpaceRenderable> s){
	s->initialize();
	_screenSpaceRenderables.push_back(s);
}

void RenderEngine::unregisterScreenSpaceRenderable(std::shared_ptr<ScreenSpaceRenderable> s){
	auto it = std::find(
		_screenSpaceRenderables.begin(),
		_screenSpaceRenderables.end(),
		s
		);

	if (it != _screenSpaceRenderables.end()) {
		s->deinitialize();
		_screenSpaceRenderables.erase(it);
	}
}

std::shared_ptr<ScreenSpaceRenderable> RenderEngine::screenSpaceRenderable(std::string name){
	for(auto s : _screenSpaceRenderables){
		if(s->name() == name){
			return s;
		}
	}
	return nullptr;
}

RenderEngine::RendererImplementation RenderEngine::rendererFromString(const std::string& impl) {
    const std::map<std::string, RenderEngine::RendererImplementation> RenderingMethods = {
        { "ABuffer", RendererImplementation::ABuffer },
        { "Framebuffer", RendererImplementation::Framebuffer }
    };

    if (RenderingMethods.find(impl) != RenderingMethods.end())
        return RenderingMethods.at(impl);
    else
        return RendererImplementation::Invalid;
}

void RenderEngine::renderInformation() {
	// TODO: Adjust font_size properly when using retina screen
	const float fontSizeMono = 10.f;
	const float fontSizeTime = 15.f;

	using Font = ghoul::fontrendering::Font;
	using ghoul::fontrendering::RenderFont;


	if (_showInfo && _fontDate && _fontInfo) {
		double currentTime = Time::ref().currentTime();

		glm::vec2 penPosition = glm::vec2(
			10.f,
			OsEng.windowWrapper().viewportPixelCoordinates().w
			);
		penPosition.y -= _fontDate->height();

		RenderFontCr(*_fontDate,
			penPosition,
			"Date: %s",
			Time::ref().currentTimeUTC().c_str()
			);

		RenderFontCr(*_fontInfo,
			penPosition,
			"Simulation increment (s): %.0f",
			Time::ref().deltaTime()
			);

		RenderFontCr(*_fontInfo,
			penPosition,
			"Avg. Frametime: %.5f",
			OsEng.windowWrapper().averageDeltaTime()
			);

#ifdef OPENSPACE_MODULE_NEWHORIZONS_ENABLED
		if (openspace::ImageSequencer2::ref().isReady()) {
			penPosition.y -= 25.f;

			glm::vec4 targetColor(0.00, 0.75, 1.00, 1);

			double lt;
			glm::dvec3 p =
				SpiceManager::ref().targetPosition("PLUTO", "NEW HORIZONS", "GALACTIC", {}, currentTime, lt);
			psc nhPos = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);
			float a, b, c;
			glm::dvec3 radii;
			SpiceManager::ref().getValue("PLUTO", "RADII", radii);
			a = radii.x;
			b = radii.y;
			c = radii.z;
			float radius = (a + b) / 2.f;
			float distToSurf = glm::length(nhPos.vec3()) - radius;

			RenderFont(*_fontInfo,
				penPosition,
				"Distance to Pluto: % .1f (KM)",
				distToSurf
				);
			penPosition.y -= _fontInfo->height();


			double remaining = openspace::ImageSequencer2::ref().getNextCaptureTime() - currentTime;
			float t = static_cast<float>(1.0 - remaining / openspace::ImageSequencer2::ref().getIntervalLength());
			std::string progress = "|";
			int g = static_cast<int>((t * 24) + 1);
			g = std::max(g, 0);
			for (int i = 0; i < g; i++)
				progress.append("-");
			progress.append(">");
			for (int i = 0; i < 25 - g; i++)
				progress.append(" ");

			std::string str = SpiceManager::ref().dateFromEphemerisTime(
				ImageSequencer2::ref().getNextCaptureTime(),
				"YYYY MON DD HR:MN:SC"
				);

			glm::vec4 active(0.6, 1, 0.00, 1);
			glm::vec4 brigther_active(0.9, 1, 0.75, 1);

			progress.append("|");
			if (remaining > 0) {
				brigther_active *= (1 - t);

				RenderFontCr(*_fontInfo,
					penPosition,
					active * t + brigther_active,
					"Next instrument activity:"
					);

				RenderFontCr(*_fontInfo,
					penPosition,
					active * t + brigther_active,
					"%.0f s %s %.1f %%",
					remaining, progress.c_str(), t * 100
					);

				RenderFontCr(*_fontInfo,
					penPosition,
					active,
					"Data acquisition time: %s",
					str.c_str()
					);
			}
			std::pair<double, std::string> nextTarget = ImageSequencer2::ref().getNextTarget();
			std::pair<double, std::string> currentTarget = ImageSequencer2::ref().getCurrentTarget();

			if (currentTarget.first > 0.0) {
				int timeleft = static_cast<int>(nextTarget.first - currentTime);

				int hour = timeleft / 3600;
				int second = timeleft % 3600;
				int minute = second / 60;
				second = second % 60;

				std::string hh, mm, ss, coundtown;

				if (hour   < 10) hh.append("0");
				if (minute < 10) mm.append("0");
				if (second < 10) ss.append("0");

				hh.append(std::to_string(hour));
				mm.append(std::to_string(minute));
				ss.append(std::to_string(second));

				RenderFontCr(*_fontInfo,
					penPosition,
					targetColor,
					"Data acquisition adjacency: [%s:%s:%s]",
					hh.c_str(), mm.c_str(), ss.c_str()
					);


				std::pair<double, std::vector<std::string>> incidentTargets = ImageSequencer2::ref().getIncidentTargetList(2);
				std::string space;
				glm::vec4 color;
				size_t isize = incidentTargets.second.size();
				for (size_t p = 0; p < isize; p++) {
					double t = static_cast<double>(p + 1) / static_cast<double>(isize + 1);
					t = (p > isize / 2) ? 1 - t : t;
					t += 0.3;
					color = (p == isize / 2) ? targetColor : glm::vec4(t, t, t, 1);

					RenderFont(*_fontInfo,
						penPosition,
						color,
						"%s%s",
						space.c_str(), incidentTargets.second[p].c_str()
						);


					for (int k = 0; k < incidentTargets.second[p].size() + 2; k++)
						space += " ";
				}
				penPosition.y -= _fontInfo->height();

				std::map<std::string, bool> activeMap = ImageSequencer2::ref().getActiveInstruments();
				glm::vec4 firing(0.58 - t, 1 - t, 1 - t, 1);
				glm::vec4 notFiring(0.5, 0.5, 0.5, 1);

				RenderFontCr(*_fontInfo,
					penPosition,
					active,
					"Active Instruments:"
					);

				for (auto t : activeMap) {
					if (t.second == false) {
						RenderFont(*_fontInfo,
							penPosition,
							glm::vec4(0.3, 0.3, 0.3, 1),
							"| |"
							);
						RenderFontCr(*_fontInfo,
							penPosition,
							glm::vec4(0.3, 0.3, 0.3, 1),
							"    %5s",
							t.first.c_str()
							);

					}
					else {
						RenderFont(*_fontInfo,
							penPosition,
							glm::vec4(0.3, 0.3, 0.3, 1),
							"|"
							);
						if (t.first == "NH_LORRI") {
							RenderFont(*_fontInfo,
								penPosition,
								firing,
								" + "
								);
						}
						RenderFont(*_fontInfo,
							penPosition,
							glm::vec4(0.3, 0.3, 0.3, 1),
							"  |"
							);
						RenderFontCr(*_fontInfo,
							penPosition,
							active,
							"    %5s",
							t.first.c_str()
							);
					}
				}
			}
		}
#endif
	}
}

void RenderEngine::renderScreenLog() {
	_log->removeExpiredEntries();

	const int max = 10;
	const int category_length = 20;
	const int msg_length = 140;
	std::chrono::seconds fade(5);

	auto entries = _log->entries();
	auto lastEntries = entries.size() > max ? std::make_pair(entries.rbegin(), entries.rbegin() + max) : std::make_pair(entries.rbegin(), entries.rend());

	//                if (entries.size() > max)

	//ScreenLog::const_range ScreenLog::last(size_t n) {
	//	if (_entries.size() > n) {
	//		return std::make_pair(_entries.rbegin(), _entries.rbegin() + n);
	//	} else {
	//		return std::make_pair(_entries.rbegin(), _entries.rend());
	//	}
	//}

	//                auto entries = _log->last(max);

	const glm::vec4 white(0.9, 0.9, 0.9, 1);
	const glm::vec4 red(1, 0, 0, 1);
	const glm::vec4 yellow(1, 1, 0, 1);
	const glm::vec4 green(0, 1, 0, 1);
	const glm::vec4 blue(0, 0, 1, 1);

	size_t nr = 1;
	auto now = std::chrono::steady_clock::now();
	for (auto& it = lastEntries.first; it != lastEntries.second; ++it) {
		const ScreenLog::LogEntry* e = &(*it);

		std::chrono::duration<double> diff = now - e->timeStamp;

		float alpha = 1;
		std::chrono::duration<double> ttf = ScreenLogTimeToLive - fade;
		if (diff > ttf) {
			auto d = (diff - ttf).count();
			auto t = static_cast<float>(d) / static_cast<float>(fade.count());
			float p = 0.8f - t;
			alpha = (p <= 0.f) ? 0.f : pow(p, 0.3f);
		}

		// Since all log entries are ordered, once one exceeds alpha, all have
		if (alpha <= 0.0)
			break;

		const std::string lvl = "(" + ghoul::logging::LogManager::stringFromLevel(e->level) + ")";
		const std::string& message = e->message.substr(0, msg_length);
		nr += std::count(message.begin(), message.end(), '\n');

		RenderFont(*_fontLog,
			glm::vec2(10.f, _fontLog->pointSize() * nr * 2),
			white * alpha,
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

		//                    const float font_with_light = 5;
		RenderFont(*_fontLog,
			glm::vec2(static_cast<float>(10 + 39 * _fontLog->pointSize()), _fontLog->pointSize() * nr * 2),
			color * alpha,
			"%s",									// Format
			lvl.c_str());		// Pad category with "..." if exceeds category_length

		RenderFont(*_fontLog,
			glm::vec2(static_cast<float>(10 + 53 * _fontLog->pointSize()), _fontLog->pointSize() * nr * 2),
			white * alpha,
			"%s",									// Format
			message.c_str());		// Pad category with "..." if exceeds category_length
		++nr;
	}
}

}// namespace openspace
