/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/network/parallelconnection.h>

#ifdef OPENSPACE_MODULE_NEWHORIZONS_ENABLED
#include <modules/newhorizons/util/imagesequencer.h>
#endif
#include <openspace/mission/missionmanager.h>

#include <openspace/rendering/renderer.h>
#include <openspace/rendering/abufferrenderer.h>
#include <openspace/rendering/framebufferrenderer.h>
#include <openspace/rendering/raycastermanager.h>

#include <modules/base/rendering/screenspaceimage.h>
#include <modules/base/rendering/screenspaceframebuffer.h>
#include <openspace/engine/wrapper/windowwrapper.h>

#include <openspace/performance/performancemanager.h>

#include <openspace/documentation/documentationengine.h>
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
#include <ghoul/font/font.h>
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
#include <stack>

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

const std::string RenderEngine::KeyFontMono = "Mono";
const std::string RenderEngine::KeyFontLight = "Light";
const std::vector<RenderEngine::FrametimeType> RenderEngine::FrametimeTypes({
    RenderEngine::FrametimeType::DtTimeAvg,
    RenderEngine::FrametimeType::FPS,
    RenderEngine::FrametimeType::FPSAvg
});

RenderEngine::RenderEngine()
    : _mainCamera(nullptr)
    , _performanceMeasurements("performanceMeasurements", "Performance Measurements")
    , _frametimeType(
        "frametimeType",
        "Type of the frametime display",
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _sceneGraph(nullptr)
    , _renderer(nullptr)
    , _rendererImplementation(RendererImplementation::Invalid)
    , _performanceManager(nullptr)
    , _log(nullptr)
    , _showInfo(true)
    , _showLog(true)
    , _takeScreenshot(false)
    , _showFrameNumber(false)
    , _globalBlackOutFactor(1.f)
    , _fadeDuration(2.f)
    , _currentFadeTime(0.f)
    , _fadeDirection(0)
    , _frameNumber(0)
    //, _frametimeType(FrametimeType::DtTimeAvg)
{
    setName("RenderEngine");

    _performanceMeasurements.onChange([this](){
        if (_performanceMeasurements) {
            if (!_performanceManager) {
                _performanceManager = std::make_unique<performance::PerformanceManager>();
            }
        }
        else {
            _performanceManager = nullptr;
        }

    });
    addProperty(_performanceMeasurements);

    _frametimeType.addOption(
        static_cast<int>(FrametimeType::DtTimeAvg),
        "Average Deltatime"
    );
    _frametimeType.addOption(
        static_cast<int>(FrametimeType::FPS),
        "Frames per second"
    );
    _frametimeType.addOption(
        static_cast<int>(FrametimeType::FPSAvg),
        "Average frames per second"
    );
    addProperty(_frametimeType);
}

RenderEngine::~RenderEngine() {
    delete _sceneGraph;
    _sceneGraph = nullptr;

    delete _mainCamera;
    delete _raycasterManager;

}

bool RenderEngine::deinitialize() {
    for (auto screenspacerenderable : _screenSpaceRenderables) {
        screenspacerenderable->deinitialize();
    }

    MissionManager::deinitialize();

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
    _frameNumber = 0;
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

    _raycasterManager = new RaycasterManager();
    _nAaSamples = OsEng.windowWrapper().currentNumberOfAaSamples();

    LINFO("Seting renderer from string: " << renderingMethod);
    setRendererFromString(renderingMethod);

    // init camera and set temporary position and scaling
    _mainCamera = new Camera();

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

    MissionManager::initialize();

    return true;
}

bool RenderEngine::initializeGL() {
    // TODO:    Fix the power scaled coordinates in such a way that these 
    //            values can be set to more realistic values

    // set the close clip plane and the far clip plane to extreme values while in
    // development
    OsEng.windowWrapper().setNearFarClippingPlane(0.001f, 1000.f);
    
    try {
        const float fontSizeBig = 50.f;
        _fontBig = OsEng.fontManager().font(KeyFontMono, fontSizeBig);
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
    
    
//    sgct::Engine::instance()->setNearAndFarClippingPlanes(0.001f, 1000.0f);
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
     //_mainCamera->setCameraDirection(glm::vec3(0.f, 0.f, -1.f));
        //_mainCamera->setLookUpVector(glm::normalize(upVector));
        //_mainCamera->setLookUpVector(glm::vec3(0.f, 1.f, 0.f));

        // set the initial fov to be 0.0 which means everything will be culled
        //float maxFov = 0.0f;
        float maxFov = std::numeric_limits<float>::max();

        //// for each corner
        //for (int i = 0; i < 4; ++i) {
        //    // calculate radians to corner
        //    glm::vec3 dir = glm::normalize(eyePosition - corners[i]);
        //    float radsbetween = acos(glm::dot(viewdir, dir))
        //        / (glm::length(viewdir) * glm::length(dir));

        //    // the angle to a corner is larger than the current maxima
        //    if (radsbetween > maxFov) {
        //        maxFov = radsbetween;
        //    }
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

void RenderEngine::updateSceneGraph() {
    _sceneGraph->update({
        glm::dvec3(0),
        glm::dmat3(1),
        1,
        Time::ref().j2000Seconds(),
        Time::ref().deltaTime(),
        Time::ref().paused(),
        Time::ref().timeJumped(),
        _performanceManager != nullptr
    });

    _sceneGraph->evaluate(_mainCamera);
    
    //Allow focus node to update camera (enables camera-following)
    //FIX LATER: THIS CAUSES MASTER NODE TO BE ONE FRAME AHEAD OF SLAVES
    //if (const SceneGraphNode* node = OsEng.ref().interactionHandler().focusNode()){
    //node->updateCamera(_mainCamera);
    //}
}

void RenderEngine::updateShaderPrograms() {
    for (auto program : _programs) {
        try {
            if (program->isDirty()) {
                program->rebuildFromFile();
            }
        }
        catch (const ghoul::opengl::ShaderObject::ShaderCompileError& e) {
            LERRORC(e.component, e.what());
        }
    }
}

void RenderEngine::updateRenderer() {
    bool windowResized = OsEng.windowWrapper().windowHasResized();

    if (windowResized) {
        _renderer->setResolution(renderingResolution());

        ghoul::fontrendering::FontRenderer::defaultRenderer().setFramebufferSize(
            fontResolution()
        );
    }

    _renderer->update();
}

void RenderEngine::updateScreenSpaceRenderables() {
    for (auto screenspacerenderable : _screenSpaceRenderables) {
        screenspacerenderable->update();
    }
}

glm::ivec2 RenderEngine::renderingResolution() const {
    if (OsEng.windowWrapper().isRegularRendering()) {
        return OsEng.windowWrapper().currentWindowResolution();
    }
    else {
        return OsEng.windowWrapper().currentDrawBufferResolution();
    }
}

glm::ivec2 RenderEngine::fontResolution() const {
    std::string value;
    bool hasValue = OsEng.configurationManager().getValue(
        ConfigurationManager::KeyOnScreenTextScaling,
        value
    );
    if (hasValue && value == "framebuffer") {
        return OsEng.windowWrapper().currentWindowResolution();
    }
    else {
        // The default is to use the window size
        return OsEng.windowWrapper().currentWindowSize();
    }
}


void RenderEngine::updateFade() {
    //temporary fade funtionality
    float fadedIn = 1.0;
    float fadedOut = 0.0;
    // Don't restart the fade if you've already done it in that direction
    if ((_fadeDirection > 0 && _globalBlackOutFactor == fadedIn)
        || (_fadeDirection < 0 && _globalBlackOutFactor == fadedOut)) {
        _fadeDirection = 0;
    }

    if (_fadeDirection != 0) {
        if (_currentFadeTime > _fadeDuration) {
            _globalBlackOutFactor = _fadeDirection > 0 ? fadedIn : fadedOut;
            _fadeDirection = 0;
        }
        else {
            if (_fadeDirection < 0)
                _globalBlackOutFactor = glm::smoothstep(1.f, 0.f, _currentFadeTime / _fadeDuration);
            else
                _globalBlackOutFactor = glm::smoothstep(0.f, 1.f, _currentFadeTime / _fadeDuration);
            _currentFadeTime += static_cast<float>(OsEng.windowWrapper().averageDeltaTime());
        }
    }
}

void RenderEngine::render(const glm::mat4& projectionMatrix, const glm::mat4& viewMatrix){
    _mainCamera->sgctInternal.setViewMatrix(viewMatrix);
    _mainCamera->sgctInternal.setProjectionMatrix(projectionMatrix);

    if (!(OsEng.isMaster() && _disableMasterRendering) && !OsEng.windowWrapper().isGuiWindow()) {
        _renderer->render(_globalBlackOutFactor, _performanceManager != nullptr);
    }

    // Print some useful information on the master viewport
    if (OsEng.isMaster() && OsEng.windowWrapper().isSimpleRendering()) {
        renderInformation();
    }

    glm::vec2 penPosition = glm::vec2(
        OsEng.windowWrapper().viewportPixelCoordinates().y / 2 - 50,
        OsEng.windowWrapper().viewportPixelCoordinates().w / 3
        );

    if(_showFrameNumber) {
        RenderFontCr(*_fontBig, penPosition, "%i", _frameNumber);
    }
    
    _frameNumber++;

    
    for (auto screenSpaceRenderable : _screenSpaceRenderables) {
        if (screenSpaceRenderable->isEnabled() && screenSpaceRenderable->isReady())
            screenSpaceRenderable->render();
    }
}

void RenderEngine::renderShutdownInformation(float timer, float fullTime) {
    timer = timer < 0.f ? 0.f : timer;

    auto size = ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
        *_fontDate,
        "Shutdown in: %.2fs/%.2fs",
        timer,
        fullTime
    );

    glm::vec2 penPosition = glm::vec2(
        OsEng.windowWrapper().viewportPixelCoordinates().y - size.boundingBox.x,
        OsEng.windowWrapper().viewportPixelCoordinates().w - size.boundingBox.y
    );
    penPosition.y -= _fontDate->height();

    RenderFontCr(
        *_fontDate,
        penPosition,
        "Shutdown in: %.2fs/%.2fs",
        timer,
        fullTime
    );
}

void RenderEngine::postDraw() {
    if (Time::ref().timeJumped()) {
        Time::ref().setTimeJumped(false);
    }

    if (_takeScreenshot) {
        OsEng.windowWrapper().takeScreenshot(_applyWarping);
        _takeScreenshot = false;
    }

    if (_performanceManager) {
        _performanceManager->storeScenePerformanceMeasurements(scene()->allSceneGraphNodes());
    }
}

void RenderEngine::takeScreenshot(bool applyWarping) {
    _takeScreenshot = true;
    _applyWarping = applyWarping;
}

void RenderEngine::toggleInfoText(bool b) {
    _showInfo = b;
}

Scene* RenderEngine::scene() {
    ghoul_assert(_sceneGraph, "Scenegraph not initialized");
    return _sceneGraph;
}

RaycasterManager& RenderEngine::raycasterManager() {
    return *_raycasterManager;
}

void RenderEngine::setSceneGraph(Scene* sceneGraph) {
    _sceneGraph = sceneGraph;
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
    if (!program)
        return;

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
* Set resolve data
* Called from the renderer, whenever it needs to update
* the dictionary of all post rendering programs.
*/
void RenderEngine::setResolveData(const ghoul::Dictionary& data) {
    _resolveData = data;
    for (auto program : _programs) {
        ghoul::Dictionary dict = program->dictionary();
        dict.setValue("resolveData", _resolveData);
        program->setDictionary(dict);
    }
}

/**
* Set raycasting uniforms on the program object, and setup raycasting.
*/
void RenderEngine::preRaycast(ghoul::opengl::ProgramObject& programObject) {
    _renderer->preRaycast(programObject);
}

/**
* Tear down raycasting for the specified program object.
*/
void RenderEngine::postRaycast(ghoul::opengl::ProgramObject& programObject) {
    _renderer->postRaycast(programObject);
}



/**
 * Set renderer
 */
void RenderEngine::setRenderer(std::unique_ptr<Renderer> renderer) {
    if (_renderer) {
        _renderer->deinitialize();
    }

    _renderer = std::move(renderer);
    _renderer->setResolution(renderingResolution());
    _renderer->setNAaSamples(_nAaSamples);
    _renderer->initialize();
    _renderer->setCamera(_mainCamera);
    _renderer->setScene(_sceneGraph);
}


void RenderEngine::setNAaSamples(int nAaSamples) {
    _nAaSamples = nAaSamples;
    if (_renderer) {
        _renderer->setNAaSamples(_nAaSamples);
    }
}

scripting::LuaLibrary RenderEngine::luaLibrary() {
    return {
        "",
        {
            {
                "takeScreenshot",
                &luascriptfunctions::takeScreenshot,
                "(optional bool)",
                "Renders the current image to a file on disk. If the boolean parameter "
                "is set to 'true', the screenshot will include the blending and the "
                "meshes. If it is 'false', the straight FBO will be recorded."
            },
            {
                "setRenderer",
                &luascriptfunctions::setRenderer,
                "string",
                "Sets the renderer (ABuffer or FrameBuffer)"
            },
            {
                "setNAaSamples",
                &luascriptfunctions::setNAaSamples,
                "int",
                "Sets the number of anti-aliasing (MSAA) samples"
            },
            {
                "showRenderInformation",
                &luascriptfunctions::showRenderInformation,
                "bool",
                "Toggles the showing of render information on-screen text"
            },
            {
                "toggleFade",
                &luascriptfunctions::toggleFade,
                "number",
                "Toggles fading in or out"
            },
            {
                "fadeIn",
                &luascriptfunctions::fadeIn,
                "number",
                ""
            },
            //also temporary @JK
            {
                "fadeOut",
                &luascriptfunctions::fadeOut,
                "number",
                ""
            },
            {
                "registerScreenSpaceRenderable",
                &luascriptfunctions::registerScreenSpaceRenderable,
                "table",
                "Will create a ScreenSpaceRenderable from a lua Table and register it in the RenderEngine"
            },
            {
                "unregisterScreenSpaceRenderable",
                &luascriptfunctions::unregisterScreenSpaceRenderable,
                "string",
                "Given a ScreenSpaceRenderable name this script will remove it from the renderengine"
            },
        },
    };
}

bool RenderEngine::doesPerformanceMeasurements() const {
    return _performanceManager != nullptr;
}

performance::PerformanceManager* RenderEngine::performanceManager() {
    return _performanceManager.get();
}

// This method is temporary and will be removed once the scalegraph is in effect ---abock
void RenderEngine::changeViewPoint(std::string origin) {
//    SceneGraphNode* solarSystemBarycenterNode = scene()->sceneGraphNode("SolarSystemBarycenter");
//    SceneGraphNode* plutoBarycenterNode = scene()->sceneGraphNode("PlutoBarycenter");
//    SceneGraphNode* newHorizonsNode = scene()->sceneGraphNode("NewHorizons");
//    SceneGraphNode* newHorizonsPathNodeJ = scene()->sceneGraphNode("NewHorizonsPathJupiter");
//    SceneGraphNode* newHorizonsPathNodeP = scene()->sceneGraphNode("NewHorizonsPathPluto");
////    SceneGraphNode* cg67pNode = scene()->sceneGraphNode("67P");
////    SceneGraphNode* rosettaNode = scene()->sceneGraphNode("Rosetta");
//
//    RenderablePath* nhPath;
//
//    SceneGraphNode* jupiterBarycenterNode = scene()->sceneGraphNode("JupiterBarycenter");
//
//    //SceneGraphNode* newHorizonsGhostNode = scene()->sceneGraphNode("NewHorizonsGhost");
//    //SceneGraphNode* dawnNode = scene()->sceneGraphNode("Dawn");
//    //SceneGraphNode* vestaNode = scene()->sceneGraphNode("Vesta");
//
//  //  if (solarSystemBarycenterNode == nullptr || plutoBarycenterNode == nullptr || 
//        //jupiterBarycenterNode == nullptr) {
//     //   LERROR("Necessary nodes does not exist");
//        //return;
//  //  }
//
//    if (origin == "Pluto") {
//        if (newHorizonsPathNodeP) {
//            Renderable* R = newHorizonsPathNodeP->renderable();
//            newHorizonsPathNodeP->setParent(plutoBarycenterNode);
//            nhPath = static_cast<RenderablePath*>(R);
//            nhPath->calculatePath("PLUTO BARYCENTER");
//        }
//
//        plutoBarycenterNode->setParent(scene()->sceneGraphNode("SolarSystem"));
//        plutoBarycenterNode->setEphemeris(new StaticEphemeris);
//        
//        solarSystemBarycenterNode->setParent(plutoBarycenterNode);
//        newHorizonsNode->setParent(plutoBarycenterNode);
//        //newHorizonsGhostNode->setParent(plutoBarycenterNode);
//
//        //dawnNode->setParent(plutoBarycenterNode);
//        //vestaNode->setParent(plutoBarycenterNode);
//
//        //newHorizonsTrailNode->setParent(plutoBarycenterNode);
//        ghoul::Dictionary solarDictionary =
//        {
//            { std::string("Type"), std::string("Spice") },
//            { std::string("Body"), std::string("SUN") },
//            { std::string("Reference"), std::string("GALACTIC") },
//            { std::string("Observer"), std::string("PLUTO BARYCENTER") },
//            { std::string("Kernels"), ghoul::Dictionary() }
//        };
//        
//        ghoul::Dictionary jupiterDictionary =
//        {
//            { std::string("Type"), std::string("Spice") },
//            { std::string("Body"), std::string("JUPITER BARYCENTER") },
//            { std::string("Reference"), std::string("GALACTIC") },
//            { std::string("Observer"), std::string("PLUTO BARYCENTER") },
//            { std::string("Kernels"), ghoul::Dictionary() }
//        };
//
//        ghoul::Dictionary newHorizonsDictionary =
//        {
//            { std::string("Type"), std::string("Spice") },
//            { std::string("Body"), std::string("NEW HORIZONS") },
//            { std::string("Reference"), std::string("GALACTIC") },
//            { std::string("Observer"), std::string("PLUTO BARYCENTER") },
//            { std::string("Kernels"), ghoul::Dictionary() }
//        };
//
//        solarSystemBarycenterNode->setEphemeris(new SpiceEphemeris(solarDictionary));
//        jupiterBarycenterNode->setEphemeris(new SpiceEphemeris(jupiterDictionary));
//        newHorizonsNode->setEphemeris(new SpiceEphemeris(newHorizonsDictionary));
//        //newHorizonsTrailNode->setEphemeris(new SpiceEphemeris(newHorizonsDictionary));
//
//
//        //ghoul::Dictionary dawnDictionary =
//        //{
//        //    { std::string("Type"), std::string("Spice") },
//        //    { std::string("Body"), std::string("DAWN") },
//        //    { std::string("Reference"), std::string("GALACTIC") },
//        //    { std::string("Observer"), std::string("PLUTO BARYCENTER") },
//        //    { std::string("Kernels"), ghoul::Dictionary() }
//        //};
//        //dawnNode->setEphemeris(new SpiceEphemeris(dawnDictionary));
//        //
//        //ghoul::Dictionary vestaDictionary =
//        //{
//        //      { std::string("Type"), std::string("Spice") },
//        //      { std::string("Body"), std::string("VESTA") },
//        //      { std::string("Reference"), std::string("GALACTIC") },
//        //      { std::string("Observer"), std::string("PLUTO BARYCENTER") },
//        //      { std::string("Kernels"), ghoul::Dictionary() }
//        //};
//        //vestaNode->setEphemeris(new SpiceEphemeris(vestaDictionary));
//
//        
//        //ghoul::Dictionary newHorizonsGhostDictionary =
//        //{
//        //    { std::string("Type"), std::string("Spice") },
//        //    { std::string("Body"), std::string("NEW HORIZONS") },
//        //    { std::string("EphmerisGhosting"), std::string("TRUE") },
//        //    { std::string("Reference"), std::string("GALACTIC") },
//        //    { std::string("Observer"), std::string("PLUTO BARYCENTER") },
//        //    { std::string("Kernels"), ghoul::Dictionary() }
//        //};
//        //newHorizonsGhostNode->setEphemeris(new SpiceEphemeris(newHorizonsGhostDictionary));
//        
//        return;
//    }
//    if (origin == "Sun") {
//        solarSystemBarycenterNode->setParent(scene()->sceneGraphNode("SolarSystem"));
//
//        if (plutoBarycenterNode)
//            plutoBarycenterNode->setParent(solarSystemBarycenterNode);
//        jupiterBarycenterNode->setParent(solarSystemBarycenterNode);
//        if (newHorizonsNode)
//            newHorizonsNode->setParent(solarSystemBarycenterNode);
//        //newHorizonsGhostNode->setParent(solarSystemBarycenterNode);
//
//        //newHorizonsTrailNode->setParent(solarSystemBarycenterNode);
//        //dawnNode->setParent(solarSystemBarycenterNode);
//        //vestaNode->setParent(solarSystemBarycenterNode);
//
//        ghoul::Dictionary plutoDictionary =
//        {
//            { std::string("Type"), std::string("Spice") },
//            { std::string("Body"), std::string("PLUTO BARYCENTER") },
//            { std::string("Reference"), std::string("GALACTIC") },
//            { std::string("Observer"), std::string("SUN") },
//            { std::string("Kernels"), ghoul::Dictionary() }
//        };
//        ghoul::Dictionary jupiterDictionary =
//        {
//            { std::string("Type"), std::string("Spice") },
//            { std::string("Body"), std::string("JUPITER BARYCENTER") },
//            { std::string("Reference"), std::string("GALACTIC") },
//            { std::string("Observer"), std::string("SUN") },
//            { std::string("Kernels"), ghoul::Dictionary() }
//        };
//        
//        solarSystemBarycenterNode->setEphemeris(new StaticEphemeris);
//        jupiterBarycenterNode->setEphemeris(new SpiceEphemeris(jupiterDictionary));
//        if (plutoBarycenterNode)
//            plutoBarycenterNode->setEphemeris(new SpiceEphemeris(plutoDictionary));
//
//        ghoul::Dictionary newHorizonsDictionary =
//        {
//            { std::string("Type"), std::string("Spice") },
//            { std::string("Body"), std::string("NEW HORIZONS") },
//            { std::string("Reference"), std::string("GALACTIC") },
//            { std::string("Observer"), std::string("SUN") },
//            { std::string("Kernels"), ghoul::Dictionary() }
//        };
//        if (newHorizonsNode)
//            newHorizonsNode->setEphemeris(new SpiceEphemeris(newHorizonsDictionary));
//        //newHorizonsTrailNode->setEphemeris(new SpiceEphemeris(newHorizonsDictionary));
//
//        
//        //ghoul::Dictionary dawnDictionary =
//        //{
//        //    { std::string("Type"), std::string("Spice") },
//        //    { std::string("Body"), std::string("DAWN") },
//        //    { std::string("Reference"), std::string("GALACTIC") },
//        //    { std::string("Observer"), std::string("SUN") },
//        //    { std::string("Kernels"), ghoul::Dictionary() }
//        //};
//        //dawnNode->setEphemeris(new SpiceEphemeris(dawnDictionary));
//        //
//        //ghoul::Dictionary vestaDictionary =
//        //{
//        //    { std::string("Type"), std::string("Spice") },
//        //    { std::string("Body"), std::string("VESTA") },
//        //    { std::string("Reference"), std::string("GALACTIC") },
//        //    { std::string("Observer"), std::string("SUN") },
//        //    { std::string("Kernels"), ghoul::Dictionary() }
//        //};
//        //vestaNode->setEphemeris(new SpiceEphemeris(vestaDictionary));
//        
//        
//        //ghoul::Dictionary newHorizonsGhostDictionary =
//        //{
//        //    { std::string("Type"), std::string("Spice") },
//        //    { std::string("Body"), std::string("NEW HORIZONS") },
//        //    { std::string("EphmerisGhosting"), std::string("TRUE") },
//        //    { std::string("Reference"), std::string("GALACTIC") },
//        //    { std::string("Observer"), std::string("JUPITER BARYCENTER") },
//        //    { std::string("Kernels"), ghoul::Dictionary() }
//        //};
//        //newHorizonsGhostNode->setEphemeris(new SpiceEphemeris(newHorizonsGhostDictionary));
//        
//        return;
//    }
//    if (origin == "Jupiter") {
//        if (newHorizonsPathNodeJ) {
//            Renderable* R = newHorizonsPathNodeJ->renderable();
//            newHorizonsPathNodeJ->setParent(jupiterBarycenterNode);
//            nhPath = static_cast<RenderablePath*>(R);
//            nhPath->calculatePath("JUPITER BARYCENTER");
//        }
//
//        jupiterBarycenterNode->setParent(scene()->sceneGraphNode("SolarSystem"));
//        jupiterBarycenterNode->setEphemeris(new StaticEphemeris);
//
//        solarSystemBarycenterNode->setParent(jupiterBarycenterNode);
//        if (newHorizonsNode)
//            newHorizonsNode->setParent(jupiterBarycenterNode);
//        //newHorizonsTrailNode->setParent(jupiterBarycenterNode);
//
//        //dawnNode->setParent(jupiterBarycenterNode);
//        //vestaNode->setParent(jupiterBarycenterNode);
//
//
//        ghoul::Dictionary solarDictionary =
//        {
//            { std::string("Type"), std::string("Spice") },
//            { std::string("Body"), std::string("SUN") },
//            { std::string("Reference"), std::string("GALACTIC") },
//            { std::string("Observer"), std::string("JUPITER BARYCENTER") },
//            { std::string("Kernels"), ghoul::Dictionary() }
//        };
//
//        ghoul::Dictionary plutoDictionary =
//        {
//            { std::string("Type"), std::string("Spice") },
//            { std::string("Body"), std::string("PlUTO BARYCENTER") },
//            { std::string("Reference"), std::string("GALACTIC") },
//            { std::string("Observer"), std::string("JUPITER BARYCENTER") },
//            { std::string("Kernels"), ghoul::Dictionary() }
//        };
//
//        ghoul::Dictionary newHorizonsDictionary =
//        {
//            { std::string("Type"), std::string("Spice") },
//            { std::string("Body"), std::string("NEW HORIZONS") },
//            { std::string("Reference"), std::string("GALACTIC") },
//            { std::string("Observer"), std::string("JUPITER BARYCENTER") },
//            { std::string("Kernels"), ghoul::Dictionary() }
//        };
//        solarSystemBarycenterNode->setEphemeris(new SpiceEphemeris(solarDictionary));
//        if (plutoBarycenterNode)
//            plutoBarycenterNode->setEphemeris(new SpiceEphemeris(plutoDictionary));
//        if (newHorizonsNode)
//            newHorizonsNode->setEphemeris(new SpiceEphemeris(newHorizonsDictionary));
//        //newHorizonsGhostNode->setParent(jupiterBarycenterNode);
//        //newHorizonsTrailNode->setEphemeris(new SpiceEphemeris(newHorizonsDictionary));
//
//
//        //ghoul::Dictionary dawnDictionary =
//        //{
//        //    { std::string("Type"), std::string("Spice") },
//        //    { std::string("Body"), std::string("DAWN") },
//        //    { std::string("Reference"), std::string("GALACTIC") },
//        //    { std::string("Observer"), std::string("JUPITER BARYCENTER") },
//        //    { std::string("Kernels"), ghoul::Dictionary() }
//        //};
//        //dawnNode->setEphemeris(new SpiceEphemeris(dawnDictionary));
//        //
//        //ghoul::Dictionary vestaDictionary =
//        //{
//        //    { std::string("Type"), std::string("Spice") },
//        //    { std::string("Body"), std::string("VESTA") },
//        //    { std::string("Reference"), std::string("GALACTIC") },
//        //    { std::string("Observer"), std::string("JUPITER BARYCENTER") },
//        //    { std::string("Kernels"), ghoul::Dictionary() }
//        //};
//        //vestaNode->setEphemeris(new SpiceEphemeris(vestaDictionary));
//
//
//        
//        //ghoul::Dictionary newHorizonsGhostDictionary =
//        //{
//        //    { std::string("Type"), std::string("Spice") },
//        //    { std::string("Body"), std::string("NEW HORIZONS") },
//        //    { std::string("EphmerisGhosting"), std::string("TRUE") },
//        //    { std::string("Reference"), std::string("GALACTIC") },
//        //    { std::string("Observer"), std::string("JUPITER BARYCENTER") },
//        //    { std::string("Kernels"), ghoul::Dictionary() }
//        //};
//        //newHorizonsGhostNode->setEphemeris(new SpiceEphemeris(newHorizonsGhostDictionary));
//        //newHorizonsGhostNode->setParent(jupiterBarycenterNode);
//
//    
//        return;
//    }
//    //if (origin == "Vesta") {
//    //    
//    //    vestaNode->setParent(scene()->sceneGraphNode("SolarSystem"));
//    //    vestaNode->setEphemeris(new StaticEphemeris);
//    //
//    //    solarSystemBarycenterNode->setParent(vestaNode);
//    //    newHorizonsNode->setParent(vestaNode);
//    //
//    //    dawnNode->setParent(vestaNode);
//    //    plutoBarycenterNode->setParent(vestaNode);
//    //
//    //
//    //    ghoul::Dictionary plutoDictionary =
//    //    {
//    //        { std::string("Type"), std::string("Spice") },
//    //        { std::string("Body"), std::string("PLUTO BARYCENTER") },
//    //        { std::string("Reference"), std::string("GALACTIC") },
//    //        { std::string("Observer"), std::string("VESTA") },
//    //        { std::string("Kernels"), ghoul::Dictionary() }
//    //    };
//    //    ghoul::Dictionary solarDictionary =
//    //    {
//    //        { std::string("Type"), std::string("Spice") },
//    //        { std::string("Body"), std::string("SUN") },
//    //        { std::string("Reference"), std::string("GALACTIC") },
//    //        { std::string("Observer"), std::string("VESTA") },
//    //        { std::string("Kernels"), ghoul::Dictionary() }
//    //    };
//    //
//    //    ghoul::Dictionary jupiterDictionary =
//    //    {
//    //        { std::string("Type"), std::string("Spice") },
//    //        { std::string("Body"), std::string("JUPITER BARYCENTER") },
//    //        { std::string("Reference"), std::string("GALACTIC") },
//    //        { std::string("Observer"), std::string("VESTA") },
//    //        { std::string("Kernels"), ghoul::Dictionary() }
//    //    };
//    //
//    //    solarSystemBarycenterNode->setEphemeris(new SpiceEphemeris(solarDictionary));
//    //    plutoBarycenterNode->setEphemeris(new SpiceEphemeris(plutoDictionary));
//    //    jupiterBarycenterNode->setEphemeris(new SpiceEphemeris(jupiterDictionary));
//    //
//    //    ghoul::Dictionary newHorizonsDictionary =
//    //    {
//    //        { std::string("Type"), std::string("Spice") },
//    //        { std::string("Body"), std::string("NEW HORIZONS") },
//    //        { std::string("Reference"), std::string("GALACTIC") },
//    //        { std::string("Observer"), std::string("VESTA") },
//    //        { std::string("Kernels"), ghoul::Dictionary() }
//    //    };
//    //    newHorizonsNode->setEphemeris(new SpiceEphemeris(newHorizonsDictionary));
//    //
//    //    ghoul::Dictionary dawnDictionary =
//    //    {
//    //        { std::string("Type"), std::string("Spice") },
//    //        { std::string("Body"), std::string("DAWN") },
//    //        { std::string("Reference"), std::string("GALACTIC") },
//    //        { std::string("Observer"), std::string("VESTA") },
//    //        { std::string("Kernels"), ghoul::Dictionary() }
//    //    };
//    //    dawnNode->setEphemeris(new SpiceEphemeris(dawnDictionary));
//    //    vestaNode->setEphemeris(new StaticEphemeris);
//    //
//    //    return;
//    //}
//
//    if (origin == "67P") {
//        SceneGraphNode* rosettaNode = scene()->sceneGraphNode("Rosetta");
//        SceneGraphNode* cgNode = scene()->sceneGraphNode("67P");
//        //jupiterBarycenterNode->setParent(solarSystemBarycenterNode);
//        //plutoBarycenterNode->setParent(solarSystemBarycenterNode);
//        solarSystemBarycenterNode->setParent(cgNode);
//        rosettaNode->setParent(cgNode);
//        
//        ghoul::Dictionary solarDictionary =
//            {
//            { std::string("Type"), std::string("Spice") },
//                { std::string("Body"), std::string("SUN") },
//                { std::string("Reference"), std::string("GALACTIC") },
//                { std::string("Observer"), std::string("CHURYUMOV-GERASIMENKO") },
//                { std::string("Kernels"), ghoul::Dictionary() }
//            };
//        solarSystemBarycenterNode->setEphemeris(new SpiceEphemeris(solarDictionary));
//        
//        ghoul::Dictionary rosettaDictionary =
//            {
//            { std::string("Type"), std::string("Spice") },
//                { std::string("Body"), std::string("ROSETTA") },
//                { std::string("Reference"), std::string("GALACTIC") },
//                { std::string("Observer"), std::string("CHURYUMOV-GERASIMENKO") },
//                { std::string("Kernels"), ghoul::Dictionary() }
//            };
//        
//        cgNode->setParent(scene()->sceneGraphNode("SolarSystem"));
//        rosettaNode->setEphemeris(new SpiceEphemeris(rosettaDictionary));
//        cgNode->setEphemeris(new StaticEphemeris);
//        
//        return;
//        
//    }
//
//    LFATAL("This function is being misused with an argument of '" << origin << "'");
}

void RenderEngine::setShowFrameNumber(bool enabled){
    _showFrameNumber = enabled;
}

void RenderEngine::setDisableRenderingOnMaster(bool enabled) {
    _disableMasterRendering = enabled;
}

void RenderEngine::registerScreenSpaceRenderable(std::shared_ptr<ScreenSpaceRenderable> s)
{
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

void RenderEngine::unregisterScreenSpaceRenderable(std::string name){
    auto s = screenSpaceRenderable(name);
    if(s)
        unregisterScreenSpaceRenderable(s);
}

std::shared_ptr<ScreenSpaceRenderable> RenderEngine::screenSpaceRenderable(std::string name){
    for(auto s : _screenSpaceRenderables){
        if(s->name() == name){
            return s;
        }
    }
    return nullptr;
}

std::vector<ScreenSpaceRenderable*> RenderEngine::screenSpaceRenderables() const {
    std::vector<ScreenSpaceRenderable*> res(_screenSpaceRenderables.size());
    std::transform(
        _screenSpaceRenderables.begin(),
        _screenSpaceRenderables.end(),
        res.begin(),
        [](std::shared_ptr<ScreenSpaceRenderable> p) { return p.get(); }
    );
    return res;
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

std::string RenderEngine::progressToStr(int size, double t) {
    std::string progress = "|";
    int g = static_cast<int>((t * (size - 1)) + 1);
    g = std::max(g, 0);
    for (int i = 0; i < g; i++)
        progress.append("-");
    progress.append(">");
    for (int i = 0; i < size - g; i++)
        progress.append(" ");
    progress.append("|");
    return progress;
}

void RenderEngine::renderInformation() {
    // TODO: Adjust font_size properly when using retina screen
    using Font = ghoul::fontrendering::Font;
    using ghoul::fontrendering::RenderFont;

    if (_fontDate) {
        glm::vec2 penPosition = glm::vec2(
            10.f,
            fontResolution().y
            //OsEng.windowWrapper().viewportPixelCoordinates().w
        );
        penPosition.y -= _fontDate->height();

        if (_showInfo && _fontDate) {
            RenderFontCr(*_fontDate,
                penPosition,
                "Date: %s",
                Time::ref().UTC().c_str()
            );
        }
        if (_showInfo && _fontInfo) {
            RenderFontCr(*_fontInfo,
                         penPosition,
                         "Simulation increment (s): %.0f",
                         Time::ref().deltaTime()
            );

            FrametimeType frametimeType = FrametimeType(_frametimeType.value());
            switch (frametimeType) {
                case FrametimeType::DtTimeAvg:
                    RenderFontCr(*_fontInfo,
                                 penPosition,
                                 "Avg. Frametime: %.5f",
                                 OsEng.windowWrapper().averageDeltaTime()
                    );
                    break;
                case FrametimeType::FPS:
                    RenderFontCr(*_fontInfo,
                                 penPosition,
                                 "FPS: %3.2f",
                                 1.0 / OsEng.windowWrapper().deltaTime()
                    );
                    break;
                case FrametimeType::FPSAvg:
                    RenderFontCr(*_fontInfo,
                                 penPosition,
                                 "Avg. FPS: %3.2f",
                                 1.0 / OsEng.windowWrapper().averageDeltaTime()
                    );
                    break;
                default:
                    RenderFontCr(*_fontInfo,
                                 penPosition,
                                 "Avg. Frametime: %.5f",
                                 OsEng.windowWrapper().averageDeltaTime()
                    );
                    break;
            }

        ParallelConnection::Status status = OsEng.parallelConnection().status();
        size_t nConnections = OsEng.parallelConnection().nConnections();
        const std::string& hostName = OsEng.parallelConnection().hostName();

        std::string connectionInfo = "";
        int nClients = nConnections;
        if (status == ParallelConnection::Status::Host) {
            nClients--;
            if (nClients == 1) {
                connectionInfo = "Hosting session with 1 client";
            } else {
                connectionInfo = "Hosting session with " + std::to_string(nClients) + " clients";
            }
        } else if (status == ParallelConnection::Status::ClientWithHost) {
            nClients--;
            connectionInfo = "Session hosted by '" + hostName + "'";
        } else if (status == ParallelConnection::Status::ClientWithoutHost) {
            connectionInfo = "Host is disconnected";
        }

        if (status == ParallelConnection::Status::ClientWithHost ||
            status == ParallelConnection::Status::ClientWithoutHost) {
            connectionInfo += "\n";
            if (nClients > 2) {
                connectionInfo += "You and " + std::to_string(nClients - 1) + " more clients are tuned in";
            } else if (nClients == 2) {
                connectionInfo += "You and " + std::to_string(nClients - 1) + " more client are tuned in";
            } else if (nClients == 1) {
                connectionInfo += "You are the only client";
            }
        }

        if (connectionInfo != "") {
            RenderFontCr(*_fontInfo,
                penPosition,
                connectionInfo.c_str()
            );
        }


#ifdef OPENSPACE_MODULE_NEWHORIZONS_ENABLED
//<<<<<<< HEAD
        bool hasNewHorizons = scene()->sceneGraphNode("NewHorizons");
        double currentTime = Time::ref().j2000Seconds();

        if (MissionManager::ref().hasCurrentMission()) {

            const Mission& mission = MissionManager::ref().currentMission();
//=======
//            bool hasNewHorizons = scene()->sceneGraphNode("NewHorizons");
//            double currentTime = Time::ref().currentTime();
//>>>>>>> develop
//
//            if (MissionManager::ref().hasCurrentMission()) {
//
//                const Mission& mission = MissionManager::ref().currentMission();

                if (mission.phases().size() > 0) {

                    static const glm::vec4 nextMissionColor(0.7, 0.3, 0.3, 1);
                    //static const glm::vec4 missionProgressColor(0.4, 1.0, 1.0, 1);
                    static const glm::vec4 currentMissionColor(0.0, 0.5, 0.5, 1);
                    static const glm::vec4 missionProgressColor = currentMissionColor;// (0.4, 1.0, 1.0, 1);
                    static const glm::vec4 currentLeafMissionColor = missionProgressColor;
                    static const glm::vec4 nonCurrentMissionColor(0.3, 0.3, 0.3, 1);

                    // Add spacing
                    RenderFontCr(*_fontInfo, penPosition, nonCurrentMissionColor, " ");

                    auto phaseTrace = mission.phaseTrace(currentTime);

                    if (phaseTrace.size()) {
                        const MissionPhase& phase = phaseTrace.back().get();
                        std::string title = "Current Mission Phase: " + phase.name();
                        RenderFontCr(*_fontInfo, penPosition, missionProgressColor, title.c_str());
                        double remaining = phase.timeRange().end - currentTime;
                        float t = static_cast<float>(1.0 - remaining / phase.timeRange().duration());
                        std::string progress = progressToStr(25, t);
                        //RenderFontCr(*_fontInfo, penPosition, missionProgressColor,
                        //   "%.0f s %s %.1f %%", remaining, progress.c_str(), t * 100);
                    }
                    else {
                        RenderFontCr(*_fontInfo, penPosition, nextMissionColor, "Next Mission:");
                        double remaining = mission.timeRange().start - currentTime;
                        RenderFontCr(*_fontInfo, penPosition, nextMissionColor,
                            "%.0f s", remaining);
                    }

                    bool showAllPhases = false;

                    typedef std::pair<const MissionPhase*, int> PhaseWithDepth;
                    std::stack<PhaseWithDepth> S;
                    int pixelIndentation = 20;
                    S.push({ &mission, 0 });
                    while (!S.empty()) {
                        const MissionPhase* phase = S.top().first;
                        int depth = S.top().second;
                        S.pop();

                        bool isCurrentPhase = phase->timeRange().includes(currentTime);

                        penPosition.x += depth * pixelIndentation;
                        if (isCurrentPhase) {
                            double remaining = phase->timeRange().end - currentTime;
                            float t = static_cast<float>(1.0 - remaining / phase->timeRange().duration());
                            std::string progress = progressToStr(25, t);
                            RenderFontCr(*_fontInfo, penPosition, currentMissionColor,
                                "%s  %s %.1f %%",
                                phase->name().c_str(),
                                progress.c_str(),
                                t * 100
                                );
                        }
                        else {
                            RenderFontCr(*_fontInfo, penPosition, nonCurrentMissionColor, phase->name().c_str());
                        }
                        penPosition.x -= depth * pixelIndentation;

                        if (isCurrentPhase || showAllPhases) {
                            // phases are sorted increasingly by start time, and will be popped
                            // last-in-first-out from the stack, so add them in reversed order.
                            int indexLastPhase = phase->phases().size() - 1;
                            for (int i = indexLastPhase; 0 <= i; --i) {
                                S.push({ &phase->phases()[i], depth + 1 });
                            }
                        }
                    }
                }
            }



            if (openspace::ImageSequencer::ref().isReady()) {
                penPosition.y -= 25.f;

                glm::vec4 targetColor(0.00, 0.75, 1.00, 1);

                if (hasNewHorizons) {
                    try {
                        double lt;
                        glm::dvec3 p =
                            SpiceManager::ref().targetPosition("PLUTO", "NEW HORIZONS", "GALACTIC", {}, currentTime, lt);
                        psc nhPos = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);
                        float a, b, c;
                        glm::dvec3 radii;
                        SpiceManager::ref().getValue("PLUTO", "RADII", radii);
                        a = radii.x;
                        b = radii.y;
                        float radius = (a + b) / 2.f;
                        float distToSurf = glm::length(nhPos.vec3()) - radius;

                        RenderFont(*_fontInfo,
                                   penPosition,
                                   "Distance to Pluto: % .1f (KM)",
                                   distToSurf
                        );
                        penPosition.y -= _fontInfo->height();
                    }
                    catch (...) {
                    }
                }

                double remaining = openspace::ImageSequencer::ref().getNextCaptureTime() - currentTime;
                float t = static_cast<float>(1.0 - remaining / openspace::ImageSequencer::ref().getIntervalLength());

                std::string str = SpiceManager::ref().dateFromEphemerisTime(
                    ImageSequencer::ref().getNextCaptureTime(),
                    "YYYY MON DD HR:MN:SC"
                    );

                glm::vec4 active(0.6, 1, 0.00, 1);
                glm::vec4 brigther_active(0.9, 1, 0.75, 1);

                if (remaining > 0) {
                
                    std::string progress = progressToStr(25, t);
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
                std::pair<double, std::string> nextTarget = ImageSequencer::ref().getNextTarget();
                std::pair<double, std::string> currentTarget = ImageSequencer::ref().getCurrentTarget();

                if (currentTarget.first > 0.0) {
                    int timeleft = static_cast<int>(nextTarget.first - currentTime);

                    int hour = timeleft / 3600;
                    int second = timeleft % 3600;
                    int minute = second / 60;
                    second = second % 60;

                    std::string hh, mm, ss;

                    if (hour   < 10)
                        hh.append("0");
                    if (minute < 10)
                        mm.append("0");
                    if (second < 10)
                        ss.append("0");

                    hh.append(std::to_string(hour));
                    mm.append(std::to_string(minute));
                    ss.append(std::to_string(second));

                    RenderFontCr(*_fontInfo,
                        penPosition,
                        targetColor,
                        "Data acquisition adjacency: [%s:%s:%s]",
                        hh.c_str(), mm.c_str(), ss.c_str()
                        );

    #if 0
    // Why is it (2) in the original? ---abock
                    //std::pair<double, std::vector<std::string>> incidentTargets = ImageSequencer::ref().getIncidentTargetList(0);
                    //std::pair<double, std::vector<std::string>> incidentTargets = ImageSequencer::ref().getIncidentTargetList(2);
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
    #endif
                    penPosition.y -= _fontInfo->height();

                    std::map<std::string, bool> activeMap = ImageSequencer::ref().getActiveInstruments();
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
        }
#endif
    }
}

void RenderEngine::renderScreenLog() {
    if (!_showLog)
        return;

    _log->removeExpiredEntries();

    const int max = 10;
    const int category_length = 20;
    const int msg_length = 140;
    std::chrono::seconds fade(5);

    auto entries = _log->entries();
    auto lastEntries = entries.size() > max ? std::make_pair(entries.rbegin(), entries.rbegin() + max) : std::make_pair(entries.rbegin(), entries.rend());

    //                if (entries.size() > max)

    //ScreenLog::const_range ScreenLog::last(size_t n) {
    //    if (_entries.size() > n) {
    //        return std::make_pair(_entries.rbegin(), _entries.rbegin() + n);
    //    } else {
    //        return std::make_pair(_entries.rbegin(), _entries.rend());
    //    }
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

        const std::string lvl = "(" + ghoul::logging::stringFromLevel(e->level) + ")";
        const std::string& message = e->message.substr(0, msg_length);
        nr += std::count(message.begin(), message.end(), '\n');

        RenderFont(*_fontLog,
            glm::vec2(10.f, _fontLog->pointSize() * nr * 2),
            white * alpha,
            "%-14s %s%s",                                    // Format
            e->timeString.c_str(),                            // Time string
            e->category.substr(0, category_length).c_str(), // Category string (up to category_length)
            e->category.length() > 20 ? "..." : "");        // Pad category with "..." if exceeds category_length

        glm::vec4 color = white;
        if (e->level == ghoul::logging::LogLevel::Debug)
            color = green;
        if (e->level == ghoul::logging::LogLevel::Warning)
            color = yellow;
        if (e->level == ghoul::logging::LogLevel::Error)
            color = red;
        if (e->level == ghoul::logging::LogLevel::Fatal)
            color = blue;

        //                    const float font_with_light = 5;
        RenderFont(*_fontLog,
            glm::vec2(static_cast<float>(10 + 39 * _fontLog->pointSize()), _fontLog->pointSize() * nr * 2),
            color * alpha,
            "%s",                                    // Format
            lvl.c_str());        // Pad category with "..." if exceeds category_length

        RenderFont(*_fontLog,
            glm::vec2(static_cast<float>(10 + 53 * _fontLog->pointSize()), _fontLog->pointSize() * nr * 2),
            white * alpha,
            "%s",                                    // Format
            message.c_str());        // Pad category with "..." if exceeds category_length
        ++nr;
    }
}

std::vector<Syncable*> RenderEngine::getSyncables(){
    return _mainCamera->getSyncables();
}

void RenderEngine::sortScreenspaceRenderables() {
    std::sort(
        _screenSpaceRenderables.begin(),
        _screenSpaceRenderables.end(),
        [](auto j, auto i) {
            return i->depth() > j->depth();
        }
    );
}

}// namespace openspace
