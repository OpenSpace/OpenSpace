/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_CORE___RENDERENGINE___H__
#define __OPENSPACE_CORE___RENDERENGINE___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/triggerproperty.h>

namespace ghoul {
    class Dictionary;
    class SharedMemory;
}
namespace ghoul::fontrendering { class Font; }
namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

namespace performance { class PerformanceManager; }
namespace scripting { struct LuaLibrary; }

class Camera;
class RaycasterManager;
class DeferredcasterManager;
class Renderer;
class Scene;
class SceneManager;
class ScreenLog;
class ScreenSpaceRenderable;
struct ShutdownInformation;
class Syncable;
class SyncBuffer;

class RenderEngine : public properties::PropertyOwner {
public:
    enum class RendererImplementation {
        Framebuffer = 0,
        ABuffer,
        Invalid
    };

    RenderEngine();
    ~RenderEngine();

    void initialize();
    void initializeGL();
    void deinitialize();
    void deinitializeGL();

    void setScene(Scene* scene);
    Scene* scene();
    void updateScene();

    Camera* camera() const;
    Renderer* renderer() const;
    RendererImplementation rendererImplementation() const;
    RaycasterManager& raycasterManager();
    DeferredcasterManager& deferredcasterManager();


    void updateShaderPrograms();
    void updateFade();
    void updateRenderer();
    void updateScreenSpaceRenderables();
    void render(const glm::mat4& sceneMatrix, const glm::mat4& viewMatrix,
        const glm::mat4& projectionMatrix);

    void renderOverlays(const ShutdownInformation& shutdownInfo);
    void postDraw();

    // Performance measurements
    bool doesPerformanceMeasurements() const;
    std::shared_ptr<performance::PerformanceManager> performanceManager();

    float globalBlackOutFactor();
    void setGlobalBlackOutFactor(float factor);

    void addScreenSpaceRenderable(std::shared_ptr<ScreenSpaceRenderable> s);
    void removeScreenSpaceRenderable(std::shared_ptr<ScreenSpaceRenderable> s);
    void removeScreenSpaceRenderable(const std::string& name);
    std::shared_ptr<ScreenSpaceRenderable> screenSpaceRenderable(const std::string& name);
    std::vector<ScreenSpaceRenderable*> screenSpaceRenderables() const;

    std::unique_ptr<ghoul::opengl::ProgramObject> buildRenderProgram(
        std::string name,
        std::string vsPath,
        std::string fsPath,
        const ghoul::Dictionary& dictionary = ghoul::Dictionary());

    std::unique_ptr<ghoul::opengl::ProgramObject> buildRenderProgram(
        std::string name,
        std::string vsPath,
        std::string fsPath,
        std::string csPath,
        const ghoul::Dictionary& dictionary = ghoul::Dictionary());

    void removeRenderProgram(
        const std::unique_ptr<ghoul::opengl::ProgramObject>& program);

    /**
    * Set raycasting uniforms on the program object, and setup raycasting.
    */
    void preRaycast(ghoul::opengl::ProgramObject& programObject);

    /**
    * Tear down raycasting for the specified program object.
    */
    void postRaycast(ghoul::opengl::ProgramObject& programObject);

    /**
     * Set the camera to use for rendering
     */
    void setCamera(Camera* camera);


    void setRendererFromString(const std::string& method);

    /**
     * Lets the renderer update the data to be brought into the rendererer programs
     * as a 'rendererData' variable in the dictionary.
     */
    void setRendererData(const ghoul::Dictionary& rendererData);

    /**
    * Lets the renderer update the data to be brought into the post rendererer programs
    * as a 'resolveData' variable in the dictionary.
    */
    void setResolveData(const ghoul::Dictionary& resolveData);

    /**
     * Returns the Lua library that contains all Lua functions available to affect the
     * rendering.
     */
    static scripting::LuaLibrary luaLibrary();

    // Temporary fade functionality
    void startFading(int direction, float fadeDuration);

    glm::ivec2 renderingResolution() const;
    glm::ivec2 fontResolution() const;

    std::vector<Syncable*> getSyncables();

    properties::PropertyOwner& screenSpaceOwner();

private:
    void setRenderer(std::unique_ptr<Renderer> renderer);
    RendererImplementation rendererFromString(const std::string& method) const;

    void renderScreenLog();
    void renderVersionInformation();
    void renderCameraInformation();
    void renderShutdownInformation(float timer, float fullTime);
    void renderDashboard();


    Camera* _camera;
    Scene* _scene;
    std::unique_ptr<RaycasterManager> _raycasterManager;
    std::unique_ptr<DeferredcasterManager> _deferredcasterManager;

    properties::BoolProperty _doPerformanceMeasurements;
    std::shared_ptr<performance::PerformanceManager> _performanceManager;

    std::unique_ptr<Renderer> _renderer;
    RendererImplementation _rendererImplementation;
    ghoul::Dictionary _rendererData;
    ghoul::Dictionary _resolveData;
    ScreenLog* _log;

    properties::BoolProperty _showOverlayOnSlaves;
    properties::BoolProperty _showLog;
    properties::BoolProperty _showVersionInfo;
    properties::BoolProperty _showCameraInfo;

    properties::TriggerProperty _takeScreenshot;
    bool _shouldTakeScreenshot;
    properties::BoolProperty _applyWarping;
    properties::BoolProperty _showFrameNumber;
    properties::BoolProperty _disableMasterRendering;
    properties::BoolProperty _disableSceneTranslationOnMaster;

    float _globalBlackOutFactor;
    float _fadeDuration;
    float _currentFadeTime;
    int _fadeDirection;
    properties::IntProperty _nAaSamples;
    properties::FloatProperty _hdrExposure;
    properties::FloatProperty _hdrBackground;
    properties::FloatProperty _gamma;

    uint64_t _frameNumber;

    std::vector<ghoul::opengl::ProgramObject*> _programs;
    properties::PropertyOwner _screenSpaceOwner;
    std::vector<std::shared_ptr<ScreenSpaceRenderable>> _screenSpaceRenderables;

    std::shared_ptr<ghoul::fontrendering::Font> _fontBig = nullptr;
    std::shared_ptr<ghoul::fontrendering::Font> _fontInfo = nullptr;
    std::shared_ptr<ghoul::fontrendering::Font> _fontDate = nullptr;
    std::shared_ptr<ghoul::fontrendering::Font> _fontLog = nullptr;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___RENDERENGINE___H__
