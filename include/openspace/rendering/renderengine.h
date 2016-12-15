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

#ifndef __OPENSPACE_CORE___RENDERENGINE___H__
#define __OPENSPACE_CORE___RENDERENGINE___H__

#include <openspace/scripting/scriptengine.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalar/boolproperty.h>

#include <openspace/util/syncdata.h>

namespace ghoul {
namespace fontrendering {
    class Font;
}
namespace opengl {
    class ProgramObject;
}
class Dictionary;
class SharedMemory;
}

namespace openspace {

namespace performance {
class PerformanceManager;
}

// Forward declare to minimize dependencies
class Camera;
class SyncBuffer;
class Scene;
class SceneManager;
class Renderer;
class RaycasterManager;
class ScreenLog;
class ScreenSpaceRenderable;

class RenderEngine : public properties::PropertyOwner {
public:
    enum class RendererImplementation {
        Framebuffer = 0,
        ABuffer,
        Invalid
    };

    enum class FrametimeType {
        DtTimeAvg = 0,
        FPS,
        FPSAvg
    };

    static const std::string KeyFontMono;
    static const std::string KeyFontLight;
    static const std::vector<FrametimeType> FrametimeTypes;

    RenderEngine();
    ~RenderEngine() = default;
    
    bool initialize();
    bool deinitialize();

    void setScene(Scene* scene);
    Scene* scene();
    void updateScene();

    Camera* camera() const;
    Renderer* renderer() const;
    RendererImplementation rendererImplementation() const;
    RaycasterManager& raycasterManager();

    // sgct wrapped functions
    bool initializeGL();
    

    void updateShaderPrograms();
    void updateFade();
    void updateRenderer();
    void updateScreenSpaceRenderables();
    void render(const glm::mat4& projectionMatrix, const glm::mat4& viewMatrix);

    void renderScreenLog();
    void renderShutdownInformation(float timer, float fullTime);
    void postDraw();

    void takeScreenshot(bool applyWarping = false);
    void toggleInfoText(bool b);

    // Performance measurements
    bool doesPerformanceMeasurements() const;
    performance::PerformanceManager* performanceManager();

    float globalBlackOutFactor();
    void setGlobalBlackOutFactor(float factor);
    void setNAaSamples(int nAaSamples);
    void setShowFrameNumber(bool enabled);

    void setDisableRenderingOnMaster(bool enabled);

    void registerScreenSpaceRenderable(std::shared_ptr<ScreenSpaceRenderable> s);
    void unregisterScreenSpaceRenderable(std::shared_ptr<ScreenSpaceRenderable> s);
    void unregisterScreenSpaceRenderable(std::string name);
    std::shared_ptr<ScreenSpaceRenderable> screenSpaceRenderable(std::string name);
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

    std::string progressToStr(int size, double t);

    void removeRenderProgram(const std::unique_ptr<ghoul::opengl::ProgramObject>& program);

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

    void sortScreenspaceRenderables();

    glm::ivec2 renderingResolution() const;
    glm::ivec2 fontResolution() const;

    std::vector<Syncable*> getSyncables();
    
private:
    void setRenderer(std::unique_ptr<Renderer> renderer);
    RendererImplementation rendererFromString(const std::string& method);

    void renderInformation();

    Camera* _camera;
    Scene* _scene;
    std::unique_ptr<RaycasterManager> _raycasterManager;

    properties::BoolProperty _performanceMeasurements;
    std::unique_ptr<performance::PerformanceManager> _performanceManager;

    std::unique_ptr<Renderer> _renderer;
    RendererImplementation _rendererImplementation;
    ghoul::Dictionary _rendererData;
    ghoul::Dictionary _resolveData;
    ScreenLog* _log;

    properties::OptionProperty _frametimeType;

    //FrametimeType _frametimeType;

    bool _showInfo;
    bool _showLog;
    bool _takeScreenshot;
    bool _applyWarping;
    bool _showFrameNumber;

    float _globalBlackOutFactor;
    float _fadeDuration;
    float _currentFadeTime;
    int _fadeDirection;
    int _nAaSamples;
    unsigned int _frameNumber;

    std::vector<ghoul::opengl::ProgramObject*> _programs;
    std::vector<std::shared_ptr<ScreenSpaceRenderable>> _screenSpaceRenderables;
    
    std::shared_ptr<ghoul::fontrendering::Font> _fontBig = nullptr;
    std::shared_ptr<ghoul::fontrendering::Font> _fontInfo = nullptr;
    std::shared_ptr<ghoul::fontrendering::Font> _fontDate = nullptr;
    std::shared_ptr<ghoul::fontrendering::Font> _fontLog = nullptr;

    bool _disableMasterRendering = false;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___RENDERENGINE___H__
