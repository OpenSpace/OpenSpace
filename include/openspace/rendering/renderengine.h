/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/triggerproperty.h>

namespace ghoul {
    class Dictionary;
    class SharedMemory;
} // ghoul
namespace ghoul::fontrendering { class Font; }
namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

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
    void deinitializeGL();

    void setScene(Scene* scene);
    Scene* scene();
    void updateScene();

    const Renderer& renderer() const;
    RendererImplementation rendererImplementation() const;

    void updateShaderPrograms();
    void updateRenderer();
    void updateScreenSpaceRenderables();
    void render(const glm::mat4& sceneMatrix, const glm::mat4& viewMatrix,
        const glm::mat4& projectionMatrix);

    bool mouseActivationCallback(const glm::dvec2& mousePosition) const;

    void renderOverlays(const ShutdownInformation& shutdownInfo);
    void renderEndscreen();
    void postDraw();

    float globalBlackOutFactor();
    void setGlobalBlackOutFactor(float opacity);

    float hdrExposure() const;
    bool isHdrDisabled() const;

    void addScreenSpaceRenderable(std::unique_ptr<ScreenSpaceRenderable> s);
    void removeScreenSpaceRenderable(ScreenSpaceRenderable* s);
    void removeScreenSpaceRenderable(const std::string& identifier);
    ScreenSpaceRenderable* screenSpaceRenderable(const std::string& identifier);
    std::vector<ScreenSpaceRenderable*> screenSpaceRenderables() const;

    std::unique_ptr<ghoul::opengl::ProgramObject> buildRenderProgram(
        const std::string& name, const std::string& vsPath, std::string fsPath,
        ghoul::Dictionary data = ghoul::Dictionary());

    std::unique_ptr<ghoul::opengl::ProgramObject> buildRenderProgram(
        const std::string& name, const std::string& vsPath, std::string fsPath,
        const std::string& csPath, ghoul::Dictionary data = ghoul::Dictionary());

    void removeRenderProgram(ghoul::opengl::ProgramObject* program);

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


    void setRendererFromString(const std::string& renderingMethod);

    /**
     * Lets the renderer update the data to be brought into the rendererer programs
     * as a 'rendererData' variable in the dictionary.
     */
    void setRendererData(ghoul::Dictionary rendererData);

    /**
    * Lets the renderer update the data to be brought into the post rendererer programs
    * as a 'resolveData' variable in the dictionary.
    */
    void setResolveData(ghoul::Dictionary resolveData);

    /**
     * Take a screenshot and store in the ${SCREENSHOTS} directory
     */
    void takeScreenshot();

    /**
     * Get the filename of the latest screenshot
     */
    unsigned int latestScreenshotNumber() const;

    /**
     * Returns the Lua library that contains all Lua functions available to affect the
     * rendering.
     */
    static scripting::LuaLibrary luaLibrary();

    glm::ivec2 renderingResolution() const;
    glm::ivec2 fontResolution() const;

    glm::mat4 globalRotation() const;
    glm::mat4 screenSpaceRotation() const;
    glm::mat4 nodeRotation() const;

    uint64_t frameNumber() const;

private:
    void setRenderer(std::unique_ptr<Renderer> renderer);
    RendererImplementation rendererFromString(const std::string& renderingMethod) const;

    void renderScreenLog();
    void renderVersionInformation();
    void renderCameraInformation();
    void renderShutdownInformation(float timer, float fullTime);
    void renderDashboard();

    Camera* _camera = nullptr;
    Scene* _scene = nullptr;

    properties::BoolProperty _doPerformanceMeasurements;

    std::unique_ptr<Renderer> _renderer;
    RendererImplementation _rendererImplementation = RendererImplementation::Invalid;
    ghoul::Dictionary _rendererData;
    ghoul::Dictionary _resolveData;
    ScreenLog* _log = nullptr;

    properties::BoolProperty _showOverlayOnSlaves;
    properties::BoolProperty _showLog;
    properties::BoolProperty _showVersionInfo;
    properties::BoolProperty _showCameraInfo;

    properties::BoolProperty _applyWarping;
    properties::BoolProperty _showFrameInformation;
#ifdef OPENSPACE_WITH_INSTRUMENTATION
    struct FrameInfo {
        uint64_t iFrame;
        double deltaTime;
        double avgDeltaTime;
    };

    struct {
        std::vector<FrameInfo> frames;
        uint64_t lastSavedFrame = 0;
        uint16_t saveEveryNthFrame = 2048;
    } _frameInfo;
    properties::BoolProperty _saveFrameInformation;
#endif // OPENSPACE_WITH_INSTRUMENTATION
    properties::BoolProperty _disableMasterRendering;

    properties::FloatProperty _globalBlackOutFactor;

    properties::BoolProperty _enableFXAA;

    properties::BoolProperty _disableHDRPipeline;
    properties::FloatProperty _hdrExposure;
    properties::FloatProperty _gamma;

    properties::FloatProperty _hue;
    properties::FloatProperty _saturation;
    properties::FloatProperty _value;

    properties::FloatProperty _horizFieldOfView;

    properties::Vec3Property _globalRotation;
    properties::Vec3Property _screenSpaceRotation;
    properties::Vec3Property _masterRotation;

    uint64_t _frameNumber = 0;
    unsigned int _latestScreenshotNumber = 0;

    std::vector<ghoul::opengl::ProgramObject*> _programs;

    std::shared_ptr<ghoul::fontrendering::Font> _fontFrameInfo;
    std::shared_ptr<ghoul::fontrendering::Font> _fontInfo;
    std::shared_ptr<ghoul::fontrendering::Font> _fontDate;
    std::shared_ptr<ghoul::fontrendering::Font> _fontLog;

    struct {
        glm::ivec4 rotation = glm::ivec4(0);
        glm::ivec4 zoom = glm::ivec4(0);
        glm::ivec4 roll = glm::ivec4(0);
    } _cameraButtonLocations;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___RENDERENGINE___H__
