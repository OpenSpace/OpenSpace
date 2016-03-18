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

#ifndef __RENDERENGINE_H__
#define __RENDERENGINE_H__

#include <openspace/scripting/scriptengine.h>

#include <openspace/properties/vectorproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/rendering/screenspacerenderable.h>

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

// Forward declare to minimize dependencies
class Camera;
class SyncBuffer;
class Scene;
class Renderer;
class ScreenLog;
class ScreenSpaceRenderable;

class RenderEngine {
public:
    enum class RendererImplementation {
        Framebuffer = 0,
        ABuffer,
        Invalid
    };

	static const std::string PerformanceMeasurementSharedData;
    
    static const std::string KeyFontMono;
    static const std::string KeyFontLight;

	RenderEngine();
	~RenderEngine();
	
	bool initialize();
	bool deinitialize();

    void setSceneGraph(Scene* sceneGraph);
    Scene* scene();

    Camera* camera() const;
    Renderer* renderer() const;
    RendererImplementation rendererImplementation() const;

	// sgct wrapped functions
    bool initializeGL();
    void postSynchronizationPreDraw();
	void preSynchronization();
	void render(const glm::mat4 &projectionMatrix, const glm::mat4 &viewMatrix);
    void postDraw();

	void takeScreenshot();
	void toggleInfoText(bool b);

	void setPerformanceMeasurements(bool performanceMeasurements);
	bool doesPerformanceMeasurements() const;

	void serialize(SyncBuffer* syncBuffer);
	void deserialize(SyncBuffer* syncBuffer);

	float globalBlackOutFactor();
	void setGlobalBlackOutFactor(float factor);

    void setDisableRenderingOnMaster(bool enabled);

    void registerScreenSpaceRenderable(std::shared_ptr<ScreenSpaceRenderable> s);
    void unregisterScreenSpaceRenderable(std::shared_ptr<ScreenSpaceRenderable> s);
    std::shared_ptr<ScreenSpaceRenderable> screenSpaceRenderable(std::string name);

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

    void removeRenderProgram(const std::unique_ptr<ghoul::opengl::ProgramObject>& program);

    void setRendererFromString(const std::string& method);

    /**
     * Let's the renderer update the data to be brought into the rendererer programs
     * as a 'rendererData' variable in the dictionary.
     */
    void setRendererData(const ghoul::Dictionary& renderer);
	
	/**
	 * Returns the Lua library that contains all Lua functions available to affect the
	 * rendering.
	 */
	static scripting::ScriptEngine::LuaLibrary luaLibrary();

    // This is a temporary method to change the origin of the coordinate system ---abock
    void changeViewPoint(std::string origin);

	// Temporary fade functionality
	void startFading(int direction, float fadeDuration);

    void sortScreenspaceRenderables();
    // This is temporary until a proper screenspace solution is found ---abock
    struct {
        glm::vec2 _position;
        unsigned int _size;
        int _node;
    } _onScreenInformation;

    std::shared_ptr<ScreenSpaceRenderable> ssr;
private:
    void setRenderer(std::unique_ptr<Renderer> renderer);
    RendererImplementation rendererFromString(const std::string& method);
	void storePerformanceMeasurements();
    void renderInformation();
    void renderScreenLog();

	Camera* _mainCamera;
	Scene* _sceneGraph;
    std::unique_ptr<Renderer> _renderer;
    RendererImplementation _rendererImplementation;
    ghoul::Dictionary _rendererData;
	ScreenLog* _log;

	bool _showInfo;
	bool _showLog;
	bool _takeScreenshot;

	bool _doPerformanceMeasurements;
	ghoul::SharedMemory* _performanceMemory;
    
	float _globalBlackOutFactor;
	float _fadeDuration;
	float _currentFadeTime;
	int _fadeDirection;

    std::vector<ghoul::opengl::ProgramObject*> _programs;
    std::vector<std::shared_ptr<ScreenSpaceRenderable>> _screenSpaceRenderables;
    
    std::shared_ptr<ghoul::fontrendering::Font> _fontInfo = nullptr;
    std::shared_ptr<ghoul::fontrendering::Font> _fontDate = nullptr;
    std::shared_ptr<ghoul::fontrendering::Font> _fontLog = nullptr;

    bool _disableMasterRendering = false;
};

} // namespace openspace

#endif // __RENDERENGINE_H__
