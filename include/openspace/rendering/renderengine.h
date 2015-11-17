/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

namespace ghoul {
namespace fontrendering {
    class Font;
}

class SharedMemory;
}

namespace openspace {

// Forward declare to minimize dependencies
class Camera;
class SyncBuffer;
class Scene;
class ABuffer;
class ABufferVisualizer;
class ScreenLog;

class RenderEngine {
public:
    enum class ABufferImplementation {
        FrameBuffer = 0,
        SingleLinked,
        Fixed,
        Dynamic,
        Invalid
    };

	static const std::string PerformanceMeasurementSharedData;

	RenderEngine();
	~RenderEngine();
	
	bool initialize();

    void setSceneGraph(Scene* sceneGraph);
    Scene* scene();

    Camera* camera() const;
    ABuffer* aBuffer() const;
    ABufferImplementation aBufferImplementation() const;

	// sgct wrapped functions
    bool initializeGL();
    void postSynchronizationPreDraw();
	void preSynchronization();
	void render(const glm::mat4 &projectionMatrix, const glm::mat4 &viewMatrix);
    void postDraw();

	void takeScreenshot();
	void toggleVisualizeABuffer(bool b);

	void toggleInfoText(bool b);

	void setPerformanceMeasurements(bool performanceMeasurements);
	bool doesPerformanceMeasurements() const;

	void serialize(SyncBuffer* syncBuffer);
	void deserialize(SyncBuffer* syncBuffer);

	float globalBlackOutFactor();
	void setGlobalBlackOutFactor(float factor);

    void setDisableRenderingOnMaster(bool enabled);
	
	/**
	 * Returns the Lua library that contains all Lua functions available to affect the
	 * rendering. The functions contained are
	 * - openspace::luascriptfunctions::printImage
	 * - openspace::luascriptfunctions::visualizeABuffer
	 * \return The Lua library that contains all Lua functions available to affect the
	 * rendering
	 */
	static scripting::ScriptEngine::LuaLibrary luaLibrary();

    // This is a temporary method to change the origin of the coordinate system ---abock
    void changeViewPoint(std::string origin);

	//temporaray fade functionality
	void startFading(int direction, float fadeDuration);

    // This is temporary until a proper screenspace solution is found ---abock
    struct {
        glm::vec2 _position;
        unsigned int _size;
        int _node;
    } _onScreenInformation;

private:
    ABufferImplementation aBufferFromString(const std::string& impl);

	void storePerformanceMeasurements();

	Camera* _mainCamera;
	Scene* _sceneGraph;
	ABuffer* _abuffer;
    ABufferImplementation _abufferImplementation;
	ScreenLog* _log;

	bool _showInfo;
	bool _showScreenLog;
	bool _takeScreenshot;

	bool _doPerformanceMeasurements;
	ghoul::SharedMemory* _performanceMemory;
    
	void generateGlslConfig();

	float _globalBlackOutFactor;
	float _fadeDuration;
	float _currentFadeTime;
	int _fadeDirection;
//    bool _sgctRenderStatisticsVisible;

	bool _visualizeABuffer;
	ABufferVisualizer* _visualizer;

    bool _disableMasterRendering = false;
};

} // namespace openspace

#endif // __RENDERENGINE_H__
