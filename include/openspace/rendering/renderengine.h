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

#ifndef __RENDERENGINE_H__
#define __RENDERENGINE_H__

#include <openspace/scripting/scriptengine.h>
#include <string>

#define ABUFFER_FRAMEBUFFER 0
#define ABUFFER_SINGLE_LINKED 1
#define ABUFFER_FIXED 2
#define ABUFFER_DYNAMIC 3

#ifdef __APPLE__
#define ABUFFER_IMPLEMENTATION ABUFFER_FRAMEBUFFER
#else
#define ABUFFER_IMPLEMENTATION ABUFFER_SINGLE_LINKED
#endif

namespace openspace {

// Forward declare to minimize dependencies
class Camera;
class SyncBuffer;
class SceneGraph;
class ABuffer;
class ScreenLog;

class RenderEngine {
public:
	RenderEngine();
	~RenderEngine();
	
	bool initialize();

    void setSceneGraph(SceneGraph* sceneGraph);
    SceneGraph* sceneGraph();

    Camera* camera() const;
    ABuffer* abuffer() const;

	// sgct wrapped functions
    bool initializeGL();
    void postSynchronizationPreDraw();
    void render();
    void postDraw();

	void takeScreenshot();

	void serialize(SyncBuffer* syncBuffer);
	void deserialize(SyncBuffer* syncBuffer);
	
	/**
	 * Returns the Lua library that contains all Lua functions available to affect the
	 * rendering. The functions contained are
	 * - openspace::luascriptfunctions::printImage
	 * \return The Lua library that contains all Lua functions available to affect the
	 * rendering
	 */
	static scripting::ScriptEngine::LuaLibrary luaLibrary();


private:
	Camera* _mainCamera;
	SceneGraph* _sceneGraph;
	ABuffer* _abuffer;
	ScreenLog* _log;

	bool _showInfo;
	bool _showScreenLog;
	bool _takeScreenshot;

	void generateGlslConfig();
};

} // namespace openspace

#endif // __RENDERENGINE_H__
