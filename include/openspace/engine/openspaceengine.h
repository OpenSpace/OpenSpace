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

#ifndef __OPENSPACEENGINE_H__
#define __OPENSPACEENGINE_H__

#include <openspace/gui/gui.h>

#include <string>
#include <vector>

namespace ghoul {
namespace cmdparser {
    class CommandlineParser;
}
}

namespace openspace {

class ConfigurationManager;
class LuaConsole;
class GUI;
class RenderEngine;
class SyncBuffer;

namespace interaction {
    class InteractionHandler;
}
namespace scripting {
	class ScriptEngine;
}

class OpenSpaceEngine {
public:
    static bool create(int argc, char** argv, std::vector<std::string>& sgctArguments);
    static void destroy();
    static OpenSpaceEngine& ref();

    static bool isInitialized();
    bool initialize();
	bool isMaster();
	void setMaster(bool master);
    static bool findConfiguration(std::string& filename);

    // Guaranteed to return a valid pointer
    ConfigurationManager* configurationManager();
    interaction::InteractionHandler* interactionHandler();
    RenderEngine* renderEngine();
	scripting::ScriptEngine* scriptEngine();
	LuaConsole* console();

	gui::GUI* gui();

    // SGCT callbacks
    bool initializeGL();
    void preSynchronization();
    void postSynchronizationPreDraw();
	void render(const glm::mat4 &projectionMatrix, const glm::mat4 &viewMatrix);
	void postDraw();
	void keyboardCallback(int key, int action);
	void charCallback(unsigned int codepoint);
    void mouseButtonCallback(int key, int action);
    void mousePositionCallback(int x, int y);
    void mouseScrollWheelCallback(int pos);
	void externalControlCallback(const char* receivedChars, int size, int clientId);
    void encode();
    void decode();

    void runSettingsScripts();

private:
    OpenSpaceEngine(std::string programName);
    ~OpenSpaceEngine();
    OpenSpaceEngine(const OpenSpaceEngine& rhs) = delete;

	void clearAllWindows();
	bool gatherCommandlineArguments();
	bool loadSpiceKernels();
	void loadFonts();
    void runScripts(const ghoul::Dictionary& scripts);
    void runStartupScripts();
	void configureLogging();

    static OpenSpaceEngine* _engine;

    ConfigurationManager* _configurationManager;
    interaction::InteractionHandler* _interactionHandler;
    RenderEngine* _renderEngine;
	scripting::ScriptEngine* _scriptEngine;
	ghoul::cmdparser::CommandlineParser* _commandlineParser;
	LuaConsole* _console;
    gui::GUI* _gui;
	double _dt;
	bool _isMaster;

	SyncBuffer* _syncBuffer;
};

#define OsEng (openspace::OpenSpaceEngine::ref())

}  // namespace openspace

#endif  // __OPENSPACEENGINE_H__
