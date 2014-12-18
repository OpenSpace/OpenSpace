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

#ifndef __GUI_H__
#define __GUI_H__

#include <openspace/scripting/scriptengine.h>

#include <ghoul/glm.h>

#include <map>
#include <set>
#include <string>
#include <vector>

namespace ghoul {
	class SharedMemory;
}

namespace openspace {

namespace properties {
	class Property;
}

class GUI {
public:
	GUI();
	~GUI();

	bool isEnabled() const;
	void setEnabled(bool enabled);

	void initialize();

	void initializeGL();
	void deinitializeGL();

	void registerProperty(properties::Property* prop);

	bool mouseButtonCallback(int key, int action);
	bool mouseWheelCallback(int position);
	bool keyCallback(int key, int action);
	bool charCallback(unsigned int character);

	void startFrame(float deltaTime, const glm::vec2& windowSize, const glm::vec2& mousePos, bool mouseButtonsPressed[2]);
	void endFrame();


	static scripting::ScriptEngine::LuaLibrary luaLibrary();

private:
	void renderMainWindow();

	void renderPropertyWindow();
	void renderPerformanceWindow();

	bool _isEnabled;

	bool _showPropertyWindow;
	bool _showPerformanceWindow;
	bool _showHelp;


	ghoul::SharedMemory* _performanceMemory;
	float _minMaxValues[2];

	std::set<properties::Property*> _boolProperties;
	std::set<properties::Property*> _intProperties;
	std::set<properties::Property*> _floatProperties;
	std::set<properties::Property*> _vec2Properties;
	std::set<properties::Property*> _vec3Properties;
	std::set<properties::Property*> _stringProperties;
	std::set<properties::Property*> _optionProperty;
	std::set<properties::Property*> _selectionProperty;
	std::set<properties::Property*> _triggerProperty;

	std::map<std::string, std::vector<properties::Property*>> _propertiesByOwner;
};

} // namespace openspace

#endif // __GUI_H__
