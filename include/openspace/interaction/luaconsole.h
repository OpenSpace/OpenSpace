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

#ifndef LUACONSOLE_H
#define LUACONSOLE_H

#include <openspace/scripting/scriptengine.h>

#include <string>
#include <vector>

namespace openspace {

class LuaConsole {
public:
	LuaConsole();
	~LuaConsole();

	void loadHistory();

	void keyboardCallback(int key, int action);
	void charCallback(unsigned int codepoint);

	void render();

	unsigned int commandInputButton();
	unsigned int ignoreCodepoint();

	bool isVisible() const;
	void setVisible(bool visible);
	void toggleVisibility();
		
	/**
	 * Returns the Lua library that contains all Lua functions available to affect the
	 * console. The functions contained are
	 * \return The Lua library that contains all Lua functions available to affect the
	 * console
	 */
	static scripting::ScriptEngine::LuaLibrary luaLibrary();


private:
	void addToCommand(std::string c);
	std::string UnicodeToUTF8(unsigned int codepoint);

	size_t _inputPosition;
	std::vector<std::string> _commandsHistory;
	size_t _activeCommand;
	std::vector<std::string> _commands;
	
	std::string _filename;

	bool _isVisible;
};

} // namespace openspace

#endif
