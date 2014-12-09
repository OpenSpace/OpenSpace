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

#include <openspace/interaction/luaconsole.h>
#include <openspace/util/constants.h>
#include <openspace/engine/openspaceengine.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/logging/logmanager.h>

#include <string>
#include <iostream>
#include <fstream>

#include <sgct.h>

namespace {
	const std::string _loggerCat = "LuaConsole";
	const std::string historyFile = "ConsoleHistory";

#if !defined(WIN32)
	// Dangerus as fuck (if malicious input)
	bool exec(const std::string& cmd, std::string& value)
	{
		FILE* pipe = popen(cmd.c_str(), "r");
		if (!pipe)
			return false;

		const int buffer_size = 1024;
		char buffer[buffer_size];
		value = "";
		while (!feof(pipe))
		{
			if (fgets(buffer, buffer_size, pipe) != NULL)
			{
				value += buffer;
			}
		}
		pclose(pipe);
		return true;
	}
#endif

	// TODO: Put this functio nsomewhere appropriate
	// get text from clipboard
	std::string getClipboardText() {
#if defined(WIN32)
	// Try opening the clipboard
	if (!OpenClipboard(nullptr))
		return "";

	// Get handle of clipboard object for ANSI text
	HANDLE hData = GetClipboardData(CF_TEXT);
	if (hData == nullptr)
		return "";

	// Lock the handle to get the actual text pointer
	char * pszText = static_cast<char*>(GlobalLock(hData));
	if (pszText == nullptr)
		return "";

	// Save text in a string class instance
	std::string text(pszText);

	// Release the lock
	GlobalUnlock(hData);

	// Release the clipboard
	CloseClipboard();

	text.erase(std::remove(text.begin(), text.end(), '\r'), text.end());
	return text;
#elif defined(__APPLE__)
	std::string text;
	if (exec("pbpaste", text))
		return text.substr(0, text.length() - 1);
	return ""; // remove a line ending
#else
	std::string text;
	if (exec("xclip -o -sel c -f", text))
		return text.substr(0, text.length() - 1);
	return ""; // remove a line ending
#endif
}

	// TODO: Put this function somewhere appropriate
	// set text to clipboard
	bool setClipboardText(std::string text)
	{
#if defined(WIN32)
		char *ptrData = nullptr;
		HANDLE hData = GlobalAlloc(GMEM_MOVEABLE | GMEM_DDESHARE, text.length() + 1);

		ptrData = (char*)GlobalLock(hData);
		memcpy(ptrData, text.c_str(), text.length() + 1);

		GlobalUnlock(hData);

		if (!OpenClipboard(nullptr))
			return false;

		if (!EmptyClipboard())
			return false;

		SetClipboardData(CF_TEXT, hData);

		CloseClipboard();

		return true;
#elif defined(__APPLE__)
		std::stringstream cmd;
		cmd << "echo \"" << text << "\" | pbcopy";
		std::string buf;
		return exec(cmd.str(), buf);
#else
		std::stringstream cmd;
		cmd << "echo \"" << text << "\" | xclip -i -sel c -f";
		std::string buf;
		return exec(cmd.str(), buf);
#endif
	}
}

namespace openspace {

namespace luascriptfunctions {

/**
 * \ingroup LuaScripts
 * show():
 * Shows the console
 */
int show(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 0)
		return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);

	OsEng.console().setVisible(true);
	return 0;
}

/**
 * \ingroup LuaScripts
 * hide():
 * Hides the console
 */
int hide(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 0)
		return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);

	OsEng.console().setVisible(false);
	return 0;
}

/**
 * \ingroup LuaScripts
 * toggle():
 * Toggles the console
 */
int toggle(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 0)
		return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);

	OsEng.console().toggleVisibility();
	return 0;
}

}

LuaConsole::LuaConsole() 
	: _inputPosition(0)
	, _activeCommand(0)
	, _filename("")
	, _isVisible(false)
{
	_commands.push_back("");
	_activeCommand = _commands.size() - 1;
}

LuaConsole::~LuaConsole() {
	std::ofstream file(absPath(_filename), std::ios::binary | std::ios::out);
	if (file.is_open()) {
		size_t n = _commandsHistory.size();
		file.write(reinterpret_cast<const char*>(&n), sizeof(size_t));
		for (auto s : _commandsHistory) {
			size_t length = s.length();
			file.write(reinterpret_cast<const char*>(&length), sizeof(size_t));
			file.write(s.c_str(), sizeof(char)*length);
		}
		file.close();
	}
}

void LuaConsole::loadHistory() {
	FileSys.cacheManager()->getCachedFile(historyFile, "", _filename, true);

	std::ifstream file(absPath(_filename), std::ios::binary | std::ios::in);
	if (file.is_open()) {
		size_t n;

		file.read(reinterpret_cast<char*>(&n), sizeof(size_t));

		for (size_t i = 0; i < n; ++i) {
			size_t length;
			file.read(reinterpret_cast<char*>(&length), sizeof(size_t));
			char* tmp = new char[length + 1];
			file.read(tmp, sizeof(char)*length);
			tmp[length] = '\0';
			_commandsHistory.emplace_back(tmp);
			delete[] tmp;
		}
		file.close();
		_commands = _commandsHistory;
	}
	_commands.push_back("");
	_activeCommand = _commands.size() - 1;
}

void LuaConsole::keyboardCallback(int key, int action) {
	if (action == SGCT_PRESS || action == SGCT_REPEAT) {
		const size_t windowIndex = sgct::Engine::instance()->getFocusedWindowIndex();
		const bool mod_CONTROL = sgct::Engine::instance()->getKey(windowIndex, SGCT_KEY_LEFT_CONTROL) ||
			sgct::Engine::instance()->getKey(windowIndex, SGCT_KEY_RIGHT_CONTROL);
		const bool mod_SHIFT = sgct::Engine::instance()->getKey(windowIndex, SGCT_KEY_LEFT_SHIFT) ||
			sgct::Engine::instance()->getKey(windowIndex, SGCT_KEY_RIGHT_SHIFT);

		// Paste from clipboard
		if (key == SGCT_KEY_V) {
			if (mod_CONTROL) {
				addToCommand(getClipboardText());
			}
		}

		// Copy to clipboard
		if (key == SGCT_KEY_C) {
			if (mod_CONTROL) {
				setClipboardText(_commands.at(_activeCommand));
			}
		}

		// Go to the previous character
		if (key == SGCT_KEY_LEFT) {
			if (_inputPosition > 0)
				_inputPosition -= 1;
		}

		// Go to the next character
		if (key == SGCT_KEY_RIGHT) {
			if (_inputPosition < _commands.at(_activeCommand).length())
				++_inputPosition;
		}

		// Go to previous command
		if (key == SGCT_KEY_UP) {
			if (_activeCommand > 0)
				--_activeCommand;
			_inputPosition = _commands.at(_activeCommand).length();
		}

		// Go to next command (the last is empty)
		if (key == SGCT_KEY_DOWN) {
			if (_activeCommand < _commands.size() - 1)
				++_activeCommand;
			_inputPosition = _commands.at(_activeCommand).length();
		}

		// Remove character before _inputPosition
		if (key == SGCT_KEY_BACKSPACE) {
			if (_inputPosition > 0) {
				_commands.at(_activeCommand).erase(_inputPosition - 1, 1);
				--_inputPosition;
			}
		}

		// Remove character after _inputPosition
		if (key == SGCT_KEY_DELETE) {
			if (_inputPosition <= _commands.at(_activeCommand).size()) {
				_commands.at(_activeCommand).erase(_inputPosition, 1);
			}
		}

		// Go to the beginning of command string
		if (key == SGCT_KEY_HOME) {
			_inputPosition = 0;
		}

		// Go to the end of command string
		if (key == SGCT_KEY_END) {
			_inputPosition = _commands.at(_activeCommand).size();
		}

		if (key == SGCT_KEY_ENTER) {

			// SHIFT+ENTER == new line
			if (mod_SHIFT) {
				addToCommand("\n");
			}
			// CTRL+ENTER == Debug print the command
			else if (mod_CONTROL) {
				LDEBUG("Active command from next line:\n" << _commands.at(_activeCommand));
			}
			// ENTER == run lua script
			else {
				if (_commands.at(_activeCommand) != "") {

					OsEng.scriptEngine().runScript(_commands.at(_activeCommand));
					if (_commandsHistory.size() > 0 &&
						_commands.at(_activeCommand) != _commandsHistory.at(_commandsHistory.size() - 1))
						_commandsHistory.push_back(_commands.at(_activeCommand));
					else if (_commandsHistory.size() == 0)
						_commandsHistory.push_back(_commands.at(_activeCommand));

					_commands = _commandsHistory;
					_commands.push_back("");
					_activeCommand = _commands.size() - 1;
					_inputPosition = 0;
					setVisible(false);
				}
				else {
					_commands = _commandsHistory;
					_commands.push_back("");
					setVisible(false);
				}
			}
		}
	}
}

void LuaConsole::charCallback(unsigned int codepoint) {
	if (codepoint == ignoreCodepoint())
		return;

#ifndef WIN32
	const size_t windowIndex = sgct::Engine::instance()->getFocusedWindowIndex();
	const bool mod_CONTROL = sgct::Engine::instance()->getKey(windowIndex, SGCT_KEY_LEFT_CONTROL) ||
		sgct::Engine::instance()->getKey(windowIndex, SGCT_KEY_RIGHT_CONTROL);

	const int codepoint_C = 99;
	const int codepoint_V = 118;
	if (mod_CONTROL && (codepoint == codepoint_C || codepoint == codepoint_V)) {
		return;
	}
#endif
	addToCommand(UnicodeToUTF8(codepoint));

}

void LuaConsole::render() {
	const float font_size = 10.0f;
	int x1, xSize, y1, ySize;
	sgct::Engine::instance()->getActiveWindowPtr()->getCurrentViewportPixelCoords(x1, y1, xSize, ySize);
	float startY = static_cast<float>(ySize) - 2.0f * font_size;
	startY = startY - font_size * 10.0f * 2.0f;

	const glm::vec4 red(1, 0, 0, 1);
	const glm::vec4 green(0, 1, 0, 1);
	const glm::vec4 white(1, 1, 1, 1);
	const sgct_text::Font* font = sgct_text::FontManager::instance()->getFont(constants::fonts::keyMono, static_cast<int>(font_size));
	Freetype::print(font, 10.0f, startY, red, "$");
	Freetype::print(font, 10.0f + font_size, startY, white, "%s", _commands.at(_activeCommand).c_str());

	size_t n = std::count(_commands.at(_activeCommand).begin(), _commands.at(_activeCommand).begin() + _inputPosition, '\n');
	size_t p = _commands.at(_activeCommand).find_last_of('\n', _inputPosition);
	size_t linepos = _inputPosition;

	if (n>0) {
		if (p == _inputPosition) {
			p = _commands.at(_activeCommand).find_last_of('\n', _inputPosition - 1);
			if (p != std::string::npos) {
				linepos -= p + 1;
			}
			else {
				linepos = _inputPosition - 1;
			}
		}
		else{
			linepos -= p + 1;
		}
	}

	std::stringstream ss;
	ss << "%" << linepos + 1 << "s";
	Freetype::print(font, 10.0f + font_size*0.5f, startY - (font_size)*(n + 1)*3.0f / 2.0f, green, ss.str().c_str(), "^");
}

unsigned int LuaConsole::commandInputButton(){
	// Button left of 1 and abobe TAB
#ifdef WIN32
	return SGCT_KEY_BACKSLASH;
#else
	return SGCT_KEY_GRAVE_ACCENT;
#endif
}

unsigned int LuaConsole::ignoreCodepoint() {
	// Correesponding codepoint for commandInputButton()
	return 167;
}

void LuaConsole::addToCommand(std::string c) {
	size_t length = c.length();
	_commands.at(_activeCommand).insert(_inputPosition, c);
	_inputPosition += length;
}

std::string LuaConsole::UnicodeToUTF8(unsigned int codepoint) {
	std::string out;

	if (codepoint <= 0x7f)
		out.append(1, static_cast<char>(codepoint));
	else if (codepoint <= 0x7ff)
	{
		out.append(1, static_cast<char>(0xc0 | ((codepoint >> 6) & 0x1f)));
		out.append(1, static_cast<char>(0x80 | (codepoint & 0x3f)));
	}
	else if (codepoint <= 0xffff)
	{
		out.append(1, static_cast<char>(0xe0 | ((codepoint >> 12) & 0x0f)));
		out.append(1, static_cast<char>(0x80 | ((codepoint >> 6) & 0x3f)));
		out.append(1, static_cast<char>(0x80 | (codepoint & 0x3f)));
	}
	else
	{
		out.append(1, static_cast<char>(0xf0 | ((codepoint >> 18) & 0x07)));
		out.append(1, static_cast<char>(0x80 | ((codepoint >> 12) & 0x3f)));
		out.append(1, static_cast<char>(0x80 | ((codepoint >> 6) & 0x3f)));
		out.append(1, static_cast<char>(0x80 | (codepoint & 0x3f)));
	}
	return out;
}

bool LuaConsole::isVisible() const {
	return _isVisible;
}

void LuaConsole::setVisible(bool visible) {
	_isVisible = visible;
}

void LuaConsole::toggleVisibility() {
	_isVisible = !_isVisible;
}

scripting::ScriptEngine::LuaLibrary LuaConsole::luaLibrary() {
	return {
		"console",
		{
			{
				"show",
				&luascriptfunctions::show,
				"",
				"Shows the console"
			},
			{
				"hide",
				&luascriptfunctions::hide,
				"",
				"Hides the console"
			},
			{
				"toggle",
				&luascriptfunctions::toggle,
				"",
				"Toggles the console"
			}
		}
	};
}

} // namespace openspace
