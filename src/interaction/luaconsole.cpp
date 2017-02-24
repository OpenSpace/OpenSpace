/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/network/parallelconnection.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/clipboard.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

#include <string>
#include <iostream>
#include <iterator>
#include <fstream>

namespace {
    const std::string _loggerCat = "LuaConsole";
    const std::string historyFile = "ConsoleHistory";

    const int NoAutoComplete = -1;
}

#include "luaconsole_lua.inl"

namespace openspace {

LuaConsole::LuaConsole() 
    : _inputPosition(0)
    , _activeCommand(0)
    , _filename("")
    , _autoCompleteInfo({NoAutoComplete, false, ""})
    , _isVisible(false)
    , _remoteScripting(true)
{
//    _commands.push_back("");
//    _activeCommand = _commands.size() - 1;
}

void LuaConsole::initialize() {
    _filename = FileSys.cacheManager()->cachedFilename(
        historyFile,
        "",
        ghoul::filesystem::CacheManager::Persistent::Yes
    );

    std::ifstream file(_filename, std::ios::binary | std::ios::in);
    if (file.good()) {
        int64_t nCommands;
        file.read(reinterpret_cast<char*>(&nCommands), sizeof(int64_t));

        std::vector<char> tmp;
        for (int64_t i = 0; i < nCommands; ++i) {
            int64_t length;
            file.read(reinterpret_cast<char*>(&length), sizeof(int64_t));
            tmp.resize(length + 1);
            file.read(tmp.data(), length);
            tmp[length] = '\0';
            _commandsHistory.emplace_back(std::string(tmp.begin(), tmp.end()));
        }
        file.close();
        _commands = _commandsHistory;
    }
    _commands.push_back("");
    _activeCommand = _commands.size() - 1;

    OsEng.parallelConnection().connectionEvent()->subscribe("luaConsole",
        "statusChanged", [this]() {
        ParallelConnection::Status status = OsEng.parallelConnection().status();
        parallelConnectionChanged(status);
    });

}

void LuaConsole::deinitialize() {
    std::ofstream file(_filename, std::ios::binary | std::ios::out);
    if (file.good()) {
        int64_t nCommands = _commandsHistory.size();
        file.write(reinterpret_cast<const char*>(&nCommands), sizeof(int64_t));
        for (const std::string& s : _commandsHistory) {
            int64_t length = s.length();
            file.write(reinterpret_cast<const char*>(&length), sizeof(int64_t));
            file.write(s.c_str(), length);
        }
    }

    OsEng.parallelConnection().connectionEvent()->unsubscribe("luaConsole");
}

void LuaConsole::keyboardCallback(Key key, KeyModifier modifier, KeyAction action) {
    if (action == KeyAction::Press || action == KeyAction::Repeat) {
        const bool modifierControl = (modifier == KeyModifier::Control);
        const bool modifierShift = (modifier == KeyModifier::Shift);

        // Paste from clipboard
        if (modifierControl && (key == Key::V))
            addToCommand(ghoul::clipboardText());

        // Copy to clipboard
        if (modifierControl && (key == Key::C))
            ghoul::setClipboardText(_commands.at(_activeCommand));

        // Go to the previous character
        if ((key == Key::Left) && (_inputPosition > 0))
            --_inputPosition;

        // Go to the next character
        if ((key == Key::Right) && _inputPosition < _commands.at(_activeCommand).length())
            ++_inputPosition;

        // Go to previous command
        if (key == Key::Up) {
            if (_activeCommand > 0)
                --_activeCommand;
            _inputPosition = _commands.at(_activeCommand).length();
        }

        // Go to next command (the last is empty)
        if (key == Key::Down) {
            if (_activeCommand < _commands.size() - 1)
                ++_activeCommand;
            _inputPosition = _commands.at(_activeCommand).length();
        }

        // Remove character before _inputPosition
        if (key == Key::BackSpace) {
            if (_inputPosition > 0) {
                _commands.at(_activeCommand).erase(_inputPosition - 1, 1);
                --_inputPosition;
            }
        }

        // Remove character after _inputPosition
        if (key == Key::Delete) {
            if (_inputPosition <= _commands.at(_activeCommand).size())
                _commands.at(_activeCommand).erase(_inputPosition, 1);
        }
        
        // Go to the beginning of command string
        if (key == Key::Home)
            _inputPosition = 0;

        // Go to the end of command string
        if (key == Key::End)
            _inputPosition = _commands.at(_activeCommand).size();

        if (key == Key::Enter) {
            // SHIFT+ENTER == new line
            if (modifierShift)
                addToCommand("\n");
            // ENTER == run lua script
            else {
                std::string cmd = _commands.at(_activeCommand);
                if (cmd != "") {
                    OsEng.scriptEngine().queueScript(cmd,
                        _remoteScripting ? scripting::ScriptEngine::RemoteScripting::Yes : scripting::ScriptEngine::RemoteScripting::No);
                    
                    // Only add the current command to the history if it hasn't been
                    // executed before. We don't want two of the same commands in a row
                    if (_commandsHistory.empty() || (cmd != _commandsHistory.back()))
                        _commandsHistory.push_back(_commands.at(_activeCommand));
                }
                
                // Some clean up after the execution of the command
                _commands = _commandsHistory;
                _commands.push_back("");
                _activeCommand = _commands.size() - 1;
                _inputPosition = 0;
                setVisible(false);
            }
        }

        if (key == Key::Tab) {
            // We get a list of all the available commands and initially find the first
            // command that starts with how much we typed sofar. We store the index so
            // that in subsequent "tab" presses, we will discard previous commands. This
            // implements the 'hop-over' behavior. As soon as another key is pressed,
            // everything is set back to normal

            // If the shift key is pressed, we decrement the current index so that we will
            // find the value before the one that was previously found
            if (_autoCompleteInfo.lastIndex != NoAutoComplete && modifierShift)
                _autoCompleteInfo.lastIndex -= 2;
            std::vector<std::string> allCommands = OsEng.scriptEngine().allLuaFunctions();
            std::sort(allCommands.begin(), allCommands.end());

            std::string currentCommand = _commands.at(_activeCommand);
            
            // Check if it is the first time the tab has been pressed. If so, we need to
            // store the already entered command so that we can later start the search
            // from there. We will overwrite the 'currentCommand' thus making the storage
            // necessary
            if (!_autoCompleteInfo.hasInitialValue) {
                _autoCompleteInfo.initialValue = currentCommand;
                _autoCompleteInfo.hasInitialValue = true;
            }

            for (int i = 0; i < static_cast<int>(allCommands.size()); ++i) {
                const std::string& command = allCommands[i];

                // Check if the command has enough length (we don't want crashes here)
                // Then check if the iterator-command's start is equal to what we want
                // then check if we need to skip the first found values as the user has
                // pressed TAB repeatedly
                size_t fullLength = _autoCompleteInfo.initialValue.length();
                bool correctLength = command.length() >= fullLength;

                std::string commandLowerCase;
                std::transform(
                    command.begin(), command.end(),
                    std::back_inserter(commandLowerCase),
                    ::tolower
                );
                
                std::string initialValueLowerCase;
                std::transform(
                    _autoCompleteInfo.initialValue.begin(),
                    _autoCompleteInfo.initialValue.end(),
                    std::back_inserter(initialValueLowerCase),
                    ::tolower
                );
    
                bool correctCommand =
                    commandLowerCase.substr(0, fullLength) == initialValueLowerCase;
                
                if (correctLength && correctCommand && (i > _autoCompleteInfo.lastIndex)){
                    // We found our index, so store it
                    _autoCompleteInfo.lastIndex = i;

                    // We only want to auto-complete until the next separator "."
                    size_t pos = command.find('.', fullLength);
                    if (pos == std::string::npos) {
                        // If we don't find a separator, we autocomplete until the end
                        // Set the found command as active command
                        _commands.at(_activeCommand) = command + "();";
                        // Set the cursor position to be between the brackets
                        _inputPosition = _commands.at(_activeCommand).size() - 2;
                    }
                    else {
                        // If we find a separator, we autocomplete until and including the
                        // separator unless the autocompletion would be the same that we
                        // already have (the case if there are multiple commands in the
                        // same group
                        std::string subCommand = command.substr(0, pos + 1);
                        if (subCommand == _commands.at(_activeCommand))
                            continue;
                        else {
                            _commands.at(_activeCommand) = command.substr(0, pos + 1);
                            _inputPosition = _commands.at(_activeCommand).length();
                            // We only want to remove the autocomplete info if we just
                            // entered the 'default' openspace namespace
                            if (command.substr(0, pos + 1) == "openspace.")
                                _autoCompleteInfo = { NoAutoComplete, false, "" };
                        }
                    }

                    break;
                }
            }
        }
        else {
            // If any other key is pressed, we want to remove our previous findings
            // The special case for Shift is necessary as we want to allow Shift+TAB
            if (!modifierShift)
                _autoCompleteInfo = { NoAutoComplete, false, ""};
        }
    }
}

void LuaConsole::charCallback(unsigned int codepoint, KeyModifier modifier) {
    if (codepoint == static_cast<unsigned int>(commandInputButton()))
        return;

#ifndef WIN32
    const bool modifierControl = (modifier == KeyModifier::Control);

    const int codepoint_C = 99;
    const int codepoint_V = 118;
    if (modifierControl && (codepoint == codepoint_C || codepoint == codepoint_V)) {
        return;
    }
#endif
    addToCommand(UnicodeToUTF8(codepoint));

}

void LuaConsole::render() {
    const float font_size = 10.0f;
    
    int ySize = OsEng.renderEngine().fontResolution().y;
    //int ySize = OsEng.windowWrapper().currentWindowSize().y;
    //int ySize = OsEng.windowWrapper().viewportPixelCoordinates().w;

    float startY = static_cast<float>(ySize) - 2.0f * font_size;
    startY = startY - font_size * 15.0f * 2.0f;

    const glm::vec4 red(1, 0, 0, 1);
    const glm::vec4 lightBlue(0.4, 0.4, 1, 1);
    const glm::vec4 green(0, 1, 0, 1);
    const glm::vec4 white(1, 1, 1, 1);
    std::shared_ptr<ghoul::fontrendering::Font> font = OsEng.fontManager().font("Mono", font_size);

    using ghoul::fontrendering::RenderFont;

    if (_remoteScripting) {
        int nClients = OsEng.parallelConnection().nConnections() - 1;
        if (nClients == 1) {
            RenderFont(*font, glm::vec2(15.f, startY + 20.0f), red, "Broadcasting script to 1 client");
        } else {
            RenderFont(*font, glm::vec2(15.f, startY + 20.0f), red, ("Broadcasting script to " + std::to_string(nClients) + " clients").c_str());
        }
        RenderFont(*font, glm::vec2(15.f, startY), red, "$");
    }
    else {
        if (OsEng.parallelConnection().isHost()) {
            RenderFont(*font, glm::vec2(15.f, startY + 20.0f), lightBlue, "Local script execution");
        }
        RenderFont(*font, glm::vec2(15.f, startY), lightBlue, "$");
    }
    RenderFont(*font, glm::vec2(15.f + font_size, startY), white, "%s", _commands.at(_activeCommand).c_str());
    
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
    RenderFont(*font, glm::vec2(15.f + font_size * 0.5f, startY - (font_size)*(n + 1)*3.0f / 2.0f), green, ss.str().c_str(), "^");
    
//    sgct_text::print(font, 15.0f + font_size*0.5f, startY - (font_size)*(n + 1)*3.0f / 2.0f, green, ss.str().c_str(), "^");
}

Key LuaConsole::commandInputButton() {
    // Button left of 1 and above TAB
    // How to deal with different keyboard languages? ---abock
    return Key::GraveAccent;
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

void LuaConsole::toggleMode() {
    if (_isVisible) {
        if (_remoteScripting) {
            _remoteScripting = false;
        } else {
            _isVisible = false;
        }
    } else {
        _remoteScripting = OsEng.parallelConnection().isHost();
        _isVisible = true;
    }
}

void LuaConsole::parallelConnectionChanged(const ParallelConnection::Status& status) {
    _remoteScripting = status == ParallelConnection::Status::Host;
}


scripting::LuaLibrary LuaConsole::luaLibrary() {
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
