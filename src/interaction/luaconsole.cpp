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

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/network/parallelconnection.h>

#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/misc/clipboard.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>

#include <glm/gtc/matrix_transform.hpp>

#include <fstream>
#include <string>

namespace {
    const char* HistoryFile = "ConsoleHistory";

    const int NoAutoComplete = -1;

    const int MaximumHistoryLength = 1000;

    // A high number is chosen since we didn't have a version number before
    // any small number might also be equal to the console history length

    // @CPP17
    //const uint64_t CurrentVersion = 0xFEEE'FEEE'0000'0001;
    const uint64_t CurrentVersion = 0xFEEEFEEE00000001;

    const openspace::Key CommandInputButton = openspace::Key::GraveAccent;
} // namespace

namespace openspace {

LuaConsole::LuaConsole()
    : properties::PropertyOwner("LuaConsole")
    , _isVisible("isVisible", "Is Visible", false)
    , _remoteScripting("remoteScripting", "Remote scripting", false)
    , _width("width", "Width", 1.f, 0.f, 1.f)
    , _backgroundColor(
        "backgroundColor",
        "Background Color",
        glm::vec4(0.f, 60.f / 255.f, 102.f / 255.f, 1.f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _highlightColor(
        "highlightColor",
        "Highlight Color",
        glm::vec4(1.f, 1.f, 1.f, 0.8f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _historyLength("historyLength", "History Length", 13, 0, 100)
    //, _height("height", "Height", 0.04f, 0.f, 1.f)
    , _inputPosition(0)
    , _activeCommand(0)
    , _autoCompleteInfo({NoAutoComplete, false, ""})
    , _currentHeight(0.f)
{
    addProperty(_isVisible);
    addProperty(_remoteScripting);

    //addProperty(_height);
    addProperty(_width);
    _backgroundColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_backgroundColor);
    _highlightColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_highlightColor);
    addProperty(_historyLength);
}

void LuaConsole::initialize() {
    const std::string filename = FileSys.cacheManager()->cachedFilename(
        HistoryFile,
        "",
        ghoul::filesystem::CacheManager::Persistent::Yes
    );

    if (FileSys.fileExists(filename)) {
        std::ifstream file(filename, std::ios::binary | std::ios::in);

        if (file.good()) {
            // Read the number of commands from the history
            uint64_t version;
            file.read(reinterpret_cast<char*>(&version), sizeof(uint64_t));

            if (version != CurrentVersion) {
                LWARNINGC(
                    "LuaConsole",
                    "Outdated console history version: " << version
                );
            }
            else {
                int64_t nCommands;
                file.read(reinterpret_cast<char*>(&nCommands), sizeof(int64_t));

                for (int64_t i = 0; i < nCommands; ++i) {
                    int64_t length;
                    file.read(reinterpret_cast<char*>(&length), sizeof(int64_t));

                    std::vector<char> tmp(length);
                    file.read(tmp.data(), length);
                    _commandsHistory.emplace_back(std::string(tmp.begin(), tmp.end()));
                }
            }
        }
    }

    _commands = _commandsHistory;
    _commands.push_back("");
    _activeCommand = _commands.size() - 1;

    _program = ghoul::opengl::ProgramObject::Build(
        "Console",
        "${SHADERS}/luaconsole.vert",
        "${SHADERS}/luaconsole.frag"
    );

    GLfloat data[] = {
        0.f, 0.f,
        1.f, 1.f,
        0.f, 1.f,

        0.f, 0.f,
        1.f, 0.f,
        1.f, 1.f
    };

    glGenVertexArrays(1, &_vao);
    glBindVertexArray(_vao);
    glGenBuffers(1, &_vbo);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(GL_ARRAY_BUFFER, sizeof(data), data, GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0,
        2,
        GL_FLOAT,
        GL_FALSE,
        2 * sizeof(GLfloat),
        reinterpret_cast<void*>(0)
    );

    glBindVertexArray(0);

    OsEng.parallelConnection().connectionEvent()->subscribe(
        "luaConsole",
        "statusChanged",
        [this]() {
            ParallelConnection::Status status = OsEng.parallelConnection().status();
            parallelConnectionChanged(status);
        }
    );
}

void LuaConsole::deinitialize() {
    const std::string filename = FileSys.cacheManager()->cachedFilename(
        HistoryFile,
        "",
        ghoul::filesystem::CacheManager::Persistent::Yes
    );

    // We want to limit the command history to a realistic value, so that it doesn't
    // grow without bounds
    if (_commandsHistory.size() > MaximumHistoryLength) {
        _commandsHistory = std::vector<std::string>(
            _commandsHistory.end() - MaximumHistoryLength,
            _commandsHistory.end()
        );
    }

    std::ofstream file(filename, std::ios::binary);
    if (file.good()) {
        uint64_t version = CurrentVersion;
        file.write(reinterpret_cast<const char*>(&version), sizeof(uint64_t));

        int64_t nCommands = _commandsHistory.size();
        file.write(reinterpret_cast<const char*>(&nCommands), sizeof(int64_t));

        for (const std::string& s : _commandsHistory) {
            int64_t length = s.length();
            file.write(reinterpret_cast<const char*>(&length), sizeof(int64_t));
            // We don't write the \0 at the end on purpose
            file.write(s.c_str(), length);
        }
    }

    _program = nullptr;

    OsEng.parallelConnection().connectionEvent()->unsubscribe("luaConsole");
}

bool LuaConsole::keyboardCallback(Key key, KeyModifier modifier, KeyAction action) {
    if (action != KeyAction::Press && action != KeyAction::Repeat) {
        return false;
    }

    if (key == CommandInputButton) {
        // Button left of 1 and above TAB
        // How to deal with different keyboard languages? ---abock
        if (_isVisible) {
            if (_remoteScripting) {
                _remoteScripting = false;
            } else {
                _isVisible = false;
            }
        } else {
            _isVisible = true;
            if (OsEng.parallelConnection().status() == ParallelConnection::Status::Host) {
                _remoteScripting = true;
            }
        }
        
        return true;
    }

    if (!_isVisible) {
        return false;
    }

    const bool modifierControl = (modifier == KeyModifier::Control);
    const bool modifierShift = (modifier == KeyModifier::Shift);

    // Paste from clipboard
    if (modifierControl && (key == Key::V)) {
        addToCommand(ghoul::clipboardText());
        return true;
    }

    // Copy to clipboard
    if (modifierControl && (key == Key::C)) {
        ghoul::setClipboardText(_commands.at(_activeCommand));
        return true;
    }

    // Go to the previous character
    if ((key == Key::Left) && (_inputPosition > 0)) {
        --_inputPosition;
        return true;
    }

    // Go to the next character
    if (key == Key::Right) {
        _inputPosition = std::min(
            _inputPosition + 1,
            _commands.at(_activeCommand).length()
        );
        return true;
    }

    // Go to previous command
    if (key == Key::Up) {
        if (_activeCommand > 0) {
            --_activeCommand;
        }
        _inputPosition = _commands.at(_activeCommand).length();
        return true;
    }

    // Go to next command (the last is empty)
    if (key == Key::Down) {
        if (_activeCommand < _commands.size() - 1) {
            ++_activeCommand;
        }
        _inputPosition = _commands.at(_activeCommand).length();
        return true;
    }

    // Remove character before _inputPosition
    if (key == Key::BackSpace) {
        if (_inputPosition > 0) {
            _commands.at(_activeCommand).erase(_inputPosition - 1, 1);
            --_inputPosition;
        }
        return true;
    }

    // Remove character after _inputPosition
    if (key == Key::Delete) {
        if (_inputPosition <= _commands.at(_activeCommand).size()) {
            _commands.at(_activeCommand).erase(_inputPosition, 1);
        }
        return true;
    }
        
    // Go to the beginning of command string
    if (key == Key::Home) {
        _inputPosition = 0;
        return true;
    }

    // Go to the end of command string
    if (key == Key::End) {
        _inputPosition = _commands.at(_activeCommand).size();
        return true;
    }

    if (key == Key::Enter) {
        std::string cmd = _commands.at(_activeCommand);
        if (cmd != "") {
            using RemoteScripting = scripting::ScriptEngine::RemoteScripting;
            OsEng.scriptEngine().queueScript(
                cmd,
                _remoteScripting ? RemoteScripting::Yes : RemoteScripting::No
            );
                    
            // Only add the current command to the history if it hasn't been
            // executed before. We don't want two of the same commands in a row
            if (_commandsHistory.empty() || (cmd != _commandsHistory.back())) {
                _commandsHistory.push_back(_commands.at(_activeCommand));
            }
        }
                
        // Some clean up after the execution of the command
        _commands = _commandsHistory;
        _commands.push_back("");
        _activeCommand = _commands.size() - 1;
        _inputPosition = 0;
        _isVisible = false;
        return true;
    }

    if (key == Key::Tab) {
        // We get a list of all the available commands and initially find the first
        // command that starts with how much we typed sofar. We store the index so
        // that in subsequent "tab" presses, we will discard previous commands. This
        // implements the 'hop-over' behavior. As soon as another key is pressed,
        // everything is set back to normal

        // If the shift key is pressed, we decrement the current index so that we will
        // find the value before the one that was previously found
        if (_autoCompleteInfo.lastIndex != NoAutoComplete && modifierShift) {
            _autoCompleteInfo.lastIndex -= 2;
        }
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
                [](char v) { return static_cast<char>(tolower(v)); }
            );
                
            std::string initialValueLowerCase;
            std::transform(
                _autoCompleteInfo.initialValue.begin(),
                _autoCompleteInfo.initialValue.end(),
                std::back_inserter(initialValueLowerCase),
                [](char v) { return static_cast<char>(tolower(v)); }
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
                    if (subCommand == _commands.at(_activeCommand)) {
                        continue;
                    }
                    else {
                        _commands.at(_activeCommand) = command.substr(0, pos + 1);
                        _inputPosition = _commands.at(_activeCommand).length();
                        // We only want to remove the autocomplete info if we just
                        // entered the 'default' openspace namespace
                        if (command.substr(0, pos + 1) == "openspace.") {
                            _autoCompleteInfo = { NoAutoComplete, false, "" };
                        }
                    }
                }

                break;
            }
        }
        return true;
    }
    else {
        // If any other key is pressed, we want to remove our previous findings
        // The special case for Shift is necessary as we want to allow Shift+TAB
        if (!modifierShift) {
            _autoCompleteInfo = { NoAutoComplete, false, "" };
        }
    }

    return true;
}

void LuaConsole::charCallback(unsigned int codepoint, KeyModifier modifier) {
    if (!_isVisible) {
        return;
    }

    if (codepoint == static_cast<unsigned int>(CommandInputButton)) {
        return;
    }

#ifndef WIN32
    const bool modifierControl = (modifier == KeyModifier::Control);

    const int codepoint_C = 99;
    const int codepoint_V = 118;
    if (modifierControl && (codepoint == codepoint_C || codepoint == codepoint_V)) {
        return;
    }
#endif

// Disallow all non ASCII characters for now
if (codepoint > 0x7f) {
    return;
}

addToCommand(std::string(1, static_cast<const char>(codepoint)));
}

void LuaConsole::render() {
    static const float EntryFontSize = 13.0f;
    static const float HistoryFontSize = 10.0f;

    if (!_isVisible) {
        // When we toggle the console back to visible, we want to start at 0 height
        _currentHeight = 0.f;
        return;
    }

    if (_program->isDirty()) {
        _program->rebuildFromFile();
    }

    std::shared_ptr<ghoul::fontrendering::Font> font = OsEng.fontManager().font(
        "Inconsolata", EntryFontSize
    );
    std::shared_ptr<ghoul::fontrendering::Font> historyFont = OsEng.fontManager().font(
        "Inconsolata", HistoryFontSize
    );

    glm::ivec2 res = OsEng.windowWrapper().currentWindowResolution();

    // Update the current height
    const float frametime = static_cast<float>(OsEng.windowWrapper().averageDeltaTime());
    static const float Delta = 2.f; // Speed at which the console opens up

    auto bbox = ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
        *historyFont,
        std::string(_historyLength, '\n').c_str()
    );

    const float MaximumHeight = bbox.boundingBox.y + 2 * EntryFontSize;
    _currentHeight = std::min(_currentHeight + Delta * frametime, MaximumHeight / res.y);


    // Render background
    glDisable(GL_CULL_FACE);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_DEPTH_TEST);

    _program->activate();

    const glm::mat4 projection = glm::ortho(
        0.f,
        static_cast<float>(res.x),
        0.f,
        static_cast<float>(res.y)
    );

    _program->setUniform("res", res);
    _program->setUniform("width", _width);
    _program->setUniform("color", _backgroundColor);
    _program->setUniform("height", _currentHeight);
    _program->setUniform("ortho", projection);

    glBindVertexArray(_vao);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    _program->setUniform("color", _highlightColor);
    glDrawArrays(GL_LINES, 1, 4);

    _program->deactivate();

    glEnable(GL_CULL_FACE);
    glEnable(GL_DEPTH_TEST);


    // Render text
    const int ySize = OsEng.renderEngine().fontResolution().y;

    const glm::vec4 red(1, 0, 0, 1);
    const glm::vec4 lightBlue(0.4, 0.4, 1, 1);
    const glm::vec4 green(0, 1, 0, 1);
    const glm::vec4 white(1, 1, 1, 1);



    glm::vec2 inputLocation = glm::vec2(
        res.x / 2.f - _width * res.x / 2.f + EntryFontSize / 2.f,
        res.y - _currentHeight * res.y + EntryFontSize / 2.f
    );

    RenderFont(
        *font,
        inputLocation,
        white,
        "> %s",
        _commands.at(_activeCommand).c_str()
    );

    glm::vec2 historyInputLocation = glm::vec2(
        res.x / 2.f - _width * res.x / 2.f + HistoryFontSize / 2.f,
        res.y - HistoryFontSize * 1.5f
    );

    // @CPP17: Replace with array_view
    std::vector<std::string> commandSubset;
    if (_commandsHistory.size() < _historyLength) {
        commandSubset = _commandsHistory;
    }
    else {
        commandSubset = std::vector<std::string>(
            _commandsHistory.end() - _historyLength,
            _commandsHistory.end()
        );
    }

    for (const std::string& cmd : commandSubset) {
    //for (size_t i = 0; i < _historyLength; ++i) {
        RenderFontCr(
            *historyFont,
            historyInputLocation,
            white,
            cmd.c_str()
        );
    }



    auto locationForRightJustifiedText = [&](const std::string& text) {
        using namespace ghoul::fontrendering;
        auto bbox = FontRenderer::defaultRenderer().boundingBox(*font, text.c_str());
        return glm::vec2(
            inputLocation.x + _width * res.x - bbox.boundingBox.x - 10.f,
            inputLocation.y
        );
    };

    if (_remoteScripting) {
        int nClients = OsEng.parallelConnection().nConnections() - 1;
        if (nClients == 1) {
            const glm::vec2 loc = locationForRightJustifiedText(
                "Broadcasting script to 1 client"
            );

            RenderFont(
                *font,
                loc,
                red,
                "Broadcasting script to 1 client"
            );
        }
        else {
            const glm::vec2 loc = locationForRightJustifiedText(
                "Broadcasting script to " + std::to_string(nClients) + " clients"
            );

            RenderFont(
                *font,
                loc,
                red,
                ("Broadcasting script to " + std::to_string(nClients) + " clients").c_str()
            );
        }
    }
    else {
        if (OsEng.parallelConnection().isHost()) {
            const glm::vec2 loc = locationForRightJustifiedText(
                "Local script execution"
            );

            RenderFont(
                *font,
                loc,
                lightBlue,
                "Local script execution"
            );
        }
    }
}

void LuaConsole::addToCommand(std::string c) {
    const size_t length = c.length();
    _commands.at(_activeCommand).insert(_inputPosition, std::move(c));
    _inputPosition += length;
}

void LuaConsole::parallelConnectionChanged(const ParallelConnection::Status& status) {
    _remoteScripting = (status == ParallelConnection::Status::Host);
}

} // namespace openspace
