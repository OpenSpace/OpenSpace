/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/rendering/luaconsole.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/misc/clipboard.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>
#include <fstream>

namespace {
    constexpr const char* HistoryFile = "ConsoleHistory";

    constexpr const int NoAutoComplete = -1;

    constexpr const int MaximumHistoryLength = 1000;

    // A high number is chosen since we didn't have a version number before
    // any small number might also be equal to the console history length

    constexpr const uint64_t CurrentVersion = 0xFEEE'FEEE'0000'0001;

    constexpr const openspace::Key CommandInputButton = openspace::Key::GraveAccent;

    constexpr const char* FontName = "Console";
    constexpr const float EntryFontSize = 14.0f;
    constexpr const float HistoryFontSize = 11.0f;

    // Additional space between the entry text and the history (in pixels)
    constexpr const float SeparatorSpace = 30.f;

    // Determines at which speed the console opens.
    constexpr const float ConsoleOpenSpeed = 2.5;

    // The number of characters to display after the cursor
    // when horizontal scrolling is required.
    constexpr const int NVisibleCharsAfterCursor = 5;

    constexpr std::array<const char*, 4> UniformNames = {
        "res", "color", "height", "ortho"
    };

    constexpr openspace::properties::Property::PropertyInfo VisibleInfo = {
        "IsVisible",
        "Is Visible",
        "Determines whether the Lua console is shown on the screen or not. Toggling it "
        "will fade the console in and out."
    };

    constexpr openspace::properties::Property::PropertyInfo RemoveScriptingInfo = {
        "RemoteScripting",
        "Remote scripting",
        "Determines whether the entered commands will only be executed locally (if this "
        "is disabled), or whether they will be send to connected remove instances."
    };

    constexpr openspace::properties::Property::PropertyInfo BackgroundColorInfo = {
        "BackgroundColor",
        "Background Color",
        "Sets the background color of the console."
    };

    constexpr openspace::properties::Property::PropertyInfo HighlightColorInfo = {
        "HighlightColor",
        "Highlight Color",
        "Sets the color of the lines below the console."
    };

    constexpr openspace::properties::Property::PropertyInfo SeparatorColorInfo = {
        "SeparatorColor",
        "Separator Color",
        "Sets the color of the separator between the history part and the entry part of "
        "the console."
    };

    constexpr openspace::properties::Property::PropertyInfo EntryTextColorInfo = {
        "EntryTextColor",
        "Entry Text Color",
        "Sets the text color of the entry area of the console."
    };

    constexpr openspace::properties::Property::PropertyInfo HistoryTextColorInfo = {
        "HistoryTextColor",
        "History Text Color",
        "Sets the text color of the history area of the console."
    };

    constexpr openspace::properties::Property::PropertyInfo HistoryLengthInfo = {
        "HistoryLength",
        "History Length",
        "Determines the length of the history in number of lines."
    };

    std::string sanitizeInput(std::string str) {
        // Remove carriage returns.
        str.erase(std::remove(str.begin(), str.end(), '\r'), str.end());

        // Replace newlines with spaces.
        std::transform(
            str.begin(),
            str.end(),
            str.begin(),
            [](char c) { return c == '\n' ? ' ' : c; }
        );

        return str;
    }

} // namespace

namespace openspace {

LuaConsole::LuaConsole()
    : properties::PropertyOwner({ "LuaConsole" })
    , _isVisible(VisibleInfo, false)
    , _remoteScripting(RemoveScriptingInfo, false)
    , _backgroundColor(
        BackgroundColorInfo,
        glm::vec4(21.f / 255.f, 23.f / 255.f, 28.f / 255.f, 0.8f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _highlightColor(
        HighlightColorInfo,
        glm::vec4(1.f, 1.f, 1.f, 0.f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _separatorColor(
        SeparatorColorInfo,
        glm::vec4(0.4f, 0.4f, 0.4f, 0.f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _entryTextColor(
        EntryTextColorInfo,
        glm::vec4(1.f, 1.f, 1.f, 1.f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _historyTextColor(
        HistoryTextColorInfo,
        glm::vec4(1.0f, 1.0f, 1.0f, 0.65f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _historyLength(HistoryLengthInfo, 13, 0, 100)
    , _autoCompleteInfo({NoAutoComplete, false, ""})
{
    addProperty(_isVisible);
    addProperty(_remoteScripting);
    addProperty(_historyLength);

    _backgroundColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_backgroundColor);

    _highlightColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_highlightColor);

    _separatorColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_separatorColor);

    _entryTextColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_entryTextColor);

    _historyTextColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_historyTextColor);
}

LuaConsole::~LuaConsole() {} // NOLINT

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
                    fmt::format("Outdated console history version: {}", version)
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
    _commands.emplace_back("");
    _activeCommand = _commands.size() - 1;

    _program = ghoul::opengl::ProgramObject::Build(
        "Console",
        absPath("${SHADERS}/luaconsole.vert"),
        absPath("${SHADERS}/luaconsole.frag")
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);

    const GLfloat data[] = {
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
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(GLfloat), nullptr);

    glBindVertexArray(0);

    _font = global::fontManager.font(
        FontName,
        EntryFontSize,
        ghoul::fontrendering::FontManager::Outline::No
    );

    _historyFont = global::fontManager.font(
        FontName,
        HistoryFontSize,
        ghoul::fontrendering::FontManager::Outline::No
    );

    global::parallelPeer.connectionEvent().subscribe(
        "luaConsole",
        "statusChanged",
        [this]() {
            ParallelConnection::Status status = global::parallelPeer.status();
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
        const uint64_t version = CurrentVersion;
        file.write(reinterpret_cast<const char*>(&version), sizeof(uint64_t));

        const int64_t nCommands = _commandsHistory.size();
        file.write(reinterpret_cast<const char*>(&nCommands), sizeof(int64_t));

        for (const std::string& s : _commandsHistory) {
            const int64_t length = s.length();
            file.write(reinterpret_cast<const char*>(&length), sizeof(int64_t));
            // We don't write the \0 at the end on purpose
            file.write(s.c_str(), length);
        }
    }

    _program = nullptr;

    global::parallelPeer.connectionEvent().unsubscribe("luaConsole");
}

bool LuaConsole::keyboardCallback(Key key, KeyModifier modifier, KeyAction action) {
    if ((action != KeyAction::Press) && (action != KeyAction::Repeat)) {
        return false;
    }

    if (key == CommandInputButton) {
        // Button left of 1 and above TAB
        // How to deal with different keyboard languages? ---abock
        if (_isVisible) {
            if (_remoteScripting) {
                _remoteScripting = false;
            }
            else {
                _isVisible = false;
                _commands.back() = "";
                _inputPosition = 0;
            }
        }
        else {
            _isVisible = true;
            if (global::parallelPeer.status() == ParallelConnection::Status::Host) {
                _remoteScripting = true;
            }
        }

        return true;
    }

    if (!_isVisible) {
        return false;
    }

    if (key == Key::Escape) {
        _isVisible = false;
        return true;
    }


    const bool modifierControl = (modifier == KeyModifier::Control);
    const bool modifierShift = (modifier == KeyModifier::Shift);

    // Paste from clipboard
    if (modifierControl && (key == Key::V || key == Key::Y)) {
        addToCommand(sanitizeInput(ghoul::clipboardText()));
        return true;
    }

    // Copy to clipboard
    if (modifierControl && key == Key::C) {
        ghoul::setClipboardText(_commands.at(_activeCommand));
        return true;
    }

    // Cut to clipboard
    if (modifierControl && key == Key::X) {
        ghoul::setClipboardText(_commands.at(_activeCommand));
        _commands.at(_activeCommand).clear();
        _inputPosition = 0;
    }

    // Cut part after cursor to clipboard ("Kill")
    if (modifierControl && key == Key::K) {
        auto here = _commands.at(_activeCommand).begin() + _inputPosition;
        auto end = _commands.at(_activeCommand).end();
        ghoul::setClipboardText(std::string(here, end));
        _commands.at(_activeCommand).erase(here, end);
    }

    // Go to the previous character
    if (key == Key::Left || (modifierControl && key == Key::B)) {
        if (_inputPosition > 0) {
            --_inputPosition;
        }
        return true;
    }

    // Go to the next character
    if (key == Key::Right || (modifierControl && key == Key::F)) {
        _inputPosition = std::min(
            _inputPosition + 1,
            _commands.at(_activeCommand).length()
        );
        return true;
    }

    // Go to the previous '.' character
    if (modifierControl && key == Key::Left) {
        std::string current = _commands.at(_activeCommand);
        std::reverse(current.begin(), current.end());
        auto it = current.find('.', current.size() - (_inputPosition - 1));
        if (it != std::string::npos) {
            _inputPosition = current.size() - it;
        }
        else {
            _inputPosition = 0;
        }
    }

    // Go to the next '.' character
    if (modifierControl && key == Key::Right) {
        auto it = _commands.at(_activeCommand).find('.', _inputPosition);
        if (it != std::string::npos) {
            _inputPosition = it + 1;
        }
        else {
            _inputPosition = _commands.at(_activeCommand).size();
        }
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
    if (key == Key::Home || (modifierControl && key == Key::A)) {
        _inputPosition = 0;
        return true;
    }

    // Go to the end of command string
    if (key == Key::End || (modifierControl && key == Key::E)) {
        _inputPosition = _commands.at(_activeCommand).size();
        return true;
    }

    if (key == Key::Enter || key == Key::KeypadEnter) {
        std::string cmd = _commands.at(_activeCommand);
        if (!cmd.empty()) {
            using RemoteScripting = scripting::ScriptEngine::RemoteScripting;
            global::scriptEngine.queueScript(cmd, RemoteScripting(_remoteScripting));

            // Only add the current command to the history if it hasn't been
            // executed before. We don't want two of the same commands in a row
            if (_commandsHistory.empty() || (cmd != _commandsHistory.back())) {
                _commandsHistory.push_back(_commands.at(_activeCommand));
            }
        }

        // Some clean up after the execution of the command
        _commands = _commandsHistory;
        _commands.emplace_back("");
        _activeCommand = _commands.size() - 1;
        _inputPosition = 0;
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
        std::vector<std::string> allCommands = global::scriptEngine.allLuaFunctions();
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
            const size_t fullLength = _autoCompleteInfo.initialValue.length();
            const bool correctLength = command.length() >= fullLength;

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

            const bool correctCommand =
                commandLowerCase.substr(0, fullLength) == initialValueLowerCase;

            if (correctLength && correctCommand && (i > _autoCompleteInfo.lastIndex)) {
                // We found our index, so store it
                _autoCompleteInfo.lastIndex = i;

                // We only want to auto-complete until the next separator "."
                const size_t pos = command.find('.', fullLength);
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
                    const std::string subCommand = command.substr(0, pos + 1);
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

    // We want to ignore the function keys as they don't translate to text anyway
    if (key >= Key::F1 && key <= Key::F25) {
        return false;
    }

    // Do not consume modifier keys
    switch (key) {
        case Key::LeftShift:
        case Key::RightShift:
        case Key::LeftAlt:
        case Key::RightAlt:
        case Key::LeftControl:
        case Key::RightControl:
            return false;
        default:
            return true;
    }
}

void LuaConsole::charCallback(unsigned int codepoint,
                              [[maybe_unused]] KeyModifier modifier)
{
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

void LuaConsole::update() {
    // Compute the height by simulating _historyFont number of lines and checking
    // what the bounding box for that text would be.
    using namespace ghoul::fontrendering;
    const size_t nLines = std::min(
        static_cast<size_t>(_historyLength),
        _commandsHistory.size()
    );
    const FontRenderer::BoundingBoxInformation& bbox =
        FontRenderer::defaultRenderer().boundingBox(
            *_historyFont,
            std::string(nLines, '\n')
        );

    // Update the full height and the target height.
    // Add the height of the entry line and space for a separator.
    _fullHeight = (bbox.boundingBox.y + EntryFontSize + SeparatorSpace);
    _targetHeight = _isVisible ? _fullHeight : 0;

    // The first frame is going to be finished in approx 10 us, which causes a floating
    // point overflow when computing dHeight
    constexpr double Epsilon = 1e-4;
    const double frametime = std::max(global::windowDelegate.deltaTime(), Epsilon);

    // Update the current height.
    // The current height is the offset that is used to slide
    // the console in from the top.
    const glm::ivec2 res = global::windowDelegate.currentWindowResolution();
    const glm::vec2 dpiScaling = global::windowDelegate.dpiScaling();
    const double dHeight = (_targetHeight - _currentHeight) *
        std::pow(0.98, 1.0 / (ConsoleOpenSpeed / dpiScaling.y * frametime));

    _currentHeight += static_cast<float>(dHeight);

    _currentHeight = std::max(0.0f, _currentHeight);
    _currentHeight = std::min(static_cast<float>(res.y), _currentHeight);
}

void LuaConsole::render() {
    using namespace ghoul::fontrendering;

    // Don't render the console if it's collapsed.
    if (_currentHeight < 1.f) {
        return;
    }

    if (_program->isDirty()) {
        _program->rebuildFromFile();

        ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);
    }

    const glm::vec2 dpiScaling = global::windowDelegate.dpiScaling();
    const glm::ivec2 res =
        glm::vec2(global::windowDelegate.currentWindowResolution()) / dpiScaling;


    // Render background
    glDisable(GL_CULL_FACE);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_DEPTH_TEST);

    _program->activate();

    _program->setUniform(_uniformCache.res, res);
    _program->setUniform(_uniformCache.color, _backgroundColor);
    _program->setUniform(_uniformCache.height, _currentHeight / res.y);
    _program->setUniform(
        _uniformCache.ortho,
        glm::ortho(0.f, static_cast<float>(res.x), 0.f, static_cast<float>(res.y))
    );

    // Draw the background color
    glBindVertexArray(_vao);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    // Draw the highlight lines above and below the background
    _program->setUniform(_uniformCache.color, _highlightColor);
    glDrawArrays(GL_LINES, 1, 4);

    // Draw the separator between the current entry box and the history
    _program->setUniform(_uniformCache.color, _separatorColor);
    _program->setUniform(
        _uniformCache.height,
        _currentHeight / res.y - 2.5f * EntryFontSize / res.y
    );
    glDrawArrays(GL_LINES, 1, 2);

    _program->deactivate();

    glEnable(GL_CULL_FACE);
    glEnable(GL_DEPTH_TEST);

    // Render text on top of the background
    glm::vec2 inputLocation = glm::vec2(
        EntryFontSize / 2.f,
        res.y - _currentHeight + EntryFontSize
    );

    // Render the current command
    std::string currentCommand = _commands.at(_activeCommand);
    // We chop off the beginning and end of the string until it fits on the screen (with
    // a margin) this should be replaced as soon as the mono-spaced fonts work properly.
    // Right now, every third character is a bit wider than the others

    size_t nChoppedCharsBeginning = 0;
    size_t nChoppedCharsEnd = 0;

    const size_t inputPositionFromEnd = currentCommand.size() - _inputPosition;
    while (true) {
        using namespace ghoul::fontrendering;

        // Compute the current width of the string and console prefix.
        const float currentWidth = FontRenderer::defaultRenderer().boundingBox(
            *_font,
            "> " + currentCommand
        ).boundingBox.x + inputLocation.x;

        // Compute the overflow in pixels
        const float overflow = currentWidth - res.x * 0.995f;
        if (overflow <= 0.f) {
            break;
        }

        // Since the overflow is positive, at least one character needs to be removed.
        const size_t nCharsOverflow = static_cast<size_t>(std::min(
            std::max(1.f, overflow / _font->glyph('m')->width()),
            static_cast<float>(currentCommand.size())
        ));

        // Do not hide the cursor and `NVisibleCharsAfterCursor` characters in the end.
        const size_t maxAdditionalCharsToChopEnd = std::max(
            0,
            static_cast<int>(inputPositionFromEnd) - (NVisibleCharsAfterCursor + 1) -
                static_cast<int>(nChoppedCharsEnd)
        );

        // Do not hide the cursor in the beginning.
        const size_t maxAdditionalCharsToChopBeginning = std::max(
            0,
            static_cast<int>(_inputPosition) - 1 -
                static_cast<int>(nChoppedCharsBeginning)
        );

        // Prioritize chopping in the end of the string.
        const size_t nCharsToChopEnd = std::min(
            nCharsOverflow,
            maxAdditionalCharsToChopEnd
        );
        const size_t nCharsToChopBeginning = std::min(
            nCharsOverflow - nCharsToChopEnd,
            maxAdditionalCharsToChopBeginning
        );

        nChoppedCharsBeginning += nCharsToChopBeginning;
        nChoppedCharsEnd += nCharsToChopEnd;

        const size_t displayLength =
            _commands.at(_activeCommand).size() -
            nChoppedCharsBeginning - nChoppedCharsEnd;

        currentCommand = _commands.at(_activeCommand).substr(
            nChoppedCharsBeginning,
            displayLength
        );
    }

    RenderFont(
        *_font,
        inputLocation,
        "> " + currentCommand,
        _entryTextColor,
        ghoul::fontrendering::CrDirection::Down
    );

    // Just offset the ^ marker slightly for a nicer look
    inputLocation.y += 3 * dpiScaling.y;

    // Render the ^ marker below the text to show where the current entry point is
    RenderFont(
        *_font,
        inputLocation,
        (std::string(_inputPosition - nChoppedCharsBeginning + 2, ' ') + "^"),
        _entryTextColor
    );

    glm::vec2 historyInputLocation = glm::vec2(
        HistoryFontSize / 2.f,
        res.y - HistoryFontSize * 1.5f + _fullHeight - _currentHeight
    );

    // @CPP: Replace with array_view
    std::vector<std::string> commandSubset;
    if (_commandsHistory.size() < static_cast<size_t>(_historyLength)) {
        commandSubset = _commandsHistory;
    }
    else {
        commandSubset = std::vector<std::string>(
            _commandsHistory.end() - _historyLength,
            _commandsHistory.end()
        );
    }

    for (const std::string& cmd : commandSubset) {
        RenderFont(
            *_historyFont,
            historyInputLocation,
            cmd,
            _historyTextColor,
            ghoul::fontrendering::CrDirection::Down
        );
    }

    // Computes the location for right justified text on the same y height as the entry
    auto locationForRightJustifiedText = [this, res](const std::string& text) {
        using namespace ghoul::fontrendering;

        const glm::vec2 loc = glm::vec2(
            EntryFontSize / 2.f,
            res.y - _currentHeight + EntryFontSize
        );

        const FontRenderer::BoundingBoxInformation bbox =
            FontRenderer::defaultRenderer().boundingBox(*_font, text);

        return glm::vec2(
            loc.x + res.x - bbox.boundingBox.x - 10.f,
            loc.y
        );
    };

    if (_remoteScripting) {
        const glm::vec4 Red(1, 0, 0, 1);

        ParallelConnection::Status status = global::parallelPeer.status();
        const int nClients =
            status != ParallelConnection::Status::Disconnected ?
            global::parallelPeer.nConnections() - 1 :
            0;

        const std::string nClientsText =
            nClients == 1 ?
            "Broadcasting script to 1 client" :
            "Broadcasting script to " + std::to_string(nClients) + " clients";

        const glm::vec2 loc = locationForRightJustifiedText(nClientsText);
        RenderFont(*_font, loc, nClientsText, Red);
    } else if (global::parallelPeer.isHost()) {
        const glm::vec4 LightBlue(0.4, 0.4, 1, 1);

        const std::string localExecutionText = "Local script execution";
        const glm::vec2 loc = locationForRightJustifiedText(localExecutionText);
        RenderFont(*_font, loc, localExecutionText, LightBlue);
    }
}

float LuaConsole::currentHeight() const {
    return _currentHeight;
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
