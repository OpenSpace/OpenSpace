/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <openspace/rendering/helper.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/misc/clipboard.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/stringhelper.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
#include <filesystem>
#include <fstream>

namespace {
    constexpr std::string_view HistoryFile = "ConsoleHistory";

    constexpr int NoAutoComplete = -1;

    constexpr int MaximumHistoryLength = 1000;

    // A high number is chosen since we didn't have a version number before
    // any small number might also be equal to the console history length

    constexpr uint64_t CurrentVersion = 0xFEEE'FEEE'0000'0001;

    constexpr std::string_view FontName = "Console";
    constexpr float EntryFontSize = 14.f;
    constexpr float HistoryFontSize = 11.f;

    // Additional space between the entry text and the history (in pixels)
    constexpr float SeparatorSpace = 30.f;

    // Determines at which speed the console opens.
    constexpr float ConsoleOpenSpeed = 2.5;

    // The number of characters to display after the cursor
    // when horizontal scrolling is required.
    constexpr int NVisibleCharsAfterCursor = 5;

    constexpr openspace::properties::Property::PropertyInfo VisibleInfo = {
        "IsVisible",
        "Is Visible",
        "Determines whether the Lua console is shown on the screen or not. Toggling it "
        "will fade the console in and out.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShouldBeSynchronizedInfo = {
       "ShouldBeSynchronized",
       "Should Be Synchronized",
       "Determines whether the entered commands will only be executed locally (if this "
       "is disabled), or whether they will be send to other connected nodes, for "
       "example in a cluster environment.",
       openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShouldSendToRemoteInfo = {
        "ShouldSendToRemote",
        "Should Send To Remote",
        "Determines whether the entered commands will only be executed locally (if this "
        "is disabled), or whether they will be send to connected remote instances (other "
        "peers through a parallel connection).",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo BackgroundColorInfo = {
        "BackgroundColor",
        "Background Color",
        "Sets the background color of the console.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo EntryTextColorInfo = {
        "EntryTextColor",
        "Entry Text Color",
        "Sets the text color of the entry area of the console.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo HistoryTextColorInfo = {
        "HistoryTextColor",
        "History Text Color",
        "Sets the text color of the history area of the console.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo HistoryLengthInfo = {
        "HistoryLength",
        "History Length",
        "Determines the length of the history in number of lines.",
        openspace::properties::Property::Visibility::AdvancedUser
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
    : properties::PropertyOwner({ "LuaConsole", "Lua Console" })
    , _isVisible(VisibleInfo, false)
    , _shouldBeSynchronized(ShouldBeSynchronizedInfo, true)
    , _shouldSendToRemote(ShouldSendToRemoteInfo, false)
    , _backgroundColor(
        BackgroundColorInfo,
        glm::vec4(21.f / 255.f, 23.f / 255.f, 28.f / 255.f, 0.8f),
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
        glm::vec4(1.f, 1.f, 1.f, 0.65f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _historyLength(HistoryLengthInfo, 13, 0, 100)
    , _autoCompleteInfo({NoAutoComplete, false, ""})
{
    addProperty(_isVisible);
    addProperty(_shouldBeSynchronized);
    addProperty(_shouldSendToRemote);
    addProperty(_historyLength);

    _backgroundColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_backgroundColor);

    _entryTextColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_entryTextColor);

    _historyTextColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_historyTextColor);
}

void LuaConsole::initialize() {
    ZoneScoped;

    const std::filesystem::path filename = FileSys.cacheManager()->cachedFilename(
        HistoryFile,
        ""
    );
    if (std::filesystem::is_regular_file(filename)) {
        std::ifstream file(filename, std::ios::binary | std::ios::in);

        if (file.good()) {
            // Read the number of commands from the history
            uint64_t version = 0;
            file.read(reinterpret_cast<char*>(&version), sizeof(uint64_t));

            if (version != CurrentVersion) {
                LWARNINGC(
                    "LuaConsole",
                    std::format("Outdated console history version: {}", version)
                );
            }
            else {
                int64_t nCommands = 0;
                file.read(reinterpret_cast<char*>(&nCommands), sizeof(int64_t));

                for (int64_t i = 0; i < nCommands; i++) {
                    int64_t length = 0;
                    file.read(reinterpret_cast<char*>(&length), sizeof(int64_t));

                    std::vector<char> tmp(length);
                    file.read(tmp.data(), length);
                    _commandsHistory.emplace_back(tmp.begin(), tmp.end());
                }
            }
        }
    }

    _commands = _commandsHistory;
    _commands.emplace_back("");
    _activeCommand = _commands.size() - 1;

    const float dpi = global::windowDelegate->osDpiScaling();

    _font = global::fontManager->font(
        FontName,
        EntryFontSize * dpi,
        ghoul::fontrendering::FontManager::Outline::No
    );

    _historyFont = global::fontManager->font(
        FontName,
        HistoryFontSize * dpi,
        ghoul::fontrendering::FontManager::Outline::No
    );

    global::parallelPeer->connectionEvent().subscribe(
        "luaConsole",
        "statusChanged",
        [this]() {
            const ParallelConnection::Status status = global::parallelPeer->status();
            parallelConnectionChanged(status);
        }
    );
}

void LuaConsole::deinitialize() {
    ZoneScoped;

    if (!FileSys.cacheManager()) {
        return;
    }

    const std::filesystem::path filename = FileSys.cacheManager()->cachedFilename(
        HistoryFile,
        ""
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

    global::parallelPeer->connectionEvent().unsubscribe("luaConsole");
}

bool LuaConsole::keyboardCallback(Key key, KeyModifier modifier, KeyAction action) {
    if ((action != KeyAction::Press) && (action != KeyAction::Repeat)) {
        return false;
    }

    const bool modifierShift = (modifier == KeyModifier::Shift);
    const bool modifierControl = (modifier == KeyModifier::Control);

    // Button left of 1 and above TAB (default)
    // Can be changed to any other key with the setCommandInputButton funciton
    if (key == _commandInputButton) {
        if (_isVisible) {
            if (modifierShift) {
                // Toggle ShouldBeSynchronized property for all scripts
                _shouldBeSynchronized = !_shouldBeSynchronized;
            }
            else if (modifierControl) {
                // Only allow this toggle if a ParallelConnection exists
                if (_shouldSendToRemote) {
                    _shouldSendToRemote = false;
                }
                else if (global::parallelPeer->status() ==
                         ParallelConnection::Status::Host)
                {
                    _shouldSendToRemote = true;
                }
            }
            else {
                _isVisible = false;
                _commands.back() = "";
                _inputPosition = 0;
            }
        }
        else {
            _isVisible = true;
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
        const std::string cmd = _commands.at(_activeCommand);
        if (!cmd.empty()) {
            global::scriptEngine->queueScript(
                cmd,
                scripting::ScriptEngine::ShouldBeSynchronized(_shouldBeSynchronized),
                scripting::ScriptEngine::ShouldSendToRemote(_shouldSendToRemote)
            );

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
        std::vector<std::string> allCommands = global::scriptEngine->allLuaFunctions();
        std::sort(allCommands.begin(), allCommands.end());

        const std::string currentCommand = _commands.at(_activeCommand);

        // Check if it is the first time the tab has been pressed. If so, we need to
        // store the already entered command so that we can later start the search
        // from there. We will overwrite the 'currentCommand' thus making the storage
        // necessary
        if (!_autoCompleteInfo.hasInitialValue) {
            _autoCompleteInfo.initialValue = currentCommand;
            _autoCompleteInfo.hasInitialValue = true;
        }

        for (int i = 0; i < static_cast<int>(allCommands.size()); i++) {
            const std::string& command = allCommands[i];

            // Check if the command has enough length (we don't want crashes here)
            // Then check if the iterator-command's start is equal to what we want
            // then check if we need to skip the first found values as the user has
            // pressed TAB repeatedly
            const size_t fullLength = _autoCompleteInfo.initialValue.length();
            const bool correctLength = command.length() >= fullLength;

            const std::string commandLowerCase = ghoul::toLowerCase(command);

            const std::string initialValueLowerCase = ghoul::toLowerCase(
                _autoCompleteInfo.initialValue
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
                            _autoCompleteInfo = {
                                .lastIndex = NoAutoComplete,
                                .hasInitialValue = false,
                                .initialValue = ""
                            };
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
            _autoCompleteInfo = {
                .lastIndex = NoAutoComplete,
                .hasInitialValue = false,
                .initialValue = ""
            };
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

    if (codepoint == static_cast<unsigned int>(_commandInputButton)) {
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

    addToCommand(std::string(1, static_cast<char>(codepoint)));
}

void LuaConsole::update() {
    ZoneScoped;

    // Compute the height by simulating _historyFont number of lines and checking
    // what the bounding box for that text would be.
    using namespace ghoul::fontrendering;

    const float height =
        _historyFont->height() *
        (std::min(static_cast<int>(_commandsHistory.size()), _historyLength.value()) + 1);

    // Update the full height and the target height.
    // Add the height of the entry line and space for a separator.
    const float dpi = global::windowDelegate->osDpiScaling();
    _fullHeight = (height + EntryFontSize * dpi + SeparatorSpace);
    _targetHeight = _isVisible ? _fullHeight : 0;

    // The first frame is going to be finished in approx 10 us, which causes a floating
    // point overflow when computing dHeight
    constexpr double Epsilon = 1e-4;
    const double frametime = std::max(global::windowDelegate->deltaTime(), Epsilon);

    // Update the current height.
    // The current height is the offset that is used to slide
    // the console in from the top.
    const glm::ivec2 res = global::windowDelegate->currentSubwindowSize();
    const glm::vec2 dpiScaling = global::windowDelegate->dpiScaling();
    const double dHeight = (_targetHeight - _currentHeight) *
        std::pow(0.98, 1.0 / (ConsoleOpenSpeed / dpiScaling.y * frametime));

    _currentHeight += static_cast<float>(dHeight);

    _currentHeight = std::max(0.f, _currentHeight);
    _currentHeight = std::min(static_cast<float>(res.y), _currentHeight);
}

void LuaConsole::render() {
    ZoneScoped;

    using namespace ghoul::fontrendering;

    const ghoul::GLDebugGroup group("LuaConsole");

    // Don't render the console if it's collapsed.
    if (_currentHeight < 1.f) {
        return;
    }

    const glm::vec2 dpiScaling = global::windowDelegate->dpiScaling();
    const glm::ivec2 res =
        glm::vec2(global::windowDelegate->currentSubwindowSize()) / dpiScaling;


    // Render background
    glDisable(GL_CULL_FACE);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_DEPTH_TEST);

    rendering::helper::renderBox(
        glm::vec2(0.f),
        glm::vec2(1.f, _currentHeight / res.y),
        _backgroundColor
    );

    glEnable(GL_CULL_FACE);
    glEnable(GL_DEPTH_TEST);

    // Render text on top of the background
    const float dpi = global::windowDelegate->osDpiScaling();
    glm::vec2 inputLocation = glm::vec2(
        EntryFontSize * dpi / 2.f,
        res.y - _currentHeight + EntryFontSize * dpi
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
        const float currentWidth =
            _font->boundingBox("> " + currentCommand).x + inputLocation.x;

        // Compute the overflow in pixels
        const float overflow = currentWidth - res.x * 0.995f;
        if (overflow <= 0.f) {
            break;
        }

        // Since the overflow is positive, at least one character needs to be removed.
        const size_t nCharsOverflow = static_cast<size_t>(std::min(
            std::max(1.f, overflow / _font->glyph('m')->width),
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
        HistoryFontSize * dpi / 2.f,
        res.y - HistoryFontSize * dpi * 1.5f + _fullHeight - _currentHeight
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
    auto locationForRightJustifiedText = [this, res, dpi](const std::string& text) {
        using namespace ghoul::fontrendering;

        const glm::vec2 loc = glm::vec2(
            EntryFontSize * dpi / 2.f,
            res.y - EntryFontSize * dpi
        );

        const glm::vec2 bbox = _font->boundingBox(text);
        return glm::vec2(loc.x + res.x - bbox.x - 10.f, loc.y);
    };

    if (!_shouldBeSynchronized) {
        const glm::vec4 Yellow(1.0f, 1.0f, 0.f, 1.f);

        const std::string masterOnlyExecutionText =
            "Master only script execution (Nodes and Peers will not recieve scripts)";
        const glm::vec2 loc = locationForRightJustifiedText(masterOnlyExecutionText);
        RenderFont(*_font, loc, masterOnlyExecutionText, Yellow);
    }
    else if (_shouldSendToRemote) {
        const glm::vec4 Red(1.f, 0.f, 0.f, 1.f);

        const ParallelConnection::Status status = global::parallelPeer->status();
        const int nClients =
            status != ParallelConnection::Status::Disconnected ?
            global::parallelPeer->nConnections() - 1 :
            0;

        const std::string nClientsText =
            nClients == 1 ?
            "Broadcasting script to 1 client" :
            "Broadcasting script to " + std::to_string(nClients) + " clients";

        const glm::vec2 loc = locationForRightJustifiedText(nClientsText);
        RenderFont(*_font, loc, nClientsText, Red);
    }
    else if (global::parallelPeer->isHost()) {
        const glm::vec4 LightBlue(0.4f, 0.4f, 1.f, 1.f);

        const std::string localExecutionText =
            "Local script execution (Peers will not recieve scripts)";
        const glm::vec2 loc = locationForRightJustifiedText(localExecutionText);
        RenderFont(*_font, loc, localExecutionText, LightBlue);
    }
}

float LuaConsole::currentHeight() const {
    return _currentHeight;
}

void LuaConsole::setCommandInputButton(Key key) {
    _commandInputButton = key;
}

void LuaConsole::addToCommand(const std::string& c) {
    const size_t length = c.length();
    _commands.at(_activeCommand).insert(_inputPosition, c);
    _inputPosition += length;
}

void LuaConsole::parallelConnectionChanged(const ParallelConnection::Status& status) {
    _shouldSendToRemote = (status == ParallelConnection::Status::Host);
}

} // namespace openspace
