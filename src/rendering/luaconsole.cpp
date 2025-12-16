/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
#include <filesystem>
#include <fstream>

namespace {
    constexpr std::string_view HistoryFile = "ConsoleHistory";
    constexpr std::string_view JumpCharacters = ".,'/()\\";
    constexpr std::string_view PathStartIdentifier = "\"'[";
    constexpr std::string_view PathEndIdentifier = "\"']";

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

    constexpr openspace::properties::Property::PropertyInfo VisibleInfo = {
        "IsVisible",
        "Is visible",
        "Determines whether the Lua console is shown on the screen or not. Toggling it "
        "will fade the console in and out.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShouldBeSynchronizedInfo = {
       "ShouldBeSynchronized",
       "Should be synchronized",
       "Determines whether the entered commands will only be executed locally (if this "
       "is disabled), or whether they will be send to other connected nodes, for "
       "example in a cluster environment.",
       openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShouldSendToRemoteInfo = {
        "ShouldSendToRemote",
        "Should send to remote",
        "Determines whether the entered commands will only be executed locally (if this "
        "is disabled), or whether they will be send to connected remote instances (other "
        "peers through a parallel connection).",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo BackgroundColorInfo = {
        "BackgroundColor",
        "Background color",
        "Sets the background color of the console.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo EntryTextColorInfo = {
        "EntryTextColor",
        "Entry text color",
        "Sets the text color of the entry area of the console.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo HistoryTextColorInfo = {
        "HistoryTextColor",
        "History text color",
        "Sets the text color of the history area of the console.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo HistoryLengthInfo = {
        "HistoryLength",
        "History length",
        "Determines the length of the history in number of lines.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    std::string sanitizeInput(std::string str) {
        // Remove carriage returns
        str.erase(std::remove(str.begin(), str.end(), '\r'), str.end());

        std::transform(
            str.begin(),
            str.end(),
            str.begin(),
            [](char c) {
                // Replace newlines with spaces
                if (c == '\n') {
                    return ' ';
                }

                // The documentation contains “ and ” which we convert to " to make them
                // copy-and-pastable
                if (c == -109 || c == -108) {
                    return '"';
                }

                // Convert \ into / to make paths pastable
                if (c == '\\') {
                    return '/';
                }

                return c;
            }
        );

        return str;
    }
} // namespace

namespace openspace {


LuaConsole::AutoCompleteState::AutoCompleteState()
    : context{ Context::None }
    , isDataDirty{ true }
    , input{ "" }
    , suggestions{ }
    , currentIndex{ NoAutoComplete }
    , suggestion{ "" }
    , cycleReverse{ false }
    , insertPosition{ 0 }
{}

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
    , _autoCompleteState{}
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

    registerKeyHandlers();
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
                _autoCompleteState = AutoCompleteState();
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

    KeyWithModifier keyCombination = KeyWithModifier(key, modifier);

    // Call the registered function for the key combination pressed
    auto it = _keyHandlers.find(keyCombination);
    if (it != _keyHandlers.end()) {
        it->second();
    }

    // If any other key is pressed, we want to remove our previous findings
    // The special case for Shift is necessary as we want to allow Shift+TAB
    const bool isShiftModifierOnly = (key == Key::LeftShift || key == Key::RightShift);
    if (key != Key::Tab && !isShiftModifierOnly) {
        _autoCompleteState = AutoCompleteState();
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
    std::string currentCommand = _commands[_activeCommand];

    // Render the command and suggestions with line breaks where required, + 2 accounts
    // for the '> ' characters in the beginning of the command
    const size_t totalCommandSize = 2 + currentCommand.size() +
        _autoCompleteState.suggestion.size();
    // Scalefactor 0.925f chosen arbitraily to fit characters on screen with some margin
    const size_t nCharactersPerRow = std::max(
        static_cast<size_t>(1),
        static_cast<size_t>(res.x * 0.925f / static_cast<float>(_font->glyph('m')->width))
    );
    size_t nCommandRows = static_cast<size_t>(
        std::ceil(static_cast<double>(totalCommandSize) / nCharactersPerRow)
    );
    // We're intrested in the zero based index when computing the input position
    // If the characters fit on one line we should not add any extra rows
    nCommandRows = nCommandRows > 1 ? nCommandRows - 1 : 0;

    // The command is split in 3 parts to render the suggestion in a different color:
    // the part before the suggestion, the suggestion, and the part after the suggestion
    std::string beforeSuggestion =  currentCommand;
    std::string afterSuggestion = "";

    if (_autoCompleteState.insertPosition != 0) {
        beforeSuggestion = currentCommand.substr(0, _autoCompleteState.insertPosition);
        afterSuggestion = currentCommand.substr(_autoCompleteState.insertPosition);
    }

    // We pad the strings with empty spaces so that each part is rendered in their correct
    // positions, even if linebreaks are added
    // Pad suggestion before and after with ' '
    const std::string suggestion = std::string(beforeSuggestion.size() + 2, ' ') +
        _autoCompleteState.suggestion + std::string(afterSuggestion.size(), ' ');
    // Pad first part at the end with ' '
    beforeSuggestion.insert(
        beforeSuggestion.end(),
        totalCommandSize - beforeSuggestion.size(),
        ' '
    );
    // Pad second part in the beginning with ' '
    afterSuggestion.insert(
        afterSuggestion.begin(),
        totalCommandSize - afterSuggestion.size(),
        ' '
    );

    // Adds newline character to command whenever it reaches the max width of the window
    auto linebreakCommand = [nCharactersPerRow](const std::string& s) {
        const bool requiresSplitting = s.size() > nCharactersPerRow;

        if (!requiresSplitting) {
            return s;
        }

        std::string result;
        result.reserve(s.size() + s.size() / nCharactersPerRow);

        size_t count = 0;
        for (char c : s) {
            result += c;
            count++;

            if (c == '\n') {
                count = 0;
                continue;
            }

            if (count >= nCharactersPerRow) {
                result += '\n';
                count = 0;
            }
        }

        return result;
    };

    // Offset the command depending on how many rows we require for a nicer look
    inputLocation.y += nCommandRows * EntryFontSize * 1.25f * dpiScaling.y;

    RenderFont(
        *_font,
        inputLocation,
        linebreakCommand("> " + beforeSuggestion),
        _entryTextColor
    );

    // Render suggestion
    RenderFont(
        *_font,
        inputLocation,
        linebreakCommand(suggestion),
        glm::vec4(0.7f, 0.7f, 0.7f, 1.f)
    );

    RenderFont(
        *_font,
        inputLocation,
        linebreakCommand(afterSuggestion),
        _entryTextColor,
        ghoul::fontrendering::CrDirection::Down
    );

    // Move the marker to the correct row if there are multiple
    const size_t markerStartRow = nCommandRows - (_inputPosition + 2) / nCharactersPerRow;
    inputLocation.y += markerStartRow * EntryFontSize * 1.25f * dpiScaling.y;
    // Just offset the ^ marker slightly for a nicer look
    inputLocation.y += 3 * dpiScaling.y;

    // Render the ^ marker below the text to show where the current entry point is
    RenderFont(
        *_font,
        inputLocation,
        (std::string((_inputPosition + 2) % nCharactersPerRow, ' ') + "^"),
        _entryTextColor
    );

    glm::vec2 historyInputLocation = glm::vec2(
        HistoryFontSize * dpi / 2.f,
        res.y - HistoryFontSize * dpi * 1.5f + _fullHeight - _currentHeight
    );

    // @CPP: Replace with array_view
    std::vector<std::string> commandSubset;
    if ((_commandsHistory.size() + nCommandRows) < static_cast<size_t>(_historyLength)) {
        commandSubset = _commandsHistory;
    }
    else {
        // Historic lines are reduced by the number of rows the command is occupying
        const size_t historyOffset = std::min(
            nCommandRows, static_cast<size_t>(_historyLength)
        );
        commandSubset = std::vector<std::string>(
            _commandsHistory.end() - _historyLength + historyOffset,
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
    _commands[_activeCommand].insert(_inputPosition, c);
    _inputPosition += length;
}

void LuaConsole::parallelConnectionChanged(const ParallelConnection::Status& status) {
    _shouldSendToRemote = (status == ParallelConnection::Status::Host);
}

void LuaConsole::registerKeyHandler(Key key, KeyModifier modifier,
                                    std::function<void()> callback)
{
    _keyHandlers[{ key, modifier }] = std::move(callback);
}

void LuaConsole::registerKeyHandlers() {
    registerKeyHandler(
        Key::Escape,
        KeyModifier::None,
        [this]() { _isVisible = false; }
    );

    // Paste from clipboard
    registerKeyHandler(Key::V, KeyModifier::Control, [this]() {
        addToCommand(sanitizeInput(ghoul::clipboardText()));
    });

    // Paste from clipboard
    registerKeyHandler(Key::Y, KeyModifier::Control, [this]() {
        addToCommand(sanitizeInput(ghoul::clipboardText()));
    });

    // Copy to clipboard
    registerKeyHandler(Key::C, KeyModifier::Control, [this]() {
        ghoul::setClipboardText(_commands[_activeCommand]);
    });

    // Cut to clipboard
    registerKeyHandler(
        Key::X,
        KeyModifier::Control,
        [this]() {
            ghoul::setClipboardText(_commands[_activeCommand]);
            _commands[_activeCommand].clear();
            _inputPosition = 0;
        }
    );

    // Cut part after cursor to clipboard ("Kill")
    registerKeyHandler(
        Key::K,
        KeyModifier::Control,
        [this]() {
            auto here = _commands[_activeCommand].begin() + _inputPosition;
            auto end = _commands[_activeCommand].end();
            ghoul::setClipboardText(std::string(here, end));
            _commands[_activeCommand].erase(here, end);
        }
    );

    // Go to the previous JumpCharacter character
    registerKeyHandler(
        Key::Left,
        KeyModifier::Control,
        [this]() {
            std::string current = _commands[_activeCommand];
            std::reverse(current.begin(), current.end());
            const size_t start = current.size() - (_inputPosition - 1);
            const size_t jumpCharPos = current.find_first_of(JumpCharacters, start);
            if (jumpCharPos != std::string::npos) {
                _inputPosition = current.size() - jumpCharPos;
            }
            else {
                _inputPosition = 0;
            }
        }
    );

    // Go to the next JumpCharacter character
    registerKeyHandler(
        Key::Right,
        KeyModifier::Control,
        [this]() {
            const std::string current = _commands[_activeCommand];
            const size_t jumpCharPos = current.find_first_of(
                JumpCharacters,
                _inputPosition + 1
            );
            if (jumpCharPos != std::string::npos) {
                _inputPosition = jumpCharPos;
            }
            else {
                _inputPosition = current.size();
            }
        }
    );

    // Go to the previous character
    registerKeyHandler(
        Key::Left,
        KeyModifier::None,
        [this]() {
            if (_inputPosition > 0) {
                _inputPosition--;
            }
        }
    );

    // Go to the previous character
    registerKeyHandler(
        Key::B,
        KeyModifier::Control,
            [this]() {
            if (_inputPosition > 0) {
                _inputPosition--;
            }
        }
    );

    // Go to the next character
    registerKeyHandler(
        Key::Right,
        KeyModifier::None,
        [this]() {
            if (!_autoCompleteState.suggestion.empty()) {
                applySuggestion();
                return;
            }

            _inputPosition = std::min(
                _inputPosition + 1,
                _commands[_activeCommand].length()
            );
        }
    );

    // Go to the next character
    registerKeyHandler(
        Key::F,
        KeyModifier::Control,
        [this]() {
            _inputPosition = std::min(
                _inputPosition + 1,
                _commands[_activeCommand].length()
            );
        }
    );

    // Go to previous command
    registerKeyHandler(
        Key::Up,
        KeyModifier::None,
        [this]() {
            if (_activeCommand > 0) {
                _activeCommand--;
            }
            _inputPosition = _commands[_activeCommand].length();
        }
    );

    // Go to next command (the last is empty)
    registerKeyHandler(
        Key::Down,
        KeyModifier::None,
        [this]() {
            if (_activeCommand < _commands.size() - 1) {
                _activeCommand++;
            }
            _inputPosition = _commands[_activeCommand].length();
        }
    );

    // Remove character before _inputPosition
    registerKeyHandler(
        Key::BackSpace,
        KeyModifier::None,
        [this]() {
            if (_inputPosition > 0) {
                _commands[_activeCommand].erase(_inputPosition - 1, 1);
                _inputPosition--;
            }
        }
    );

    // Remove characters before _inputPosition until the previous JumpCharacter.
    registerKeyHandler(
        Key::BackSpace,
        KeyModifier::Control,
        [this]() {
            if (_inputPosition == 0) {
                return;
            }

            std::string& command = _commands[_activeCommand];

            // If the previous character is a JumpCharacter, remove just that one. This
            // behavior results in abc.de -> abc. -> abc -> 'empty string'
            if (JumpCharacters.find(command[_inputPosition - 1]) != std::string::npos) {
                command.erase(_inputPosition - 1, 1);
                _inputPosition--;
                return;
            }

            // Find the position of the last JumpCharacter before _inputPosition
            size_t start = 0;
            for (size_t i = _inputPosition; i > 0; i--) {
                if (JumpCharacters.find(command[i - 1]) != std::string::npos) {
                    start = i;
                    break;
                }
            }

            size_t count = _inputPosition - start;
            command.erase(start, count);
            _inputPosition -= count;
        }
    );

    // Remove character after _inputPosition
    registerKeyHandler(
        Key::Delete,
        KeyModifier::None,
            [this]() {
            if (_inputPosition <= _commands[_activeCommand].size()) {
                _commands[_activeCommand].erase(_inputPosition, 1);
            }
        }
    );

    // Remove characters after _inputPosition until the ne JumpCharacter
    registerKeyHandler(
        Key::Delete,
        KeyModifier::Control,
        [this]() {
            std::string& command = _commands[_activeCommand];
            if (_inputPosition >= command.size()) {
                return;
            }

            // If the next character after _inputPosition is a JumpCharacter, delete just
            // that
            if (JumpCharacters.find(command[_inputPosition]) != std::string::npos) {
                command.erase(_inputPosition, 1);
                return;
            }

            // Find the position of the next Jumpcharacter after _inputPosition
            size_t next = command.find_first_of(JumpCharacters, _inputPosition);
            size_t end = next != std::string::npos ? next : command.size();
            command.erase(_inputPosition, end - _inputPosition);
        }
    );

    // Go to the beginning of command string
    registerKeyHandler(
        Key::Home,
        KeyModifier::None,
        [this]() {
            _inputPosition = 0;
        }
    );

    // Go to the beginning of command string
    registerKeyHandler(
        Key::A,
        KeyModifier::Control,
        [this]() {
            _inputPosition = 0;
        }
    );

    // Go to end of command string
    registerKeyHandler(
        Key::End,
        KeyModifier::None,
        [this]() {
            _inputPosition = _commands[_activeCommand].size();
        }
    );

    // Go to end of command string
    registerKeyHandler(
        Key::E,
        KeyModifier::Control,
        [this]() {
            _inputPosition = _commands[_activeCommand].size();
        }
    );


    auto executeCommand = [this]() {
        if (!_autoCompleteState.suggestion.empty()) {
            applySuggestion();
            return;
        }

        const std::string cmd = _commands[_activeCommand];
        if (!cmd.empty()) {
            using Script = scripting::ScriptEngine::Script;
            global::scriptEngine->queueScript({
                .code = cmd,
                .synchronized = Script::ShouldBeSynchronized(_shouldBeSynchronized),
                .sendToRemote = Script::ShouldSendToRemote(_shouldSendToRemote)
            });

            // Only add the current command to the history if it hasn't been
            // executed before. We don't want two of the same commands in a row
            if (_commandsHistory.empty() || (cmd != _commandsHistory.back())) {
                _commandsHistory.push_back(_commands[_activeCommand]);
            }
        }

        // Some clean up after the execution of the command
        _commands = _commandsHistory;
        _commands.emplace_back("");
        _activeCommand = _commands.size() - 1;
        _inputPosition = 0;
    };

    registerKeyHandler(Key::Enter, KeyModifier::None, executeCommand);
    registerKeyHandler(Key::KeypadEnter, KeyModifier::None, executeCommand);

    registerKeyHandler(Key::Tab, KeyModifier::None, [this]() {
        _autoCompleteState.cycleReverse = false;
        autoCompleteCommand();
    });

    registerKeyHandler(Key::Tab, KeyModifier::Shift, [this]() {
        _autoCompleteState.cycleReverse = true;
        autoCompleteCommand();
    });
}

void LuaConsole::autoCompleteCommand() {
    // We get a list of all the available commands or paths and initially find the
    // first match that starts with how much we typed so far. We store the index so
    // that in subsequent "tab" presses, we will discard previous matches. This
    // implements the 'hop-over' behavior. As soon as another key is pressed,
    // everything is set back to normal

    const std::string currentCommand = _commands[_activeCommand];
    // Determine if we are currently in a function or path context
    if (_autoCompleteState.isDataDirty) {
        const size_t contextStart = detectContext(currentCommand);

        switch (_autoCompleteState.context) {
            case Context::Path: {
                const bool hasSugestions = gatherPathSuggestions(contextStart);
                if (!hasSugestions) {
                    return;
                }
                break;
            }
            case Context::Function:
                gatherFunctionSuggestions(contextStart);
                break;
            default:
                throw ghoul::RuntimeError("Unhandled context");
        }

        filterSuggestions();
        _autoCompleteState.isDataDirty = false;
    }
    cycleSuggestion();
}

size_t LuaConsole::detectContext(std::string_view command) {
    // Find the path starting point which can start with either " ' or [
    size_t pathStartIndex = 0;
    for (size_t i = _inputPosition; i > 0; i--) {
        if (PathStartIdentifier.find(command[i - 1]) != std::string::npos) {
            pathStartIndex = i;
            break;
        }
    }

    // @TODO (anden88, 2025-08-08): Detect functions in a smarter way that allows nested
    // function calls. The following example currently does not work.
    // If the user typed e.g., "openspace.printInfo(open", we will not be able to
    // autocomplete the last openspace. since the first instance we find is at the
    // beginning, resulting in the fragment being wrongly assumed as "printInfo(open"
    size_t functionStartIndex = command.rfind("openspace.");

    if (functionStartIndex == std::string::npos) {
        functionStartIndex = 0;
    }

    const bool isPathContext = pathStartIndex > functionStartIndex;

    _autoCompleteState.context = isPathContext ? Context::Path : Context::Function;
    return isPathContext ? pathStartIndex : functionStartIndex;
}

bool LuaConsole::gatherPathSuggestions(size_t contextStart) {
    const std::string currentCommand = _commands[_activeCommand];
    // Find the end of the path
    const std::string possiblePath = currentCommand.substr(contextStart);
    // Find the last ' " ] if any exists, which marks the end of the path string.
    // Otherwise the rest of the string is assumed to be part of the path
    const size_t pathEnd = possiblePath.find_last_of(PathEndIdentifier);
    const std::string userTypedPath = currentCommand.substr(contextStart, pathEnd);

    const std::filesystem::path path = std::filesystem::path(userTypedPath);

    std::filesystem::path dirToSearch;

    if (std::filesystem::exists(path) && std::filesystem::is_directory(path)) {
        // User typed a full valid directory - show its contents
        dirToSearch = path;
    }
    else {
        // Not a valid dir - check the parent
        std::filesystem::path parent = path.parent_path();
        if (std::filesystem::exists(parent) && std::filesystem::is_directory(parent)) {
            dirToSearch = parent;
        }
        else {
            // Neither path nor parent is valid, cancel autocomplete
            return false;
        }
    }

    // Get the entries in directory
    std::vector<std::filesystem::path> suggestions =
        ghoul::filesystem::walkDirectory(
            dirToSearch,
            ghoul::filesystem::Recursive::No,
            ghoul::filesystem::Sorted::Yes
        );

    std::vector<std::string> entries;
    for (const std::filesystem::path& entry : suggestions) {
        // Filter paths that contain non-ASCII characters
        if (ghoul::containsNonAscii(entry)) {
            continue;
        }

        entries.push_back(entry.string());
    }

    _autoCompleteState.suggestions = std::move(entries);
    _autoCompleteState.input = userTypedPath;

    if (pathEnd != std::string::npos) {
        // There is something after the path so we want to insert inbetween
        _autoCompleteState.insertPosition = contextStart + pathEnd;
    }
    else {
        // There is nothing after the path so we render normally at the end
        _autoCompleteState.insertPosition = currentCommand.size();
    }

    return true;
}

void LuaConsole::gatherFunctionSuggestions(size_t contextStart) {
    const std::string currentCommand = _commands[_activeCommand];
    // Get a list of all the available commands
    std::vector<std::string> allCommands = global::scriptEngine->allLuaFunctions();
    std::sort(allCommands.begin(), allCommands.end());

    _autoCompleteState.suggestions = std::move(allCommands);

    std::string possibleFunction = currentCommand.substr(contextStart);
    // Find the nearest parenthesis if any exists, which marks the end of the
    // function. Otherwise the rest of the string is assumed to be part of
    // the function
    size_t functionEnd = possibleFunction.find_first_of("()");
    _autoCompleteState.input = possibleFunction.substr(0, functionEnd);

    if (functionEnd != std::string::npos) {
        // There is something after the function so we want to insert inbetween
        _autoCompleteState.insertPosition = contextStart + functionEnd;
    }
    else {
        // There is nothing after the path so we render normally at the end
        _autoCompleteState.insertPosition = currentCommand.size();
    }
}

void LuaConsole::filterSuggestions() {
    auto normalize = [this](const std::string& s) {
        std::string out = s;

        if (_autoCompleteState.context == Context::Function) {
            out = ghoul::toLowerCase(out);
        }

#ifdef WIN32
        // On Windows, file paths are generally case-insensitive. For example,
        // "C:/User/Desktop/Foo" refers to the same location as "c:/user/desktop/foo"
        // Normalize paths to lowercase so they are treated equivalently
        if (_autoCompleteState.context == Context::Path) {
            out = ghoul::toLowerCase(sanitizeInput(out));
        }
#endif // WIN32

        return out;
    };

    std::vector<std::string> results;
    results.reserve(_autoCompleteState.suggestions.size());

    std::string input = normalize(_autoCompleteState.input);

    for (const std::string& suggestion : _autoCompleteState.suggestions) {
        std::string suggestionNormalized = normalize(suggestion);
        std::string result = sanitizeInput(suggestion);

        if (_autoCompleteState.context == Context::Function) {
            // We're only interested in autocomplete until the next separator "."
            const size_t offset = input.size();
            const size_t pos = suggestionNormalized.find('.', offset);

            if (pos != std::string::npos) {
                result = result.substr(0, pos + 1); // include the "."
            }
        }

        if (suggestionNormalized.starts_with(input)) {
            results.push_back(result);
        }
    }

    results.shrink_to_fit();

    // We're only interested in unique matches, and want them sorted alphabetically
    std::sort(results.begin(), results.end());
    results.erase(std::unique(results.begin(), results.end()), results.end());

    _autoCompleteState.suggestions = std::move(results);
}

void LuaConsole::cycleSuggestion() {
    if (_autoCompleteState.suggestions.empty()) {
        return;
    }

    const int size = static_cast<int>(_autoCompleteState.suggestions.size());
    const int dir = _autoCompleteState.cycleReverse ? -1 : 1;

    // First time cycling: pick start depending on direction
    if (_autoCompleteState.currentIndex == NoAutoComplete) {
        _autoCompleteState.currentIndex = _autoCompleteState.cycleReverse ? size - 1 : 0;
    }
    else {
        // Wrap around on either start or end edges
        _autoCompleteState.currentIndex =
            (_autoCompleteState.currentIndex + dir + size) % size;
    }

    const std::string& suggestion =
        _autoCompleteState.suggestions[_autoCompleteState.currentIndex];
    // Show only the characters not yet written
    _autoCompleteState.suggestion = suggestion.substr(_autoCompleteState.input.size());
}

void LuaConsole::applySuggestion() {
    std::string& currentCommand = _commands[_activeCommand];
    currentCommand.insert(
        _autoCompleteState.insertPosition,
        _autoCompleteState.suggestion
    );
    // Set cursor to the end of the command
    _inputPosition = _autoCompleteState.insertPosition +
        _autoCompleteState.suggestion.size();

    if (_autoCompleteState.context == Context::Function &&
        !_autoCompleteState.suggestion.ends_with('.'))
    {
        // We're in a leaf function => add parantheses
        currentCommand.insert(_inputPosition, "()");
        // Set the cursor position to be between the brackets
        _inputPosition++;
    }

    // Clear suggestion
    _autoCompleteState = AutoCompleteState();
}

} // namespace openspace
