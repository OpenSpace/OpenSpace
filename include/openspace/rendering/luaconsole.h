/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_CORE___LUACONSOLE___H__
#define __OPENSPACE_CORE___LUACONSOLE___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/network/parallelconnection.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/vec4property.h>
#include <openspace/util/keys.h>
#include <openspace/util/mouse.h>
#include <functional>
#include <memory>

namespace ghoul {
    namespace fontrendering { class Font; }
    namespace opengl { class ProgramObject; }
} // namespace ghoul

namespace openspace {

class LuaConsole : public properties::PropertyOwner {
public:
    LuaConsole();
    ~LuaConsole() override = default;

    void initialize();
    void deinitialize();

    bool keyboardCallback(Key key, KeyModifier modifier, KeyAction action);
    void charCallback(unsigned int codepoint, KeyModifier modifier);
    bool mouseActivationCallback(glm::vec2 pos, MouseButton button, MouseAction action,
        KeyModifier mods);

    void update();
    void render();
    float currentHeight() const;

    void setCommandInputButton(Key key);

private:
    void parallelConnectionChanged(const ParallelConnection::Status& status);
    void addToCommand(const std::string& c);
    void registerKeyHandlers();
    void registerKeyHandler(Key key, KeyModifier modifier,
        std::function<void()> callback);

    // Helper functions for tab autocomplete
    void autoCompleteCommand();
    size_t detectContext(std::string_view command);
    bool gatherPathSuggestions(size_t contextStart);
    void gatherFunctionSuggestions(size_t contextStart);
    void filterSuggestions();
    void cycleSuggestion();
    void applySuggestion();

    properties::BoolProperty _isVisible;
    properties::BoolProperty _shouldBeSynchronized;
    properties::BoolProperty _shouldSendToRemote;

    properties::Vec4Property _backgroundColor;
    properties::Vec4Property _entryTextColor;
    properties::Vec4Property _historyTextColor;
    properties::IntProperty _historyLength;

    Key _commandInputButton = Key::GraveAccent;

    size_t _inputPosition = 0;
    std::vector<std::string> _commandsHistory;
    size_t _activeCommand = 0;
    std::vector<std::string> _commands;
    // Map of registered keybinds and their corresponding callbacks
    std::map<KeyWithModifier, std::function<void()>> _keyHandlers;

    enum class Context {
        None = 0,
        Function,
        Path
    };

    struct AutoCompleteState {
        AutoCompleteState();

        Context context; // Assumed context we are currently in based on
        bool isDataDirty; // Flag indicating if we need to update the suggestion data
        std::string input; // Part of the command that we're intrested in
        std::vector<std::string> suggestions; // All suggestions found so far
        int currentIndex; // Current suggestion index
        std::string suggestion; // Current suggestion to show
        bool cycleReverse; // Whether we should cycle suggestions forward or backwards
        size_t insertPosition; // Where to insert the suggestion in the command
    };

    AutoCompleteState  _autoCompleteState;

    float _currentHeight = 0.f;
    float _targetHeight = 0.f;
    float _fullHeight = 0.f;

    std::shared_ptr<ghoul::fontrendering::Font> _font;
    std::shared_ptr<ghoul::fontrendering::Font> _historyFont;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___LUACONSOLE___H__
