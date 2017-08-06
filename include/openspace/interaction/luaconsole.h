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

#ifndef __OPENSPACE_CORE___LUACONSOLE___H__
#define __OPENSPACE_CORE___LUACONSOLE___H__

#include <openspace/network/parallelconnection.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/vector/vec4property.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/keys.h>

#include <string>
#include <vector>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

class LuaConsole : public properties::PropertyOwner {
public:
    LuaConsole();

    void initialize();
    void deinitialize();

    bool keyboardCallback(Key key, KeyModifier modifier, KeyAction action);
    void charCallback(unsigned int codepoint, KeyModifier modifier);

    void update();
    void render();
    float currentHeight() const;

private:
    void parallelConnectionChanged(const ParallelConnection::Status& status);
    void addToCommand(std::string c);
    std::string sanitizeInput(std::string str);

    properties::BoolProperty _isVisible;
    properties::BoolProperty _remoteScripting;

    properties::Vec4Property _backgroundColor;
    properties::Vec4Property _highlightColor;
    properties::Vec4Property _separatorColor;
    properties::Vec4Property _entryTextColor;
    properties::Vec4Property _historyTextColor;
    properties::IntProperty _historyLength;

    
    size_t _inputPosition;
    std::vector<std::string> _commandsHistory;
    size_t _activeCommand;
    std::vector<std::string> _commands;

    struct {
        int lastIndex;
        bool hasInitialValue;
        std::string initialValue;
    } _autoCompleteInfo;

    float _currentHeight;
    float _targetHeight;
    float _fullHeight;

    std::shared_ptr<ghoul::fontrendering::Font> _font;
    std::shared_ptr<ghoul::fontrendering::Font> _historyFont;

    std::unique_ptr<ghoul::opengl::ProgramObject> _program;
    GLuint _vao;
    GLuint _vbo;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___LUACONSOLE___H__
