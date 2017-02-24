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

#include <openspace/scripting/scriptengine.h>
#include <openspace/network/parallelconnection.h>

#include <openspace/util/keys.h>

#include <string>
#include <vector>

namespace openspace {

class LuaConsole {
public:
    LuaConsole();

    void initialize();
    void deinitialize();

    void keyboardCallback(Key key, KeyModifier modifier, KeyAction action);
    void charCallback(unsigned int codepoint, KeyModifier modifier);

    void render();

    Key commandInputButton();

    bool isVisible() const;
    void setVisible(bool visible);
    bool isRemoteScripting() const;
    void setRemoteScripting(bool remoteScripting);

    void toggleMode();
        
    static scripting::LuaLibrary luaLibrary();


private:
    void parallelConnectionChanged(const ParallelConnection::Status& status);
    void addToCommand(std::string c);
    std::string UnicodeToUTF8(unsigned int codepoint);

    size_t _inputPosition;
    std::vector<std::string> _commandsHistory;
    size_t _activeCommand;
    std::vector<std::string> _commands;

    std::string _filename;

    struct {
        int lastIndex;
        bool hasInitialValue;
        std::string initialValue;
    } _autoCompleteInfo;

    bool _isVisible;
    bool _remoteScripting;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___LUACONSOLE___H__
