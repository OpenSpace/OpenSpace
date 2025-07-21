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

#ifndef __OPENSPACE_CORE___KEYFRAMERECORDINGHANDLER___H__
#define __OPENSPACE_CORE___KEYFRAMERECORDINGHANDLER___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/interaction/sessionrecording.h>
#include <openspace/scripting/lualibrary.h>
#include <filesystem>
#include <string>
#include <vector>

namespace openspace::interaction {

class KeyframeRecordingHandler : public properties::PropertyOwner {
public:
    KeyframeRecordingHandler();

    void newSequence();
    void addCameraKeyframe(double sequenceTime);
    void addScriptKeyframe(double sequenceTime, std::string script);
    void removeKeyframe(int index);
    void updateKeyframe(int index);
    void moveKeyframe(int index, double sequenceTime);
    void saveSequence(std::filesystem::path filename);
    void loadSequence(std::filesystem::path filename);
    void play();
    bool hasKeyframeRecording() const;
    std::vector<ghoul::Dictionary> keyframes() const;

    static openspace::scripting::LuaLibrary luaLibrary();

private:
    SessionRecording _timeline;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___KEYFRAMERECORDINGHANDLER___H__
