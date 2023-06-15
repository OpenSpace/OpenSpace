/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#ifndef __OPENSPACE_CORE___KEYFRAMERECORDING___H__
#define __OPENSPACE_CORE___KEYFRAMERECORDING___H__

#include <vector>
#include <string>

#include <json/json.hpp>

#include <openspace/properties/propertyowner.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/navigation/keyframenavigator.h>

namespace openspace::interaction {

class KeyframeRecording : public properties::PropertyOwner {
public:
    KeyframeRecording();

    ~KeyframeRecording() override;

    bool newSequence(std::string filename);

    bool addKeyframe(double sequenceTime);

    bool updateKeyframe(int index);

    bool moveKeyframe(int index, double sequenceTime);

    bool saveSequence();

    bool loadSequence(std::string filename);

    void preSynchronization(double dt);

    void play();

    void pause();

    void setSequenceTime(double sequenceTime);

    /**
     * \return The Lua library that contains all Lua functions available to affect the
     * interaction
     */
    static openspace::scripting::LuaLibrary luaLibrary();
protected:

private:
    void sortKeyframes();

    nlohmann::json newKeyframe(double sequenceTime);

    KeyframeNavigator::CameraPose keyframeToPose(const nlohmann::json& keyframe) const;

    std::vector<nlohmann::json> _keyframes;
    std::string _filename;
    bool _playing;
    bool _timeChanged;
    bool _stateChanged;
    double _sequenceTime;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___KEYFRAMERECORDING___H__
