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

#ifndef __OPENSPACE_MODULE_SPACE___SOUNDMODULE___H__
#define __OPENSPACE_MODULE_SPACE___SOUNDMODULE___H__

#include <openspace/util/openspacemodule.h>

#include <openspace/documentation/documentation.h>
#include <openspace/scripting/lualibrary.h>
#include <ghoul/misc/boolean.h>

namespace FMOD {
    class Channel;
    class Sound;
    class System;
} // namespace FMOD

namespace openspace {

class SoundModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "Sound";

    SoundModule();
    ~SoundModule() override = default;
    std::vector<documentation::Documentation> documentations() const override;

    scripting::LuaLibrary luaLibrary() const override;

    BooleanType(ShouldLoop);
    int playAudio(std::string path, ShouldLoop loop);
    void stopAudio(int handle);
    bool isPlaying(int handle) const;

    void stopAll() const;
    std::vector<int> currentlyPlaying() const;

    BooleanType(IsRamped);
    void setVolumeChangeRamped(int handle, IsRamped isRamped) const;

    void setVolume(int handle, float volume) const;
    float volume(int handle) const;

    BooleanType(IsMute);
    void setMute(int handle, IsMute mute) const;
    bool isMute(int handle) const;

    std::vector<std::string> drivers() const;
    void setDriver(int index) const;

private:
    void internalInitialize(const ghoul::Dictionary&) override;
    void internalDeinitializeGL() override;

    FMOD::System* _system = nullptr;
    struct Info {
        FMOD::Channel* channel = nullptr;
        FMOD::Sound* sound = nullptr;
    };
    std::map<int, Info> _channels;
    int _nextHandle = 0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___SOUNDMODULE___H__
