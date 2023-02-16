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

#include <modules/video/include/renderablevideosphere.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/sphere.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/programobject.h>
#include <optional>

namespace {

    constexpr openspace::properties::Property::PropertyInfo PlayInfo = {
        "Play",
        "Play",
        "Play video"
    };

    constexpr openspace::properties::Property::PropertyInfo PauseInfo = {
        "Pause",
        "Pause",
        "Pause video"
    };

    constexpr openspace::properties::Property::PropertyInfo GoToStartInfo = {
        "GoToStart",
        "Go To Start",
        "Go to start in video"
    };

    constexpr openspace::properties::Property::PropertyInfo ResetInfo = {
       "Reset",
       "Reset",
       "Reset video"
    };

struct [[codegen::Dictionary(RenderableVideoSphere)]] Parameters {
    
};
#include "renderablevideosphere_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableVideoSphere::Documentation() {
    return codegen::doc<Parameters>("base_renderable_sphere");
}

RenderableVideoSphere::RenderableVideoSphere(const ghoul::Dictionary& dictionary)
    : RenderableSphere(dictionary)
    , _videoPlayer(dictionary)
    , _play(PlayInfo)
    , _pause(PauseInfo)
    , _goToStart(GoToStartInfo)
    , _reset(ResetInfo)
{
    _play.onChange([this]() { _videoPlayer.play(); });
    addProperty(_play);
    _pause.onChange([this]() { _videoPlayer.pause(); });
    addProperty(_pause);
    _goToStart.onChange([this]() { _videoPlayer.goToStart(); });
    addProperty(_goToStart);
    _reset.onChange([this]() { _videoPlayer.reset(); });
    addProperty(_reset);
}

void RenderableVideoSphere::bindTexture() {
    _videoPlayer.frameTexture().get()->bind();
}
} // namespace openspace