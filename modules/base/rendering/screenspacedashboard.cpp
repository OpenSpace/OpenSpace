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

#include <modules/base/rendering/screenspacedashboard.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/distanceconversion.h>
#include <openspace/util/timeconversion.h>
#include <openspace/util/timemanager.h>

#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

namespace {
    const char* KeyName = "Name";
    const char* KeyFontMono = "Mono";

} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceDashboard::Documentation() {
    using namespace openspace::documentation;
    return {
        "ScreenSpace Dashboard",
        "base_screenspace_dashboard",
        {
            {
                KeyName,
                new StringVerifier,
                Optional::Yes,
                "Specifies the GUI name of the ScreenSpaceDashboard"
            }
        }
    };
}

ScreenSpaceDashboard::ScreenSpaceDashboard(const ghoul::Dictionary& dictionary)
    : ScreenSpaceFramebuffer(dictionary)
    , _fontRenderer(nullptr)
    , _fontDate(nullptr)
    , _fontInfo(nullptr)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "ScreenSpaceDashboard"
    );

    if (dictionary.hasKey(KeyName)) {
        setName(dictionary.value<std::string>(KeyName));
    }
    else {
        static int id = 0;
        if (id == 0) {
            setName("ScreenSpaceDashboard");
        }
        else {
            setName("ScreenSpaceDashboard" + std::to_string(id));
        }
        ++id;
    }

    _scale = 1.f;
    _scale.setMaxValue(15.f);

    _size.onChange([this]() {
        _fontRenderer->setFramebufferSize({ _size.value().z, _size.value().w });
    });
}

ScreenSpaceDashboard::~ScreenSpaceDashboard() {}

bool ScreenSpaceDashboard::initializeGL() {
    ScreenSpaceFramebuffer::initializeGL();

    _fontRenderer = ghoul::fontrendering::FontRenderer::createDefault();
    _fontRenderer->setFramebufferSize({ _size.value().z, _size.value().w });


    _fontDate = OsEng.fontManager().font(
        KeyFontMono,
        48
    );
    _fontInfo = OsEng.fontManager().font(
        KeyFontMono,
        32
    );

    addRenderFunction([this]() {

        glm::vec2 penPosition = glm::vec2(
            10.f,
            _size.value().w
        );

        penPosition.y -= _fontDate->height();
        RenderFontCr(
            *_fontDate,
            penPosition,
            "Date: %s",
            OsEng.timeManager().time().UTC().c_str()
        );

        std::pair<double, std::string> deltaTime = simplifyTime(
            OsEng.timeManager().time().deltaTime()
        );
        RenderFontCr(
            *_fontInfo,
            penPosition,
            "Simulation increment: %.1f %s / second",
            deltaTime.first,
            deltaTime.second.c_str()
        );

        double distance = glm::length(
            OsEng.renderEngine().camera()->positionVec3() -
            OsEng.navigationHandler().focusNode()->worldPosition()
        );
        std::pair<double, std::string> dist = simplifyDistance(distance);
        RenderFontCr(
            *_fontInfo,
            penPosition,
            "Distance from focus: %f %s",
            dist.first,
            dist.second.c_str()
        );
    });

    return true;
}

bool ScreenSpaceDashboard::deinitializeGL() {
    _fontRenderer = nullptr;
    return ScreenSpaceFramebuffer::deinitializeGL();
}

bool ScreenSpaceDashboard::isReady() const {
    return (_fontRenderer != nullptr) &&
           (_fontDate != nullptr) &&
           (_fontInfo != nullptr) &&
           ScreenSpaceFramebuffer::isReady();
}

void ScreenSpaceDashboard::update() {
    if (OsEng.windowWrapper().windowHasResized()) {
        glm::ivec2 size = OsEng.windowWrapper().currentWindowResolution();
        _size = { 0.f, 0.f, size.x, size.y };
        _originalViewportSize = size;
        createFramebuffer();
    }
}

} // namespace openspace
