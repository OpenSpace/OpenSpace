/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/base/dashboard/dashboarditemsimulationincrement.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/timeconversion.h>
#include <openspace/util/timemanager.h>

#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

namespace {
    const char* KeyFontMono = "Mono";
    const float DefaultFontSize = 10.f;

    static const openspace::properties::Property::PropertyInfo FontNameInfo = {
        "FontName",
        "Font Name",
        "This value is the name of the font that is used. It can either refer to an "
        "internal name registered previously, or it can refer to a path that is used."
    };

    static const openspace::properties::Property::PropertyInfo FontSizeInfo = {
        "FontSize",
        "Font Size",
        "This value determines the size of the font that is used to render the date."
    };

    static const openspace::properties::Property::PropertyInfo SimplificationInfo = {
        "Simplification",
        "Time Simplification",
        "If this value is enabled, the time is displayed in nuanced units, such as "
        "minutes, hours, days, years, etc. If this value is disabled, it is always "
        "displayed in seconds."
    };
} // namespace

namespace openspace {

documentation::Documentation DashboardItemSimulationIncrement::Documentation() {
    using namespace documentation;
    return {
        "DashboardItem Simulation Increment",
        "base_dashboarditem_simulationincrement",
        {
            {
                "Type",
                new StringEqualVerifier("DashboardItemSimulationIncrement"),
                Optional::No
            },
            {
                FontNameInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                FontNameInfo.description
            },
            {
                FontSizeInfo.identifier,
                new IntVerifier,
                Optional::Yes,
                FontSizeInfo.description
            },
            {
                SimplificationInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                SimplificationInfo.description
            }
        }
    };
}

DashboardItemSimulationIncrement::DashboardItemSimulationIncrement(
                                                             ghoul::Dictionary dictionary)
    : DashboardItem("Simulation Increment")
    , _fontName(FontNameInfo, KeyFontMono)
    , _fontSize(FontSizeInfo, DefaultFontSize, 6.f, 144.f, 1.f)
    , _doSimplification(SimplificationInfo, true)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "DashboardItemDate"
    );

    if (dictionary.hasKey(FontNameInfo.identifier)) {
        _fontName = dictionary.value<std::string>(FontNameInfo.identifier);
    }
    _fontName.onChange([this](){
        _font = OsEng.fontManager().font(_fontName, _fontSize);
    });
    addProperty(_fontName);

    if (dictionary.hasKey(FontSizeInfo.identifier)) {
        _fontSize = static_cast<float>(
            dictionary.value<double>(FontSizeInfo.identifier)
        );
    }
    _fontSize.onChange([this](){
        _font = OsEng.fontManager().font(_fontName, _fontSize);
    });
    addProperty(_fontSize);

    if (dictionary.hasKey(SimplificationInfo.identifier)) {
        _doSimplification = dictionary.value<bool>(SimplificationInfo.identifier);
    }
    addProperty(_doSimplification);

    _font = OsEng.fontManager().font(_fontName, _fontSize);
}

void DashboardItemSimulationIncrement::render(glm::vec2& penPosition) {
    double t = OsEng.timeManager().time().deltaTime();
    std::pair<double, std::string> deltaTime =
        _doSimplification.value() ?
        simplifyTime(t) :
        std::make_pair(t, t == 1.0 ? std::string("second") : std::string("seconds"));

    penPosition.y -= _font->height();
    RenderFont(
        *_font,
        penPosition,
        "Simulation increment: %.1f %s / second",
        deltaTime.first,
        deltaTime.second.c_str()
    );
}

glm::vec2 DashboardItemSimulationIncrement::size() const {
    double t = OsEng.timeManager().time().deltaTime();
    std::pair<double, std::string> deltaTime =
        _doSimplification.value() ?
        simplifyTime(t) :
        std::make_pair(t, t == 1.0 ? std::string("seconds") : std::string("second"));

    return ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
        *_font,
        "Simulation increment: %.1f %s / second",
        deltaTime.first,
        deltaTime.second.c_str()
    ).boundingBox;
}

} // namespace openspace
