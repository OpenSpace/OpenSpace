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

#include <modules/base/dashboard/dashboarditemdistance.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/camera.h>
#include <openspace/util/distanceconversion.h>

#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

namespace {
    const char* KeyFontMono = "Mono";
} // namespace

namespace openspace {

DashboardItemDistance::DashboardItemDistance(ghoul::Dictionary dictionary)
    : DashboardItem("Distance")
    , _font(OsEng.fontManager().font(KeyFontMono, 10))
{

}

void DashboardItemDistance::render(glm::vec2& penPosition) {
    double distance = glm::length(
        OsEng.renderEngine().camera()->positionVec3() -
        OsEng.navigationHandler().focusNode()->worldPosition()
    );
    std::pair<double, std::string> dist = simplifyDistance(distance);
    RenderFontCr(
        *_font,
        penPosition,
        "Distance from focus: %f %s",
        dist.first,
        dist.second.c_str()
    );
}

glm::vec2 DashboardItemDistance::size() const {
    double distance = glm::length(
        OsEng.renderEngine().camera()->positionVec3() -
        OsEng.navigationHandler().focusNode()->worldPosition()
    );
    std::pair<double, std::string> dist = simplifyDistance(distance);

    return ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
        *_font,
        "Distance from focus: %f %s",
        dist.first,
        dist.second.c_str()
    ).boundingBox;
}

} // namespace openspace
