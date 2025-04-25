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

#include <modules/base/dashboard/dashboarditemcameraorientation.h>

#include <openspace/camera/camera.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <ghoul/font/font.h>

namespace {
    // This `DashboardItem` shows the current camera orientation in the yaw, pitch, and
    // roll directions in degrees. Note that the camera's orientation is relative to the
    // global coordinate system used in the system.
    struct [[codegen::Dictionary(DashboardItemCameraOrientation)]] Parameters {
    };
#include "dashboarditemcameraorientation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation DashboardItemCameraOrientation::Documentation() {
    return codegen::doc<Parameters>(
        "base_dashboarditem_cameraorientation",
        DashboardTextItem::Documentation()
    );
}

DashboardItemCameraOrientation::DashboardItemCameraOrientation(
                                                      const ghoul::Dictionary& dictionary)
    : DashboardTextItem(dictionary)
{}

void DashboardItemCameraOrientation::update() {
    ZoneScoped;

    const Camera* camera = global::renderEngine->scene()->camera();
    const glm::dquat orientation = camera->rotationQuaternion();
    const glm::dvec3 pitchYawRoll = glm::eulerAngles(orientation);
    const glm::dvec3 pitchYawRollDeg = glm::degrees(pitchYawRoll);

    _buffer = std::format(
        "Yaw: {:.2f}\nPitch: {:.2f}\nRoll: {:.2f}",
        pitchYawRollDeg.y, pitchYawRollDeg.x, pitchYawRollDeg.z
    );
}

} // namespace openspace
