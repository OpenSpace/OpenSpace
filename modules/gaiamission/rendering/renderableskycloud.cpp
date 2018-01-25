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

#include <modules/gaiamission/rendering/renderableskycloud.h>

#include <ghoul/io/texture/texturereader.h>

#include <openspace/rendering/renderable.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>

#include <ghoul/glm.h>
#include <glm/gtc/matrix_transform.hpp>
#include <ghoul/opengl/ghoul_gl.h>

#include <modules/volume/rawvolumereader.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

#include <ghoul/opengl/programobject.h>

#include <fstream>


namespace {
    const std::string GlslRayCastPath  = "${MODULES}/toyvolume/shaders/rayCast.glsl";
    const std::string GlslBoundsVsPath = "${MODULES}/toyvolume/shaders/boundsVs.glsl";
    const std::string GlslBoundsFsPath = "${MODULES}/toyvolume/shaders/boundsFs.glsl";
    const std::string _loggerCat       = "Renderable Galaxy";

    static const openspace::properties::Property::PropertyInfo StepSizeInfo = {
        "StepSize",
        "Step Size",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo PointStepSizeInfo = {
        "PointStepSize",
        "Point Step Size",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo TranslationInfo = {
        "Translation",
        "Translation",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo RotationInfo = {
        "Rotation",
        "Euler rotation",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo EnabledPointsRatioInfo = {
        "NEnabledPointsRatio",
        "Enabled points",
        "" // @TODO Missing documentation
    };
} // namespace

namespace openspace {

    RenderableSkyCloud::RenderableSkyCloud(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _stepSize(StepSizeInfo, 0.012f, 0.0005f, 0.05f)
    , _pointStepSize(PointStepSizeInfo, 0.01f, 0.01f, 0.1f)
    , _translation(TranslationInfo, glm::vec3(0.f), glm::vec3(0.f), glm::vec3(10.f))
    , _rotation(RotationInfo, glm::vec3(0.f), glm::vec3(0.f), glm::vec3(6.28f))
    , _enabledPointsRatio(EnabledPointsRatioInfo, 0.2f, 0.f, 1.f)
{
    float stepSize;
    glm::vec3 scaling, translation, rotation;
    glm::vec4 color;
    ghoul::Dictionary volumeDictionary, pointsDictionary;

    if (dictionary.getValue("Translation", translation)) {
        _translation = translation;
    }
    if (dictionary.getValue("Rotation", rotation)) {
        _rotation = rotation;
    }
    if (dictionary.getValue("StepSize", stepSize)) {
        _stepSize = stepSize;
    }
    if (dictionary.getValue("Volume", volumeDictionary)) {
        std::string volumeFilename;
        if (volumeDictionary.getValue("Filename", volumeFilename)) {
            _volumeFilename = absPath(volumeFilename);
        } else {
            LERROR("No volume filename specified.");
        }
        glm::vec3 volumeDimensions;
        if (volumeDictionary.getValue("Dimensions", volumeDimensions)) {
            _volumeDimensions = static_cast<glm::ivec3>(volumeDimensions);
        } else {
            LERROR("No volume dimensions specified.");
        }
        glm::vec3 volumeSize;
        if (volumeDictionary.getValue("Size", volumeSize)) {
            _volumeSize = static_cast<glm::vec3>(volumeSize);
        }
        else {
            LERROR("No volume dimensions specified.");
        }

    } else {
        LERROR("No volume dictionary specified.");
    }
    if (dictionary.getValue("Points", pointsDictionary)) {
        std::string pointsFilename;
        if (pointsDictionary.getValue("Filename", pointsFilename)) {
            _pointsFilename = absPath(pointsFilename);
        } else {
            LERROR("No points filename specified.");
        }
        glm::vec3 pointsScaling;
        if (pointsDictionary.getValue("Scaling", pointsScaling)) {
            _pointScaling = static_cast<glm::vec3>(pointsScaling);
        }
        else {
            LERROR("No volume dimensions specified.");
        }
    } else {
        LERROR("No points dictionary specified.");
    }
}

RenderableSkyCloud::~RenderableSkyCloud() {}

void RenderableSkyCloud::initializeGL() {

}

void RenderableSkyCloud::deinitializeGL() {

}

bool RenderableSkyCloud::isReady() const {
    return true;
}

void RenderableSkyCloud::update(const UpdateData& data) {

}

void RenderableSkyCloud::render(const RenderData& data, RendererTasks& tasks) {

}

float RenderableSkyCloud::safeLength(const glm::vec3& vector) {
    return 1.0;
}

} // namespace openspace
