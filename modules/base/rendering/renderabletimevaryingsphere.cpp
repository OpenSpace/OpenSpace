/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/base/rendering/renderabletimevaryingsphere.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
// Why will it not compile without this library?
#include <openspace/util/powerscaledsphere.h>

namespace {
    constexpr const char* _loggerCat = "RenderableTimeVaryingSphere";
    constexpr const char* ProgramName = "TimeVaryingSphere";

    constexpr openspace::properties::Property::PropertyInfo DefaultTextureInfo = {
        "DefaultTexture",
        "Default Texture",
        "This value specifies an image that is loaded from disk and is used as a default "
        "texture that is applied to this sphere, when online textures has not yet loaded,"
        " or data is missing. This image is expected to be an equirectangular projection."
    };

    constexpr openspace::properties::Property::PropertyInfo MirrorTextureInfo = {
        "MirrorTexture",
        "Mirror Texture",
        "Mirror the texture along the x-axis."
    };

    constexpr openspace::properties::Property::PropertyInfo OrientationInfo = {
        "Orientation",
        "Orientation",
        "Specifies whether the texture is applied to the inside of the sphere, the "
        "outside of the sphere, or both."
    };

    constexpr openspace::properties::Property::PropertyInfo UseAdditiveBlendingInfo = {
        "UseAdditiveBlending",
        "Use Additive Blending",
        "Render the object using additive blending."
    };

    constexpr openspace::properties::Property::PropertyInfo SegmentsInfo = {
        "Segments",
        "Number of Segments",
        "This value specifies the number of segments that the sphere is separated in."
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size (in meters)",
        "This value specifies the radius of the sphere in meters."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeOutThresholdInfo = {
        "FadeOutThreshold",
        "Fade-Out Threshold",
        "This value determines percentage of the sphere is visible before starting "
        "fading-out it."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeInThresholdInfo = {
        "FadeInThreshold",
        "Fade-In Threshold",
        "Distance from center of MilkyWay from where the astronomical object starts to "
        "fade in."
    };

    constexpr openspace::properties::Property::PropertyInfo DisableFadeInOutInfo = {
        "DisableFadeInOut",
        "Disable Fade-In/Fade-Out effects",
        "Enables/Disables the Fade-In/Out effects."
    };

    constexpr openspace::properties::Property::PropertyInfo BackgroundInfo = {
        "Background",
        "Sets the current sphere rendering as a background rendering type",
        "Enables/Disables background rendering."
    };
} // namespace

namespace openspace {

documentation::Documentation RenderableTimeVaryingSphere::Documentation() {
    using namespace documentation;
    return {
        "RenderableTimeVaryingSphere",
        "base_renderable_timevarying_sphere",
        {
            {
                SizeInfo.identifier,
                new DoubleVerifier,
                Optional::No,
                SizeInfo.description
            },
            {
                SegmentsInfo.identifier,
                new IntVerifier,
                Optional::No,
                SegmentsInfo.description
            },
            {
                DefaultTextureInfo.identifier,
                new StringVerifier,
                Optional::No,
                DefaultTextureInfo.description
            },
            {
                OrientationInfo.identifier,
                new StringInListVerifier({ "Inside", "Outside", "Both" }),
                Optional::Yes,
                OrientationInfo.description
            },
            {
                UseAdditiveBlendingInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                UseAdditiveBlendingInfo.description
            },
            {
                MirrorTextureInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                MirrorTextureInfo.description
            },
            {
                FadeOutThresholdInfo.identifier,
                new DoubleInRangeVerifier(0.0, 1.0),
                Optional::Yes,
                FadeOutThresholdInfo.description
            },
            {
                FadeInThresholdInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                FadeInThresholdInfo.description
            },
            {
                DisableFadeInOutInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                DisableFadeInOutInfo.description
            },
            {
                BackgroundInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                BackgroundInfo.description
            },
        }
    };
}

void RenderableTimeVaryingSphere::initializeWebManager()
{
    size_t hej = 0;
    _webFieldlinesManager.initializeWebFieldlinesManager(
        _identifier, "https://iswa.gsfc.nasa.gov/IswaSystemWebApp/FilesInRangeServlet?dataID=1180", hej, _sourceFiles, _startTimes
    );
}


RenderableTimeVaryingSphere::RenderableTimeVaryingSphere
    (const ghoul::Dictionary& dictionary)
    // Does this have any consequences? Will there be an additional renderable in the scene?
    : Renderable(dictionary),
    _defaultTexturePath(DefaultTextureInfo)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableTimeVaryingSphere"
    );
    
    _defaultTexturePath = absPath(dictionary.value<std::string>(DefaultTextureInfo.identifier));
    ghoul::Dictionary renderableSphereDictionary(dictionary);
    renderableSphereDictionary.setValue<std::string>("Texture", _defaultTexturePath.value());
    renderableSphereDictionary.getValue("Identifier", _identifier);

    _renderableSphere = std::make_unique<RenderableSphere>(renderableSphereDictionary);
}

bool RenderableTimeVaryingSphere::isReady() const {
    return  _renderableSphere->isReady();
}

void RenderableTimeVaryingSphere::initializeGL() {
    
    _renderableSphere->initializeGL();
    _webFieldlinesManager.initializeSyncDirectory(_identifier);
    initializeWebManager();
}

void RenderableTimeVaryingSphere::deinitializeGL() {
    _renderableSphere->deinitializeGL();
}

void RenderableTimeVaryingSphere::render(const RenderData& data, RendererTasks& rendererTasks) {
    _renderableSphere->render(data, rendererTasks);
}

void RenderableTimeVaryingSphere::update(const UpdateData& updateData) {
    _webFieldlinesManager.update();
    _renderableSphere->update(updateData);
}

} // namespace openspace
