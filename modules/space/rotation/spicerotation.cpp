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

#include <modules/space/rotation/spicerotation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>

#include <iostream>
#include <string>

namespace {
    const char* KeyKernels = "Kernels";

    static const openspace::properties::Property::PropertyInfo SourceInfo = {
        "SourceFrame",
        "Source",
        "This value specifies the source frame that is used as the basis for the "
        "coordinate transformation. This has to be a valid SPICE name."
    };

    static const openspace::properties::Property::PropertyInfo DestinationInfo = {
        "DestinationFrame",
        "Destination",
        "This value specifies the destination frame that is used for the coordinate "
        "transformation. This has to be a valid SPICE name."
    };
    static const openspace::properties::Property::PropertyInfo InstrumentCoordinateSystemInfo = {
        "DestinationFrame",
        "Destination",
        "This value specifies the destination frame that is used for the coordinate "
        "transformation. This has to be a valid SPICE name."
    };
} // namespace

namespace openspace {

constexpr const char* _loggerCat = "ghoul.opengl.FramebufferObject";

documentation::Documentation SpiceRotation::Documentation() {
    using namespace openspace::documentation;
    return {
        "Spice Rotation",
        "space_transform_rotation_spice",
        {
            {
                "Type",
                new StringEqualVerifier("SpiceRotation"),
                Optional::No
            },
            {
                SourceInfo.identifier,
                new StringAnnotationVerifier("A valid SPICE NAIF name or integer"),
                Optional::No,
                SourceInfo.description
            },
            {
                DestinationInfo.identifier,
                new StringAnnotationVerifier("A valid SPICE NAIF name or integer"),
                Optional::No,
                DestinationInfo.description
            },
            /*{
                InstrumentCoordinateSystemInfo.identifier,
                new StringAnnotationVerifier("COORDINATE SYSTEM"),
                Optional::No,
                InstrumentCoordinateSystemInfo.description
            },*/
            {
                KeyKernels,
                new OrVerifier(
                    new StringListVerifier,
                    new StringVerifier
                ),
                Optional::Yes,
                "A single kernel or list of kernels that this SpiceTranslation depends "
                "on. All provided kernels will be loaded before any other operation is "
                "performed."
            }
        }
    };
}

SpiceRotation::SpiceRotation(const ghoul::Dictionary& dictionary)
    : _sourceFrame(SourceInfo) // @TODO Missing documentation
    , _destinationFrame(DestinationInfo)
    , _InstrumentCoordinateSystem(InstrumentCoordinateSystemInfo)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "SpiceRotation"
    );

    _sourceFrame = dictionary.value<std::string>(SourceInfo.identifier);
    _destinationFrame = dictionary.value<std::string>(DestinationInfo.identifier);

    _InstrumentCoordinateSystem = dictionary.value<std::string>(InstrumentCoordinateSystemInfo.identifier);

    if (dictionary.hasKeyAndValue<std::string>(KeyKernels)) {
        SpiceManager::ref().loadKernel(dictionary.value<std::string>(KeyKernels));
    }
    else if (dictionary.hasKeyAndValue<ghoul::Dictionary>(KeyKernels)) {
        ghoul::Dictionary kernels = dictionary.value<ghoul::Dictionary>(KeyKernels);
        for (size_t i = 1; i <= kernels.size(); ++i) {
            if (!kernels.hasKeyAndValue<std::string>(std::to_string(i))) {
                throw ghoul::RuntimeError("Kernels has to be an array-style table");
            }

            std::string kernel = kernels.value<std::string>(std::to_string(i));
            SpiceManager::ref().loadKernel(kernel);
        }
    }

    addProperty(_sourceFrame);
    addProperty(_destinationFrame);
    addProperty(_InstrumentCoordinateSystem);

    auto update = [this]() {
        requireUpdate();
    };

    _sourceFrame.onChange(update);
    _destinationFrame.onChange(update);
    _InstrumentCoordinateSystem.onChange(update);
}

glm::dmat3 SpiceRotation::matrix(const Time& time) const {
    //LERROR(fmt::format("SpiceRotation: '{}' ", time.j2000Seconds()));
    try {
        //multiplicate _sourceframe and _destinationframe with the right rotation matrix before sending it to spicemanager!!!!!!!!
        glm::dmat3 result;

        result = SpiceManager::ref().positionTransformMatrix(
            _sourceFrame,
            _destinationFrame,
            time.j2000Seconds()
        );
        return result;

    }
    catch (const SpiceManager::SpiceException&) {
        return glm::dmat3(1.0);
    }
}

} // namespace openspace
