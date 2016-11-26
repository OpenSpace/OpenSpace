/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <modules/base/rotation/spicerotation.h>

#include <openspace/documentation/verifier.h>

#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>

namespace {
    const std::string _loggerCat = "SpiceRotation";
    //const std::string keyGhosting = "EphmerisGhosting";

    const char* KeySourceFrame = "SourceFrame";
    const char* KeyDestinationFrame = "DestinationFrame";
    const char* KeyKernels = "Kernels";
}

namespace openspace {
    
Documentation SpiceRotation::Documentation() {
    using namespace openspace::documentation;
    return {
        "Spice Rotation",
        "base_transform_rotation_spice",
        {
            {
                "Type",
                new StringEqualVerifier("SpiceRotation"),
                "",
                Optional::No
            },
            {
                KeySourceFrame,
                new StringAnnotationVerifier("A valid SPICE NAIF name or integer"),
                "The source frame that is used as the basis for the coordinate "
                "transformation. This has to be a valid SPICE name.",
                Optional::No
            },
            {
                KeyDestinationFrame,
                new StringAnnotationVerifier("A valid SPICE NAIF name or integer"),
                "The destination frame that is used for the coordinate transformation. "
                "This has to be a valid SPICE name.",
                Optional::No
            },
            {
                KeyKernels,
                new OrVerifier(
                    new TableVerifier({
                        { "*", new StringVerifier }
                    }),
                    new StringVerifier
                ),
                "A single kernel or list of kernels that this SpiceTranslation depends "
                "on. All provided kernels will be loaded before any other operation is "
                "performed.",
                Optional::Yes
            }
        }
    };
}

SpiceRotation::SpiceRotation(const ghoul::Dictionary& dictionary)
    : _sourceFrame("source", "Source", "")
    , _destinationFrame("destination", "Destination", "")
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "SpiceRotation"
    );

    _sourceFrame = dictionary.value<std::string>(KeySourceFrame);
    _destinationFrame = dictionary.value<std::string>(KeyDestinationFrame);

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
}
    
void SpiceRotation::update(const UpdateData& data) {
    _matrix = SpiceManager::ref().positionTransformMatrix(
        _sourceFrame,
        _destinationFrame,
        data.time
    );
}

} // namespace openspace
