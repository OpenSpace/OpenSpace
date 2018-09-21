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

#include <modules/space/translation/spicetranslation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* KeyKernels = "Kernels";

    constexpr const char* DefaultReferenceFrame = "GALACTIC";

    constexpr openspace::properties::Property::PropertyInfo TargetInfo = {
        "Target",
        "Target",
        "This is the SPICE NAIF name for the body whose translation is to be computed by "
        "the SpiceTranslation. It can either be a fully qualified name (such as 'EARTH') "
        "or a NAIF integer id code (such as '399')."
    };

    constexpr openspace::properties::Property::PropertyInfo ObserverInfo = {
        "Observer",
        "Observer",
        "This is the SPICE NAIF name for the parent of the body whose translation is to "
        "be computed by the SpiceTranslation. It can either be a fully qualified name "
        "(such as 'SOLAR SYSTEM BARYCENTER') or a NAIF integer id code (such as '0')."
    };

    constexpr openspace::properties::Property::PropertyInfo FrameInfo = {
        "Frame",
        "Reference Frame",
        "This is the SPICE NAIF name for the reference frame in which the position "
        "should be retrieved. The default value is GALACTIC."
    };
} // namespace

namespace openspace {

documentation::Documentation SpiceTranslation::Documentation() {
    using namespace openspace::documentation;

    return {
        "Spice Translation",
        "space_translation_spicetranslation",
        {
            {
                "Type",
                new StringEqualVerifier("SpiceTranslation"),
                Optional::No
            },
            {
                TargetInfo.identifier,
                new StringAnnotationVerifier("A valid SPICE NAIF name or identifier"),
                Optional::No,
                "This is the SPICE NAIF name for the body whose translation is to be "
                "computed by the SpiceTranslation. It can either be a fully qualified "
                "name (such as 'EARTH') or a NAIF integer id code (such as '399')."
            },
            {
                ObserverInfo.identifier,
                new StringAnnotationVerifier("A valid SPICE NAIF name or identifier"),
                Optional::No,
                ObserverInfo.description
            },
            {
                FrameInfo.identifier,
                new StringAnnotationVerifier(
                    "A valid SPICE NAIF name for a reference frame"
                ),
                Optional::Yes,
                FrameInfo.description
            },
            {
                KeyKernels,
                new OrVerifier({ new StringListVerifier, new StringVerifier }),
                Optional::Yes,
                "A single kernel or list of kernels that this SpiceTranslation depends "
                "on. All provided kernels will be loaded before any other operation is "
                "performed."
            }
        }
    };
}

SpiceTranslation::SpiceTranslation(const ghoul::Dictionary& dictionary)
    : _target(TargetInfo)
    , _observer(ObserverInfo)
    , _frame(FrameInfo, DefaultReferenceFrame)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "SpiceTranslation"
    );

    _target = dictionary.value<std::string>(TargetInfo.identifier);
    _observer = dictionary.value<std::string>(ObserverInfo.identifier);

    if (dictionary.hasKey(FrameInfo.identifier)) {
        _frame = dictionary.value<std::string>(FrameInfo.identifier);
    }

    auto loadKernel = [](const std::string& kernel) {
        if (!FileSys.fileExists(kernel)) {
            throw SpiceManager::SpiceException("Kernel '" + kernel + "' does not exist");
        }

        try {
            SpiceManager::ref().loadKernel(kernel);
        }
        catch (const SpiceManager::SpiceException& exception) {
            LERRORC("SpiceEphemeris", exception.message);
        }
    };

    if (dictionary.hasKey(KeyKernels)) {
        // Due to the specification, we can be sure it is either a Dictionary or a string
        if (dictionary.hasValue<std::string>(KeyKernels)) {
            std::string kernel = dictionary.value<std::string>(KeyKernels);
            loadKernel(absPath(kernel));
        }
        else {
            ghoul::Dictionary kernels = dictionary.value<ghoul::Dictionary>(KeyKernels);
            for (size_t i = 1; i <= kernels.size(); ++i) {
                std::string kernel = kernels.value<std::string>(std::to_string(i));
                loadKernel(absPath(kernel));
            }
        }
    }

    auto update = [this](){
        requireUpdate();
        notifyObservers();
    };

    _target.onChange(update);
    addProperty(_target);

    _observer.onChange(update);
    addProperty(_observer);

    _frame.onChange(update);
    addProperty(_frame);
}

glm::dvec3 SpiceTranslation::position(const UpdateData& data) const {
    double lightTime = 0.0;
    return SpiceManager::ref().targetPosition(
        _target,
        _observer,
        _frame,
        {},
        data.time.j2000Seconds(),
        lightTime
    ) * glm::pow(10.0, 3.0);
}

} // namespace openspace
