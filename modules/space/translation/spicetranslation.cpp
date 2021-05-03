/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <ghoul/misc/profiling.h>
#include <optional>

namespace {
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

    struct [[codegen::Dictionary(SpiceTranslation)]] Parameters {
        // [[codegen::verbatim(TargetInfo.description)]]
        std::string target
            [[codegen::annotation("A valid SPICE NAIF name or identifier")]];

        // [[codegen::verbatim(ObserverInfo.description)]]
        std::string observer
            [[codegen::annotation("A valid SPICE NAIF name or identifier")]];

        std::optional<std::string> frame
            [[codegen::annotation("A valid SPICE NAIF name for a reference frame")]];

        // A single kernel or list of kernels that this SpiceTranslation depends on. All
        // provided kernels will be loaded before any other operation is performed
        std::optional<std::variant<std::vector<std::string>, std::string>> kernels;
    };
#include "spicetranslation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation SpiceTranslation::Documentation() {
    documentation::Documentation doc = codegen::doc<Parameters>();
    doc.id = "space_translation_spicetranslation";
    return doc;
}

SpiceTranslation::SpiceTranslation(const ghoul::Dictionary& dictionary)
    : _target(TargetInfo)
    , _observer(ObserverInfo)
    , _frame(FrameInfo, DefaultReferenceFrame)
    , _cachedFrame(DefaultReferenceFrame)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    auto loadKernel = [](const std::string& kernel) {
        if (!FileSys.fileExists(kernel)) {
            throw SpiceManager::SpiceException(fmt::format(
                "Kernel '{}' does not exist", kernel
            ));
        }

        try {
            SpiceManager::ref().loadKernel(kernel);
        }
        catch (const SpiceManager::SpiceException& exception) {
            LERRORC("SpiceEphemeris", exception.message);
        }
    };

    if (p.kernels.has_value()) {
        if (std::holds_alternative<std::string>(*p.kernels)) {
            loadKernel(absPath(std::get<std::string>(*p.kernels)));
        }
        else {
            for (const std::string& k : std::get<std::vector<std::string>>(*p.kernels)) {
                loadKernel(absPath(k));
            }
        }
    }

    _target.onChange([this]() {
        _cachedTarget = _target;
        requireUpdate();
        notifyObservers();
    });
    addProperty(_target);

    _observer.onChange([this]() {
        _cachedObserver = _observer;
        requireUpdate();
        notifyObservers();
    });
    addProperty(_observer);

    _frame.onChange([this]() {
        _cachedFrame = _frame;
        requireUpdate();
        notifyObservers();
    });
    addProperty(_frame);

    _target = p.target;
    _observer = p.observer;
    _frame = p.frame.value_or(_frame);
}

glm::dvec3 SpiceTranslation::position(const UpdateData& data) const {
    double lightTime = 0.0;
    return SpiceManager::ref().targetPosition(
        _cachedTarget,
        _cachedObserver,
        _cachedFrame,
        {},
        data.time.j2000Seconds(),
        lightTime
    ) * 1000.0;
}

} // namespace openspace
