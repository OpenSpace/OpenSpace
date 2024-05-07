/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <filesystem>
#include <optional>
#include <variant>

namespace {
    constexpr openspace::properties::Property::PropertyInfo TargetInfo = {
        "Target",
        "Target",
        "This is the SPICE NAIF name for the body whose translation is to be computed by "
        "the SpiceTranslation. It can either be a fully qualified name (such as 'EARTH') "
        "or a NAIF integer id code (such as '399').",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ObserverInfo = {
        "Observer",
        "Observer",
        "This is the SPICE NAIF name for the parent of the body whose translation is to "
        "be computed by the SpiceTranslation. It can either be a fully qualified name "
        "(such as 'SOLAR SYSTEM BARYCENTER') or a NAIF integer id code (such as '0').",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FrameInfo = {
        "Frame",
        "Reference Frame",
        "This is the SPICE NAIF name for the reference frame in which the position "
        "should be retrieved. The default value is GALACTIC.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FixedDateInfo = {
        "FixedDate",
        "Fixed Date",
        "A time to lock the position to. Setting this to an empty string will "
        "unlock the time and return to position based on current simulation time.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(SpiceTranslation)]] Parameters {
        // [[codegen::verbatim(TargetInfo.description)]]
        std::variant<std::string, int> target;

        // [[codegen::verbatim(ObserverInfo.description)]]
        std::variant<std::string, int> observer;

        std::optional<std::string> frame
            [[codegen::annotation("A valid SPICE NAIF name for a reference frame")]];

        std::optional<std::string> fixedDate
            [[codegen::annotation("A date to lock the position to")]];
    };
#include "spicetranslation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation SpiceTranslation::Documentation() {
    return codegen::doc<Parameters>("space_translation_spicetranslation");
}

SpiceTranslation::SpiceTranslation(const ghoul::Dictionary& dictionary)
    : _target(TargetInfo)
    , _observer(ObserverInfo)
    , _frame(FrameInfo, "GALACTIC")
    , _fixedDate(FixedDateInfo)
    , _cachedFrame("GALACTIC")
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

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

    _fixedDate.onChange([this]() {
        if (_fixedDate.value().empty()) {
            _fixedEphemerisTime = std::nullopt;
        }
        else {
            _fixedEphemerisTime = SpiceManager::ref().ephemerisTimeFromDate(_fixedDate);
        }
    });
    _fixedDate = p.fixedDate.value_or(_fixedDate);
    addProperty(_fixedDate);

    if (std::holds_alternative<std::string>(p.target)) {
        _target = std::get<std::string>(p.target);
    }
    else {
        _target = std::to_string(std::get<int>(p.target));
    }

    if (std::holds_alternative<std::string>(p.observer)) {
        _observer = std::get<std::string>(p.observer);
    }
    else {
        _observer = std::to_string(std::get<int>(p.observer));
    }

    _frame = p.frame.value_or(_frame);
}

glm::dvec3 SpiceTranslation::position(const UpdateData& data) const {
    double lightTime = 0.0;

    double time = data.time.j2000Seconds();
    if (_fixedEphemerisTime.has_value()) {
        time = *_fixedEphemerisTime;
    }
    return SpiceManager::ref().targetPosition(
        _cachedTarget,
        _cachedObserver,
        _cachedFrame,
        {},
        time,
        lightTime
    ) * 1000.0;
}

} // namespace openspace
