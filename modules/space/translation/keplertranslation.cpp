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

#include <modules/space/translation/keplertranslation.h>

#include <openspace/documentation/verifier.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/transform.hpp>

namespace {
    template <typename T, typename Func>
    T solveIteration(const Func& function, T x0, const T& err = 0.0, int maxIter = 100) {
        T x2 = x0;

        for (int i = 0; i < maxIter; i++) {
            T x = x2;
            x2 = function(x);
            if (std::abs(x2 - x) < err) {
                return x2;
            }
        }

        return x2;
    }

    constexpr openspace::properties::Property::PropertyInfo EccentricityInfo = {
        "Eccentricity",
        "Eccentricity",
        "This value determines the eccentricity, that is the deviation from a perfect "
        "sphere, for this orbit. Currently, hyperbolic orbits using Keplerian elements "
        "are not supported.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SemiMajorAxisInfo = {
        "SemiMajorAxis",
        "Semi-major axis",
        "This value determines the semi-major axis, that is the distance of the object "
        "from the central body in kilometers (semi-major axis = average of periapsis and "
        "apoapsis).",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo InclinationInfo = {
        "Inclination",
        "Inclination",
        "This value determines the degrees of inclination, or the angle of the orbital "
        "plane, relative to the reference plane, on which the object orbits around the "
        "central body.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AscendingNodeInfo = {
        "AscendingNode",
        "Right ascension of ascending Node",
        "This value determines the right ascension of the ascending node in degrees, "
        "that is the location of position along the orbit where the inclined plane and "
        "the horizonal reference plane intersect.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ArgumentOfPeriapsisInfo = {
        "ArgumentOfPeriapsis",
        "Argument of Periapsis",
        "This value determines the argument of periapsis in degrees, that is the "
        "position on the orbit that is closest to the orbiting body.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MeanAnomalyAtEpochInfo = {
        "MeanAnomaly",
        "Mean anomaly at epoch",
        "This value determines the mean anomaly at the epoch in degrees, which "
        "determines the initial location of the object along the orbit at epoch.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo EpochInfo = {
        "Epoch",
        "Epoch",
        "This value determines the epoch for which the initial location is defined in "
        "the form of YYYY MM DD HH:mm:ss.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo PeriodInfo = {
        "Period",
        "Orbit period",
        "Specifies the orbital period (in seconds).",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(KeplerTranslation)]] Parameters {
        // [[codegen::verbatim(EccentricityInfo.description)]]
        double eccentricity [[codegen::inrange(0.0, 1.0)]];

        // [[codegen::verbatim(SemiMajorAxisInfo.description)]]
        double semiMajorAxis;

        // [[codegen::verbatim(InclinationInfo.description)]]
        double inclination [[codegen::inrange(-360.0, 360.0)]];

        // [[codegen::verbatim(AscendingNodeInfo.description)]]
        double ascendingNode [[codegen::inrange(-360.0, 360.0)]];

        // [[codegen::verbatim(ArgumentOfPeriapsisInfo.description)]]
        double argumentOfPeriapsis [[codegen::inrange(-360.0, 360.0)]];

        // [[codegen::verbatim(MeanAnomalyAtEpochInfo.description)]]
        double meanAnomaly [[codegen::inrange(-360.0, 360.0)]];

        // [[codegen::verbatim(EpochInfo.description)]]
        std::string epoch;

        // [[codegen::verbatim(PeriodInfo.description)]]
        double period [[codegen::greater(0.0)]];
    };
#include "keplertranslation_codegen.cpp"
} // namespace

namespace openspace {

KeplerTranslation::RangeError::RangeError(std::string off)
    : ghoul::RuntimeError("Value '" + off + "' out of range", "KeplerTranslation")
    , offender(std::move(off))
{}

documentation::Documentation KeplerTranslation::Documentation() {
    return codegen::doc<Parameters>("space_transform_kepler");
}

KeplerTranslation::KeplerTranslation()
    : _eccentricity(EccentricityInfo, 0.0, 0.0, 1.0)
    , _semiMajorAxis(SemiMajorAxisInfo, 0.0, 0.0, 1e6)
    , _inclination(InclinationInfo, 0.0, 0.0, 360.0)
    , _ascendingNode(AscendingNodeInfo, 0.0, 0.0, 360.0)
    , _argumentOfPeriapsis(ArgumentOfPeriapsisInfo, 0.0, 0.0, 360.0)
    , _meanAnomalyAtEpoch(MeanAnomalyAtEpochInfo, 0.0, 0.0, 360.0)
    , _epoch(EpochInfo, 0.0, 0.0, 1e9)
    , _period(PeriodInfo, 0.0, 0.0, 1e6)
{
    auto update = [this]() {
        _orbitPlaneDirty = true;
        requireUpdate();
    };

    // Only the eccentricity, semimajor axis, inclination, and location of ascending node
    // invalidate the shape of the orbit. The other parameters only determine the location
    // the spacecraft on that orbit
    _eccentricity.onChange(update);
    addProperty(_eccentricity);

    _semiMajorAxis.onChange(update);
    addProperty(_semiMajorAxis);

    _inclination.onChange(update);
    addProperty(_inclination);

    _ascendingNode.onChange(update);
    addProperty(_ascendingNode);

    _argumentOfPeriapsis.onChange(update);
    addProperty(_argumentOfPeriapsis);

    addProperty(_meanAnomalyAtEpoch);
    addProperty(_epoch);
    addProperty(_period);
}

KeplerTranslation::KeplerTranslation(const ghoul::Dictionary& dictionary)
    : KeplerTranslation()
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    setKeplerElements(
        p.eccentricity,
        p.semiMajorAxis,
        p.inclination < 0.0 ? p.inclination + 360.0 : p.inclination,
        p.ascendingNode < 0.0 ? p.ascendingNode + 360.0 : p.ascendingNode,
        p.argumentOfPeriapsis < 0.0 ?
            p.argumentOfPeriapsis + 360.0 :
            p.argumentOfPeriapsis,
        p.meanAnomaly < 0.0 ? p.meanAnomaly + 360.0 : p.meanAnomaly,
        p.period,
        p.epoch
    );
}

double KeplerTranslation::eccentricAnomaly(double meanAnomaly) const {
    // Compute the eccentric anomaly (the location of the spacecraft taking the
    // eccentricity of the orbit into account) using different solves for the regimes in
    // which they are most efficient

    if (_eccentricity == 0.0) {
        // In a circular orbit, the eccentric anomaly = mean anomaly
        return meanAnomaly;
    }
    else if (_eccentricity < 0.2) {
        auto solver = [this, &meanAnomaly](double x) -> double {
            // For low eccentricity, using a first order solver sufficient
            return meanAnomaly + _eccentricity * sin(x);
        };
        return solveIteration(solver, meanAnomaly, 0.0, 5);
    }
    else if (_eccentricity < 0.9) {
        auto solver = [this, &meanAnomaly](double x) -> double {
            const double e = _eccentricity;
            return x + (meanAnomaly + e * sin(x) - x) / (1.0 - e * cos(x));
        };
        return solveIteration(solver, meanAnomaly, 0.0, 6);
    }
    else if (_eccentricity < 1.0) {
        auto sign = [](double val) -> double {
            return val > 0.0 ? 1.0 : ((val < 0.0) ? -1.0 : 0.0);
        };
        const double e = meanAnomaly + 0.85 * _eccentricity * sign(sin(meanAnomaly));

        auto solver = [this, &meanAnomaly, &sign](double x) -> double {
            const double s = _eccentricity * sin(x);
            const double c = _eccentricity * cos(x);
            const double f = x - s - meanAnomaly;
            const double f1 = 1 - c;
            const double f2 = s;
            return x + (-5 * f / (f1 + sign(f1) *
                sqrt(std::abs(16 * f1 * f1 - 20 * f * f2))));
        };
        return solveIteration(solver, e, 0.0, 8);
    }
    else {
        ghoul_assert(false, "Eccentricity must not be >= 1.0");
        LERRORC("KeplerTranslation", "Eccentricity must not be >= 1.0");
        return 0.0;
    }
}

glm::dvec3 KeplerTranslation::position(const UpdateData& data) const {
    if (_orbitPlaneDirty) {
        computeOrbitPlane();
        _orbitPlaneDirty = false;
    }

    const double t = data.time.j2000Seconds() - _epoch;
    const double meanMotion = glm::two_pi<double>() / _period;
    const double meanAnomaly = glm::radians(_meanAnomalyAtEpoch.value()) + t * meanMotion;
    const double e = eccentricAnomaly(meanAnomaly);

    // Use the eccentric anomaly to compute the actual location
    const glm::dvec3 p = glm::dvec3(
        _semiMajorAxis * 1000.0 * (cos(e) - _eccentricity),
        _semiMajorAxis * 1000.0 * sin(e) * sqrt(1.0 - _eccentricity * _eccentricity),
        0.0
    );
    return _orbitPlaneRotation * p;
}

void KeplerTranslation::computeOrbitPlane() const {
    // We assume the following coordinate system:
    // z = axis of rotation
    // x = pointing towards the first point of Aries
    // y completes the righthanded coordinate system

    // Perform three rotations:
    // 1. Around the z axis to place the location of the ascending node
    // 2. Around the x axis (now aligned with the ascending node) to get the correct
    // inclination
    // 3. Around the new z axis to place the closest approach to the correct location

    const glm::vec3 ascendingNodeAxisRot = glm::vec3(0.f, 0.f, 1.f);
    const glm::vec3 inclinationAxisRot = glm::vec3(1.f, 0.f, 0.f);
    const glm::vec3 argPeriapsisAxisRot = glm::vec3(0.f, 0.f, 1.f);

    const double asc = glm::radians(_ascendingNode.value());
    const double inc = glm::radians(_inclination.value());
    const double per = glm::radians(_argumentOfPeriapsis.value());

    _orbitPlaneRotation = glm::rotate(asc, glm::dvec3(ascendingNodeAxisRot)) *
                          glm::rotate(inc, glm::dvec3(inclinationAxisRot)) *
                          glm::rotate(per, glm::dvec3(argPeriapsisAxisRot));

    notifyObservers();
    _orbitPlaneDirty = false;
}

void KeplerTranslation::setKeplerElements(double eccentricity, double semiMajorAxis,
                                          double inclination, double ascendingNode,
                                          double argumentOfPeriapsis,
                                          double meanAnomalyAtEpoch, double orbitalPeriod,
                                          const std::string& epoch)
{
    setKeplerElements(
        eccentricity,
        semiMajorAxis,
        inclination,
        ascendingNode,
        argumentOfPeriapsis,
        meanAnomalyAtEpoch,
        orbitalPeriod,
        SpiceManager::ref().ephemerisTimeFromDate(epoch)
    );
}

void KeplerTranslation::setKeplerElements(double eccentricity, double semiMajorAxis,
                                          double inclination, double ascendingNode,
                                          double argumentOfPeriapsis,
                                          double meanAnomalyAtEpoch, double orbitalPeriod,
                                          double epoch)
{
    auto isInRange = [](double val, double min, double max) -> bool {
        return val >= min && val <= max;
    };
    if (isInRange(eccentricity, 0.0, 1.0)) {
        _eccentricity = eccentricity;
    }
    else {
        throw RangeError("Eccentricity");
    }

    _semiMajorAxis = semiMajorAxis;

    if (isInRange(inclination, 0.0, 360.0)) {
        _inclination = inclination;
    }
    else {
        throw RangeError("Inclination");
    }

    if (isInRange(_ascendingNode, 0.0, 360.0)) {
        _ascendingNode = ascendingNode;
    }
    else {
        throw RangeError("Ascending Node");
    }
    if (isInRange(_argumentOfPeriapsis, 0.0, 360.0)) {
        _argumentOfPeriapsis = argumentOfPeriapsis;
    }
    else {
        throw RangeError("Argument of Periapsis");
    }

    if (isInRange(_meanAnomalyAtEpoch, 0.0, 360.0)) {
        _meanAnomalyAtEpoch = meanAnomalyAtEpoch;
    }
    else {
        throw RangeError("Mean anomaly at epoch");
    }

    _period = orbitalPeriod;
    _epoch = epoch;

    computeOrbitPlane();
}

} // namespace openspace
