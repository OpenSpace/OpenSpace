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

#include <modules/space/translation/keplertranslation.h>

#include <openspace/documentation/verifier.h>
#include <openspace/util/spicemanager.h>

#include <openspace/util/updatestructures.h>

#include <glm/gtx/transform.hpp>

#include <math.h>

namespace {
    const char* KeyEccentricity = "Eccentricity";
    const char* KeySemiMajorAxis = "SemiMajorAxis";
    const char* KeyInclination = "Inclination";
    const char* KeyAscendingNode = "AscendingNode";
    const char* KeyArgumentOfPeriapsis = "ArgumentOfPeriapsis";
    const char* KeyMeanAnomaly = "MeanAnomaly";
    const char* KeyEpoch = "Epoch";
    const char* KeyPeriod = "Period";


template <typename T, typename Func>
T solveIteration(Func function, T x0, const T& err = 0.0, int maxIterations = 100) {
    T x = 0;
    T x2 = x0;

    for (int i = 0; i < maxIterations; ++i) {
        x = x2;
        x2 = function(x);
        if (std::abs(x2 - x) < err) {
            return x2;
        }
    }

    return x2;
}
} // namespace

namespace openspace {
    
KeplerTranslation::RangeError::RangeError(std::string offender)
    : ghoul::RuntimeError("Value '" + offender + "' out of range", "KeplerTranslation")
    , offender(std::move(offender))
{}

documentation::Documentation KeplerTranslation::Documentation() {
    using namespace openspace::documentation;
    return{
        "Kepler Translation",
        "space_transform_kepler",
        {
            {
                "Type",
                new StringEqualVerifier("KeplerTranslation"),
                "",
                Optional::No
            },
            {
                KeyEccentricity,
                new DoubleInRangeVerifier(0.0, 1.0),
                "Specifies the eccentricity of the orbit; currently, OpenSpace does not "
                "support hyperbolic orbits using Keplerian elements.",
                Optional::No
            },
            {
                KeySemiMajorAxis,
                new DoubleVerifier,
                "Specifies the semi-major axis of the orbit in kilometers (semi-major "
                "axis = average of periapsis and apoapsis).",
                Optional::No
            },
            {
                KeyInclination,
                new DoubleInRangeVerifier(0.0, 360.0),
                "Specifies the inclination angle (degrees) of the orbit relative to the "
                "reference plane (in the case of Earth, the equatorial plane.",
                Optional::No
            },
            {
                KeyAscendingNode,
                new DoubleInRangeVerifier(0.0, 360.0),
                "Specifies the right ascension of the ascending node (in degrees!) "
                "relative to the vernal equinox.",
                Optional::No
            },
            {
                KeyArgumentOfPeriapsis,
                new DoubleInRangeVerifier(0.0, 360.0),
                "Specifies the argument of periapsis as angle (in degrees) from the "
                "ascending.",
                Optional::No
            },
            {
                KeyMeanAnomaly,
                new DoubleInRangeVerifier(0.0, 360.0),
                "Specifies the position of the orbiting body (in degrees) along the "
                "elliptical orbit at epoch time.",
                Optional::No
            },
            {
                KeyEpoch,
                new StringVerifier,
                "Specifies the epoch time used for position as a string of the form: "
                "YYYY MM DD HH:mm:ss",
                Optional::No
            },
            {
                KeyPeriod,
                new DoubleGreaterVerifier(0.0),
                "Specifies the orbital period (in seconds).",
                Optional::No
            },
        },
        Exhaustive::Yes
    };
}

KeplerTranslation::KeplerTranslation()
    : Translation()
    , _eccentricity("eccentricity", "Eccentricity", "", 0.0, 0.0, 1.0) // @TODO Missing documentation
    , _semiMajorAxis("semimajorAxis", "Semi-major axis", "", 0.0, 0.0, 1e6) // @TODO Missing documentation
    , _inclination("inclination", "Inclination", "", 0.0, 0.0, 360.0) // @TODO Missing documentation
    , _ascendingNode(
        "ascendingNode",
        "Right ascension of ascending Node",
        "", // @TODO Missing documentation
        0.0,
        0.0,
        360.0
    )
    , _argumentOfPeriapsis(
        "argumentOfPeriapsis",
        "Argument of Periapsis",
        "", // @TODO Missing documentation
        0.0,
        0.0,
        360.0
    )
    , _meanAnomalyAtEpoch("meanAnomalyAtEpoch", "Mean anomaly at epoch", "", 0.0, 0.0, 360.0) // @TODO Missing documentation
    , _epoch("epoch", "Epoch", "", 0.0, 0.0, 1e9) // @TODO Missing documentation
    , _period("period", "Orbit period", "", 0.0, 0.0, 1e6) // @TODO Missing documentation
    , _orbitPlaneDirty(true)
{
    auto update = [this]() {
        _orbitPlaneDirty = true;
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
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "KeplerTranslation"
    );

    setKeplerElements(
        dictionary.value<double>(KeyEccentricity),
        dictionary.value<double>(KeySemiMajorAxis),
        dictionary.value<double>(KeyInclination),
        dictionary.value<double>(KeyAscendingNode),
        dictionary.value<double>(KeyArgumentOfPeriapsis),
        dictionary.value<double>(KeyMeanAnomaly),
        dictionary.value<double>(KeyPeriod),
        dictionary.value<std::string>(KeyEpoch)
    );
}

glm::dvec3 KeplerTranslation::position() const {
    return _position;
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
            double e = _eccentricity;
            return x + (meanAnomaly + e * sin(x) - x) / (1.0 - e * cos(x));
        };
        return solveIteration(solver, meanAnomaly, 0.0, 6);
    }
    else if (_eccentricity < 1.0) {
        auto sign = [](double val) -> double {
            return val > 0.0 ? 1.0 : ((val < 0.0) ? -1.0 : 0.0);
        };
        double e = meanAnomaly + 0.85 * _eccentricity * sign(sin(meanAnomaly));

        auto solver = [this, &meanAnomaly, &sign](double x) -> double {
            double e = _eccentricity;
            double s = e * sin(x);
            double c = e * cos(x);
            double f = x - s - meanAnomaly;
            double f1 = 1 - c;
            double f2 = s;
            return x + (-5 * f / (f1 + sign(f1) * sqrt(std::abs(16 * f1 * f1 - 20 * f * f2))));
        };
        return solveIteration(solver, e, 0.0, 8);
    }
    else {
        ghoul_assert(false, "Eccentricity must not be >= 1.0");
        LERRORC("KeplerTranslation", "Eccentricity must not be >= 1.0");
        return 0.0;
    }
}

void KeplerTranslation::update(const UpdateData& data) {
    if (_orbitPlaneDirty) {
        computeOrbitPlane();
        _orbitPlaneDirty = false;
    }

    double t = data.time.j2000Seconds() - _epoch;
    double meanMotion = 2.0 * glm::pi<double>() / _period;
    double meanAnomaly = glm::radians(_meanAnomalyAtEpoch.value()) + t * meanMotion;
    double e = eccentricAnomaly(meanAnomaly);

    // Use the eccentric anomaly to compute the actual location
    double a = _semiMajorAxis / (1.0 - _eccentricity) * 1000.0;
    glm::dvec3 p = {
        a * (cos(e) - _eccentricity),
        a * sqrt(1.0 - _eccentricity * _eccentricity) * sin(e),
        0.0
    };
    _position = _orbitPlaneRotation * p;
}

void KeplerTranslation::computeOrbitPlane() {
    // We assume the following coordinate system:
    // z = axis of rotation
    // x = pointing towards the first point of Aries
    // y completes the righthanded coordinate system
    
    // Perform three rotations:
    // 1. Around the z axis to place the location of the ascending node
    // 2. Around the x axis (now aligned with the ascending node) to get the correct
    // inclination
    // 3. Around the new z axis to place the closest approach to the correct location
    
    const glm::vec3 ascendingNodeAxisRot = { 0.f, 0.f, 1.f };
    const glm::vec3 inclinationAxisRot = { 1.f, 0.f, 0.f };
    const glm::vec3 argPeriapsisAxisRot = { 0.f, 0.f, 1.f };
    
    const double asc = glm::radians(_ascendingNode.value());
    const double inc = glm::radians(_inclination.value());
    const double per = glm::radians(_argumentOfPeriapsis.value());

    _orbitPlaneRotation =
        glm::rotate(asc, glm::dvec3(ascendingNodeAxisRot)) *
        glm::rotate(inc, glm::dvec3(inclinationAxisRot)) *
        glm::rotate(per, glm::dvec3(argPeriapsisAxisRot));

    notifyObservers();
    _orbitPlaneDirty = false;
}

void KeplerTranslation::setKeplerElements(double eccentricity, double semiMajorAxis,
                                          double inclination, double ascendingNode,
                                          double argumentOfPeriapsis,
                                          double meanAnomalyAtEpoch,
                                          double orbitalPeriod, const std::string& epoch)
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
                                          double meanAnomalyAtEpoch,
                                          double orbitalPeriod, double epoch)
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
