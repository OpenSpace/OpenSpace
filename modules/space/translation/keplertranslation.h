/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_MODULE_SPACE___KEPLERTRANSLATION___H__
#define __OPENSPACE_MODULE_SPACE___KEPLERTRANSLATION___H__

#include <openspace/scene/translation.h>

#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/util/time.h>
#include <ghoul/glm.h>
#include <ghoul/misc/exception.h>
#include <openspace/util/time.h>

namespace openspace {

/**
 * The KeplerTranslation is a concrete Translation implementation that uses the 6
 * Keplerian elements (eccentricity, semi-major axis, inclination, right ascension of the
 * ascending node, argument of periapsis, and mean anomaly at epoch) for computing the
 * position of a space craft. So far, only eccentricities between [0, 1) are supoorted.
 */
class KeplerTranslation : public Translation {
public:
    struct RangeError : public ghoul::RuntimeError {
        explicit RangeError(std::string off);

        std::string offender;
    };

    /**
     * The constructor that retrieves the required Keplerian elements from the passed
     * \p dictionary. These values are then apssed to the setKeplerElements method for
     * further processing.
     * The \p dictionary is tested against the Documentation for conformance.
     *
     * \param dictionary The ghoul::Dictionary containing all the information about the
     *        Keplerian elements (see Documentation)
     */
    KeplerTranslation(const ghoul::Dictionary& dictionary);

    /// Default destructor
    virtual ~KeplerTranslation() = default;

    /**
    * Method returning the translation vector at a given time.
    *
    * \param time The time to use when doing the position lookup
    */
    glm::dvec3 position(const UpdateData& data) const override;

    /**
     * Method returning the openspace::Documentation that describes the ghoul::Dictinoary
     * that can be passed to the constructor.
     *
     * \return The openspace::Documentation that describes the ghoul::Dicitonary that can
     *         be passed to the constructor
     */
    static documentation::Documentation Documentation();

    /**
     * Sets the internal values for the Keplerian elements and the epoch as a string of
     * the form YYYY MM DD HH:mm:ss.
     *
     * \param eccentricity The eccentricity of the orbit
     * \param semiMajorAxis The semi-major axis of the orbit
     * \param inclination The inclination of the orbit relative to the (x-y) reference
     *        plane (in the case of J2000, the equator)
     * \param ascendingNode The right ascension of the ascending node computed relative
     *        to the x axis (in the case of J2000, the first point of Aries)
     * \param argumentOfPeriapsis The location on the orbit with the closes approach
     * \param meanAnomalyAtEpoch The location of the space craft on the orbit at the time
     *        of the \p epoch
     * \param orbitalPeriod The period of the orbit in seconds
     * \param epoch The epoch to which the orbit is defined as a string of the form:
     *        YYYY MM DD HH:mm::ss
     */
    void setKeplerElements(double eccentricity, double semiMajorAxis, double inclination,
        double ascendingNode, double argumentOfPeriapsis, double meanAnomalyAtEpoch,
        double orbitalPeriod, const std::string& epoch);

    /**
     * Sets the internal values for the Keplerian elements and the epoch as seconds past
     * J2000 epoch.
     *
     * \param eccentricity The eccentricity of the orbit
     * \param semiMajorAxis The semi-major axis of the orbit
     * \param inclination The inclination of the orbit relative to the (x-y) reference
     *        plane (in the case of J2000, the equator)
     * \param ascendingNode The right ascension of the ascending node computed relative
     *        to the x axis (in the case of J2000, the first point of Aries)
     * \param argumentOfPeriapsis The location on the orbit with the closes approach
     * \param meanAnomalyAtEpoch The location of the space craft on the orbit at the time
     *        of the \p epoch
     * \param orbitalPeriod The period of the orbit in seconds
     * \param epoch The epoch to which the orbit is defined as number of seconds past the
     *        J2000 epoch
     */
    void setKeplerElements(double eccentricity, double semiMajorAxis, double inclination,
        double ascendingNode, double argumentOfPeriapsis, double meanAnomalyAtEpoch,
        double orbitalPeriod, double epoch);

    /// Default construct that initializes all the properties and member variables
    KeplerTranslation();

    /// Recombutes the rotation matrix used in the update method
    void computeOrbitPlane() const;

private:
    /**
     * This method computes the eccentric anomaly (location of the space craft taking the
     * eccentricity into acount) based on the mean anomaly (location of the space craft
     * assuming an eccentricity of 0.0).
     *
     * \param meanAnomaly The mean anomaly for which the eccentric anomaly shall be
     *        computed
     * \return The eccentric anomaly for the provided \p meanAnomaly
     */
    double eccentricAnomaly(double meanAnomaly) const;

    /// The eccentricity of the orbit in [0, 1)
    properties::DoubleProperty _eccentricity;
    /// The semi-major axis in km
    properties::DoubleProperty _semiMajorAxis;
    /// The inclination of the orbit in [0, 360]
    properties::DoubleProperty _inclination;
    /// The right ascension of the ascending node in [0, 360]
    properties::DoubleProperty _ascendingNode;
    /// The argument of periapsis in [0, 360]
    properties::DoubleProperty _argumentOfPeriapsis;
    /// The mean anomaly at the epoch in [0, 360]
    properties::DoubleProperty _meanAnomalyAtEpoch;

    /// The epoch in seconds relative to the J2000 epoch
    properties::DoubleProperty _epoch;
    /// The period of the orbit in seconds
    properties::DoubleProperty _period;

    /// Dirty flag for the _orbitPlaneRotation parameters
    mutable bool _orbitPlaneDirty = true;
    /// The rotation matrix that defines the plane of the orbit
    mutable glm::dmat3 _orbitPlaneRotation = glm::dmat3(1.0);

    /// The cached position for the last time with which the update method was called
    glm::dvec3 _position = glm::dvec3(0.0);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___KEPLERTRANSLATION___H__
