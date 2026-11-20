/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>

#include <openspace/filesystem/filesystem.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/timeconstants.h>
#include <optional>
#include "SpiceUsr.h"
#include "SpiceZpr.h"

using namespace openspace;

namespace {
    // In this testclass only a handset of the testfunctions require a single kernel.
    // The remaining methods rely on multiple kernels, loaded as a SPICE 'meta-kernel'
    void loadMetaKernel() {
        const int k1 = SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/cas00084.tsc")
        );
        CHECK(k1 == 3);

        const int k2 = SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/981005_PLTEPH-DE405S.bsp")
        );
        CHECK(k2 == 4);

        const int k3 = SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/020514_SE_SAT105.bsp")
        );
        CHECK(k3 == 5);

        const int k4 = SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/030201AP_SK_SM546_T45.bsp")
        );
        CHECK(k4 == 6);

        const int k5 = SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/cas_v37.tf")
        );
        CHECK(k5 == 7);

        const int k6 = SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/04135_04171pc_psiv2.bc")
        );
        CHECK(k6 == 8);

        const int k7 = SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/cpck05Mar2004.tpc")
        );
        CHECK(k7 == 9);

        const int k8 = SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/cas_iss_v09.ti")
        );
        CHECK(k8 == 10);
    }

    // Returns the angle in degrees between the surface normal and the direction to the
    // Sun for the point on the surface of the Earth at the provided location, which is
    // computed by SPICE independently of the SpiceManager::solarEventTime function.
    // The angle is 90 degrees when the Sun is on the horizon.
    double solarIncidenceAngle(double et, double latitude, double longitude) {
        SpiceInt dim = 0;
        std::array<double, 3> radii = { 0.0, 0.0, 0.0 };
        bodvrd_c("EARTH", "RADII", 3, &dim, radii.data());
        const double flattening = (radii[0] - radii[2]) / radii[0];

        std::array<double, 3> surfacePoint = { 0.0, 0.0, 0.0 };
        georec_c(
            glm::radians(longitude),
            glm::radians(latitude),
            0.0,
            radii[0],
            flattening,
            surfacePoint.data()
        );

        double surfaceEpoch = 0.0;
        std::array<double, 3> surfaceVector = { 0.0, 0.0, 0.0 };
        double phase = 0.0;
        double incidence = 0.0;
        double emission = 0.0;
        ilumin_c(
            "ELLIPSOID",
            "EARTH",
            et,
            "IAU_EARTH",
            "LT+S",
            "MOON",
            surfacePoint.data(),
            &surfaceEpoch,
            surfaceVector.data(),
            &phase,
            &incidence,
            &emission
        );
        if (failed_c()) {
            throwSpiceError("Error computing the solar incidence angle");
        }
        return glm::degrees(incidence);
    }
} // namespace

TEST_CASE("SpiceManager: String To Ephemeris Time", "[spicemanager]") {
    SpiceManager::initialize();

    double control_ephemerisTime = 0.0;
    const std::string date = "Thu Mar 20 12:53:29 PST 1997";
    str2et_c(date.c_str(), &control_ephemerisTime);

    double ephemerisTime = -1.0;
    CHECK_NOTHROW(ephemerisTime = SpiceManager::ref().ephemerisTimeFromDate(date));

    CHECK(ephemerisTime == control_ephemerisTime);

    SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Get Target Position", "[spicemanager]") {
    SpiceManager::initialize();

    loadMetaKernel();

    const std::string utctime = "2004 JUN 11 19:32:00";
    double et = 0.0;
    str2et_c(utctime.c_str(), &et);

    std::array<double, 3> pos = { 0.0, 0.0, 0.0 };
    double lt = 0.0;
    spkpos_c("EARTH", et, "J2000", "LT+S", "CASSINI", pos.data(), &lt);

    glm::dvec3 targetPosition = glm::dvec3(0.0);
    double lightTime = 0.0;
    const SpiceManager::AberrationCorrection corr = {
        SpiceManager::AberrationCorrection::Type::LightTimeStellar,
        SpiceManager::AberrationCorrection::Direction::Reception
    };

    CHECK_NOTHROW(
        [&]() {
            targetPosition = SpiceManager::ref().targetPosition(
                "EARTH",
                "CASSINI",
                "J2000",
                corr,
                et,
                lightTime
            );
        }()
    );
    CHECK(pos[0] == Catch::Approx(targetPosition[0]));
    CHECK(pos[1] == Catch::Approx(targetPosition[1]));
    CHECK(pos[2] == Catch::Approx(targetPosition[2]));

    SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Get Target State", "[spicemanager]") {
    SpiceManager::initialize();

    loadMetaKernel();

    double et = 0.0;
    const std::string utctime = "2004 JUN 11 19:32:00";
    str2et_c(utctime.c_str(), &et);

    std::array<double, 6> state;
    double lt = 0.0;
    spkezr_c("EARTH", et, "J2000", "LT+S", "CASSINI", state.data(), &lt);

    const SpiceManager::AberrationCorrection corr = {
        SpiceManager::AberrationCorrection::Type::LightTimeStellar,
        SpiceManager::AberrationCorrection::Direction::Reception
    };

    SpiceManager::TargetStateResult res;
    CHECK_NOTHROW(
        res = SpiceManager::ref().targetState("EARTH", "CASSINI", "J2000", corr, et)
    );

    for (int i = 0; i < 3; i++) {
        CHECK(state[i] == Catch::Approx(res.position[i]));
        CHECK(state[i+3] == Catch::Approx(res.velocity[i]));
    }

    SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Transform matrix", "[spicemanager]") {
    SpiceManager::initialize();

    loadMetaKernel();


    double et = 0.0;
    str2et_c("2004 JUN 11 19:32:00", &et);

    std::array<double, 6> state;
    double lt = 0.0;
    spkezr_c("PHOEBE", et, "J2000", "LT+S", "CASSINI", state.data(), &lt);

    std::array<double[6], 6> referenceMatrix;
    sxform_c("J2000", "IAU_PHOEBE", et, referenceMatrix.data());

    SpiceManager::TransformMatrix stateMatrix;
    CHECK_NOTHROW(
        stateMatrix = SpiceManager::ref().stateTransformMatrix(
            "J2000",
             "IAU_PHOEBE",
             et
        )
    );

   // check for matrix consistency
   for (int i = 0; i < 6; i++) {
       for (int j = 0; j < 6; j++) {
           CHECK(referenceMatrix[i][j] == Catch::Approx(stateMatrix[i * 6 + j]));
       }
   }

    SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Get Position Transform Matrix", "[spicemanager]") {
    SpiceManager::initialize();

    loadMetaKernel();


    double et = 0.0;
    str2et_c("2004 JUN 11 19:32:00", &et);

    std::array<double[3], 3> referenceMatrix;
    pxform_c("CASSINI_HGA", "J2000", et, referenceMatrix.data());

    glm::dmat3 positionMatrix = glm::dmat3(1.0);
    std::array<double, 3> state = { 1.0, 1.0, 1.0 };
    glm::dvec3 position(state[0], state[1], state[2]);
    CHECK_NOTHROW(
        [&]() {
            positionMatrix = SpiceManager::ref().positionTransformMatrix(
                "CASSINI_HGA", "J2000", et
            );
        }()
    );

    // check for matrix consistency
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            CHECK(referenceMatrix[i][j] == Catch::Approx(positionMatrix[j][i]));
        }
    }

#if defined __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wold-style-cast"
#elif defined __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wold-style-cast"
#endif // __clang__

    // transform reference position into new frame
    std::array<double, 3> stateTransformed;
    mxvg_c(referenceMatrix.data(), state.data(), 3, 3, stateTransformed.data());

#if defined __clang__
#pragma clang diagnostic pop
#elif defined __GNUC__
#pragma GCC diagnostic pop
#endif // __clang__

    position = positionMatrix * position;
    // check transformed values match
    for (int i = 0; i < 3; i++) {
        CHECK(position[i] == Catch::Approx(stateTransformed[i]));
    }

    SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Get Field Of View", "[spicemanager]") {
    constexpr int NameLength = 128;
    constexpr int ShapeLength = 32;

    SpiceManager::initialize();

    loadMetaKernel();


    double et = 0.0;
    str2et_c("2004 JUN 11 19:32:00", &et);

    SpiceInt id = 0;
    SpiceBoolean found = SPICETRUE;
    bodn2c_c("CASSINI_ISS_NAC", &id, &found);
    CHECK(found == SPICETRUE);

    std::array<char, ShapeLength> shapeRef;
    std::array<char, NameLength> nameRef;
    std::array<double, 3> boresightVec;
    SpiceInt n = 0;
    std::array<double[3], 5> boundsRef;
    getfov_c(
        id,
        5,
        ShapeLength,
        NameLength,
        shapeRef.data(),
        nameRef.data(),
        boresightVec.data(),
        &n,
        boundsRef.data()
    );

    SpiceManager::FieldOfViewResult res;

    CHECK_NOTHROW(res = SpiceManager::ref().fieldOfView("CASSINI_ISS_NAC"));

    CHECK(found == SPICETRUE);
    // Check vectors have correct values
    for (size_t i = 0; i < res.bounds.size(); i++) {
        for (size_t j = 0; j < 3; j++) {
            CHECK(
                boundsRef[i][j] ==
                Catch::Approx(res.bounds[i][static_cast<glm::length_t>(j)])
            );
        }
    }

    SpiceManager::deinitialize();
}

// The planetary ephemeris kernel of these tests only covers the time from 2004-06-11
// 05:00 to 2004-06-12 12:00 UTC. The solar event tests use the local day of 2004-06-11 at
// western longitudes, as that day is completely inside the covered time. A longitude of
// -75 degrees is the easternmost one for which this is true

TEST_CASE("SpiceManager: Solar Event Time", "[spicemanager]") {
    SpiceManager::initialize();

    loadMetaKernel();

    // Chicago shortly before the summer solstice, which is far enough from the poles to
    // have both a sunrise and a sunset every day
    constexpr double Latitude = 41.8781;
    constexpr double Longitude = -87.6298;
    const std::string Date = "2004-06-11";

    SECTION("Sunrise and sunset") {
        std::optional<double> optionalSunrise;
        std::optional<double> optionalSunset;
        CHECK_NOTHROW(
            optionalSunrise = SpiceManager::ref().solarEventTime(
                Latitude, Longitude, 0.0, Date, "EARTH", SpiceManager::IsSunrise::Yes
            )
        );
        CHECK_NOTHROW(
            optionalSunset = SpiceManager::ref().solarEventTime(
                Latitude, Longitude, 0.0, Date, "EARTH", SpiceManager::IsSunrise::No
            )
        );
        REQUIRE(optionalSunrise.has_value());
        REQUIRE(optionalSunset.has_value());
        const double sunrise = *optionalSunrise;
        const double sunset = *optionalSunset;

        // The events happen on the requested day in local mean solar time, which at this
        // longitude starts about six hours after midnight UTC
        const double localStart = SpiceManager::ref().ephemerisTimeFromDate(Date) -
            Longitude / 360.0 * timeconstants::SecondsPerDay;
        const double localEnd = localStart + timeconstants::SecondsPerDay;
        CHECK(sunrise >= localStart);
        CHECK(sunrise < sunset);
        CHECK(sunset < localEnd);

        // A day at this latitude and time of the year is about 15 hours long
        const double dayLengthHours = (sunset - sunrise) / timeconstants::SecondsPerHour;
        CHECK(dayLengthHours > 14.5);
        CHECK(dayLengthHours < 15.8);

        // The Sun is on the horizon at both events, as verified by SPICE itself. The Sun
        // moves at a quarter of a degree per minute, so the margin corresponds to a
        // couple of seconds, which is more than the one-second tolerance of the search
        const double incidenceAtSunrise = solarIncidenceAngle(
            sunrise,
            Latitude,
            Longitude
        );
        const double incidenceAtSunset = solarIncidenceAngle(sunset, Latitude, Longitude);
        CHECK(incidenceAtSunrise == Catch::Approx(90.0).margin(0.01));
        CHECK(incidenceAtSunset == Catch::Approx(90.0).margin(0.01));
    }

    SECTION("Follows longitude") {
        // San Francisco is seven to eight hours behind UTC, so the sunset of the local
        // day of June 11th is already on June 12th in UTC
        constexpr double SanFranciscoLatitude = 37.7749;
        constexpr double SanFranciscoLongitude = -122.4194;

        const double sunrise = SpiceManager::ref().solarEventTime(
            SanFranciscoLatitude,
            SanFranciscoLongitude,
            0.0,
            Date,
            "EARTH",
            SpiceManager::IsSunrise::Yes
        ).value();
        const double sunset = SpiceManager::ref().solarEventTime(
            SanFranciscoLatitude,
            SanFranciscoLongitude,
            0.0,
            Date,
            "EARTH",
            SpiceManager::IsSunrise::No
        ).value();

        const double utcMidnight = SpiceManager::ref().ephemerisTimeFromDate(
            "2004-06-12"
        );
        CHECK(sunrise < utcMidnight);
        CHECK(sunset > utcMidnight);

        // A day in June at this latitude is a bit more than 14 hours long
        const double dayLengthHours = (sunset - sunrise) / timeconstants::SecondsPerHour;
        CHECK(dayLengthHours > 14.0);
        CHECK(dayLengthHours < 15.0);
    }

    SECTION("Altitude") {
        // Seen from above the surface, the horizon is lower so the Sun rises earlier and
        // sets later
        const double sunriseGround = SpiceManager::ref().solarEventTime(
            Latitude, Longitude, 0.0, Date, "EARTH", SpiceManager::IsSunrise::Yes
        ).value();
        const double sunriseHigh = SpiceManager::ref().solarEventTime(
            Latitude, Longitude, 3000.0, Date, "EARTH", SpiceManager::IsSunrise::Yes
        ).value();
        const double sunsetGround = SpiceManager::ref().solarEventTime(
            Latitude, Longitude, 0.0, Date, "EARTH", SpiceManager::IsSunrise::No
        ).value();
        const double sunsetHigh = SpiceManager::ref().solarEventTime(
            Latitude, Longitude, 3000.0, Date, "EARTH", SpiceManager::IsSunrise::No
        ).value();
        CHECK(sunriseHigh < sunriseGround);
        CHECK(sunsetHigh > sunsetGround);
    }

    SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Solar Event Time Errors", "[spicemanager]") {
    SpiceManager::initialize();

    loadMetaKernel();

    const std::string Date = "2004-06-11";

    SECTION("Exceptions enabled") {
        // The Sun does not rise or set near the south pole in June. This is not an error,
        // but there is simply no time to return
        CHECK_FALSE(
            SpiceManager::ref().solarEventTime(
                -85.0, -100.0, 0.0, Date, "EARTH", SpiceManager::IsSunrise::Yes
            ).has_value()
        );
        CHECK_FALSE(
            SpiceManager::ref().solarEventTime(
                -85.0, -100.0, 0.0, Date, "EARTH", SpiceManager::IsSunrise::No
            ).has_value()
        );

        // A date that cannot be parsed or a body that does not exist throw, as
        // exceptions are enabled by default
        CHECK_THROWS_AS(
            SpiceManager::ref().solarEventTime(
                40.0, -100.0, 0.0, "not a date", "EARTH", SpiceManager::IsSunrise::Yes
            ),
            SpiceManager::SpiceException
        );
        CHECK_THROWS_AS(
            SpiceManager::ref().solarEventTime(
                40.0, -100.0, 0.0, Date, "NOTABODY", SpiceManager::IsSunrise::Yes
            ),
            SpiceManager::SpiceException
        );
    }

    SECTION("Exceptions disabled") {
        // This is how the application runs by default. Nothing is thrown, and there is
        // no result if the time cannot be computed
        SpiceManager::ref().setExceptionHandling(SpiceManager::UseException::No);

        CHECK_NOTHROW(
            SpiceManager::ref().solarEventTime(
                -85.0, -100.0, 0.0, Date, "EARTH", SpiceManager::IsSunrise::Yes
            )
        );
        CHECK_FALSE(
            SpiceManager::ref().solarEventTime(
                -85.0, -100.0, 0.0, Date, "EARTH", SpiceManager::IsSunrise::Yes
            ).has_value()
        );
        CHECK_FALSE(
            SpiceManager::ref().solarEventTime(
                40.0, -100.0, 0.0, "not a date", "EARTH", SpiceManager::IsSunrise::Yes
            ).has_value()
        );
        CHECK_FALSE(
            SpiceManager::ref().solarEventTime(
                40.0, -100.0, 0.0, Date, "NOTABODY", SpiceManager::IsSunrise::Yes
            ).has_value()
        );

        // The normal case still works
        CHECK(
            SpiceManager::ref().solarEventTime(
                41.8781, -87.6298, 0.0, Date, "EARTH", SpiceManager::IsSunrise::Yes
            ).has_value()
        );
    }

    SpiceManager::deinitialize();
}
