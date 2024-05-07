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

#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>

#include <openspace/util/spicemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include "SpiceUsr.h"
#include "SpiceZpr.h"

using namespace openspace;

namespace {
    // In this testclass only a handset of the testfunctions require a single kernel.
    // The remaining methods rely on multiple kernels, loaded as a SPICE 'meta-kernel'
    void loadMetaKernel() {
        const int k1 = SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
        );
        CHECK(k1 == 1);

        const int k2 = SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/cas00084.tsc")
        );
        CHECK(k2 == 2);

        const int k3 = SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/981005_PLTEPH-DE405S.bsp")
        );
        CHECK(k3 == 3);

        const int k4 = SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/020514_SE_SAT105.bsp")
        );
        CHECK(k4 == 4);

        const int k5 = SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/030201AP_SK_SM546_T45.bsp")
        );
        CHECK(k5 == 5);

        const int k6 = SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/cas_v37.tf")
        );
        CHECK(k6 == 6);

        const int k7 = SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/04135_04171pc_psiv2.bc")
        );
        CHECK(k7 == 7);

        const int k8 = SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/cpck05Mar2004.tpc")
        );
        CHECK(k8 == 8);

        const int k9 = SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/cas_iss_v09.ti")
        );
        CHECK(k9 == 9);
    }

    int loadLSKKernel() {
        const int kernelID = SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
        );
        CHECK(kernelID == 1);
        return kernelID;
    }

    int loadPCKKernel() {
        const int kernelID = SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/cpck05Mar2004.tpc")
        );
        CHECK(kernelID == 1);
        return kernelID;
    }
} // namespace

TEST_CASE("SpiceManager: Load Single Kernel", "[spicemanager]") {
    constexpr int FileLength = 128;
    constexpr int TypeLength = 32;
    constexpr int SourceLength = 128;

    SpiceManager::initialize();

    loadLSKKernel();

    SpiceInt handle = 0;
    std::array<char, FileLength> file;
    std::array<char, TypeLength> filtyp;
    std::array<char, SourceLength> source;
    SpiceBoolean found = SPICEFALSE;
    kdata_c(
        0,
        "text",
        FileLength,
        TypeLength,
        SourceLength,
        file.data(),
        filtyp.data(),
        source.data(),
        &handle,
        &found
    );
    CHECK(found == SPICETRUE);

    SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Unload Kernel String", "[spicemanager]") {
    constexpr int FileLength = 128;
    constexpr int TypeLength = 32;
    constexpr int SourceLength = 128;

    SpiceManager::initialize();

    loadLSKKernel();

    SpiceInt handle = 0;
    std::array<char, FileLength> file;
    std::array<char, TypeLength> filtyp;
    std::array<char, SourceLength> source;
    SpiceBoolean found = SPICEFALSE;
    kdata_c(
        0,
        "text",
        FileLength,
        TypeLength,
        SourceLength,
        file.data(),
        filtyp.data(),
        source.data(),
        &handle,
        &found
    );
    CHECK(found == SPICETRUE);

    // unload using string keyword
    SpiceManager::ref().unloadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );

    found = SPICEFALSE;
    kdata_c(
        0,
        "text",
        FileLength,
        TypeLength,
        SourceLength,
        file.data(),
        filtyp.data(),
        source.data(),
        &handle,
        &found
    );
    CHECK(found != SPICETRUE);

    SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Unload Kernel Integer", "[spicemanager]") {
    constexpr int FileLength = 128;
    constexpr int TypeLength = 32;
    constexpr int SourceLength = 128;

    SpiceManager::initialize();

    const int kernelID = loadLSKKernel();

    SpiceInt handle = 0;
    std::array<char, FileLength> file;
    std::array<char, TypeLength> filtyp;
    std::array<char, SourceLength> source;
    SpiceBoolean found = SPICEFALSE;
    kdata_c(
        0,
        "text",
        FileLength,
        TypeLength,
        SourceLength,
        file.data(),
        filtyp.data(),
        source.data(),
        &handle,
        &found
    );
    CHECK(found == SPICETRUE);

    // unload using unique int ID
    SpiceManager::ref().unloadKernel(kernelID);

    found = SPICEFALSE;
    kdata_c(
        0,
        "text",
        FileLength,
        TypeLength,
        SourceLength,
        file.data(),
        filtyp.data(),
        source.data(),
        &handle,
        &found
    );
    CHECK(found != SPICETRUE);

    SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Has Value", "[spicemanager]") {
    SpiceManager::initialize();

    loadPCKKernel();

    const int naifId = 399; // Earth
    const std::string kernelPoolValue = "RADII";
    const bool found = SpiceManager::ref().hasValue(naifId, kernelPoolValue);
    CHECK(found);

    SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Get Value From ID 1D", "[spicemanager]") {
    SpiceManager::initialize();

    loadPCKKernel();

    const std::string target  = "EARTH";
    const std::string value1D = "MAG_NORTH_POLE_LAT";
    double return1D = 0.0;
    CHECK_NOTHROW(SpiceManager::ref().getValue(target, value1D, return1D));
    CHECK(return1D == 78.565);

    SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Get Value From ID 3D", "[spicemanager]") {
    SpiceManager::initialize();

    loadPCKKernel();

    const std::string target  = "EARTH";
    const std::string value3D = "RADII";
    glm::dvec3 return3D = glm::dvec3(0.0);
    CHECK_NOTHROW(SpiceManager::ref().getValue(target, value3D, return3D));

    CHECK(return3D.x == 6378.14);
    CHECK(return3D.y == 6378.14);
    CHECK(return3D.z == 6356.75);

    SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Get Value From ID ND", "[spicemanager]") {
    SpiceManager::initialize();

    loadPCKKernel();

    const std::string target = "SATURN";
    const std::string valueND = "RING6_A";
    std::vector<double> returnND;
    returnND.resize(5);
    CHECK_NOTHROW(SpiceManager::ref().getValue(target, valueND, returnND));

    std::vector<double> controlVec = { 189870.0, 256900.0, 9000.0, 9000.0, 0.000003 };

    CHECK(controlVec.size() == returnND.size());

    for (unsigned int i = 0; i < returnND.size(); i++) {
        CHECK(controlVec[i] == returnND[i]);
    }

    SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: String To Ephemeris Time", "[spicemanager]") {
    SpiceManager::initialize();

    loadLSKKernel();

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

    for (int i = 0; i < 3; i++){
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
#endif

    // transform reference position into new frame
    std::array<double, 3> stateTransformed;
    mxvg_c(referenceMatrix.data(), state.data(), 3, 3, stateTransformed.data());

#if defined __clang__
#pragma clang diagnostic pop
#elif defined __GNUC__
#pragma GCC diagnostic pop
#endif

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
    //check vectors have correct values
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
