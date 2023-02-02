/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

namespace {
    constexpr int FILLEN = 128;
    constexpr int TYPLEN = 32;
    constexpr int SRCLEN = 128;

    namespace spicemanager_constants {
        SpiceInt handle;
        char file[FILLEN], filtyp[TYPLEN], source[SRCLEN];
    } // namespace spicemanager_constants



    // In this testclass only a handset of the testfunctions require a single kernel.
    // The remaining methods rely on multiple kernels, loaded as a SPICE 'meta-kernel'
    void loadMetaKernel() {
        const int k1 = openspace::SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls").string()
        );
        CHECK(k1 == 1);

        const int k2 = openspace::SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/cas00084.tsc").string()
        );
        CHECK(k2 == 2);

        const int k3 = openspace::SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/981005_PLTEPH-DE405S.bsp").string()
        );
        CHECK(k3 == 3);

        const int k4 = openspace::SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/020514_SE_SAT105.bsp").string()
        );
        CHECK(k4 == 4);

        const int k5 = openspace::SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/030201AP_SK_SM546_T45.bsp").string()
        );
        CHECK(k5 == 5);

        const int k6 = openspace::SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/cas_v37.tf").string()
        );
        CHECK(k6 == 6);

        const int k7 = openspace::SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/04135_04171pc_psiv2.bc").string()
        );
        CHECK(k7 == 7);

        const int k8 = openspace::SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/cpck05Mar2004.tpc").string()
        );
        CHECK(k8 == 8);

        const int k9 = openspace::SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/cas_iss_v09.ti").string()
        );
        CHECK(k9 == 9);
    }

    int loadLSKKernel()  { 
        int kernelID = openspace::SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls").string()
        );
        CHECK(kernelID == 1);
        return kernelID;
    }

    int loadPCKKernel()  {
        int kernelID = openspace::SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/cpck05Mar2004.tpc").string()
        );
        CHECK(kernelID == 1);
        return kernelID;
    }
} // namespace

TEST_CASE("SpiceManager: Load Single Kernel", "[spicemanager]") {
    openspace::SpiceManager::initialize();

    loadLSKKernel();
    // naif0008.tls is a text file, check if loaded.
    SpiceBoolean found;
    kdata_c(
        0,
        "text",
        FILLEN,
        TYPLEN,
        SRCLEN,
        spicemanager_constants::file,
        spicemanager_constants::filtyp,
        spicemanager_constants::source,
        &spicemanager_constants::handle,
        &found
    );
    
    CHECK(found == SPICETRUE);

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Unload Kernel String", "[spicemanager]") {
    openspace::SpiceManager::initialize();

    loadLSKKernel();
    // naif0008.tls is a text file, check if loaded.
    SpiceBoolean found;
    kdata_c(
        0,
        "text",
        FILLEN,
        TYPLEN,
        SRCLEN,
        spicemanager_constants::file,
        spicemanager_constants::filtyp,
        spicemanager_constants::source,
        &spicemanager_constants::handle,
        &found
    );
    CHECK(found == SPICETRUE);

    // unload using string keyword
    openspace::SpiceManager::ref().unloadKernel(
        absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls").string()
    );

    found = SPICEFALSE;
    kdata_c(
        0,
        "text",
        FILLEN,
        TYPLEN,
        SRCLEN,
        spicemanager_constants::file,
        spicemanager_constants::filtyp,
        spicemanager_constants::source,
        &spicemanager_constants::handle,
        &found
    );
    CHECK(found != SPICETRUE);

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Unload Kernel Integer", "[spicemanager]") {
    openspace::SpiceManager::initialize();

    int kernelID = loadLSKKernel();
    // naif0008.tls is a text file, check if loaded.
    SpiceBoolean found;
    kdata_c(
        0,
        "text",
        FILLEN,
        TYPLEN,
        SRCLEN,
        spicemanager_constants::file,
        spicemanager_constants::filtyp,
        spicemanager_constants::source,
        &spicemanager_constants::handle,
        &found
    );
    CHECK(found == SPICETRUE);

    // unload using unique int ID
    openspace::SpiceManager::ref().unloadKernel(kernelID);

    found = SPICEFALSE;
    kdata_c(
        0,
        "text",
        FILLEN,
        TYPLEN,
        SRCLEN,
        spicemanager_constants::file,
        spicemanager_constants::filtyp,
        spicemanager_constants::source,
        &spicemanager_constants::handle,
        &found
    );
    CHECK(found != SPICETRUE);

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Has Value", "[spicemanager]") {
    openspace::SpiceManager::initialize();

    loadPCKKernel();

    int naifId = 399; // Earth

    std::string kernelPoolValue = "RADII";

    const bool found = openspace::SpiceManager::ref().hasValue(naifId, kernelPoolValue);
    CHECK(found);

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Get Value From ID 1D", "[spicemanager]") {
    openspace::SpiceManager::initialize();

    loadPCKKernel();

    std::string target  = "EARTH";
    std::string value1D = "MAG_NORTH_POLE_LAT";

    double return1D;
    CHECK_NOTHROW(openspace::SpiceManager::ref().getValue(target, value1D, return1D));
    CHECK(return1D == 78.565);

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Get Value From ID 3D", "[spicemanager]") {
    openspace::SpiceManager::initialize();

    loadPCKKernel();

    std::string target  = "EARTH";
    std::string value3D = "RADII";

    glm::dvec3 return3D = glm::dvec3(0.0);
    CHECK_NOTHROW(openspace::SpiceManager::ref().getValue(target, value3D, return3D));

    CHECK(return3D.x == 6378.14);
    CHECK(return3D.y == 6378.14);
    CHECK(return3D.z == 6356.75);

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Get Value From ID ND", "[spicemanager]") {
    openspace::SpiceManager::initialize();

    loadPCKKernel();

    std::string target  = "SATURN";
    std::string valueND = "RING6_A";

    std::vector<double> returnND(5);
    CHECK_NOTHROW(openspace::SpiceManager::ref().getValue(target, valueND, returnND));

    std::vector<double> controlVec{ 189870.0, 256900.0, 9000.0, 9000.0, 0.000003 };
    
    CHECK(controlVec.size() == returnND.size());

    for (unsigned int i = 0; i < returnND.size(); ++i){
        CHECK(controlVec[i] == returnND[i]);
    }

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: String To Ephemeris Time", "[spicemanager]") {
    openspace::SpiceManager::initialize();

    loadLSKKernel();

    double ephemerisTime;
    double control_ephemerisTime;
    char date[SRCLEN] = "Thu Mar 20 12:53:29 PST 1997";
    str2et_c(date, &control_ephemerisTime);

    CHECK_NOTHROW(
        ephemerisTime = openspace::SpiceManager::ref().ephemerisTimeFromDate(date)
    );

    CHECK(ephemerisTime == control_ephemerisTime);

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Get Target Position", "[spicemanager]") {
    openspace::SpiceManager::initialize();

    using openspace::SpiceManager;
    loadMetaKernel();

    double et = 0.0;
    double pos[3] = { 0.0, 0.0, 0.0 };
    double lt = 0.0;
    char utctime[SRCLEN] = "2004 jun 11 19:32:00";

    str2et_c(utctime, &et);
    spkpos_c("EARTH", et, "J2000", "LT+S", "CASSINI", pos, &lt);

    glm::dvec3 targetPosition = glm::dvec3(0.0);
    double lightTime = 0.0;
    SpiceManager::AberrationCorrection corr = {
        SpiceManager::AberrationCorrection::Type::LightTimeStellar,
        SpiceManager::AberrationCorrection::Direction::Reception
    };

    CHECK_NOTHROW(
        [&]() {
            targetPosition = SpiceManager::ref().targetPosition(
                "EARTH", "CASSINI", "J2000", corr, et, lightTime
            );
        }()
    );
    CHECK(pos[0] == Catch::Approx(targetPosition[0]));
    CHECK(pos[1] == Catch::Approx(targetPosition[1]));
    CHECK(pos[2] == Catch::Approx(targetPosition[2]));

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Get Target State", "[spicemanager]") {
    openspace::SpiceManager::initialize();

    using openspace::SpiceManager;
    loadMetaKernel();

    double et;
    double state[6];
    double lt;
    char utctime[SRCLEN] = "2004 jun 11 19:32:00";

    str2et_c(utctime, &et);
    spkezr_c("EARTH", et, "J2000", "LT+S", "CASSINI", state, &lt);

    SpiceManager::AberrationCorrection corr = {
        SpiceManager::AberrationCorrection::Type::LightTimeStellar,
        SpiceManager::AberrationCorrection::Direction::Reception
    };

    SpiceManager::TargetStateResult res;
    CHECK_NOTHROW(
        res = SpiceManager::ref().targetState("EARTH", "CASSINI", "J2000", corr, et)
    );

    // x,y,z
    for (int i = 0; i < 3; i++){
        CHECK(state[i] == Catch::Approx(res.position[i]));
        CHECK(state[i+3] == Catch::Approx(res.velocity[i]));
    }

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Transform matrix", "[spicemanager]") {
    openspace::SpiceManager::initialize();

   loadMetaKernel();

   double et;
   double state[6];
   double lt;
   double referenceMatrix[6][6];

   str2et_c("2004 jun 11 19:32:00", &et);
   spkezr_c("PHOEBE", et, "J2000", "LT+S", "CASSINI", state, &lt);
   sxform_c("J2000", "IAU_PHOEBE", et, referenceMatrix);

   openspace::SpiceManager::TransformMatrix stateMatrix;
   CHECK_NOTHROW(
     stateMatrix = openspace::SpiceManager::ref().stateTransformMatrix(
        "J2000", "IAU_PHOEBE", et)
    );
   
   // check for matrix consistency
   for (int i = 0; i < 6; i++) {
       for (int j = 0; j < 6; j++) {
           CHECK(referenceMatrix[i][j] == Catch::Approx(stateMatrix[i * 6 + j]));
       }
   }

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Get Position Transform Matrix", "[spicemanager]") {
    openspace::SpiceManager::initialize();

    using openspace::SpiceManager;
    loadMetaKernel();

    double et;
    double state[3] = { 1.0, 1.0, 1.0 };
    double state_t[3];
    double referenceMatrix[3][3];

    str2et_c("2004 jun 11 19:32:00", &et);
    pxform_c("CASSINI_HGA", "J2000", et, referenceMatrix);

    glm::dmat3 positionMatrix = glm::dmat3(1.0);
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
    mxvg_c(referenceMatrix, state, 3, 3, state_t);

#if defined __clang__
#pragma clang diagnostic pop
#elif defined __GNUC__
#pragma GCC diagnostic pop
#endif

    position = positionMatrix * position;
    // check transformed values match 
    for (int i = 0; i < 3; i++) {
        CHECK(position[i] == Catch::Approx(state_t[i]));
    }

    openspace::SpiceManager::deinitialize();
}

TEST_CASE("SpiceManager: Get Field Of View", "[spicemanager]") {
    openspace::SpiceManager::initialize();

    using openspace::SpiceManager;
    loadMetaKernel();
    
    SpiceInt n;
    SpiceInt cassini_ID;
    double et;
    double bounds_ref[5][3];
    char shape_ref[TYPLEN];
    char name_ref[FILLEN];
    double boresightVec[3];

    str2et_c("2004 jun 11 19:32:00", &et);
    SpiceBoolean found;
    bodn2c_c("CASSINI_ISS_NAC", &cassini_ID, &found);
    CHECK(found == SPICETRUE);

    getfov_c(cassini_ID, 5, TYPLEN, TYPLEN, shape_ref, name_ref, boresightVec, &n, bounds_ref);

    SpiceManager::FieldOfViewResult res;

    CHECK_NOTHROW(res = SpiceManager::ref().fieldOfView("CASSINI_ISS_NAC"));

    CHECK(found == SPICETRUE);
    //check vectors have correct values
    for (size_t i = 0; i < res.bounds.size(); i++) {
        for (size_t j = 0; j < 3; j++) {
            CHECK(
                bounds_ref[i][j] == Catch::Approx(res.bounds[i][static_cast<glm::length_t>(j)])
            );
        }
    }

    openspace::SpiceManager::deinitialize();
}
