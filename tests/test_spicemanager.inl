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

#include <openspace/util/spicemanager.h>

#include <ghoul/filesystem/filesystem.h>

#include "SpiceUsr.h"
#include "SpiceZpr.h"

class SpiceManagerTest : public testing::Test {
protected:
    void SetUp() override {
        openspace::SpiceManager::initialize();
    }

    void TearDown() override {
        openspace::SpiceManager::deinitialize();

    }
};

//global constants 
#define  FILLEN   128
#define  TYPLEN   32
#define  SRCLEN   128

const int nrMetaKernels = 9;
SpiceInt which, handle, count = 0;
char file[FILLEN], filtyp[TYPLEN], source[SRCLEN];
double abs_error = 0.00001;


// In this testclass only a handset of the testfunctions require a single kernel.
// The remaining methods rely on multiple kernels, loaded as a SPICE 'meta-kernel'.

void loadMetaKernel() {
    int k1 = openspace::SpiceManager::ref().loadKernel(
        ("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
    );
    EXPECT_EQ(1, k1) << "loadKernel did not return proper id";

    int k2 = openspace::SpiceManager::ref().loadKernel(
        ("${TESTDIR}/SpiceTest/spicekernels/cas00084.tsc")
    );
    EXPECT_EQ(2, k2) << "loadKernel did not return proper id";

    int k3 = openspace::SpiceManager::ref().loadKernel(
        ("${TESTDIR}/SpiceTest/spicekernels/981005_PLTEPH-DE405S.bsp")
    );
    EXPECT_EQ(3, k3) << "loadKernel did not return proper id";

    int k4 = openspace::SpiceManager::ref().loadKernel(
        ("${TESTDIR}/SpiceTest/spicekernels/020514_SE_SAT105.bsp")
    );
    EXPECT_EQ(4, k4) << "loadKernel did not return proper id";

    int k5 = openspace::SpiceManager::ref().loadKernel(
        ("${TESTDIR}/SpiceTest/spicekernels/030201AP_SK_SM546_T45.bsp")
    );
    EXPECT_EQ(5, k5) << "loadKernel did not return proper id";

    int k6 = openspace::SpiceManager::ref().loadKernel(
        ("${TESTDIR}/SpiceTest/spicekernels/cas_v37.tf")
    );
    EXPECT_EQ(6, k6) << "loadKernel did not return proper id";

    int k7 = openspace::SpiceManager::ref().loadKernel(
        ("${TESTDIR}/SpiceTest/spicekernels/04135_04171pc_psiv2.bc")
    );
    EXPECT_EQ(7, k7) << "loadKernel did not return proper id";

    int k8 = openspace::SpiceManager::ref().loadKernel(
        ("${TESTDIR}/SpiceTest/spicekernels/cpck05Mar2004.tpc")
    );
    EXPECT_EQ(8, k8) << "loadKernel did not return proper id";

    int k9 = openspace::SpiceManager::ref().loadKernel(
        ("${TESTDIR}/SpiceTest/spicekernels/cas_iss_v09.ti")
    );
    EXPECT_EQ(9, k9) << "loadKernel did not return proper id";
}

#define LSK absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")

int loadLSKKernel()  { 
    int kernelID = openspace::SpiceManager::ref().loadKernel(
        LSK
    );
    EXPECT_EQ(1, kernelID) << "loadKernel did not return proper id";
    return kernelID;
}

#define PCK absPath("${TESTDIR}/SpiceTest/spicekernels/cpck05Mar2004.tpc")

int loadPCKKernel()  { 
    int kernelID = openspace::SpiceManager::ref().loadKernel(
        PCK
    );
    EXPECT_EQ(1, kernelID) << "loadKernel did not return proper id";
    return kernelID;
}

std::string fileType(char type[]){
    std::string str(type);
    return str;
}

// Try loading single kernel
TEST_F(SpiceManagerTest, loadSingleKernel) {
    loadLSKKernel();
    // naif0008.tls is a text file, check if loaded.
    SpiceBoolean found;
    kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
    
    ASSERT_TRUE(found == SPICETRUE) << "Kernel not loaded";
}

// Try unloading kernel using user assigned keyword
TEST_F(SpiceManagerTest, unloadKernelString) {
    loadLSKKernel();
    // naif0008.tls is a text file, check if loaded.
    SpiceBoolean found;
    kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
    ASSERT_TRUE(found == SPICETRUE);

    // unload using string keyword
    openspace::SpiceManager::ref().unloadKernel(LSK);

    found = SPICEFALSE;
    kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
    EXPECT_FALSE(found == SPICETRUE);
}

// Try unloading kernel using integer as ID
TEST_F(SpiceManagerTest, unloadKernelInteger) {
    int kernelID = loadLSKKernel();
    // naif0008.tls is a text file, check if loaded.
    SpiceBoolean found;
    kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
    ASSERT_TRUE(found == SPICETRUE);

    // unload using unique int ID
    openspace::SpiceManager::ref().unloadKernel(kernelID);

    found = SPICEFALSE;
    kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
    EXPECT_FALSE(found == SPICETRUE) << "One or more kernels still present in kernel-pool";
}

// Attempt finding a value in kernelpool 
TEST_F(SpiceManagerTest, hasValue) {
    loadPCKKernel();

    int naifId = 399; // Earth

    std::string kernelPoolValue = "RADII";

    bool found = openspace::SpiceManager::ref().hasValue(naifId, kernelPoolValue);
    ASSERT_TRUE(found) << "Could not find value for specified kernel";
}

// Get 1dim value from kernelpool
TEST_F(SpiceManagerTest, getValueFromID_1D) {
    loadPCKKernel();

    std::string target  = "EARTH";
    std::string value1D = "MAG_NORTH_POLE_LAT";

    double return1D;
    ASSERT_NO_THROW(openspace::SpiceManager::ref().getValue(target, value1D, return1D));
    EXPECT_EQ(return1D, 78.565) << "Value not found / differs from expected return";
}

// Get 2dim value from kernelpool
TEST_F(SpiceManagerTest, getValueFromID_3D) {
    loadPCKKernel();

    std::string target  = "EARTH";
    std::string value3D = "RADII";

    glm::dvec3 return3D;
    ASSERT_NO_THROW(openspace::SpiceManager::ref().getValue(target, value3D, return3D));

    EXPECT_EQ(return3D.x, 6378.14) << "Value not found / differs from expected return";
    EXPECT_EQ(return3D.y, 6378.14) << "Value not found / differs from expected return";
    EXPECT_EQ(return3D.z, 6356.75) << "Value not found / differs from expected return";
}

// Get Ndim value from kernelpool
TEST_F(SpiceManagerTest, getValueFromID_ND) {
    loadPCKKernel();

    std::string target  = "SATURN";
    std::string valueND = "RING6_A";

    std::vector<double> returnND(5);
    ASSERT_NO_THROW(openspace::SpiceManager::ref().getValue(target, valueND, returnND));

    std::vector<double> controlVec{ 189870.0, 256900.0, 9000.0, 9000.0, 0.000003 };
    
    ASSERT_EQ(controlVec.size(), returnND.size()) << "Vectors differ in size";

    for (unsigned int i = 0; i < returnND.size(); ++i){
        EXPECT_EQ(controlVec[i], returnND[i]) << "Vector value not equal";
    }
}

// Try converting string to Ephemeris time
TEST_F(SpiceManagerTest, stringToEphemerisTime) {
    loadLSKKernel();

    double ephemerisTime;
    double control_ephemerisTime;
    char   date[SRCLEN] = "Thu Mar 20 12:53:29 PST 1997";
    str2et_c(date, &control_ephemerisTime);

    ASSERT_NO_THROW(ephemerisTime = openspace::SpiceManager::ref().ephemerisTimeFromDate(date));
    
    EXPECT_EQ(ephemerisTime, control_ephemerisTime) << "Ephemeries times differ / not found";
}

// Try getting positional vector of target
TEST_F(SpiceManagerTest, getTargetPosition) {
    using openspace::SpiceManager;
    loadMetaKernel();

    double et;
    double pos[3];
    double lt;
    char utctime[SRCLEN] = "2004 jun 11 19:32:00";

    str2et_c(utctime, &et);
    spkpos_c("EARTH", et, "J2000", "LT+S", "CASSINI", pos, &lt);

    glm::dvec3 targetPosition;
    double lightTime = 0.0;
    SpiceManager::AberrationCorrection corr = {
        SpiceManager::AberrationCorrection::Type::LightTimeStellar,
        SpiceManager::AberrationCorrection::Direction::Reception
    };

    ASSERT_NO_THROW(targetPosition = SpiceManager::ref().targetPosition(
        "EARTH", "CASSINI", "J2000", corr, et, lightTime)
    );
    EXPECT_DOUBLE_EQ(pos[0], targetPosition[0]) << "Position not found or differs from expected return";
    EXPECT_DOUBLE_EQ(pos[1], targetPosition[1]) << "Position not found or differs from expected return";
    EXPECT_DOUBLE_EQ(pos[2], targetPosition[2]) << "Position not found or differs from expected return";
}

// Try getting position & velocity vectors of target
TEST_F(SpiceManagerTest, getTargetState) {
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
    ASSERT_NO_THROW(res = SpiceManager::ref().targetState("EARTH", "CASSINI", "J2000", corr, et));

    // x,y,z
    for (int i = 0; i < 3; i++){
        EXPECT_DOUBLE_EQ(state[i], res.position[i])   << "Position not found or differs from expected return";
        EXPECT_DOUBLE_EQ(state[i+3], res.velocity[i]) << "Velocity not found or differs from expected return";
    }
}

// Try getting transformation matrix and transform position and velocity into new reference frame
//TEST_F(SpiceManagerTest, getStateTransformMatrix) {
//    loadMetaKernel();
//
//    double et;
//    double state[6];
//    double state_t[6];
//    double lt;
//    double referenceMatrix[6][6];
//
//    str2et_c("2004 jun 11 19:32:00", &et);
//    spkezr_c("PHOEBE", et, "J2000", "LT+S", "CASSINI", state, &lt);
//    sxform_c("J2000", "IAU_PHOEBE", et, referenceMatrix);
//
//    glm::dvec3 position(state[0], state[1], state[2]);
//    glm::dvec3 velocity(state[3], state[4], state[5]);
//
//    openspace::SpiceManager::TransformMatrix stateMatrix;
//    ASSERT_NO_THROW(stateMatrix = openspace::SpiceManager::ref().stateTransformMatrix(
//        "J2000", "IAU_PHOEBE", et));
//    
//    // check for matrix consistency
//    for (int i = 0; i < 6; i++) {
//        for (int j = 0; j < 6; j++) {
//            EXPECT_DOUBLE_EQ(referenceMatrix[i][j], stateMatrix[i * 6 + j]) << "State-matrix not set or has wrong values";
//        }
//    }
//    mxvg_c(referenceMatrix, state, 6, 6, state_t);
//
//
//    for (int i = 0; i < 3; i++) {
//        EXPECT_DOUBLE_EQ(position[i], state_t[i])     << "Position vector differs from its reference";
//        EXPECT_DOUBLE_EQ(velocity[i], state_t[i + 3]) << "Velocity vector differs from its reference";
//    }
//}

// Try getting transformation matrix and transform the position only into new reference frame
TEST_F(SpiceManagerTest, getPositionTransformMatrix) {
    using openspace::SpiceManager;
    loadMetaKernel();

    double et;
    double state[3] = { 1.0, 1.0, 1.0 };
    double state_t[3];
    double referenceMatrix[3][3];

    str2et_c("2004 jun 11 19:32:00", &et);
    pxform_c("CASSINI_HGA", "J2000", et, referenceMatrix);

    glm::dmat3 positionMatrix;
    glm::dvec3 position(state[0], state[1], state[2]);
    ASSERT_NO_THROW(positionMatrix = SpiceManager::ref().positionTransformMatrix(
        "CASSINI_HGA", "J2000", et)
    );

    // check for matrix consistency
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            EXPECT_DOUBLE_EQ(referenceMatrix[i][j], positionMatrix[j][i]) << "Position-matrix not set or has wrong values";
        }
    }
    // transform reference position into new frame
    mxvg_c(referenceMatrix, state, 3, 3, state_t);

    position = positionMatrix * position;
    // check transformed values match 
    for (int i = 0; i < 3; i++) {
        EXPECT_DOUBLE_EQ(position[i], state_t[i]) << "Position vector differs from its reference";
    }
}

// Try to get boresight vector and instrument field of view boundary vectors
TEST_F(SpiceManagerTest, getFieldOfView) {
    using openspace::SpiceManager;
    loadMetaKernel();
    
    SpiceInt n;
    SpiceInt cassini_ID;
    double et;
    glm::dvec3 boresight;
    double bounds_ref[5][3];
    char shape_ref[TYPLEN];
    char name_ref[FILLEN];
    double boresightVec[3];

    str2et_c("2004 jun 11 19:32:00", &et);
    SpiceBoolean found;
    bodn2c_c("CASSINI_ISS_NAC", &cassini_ID, &found);
    ASSERT_TRUE(found == SPICETRUE) << "Cannot locate ID for Cassini";

    getfov_c(cassini_ID, 5, TYPLEN, TYPLEN, shape_ref, name_ref, boresightVec, &n, bounds_ref);

    SpiceManager::FieldOfViewResult res;

    ASSERT_NO_THROW(res = SpiceManager::ref().fieldOfView("CASSINI_ISS_NAC"));

    ASSERT_TRUE(found == SPICETRUE);
    //check vectors have correct values
    for (int i = 0; i < res.bounds.size(); i++){
        for (int j = 0; j < 3; j++){
            EXPECT_DOUBLE_EQ(bounds_ref[i][j], res.bounds[i][j]) << "One or more Field of View Boundary vectors \
                                                                 differ from expected output";
        }
    }
}
