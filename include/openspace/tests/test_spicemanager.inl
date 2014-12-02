/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#include <ghoul/filesystem/filesystem.h>
#include "gtest/gtest.h"
#include "openspace/util/spicemanager.h"

class SpiceManagerTest : public testing::Test{
protected:
	SpiceManagerTest(){
		openspace::SpiceManager::initialize();
	}
	~SpiceManagerTest(){
		openspace::SpiceManager::deinitialize();
	}

	void reset() {
		openspace::SpiceManager::deinitialize();
		openspace::SpiceManager::initialize();
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


// Shorthand-path definitions
#define LSK  absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
#define PCK  absPath("${TESTDIR}/SpiceTest/spicekernels/cpck05Mar2004.tpc")
#define META absPath("${TESTDIR}/SpiceTest/spicekernels/metaKernel.tm")

// In this testclass only a handset of the testfunctions require a single kernel.
// The remaining methods rely on multiple kernels, loaded as a SPICE 'meta-kernel'.
#define KERNEL(param) int kernelID = -1; \
	                        kernelID = openspace::SpiceManager::ref().loadKernel(param); \
	                        EXPECT_TRUE(kernelID != -1) << "loadKernel did not return proper id"; \
	                        return kernelID; \

int loadMetaKernel() { KERNEL(META); }
int loadLSKKernel()  { KERNEL(LSK); }
int loadPCKKernel()  { KERNEL(PCK); }

std::string fileType(char type[]){
	std::string str(type);
	return str;
}
// Try loading single kernel
TEST_F(SpiceManagerTest, loadSingleKernel){
	loadLSKKernel();
	//naif0008.tls is a text file, check if loaded.
	SpiceBoolean found;
	kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
	
	ASSERT_TRUE(found == SPICETRUE) << "Kernel not loaded";
	unload_c(LSK.c_str());
}

// Try loading multiple kernels via META file
TEST_F(SpiceManagerTest, loadMetaKernel){
	loadMetaKernel();
	// typeArr[] has values ordered to match each type of kernel 
	// as specified in the 'metaKernel.tm' file 
	std::string typeArr[nrMetaKernels] = { "META", "TEXT", "TEXT",
		                                   "SPK", "SPK", "SPK", 
							               "TEXT", "CK", "TEXT" };
	// If one of the kernels does not load we expect a mismatch
	for (int i = 0; i < nrMetaKernels; i++){
		SpiceBoolean found;
		kdata_c(i, "all", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
		EXPECT_EQ(fileType(filtyp), typeArr[i]) << "One or more kernels did not load properly";
	}
	unload_c(META.c_str());
}
// Try unloading kernel using user assigned keyword
TEST_F(SpiceManagerTest, unloadKernelString){
	loadLSKKernel();
	//naif0008.tls is a text file, check if loaded.
	SpiceBoolean found;
	kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
	ASSERT_TRUE(found == SPICETRUE);

	//unload using string keyword
	openspace::SpiceManager::ref().unloadKernel(LSK);

	found = SPICEFALSE;
	kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
	EXPECT_FALSE(found == SPICETRUE);
}
// Try unloading kernel using integer as ID
TEST_F(SpiceManagerTest, unloadKernelInteger){
	int kernelID = loadLSKKernel();
	//naif0008.tls is a text file, check if loaded.
	SpiceBoolean found;
	kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
	ASSERT_TRUE(found == SPICETRUE);

	//unload using unique int ID
	openspace::SpiceManager::ref().unloadKernel(kernelID);

	found = SPICEFALSE;
	kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
	EXPECT_FALSE(found == SPICETRUE) << "One or more kernels still present in kernel-pool";
}
// Try unloading multiple kernels
TEST_F(SpiceManagerTest, unloadMetaKernel){
	loadMetaKernel();
	// The metakernel loads these kerneltypes in the exact order as in typeArr
	std::string typeArr[nrMetaKernels] = { "META", "TEXT", "TEXT",
		                                   "SPK", "SPK", "SPK",
		                                   "TEXT", "CK", "TEXT" };

	for (int i = 0; i < nrMetaKernels; i++){
		// check kernelpool against typeArr
		SpiceBoolean found;
		kdata_c(i, "all", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
		EXPECT_EQ(fileType(filtyp), typeArr[i]) << "One or more kernels did not load properly";
	}
	openspace::SpiceManager::ref().unloadKernel(META);

	for (int i = 0; i < nrMetaKernels; i++){
		// the values should by now be unloaded
		SpiceBoolean found;
		kdata_c(i, "all", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
		EXPECT_FALSE(found == SPICETRUE) << "Failed unloading kernel";
	}
	unload_c(META.c_str());
}
// Attempt finding a value in kernelpool 
TEST_F(SpiceManagerTest, hasValue){
	loadPCKKernel();

	int naifId = 399; //Earth

	std::string kernelPoolValue = "RADII";

	bool found = openspace::SpiceManager::ref().hasValue(naifId, kernelPoolValue);
	ASSERT_TRUE(found) << "Could not find value for specified kernel";
	unload_c(PCK.c_str());
}
// Get 1dim value from kernelpool
TEST_F(SpiceManagerTest, getValueFromID_1D){
	loadPCKKernel();

	std::string target  = "EARTH";
	std::string value1D = "MAG_NORTH_POLE_LAT";

	double return1D;
	bool found = openspace::SpiceManager::ref().getValue(target, value1D, return1D);
	ASSERT_TRUE(found) << "Could not retrieve value";
	EXPECT_EQ(return1D, 78.565) << "Value not found / differs from expected return";
	unload_c(PCK.c_str());
}
// Get 2dim value from kernelpool
TEST_F(SpiceManagerTest, getValueFromID_3D){
	loadPCKKernel();

	std::string target  = "EARTH";
	std::string value3D = "RADII";

	glm::dvec3 return3D;
    openspace::SpiceManager::ref().getValue(target, value3D, return3D);

	EXPECT_EQ(return3D.x, 6378.14) << "Value not found / differs from expected return";
	EXPECT_EQ(return3D.y, 6378.14) << "Value not found / differs from expected return";
	EXPECT_EQ(return3D.z, 6356.75) << "Value not found / differs from expected return";
	unload_c(PCK.c_str());
}
// Get Ndim value from kernelpool
TEST_F(SpiceManagerTest, getValueFromID_ND){
	loadPCKKernel();

	std::string target  = "SATURN";
	std::string valueND = "RING6_A";

	std::vector<double> returnND(5);
	bool found = openspace::SpiceManager::ref().getValue(target, valueND, returnND);
	ASSERT_TRUE(found) << "Could not retrieve value for specified kernel";

	std::vector<double> controlVec{ 189870.0, 256900.0, 9000.0, 9000.0, 0.000003 };
	
	ASSERT_EQ(controlVec.size(), returnND.size()) << "Vectors differ in size";

	for (unsigned int i = 0; i < returnND.size(); ++i){
		EXPECT_EQ(controlVec[i], returnND[i]) << "Vector value not equal";
	}
	unload_c(PCK.c_str());
}
// Try converting string to Ephemeris time
TEST_F(SpiceManagerTest, stringToEphemerisTime){
	loadLSKKernel();

	double ephemerisTime;
	double control_ephemerisTime;
	char   date[SRCLEN] = "Thu Mar 20 12:53:29 PST 1997";
	str2et_c(date, &control_ephemerisTime);

	bool success = openspace::SpiceManager::ref().getETfromDate(date, ephemerisTime);
	EXPECT_EQ(success, true);
	
	EXPECT_EQ(ephemerisTime, control_ephemerisTime) << "Ephemeries times differ / not found";
	unload_c(LSK.c_str());
}
// Try getting positional vector of target
TEST_F(SpiceManagerTest, getTargetPosition){
	loadMetaKernel();

	double et;
	double pos[3];
	double lt;
	char utctime[SRCLEN] = "2004 jun 11 19:32:00";

	str2et_c(utctime, &et);
	spkpos_c("EARTH", et, "J2000", "LT+S", "CASSINI", pos, &lt);

	glm::dvec3 targetPosition;
	double lightTime = 0.0;
	bool found = openspace::SpiceManager::ref().getTargetPosition("EARTH", "CASSINI", "J2000", "LT+S", et,
		targetPosition, lightTime);
	ASSERT_TRUE(found);
	EXPECT_DOUBLE_EQ(pos[0], targetPosition[0]) << "Position not found or differs from expected return";
	EXPECT_DOUBLE_EQ(pos[1], targetPosition[1]) << "Position not found or differs from expected return";
	EXPECT_DOUBLE_EQ(pos[2], targetPosition[2]) << "Position not found or differs from expected return";
	unload_c(META.c_str());
}
// Try getting position & velocity vectors of target
TEST_F(SpiceManagerTest, getTargetState){
	loadMetaKernel();

	double et;
	double state[6];
	double lt;
	char utctime[SRCLEN] = "2004 jun 11 19:32:00";

	str2et_c(utctime, &et);
	spkezr_c("EARTH", et, "J2000", "LT+S", "CASSINI", state, &lt);

	glm::dvec3 targetPosition;
	glm::dvec3 targetVelocity;
	double lightTime = 0.0;
	bool found = openspace::SpiceManager::ref().getTargetState("EARTH", "CASSINI", "J2000", "LT+S", et, 
		                                                  targetPosition, targetVelocity, lightTime);
	ASSERT_TRUE(found);
	//x,y,z
	for (int i = 0; i < 3; i++){
		EXPECT_DOUBLE_EQ(state[i], targetPosition[i])   << "Position not found or differs from expected return";
		EXPECT_DOUBLE_EQ(state[i+3], targetVelocity[i]) << "Velocity not found or differs from expected return";
	}
	unload_c(META.c_str());
}
// Try getting transformation matrix and transform position and velocity into new reference frame
TEST_F(SpiceManagerTest, getStateTransformMatrix){
	loadMetaKernel();

	double et;
	double state[6];
	double state_t[6];
	double lt;
	double referenceMatrix[6][6];

	str2et_c("2004 jun 11 19:32:00", &et);
	spkezr_c("PHOEBE", et, "J2000", "LT+S", "CASSINI", state, &lt);
	sxform_c("J2000", "IAU_PHOEBE", et, referenceMatrix);

	glm::dvec3 position(state[0], state[1], state[2]);
	glm::dvec3 velocity(state[3], state[4], state[5]);

	openspace::SpiceManager::TransformMatrix stateMatrix;
	bool found = openspace::SpiceManager::ref().getStateTransformMatrix("J2000",
		                                                           "IAU_PHOEBE",
		                                                           et,
		                                                           stateMatrix);
	ASSERT_TRUE(found);
	//check for matrix consistency
	for (int i = 0; i < 6; i++){
		for (int j = 0; j < 6; j++){
			EXPECT_DOUBLE_EQ(referenceMatrix[i][j], stateMatrix[i * 6 + j]) << "State-matrix not set or has wrong values";
		}
	}
	mxvg_c(referenceMatrix, state, 6, 6, state_t);

	openspace::SpiceManager::ref().applyTransformationMatrix(position, velocity, stateMatrix);

	for (int i = 0; i < 3; i++){
		EXPECT_DOUBLE_EQ(position[i], state_t[i])     << "Position vector differs from its reference";
		EXPECT_DOUBLE_EQ(velocity[i], state_t[i + 3]) << "Velocity vector differs from its reference";
	}
	unload_c(META.c_str());
}
// Try getting transformation matrix and transform the position only into new reference frame
TEST_F(SpiceManagerTest, getPositionTransformMatrix){
	loadMetaKernel();

	double et;
	double state[3] = { 1.0, 1.0, 1.0 };
	double state_t[3];
	double referenceMatrix[3][3];

	str2et_c("2004 jun 11 19:32:00", &et);
	pxform_c("CASSINI_HGA", "J2000", et, referenceMatrix);

	glm::dmat3 positionMatrix;
	glm::dvec3 position(state[0], state[1], state[2]);
	bool found = openspace::SpiceManager::ref().getPositionTransformMatrix("CASSINI_HGA", 
		                                                              "J2000", 
																	  et, 
																	  positionMatrix);
	ASSERT_TRUE(found);
	//check for matrix consistency
	for (int i = 0; i < 3; i++){
		for (int j = 0; j < 3; j++){
			EXPECT_DOUBLE_EQ(referenceMatrix[i][j], positionMatrix[j][i]) << "Position-matrix not set or has wrong values";
		}
	}
	//transform reference position into new frame
	mxvg_c(referenceMatrix, state, 3, 3, state_t);

	position = positionMatrix * position;
	//check transformed values match 
	for (int i = 0; i < 3; i++){
		EXPECT_DOUBLE_EQ(position[i], state_t[i]) << "Position vector differs from its reference";
	}
	unload_c(META.c_str());
}
// Try to get boresight vector and instrument field of view boundary vectors
TEST_F(SpiceManagerTest, getFieldOfView){
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

	std::string shape, name;
	shape.resize(32);
	name.resize(32);
	std::vector<glm::dvec3> bounds;
	found = openspace::SpiceManager::ref().getFieldOfView("CASSINI_ISS_NAC", 
		                                                  shape, 
														  name, 
														  boresight, 
														  bounds);
	ASSERT_TRUE(found == SPICETRUE);
	//check vectors have correct values
	for (int i = 0; i < bounds.size(); i++){
		for (int j = 0; j < 3; j++){
			EXPECT_DOUBLE_EQ(bounds_ref[i][j], bounds[i][j]) << "One or more Field of View Boundary vectors \
																 differ from expected output";
		}
	}
	unload_c(META.c_str());
}
// Try to convert planetocentric coordinates to rectangular
TEST_F(SpiceManagerTest, planetocentricToRectangular){
	loadPCKKernel();

	double lat = -35.0; //initial values
	double lon = 100.0;
	double rectangular_ref[3];
	SpiceInt naifId;
	SpiceBoolean foundSpice;

	bodn2c_c("EARTH", &naifId, &foundSpice);
	ASSERT_TRUE(foundSpice == SPICETRUE);
	srfrec_c(naifId, lon*rpd_c(), lat*rpd_c(), rectangular_ref);

	glm::dvec3 rectangular;
	bool found = openspace::SpiceManager::ref().geographicToRectangular("EARTH", lon, lat, rectangular);
	ASSERT_TRUE(found);
	
	for (int i = 0; i < 3; i++){
		EXPECT_EQ(rectangular[i], rectangular_ref[i]) << "Rectangular coordinates differ from expected output";
	}
	unload_c(PCK.c_str());
}
// Try getting sub-observer point
TEST_F(SpiceManagerTest, getSubObserverPoint){
	loadMetaKernel();

	double et;
	double targetEt_ref;
	double targetEt;
	double subObserverPoint_ref[3];
	double vectorToSurfacePoint_ref[3];
	static SpiceChar * method[2] = { "Intercept:  ellipsoid", "Near point: ellipsoid" };

	str2et_c("2004 jun 11 19:32:00", &et);

	glm::dvec3 subObserverPoint;
	glm::dvec3 vectorToSurfacePoint;

	for (int i = 0; i < 2; i++){
		subpnt_c(method[i], "phoebe", et, "iau_phoebe", 
			"lt+s", "earth", subObserverPoint_ref, &targetEt_ref, vectorToSurfacePoint_ref);

		bool found = openspace::SpiceManager::ref().getSubObserverPoint(
			"phoebe", "earth", method[i], "iau_phoebe", "lt+s", et, subObserverPoint, 
			targetEt, vectorToSurfacePoint);
		ASSERT_TRUE(found);
		EXPECT_EQ(targetEt_ref, targetEt);
		for (int i = 0; i < 3; i++){
			EXPECT_EQ(subObserverPoint_ref[i], subObserverPoint[i]) 
				      << "Sub-observer vector differs from its reference";
			EXPECT_EQ(vectorToSurfacePoint_ref[i], vectorToSurfacePoint[i]) 
				      << "Observer to surface point vector differs from its reference";
		}
	}
	unload_c(META.c_str());
}
// Try getting sub-solar point
//TEST_F(SpiceManagerTest, getSubSolarPoint){
//	loadMetaKernel();
//
//	double et, targetEt_ref, targetEt;
//	double subSolarPoint_ref[3];
//	double vectorToSurfacePoint_ref[3];
//	static SpiceChar * method[2] = { "Intercept:  ellipsoid", "Near point: ellipsoid" };
//
//	str2et_c("2004 jun 11 19:32:00", &et);
//
//	glm::dvec3 subSolarPoint;
//	glm::dvec3 vectorToSurfacePoint;
//
//	for (int i = 0; i < 2; i++){
//		subslr_c(method[i], "phoebe", et, "iau_phoebe", 
//			"lt+s", "earth", subSolarPoint_ref, &targetEt_ref, vectorToSurfacePoint_ref);
//
//		bool found = openspace::SpiceManager::ref().getSubSolarPoint(method[i], "phoebe", et, "iau_phoebe",
//			                                                    "lt+s", "earth", subSolarPoint,
//			                                                    targetEt, vectorToSurfacePoint);
//		ASSERT_TRUE(found);
//		EXPECT_EQ(targetEt_ref, targetEt);
//		for (int i = 0; i < 3; i++){
//			EXPECT_EQ(subSolarPoint_ref[i], subSolarPoint[i])
//			     	 << "Sub-solar vector differs from its reference";
//			EXPECT_EQ(vectorToSurfacePoint_ref[i], vectorToSurfacePoint[i])
//				     << "Observer to surface point vector differs from its reference";
//		}
//	}
//	unload_c(META.c_str());
//}
