/*****************************************************************************************
*                                                                                       *
* GHOUL                                                                                 *
* General Helpful Open Utility Library                                                  *
*                                                                                       *
* Copyright (c) 2012-2014                                                               *
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
int which, handle, count = 0;
char file[FILLEN], filtyp[TYPLEN], source[SRCLEN];
int found;
double abs_error = 0.00001;


// Shorthand-path definitions
#define LSK  absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
#define PCK  absPath("${TESTDIR}/SpiceTest/spicekernels/cpck05Mar2004.tpc")
#define META absPath("${TESTDIR}/SpiceTest/spicekernels/metaKernel.tm")

// In this testclass only a handset of the testfunctions require a single kernel.
// The remaining methods rely on multiple kernels, loaded as a SPICE 'meta-kernel'.
#define KERNEL(param, name) int kernelID = -1; \
	                        kernelID = openspace::SpiceManager::ref().loadKernel(param, name); \
	                        EXPECT_TRUE(kernelID != -1) << "loadKernel did not return proper id"; \
	                        return kernelID; \

int loadMetaKernel() { KERNEL(META , "METAKERNEL" ); }
int loadLSKKernel()  { KERNEL(LSK  , "LEAPSECONDS"); }
int loadPCKKernel()  { KERNEL(PCK  , "CASSINI_PCK"); }

std::string fileType(char type[]){
	std::string str(type);
	return str;
}
// Try loading single kernel
TEST_F(SpiceManagerTest, loadSingleKernel){
	loadLSKKernel();
	//naif0008.tls is a text file, check if loaded.
	kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
	
	ASSERT_TRUE(found) << "Kernel not loaded";
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
		kdata_c(i, "all", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
		EXPECT_EQ(fileType(filtyp), typeArr[i]) << "One or more kernels did not load properly";
	}
	unload_c(META.c_str());
}
// Try unloading kernel using user assigned keyword
TEST_F(SpiceManagerTest, unloadKernelString){
	loadLSKKernel();
	//naif0008.tls is a text file, check if loaded.
	kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
	ASSERT_TRUE(found);

	//unload using string keyword
	bool unloaded = openspace::SpiceManager::ref().unloadKernel("LEAPSECONDS");
	EXPECT_TRUE(unloaded);

	kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
	EXPECT_FALSE(found);
}
// Try unloading kernel using integer as ID
TEST_F(SpiceManagerTest, unloadKernelInteger){
	int kernelID = loadLSKKernel();
	//naif0008.tls is a text file, check if loaded.
	kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
	ASSERT_TRUE(found);

	//unload using unique int ID
	bool unloaded = openspace::SpiceManager::ref().unloadKernel(kernelID);
	EXPECT_TRUE(unloaded) << "Kernel did not unload";

	kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
	EXPECT_FALSE(found) << "One or more kernels still present in kernel-pool";
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
		kdata_c(i, "all", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
		EXPECT_EQ(fileType(filtyp), typeArr[i]) << "One or more kernels did not load properly";
	}
	bool unloaded = openspace::SpiceManager::ref().unloadKernel("METAKERNEL");
	EXPECT_TRUE(unloaded);

	for (int i = 0; i < nrMetaKernels; i++){
		// the values should by now be unloaded
		kdata_c(i, "all", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
		EXPECT_FALSE(found) << "Failed unloading kernel";
	}
	unload_c(META.c_str());
}
// Attempt finding a value in kernelpool 
TEST_F(SpiceManagerTest, hasValue){
	loadPCKKernel();

	int n;
	int naifId = 399; //Earth
	double radii[3];

	std::string kernelPoolValue = "RADII";

	found = openspace::SpiceManager::ref().hasValue(naifId, kernelPoolValue);
	ASSERT_TRUE(found) << "Could not find value for specified kernel";
	unload_c(PCK.c_str());
}
// Get 1dim value from kernelpool
TEST_F(SpiceManagerTest, getValueFromID_1D){
	loadPCKKernel();

	std::string target  = "EARTH";
	std::string value1D = "MAG_NORTH_POLE_LAT";

	double return1D;
	found = openspace::SpiceManager::ref().getValueFromID(target, value1D, return1D);
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
    openspace::SpiceManager::ref().getValueFromID(target, value3D, return3D);

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

	std::vector<double> returnND;
	unsigned int nr = 5;
	found = openspace::SpiceManager::ref().getValueFromID(target, valueND, returnND, nr);
	ASSERT_TRUE(found) << "Could not retrieve value for specified kernel";

	std::vector<double> controlVec{ 189870.0, 256900.0, 9000.0, 9000.0, 0.000003 };
	
	ASSERT_EQ(controlVec.size(), returnND.size()) << "Vectors differ in size";

	for (int i = 0; i < nr; i++){
		EXPECT_EQ(controlVec[i], returnND[i]) << "Vector value not equal";
	}
	unload_c(PCK.c_str());
}
// Try converting string to Ephemeris time
TEST_F(SpiceManagerTest, stringToEphemerisTime){
	loadLSKKernel();

	double ephemerisTime;
	double control_ephemerisTime;
	char   *date = "Thu Mar 20 12:53:29 PST 1997";
	str2et_c(date, &control_ephemerisTime);

	ephemerisTime = openspace::SpiceManager::ref().stringToEphemerisTime(date);
	
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
	found = openspace::SpiceManager::ref().getTargetPosition("EARTH", et, "J2000", "LT+S", "CASSINI",
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
	found = openspace::SpiceManager::ref().getTargetState("EARTH", et, "J2000", "LT+S", "CASSINI",
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

	openspace::transformMatrix stateMatrix(6);
	found = openspace::SpiceManager::ref().getStateTransformMatrix("J2000",
		                                                           "IAU_PHOEBE",
		                                                           et,
		                                                           stateMatrix);
	ASSERT_TRUE(found);
	//check for matrix consistency
	for (int i = 0; i < 6; i++){
		for (int j = 0; j < 6; j++){
			EXPECT_DOUBLE_EQ(referenceMatrix[i][j], stateMatrix(i, j)) << "State-matrix not set or has wrong values";
		}
	}
	mxvg_c(referenceMatrix, state, 6, 6, state_t);

	stateMatrix.transform(position, velocity);

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
	double lt;
	double state[3] = { 1.0, 1.0, 1.0 };
	double state_t[3];
	double referenceMatrix[3][3];

	str2et_c("2004 jun 11 19:32:00", &et);
	pxform_c("CASSINI_HGA", "J2000", et, referenceMatrix);

	openspace::transformMatrix positionMatrix(3);
	glm::dvec3 position(state[0], state[1], state[2]);
	found = openspace::SpiceManager::ref().getPositionTransformMatrix("CASSINI_HGA", 
		                                                              "J2000", 
																	  et, 
																	  positionMatrix);
	ASSERT_TRUE(found);
	//check for matrix consistency
	for (int i = 0; i < 3; i++){
		for (int j = 0; j < 3; j++){
			EXPECT_DOUBLE_EQ(referenceMatrix[i][j], positionMatrix(i, j)) << "Position-matrix not set or has wrong values";
		}
	}
	//transform reference position into new frame
	mxvg_c(referenceMatrix, state, 3, 3, state_t);

	positionMatrix.transform(position);
	//check transformed values match 
	for (int i = 0; i < 3; i++){
		EXPECT_DOUBLE_EQ(position[i], state_t[i]) << "Position vector differs from its reference";
	}
	unload_c(META.c_str());
}
// Try to get boresight vector and instrument field of view boundary vectors
TEST_F(SpiceManagerTest, getFieldOfView){
	loadMetaKernel();
	
	int n;
	int cassini_ID;
	double et;
	double lt;
	double boresight[3];
	double bounds_ref[5][3];
	char shape_ref[TYPLEN];
	char name_ref[FILLEN];

	str2et_c("2004 jun 11 19:32:00", &et);
	bodn2c_c("CASSINI_ISS_NAC", &cassini_ID, &found);
	if (!found){
		printf("error cannot locate ID for Cassini \n");
	}
	getfov_c(cassini_ID, 5, TYPLEN, TYPLEN, shape_ref, name_ref, boresight, &n, bounds_ref);

	std::string shape, name;
	shape.resize(32);
	name.resize(32);
	std::vector<glm::dvec3> bounds;
	int nrReturned;
	found = openspace::SpiceManager::ref().getFieldOfView("CASSINI_ISS_NAC", 
		                                                  shape, 
														  name, 
														  boresight, 
														  bounds,
														  nrReturned);
	ASSERT_TRUE(found);
	//check vectors have correct values
	for (int i = 0; i < nrReturned; i++){
		for (int j = 0; j < 3; j++){
			EXPECT_DOUBLE_EQ(bounds_ref[i][j], bounds[i][j]) << "One or more Field of View Boundary vectors \
																 differ from expected output";
		}
	}
	unload_c(META.c_str());
}
// Try converting rectangular coordinates to latitudal
TEST_F(SpiceManagerTest, rectangularToLatitudal){
	loadMetaKernel();

	char    frame[FILLEN], shape[FILLEN];
	double  lat, lon;
	double  bounds[4][3], bsight[3], 
		         obspos[3], point_ref[3];
	double  dist, et, radius_ref, trgepc;
	int     n, naifId;
	int     found;

	// First, find an intersection point to convert to rectangular coordinates 
	str2et_c("2004 jun 11 19:32:00", &et);
	bodn2c_c("CASSINI_ISS_NAC", &naifId, &found);
	getfov_c(naifId, 4, FILLEN, FILLEN, shape, frame, bsight, &n, bounds);
	srfxpt_c("Ellipsoid", "PHOEBE", et, "LT+S", "CASSINI", frame, bsight,
		     point_ref, &dist, &trgepc, obspos, &found);

	reclat_c(point_ref, &radius_ref, &lon, &lat);
	glm::dvec3 point(point_ref[0], point_ref[1], point_ref[2]);
	double radius, longitude, latitude;
	found = openspace::SpiceManager::ref().rectangularToLatitudal(point, radius, longitude, latitude);

	ASSERT_TRUE(found);
	ASSERT_NEAR(radius,    radius_ref, abs_error) << "radius is not set / has incorrect values";
	ASSERT_NEAR(longitude, lon,        abs_error) << "longitude is not set / has incorrect values";
	ASSERT_NEAR(latitude,  lat,        abs_error) << "latitude is not set / has incorrect values";
	unload_c(META.c_str());
}
// Try converting latitudinal coordinates to rectangular
TEST_F(SpiceManagerTest, latitudinalToRectangular){
	loadMetaKernel();

	char   frame[FILLEN], shape[FILLEN];
	double lat, lon;
	double bounds[4][3], bsight[3],
		   obspos[3], point_ref[3];
	double dist, et, radius_ref, trgepc;
	int    n, naifId;

	// First, find an intersection point to convert to latitudinal coordinates //
	str2et_c("2004 jun 11 19:32:00", &et);
	bodn2c_c("CASSINI_ISS_NAC", &naifId, &found);
	getfov_c(naifId, 4, FILLEN, FILLEN, shape, frame, bsight, &n, bounds);
	srfxpt_c("Ellipsoid", "PHOEBE", et, "LT+S", "CASSINI", frame, bsight,
		     point_ref, &dist, &trgepc, obspos, &found);

	reclat_c(point_ref, &radius_ref, &lon, &lat);

	lat *= rpd_c();
	lon *= rpd_c();

	double lat_ref = lat;
	double lon_ref = lon;

	double rectangular_ref[3];
	latrec_c(radius_ref, lon, lat, rectangular_ref);

	glm::dvec3 coordinates;
	found = openspace::SpiceManager::ref().latidudinalToRectangular(radius_ref, lon, lat, coordinates);

	ASSERT_TRUE(found);
	ASSERT_NEAR(lon_ref, lon, abs_error) << "longitude is not set / has incorrect values";
	ASSERT_NEAR(lat_ref, lat, abs_error) << "latitude is not set / has incorrect values";
	unload_c(META.c_str());
}
// Try to convert planetocentric coordinates to rectangular
TEST_F(SpiceManagerTest, planetocentricToRectangular){
	loadPCKKernel();

	double lat = -35.0; //initial values
	double lon = 100.0;
	double rectangular_ref[3];
	double radius;
	int naifId;

	bodn2c_c("EARTH", &naifId, &found);
	srfrec_c(naifId, lon*rpd_c(), lat*rpd_c(), rectangular_ref);

	glm::dvec3 rectangular;
	found = openspace::SpiceManager::ref().planetocentricToRectangular("EARTH", lon, lat, rectangular);
	
	for (int i = 0; i < 3; i++){
		EXPECT_EQ(rectangular[i], rectangular_ref[i]) << "Rectangular coordinates differ from expected output";
	}
	unload_c(PCK.c_str());
}
// Try getting sub-observer point
TEST_F(SpiceManagerTest, getSubObserverPoint){
	loadMetaKernel();

	double et, targetEt_ref, targetEt;
	double radii[3], subObserverPoint_ref[3], vectorToSurfacePoint_ref[3];
	static SpiceChar * method[2] = { "Intercept:  ellipsoid", "Near point: ellipsoid" };

	str2et_c("2004 jun 11 19:32:00", &et);

	glm::dvec3 subObserverPoint;
	glm::dvec3 vectorToSurfacePoint;

	for (int i = 0; i < 2; i++){
		subpnt_c(method[i], "phoebe", et, "iau_phoebe", 
			"lt+s", "earth", subObserverPoint_ref, &targetEt_ref, vectorToSurfacePoint_ref);

		found = openspace::SpiceManager::ref().getSubObserverPoint(method[i], "phoebe", et, "iau_phoebe",
			                                                      "lt+s", "earth", subObserverPoint, 
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
TEST_F(SpiceManagerTest, getSubSolarPoint){
	loadMetaKernel();

	double et, targetEt_ref, targetEt;
	double radii[3], subSolarPoint_ref[3], vectorToSurfacePoint_ref[3];
	static SpiceChar * method[2] = { "Intercept:  ellipsoid", "Near point: ellipsoid" };

	str2et_c("2004 jun 11 19:32:00", &et);

	glm::dvec3 subSolarPoint;
	glm::dvec3 vectorToSurfacePoint;

	for (int i = 0; i < 2; i++){
		subslr_c(method[i], "phoebe", et, "iau_phoebe", 
			"lt+s", "earth", subSolarPoint_ref, &targetEt_ref, vectorToSurfacePoint_ref);

		found = openspace::SpiceManager::ref().getSubSolarPoint(method[i], "phoebe", et, "iau_phoebe",
			                                                    "lt+s", "earth", subSolarPoint,
			                                                    targetEt, vectorToSurfacePoint);
		ASSERT_TRUE(found);
		EXPECT_EQ(targetEt_ref, targetEt);
		for (int i = 0; i < 3; i++){
			EXPECT_EQ(subSolarPoint_ref[i], subSolarPoint[i])
			     	 << "Sub-solar vector differs from its reference";
			EXPECT_EQ(vectorToSurfacePoint_ref[i], vectorToSurfacePoint[i])
				     << "Observer to surface point vector differs from its reference";
		}
	}
	unload_c(META.c_str());
}
