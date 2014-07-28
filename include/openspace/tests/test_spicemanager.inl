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

		// in the reset method, deinitialize it and 
		// initialize it again to remove all kernels
	}
};

#define  FILLEN   128
#define  TYPLEN   32
#define  SRCLEN   128

const int nrMetaKernels = 9;
int which, handle, count = 0;
char file[FILLEN], filtyp[TYPLEN], source[SRCLEN];
int found;


//TODO: not hardcoded path cpck05Mar2004.tpc
#define LSK  absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
#define PCK  absPath("${TESTDIR}/SpiceTest/spicekernels/cpck05Mar2004.tpc")
#define META absPath("${TESTDIR}/SpiceTest/spicekernels/metaKernel.tm")

std::string fileType(char type[]){
	std::string str(type);
	return str;
}

TEST_F(SpiceManagerTest, loadSingleKernel){
	int kernelID = -1;
	kernelID = openspace::SpiceManager::ref().loadKernel(LSK, "LEAPSECONDS");
	EXPECT_TRUE( kernelID == 1 ) << "loadKernel did not return proper id";

	//naif0008.tls is a text file, check if loaded.
	kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
	
	EXPECT_TRUE(found) << "Kernel not loaded";
	unload_c(LSK.c_str());
}


TEST_F(SpiceManagerTest, loadMetaKernel){
	int kernelID = -1;
	kernelID = openspace::SpiceManager::ref().loadKernel(META, "LEAPSECONDS");
	EXPECT_TRUE(kernelID != -1) << "loadKernel did not return proper id";


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

TEST_F(SpiceManagerTest, unloadKernelString){
	int kernelID = -1;
	kernelID = openspace::SpiceManager::ref().loadKernel(LSK, "LEAPSECONDS");
	EXPECT_TRUE(kernelID == 1) << "loadKernel did not return proper id";
	
	//naif0008.tls is a text file, check if loaded.
	kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
	EXPECT_TRUE(found);

	bool unloaded = openspace::SpiceManager::ref().unloadKernel("LEAPSECONDS");
	EXPECT_TRUE(unloaded);

	kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
	EXPECT_FALSE(found);
}

TEST_F(SpiceManagerTest, unloadKernelInteger){
	int kernelID = -1;
	kernelID = openspace::SpiceManager::ref().loadKernel(LSK, "LEAPSECONDS");
	EXPECT_TRUE(kernelID == 1) << "loadKernel did not return proper id";

	//naif0008.tls is a text file, check if loaded.
	kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
	EXPECT_TRUE(found);

	bool unloaded = openspace::SpiceManager::ref().unloadKernel(kernelID);
	EXPECT_TRUE(unloaded);

	kdata_c(0, "text", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
	EXPECT_FALSE(found);
}

TEST_F(SpiceManagerTest, unloadMetaKernel){
	int kernelID = -1;
	kernelID = openspace::SpiceManager::ref().loadKernel(META, "METAKERNEL");
	EXPECT_TRUE(kernelID == 1) << "loadKernel did not return proper id";

	std::string typeArr[nrMetaKernels] = { "META", "TEXT", "TEXT",
		                                   "SPK", "SPK", "SPK",
		                                   "TEXT", "CK", "TEXT" };

	for (int i = 0; i < nrMetaKernels; i++){
		kdata_c(i, "all", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
		EXPECT_EQ(fileType(filtyp), typeArr[i]) << "One or more kernels did not load properly";
	}
	bool unloaded = openspace::SpiceManager::ref().unloadKernel("METAKERNEL");
	EXPECT_TRUE(unloaded);

	for (int i = 0; i < nrMetaKernels; i++){
		kdata_c(i, "all", FILLEN, TYPLEN, SRCLEN, file, filtyp, source, &handle, &found);
		EXPECT_FALSE(found) << "Failed unloading kernel";
	}
	unload_c(META.c_str());
}

TEST_F(SpiceManagerTest, hasValue){
	int kernelID = -1;
	kernelID = openspace::SpiceManager::ref().loadKernel(PCK, "CASSINI_PCK");
	EXPECT_TRUE(kernelID == 1) << "loadKernel did not return proper id";

	SpiceInt n;
	SpiceInt naifId = 399; //Earth
	SpiceDouble radii[3];

	std::string kernelPoolValue = "RADII";

	found = openspace::SpiceManager::ref().hasValue(naifId, kernelPoolValue);
	EXPECT_TRUE(found);
	unload_c(META.c_str());
}

TEST_F(SpiceManagerTest, getValueFromID_1D){
	int kernelID = -1;
	kernelID = openspace::SpiceManager::ref().loadKernel(PCK, "CASSINI_PCK");
	EXPECT_TRUE(kernelID == 1) << "loadKernel did not return proper id";

	std::string target  = "EARTH";
	std::string value1D = "MAG_NORTH_POLE_LAT";

	double return1D;
	openspace::SpiceManager::ref().getValueFromID(target, value1D, return1D);
	EXPECT_EQ(return1D, 78.565);
	unload_c(META.c_str());
}

TEST_F(SpiceManagerTest, getValueFromID_3D){
	int kernelID = -1;
	kernelID = openspace::SpiceManager::ref().loadKernel(PCK, "CASSINI_PCK");
	EXPECT_TRUE(kernelID == 1) << "loadKernel did not return proper id";

	std::string target  = "EARTH";
	std::string value3D = "RADII";

	glm::dvec3 return3D;
    openspace::SpiceManager::ref().getValueFromID(target, value3D, return3D);

	EXPECT_EQ(return3D.x, 6378.14);
	EXPECT_EQ(return3D.y, 6378.14);
	EXPECT_EQ(return3D.z, 6356.75);
	unload_c(META.c_str());
}

TEST_F(SpiceManagerTest, getValueFromID_ND){
	int kernelID = -1;
	kernelID = openspace::SpiceManager::ref().loadKernel(PCK, "CASSINI_PCK");
	EXPECT_TRUE(kernelID == 1) << "loadKernel did not return proper id";

	std::string target  = "SATURN";
	std::string valueND = "RING6_A";

	std::vector<double> returnND;
	unsigned int nr = 5;
	found = openspace::SpiceManager::ref().getValueFromID(target, valueND, returnND, nr);
	EXPECT_TRUE(found);

	std::vector<double> controlVec{ 189870.0, 256900.0, 9000.0, 9000.0, 0.000003 };
	
	ASSERT_EQ(controlVec.size(), returnND.size()) << "Vectors differ in size";

	for (int i = 0; i < nr; i++){
		EXPECT_EQ(controlVec[i], returnND[i]) << "Vector value not equal";
	}
	unload_c(META.c_str());
}

TEST_F(SpiceManagerTest, stringToEphemerisTime){
	int kernelID = -1;
	kernelID = openspace::SpiceManager::ref().loadKernel(LSK, "LEAPSECONDS");
	EXPECT_TRUE(kernelID == 1) << "loadKernel did not return proper id";

	double ephemerisTime;
	double control_ephemerisTime;
	char   *date = "Thu Mar 20 12:53:29 PST 1997";
	str2et_c(date, &control_ephemerisTime);

	ephemerisTime = openspace::SpiceManager::ref().stringToEphemerisTime(date);
	
	EXPECT_EQ(ephemerisTime, control_ephemerisTime);
	unload_c(META.c_str());
}

TEST_F(SpiceManagerTest, getTargetPosition){
	int kernelID = -1;
	kernelID = openspace::SpiceManager::ref().loadKernel(META, "METAKERNEL");
	EXPECT_TRUE(kernelID == 1) << "loadKernel did not return proper id";

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
	EXPECT_DOUBLE_EQ(pos[0], targetPosition[0]);
	EXPECT_DOUBLE_EQ(pos[1], targetPosition[1]);
	EXPECT_DOUBLE_EQ(pos[2], targetPosition[2]);
	unload_c(META.c_str());
}

TEST_F(SpiceManagerTest, getTargetState){
	int kernelID = -1;
	kernelID = openspace::SpiceManager::ref().loadKernel(META, "METAKERNEL");
	EXPECT_TRUE(kernelID == 1) << "loadKernel did not return proper id";

	SpiceDouble et;
	SpiceDouble state[6];
	SpiceDouble lt;
	SpiceChar utctime[SRCLEN] = "2004 jun 11 19:32:00";

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
		EXPECT_DOUBLE_EQ(state[i], targetPosition[i])   << "Position vector is wrong";
		EXPECT_DOUBLE_EQ(state[i+3], targetVelocity[i]) << "Velocity vector is wrong";
	}
	unload_c(META.c_str());
}

TEST_F(SpiceManagerTest, getPositionTransformMatrix){
	int kernelID = -1;
	kernelID = openspace::SpiceManager::ref().loadKernel(META, "METAKERNEL");
	EXPECT_TRUE(kernelID == 1) << "loadKernel did not return proper id";

	SpiceDouble et;
	SpiceDouble state[6];
	SpiceDouble state_t[6];
	SpiceDouble lt;
	SpiceDouble referenceMatrix[6][6];


	str2et_c("2004 jun 11 19:32:00", &et);
	spkezr_c("PHOEBE", et, "J2000", "LT+S", "CASSINI", state, &lt);
	sxform_c("J2000", "IAU_PHOEBE", et, referenceMatrix);

	glm::mat3x3 positionMatrix;
	glm::mat3x3 velocityMatrix;

	openspace::mat6x6 stateMatrix;
	found = openspace::SpiceManager::ref().getStateTransformMatrix("J2000",
		                                                           "IAU_PHOEBE",
		                                                           et,
																   stateMatrix);
	ASSERT_TRUE(found);
	double absolute_range = 0.00000001;

	//check for matrix consistency
	for (int i = 0; i < 6; i++){
		for (int j = 0; j < 6; j++){
			ASSERT_NEAR(referenceMatrix[i][j], stateMatrix[i][j], absolute_range)
				 << "Assertion failed at [" << i << ", "  << j << "]"<< std::endl;
		}
	}

	openspace::matrix6 abc;

	abc.data[0][0] = 10;
}

TEST_F(SpiceManagerTest, dontknowyet){

}