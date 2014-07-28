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

#ifndef __SPICEWRAPPER_H__
#define __SPICEWRAPPER_H__

#include "SpiceUsr.h"

#include <string>
#include <ghoul/glm.h>
#include <vector>
#include <map>

namespace openspace{

//just a typedef for now..
typedef double mat6x6[6][6];

class matrix6{
private:
	int row, col;
	double data[6][6];
public: 
	matrix6& operator= (const matrix6& lhs){
		if (this != &lhs)
	}
};

class SpiceManager{
public:
// Initialization
	/**
	* Static initializer that initializes the static member. 
	*/
	static void initialize();
	static void deinitialize();
	static SpiceManager& ref();
	
	/**
	 *  Load on or more SPICE kernels into a program. If client provides
	 *  the path to a binary kernel or meta-kernel upon which its loaded 
	 *  to the appropriate SPICE subsystem. if the file is a textkernel 
	 *  it will be loaded into kernel pool. 
	 *  In order to locate the spice kernels, the method temorarily changes the 
	 *  current working directory to the client-provided <code>filePath</code>. 
	 *  For further details, please refer to <code>furnsh_c</code> in SPICE Docummentation
	 *  
	 * \param filePath  path to single kernel or meta-kernel to load.  
	 * \param kernelId  unique integer ID for the loaded kernel
	 * \return          loaded kernels/metakernels unique integer id
	 */
	int loadKernel(const std::string& fullPath, 
		           const std::string& shorthand);

	/**
	 *  Unload SPICE kernel.
	 *  For further details, please refer to 'unload_c' in SPICE Docummentation
	 *
	 * \param either correspondign unique ID or shorthand with 
	 *   which the kernel was loaded and is to be unloaded. 
	 */

	bool unloadKernel(const std::string& shorthand);
	bool unloadKernel(int kernelId);
	
// Acessing Kernel Data - Constants and Ids 

	/**
	 *  Determine whether values exist for some item for any body in the kernel pool.
	 *  For further details, please refer to 'bodfnd_c' in SPICE Docummentation
	 *
	 *  \param naifId  ID code of body.
     *  \param item    Item to find ("RADII", "NUT_AMP_RA", etc.).
	 */
	bool hasValue(int naifId, const std::string& kernelPoolValueName) const;

	/** 
	 *  Fetch from the kernel pool the double precision values of an 
	 *  item associated with a body. 
	 *  For further details, please refer to 'bodvrd_c' in SPICE Docummentation
	 *
	 *  \param bodyName         Body name.
	 *  \param kernelPoolValueName  Item for which values are desired. ("RADII", "NUT_PREC_ANGLES", etc. )
	 */
	bool SpiceManager::getValueFromID(const std::string& bodyname,
		                              const std::string& kernelPoolValueName, 
									  double& value) const;
	/*
	bool SpiceManager::getValueFromID(const std::string& bodyname, 
		                              const std::string& kernelPoolValueName, 
		                              glm::dvec2& value) const;
    */
	bool SpiceManager::getValueFromID(const std::string& bodyname, 
		                              const std::string& kernelPoolValueName,
		                              glm::dvec3& value) const;
	bool SpiceManager::getValueFromID(const std::string& bodyname, 
		                              const std::string& kernelPoolValueName,
		                              glm::dvec4& value) const;
	bool SpiceManager::getValueFromID(const std::string& bodyname, 
		                              const std::string& kernelPoolValueName,
									  std::vector<double>& values, unsigned int num) const;

// Converting between UTC and Ephemeris Time (LSK)

	/**
	 *  Convert a string representing an epoch to a double precision
     *  value representing the number of TDB seconds past the J2000
     *  epoch corresponding to the input epoch.
	 *  For further details, please refer to 'str2et_c' in SPICE Docummentation
	 *
	 *  \param epochString, A string representing an epoch.
	 *  \return Corresponding ephemeris time, equivalent value in seconds past J2000, TDB.
	 */
	double stringToEphemerisTime(const std::string& epochString) const;

// Computing Positions of Spacecraft and Natural Bodies(SPK)

	/**
	 *  Return the position of a target body relative to an observing 
     *  body, optionally corrected for light time (planetary aberration) 
     *  and stellar aberration.
	 *  For further details, please refer to 'spkpos_c' in SPICE Docummentation
	 *
	 * \param target                Target body name.
	 * \param ephemeris             Observer epoch.
	 * \param referenceFrame        Reference frame of output position vector.
	 * \param aberrationCorrection  Aberration correction flag.
	 * \param observer              Observing body name.
	 * \param targetPosition        Position of target.
	 * \param lightTime             One way light time between observer and target.
	 * \return                      Whether the function succeeded or not
	 */
	bool getTargetPosition(const std::string& target, 
		                   double ephemerisTime,
		                   const std::string& referenceFrame, 
						   const std::string& aberrationCorrection,
						   const std::string& observer,
						   glm::dvec3& targetPosition, 
						   double lightTime) const;

	/**
	 *  Return the state (position and velocity) of a target body 
     *  relative to an observing body, optionally corrected for light 
     *  time (planetary aberration) and stellar aberration. 
     *  For further details, please refer to 'spkezr_c' in SPICE Docummentation
	 *
	 * \param target                Target body name.
	 * \param ephemerisTime         Observer epoch.
	 * \param referenceFrame        Reference frame of output state vector.
	 * \param aberrationCorrection  Aberration correction flag.                                                
	 * \param observer              Observing body name.
	 * \param targetPosition        Position of target.
	 * \param targetVelocity        Velocity of target.
	 * \param lightTime             One way light time between observer and target.
	 * \return                      Whether the function succeeded or not 
	 */
	bool getTargetState(const std::string& target, 
		                double ephemerisTime, 
						const std::string& referenceFrame,
					    const std::string& aberrationCorrection,
						const std::string& observer,
						glm::dvec3& targetPosition, 
						glm::dvec3& targetVelocity,
						double lightTime) const;

// Computing Transformations Between Frames (FK)

	/** 
	 *  Return the state transformation matrix from one frame to 
     *  another at a specified epoch. 
	 *  For further details, please refer to 'sxform_c' in SPICE Docummentation
	 *
	 * \param fromFrame    Name of the frame to transform from.
	 * \param toFrame      Name of the frame to transform to.
	 * \param et           Epoch of the rotation matrix.
	 * \param posTransMat  A rotation matrix.
	 * \return             Whether the function succeeded or not
	 */
	bool getStateTransformMatrix(const std::string& fromFrame,
		                         const std::string& toFrame,
		                         double ephemerisTime,
		                         mat6x6& stateMatrix) const;
	

	/**
	 *  Multiply the 6x6 stateTransformMatrix and two 3D vector, postion and velocity.
	 */
	bool multiplyWithStateTransmMat6x6(mat6x6 stateMatrix,
		                               glm::dvec3 position,
									   glm::dvec3 velocity) const;

	/**
	 *  Return the matrix that transforms position vectors from one 
     *  specified frame to another at a specified epoch.
	 *  For further details, please refer to 'pxform_c' in SPICE Docummentation
	 *
	 * \param fromFrame      Name of the frame to transform from.
     * \param toFrame        Name of the frame to transform to.
     * \param et             Epoch of the state transformation matrix.
     * \param stateTransMat  A state transformation matrix.
	 * \return               Whether the function succeeded or not
	 */
	bool getPositionTransformMatrix(const std::string& fromFrame, 
		                            const std::string& toFrame,
		                            double ephemerisTime, 
						            glm::mat3x3& positionTransformationMatrix) const;
	
// Retrieving Instrument Parameters (IK)

	/**
	 *  This routine returns the field-of-view (FOV) parameters for a
     *  specified instrument.
	 *  For further details, please refer to 'getfov_c' in SPICE Docummentation
	 *
	 * \param naifInstrumentId         NAIF ID of an instrument.
	 * \param instrumentFovShape       Instrument Field Of View shape.
	 * \param nameOfFrame              Name of fram in which FOV vectors are defines.
	 * \param boresightVector          Boresight vector.
	 * \param numberOfBoundaryVectors  Number of boundary vectors returned. 
	 * \param bounds                   Field Of View boundary vectors
	 * \return                         Whether the function succeeded or not
	 */
	bool getFieldOfView(const std::string& naifInstrumentId, 
						std::string& instrumentFovShape, 
						std::string& nameOfFrame, 
						double boresightVector, 
						std::vector<glm::vec3>& bounds) const;
	
// Computing Planetocentric, Planetodetic, and Planetographic Coordinates

	/**
	 *  Convert from rectangular coordinates to latitudinal coordinates.
	 *  For further details, please refer to 'reclat_c ' in SPICE Docummentation
	 *
	 * \param coordinates  Rectangular coordinates of a point, 3-vectors
	 * \param radius       Distance of the point from the origin.
	 * \param longitude    Longitude of the point in radians. The range is [-pi, pi].
	 * \param latitude    Latitude of the point in radians.  The range is [-pi/2, pi/2].
	 * \return             Whether the function succeeded or not
	 */
	bool rectangularToLatitudal(const glm::vec3 coordinates, 
		                        double& radius, 
								double& longitude, 
								double& latitude) const;
	/**
  	 *  Convert from latitudinal coordinates to rectangular coordinates.
	 *  For further details, please refer to 'reclat_c ' in SPICE Docummentation
	 *
	 * \param radius         Distance of a point from the origin.
     * \param longitude      Longitude of point in radians.
     * \param longitude      Latitude of point in radians.
     * \param latitude       Rectangular coordinates of the point.
	 * \return               Whether the function succeeded or not
  	 */
	bool latidudinalToRectangular(double radius, 
		                          double& longitude, 
								  double& latitude, 
								  glm::vec3& coordinates) const;

	/**
	 *  Convert planetocentric latitude and longitude of a surface 
     *  point on a specified body to rectangular coordinates.
	 *  For further details, please refer to 'srfrec_c ' in SPICE Docummentation
	 *
	 * \param naif_id      NAIF integer code of an extended body. 
     * \param longitude Longitude of point in radians.
     * \param latitude  Latitude of point in radians.
     * \param coordinates    Rectangular coordinates of the point. 
	 * \return               Whether the function succeeded or not
	 */
	bool planetocentricToRectangular(int naif_id,
									 double& longitude,
									 double& latitude,
									 glm::vec3& coordinates) const;

// Computing Sub - observer and Sub - solar Points
	
	/**
	 * Compute the rectangular coordinates of the sub-observer point on 
     * a target body at a specified epoch, optionally corrected for 
     * light time and stellar aberration. 
	 *  For further details, please refer to 'subpnt_c ' in SPICE Docummentation
	 *
	 * \param computationMethod          Computation method.
	 * \param target                     Name of target body.
	 * \param ephemeris                  Epoch in ephemeris seconds past J2000 TDB.
	 * \param bodyFixedFrame             Body-fixed, body-centered target body frame.
	 * \param aberrationCorrection       Aberration correction.
	 * \param observer                   Name of observing body.
	 * \param subObserverPoint           Sub-observer point on the target body.
	 * \param targetEpoch                Sub-observer point epoch.
	 * \param observerToSubObserverVec   Vector from observer to sub-observer point.
	 * \return                           Whether the function succeeded or not
	 */
	bool getSubObserverPoint(std::string computationMethod,
		                     std::string target,
		                     double ephemeris,
		                     std::string bodyFixedFrame,
		                     std::string aberrationCorrection,
		                     std::string observer,
		                     glm::vec3& subObserverPoint,
		                     double& targetEpoch,
							 glm::vec3& vectorToSurfacePoint) const;

	/**
	* Compute the rectangular coordinates of the sub-observer point on
	* a target body at a specified epoch, optionally corrected for
	* light time and stellar aberration.
	*  For further details, please refer to 'subslr_c ' in SPICE Docummentation
	*
	* \param computationMethod          Computation method.
	* \param target                     Name of target body.
	* \param ephemeris                  Epoch in ephemeris seconds past J2000 TDB.
	* \param bodyFixedFrame             Body-fixed, body-centered target body frame.
	* \param aberrationCorrection       Aberration correction.
	* \param observer                   Name of observing body.
	* \param subObserverPoint           Sub-observer point on the target body.
	* \param targetEpoch                Sub-observer point epoch.
	* \param observerToSubObserverVec   Vector from observer to sub-observer point.
	* \return                           Whether the function succeeded or not
	*/
	bool getSubSolarPoint(std::string computationMethod,
		                  std::string target,
		                  double ephemeris,
		                  std::string bodyFixedFrame,
		                  std::string aberrationCorrection,
		                  std::string observer,
		                  glm::vec3& subObserverPoint,
		                  double& targetEpoch,
		                  glm::vec3& vectorToSurfacePoint) const;
	

	//TODO: add additional functions for 'mxvg_c'

private:
	SpiceManager() = default;
	~SpiceManager();
	
	SpiceManager(const SpiceManager& c) = delete;

	static SpiceManager* _manager;

	struct spiceKernel {
		std::string path;
		std::string name;
		int id;
	};

	std::vector<spiceKernel> _loadedKernels;
	unsigned int kernelCount = 0;
};
}

#endif