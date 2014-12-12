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

#ifndef __SPICEMANAGER_H__
#define __SPICEMANAGER_H__

#include "SpiceUsr.h"
#include "SpiceZpr.h"

#include <openspace/util/powerscaledcoordinate.h>

#include <ghoul/glm.h>

#include <glm/gtc/type_ptr.hpp>

#include <array>
#include <map>
#include <string>
#include <vector>

namespace openspace {

class SpiceManager {
public:
	typedef std::array<double, 36> TransformMatrix;
	typedef unsigned int KernelIdentifier;
	
	static const KernelIdentifier KernelFailed = KernelIdentifier(-1);

	/**
	* Initializer that initializes the static member. 
	*/
	static void initialize();

	/**
	 * Deinitializes the SpiceManager and unloads all kernels which have been loaded using
	 * this manager.
	 */
	static void deinitialize();

	/**
	 * Returns the reference to the singleton SpiceManager object that must have been
	 * initialized by a call to the initialize method earlier.
	 * \return The SpiceManager singleton
	 */
	static SpiceManager& ref();
	
	/**
	 * Loads one or more SPICE kernels into a program. The provided path can either be a
	 * binary, text-kernel, or meta-kernel which gets loaded into the kernel pool. The
	 * loading is done by passing the <code>filePath</code> to the <code>furnsh_c</code>
	 * function. http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/furnsh_c.html
	 * \param filePath The path to the kernel that should be loaded
	 * \return The loaded kernel's unique identifier that can be used to unload the kernel
	 */
	KernelIdentifier loadKernel(const std::string& filePath);


	/**
	 * Unloads a SPICE kernel identified by the <code>kernelId</code> which was returned
	 * by the loading call to loadKernel. The unloading is done by calling the
	 * <code>unload_c</code> function.
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/unload_c.html
	 * \param kernelId The unique identifier that was returned from the call to
	 * loadKernel which loaded the kernel
	 */
	void unloadKernel(KernelIdentifier kernelId);

	/**
	 * Unloads a SPICE kernel identified by the <code>filePath</code> which was used in
	 * the loading call to loadKernel. The unloading is done by calling the
	 * <code>unload_c</code> function.
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/unload_c.html.
	 * \param filePath The path of the kernel that should be unloaded, it has to refer to
	 * a file that was loaded in using the loadKernel method
	 */
	void unloadKernel(const std::string& filePath);
	
	/**
	 * Determines whether values exist for some <code>item</code> for any body,
	 * identified by it's <code>naifId</code>, in the kernel pool by passing it to the
	 * <code>bodfnd_c</code> function. 
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bodfnd_c.html
	 * For a description on NAIF IDs, see
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/naif_ids.html.
	 * \param naifId NAIF ID code of body
     * \param item The item to find
	 * \return <code>true</code> if the function succeeded, <code>false</code> otherwise
	 */
	bool hasValue(int naifId, const std::string& item) const;

	/**
	 * Determines whether values exist for some <code>item</code> for any
	 * code>body</code> in the kernel pool by passing it to the <code>bodfnd_c</code>
	 * function. 
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bodfnd_c.html
	 * \param body The name of the body that should be sampled
     * \param item The item to find
	 * \return <code>true</code> if the function succeeded, <code>false</code> otherwise
	 */
	bool hasValue(const std::string& body, const std::string& item) const;

	/**
	 * Returns the NAIF ID for a specific body. For a description on NAIF IDs, see
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/naif_ids.html. The
	 * <code>id</code> will only be set if the retrieval was successful, otherwise it will
	 * remain unchanged.
	 * \param body The body name that should be retrieved
	 * \param id The ID of the <code>body</code> will be stored in this variable. The
	 * value will only be changed if the retrieval was successful
	 * \return <code>true</code> if the <code>body</code> was found, <code>false</code>
	 * otherwise
	 */
	bool getNaifId(const std::string& body, int& id) const;

	/**
	 * Retrieves a single <code>value</code> for a certain <code>body</code>. This method
	 * succeeds iff <code>body</code> is the name of a valid body, <code>value</code>
	 * is a value associated with the body, and the value consists of only a single
	 * <code>double</code> value. If all conditions are true, the value is retrieved using
	 * the method <code>bodvrd_c</code> and stored in <code>v</code>
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bodvrd_c.html. If one of
	 * the conditions is false an error is logged and the value <code>v</code> is
	 * unchanged.  For a description on NAIF IDs, see
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/naif_ids.html.
	 * \param body The name of the body whose value should be retrieved or the NAIF ID of
	 * this body
	 * \param value The value of that should be retrieved, this value is case-sensitive
	 * \param v The destination for the retrieved value
	 * \return <code>true</code> if the <code>body</code> named a valid body,
	 * <code>value</code> is a valid item for the <code>body</code> and the retrieved
	 * value is only a single value. <code>false</code> otherwise
	 */
	bool getValue(const std::string& body, const std::string& value, double& v) const;

	/**
	 * Retrieves a <code>value</code> with three components for a certain
	 * <code>body</code>. This method succeeds iff <code>body</code> is the name of a
	 * valid body, <code>value</code> is a value associated with the body, and the value
	 * consists of three <code>double</code> values. If all conditions are true, the value
	 * is retrieved using the method <code>bodvrd_c</code> and stored in <code>v</code>
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bodvrd_c.html. If one of
	 * the conditions is false an error is logged and the value <code>v</code> is
	 * unchanged. For a description on NAIF IDs, see
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/naif_ids.html.
	 * \param body The name of the body whose value should be retrieved or the NAIF ID of
	 * the body
	 * \param value The value of that should be retrieved, this value is case-sensitive
	 * \param v The destination for the retrieved values
	 * \return <code>true</code> if the <code>body</code> named a valid body,
	 * <code>value</code> is a valid item for the <code>body</code> and the retrieved
	 * value is only a single value. <code>false</code> otherwise
	 */
	bool getValue(const std::string& body, const std::string& value, glm::dvec3& v) const;

	/**
	 * Retrieves a <code>value</code> with four components for a certain
	 * <code>body</code>. This method succeeds iff <code>body</code> is the name of a
	 * valid body, <code>value</code> is a value associated with the body, and the value
	 * consists of four <code>double</code> values. If all conditions are true, the value
	 * is retrieved using the method <code>bodvrd_c</code> and stored in <code>v</code>
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bodvrd_c.html. If one of
	 * the conditions is false an error is logged and the value <code>v</code> is
	 * unchanged. For a description on NAIF IDs, see
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/naif_ids.html.
	 * \param body The name of the body whose value should be retrieved or the NAIF ID of
	 * the body
	 * \param value The value of that should be retrieved, this value is case-sensitive
	 * \param v The destination for the retrieved values
	 * \return <code>true</code> if the <code>body</code> named a valid body,
	 * <code>value</code> is a valid item for the <code>body</code> and the retrieved
	 * value is only a single value. <code>false</code> otherwise
	 */
	bool getValue(const std::string& body, const std::string& value, glm::dvec4& v) const;

	/**
	 * Retrieves a <code>value</code> with an arbitrary number of components for a certain
	 * <code>body</code>. This method succeeds <code>body</code> is a valid body,
	 * <code>value</code> is a value associated with the body, and the value consists of a
	 * number of <code>double</code> values. The requested number is equal to the
	 * <code>size</code> of the passed vector <code>v</code> which means that this vector
	 * has to be preallocated. If all conditions are true, the value is retrieved using
	 * the method <code>bodvrd_c</code> and stored in <code>v</code>
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bodvrd_c.html. If one of
	 * the conditions is false an error is logged and the value <code>v</code> is
	 * unchanged. For a description on NAIF IDs, see
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/naif_ids.html.
	 * \param body The body whose information should be retrieved or the NAIF ID of that
	 * body
	 * \param value The value of that should be retrieved, this value is case-sensitive
	 * \param v The destination for the retrieved values. The size of this vector
	 * determines how many values will be retrieved
	 * \return <code>true</code> if the <code>body</code> named a valid body,
	 * <code>value</code> is a valid item for the <code>body</code> and the retrieved
	 * value is only a single value. <code>false</code> otherwise
	 */
	bool getValue(const std::string& body, const std::string& value,
		std::vector<double>& v) const;

	/**
	 * Converts the <code>timeString</code> representing a date to a double precision
     * value representing the <code>ephemerisTime</code>; that is the number of TDB
	 * seconds past the J2000 epoch. For further details, please refer to
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/str2et_c.html. If an error
	 * occurs, an error is logged, the method returns <code>false</code> and the
	 * <code>ephemerisTime</code> remains unchanged.
	 * \param timeString A string representing the time to be converted
	 * \param ephemerisTime The destination for the converted time; the number of TDB
	 * seconds past the J2000 epoch, representing the passed <code>epochString</code>
	 * \return <code>true</code> if the <code>epochString</code> is a valid string and
	 * the conversion succeeded, <code>false</code> otherwise
	 */
	bool getETfromDate(const std::string& timeString, double& ephemerisTime) const;

	/**
	 * Converts the passed <code>ephemerisTime</code> into a human-readable
	 * <code>date</code> string with a specific <code>format</code>. For details on the
	 * formatting, refer to
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/timout_c.html. In case of
	 * an error, <code>date</code> will not be modified, an error will be logged and the
	 * method returns <code>false</code>.
	 * \param ephemerisTime The ephemeris time, that is the number of TDB seconds past the
	 * J2000 epoch
	 * \param date The destination for the converted date. This will only be changed if 
	 * the conversion succeeded
	 * \param format The format string describing the output format for the
	 * <code>date</code>
	 * \return <code>true</code> if the conversion succeeded, <code>false</code> otherwise
	 */
	bool getDateFromET(double ephemerisTime, std::string& date,
		const std::string& format = "YYYY MON DDTHR:MN:SC.### ::RND");

	/**
	* This helper method converts a 3 dimensional vector from one reference frame to another.
	* \param v The vector to be converted
	* \param from The frame to be converted from
	* \param to The frame to be converted to
	* \param ephemerisTime Time at which to get rotational matrix that transforms vector
	*/
	void frameConversion(glm::dvec3& v, const std::string from, const std::string to, double ephemerisTime) const;

	/**
	 *  Finds the projection of one vector onto another vector.
     *  All vectors are 3-dimensional.
  	 *  \param v1 The vector to be projected.
	 *  \param v2 The vector onto which v1 is to be projected.
	 *  \return The projection of v1 onto v2.
	 */
	glm::dvec3 orthogonalProjection(glm::dvec3& v1, glm::dvec3& v2);

	/**
	 * Returns the <code>position</code> of a <code>target</code> body relative to an
	 * <code>observer</code> in a specific <code>referenceFrame</code>, optionally
	 * corrected for light time (planetary aberration) and stellar aberration
	 * (<code>aberrationCorrection</code>). For further details, refer to
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/spkpos_c.html. For more
	 * information on NAIF IDs, refer to
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/naif_ids.html
	 * \param target The target body name or the target body's NAIF ID
	 * \param observer The observing body name or the observing body's NAIF ID
	 * \param referenceFrame The reference frame of the output position vector
	 * \param aberrationCorrection The aberration correction flag out of the list of
	 * values (<code>NONE</code>, <code>LT</code>, <code>LT+S</code>, <code>CN</code>,
	 * <code>CN+S</code> for the reception case or <code>XLT</code>, <code>XLT+S</code>,
	 * <code>XCN</code>, or <code>XCN+S</code> for the transmission case. 
	 * \param ephemerisTime The time at which the position is to be queried
	 * \param position The output containing the position of the target; if the method
	 * fails, the target position is unchanged
	 * \param lightTime If the <code>aberrationCorrection</code> is different from
	 * <code>NONE</code>, this variable will contain the one-way light time between the
	 * observer and the target. If the method fails, the lightTime is unchanged
	 * \return <code>true</code> if the function was successful, <code>false</code>
	 * otherwise
	 */
	bool getTargetPosition(const std::string& target, 
						   const std::string& observer,
		                   const std::string& referenceFrame, 
						   const std::string& aberrationCorrection,
		                   double ephemerisTime,
						   glm::dvec3& position, 
						   double& lightTime) const;

	bool getTargetPosition(const std::string& target,
						   const std::string& observer,
		                   const std::string& referenceFrame, 
						   const std::string& aberrationCorrection,
		                   double ephemerisTime,
						   psc& position, 
						   double& lightTime) const;

	/**
	*   Given an observer and a direction vector defining a ray, compute 
    *   the surface intercept of the ray on a target body at a specified 
    *   epoch, optionally corrected for light time and stellar 
    *   aberration. 
	*   \param method     Computation method. 
    *   \param target     Name of target body. 
    *   \param et         Epoch in ephemeris seconds past J2000 TDB. 
    *   \param fixref     Body-fixed, body-centered target body frame. 
    *   \param abcorr     Aberration correction. 
    *   \param obsrvr     Name of observing body. 
    *   \param dref       Reference frame of ray's direction vector. 
    *   \param dvec       Ray's direction vector. 
    *   \param spoint     Surface intercept point on the target body. 
    *   \param trgepc     Intercept epoch. 
    *   \param srfvec     Vector from observer to intercept point. 
    *   \param found      Flag indicating whether intercept was found. 
	*   For further details, refer to
	*   http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/sincpt_c.html
	*/
	bool getSurfaceIntercept(const std::string& target,
							 const std::string& observer,
							 const std::string& fovFrame,
							 const std::string& bodyFixedFrame,
							 const std::string& method,
							 const std::string& aberrationCorrection,
							 double ephemerisTime,
							 double& targetEpoch,
							 glm::dvec3& directionVector,
							 glm::dvec3& surfaceIntercept,
							 glm::dvec3& surfaceVector) const;

	/**
	*  Determine if a specified ephemeris object is within the
    *  field-of-view (FOV) of a specified instrument at a given time.
	*  \param Name or ID code string of the instrument.
	*  \param Name or ID code string of the target.
	*  \param Type of shape model used for the target.
	*  \param Body-fixed, body-centered frame for target body.
	*  \param Aberration correction method.
	*  \param Name or ID code string of the observer.
	*  \param Time of the observation (seconds past J2000).
	*  \param Visibility flag (SPICETRUE/SPICEFALSE).
    *  For further detail, refer to 
	*  http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/fovtrg_c.html
	*/
	bool targetWithinFieldOfView(const std::string& instrument,
		                         const std::string& target,
		                         const std::string& observer,
		                         const std::string& method,
		                         const std::string& referenceFrame,
		                         const std::string& aberrationCorrection,
		                         double& targetEpoch) const;
	/**
	* This method performs the same computation as the function its overloading 
	* with the exception that in doing so it assumes the inertial bodyfixed frame 
	* is that of 'IAU' type, allowing the client to omitt the 
	* <code>referenceFrame</code> for planetary objects.   
	*/
	bool targetWithinFieldOfView(const std::string& instrument,
		                         const std::string& target,
		                         const std::string& observer,
		                         const std::string& method,
		                         const std::string& aberrationCorrection,
		                         double& targetEpoch) const;

	/**
	 * Returns the state vector (<code>position</code> and <code>velocity</code>) of a
	 * <code>target</code> body relative to an <code>observer</code> in a specific
	 * <code>referenceFrame</code>, optionally corrected for light time (planetary
	 * aberration) and stellar aberration (<code>aberrationCorrection</code>). For further
	 * details, refer to
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/spkezr_c.html. For more
	 * information on NAIF IDs, refer to
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/naif_ids.html
	 * \param target The target body name or the target body's NAIF ID
	 * \param observer The observing body name or the observing body's NAIF ID
	 * \param referenceFrame The reference frame of the output position vector
	 * \param aberrationCorrection The aberration correction flag out of the list of
	 * values (<code>NONE</code>, <code>LT</code>, <code>LT+S</code>, <code>CN</code>,
	 * <code>CN+S</code> for the reception case or <code>XLT</code>, <code>XLT+S</code>,
	 * <code>XCN</code>, or <code>XCN+S</code> for the transmission case. 
	 * \param ephemerisTime The time at which the position is to be queried
	 * \param position The output containing the position of the target; if the method
	 * fails, the position is unchanged
	 * \param velocity The output containing the velocity of the target; if the method
	 * fails, the velocity is unchanged
	 * \param lightTime If the <code>aberrationCorrection</code> is different from
	 * <code>NONE</code>, this variable will contain the one-way light time between the
	 * observer and the target.If the method fails, the lightTime is unchanged
	 * \return <code>true</code> if the function was successful, <code>false</code>
	 * otherwise
	 */
	bool getTargetState(const std::string& target, 
						const std::string& observer,
						const std::string& referenceFrame,
					    const std::string& aberrationCorrection,
		                double ephemerisTime, 
						glm::dvec3& position, 
						glm::dvec3& velocity,
						double& lightTime) const;

	bool getTargetState(const std::string& target, 
						const std::string& observer,
						const std::string& referenceFrame,
					    const std::string& aberrationCorrection,
		                double ephemerisTime, 
						PowerScaledCoordinate& position, 
						PowerScaledCoordinate& velocity,
						double& lightTime) const;

	/** 
	 * Returns the state transformation matrix used to convert from one frame to another
	 * at a specified <code>ephemerisTime</code>. For further details, please refer to
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/sxform_c.html.
	 * \param sourceFrame The name of the source reference frame
	 * \param destinationFrame The name of the destination reference frame
	 * \param ephemerisTime The time at which the transformation matrix is to be queried
	 * \param transformationMatrix The output containing the TransformMatrix containing
	 * the transformation matrix that defines the transformation from the
	 * <code>sourceFrame</code> to the <code>destinationFrame</code>. If the method fails
	 * the <code>transformationMatrix</code> is unchanged
	 * \return <code>true</code> if the function was successful, <code>false</code>
	 * otherwise
	 */
	bool getStateTransformMatrix(const std::string& sourceFrame,
		                         const std::string& destinationFrame,
		                         double ephemerisTime,
								 TransformMatrix& transformationMatrix) const;

	/**
	 * Returns the matrix that transforms position vectors from one reference frame to
	 * another at a specified <code>ephemerisTime</code>. For further details, please refer to
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/pxform_c.html.
	 * \param sourceFrame The name of the source reference frame
	 * \param destinationFrame The name of the destination reference frame
	 * \param ephemerisTime The time at which the transformation matrix is to be queried
     * \param transformationMatrix The output containing the transformation matrix that 
	 * defines the transformation from the <code>sourceFrame</code> to the
	 * <code>destinationFrame</code>. If the method fails the
	 * <code>transformationMatrix</code> is unchanged
	 * \return <code>true</code> if the function was successful, <code>false</code>
	 * otherwise
	 */
	bool getPositionTransformMatrix(const std::string& sourceFrame,
									   const std::string& destinationFrame,
									   double ephemerisTime,
									   glm::dmat3& transformationMatrix) const;

	/**
	* The following overloaded function performs similar to its default - the exception being 
	* that it computes <code>transformationMatrix</code> with respect to local time offset 
	* between an observer and its target. This allows for the accountance of light travel of 
	* photons, e.g to account for instrument pointing offsets due to said phenomenon. 
	* \param sourceFrame The name of the source reference frame
	* \param destinationFrame The name of the destination reference frame
	* \param ephemerisTimeFrom Recorded/observed observation time
	* \param ephemerisTimeTo   Emission local target-time
	* \param transformationMatrix The output containing the transformation matrix that
	*/

	bool getPositionTransformMatrix(const std::string& sourceFrame,
									const std::string& destinationFrame,
									double ephemerisTimeFrom,
									double ephemerisTimeTo,
									glm::dmat3& transformationMatrix) const;


	bool getPositionPrimeMeridian(const std::string& sourceFrame,
									const std::string& destinationFrame,
									double ephemerisTime,
									glm::dmat3& transformationMatrix) const;

	/**
	 * Applies the <code>transformationMatrix</code> retrieved from
	 * getStateTransformMatrix to the <code>position</code> and <code>velocity</code>. The
	 * <code>position</code> and <code>velocity</code> parameters are used as input and
	 * output.
	 * \param position The position that should be transformed. The transformed position
	 * will be stored back in this parameter
	 * \param velocity The velocity that should be transformed. The transformed velocity
	 * will be stored back in this parameter
	 * \param transformationMatrix The 6x6 transformation matrix retrieved from
	 * getStateTransformMatrix that is used to transform the <code>position</code> and
	 * <code>velocity</code> vectors
	 */
	void applyTransformationMatrix(glm::dvec3& position,
								   glm::dvec3& velocity,
								   const TransformMatrix& transformationMatrix);
	
	/**
	 * This routine returns the field-of-view (FOV) parameters for a specified
	 * <code>instrument</code>. For further details, please refer to
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/getfov_c.html.
	 * \param instrument The name of the instrument for which the FOV is to be retrieved
	 * \param fovShape The output containing the rough shape of the returned FOV. If the
	 * method fails, this value remains unchanged
	 * \param frameName The output containing the name of the frame in which the FOV
	 * <code>bounds</code> are computed. If the method fails, this value remains unchanged
	 * \param boresightVector The output containing the boresight, that is the vector for
	 * the center direction of the FOV. If the method fails, this value remains unchanged
	 * \param bounds The output containing the values defining the bounds of the FOV as
	 * explained by http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/getfov_c.html.
	 * If the method fails, this value remains unchanged
	 * \return <code>true</code> if the function was successful, <code>false</code>
	 * otherwise
	 */
	bool getFieldOfView(const std::string& instrument, 
						std::string& fovShape,
						std::string& frameName,
						glm::dvec3& boresightVector, 
						std::vector<glm::dvec3>& bounds) const;

	/**
	 * This routine returns the field-of-view (FOV) parameters for a specified
	 * <code>instrument</code>. For further details, please refer to
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/getfov_c.html.
	 * \param instrument The NAIF id of the instrument for which the FOV is to be
	 * retrieved. For more information on NAIF IDs, refer to
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/naif_ids.html
	 * \param fovShape The output containing the rough shape of the returned FOV. If the
	 * method fails, this value remains unchanged
	 * \param frameName The output containing the name of the frame in which the FOV
	 * <code>bounds</code> are computed. If the method fails, this value remains unchanged
	 * \param boresightVector The output containing the boresight, that is the vector for
	 * the center direction of the FOV. If the method fails, this value remains unchanged
	 * \param bounds The output containing the values defining the bounds of the FOV as
	 * explained by http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/getfov_c.html.
	 * If the method fails, this value remains unchanged
	 * \return <code>true</code> if the function was successful, <code>false</code>
	 * otherwise
	 */
	bool getFieldOfView(int instrument, std::string& fovShape, std::string& frameName,
		glm::dvec3& boresightVector, std::vector<glm::dvec3>& bounds) const;
	
	/**
	 * Converts planeto-centric <code>latitude</code> and <code>longitude</code> of a
	 * surface point on a specified <code>body</code> to rectangular
	 * <code>coordinates</code>. For further details, refer to
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/srfrec_c.html.
	 * \param body The body on which the <code>longitude</code> and <code>latitude</code>
	 * are defined. This body needs to have a defined radius for this function to work
     * \param longitude The longitude of the point on the <code>body</code> in radians
     * \param latitude The latitude of the point on the <code>body</code> in radians
     * \param coordinates The output containing the rectangular coordinates of the point
	 * defined by <code>longitude</code> and <code>latitude</code> on the
	 * <code>body</code>. If the method fails, the coordinate are unchanged
	 * \return <code>true</code> if the function was successful, <code>false</code>
	 * otherwise
	 */
	bool geographicToRectangular(const std::string& body, double longitude,
									 double latitude, glm::dvec3& coordinates) const;

	/**
	 * Converts planeto-centric <code>latitude</code> and <code>longitude</code> of a
	 * surface point on a body with the NAIF ID of <code>id</code> to rectangular
	 * <code>coordinates</code>. For further details, refer to
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/srfrec_c.html.
	 * \param body The body on which the <code>longitude</code> and <code>latitude</code>
	 * are defined. This body needs to have a defined radius for this function to work
     * \param longitude The longitude of the point on the <code>body</code> in radians
     * \param latitude The latitude of the point on the <code>body</code> in radians
     * \param coordinates The output containing the rectangular coordinates of the point
	 * defined by <code>longitude</code> and <code>latitude</code> on the
	 * <code>body</code>. If the method fails, the coordinate are unchanged
	 * \return <code>true</code> if the function was successful, <code>false</code>
	 * otherwise
	 */
	bool geographicToRectangular(int id, double longitude, double latitude,
		glm::dvec3& coordinates) const;
	/**
	 * Compute the rectangular coordinates of the sub-observer point of an
	 * <code>observer</code> on a target <code>body</code> at a specified
	 * <code>ephemerisTime</code>, optionally corrected for light time and stellar
	 * aberration. For further details, refer to
	 * http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/subpnt_c.html.
	 * Example: If the sub-observer point on Mars for MRO is requested, the
	 * <code>target</code> would be <code>Mars</code> and the <code>observer</code> would
	 * be <code>MRO</code>.
	 * \param target The name of the target body on which the sub-observer point lies
	 * \param observer The name of the ephemeris object whose sub-observer point should be
	 * retrieved
	 * \param computationMethod The computation method used for the sub-observer point.
	 * Must be one of <code>Near point: ellipsoid</code> or
	 * <code>Intercept: ellipsoid</code> and it determines the interpretation of the
	 * results; see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/subpnt_c.html
	 * for detailed description on the computation methods
	 * \param bodyFixedFrame The body-fixed, body-centered frame belonging to the
	 * <code>target</code>
	 * \param aberrationCorrection The aberration correction flag out of the list of
	 * values (<code>NONE</code>, <code>LT</code>, <code>LT+S</code>, <code>CN</code>,
	 * <code>CN+S</code> for the reception case or <code>XLT</code>, <code>XLT+S</code>,
	 * <code>XCN</code>, or <code>XCN+S</code> for the transmission case. 
	 * \param ephemerisTime The ephemeris time for which the sub-observer point should be
	 * retrieved
	 * \param subObserverPoint The output containing the observer's sub-observer point on
	 * the target body. If the method fails, the value remains unchanged
	 * \param targetEphemerisTime The output containing the target's ephemeris time
	 * accounting for the aberration, if an <code>aberrationCorrection</code> value
	 * different from <code>NONE</code> is chosen. If the method fails, the value remains
	 * unchanged
	 * \param vectorToSurfacePoint The output containing the vector from the observer to
	 * the, potentially aberration corrected, sub-observer point. If the method fails the
	 * value remains unchanged
	 * \return <code>true</code> if the function was successful, <code>false</code>
	 * otherwise
	 */
	bool getSubObserverPoint(const std::string& target,
		                     const std::string& observer,
 							 const std::string& computationMethod,
		                     const std::string& bodyFixedFrame,
		                     const std::string& aberrationCorrection,
		                     double ephemerisTime,
		                     glm::dvec3& subObserverPoint,
		                     double& targetEphemerisTime,
							 glm::dvec3& vectorToSurfacePoint) const;
    
    /**
     * This method checks if one of the previous SPICE methods has failed. If it has, the
     * <code>errorMessage</code> is used to log an error along with the original SPICE
     * error message.
     * \param errorMessage The error message that will be logged if the method fails. If
     * the argument is empty, no error message will be logged
     * \return <code>true</code> if an error occurred, <code>false</code> otherwise
     */
    static bool checkForError(std::string errorMessage);

private:
	struct KernelInformation {
		std::string path;
		KernelIdentifier id;
	};

	SpiceManager() = default;
	SpiceManager(const SpiceManager& c) = delete;
	SpiceManager& operator=(const SpiceManager& r) = delete;
	SpiceManager(SpiceManager&& r) = delete;

	std::vector<KernelInformation> _loadedKernels;
	KernelIdentifier _lastAssignedKernel;

	static SpiceManager* _manager;
};

} // namespace openspace

#endif // __SPICEMANAGER_H__
