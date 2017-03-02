/*
 TUIO C++ Library
 Copyright (c) 2005-2016 Martin Kaltenbrunner <martin@tuio.org>
 
 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 3.0 of the License, or (at your option) any later version.
 
 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 Lesser General Public License for more details.
 
 You should have received a copy of the GNU Lesser General Public
 License along with this library.
*/

#ifndef INCLUDED_TUIOCONTAINER_H
#define INCLUDED_TUIOCONTAINER_H

#include "TuioPoint.h"
#include <list>
#include <string>

#define TUIO_IDLE 0
#define TUIO_ADDED 1
#define TUIO_ACCELERATING 2
#define TUIO_DECELERATING 3
#define TUIO_ROTATING 4
#define TUIO_STOPPED 5
#define TUIO_REMOVED 6

#define MAX_PATH_SIZE 128

namespace TUIO {
	
	/**
	 * The abstract TuioContainer class defines common attributes that apply to both subclasses {@link TuioObject} and {@link TuioCursor}.
	 *
	 * @author Martin Kaltenbrunner
	 * @version 1.1.6
	 */ 
	class LIBDECL TuioContainer: public TuioPoint {
		
		
	private:
		
		TuioPoint *lastPoint;
		
	protected:
		/**
		 * The unique session ID number that is assigned to each TUIO object or cursor.
		 */ 
		long session_id;
		/**
		 * The X-axis velocity value.
		 */ 
		float x_speed;
		/**
		 * The Y-axis velocity value.
		 */ 
		float y_speed;
		/**
		 * The motion speed value.
		 */ 
		float motion_speed;
		/**
		 * The motion acceleration value.
		 */ 
		float motion_accel;
		float x_accel;
		float y_accel;
		/**
		 * A List of TuioPoints containing all the previous positions of the TUIO component.
		 */ 
		std::list<TuioPoint> path;
		/**
		 * Reflects the current state of the TuioComponent
		 */ 
		int state;
		/**
		 * The ID of the TUIO source
		 */ 
		int source_id;	
		/**
		 * The name of the TUIO source
		 */ 
		std::string source_name;
		/**
		 * The address of the TUIO source
		 */ 
		std::string source_addr;
	
	public:
		using TuioPoint::update;
		
		/**
		 * This constructor takes a TuioTime argument and assigns it along with the provided 
		 * Session ID, X and Y coordinate to the newly created TuioContainer.
		 *
		 * @param	ttime	the TuioTime to assign
		 * @param	si	the Session ID to assign
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 */
		TuioContainer (TuioTime ttime, long si, float xp, float yp);

		/**
		 * This constructor takes the provided Session ID, X and Y coordinate 
		 * and assigs these values to the newly created TuioContainer.
		 *
		 * @param	si	the Session ID to assign
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 */
		TuioContainer (long si, float xp, float yp);
		
		/**
		 * This constructor takes the atttibutes of the provided TuioContainer 
		 * and assigs these values to the newly created TuioContainer.
		 *
		 * @param	tcon	the TuioContainer to assign
		 */
		TuioContainer (TuioContainer *tcon);
		
		/**
		 * The destructor is doing nothing in particular. 
		 */
		virtual ~TuioContainer(){};

		/**
		 * Sets the ID, name and address of the TUIO source 
		 *
		 * @param	src_id		the ID of the TUIO source
		 * @param	src_name	the name of the TUIO source
		 * @param	src_addr	the address of the TUIO source
		 */
		virtual void setTuioSource(int src_id, const char *src_name, const char *src_addr);

		/**
		 * Returns the name of the TUIO source 
		 */
		virtual const char* getTuioSourceName() const;

		/**
		 * Returns the address of the TUIO source 
		 */
		virtual const char* getTuioSourceAddress() const;
		
		/**
		 * Returns the ID of the TUIO source 
		 */
		virtual int getTuioSourceID() const;
		
		/**
		 * Takes a TuioTime argument and assigns it along with the provided 
		 * X and Y coordinate to the private TuioContainer attributes.
		 * The speed and accleration values are calculated accordingly.
		 *
		 * @param	ttime	the TuioTime to assign
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 */
		virtual void update (TuioTime ttime, float xp, float yp);
		
		/**
		 * This method is used to calculate the speed and acceleration values of
		 * TuioContainers with unchanged positions.
		 */
		virtual void stop(TuioTime ttime);

		/**
		 * Takes a TuioTime argument and assigns it along with the provided 
		 * X and Y coordinate, X and Y velocity and acceleration
		 * to the private TuioContainer attributes.
		 *
		 * @param	ttime	the TuioTime to assign
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 * @param	xs	the X velocity to assign
		 * @param	ys	the Y velocity to assign
		 * @param	ma	the acceleration to assign
		 */
		virtual void update (TuioTime ttime, float xp, float yp, float xs, float ys, float ma);
		
		/**
		 * Assigns the provided X and Y coordinate, X and Y velocity and acceleration
		 * to the private TuioContainer attributes. The TuioTime time stamp remains unchanged.
		 *
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 * @param	xs	the X velocity to assign
		 * @param	ys	the Y velocity to assign
		 * @param	ma	the acceleration to assign
		 */
		virtual void update (float xp, float yp, float xs, float ys, float ma);
		
		/**
		 * Takes the atttibutes of the provided TuioContainer 
		 * and assigs these values to this TuioContainer.
		 * The TuioTime time stamp of this TuioContainer remains unchanged.
		 *
		 * @param	tcon	the TuioContainer to assign
		 */
		virtual void update (TuioContainer *tcon);
		
		/**
		 * Assigns the REMOVE state to this TuioContainer and sets
		 * its TuioTime time stamp to the provided TuioTime argument.
		 *
		 * @param	ttime	the TuioTime to assign
		 */
		virtual void remove(TuioTime ttime);

		/**
		 * Returns the Session ID of this TuioContainer.
		 * @return	the Session ID of this TuioContainer
		 */
		virtual long getSessionID() const;

		/**
		 * Sets the Session ID of this TuioContainer.
		 * @param s_id	the new Session ID for this TuioContainer
		 */
		virtual void setSessionID(long s_id);
		
		/**
		 * Returns the X velocity of this TuioContainer.
		 * @return	the X velocity of this TuioContainer
		 */
		virtual float getXSpeed() const;

		/**
		 * Returns the Y velocity of this TuioContainer.
		 * @return	the Y velocity of this TuioContainer
		 */
		virtual float getYSpeed() const;
		
		/**
		 * Returns the position of this TuioContainer.
		 * @return	the position of this TuioContainer
		 */
		virtual TuioPoint getPosition() const;
		
		/**
		 * Returns the path of this TuioContainer.
		 * @return	the path of this TuioContainer
		 */
		virtual std::list<TuioPoint> getPath() const;
		
		/**
		 * Returns the motion speed of this TuioContainer.
		 * @return	the motion speed of this TuioContainer
		 */
		virtual float getMotionSpeed() const;
		
		/**
		 * Returns the motion acceleration of this TuioContainer.
		 * @return	the motion acceleration of this TuioContainer
		 */
		virtual float getMotionAccel() const;
		
		/**
		 * Returns the TUIO state of this TuioContainer.
		 * @return	the TUIO state of this TuioContainer
		 */
		virtual int getTuioState() const;	
		
		/**
		 * Returns true of this TuioContainer is moving.
		 * @return	true of this TuioContainer is moving
		 */
		virtual bool isMoving() const;

		virtual TuioPoint predictPosition();
	};
}
#endif
