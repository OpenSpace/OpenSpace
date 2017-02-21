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

#ifndef INCLUDED_TUIOOBJECT_H
#define INCLUDED_TUIOOBJECT_H

#include "TuioContainer.h"

namespace TUIO {
	
	/**
	 * The TuioObject class encapsulates /tuio/2Dobj TUIO objects.
	 *
	 * @author Martin Kaltenbrunner
	 * @version 1.1.6
	 */ 
	class LIBDECL TuioObject: public TuioContainer {
		
	protected:
		/**
		 * The individual symbol ID number that is assigned to each TuioObject.
		 */ 
		int symbol_id;
		/**
		 * The rotation angle value.
		 */ 
		float angle;
		/**
		 * The accumulated angle value.
		 */
		float angle_sum;
		/**
		 * The rotation speed value.
		 */ 
		float rotation_speed;
		/**
		 * The rotation acceleration value.
		 */ 
		float rotation_accel;
		
		float angleThreshold;
		OneEuroFilter *angleFilter;
		
	public:
		using TuioContainer::update;
		
		/**
		 * This constructor takes a TuioTime argument and assigns it along with the provided 
		 * Session ID, Symbol ID, X and Y coordinate and angle to the newly created TuioObject.
		 *
		 * @param	ttime	the TuioTime to assign
		 * @param	si	the Session ID  to assign
		 * @param	sym	the Symbol ID  to assign
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 * @param	a	the angle to assign
		 */
		TuioObject (TuioTime ttime, long si, int sym, float xp, float yp, float a);

		/**
		 * This constructor takes the provided Session ID, Symbol ID, X and Y coordinate 
		 * and angle, and assigs these values to the newly created TuioObject.
		 *
		 * @param	si	the Session ID  to assign
		 * @param	sym	the Symbol ID  to assign
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 * @param	a	the angle to assign
		 */	
		TuioObject (long si, int sym, float xp, float yp, float a);
		
		/**
		 * This constructor takes the atttibutes of the provided TuioObject 
		 * and assigs these values to the newly created TuioObject.
		 *
		 * @param	tobj	the TuioObject to assign
		 */
		TuioObject (TuioObject *tobj);
		
		/**
		 * The destructor is doing nothing in particular. 
		 */
		virtual ~TuioObject() {
			if (angleFilter) delete angleFilter;
		};
		
		/**
		 * Takes a TuioTime argument and assigns it along with the provided 
		 * X and Y coordinate, angle, X and Y velocity, motion acceleration,
		 * rotation speed and rotation acceleration to the private TuioObject attributes.
		 *
		 * @param	ttime	the TuioTime to assign
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 * @param	a	the angle coordinate to assign
		 * @param	xs	the X velocity to assign
		 * @param	ys	the Y velocity to assign
		 * @param	rs	the rotation velocity to assign
		 * @param	ma	the motion acceleration to assign
		 * @param	ra	the rotation acceleration to assign
		 */
		void update (TuioTime ttime, float xp, float yp, float a, float xs, float ys, float rs, float ma, float ra);

		/**
		 * Assigns the provided X and Y coordinate, angle, X and Y velocity, motion acceleration
		 * rotation velocity and rotation acceleration to the private TuioContainer attributes.
		 * The TuioTime time stamp remains unchanged.
		 *
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 * @param	a	the angle coordinate to assign
		 * @param	xs	the X velocity to assign
		 * @param	ys	the Y velocity to assign
		 * @param	rs	the rotation velocity to assign
		 * @param	ma	the motion acceleration to assign
		 * @param	ra	the rotation acceleration to assign
		 */
		void update (float xp, float yp, float a, float xs, float ys, float rs, float ma, float ra);
		
		/**
		 * Takes a TuioTime argument and assigns it along with the provided 
		 * X and Y coordinate and angle to the private TuioObject attributes.
		 * The speed and accleration values are calculated accordingly.
		 *
		 * @param	ttime	the TuioTime to assign
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 * @param	a	the angle coordinate to assign
		 */
		void update (TuioTime ttime, float xp, float yp, float a);

		/**
		 * This method is used to calculate the speed and acceleration values of a
		 * TuioObject with unchanged position and angle.
		 */
		void stop (TuioTime ttime);
		
		/**
		 * Takes the atttibutes of the provided TuioObject 
		 * and assigs these values to this TuioObject.
		 * The TuioTime time stamp of this TuioContainer remains unchanged.
		 *
		 * @param	tobj	the TuioContainer to assign
		 */	
		void update (TuioObject *tobj);
		
		/**
		 * Returns the symbol ID of this TuioObject.
		 * @return	the symbol ID of this TuioObject
		 */
		int getSymbolID() const;
		
		/**
		 * Returns the rotation angle of this TuioObject.
		 * @return	the rotation angle of this TuioObject
		 */
		float getAngle() const;
		
		/**
		 * Returns the accumulated rotation angle of this TuioObject.
		 * @return	the accumulated rotation angle of this TuioObject
		 */
		float getAngleSum() const;
		
		/**
		 * Returns the rotation angle in degrees of this TuioObject.
		 * @return	the rotation angle in degrees of this TuioObject
		 */
		float getAngleDegrees() const;
		
		/**
		 * Returns the rotation speed of this TuioObject.
		 * @return	the rotation speed of this TuioObject
		 */
		float getRotationSpeed() const;
		
		/**
		 * Returns the rotation acceleration of this TuioObject.
		 * @return	the rotation acceleration of this TuioObject
		 */
		float getRotationAccel() const;

		/**
		 * Returns true of this TuioObject is moving.
		 * @return	true of this TuioObject is moving
		 */
		bool isMoving() const;
		
		void addAngleThreshold(float thresh);
		
		void removeAngleThreshold();
		
		void addAngleFilter(float mcut, float beta);
		
		void removeAngleFilter();
	};
}
#endif
