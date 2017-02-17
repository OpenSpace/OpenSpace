/*
 TUIO2 C++ Library
 Copyright (c) 2009-2014 Martin Kaltenbrunner <martin@tuio.org>
 
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

#ifndef INCLUDED_TUIOBOUNDS_H
#define INCLUDED_TUIOBOUNDS_H

#include "TuioComponent.h"

namespace TUIO2 {
	
	/**
	 * The TuioBounds class encapsulates /tuio2/bnd TUIO boundary ellipses.
	 *
	 * @author Martin Kaltenbrunner
	 * @version 2.0.a0
	 */

	class LIBDECL TuioBounds: public TuioComponent {
		
	protected:
		/**
		 * The rotation angle value.
		 */ 
		float angle;
		/**
		 * The width value.
		 */ 
		float width;
		/**
		 * The height value.
		 */ 
		float height;
		/**
		 * The area value.
		 */ 
		float area;
		/**
		 * The rotation speed value.
		 */ 
		float rotation_speed;
		/**
		 * The rotation acceleration value.
		 */ 
		float rotation_accel;
		
	public:
		using TuioComponent::update;
		
		/**
		 * This constructor takes a TuioTime argument and assigns it along with the provided 
		 * Session ID, X and Y coordinate, width, height and angle to the newly created TuioBounds.
		 *
		 * @param	ttime	the TuioTime to assign
		 * @param	tobj	the TuioObject to assign
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 * @param	a	the angle to assign
		 * @param	w	the width to assign
		 * @param	h	the height to assign
		 * @param	f	the area to assign
		 */
		TuioBounds (TuioTime ttime, TuioObject *tobj, float xp, float yp, float a, float w, float h, float f);
        
        /**
         * This constructor takes a TuioTime argument and assigns it along with the provided
         * Session ID, X and Y coordinate, width, height and angle to the newly created TuioBounds.
         *
         * @param	tobj	the TuioObject to assign
         * @param	xp	the X coordinate to assign
         * @param	yp	the Y coordinate to assign
         * @param	a	the angle to assign
         * @param	w	the width to assign
         * @param	h	the height to assign
         * @param	f	the area to assign
         */
        TuioBounds (TuioObject *tobj, float xp, float yp, float a, float w, float h, float f);
		
		/**
		 * This constructor takes the atttibutes of the provided TuioBounds 
		 * and assigs these values to the newly created TuioBounds.
		 *
		 * @param	tbnd	the TuioBounds to assign
		 */
		TuioBounds (TuioBounds *tbnd);
		
		/**
		 * The destructor is doing nothing in particular. 
		 */
		~TuioBounds() {};
		
		/**
		 * Takes a TuioTime argument and assigns it along with the provided 
		 * X and Y coordinate, angle, X and Y velocity, motion acceleration,
		 * rotation speed and rotation acceleration to the private TuioBounds attributes.
		 *
		 * @param	ttime	the TuioTime to assign
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 * @param	a	the rotation angle to assign
		 * @param	w	the width to assign
		 * @param	h	the height to assign
		 * @param	f	the area to assign
		 * @param	xs	the X velocity to assign
		 * @param	ys	the Y velocity to assign
		 * @param	rs	the rotation velocity to assign
		 * @param	ma	the motion acceleration to assign
		 * @param	ra	the rotation acceleration to assign
		 */
		void update (TuioTime ttime, float xp, float yp, float a, float w, float h, float f, float xs, float ys, float rs, float ma, float ra);

		/**
		 * Assigns the provided X and Y coordinate, angle, X and Y velocity, motion acceleration
		 * rotation velocity and rotation acceleration to the private TuioComponent attributes.
		 * The TuioTime time stamp remains unchanged.
		 *
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 * @param	a	the angle to assign
		 * @param	w	the width to assign
		 * @param	h	the height to assign
		 * @param	f	the area to assign
		 * @param	xs	the X velocity to assign
		 * @param	ys	the Y velocity to assign
		 * @param	rs	the rotation velocity to assign
		 * @param	ma	the motion acceleration to assign
		 * @param	ra	the rotation acceleration to assign
		 */
		void update (float xp, float yp, float a, float w, float h, float f, float xs, float ys, float rs, float ma, float ra);
		
		/**
		 * Takes a TuioTime argument and assigns it along with the provided 
		 * X and Y coordinate and angle to the private TuioBounds attributes.
		 * The speed and accleration values are calculated accordingly.
		 *
		 * @param	ttime	the TuioTime to assign
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 * @param	a	the angle coordinate to assign
		 * @param	w	the width to assign
		 * @param	h	the height to assign
		 * @param	f	the area to assign
		 */
		void update (TuioTime ttime, float xp, float yp, float a, float w, float h, float f);

		/**
		 * This method is used to calculate the speed and acceleration values of a
		 * TuioBounds with unchanged position and angle.
		 */
		void stop (TuioTime ttime);
		
		/**
		 * Takes the atttibutes of the provided TuioBounds 
		 * and assigs these values to this TuioBounds.
		 * The TuioTime time stamp of this TuioComponent remains unchanged.
		 *
		 * @param	tbnd	the TuioComponent to assign
		 */	
		void update (TuioBounds *tbnd);
		
		/**
		 * Returns the width of this TuioBounds.
		 * @return	the width of this TuioBounds
		 */
		float getWidth() const;

		/**
		 * Returns the height of this TuioBounds.
		 * @return	the height of this TuioBounds
		 */
		float getHeight() const;

		/**
		 * Returns the width of this TuioBounds.
		 * @return	the width of this TuioBounds
		 */
		int getScreenWidth(int w) const;
		
		/**
		 * Returns the height of this TuioBounds.
		 * @return	the height of this TuioBounds
		 */
		int getScreenHeight(int h) const;
		
		/**
		 * Returns the area of this TuioBounds.
		 * @return	the area of this TuioBounds
		 */
		float getArea() const;
	};
}
#endif
