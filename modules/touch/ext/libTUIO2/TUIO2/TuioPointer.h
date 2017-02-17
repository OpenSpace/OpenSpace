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

#ifndef INCLUDED_TUIOPOINTER_H
#define INCLUDED_TUIOPOINTER_H

#include "TuioComponent.h"

namespace TUIO2 {
	
	/**
	 * The TuioPointer class encapsulates /tuio2/ptr TUIO pointers.
	 *
	 * @author Martin Kaltenbrunner
	 * @version 2.0.a0
	 */

	class LIBDECL TuioPointer: public TuioComponent {
		
	protected:
		/**
		 * The individual pointer ID number that is assigned to each TuioPointer.
		 */ 
		unsigned int pointer_id;
        
        /**
         * The pointer type ID that is assigned to each TuioPointer.
         */
        unsigned short type_id;
        
        /**
         * The user ID that is assigned to each TuioPointer.
         */
        unsigned short user_id;
        
        /**
         * The shear angle that is assigned to each TuioPointer.
         */
        float shear;
        
        /**
         * The action radius that is assigned to each TuioPointer.
         */
        float radius;
        
        /**
         * The pressure that is assigned to each TuioPointer.
         */
        float pressure;
        
        /**
         * The pressure speed value.
         */
        float pressure_speed;
        
        /**
         * The pressure acceleration value.
         */
        float pressure_accel;
		
	public:
		using TuioComponent::update;
		
		/**
		 * This constructor takes a TuioTime argument and assigns it along with the provided 
		 * Session ID, Pointer ID, X and Y coordinate to the newly created TuioPointer.
		 *
		 * @param	ttime	the TuioTime to assign
		 * @param	tobj	the TuioObject to assign
         * @param	ti	the Type ID  to assign
         * @param	ui	the User ID  to assign
		 * @param	pi	the Pointer ID  to assign
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
         * @param	a	the angle to assign
         * @param	sa	the shear angle to assign
         * @param	r	the radius to assign
         * @param	p	the pressure to assign
		 */
		TuioPointer (TuioTime ttime, TuioObject *tobj, unsigned short ti, unsigned short ui, unsigned int pi, float xp, float yp, float a, float sa, float r, float p);

		/**
		 * This constructor takes the provided Session ID, Pointer ID, X and Y coordinate
		 * and assigs these values to the newly created TuioPointer.
		 *
		 * @param	tobj	the TuioObject to assign
         * @param	ti	the Type ID  to assign
         * @param	ui	the User ID  to assign
         * @param	pi	the Pointer ID  to assign
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
         * @param	a	the angle to assign
         * @param	sa	the shear angle to assign
         * @param	r	the radius to assign
         * @param	p	the pressure to assign
		 */
		TuioPointer (TuioObject *tobj, unsigned short ti, unsigned short ui, unsigned int pi, float xp, float yp, float a, float sa, float r, float p);
        
        
        /**
         * This constructor takes the provided Session ID, Pointer ID, X and Y coordinate
         * and assigs these values to the newly created TuioPointer.
         *
         * @param	tobj	the TuioObject to assign
         * @param	pi	the Pointer ID  to assign
         * @param	xp	the X coordinate to assign
         * @param	yp	the Y coordinate to assign
         * @param	a	the angle to assign
         * @param	sa	the shear angle to assign
         * @param	r	the radius to assign
         * @param	p	the pressure to assign
         */
        TuioPointer (TuioObject *tobj, unsigned int pi, float xp, float yp, float a, float sa, float r, float p);
		
		/**
		 * This constructor takes the atttibutes of the provided TuioPointer 
		 * and assigs these values to the newly created TuioPointer.
		 *
		 * @param	tptr	the TuioPointer to assign
		 */
		TuioPointer (TuioPointer *tptr);
		
		/**
		 * The destructor is doing nothing in particular. 
		 */
		~TuioPointer(){};
        
        /**
         * Takes a TuioTime argument and assigns it along with the provided
         * X and Y coordinate, width, pressure,  X and Y velocity, motion acceleration,
         * @param	ttime	the TuioTime to assign
         * @param	xp	the X coordinate to assign
         * @param	yp	the Y coordinate to assign
         * @param	a	the angle to assign
         * @param	sa	the shear angle to assign
         * @param	r	the radius to assign
         * @param	p	the pressure to assign
         * @param	xs	the X velocity to assign
         * @param	ys	the Y velocity to assign
         * @param	ps	the pressure velocity to assign
         * @param	ma	the motion acceleration to assign
         * @param	pa	the pressure acceleration to assign
         */
        void update (TuioTime ttime, float xp, float yp, float a, float sa, float r, float p, float xs, float ys, float ps, float ma, float pa);
        
        /**
         * Takes a TuioTime argument and assigns it along with the provided
         * X and Y coordinate, width, pressure,  X and Y velocity, motion acceleration,
         * @param	xp	the X coordinate to assign
         * @param	yp	the Y coordinate to assign
         * @param	a	the angle to assign
         * @param	sa	the shear angle to assign
         * @param	r	the radius to assign
         * @param	p	the pressure to assign
         * @param	xs	the X velocity to assign
         * @param	ys	the Y velocity to assign
         * @param	ps	the pressure velocity to assign
         * @param	ma	the motion acceleration to assign
         * @param	pa	the pressure acceleration to assign
         */
        void update (float xp, float yp, float a, float sa, float r, float p, float xs, float ys, float ps, float ma, float pa);
        
        /**
         * Takes a TuioTime argument and assigns it along with the provided
         * X and Y coordinate and angle to the private TuioToken attributes.
         * The speed and accleration values are calculated accordingly.
         *
         * @param	ttime	the TuioTime to assign
         * @param	xp	the X coordinate to assign
         * @param	yp	the Y coordinate to assign
         * @param	a	the angle to assign
         * @param	sa	the shear angle to assign
         * @param	r	the radius to assign
         * @param	p	the pressure to assign
         */
        void update (TuioTime ttime, float xp, float yp, float a, float sa, float r, float p);
        
        /**
         * Takes the atttibutes of the provided TuioPointer
         * and assigs these values to this TuioPointer.
         * The TuioTime time stamp of this TuioComponent remains unchanged.
         *
         * @param	ttok	the TuioComponent to assign
         */
        void update (TuioPointer *tptr);
        
		/**
		 * Returns the Pointer ID of this TuioPointer.
		 * @return	the Pointer ID of this TuioPointer
		 */
		unsigned int getPointerID() const;
        
        /**
         * Returns the Type ID of this TuioPointer.
         * @return	the Type ID of this TuioPointer
         */
        unsigned short getTypeID() const;
        
        /**
         * Returns the User ID of this TuioPointer.
         * @return	the User ID of this TuioPointer
         */
        unsigned short getUserID() const;
        
        /**
         * Returns the encoded Type & User ID of this TuioPointer.
         * @return	the encoded Type & User ID of this TuioPointer
         */
        unsigned int getTypeUserID() const;
        
        /**
         * Decodes and assigns the Type & User ID to this TuioPointer.
         * @param tu_id   the encoded Type & User ID of this TuioPointer
         */
        void setTypeUserID(unsigned int tu_id);
        
        /**
         * Returns the shear angle of this TuioPointer.
         * @return	the shear angle of this TuioPointer
         */
        float getShear() const;
        
        /**
         * Returns the action radius of this TuioPointer.
         * @return	the action radius of this TuioPointer
         */
        float getRadius() const;
        
        /**
         * Returns the Pressure of this TuioPointer.
         * @return	the Pressure of this TuioPointer
         */
        float getPressure() const;
        
        /**
         * Returns the pressure speed of this TuioPointer.
         * @return	the pressure speed of this TuioPointer
         */
        float getPressureSpeed() const;
        
        /**
         * Returns the pressure acceleration of this TuioPointer.
         * @return	the pressure acceleration of this TuioPointer
         */
        float getPressureAccel() const;
	};
}
#endif
