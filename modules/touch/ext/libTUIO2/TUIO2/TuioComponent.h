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

#ifndef INCLUDED_TUIOCOMPONENT_H
#define INCLUDED_TUIOCOMPONENT_H

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

namespace TUIO2 {
	
	/**
	 * The abstract TuioComponent class defines common attributes that apply to all subclasses {@link TuioToken}, {@link TuioPointer} and {@link TuioBounds}.
	 *
	 * @author Martin Kaltenbrunner
	 * @version 2.0.a0
	 */
    
    class TuioObject;
	class LIBDECL TuioComponent: public TuioPoint {
		
	protected:
		/**
		 * The TuioObject that contains this TUIO component.
		 */ 
        TuioObject *container;
        /**
         * The rotation angle value.
         */
        float angle;
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
        /**
         * The rotation speed value.
         */
        float rotation_speed;
        /**
         * The rotation acceleration value.
         */ 
        float rotation_accel;
		/**
		 * A List of TuioPoints containing all the previous positions of the TUIO component.
		 */ 
		std::list<TuioPoint> path;
		/**
		 * Reflects the current state of the TuioComponent
		 */ 
		int state;
	
	public:
		using TuioPoint::update;
		
		/**
		 * This constructor takes a TuioTime argument and assigns it along with the provided 
		 * Session ID, X and Y coordinate to the newly created TuioComponent.
		 *
		 * @param	ttime	the TuioTime to assign
		 * @param	tobj	the TuioObject to assign
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
         * @param	a	the angle to assign
		 */
		TuioComponent (TuioTime ttime, TuioObject *tobj, float xp, float yp, float a);

		/**
		 * This constructor takes the provided Session ID, X and Y coordinate
		 * and assigs these values to the newly created TuioComponent.
		 *
		 * @param	tobj	the TuioObject to assign
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
         * @param	a	the angle to assign
		 */
		TuioComponent (TuioObject *tobj, float xp, float yp,float a);
		
		/**
		 * This constructor takes the atttibutes of the provided TuioComponent 
		 * and assigs these values to the newly created TuioComponent.
		 *
		 * @param	tcon	the TuioComponent to assign
		 */
		TuioComponent (TuioComponent *tcon);
		
		/**
		 * The destructor is doing nothing in particular. 
		 */
		virtual ~TuioComponent(){};
        
        /**
         * Returns the TuioObject containing this TuioComponent.
         * @return	the TuioObject containing this TuioComponent
         */
        virtual TuioObject* getContainingTuioObject();
        
        /**
         * Returns the TuioObject containing this TuioComponent.
         * @param	the TuioObject containing this TuioComponent
         */
        virtual void setContainingTuioObject(TuioObject *tobj);
        
        /**
         * Takes a TuioTime argument and assigns it along with the provided
         * X and Y coordinate to the private TuioComponent attributes.
         * The speed and accleration values are calculated accordingly.
         *
         * @param	ttime	the TuioTime to assign
         * @param	xp	the X coordinate to assign
         * @param	yp	the Y coordinate to assign
         * @param	a	the angle to assign
         */
		virtual void update (TuioTime ttime, float xp, float yp, float a);
		
		/**
		 * This method is used to calculate the speed and acceleration values of
		 * TuioComponents with unchanged positions.
		 */
		virtual void stop(TuioTime ttime);

		/**
		 * Takes a TuioTime argument and assigns it along with the provided 
		 * X and Y coordinate, X and Y velocity and acceleration
		 * to the private TuioComponent attributes.
		 *
		 * @param	ttime	the TuioTime to assign
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
         * @param	a	the rotation angle to assign
		 * @param	xs	the X velocity to assign
		 * @param	ys	the Y velocity to assign
         * @param	rs	the rotation velocity to assign
		 * @param	ma	the motion acceleration to assign
         * @param	ra	the rotation acceleration to assign
		 */
		virtual void update (TuioTime ttime, float xp, float yp, float a, float xs, float ys, float rs, float ma, float ra);
		
		/**
		 * Assigns the provided X and Y coordinate, X and Y velocity and acceleration
		 * to the private TuioComponent attributes. The TuioTime time stamp remains unchanged.
		 *
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
         * @param	a	the rotation angle to assign
		 * @param	xs	the X velocity to assign
		 * @param	ys	the Y velocity to assign
         * @param	rs	the rotation velocity to assign
		 * @param	ma	the motion acceleration to assign
         * @param	ra	the rotation acceleration to assign
		 */
		virtual void update (float xp, float yp, float a, float xs, float ys, float rs, float ma, float ra);
		
		/**
		 * Takes the atttibutes of the provided TuioComponent 
		 * and assigs these values to this TuioComponent.
		 * The TuioTime time stamp of this TuioComponent remains unchanged.
		 *
		 * @param	tcon	the TuioComponent to assign
		 */
		virtual void update(TuioComponent *tcon);
		
		/**
		 * Assigns the REMOVE state to this TuioComponent and sets
		 * its TuioTime time stamp to the provided TuioTime argument.
		 *
		 * @param	ttime	the TuioTime to assign
		 */
		virtual void remove(TuioTime ttime);
       
        /**
         * Returns the Session ID of this TuioComponent.
         * @return	the Session ID of this TuioComponent
         */
        virtual unsigned int getSessionID() const;
		
		/**
		 * Returns the X velocity of this TuioComponent.
		 * @return	the X velocity of this TuioComponent
		 */
		virtual float getXSpeed() const;

		/**
		 * Returns the Y velocity of this TuioComponent.
		 * @return	the Y velocity of this TuioComponent
		 */
		virtual float getYSpeed() const;
		
		/**
		 * Returns the position of this TuioComponent.
		 * @return	the position of this TuioComponent
		 */
		virtual TuioPoint getPosition() const;
		
		/**
		 * Returns the path of this TuioComponent.
		 * @return	the path of this TuioComponent
		 */
		virtual std::list<TuioPoint> getPath() const;
		
		/**
		 * Returns the motion speed of this TuioComponent.
		 * @return	the motion speed of this TuioComponent
		 */
		virtual float getMotionSpeed() const;
		
		/**
		 * Returns the motion acceleration of this TuioComponent.
		 * @return	the motion acceleration of this TuioComponent
		 */
		virtual float getMotionAccel() const;
		
        /**
         * Returns the rotation angle of this TuioComponent.
         * @return	the rotation angle of this TuioComponent
         */
        float getAngle() const;
        
        /**
         * Returns the rotation angle in degrees of this TuioComponent.
         * @return	the rotation angle in degrees of this TuioComponent
         */
        float getAngleDegrees() const;
        
        /**
         * Returns the rotation speed of this TuioComponent.
         * @return	the rotation speed of this TuioComponent
         */
        float getRotationSpeed() const;
        
        /**
         * Returns the rotation acceleration of this TuioComponent.
         * @return	the rotation acceleration of this TuioComponent
         */
        float getRotationAccel() const;
        
		/**
		 * Returns the TUIO state of this TuioComponent.
		 * @return	the TUIO state of this TuioComponent
		 */
		virtual int getTuioState() const;
        
		/**
		 * Returns true of this TuioComponent is moving.
		 * @return	true of this TuioComponent is moving
		 */
		virtual bool isMoving() const;
	};
}
#endif // INCLUDED_TUIOCOMPONENT_H