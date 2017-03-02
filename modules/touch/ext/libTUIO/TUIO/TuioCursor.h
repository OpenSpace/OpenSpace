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

#ifndef INCLUDED_TUIOCURSOR_H
#define INCLUDED_TUIOCURSOR_H

#include "TuioContainer.h"

namespace TUIO {
	
	/**
	 * The TuioCursor class encapsulates /tuio/2Dcur TUIO cursors.
	 *
	 * @author Martin Kaltenbrunner
	 * @version 1.1.6
	 */ 
	class LIBDECL TuioCursor: public TuioContainer {
		
	protected:
		/**
		 * The individual cursor ID number that is assigned to each TuioCursor.
		 */ 
		int cursor_id;
		
	public:
		using TuioContainer::update;
		
		/**
		 * This constructor takes a TuioTime argument and assigns it along with the provided 
		 * Session ID, Cursor ID, X and Y coordinate to the newly created TuioCursor.
		 *
		 * @param	ttime	the TuioTime to assign
		 * @param	si	the Session ID  to assign
		 * @param	ci	the Cursor ID  to assign
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 */
		TuioCursor (TuioTime ttime, long si, int ci, float xp, float yp);

		/**
		 * This constructor takes the provided Session ID, Cursor ID, X and Y coordinate 
		 * and assigs these values to the newly created TuioCursor.
		 *
		 * @param	si	the Session ID  to assign
		 * @param	ci	the Cursor ID  to assign
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 */
		TuioCursor (long si, int ci, float xp, float yp);
		
		/**
		 * This constructor takes the atttibutes of the provided TuioCursor 
		 * and assigs these values to the newly created TuioCursor.
		 *
		 * @param	tcur	the TuioCursor to assign
		 */
		TuioCursor (TuioCursor *tcur);

		/**
		 * The destructor is doing nothing in particular. 
		 */
		virtual ~TuioCursor(){};
		
		/**
		 * Returns the Cursor ID of this TuioCursor.
		 * @return	the Cursor ID of this TuioCursor
		 */
		int getCursorID() const;
	};
}
#endif
