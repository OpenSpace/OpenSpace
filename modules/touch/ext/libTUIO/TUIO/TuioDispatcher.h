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

#ifndef INCLUDED_TUIODISPATCHER_H
#define INCLUDED_TUIODISPATCHER_H

#include "TuioListener.h"

#ifndef WIN32
#include <pthread.h>
#else
#include <windows.h>
#endif

namespace TUIO {
	
	/**
	 * <p>The TuioDispatcher generates TUIO events which are broadcasted to all 
	 * registered classes that implement the {@link TuioListener} interface.</p> 
	 *
	 * @author Martin Kaltenbrunner
	 * @version 1.1.6
	 */ 
	class LIBDECL TuioDispatcher { 
		
	public:
		/**
		 * This constructor creates a TuioDispatcher
		 *
		 */
		TuioDispatcher();

		/**
		 * The destructor is doing nothing in particular. 
		 */
		~TuioDispatcher();
				
		/**
		 * Adds the provided TuioListener to the list of registered TUIO event listeners
		 *
		 * @param  listener  the TuioListener to add
		 */
		void addTuioListener(TuioListener *listener);

		/**
		 * Removes the provided TuioListener from the list of registered TUIO event listeners
		 *
		 * @param  listener  the TuioListener to remove
		 */
		void removeTuioListener(TuioListener *listener);

		/**
		 * Removes all TuioListener from the list of registered TUIO event listeners
		 */
		void removeAllTuioListeners();
		
		/**
		 * Returns a List of all currently active TuioObjects
		 *
		 * @return  a List of all currently active TuioObjects
		 */
		std::list<TuioObject*> getTuioObjects();

		/**
		 * Returns a List with a copy of currently active TuioObjects
		 *
		 * @return  a List with a copy of all currently active TuioObjects
		 */
		std::list<TuioObject> copyTuioObjects();
		
		/**
		 * Returns a List of all currently active TuioCursors
		 *
		 * @return  a List of all currently active TuioCursors
		 */
		std::list<TuioCursor*> getTuioCursors();

		/**
		 * Returns a List with a copy of currently active TuioCursors
		 *
		 * @return  a List with a copy of all currently active TuioCursors
		 */
		std::list<TuioCursor> copyTuioCursors();
		
		/**
		 * Returns a List of all currently active TuioBlobs
		 *
		 * @return  a List of all currently active TuioBlobs
		 */
		std::list<TuioBlob*> getTuioBlobs();

		/**
		 * Returns a List with a copy of currently active TuioBlobs
		 *
		 * @return  a List with a copy of all currently active TuioBlobs
		 */
		std::list<TuioBlob> copyTuioBlobs();
		
		/**
		 * Returns the TuioObject corresponding to the provided Session ID
		 * or NULL if the Session ID does not refer to an active TuioObject
		 *
		 * @return  an active TuioObject corresponding to the provided Session ID or NULL
		 */
		TuioObject* getTuioObject(long s_id);

		/**
		 * Returns the TuioCursor corresponding to the provided Session ID
		 * or NULL if the Session ID does not refer to an active TuioCursor
		 *
		 * @return  an active TuioCursor corresponding to the provided Session ID or NULL
		 */
		TuioCursor* getTuioCursor(long s_id);

		/**
		 * Returns the TuioBlob corresponding to the provided Session ID
		 * or NULL if the Session ID does not refer to an active TuioBlob
		 *
		 * @return  an active TuioBlob corresponding to the provided Session ID or NULL
		 */
		TuioBlob* getTuioBlob(long s_id);
		
		/**
		 * Locks the TuioObject list in order to avoid updates during access
		 */
		void lockObjectList();

		/**
		 * Releases the lock of the TuioObject list
		 */
		void unlockObjectList();

		/**
		 * Locks the TuioCursor list in order to avoid updates during access
		 */
		void lockCursorList();

		/**
		 * Releases the lock of the TuioCursor list
		 */
		void unlockCursorList();

		/**
		 * Locks the TuioBlob list in order to avoid updates during access
		 */
		void lockBlobList();
		
		/**
		 * Releases the lock of the TuioBlob list
		 */
		void unlockBlobList();
		
	protected:
		std::list<TuioListener*> listenerList;
		
		std::list<TuioObject*> objectList;
		std::list<TuioCursor*> cursorList;
		std::list<TuioBlob*> blobList;
		
#ifndef WIN32
		pthread_mutex_t objectMutex;
		pthread_mutex_t cursorMutex;
		pthread_mutex_t blobMutex;
#else
		HANDLE objectMutex;
		HANDLE cursorMutex;
		HANDLE blobMutex;
#endif	
				
	};
}
#endif /* INCLUDED_TUIODISPATCHER_H */
