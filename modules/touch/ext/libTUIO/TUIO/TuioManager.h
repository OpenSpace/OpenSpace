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

#ifndef INCLUDED_TUIOMANAGER_H
#define INCLUDED_TUIOMANAGER_H

#include "TuioDispatcher.h"

#include <iostream>
#include <list>
#include <algorithm>

#define OBJ_MESSAGE_SIZE 108	// setMessage + fseqMessage size
#define CUR_MESSAGE_SIZE 88
#define BLB_MESSAGE_SIZE 116

namespace TUIO {
	/**
	 * <p>The TuioManager class is the central TUIO session management component.</p> 
	 * <p>During runtime the each frame is marked with the initFrame and commitFrame methods, 
	 * while the currently present TuioObjects are managed by the server with ADD, UPDATE and REMOVE methods in analogy to the TuioClient's TuioListener interface.</p> 
	 * <p><code>
	 * TuioManager *manager = new TuioManager();<br/>
	 * ...<br/>
	 * server->initFrame(TuioTime::getSessionTime());<br/>
	 * TuioObject *tobj = server->addTuioObject(xpos,ypos, angle);<br/>
	 * TuioCursor *tcur = server->addTuioObject(xpos,ypos);<br/>
	 * TuioBlob *tblb = server->addTuioBlob(xpos,ypos,width,height,angle);<br/>
	 * server->commitFrame();<br/>
	 * ...<br/>
	 * server->initFrame(TuioTime::getSessionTime());<br/>
	 * server->updateTuioObject(tobj, xpos,ypos, angle);<br/>
	 * server->updateTuioCursor(tcur, xpos,ypos);<br/>
	 * server->updateTuioBlob(tblb, xpos,ypos,width,height,angle);<br/>
	 * server->commitFrame();<br/>
	 * ...<br/>
	 * server->initFrame(TuioTime::getSessionTime());<br/>
	 * server->removeTuioObject(tobj);<br/>
	 * server->removeTuioCursor(tcur);<br/>
	 * server->removeTuioBlob(tblb);<br/>
	 * server->commitFrame();<br/>
	 * </code></p>
	 *
	 * @author Martin Kaltenbrunner
	 * @version 1.1.6
	 */ 
	class LIBDECL TuioManager : public TuioDispatcher { 
	
	public:

		/**
		 * The default constructor creates a TuioManager
		 */
		TuioManager();

		/**
		 * The destructor is doing nothing in particular. 
		 */
		~TuioManager();
		
		/**
		 * Creates a new TuioObject based on the given arguments.
		 * The new TuioObject is added to the TuioServer's internal list of active TuioObjects 
		 * and a reference is returned to the caller.
		 *
		 * @param	sym	the Symbol ID  to assign
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 * @param	a	the angle to assign
		 * @return	reference to the created TuioObject
		 */
		TuioObject* addTuioObject(int sym, float xp, float yp, float a);

		/**
		 * Updates the referenced TuioObject based on the given arguments.
		 *
		 * @param	tobj	the TuioObject to update
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 * @param	a	the angle to assign
		 */
		void updateTuioObject(TuioObject *tobj, float xp, float yp, float a);

		/**
		 * Removes the referenced TuioObject from the TuioServer's internal list of TuioObjects
		 * and deletes the referenced TuioObject afterwards
		 *
		 * @param	tobj	the TuioObject to remove
		 */
		void removeTuioObject(TuioObject *tobj);

		/**
		 * Adds an externally managed TuioObject to the TuioServer's internal list of active TuioObjects 
		 *
		 * @param	tobj	the TuioObject to add
		 */
		void addExternalTuioObject(TuioObject *tobj);

		/**
		 * Updates an externally managed TuioObject 
		 *
		 * @param	tobj	the TuioObject to update
		 */
		void updateExternalTuioObject(TuioObject *tobj);

		/**
		 * Removes an externally managed TuioObject from the TuioServer's internal list of TuioObjects
		 * The referenced TuioObject is not deleted
		 *
		 * @param	tobj	the TuioObject to remove
		 */
		void removeExternalTuioObject(TuioObject *tobj);
		
		/**
		 * Creates a new TuioCursor based on the given arguments.
		 * The new TuioCursor is added to the TuioServer's internal list of active TuioCursors 
		 * and a reference is returned to the caller.
		 *
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 * @return	reference to the created TuioCursor
		 */
		TuioCursor* addTuioCursor(float xp, float yp);

		/**
		 * Updates the referenced TuioCursor based on the given arguments.
		 *
		 * @param	tcur	the TuioObject to update
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 */
		void updateTuioCursor(TuioCursor *tcur, float xp, float yp);

		/**
		 * Removes the referenced TuioCursor from the TuioServer's internal list of TuioCursors
		 * and deletes the referenced TuioCursor afterwards
		 *
		 * @param	tcur	the TuioCursor to remove
		 */
		void removeTuioCursor(TuioCursor *tcur);

		/**
		 * Adds an externally managed TuioCursor 
		 *
		 * @param	tcur	the TuioCursor to add
		 */
		void addExternalTuioCursor(TuioCursor *tcur);

		/**
		 * Updates an externally managed TuioCursor 
		 *
		 * @param	tcur	the TuioCursor to update
		 */
		void updateExternalTuioCursor(TuioCursor *tcur);

		/**
		 * Removes an externally managed TuioCursor from the TuioServer's internal list of TuioCursor
		 * The referenced TuioCursor is not deleted
		 *
		 * @param	tcur	the TuioCursor to remove
		 */
		void removeExternalTuioCursor(TuioCursor *tcur);

		/**
		 * Creates a new TuioBlob based on the given arguments.
		 * The new TuioBlob is added to the TuioServer's internal list of active TuioBlobs 
		 * and a reference is returned to the caller.
		 *
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 * @param	angle	the angle to assign
		 * @param	width	the width to assign
		 * @param	height	the height to assign
		 * @param	area	the area to assign
		 * @return	reference to the created TuioBlob
		 */
		TuioBlob* addTuioBlob(float xp, float yp, float angle, float width, float height, float area);
		
		/**
		 * Updates the referenced TuioBlob based on the given arguments.
		 *
		 * @param	tblb	the TuioObject to update
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 * @param	angle	the angle to assign
		 * @param	width	the width to assign
		 * @param	height	the height to assign
		 * @param	area	the area to assign
		 */
		void updateTuioBlob(TuioBlob *tblb, float xp, float yp, float angle, float width, float height, float area);
		
		/**
		 * Removes the referenced TuioBlob from the TuioServer's internal list of TuioBlobs
		 * and deletes the referenced TuioBlob afterwards
		 *
		 * @param	tblb	the TuioBlob to remove
		 */
		void removeTuioBlob(TuioBlob *tblb);
		
		/**
		 * Updates an externally managed TuioBlob 
		 *
		 * @param	tblb	the TuioBlob to update
		 */
		void addExternalTuioBlob(TuioBlob *tblb);
		
		/**
		 * Updates an externally managed TuioBlob 
		 *
		 * @param	tblb	the TuioBlob to update
		 */
		void updateExternalTuioBlob(TuioBlob *tblb);
		
		/**
		 * Removes an externally managed TuioBlob from the TuioServer's internal list of TuioBlob
		 * The referenced TuioBlob is not deleted
		 *
		 * @param	tblb	the TuioBlob to remove
		 */
		void removeExternalTuioBlob(TuioBlob *tblb);		
		
		/**
		 * Initializes a new frame with the given TuioTime
		 *
		 * @param	ttime	the frame time
		 */
		void initFrame(TuioTime ttime);
		
		/**
		 * Commits the current frame.
		 * Generates and sends TUIO messages of all currently active and updated TuioObjects and TuioCursors.
		 */
		void commitFrame();

		/**
		 * Returns the next available Session ID for external use.
		 * @return	the next available Session ID for external use
		 */
		long getSessionID();

		/**
		 * Returns the current frame ID for external use.
		 * @return	the current frame ID for external use
		 */
		long getFrameID();
		
		/**
		 * Returns the current frame ID for external use.
		 * @return	the current frame ID for external use
		 */
		TuioTime getFrameTime();
		
		/**
		 * Returns a List of all currently inactive TuioObjects
		 *
		 * @return  a List of all currently inactive TuioObjects
		 */
		std::list<TuioObject*> getUntouchedObjects();

		/**
		 * Returns a List of all currently inactive TuioCursors
		 *
		 * @return  a List of all currently inactive TuioCursors
		 */
		std::list<TuioCursor*> getUntouchedCursors();

		/**
		 * Returns a List of all currently inactive TuioBlobs
		 *
		 * @return  a List of all currently inactive TuioBlobs
		 */
		std::list<TuioBlob*> getUntouchedBlobs();
		
		/**
		 * Calculates speed and acceleration values for all currently inactive TuioObjects
		 */
		void stopUntouchedMovingObjects();

		/**
		 * Calculates speed and acceleration values for all currently inactive TuioCursors
		 */
		void stopUntouchedMovingCursors();

		/**
		 * Calculates speed and acceleration values for all currently inactive TuioBlobs
		 */
		void stopUntouchedMovingBlobs();
		
		/**
		 * Removes all currently inactive TuioObjects from the TuioServer's internal list of TuioObjects
		 */
		void removeUntouchedStoppedObjects();

		/**
		 * Removes all currently inactive TuioCursors from the TuioServer's internal list of TuioCursors
		 */
		void removeUntouchedStoppedCursors();

		/**
		 * Removes all currently inactive TuioCursors from the TuioServer's internal list of TuioBlobs
		 */
		void removeUntouchedStoppedBlobs();
		
		/**
		 * Returns the TuioObject closest to the provided coordinates
		 * or NULL if there isn't any active TuioObject
		 *
		 * @return  the closest TuioObject to the provided coordinates or NULL
		 */
		TuioObject* getClosestTuioObject(float xp, float yp);
		
		/**
		 * Returns the TuioCursor closest to the provided coordinates
		 * or NULL if there isn't any active TuioCursor
		 *
		 * @return  the closest TuioCursor corresponding to the provided coordinates or NULL
		 */
		TuioCursor* getClosestTuioCursor(float xp, float yp);

		/**
		 * Returns the TuioBlob closest to the provided coordinates
		 * or NULL if there isn't any active TuioBlob
		 *
		 * @return  the closest TuioBlob corresponding to the provided coordinates or NULL
		 */
		TuioBlob* getClosestTuioBlob(float xp, float yp);
		
		/**
		 * The TuioServer prints verbose TUIO event messages to the console if set to true.
		 * @param	verbose	print verbose messages if set to true
		 */
		void setVerbose(bool verbose) { this->verbose=verbose; }
		bool isVerbose() { return verbose; }

		void setInversion(bool ix, bool iy, bool ia) { 
			invert_x = ix; 
			invert_y = iy; 
			invert_a = ia; 
		};

		void setInvertXpos(bool ix) { invert_x = ix; };
		void setInvertYpos(bool iy) { invert_y = iy; };
		void setInvertAngle(bool ia) { invert_a = ia; };
		bool getInvertXpos() { return invert_x; };
		bool getInvertYpos() { return invert_y; };
		bool getInvertAngle() { return invert_a; };
		void resetTuioObjects();
		void resetTuioCursors();		
		void resetTuioBlobs();		
		
	protected:
		std::list<TuioCursor*> freeCursorList;
		std::list<TuioCursor*> freeCursorBuffer;

		std::list<TuioBlob*> freeBlobList;
		std::list<TuioBlob*> freeBlobBuffer;

		TuioTime currentFrameTime;
		long currentFrame;
		int maxCursorID;
		int maxBlobID;
		long sessionID;

		bool updateObject;
		bool updateCursor;
		bool updateBlob;
		bool verbose;

		bool invert_x;
		bool invert_y;
		bool invert_a;
	};
}
#endif /* INCLUDED_TUIOMANAGER_H */
