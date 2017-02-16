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

#ifndef INCLUDED_TUIOMANAGER_H
#define INCLUDED_TUIOMANAGER_H

#include "TuioDispatcher.h"

#include <iostream>
#include <list>
#include <algorithm>

namespace TUIO2 {
	/**
	 * <p>The TuioManager class is the central TUIO session management component.</p> 
	 * <p>During runtime the each frame is marked with the initFrame and commitFrame methods, 
	 * while the currently present TuioTokens are managed by the server with ADD, UPDATE and REMOVE methods in analogy to the TuioClient's TuioListener interface.</p> 
	 * <p><code>
	 * TuioManager *manager = new TuioManager();<br/>
	 * ...<br/>
	 * server->initTuioFrame(TuioTime::getSessionTime());<br/>
	 * TuioToken *ttok = server->addTuioToken(xpos,ypos, angle);<br/>
	 * TuioPointer *tptr = server->addTuioToken(xpos,ypos);<br/>
	 * TuioBounds *tbnd = server->addTuioBounds(xpos,ypos,width,height,angle);<br/>
	 * server->commitFrame();<br/>
	 * ...<br/>
	 * server->initFrame(TuioTime::getSessionTime());<br/>
	 * server->updateTuioToken(ttok, xpos,ypos, angle);<br/>
	 * server->updateTuioPointer(tptr, xpos,ypos);<br/>
	 * server->updateTuioBounds(tbnd, xpos,ypos,width,height,angle);<br/>
	 * server->commitFrame();<br/>
	 * ...<br/>
	 * server->initFrame(TuioTime::getSessionTime());<br/>
	 * server->removeTuioToken(ttok);<br/>
	 * server->removeTuioPointer(tptr);<br/>
	 * server->removeTuioBounds(tbnd);<br/>
	 * server->commitFrame();<br/>
	 * </code></p>
	 *
	 * @author Martin Kaltenbrunner
	 * @version 2.0.a0
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
         * Creates an internally managed TuioObject with a new Session ID
         * and adds it to the TuioServer's internal list of TuioObjects
         *
         * @return	the newly created but empty TuioObject
         */
        TuioObject* createTuioObject();
        
        /**
         * Removes an internally managed TuioObject from the TuioServer's internal list of TuioObjects
         * The provided TuioObject (and all its encapsulated TUIO Components) are deleted
         *
         * @param	s_id	the Session ID of the internal TuioObject to remove
         */
        void removeTuioObject(unsigned int s_id);
        
        /**
         * Removes an internally managed TuioObject from the TuioServer's internal list of TuioObjects
         * The provided TuioObject (and all its encapsulated TUIO Components) are deleted
         *
         * @param	tobj	the internal TuioObject to remove
         */
        void removeTuioObject(TuioObject *tobj);
		
        /**
         * Creates a new TuioToken based on the given arguments.
         * A new TuioObject containing that TuioToken is added 
         * to the TuioServer's internal list of TuioObjects
         * and the TuioToken reference is returned to the caller.
         *
         * @param	sym	the Symbol ID  to assign
         * @param	xp	the X coordinate to assign
         * @param	yp	the Y coordinate to assign
         * @param	a	the angle to assign
         * @return	reference to the created TuioToken
         */
        TuioObject* createTuioToken(unsigned int sym, float xp, float yp, float a);
        
        /**
         * Creates a new TuioToken based on the given arguments.
         * A new TuioObject containing that TuioToken is added
         * to the TuioServer's internal list of TuioObjects
         * and the TuioToken reference is returned to the caller.
         *
         * @param	t_id	the Type ID to assign
         * @param	u_id	the User ID to assign
         * @param	sym	the Symbol ID  to assign
         * @param	xp	the X coordinate to assign
         * @param	yp	the Y coordinate to assign
         * @param	a	the angle to assign
         * @return	a reference to the TuioObject of the TuioToken
         */
        TuioObject* createTuioToken(unsigned int sym, unsigned short t_id, unsigned short u_id, float xp, float yp, float a);
        
        /**
         * Creates a new TuioToken based on the given arguments.
         * and adds it to an existing TuioObject with the Session ID
         * from the TuioServer's internal list of TuioObjects
         * and the TuioToken reference is returned to the caller.
         *
         * @param	tobj	the existing TuioObject
         * @param	t_id	the Type ID to assign
         * @param	u_id	the User ID to assign
         * @param	sym	the Symbol ID to assign
         * @param	xp	the X coordinate to assign
         * @param	yp	the Y coordinate to assign
         * @param	a	the angle to assign
         * @return	a reference to the TuioObject of the TuioToken
         */
        TuioObject* addTuioToken(TuioObject *tobj, unsigned short t_id, unsigned short u_id, unsigned int sym, float xp, float yp, float a);
        
        /**
         * Creates a new TuioToken based on the given arguments.
         * and adds it to an existing TuioObject with the Session ID
         * from the TuioServer's internal list of TuioObjects
         * and the TuioToken reference is returned to the caller.
         *
         * @param	tobj	the existing TuioObject
         * @param	t_id	the Type ID to assign
         * @param	u_id	the User ID to assign
         * @param	sym	the Symbol ID to assign
         * @param	xp	the X coordinate to assign
         * @param	yp	the Y coordinate to assign
         * @param	a	the angle to assign
         * @return	a reference to the TuioObject of the TuioToken
         */
        TuioObject* addTuioToken(TuioObject *tobj, unsigned int sym, float xp, float yp, float a);
        
        /**
         * Adds the provided TuioToken to an existing TuioObject
         * from the TuioServer's internal list of TuioObjects
         * or creates the according TuioObject if not found.
         *
         * @param	ttok	the existing TuioToken to add
         * @return	a reference to the TuioObject of the TuioToken
         */
        TuioObject* addTuioToken(TuioToken *ttok);
        
        /**
         * Updates the referenced TuioToken based on the given arguments.
         *
         * @param	ttok	the TuioToken to update
         * @param	xp	the X coordinate to assign
         * @param	yp	the Y coordinate to assign
         * @param	a	the angle to assign
         */
        void updateTuioToken(TuioToken *ttok, float xp, float yp, float a);
        
        /**
         * Removes the provided TuioToken from an existing TuioObject
         * and also deletes the TuioObject with all other components
         *
         * @param	ttok	the TuioToken to remove
         */
        void removeTuioToken(TuioToken *ttok);
        
		/**
		 * Creates a new TuioPointer based on the given arguments.
		 * The new TuioPointer is added to the TuioServer's internal list of active TuioPointers 
		 * and a reference is returned to the caller.
		 *
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
         * @param	a	the rotation angle to assign
         * @param	s	the shear angle to assign
         * @param	r	the radius to assign
         * @param	p	the pressure to assign
		 * @return	a reference to the TuioObject of the TuioPointer
		 */
		TuioObject* createTuioPointer(float xp, float yp, float a, float s, float r, float p);
        
        /**
         * Creates a new TuioPointer based on the given arguments.
         * and adds it to an existing TuioObject with the Session ID
         * The new TuioPointer is added to the TuioServer's internal list of active TuioPointers
         * and a reference of the TuioObject is returned to the caller.
         *
         * @param	tobj	the existing TuioObject
         * @param	xp	the X coordinate to assign
         * @param	yp	the Y coordinate to assign
         * @param	a	the rotation angle to assign
         * @param	s	the shear angle to assign
         * @param	r	the radius to assign
         * @param	p	the pressure to assign
         * @return	a reference to the TuioObject of the TuioPointer
         */
        TuioObject* addTuioPointer(TuioObject *tobj, float xp, float yp, float a, float s, float r, float p);
        
        /**
         * Creates a new TuioPointer based on the given arguments.
         * The new TuioPointer is added to the TuioServer's internal list of active TuioPointers
         * and a reference is returned to the caller.
         *
         * @param	t_id	the Type ID to assign
         * @param	u_id	the User ID to assign
         * @param	xp	the X coordinate to assign
         * @param	yp	the Y coordinate to assign
         * @param	a	the rotation angle to assign
         * @param	s	the shear angle to assign
         * @param	r	the radius to assign
         * @param	p	the pressure to assign
         * @return	a reference to the TuioObject of the TuioPointer
         */
        TuioObject* createTuioPointer(unsigned short t_id, unsigned short u_id, float xp, float yp, float a, float s, float r, float p);
     
        /**
         * Creates a new TuioPointer based on the given arguments.
         * The new TuioPointer is added to the TuioServer's internal list of active TuioPointers
         * and a reference is returned to the caller.
         *
         * @param	tobj	the existing TuioObject
         * @param	t_id	the Type ID to assign
         * @param	u_id	the User ID to assign
         * @param	xp	the X coordinate to assign
         * @param	yp	the Y coordinate to assign
         * @param	a	the rotation angle to assign
         * @param	s	the shear angle to assign
         * @param	r	the radius to assign
         * @param	p	the pressure to assign
         * @return	a reference to the TuioObject of the TuioPointer
         */
        TuioObject* addTuioPointer(TuioObject *tobj, unsigned short t_id, unsigned short u_id, float xp, float yp, float a, float s, float r, float p);
        
        /**
         * Creates a new TuioPointer based on the given arguments.
         * The new TuioPointer is added to the TuioServer's internal list of active TuioPointers
         * and a reference is returned to the caller.
         *
         * @param	p_id	the Pointer ID to assign
         * @param	xp	the X coordinate to assign
         * @param	xp	the X coordinate to assign
         * @param	yp	the Y coordinate to assign
         * @param	a	the rotation angle to assign
         * @param	s	the shear angle to assign
         * @param	r	the radius to assign
         * @param	p	the pressure to assign
         * @return	reference to the created TuioPointer
         */
        TuioObject* createTuioPointer(unsigned int p_id, float xp, float yp, float a, float s, float r, float p);
        
        /**
         * Creates a new TuioPointer based on the given arguments.
         * The new TuioPointer is added to the TuioServer's internal list of active TuioPointers
         * and a reference is returned to the caller.
         *
         * @param	tobj	the existing TuioObject
         * @param	p_id	the Pointer ID to assign
         * @param	xp	the X coordinate to assign
         * @param	xp	the X coordinate to assign
         * @param	yp	the Y coordinate to assign
         * @param	a	the rotation angle to assign
         * @param	s	the shear angle to assign
         * @param	r	the radius to assign
         * @param	p	the pressure to assign
         * @return	reference to the created TuioPointer
         */
        TuioObject* addTuioPointer(TuioObject *tobj, unsigned int p_id, float xp, float yp, float a, float s, float r, float p);
        
        /**
         * Creates a new TuioPointer based on the given arguments.
         * The new TuioPointer is added to the TuioServer's internal list of active TuioPointers
         * and a reference is returned to the caller.
         *
         * @param	p_id	the Pointer ID to assign
         * @param	t_id	the Type ID to assign
         * @param	u_id	the User ID to assign
         * @param	xp	the X coordinate to assign
         * @param	xp	the X coordinate to assign
         * @param	yp	the Y coordinate to assign
         * @param	a	the rotation angle to assign
         * @param	s	the shear angle to assign
         * @param	r	the radius to assign
         * @param	p	the pressure to assign
         * @return	a reference to the TuioObject of the TuioPointer
         */
        TuioObject* createTuioPointer(unsigned int p_id, unsigned short t_id, unsigned short u_id, float xp, float yp, float a, float s, float r, float p);
        
        /**
         * Creates a new TuioPointer based on the given arguments.
         * The new TuioPointer is added to the TuioServer's internal list of active TuioPointers
         * and a reference is returned to the caller.
         *
         * @param	tobj	the existing TuioObject
         * @param	p_id	the Pointer ID to assign
         * @param	t_id	the Type ID to assign
         * @param	u_id	the User ID to assign
         * @param	xp	the X coordinate to assign
         * @param	xp	the X coordinate to assign
         * @param	yp	the Y coordinate to assign
         * @param	a	the rotation angle to assign
         * @param	s	the shear angle to assign
         * @param	r	the radius to assign
         * @param	p	the pressure to assign
         * @return	a reference to the TuioObject of the TuioPointer
         */
        TuioObject* addTuioPointer(TuioObject *tobj, unsigned int p_id, unsigned short t_id, unsigned short u_id, float xp, float yp, float a, float s, float r, float p);
        
        /**
         * Adds the provided TuioPointer to an existing TuioObject
         * from the TuioServer's internal list of TuioObjects
         * or creates the according TuioObject if not found.
         *
         * @param	tptr	the existing TuioPointer to add
         * @return	a reference to the TuioObject of the TuioPointer
         */
        TuioObject* addTuioPointer(TuioPointer *tptr);

		/**
		 * Updates the referenced TuioPointer based on the given arguments.
		 *
		 * @param	tptr	the TuioPointer to update
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
         * @param	a	the rotation angle to assign
         * @param	s	the shear angle to assign
         * @param	r	the radius to assign
         * @param	p	the pressure to assign
		 */
		void updateTuioPointer(TuioPointer *tptr, float xp, float yp, float a, float s, float r, float p);

        /**
         * Removes the provided TuioPointer from an existing TuioObject
         * and also deletes the TuioObject with all other components
         *
         * @param	tptr	the TuioPointer to remove
         */
        void removeTuioPointer(TuioPointer *tptr);
        
		/**
		 * Creates a new TuioBounds based on the given arguments.
		 * The new TuioBounds is added to the TuioServer's internal list of active TuioBounds
		 * and a reference is returned to the caller.
		 *
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 * @param	angle	the angle to assign
		 * @param	width	the width to assign
		 * @param	height	the height to assign
		 * @param	area	the area to assign
         * @return	a reference to the TuioObject of the TuioBounds
		 */
		TuioObject* createTuioBounds(float xp, float yp, float angle, float width, float height, float area);
        
        /**
         * Creates a new TuioBounds based on the given arguments.
         * The new TuioBounds is added to the TuioServer's internal list of active TuioBounds
         * and a reference is returned to the caller.
         *
         * @param	tobj	the existing TuioObject
         * @param	xp	the X coordinate to assign
         * @param	yp	the Y coordinate to assign
         * @param	angle	the angle to assign
         * @param	width	the width to assign
         * @param	height	the height to assign
         * @param	area	the area to assign
         * @return	a reference to the TuioObject of the TuioBounds
         */
        TuioObject* addTuioBounds(TuioObject *tobj, float xp, float yp, float angle, float width, float height, float area);
		
        /**
         * Adds the provided TuioBounds to an existing TuioObject
         * from the TuioServer's internal list of TuioObjects
         * or creates the according TuioObject if not found.
         *
         * @param	tbnd	the TuioBounds to add
         * @return	a reference to the TuioObject of the TuioBounds
         */
        TuioObject* addTuioBounds(TuioBounds *tbnd);
        
		/**
		 * Updates the referenced TuioBounds based on the given arguments.
		 *
		 * @param	tbnd	the TuioToken to update
		 * @param	xp	the X coordinate to assign
		 * @param	yp	the Y coordinate to assign
		 * @param	angle	the angle to assign
		 * @param	width	the width to assign
		 * @param	height	the height to assign
		 * @param	area	the area to assign
		 */
		void updateTuioBounds(TuioBounds *tbnd, float xp, float yp, float angle, float width, float height, float area);
		
		/**
		 * Removes the referenced TuioBounds from the TuioServer's internal list of TuioObjects
         * and also deletes its TuioObject (and all its encapsulated TUIO Components)
		 *
		 * @param	tbnd	the TuioBounds to remove
		 */
		void removeTuioBounds(TuioBounds *tbnd);
        
        /**
         * Creates a new TuioSymbol based on the given arguments.
         * The new TuioSymbol is added to the TuioServer's internal list of TuioObjects
         * and a reference is returned to the caller.
         *
         * @param	t_id	the Type ID to assign
         * @param	u_id	the User ID to assign
         * @param	sym     the Symbol ID to assign
         * @param	type	the type descriptor to assign
         * @param	data	the synbol data to assign
         * @return	a reference to the TuioObject of the TuioSymbol
         */
        TuioObject* createTuioSymbol(unsigned short t_id, unsigned short u_id, unsigned int sym, const char *type, const char *data);
        
        /**
         * Creates a new TuioSymbol based on the given arguments.
         * The new TuioSymbol is added to the TuioServer's internal list of TuioObjects
         * and a reference is returned to the caller.
         *
         * @param	tobj	the existing TuioObject
         * @param	t_id	the Type ID to assign
         * @param	u_id	the User ID to assign
         * @param	sym     the Symbol ID to assign
         * @param	type	the type descriptor to assign
         * @param	data	the synbol data to assign
         * @return	a reference to the TuioObject of the TuioSymbol
         */
        TuioObject* addTuioSymbol(TuioObject *tobj, unsigned short t_id, unsigned short u_id, unsigned int sym, const char *type, const char *data);
        
        /**
         * Adds the provided TuioSymbol to an existing TuioObject
         * from the TuioServer's internal list of TuioObjects
         * or creates the according TuioObject if not found.
         *
         * @param	tsym	the TuioSymbol to add
         * @return	a reference to the TuioObject of the TuioSymbol
         */
        TuioObject* addTuioSymbol(TuioSymbol *tsym);
        
        /**
         * Removes the referenced TuioSymbol from the TuioServer's internal list of TuioObjects
         * and also deletes its TuioObject (and all its encapsulated TUIO Components)
         *
         * @param	tsym	the TuioSymbol to remove
         */
        void removeTuioSymbol(TuioSymbol *tsym);
		
		/**
		 * Initializes a new frame with the given TuioTime
		 *
		 * @param	ttime	the frame time
		 */
		void initTuioFrame(TuioTime ttime);
		
		/**
		 * Commits the current frame.
		 * Generates and sends TUIO messages of all currently active and updated TuioTokens and TuioPointers.
		 */
		void commitTuioFrame();

		/**
		 * Returns the next available Session ID for external use.
		 * @return	the next available Session ID for external use
		 */
		int getSessionID();

		/**
		 * Returns the current frame ID for external use.
		 * @return	the current frame ID for external use
		 */
		int getFrameID();
		
		/**
		 * Returns the current frame time for external use.
		 * @return	the current frame time for external use
		 */
		TuioTime getFrameTime();
		
		/**
		 * Returns a List of all currently inactive TuioObjects
		 *
		 * @return  a List of all currently inactive TuioObjects
		 */
		std::list<TuioObject*> getUntouchedObjects();
		
		/**
		 * Calculates speed and acceleration values for all currently inactive TuioObjects
		 */
		void stopUntouchedMovingObjects();
		
		/**
		 * Removes all currently inactive TuioObjects from the TuioServer's internal list
		 */
		void removeUntouchedStoppedObjects();
		
		/**
		 * Returns the TuioToken closest to the provided coordinates
		 * or NULL if there isn't any active TuioToken
		 *
		 * @return  the closest TuioToken to the provided coordinates or NULL
		 */
		TuioToken* getClosestTuioToken(float xp, float yp);
		
		/**
		 * Returns the TuioPointer closest to the provided coordinates
		 * or NULL if there isn't any active TuioPointer
		 *
		 * @return  the closest TuioPointer corresponding to the provided coordinates or NULL
		 */
		TuioPointer* getClosestTuioPointer(float xp, float yp);

		/**
		 * Returns the TuioBounds closest to the provided coordinates
		 * or NULL if there isn't any active TuioBounds
		 *
		 * @return  the closest TuioBounds corresponding to the provided coordinates or NULL
		 */
		TuioBounds* getClosestTuioBounds(float xp, float yp);
		
		/**
		 * The TuioServer prints verbose TUIO event messages to the console if set to true.
		 * @param	verbose	print verbose messages if set to true
		 */
		void setVerbose(bool verbose) { this->verbose=verbose; }

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
		void resetTuioObjectList();
		
	protected:
		std::list<TuioPointer*> freePointerList;
		std::list<TuioPointer*> freePointerBuffer;

		TuioTime currentFrameTime;
        osc::TimeTag frameTimeTag;
		unsigned int currentFrame;
		unsigned int pointerCount, maxPointerID;
		unsigned int sessionID;

		bool tobjUpdate;
		bool verbose;

		bool invert_x;
		bool invert_y;
		bool invert_a;
	};
}
#endif /* INCLUDED_TUIOMANAGER_H */
