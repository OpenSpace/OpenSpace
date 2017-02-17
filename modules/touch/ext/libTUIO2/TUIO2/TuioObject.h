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

#ifndef INCLUDED_TUIOOBJECT_H
#define INCLUDED_TUIOOBJECT_H

#include "TuioTime.h"
#include "TuioToken.h"
#include "TuioPointer.h"
#include "TuioBounds.h"
#include "TuioSymbol.h"
#include "TuioSource.h"

namespace TUIO2 {
	
	/**
	 * The TuioObject class encapsulates all Tuio objects that share the same Session ID.
	 *
	 * @author Martin Kaltenbrunner
	 * @version 2.0.a0
	 */
    
	class LIBDECL TuioObject {
		
	protected:
		/**
		 * The shared session ID
		 */
		unsigned int session_id;
        /**
         * The associated TuioSource
         */
        TuioSource source;
        /**
         * the associated TuioToken
         */
        TuioToken *token;
        /**
         * the associated TuioPointer
         */
        TuioPointer *pointer;
        /**
         * the associated TuioBounds
         */
        TuioBounds *bounds;
        /**
         * the associated TuioSymbol
         */
        TuioSymbol *symbol;
        /**
         * The time stamp of the last update represented as TuioTime (time since session start)
         */
        TuioTime currentTime;
		/**
		 * The creation time of this TuioObject represented as TuioTime (time since session start)
		 */
		TuioTime startTime;
        /**
         * Reflects the current state of the TuioObject
         */
        unsigned char state;
		
	public:
		/**
		 * The default constructor only takes the Session ID
		  * @param	s_id	the Session ID to assign
		 */
		TuioObject (unsigned int s_id);
        
        /**
         * This constructor takes a TuioTime and the Session ID
         * @param	ttime	the TuioTime to assign
         * @param	s_id	the Session ID to assign
         */
        TuioObject (TuioTime ttime, unsigned int s_id);
  
        /**
         * This constructor takes a TuioTime and the Session ID
         * @param	ttime	the TuioTime to assign
         * @param	src     the TuioSource to assign
         * @param	s_id	the Session ID to assign
         */
        TuioObject (TuioTime ttime, TuioSource *src, unsigned int s_id);
        
        /**
         * The default destructor also delets all assigned TUIO components
         */
        ~TuioObject ();
        
        /**
         * Returns the associated Session ID
         * @return	the associated Session ID
         */
        unsigned int getSessionID();
	
        /**
         * Sets the assotiated TUIO source
         *
         * @param	src         the TuioSource to assign
         */
        void setTuioSource(TuioSource *src);
  
        /**
         * Returns the associated TUIO source
         */
        TuioSource* getTuioSource();
                
		/**
		 * This method assigns a TuioToken to this TuioObject
		 * @param	ttok	the TuioToken to assign
		 */
		void setTuioToken(TuioToken *ttok);
	
        /**
         * This method assigns a TuioPointer to this TuioObject
         * @param	tptr	the TuioPointer to assign
         */
        void setTuioPointer(TuioPointer *tptr);
        
        /**
         * This method assigns a TuioBounds to this TuioObject
         * @param	tbnd	the TuioBounds to assign
         */
        void setTuioBounds(TuioBounds *tbnd);
        
        /**
         * This method assigns a TuioSymbol to this TuioObject
         * @param	tsym	the TuioSymbol to assign
         */
        void setTuioSymbol(TuioSymbol *tsym);
  
        /**
         * This method sets all TuioComponents in this TuioObject to TUIO_REMOVED state
         */
        void removeAllTuioComponents(TuioTime ttime);
        
        /**
         * This method sets the TuioToken in this TuioObject to TUIO_REMOVED state
         */
        void removeTuioToken(TuioTime ttime);
        
        /**
         * This method sets the TuioPointer in this TuioObject to TUIO_REMOVED state
         */
        void removeTuioPointer(TuioTime ttime);
        
        /**
         * This method sets the TuioBounds in this TuioObject to TUIO_REMOVED state
         */
        void removeTuioBounds(TuioTime ttime);
        
        /**
         * This method sets the TuioSymbol in this TuioObject to TUIO_REMOVED state
         */
        void removeTuioSymbol(TuioTime ttime);
        
        /**
         * This method deletes all TuioComponents in this TuioObject
         */
        void deleteAllTuioComponents();
        
        /**
         * This method deletes the TuioToken in this TuioObject
         */
        void deleteTuioToken();
        
        /**
         * This method deletes the TuioPointer in this TuioObject
         */
        void deleteTuioPointer();
        
        /**
         * This method deletes the TuioBounds in this TuioObject
         */
        void deleteTuioBounds();
        
        /**
         * This method deletes the TuioSymbol in this TuioObject
         */
        void deleteTuioSymbol();
        
        /**
         * This method clears all TuioComponents in this TuioObject
         */
        void clearAllTuioComponents();
        
        /**
         * This method clears the TuioToken in this TuioObject
         */
        void clearTuioToken();
        
        /**
         * This method clears the TuioPointer in this TuioObject
         */
        void clearTuioPointer();
        
        /**
         * This method clears the TuioBounds in this TuioObject
         */
        void clearTuioBounds();
        
        /**
         * This method clears the TuioSymbol in this TuioObject
         */
        void clearTuioSymbol();
        
        /**
         * This method tests for any TuioComponent in this TuioObject
         * @return true if any TuioComponent has been assigned
         */
        bool containsAnyTuioComponent();
        
        /**
         * This method tests for a TuioToken in this TuioObject
         * @return true if a TuioToken has been assigned
         */
        bool containsTuioToken();
        
        /**
         * This method tests for a TuioPointer in this TuioObject
         * @return true if a TuioPointer has been assigned
         */
        bool containsTuioPointer();
        
        /**
         * This method tests for a TuioBounds in this TuioObject
         * @return true if a TuioBounds has been assigned
         */
        bool containsTuioBounds();
        
        /**
         * This method tests for a TuioSymbol in this TuioObject
         * @return true if a TuioSymbol has been assigned
         */
        bool containsTuioSymbol();
        
        /**
         * This method tests if a new TuioToken has been added to this TuioObject
         * @return true if a TuioToken has been added
         */
        bool containsNewTuioToken();
        
        /**
         * This method tests if a new TuioPointer has been added this TuioObject
         * @return true if a TuioPointer has been added
         */
        bool containsNewTuioPointer();
        
        /**
         * This method tests if a new TuioBounds has been added to this TuioObject
         * @return true if a TuioBounds has been added
         */
        bool containsNewTuioBounds();
        
        /**
         * This method tests if a new TuioSymbol has beed added to this TuioObject
         * @return true if a TuioSymbol has been added
         */
        bool containsNewTuioSymbol();

        /**
         * This method returns the TuioToken associated to this TuioObject
         * @return	the associated TuioToken
         */
        TuioToken* getTuioToken();

        /**
         * This method returns the TuioPointer associated to this TuioObject
         * @return	the associated TuioPointer
         */
        TuioPointer* getTuioPointer();
 
        /**
         * This method returns the TuioBounds associated to this TuioObject
         * @return	the associated TuioBounds
         */
        TuioBounds* getTuioBounds();
        
        /**
         * This method returns the TuioSymbol associated to this TuioObject
         * @return	the associated TuioSymbol
         */
        TuioSymbol* getTuioSymbol();
        
        /**
         * This method stops all encapsulated TuioComponents
         */
        void stop(TuioTime ttime);
        
        /**
         * This method removes all encapsulated TuioComponents
         */
        void remove(TuioTime ttime);
        
        /**
         * This method returns true if any encapsulated TuioComponent is moving
         */
        bool isMoving();
        
        /**
         * This method refreshes the currentTime after an update
         */
        void update(TuioTime ttime);
    
        /**
         * Returns current time stamp of this TuioPoint as TuioTime
         *
         * @return	the  time stamp of this TuioPoint as TuioTime
         */
        TuioTime getTuioTime() const;

        /**
		 * Returns the start time of this TuioPoint as TuioTime. 
		 *
		 * @return	the start time of this TuioPoint as TuioTime
		 */
		TuioTime getStartTime() const;
        
        /**
         * Returns the TUIO state of this TuioObject.
         * @return	the TUIO state of this TuioObject
         */
        unsigned char getTuioState() const;
	};
}
#endif // INCLUDED_TUIOOBJECT_H
