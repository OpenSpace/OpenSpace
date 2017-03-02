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

#ifndef INCLUDED_TUIOLISTENER_H
#define INCLUDED_TUIOLISTENER_H

#include "TuioObject.h"
#include "TuioCursor.h"
#include "TuioBlob.h"

namespace TUIO {
	
	/**
	 * <p>The TuioListener interface provides a simple callback infrastructure which is used by the {@link TuioClient} class 
	 * to dispatch TUIO events to all registered instances of classes that implement the TuioListener interface defined here.</p> 
	 * <p>Any class that implements the TuioListener interface is required to implement all of the callback methods defined here.
	 * The {@link TuioClient} makes use of these interface methods in order to dispatch TUIO events to all registered TuioListener implementations.</p>
	 * <p><code>
	 * public class MyTuioListener implements TuioListener<br/>
	 * ...</code><p><code>
	 * MyTuioListener listener = new MyTuioListener();<br/>
	 * TuioClient client = new TuioClient();<br/>
	 * client.addTuioListener(listener);<br/>
	 * client.start();<br/>
	 * </code></p>
	 *
	 * @author Martin Kaltenbrunner
	 * @version 1.1.6
	 */
	class LIBDECL TuioListener { 
		
	public:
		/**
		 * The destructor is doing nothing in particular. 
		 */
		virtual ~TuioListener(){};
		
		/**
		 * This callback method is invoked by the TuioClient when a new TuioObject is added to the session.   
		 *
		 * @param  tobj  the TuioObject reference associated to the addTuioObject event
		 */
		virtual void addTuioObject(TuioObject *tobj)=0;

		/**
		 * This callback method is invoked by the TuioClient when an existing TuioObject is updated during the session.   
		 *
		 * @param  tobj  the TuioObject reference associated to the updateTuioObject event
		 */
		virtual void updateTuioObject(TuioObject *tobj)=0;
		
		/**
		 * This callback method is invoked by the TuioClient when an existing TuioObject is removed from the session.   
		 *
		 * @param  tobj  the TuioObject reference associated to the removeTuioObject event
		 */
		virtual void removeTuioObject(TuioObject *tobj)=0;
		
		/**
		 * This callback method is invoked by the TuioClient when a new TuioCursor is added to the session.   
		 *
		 * @param  tcur  the TuioCursor reference associated to the addTuioCursor event
		 */
		virtual void addTuioCursor(TuioCursor *tcur)=0;

		/**
		 * This callback method is invoked by the TuioClient when an existing TuioCursor is updated during the session.   
		 *
		 * @param  tcur  the TuioCursor reference associated to the updateTuioCursor event
		 */
		virtual void updateTuioCursor(TuioCursor *tcur)=0;

		/**
		 * This callback method is invoked by the TuioClient when an existing TuioCursor is removed from the session.   
		 *
		 * @param  tcur  the TuioCursor reference associated to the removeTuioCursor event
		 */
		virtual void removeTuioCursor(TuioCursor *tcur)=0;

		/**
		 * This callback method is invoked by the TuioClient when a new TuioBlob is added to the session.   
		 *
		 * @param  tcur  the TuioBlob reference associated to the addTuioBlob event
		 */
		virtual void addTuioBlob(TuioBlob *tblb)=0;
		
		/**
		 * This callback method is invoked by the TuioClient when an existing TuioBlob is updated during the session.   
		 *
		 * @param  tblb  the TuioBlob reference associated to the updateTuioBlob event
		 */
		virtual void updateTuioBlob(TuioBlob *tblb)=0;
		
		/**
		 * This callback method is invoked by the TuioClient when an existing TuioBlob is removed from the session.   
		 *
		 * @param  tblb  the TuioBlob reference associated to the removeTuioBlob event
		 */
		virtual void removeTuioBlob(TuioBlob *tblb)=0;
		
		/**
		 * This callback method is invoked by the TuioClient to mark the end of a received TUIO message bundle.   
		 *
		 * @param  ftime  the TuioTime associated to the current TUIO message bundle
		 */
		virtual void refresh(TuioTime ftime)=0;
	};
}
#endif /* INCLUDED_TUIOLISTENER_H */
