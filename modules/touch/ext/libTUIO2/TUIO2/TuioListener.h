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

#ifndef INCLUDED_TUIOLISTENER_H
#define INCLUDED_TUIOLISTENER_H

#include "TuioObject.h"

namespace TUIO2 {
	
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
	 * @version 2.0.a0
	 */
	class LIBDECL TuioListener { 
		
	public:
		/**
		 * The destructor is doing nothing in particular. 
		 */
		virtual ~TuioListener(){};
		
		/**
		 * This callback method is invoked by the TuioClient when a new TUIO Component is added to the session.
		 *
		 * @param  tobj  the TuioObject reference that encapsulates all related TUIO Components
		 */
		virtual void tuioAdd(TuioObject *tobj)=0;

		/**
		 * This callback method is invoked by the TuioClient when an existing TUIO Component  is updated during the session.
		 *
         * @param  tobj  the TuioObject reference that encapsulates all related TUIO Components
		 */
		virtual void tuioUpdate(TuioObject *tobj)=0;
		
		/**
		 * This callback method is invoked by the TuioClient when a TUIO Component is removed from the session.
		 *
         * @param  tobj  the TuioObject reference that encapsulates all related TUIO Components
		 */
		virtual void tuioRemove(TuioObject *tobj)=0;
		
		/**
		 * This callback method is invoked by the TuioClient to mark the end of a received TUIO message bundle.   
		 *
		 * @param  ftime  the TuioTime associated to the current TUIO message bundle
		 */
		virtual void tuioRefresh(TuioTime ftime)=0;
	};
}
#endif /* INCLUDED_TUIOLISTENER_H */
