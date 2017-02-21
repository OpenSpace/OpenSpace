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

#ifndef INCLUDED_TUIOTIME_H
#define INCLUDED_TUIOTIME_H

#include "LibExport.h"

#ifndef WIN32
#include <pthread.h>
#include <sys/time.h>
#else
#include <windows.h>
#include <ctime>
#endif

#define MSEC_SECOND 1000
#define USEC_SECOND 1000000
#define USEC_MILLISECOND 1000

namespace TUIO {
	
	/**
	 * The TuioTime class is a simple structure that is used to reprent the time that has elapsed since the session start.
	 * The time is internally represented as seconds and fractions of microseconds which should be more than sufficient for gesture related timing requirements.
	 * Therefore at the beginning of a typical TUIO session the static method initSession() will set the reference time for the session. 
	 * Another important static method getSessionTime will return a TuioTime object representing the time elapsed since the session start.
	 * The class also provides various addtional convience method, which allow some simple time arithmetics.
	 *
	 * @author Martin Kaltenbrunner
	 * @version 1.1.6
	 */ 
	class LIBDECL TuioTime {
		
	private:
		long seconds;
		long micro_seconds;
		static long start_seconds;
		static long start_micro_seconds;
		
	public:

		/**
		 * The default constructor takes no arguments and sets   
		 * the Seconds and Microseconds attributes of the newly created TuioTime both to zero.
		 */
		TuioTime ():seconds(0),micro_seconds(0) {};

		/**
		 * The destructor is doing nothing in particular. 
		 */
		~TuioTime() {}
		
		/**
		 * This constructor takes the provided time represented in total Milliseconds 
		 * and assigs this value to the newly created TuioTime.
		 *
		 * @param  msec  the total time in Millseconds
		 */
		TuioTime (long msec);
		
		/**
		 * This constructor takes the provided time represented in Seconds and Microseconds   
		 * and assigs these value to the newly created TuioTime.
		 *
		 * @param  sec  the total time in seconds
		 * @param  usec	the microseconds time component
		 */	
		TuioTime (long sec, long usec);

		/**
		 * Sums the provided time value represented in total Microseconds to this TuioTime.
		 *
		 * @param  us	the total time to add in Microseconds
		 * @return the sum of this TuioTime with the provided argument in microseconds
		 */	
		TuioTime operator+(long us);
		
		/**
		 * Sums the provided TuioTime to the private Seconds and Microseconds attributes.  
		 *
		 * @param  ttime	the TuioTime to add
		 * @return the sum of this TuioTime with the provided TuioTime argument
		 */
		TuioTime operator+(TuioTime ttime);

		/**
		 * Subtracts the provided time represented in Microseconds from the private Seconds and Microseconds attributes.
		 *
		 * @param  us	the total time to subtract in Microseconds
		 * @return the subtraction result of this TuioTime minus the provided time in Microseconds
		 */		
		TuioTime operator-(long us);

		/**
		 * Subtracts the provided TuioTime from the private Seconds and Microseconds attributes.
		 *
		 * @param  ttime	the TuioTime to subtract
		 * @return the subtraction result of this TuioTime minus the provided TuioTime
		 */	
		TuioTime operator-(TuioTime ttime);

		
		/**
		 * Assigns the provided TuioTime to the private Seconds and Microseconds attributes.
		 *
		 * @param  ttime	the TuioTime to assign
		 */	
		void operator=(TuioTime ttime);
		
		/**
		 * Takes a TuioTime argument and compares the provided TuioTime to the private Seconds and Microseconds attributes.
		 *
		 * @param  ttime	the TuioTime to compare
		 * @return true if the two TuioTime have equal Seconds and Microseconds attributes
		 */	
		bool operator==(TuioTime ttime);

		/**
		 * Takes a TuioTime argument and compares the provided TuioTime to the private Seconds and Microseconds attributes.
		 *
		 * @param  ttime	the TuioTime to compare
		 * @return true if the two TuioTime have differnt Seconds or Microseconds attributes
		 */	
		bool operator!=(TuioTime ttime);
		
		/**
		 * Resets the seconds and micro_seconds attributes to zero.
		 */
		void reset();
		
		/**
		 * Returns the TuioTime Seconds component.
		 * @return the TuioTime Seconds component
		 */	
		long getSeconds() const;
		
		/**
		 * Returns the TuioTime Microseconds component.
		 * @return the TuioTime Microseconds component
		 */	
		long getMicroseconds() const;
		
		/**
		 * Returns the total TuioTime in Milliseconds.
		 * @return the total TuioTime in Milliseconds
		 */	
		long getTotalMilliseconds() const;
		
		/**
		 * This static method globally resets the TUIO session time.
		 */		
		static void initSession();
		
		/**
		 * Returns the present TuioTime representing the time since session start.
		 * @return the present TuioTime representing the time since session start
		 */			
		static TuioTime getSessionTime();
		
		/**
		 * Returns the absolut TuioTime representing the session start.
		 * @return the absolut TuioTime representing the session start
		 */			
		static TuioTime getStartTime();
		
		/**
		 * Returns the absolut TuioTime representing the current system time.
		 * @return the absolut TuioTime representing the current system time
		 */	
		static TuioTime getSystemTime();
	};
}
#endif /* INCLUDED_TUIOTIME_H */
