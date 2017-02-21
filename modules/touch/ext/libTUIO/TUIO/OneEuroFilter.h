/*  reacTIVision tangible interaction framework
	Copyright (C) 2005-2016 Martin Kaltenbrunner <martin@tuio.org>
	Based on an example by Nicolas Roussel <nicolas.roussel@inria.fr>
 
	This program is free software; you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation; either version 2 of the License, or
	(at your option) any later version.
 
	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
 
	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef ONEEUROFILTER
#define ONEEUROFILTER

#include <stdexcept>
#include <cmath>

typedef double TimeStamp; // in seconds
static const TimeStamp UndefinedTime = -1.0;

namespace TUIO {
	
	class LowPassFilter {
		
	public:
		
		bool initialized;
		double lastRawValue,lastResult;

		LowPassFilter(double initval=0.0) {
			
			lastRawValue = lastResult = initval;
			initialized = false;
		}
		
		double filter(double value, double alpha);
	};
	
	// -----------------------------------------------------------------
	
	class OneEuroFilter {
		
		double freq;
		double mincutoff;
		double beta;
		double dcutoff;
		LowPassFilter *x;
		LowPassFilter *dx;
		TimeStamp lasttime;
		
		double alpha(double cutoff);
		
	public:
		
		OneEuroFilter(double f, double mc=1.0, double b=0.0, double dc=1.0) {
			
			if (f<=0) throw std::range_error("freq should be >0");
			else freq = f;
			if (mc<=0) throw std::range_error("mincutoff should be >0");
			else mincutoff = mc;
			if (b<=0) throw std::range_error("beta should be >0");
			else beta = b;
			if (dc<=0) throw std::range_error("dcutoff should be >0");
			else dcutoff = dc;
			
			x = new LowPassFilter(alpha(mincutoff));
			dx = new LowPassFilter(alpha(dcutoff));
			lasttime = UndefinedTime;
		}
		
		~OneEuroFilter(void) {
			delete x;
			delete dx;
		}
		
		double filter(double value, TimeStamp timestamp=UndefinedTime);
		
	};
	
}

#endif
