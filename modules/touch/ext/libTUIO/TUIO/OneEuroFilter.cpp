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

#include "OneEuroFilter.h"
#define M_PI 3.14159265392

using namespace TUIO;

double LowPassFilter::filter(double value, double alpha) {
	
	if (alpha<=0.0 || alpha>1.0)
		throw std::range_error("alpha should be in (0.0., 1.0]");
	
	double result;
	if (initialized)
		result = alpha*value + (1.0-alpha)*lastResult;
	else {
		result = value;
		initialized = true;
	}
	
	lastRawValue = value;
	lastResult = result;
	return result;
}

// -----------------------------------------------------------------

double OneEuroFilter::alpha(double cutoff) {
	double te = 1.0 / freq;
	double tau = 1.0 / (2*M_PI*cutoff);
	return 1.0 / (1.0 + tau/te);
}

double OneEuroFilter::filter(double value, TimeStamp timestamp) {
	// update the sampling frequency based on timestamps
	if (lasttime!=UndefinedTime && timestamp!=UndefinedTime)
		freq = 1.0 / (timestamp-lasttime);
	lasttime = timestamp;
	// estimate the current variation per second
	double dvalue = x->initialized ? (value - x->lastRawValue)*freq : value;
	
	double edvalue;
	try { edvalue = dx->filter(dvalue, alpha(dcutoff));
	} catch (std::range_error e) { return value; }
	// use it to update the cutoff frequency
	double cutoff = mincutoff + beta*fabs(edvalue);
	// filter the given value
	return x->filter(value, alpha(cutoff));
}