/*
*    Copyright (C) 2016-2017 Grok Image Compression Inc.
*
*    This source code is free software: you can redistribute it and/or  modify
*    it under the terms of the GNU Affero General Public License, version 3,
*    as published by the Free Software Foundation.
*
*    This source code is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU Affero General Public License for more details.

*    You should have received a copy of the GNU Affero General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
 */

#include "grk_includes.h"


 /*
 Calculate feasible truncation points for a given code block.
 The feasible truncation points lie on the convex hull of the
 distortion-rate curve for that block.

 See Taubman and Marcellin, Chapter 8 for discussion
 of convex hull algorithm.

 */
void RateControl::convexHull(grk_tcd_pass_t *pass, uint32_t numPasses) {

	double* slope_cache = new double[numPasses];

	// search for feasible truncation points
	for (auto p = 0U; p < numPasses; p++) {
		auto current_pass = pass + p;
		double dd = 0;
		double dr = 0;

		int p_intermed = p;
		auto intermed_pass = pass + p_intermed;

		// loop through all intermediate points from p-1 down to 0,
		// breaking when one of the following conditions occurs:
		// 1. current pass is rejected as non-feasible
		// 2. first intermediate point is found such that current pass meets
		//    feasible-truncation conditions relative to this point
		// 3. all intermediate points are processed, and pass is not rejected,
		//    in which case current pass meets feasible-truncation conditions
		while (1) {

			// calculate rate and distortion deltas
			dr += intermed_pass->len;
			if (p_intermed == 0) {

				dd += intermed_pass->distortiondec;
			}
			else {

				dd += intermed_pass->distortiondec - (intermed_pass - 1)->distortiondec;
			}

			// reject current point: all intermediate distortion-rate slopes
			// for feasible truncation point must be strictly positive
			// (Corollary 8.3)
			if (dd <= 0) {
				current_pass->slope = 0;

				break;
			}

			// decrement p_intermed
			p_intermed--;
			intermed_pass--;

			// all intermediate points have been processed and current
			// point has survived: mark current point as feasible
			if (p_intermed == -1) {

				// update slope for current point (Equation 8.8)
				slope_cache[p] = dd / dr;
				current_pass->slope = slopeToLog(slope_cache[p]);

				break;
			}

			// skip previously-rejected intermediate point
			if (intermed_pass->slope == 0) {

				continue;

			}

			// reject intermediate point as non-feasible:
			// distortion-rate slope must be finite
			if (dr == 0) {

				intermed_pass->slope = 0;

			}
			// reject intermediate point as non-feasible:
			// distortion-rate slope must be strictly monotone decreasing
			// (Corollary 8.3)
			else if ((slope_cache[p_intermed] * dr) <= dd) {

				intermed_pass->slope = 0;

			}
			// feasible truncation point
			else {

				// update slope for current point (Equation 8.8)
				slope_cache[p] = dd / dr;
				current_pass->slope = slopeToLog(slope_cache[p]);

				// switching to lower resolution log domain may break
				// strict monotone condition on slopes. In this case, reject
				// lower quality point
				if (current_pass->slope >= intermed_pass->slope) {
					intermed_pass->slope = 0;
				}

				break;
			}
		}
	}

	delete[] slope_cache;
}



/*
Convert slope to Q8.8 fixed point of ln(slope)

Notes:

1) Take slope upper cutoff to be ULLONG_MAX+1 == 2^64.

2) To convert to Q8.8, we

i)   divide by upper cutoff, mapping slope to the interval (0,1]

ii)  take natural logarithm, mapping slope to (-INF, 0]

iii) scale values based on following logic:
when slope is doubled, log(slope) increases by ln(2).
We want this increase to correspond to an increase of 1
in the fixed point domain, which is equivalent to 256
in fixed point. So, scaling factor is 256/ln(2)

iv) Map range to 16 bit unsigned int, i.e.
add (1<<16), then truncate to range [1,0xFFFF].
Value of zero is reserved to indicate a non-feasible truncation point.


Putting this all together:

slopeCutoff = 2^64
scale = 256/ln(2)
shift = 1<<16

logSlope = ln(slope/slopeCutoff)*scale + shift
		 = ln(slope)*scale - ln(slopeCutoff)*scale + shift
*/

const double slopeCutoff = pow(2, 64);
const double scale = 256 / log(2);
const double invScale = log(2) / 256;
const double shift = 1 << 16;

uint16_t RateControl::slopeToLog(double slope) {
	if (slope > slopeCutoff)
		slope = slopeCutoff;
	double logSlope = log(slope)*scale - log(slopeCutoff) * scale + shift;
	if (logSlope < 1)
		logSlope = 1;
	if (logSlope > 0xFFFF)
		logSlope = 0xFFFF;
	return (uint16_t)logSlope;
}

/* Convert from Q8.8 fixed point of log(slope) to slope

See above discussion. We just need to reverse the forumula above.
*/
double RateControl::slopeFromLog(uint16_t logSlope) {
	return exp((logSlope + log(slopeCutoff) * scale - shift)*invScale);
}
