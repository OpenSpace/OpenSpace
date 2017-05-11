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

RateInfo::RateInfo() : 	minimumSlope(USHRT_MAX), 
						maximumSlope(0) {

}

/*
Synchronize with code block
*/
void RateInfo::synch(grk_tcd_cblk_enc_t *cblk) {
	for (auto passno = 0U; passno < cblk->num_passes_encoded; passno++) {
		grk_tcd_pass_t *pass = &cblk->passes[passno];

		//2. only process feasible truncation points
		if (pass->slope == 0)
			continue;

		uint16_t s = pass->slope;

		//4. update min and max
		if (s < minimumSlope)
			minimumSlope = s;
		if (s > maximumSlope)
			maximumSlope = s;
	}
}

uint16_t RateInfo::getMinimumThresh() {
	return minimumSlope;
}
