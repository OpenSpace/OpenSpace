/**
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
*
*    You should have received a copy of the GNU Affero General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
*/


#pragma once

struct grk_pt_t {
    int64_t x;
    int64_t y;

};

struct rect_t {
	int64_t x0;
    int64_t y0;
    int64_t x1;
    int64_t y1;

	rect_t();
	rect_t(int64_t x0, int64_t y0, int64_t x1, int64_t y1);


	/* valid if x0 <= x1 && y0 <= y1. Can include degenerate rectangles: line and point*/
	bool is_valid(void);

	int64_t get_area(void);

	bool is_non_degenerate(void);

	bool are_equal(rect_t* r2);

	bool clip( rect_t* r2, rect_t* result);

	void ceildivpow2( int32_t power);

	void grow(int64_t boundary);

	void grow2(int64_t boundaryx, int64_t boundaryy);

	void subsample(uint32_t dx, uint32_t dy);

	void pan(grk_pt_t* shift);

	void print(void);


};


typedef struct grk_buf {
    uint8_t *buf;		/* internal array*/
    uint64_t offset;	/* current offset into array */
    size_t len;		/* length of array */
    bool owns_data;	/* true if buffer manages the buf array */
} grk_buf_t;

