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

#include "grk_includes.h"


#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

/**
Divide an integer and round upwards
@return Returns a divided by b
*/
static inline int64_t grk_int64_ceildiv(int64_t a, int64_t b)
{
	assert(b);
	return (a + b - 1) / b;
}


void rect_t::print(void)
{
       printf("Rectangle:  [%I64d,%I64d,%I64d,%I64d] \n", x0, y0, x1, y1);
}

rect_t::rect_t(void) : x0(0), y0(0), x1(0), y1(0)
{
}

rect_t::rect_t(int64_t x0, int64_t y0, int64_t x1, int64_t y1) : x0(x0), y0(y0), x1(x1), y1(y1)
{
}

bool rect_t::is_valid(void)
{
    return x0 <= x1 && y0 <= y1;
}

bool rect_t::is_non_degenerate(void)
{
    return x0 < x1 && y0 < y1;
}

bool rect_t::are_equal(rect_t* r2)
{

    if (!r2)
        return false;

    return x0 == r2->x0 &&
           y0 == r2->y0 &&
           x1 == r2->x1 &&
           y1 == r2->y1;
}

bool rect_t::clip(rect_t* r2, rect_t* result)
{
    bool rc;
    rect_t temp;

    if (!r2 || !result)
        return false;

    temp.x0 = MAX(x0, r2->x0);
    temp.y0 = MAX(y0, r2->y0);

    temp.x1 = MIN(x1, r2->x1);
    temp.y1 = MIN(y1, r2->y1);

    rc = temp.is_valid();

    if (rc)
        *result = temp;
    return rc;
}


void rect_t::ceildivpow2( int32_t power)
{
    x0 = grk_int64_ceildivpow2(x0,power);
    y0 = grk_int64_ceildivpow2(y0,power);
    x1 = grk_int64_ceildivpow2(x1,power);
    y1 = grk_int64_ceildivpow2(y1,power);

}


int64_t rect_t::get_area(void)
{
    return (x1 - x0) * (y1 - y0);
}

void rect_t::pan(grk_pt_t* shift)
{
    x0 += shift->x;
    y0 += shift->y;
    x1 += shift->x;
    y1 += shift->y;
}

void rect_t::subsample( uint32_t dx, uint32_t dy)
{
    x0 = grk_int64_ceildiv(x0, (int64_t)dx);
    y0 = grk_int64_ceildiv(y0, (int64_t)dy);
    x1 = grk_int64_ceildiv(x1, (int64_t)dx);
    y1 = grk_int64_ceildiv(y1, (int64_t)dy);
}

void rect_t::grow(int64_t boundary)
{
    grow2(boundary, boundary);
}

void rect_t::grow2(int64_t boundaryx, int64_t boundaryy)
{

    x0 -= boundaryx;
    y0 -= boundaryy;
    x1 += boundaryx;
    y1 += boundaryy;
}