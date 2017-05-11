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

/*
Note: various coordinate systems are used to describe regions in the tile buffer.

1) Canvas coordinate system:  jpeg 2000 global image coordinates

2) Tile coordinate system:  coordinates relative to a tile's top left hand corner

3) Resolution coordinate system:  coordinates relative to a resolution's top left hand corner

4) Sub-band coordinate system: coordinates relative to a particular sub-band's top left hand corner

*/

typedef struct grk_tile_buf_band {
    rect_t dim;			/* coordinates of sub-band region (canvas coordinates)  */
    rect_t data_dim;	/* coordinates of sub-band data region, (tile coordinates ) */
} grk_tile_buf_band_t;

typedef struct grk_tile_buf_resolution {
    grk_tile_buf_band_t band_region[3];
    uint32_t num_bands;
    grk_pt_t origin;		/* resolution origin, in canvas coordinates */
    grk_pt_t bounds;		/* full width and height of resolution */
} grk_tile_buf_resolution_t;

typedef struct grk_tile_buf_component {
	std::vector<grk_tile_buf_resolution_t*> resolutions;
    int32_t *data;
    uint64_t data_size_needed;	/* we may either need to allocate this amount of data,
									   or re-use image data and ignore this value */
    uint64_t data_size;			/* size of the data of the component */
    bool owns_data;				/* true if tile buffer manages its data array, false otherwise */

    rect_t dim;		  /* canvas coordinates of region */
    rect_t tile_dim;  /* canvas coordinates of tile */

} grk_tile_buf_component_t;

/* offsets are in canvas coordinate system*/
int32_t* grk_tile_buf_get_ptr(grk_tile_buf_component_t* buf,
                              uint32_t resno,
                              uint32_t bandno,
                              uint32_t offsetx,
                              uint32_t offsety);

void grk_tile_buf_set_ptr(grk_tile_buf_component_t* buf, int32_t* ptr);

bool grk_tile_buf_alloc_component_data_decode(grk_tile_buf_component_t* buf);

bool grk_tile_buf_alloc_component_data_encode(grk_tile_buf_component_t* buf);

bool grk_tile_buf_is_decode_region(grk_tile_buf_component_t* buf);

void grk_tile_buf_destroy_component(grk_tile_buf_component_t* comp);

/* Check if rect overlaps with region.
   rect coordinates must be stored in canvas coordinates
*/
bool grk_tile_buf_hit_test(grk_tile_buf_component_t* comp, rect_t* rect);

/* sub-band coordinates */
grk_pt_t grk_tile_buf_get_uninterleaved_range(grk_tile_buf_component_t* comp,
        uint32_t resno,
        bool is_even,
        bool is_horizontal);


/* resolution coordinates */
grk_pt_t grk_tile_buf_get_interleaved_range(grk_tile_buf_component_t* comp,
        uint32_t resno,
        bool is_horizontal);

int64_t grk_tile_buf_get_interleaved_upper_bound(grk_tile_buf_component_t* comp);

