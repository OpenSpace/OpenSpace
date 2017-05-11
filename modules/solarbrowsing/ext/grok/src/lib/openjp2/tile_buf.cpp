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

bool grk_tile_buf_create_component(grk_tcd_tilecomp_t* tilec,
									bool isEncoder,
                                   bool irreversible,
                                   uint32_t cblkw,
                                   uint32_t cblkh,
                                   opj_image_t* output_image,
                                   uint32_t dx,
                                   uint32_t dy)
{
    int32_t resno = 0;
    rect_t	component_output_rect;
    grk_tile_buf_component_t* comp = NULL;

    if (!tilec)
        return false;

    /* create region component struct*/
	comp = new grk_tile_buf_component_t();
    if (!comp) {
        return false;
    }
	comp->data = NULL;

	comp->tile_dim= rect_t(
                  tilec->x0,
                  tilec->y0,
                  tilec->x1,
                  tilec->y1);

    if (output_image) {
		comp->dim= rect_t(
                      grk_uint_ceildiv(output_image->x0,dx),
                      grk_uint_ceildiv(output_image->y0,dy),
                      grk_uint_ceildiv(output_image->x1,dx),
                      grk_uint_ceildiv(output_image->y1,dy));

        /* clip output image to tile */
		comp->tile_dim.clip(&comp->dim, &comp->dim);

    } else {
        comp->dim = comp->tile_dim;
    }


    /* for encode, we don't need to allocate resolutions */
    if (isEncoder) {
        grk_tile_buf_destroy_component(tilec->buf);
        tilec->buf = comp;
        return true;
    }


    component_output_rect = comp->dim;


    /* fill resolutions vector */
    for (resno = (int32_t)(tilec->numresolutions-1); resno >= 0; --resno) {
        uint32_t bandno;
        grk_tcd_resolution_t*  tcd_res = tilec->resolutions + resno;
        grk_tile_buf_resolution_t* res = (grk_tile_buf_resolution_t*)grk_calloc(1, sizeof(grk_tile_buf_resolution_t));
        if (!res) {
            grk_tile_buf_destroy_component(comp);
            return false;
        }

        res->bounds.x = tcd_res->x1 - tcd_res->x0;
        res->bounds.y = tcd_res->y1 - tcd_res->y0;
        res->origin.x = tcd_res->x0;
        res->origin.y = tcd_res->y0;

        for (bandno = 0; bandno < tcd_res->numbands; ++bandno) {
            grk_tcd_band_t* band = tcd_res->bands + bandno;
            rect_t  band_rect;
			band_rect= rect_t(
                          band->x0,
                          band->y0,
                          band->x1,
                          band->y1
                         );

            res->band_region[bandno].dim = component_output_rect;
            if (resno > 0) {

                /*For next level down, E' = ceil((E-b)/2) where b in {0,1} identifies band  */
                grk_pt_t shift;
                shift.x = band->bandno & 1;
                shift.y = band->bandno & 2;

				res->band_region[bandno].dim.pan(&shift);
				res->band_region[bandno].dim.ceildivpow2(1);

                /* boundary padding */
				res->band_region[bandno].dim.grow(irreversible ? 3 : 2);

            }

            /* add code block padding around region */
            (res->band_region + bandno)->data_dim = (res->band_region + bandno)->dim;
			(res->band_region + bandno)->data_dim.grow2(cblkw, cblkh);

        }
        component_output_rect = res->band_region[0].dim;
        res->num_bands = tcd_res->numbands;
        comp->resolutions.push_back(res);
    }

    grk_tile_buf_destroy_component(tilec->buf);
    tilec->buf = comp;

    return true;
}

bool grk_tile_buf_is_decode_region(grk_tile_buf_component_t* buf)
{
    if (!buf)
        return false;
    return !buf->dim.are_equal(&buf->tile_dim);
}

int32_t* grk_tile_buf_get_ptr(grk_tile_buf_component_t* buf,
                              uint32_t resno,
                              uint32_t bandno,
                              uint32_t offsetx,
                              uint32_t offsety)
{
	(void)resno;
	(void)bandno;
    return buf->data + (uint64_t)offsetx + (uint64_t)offsety* (buf->tile_dim.x1 - buf->tile_dim.x0);

}

void grk_tile_buf_set_ptr(grk_tile_buf_component_t* buf, int32_t* ptr)
{
    buf->data = ptr;
    buf->owns_data = false;
}

bool grk_tile_buf_alloc_component_data_encode(grk_tile_buf_component_t* buf)
{
    if (!buf)
        return false;

    if ((buf->data == nullptr) || ((buf->data_size_needed > buf->data_size) && (buf->owns_data == false))) {
        buf->data = (int32_t *)grk_aligned_malloc(buf->data_size_needed);
        if (!buf->data) {
            return false;
        }
        buf->data_size = buf->data_size_needed;
        buf->owns_data = true;
    } else if (buf->data_size_needed > buf->data_size) {
        /* We don't need to keep old data */
        grk_aligned_free(buf->data);
        buf->data = (int32_t *)grk_aligned_malloc(buf->data_size_needed);
        if (!buf->data) {
            buf->data_size = 0;
            buf->data_size_needed = 0;
            buf->owns_data = false;
            return false;
        }

        buf->data_size = buf->data_size_needed;
        buf->owns_data = true;
    }
    return true;
}


bool grk_tile_buf_alloc_component_data_decode(grk_tile_buf_component_t* buf)
{
    if (!buf)
        return false;

    if (!buf->data ) {
        int64_t area = buf->tile_dim.get_area();
		if (area) {
			buf->data = (int32_t *)grk_aligned_malloc(area * sizeof(int32_t));
			if (!buf->data) {
				return false;
			}
		}
        buf->data_size = area * sizeof(int32_t);
        buf->data_size_needed = buf->data_size;
        buf->owns_data = true;
    }

    return true;
}


void grk_tile_buf_destroy_component(grk_tile_buf_component_t* comp)
{
    if (!comp)
        return;
    if (comp->data && comp->owns_data)
        grk_aligned_free(comp->data);
    comp->data = NULL;
    comp->data_size = 0;
    comp->data_size_needed = 0;
	for (auto& res : comp->resolutions) {
		grk_free(res);
	}
	comp->resolutions.clear();
    delete comp;
}

bool grk_tile_buf_hit_test(grk_tile_buf_component_t* comp, rect_t* rect)
{
    if (!comp || !rect)
        return false;
    for (auto& res : comp->resolutions) {
        rect_t dummy;
        uint32_t j;
        for (j = 0; j < res->num_bands; ++j) {
            if ((res->band_region + j)->dim.clip(rect, &dummy))
                return true;
        }
    }
    return false;
}

grk_pt_t grk_tile_buf_get_uninterleaved_range(grk_tile_buf_component_t* comp,
        uint32_t resno,
        bool is_even,
        bool is_horizontal)
{
    grk_pt_t rc;
    grk_tile_buf_resolution_t* res= NULL;
    grk_tile_buf_resolution_t* prev_res = NULL;
    grk_tile_buf_band_t *band= NULL;
    memset(&rc, 0, sizeof(grk_pt_t));
    if (!comp)
        return rc;

    res = comp->resolutions[comp->resolutions.size() - 1 - resno];
    if (!res)
        return rc;

    prev_res = comp->resolutions[comp->resolutions.size() - 1 - resno+1];

    if (resno == 0) {
        band = res->band_region;
    } else {
        if (!is_even) {
            band = res->band_region + 2;
        } else {
            band = is_horizontal ? res->band_region + 1 : res->band_region;
        }
    }

    if (is_horizontal) {
        rc.x = band->dim.x0 - prev_res->origin.x;
        rc.y = band->dim.x1 - prev_res->origin.x;
    } else {
        rc.x = band->dim.y0 - prev_res->origin.y;
        rc.y = band->dim.y1 - prev_res->origin.y;
    }

    /* clip */
    rc.x = grk_max<int64_t>(0, rc.x);

    /* if resno == 0, then prev_res is null */
    if (resno == 0) {
        rc.y = grk_min<int64_t>(rc.y, is_horizontal ? res->bounds.x : res->bounds.y);
    } else {
        if (is_even)
            rc.y = grk_min<int64_t>(rc.y, is_horizontal ? prev_res->bounds.x : prev_res->bounds.y);
        else
            rc.y = grk_min<int64_t>(rc.y,
                               is_horizontal ? res->bounds.x - prev_res->bounds.x : res->bounds.y - prev_res->bounds.y);

    }

    return rc;

}

grk_pt_t grk_tile_buf_get_interleaved_range(grk_tile_buf_component_t* comp,
        uint32_t resno,
        bool is_horizontal)
{
    grk_pt_t rc;
    grk_pt_t even;
    grk_pt_t odd;
    grk_tile_buf_resolution_t* res = NULL;
    memset(&rc, 0, sizeof(grk_pt_t));
    if (!comp)
        return rc;

    res = comp->resolutions[comp->resolutions.size()- 1 - resno];
    if (!res)
        return rc;

    even = grk_tile_buf_get_uninterleaved_range(comp, resno, true, is_horizontal);
    odd = grk_tile_buf_get_uninterleaved_range(comp, resno, false, is_horizontal);

    rc.x = grk_min<int64_t>( (even.x <<1), (odd.x << 1) + 1 );
    rc.y = grk_max<int64_t>( (even.y<< 1),  (odd.y << 1) + 1);

    /* clip to resolution bounds */
    rc.x = grk_max<int64_t>(0, rc.x);
    rc.y = grk_min<int64_t>(rc.y, is_horizontal ? res->bounds.x : res->bounds.y);
    return rc;
}

int64_t grk_tile_buf_get_interleaved_upper_bound(grk_tile_buf_component_t* comp)
{
    if (!comp || comp->resolutions.empty())
        return 0;
	grk_pt_t horizontal = grk_tile_buf_get_interleaved_range(comp, (uint32_t)comp->resolutions.size() - 1, true);
	grk_pt_t vertical   = grk_tile_buf_get_interleaved_range(comp, (uint32_t)comp->resolutions.size() - 1, false);

    return grk_max<int64_t>(horizontal.y, vertical.y);
}

