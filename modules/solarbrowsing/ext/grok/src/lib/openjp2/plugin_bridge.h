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

bool decode_synch_plugin_with_host(grk_tcd_t *tcd);
bool decode_synch_host_with_plugin(grk_tcd_t *tcd);

void encode_synch_with_plugin(grk_tcd_t *tcd,
	uint32_t compno,
	uint32_t resno,
	uint32_t bandno,
	uint32_t precno,
	uint32_t cblkno,
	grk_tcd_band_t *band,
	grk_tcd_cblk_enc_t *cblk,
	uint32_t* numPix);


bool tile_equals(opj_plugin_tile_t* plugin_tile,
				grk_tcd_tile_t *p_tile);

// set context stream for debugging purposes
void set_context_stream(grk_tcd_t *p_tcd);

void nextCXD(plugin_debug_mqc_t *mqc, uint32_t d);

void  mqc_next_plane(plugin_debug_mqc_t *mqc);


