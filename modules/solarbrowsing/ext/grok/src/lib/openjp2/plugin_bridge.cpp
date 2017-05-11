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
#include "plugin_bridge.h"

// Performed after T2, just before plugin decode is triggered
// note: only support single segment at the moment
bool decode_synch_plugin_with_host(grk_tcd_t *tcd) {
	if (tcd->current_plugin_tile && tcd->current_plugin_tile->tileComponents) {
		auto tcd_tile = tcd->tile;
		for (uint32_t compno = 0; compno < tcd_tile->numcomps; compno++) {
			auto tilec = &tcd_tile->comps[compno];
			auto plugin_tilec = tcd->current_plugin_tile->tileComponents[compno];
			assert(tilec->numresolutions == plugin_tilec->numResolutions);
			for (uint32_t resno = 0; resno < tilec->numresolutions; resno++) {
				auto res = &tilec->resolutions[resno];
				auto plugin_res = plugin_tilec->resolutions[resno];
				assert(plugin_res->numBands == res->numbands);
				for (uint32_t bandno = 0; bandno < res->numbands; bandno++) {
					auto band = &res->bands[bandno];
					auto plugin_band = plugin_res->bands[bandno];
					assert(plugin_band->numPrecincts == res->pw * res->ph);
					for (uint32_t precno = 0; precno < res->pw * res->ph; precno++) {
						auto prc = &band->precincts[precno];
						auto plugin_prc = plugin_band->precincts[precno];
						assert(plugin_prc->numBlocks == prc->cw * prc->ch);
						for (uint32_t cblkno = 0; cblkno < prc->cw * prc->ch; cblkno++) {
							auto cblk = &prc->cblks.dec[cblkno];
							if (!cblk->numSegments)
								continue;
							if (cblk->numSegments != 1)
								return false;
							opj_plugin_code_block_t* plugin_cblk = plugin_prc->blocks[cblkno];

							// copy segments into plugin codeblock buffer, and point host code block data
							// to plugin data buffer
							plugin_cblk->compressedDataLength = grk_min_buf_vec_get_len(&cblk->seg_buffers);
							grk_min_buf_vec_copy_to_contiguous_buffer(&cblk->seg_buffers, plugin_cblk->compressedData);
							cblk->data = plugin_cblk->compressedData;
							cblk->dataSize = (uint32_t)plugin_cblk->compressedDataLength;

							plugin_cblk->numBitPlanes = cblk->numbps;
							plugin_cblk->numPasses = cblk->segs[0].numpasses;
						}
					}
				}
			}
		}
	}
	return true;
}

// Performed after plugin decode
bool decode_synch_host_with_plugin(grk_tcd_t *tcd) {
	if (tcd->current_plugin_tile && tcd->current_plugin_tile->tileComponents) {
		grk_tcd_tile_t *tcd_tile = tcd->tile;
		for (uint32_t compno = 0; compno < tcd_tile->numcomps; compno++) {
			grk_tcd_tilecomp_t *tilec = &tcd_tile->comps[compno];
			for (uint32_t resno = 0; resno < tilec->numresolutions; resno++) {
				grk_tcd_resolution_t *res = &tilec->resolutions[resno];

				for (uint32_t bandno = 0; bandno < res->numbands; bandno++) {
					grk_tcd_band_t *band = &res->bands[bandno];

					for (uint32_t precno = 0; precno < res->pw * res->ph; precno++) {
						grk_tcd_precinct_t *prc = &band->precincts[precno];

						for (uint32_t cblkno = 0; cblkno < prc->cw * prc->ch; cblkno++) {

							opj_plugin_band_t* plugin_band = tcd->current_plugin_tile->tileComponents[compno]->resolutions[resno]->bands[bandno];
							opj_plugin_precinct_t* precinct = plugin_band->precincts[precno];
							opj_plugin_code_block_t* plugin_cblk = precinct->blocks[cblkno];

							grk_tcd_cblk_dec_t *cblk = &prc->cblks.dec[cblkno];
							if (!cblk->numSegments)
								continue;
							if (cblk->numSegments != 1)
								return false;

							// copy segments into plugin codeblock buffer, and point host code block data
							// to plugin data buffer
							plugin_cblk->compressedDataLength = grk_min_buf_vec_get_len(&cblk->seg_buffers);
							grk_min_buf_vec_copy_to_contiguous_buffer(&cblk->seg_buffers, plugin_cblk->compressedData);
							cblk->data = plugin_cblk->compressedData;
							cblk->dataSize = (uint32_t)plugin_cblk->compressedDataLength;

							plugin_cblk->numBitPlanes = cblk->numbps;
							plugin_cblk->numPasses = cblk->segs[0].numpasses;
						}
					}
				}
			}
		}
	}
	return true;
}



bool tile_equals(opj_plugin_tile_t* plugin_tile,
	grk_tcd_tile_t *p_tile) {
	uint32_t state = opj_plugin_get_debug_state();
	if (!(state & OPJ_PLUGIN_STATE_DEBUG))
		return true;
	if ((!plugin_tile && p_tile) || (plugin_tile && !p_tile))
		return false;
	if (!plugin_tile && !p_tile)
		return true;
	if (plugin_tile->numComponents != p_tile->numcomps)
		return false;
	for (uint32_t compno = 0; compno < p_tile->numcomps; ++compno) {
		grk_tcd_tilecomp_t* tilecomp = p_tile->comps + compno;
		opj_plugin_tile_component_t* plugin_tilecomp = plugin_tile->tileComponents[compno];
		if (tilecomp->numresolutions != plugin_tilecomp->numResolutions)
			return false;
		for (uint32_t resno = 0; resno < tilecomp->numresolutions; ++resno) {
			grk_tcd_resolution_t* resolution = tilecomp->resolutions + resno;
			opj_plugin_resolution_t* plugin_resolution = plugin_tilecomp->resolutions[resno];
			if (resolution->numbands != plugin_resolution->numBands)
				return false;
			for (uint32_t bandno = 0; bandno < resolution->numbands; ++bandno) {
				grk_tcd_band_t* band = resolution->bands + bandno;
				opj_plugin_band_t* plugin_band = plugin_resolution->bands[bandno];
				size_t num_precincts = band->numPrecincts();
				if (num_precincts != plugin_band->numPrecincts)
					return false;
				for (size_t precno = 0; precno < num_precincts; ++precno) {
					grk_tcd_precinct_t* precinct = band->precincts + precno;
					opj_plugin_precinct_t* plugin_precinct = plugin_band->precincts[precno];
					if (precinct->ch * precinct->cw != plugin_precinct->numBlocks) {
						return false;
					}
					for (uint32_t cblkno = 0; cblkno < precinct->ch * precinct->cw; ++cblkno) {
						grk_tcd_cblk_dec_t* cblk = precinct->cblks.dec + cblkno;
						opj_plugin_code_block_t* plugin_cblk = plugin_precinct->blocks[cblkno];
						if (cblk->x0 != plugin_cblk->x0 ||
							cblk->x1 != plugin_cblk->x1 ||
							cblk->y0 != plugin_cblk->y0 ||
							cblk->y1 != plugin_cblk->y1)
							return false;
					}
				}
			}
		}
	}
	return true;
}

void encode_synch_with_plugin(grk_tcd_t *tcd,
							uint32_t compno,
							uint32_t resno,
							uint32_t bandno,
							uint32_t precno,
							uint32_t cblkno,
							grk_tcd_band_t *band,
							grk_tcd_cblk_enc_t *cblk,
							uint32_t* numPix) {

	if (tcd->current_plugin_tile && tcd->current_plugin_tile->tileComponents) {
		opj_plugin_band_t* plugin_band = tcd->current_plugin_tile->tileComponents[compno]->resolutions[resno]->bands[bandno];
		opj_plugin_precinct_t* precinct = plugin_band->precincts[precno];
		opj_plugin_code_block_t* plugin_cblk = precinct->blocks[cblkno];
		uint32_t state = opj_plugin_get_debug_state();

		if (state & OPJ_PLUGIN_STATE_DEBUG) {
			if (band->stepsize != plugin_band->stepsize) {
				printf("Warning: ojp band step size %f differs from plugin step size %f\n", band->stepsize, plugin_band->stepsize);
			}
			if (cblk->num_passes_encoded != plugin_cblk->numPasses)
				printf("Warning: OPJ total number of passes (%d) differs from plugin total number of passes (%d) : component=%d, res=%d, band=%d, block=%d\n", cblk->num_passes_encoded, (uint32_t)plugin_cblk->numPasses, compno, resno, bandno, cblkno);
		}

		cblk->num_passes_encoded = (uint32_t)plugin_cblk->numPasses;
		*numPix = (uint32_t)plugin_cblk->numPix;

		if (state & OPJ_PLUGIN_STATE_DEBUG) {
			uint32_t opjNumPix = ((cblk->x1 - cblk->x0) * (cblk->y1 - cblk->y0));
			if (plugin_cblk->numPix != opjNumPix)
				printf("Warning: ojp numPix %d differs from plugin numPix %d\n", opjNumPix, (uint32_t)plugin_cblk->numPix);
		}

		bool goodData = true;
		uint32_t totalRatePlugin = (uint32_t)plugin_cblk->compressedDataLength;

		//check data
		if (state & OPJ_PLUGIN_STATE_DEBUG) {
			uint32_t totalRate = 0;
			if (cblk->num_passes_encoded > 0) {
				totalRate = (cblk->passes + cblk->num_passes_encoded - 1)->rate;
				if (totalRatePlugin != totalRate) {
					printf("Warning: opj rate %d differs from plugin rate %d\n", totalRate, totalRatePlugin);
				}
			}

			for (uint32_t p = 0; p < totalRate; ++p) {
				if (cblk->data[p] != plugin_cblk->compressedData[p]) {
					printf("Warning: data differs at position=%d, component=%d, res=%d, band=%d, block=%d, opj rate =%d, plugin rate=%d\n",
						p,
						compno,
						resno,
						bandno,
						cblkno,
						totalRate,
						totalRatePlugin);
					goodData = false;
					break;
				}
			}
		}

		if (goodData)
			cblk->data = plugin_cblk->compressedData;
		cblk->data_size = (uint32_t)(plugin_cblk->compressedDataLength);
		cblk->owns_data = false;
		cblk->numbps = (uint32_t)plugin_cblk->numBitPlanes;
		if (state & OPJ_PLUGIN_STATE_DEBUG) {
			if (cblk->x0 != plugin_cblk->x0 ||
				cblk->y0 != plugin_cblk->y0 ||
				cblk->x1 != plugin_cblk->x1 ||
				cblk->y1 != plugin_cblk->y1) {
				printf("Error: plugin code block bounding box differs from OPJ code block");
			}
		}

		uint32_t lastRate = 0;
		for (uint32_t passno = 0; passno < cblk->num_passes_encoded; passno++) {
			grk_tcd_pass_t *pass = cblk->passes + passno;
			opj_plugin_pass_t* pluginPass = plugin_cblk->passes + passno;

			// synch distortion, if applicable
			if (grk_tcd_needs_rate_control(tcd->tcp, &tcd->cp->m_specific_param.m_enc)) {
				if (state & OPJ_PLUGIN_STATE_DEBUG) {
					if (fabs(pass->distortiondec - pluginPass->distortionDecrease) / fabs(pass->distortiondec) > 0.01) {
						printf("Warning: distortion decrease for pass %d differs between plugin and OPJ:  plugin: %f, OPJ : %f\n", passno, pluginPass->distortionDecrease, pass->distortiondec);
					}
				}
				pass->distortiondec = pluginPass->distortionDecrease;
			}
			uint32_t pluginRate = (uint32_t)(pluginPass->rate + 3);
			if (pluginRate > totalRatePlugin)
				pluginRate = totalRatePlugin;

			//Preventing generation of FF as last data byte of a pass
			if ((pluginRate>1) && (plugin_cblk->compressedData[pluginRate - 1] == 0xFF)) {
				pluginRate--;
			}

			if (state & OPJ_PLUGIN_STATE_DEBUG) {
				if (pluginRate != pass->rate) {
					printf("Warning: plugin rate %d differs from OPJ rate %d\n", pluginRate, pass->rate);
				}
			}

			pass->rate = pluginRate;
			pass->len = pass->rate - lastRate;
			lastRate = pass->rate;
		}
	}
}


// set context stream for debugging purposes
void set_context_stream(grk_tcd_t *p_tcd) {
	for (uint32_t compno = 0; compno < p_tcd->tile->numcomps; compno++) {
		grk_tcd_tilecomp_t *tilec = p_tcd->tile->comps + compno;
		tilec->numpix = 0;

		for (uint32_t resno = 0; resno < tilec->numresolutions; resno++) {
			grk_tcd_resolution_t *res = &tilec->resolutions[resno];

			for (uint32_t bandno = 0; bandno < res->numbands; bandno++) {
				grk_tcd_band_t *band = &res->bands[bandno];

				for (uint32_t precno = 0; precno < res->pw * res->ph; precno++) {
					grk_tcd_precinct_t *prc = &band->precincts[precno];

					for (uint32_t cblkno = 0; cblkno < prc->cw * prc->ch; cblkno++) {
						grk_tcd_cblk_enc_t *cblk = &prc->cblks.enc[cblkno];

						if (p_tcd->current_plugin_tile && p_tcd->current_plugin_tile->tileComponents) {
							opj_plugin_tile_component_t* comp = p_tcd->current_plugin_tile->tileComponents[compno];
							if (resno < comp->numResolutions) {
								opj_plugin_band_t* plugin_band = comp->resolutions[resno]->bands[bandno];
								opj_plugin_precinct_t* precinct = plugin_band->precincts[precno];
								opj_plugin_code_block_t* plugin_cblk = precinct->blocks[cblkno];
								cblk->contextStream = plugin_cblk->contextStream;
							}
						}
					}
				}
			}
		}
	}
}

// Debug: these methods wrap plugin methods for parsing a context stream
void  mqc_next_plane(plugin_debug_mqc_t *mqc) {
	minpf_plugin_manager* mgr = NULL;
	PLUGIN_DEBUG_MQC_NEXT_PLANE func = NULL;
	mgr = minpf_get_plugin_manager();
	if (mgr && mgr->num_libraries > 0) {
		func = (PLUGIN_DEBUG_MQC_NEXT_PLANE)minpf_get_symbol(mgr->dynamic_libraries[0], plugin_debug_mqc_next_plane_method_name);
		if (func) {
			func(mqc);
		}
	}
}

void nextCXD(plugin_debug_mqc_t *mqc, uint32_t d) {
	minpf_plugin_manager* mgr = NULL;
	PLUGIN_DEBUG_MQC_NEXT_CXD func = NULL;
	mgr = minpf_get_plugin_manager();
	if (mgr && mgr->num_libraries > 0) {
		func = (PLUGIN_DEBUG_MQC_NEXT_CXD)minpf_get_symbol(mgr->dynamic_libraries[0], plugin_debug_mqc_next_cxd_method_name);
		if (func) {
			func(mqc, d);
		}
	}
}

