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
*
*    You should have received a copy of the GNU Affero General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
*
*    This source code incorporates work covered by the following copyright and
*    permission notice:
*
 * The copyright in this software is being made available under the 2-clauses
 * BSD License, included below. This software may be subject to other third
 * party and contributor rights, including patent rights, and no such rights
 * are granted under this license.
 *
 * Copyright (c) 2002-2014, Universite catholique de Louvain (UCL), Belgium
 * Copyright (c) 2002-2014, Professor Benoit Macq
 * Copyright (c) 2001-2003, David Janssens
 * Copyright (c) 2002-2003, Yannick Verschueren
 * Copyright (c) 2003-2007, Francois-Olivier Devaux
 * Copyright (c) 2003-2014, Antonin Descampe
 * Copyright (c) 2005, Herve Drolon, FreeImage Team
 * Copyright (c) 2006-2007, Parvatha Elangovan
 * Copyright (c) 2008, 2011-2012, Centre National d'Etudes Spatiales (CNES), FR
 * Copyright (c) 2012, CS Systemes d'Information, France
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS `AS IS'
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include "grk_includes.h"
#include "T1Decoder.h"

/* ----------------------------------------------------------------------- */


/**
 * Initializes tile coding/decoding
 */
static inline bool grk_tcd_init_tile(grk_tcd_t *p_tcd,
                                     uint32_t p_tile_no,
                                     opj_image_t* output_image,
                                     bool isEncoder,
                                     float fraction,
                                     size_t sizeof_block,
                                     grk_event_mgr_t* manager);



/**
 * Deallocates the decoding data of the given precinct.
 */
static void grk_tcd_code_block_dec_deallocate (grk_tcd_precinct_t * p_precinct);

/**
 * Allocates memory for an encoding code block (but not data).
 */
static bool grk_tcd_code_block_enc_allocate (grk_tcd_cblk_enc_t * p_code_block);

/**
 * Allocates data for an encoding code block
 */
static bool grk_tcd_code_block_enc_allocate_data (grk_tcd_cblk_enc_t * p_code_block, size_t nominalBlockSize);

/**
 * Deallocates the encoding data of the given precinct.
 */
static void grk_tcd_code_block_enc_deallocate (grk_tcd_precinct_t * p_precinct);


/**
Free the memory allocated for encoding
@param tcd TCD handle
*/
static void grk_tcd_free_tile(grk_tcd_t *tcd);


static bool grk_tcd_t2_decode ( grk_tcd_t *p_tcd,
								uint32_t p_tile_no,
                                opj_seg_buf_t* src_buf,
                                uint64_t * p_data_read,
                                grk_event_mgr_t *p_manager);

static bool grk_tcd_t1_decode (grk_tcd_t *p_tcd, grk_event_mgr_t *p_manager);

static bool grk_tcd_dwt_decode (grk_tcd_t *p_tcd);

static bool grk_tcd_mct_decode (grk_tcd_t *p_tcd, grk_event_mgr_t *p_manager);

static bool grk_tcd_dc_level_shift_decode (grk_tcd_t *p_tcd);


static bool grk_tcd_dc_level_shift_encode ( grk_tcd_t *p_tcd );

static bool grk_tcd_mct_encode ( grk_tcd_t *p_tcd );

static bool grk_tcd_dwt_encode ( grk_tcd_t *p_tcd );

static bool grk_tcd_t1_encode ( grk_tcd_t *p_tcd );

static bool grk_tcd_t2_encode (     grk_tcd_t *p_tcd,
                                    uint8_t * p_dest_data,
                                    uint64_t * p_data_written,
                                    uint64_t p_max_dest_size,
                                    opj_codestream_info_t *p_cstr_info,
									grk_event_mgr_t * p_manager);

static bool grk_tcd_rate_allocate_encode(   grk_tcd_t *p_tcd,
											uint64_t p_max_dest_size,
											opj_codestream_info_t *p_cstr_info );

static bool grk_tcd_layer_needs_rate_control(uint32_t layno, grk_tcp_t *tcd_tcp, opj_encoding_param_t* enc_params);

static bool grk_tcd_make_single_lossless_layer(grk_tcd_t *tcd);

static void grk_tcd_makelayer_final(grk_tcd_t *tcd,
	uint32_t layno);


static bool grk_tcd_pcrd_bisect_simple(grk_tcd_t *tcd,
	uint64_t * p_data_written,
	uint64_t len);

static void grk_tcd_make_layer_simple(grk_tcd_t *tcd,
	uint32_t layno,
	double thresh,
	bool final);

static bool grk_tcd_pcrd_bisect_feasible(grk_tcd_t *tcd,
	uint64_t * p_data_written,
	uint64_t len);


static void grk_tcd_makelayer_feasible(grk_tcd_t *tcd,
	uint32_t layno,
	uint16_t thresh,
	bool final);




/* ----------------------------------------------------------------------- */

/**
Create a new TCD handle
*/
grk_tcd_t* grk_tcd_create(bool p_is_decoder)
{
    grk_tcd_t *l_tcd = nullptr;

    /* create the tcd structure */
    l_tcd = (grk_tcd_t*) grk_calloc(1,sizeof(grk_tcd_t));
    if (!l_tcd) {
        return nullptr;
    }

    l_tcd->m_is_decoder = p_is_decoder ? 1 : 0;

    return l_tcd;
}

/*
if
- r xx, yy, zz, 0   (disto_alloc == 1 and rates == 0)
or
- q xx, yy, zz, 0   (fixed_quality == 1 and distoratio == 0)

then don't try to find an optimal threshold but rather take everything not included yet.

It is possible to have some lossy layers and the last layer for sure lossless

*/
bool grk_tcd_layer_needs_rate_control(uint32_t layno, grk_tcp_t *tcd_tcp, opj_encoding_param_t* enc_params) {

	return ((enc_params->m_disto_alloc == 1) && (tcd_tcp->rates[layno] > 0.0)) ||
		((enc_params->m_fixed_quality == 1) && (tcd_tcp->distoratio[layno] > 0.0f));
}

bool grk_tcd_needs_rate_control(grk_tcp_t *tcd_tcp, opj_encoding_param_t* enc_params) {
	for (uint32_t i = 0; i < tcd_tcp->numlayers; ++i) {
		if (grk_tcd_layer_needs_rate_control(i, tcd_tcp, enc_params))
			return true;
	}
	return false;
}


bool grk_tcd_make_single_lossless_layer(grk_tcd_t *tcd) {
	if (tcd->tcp->numlayers == 1 && !grk_tcd_layer_needs_rate_control(0, tcd->tcp, &tcd->cp->m_specific_param.m_enc)) {

		grk_tcd_makelayer_final(tcd, 0);
		return true;
	}
	return false;
}


void grk_tcd_makelayer_feasible(grk_tcd_t *tcd,
								uint32_t layno,
								uint16_t thresh,
								bool final)
{
	uint32_t compno, resno, bandno, precno, cblkno;
	uint32_t passno;
	grk_tcd_tile_t *tcd_tile = tcd->tile;

	tcd_tile->distolayer[layno] = 0;

	for (compno = 0; compno < tcd_tile->numcomps; compno++) {
		grk_tcd_tilecomp_t *tilec = tcd_tile->comps + compno;

		for (resno = 0; resno < tilec->numresolutions; resno++) {
			grk_tcd_resolution_t *res = tilec->resolutions + resno;

			for (bandno = 0; bandno < res->numbands; bandno++) {
				grk_tcd_band_t *band = res->bands + bandno;

				for (precno = 0; precno < res->pw * res->ph; precno++) {
					grk_tcd_precinct_t *prc = band->precincts + precno;

					for (cblkno = 0; cblkno < prc->cw * prc->ch; cblkno++) {
						grk_tcd_cblk_enc_t *cblk = prc->cblks.enc + cblkno;
						grk_tcd_layer_t *layer = cblk->layers + layno;
						uint32_t cumulative_included_passes_in_block;

						if (layno == 0) {
							cblk->num_passes_included_in_other_layers = 0;
						}

						cumulative_included_passes_in_block =
							cblk->num_passes_included_in_other_layers;

						for (passno = cblk->num_passes_included_in_other_layers;
							passno < cblk->num_passes_encoded; passno++) {
							grk_tcd_pass_t *pass = &cblk->passes[passno];

							//truncate or include feasible, otherwise ignore
							if (pass->slope) {
								if (pass->slope <= thresh)
									break;
								cumulative_included_passes_in_block = passno + 1;
							}
						}

						layer->numpasses =
							cumulative_included_passes_in_block - cblk->num_passes_included_in_other_layers;

						if (!layer->numpasses) {
							layer->disto = 0;
							continue;
						}

						// update layer
						if (cblk->num_passes_included_in_other_layers == 0) {
							layer->len = cblk->passes[cumulative_included_passes_in_block - 1].rate;
							layer->data = cblk->data;
							layer->disto =	cblk->passes[cumulative_included_passes_in_block - 1].distortiondec;
						}
						else {
							layer->len =
								cblk->passes[cumulative_included_passes_in_block - 1].rate - cblk->passes[cblk->num_passes_included_in_other_layers - 1].rate;
							layer->data =	cblk->data + cblk->passes[cblk->num_passes_included_in_other_layers - 1].rate;
							layer->disto =	
								cblk->passes[cumulative_included_passes_in_block - 1].distortiondec - cblk->passes[cblk->num_passes_included_in_other_layers - 1].distortiondec;
						}

						tcd_tile->distolayer[layno] += layer->disto;

						if (final)
							cblk->num_passes_included_in_other_layers = cumulative_included_passes_in_block;
					}
				}
			}
		}
	}
}




/*
Hybrid rate control using bisect algorithm with optimal truncation points
*/
bool grk_tcd_pcrd_bisect_feasible(grk_tcd_t *tcd,
	uint64_t * p_data_written,
	uint64_t len)
{

	bool single_lossless = tcd->tcp->numlayers == 1 && !grk_tcd_layer_needs_rate_control(0, tcd->tcp, &tcd->cp->m_specific_param.m_enc);
	double cumdisto[100];
	const double K = 1;
	double maxSE = 0;

	opj_cp_t *cp = tcd->cp;
	grk_tcd_tile_t *tcd_tile = tcd->tile;
	grk_tcp_t *tcd_tcp = tcd->tcp;

	tcd_tile->numpix = 0;
	uint32_t state = opj_plugin_get_debug_state();

	RateInfo rateInfo;
	for (uint32_t compno = 0; compno < tcd_tile->numcomps; compno++) {
		grk_tcd_tilecomp_t *tilec = &tcd_tile->comps[compno];
		tilec->numpix = 0;
		for (uint32_t resno = 0; resno < tilec->numresolutions; resno++) {
			grk_tcd_resolution_t *res = &tilec->resolutions[resno];

			for (uint32_t bandno = 0; bandno < res->numbands; bandno++) {
				grk_tcd_band_t *band = &res->bands[bandno];

				for (uint32_t precno = 0; precno < res->pw * res->ph; precno++) {
					grk_tcd_precinct_t *prc = &band->precincts[precno];

					for (uint32_t cblkno = 0; cblkno < prc->cw * prc->ch; cblkno++) {
						grk_tcd_cblk_enc_t *cblk = &prc->cblks.enc[cblkno];

						uint32_t numPix = ((cblk->x1 - cblk->x0) * (cblk->y1 - cblk->y0));
						if (!(state &OPJ_PLUGIN_STATE_PRE_TR1)) {
							encode_synch_with_plugin(tcd,
								compno,
								resno,
								bandno,
								precno,
								cblkno,
								band,
								cblk,
								&numPix);
						}

						if (!single_lossless) {
							RateControl::convexHull(cblk->passes, cblk->num_passes_encoded);
							rateInfo.synch(cblk);

							tcd_tile->numpix += numPix;
							tilec->numpix += numPix;
						}
					} /* cbklno */
				} /* precno */
			} /* bandno */
		} /* resno */

		if (!single_lossless) {
			maxSE += (double)(((uint64_t)1 << tcd->image->comps[compno].prec) - 1.0)
				* (((uint64_t)1 << tcd->image->comps[compno].prec) - 1.0)
				* tilec->numpix;
		}
	} /* compno */

	if (grk_tcd_make_single_lossless_layer(tcd)) {
		return true;
	}

	uint32_t min_slope = rateInfo.getMinimumThresh();
	uint32_t max_slope = USHRT_MAX;

	uint32_t upperBound = max_slope;
	for (uint32_t layno = 0; layno < tcd_tcp->numlayers; layno++) {
		uint32_t lowerBound = min_slope;
		uint64_t maxlen = tcd_tcp->rates[layno] > 0.0f ? grk_min<uint64_t>(((uint64_t)ceil(tcd_tcp->rates[layno])), len) : len;

		/* Threshold for Marcela Index */
		// start by including everything in this layer
		uint32_t goodthresh = 0;
		// thresh from previous iteration - starts off uninitialized
		// used to bail out if difference with current thresh is small enough
		uint32_t prevthresh = 0;
		if (grk_tcd_layer_needs_rate_control(layno, tcd_tcp, &cp->m_specific_param.m_enc)) {
			grk_t2_t*t2 = grk_t2_create(tcd->image, cp);
			if (t2 == nullptr) {
				return false;
			}
			double distotarget =
				tcd_tile->distotile - ((K * maxSE) / pow(10.0, tcd_tcp->distoratio[layno] / 10.0));

			uint32_t thresh;
			for (uint32_t i = 0; i < 128; ++i) {
				thresh = (lowerBound + upperBound) >> 1;
				if (prevthresh != 0 && prevthresh ==thresh)
					break;
				grk_tcd_makelayer_feasible(tcd, layno, thresh, false);
				prevthresh = thresh;
				if (cp->m_specific_param.m_enc.m_fixed_quality) {
					double distoachieved =
						layno == 0 ?
						tcd_tile->distolayer[0] :
						cumdisto[layno - 1] + tcd_tile->distolayer[layno];

					if (distoachieved < distotarget) {
						upperBound = thresh;
						continue;
					}
					lowerBound = thresh;
				}
				else {
					if (!grk_t2_encode_packets_simulate(t2,
						tcd->tcd_tileno,
						tcd_tile,
						layno + 1,
						p_data_written,
						maxlen,
						tcd->tp_pos)) {
						lowerBound = thresh;
						continue;
					}
					upperBound = thresh;
				}
			}
			// choose conservative value for goodthresh
			goodthresh = upperBound;
			grk_t2_destroy(t2);

			grk_tcd_makelayer_feasible(tcd, layno, goodthresh, true);
			cumdisto[layno] =
				(layno == 0) ?
				tcd_tile->distolayer[0] :
				(cumdisto[layno - 1] + tcd_tile->distolayer[layno]);
			// upper bound for next layer is initialized to lowerBound for current layer, minus one
			upperBound = lowerBound - 1;;
		}
		else {
			grk_tcd_makelayer_final(tcd, layno);
		}
	}
	return true;
}




/*
Simple bisect algorithm to calculate optimal layer truncation points
*/
bool grk_tcd_pcrd_bisect_simple(  grk_tcd_t *tcd,
                            uint64_t * p_data_written,
                            uint64_t len)
{
    uint32_t compno, resno, bandno, precno, cblkno, layno;
    uint32_t passno;
    double cumdisto[100];     
    const double K = 1;              
    double maxSE = 0;

    opj_cp_t *cp = tcd->cp;
    grk_tcd_tile_t *tcd_tile = tcd->tile;
    grk_tcp_t *tcd_tcp = tcd->tcp;

	double min_slope = DBL_MAX;
	double max_slope = -1;

    tcd_tile->numpix = 0;         
	uint32_t state = opj_plugin_get_debug_state();

    for (compno = 0; compno < tcd_tile->numcomps; compno++) {
        grk_tcd_tilecomp_t *tilec = &tcd_tile->comps[compno];
        tilec->numpix = 0;

        for (resno = 0; resno < tilec->numresolutions; resno++) {
            grk_tcd_resolution_t *res = &tilec->resolutions[resno];

            for (bandno = 0; bandno < res->numbands; bandno++) {
                grk_tcd_band_t *band = &res->bands[bandno];

                for (precno = 0; precno < res->pw * res->ph; precno++) {
                    grk_tcd_precinct_t *prc = &band->precincts[precno];

                    for (cblkno = 0; cblkno < prc->cw * prc->ch; cblkno++) {
                        grk_tcd_cblk_enc_t *cblk = &prc->cblks.enc[cblkno];

						uint32_t numPix = ((cblk->x1 - cblk->x0) * (cblk->y1 - cblk->y0));
						if (!(state &OPJ_PLUGIN_STATE_PRE_TR1)) {
							encode_synch_with_plugin(tcd,
								compno,
								resno,
								bandno,
								precno,
								cblkno,
								band,
								cblk,
								&numPix);
						}

						for (passno = 0; passno < cblk->num_passes_encoded; passno++) {
							grk_tcd_pass_t *pass = &cblk->passes[passno];
							int32_t dr;
							double dd, rdslope;

							if (passno == 0) {
								dr = (int32_t)pass->rate;
								dd = pass->distortiondec;
							}
							else {
								dr = (int32_t)(pass->rate - cblk->passes[passno - 1].rate);
								dd = pass->distortiondec - cblk->passes[passno - 1].distortiondec;
							}

							if (dr == 0) {
								continue;
							}

							rdslope = dd / dr;
							if (rdslope < min_slope) {
								min_slope = rdslope;
							}

							if (rdslope > max_slope) {
								max_slope = rdslope;
							}
						} /* passno */
						tcd_tile->numpix += numPix;
						tilec->numpix += numPix;
	
                    } /* cbklno */
                } /* precno */
            } /* bandno */
        } /* resno */

		maxSE += (double)(((uint64_t)1 << tcd->image->comps[compno].prec) - 1.0)
			* (((uint64_t)1 << tcd->image->comps[compno].prec) - 1.0)
			* tilec->numpix;

    } /* compno */

	if (grk_tcd_make_single_lossless_layer(tcd)) {
		return true;
	}

	double upperBound = max_slope;
	for (layno = 0; layno < tcd_tcp->numlayers; layno++) {
		if (grk_tcd_layer_needs_rate_control(layno, tcd_tcp, &cp->m_specific_param.m_enc)) {

			double lowerBound = min_slope;
			uint64_t maxlen = tcd_tcp->rates[layno] > 0.0f ? grk_min<uint64_t>(((uint64_t)ceil(tcd_tcp->rates[layno])), len) : len;

			/* Threshold for Marcela Index */
			// start by including everything in this layer
			double goodthresh = 0;


			// thresh from previous iteration - starts off uninitialized
			// used to bail out if difference with current thresh is small enough
			double prevthresh = -1;

			double distotarget =
				tcd_tile->distotile - ((K * maxSE) / pow(10.0, tcd_tcp->distoratio[layno] / 10.0));


			grk_t2_t*t2 = grk_t2_create(tcd->image, cp);
			if (t2 == nullptr) {
				return false;
			}
			double thresh;
			for (uint32_t i = 0; i < 128; ++i) {
				thresh = (upperBound == -1) ? lowerBound : (lowerBound + upperBound) / 2;
				grk_tcd_make_layer_simple(tcd, layno, thresh, false);
				if (prevthresh != -1 && (fabs(prevthresh - thresh)) < 0.001)
					break;
				prevthresh = thresh;
				if (cp->m_specific_param.m_enc.m_fixed_quality) {
					double distoachieved =	layno == 0 ?
												tcd_tile->distolayer[0] :
													cumdisto[layno - 1] + tcd_tile->distolayer[layno];

					if (distoachieved < distotarget) {
						upperBound = thresh;
						continue;
					}
					lowerBound = thresh;
				}
				else {
					if (!grk_t2_encode_packets_simulate(t2,
														tcd->tcd_tileno,
														tcd_tile,
														layno + 1,
														p_data_written,
														maxlen,
														tcd->tp_pos)) {
						lowerBound = thresh;
						continue;
					}
					upperBound = thresh;
				}
			}
			// choose conservative value for goodthresh
			goodthresh = (upperBound == -1) ? thresh : upperBound;
			grk_t2_destroy(t2);

			grk_tcd_make_layer_simple(tcd, layno, goodthresh, true);
			cumdisto[layno] =	(layno == 0) ?
									tcd_tile->distolayer[0] :
										(cumdisto[layno - 1] + tcd_tile->distolayer[layno]);

			// upper bound for next layer will equal lowerBound for previous layer, minus one
			upperBound = lowerBound - 1;
		}
		else {
			//todo: shouldn't need rate-distortion slope calculations to make this last layer
			grk_tcd_make_layer_simple(tcd, layno, 0, true);
			assert(layno == tcd_tcp->numlayers - 1);
			// this has to be the last layer, so return 
			return true;
		}
    }
    return true;
}

static void prepareBlockForFirstLayer(grk_tcd_cblk_enc_t* cblk) {
	cblk->num_passes_included_in_other_layers = 0;
	cblk->num_passes_included_in_current_layer = 0;
	cblk->numlenbits = 0;

}

/*
Form layer for bisect rate control algorithm
*/
void grk_tcd_make_layer_simple(grk_tcd_t *tcd,
								uint32_t layno,
								double thresh,
								bool final)
{
	uint32_t compno, resno, bandno, precno, cblkno;
	uint32_t passno;
	grk_tcd_tile_t *tcd_tile = tcd->tile;

	tcd_tile->distolayer[layno] = 0;

	for (compno = 0; compno < tcd_tile->numcomps; compno++) {
		grk_tcd_tilecomp_t *tilec = tcd_tile->comps + compno;

		for (resno = 0; resno < tilec->numresolutions; resno++) {
			grk_tcd_resolution_t *res = tilec->resolutions + resno;

			for (bandno = 0; bandno < res->numbands; bandno++) {
				grk_tcd_band_t *band = res->bands + bandno;

				for (precno = 0; precno < res->pw * res->ph; precno++) {
					grk_tcd_precinct_t *prc = band->precincts + precno;

					for (cblkno = 0; cblkno < prc->cw * prc->ch; cblkno++) {
						grk_tcd_cblk_enc_t *cblk = prc->cblks.enc + cblkno;
						grk_tcd_layer_t *layer = cblk->layers + layno;
						uint32_t cumulative_included_passes_in_block;

						if (layno == 0) {
							prepareBlockForFirstLayer(cblk);
						}

						if (thresh == 0) {
							cumulative_included_passes_in_block = cblk->num_passes_encoded;
						}
						else {

							cumulative_included_passes_in_block =	cblk->num_passes_included_in_other_layers;

							for (passno = cblk->num_passes_included_in_other_layers; passno < cblk->num_passes_encoded; passno++) {
								uint32_t dr;
								double dd;
								grk_tcd_pass_t *pass = &cblk->passes[passno];

								if (cumulative_included_passes_in_block == 0) {
									dr = pass->rate;
									dd = pass->distortiondec;
								}
								else {
									dr =
										pass->rate - cblk->passes[cumulative_included_passes_in_block - 1].rate;
									dd =
										pass->distortiondec - cblk->passes[cumulative_included_passes_in_block - 1].distortiondec;
								}

								if (!dr) {
									if (dd != 0)
										cumulative_included_passes_in_block = passno + 1;
									continue;
								}
								auto slope = dd / dr;
								/* do not rely on float equality, check with DBL_EPSILON margin */
								if (thresh - slope < DBL_EPSILON)
									cumulative_included_passes_in_block = passno + 1;
							}
						}

						layer->numpasses = 	cumulative_included_passes_in_block - cblk->num_passes_included_in_other_layers;

						if (!layer->numpasses) {
							layer->disto = 0;
							continue;
						}

						// update layer
						if (cblk->num_passes_included_in_other_layers == 0) {
							layer->len = cblk->passes[cumulative_included_passes_in_block - 1].rate;
							layer->data = cblk->data;
							layer->disto =
								cblk->passes[cumulative_included_passes_in_block - 1].distortiondec;
						}
						else {
							layer->len =
								cblk->passes[cumulative_included_passes_in_block - 1].rate - cblk->passes[cblk->num_passes_included_in_other_layers - 1].rate;
							layer->data =
								cblk->data + cblk->passes[cblk->num_passes_included_in_other_layers - 1].rate;
							layer->disto =
								cblk->passes[cumulative_included_passes_in_block - 1].distortiondec - cblk->passes[cblk->num_passes_included_in_other_layers - 1].distortiondec;
						}

						tcd_tile->distolayer[layno] += layer->disto;

						if (final)
							cblk->num_passes_included_in_other_layers = cumulative_included_passes_in_block;
					}
				}
			}
		}
	}
}


/*
Add all remaining passes to this layer
*/
void grk_tcd_makelayer_final(grk_tcd_t *tcd, uint32_t layno)
{
	uint32_t compno, resno, bandno, precno, cblkno;
	uint32_t passno;
	grk_tcd_tile_t *tcd_tile = tcd->tile;

	tcd_tile->distolayer[layno] = 0;

	for (compno = 0; compno < tcd_tile->numcomps; compno++) {
		grk_tcd_tilecomp_t *tilec = tcd_tile->comps + compno;

		for (resno = 0; resno < tilec->numresolutions; resno++) {
			grk_tcd_resolution_t *res = tilec->resolutions + resno;

			for (bandno = 0; bandno < res->numbands; bandno++) {
				grk_tcd_band_t *band = res->bands + bandno;

				for (precno = 0; precno < res->pw * res->ph; precno++) {
					grk_tcd_precinct_t *prc = band->precincts + precno;

					for (cblkno = 0; cblkno < prc->cw * prc->ch; cblkno++) {
						grk_tcd_cblk_enc_t *cblk = prc->cblks.enc + cblkno;
						grk_tcd_layer_t *layer = cblk->layers + layno;
						uint32_t cumulative_included_passes_in_block;

						if (layno == 0) {
							prepareBlockForFirstLayer(cblk);
						}

						cumulative_included_passes_in_block =
							cblk->num_passes_included_in_other_layers;

						for (passno = cblk->num_passes_included_in_other_layers; passno < cblk->num_passes_encoded; passno++) {
							grk_tcd_pass_t *pass = &cblk->passes[passno];
							cumulative_included_passes_in_block = passno + 1;
						}

						layer->numpasses =
							cumulative_included_passes_in_block - cblk->num_passes_included_in_other_layers;

						if (!layer->numpasses) {
							layer->disto = 0;
							continue;
						}

						// update layer
						if (cblk->num_passes_included_in_other_layers == 0) {
							layer->len = cblk->passes[cumulative_included_passes_in_block - 1].rate;
							layer->data = cblk->data;
							layer->disto =
								cblk->passes[cumulative_included_passes_in_block - 1].distortiondec;
						}
						else {
							layer->len =
								cblk->passes[cumulative_included_passes_in_block - 1].rate - cblk->passes[cblk->num_passes_included_in_other_layers - 1].rate;
							layer->data =
								cblk->data + cblk->passes[cblk->num_passes_included_in_other_layers - 1].rate;
							layer->disto =
								cblk->passes[cumulative_included_passes_in_block - 1].distortiondec - cblk->passes[cblk->num_passes_included_in_other_layers - 1].distortiondec;
						}

						tcd_tile->distolayer[layno] += layer->disto;

						cblk->num_passes_included_in_other_layers = cumulative_included_passes_in_block;

						assert(cblk->num_passes_included_in_other_layers == cblk->num_passes_encoded);
					}
				}
			}
		}
	}
}



bool grk_tcd_init( grk_tcd_t *p_tcd,
                   opj_image_t * p_image,
                   opj_cp_t * p_cp,
				   uint32_t numThreads)
{
    p_tcd->image = p_image;
    p_tcd->cp = p_cp;

    p_tcd->tile = (grk_tcd_tile_t *) grk_calloc(1,sizeof(grk_tcd_tile_t));
    if (! p_tcd->tile) {
        return false;
    }

    p_tcd->tile->comps = (grk_tcd_tilecomp_t *) grk_calloc(p_image->numcomps,sizeof(grk_tcd_tilecomp_t));
    if (! p_tcd->tile->comps ) {
        return false;
    }

    p_tcd->tile->numcomps = p_image->numcomps;
    p_tcd->tp_pos = p_cp->m_specific_param.m_enc.m_tp_pos;
	p_tcd->numThreads = numThreads;

    return true;
}

/**
Destroy a previously created TCD handle
*/
void grk_tcd_destroy(grk_tcd_t *tcd)
{
    if (tcd) {
        grk_tcd_free_tile(tcd);
        grk_free(tcd);
    }
}

/* ----------------------------------------------------------------------- */

static inline bool grk_tcd_init_tile(grk_tcd_t *p_tcd,
                                     uint32_t p_tile_no,
                                     opj_image_t* output_image,
                                     bool isEncoder,
                                     float fraction,
                                     size_t sizeof_block,
                                     grk_event_mgr_t* manager)
{
    uint32_t (*l_gain_ptr)(uint32_t) = nullptr;
    uint32_t compno, resno, bandno, precno, cblkno;
    grk_tcp_t * l_tcp = nullptr;
    opj_cp_t * l_cp = nullptr;
    grk_tcd_tile_t * l_tile = nullptr;
    opj_tccp_t *l_tccp = nullptr;
    grk_tcd_tilecomp_t *l_tilec = nullptr;
    opj_image_comp_t * l_image_comp = nullptr;
    grk_tcd_resolution_t *l_res = nullptr;
    grk_tcd_band_t *l_band = nullptr;
    opj_stepsize_t * l_step_size = nullptr;
    grk_tcd_precinct_t *l_current_precinct = nullptr;
    opj_image_t *l_image = nullptr;
    uint32_t p,q;
    uint32_t l_level_no;
    uint32_t l_pdx, l_pdy;
    uint32_t l_gain;
    uint32_t l_x0b, l_y0b;
    uint32_t l_tx0, l_ty0;
    /* extent of precincts , top left, bottom right**/
    uint32_t l_tl_prc_x_start, l_tl_prc_y_start, l_br_prc_x_end, l_br_prc_y_end;
    /* number of precinct for a resolution */
    uint32_t l_nb_precincts;
    /* room needed to store l_nb_precinct precinct for a resolution */
    uint32_t l_nb_precinct_size;
    /* number of code blocks for a precinct*/
    uint32_t l_nb_code_blocks;
    /* room needed to store l_nb_code_blocks code blocks for a precinct*/
    uint32_t l_nb_code_blocks_size;

	uint32_t state = opj_plugin_get_debug_state();

    l_cp = p_tcd->cp;
    l_tcp = &(l_cp->tcps[p_tile_no]);
    l_tile = p_tcd->tile;
    l_tccp = l_tcp->tccps;
    l_tilec = l_tile->comps;
    l_image = p_tcd->image;
    l_image_comp = p_tcd->image->comps;

    opj_seg_buf_rewind(l_tcp->m_data);

    p = p_tile_no % l_cp->tw;       /* tile coordinates */
    q = p_tile_no / l_cp->tw;
    /*fprintf(stderr, "Tile coordinate = %d,%d\n", p, q);*/

    /* 4 borders of the tile rescale on the image if necessary */
    l_tx0 = l_cp->tx0 + p * l_cp->tdx; /* can't be greater than l_image->x1 so won't overflow */
    l_tile->x0 = grk_max<uint32_t>(l_tx0, l_image->x0);
    l_tile->x1 = grk_min<uint32_t>(grk_uint_adds(l_tx0, l_cp->tdx), l_image->x1);
	if (l_tile->x1 <= l_tile->x0) {
		grk_event_msg(manager, EVT_ERROR, "Tile x coordinates are not valid\n");
		return false;
	}
    l_ty0 = l_cp->ty0 + q * l_cp->tdy; /* can't be greater than l_image->y1 so won't overflow */
    l_tile->y0 = grk_max<uint32_t>(l_ty0, l_image->y0);
    l_tile->y1 = grk_min<uint32_t>(grk_uint_adds(l_ty0, l_cp->tdy), l_image->y1);
	if (l_tile->y1 <= l_tile->y0) {
		grk_event_msg(manager, EVT_ERROR, "Tile y coordinates are not valid\n");
		return false;
	}

    /* testcase 1888.pdf.asan.35.988 */
    if (l_tccp->numresolutions == 0) {
        grk_event_msg(manager, EVT_ERROR, "tiles require at least one resolution\n");
        return false;
    }
    /*fprintf(stderr, "Tile border = %d,%d,%d,%d\n", l_tile->x0, l_tile->y0,l_tile->x1,l_tile->y1);*/

    /*tile->numcomps = image->numcomps; */
    for (compno = 0; compno < l_tile->numcomps; ++compno) {
        uint64_t l_tile_data_size=0;
        uint32_t l_res_data_size=0;
        /*fprintf(stderr, "compno = %d/%d\n", compno, l_tile->numcomps);*/
		if (l_image_comp->dx == 0 || l_image_comp->dy == 0) {
			return false;
		}
		l_image_comp->resno_decoded = 0;
        /* border of each l_tile component (global) */
        l_tilec->x0 = grk_uint_ceildiv(l_tile->x0, l_image_comp->dx);
        l_tilec->y0 = grk_uint_ceildiv(l_tile->y0, l_image_comp->dy);
        l_tilec->x1 = grk_uint_ceildiv(l_tile->x1, l_image_comp->dx);
        l_tilec->y1 = grk_uint_ceildiv(l_tile->y1, l_image_comp->dy);
        /*fprintf(stderr, "\tTile compo border = %d,%d,%d,%d\n", l_tilec->x0, l_tilec->y0,l_tilec->x1,l_tilec->y1);*/

        /* compute l_data_size with overflow check */
        l_tile_data_size = (uint64_t)(l_tilec->x1 - l_tilec->x0) * (uint64_t)(l_tilec->y1 - l_tilec->y0) * sizeof(uint32_t);
        l_tilec->numresolutions = l_tccp->numresolutions;
        if (l_tccp->numresolutions < l_cp->m_specific_param.m_dec.m_reduce) {
            l_tilec->minimum_num_resolutions = 1;
        } else {
            l_tilec->minimum_num_resolutions = l_tccp->numresolutions - l_cp->m_specific_param.m_dec.m_reduce;
        }

        l_res_data_size = l_tilec->numresolutions * (uint32_t)sizeof(grk_tcd_resolution_t);

        if (l_tilec->resolutions == nullptr) {
            l_tilec->resolutions = (grk_tcd_resolution_t *) grk_malloc(l_res_data_size);
            if (! l_tilec->resolutions ) {
                return false;
            }
            /*fprintf(stderr, "\tAllocate resolutions of tilec (grk_tcd_resolution_t): %d\n",l_data_size);*/
            l_tilec->resolutions_size = l_res_data_size;
            memset(l_tilec->resolutions,0, l_res_data_size);
        } else if (l_res_data_size > l_tilec->resolutions_size) {
            grk_tcd_resolution_t* new_resolutions = (grk_tcd_resolution_t *) grk_realloc(l_tilec->resolutions, l_res_data_size);
            if (! new_resolutions) {
                grk_event_msg(manager, EVT_ERROR, "Not enough memory for tile resolutions\n");
                grk_free(l_tilec->resolutions);
                l_tilec->resolutions = NULL;
                l_tilec->resolutions_size = 0;
                return false;
            }
            l_tilec->resolutions = new_resolutions;
            /*fprintf(stderr, "\tReallocate data of tilec (int): from %d to %d x uint32_t\n", l_tilec->resolutions_size, l_data_size);*/
            memset(((uint8_t*) l_tilec->resolutions)+l_tilec->resolutions_size,0, l_res_data_size - l_tilec->resolutions_size);
            l_tilec->resolutions_size = l_res_data_size;
        }

        l_level_no = l_tilec->numresolutions;
        l_res = l_tilec->resolutions;
        l_step_size = l_tccp->stepsizes;
        if (l_tccp->qmfbid == 0) {
            l_gain_ptr = &grk_dwt_getgain_real;
        } else {
            l_gain_ptr  = &grk_dwt_getgain;
        }
        /*fprintf(stderr, "\tlevel_no=%d\n",l_level_no);*/

        for (resno = 0; resno < l_tilec->numresolutions; ++resno) {
            /*fprintf(stderr, "\t\tresno = %d/%d\n", resno, l_tilec->numresolutions);*/
            uint32_t tlcbgxstart, tlcbgystart /*, brcbgxend, brcbgyend*/;
            uint32_t cbgwidthexpn, cbgheightexpn;
            uint32_t cblkwidthexpn, cblkheightexpn;

			--l_level_no;

            /* border for each resolution level (global) */
            l_res->x0 = grk_uint_ceildivpow2(l_tilec->x0, l_level_no);
            l_res->y0 = grk_uint_ceildivpow2(l_tilec->y0, l_level_no);
            l_res->x1 = grk_uint_ceildivpow2(l_tilec->x1, l_level_no);
            l_res->y1 = grk_uint_ceildivpow2(l_tilec->y1, l_level_no);
            /*fprintf(stderr, "\t\t\tres_x0= %d, res_y0 =%d, res_x1=%d, res_y1=%d\n", l_res->x0, l_res->y0, l_res->x1, l_res->y1);*/
            /* p. 35, table A-23, ISO/IEC FDIS154444-1 : 2000 (18 august 2000) */
            l_pdx = l_tccp->prcw[resno];
            l_pdy = l_tccp->prch[resno];
            /*fprintf(stderr, "\t\t\tpdx=%d, pdy=%d\n", l_pdx, l_pdy);*/
            /* p. 64, B.6, ISO/IEC FDIS15444-1 : 2000 (18 august 2000)  */
            l_tl_prc_x_start = grk_uint_floordivpow2(l_res->x0, l_pdx) << l_pdx;
            l_tl_prc_y_start = grk_uint_floordivpow2(l_res->y0, l_pdy) << l_pdy;
            l_br_prc_x_end = grk_uint_ceildivpow2(l_res->x1, l_pdx) << l_pdx;
            l_br_prc_y_end = grk_uint_ceildivpow2(l_res->y1, l_pdy) << l_pdy;
            /*fprintf(stderr, "\t\t\tprc_x_start=%d, prc_y_start=%d, br_prc_x_end=%d, br_prc_y_end=%d \n", l_tl_prc_x_start, l_tl_prc_y_start, l_br_prc_x_end ,l_br_prc_y_end );*/

            l_res->pw = (l_res->x0 == l_res->x1) ? 0 : ((l_br_prc_x_end - l_tl_prc_x_start) >> l_pdx);
            l_res->ph = (l_res->y0 == l_res->y1) ? 0 : ((l_br_prc_y_end - l_tl_prc_y_start) >> l_pdy);
            /*fprintf(stderr, "\t\t\tres_pw=%d, res_ph=%d\n", l_res->pw, l_res->ph );*/

			if (grk_mult_will_overflow(l_res->pw, l_res->ph)) {
				grk_event_msg(manager, EVT_ERROR, "l_nb_precincts calculation would overflow \n");
				return false;
			}
            l_nb_precincts = l_res->pw * l_res->ph;

			if (grk_mult_will_overflow(l_nb_precincts, (uint32_t)sizeof(grk_tcd_precinct_t))) {
				grk_event_msg(manager, EVT_ERROR, "l_nb_precinct_size calculation would overflow \n");
				return false;
			}
            l_nb_precinct_size = l_nb_precincts * (uint32_t)sizeof(grk_tcd_precinct_t);
            if (resno == 0) {
                tlcbgxstart = l_tl_prc_x_start;
                tlcbgystart = l_tl_prc_y_start;
                /*brcbgxend = l_br_prc_x_end;*/
                /* brcbgyend = l_br_prc_y_end;*/
                cbgwidthexpn = l_pdx;
                cbgheightexpn = l_pdy;
                l_res->numbands = 1;
            } else {
                tlcbgxstart = grk_uint_ceildivpow2(l_tl_prc_x_start, 1);
                tlcbgystart = grk_uint_ceildivpow2(l_tl_prc_y_start, 1);
                cbgwidthexpn = l_pdx - 1;
                cbgheightexpn = l_pdy - 1;
                l_res->numbands = 3;
            }

            cblkwidthexpn = grk_min<uint32_t>(l_tccp->cblkw, cbgwidthexpn);
            cblkheightexpn = grk_min<uint32_t>(l_tccp->cblkh, cbgheightexpn);
			size_t nominalBlockSize = (1 << cblkwidthexpn)*(1 << cblkheightexpn);
            l_band = l_res->bands;

            for (bandno = 0; bandno < l_res->numbands; ++bandno) {
                uint32_t numbps;
                /*fprintf(stderr, "\t\t\tband_no=%d/%d\n", bandno, l_res->numbands );*/

                if (resno == 0) {
                    l_band->bandno = 0 ;
                    l_band->x0 = grk_uint_ceildivpow2(l_tilec->x0, l_level_no);
                    l_band->y0 = grk_uint_ceildivpow2(l_tilec->y0, l_level_no);
                    l_band->x1 = grk_uint_ceildivpow2(l_tilec->x1, l_level_no);
                    l_band->y1 = grk_uint_ceildivpow2(l_tilec->y1, l_level_no);
                } else {
                    l_band->bandno = bandno + 1;
                    /* x0b = 1 if bandno = 1 or 3 */
                    l_x0b = l_band->bandno&1;
                    /* y0b = 1 if bandno = 2 or 3 */
                    l_y0b = (int32_t)((l_band->bandno)>>1);
                    /* l_band border (global) */
                    l_band->x0 = grk_uint64_ceildivpow2(l_tilec->x0 - ((uint64_t)l_x0b << l_level_no), l_level_no + 1);
                    l_band->y0 = grk_uint64_ceildivpow2(l_tilec->y0 - ((uint64_t)l_y0b << l_level_no), l_level_no + 1);
                    l_band->x1 = grk_uint64_ceildivpow2(l_tilec->x1 - ((uint64_t)l_x0b << l_level_no), l_level_no + 1);
                    l_band->y1 = grk_uint64_ceildivpow2(l_tilec->y1 - ((uint64_t)l_y0b << l_level_no), l_level_no + 1);
                }

                /** avoid an if with storing function pointer */
                l_gain = (*l_gain_ptr) (l_band->bandno);
                numbps = l_image_comp->prec + l_gain;
                l_band->stepsize = (float)(((1.0 + l_step_size->mant / 2048.0) * pow(2.0, (int32_t) (numbps - l_step_size->expn)))) * fraction;
                l_band->numbps = l_step_size->expn + l_tccp->numgbits - 1;      /* WHY -1 ? */

                if (!l_band->precincts && (l_nb_precincts > 0U)) {
                    l_band->precincts = (grk_tcd_precinct_t *) grk_malloc( /*3 * */ l_nb_precinct_size);
                    if (! l_band->precincts) {
						grk_event_msg(manager, EVT_ERROR, "Not enough memory for band precints\n");
                        return false;
                    }
                    /*fprintf(stderr, "\t\t\t\tAllocate precincts of a band (grk_tcd_precinct_t): %d\n",l_nb_precinct_size);     */
                    memset(l_band->precincts,0,l_nb_precinct_size);
                    l_band->precincts_data_size = l_nb_precinct_size;
                } else if (l_band->precincts_data_size < l_nb_precinct_size) {

                    grk_tcd_precinct_t * new_precincts = (grk_tcd_precinct_t *) grk_realloc(l_band->precincts,/*3 * */ l_nb_precinct_size);
                    if (! new_precincts) {
                        grk_event_msg(manager, EVT_ERROR, "Not enough memory to handle band precints\n");
                        grk_free(l_band->precincts);
                        l_band->precincts = NULL;
                        l_band->precincts_data_size = 0;
                        return false;
                    }
                    l_band->precincts = new_precincts;
                    /*fprintf(stderr, "\t\t\t\tReallocate precincts of a band (grk_tcd_precinct_t): from %d to %d\n",l_band->precincts_data_size, l_nb_precinct_size);*/
                    memset(((uint8_t *) l_band->precincts) + l_band->precincts_data_size,0,l_nb_precinct_size - l_band->precincts_data_size);
                    l_band->precincts_data_size = l_nb_precinct_size;
                }

                l_current_precinct = l_band->precincts;
                for (precno = 0; precno < l_nb_precincts; ++precno) {
                    uint32_t tlcblkxstart, tlcblkystart, brcblkxend, brcblkyend;
                    uint32_t cbgxstart = tlcbgxstart + (precno % l_res->pw) * (1 << cbgwidthexpn);
                    uint32_t cbgystart = tlcbgystart + (precno / l_res->pw) * (1 << cbgheightexpn);
                    uint32_t cbgxend = cbgxstart + (1 << cbgwidthexpn);
                    uint32_t cbgyend = cbgystart + (1 << cbgheightexpn);
                    /*fprintf(stderr, "\t precno=%d; bandno=%d, resno=%d; compno=%d\n", precno, bandno , resno, compno);*/
                    /*fprintf(stderr, "\t tlcbgxstart(=%d) + (precno(=%d) percent res->pw(=%d)) * (1 << cbgwidthexpn(=%d)) \n",tlcbgxstart,precno,l_res->pw,cbgwidthexpn);*/

                    /* precinct size (global) */
                    /*fprintf(stderr, "\t cbgxstart=%d, l_band->x0 = %d \n",cbgxstart, l_band->x0);*/

                    l_current_precinct->x0 = grk_max<uint32_t>(cbgxstart, l_band->x0);
                    l_current_precinct->y0 = grk_max<uint32_t>(cbgystart, l_band->y0);
                    l_current_precinct->x1 = grk_min<uint32_t>(cbgxend, l_band->x1);
                    l_current_precinct->y1 = grk_min<uint32_t>(cbgyend, l_band->y1);
                    /*fprintf(stderr, "\t prc_x0=%d; prc_y0=%d, prc_x1=%d; prc_y1=%d\n",l_current_precinct->x0, l_current_precinct->y0 ,l_current_precinct->x1, l_current_precinct->y1);*/

                    tlcblkxstart = grk_uint_floordivpow2(l_current_precinct->x0, cblkwidthexpn) << cblkwidthexpn;
                    /*fprintf(stderr, "\t tlcblkxstart =%d\n",tlcblkxstart );*/
                    tlcblkystart = grk_uint_floordivpow2(l_current_precinct->y0, cblkheightexpn) << cblkheightexpn;
                    /*fprintf(stderr, "\t tlcblkystart =%d\n",tlcblkystart );*/
                    brcblkxend = grk_uint_ceildivpow2(l_current_precinct->x1, cblkwidthexpn) << cblkwidthexpn;
                    /*fprintf(stderr, "\t brcblkxend =%d\n",brcblkxend );*/
                    brcblkyend = grk_uint_ceildivpow2(l_current_precinct->y1, cblkheightexpn) << cblkheightexpn;
                    /*fprintf(stderr, "\t brcblkyend =%d\n",brcblkyend );*/
                    l_current_precinct->cw = ((brcblkxend - tlcblkxstart) >> cblkwidthexpn);
                    l_current_precinct->ch = ((brcblkyend - tlcblkystart) >> cblkheightexpn);

					if (grk_mult_will_overflow(l_current_precinct->cw, l_current_precinct->ch)) {
						grk_event_msg(manager, EVT_ERROR, "l_nb_code_blocks calculation would overflow \n");
						return false;
					}
                    l_nb_code_blocks = l_current_precinct->cw * l_current_precinct->ch;
                    /*fprintf(stderr, "\t\t\t\t precinct_cw = %d x recinct_ch = %d\n",l_current_precinct->cw, l_current_precinct->ch);      */

					if (grk_mult_will_overflow(l_nb_code_blocks, (uint32_t)sizeof_block)) {
						grk_event_msg(manager, EVT_ERROR, "l_nb_code_blocks_size calculation would overflow \n");
						return false;
					}
                    l_nb_code_blocks_size = l_nb_code_blocks * (uint32_t)sizeof_block;

                    if (!l_current_precinct->cblks.blocks && (l_nb_code_blocks > 0U)) {
                        l_current_precinct->cblks.blocks = grk_malloc(l_nb_code_blocks_size);
                        if (! l_current_precinct->cblks.blocks ) {
                            return false;
                        }
                        /*fprintf(stderr, "\t\t\t\tAllocate cblks of a precinct (grk_tcd_cblk_dec_t): %d\n",l_nb_code_blocks_size);*/
                        memset(l_current_precinct->cblks.blocks,0,l_nb_code_blocks_size);

                        l_current_precinct->block_size = l_nb_code_blocks_size;
                    } else if (l_nb_code_blocks_size > l_current_precinct->block_size) {
                        void *new_blocks = grk_realloc(l_current_precinct->cblks.blocks, l_nb_code_blocks_size);
                        if (! new_blocks) {
                            grk_free(l_current_precinct->cblks.blocks);
                            l_current_precinct->cblks.blocks = NULL;
                            l_current_precinct->block_size = 0;
                            grk_event_msg(manager, EVT_ERROR, "Not enough memory for current precinct codeblock element\n");
                            return false;
                        }
                        l_current_precinct->cblks.blocks = new_blocks;
                        /*fprintf(stderr, "\t\t\t\tReallocate cblks of a precinct (grk_tcd_cblk_dec_t): from %d to %d\n",l_current_precinct->block_size, l_nb_code_blocks_size);     */

                        memset(((uint8_t *) l_current_precinct->cblks.blocks) + l_current_precinct->block_size,0,l_nb_code_blocks_size - l_current_precinct->block_size);
                        l_current_precinct->block_size = l_nb_code_blocks_size;
                    }

					l_current_precinct->initTagTrees(manager);
					
                    for (cblkno = 0; cblkno < l_nb_code_blocks; ++cblkno) {
                        uint32_t cblkxstart = tlcblkxstart + (cblkno % l_current_precinct->cw) * (1 << cblkwidthexpn);
                        uint32_t cblkystart = tlcblkystart + (cblkno / l_current_precinct->cw) * (1 << cblkheightexpn);
                        uint32_t cblkxend = cblkxstart + (1 << cblkwidthexpn);
                        uint32_t cblkyend = cblkystart + (1 << cblkheightexpn);

                        if (isEncoder) {
                            grk_tcd_cblk_enc_t* l_code_block = l_current_precinct->cblks.enc + cblkno;

                            if (! grk_tcd_code_block_enc_allocate(l_code_block)) {
                                return false;
                            }
                            /* code-block size (global) */
                            l_code_block->x0 = grk_max<uint32_t>(cblkxstart, l_current_precinct->x0);
                            l_code_block->y0 = grk_max<uint32_t>(cblkystart, l_current_precinct->y0);
                            l_code_block->x1 = grk_min<uint32_t>(cblkxend, l_current_precinct->x1);
                            l_code_block->y1 = grk_min<uint32_t>(cblkyend, l_current_precinct->y1);

							if (!p_tcd->current_plugin_tile || (state & OPJ_PLUGIN_STATE_DEBUG)) {
								if (!grk_tcd_code_block_enc_allocate_data(l_code_block, nominalBlockSize)) {
									return false;
								}
							}
                        } else {
                            grk_tcd_cblk_dec_t* l_code_block = l_current_precinct->cblks.dec + cblkno;
							if (!p_tcd->current_plugin_tile || (state & OPJ_PLUGIN_STATE_DEBUG)) {
								if (!l_code_block->alloc()) {
									return false;
								}
							}

                            /* code-block size (global) */
                            l_code_block->x0 = grk_max<uint32_t>(cblkxstart, l_current_precinct->x0);
                            l_code_block->y0 = grk_max<uint32_t>(cblkystart, l_current_precinct->y0);
                            l_code_block->x1 = grk_min<uint32_t>(cblkxend, l_current_precinct->x1);
                            l_code_block->y1 = grk_min<uint32_t>(cblkyend, l_current_precinct->y1);
                        }
                    }
                    ++l_current_precinct;
                } /* precno */
                ++l_band;
                ++l_step_size;
            } /* bandno */
            ++l_res;
        } /* resno */
        if (!grk_tile_buf_create_component(l_tilec, 
											isEncoder,
                                           l_tccp->qmfbid ? false : true,
                                           1 << l_tccp->cblkw,
                                           1 << l_tccp->cblkh,
                                           output_image,
                                           l_image_comp->dx,
                                           l_image_comp->dy)) {
            return false;
        }
        l_tilec->buf->data_size_needed = l_tile_data_size;

        ++l_tccp;
        ++l_tilec;
        ++l_image_comp;
    } /* compno */


	// decoder plugin debug sanity check on tile struct
	if (!isEncoder) {
		if (state & OPJ_PLUGIN_STATE_DEBUG) {
			if (!tile_equals(p_tcd->current_plugin_tile, l_tile)) {
				manager->warning_handler("plugin tile differs from opj tile", NULL);
			}
		}
	}
    return true;
}

bool grk_tcd_init_encode_tile (grk_tcd_t *p_tcd, uint32_t p_tile_no, grk_event_mgr_t* p_manager)
{
    return grk_tcd_init_tile(p_tcd, p_tile_no, NULL, true, 1.0F, sizeof(grk_tcd_cblk_enc_t), p_manager);
}

bool grk_tcd_init_decode_tile (grk_tcd_t *p_tcd,
                               opj_image_t* output_image,
                               uint32_t p_tile_no,
                               grk_event_mgr_t* p_manager)
{
    return  grk_tcd_init_tile(p_tcd,
                              p_tile_no,
                              output_image,
                              false,
                              0.5F,
                              sizeof(grk_tcd_cblk_dec_t),
                              p_manager);

}

/**
 * Allocates memory for an encoding code block (but not data memory).
 */
static bool grk_tcd_code_block_enc_allocate (grk_tcd_cblk_enc_t * p_code_block)
{
    if (! p_code_block->layers) {
        /* no memset since data */
        p_code_block->layers = (grk_tcd_layer_t*) grk_calloc(100, sizeof(grk_tcd_layer_t));
        if (! p_code_block->layers) {
            return false;
        }
    }
    if (! p_code_block->passes) {
        p_code_block->passes = (grk_tcd_pass_t*) grk_calloc(100, sizeof(grk_tcd_pass_t));
        if (! p_code_block->passes) {
            return false;
        }
    }
    return true;
}

/**
 * Allocates data memory for an encoding code block.
 */
static bool grk_tcd_code_block_enc_allocate_data (grk_tcd_cblk_enc_t * p_code_block, size_t nominalBlockSize)
{
    uint32_t l_data_size = (uint32_t)(nominalBlockSize * sizeof(uint32_t));

    if (l_data_size > p_code_block->data_size) {
        if (p_code_block->data) {
            grk_free(p_code_block->data); 
        }
        p_code_block->data = (uint8_t*) grk_malloc(l_data_size+1);
        if(! p_code_block->data) {
            p_code_block->data_size = 0U;
            return false;
        }
        p_code_block->data_size = l_data_size;
		p_code_block->owns_data = true;
    }
    return true;
}

/**
 * Allocates memory for a decoding code block (but not data)
 */
bool grk_tcd_code_block_dec_allocate (grk_tcd_cblk_dec_t * p_code_block)
{
    if (!p_code_block->segs) {
        p_code_block->segs = (grk_tcd_seg_t *)grk_calloc(OPJ_J2K_DEFAULT_NB_SEGS, sizeof(grk_tcd_seg_t));
        if (!p_code_block->segs) {
            return false;
        }
        /*fprintf(stderr, "Allocate %d elements of code_block->data\n", OPJ_J2K_DEFAULT_NB_SEGS * sizeof(grk_tcd_seg_t));*/

        p_code_block->numSegmentsAllocated = OPJ_J2K_DEFAULT_NB_SEGS;

        /*fprintf(stderr, "Allocate 8192 elements of code_block->data\n");*/
        /*fprintf(stderr, "numSegmentsAllocated of code_block->data = %d\n", p_code_block->numSegmentsAllocated);*/
    } else {
        /* sanitize */
        grk_tcd_seg_t * l_segs = p_code_block->segs;
        uint32_t l_current_max_segs = p_code_block->numSegmentsAllocated;

        /* Note: since seg_buffers simply holds references to another data buffer,
        we do not need to copy it  to the sanitized block  */
		p_code_block->seg_buffers.cleanup();

        memset(p_code_block, 0, sizeof(grk_tcd_cblk_dec_t));
        p_code_block->segs = l_segs;
        p_code_block->numSegmentsAllocated = l_current_max_segs;
    }
    return true;
}
/*
Get size of tile data, summed over all components, reflecting actual precision of data.
opj_image_t always stores data in 32 bit format.
*/
uint64_t grk_tcd_get_decoded_tile_size ( grk_tcd_t *p_tcd )
{
    uint32_t i;
    uint64_t l_data_size = 0;
    opj_image_comp_t * l_img_comp = nullptr;
    grk_tcd_tilecomp_t * l_tile_comp = nullptr;
    grk_tcd_resolution_t * l_res = nullptr;
    uint32_t l_size_comp;

    l_tile_comp = p_tcd->tile->comps;
    l_img_comp = p_tcd->image->comps;

    for (i=0; i<p_tcd->image->numcomps; ++i) {
        l_size_comp = (l_img_comp->prec + 7) >> 3;

        if (l_size_comp == 3) {
            l_size_comp = 4;
        }

        l_res = l_tile_comp->resolutions + l_tile_comp->minimum_num_resolutions - 1;
        l_data_size += l_size_comp * (uint64_t)(l_res->x1 - l_res->x0) * (l_res->y1 - l_res->y0);
        ++l_img_comp;
        ++l_tile_comp;
    }

    return l_data_size;
}

bool grk_tcd_encode_tile(   grk_tcd_t *p_tcd,
                            uint32_t p_tile_no,
                            uint8_t *p_dest,
                            uint64_t * p_data_written,
                            uint64_t p_max_length,
                            opj_codestream_info_t *p_cstr_info,
							grk_event_mgr_t * p_manager)
{
	uint32_t state = opj_plugin_get_debug_state();
    if (p_tcd->cur_tp_num == 0) {

        p_tcd->tcd_tileno = p_tile_no;
        p_tcd->tcp = &p_tcd->cp->tcps[p_tile_no];

        /* INDEX >> "Precinct_nb_X et Precinct_nb_Y" */
        if(p_cstr_info)  {
            uint32_t l_num_packs = 0;
            uint32_t i;
            grk_tcd_tilecomp_t *l_tilec_idx = &p_tcd->tile->comps[0];        /* based on component 0 */
            opj_tccp_t *l_tccp = p_tcd->tcp->tccps; /* based on component 0 */

            for (i = 0; i < l_tilec_idx->numresolutions; i++) {
                grk_tcd_resolution_t *l_res_idx = &l_tilec_idx->resolutions[i];

                p_cstr_info->tile[p_tile_no].pw[i] = (int)l_res_idx->pw;
                p_cstr_info->tile[p_tile_no].ph[i] = (int)l_res_idx->ph;

                l_num_packs += l_res_idx->pw * l_res_idx->ph;
                p_cstr_info->tile[p_tile_no].pdx[i] = (int)l_tccp->prcw[i];
                p_cstr_info->tile[p_tile_no].pdy[i] = (int)l_tccp->prch[i];
            }
            p_cstr_info->tile[p_tile_no].packet = (opj_packet_info_t*) grk_calloc((size_t)p_cstr_info->numcomps * (size_t)p_cstr_info->numlayers * l_num_packs, sizeof(opj_packet_info_t));
            if (!p_cstr_info->tile[p_tile_no].packet) {
                /* FIXME event manager error callback */
                return false;
            }
        }
		/* << INDEX */
		if (state & OPJ_PLUGIN_STATE_DEBUG) {
			set_context_stream(p_tcd);
		}

		// When debugging the encoder, we do all of T1 up to and including DWT in the plugin, and pass this in as image data.
		// This way, both OPJ and plugin start with same inputs for context formation and MQ coding.
		bool debugEncode = state & OPJ_PLUGIN_STATE_DEBUG;
		bool debugMCT = (state & OPJ_PLUGIN_STATE_MCT_ONLY) ? true : false ;

		if (!p_tcd->current_plugin_tile || debugEncode) {

			if (!debugEncode) {
				/* FIXME _ProfStart(PGROUP_DC_SHIFT); */
				/*---------------TILE-------------------*/
				if (!grk_tcd_dc_level_shift_encode(p_tcd)) {
					return false;
				}
				/* FIXME _ProfStop(PGROUP_DC_SHIFT); */

				/* FIXME _ProfStart(PGROUP_MCT); */
				if (!grk_tcd_mct_encode(p_tcd)) {
					return false;
				}
				/* FIXME _ProfStop(PGROUP_MCT); */
			}

			if (!debugEncode || debugMCT) {
				/* FIXME _ProfStart(PGROUP_DWT); */
				if (!grk_tcd_dwt_encode(p_tcd)) {
					return false;
				}
				/* FIXME  _ProfStop(PGROUP_DWT); */
			}


			/* FIXME  _ProfStart(PGROUP_T1); */
			if (!grk_tcd_t1_encode(p_tcd)) {
				return false;
			}
			/* FIXME _ProfStop(PGROUP_T1); */

		}

		/* FIXME _ProfStart(PGROUP_RATE); */
		if (!grk_tcd_rate_allocate_encode(p_tcd, p_max_length, p_cstr_info)) {
			return false;
		}
		/* FIXME _ProfStop(PGROUP_RATE); */

    }
    /*--------------TIER2------------------*/

    /* INDEX */
    if (p_cstr_info) {
        p_cstr_info->index_write = 1;
    }
    /* FIXME _ProfStart(PGROUP_T2); */

    if (! grk_tcd_t2_encode(p_tcd,
							p_dest,
							p_data_written,
							p_max_length,
							p_cstr_info,
							p_manager)) {
        return false;
    }
    /* FIXME _ProfStop(PGROUP_T2); */

    /*---------------CLEAN-------------------*/

    return true;
}

bool grk_tcd_decode_tile(grk_tcd_t *p_tcd,
	opj_seg_buf_t* src_buf,
	uint32_t p_tile_no,
	grk_event_mgr_t *p_manager) {
	p_tcd->tcp = p_tcd->cp->tcps + p_tile_no;

	bool doT2 = !p_tcd->current_plugin_tile ||
		(p_tcd->current_plugin_tile->decode_flag& OPJ_PLUGIN_DECODE_T2);

	bool doT1 = !p_tcd->current_plugin_tile ||
		(p_tcd->current_plugin_tile->decode_flag & OPJ_PLUGIN_DECODE_T1);

	bool doPostT1 = !p_tcd->current_plugin_tile ||
		(p_tcd->current_plugin_tile->decode_flag & OPJ_PLUGIN_DECODE_POST_T1);

	if (doT2) {
		uint64_t l_data_read = 0;
		if (!grk_tcd_t2_decode(p_tcd, p_tile_no, src_buf, &l_data_read, p_manager)) {
			return false;
		}

		// synch plugin with T2 data
		if (!decode_synch_plugin_with_host(p_tcd))
			return false;
	}

	if (doT1) {
		if (!decode_synch_host_with_plugin(p_tcd))
			return false;

		if (!grk_tcd_t1_decode(p_tcd, p_manager)) {
			return false;
		}
	}

	if (doPostT1) {

		if (!grk_tcd_dwt_decode(p_tcd)) {
			return false;
		}
		if (!grk_tcd_mct_decode(p_tcd, p_manager)) {
			return false;
		}
		if (!grk_tcd_dc_level_shift_decode(p_tcd)) {
			return false;
		}
	}
    return true;
}

/*

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
ToDo: does the code below assume consistent resolution dimensions across components? Because of subsampling, I don't think
we can assume that this is the code, but need to check that code is aware of this.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



For each component, copy decoded resolutions from the tile data buffer
into p_dest buffer.

So, p_dest stores a sub-region of the tcd data, based on the number
of resolutions decoded. (why doesn't tile data buffer also match number of resolutions decoded ?)

Note: p_dest stores data in the actual precision of the decompressed image,
vs. tile data buffer which is always 32 bits.

If we are decoding all resolutions, then this step is not necessary ??

*/
bool grk_tcd_update_tile_data ( grk_tcd_t *p_tcd,
                                uint8_t * p_dest,
                                uint64_t p_dest_length
                              )
{
    uint32_t i,j,k;
    opj_image_comp_t * l_img_comp = nullptr;
    grk_tcd_tilecomp_t * l_tilec = nullptr;
    grk_tcd_resolution_t * l_res;
    uint32_t l_size_comp;
    uint32_t l_stride, l_width,l_height;
	uint64_t l_data_size = grk_tcd_get_decoded_tile_size(p_tcd);
    if (l_data_size > p_dest_length) {
        return false;
    }

    l_tilec = p_tcd->tile->comps;
    l_img_comp = p_tcd->image->comps;

    for (i=0; i<p_tcd->image->numcomps; ++i) {
        l_size_comp = (l_img_comp->prec + 7) >> 3;
        l_res = l_tilec->resolutions + l_img_comp->resno_decoded;
        l_width = (l_res->x1 - l_res->x0);
        l_height = (l_res->y1 - l_res->y0);
        l_stride = (l_tilec->x1 - l_tilec->x0) - l_width;

        if (l_size_comp == 3) {
            l_size_comp = 4;
        }

        switch (l_size_comp) {
        case 1: {
            char * l_dest_ptr = (char *) p_dest;
            const int32_t * l_src_ptr = grk_tile_buf_get_ptr(l_tilec->buf, 0, 0, 0, 0);

            if (l_img_comp->sgnd) {
                for (j=0; j<l_height; ++j) {
                    for (k=0; k<l_width; ++k) {
                        *(l_dest_ptr++) = (char) (*(l_src_ptr++));
                    }
                    l_src_ptr += l_stride;
                }
            } else {
                for (j=0; j<l_height; ++j) {
                    for     (k=0; k<l_width; ++k) {
                        *(l_dest_ptr++) = (char) ((*(l_src_ptr++))&0xff);
                    }
                    l_src_ptr += l_stride;
                }
            }

            p_dest = (uint8_t *)l_dest_ptr;
        }
        break;
        case 2: {
            const int32_t * l_src_ptr = grk_tile_buf_get_ptr(l_tilec->buf, 0, 0, 0, 0);
            int16_t * l_dest_ptr = (int16_t *) p_dest;

            if (l_img_comp->sgnd) {
                for (j=0; j<l_height; ++j) {
                    for (k=0; k<l_width; ++k) {
                        *(l_dest_ptr++) = (int16_t) (*(l_src_ptr++));
                    }
                    l_src_ptr += l_stride;
                }
            } else {
                for (j=0; j<l_height; ++j) {
                    for (k=0; k<l_width; ++k) {
						//cast and mask to avoid sign extension
                        *(l_dest_ptr++) = (int16_t) ((*(l_src_ptr++))&0xffff);
                    }
                    l_src_ptr += l_stride;
                }
            }

            p_dest = (uint8_t*) l_dest_ptr;
        }
        break;
        case 4: {
            int32_t * l_dest_ptr = (int32_t *) p_dest;
            int32_t * l_src_ptr = grk_tile_buf_get_ptr(l_tilec->buf, 0, 0, 0, 0);

            for (j=0; j<l_height; ++j) {
                for (k=0; k<l_width; ++k) {
                    *(l_dest_ptr++) = (*(l_src_ptr++));
                }
                l_src_ptr += l_stride;
            }

            p_dest = (uint8_t*) l_dest_ptr;
        }
        break;
        }

        ++l_img_comp;
        ++l_tilec;
    }

    return true;
}




static void grk_tcd_free_tile(grk_tcd_t *p_tcd)
{
    uint32_t compno, resno, bandno, precno;
    grk_tcd_tile_t *l_tile = nullptr;
    grk_tcd_tilecomp_t *l_tile_comp = nullptr;
    grk_tcd_resolution_t *l_res = nullptr;
    grk_tcd_band_t *l_band = nullptr;
    grk_tcd_precinct_t *l_precinct = nullptr;
    size_t l_nb_resolutions, l_nb_precincts;
    void (* l_tcd_code_block_deallocate) (grk_tcd_precinct_t *) = nullptr;

    if (! p_tcd) {
        return;
    }

    if (! p_tcd->tile) {
        return;
    }

    if (p_tcd->m_is_decoder) {
        l_tcd_code_block_deallocate = grk_tcd_code_block_dec_deallocate;
    } else {
        l_tcd_code_block_deallocate = grk_tcd_code_block_enc_deallocate;
    }

    l_tile = p_tcd->tile;
    if (! l_tile) {
        return;
    }

    l_tile_comp = l_tile->comps;

    for (compno = 0; compno < l_tile->numcomps; ++compno) {
        l_res = l_tile_comp->resolutions;
        if (l_res) {

            l_nb_resolutions = l_tile_comp->resolutions_size / sizeof(grk_tcd_resolution_t);
            for (resno = 0; resno < l_nb_resolutions; ++resno) {
                l_band = l_res->bands;
                for     (bandno = 0; bandno < 3; ++bandno) {
                    l_precinct = l_band->precincts;
                    if (l_precinct) {

						l_nb_precincts = l_band->numPrecincts();
                        for (precno = 0; precno < l_nb_precincts; ++precno) {
							if (l_precinct->incltree)
								delete l_precinct->incltree;
                            l_precinct->incltree = nullptr;
							if (l_precinct->imsbtree)
								delete l_precinct->imsbtree;
                            l_precinct->imsbtree = nullptr;
                            (*l_tcd_code_block_deallocate) (l_precinct);
                            ++l_precinct;
                        }

                        grk_free(l_band->precincts);
                        l_band->precincts = nullptr;
                    }
                    ++l_band;
                } /* for (resno */
                ++l_res;
            }

            grk_free(l_tile_comp->resolutions);
            l_tile_comp->resolutions = nullptr;
        }

        grk_tile_buf_destroy_component(l_tile_comp->buf);
        l_tile_comp->buf = NULL;
        ++l_tile_comp;
    }

    grk_free(l_tile->comps);
    l_tile->comps = nullptr;
    grk_free(p_tcd->tile);
    p_tcd->tile = nullptr;
}


static bool grk_tcd_t2_decode (grk_tcd_t *p_tcd,
								uint32_t p_tile_no,
                               opj_seg_buf_t* src_buf,
                               uint64_t * p_data_read,
                               grk_event_mgr_t *p_manager
                              )
{
    grk_t2_t * l_t2;

    l_t2 = grk_t2_create(p_tcd->image, p_tcd->cp);
    if (l_t2 == nullptr) {
        return false;
    }

    if (! grk_t2_decode_packets(
                l_t2,
				p_tile_no,
                p_tcd->tile,
                src_buf,
                p_data_read,
                p_manager)) {
        grk_t2_destroy(l_t2);
        return false;
    }

    grk_t2_destroy(l_t2);

    return true;
}

static bool grk_tcd_t1_decode ( grk_tcd_t *p_tcd, grk_event_mgr_t * p_manager)
{
    uint32_t compno;
    grk_tcd_tile_t * l_tile = p_tcd->tile;
    grk_tcd_tilecomp_t* l_tile_comp = l_tile->comps;
    opj_tccp_t * l_tccp = p_tcd->tcp->tccps;
	std::vector<decodeBlockInfo*> blocks;
	T1Decoder decoder(l_tccp->cblkw, l_tccp->cblkh);
    for (compno = 0; compno < l_tile->numcomps; ++compno) {
        if (false == grk_t1_prepare_decode_cblks(l_tile_comp, l_tccp,&blocks, p_manager)) {
            return false;
        }
        ++l_tile_comp;
        ++l_tccp;
    }
	decoder.decode(&blocks, p_tcd->numThreads);
    return true;
}


static bool grk_tcd_dwt_decode ( grk_tcd_t *p_tcd )
{
    grk_tcd_tile_t * l_tile = p_tcd->tile;
    int64_t compno=0;
    bool rc = true;
#ifdef _OPENMP
    #pragma omp parallel default(none) private(compno) shared(p_tcd, l_tile, rc)
    {
        #pragma omp for
#endif
        for (compno = 0; compno < (int64_t)l_tile->numcomps; compno++) {
            grk_tcd_tilecomp_t * l_tile_comp = l_tile->comps + compno;
            opj_tccp_t * l_tccp = p_tcd->tcp->tccps + compno;
            opj_image_comp_t * l_img_comp = p_tcd->image->comps + compno;
            if (l_tccp->qmfbid == 1) {
                if (! grk_dwt_decode_53(l_tile_comp,
									l_img_comp->resno_decoded+1,
									p_tcd->numThreads)) {
                    rc = false;
                    continue;
                }
            } else {
                if (! grk_dwt_decode_97(l_tile_comp, 
											l_img_comp->resno_decoded+1,
											p_tcd->numThreads)) {
                    rc = false;
                    continue;
                }
            }
#ifdef _OPENMP
        }
#endif

    }

    return rc;
}
static bool grk_tcd_mct_decode ( grk_tcd_t *p_tcd, grk_event_mgr_t *p_manager)
{
    grk_tcd_tile_t * l_tile = p_tcd->tile;
    grk_tcp_t * l_tcp = p_tcd->tcp;
    grk_tcd_tilecomp_t * l_tile_comp = l_tile->comps;
    uint64_t l_samples,i;

    if (! l_tcp->mct) {
        return true;
    }

    l_samples = (uint64_t)(l_tile_comp->x1 - l_tile_comp->x0) * (l_tile_comp->y1 - l_tile_comp->y0);

    if (l_tile->numcomps >= 3 ) {
        /* testcase 1336.pdf.asan.47.376 */
        if ((l_tile->comps[0].x1 - l_tile->comps[0].x0) * (l_tile->comps[0].y1 - l_tile->comps[0].y0) < l_samples ||
                (l_tile->comps[1].x1 - l_tile->comps[1].x0) * (l_tile->comps[1].y1 - l_tile->comps[1].y0) < l_samples ||
                (l_tile->comps[2].x1 - l_tile->comps[2].x0) * (l_tile->comps[2].y1 - l_tile->comps[2].y0) < l_samples) {
            grk_event_msg(p_manager, EVT_ERROR, "Tiles don't all have the same dimension. Skip the MCT step.\n");
            return false;
        } else if (l_tcp->mct == 2) {
            uint8_t ** l_data;

            if (! l_tcp->m_mct_decoding_matrix) {
                return true;
            }

            l_data = (uint8_t **) grk_malloc(l_tile->numcomps*sizeof(uint8_t*));
            if (! l_data) {
                return false;
            }

            for (i=0; i<l_tile->numcomps; ++i) {
                l_data[i] = (uint8_t*)grk_tile_buf_get_ptr(l_tile_comp->buf, 0, 0, 0, 0);
                ++l_tile_comp;
            }

            if (! grk_mct_decode_custom(/* MCT data */
                        (uint8_t*) l_tcp->m_mct_decoding_matrix,
                        /* size of components */
                        l_samples,
                        /* components */
                        l_data,
                        /* nb of components (i.e. size of pData) */
                        l_tile->numcomps,
                        /* tells if the data is signed */
                        p_tcd->image->comps->sgnd)) {
                grk_free(l_data);
                return false;
            }

            grk_free(l_data);
        } else {
            if (l_tcp->tccps->qmfbid == 1) {
                grk_mct_decode(grk_tile_buf_get_ptr(l_tile->comps[0].buf, 0, 0, 0, 0) ,
                               grk_tile_buf_get_ptr(l_tile->comps[1].buf, 0, 0, 0, 0),
                               grk_tile_buf_get_ptr(l_tile->comps[2].buf, 0, 0, 0, 0),
                               l_samples);
            } else {
                grk_mct_decode_real((float*)grk_tile_buf_get_ptr(l_tile->comps[0].buf, 0, 0, 0, 0),
                                    (float*)grk_tile_buf_get_ptr(l_tile->comps[1].buf, 0, 0, 0, 0),
                                    (float*)grk_tile_buf_get_ptr(l_tile->comps[2].buf, 0, 0, 0, 0),
                                    l_samples);
            }
        }
    } else {
        grk_event_msg(p_manager, EVT_ERROR, "Number of components (%d) is inconsistent with a MCT. Skip the MCT step.\n",l_tile->numcomps);
    }

    return true;
}


static bool grk_tcd_dc_level_shift_decode ( grk_tcd_t *p_tcd )
{
    uint32_t compno=0;

#ifdef _OPENMP
#pragma omp parallel default(none) private(compno) shared(p_tcd)
	{
#pragma omp for
#endif

		for (compno = 0; compno < p_tcd->tile->numcomps; compno++) {
			int32_t l_min = INT32_MAX, l_max = INT32_MIN;

			grk_tcd_tilecomp_t *l_tile_comp		= p_tcd->tile->comps + compno;
			opj_tccp_t * l_tccp					= p_tcd->tcp->tccps + compno;
			opj_image_comp_t * l_img_comp		= p_tcd->image->comps + compno;

			grk_tcd_resolution_t* l_res			= l_tile_comp->resolutions + l_img_comp->resno_decoded;
			uint32_t l_width					= (l_res->x1 - l_res->x0);
			uint32_t l_height					= (l_res->y1 - l_res->y0);

			uint32_t scaledTileX0 = grk_uint_ceildivpow2((uint32_t)l_tile_comp->buf->tile_dim.x0, l_img_comp->decodeScaleFactor);
			uint32_t scaledTileY0 = grk_uint_ceildivpow2((uint32_t)l_tile_comp->buf->tile_dim.y0, l_img_comp->decodeScaleFactor);

			uint32_t x0 = (grk_uint_ceildivpow2((uint32_t)l_tile_comp->buf->dim.x0, l_img_comp->decodeScaleFactor) - scaledTileX0);
			uint32_t y0 = (grk_uint_ceildivpow2((uint32_t)l_tile_comp->buf->dim.y0, l_img_comp->decodeScaleFactor) - scaledTileY0);
			uint32_t x1 = (grk_uint_ceildivpow2((uint32_t)l_tile_comp->buf->dim.x1, l_img_comp->decodeScaleFactor) - scaledTileX0);
			uint32_t y1 = (grk_uint_ceildivpow2((uint32_t)l_tile_comp->buf->dim.y1, l_img_comp->decodeScaleFactor) - scaledTileY0);

			uint32_t l_stride					= (l_tile_comp->x1 - l_tile_comp->x0) - (x1-x0);

		//	assert(l_height == 0 || l_width + l_stride <= l_tile_comp->buf->data_size / l_height); 

			if (l_img_comp->sgnd) {
				l_min = -(1 << (l_img_comp->prec - 1));
				l_max = (1 << (l_img_comp->prec - 1)) - 1;
			}
			else {
				l_min = 0;
				l_max = (1 << l_img_comp->prec) - 1;
			}

			int32_t* l_current_ptr = grk_tile_buf_get_ptr(l_tile_comp->buf, 0, 0, 0, 0);
			l_current_ptr += x0 + y0 * (l_tile_comp->x1 - l_tile_comp->x0);

			if (l_tccp->qmfbid == 1) {
				for (uint32_t j = y0; j < y1; ++j) {
					for (uint32_t i = x0; i < x1; ++i) {
						*l_current_ptr = grk_int_clamp(*l_current_ptr + l_tccp->m_dc_level_shift, l_min, l_max);
						l_current_ptr++;
					}
					l_current_ptr += l_stride;
				}
			}
			else {
				for (uint32_t j = y0; j < y1; ++j) {
					for (uint32_t i = x0; i < x1; ++i) {
						float l_value	= *((float *)l_current_ptr);
						*l_current_ptr	= grk_int_clamp((int32_t)grk_lrintf(l_value) + l_tccp->m_dc_level_shift, l_min, l_max); 
						l_current_ptr++;
					}
					l_current_ptr += l_stride;
				}
			}
		}

#ifdef _OPENMP
	}
#endif

    return true;
}



/**
 * Deallocates the encoding data of the given precinct.
 */
static void grk_tcd_code_block_dec_deallocate (grk_tcd_precinct_t * p_precinct)
{
    uint32_t cblkno , l_nb_code_blocks;

    grk_tcd_cblk_dec_t * l_code_block = p_precinct->cblks.dec;
    if (l_code_block) {
        /*fprintf(stderr,"deallocate codeblock:{\n");*/
        /*fprintf(stderr,"\t x0=%d, y0=%d, x1=%d, y1=%d\n",l_code_block->x0, l_code_block->y0, l_code_block->x1, l_code_block->y1);*/
        /*fprintf(stderr,"\t numbps=%d, numlenbits=%d, len=%d, numPassesInPacket=%d, real_num_segs=%d, numSegmentsAllocated=%d\n ",
                        l_code_block->numbps, l_code_block->numlenbits, l_code_block->len, l_code_block->numPassesInPacket, l_code_block->numSegments, l_code_block->numSegmentsAllocated );*/


        l_nb_code_blocks = p_precinct->block_size / sizeof(grk_tcd_cblk_dec_t);
        /*fprintf(stderr,"nb_code_blocks =%d\t}\n", l_nb_code_blocks);*/

        for (cblkno = 0; cblkno < l_nb_code_blocks; ++cblkno) {
			l_code_block->seg_buffers.cleanup();
            if (l_code_block->segs) {
                grk_free(l_code_block->segs );
                l_code_block->segs = nullptr;
            }

            ++l_code_block;
        }

        grk_free(p_precinct->cblks.dec);
        p_precinct->cblks.dec = nullptr;
    }
}

/**
 * Deallocates the encoding data of the given precinct.
 */
static void grk_tcd_code_block_enc_deallocate (grk_tcd_precinct_t * p_precinct)
{
    uint32_t cblkno , l_nb_code_blocks;

    grk_tcd_cblk_enc_t * l_code_block = p_precinct->cblks.enc;
    if (l_code_block) {
        l_nb_code_blocks = p_precinct->block_size / sizeof(grk_tcd_cblk_enc_t);

        for     (cblkno = 0; cblkno < l_nb_code_blocks; ++cblkno)  {
            if (l_code_block->owns_data && l_code_block->data) {
                grk_free(l_code_block->data);
                l_code_block->data = nullptr;
				l_code_block->owns_data = false;
            }

            if (l_code_block->layers) {
                grk_free(l_code_block->layers );
                l_code_block->layers = nullptr;
            }

            if (l_code_block->passes) {
                grk_free(l_code_block->passes );
                l_code_block->passes = nullptr;
            }
            ++l_code_block;
        }

        grk_free(p_precinct->cblks.enc);

        p_precinct->cblks.enc = nullptr;
    }
}

uint64_t grk_tcd_get_encoded_tile_size ( grk_tcd_t *p_tcd )
{
    uint32_t i = 0;
    opj_image_comp_t * l_img_comp = nullptr;
    grk_tcd_tilecomp_t * l_tilec = nullptr;
	uint32_t l_size_comp, l_remaining;
	uint64_t l_data_size = 0;

    l_tilec = p_tcd->tile->comps;
    l_img_comp = p_tcd->image->comps;
    for (i=0; i<p_tcd->image->numcomps; ++i) {
        l_size_comp = l_img_comp->prec >> 3; /*(/ 8)*/
        l_remaining = l_img_comp->prec & 7;  /* (%8) */

        if (l_remaining) {
            ++l_size_comp;
        }

        if (l_size_comp == 3) {
            l_size_comp = 4;
        }

        l_data_size += l_size_comp * (uint64_t)(l_tilec->x1 - l_tilec->x0) * (l_tilec->y1 - l_tilec->y0);
        ++l_img_comp;
        ++l_tilec;
    }

    return l_data_size;
}

static bool grk_tcd_dc_level_shift_encode ( grk_tcd_t *p_tcd )
{
    uint32_t compno;
    grk_tcd_tilecomp_t * l_tile_comp = nullptr;
    opj_tccp_t * l_tccp = nullptr;
    opj_image_comp_t * l_img_comp = nullptr;
    grk_tcd_tile_t * l_tile;
    uint64_t l_nb_elem,i;
    int32_t * l_current_ptr;

    l_tile = p_tcd->tile;
    l_tile_comp = l_tile->comps;
    l_tccp = p_tcd->tcp->tccps;
    l_img_comp = p_tcd->image->comps;

    for (compno = 0; compno < l_tile->numcomps; compno++) {
        l_current_ptr = grk_tile_buf_get_ptr(l_tile_comp->buf, 0, 0, 0, 0);
        l_nb_elem = (uint64_t)(l_tile_comp->x1 - l_tile_comp->x0) * (l_tile_comp->y1 - l_tile_comp->y0);

        if (l_tccp->qmfbid == 1) {
            for (i = 0; i < l_nb_elem; ++i) {
                *l_current_ptr -= l_tccp->m_dc_level_shift ;
                ++l_current_ptr;
            }
        } else {
            for (i = 0; i < l_nb_elem; ++i) {
                *l_current_ptr = (*l_current_ptr - l_tccp->m_dc_level_shift) * (1 << 11) ;
                ++l_current_ptr;
            }
        }

        ++l_img_comp;
        ++l_tccp;
        ++l_tile_comp;
    }

    return true;
}

static bool grk_tcd_mct_encode ( grk_tcd_t *p_tcd )
{
    grk_tcd_tile_t * l_tile = p_tcd->tile;
    grk_tcd_tilecomp_t * l_tile_comp = p_tcd->tile->comps;
    uint64_t samples = (uint64_t)(l_tile_comp->x1 - l_tile_comp->x0) * (l_tile_comp->y1 - l_tile_comp->y0);
    uint32_t i;
    uint8_t ** l_data = nullptr;
    grk_tcp_t * l_tcp = p_tcd->tcp;

    if(!p_tcd->tcp->mct) {
        return true;
    }

    if (p_tcd->tcp->mct == 2) {
        if (! p_tcd->tcp->m_mct_coding_matrix) {
            return true;
        }

        l_data = (uint8_t **) grk_malloc(l_tile->numcomps*sizeof(uint8_t*));
        if (! l_data) {
            return false;
        }

        for (i=0; i<l_tile->numcomps; ++i) {
            l_data[i] = (uint8_t*)grk_tile_buf_get_ptr(l_tile_comp->buf, 0, 0, 0, 0);
            ++l_tile_comp;
        }

        if (! grk_mct_encode_custom(/* MCT data */
                    (uint8_t*) p_tcd->tcp->m_mct_coding_matrix,
                    /* size of components */
                    samples,
                    /* components */
                    l_data,
                    /* nb of components (i.e. size of pData) */
                    l_tile->numcomps,
                    /* tells if the data is signed */
                    p_tcd->image->comps->sgnd) ) {
            grk_free(l_data);
            return false;
        }

        grk_free(l_data);
    } else if (l_tcp->tccps->qmfbid == 0) {
        grk_mct_encode_real(grk_tile_buf_get_ptr(l_tile->comps[0].buf, 0, 0, 0, 0),
                            grk_tile_buf_get_ptr(l_tile->comps[1].buf, 0, 0, 0, 0),
                            grk_tile_buf_get_ptr(l_tile->comps[2].buf, 0, 0, 0, 0),
                            samples);
    } else {
        grk_mct_encode(grk_tile_buf_get_ptr(l_tile->comps[0].buf, 0, 0, 0, 0),
                       grk_tile_buf_get_ptr(l_tile->comps[1].buf, 0, 0, 0, 0),
                       grk_tile_buf_get_ptr(l_tile->comps[2].buf, 0, 0, 0, 0),
                       samples);
    }

    return true;
}


bool grk_tcd_dwt_encode ( grk_tcd_t *p_tcd )
{
    grk_tcd_tile_t * l_tile = p_tcd->tile;
    int64_t compno=0;
    bool rc = true;
#ifdef _OPENMP
    #pragma omp parallel default(none) private(compno) shared(p_tcd, l_tile, rc)
    {
        #pragma omp for
#endif
        for (compno = 0; compno < (int64_t)l_tile->numcomps; ++compno) {
            grk_tcd_tilecomp_t * tile_comp = p_tcd->tile->comps + compno;
            opj_tccp_t * l_tccp = p_tcd->tcp->tccps + compno;
            if (l_tccp->qmfbid == 1) {
                if (! grk_dwt_encode_53(tile_comp)) {
                    rc = false;
                    continue;
                }
            } else if (l_tccp->qmfbid == 0) {
                if (! grk_dwt_encode_97(tile_comp)) {
                    rc = false;
                    continue;
                }
            }
        }
#ifdef _OPENMP
    }
#endif

    return rc;
}

static bool grk_tcd_t1_encode ( grk_tcd_t *p_tcd )
{
    const double * l_mct_norms;
    uint32_t l_mct_numcomps = 0U;
    grk_tcp_t * l_tcp = p_tcd->tcp;

    if (l_tcp->mct == 1) {
        l_mct_numcomps = 3U;
        /* irreversible encoding */
        if (l_tcp->tccps->qmfbid == 0) {
            l_mct_norms = grk_mct_get_mct_norms_real();
        } else {
            l_mct_norms = grk_mct_get_mct_norms();
        }
    } else {
        l_mct_numcomps = p_tcd->image->numcomps;
        l_mct_norms = (const double *) (l_tcp->mct_norms);
    }

    return grk_t1_encode_cblks(p_tcd->tile,
								l_tcp,
								l_mct_norms,
								l_mct_numcomps,
								p_tcd->numThreads);
}

static bool grk_tcd_t2_encode (grk_tcd_t *p_tcd,
                               uint8_t * p_dest_data,
                               uint64_t * p_data_written,
                               uint64_t p_max_dest_size,
                               opj_codestream_info_t *p_cstr_info,
								grk_event_mgr_t * p_manager)
{
    grk_t2_t * l_t2;

    l_t2 = grk_t2_create(p_tcd->image, p_tcd->cp);
    if (l_t2 == nullptr) {
        return false;
    }

    if (! grk_t2_encode_packets(
                l_t2,
                p_tcd->tcd_tileno,
                p_tcd->tile,
                p_tcd->tcp->numlayers,
                p_dest_data,
                p_data_written,
                p_max_dest_size,
                p_cstr_info,
                p_tcd->tp_num,
                p_tcd->tp_pos,
                p_tcd->cur_pino,
				p_manager)) {
        grk_t2_destroy(l_t2);
        return false;
    }

    grk_t2_destroy(l_t2);

    /*---------------CLEAN-------------------*/
    return true;
}


static bool grk_tcd_rate_allocate_encode(  grk_tcd_t *p_tcd,
											uint64_t p_max_dest_size,
											opj_codestream_info_t *p_cstr_info )
{
    opj_cp_t * l_cp = p_tcd->cp;
    uint64_t l_nb_written = 0;

    if (p_cstr_info)  {
        p_cstr_info->index_write = 0;
    }
	
    if (l_cp->m_specific_param.m_enc.m_disto_alloc|| l_cp->m_specific_param.m_enc.m_fixed_quality)  {
        // rate control by rate/distortion or fixed quality 
		switch (l_cp->m_specific_param.m_enc.rateControlAlgorithm) {
		case 0:
			if (!grk_tcd_pcrd_bisect_simple(p_tcd, &l_nb_written, p_max_dest_size)) {
				return false;
			}
			break;
		case 1:
			if (!grk_tcd_pcrd_bisect_feasible(p_tcd, &l_nb_written, p_max_dest_size)) {
				return false;
			}
			break;
		default:
			if (!grk_tcd_pcrd_bisect_feasible(p_tcd, &l_nb_written, p_max_dest_size)) {
				return false;
			}
			break;

		}

    } 

    return true;
}


bool grk_tcd_copy_tile_data (       grk_tcd_t *p_tcd,
                                    uint8_t * p_src,
                                    uint64_t p_src_length )
{
    uint64_t i,j;
    opj_image_comp_t * l_img_comp = nullptr;
    grk_tcd_tilecomp_t * l_tilec = nullptr;
    uint32_t l_size_comp, l_remaining;
    uint64_t l_nb_elem;
	uint64_t l_data_size = grk_tcd_get_encoded_tile_size(p_tcd);
    if (l_data_size != p_src_length) {
        return false;
    }

    l_tilec = p_tcd->tile->comps;
    l_img_comp = p_tcd->image->comps;
    for (i=0; i<p_tcd->image->numcomps; ++i) {
        l_size_comp = l_img_comp->prec >> 3; /*(/ 8)*/
        l_remaining = l_img_comp->prec & 7;  /* (%8) */
        l_nb_elem = (uint64_t)(l_tilec->x1 - l_tilec->x0) * (l_tilec->y1 - l_tilec->y0);

        if (l_remaining) {
            ++l_size_comp;
        }

        if (l_size_comp == 3) {
            l_size_comp = 4;
        }

        switch (l_size_comp) {
        case 1: {
            char * l_src_ptr = (char *) p_src;
            int32_t * l_dest_ptr = l_tilec->buf->data;

            if (l_img_comp->sgnd) {
                for (j=0; j<l_nb_elem; ++j) {
                    *(l_dest_ptr++) = (int32_t) (*(l_src_ptr++));
                }
            } else {
                for (j=0; j<l_nb_elem; ++j) {
                    *(l_dest_ptr++) = (*(l_src_ptr++))&0xff;
                }
            }

            p_src = (uint8_t*) l_src_ptr;
        }
        break;
        case 2: {
            int32_t * l_dest_ptr = l_tilec->buf->data;
            int16_t * l_src_ptr = (int16_t *) p_src;

            if (l_img_comp->sgnd) {
                for (j=0; j<l_nb_elem; ++j) {
                    *(l_dest_ptr++) = (int32_t) (*(l_src_ptr++));
                }
            } else {
                for (j=0; j<l_nb_elem; ++j) {
                    *(l_dest_ptr++) = (*(l_src_ptr++))&0xffff;
                }
            }

            p_src = (uint8_t*) l_src_ptr;
        }
        break;
        case 4: {
            int32_t * l_src_ptr = (int32_t *) p_src;
            int32_t * l_dest_ptr = l_tilec->buf->data;

            for (j=0; j<l_nb_elem; ++j) {
                *(l_dest_ptr++) = (int32_t) (*(l_src_ptr++));
            }

            p_src = (uint8_t*) l_src_ptr;
        }
        break;
        }

        ++l_img_comp;
        ++l_tilec;
    }

    return true;
}


grk_tcd_cblk_enc_t::~grk_tcd_cblk_enc_t() {
	cleanup();
}

void grk_tcd_cblk_enc_t::cleanup() {
	if (data && owns_data)
		grk_free(data);
	if (layers)
		delete[] layers;
	if (passes)
		delete[] passes;
}

bool grk_tcd_cblk_dec_t::alloc() {
	if (!segs) {
		segs = (grk_tcd_seg_t *)grk_calloc(OPJ_J2K_DEFAULT_NB_SEGS, sizeof(grk_tcd_seg_t));
		if (!segs) {
			return false;
		}
		/*fprintf(stderr, "Allocate %d elements of code_block->data\n", OPJ_J2K_DEFAULT_NB_SEGS * sizeof(grk_tcd_seg_t));*/

		numSegmentsAllocated = OPJ_J2K_DEFAULT_NB_SEGS;

		/*fprintf(stderr, "Allocate 8192 elements of code_block->data\n");*/
		/*fprintf(stderr, "numSegmentsAllocated of code_block->data = %d\n", p_code_block->numSegmentsAllocated);*/
	}
	else {
		/* sanitize */
		grk_tcd_seg_t * l_segs = segs;
		uint32_t l_current_max_segs = numSegmentsAllocated;

		/* Note: since seg_buffers simply holds references to another data buffer,
		we do not need to copy it  to the sanitized block  */
		seg_buffers.cleanup();

		memset(this, 0, sizeof(grk_tcd_cblk_dec_t));
		segs = l_segs;
		numSegmentsAllocated = l_current_max_segs;
	}
	return true;
}


grk_tcd_precinct_t::~grk_tcd_precinct_t() {
	if (incltree)
		delete incltree;
	if (imsbtree)
		delete imsbtree;
}

void grk_tcd_precinct_t::initTagTrees(grk_event_mgr_t* manager) {
	
	// if l_current_precinct->cw == 0 or l_current_precinct->ch == 0, then the precinct has no code blocks, therefore 
	// no need for inclusion and msb tag trees
	if (cw > 0 && ch > 0) {
		if (!incltree) {
			try {
				incltree = new TagTree(cw, ch, manager);
			}
			catch (std::exception e) {
				grk_event_msg(manager, EVT_WARNING, "No incltree created.\n");
			}
		}
		else {
			if (!incltree->init(cw, ch, manager)) {
				grk_event_msg(manager, EVT_WARNING, "Failed to re-initialize incltree.\n");
				delete incltree;
				incltree = nullptr;
			}
		}

		if (!imsbtree) {
			try {
				imsbtree = new TagTree(cw, ch, manager);
			}
			catch (std::exception e) {
				grk_event_msg(manager, EVT_WARNING, "No imsbtree created.\n");
			}
		}
		else {
			if (!imsbtree->init(cw, ch, manager)) {
				grk_event_msg(manager, EVT_WARNING, "Failed to re-initialize imsbtree.\n");
				delete imsbtree;
				imsbtree = nullptr;
			}
		}
	}
}
grk_tcd_band_t::~grk_tcd_band_t() {
	if (precincts) {
		delete[] precincts;
	}
}

