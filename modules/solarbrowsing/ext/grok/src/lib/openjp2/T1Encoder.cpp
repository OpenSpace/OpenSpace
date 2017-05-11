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
 */

#include "grk_includes.h"
#include "T1Encoder.h"
#include "Barrier.h"
#include "ThreadPool.h"
#include "testing.h"


T1Encoder::T1Encoder() : tile(NULL), 
						maxCblkW(0),
						maxCblkH(0)
{

}

void T1Encoder::encode(void) {
	auto state = opj_plugin_get_debug_state();
	auto t1 = grk_t1_create(true, 0,0);
	if (!t1) {
		return_code = false;
	}
	encodeBlockInfo* block = NULL;
	while (return_code && encodeQueue.tryPop(block)) {
		uint32_t tileIndex = 0, tileLineAdvance;
		if (!grk_t1_allocate_buffers(
			t1,
			block->cblk->x1 - block->cblk->x0,
			block->cblk->y1 - block->cblk->y0)) {
			delete block;
			return_code = false;
			break;
		}
		auto tilec = tile->comps + block->compno;
		uint32_t tile_width = tilec->x1 - tilec->x0;
		tileLineAdvance = tile_width - t1->w;
		block->tiledp =
			grk_tile_buf_get_ptr(tilec->buf, block->resno, block->bandno, block->x, block->y);
		t1->data = block->tiledp;
		t1->data_stride = tile_width;
		if (block->qmfbid == 1) {
			for (auto j = 0U; j < t1->h; ++j) {
				for (auto i = 0U; i < t1->w; ++i) {
					// pass through otherwise
					if ( !(state & OPJ_PLUGIN_STATE_DEBUG) || (state & OPJ_PLUGIN_STATE_PRE_TR1)) {
						block->tiledp[tileIndex] *= (1<<T1_NMSEDEC_FRACBITS);
					}
					tileIndex++;
				}
				tileIndex += tileLineAdvance;
			}
		}
		else {
			for (auto j = 0U; j < t1->h; ++j) {
				for (auto i = 0U; i < t1->w; ++i) {
					// In lossy mode, we do a direct pass through of the image data in two cases while in debug encode mode:
					// 1. plugin is being used for full T1 encoding, so no need to quantize in OPJ
					// 2. plugin is only being used for pre T1 encoding, and we are applying quantization
					//    in the plugin DWT step
					if (!(state & OPJ_PLUGIN_STATE_DEBUG) ||
						((state & OPJ_PLUGIN_STATE_PRE_TR1) && !(state & OPJ_PLUGIN_STATE_DWT_QUANTIZATION))) {
						block->tiledp[tileIndex] = grk_int_fix_mul_t1(block->tiledp[tileIndex], block->bandconst);
					}
					tileIndex++;
				}
				tileIndex += tileLineAdvance;
			}
		}

		auto dist =  grk_t1_encode_cblk(t1,
										block->cblk,
										block->bandno,
										block->compno,
										tilec->numresolutions - 1 - block->resno,
										block->qmfbid,
										block->stepsize,
										block->cblksty,
										tile->numcomps,
										block->mct_norms,
										block->mct_numcomps);

		delete block;

		std::unique_lock<std::mutex> lk(distortion_mutex);
		tile->distotile += dist;
	}
	grk_t1_destroy(t1);
}
void T1Encoder::encodeOpt(size_t threadId) {
	auto state = opj_plugin_get_debug_state();
	auto t1 = t1OptVec[threadId];
	encodeBlockInfo* block = NULL;
	while (return_code && encodeQueue.tryPop(block)) {

		auto tilec = tile->comps + block->compno;
		grk_t1_opt_init_buffers(t1,
								(block->cblk->x1 - block->cblk->x0),
								(block->cblk->y1 - block->cblk->y0));

		uint32_t tile_width = (tilec->x1 - tilec->x0);
		auto tileLineAdvance = tile_width - t1->w;
		auto tiledp = block->tiledp;

#ifdef DEBUG_LOSSLESS_T1
		block->unencodedData =  new int32_t[t1->w * t1->h];
#endif
		uint32_t tileIndex = 0;
		uint32_t max = 0;
		uint32_t cblk_index = 0;
		if (block->qmfbid == 1) {
			for (auto j = 0U; j < t1->h; ++j) {
				for (auto i = 0U; i < t1->w; ++i) {
					int32_t tmp=0;
					// the next few lines were messing up post-encode comparison
					// between plugin and grok open source
					/*
					// pass through otherwise
					if (!(state & OPJ_PLUGIN_STATE_DEBUG) || (state & OPJ_PLUGIN_STATE_PRE_TR1)) {
						tmp = block->tiledp[tileIndex] *= (1<<T1_NMSEDEC_FRACBITS);
					}
					else
					{
						tmp = block->tiledp[tileIndex];
					}
					*/
#ifdef DEBUG_LOSSLESS_T1
					block->unencodedData[cblk_index] = block->tiledp[tileIndex];
#endif
					tmp = block->tiledp[tileIndex] *= (1 << T1_NMSEDEC_FRACBITS);
					uint32_t mag = (uint32_t)abs(tmp);
					max = grk_max<uint32_t>(max, mag);
					t1->data[cblk_index] = mag | ((uint32_t)(tmp < 0) << T1_DATA_SIGN_BIT_INDEX);
					tileIndex++;
					cblk_index++;
				}
				tileIndex += tileLineAdvance;
			}
		}
		else {
			for (auto j = 0U; j < t1->h; ++j) {
				for (auto i = 0U; i < t1->w; ++i) {
					// In lossy mode, we do a direct pass through of the image data in two cases while in debug encode mode:
					// 1. plugin is being used for full T1 encoding, so no need to quantize in OPJ
					// 2. plugin is only being used for pre T1 encoding, and we are applying quantization
					//    in the plugin DWT step
					int32_t tmp = 0;
					if (!(state & OPJ_PLUGIN_STATE_DEBUG) ||
						((state & OPJ_PLUGIN_STATE_PRE_TR1) && !(state & OPJ_PLUGIN_STATE_DWT_QUANTIZATION))) {
						tmp = grk_int_fix_mul_t1(tiledp[tileIndex], block->bandconst);
					}
					else{
						tmp = tiledp[tileIndex];
					}

					uint32_t mag = (uint32_t)abs(tmp);
					uint32_t sign_mag = mag | ((uint32_t)(tmp < 0) << T1_DATA_SIGN_BIT_INDEX);
					max = grk_max<uint32_t>(max, mag);
					t1->data[cblk_index] = sign_mag;
					
					tileIndex++;
					cblk_index++;
				}
				tileIndex += tileLineAdvance;
			}
		}

		auto dist = grk_t1_opt_encode_cblk(	t1,
											block->cblk,
											block->bandno,
											block->compno,
											tilec->numresolutions - 1 - block->resno,
											block->qmfbid,
											block->stepsize,
											block->cblksty,
											tile->numcomps,
											block->mct_norms,
											block->mct_numcomps,
											max);


#ifdef DEBUG_LOSSLESS_T1
		grk_t1_t* t1Decode = grk_t1_create(false, t1->w, t1->h);

		grk_tcd_cblk_dec_t* cblkDecode = new grk_tcd_cblk_dec_t();
		cblkDecode->data = nullptr;
		cblkDecode->segs = nullptr;
		if (!cblkDecode->alloc()) {
			continue;
		}
		cblkDecode->x0 = block->cblk->x0;
		cblkDecode->x1 = block->cblk->x1;
		cblkDecode->y0 = block->cblk->y0;
		cblkDecode->y1 = block->cblk->y1;
		cblkDecode->numbps = block->cblk->numbps;
		cblkDecode->numSegments = 1;
		memset(cblkDecode->segs, 0, sizeof(grk_tcd_seg_t));
		auto seg = cblkDecode->segs;
		seg->numpasses = block->cblk->num_passes_encoded;
		if (block->resno == 1 &&
			block->bandno ==2 &&
			block->precno == 0 &&
			block->cblkno == 0) {

			seg->numpasses = 15;
		}
		auto rate = seg->numpasses  ? block->cblk->passes[seg->numpasses - 1].rate : 0;
		seg->len = rate;
		seg->dataindex = 0;
		grk_min_buf_vec_push_back(&cblkDecode->seg_buffers, block->cblk->data, (uint16_t)rate);
		//decode
		grk_t1_decode_cblk(t1Decode, cblkDecode, block->bandno, 0, 0);

		//compare
		auto index = 0;
		for (uint32_t j = 0; j < t1->h; ++j) {
			for (uint32_t i = 0; i < t1->w; ++i) {
				auto valBefore = block->unencodedData[index];
				auto valAfter = t1Decode->data[index]/2;
				if (valAfter != valBefore) {
					printf("(%d,%d); expected=%x, actual=%x\n", i, j, valBefore, valAfter);
				}
				index++;
			}
		}

		grk_t1_destroy(t1Decode);
		grk_free(cblkDecode->segs);
		delete cblkDecode;
		delete[] block->unencodedData;
		block->unencodedData = nullptr;
#endif
		delete block;
		std::unique_lock<std::mutex> lk(distortion_mutex);
		tile->distotile += dist;
	}
}

bool T1Encoder::encode(bool do_opt, 
						grk_tcd_tile_t *encodeTile,
						std::vector<encodeBlockInfo*>* blocks, 
						uint32_t encodeMaxCblkW,
						uint32_t encodeMaxCblkH,
						uint32_t numThreads) {
	if (!blocks || blocks->size() == 0)
		return true;
	tile = encodeTile;
	maxCblkW = encodeMaxCblkW;
	maxCblkH = encodeMaxCblkH;
#ifdef DEBUG_LOSSLESS_T1
	numThreads = 1;
#endif

	for (auto i = 0U; i < numThreads; ++i) {
		if (do_opt) {
			auto t1 = grk_t1_opt_create(true);
			if (!t1) {
				for (auto t : t1OptVec) {
					grk_t1_opt_destroy(t);
				}
				return false;
			}
			if (!grk_t1_opt_allocate_buffers(t1,
											maxCblkW,
											maxCblkH)) {
				for (auto t : t1OptVec) {
					grk_t1_opt_destroy(t);
				}
				return false;
			}
			t1OptVec.push_back(t1);
		}
	}
	encodeQueue.push_no_lock(blocks);
	return_code = true;

	Barrier encode_t1_barrier(numThreads);
	Barrier encode_t1_calling_barrier(numThreads + 1);

	auto pool = new ThreadPool(numThreads);
	for (auto threadId = 0U; threadId < numThreads; threadId++) {
		pool->enqueue([this,
						do_opt,
						&encode_t1_barrier,
						&encode_t1_calling_barrier,
						threadId] {

			if (do_opt)
				encodeOpt(threadId);
			else
				encode();
			encode_t1_barrier.arrive_and_wait();
			encode_t1_calling_barrier.arrive_and_wait();
		});
	}

	encode_t1_calling_barrier.arrive_and_wait();
	delete pool;
	
	// clean up remaining blocks
	encodeBlockInfo* block = NULL;
	while (encodeQueue.tryPop(block)) {
		delete block;
	}

	// clean up t1 structs
	for (auto t : t1OptVec) {
		grk_t1_opt_destroy(t);
	}

	for (auto t : t1Vec) {
		grk_t1_destroy(t);
	}
	return return_code;

}
