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

#pragma once

#include "BlockingQueue.h"
#include <atomic>
#include <thread>


class T1Encoder
{
public:
	T1Encoder();
	bool encode(bool do_opt, grk_tcd_tile_t *tile,
				std::vector<encodeBlockInfo*>* blocks,
				uint32_t maxCblkW, 
				uint32_t maxCblkH,
				uint32_t numThreads);

	void encode();
	void encodeOpt(size_t threadId);

	std::atomic_bool return_code;

private:
	grk_tcd_tile_t *tile;
	uint32_t maxCblkW;
	uint32_t maxCblkH;

	std::vector<grk_t1_opt*> t1OptVec;
	std::vector<grk_t1*> t1Vec;

	BlockingQueue<encodeBlockInfo*> encodeQueue;
	mutable std::mutex distortion_mutex;

};
