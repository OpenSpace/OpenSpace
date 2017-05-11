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

EncodedTileData::~EncodedTileData() {
	dealloc();
}

void EncodedTileData::dealloc() {
	if (data)
		delete[] data;
	data = nullptr;
	size = 0;
	offset = 0;
}

void EncodedTileData::alloc(uint64_t len) {
	if (!len)
		return;
	if (!data) {
		data = new uint8_t[len];
		size = len;
	}
	else if (len > size) {
		auto temp = new uint8_t[len];
		memcpy(temp, data, size);
		delete[] data;
		data = temp;
		size = len;
	}
}

void EncodedTileData::grow() {
	if (!data)
		return;
	alloc(size+32768);
}

