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

#include "minpf_common.h"

#ifdef WIN32
#include <Windows.h>
#else
#include <dlfcn.h>
#endif

#ifdef _WIN32
typedef HMODULE dynamic_handle_t;
#else
typedef void* dynamic_handle_t;
#endif

struct minpf_dynamic_library {

    char path[MINPF_MAX_PATH_LEN];
	dynamic_handle_t handle;

};

minpf_dynamic_library* minpf_load_dynamic_library(const char* path, char* error);
bool minpf_unload_dynamic_library(minpf_dynamic_library* library);
void* minpf_get_symbol(minpf_dynamic_library* library, const char* symbol);
bool minpf_get_full_path(const char* path,
							void *addr,
							dynamic_handle_t handle,
							char* fullPath,
							size_t fullPathLen);



