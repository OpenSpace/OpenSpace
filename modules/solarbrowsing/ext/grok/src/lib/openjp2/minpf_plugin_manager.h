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


#include "minpf_dynamic_library.h"
#include <map>

#include <stdint.h>

#define MINPF_MAX_PLUGINS 32

typedef struct minpf_plugin_manager {

    minpf_dynamic_library* dynamic_libraries[MINPF_MAX_PLUGINS];
    size_t num_libraries;

    minpf_exit_func exit_functions[MINPF_MAX_PLUGINS];
    size_t num_exit_functions;

    minpf_platform_services platformServices;

    std::map<const char*, minpf_register_params*>* plugins;


} minpf_plugin_manager;


minpf_plugin_manager*  minpf_get_plugin_manager(void);
void                   minpf_cleanup_plugin_manager(void);


int32_t minpf_load_from_dir(const char* dir, minpf_invoke_service_func func);
int32_t minpf_load_from_path(const char* path, minpf_invoke_service_func func);
const char* minpf_get_dynamic_library_extension(void);


