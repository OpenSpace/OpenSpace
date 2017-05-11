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

#include <stdint.h>

struct minpf_platform_services;


typedef struct minpf_object_params {
    const char * id;
    const struct minpf_platform_services * platformServices;
} minpf_object_params;

typedef struct minpf_plugin_api_version {
    int32_t major;
    int32_t minor;
} minpf_plugin_api_version;

typedef void * (*minpf_create_func)(minpf_object_params *);
typedef int32_t (*minpf_destroy_func)(void *);


typedef struct minpf_register_params {
    minpf_plugin_api_version version;
    minpf_create_func createFunc;
    minpf_destroy_func destroyFunc;
} minpf_register_params;


typedef int32_t (*minpf_register_func)(const char * nodeType, const minpf_register_params * params);
typedef int32_t (*minpf_invoke_service_func)(const char * serviceName, void * serviceParams);

typedef struct minpf_platform_services {
    minpf_plugin_api_version version;
    minpf_register_func registerObject;
    minpf_invoke_service_func invokeService;
} minpf_platform_services;


typedef int32_t (*minpf_exit_func)();

typedef minpf_exit_func (*minpf_post_load_func)(const char* pluginPath, const minpf_platform_services *);

#ifndef PLUGIN_API
#ifdef WIN32
#define PLUGIN_API __declspec(dllexport)
#else
#define PLUGIN_API
#endif
#endif

extern "C" PLUGIN_API minpf_exit_func minpf_init_plugin(const char* pluginPath, const minpf_platform_services * params);

