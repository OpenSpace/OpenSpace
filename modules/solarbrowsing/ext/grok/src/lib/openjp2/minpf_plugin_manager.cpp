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

#include "minpf_plugin_manager.h"
#include "minpf_plugin.h"
#include <stdio.h>
#include <cstring>


#ifdef _WIN32
#include "windirent.h"
#else
#include <dirent.h>
#endif /* _WIN32 */

minpf_plugin_manager* managerInstance;


static int32_t minpf_post_load_plugin(const char* pluginPath, minpf_post_load_func initFunc);
static const char *get_filename_ext(const char *filename);
static int32_t minpf_load(const char* path);

static uint32_t minpf_is_valid_plugin(const char * id, const minpf_register_params * params)
{
    if (!id || id[0] == '\0')
        return 0;
    if (!params ||!params->createFunc || !params->destroyFunc)
        return 0;

    return 1;
}


int32_t minpf_register_object(const char * id, const minpf_register_params * params)
{
    minpf_plugin_api_version v;
    minpf_plugin_manager* pluginManager = minpf_get_plugin_manager();
    minpf_register_params* registered_params = NULL;

    if (!id || id[0] == '\0' || !params)
        return -1;

    if (!minpf_is_valid_plugin(id, params))
        return -1;


    v = pluginManager->platformServices.version;
    if (v.major != params->version.major)
        return -1;

    // check if plugin is already registered
	if (pluginManager->plugins->find(id) != pluginManager->plugins->end()) {
		delete pluginManager->plugins->operator[](id);
	}
    registered_params = new minpf_register_params();
    *registered_params = *params;
	pluginManager->plugins->operator[](id) = registered_params;
	return 0;

}

const char* minpf_get_dynamic_library_extension(void) {
#ifdef _WIN32
	return "dll";
#else
#if defined(__APPLE__)
	return "dylib";
#elif defined(__linux)
	return "so";
#endif
#endif /* _WIN32 */
}

void minpf_initialize_plugin_manager(minpf_plugin_manager* manager)
{
    if (!manager)
        return;
    manager->platformServices.version.major = 1;
    manager->platformServices.version.minor = 0;
    manager->platformServices.invokeService = NULL;
    manager->platformServices.registerObject = minpf_register_object;

    manager->plugins  = new std::map<const char*, minpf_register_params*>();

}

minpf_plugin_manager*  minpf_get_plugin_manager(void)
{
    if (!managerInstance) {
		managerInstance = (minpf_plugin_manager*)calloc(1, sizeof(minpf_plugin_manager));
        if (!managerInstance)
            return NULL;
        minpf_initialize_plugin_manager(managerInstance);
    }
    return managerInstance;

}

void   minpf_cleanup_plugin_manager(void)
{
    if (managerInstance) {
        size_t i = 0;

        for (i = 0; i < managerInstance->num_exit_functions; ++i)
			managerInstance->exit_functions[i]();

        for (i = 0; i < managerInstance->num_libraries; ++i) {
			if (managerInstance->dynamic_libraries[i]) {
				minpf_unload_dynamic_library(managerInstance->dynamic_libraries[i]);
			}
        }

		for (auto plug = managerInstance->plugins->begin(); plug != managerInstance->plugins->end(); ++plug) {
			if (plug->second)
				delete plug->second;
		}
		delete managerInstance->plugins;
        free(managerInstance);
    }
	managerInstance = NULL;
}

static int32_t minpf_load(const char* path)
{
    minpf_post_load_func postLoadFunc = NULL;
    minpf_dynamic_library* lib = NULL;
    int32_t res = 0;

    minpf_plugin_manager* mgr = minpf_get_plugin_manager();
    if (!mgr || mgr->num_libraries == MINPF_MAX_PLUGINS) {
        return -1;
    }
    lib = minpf_load_dynamic_library(path, NULL);
    if (!lib) {
        return -1;
    }
	//printf("Plugin %s loaded\n", path);
	postLoadFunc = (minpf_post_load_func)(minpf_get_symbol(lib, "minpf_post_load_plugin"));
    if (!postLoadFunc) {
		minpf_unload_dynamic_library(lib);
        return -1;
    }

	char fullPath[4096];
	if (minpf_get_full_path(path, (void*)postLoadFunc, lib->handle, fullPath, 4096)) {
		//printf("Full library path: %s\n", fullPath);
	}
	else {
		minpf_unload_dynamic_library(lib);
		return -1;
	}


    mgr->dynamic_libraries[mgr->num_libraries++] = lib;
    auto rc =  minpf_post_load_plugin(fullPath, postLoadFunc);
	if (rc)
		fprintf(stderr, "Plugin %s failed to initialize \n", fullPath);
	//else
	//	fprintf(stdout, "Plugin %s initialized \n", fullPath);
	return rc;

}


int32_t minpf_load_from_path(const char* path, minpf_invoke_service_func func)
{
	minpf_plugin_manager* mgr = minpf_get_plugin_manager();

	if (!path || path[0] == '\0') // Check that the path is non-empty.
		return -1;

	mgr->platformServices.invokeService = func;

	return minpf_load(path);
}


int32_t minpf_load_from_dir(const char* directory_path, minpf_invoke_service_func func)
{
    DIR *dir;
    struct dirent* content;
    char libraryPath[MINPF_MAX_PATH_LEN];
    minpf_plugin_manager* mgr = minpf_get_plugin_manager();

    if (!directory_path || directory_path[0] == '\0') // Check that the path is non-empty.
        return -1;

    mgr->platformServices.invokeService = func;

    dir= opendir(directory_path);
    if(!dir) {
        fprintf(stderr,"Unable to open folder %s\n",directory_path);
        return -1;
    }

    int32_t rc = -1;
    while((content=readdir(dir))!=NULL) {

        if(strcmp(".",content->d_name)==0 || strcmp("..",content->d_name)==0 )
            continue;

        //ignore files with incorrect extensions
        if (  strcmp(get_filename_ext(content->d_name), minpf_get_dynamic_library_extension()) != 0)
            continue;
        strcpy(libraryPath, directory_path);
        strcat(libraryPath, MINPF_FILE_SEPARATOR);
        strcat(libraryPath, content->d_name);
        if (minpf_load(libraryPath) != 0)
            continue;
        rc = 0;
    }
    auto dir_rc = closedir(dir);
    return rc || dir_rc;
}


static int32_t minpf_post_load_plugin(const char* pluginPath, minpf_post_load_func postLoadFunc)
{
    minpf_plugin_manager* mgr = minpf_get_plugin_manager();

    minpf_exit_func exitFunc = postLoadFunc(pluginPath, &mgr->platformServices);
	if (!exitFunc)
		return -1;

    mgr->exit_functions[mgr->num_exit_functions++] = exitFunc;
    return 0;
}


static const char *get_filename_ext(const char *filename)
{
    const char *dot = strrchr(filename, '.');
    if(!dot || dot == filename)
        return "";
    return dot + 1;
}