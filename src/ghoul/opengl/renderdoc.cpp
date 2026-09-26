/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include <ghoul/opengl/renderdoc.h>

#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>

#ifdef WIN32
#include <Windows.h>
#else // ^^^^ WIN32 | !WIN32 vvvv
#include <dlfcn.h>
#endif // WIN32

namespace ghoul::opengl {

void loadRenderDoc() {
    pRENDERDOC_GetAPI getApi = nullptr;
#ifdef WIN32
    if (HMODULE mod = GetModuleHandleA("renderdoc.dll");  mod) {
        getApi = reinterpret_cast<pRENDERDOC_GetAPI>(
            GetProcAddress(mod, "RENDERDOC_GetAPI")
        );
    }
#else // ^^^^ WIN32 ||| !WIN32 vvvv
    if (void* mod = dlopen("librenderdoc.so", RTLD_NOW | RTLD_NOLOAD);  mod) {
        getApi = reinterpret_cast<pRENDERDOC_GetAPI>(dlsym(mod, "RENDERDOC_GetAPI"));
    }
#endif // WIN32

    if (!getApi) {
        return;
    }

    const int ret = getApi(
        eRENDERDOC_API_Version_1_6_0,
        reinterpret_cast<void**>(&renderdocApi)
    );
    if (!ret) {
        LERRORC("RenderDoc", "Error loading API");
        return;
    }

    int major = 0;
    int minor = 0;
    int patch = 0;
    renderdocApi->GetAPIVersion(&major, &minor, &patch);
    LINFOC("RenderDoc", std::format("Loaded API {}.{}.{}", major, minor, patch));
}

} // namespace ghoul::opengl
