/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>

namespace openspace::luascriptfunctions {

int syncResource(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::syncResource");
    auto [identifier, version] = ghoul::lua::values<std::string, double>(L);

    ghoul::Dictionary dict;
    dict.setValue("Identifier", identifier);
    dict.setValue("Version", version);

    const SyncModule* module = global::moduleEngine->module<SyncModule>();
    HttpSynchronization sync(
        dict,
        module->synchronizationRoot(),
        module->httpSynchronizationRepositories()
    );

    sync.start();

    while (sync.isSyncing()) {
        std::this_thread::sleep_for(std::chrono::milliseconds(20));
    }

    ghoul::lua::push(L, sync.isResolved());
    return 1;
}

int unsyncResource(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 1, 2 }, "lua::unsyncResource");
    auto [identifier, version] = ghoul::lua::values<std::string, std::optional<int>>(L);

    const SyncModule* module = global::moduleEngine->module<SyncModule>();
    std::filesystem::path sync = absPath(module->synchronizationRoot());
    std::filesystem::path base = sync / "http" / identifier;

    if (!version.has_value()) {
        // If no version was provided, we remove all of the files
        std::filesystem::remove_all(base);
    }
    else {
        const int v = *version;

        std::filesystem::path folder = base / std::to_string(v);
        std::string syncFile = std::to_string(v) + ".ossync";

        std::filesystem::remove_all(folder);
        std::filesystem::remove(base / syncFile);
    }

    return 0;
}

} // namespace openspace::luascriptfunctions
