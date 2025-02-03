/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

namespace {

/**
 * Synchronizes the http resource identified by the name passed as the first parameter and
 * the version provided as the second parameter. The application will hang while the data
 * is being downloaded.
 */
[[codegen::luawrap]] bool syncResource(std::string identifier, int version) {
    using namespace openspace;

    ghoul::Dictionary dict;
    dict.setValue("Type", std::string("HttpSynchronization"));
    dict.setValue("Identifier", identifier);
    dict.setValue("Version", version);

    std::unique_ptr<ResourceSynchronization> sync =
        ResourceSynchronization::createFromDictionary(dict);

    sync->start();

    while (sync->isSyncing()) {
        std::this_thread::sleep_for(std::chrono::milliseconds(20));
    }

    bool isResolved = sync->isResolved();
    return isResolved;
}

/**
 * Unsynchronizes the http resources identified by the name passed as the first parameter
 * by removing all data that was downloaded as part of the original synchronization. If
 * the second parameter is provided, is it the version of the resources that is
 * unsynchronized, if the parameter is not provided, all versions for the specified http
 * resource are removed.
 */
[[codegen::luawrap]] void unsyncResource(std::string identifier,
                                         std::optional<int> version)
{
    using namespace openspace;

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
}

#include "syncmodule_lua_codegen.cpp"

} // namespace
