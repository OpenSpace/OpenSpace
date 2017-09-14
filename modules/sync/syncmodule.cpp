/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/sync/syncmodule.h>

#include <openspace/documentation/documentation.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/util/factorymanager.h>

#include <ghoul/misc/assert.h>

#include <openspace/util/resourcesynchronization.h>

#include <modules/sync/syncs/httpsynchronization.h>
//#include <modules/sync/syncs/resourcesyncrhonization.h>
//#include <modules/sync/syncs/torrentsyncrhonization.h>

namespace openspace {

SyncModule::SyncModule()
    : OpenSpaceModule(Name)
{}

void SyncModule::internalInitialize() {


    auto fSynchronization = FactoryManager::ref().factory<ResourceSynchronization>();
    ghoul_assert(fSynchronization, "Synchronization factory was not created");

    fSynchronization->registerClass<HttpSynchronization>("HttpSyncrhonization");
}

std::vector<documentation::Documentation> SyncModule::documentations() const {
    return {
        HttpSynchronization::Documentation()
        //ResourceSynchronization::Documentation(),
        //TorrentSynchronization::Documentation()
    };
}

} // namespace openspace
