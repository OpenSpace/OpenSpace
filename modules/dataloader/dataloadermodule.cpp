/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/dataloader/dataloadermodule.h>
#include <modules/dataloader/operators/reader.h>
#include <modules/dataloader/operators/loader.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/openspaceengine.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>

namespace {
    constexpr const char* _loggerCat = "DataLoaderModule";
}

namespace openspace {

DataLoaderModule::DataLoaderModule() : OpenSpaceModule(Name) {}

DataLoaderModule::~DataLoaderModule() {}

void DataLoaderModule::internalInitialize(const ghoul::Dictionary&) {
    OsEng.registerModuleCallback(OpenSpaceEngine::CallbackOption::Initialize, [&] {
        _reader = std::make_unique<openspace::dataloader::Reader>();
        _loader = std::make_unique<openspace::dataloader::Loader>();
        addPropertySubOwner(*_reader);
        addPropertySubOwner(*_loader);
    });
}

void DataLoaderModule::setDataDirectoryRead(bool isRead) {
    LDEBUG("Called a module function");
    _dataDirectoryIsRead = isRead;
}

void DataLoaderModule::validateDataDirectory() {
    if(!_dataDirectoryIsRead) {
        _reader->readVolumeDataItems();
    }
}

std::vector<std::string> DataLoaderModule::volumeDataItems() {
    validateDataDirectory();
    return _volumeDataItems;
}

void DataLoaderModule::setVolumeDataItems(std::vector<std::string> items) {
    _volumeDataItems = items;
}

openspace::dataloader::Reader* DataLoaderModule::reader() {
    return _reader.get();
}

openspace::dataloader::Loader* DataLoaderModule::loader() {
    return _loader.get();
}

} // namespace openspace
