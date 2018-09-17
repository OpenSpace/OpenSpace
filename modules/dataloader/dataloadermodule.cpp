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
#include <openspace/scripting/scriptengine.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>

#include "dataloadermodule_lua.inl"

namespace {
    constexpr const char* _loggerCat = "DataLoaderModule";
}

namespace {
    static const openspace::properties::Property::PropertyInfo ShowInternalVolumesInfo = {
        "ShowInternalVolumesTrigger",
        "Trigger load volume data files",
        "If this property is triggered it will call the function to load and show volume data"
    };

    static const openspace::properties::Property::PropertyInfo VolumesInfo = {
        "Volumes",
        "List of volume items stored internally and ready to load",
        "This list contains names of volume data files converted from the CDF format"
    };

    static const openspace::properties::Property::PropertyInfo FieldlinesInfo = {
        "Fieldlines",
        "List of fieldline items stored internally and ready to load",
        "This list contains names of fieldline data files converted from the CDF format"
    };

    static const openspace::properties::Property::PropertyInfo TransferFunctionPresetsJSONInfo = {
        "TransferFunctionPresetsJSON",
        "JSON formatted string of a list of paths to transferfunction files and corresponding screenshot image paths",
        ""
    };

    static const openspace::properties::Property::PropertyInfo ReadTransferFunctionPresetsInfo = {
        "ReadTransferFunctionPresets",
        "Trigger a read of the directory of transfer function presets and their screenshot image links",
        ""
    };
}

namespace openspace {

DataLoaderModule::DataLoaderModule() 
    : OpenSpaceModule(Name)
    , _showInternalVolumesTrigger(ShowInternalVolumesInfo)
    , _volumeDataItems(VolumesInfo)
    , _transferFunctionPresetsJSON(TransferFunctionPresetsJSONInfo)
    , _readTransferFunctionPresets(ReadTransferFunctionPresetsInfo)
{
    _showInternalVolumesTrigger.onChange([this](){
        // showInternalDataType(DataTypes.volume);
        validateDataDirectory();
//        _loader->createInternalDataItemProperties();
    });

    _readTransferFunctionPresets.onChange([this](){
        setTransferFunctionPresets();
    });

    addProperty(_volumeDataItems);
    addProperty(_transferFunctionPresetsJSON);
    addProperty(_showInternalVolumesTrigger);
    addProperty(_readTransferFunctionPresets);
}

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
    _dataDirectoryIsRead = isRead;
}

void DataLoaderModule::validateDataDirectory() {
    if(_dataDirectoryIsRead == false) {

        // TODO: generalize
        _reader->readVolumeDataItems();
    }
}

std::vector<std::string> DataLoaderModule::volumeDataItems() {
    validateDataDirectory();
    return _volumeDataItems;
}

void DataLoaderModule::loadDataItem(const std::string& absPathToItem) {
    //_loader->loadDataItem(absPathToItem);
    LERROR("Change to load asset file");
}

void DataLoaderModule::uploadDataItem(const std::string& dictionaryString) {
    _loader->processCurrentlySelectedUploadData(dictionaryString);
}

void DataLoaderModule::setVolumeDataItems(std::vector<std::string> items) {
    _volumeDataItems = items;
}

void DataLoaderModule::setTransferFunctionPresets() {
    const std::string presetsString = _reader->readTransferFunctionPresets();
    _transferFunctionPresetsJSON = presetsString;
}

openspace::dataloader::Reader* DataLoaderModule::reader() {
    return _reader.get();
}

openspace::dataloader::Loader* DataLoaderModule::loader() {
    return _loader.get();
}

scripting::LuaLibrary DataLoaderModule::luaLibrary() const {
    return{
        "dataloader",
        {
            {
                "loadItem",
                &luascriptfunctions::loadItem,
                {},
                "string",
                "Loads a data item into Open Space"
            },
            {
                "uploadItem",
                &luascriptfunctions::uploadItem,
                {},
                "string",
                "Uploads data for an item"
            }
        },
    };
}

} // namespace openspace
