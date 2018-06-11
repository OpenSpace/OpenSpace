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

#ifndef __OPENSPACE_MODULE_DATALOADER___DATALOADERMODULE___H__
#define __OPENSPACE_MODULE_DATALOADER___DATALOADERMODULE___H__

#include <openspace/util/openspacemodule.h>
#include <openspace/properties/property.h> // do we need this
#include <openspace/properties/stringlistproperty.h>
#include <openspace/properties/triggerproperty.h>

namespace openspace::dataloader {
    class Reader;
    class Loader;
}

namespace openspace {

enum DataTypes {
    volume
};

/**
 * Reference reader, writer, loader
 * Have functions like getDataItemList that gets list from reader
 */
class DataLoaderModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "DataLoader";

    DataLoaderModule();
    ~DataLoaderModule();

    void internalInitialize(const ghoul::Dictionary&) override;

    void validateDataDirectory();
    void setDataDirectoryRead(bool isRead);

    std::vector<std::string> volumeDataItems();
    void setVolumeDataItems(std::vector<std::string> items);

    dataloader::Reader* reader();
    dataloader::Loader* loader();

private:
    bool _dataDirectoryIsRead = false;

    std::unique_ptr<dataloader::Reader> _reader;
    std::unique_ptr<dataloader::Loader> _loader;

    properties::StringListProperty _volumeDataItems;
    properties::TriggerProperty _showInternalVolumesTrigger;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_DATALOADER___DATALOADERMODULE___H__
