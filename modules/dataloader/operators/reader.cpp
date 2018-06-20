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

#include <modules/dataloader/operators/reader.h>
#include <modules/dataloader/dataloadermodule.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/moduleengine.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>

#include <stdio.h>
#include <stdlib.h>

#include <regex>
#include <iostream>

#ifdef _WIN32
#include <windows.h>
#endif


using Directory = ghoul::filesystem::Directory;
using Recursive = ghoul::filesystem::Directory::Recursive;
using Sort = ghoul::filesystem::Directory::Sort;

namespace {
    constexpr const char* _loggerCat = "Reader";
} 

// namespace {
//     static const openspace::properties::Property::PropertyInfo ReadVolumesTriggerInfo = {
//         "ReadVolumesTrigger",
//         "Trigger load volume data files",
//         "If this property is triggered it will call the function to load volume data"
//     };

//     static const openspace::properties::Property::PropertyInfo ReadFieldlinesTriggerInfo = {
//         "ReadFieldlinesTrigger",
//         "Trigger load fieldline data files",
//         "If this property is triggered it will call the function to load fieldline data"
//     };
// }

namespace openspace::dataloader {

Reader::Reader()
    : PropertyOwner({ "Reader" })
    // , _readVolumesTrigger(ReadVolumesTriggerInfo)
{
    _topDir = ghoul::filesystem::Directory(
      "${DATA}/.internal",
      ghoul::filesystem::Directory::RawPath::No
    );

    // _readVolumesTrigger.onChange([this](){
    //     readVolumeDataItems();
    // });

    // addProperty(_readVolumesTrigger);
}

void Reader::readVolumeDataItems() {
    Directory volumeDir = getVolumeDir();

    std::vector volumeItems = volumeDir.readDirectories(
      Recursive::No,
      Sort::Yes
    );

    module()->setVolumeDataItems(volumeItems);
    module()->setDataDirectoryRead(true);

    // for (auto el : volumeItems) {
    //     LINFO("A dir: " + el);
    // }

    // Take out leaves of uri:s
    // std::regex dirLeaf_regex("([^/]+)/?$");
    // std::smatch dirLeaf_match;
    // std::vector<std::string> itemDirLeaves;

    // // Add each directory uri leaf to list
    // for (const std::string dir : itemDirectories) {
    //     if (std::regex_search(dir, dirLeaf_match, dirLeaf_regex)) {
    //         itemDirLeaves.push_back(dirLeaf_match[0].str());
    //     } else {
    //     }

    // }

    // Store a reference somehow if necessary 
}

Directory Reader::getVolumeDir() {
    return Directory(
        _topDir.path() +
        ghoul::filesystem::FileSystem::PathSeparator +
        "volumes_from_cdf" 
    );
}

}
