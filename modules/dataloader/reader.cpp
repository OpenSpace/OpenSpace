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

#include <modules/dataloader/reader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>

namespace {
    constexpr const char* _loggerCat = "Reader";
} // namespace

namespace {
    static const openspace::properties::Property::PropertyInfo VolumesInfo = {
        "Volumes",
        "List of volumes stored internally and ready to load",
        "This list contains names of volume data files converted from the CDF format"
    };

    static const openspace::properties::Property::PropertyInfo ReadVolumeTriggerInfo = {
        "Volume Trigger",
        "Trigger load volume data files",
        "If this property is triggered it will call the function to load volume data"
    };
}

namespace openspace::dataloader {

Reader::Reader()
    : PropertyOwner({ "Reader" })
    , _volumes(VolumesInfo) 
    , _readVolumeTrigger(ReadVolumeTriggerInfo) 
{
    _topDir = ghoul::filesystem::Directory(
      "${DATA}/.internal",
      ghoul::filesystem::Directory::RawPath::No
    );

    _readVolumeTrigger.onChange([this](){
      loadVolumeDataItems();
    });

    addProperty(_volumes);
    addProperty(_readVolumeTrigger);
}

void Reader::readVolumeDataItems() {

    // Go into data/.internal(
    ghoul::filesystem::Directory volumeDir(
        _topDir.path() +
        ghoul::filesystem::FileSystem::PathSeparator +
        "CDF" 
    );

    // Read files
    std::vector<std::string> files = volumeDir.readFiles(
      ghoul::filesystem::Directory::Recursive::No,
      ghoul::filesystem::Directory::Sort::Yes
    );

    // Print vector
    for (auto el : files) {
      LINFO("A file: " + el);
    }

    // For each folder
      // Take first part of file name
      // Add to list
      // Store a reference somehow if necessary 
}

}