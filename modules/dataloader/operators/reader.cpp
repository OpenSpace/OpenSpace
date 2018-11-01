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
#include <modules/dataloader/helpers.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/moduleengine.h>

#include <ext/json/json.hpp>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/lua/lua_helper.h>

#include <fstream>
#include <iterator>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <regex>

#ifdef _WIN32
#include <windows.h>
#endif

using Directory = ghoul::filesystem::Directory;
using File = ghoul::filesystem::File;
using Recursive = ghoul::filesystem::Directory::Recursive;
using RawPath = ghoul::filesystem::Directory::RawPath;
using Sort = ghoul::filesystem::Directory::Sort;
using json = nlohmann::json;

namespace
{
constexpr const char *_loggerCat = "Reader";

constexpr const char *KeyImage = "Image";
constexpr const char *KeyLabel = "Label";
constexpr const char *KeyMin = "Min";
constexpr const char *KeyMax = "Max";
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

namespace openspace::dataloader
{

Reader::Reader()
    : PropertyOwner({"Reader"})
// , _readVolumesTrigger(ReadVolumesTriggerInfo)
{
    _topDir = ghoul::filesystem::Directory("${DATA}/.internal", RawPath::No);

    // _readVolumesTrigger.onChange([this](){
    //     readVolumeDataItems();
    // });

    // addProperty(_readVolumesTrigger);
}

void Reader::readVolumeDataItems()
{
    Directory volumeDir = getVolumeDir();

    std::vector<std::string> volumeItems = volumeDir.readDirectories(
        Recursive::No,
        Sort::Yes);

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

std::string Reader::readTransferFunctionPresets()
{
    Directory d(_topDir.path() + ghoul::filesystem::FileSystem::PathSeparator + "tf_presets", RawPath::Yes);

    std::vector<std::string> tfFiles = d.readFiles(Recursive::No, Sort::No);
    std::vector<std::pair<std::string, std::string>> tfLinkList;

    // Put last line of transferfunction preset files together with file path in tfLinkList
    // for (auto file : tfFiles)
    // {
    //     std::ifstream in;
    //     in.open(file);
    //     if (in.is_open())
    //     {
    //         std::vector<std::string> lines;
    //         copy(std::istream_iterator<std::string>(in),
    //              std::istream_iterator<std::string>(),
    //              back_inserter(lines));
    //         tfLinkList.push_back(std::make_pair(file, lines.back()));
    //     }
    //     in.close();
    // }

    // auto j = json::array();
    // for (auto pair : tfLinkList)
    // {
    //     j.push_back(json::object({{"path", pair.first}, {"image", pair.second}}));
    // }

    json j;

    unsigned int jsonIdx = 0;
    for (auto file : tfFiles)
    {
        File fileHandle = File(file);
        std::string fileExtension = fileHandle.fileExtension();

        if (fileExtension != "txt")
        {
          continue;
        }

        std::string baseName = fileHandle.fullBaseName();
        std::string dictionaryFile = baseName + ".dictionary";
        ghoul::Dictionary dict;

        try {
          dict = ghoul::lua::loadDictionaryFromFile(dictionaryFile);
        } catch(const std::exception& e) {
          LWARNING("Couldn't find file " + dictionaryFile);
          LWARNING(e.what());
          continue;
        }

        openspace::dataloader::helpers::replaceDoubleBackslashesWithForward(file);

        json tfJson;
        tfJson["path"] = file;
        tfJson["img"] = dict.value<std::string>(KeyImage);
        tfJson["label"] = dict.value<std::string>(KeyLabel);
        tfJson["minValue"] = dict.value<std::string>(KeyMin);
        tfJson["maxValue"] = dict.value<std::string>(KeyMax);

        j[jsonIdx] = tfJson;
        jsonIdx++;
    }

    return j.dump();
}

Directory Reader::getVolumeDir()
{
    return Directory(
        _topDir.path() +
        ghoul::filesystem::FileSystem::PathSeparator +
        "volumes_from_cdf");
}

} // namespace openspace::dataloader
