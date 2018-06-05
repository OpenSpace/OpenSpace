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
#include <stdio.h>
#include <stdlib.h>

#include <string>
#include <regex>

#include <iostream>

#ifdef _WIN32
#include <windows.h>
#endif


namespace {
    constexpr const char* _loggerCat = "Reader";
} // namespace

namespace {
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

    static const openspace::properties::Property::PropertyInfo ReadVolumesTriggerInfo = {
        "ReadVolumesTrigger",
        "Trigger load volume data files",
        "If this property is triggered it will call the function to load volume data"
    };

    static const openspace::properties::Property::PropertyInfo ReadFieldlinesTriggerInfo = {
        "ReadFieldlinesTrigger",
        "Trigger load fieldline data files",
        "If this property is triggered it will call the function to load fieldline data"
    };
}

namespace openspace::dataloader {

Reader::Reader()
    : PropertyOwner({ "Reader" })
    , _volumeItems(VolumesInfo) 
    , _readVolumesTrigger(ReadVolumesTriggerInfo) 
    , _loadDataTrigger(LoadDataTriggerInfo)
{
    _topDir = ghoul::filesystem::Directory(
      "${DATA}/.internal",
      ghoul::filesystem::Directory::RawPath::No
    );

    _readVolumesTrigger.onChange([this](){
        readVolumeDataItems();
    });

    addProperty(_volumeItems);
    addProperty(_readVolumesTrigger);
    addProperty(_loadDataTrigger);
}

void Reader::readVolumeDataItems() {
    ghoul::filesystem::Directory volumeDir(
        _topDir.path() +
        ghoul::filesystem::FileSystem::PathSeparator +
        "volumes_from_cdf" 
    );

    _volumeItems = volumeDir.readDirectories(
      ghoul::filesystem::Directory::Recursive::No,
      ghoul::filesystem::Directory::Sort::Yes
    );

    // DataLoader _internalDirDirty = false

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
    //         LWARNING("Looked for match in " + dir + " but found none.");
    //     }

    // }

    // Store a reference somehow if necessary 
}

void Reader::loadData() {

  char filePath = "";
  // Linux
  #ifdef _linux
  system("thunar /home/mberg");

  // Windows 
  #elif _WIN32
  char filename[ MAX_PATH ];

  OPENFILENAME ofn;
    ZeroMemory( &filename, sizeof( filename ) );
    ZeroMemory( &ofn,      sizeof( ofn ) );
    ofn.lStructSize  = sizeof( ofn );
    ofn.hwndOwner    = NULL;  // If you have a window to center over, put its HANDLE here
    ofn.lpstrFilter  = "Text Files\0*.txt\0Any File\0*.*\0";
    ofn.lpstrFile    = filename;
    ofn.nMaxFile     = MAX_PATH;
    ofn.lpstrTitle   = "Select a File, yo!";
    ofn.Flags        = OFN_DONTADDTORECENT | OFN_FILEMUSTEXIST;
  
  if (GetOpenFileNameA( &ofn ))
  {
    std::cout << "You chose the file \"" << filename << "\"\n";
  }
  else
  {
    // All this stuff below is to tell you exactly how you messed up above. 
    // Once you've got that fixed, you can often (not always!) reduce it to a 'user cancelled' assumption.
    switch (CommDlgExtendedError())
    {
      case CDERR_DIALOGFAILURE   : std::cout << "CDERR_DIALOGFAILURE\n";   break;
      case CDERR_FINDRESFAILURE  : std::cout << "CDERR_FINDRESFAILURE\n";  break;
      case CDERR_INITIALIZATION  : std::cout << "CDERR_INITIALIZATION\n";  break;
      case CDERR_LOADRESFAILURE  : std::cout << "CDERR_LOADRESFAILURE\n";  break;
      case CDERR_LOADSTRFAILURE  : std::cout << "CDERR_LOADSTRFAILURE\n";  break;
      case CDERR_LOCKRESFAILURE  : std::cout << "CDERR_LOCKRESFAILURE\n";  break;
      case CDERR_MEMALLOCFAILURE : std::cout << "CDERR_MEMALLOCFAILURE\n"; break;
      case CDERR_MEMLOCKFAILURE  : std::cout << "CDERR_MEMLOCKFAILURE\n";  break;
      case CDERR_NOHINSTANCE     : std::cout << "CDERR_NOHINSTANCE\n";     break;
      case CDERR_NOHOOK          : std::cout << "CDERR_NOHOOK\n";          break;
      case CDERR_NOTEMPLATE      : std::cout << "CDERR_NOTEMPLATE\n";      break;
      case CDERR_STRUCTSIZE      : std::cout << "CDERR_STRUCTSIZE\n";      break;
      case FNERR_BUFFERTOOSMALL  : std::cout << "FNERR_BUFFERTOOSMALL\n";  break;
      case FNERR_INVALIDFILENAME : std::cout << "FNERR_INVALIDFILENAME\n"; break;
      case FNERR_SUBCLASSFAILURE : std::cout << "FNERR_SUBCLASSFAILURE\n"; break;
      default                    : std::cout << "You cancelled.\n";
    }
  }
  // MAC
  #elif __APPLE__
  // Still to do

  #endif

  // return filePath;
}


}