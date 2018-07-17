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

#include <iostream>
#include <thread>
#include <string>

#include <nfd.h>
#include <stdio.h> // nfd
#include <stdlib.h> // nfd
#include <experimental/filesystem>

#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <modules/dataloader/operators/loader.h>
#include <modules/dataloader/dataloadermodule.h>
#include <openspace/scene/scene.h>
#include <openspace/util/task.h>
#include <ghoul/logging/logmanager.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <modules/dataloader/helpers.cpp>

#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/dictionary.h>

#include <fstream>
#include <chrono>
#include <ghoul/glm.h>

using Directory = ghoul::filesystem::Directory;
using File = ghoul::filesystem::File;
using RawPath = ghoul::filesystem::Directory::RawPath;
using Recursive = ghoul::filesystem::Directory::Recursive;
using TestResult = openspace::documentation::TestResult;

namespace {
constexpr const char* _loggerCat = "Loader";
constexpr const char* KeyTime = "Time";
const std::string KeyRenderableType = "RenderableTimeVaryingVolume";
const std::string scaleTypeKey = "StaticScale";
const std::string translationTypeKey = "SpiceTranslation";
const std::string volumesGuiPathKey = "/Solar System/Volumes";
const std::string KeyVolumeToRawTask = "KameleonVolumeToRawTask";
constexpr const char* KeyVariable = "Variable";

const std::string KeyStepSize = "StepSize";
const std::string KeyGridType = "GridType";
const std::string KeySecondsAfter = "SecondsAfter";
const std::string KeySecondsBefore = "SecondsBefore";

constexpr const char* KeyTask = "Task";
constexpr const char* KeyItemName = "ItemName";
constexpr const char* KeyTransform = "Transform";
constexpr const char* KeyTranslation = "Translation";
constexpr const char* KeyPosition = "Position";
constexpr const char* KeyType = "Type";
constexpr const char* KeyObserver = "Observer";
constexpr const char* KeyTarget = "Target";
constexpr const char* KeyStaticTranslation = "StaticTranslation";
constexpr const char* KeyStaticScale = "StaticScale";
constexpr const char* KeyScale = "Scale";
constexpr const char* KeyTransferFunctionPath = "TransferFunctionPath";
constexpr const char* KeyTranslationValues = "TranslationValues";

const double DefaultStepSize = 0.02;
const std::string DefaultGridType = "Spherical";
const double DefaultSecondsAfter = 24*60;
const double DefaultSecondsBefore = 24*60;
const float sunRadiusScale = 695508000;

const bool guiHidden = false;
} 

namespace {
  static const openspace::properties::Property::PropertyInfo SelectedFilesInfo = {
      "SelectedFiles",
      "List of selected files and ready to load",
      "This list contains names of selected files in char format"
  };

  static const openspace::properties::Property::PropertyInfo CurrentVolumesConvertedCountInfo = {
      "CurrentVolumesConvertedCount",
      "The amount of volumes currently converted",
      "This number indicates how many of the volumes currently selected "
      "for conversion have been converted."
  };
  
  static const openspace::properties::Property::PropertyInfo CurrentVolumesToConvertCount = {
      "CurrentVolumesToConvertCount",
      "The amount of volumes to be converted",
      "This number indicates how many volumes are currently selected to be converted."
  };

  static const openspace::properties::Property::PropertyInfo UploadDataTriggerInfo = {
      "UploadDataTrigger",
      "Trigger load data files",
      "If this property is triggered it will call the function to load data"
  };

  static const openspace::properties::Property::PropertyInfo VolumeConversionProgressInfo = {
      "VolumeConversionProgress",
      "Progress value for volume conversion",
      "This float value between 0 and 1 corresponds to the progress of volume conversion"
  };
}

namespace openspace::dataloader {

Loader::Loader() 
    : PropertyOwner({ "Loader" })
    , _selectedFilePaths(SelectedFilesInfo)
    , _currentVolumesConvertedCount(CurrentVolumesConvertedCountInfo)
    , _currentVolumesToConvertCount(CurrentVolumesToConvertCount)
    , _uploadDataTrigger(UploadDataTriggerInfo)
    , _volumeConversionProgress(VolumeConversionProgressInfo)
{
    _uploadDataTrigger.onChange([this](){
        selectData();
    });

    addProperty(_selectedFilePaths);
    addProperty(_currentVolumesConvertedCount);
    addProperty(_currentVolumesToConvertCount);
    addProperty(_uploadDataTrigger);
    addProperty(_volumeConversionProgress);

    _volumeConversionThreadCanRun = false;
}

void Loader::selectData() {
    {
    std::thread t([&](){
        nfdpathset_t outPathSet;
        std::vector<std::string> paths;
        nfdresult_t result = NFD_OpenDialogMultiple( "cdf", NULL, &outPathSet );

        size_t count = NFD_PathSet_GetCount(&outPathSet);
        if (count > 0 && result == NFD_OKAY) {
            for (size_t i = 0; i < count; ++i) {
                nfdchar_t *path = NFD_PathSet_GetPath(&outPathSet, i);
                paths.push_back(static_cast<std::string>(path));
            }

            // TODO: reset function
            _volumeConversionProgress = FLT_MIN;
            _currentVolumesConvertedCount = 0;
            _volumeConversionThreadCanRun = false;
            _currentVolumesToConvertCount = count;
            _selectedFilePaths = paths;
            NFD_PathSet_Free(&outPathSet);
        }
        else if ( result == NFD_CANCEL ) {
            LINFO("User pressed cancel."); 
        }
        else {
            std::string error = NFD_GetError();
            LINFO("Error: \n" + error);
        }
    });

    t.detach();
    }
}

void Loader::createInternalDataItemProperties() {
    module()->validateDataDirectory();
    std::vector<std::string> volumeItems = module()->volumeDataItems();

    // LDEBUG("volume items vec size " + std::to_string(volumeItems.size()));

    for (auto item : volumeItems) {
        const std::string dirLeaf = openspace::dataloader::helpers::getDirLeaf(item);
        const openspace::properties::Property::PropertyInfo info = {
            "ItemTrigger_" + dirLeaf,
            dirLeaf,
            ""
        };

        // Initialize trigger property with data item name (are they unique?)
        auto volumeItemTrigger = properties::TriggerProperty(info);

        // Set onChange method to call loadDataItem with the path as argument
        volumeItemTrigger.onChange([this](){
            // loadDataItem(item);
        });

        // addProperty(volumeItemTrigger);
        // LDEBUG("Added property " + dirLeaf);
    }
}

// addDataItemProperty();
// removeDataItemProperties();

// void Loader::createVolumeDataItem(std::string absPath) {}

void Loader::loadDataItem(const std::string& absPathToItem) {
    std::string sourceDir = absPathToItem;
    const std::string dirLeaf = openspace::dataloader::helpers::getDirLeaf(absPathToItem);

    if (scene()->sceneGraphNode(dirLeaf)) {
      LINFO(fmt::format("The sceneGraphNode {} has already been created!", std::string(dirLeaf)));
      goToFirstTimeStep(absPathToItem);
      return;
    }

    // TODO: Variables to let user initialize in GUI
    std::string sunKey = "SUN";
    const float sunRadiusScale = 695508000;
    const float auScale = 149600000000;
    const std::string parent = "SolarSystemBarycenter"; // valid for all volume data?
    
    std::string stateFile = openspace::dataloader::helpers::getFileWithExtensionFromItemFolder(absPathToItem, "state");
    ghoul::Dictionary renderableDictionary = ghoul::lua::loadDictionaryFromFile(stateFile);

    if( !renderableDictionary.hasKey(KeyTransform)) {
      LERROR("Transform was not declared in the .state file!");
    }

    ghoul::Dictionary transformDictionary;
    renderableDictionary.getValue<ghoul::Dictionary>(KeyTransform, transformDictionary);

    std::string tfFilePath = absPathToItem +
        ghoul::filesystem::FileSystem::PathSeparator +
        "transferfunction.txt";

    renderableDictionary.setValue("Type", KeyRenderableType);
    renderableDictionary.setValue("SourceDirectory", sourceDir);
    renderableDictionary.setValue("TransferFunction", tfFilePath);

    std::initializer_list<std::pair<std::string, ghoul::any>> gui = {
        std::make_pair( "Path", volumesGuiPathKey ),
        std::make_pair( "Hidden", guiHidden ),
    };
    ghoul::Dictionary guiDictionary(gui);

    std::initializer_list<std::pair<std::string, ghoul::any>> completeList = {
        std::make_pair( "Identifier", dirLeaf ),
        std::make_pair( "Parent", parent ),
        std::make_pair( "Renderable", renderableDictionary ),
        std::make_pair( "GUI", guiDictionary )
    }; 

    LINFO("Before setting transformDictionary to completeDictionary");
    ghoul::Dictionary completeDictionary(completeList);
    completeDictionary.setValue<ghoul::Dictionary>(KeyTransform, transformDictionary);
    LINFO("Before setting transformDictionary to completeDictionary");
    initializeNode(completeDictionary);

    goToFirstTimeStep(absPathToItem);
}

void Loader::initializeNode(ghoul::Dictionary dict) {
    SceneGraphNode* node = scene()->loadNode(dict);
    scene()->initializeNode(node);
}

void Loader::goToFirstTimeStep(const std::string& absPathToItem) {
    std::string firstDictionaryFilePath = openspace::dataloader::helpers::getFileWithExtensionFromItemFolder(absPathToItem, "dictionary");
    ghoul::Dictionary dict = ghoul::lua::loadDictionaryFromFile(firstDictionaryFilePath);
    std::string firstTimeStep = dict.value<std::string>(KeyTime);
    time().setTime(firstTimeStep);
}

void Loader::processCurrentlySelectedUploadData(const std::string& dictionaryString) {
    LINFO(dictionaryString);
    _currentVolumeConversionDictionary = ghoul::lua::loadDictionaryFromString(dictionaryString);

    TestResult result;
    result.success = false;
    result = openspace::documentation::testSpecification(
        volumeConversionDocumentation(),
        _currentVolumeConversionDictionary
    );

    if (!result.success) {
        throw "Error in specification for volume conversion dictionary: " + dictionaryString;
    } else {
        _volumeConversionThreadCanRun = true;
    }

    {
    std::thread t([&](){
        // ??? won't work multithreaded
        // std::string testPath = "${DATA}" +
        //     ghoul::filesystem::FileSystem::PathSeparator;
        // testPath += ".internal";
        // LINFO(testPath);

        // Hold up thread until dictionary is loaded and valid
        while(!_volumeConversionThreadCanRun) {}


        std::string itemName = _currentVolumeConversionDictionary.value<std::string>(KeyItemName);
        std::string itemPathBase = "${DATA}/.internal/volumes_from_cdf/" + itemName;
        Directory d(itemPathBase, RawPath::No);
        FileSys.createDirectory(d);

        ghoul::Dictionary translationDictionary;
        if (!_currentVolumeConversionDictionary.getValue<ghoul::Dictionary>(KeyTranslation, translationDictionary)) {
            throw ghoul::RuntimeError("Must provide Translation table for volume conversion.");
        }

        // Quick fix to make formatter handle nestled dvec3 type correctly
        glm::dvec3 position = translationDictionary.value<glm::dvec3>(KeyPosition);  
        translationDictionary.setValue<glm::dvec3>(KeyPosition, position);

        // Read and create Scale Dictionary
        float scale = _currentVolumeConversionDictionary.value<float>(KeyScale) * sunRadiusScale;
        ghoul::Dictionary scaleDictionary;
        scaleDictionary.setValue<std::string>(KeyType, KeyStaticScale);
        scaleDictionary.setValue<float>(KeyScale, scale);

        // Create Transform Dictionary 
        ghoul::Dictionary transformDictionary;
        transformDictionary.setValue<ghoul::Dictionary>(KeyTranslation, translationDictionary);
        transformDictionary.setValue<ghoul::Dictionary>(KeyScale, scaleDictionary); 

        /*** create state file ***/
        // Check if file exists
        // If it exists, clear contents? delete and create new?
        // Create file, write dictionary to string contents
        const std::string gridType = _currentVolumeConversionDictionary.value<std::string>(KeyGridType);
        std::initializer_list<std::pair<std::string, ghoul::any>> stateList = {
            std::make_pair( KeyStepSize, DefaultStepSize ),
            std::make_pair( KeyGridType, gridType ),
            std::make_pair( KeySecondsAfter, DefaultSecondsAfter ),
            std::make_pair( KeySecondsBefore, DefaultSecondsBefore ),
            std::make_pair( KeyTransform, transformDictionary )
        };
        ghoul::Dictionary stateDict(stateList);
        ghoul::DictionaryLuaFormatter formatter;
        std::string stateString = formatter.format(stateDict);
        LINFO(stateString);
        std::fstream stateStream(absPath(itemPathBase + "/" + itemName + ".state"), std::ios::out);
        if (!stateStream) {
            LERROR("Could not create state file");
        }
        stateStream << "return " << stateString;
        stateStream.close();

        /*** copy over tf file ***/
        const std::string tfFileToCopyPath = _currentVolumeConversionDictionary.value<std::string>(KeyTransferFunctionPath);
        std::ifstream tfSource(tfFileToCopyPath);
        std::ofstream tfDest(absPath(itemPathBase + "/transferfunction.txt"));
        if (!tfSource) {
            LERROR("Could not open source transferfunction file.");
        }
        if (!tfDest) {
            LERROR("Could not open destination transferfunction file.");
        }

        tfDest << tfSource.rdbuf();
        tfSource.close();
        tfDest.close();

        ghoul::Dictionary taskDictionary;
        if (!_currentVolumeConversionDictionary.getValue<ghoul::Dictionary>(KeyTask, taskDictionary)) {
            throw ghoul::RuntimeError("Must provide Task dictionary for volume conversion.");
        }

        std::mutex m;
        std::function<void(float)> cb = [&](float progress) {
            std::lock_guard<std::mutex> g(m);
            _volumeConversionProgress = progress;
        };

        std::vector<std::string> selectedFiles = _selectedFilePaths;
        unsigned int counter = 0;
        for (const std::string &file : selectedFiles) {
            _volumeConversionProgress = FLT_MIN;
            auto selectedFile = File(file);
            const std::string outputBasePath = d.path() + "/" + selectedFile.filename();

            const std::string rawVolumeOutput = outputBasePath + ".rawvolume";
            const std::string dictionaryOutput = outputBasePath + ".dictionary";

            taskDictionary.setValue("Type", KeyVolumeToRawTask);
            taskDictionary.setValue("Input", file);
            taskDictionary.setValue("RawVolumeOutput", rawVolumeOutput);
            taskDictionary.setValue("DictionaryOutput", dictionaryOutput);

            auto volumeToRawTask = Task::createFromDictionary(taskDictionary);

            volumeToRawTask->perform(cb);
            counter++;
            _currentVolumesConvertedCount = counter;
        }

        LINFO("Created files in " + d.path());

        _volumeConversionThreadCanRun = false;
        loadDataItem(absPath(itemPathBase));
    });

    t.detach();
    }

    // Create state file, transferfunction file in volume item directory
}

// void Loader::createVolumeDataItem(std::string absPath) {}

// ghoul::Dictionary Loader::createTaskDictionaryForOneVolumeItem(std::string inputPath, std::string outputBasePath) {

    // defaults
    // const int dimensions[3] = {100, 100, 128};
    // const int lowerDomainBound[3] = {1, -90, 0};
    // const int upperDomainBound[3] = {15, 90, 360};

    // Set item dirLeaf as namelowerDomainBound
    // const std::string itemName = openspace::dataloader::helpers::getDirLeaf(inputPath);
    // const std::string itemOutputFilePath = outputBasePath + 
    //     ghoul::filesystem::FileSystem::PathSeparator +
    //     itemName;

    // const std::string RawVolumeOutput = itemOutputFilePath + ".rawvolume";
    // const std::string DictionaryOutput = itemOutputFilePath + ".dictionary";

//     std::initializer_list<std::pair<std::string, ghoul::any>> list = {
//         std::make_pair( "Type", "KameleonVolumeToRawTask" ),
//         std::make_pair( "Input", inputPath ),
//         std::make_pair( "Dimensions", _uploadedDataDimensions ),
//         std::make_pair( "Variable", _uploadedDataVariable),
//         std::make_pair( "FactorRSquared", _uploadedDataFactorRSquared ),
//         std::make_pair( "LowerDomainBound", _uploadedDataLowerDomainBounds ),
//         std::make_pair( "UpperDomainBound", _uploadedDataHigherDomainBounds ),
//         std::make_pair( "RawVolumeOutput", RawVolumeOutput ),
//         std::make_pair( "DictionaryOutput", DictionaryOutput)
//     };

//     return ghoul::Dictionary(list);
// }

// ghoul::Dictionary Loader::createVolumeItemDictionary(std::string dataDictionaryPath, std::string dataStatePath) {

// }

documentation::Documentation Loader::volumeConversionDocumentation() {
    using namespace documentation;
    return {
        "LoaderVolumeConversion",
        "",
        {
            {
                KeyItemName,
                new StringAnnotationVerifier("A name for the volume item"),
                Optional::No,
                "The volume item name",
            },
            {
                KeyGridType,
                new StringAnnotationVerifier("What type of grid for ray caster"),
                Optional::No,
                "What type of grid for ray caster",
            },
            {
                KeyTranslation,
                new TableAnnotationVerifier("A table with values for translation"),
                Optional::No,
                "The table with values for translation transform",
            },
            {
                KeyTask,
                new TableAnnotationVerifier("A table with values for volume conversion task"),
                Optional::No,
                "The table with values for volume conversion task",
            },
            {
                KeyTransferFunctionPath,
                new StringAnnotationVerifier("An absolute path to a transfer function file"),
                Optional::No,
                "An absolute path to a transfer function file to copy to the item directory"
            }
        }
    };
}

} // namespace openspace::dataloader

