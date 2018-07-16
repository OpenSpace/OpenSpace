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

#ifndef __OPENSPACE_MODULE_DATALOADER___LOADER___H__
#define __OPENSPACE_MODULE_DATALOADER___LOADER___H__

#include <modules/dataloader/operators/operator.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/stringlistproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/vector/dvec3property.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/uintproperty.h>

#include <openspace/util/taskloader.h>

namespace ghoul {
  class Dictionary;
}

namespace documentation { struct Documentation; }

namespace openspace::dataloader {

using properties::PropertyOwner;

class Loader : public PropertyOwner, public Operator {
  public:
    Loader();

    // Select file data path
    void selectData();

    /**
     * Creates and adds trigger properties for data items in the internal directory
     */
    void createInternalDataItemProperties();

    // Add one data item trigger property
    void addDataItemProperty();

    // Remove the trigger properties 
    void removeDataItemProperties();

    // Load a data item with an abs path to the item under its data type subfolder in data/.internal
    void loadDataItem(const std::string& absPathToItem);

    // void createVolumeDataItem(std::string absPath);

    void processCurrentlySelectedUploadData(const std::string& dictionaryString);

    static documentation::Documentation volumeConversionDocumentation();

  private:
    properties::StringListProperty _selectedFilePaths;
    properties::TriggerProperty _uploadDataTrigger;
    properties::FloatProperty _volumeConversionProgress;

    ghoul::Dictionary _currentVolumeConversionDictionary;
    properties::UIntProperty _currentVolumesConvertedCount;
    properties::UIntProperty _currentVolumesToConvertCount;
    bool _volumeConversionThreadCanRun = false;
  
    TaskLoader taskLoader;
    std::vector<std::unique_ptr<Task>> tasks;

  void initializeNode(ghoul::Dictionary dict);
  void goToFirstTimeStep(const std::string& absPathToItem);

};

}

#endif // __OPENSPACE_MODULE_DATALOADER___LOADER___H__
