/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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

#include <openspace/util/task.h>
#include <ghoul/misc/dictionary.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/logging/logmanager.h>

namespace {
    const std::string _loggerCat = "Task";
}

namespace openspace {

Documentation Task::documentation() {
    using namespace openspace::documentation;
    return{
        "Renderable",
        "renderable",
        {
            {
                "Type",
                new StringAnnotationVerifier("A valid Task created by a factory"),
                "This key specifies the type of Task that gets created. It has to be one"
                "of the valid Tasks that are available for creation (see the "
                "FactoryDocumentation for a list of possible Tasks), which depends on "
                "the configration of the application",
                Optional::No
            }
        }
    };
}

std::unique_ptr<Task> Task::createFromDictionary(const ghoul::Dictionary& dictionary) {
    openspace::documentation::testSpecificationAndThrow(Documentation(), dictionary, "Task");
    std::string taskType = dictionary.value<std::string>("Type");
    auto factory = FactoryManager::ref().factory<Task>();
    Task *task = factory->create(taskType, dictionary);

    if (task == nullptr) {
        LERROR("Failed to create a Task object of type '" << taskType << "'");
        return nullptr;
    }

    return std::unique_ptr<Task>(task);
}

}