/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/templatefactory.h>

namespace {

    // The base class of all tasks. Specify the Type property to create one of the
    // available task types. This property should be included in the same table object
    // as the properties of the specific task.
    //
    // Tasks can be executed using the separate TaskRunner application. When starting the
    // application, just enter the path to the file describing the task you want to run
    // as input in the command line to initiate the run. All tasks should live in the task
    // folder in the data folder of the OpenSpace installation.
    struct [[codegen::Dictionary(Task)]] Parameters {
        // This key specifies the type of Task that gets created. It has to be one of the
        // valid Tasks that are available for creation (see the FactoryDocumentation for a
        // list of possible Tasks), which depends on the configration of the application
        std::string type [[codegen::annotation("A valid Task created by a factory")]];
    };
#include "task_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation Task::documentation() {
    return codegen::doc<Parameters>("core_task");
}

std::unique_ptr<Task> Task::createFromDictionary(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    ghoul::TemplateFactory<Task>* factory = FactoryManager::ref().factory<Task>();
    Task* task = factory->create(p.type, dictionary);
    return std::unique_ptr<Task>(task);
}

} // namespace openspace
