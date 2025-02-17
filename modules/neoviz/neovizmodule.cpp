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

#include <modules/neoviz/neovizmodule.h>

#include <modules/neoviz/tasks/impactcorridortask.h>
#include <openspace/documentation/documentation.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/misc/templatefactory.h>

namespace openspace {

NEOvizModule::NEOvizModule() : OpenSpaceModule(Name) {}

std::vector<documentation::Documentation> NEOvizModule::documentations() const {
    using namespace neoviz;

    return { ImpactCorridorTask::documentation() };
}

void NEOvizModule::internalInitialize(const ghoul::Dictionary&) {
    using namespace neoviz;

    ghoul::TemplateFactory<Task>* fTask = FactoryManager::ref().factory<Task>();
    ghoul_assert(fTask, "No task factory existed");
    fTask->registerClass<ImpactCorridorTask>("ImpactCorridorTask");
}

} // namespace openspace
