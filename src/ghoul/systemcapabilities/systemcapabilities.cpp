/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#include <ghoul/systemcapabilities/systemcapabilities.h>

#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <algorithm>
#include <typeinfo>
#include <utility>

namespace ghoul::systemcapabilities {

SystemCapabilities SystemCapabilities::_systemCapabilities;
bool SystemCapabilities::_isInitialized = false;

SystemCapabilities::CapabilitiesComponentNotFoundError::
CapabilitiesComponentNotFoundError()
    : ghoul::RuntimeError("SystemCapabilities not found", "SystemCapabilities")
{}

SystemCapabilities& SystemCapabilities::ref() {
    return _systemCapabilities;
}

bool SystemCapabilities::isInitialized() {
    return _isInitialized;
}

void SystemCapabilities::detectCapabilities() {
    clearCapabilities();
    for (const std::unique_ptr<SystemCapabilitiesComponent>& c : _components) {
        c->detectCapabilities();
    }
    _isInitialized = true;
}

void SystemCapabilities::clearCapabilities() {
    for (const std::unique_ptr<SystemCapabilitiesComponent>& c : _components) {
        c->clearCapabilities();
    }
    _isInitialized = false;
}

void SystemCapabilities::logCapabilities(
                                   SystemCapabilitiesComponent::Verbosity verbosity) const
{
    using SCC = SystemCapabilitiesComponent;
    for (const std::unique_ptr<SCC>& c : _components) {
        const std::vector<SCC::CapabilityInformation>& caps = c->capabilities();
        for (const SCC::CapabilityInformation& cap : caps) {
            if (verbosity >= cap.verbosity) {
                LINFOC(
                    std::format("SystemCapabilitiesComponent.{}", c->name()),
                    std::format("{}: {}", cap.description, cap.value)
                );
            }
        }
    }
}

void SystemCapabilities::addComponent(
                                   std::unique_ptr<SystemCapabilitiesComponent> component)
{
    ghoul_assert(component != nullptr, "Component must not be nullptr");

    const auto it = std::find_if(
        _components.cbegin(),
        _components.cend(),
        [&component](const std::unique_ptr<SystemCapabilitiesComponent>& rhs) {
            SystemCapabilitiesComponent* l = component.get();
            SystemCapabilitiesComponent* r = rhs.get();

            return typeid(*l) == typeid(*r);
        }
    );

    ghoul_assert(it == _components.cend(), "Component must not have been added before");
    _components.push_back(std::move(component));
}

} // namespace ghoul::systemcapabilities
