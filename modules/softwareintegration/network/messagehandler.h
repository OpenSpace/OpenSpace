/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_MODULE_SOFTWAREINTEGRATION___MESSAGEHANDLER___H__
#define __OPENSPACE_MODULE_SOFTWAREINTEGRATION___MESSAGEHANDLER___H__

#include <unordered_map>

#include <modules/softwareintegration/network/softwareconnection.h>
#include <modules/softwareintegration/utils/syncablestorage.h>

#include <openspace/properties/propertyowner.h>

namespace openspace::softwareintegration::messagehandler {

struct Callback {
    std::function<void()> function;
    std::vector<softwareintegration::storage::Key> waitForData = {};
    std::string description = "???"; // To help debugging. Maybe remove?
};
using CallbackList = std::vector<Callback>;
using CallbackMap = std::unordered_map<std::string, CallbackList>;

void postSyncCallbacks();

void handleMessage(IncomingMessage& incomingMessage);

template<typename T>
bool handleEnumValue(
    const std::vector<std::byte>& message,
    size_t& offset,
    const simp::DataKey& dataKey,
    const std::string& identifier,
    const std::string& propertyName
);

void addCallback(const std::string& identifier, const Callback& newCallback);

} // namespace openspace::softwareintegration::messagehandler

#include "messagehandler.inl"

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___MESSAGEHANDLER___H__
