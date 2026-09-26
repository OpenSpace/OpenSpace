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

#include <ghoul/format.h>
#include <ghoul/misc/assert.h>
#include <ghoul/logging/logmanager.h>
#include <utility>

namespace ghoul {

template <typename T>
ObjectManager<T>::ObjectManager(std::string name)
    : _loggerCat(std::move(name))
{}

template <typename T>
ObjectManager<T>::~ObjectManager() {
    ghoul_assert(_objects.empty(), "Objects were left at the end of the program");

    // This loop is just to check *which* programs were left. If everything goes fine, the
    // next loop should iterate 0 times
    for (const std::pair<const std::string, Info>& p [[maybe_unused]] : _objects) {
        ghoul_assert(
            p.second.refCount == 0,
            "Ref count for Object '" + p.first + "' was not 0"
        );
    }
}

template <typename T>
void ObjectManager<T>::releaseAll(Warnings emitWarnings) {
    // If we are not interested in the warnings, we can just clear the map and let the
    // destructors do their job right away
    if (!emitWarnings) {
        _objects.clear();
        return;
    }

    if (!_objects.empty()) {
        LWARNING(
            "Remaining Objects detected. There was probably some error during "
            "deinitialization that caused this"
        );
    }
    for (std::pair<const std::string, Info>& p : _objects) {
        LWARNINGC(
            p.first,
            std::format("Remaining reference counter: {}", p.second.refCount)
        );
        // We have to destroy the ProgramObject now; otherwise it will destroyed when this
        // ProgramObjectManager leaves scope, at which point there might not be a valid
        // OpenGL state left
        p.second.object = nullptr;
    }
}

template <typename T>
T* ObjectManager<T>::request(const std::string& name,
                             const CreationCallback& creationFunction)
{
    auto it = _objects.find(name);
    if (it == _objects.end()) {
        // If we couldn't find the name, we'll have to create the object and initialize
        // the reference counter to 1
        LDEBUGC(name, "Creating object");
        Info info {
            .object = creationFunction(),
            .refCount = 1
        };
        T* p = info.object.get();
        _objects[name] = std::move(info);
        return p;
    }
    else {
        // If we found the name, we increase the reference counter and return the pointer
        (it->second.refCount)++;
        return it->second.object.get();
    }
}

template <typename T>
void ObjectManager<T>::release(const std::string& name,
                               const DestructionCallback& destructionFunction)
{
    auto it = _objects.find(name);
    ghoul_assert(it != _objects.end(), "Could not find object '" + name + "'");
    ghoul_assert(it->second.refCount >= 0, "Ref count cannot be negative");

    --(it->second.refCount);
    if (it->second.refCount == 0) {
        // This was the final call, so we will delete the ProgramObject
        destructionFunction(it->second.object.get());
        it->second.object = nullptr;
        _objects.erase(it);
    }
}

template <typename T>
void ObjectManager<T>::release(T* object, const DestructionCallback& destructionFunction)
{
    if (!object) {
        return;
    }

    for (const std::pair<const std::string, Info>& p : _objects) {
        if (p.second.object.get() == object) {
            release(p.first, destructionFunction);
            return;
        }
    }
}

} // namespace ghoul
