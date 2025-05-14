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

#include <openspace/util/tstring.h>

#include <openspace/engine/globals.h>
#include <openspace/util/memorymanager.h>

namespace openspace {

tstring temporaryString(const std::string& str) {
    void* ptr = global::memoryManager->TemporaryMemory.do_allocate(str.size(), 8);
    std::strcpy(reinterpret_cast<char*>(ptr), str.data());
    return tstring(reinterpret_cast<char*>(ptr), str.size());
}

tstring temporaryString(std::string_view str) {
    void* ptr = global::memoryManager->TemporaryMemory.do_allocate(str.size(), 8);
    std::strcpy(reinterpret_cast<char*>(ptr), str.data());
    return tstring(reinterpret_cast<char*>(ptr), str.size());
}

tstring temporaryString(const char str[]) {
    const size_t size = strlen(str);
    void* ptr = global::memoryManager->TemporaryMemory.do_allocate(size, 8);
    std::strcpy(reinterpret_cast<char*>(ptr), str);
    return tstring(reinterpret_cast<char*>(ptr), size);
}

} // namespace openspace
