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

#include <typeinfo>

namespace openspace::properties {

template <typename T>
std::string PropertyDelegate<T>::className() {
    static_assert(sizeof(T) == 0,
        "Unimplemented PropertyDelegate::className specialization");
    return "";
}

template <typename T>
template <typename U>
U PropertyDelegate<T>::defaultValue() {
    static_assert(sizeof(T) == 0,
        "Unimplemented PropertyDelegate::defaultValue specialization");
}

template <typename T>
template <typename U>
U PropertyDelegate<T>::defaultMinimumValue() {
    static_assert(sizeof(T) == 0,
        "Unimplemented PropertyDelegate::defaultMinimumValue specialization");
}

template <typename T>
template <typename U>
U PropertyDelegate<T>::defaultMaximumValue() {
    static_assert(sizeof(T) == 0,
        "Unimplemented PropertyDelegate::defaultMaximumValue specialization");
}

template <typename T>
template <typename U>
U PropertyDelegate<T>::defaultSteppingValue() {
    static_assert(sizeof(T) == 0,
        "Unimplemented PropertyDelegate::defaultSteppingValue specialization");
}

template <typename T>
template <typename U>
U PropertyDelegate<T>::fromLuaValue(lua_State*, bool&) {
    static_assert(sizeof(T) == 0,
        "Unimplemented PropertyDelegate::fromLuaValue specialization");
}

template <typename T>
template <typename U>
bool PropertyDelegate<T>::toLuaValue(lua_State*, const U&) {
    static_assert(sizeof(T) == 0,
        "Unimplemented PropertyDelegate::toLuaValue specialization");
    return false;
}

template <typename T>
int PropertyDelegate<T>::typeLua() {
    static_assert(sizeof(T) == 0,
        "Unimplemented PropertyDelegate::luaType specialization");
    return 0;
}

template <typename T>
template <typename U>
bool PropertyDelegate<T>::toString(std::string&, const U&) {
    static_assert(sizeof(T) == 0,
        "Unimplemented PropertyDelegate::toString specialization");
    return false;
}

template <typename T>
template <typename U>
U PropertyDelegate<T>::fromString(const std::string&, bool&) {
    static_assert(sizeof(T) == 0,
        "Unimplemented PropertyDelegate::fromString specialization");
}

} // namespace openspace::properties
