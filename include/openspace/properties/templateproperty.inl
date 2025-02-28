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

namespace openspace::properties {

template <typename T>
TemplateProperty<T>::TemplateProperty(Property::PropertyInfo info, T value)
    : Property(std::move(info))
    , _value(std::move(value))
{}

template <typename T>
TemplateProperty<T>::operator T() {
    return _value;
}

template <typename T>
TemplateProperty<T>::operator T() const {
    return _value;
}

template <typename T>
TemplateProperty<T>& TemplateProperty<T>::operator=(T val) {
    setValue(std::move(val));
    return *this;
}

template <typename T>
T TemplateProperty<T>::value() const {
    return _value;
}

template <typename T>
void TemplateProperty<T>::setValue(T val) {
    if (val != _value) {
        _value = std::move(val);
        notifyChangeListeners();
        _isValueDirty = true;
    }
}

template <typename T>
void TemplateProperty<T>::setLuaValue(lua_State* state) {
    T thisValue = toValue(state);
    setValue(std::move(thisValue));
}

template <typename T>
const std::type_info& TemplateProperty<T>::type() const {
    return typeid(T);
}

}  // namespace openspace::properties
