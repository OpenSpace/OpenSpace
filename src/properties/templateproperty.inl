/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014                                                                    *
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

namespace openspace {
namespace properties {

template <typename T>
TemplateProperty<T>::TemplateProperty(const std::string& identifier,
                                      const std::string& guiName)
    : TemplateProperty<T>(identifier, guiName,
    PropertyDelegate<TemplateProperty<T>>::defaultValue<T>())
{}


template <typename T>
TemplateProperty<T>::TemplateProperty(const std::string& identifier,
                                      const std::string& guiName, const T& value)
    : Property(identifier, guiName)
    , _value(value)
{}

template <typename T>
std::string TemplateProperty<T>::className() const {
    return PropertyDelegate<TemplateProperty<T>>::className();
}

template <typename T>
TemplateProperty<T>::operator T() {
    return _value;
}

template <typename T>
TemplateProperty<T>& TemplateProperty<T>::operator=(T val) {
    _value = val;
    return *this;
}

} // namespace properties
} // namespace openspace
