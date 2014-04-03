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

// Delegating constructors seem to be necessary; Visual Studio 2013 compiler could not
// deduce template argument for 'U' if 'default' methods are used as default values in
// a single constructor    
        
template <typename T>
NumericalProperty<T>::NumericalProperty(const std::string& identifier,
    const std::string& guiName)
    : NumericalProperty<T>(identifier, guiName,
    PropertyDelegate<NumericalProperty<T>>::template defaultValue<T>(),
    PropertyDelegate<NumericalProperty<T>>::template defaultMinimumValue<T>(),
    PropertyDelegate<NumericalProperty<T>>::template defaultMaximumValue<T>(),
    PropertyDelegate<NumericalProperty<T>>::template defaultStepping<T>())
{}

template <typename T>
NumericalProperty<T>::NumericalProperty(const std::string& identifier,
                                        const std::string& guiName, const T& value)
    : NumericalProperty<T>(identifier, guiName, value,
    PropertyDelegate<NumericalProperty<T>>::template defaultMinimumValue<T>(),
    PropertyDelegate<NumericalProperty<T>>::template defaultMaximumValue<T>(),
    PropertyDelegate<NumericalProperty<T>>::template defaultValue<T>())
{}

template <typename T>
NumericalProperty<T>::NumericalProperty(const std::string& identifier,
                                        const std::string& guiName, const T& value,
                                        const T& minimumValue)
     : NumericalProperty<T>(identifier, guiName, value, minimumValue,
     PropertyDelegate<NumericalProperty<T>>::template defaultMaximumValue<T>(),
     PropertyDelegate<NumericalProperty<T>>::template defaultValue<T>())
 {}

template <typename T>
NumericalProperty<T>::NumericalProperty(const std::string& identifier,
                                        const std::string& guiName, const T& value,
                                        const T& minimumValue, const T& maximumValue)
    : NumericalProperty<T>(identifier, guiName, value, minimumValue, maximumValue,
    PropertyDelegate<NumericalProperty<T>>::template defaultValue<T>())
{}


template <typename T>
NumericalProperty<T>::NumericalProperty(const std::string& identifier,
                                        const std::string& guiName, const T& value,
                                        const T& minimumValue, const T& maximumValue,
                                        const T& stepping)
    : TemplateProperty<T>(identifier, guiName, value)
    , _minimumValue(minimumValue)
    , _maximumValue(maximumValue)
    , _stepping(stepping)
{}

template <typename T>
std::string NumericalProperty<T>::className() const {
    return PropertyDelegate<NumericalProperty<T>>::className();
}

} // namespace properties
} // namespace openspace
