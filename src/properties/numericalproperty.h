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

#ifndef __NUMERICALPROPERTY_H__
#define __NUMERICALPROPERTY_H__

#include "properties/templateproperty.h"

namespace openspace {
namespace properties {

template <typename T>
class NumericalProperty : public TemplateProperty<T> {
public:
    NumericalProperty(const std::string& identifier, const std::string& guiName);

    NumericalProperty(const std::string& identifier, const std::string& guiName,
        const T& value);

    NumericalProperty(const std::string& identifier, const std::string& guiName,
        const T& value, const T& minimumValue);

    NumericalProperty(const std::string& identifier, const std::string& guiName,
        const T& value, const T& minimumValue, const T& maximumValue);

    NumericalProperty(const std::string& identifier, const std::string& guiName,
        const T& value, const T& minimumValue, const T& maximumValue,
        const T& stepping);

    virtual std::string className() const;

    using TemplateProperty<T>::operator=;

protected:
    T _minimumValue;
    T _maximumValue;
    T _stepping;
};

} // namespace properties
} // namespace openspace

// use inside namespace (?)
#define REGISTER_NUMERICALPROPERTY_HEADER(CLASS_NAME, TYPE) \
    typedef NumericalProperty<TYPE> CLASS_NAME; \
    template <> std::string PropertyDelegate<NumericalProperty<TYPE>>::className(); \
    template <> std::string PropertyDelegate<TemplateProperty<TYPE>>::className(); \
    template <> template <> \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultValue<TYPE>(); \
    template <> template <> \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultMinimumValue<TYPE>(); \
    template <> template <> \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultMaximumValue<TYPE>(); \
    template <> template <> \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultStepping<TYPE>();


#define REGISTER_NUMERICALPROPERTY_SOURCE(CLASS_NAME, TYPE, \
    DEFAULT_VALUE, DEFAULT_MIN_VALUE, DEFAULT_MAX_VALUE, DEFAULT_STEPPING) \
    template <> \
    std::string PropertyDelegate<NumericalProperty<TYPE>>::className() { \
        return #CLASS_NAME; \
    } \
    template <> \
    std::string PropertyDelegate<TemplateProperty<TYPE>>::className() { \
        return #CLASS_NAME; \
    } \
    template <> template <> \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultValue<TYPE>() { \
        return DEFAULT_VALUE; \
    } \
    template <> template <> \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultMinimumValue<TYPE>() { \
        return DEFAULT_MIN_VALUE; \
    } \
    template <> template <> \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultMaximumValue<TYPE>() { \
        return DEFAULT_MAX_VALUE; \
    } \
    template <> template <> \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultStepping<TYPE>() { \
        return DEFAULT_STEPPING; \
    }

#include "properties/numericalproperty.inl"

#endif // __NUMERICALPROPERTY_H__
