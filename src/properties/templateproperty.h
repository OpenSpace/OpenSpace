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

#ifndef __TEMPLATEPROPERTY_H__
#define __TEMPLATEPROPERTY_H__

#include "properties/property.h"

namespace openspace {
namespace properties {

template <typename T>
class TemplateProperty : public Property {
public:
    TemplateProperty(const std::string& identifier, const std::string& guiName);

    TemplateProperty(const std::string& identifier, const std::string& guiName,
        const T& value);

    virtual std::string className() const;

    operator T();
    TemplateProperty<T>& operator=(T val);

protected:
    T _value;
};

} // namespace properties
} // namespace openspace

// use inside namespace (?)
#define REGISTER_TEMPLATEPROPERTY_HEADER(CLASS_NAME, TYPE) \
    typedef TemplateProperty<TYPE> CLASS_NAME; \
    template <> std::string PropertyDelegate<TemplateProperty<TYPE>>::className(); \
    template <> template <> \
    TYPE PropertyDelegate<TemplateProperty<TYPE>>::defaultValue<TYPE>(); 

#define REGISTER_TEMPLATEPROPERTY_SOURCE(CLASS_NAME, TYPE, DEFAULT_VALUE) \
    template <> \
    std::string PropertyDelegate<TemplateProperty<TYPE>>::className() { \
        return #CLASS_NAME; \
    } \
    template <> template <> \
    TYPE PropertyDelegate<TemplateProperty<TYPE>>::defaultValue<TYPE>() { \
        return DEFAULT_VALUE; \
    }

#include "properties/templateproperty.inl"

#endif // __TEMPLATEPROPERTY_H__
