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

#ifndef __PROPERTY_H__
#define __PROPERTY_H__

#include "openspace/properties/propertydelegate.h"

#include <boost/any.hpp>
#include <string>

namespace openspace {
namespace properties {

class Property {
public:
    Property(const std::string& identifier, const std::string& guiName);
    virtual ~Property();

    //virtual Property* create() const = 0;
    virtual std::string className() const = 0;

    virtual boost::any get() const;
    virtual void set(const boost::any& value);
    virtual const std::type_info& type() const;

    const std::string& identifier() const;
    const std::string& guiName() const;

    void setGroupIdentifier(const std::string& groupId);
    const std::string& groupIdentifier() const;

    void setVisible(bool state);
    bool isVisible() const;

    void setReadOnly(bool state);
    bool isReadOnly() const;

protected:
    std::string _identifier;
    std::string _guiName;
    std::string _groupId;

    bool _isVisible;
    bool _isReadOnly;
};

} // namespace properties
} // namespace openspace

#endif // __PROPERTY_H__
