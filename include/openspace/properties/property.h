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

#include <ghoul/misc/dictionary.h>
#include <boost/any.hpp>
#include <functional>
#include <string>

namespace openspace {
namespace properties {

class PropertyOwner;

class Property {
public:
    Property(std::string identifier, std::string guiName);
    virtual ~Property();

    //virtual Property* create() const = 0;
    virtual std::string className() const = 0;

    virtual boost::any get() const;
    virtual void set(boost::any value);
    virtual const std::type_info& type() const;

    virtual void onChange(std::function<void()> callback);

    const std::string& identifier() const;
    const std::string& guiName() const;

    PropertyOwner* owner() const;
    void setPropertyOwner(PropertyOwner* owner);

    void setGroupIdentifier(std::string groupId);
    std::string groupIdentifier() const;

    void setVisible(bool state);
    bool isVisible() const;

    void setReadOnly(bool state);
    bool isReadOnly() const;

    void setViewOption(std::string option, bool value = true);
    bool viewOption(const std::string& option) const;

    struct ViewOptions {
        static const std::string Color;
        static const std::string LightPosition;
        static const std::string PowerScaledScalar;
        static const std::string PowerScaledCoordinate;
    };

    const ghoul::Dictionary& metaData() const;

protected:
    PropertyOwner* _owner;

    std::string _identifier;
    std::string _guiName;
    ghoul::Dictionary _metaData;

    std::vector<std::function<void()>> _onChangeCallbacks;
};

} // namespace properties
} // namespace openspace

#endif // __PROPERTY_H__
