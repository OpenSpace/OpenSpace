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

#ifndef __PROPERTYOWNER_H__
#define __PROPERTYOWNER_H__

#include <openspace/properties/property.h>
#include <map>
#include <string>
#include <vector>

namespace openspace {
namespace properties {

class PropertyOwner {
public:
    virtual ~PropertyOwner();

    virtual const std::string& name() const = 0;
    const std::vector<Property*>& properties() const;
    Property* property(const std::string& id) const;

    void setPropertyGroupName(std::string groupID, std::string name);
    const std::string& propertyGroupName(const std::string& groupID) const;

protected:
    void addProperty(Property* prop);
    void addProperty(Property& prop);

    void removeProperty(Property* prop);
    void removeProperty(Property& prop);

private:
    std::vector<Property*> _properties;
    std::map<std::string, std::string> _groupNames;
};

}  // namespace properties
}  // namespace openspace

#endif // __PROPERTYOWNER_H__
