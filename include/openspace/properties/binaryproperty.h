/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_CORE___BINARYPROPERTY___H__
#define __OPENSPACE_CORE___BINARYPROPERTY___H__

#include <ext/json/json.hpp>
#include <openspace/properties/templateproperty.h>


#include <algorithm>
#include <cctype>
#include <cassert>
#include <cstring>

namespace openspace::properties {
    using json = nlohmann::json;

    class VectorProperty {
    public:
        VectorProperty() {
        }
        VectorProperty(float val) {
            data.push_back(val);
        }

        VectorProperty(std::vector<float> val) {
            data = val;
        }

        VectorProperty(const VectorProperty& vec) : VectorProperty(vec.getValues()) { }

        VectorProperty(const VectorProperty&& vec) { 
            data = std::move(vec.data);
        }

        VectorProperty& operator=(const VectorProperty& other)
        {
            data = other.data;
            return *this;
        }
        VectorProperty& operator=(VectorProperty&& other)
        {
            data = std::move(other.data);
            return *this;
        }

        void setValues(std::vector<float> values) {
            data = values;
        }

        std::vector<float> getValues() const {
            return data;
        }

        bool operator!=(const VectorProperty& vec) {
            if (data != vec.data)
                return true;
            return false;
        }

        std::string getString() {
            json j(data);
            return j.dump();
        }

    private:
        std::vector<float> data;
    };

    REGISTER_TEMPLATEPROPERTY_HEADER(BinaryProperty, VectorProperty);

} // namespace openspace::properties

#endif // __OPENSPACE_CORE___BINARYPROPERTY___H__
