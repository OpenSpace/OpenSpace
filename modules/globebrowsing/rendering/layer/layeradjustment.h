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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___LAYER_ADJUSTMENT___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___LAYER_ADJUSTMENT___H__

#include <openspace/properties/propertyowner.h>

#include <modules/globebrowsing/rendering/layer/layergroupid.h>

#include <openspace/properties/scalarproperty.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/vectorproperty.h>

namespace openspace {
namespace globebrowsing {

namespace tileprovider {
    class TileProvider;
}

class LayerAdjustment : public properties::PropertyOwner
{
public:
    LayerAdjustment();
    ~LayerAdjustment() = default;

    void setValuesFromDictionary(const ghoul::Dictionary& adjustmentDict);

    layergroupid::AdjustmentTypeID type() const;

    // Properties
    properties::Vec3Property chromaKeyColor;
    properties::FloatProperty chromaKeyTolerance;

    void onChange(std::function<void(void)> callback);

private:
    void addVisibleProperties();
    void removeVisibleProperties();
    
    properties::OptionProperty _typeOption;
    layergroupid::AdjustmentTypeID _type;

    std::function<void(void)> _onChangeCallback;
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___LAYER_ADJUSTMENT___H__
