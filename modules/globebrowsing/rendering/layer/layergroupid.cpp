/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/globebrowsing/rendering/layer/layergroupid.h>

namespace openspace::globebrowsing::layergroupid {

TypeID getTypeIDFromTypeString(const std::string& typeString) {
    for (int i = 0; i < NUM_LAYER_TYPES; ++i) {
        if (typeString == LAYER_TYPE_NAMES[i]) {
            return static_cast<TypeID>(i);
        }
    }
    return TypeID::Unknown;
}

layergroupid::GroupID getGroupIDFromName(const std::string& layerGroupName) {
    for (int i = 0; i < layergroupid::NUM_LAYER_GROUPS; ++i) {
        if (layerGroupName == layergroupid::LAYER_GROUP_IDENTIFIERS[i]) {
            return static_cast<layergroupid::GroupID>(i);
        }
    }
    return GroupID::Unknown;
}

layergroupid::AdjustmentTypeID getAdjustmentTypeIDFromName(
                                                    const std::string& adjustmentTypeName)
{
    for (int i = 0; i < layergroupid::NUM_ADJUSTMENT_TYPES; ++i) {
        if (adjustmentTypeName == layergroupid::ADJUSTMENT_TYPE_NAMES[i]) {
            return static_cast<layergroupid::AdjustmentTypeID>(i);
        }
    }
    return AdjustmentTypeID::None;
}

layergroupid::BlendModeID getBlendModeIDFromName(const std::string& blendModeName) {
    for (int i = 0; i < layergroupid::NUM_BLEND_MODES; ++i) {
        if (blendModeName == layergroupid::BLEND_MODE_NAMES[i]) {
            return static_cast<layergroupid::BlendModeID>(i);
        }
    }
    return BlendModeID::Normal;
}

} // namespace openspace::globebrowsing::layergroupid
