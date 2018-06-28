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

#include <modules/base/dashboard/dashboarditemspacing.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo SpacingInfo = {
        "Spacing",
        "Spacing",
        "This value determines the spacing (in pixels) that this item represents. The "
        "default value is 15."
    };
} // namespace

namespace openspace {

documentation::Documentation DashboardItemSpacing::Documentation() {
    using namespace documentation;
    return {
        "DashboardItem Spacing",
        "base_dashboarditem_spacing",
        {
            {
                "Type",
                new StringEqualVerifier("DashboardItemSpacing"),
                Optional::No
            },
            {
                SpacingInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                SpacingInfo.description
            }
        }
    };
}

DashboardItemSpacing::DashboardItemSpacing(const ghoul::Dictionary& dictionary)
    : DashboardItem(dictionary)
    , _spacing(SpacingInfo, 15.f, 0.f, 2048.f)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "DashboardItemSpacing"
    );

    if (dictionary.hasKey(SpacingInfo.identifier)) {
        _spacing = static_cast<float>(dictionary.value<double>(SpacingInfo.identifier));
    }
    addProperty(_spacing);
}

void DashboardItemSpacing::render(glm::vec2& penPosition) {
    penPosition.y -= _spacing;
}

glm::vec2 DashboardItemSpacing::size() const {
    return { 0.f, _spacing };
}

} // namespace openspace
