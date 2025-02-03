/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/globebrowsing/src/layergroupid.h>

namespace openspace::globebrowsing::layers {

// The order and values of these enums are implicitly used in many places in the code and
// in the shaders, so we need to ensure that the order is not messed with

static_assert(
    std::is_sorted(
        Groups.begin(),
        Groups.end(),
        [](const Group& lhs, const Group& rhs) { return lhs.id < rhs.id; }
    )
);
static_assert(
    std::all_of(
        Groups.begin(),
        Groups.end(),
        [](const Group& gi) { return !gi.identifier.empty(); }
    )
);
static_assert(
    std::all_of(
        Groups.begin(),
        Groups.end(),
        [](const Group& gi) { return !gi.name.empty(); }
    )
);
static_assert(
    std::all_of(
        Groups.begin(),
        Groups.end(),
        [](const Group& gi) {
            const auto it = std::find_if(
                Groups.begin(),
                Groups.end(),
                [gi](const Group& g) { return g.id == gi.id; }
            );
            const std::ptrdiff_t pos = std::distance(Groups.begin(), it);
            return static_cast<int>(pos) == static_cast<int>(gi.id);
        }
    )
);

static_assert(
    std::is_sorted(
        Layers.begin(),
        Layers.end(),
        [](const Layer& lhs, const Layer& rhs) { return lhs.id < rhs.id; }
    )
);
static_assert(
    std::all_of(
        Layers.begin(),
        Layers.end(),
        [](const Layer& li) { return !li.identifier.empty(); }
    )
);
static_assert(
    std::all_of(
        Layers.begin(),
        Layers.end(),
        [](const Layer& li) {
            const auto it = std::find_if(
                Layers.begin(),
                Layers.end(),
                [li](const Layer& l) { return l.id == li.id; }
            );
            const std::ptrdiff_t pos = std::distance(Layers.begin(), it);
            return static_cast<int>(pos) == static_cast<int>(li.id);
        }
    )
);



static_assert(static_cast<int>(Adjustments[0].id) == 0);
static_assert(static_cast<size_t>(Adjustments.back().id) == Adjustments.size() - 1);
static_assert(
    std::is_sorted(
        Adjustments.begin(),
        Adjustments.end(),
        [](const Adjustment& lhs, const Adjustment& rhs) { return lhs.id < rhs.id; }
    )
);
static_assert(
    std::all_of(
        Adjustments.begin(),
        Adjustments.end(),
        [](const Adjustment& ai) { return !ai.identifier.empty(); }
    )
);
static_assert(
    std::all_of(
        Adjustments.begin(),
        Adjustments.end(),
        [](const Adjustment& ai) {
            const auto it = std::find_if(
                Adjustments.begin(),
                Adjustments.end(),
                [ai](const Adjustment& a) { return a.id == ai.id; }
            );
            const std::ptrdiff_t pos = std::distance(Adjustments.begin(), it);
            return static_cast<int>(pos) == static_cast<int>(ai.id);
        }
    )
);



static_assert(static_cast<int>(Blends[0].id) == 0);
static_assert(static_cast<size_t>(Blends.back().id) == Blends.size() - 1);
static_assert(
    std::is_sorted(
        Blends.begin(),
        Blends.end(),
        [](const Blend& lhs, const Blend& rhs) { return lhs.id < rhs.id; }
    )
);
static_assert(
    std::all_of(
        Blends.begin(),
        Blends.end(),
        [](const Blend& bi) { return !bi.identifier.empty(); }
    )
);
static_assert(
    std::all_of(
        Blends.begin(),
        Blends.end(),
        [](const Blend& bi) {
            const auto it = std::find_if(
                Blends.begin(),
                Blends.end(),
                [bi](const Blend& b) { return b.id == bi.id; }
            );
            const std::ptrdiff_t pos = std::distance(Blends.begin(), it);
            return static_cast<int>(pos) == static_cast<int>(bi.id);
        }
    )
);

} // namespace openspace::globebrowsing::layers
