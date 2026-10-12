/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___LASSO___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___LASSO___H__

#include <openspace/glm.h>
#include <vector>

namespace openspace::exoplanets {

/**
 * A closed lasso polygon in window pixel coordinates. An open stroke is closed by
 * connecting its end points and, if it self-intersects, only its largest closed loop is
 * kept.
 */
class Lasso {
public:
    explicit Lasso(std::vector<glm::vec2> stroke);

    bool isValid() const;
    const std::vector<glm::vec2>& points() const;

    bool contains(const glm::vec2& p) const;

    /**
     * Distance from \p p to the lasso outline; positive inside and negative outside.
     */
    float signedDistance(const glm::vec2& p) const;

    float diameter() const;
    glm::vec2 boundsMin() const;
    glm::vec2 boundsMax() const;

private:
    std::vector<glm::vec2> _points;
    glm::vec2 _min = glm::vec2(0.f);
    glm::vec2 _max = glm::vec2(0.f);
    float _diameter = 0.f;
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___LASSO___H__
