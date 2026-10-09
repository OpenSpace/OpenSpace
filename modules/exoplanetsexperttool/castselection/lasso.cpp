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

#include <modules/exoplanetsexperttool/castselection/lasso.h>

#include <algorithm>
#include <cmath>
#include <limits>
#include <utility>

namespace {
    constexpr size_t MaxLassoPoints = 128;

    float cross(const glm::vec2& a, const glm::vec2& b) {
        return a.x * b.y - a.y * b.x;
    }

    float polygonArea(const std::vector<glm::vec2>& points) {
        float area = 0.f;
        for (size_t i = 0; i < points.size(); i++) {
            area += cross(points[i], points[(i + 1) % points.size()]);
        }
        return 0.5f * std::abs(area);
    }

    bool segmentIntersection(const glm::vec2& a, const glm::vec2& b, const glm::vec2& c,
                             const glm::vec2& d, glm::vec2& intersection)
    {
        const glm::vec2 r = b - a;
        const glm::vec2 s = d - c;
        const float denom = cross(r, s);
        if (std::abs(denom) < 1e-6f) {
            return false;
        }
        const float t = cross(c - a, s) / denom;
        const float u = cross(c - a, r) / denom;
        if (t < 0.f || t > 1.f || u < 0.f || u > 1.f) {
            return false;
        }
        intersection = a + t * r;
        return true;
    }

    float segmentDistance(const glm::vec2& p, const glm::vec2& a, const glm::vec2& b) {
        const glm::vec2 ab = b - a;
        const float len2 = glm::dot(ab, ab);
        const float t = len2 > 0.f ?
            std::clamp(glm::dot(p - a, ab) / len2, 0.f, 1.f) :
            0.f;
        return glm::distance(p, a + t * ab);
    }

    // Returns the largest closed loop that starts and ends at a self-intersection, or the
    // input if the polygon does not self-intersect
    std::vector<glm::vec2> largestLoop(const std::vector<glm::vec2>& points) {
        const size_t n = points.size();
        std::vector<glm::vec2> best;
        float bestArea = -1.f;

        for (size_t i = 0; i < n; i++) {
            for (size_t j = i + 2; j < n; j++) {
                if (i == 0 && j == n - 1) {
                    // These segments share the first point
                    continue;
                }
                glm::vec2 x;
                const bool intersects = segmentIntersection(
                    points[i], points[i + 1], points[j], points[(j + 1) % n], x
                );
                if (!intersects) {
                    continue;
                }

                std::vector<glm::vec2> inner = { x };
                inner.insert(inner.end(), points.begin() + i + 1, points.begin() + j + 1);

                std::vector<glm::vec2> outer = { x };
                outer.insert(outer.end(), points.begin() + j + 1, points.end());
                outer.insert(outer.end(), points.begin(), points.begin() + i + 1);

                for (std::vector<glm::vec2>* loop : { &inner, &outer }) {
                    const float area = polygonArea(*loop);
                    if (loop->size() >= 3 && area > bestArea) {
                        bestArea = area;
                        best = std::move(*loop);
                    }
                }
            }
        }

        return bestArea < 0.f ? points : best;
    }
} // namespace

namespace openspace::exoplanets {

Lasso::Lasso(std::vector<glm::vec2> stroke) {
    std::vector<glm::vec2> points;
    points.reserve(stroke.size());
    for (const glm::vec2& p : stroke) {
        if (points.empty() || glm::distance(p, points.back()) > 0.5f) {
            points.push_back(p);
        }
    }

    if (points.size() > MaxLassoPoints) {
        std::vector<glm::vec2> resampled;
        resampled.reserve(MaxLassoPoints);
        for (size_t i = 0; i < MaxLassoPoints; i++) {
            resampled.push_back(points[i * points.size() / MaxLassoPoints]);
        }
        points = std::move(resampled);
    }

    if (points.size() < 3) {
        return;
    }

    points = largestLoop(points);
    if (points.size() < 3 || polygonArea(points) <= 0.f) {
        return;
    }

    _points = std::move(points);
    _min = _points.front();
    _max = _points.front();
    for (const glm::vec2& p : _points) {
        _min = glm::min(_min, p);
        _max = glm::max(_max, p);
    }
    for (size_t i = 0; i < _points.size(); i++) {
        for (size_t j = i + 1; j < _points.size(); j++) {
            _diameter = std::max(_diameter, glm::distance(_points[i], _points[j]));
        }
    }
}

bool Lasso::isValid() const {
    return !_points.empty();
}

const std::vector<glm::vec2>& Lasso::points() const {
    return _points;
}

bool Lasso::contains(const glm::vec2& p) const {
    bool inside = false;
    const size_t n = _points.size();
    for (size_t i = 0, j = n - 1; i < n; j = i++) {
        const glm::vec2& a = _points[i];
        const glm::vec2& b = _points[j];
        if ((a.y > p.y) != (b.y > p.y) &&
            p.x < (b.x - a.x) * (p.y - a.y) / (b.y - a.y) + a.x)
        {
            inside = !inside;
        }
    }
    return inside;
}

float Lasso::signedDistance(const glm::vec2& p) const {
    float distance = std::numeric_limits<float>::max();
    const size_t n = _points.size();
    for (size_t i = 0; i < n; i++) {
        distance = std::min(
            distance,
            segmentDistance(p, _points[i], _points[(i + 1) % n])
        );
    }
    return contains(p) ? distance : -distance;
}

float Lasso::diameter() const {
    return _diameter;
}

glm::vec2 Lasso::boundsMin() const {
    return _min;
}

glm::vec2 Lasso::boundsMax() const {
    return _max;
}

} // namespace openspace::exoplanets
