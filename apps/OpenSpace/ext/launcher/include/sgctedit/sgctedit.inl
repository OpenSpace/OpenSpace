/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

template <typename T>
bool compareOptionalCfgElement(std::optional<T>& first, std::optional<T>& compare) {
    bool matches = false;
    if (first.has_value() && compare.has_value()) {
        if (*first == *compare) {
            matches = true;
        }
    }
    return matches;
}

template <>
bool compareOptionalCfgElement(std::optional<sgct::vec2>& first,
                               std::optional<sgct::vec2>& compare)
{
    bool matches = false;
    if (first.has_value() && compare.has_value()) {
        if ((first->x == compare->x) && (first->y == compare->y)) {
            matches = true;
        }
    }
    return matches;
}

template <typename T>
bool compareCfgElement(T& first, T& compare) {
    bool matches = false;
    if (first == compare) {
        matches = true;
    }
    return matches;
}

template <>
bool compareCfgElement(sgct::config::PlanarProjection::FOV& first,
                       sgct::config::PlanarProjection::FOV& compare)
{
    bool matches = false;
    if ((first.down == compare.down) &&
        (first.left == compare.left) &&
        (first.right == compare.right) &&
        (first.up == compare.up))
    {
        matches = true;
    }
    return matches;
}
