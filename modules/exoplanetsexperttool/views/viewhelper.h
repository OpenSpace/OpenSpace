/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___GUIRENDERHELPER___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___GUIRENDERHELPER___H__

#include <modules/imgui/include/imgui_include.h>
#include <ghoul/glm.h>

namespace openspace::view {

namespace colors {
    constexpr const glm::vec3 DefaultSelected = { 0.2f, 0.8f, 1.f };
    constexpr const glm::vec4 DescriptiveText = { 0.6f, 0.6f, 0.6f, 1.f };
    constexpr const glm::vec4 Error = { 1.f, 0.2f, 0.2f, 1.f };
    constexpr const glm::vec4 DisabledButton = { 0.3f, 0.3f, 0.3f, 0.7f };
} // namespace colors

namespace helper {
    ImVec4 toImVec4(const glm::vec4& v);

    void renderDescriptiveText(const char* text);
    void renderHelpMarker(const char* text);

} // namespace helper

} // namespace openspace::view

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___GUIRENDERHELPER___H__
