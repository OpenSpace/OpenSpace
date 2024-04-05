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

#ifndef __OPENSPACE_CORE___NAVIGATIONSTATE___H__
#define __OPENSPACE_CORE___NAVIGATIONSTATE___H__

#include <openspace/documentation/documentation.h>
#include <openspace/json.h>
#include <optional>

namespace openspace {
    struct CameraPose;
} // namespace openspace

namespace openspace::interaction {

struct NavigationState {
    NavigationState() = default;
    explicit NavigationState(const ghoul::Dictionary& dictionary);
    explicit NavigationState(const nlohmann::json& json);
    NavigationState(std::string anchor, std::string aim, std::string referenceFrame,
        glm::dvec3 position, std::optional<glm::dvec3> up = std::nullopt,
        double yaw = 0.0, double pitch = 0.0,
        std::optional<double> timestamp = std::nullopt);

    CameraPose cameraPose() const;
    ghoul::Dictionary dictionary() const;
    nlohmann::json toJson() const;
    static documentation::Documentation Documentation();

    std::string anchor;
    std::string aim;
    std::string referenceFrame;
    glm::dvec3 position = glm::dvec3(0.0);
    std::optional<glm::dvec3> up;
    double yaw = 0.0;
    double pitch = 0.0;

    std::optional<double> timestamp;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___NAVIGATIONSTATE___H__
