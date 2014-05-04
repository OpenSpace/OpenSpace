/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#ifndef __CONSTANTS_H__
#define __CONSTANTS_H__

#include <string>

namespace openspace {
namespace constants {
namespace openspaceengine {
    const std::string pathKey = "Paths";
    const std::string scenePathKey = "Paths.SCENEPATH";
    const std::string sgctConfigKey = "SGCTConfig";
    const std::string sceneConfigurationKey = "Scene";
} // namespace openspaceengine

namespace scenegraph {
    const std::string modulesKey = "Modules";
    const std::string cameraKey = "Camera";
    const std::string focusKey = "Focus";
    const std::string positionKey = "Position";
    const std::string modulePathKey = "ModulePath";
}  // namespace scenegraph

namespace scenegraphnode {
    const std::string nameKey = "Name";
    const std::string parentKey = "Parent";
    const std::string renderableKey = "Renderable";
    const std::string ephemerisKey = "Position";
} // namespace scenegraphnode

}  // namespace constants
}  // namespace openspace


#endif // __CONSTANTS_H__