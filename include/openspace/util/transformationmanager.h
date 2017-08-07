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

#ifndef __OPENSPACE_CORE___TRANSFORMATIONMANAGER___H__
#define __OPENSPACE_CORE___TRANSFORMATIONMANAGER___H__

#include <ghoul/designpattern/singleton.h>
#include <ghoul/glm.h>

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED

#ifdef WIN32
#pragma warning (push)
#pragma warning (disable : 4619) // #pragma warning: there is no warning number '4675'
#endif // WIN32

#include <ccmc/Kameleon.h>

#ifdef WIN32
#pragma warning (pop)
#endif // WIN32
#endif


#include <set>

namespace ccmc {
    class Kameleon;
} // namespace ccmc

namespace openspace {
#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
#endif
class TransformationManager : public ghoul::Singleton<TransformationManager> {
    friend class ghoul::Singleton<TransformationManager>;

public:
    TransformationManager();
    ~TransformationManager();

    glm::dmat3 frameTransformationMatrix(std::string from, std::string to, double ephemerisTime) const;

private:
    glm::dmat3 kameleonTransformationMatrix(std::string from, std::string to, double ephemerisTime) const;

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
    std::shared_ptr<ccmc::Kameleon> _kameleon;
#endif
    std::set<std::string> _kameleonFrames;
    std::set<std::string> _dipoleFrames;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___TRANSFORMATIONMANAGER___H__
