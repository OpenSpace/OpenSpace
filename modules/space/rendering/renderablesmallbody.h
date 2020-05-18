/****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_MODULE_SPACE___RENDERABLESMALLBODY___H__
#define __OPENSPACE_MODULE_SPACE___RENDERABLESMALLBODY___H__

#include <modules/space/rendering/renderableorbitalkepler.h>
#include <openspace/rendering/renderable.h>

#include <modules/base/rendering/renderabletrail.h>
#include <modules/space/translation/keplertranslation.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/uintproperty.h>
#include <ghoul/glm.h>
#include <ghoul/misc/objectmanager.h>
#include <ghoul/opengl/programobject.h>

namespace openspace {

static double importAngleValue(const std::string& angle);

class RenderableSmallBody : public RenderableOrbitalKepler {
public:
    RenderableSmallBody(const ghoul::Dictionary& dictionary);
    static documentation::Documentation Documentation();

private:
    void readOrbitalParamsFromThisLine(bool firstDataLine, int& fieldCount,
        unsigned int& csvLine, std::ifstream& file);
    void readDataFile(const std::string& filename);
    void initializeFileReading();
    void skipSingleLineInFile(std::ifstream& file);

    std::vector<std::string> _sbNames;

    /// The index array that is potentially used in the draw call. If this is empty, no
    /// element draw call is used.
    std::vector<unsigned int> _indexBufferData;
};

static double importAngleValue(const std::string& angle);
static std::string& formatObjectName(std::string& name);

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___RENDERABLESMALLBODY___H__

