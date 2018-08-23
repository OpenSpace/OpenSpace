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

#ifndef __OPENSPACE_MODULE_SPACE___SPACECRAFTPROJECTION___H__
#define __OPENSPACE_MODULE_SPACE___SPACECRAFTPROJECTION___H__

#include <openspace/scene/translation.h>

//#include <modules/marsrover/surfaceprojection/mslterrain.h>

#include <openspace/properties/vector/dvec3property.h>
#include <openspace/properties/stringproperty.h>
#include <vector>
#include <ghoul/glm.h>

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
}

namespace openspace {

namespace documentation { struct Documentation; }

class SpacecraftProjection : public Translation {
public:
    SpacecraftProjection(const ghoul::Dictionary& dictionary);

    static constexpr const char* RootNodeIdentifier = "Root";
    static constexpr const char* KeyIdentifier = "Identifier";
    static constexpr const char* KeyParentName = "Parent";
    static constexpr const char* KeyDependencies = "Dependencies";
    static constexpr const char* KeyTag = "Tag";

	void readHeightMapFile();
	
    glm::dvec3 getNewHeight(glm::dvec3 p); 
    glm::dvec3 convertFromRectangularToLatitudinal(glm::dvec3 p);

    glm::dvec3 position(const UpdateData& data) const override;

private:
    properties::StringProperty _target;
    properties::StringProperty _observer;
    properties::StringProperty _frame;
    properties::StringProperty _heightTexture;


    glm::dvec3 MSL_position;

    std::string _texturePath;
    std::unique_ptr<ghoul::opengl::Texture> _texture;
    glm::dvec3 directionVector;
    glm::dvec3 newPoint;

    int nrPoints;

    unsigned int arrayWidth;
    unsigned int arrayHeight;
    std::vector<std::vector<double>> heightMap;
    bool fileRead = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___SPACECRAFTPROJECTION___H__