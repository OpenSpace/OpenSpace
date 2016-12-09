/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLEPLANET___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLEPLANET___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/opengl/textureunit.h>

#include <vector>
#include <string>

namespace ghoul {
    namespace opengl {
        class ProgramObject;
        class Texture;
    }
}

namespace openspace {

namespace planetgeometry {
class PlanetGeometry;
}

class RenderablePlanet : public Renderable {
public:
    // Shadow structure
    typedef struct {
        std::pair<std::string, float> source;
        std::pair<std::string, float> caster;
    } ShadowConf;

    struct ShadowRenderingStruct {
        float xu, xp;
        float rs, rc;
        glm::vec3 sourceCasterVec;
        glm::vec3 casterPositionVec;
        bool isShadowing;
    };

public:
    explicit RenderablePlanet(const ghoul::Dictionary& dictionary);
    ~RenderablePlanet();

    bool initialize() override;
    bool deinitialize() override;
    bool isReady() const override;

    void render(const RenderData& data) override;
    void update(const UpdateData& data) override;

protected:
    void loadTexture();
private:
    properties::StringProperty _colorTexturePath;
    properties::StringProperty _nightTexturePath;
    properties::StringProperty _heightMapTexturePath;
    
    std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;
    
    std::unique_ptr<ghoul::opengl::Texture> _texture;
    std::unique_ptr<ghoul::opengl::Texture> _nightTexture;    
    std::unique_ptr<ghoul::opengl::Texture> _heightMapTexture;
    
    properties::FloatProperty _heightExaggeration;

    planetgeometry::PlanetGeometry* _geometry;
    properties::BoolProperty _performShading;
    properties::IntProperty _rotation;
    float _alpha;
    std::vector< ShadowConf > _shadowConfArray;
    float _planetRadius;

    glm::dmat3 _stateMatrix;
    std::string _frame;
    std::string _target;
    bool _hasNightTexture;
    bool _hasHeightTexture;
    bool _shadowEnabled;
    double _time;

    bool tempPic;
};

}  // namespace openspace

#endif  // __OPENSPACE_MODULE_BASE___RENDERABLEPLANET___H__
