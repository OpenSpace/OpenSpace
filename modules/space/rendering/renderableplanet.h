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

#ifndef __OPENSPACE_MODULE_SPACE___RENDERABLEPLANET___H__
#define __OPENSPACE_MODULE_SPACE___RENDERABLEPLANET___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>

#include <memory>
#include <vector>

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

struct TransformData;

namespace documentation { struct Documentation; }
namespace planetgeometry { class PlanetGeometry; }

class RenderablePlanet : public Renderable {
public:
    // Shadow structure
    struct ShadowConfiguration {
        std::pair<std::string, float> source;
        std::pair<std::string, float> caster;
    };

    struct ShadowRenderingStruct {
        float xu;
        float xp;
        float rs;
        float rc;
        glm::vec3 sourceCasterVec;
        glm::vec3 casterPositionVec;
        bool isShadowing;
    };

    RenderablePlanet(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;
    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

protected:
    void loadTexture();

private:
    glm::dmat4 computeModelTransformMatrix(const openspace::TransformData& transformData);

    properties::StringProperty _colorTexturePath;
    properties::StringProperty _nightTexturePath;
    properties::StringProperty _heightMapTexturePath;

    ghoul::opengl::ProgramObject* _programObject = nullptr;

    std::unique_ptr<ghoul::opengl::Texture> _texture;
    std::unique_ptr<ghoul::opengl::Texture> _nightTexture;
    std::unique_ptr<ghoul::opengl::Texture> _heightMapTexture;

    properties::FloatProperty _heightExaggeration;

    std::unique_ptr<planetgeometry::PlanetGeometry> _geometry;
    properties::BoolProperty _performShading;

    float _alpha = 1.f;
    float _planetRadius = 0.f;
    bool _hasNightTexture = false;
    bool _hasHeightTexture = false;
    bool _shadowEnabled = false;
    double _time = 0.f;

    glm::dmat3 _stateMatrix;

    std::vector<ShadowConfiguration> _shadowConfArray;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___RENDERABLEPLANET___H__
