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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLEMODEL___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLEMODEL___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>

#include <memory>

namespace openspace {

namespace modelgeometry {
class ModelGeometry;
}

class RenderableModel : public Renderable {
public:
    RenderableModel(const ghoul::Dictionary& dictionary);

    bool initialize() override;
    bool deinitialize() override;

    bool isReady() const override;

    void render(const RenderData& data) override;
    void update(const UpdateData& data) override;


protected:
    void loadTexture();

private:
    properties::StringProperty _colorTexturePath;
    properties::BoolProperty _performFade;
    properties::FloatProperty _fading;
    std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;
    std::unique_ptr<ghoul::opengl::Texture> _texture;

    std::unique_ptr<modelgeometry::ModelGeometry> _geometry;
    
    glm::dmat3 _modelTransform;

    float _alpha;
    //glm::dmat3 _stateMatrix; 

    //std::string _source;
    //std::string _destination;
    std::string _target;

    //bool _isGhost;
    int _frameCount;

    glm::dvec3 _sunPos;

    properties::BoolProperty _performShading;
    properties::Vec3Property _debugModelRotation;
};

}  // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLEMODEL___H__
