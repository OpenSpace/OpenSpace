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

#ifndef __RENDERABLEPLANET_H__
#define __RENDERABLEPLANET_H__

// open space includes
#include <openspace/rendering/renderable.h>
#include <openspace/util/powerscaledsphere.h>
#include <openspace/properties/scalarproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vectorproperty.h>

// ghoul includes
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>

namespace openspace {

class RenderablePlanet : public Renderable {
public:
    RenderablePlanet(const ghoul::Dictionary& dictionary);
    ~RenderablePlanet();

    bool initialize() override;
    bool deinitialize() override;

    void setProgramObject(ghoul::opengl::ProgramObject* programObject = nullptr);
    void setTexture(ghoul::opengl::Texture* texture);

    void render(const Camera* camera, const psc& thisPosition) override;
    void update() override;

protected:
    void createSphere();
    void loadTexture();

private:
    properties::Vec2Property _radius;
    properties::IntProperty _segments;
    properties::StringProperty _colorTexturePath;

    ghoul::opengl::ProgramObject* _programObject;
    ghoul::opengl::Texture* _texture;
    PowerScaledSphere* _planet;
};

}  // namespace openspace

#endif  // __RENDERABLEPLANET_H__