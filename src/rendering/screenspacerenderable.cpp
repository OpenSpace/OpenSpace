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
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/screenspacerenderable.h>

namespace openspace {
ScreenSpaceRenderable::ScreenSpaceRenderable()
    :_enabled("enabled", "Is Enabled", true)
    ,_useFlatScreen("flatScreen", "Flat Screen", true)
    ,_euclideanPosition("euclideanPosition", "Euclidean coordinates", glm::vec2(0),glm::vec2(-4),glm::vec2(4))
    ,_sphericalPosition("sphericalPosition", "Spherical coordinates", glm::vec2(0, M_PI_2),glm::vec2(-M_PI),glm::vec2(M_PI))
    ,_depth("depth", "Depth", 0, 0, 1)
    ,_scale("scale", "Scale" , 0.25, 0, 2)
    ,_alpha("alpha", "Alpha" , 1, 0, 1)
    ,_delete("delete", "Delete")
    ,_quad(0)
    ,_vertexPositionBuffer(0)
    ,_rendererPath("${SHADERS}/framebuffer/renderframebuffer.frag")
    ,_vertexPath("${MODULE_BASE}/shaders/screnspace_vs.glsl")
    ,_fragmentPath("${MODULE_BASE}/shaders/screnspace_fs.glsl")
    ,_texture(nullptr)
    ,_shader(nullptr)
{
    addProperty(_enabled);
    addProperty(_useFlatScreen);
    addProperty(_euclideanPosition);
    addProperty(_sphericalPosition);
    addProperty(_depth);
    addProperty(_scale);
    addProperty(_alpha);
    addProperty(_delete);

    _rendererData = ghoul::Dictionary();
    _rendererData.setValue("fragmentRendererPath", _rendererPath);
    _rendererData.setValue("windowWidth", OsEng.windowWrapper().currentWindowResolution().x);
    _rendererData.setValue("windowHeight", OsEng.windowWrapper().currentWindowResolution().y);

    _radius = _planeDepth;

    useEuclideanCoordinates(_useFlatScreen.value());

    _euclideanPosition.onChange([this](){
        _sphericalPosition.set(toSpherical(_euclideanPosition.value()));
    });
    
    _sphericalPosition.onChange([this](){
        _euclideanPosition.set(toEuclidean(_sphericalPosition.value(), _radius));
    });

    // Setting spherical/euclidean onchange handler
    _useFlatScreen.onChange([this](){
        useEuclideanCoordinates(_useFlatScreen.value());
    });

    _delete.onChange([this](){OsEng.renderEngine().unregisterScreenSpaceRenderable(name());});
}

ScreenSpaceRenderable::~ScreenSpaceRenderable(){}

bool ScreenSpaceRenderable::isEnabled() const {
    return _enabled;
}

void ScreenSpaceRenderable::createPlane() {
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
    // ============================
    //         GEOMETRY (quad)
    // ============================
    const GLfloat vertex_data[] = { // square of two triangles (sigh)
        //      x      y     z     w     s     t
        -1, -1, 0.0f, 1, 0, 1,
         1,  1, 0.0f, 1, 1, 0,
        -1,  1, 0.0f, 1, 0, 0,
        -1, -1, 0.0f, 1, 0, 1,
         1, -1, 0.0f, 1, 1, 1,
         1,  1, 0.0f, 1, 1, 0,
    };

    glBindVertexArray(_quad); // bind array
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(sizeof(GLfloat) * 4));
}

void ScreenSpaceRenderable::useEuclideanCoordinates(bool b){
    _useEuclideanCoordinates = b;
    if(_useEuclideanCoordinates){
        _euclideanPosition.set(toEuclidean(_sphericalPosition.value(), _radius));
    } else {
        _sphericalPosition.set(toSpherical(_euclideanPosition.value()));
    }
}

glm::vec2 ScreenSpaceRenderable::toEuclidean(glm::vec2 polar, float r){
    float x = r*sin(polar[0])*sin(polar[1]);
    float y = r*cos(polar[1]);
    
    return glm::vec2(x, y);
}

glm::vec2 ScreenSpaceRenderable::toSpherical(glm::vec2 euclidean){    
    _radius = -sqrt(pow(euclidean[0],2)+pow(euclidean[1],2)+pow(_planeDepth,2));
    float theta    = atan2(-_planeDepth,euclidean[0])-M_PI/2.0;
    float phi = acos(euclidean[1]/_radius);

    return glm::vec2(theta, phi);
}

void ScreenSpaceRenderable::registerProperties(){
    OsEng.gui()._screenSpaceProperty.registerProperty(&_enabled);
    OsEng.gui()._screenSpaceProperty.registerProperty(&_useFlatScreen);
    OsEng.gui()._screenSpaceProperty.registerProperty(&_euclideanPosition);
    OsEng.gui()._screenSpaceProperty.registerProperty(&_sphericalPosition);
    OsEng.gui()._screenSpaceProperty.registerProperty(&_depth);
    OsEng.gui()._screenSpaceProperty.registerProperty(&_scale);
    OsEng.gui()._screenSpaceProperty.registerProperty(&_alpha);
    OsEng.gui()._screenSpaceProperty.registerProperty(&_delete);
}

void ScreenSpaceRenderable::unregisterProperties(){
    OsEng.gui()._screenSpaceProperty.unregisterProperties(name());
}

void ScreenSpaceRenderable::createShaders(){
    if(!_shader) {

        ghoul::Dictionary dict = ghoul::Dictionary();

        dict.setValue("rendererData", _rendererData);
        dict.setValue("fragmentPath", _fragmentPath);
        _shader = ghoul::opengl::ProgramObject::Build("ScreenSpaceProgram",
            _vertexPath,
            "${SHADERS}/render.frag",
            dict
            );
    }
}

glm::mat4 ScreenSpaceRenderable::scaleMatrix(){
    glm::mat4 scale(1.0);

    glm::vec2 resolution = OsEng.windowWrapper().currentWindowResolution();

    //to scale the plane
    float textureRatio =  (float(_texture->height())/float(_texture->width()));
    float scalingRatioX = _originalViewportSize[0]/ resolution[0];
    float scalingRatioY = _originalViewportSize[1]/ resolution[1];
    scale = glm::scale(scale, glm::vec3(_scale.value() * scalingRatioX,
                                        _scale.value() * scalingRatioY * textureRatio,
                                        1)); 
    return scale;
}

glm::mat4 ScreenSpaceRenderable::rotationMatrix(){
    glm::mat4 rotation(1.0);
    if(!_useEuclideanCoordinates){
        glm::vec2 position = _sphericalPosition.value();

        float theta = position.x;
        float phi = position.y - M_PI/2.0;

        rotation = glm::rotate(rotation, position.x, glm::vec3(0.0f, 1.0f, 0.0f));
        rotation = glm::rotate(rotation, (float) (position.y - M_PI/2.0) , glm::vec3(1.0f, 0.0f, 0.0f));
    }

    return rotation;
}


glm::mat4 ScreenSpaceRenderable::translationMatrix(){
    glm::mat4 translation(1.0);
    if(!_useEuclideanCoordinates){
        translation = glm::translate(translation, glm::vec3(0.0f, 0.0f, _planeDepth));
    }else{
        translation = glm::translate(glm::mat4(1.f), glm::vec3(_euclideanPosition.value(), _planeDepth));
    }

    return translation;
}


void ScreenSpaceRenderable::draw(glm::mat4 modelTransform){
    float occlusionDepth = 1-_depth.value();

    glEnable(GL_DEPTH_TEST);
    glDisable(GL_CULL_FACE);

    _shader->activate();
    _shader->setUniform("OcclusionDepth", occlusionDepth);
    _shader->setUniform("Alpha", _alpha);
    _shader->setUniform("ModelTransform",modelTransform);
    _shader->setUniform("ViewProjectionMatrix", OsEng.renderEngine().camera()->viewProjectionMatrix());
    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _shader->setUniform("texture1", unit);

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glEnable(GL_CULL_FACE);

    _shader->deactivate();
}
}// namespace openspace